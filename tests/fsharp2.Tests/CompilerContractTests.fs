namespace fsharp2.Tests

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography
open System.Threading
open Expecto
open FSharp2.Compiler

module CompilerContractTests =
    type private MethodCall = {
        CallerType: string
        CallerMethod: string
        CalleeType: string
        CalleeMethod: string
    }

    let private bytes (values: seq<byte>) =
        values
        |> Seq.toArray

    let private fingerprint (values: byte array) =
        values
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let private metadataTypeName (metadata: MetadataReader) (handle: EntityHandle) =
        let fullName namespaceName name =
            if String.IsNullOrEmpty(namespaceName) then
                name
            else
                namespaceName
                + "."
                + name

        match handle.Kind with
        | HandleKind.TypeDefinition ->
            let definition =
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.TypeDefinitionHandle
                |> metadata.GetTypeDefinition

            fullName
                (metadata.GetString(definition.Namespace))
                (metadata.GetString(definition.Name))
        | HandleKind.TypeReference ->
            let reference =
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.TypeReferenceHandle
                |> metadata.GetTypeReference

            fullName (metadata.GetString(reference.Namespace)) (metadata.GetString(reference.Name))
        | _ -> handle.Kind.ToString()

    let private typeDefinitionEntityHandle (handle: TypeDefinitionHandle) =
        MetadataTokens.EntityHandle(TableIndex.TypeDef, MetadataTokens.GetRowNumber(handle))

    let rec private metadataMethodName (metadata: MetadataReader) (handle: EntityHandle) =
        match handle.Kind with
        | HandleKind.MethodDefinition ->
            let definition =
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.MethodDefinitionHandle
                |> metadata.GetMethodDefinition

            metadataTypeName metadata (typeDefinitionEntityHandle (definition.GetDeclaringType())),
            metadata.GetString(definition.Name)
        | HandleKind.MemberReference ->
            let reference =
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.MemberReferenceHandle
                |> metadata.GetMemberReference

            metadataTypeName metadata reference.Parent, metadata.GetString(reference.Name)
        | HandleKind.MethodSpecification ->
            handle
            |> MetadataTokens.GetRowNumber
            |> MetadataTokens.MethodSpecificationHandle
            |> metadata.GetMethodSpecification
            |> _.Method
            |> metadataMethodName metadata
        | _ -> handle.Kind.ToString(), String.Empty

    let private opCodes =
        typeof<OpCodes>
            .GetFields(
                BindingFlags.Public
                ||| BindingFlags.Static
            )
        |> Seq.map (fun field ->
            let opCode = field.GetValue(null) :?> OpCode
            int (uint16 opCode.Value), opCode
        )
        |> Map.ofSeq

    let private operandSize (bytes: byte array) offset operandType =
        match operandType with
        | OperandType.InlineNone -> 0
        | OperandType.ShortInlineBrTarget
        | OperandType.ShortInlineI
        | OperandType.ShortInlineVar -> 1
        | OperandType.InlineVar -> 2
        | OperandType.InlineBrTarget
        | OperandType.InlineField
        | OperandType.InlineI
        | OperandType.InlineMethod
        | OperandType.InlineSig
        | OperandType.InlineString
        | OperandType.InlineTok
        | OperandType.InlineType
        | OperandType.ShortInlineR -> 4
        | OperandType.InlineI8
        | OperandType.InlineR -> 8
        | OperandType.InlineSwitch ->
            let count = BitConverter.ToInt32(bytes, offset)

            4
            + (count * 4)
        | operand -> failtestf "Unsupported IL operand type %A." operand

    let private methodCalls assemblyPath =
        use stream = File.OpenRead(assemblyPath)
        use pe = new PEReader(stream)
        let metadata = pe.GetMetadataReader()

        [|
            for methodHandle in metadata.MethodDefinitions do
                let definition = metadata.GetMethodDefinition(methodHandle)

                if
                    definition.RelativeVirtualAddress
                    <> 0
                then
                    let callerType =
                        metadataTypeName
                            metadata
                            (typeDefinitionEntityHandle (definition.GetDeclaringType()))

                    let callerMethod = metadata.GetString(definition.Name)
                    let bytes = pe.GetMethodBody(definition.RelativeVirtualAddress).GetILBytes()
                    let mutable offset = 0

                    while offset < bytes.Length do
                        let first = int bytes[offset]
                        offset <- offset + 1

                        let value =
                            if first = 0xfe then
                                let second = int bytes[offset]
                                offset <- offset + 1

                                0xfe00
                                ||| second
                            else
                                first

                        let opCode = opCodes[value]

                        if
                            opCode = OpCodes.Call
                            || opCode = OpCodes.Callvirt
                        then
                            let token = BitConverter.ToInt32(bytes, offset)

                            let calleeType, calleeMethod =
                                token
                                |> MetadataTokens.EntityHandle
                                |> metadataMethodName metadata

                            yield {
                                CallerType = callerType
                                CallerMethod = callerMethod
                                CalleeType = calleeType
                                CalleeMethod = calleeMethod
                            }

                        offset <-
                            offset
                            + operandSize bytes offset opCode.OperandType
        |]

    let private hasCall callerType callerMethod calleeType calleeMethod calls =
        calls
        |> Seq.exists (fun call ->
            call.CallerType = callerType
            && call.CallerMethod = callerMethod
            && call.CalleeType = calleeType
            && call.CalleeMethod = calleeMethod
        )

    let private contractPhases = [|
        CompilationPhase.Source
        CompilationPhase.Syntax
        CompilationPhase.ResolvedSymbols
        CompilationPhase.TypedDeclarations
        CompilationPhase.LoweredCode
        CompilationPhase.OptimizedCode
        CompilationPhase.SymbolicEmission
        CompilationPhase.FinalLinking
    |]

    let private createRequest
        semanticOptions
        diagnosticOptions
        emissionOptions
        signingOptions
        resources
        requestedArtifacts
        (sourceText: string)
        =
        let sourceBytes = Text.Encoding.UTF8.GetBytes(sourceText)
        let referenceImage = File.ReadAllBytes(Assembly.Load("System.Runtime").Location)

        CompilationRequest.Create(
            CompilerContract.Version,
            StableIdentity.create "request:tracer",
            CompilationAssemblyIdentity.Create(StableIdentity.create "assembly:Tracer", "Tracer"),
            [|
                SourceSnapshot.Create(
                    StableIdentity.create "source:tracer",
                    "Tracer.fs",
                    sourceText,
                    fingerprint sourceBytes
                )
            |],
            [|
                TargetReferenceSnapshot.Create(
                    StableIdentity.create "reference:System.Runtime",
                    "System.Runtime.dll",
                    referenceImage,
                    fingerprint referenceImage
                )
            |],
            semanticOptions,
            diagnosticOptions,
            emissionOptions,
            signingOptions,
            resources,
            requestedArtifacts
        )

    let private defaultSemanticOptions () =
        SemanticOptions.Create([||], None, OptimizationMode.Disabled, false, false, None)

    let private defaultDiagnosticOptions () =
        DiagnosticOptions.Create(None, [||], false, [||])

    let private defaultEmissionOptions () =
        EmissionOptions.Create(true, false, DebugFormat.None, [||], [| "Tracer.fs" |], [||])

    let private defaultSigningOptions () =
        SigningOptions.Create(SigningMode.Unsigned, [||])

    let private emptyResources () = ResourceInputs.Create([||], [||])

    let private defaultRequestedArtifacts = [| RequestedArtifact.ImplementationAssembly |]

    let private compileRequest request =
        Compiler().Compile(request, CancellationToken.None)

    let private assertUnsupported expectedCode expectedPhase expectedIdentity result =
        match result.Outcome with
        | CompilationOutcome.Unsupported failure ->
            Expect.equal failure.Code expectedCode "The failure code must be stable."
            Expect.equal failure.StoppingPhase expectedPhase "The stopping phase must be exact."

            Expect.equal
                failure.UnsupportedValueIdentity
                expectedIdentity
                "The unsupported value identity must be stable."
        | outcome -> failtestf "Expected an unsupported result, but received %A." outcome

        Expect.sequenceEqual
            (result.PhaseResults
             |> Seq.map _.Phase)
            contractPhases
            "Every result must contain the complete contract phase ledger."

        let stoppingIndex =
            contractPhases
            |> Array.findIndex ((=) expectedPhase)

        let expectedStatuses =
            contractPhases
            |> Array.mapi (fun index phase ->
                if index = stoppingIndex then
                    PhaseStatus.Unsupported
                else
                    PhaseStatus.NotStarted
            )

        Expect.sequenceEqual
            (result.PhaseResults
             |> Seq.map _.Status)
            expectedStatuses
            "The phase statuses must stop at the unsupported value."

        result.PhaseResults
        |> Seq.iter (fun phase ->
            Expect.isNone
                phase.InputFingerprint
                "A prevalidated phase must not have an input fingerprint."

            Expect.isNone
                phase.OutputFingerprint
                "A prevalidated phase must not have an output fingerprint."
        )

        Expect.isEmpty
            result.Diagnostics
            "A structured unsupported result must not contain diagnostics."

        Expect.isEmpty result.Artifacts "An unsupported compilation must not contain artifacts."

        Expect.isEmpty
            result.Fingerprints
            "An unsupported compilation must not contain artifact fingerprints."

    let private compileWithOptimization optimization (sourceText: string) =
        let request =
            createRequest
                (SemanticOptions.Create([||], None, optimization, false, false, None))
                (defaultDiagnosticOptions ())
                (defaultEmissionOptions ())
                (defaultSigningOptions ())
                (emptyResources ())
                defaultRequestedArtifacts
                sourceText

        compileRequest request

    let private compile sourceText =
        compileWithOptimization OptimizationMode.Disabled sourceText

    [<Tests>]
    let tests =
        testList "Compiler Contract" [
            testCase "request normalization defensively copies every collection and byte input"
            <| fun _ ->
                let sourceIdentity = StableIdentity.create "source:one"
                let otherSourceIdentity = StableIdentity.create "source:two"
                let referenceIdentity = StableIdentity.create "reference:one"
                let otherReferenceIdentity = StableIdentity.create "reference:two"
                let managedIdentity = StableIdentity.create "managed-resource:one"
                let otherManagedIdentity = StableIdentity.create "managed-resource:two"
                let nativeIdentity = StableIdentity.create "native-resource:one"
                let otherNativeIdentity = StableIdentity.create "native-resource:two"

                let sourceLinkJson = [|
                    1uy
                    2uy
                    3uy
                |]

                let targetImage = [|
                    4uy
                    5uy
                    6uy
                |]

                let otherTargetImage = [|
                    7uy
                    8uy
                    9uy
                |]

                let managedContent = [|
                    10uy
                    11uy
                    12uy
                |]

                let otherManagedContent = [|
                    13uy
                    14uy
                    15uy
                |]

                let nativeContent = [|
                    16uy
                    17uy
                    18uy
                |]

                let otherNativeContent = [|
                    19uy
                    20uy
                    21uy
                |]

                let signingKey = [|
                    22uy
                    23uy
                    24uy
                |]

                let sources = [|
                    SourceSnapshot.Create(sourceIdentity, "one.fs", "module One", "source-one")
                    SourceSnapshot.Create(otherSourceIdentity, "two.fs", "module Two", "source-two")
                |]

                let references = [|
                    TargetReferenceSnapshot.Create(
                        referenceIdentity,
                        "System.Runtime.dll",
                        targetImage,
                        "reference-one"
                    )
                    TargetReferenceSnapshot.Create(
                        otherReferenceIdentity,
                        "FSharp.Core.dll",
                        otherTargetImage,
                        "reference-two"
                    )
                |]

                let defines = [|
                    "DEBUG"
                    "TRACE"
                |]

                let disabledWarnings = [|
                    "0044"
                    "0066"
                |]

                let warningsAsErrors = [|
                    "0025"
                    "0026"
                |]

                let embeddedSources = [|
                    sourceIdentity
                    otherSourceIdentity
                |]

                let debugDocumentPaths = [|
                    "one.fs"
                    "two.fs"
                |]

                let semanticOptions =
                    SemanticOptions.Create(
                        defines,
                        Some "9.0",
                        OptimizationMode.Disabled,
                        true,
                        false,
                        Some "netcore"
                    )

                let diagnosticOptions =
                    DiagnosticOptions.Create(Some 5, disabledWarnings, true, warningsAsErrors)

                let emissionOptions =
                    EmissionOptions.Create(
                        true,
                        false,
                        DebugFormat.Portable,
                        embeddedSources,
                        debugDocumentPaths,
                        sourceLinkJson
                    )

                let signingOptions = SigningOptions.Create(SigningMode.FullSign, signingKey)

                let managedResources = [|
                    ManagedResourceSnapshot.Create(
                        managedIdentity,
                        "one.resources",
                        ResourceVisibility.Public,
                        managedContent,
                        "managed-one"
                    )
                    ManagedResourceSnapshot.Create(
                        otherManagedIdentity,
                        "two.resources",
                        ResourceVisibility.Private,
                        otherManagedContent,
                        "managed-two"
                    )
                |]

                let nativeResources = [|
                    NativeResourceSnapshot.Create(nativeIdentity, nativeContent, "native-one")
                    NativeResourceSnapshot.Create(
                        otherNativeIdentity,
                        otherNativeContent,
                        "native-two"
                    )
                |]

                let resourceInputs = ResourceInputs.Create(managedResources, nativeResources)

                let requestedArtifacts = [|
                    RequestedArtifact.ImplementationAssembly
                    RequestedArtifact.PortablePdb
                |]

                let request =
                    CompilationRequest.Create(
                        CompilerContract.Version,
                        StableIdentity.create "request:one",
                        CompilationAssemblyIdentity.Create(
                            StableIdentity.create "assembly:ContractProbe",
                            "ContractProbe"
                        ),
                        sources,
                        references,
                        semanticOptions,
                        diagnosticOptions,
                        emissionOptions,
                        signingOptions,
                        resourceInputs,
                        requestedArtifacts
                    )

                Array.Reverse sources
                sources[1] <- sources[0]
                Array.Reverse references
                references[1] <- references[0]
                targetImage[0] <- 90uy
                defines[0] <- "MUTATED"
                Array.Reverse defines
                disabledWarnings[0] <- "9998"
                Array.Reverse disabledWarnings
                warningsAsErrors[0] <- "9999"
                Array.Reverse warningsAsErrors
                embeddedSources[0] <- otherSourceIdentity
                Array.Reverse embeddedSources
                debugDocumentPaths[0] <- "mutated.fs"
                Array.Reverse debugDocumentPaths
                sourceLinkJson[0] <- 91uy
                Array.Reverse managedResources
                managedResources[1] <- managedResources[0]
                managedContent[0] <- 92uy
                Array.Reverse nativeResources
                nativeResources[1] <- nativeResources[0]
                nativeContent[0] <- 93uy
                signingKey[0] <- 94uy
                requestedArtifacts[0] <- RequestedArtifact.Documentation
                Array.Reverse requestedArtifacts

                Expect.equal
                    request.Sources[0].StableId
                    sourceIdentity
                    "The source order is immutable."

                Expect.equal
                    request.TargetReferences[0].StableId
                    referenceIdentity
                    "The reference order is immutable."

                Expect.sequenceEqual
                    (bytes request.TargetReferences[0].PeImage)
                    [|
                        4uy
                        5uy
                        6uy
                    |]
                    "The reference image is immutable."

                Expect.sequenceEqual
                    request.SemanticOptions.Defines
                    [|
                        "DEBUG"
                        "TRACE"
                    |]
                    "The defines are immutable."

                Expect.sequenceEqual
                    request.DiagnosticOptions.DisabledWarnings
                    [|
                        "0044"
                        "0066"
                    |]
                    "The disabled warnings are immutable."

                Expect.sequenceEqual
                    request.DiagnosticOptions.WarningsAsErrors
                    [|
                        "0025"
                        "0026"
                    |]
                    "The promoted warnings are immutable."

                Expect.sequenceEqual
                    request.EmissionOptions.EmbeddedSourceIdentities
                    [|
                        sourceIdentity
                        otherSourceIdentity
                    |]
                    "The embedded source identities are immutable."

                Expect.sequenceEqual
                    request.EmissionOptions.DebugDocumentPaths
                    [|
                        "one.fs"
                        "two.fs"
                    |]
                    "The debug document paths are immutable."

                Expect.sequenceEqual
                    (bytes request.EmissionOptions.SourceLinkJson)
                    [|
                        1uy
                        2uy
                        3uy
                    |]
                    "The SourceLink JSON is immutable."

                Expect.equal
                    request.Resources.Managed[0].StableId
                    managedIdentity
                    "The managed resource order is immutable."

                Expect.sequenceEqual
                    (bytes request.Resources.Managed[0].Content)
                    [|
                        10uy
                        11uy
                        12uy
                    |]
                    "The managed resource content is immutable."

                Expect.equal
                    request.Resources.Native[0].StableId
                    nativeIdentity
                    "The native resource order is immutable."

                Expect.sequenceEqual
                    (bytes request.Resources.Native[0].Content)
                    [|
                        16uy
                        17uy
                        18uy
                    |]
                    "The native resource content is immutable."

                Expect.sequenceEqual
                    (bytes request.SigningOptions.Key)
                    [|
                        22uy
                        23uy
                        24uy
                    |]
                    "The strong-name key is immutable."

                Expect.sequenceEqual
                    request.RequestedArtifacts
                    [|
                        RequestedArtifact.ImplementationAssembly
                        RequestedArtifact.PortablePdb
                    |]
                    "The requested artifacts are immutable."

                Expect.equal
                    (typeof<CompilationRequest>.Assembly.GetName().Name)
                    "FSharp2.Compiler.Core"
                    "The core assembly name is stable."

                let duplicateSource =
                    SourceSnapshot.Create(
                        sourceIdentity,
                        "duplicate.fs",
                        "module Duplicate",
                        "duplicate"
                    )

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        CompilationRequest.Create(
                            CompilerContract.Version,
                            StableIdentity.create "request:duplicate",
                            CompilationAssemblyIdentity.Create(
                                StableIdentity.create "assembly:DuplicateProbe",
                                "DuplicateProbe"
                            ),
                            [|
                                request.Sources[0]
                                duplicateSource
                            |],
                            [||],
                            semanticOptions,
                            diagnosticOptions,
                            emissionOptions,
                            signingOptions,
                            ResourceInputs.Create([||], [||]),
                            [| RequestedArtifact.ImplementationAssembly |]
                        )
                        |> ignore
                    )
                    "A duplicate stable identity is invalid."

                Expect.throwsT<ArgumentException>
                    (fun () ->
                        CompilationRequest.Create(
                            CompilerContract.Version
                            + 1,
                            StableIdentity.create "request:unsupported-version",
                            CompilationAssemblyIdentity.Create(
                                StableIdentity.create "assembly:UnsupportedVersionProbe",
                                "UnsupportedVersionProbe"
                            ),
                            [| request.Sources[0] |],
                            [||],
                            semanticOptions,
                            diagnosticOptions,
                            emissionOptions,
                            signingOptions,
                            ResourceInputs.Create([||], [||]),
                            [| RequestedArtifact.ImplementationAssembly |]
                        )
                        |> ignore
                    )
                    "An unsupported contract version is invalid."

            testCase "Compiler.Compile uses captured inputs and a path-free assembly identity"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-compiler-contract",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "input.fs")
                    let referencePath = Path.Combine(root, "target-reference.dll")
                    let sourceText = "module Tracer\nlet answer () = 42\n"
                    let referenceImage = File.ReadAllBytes(Assembly.Load("System.Runtime").Location)

                    File.WriteAllText(sourcePath, sourceText)
                    File.WriteAllBytes(referencePath, referenceImage)

                    let sourceSnapshot =
                        SourceSnapshot.Create(
                            StableIdentity.create "source:tracer",
                            "Tracer.fs",
                            File.ReadAllText(sourcePath),
                            fingerprint (Text.Encoding.UTF8.GetBytes(sourceText))
                        )

                    let referenceSnapshot =
                        let capturedImage = File.ReadAllBytes(referencePath)

                        TargetReferenceSnapshot.Create(
                            StableIdentity.create "reference:System.Runtime",
                            "System.Runtime.dll",
                            capturedImage,
                            fingerprint capturedImage
                        )

                    let request =
                        CompilationRequest.Create(
                            CompilerContract.Version,
                            StableIdentity.create "request:tracer",
                            CompilationAssemblyIdentity.Create(
                                StableIdentity.create "assembly:Tracer",
                                "Tracer"
                            ),
                            [| sourceSnapshot |],
                            [| referenceSnapshot |],
                            SemanticOptions.Create(
                                [||],
                                None,
                                OptimizationMode.Disabled,
                                false,
                                false,
                                None
                            ),
                            DiagnosticOptions.Create(None, [||], false, [||]),
                            EmissionOptions.Create(
                                true,
                                false,
                                DebugFormat.None,
                                [||],
                                [| "Tracer.fs" |],
                                [||]
                            ),
                            SigningOptions.Create(SigningMode.Unsigned, [||]),
                            ResourceInputs.Create([||], [||]),
                            [| RequestedArtifact.ImplementationAssembly |]
                        )

                    File.Delete(sourcePath)
                    File.WriteAllBytes(referencePath, [| 0uy |])

                    let result = Compiler().Compile(request, CancellationToken.None)

                    Expect.equal
                        result.Outcome
                        CompilationOutcome.Succeeded
                        "Compilation must succeed."

                    let implementation =
                        result.Artifacts
                        |> Seq.exactlyOne

                    Expect.equal
                        implementation.Kind
                        RequestedArtifact.ImplementationAssembly
                        "The result must contain the implementation assembly."

                    let emittedAssembly = Assembly.Load(bytes implementation.Bytes)

                    Expect.equal
                        (emittedAssembly.GetName().Name)
                        "Tracer"
                        "The assembly name must be path-free."

                    let answer = emittedAssembly.GetType("Tracer", true).GetMethod("answer")

                    Expect.equal
                        (answer.Invoke(null, Array.empty<obj>))
                        (box 42)
                        "The captured source must execute."
                finally
                    Directory.Delete(root, true)

            testCase "unresolved value stops at ResolvedSymbols"
            <| fun _ ->
                let result = compile "module Tracer\nlet answer () = missing\n"

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let phaseStatus phase =
                    result.PhaseResults
                    |> Seq.find (fun phaseResult -> phaseResult.Phase = phase)
                    |> _.Status

                Expect.equal
                    (phaseStatus CompilationPhase.Source)
                    PhaseStatus.Completed
                    "The source phase must complete."

                Expect.equal
                    (phaseStatus CompilationPhase.Syntax)
                    PhaseStatus.Completed
                    "The syntax phase must complete."

                Expect.equal
                    (phaseStatus CompilationPhase.ResolvedSymbols)
                    PhaseStatus.Failed
                    "Symbol resolution must fail."

                for phase in
                    [
                        CompilationPhase.TypedDeclarations
                        CompilationPhase.LoweredCode
                        CompilationPhase.OptimizedCode
                        CompilationPhase.SymbolicEmission
                        CompilationPhase.FinalLinking
                    ] do
                    Expect.equal
                        (phaseStatus phase)
                        PhaseStatus.NotStarted
                        $"The {phase} phase must not start."

                Expect.isEmpty result.Artifacts "A failed compilation must not contain an artifact."

            testCase "type mismatch stops at TypedDeclarations"
            <| fun _ ->
                let result = compile "module Tracer\nlet answer: int = \"text\"\n"

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.equal diagnostic.Code "FS0001" "The diagnostic code must be FS0001."

                let phaseStatus phase =
                    result.PhaseResults
                    |> Seq.find (fun phaseResult -> phaseResult.Phase = phase)
                    |> _.Status

                Expect.equal
                    (phaseStatus CompilationPhase.ResolvedSymbols)
                    PhaseStatus.Completed
                    "Symbol resolution must complete."

                Expect.equal
                    (phaseStatus CompilationPhase.TypedDeclarations)
                    PhaseStatus.Failed
                    "Type checking must fail."

                for phase in
                    [
                        CompilationPhase.LoweredCode
                        CompilationPhase.OptimizedCode
                        CompilationPhase.SymbolicEmission
                        CompilationPhase.FinalLinking
                    ] do
                    Expect.equal
                        (phaseStatus phase)
                        PhaseStatus.NotStarted
                        $"The {phase} phase must not start."

                Expect.isEmpty result.Artifacts "A failed compilation must not contain an artifact."

            testCase "success reports every contract phase in dependency order"
            <| fun _ ->
                let sourceText = "module Tracer\nlet answer () = 42\n"
                let first = compile sourceText
                let second = compile sourceText

                Expect.equal first.Outcome CompilationOutcome.Succeeded "Compilation must succeed."

                Expect.equal
                    second.Outcome
                    CompilationOutcome.Succeeded
                    "Compilation must succeed again."

                Expect.sequenceEqual
                    (first.PhaseResults
                     |> Seq.map _.Phase)
                    [
                        CompilationPhase.Source
                        CompilationPhase.Syntax
                        CompilationPhase.ResolvedSymbols
                        CompilationPhase.TypedDeclarations
                        CompilationPhase.LoweredCode
                        CompilationPhase.OptimizedCode
                        CompilationPhase.SymbolicEmission
                        CompilationPhase.FinalLinking
                    ]
                    "The phase order must match the compiler contract."

                Expect.sequenceEqual
                    (first.PhaseResults
                     |> Seq.map _.Status)
                    [
                        PhaseStatus.Completed
                        PhaseStatus.Completed
                        PhaseStatus.Completed
                        PhaseStatus.Completed
                        PhaseStatus.Completed
                        PhaseStatus.Skipped
                        PhaseStatus.Completed
                        PhaseStatus.Completed
                    ]
                    "The success statuses must match the compiler contract."

                Expect.sequenceEqual
                    (first.PhaseResults
                     |> Seq.map (fun phase -> phase.InputFingerprint, phase.OutputFingerprint))
                    (second.PhaseResults
                     |> Seq.map (fun phase -> phase.InputFingerprint, phase.OutputFingerprint))
                    "Phase fingerprints must be stable."

            testCase
                "ordered value and entry-point declarations compile through the public contract"
            <| fun _ ->
                let sourceText =
                    """module Program

let answer () = 42

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
"""

                let result = compile sourceText

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map (fun diagnostic -> diagnostic.Message)
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"Ordered value and entry-point declarations must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

            testCase "enabled optimization stops at OptimizedCode"
            <| fun _ ->
                let result =
                    compileWithOptimization
                        OptimizationMode.Enabled
                        "module Tracer\nlet answer () = 42\n"

                match result.Outcome with
                | CompilationOutcome.Unsupported failure ->
                    Expect.equal failure.Code "FSC2C2002" "The failure code must be stable."

                    Expect.equal
                        failure.StoppingPhase
                        CompilationPhase.OptimizedCode
                        "Optimization must be the stopping phase."

                    Expect.equal
                        failure.UnsupportedValueIdentity
                        "semantic.optimization=enabled"
                        "The unsupported value identity must be stable."
                | outcome -> failtestf "Expected an unsupported result, but received %A." outcome

                let phaseStatus phase =
                    result.PhaseResults
                    |> Seq.find (fun phaseResult -> phaseResult.Phase = phase)
                    |> _.Status

                Expect.equal
                    (phaseStatus CompilationPhase.LoweredCode)
                    PhaseStatus.Completed
                    "Lowering must complete."

                Expect.equal
                    (phaseStatus CompilationPhase.OptimizedCode)
                    PhaseStatus.Unsupported
                    "Optimization must be unsupported."

                Expect.equal
                    (phaseStatus CompilationPhase.SymbolicEmission)
                    PhaseStatus.NotStarted
                    "Symbolic emission must not start."

                Expect.equal
                    (phaseStatus CompilationPhase.FinalLinking)
                    PhaseStatus.NotStarted
                    "Final linking must not start."

                Expect.isEmpty
                    result.Artifacts
                    "An unsupported compilation must not contain an artifact."

            testCase "unsupported language version stops at Syntax"
            <| fun _ ->
                let request =
                    createRequest
                        (SemanticOptions.Create(
                            [||],
                            Some "8.0",
                            OptimizationMode.Disabled,
                            false,
                            false,
                            None
                        ))
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2001"
                    CompilationPhase.Syntax
                    "semantic.language-version=8.0"

            testCase "unsupported target profile stops at ResolvedSymbols"
            <| fun _ ->
                let request =
                    createRequest
                        (SemanticOptions.Create(
                            [||],
                            None,
                            OptimizationMode.Disabled,
                            false,
                            false,
                            Some "legacy"
                        ))
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2003"
                    CompilationPhase.ResolvedSymbols
                    "semantic.target-profile=legacy"

            testCase "invalid warning level stops at Source"
            <| fun _ ->
                for warningLevel in
                    [
                        -1
                        6
                    ] do
                    let request =
                        createRequest
                            (defaultSemanticOptions ())
                            (DiagnosticOptions.Create(Some warningLevel, [||], false, [||]))
                            (defaultEmissionOptions ())
                            (defaultSigningOptions ())
                            (emptyResources ())
                            defaultRequestedArtifacts
                            "module Tracer\nlet answer () = 42\n"

                    compileRequest request
                    |> assertUnsupported
                        "FSC2C2101"
                        CompilationPhase.Source
                        $"diagnostics.warning-level={warningLevel}"

            testCase "nondeterministic emission stops at FinalLinking"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            false,
                            false,
                            DebugFormat.None,
                            [||],
                            [| "Tracer.fs" |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2201"
                    CompilationPhase.FinalLinking
                    "emission.deterministic=false"

            testCase "high entropy virtual address stops at FinalLinking"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            true,
                            true,
                            DebugFormat.None,
                            [||],
                            [| "Tracer.fs" |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2202"
                    CompilationPhase.FinalLinking
                    "emission.high-entropy-va=true"

            testCase "portable PDB with no debug format stops at SymbolicEmission"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        [|
                            RequestedArtifact.ImplementationAssembly
                            RequestedArtifact.PortablePdb
                        |]
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2203"
                    CompilationPhase.SymbolicEmission
                    "emission.debug-format=none+pdb"

            testCase "embedded source stops at SymbolicEmission"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            true,
                            false,
                            DebugFormat.None,
                            [|
                                StableIdentity.create "source:tracer"
                                StableIdentity.create "source:other"
                            |],
                            [| "Tracer.fs" |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2204"
                    CompilationPhase.SymbolicEmission
                    "emission.embedded-source=source:tracer"

            testCase "debug document count stops at SymbolicEmission"
            <| fun _ ->
                for debugDocumentPaths in
                    [
                        [||]
                        [|
                            "Tracer.fs"
                            "Other.fs"
                        |]
                    ] do
                    let request =
                        createRequest
                            (defaultSemanticOptions ())
                            (defaultDiagnosticOptions ())
                            (EmissionOptions.Create(
                                true,
                                false,
                                DebugFormat.None,
                                [||],
                                debugDocumentPaths,
                                [||]
                            ))
                            (defaultSigningOptions ())
                            (emptyResources ())
                            defaultRequestedArtifacts
                            "module Tracer\nlet answer () = 42\n"

                    compileRequest request
                    |> assertUnsupported
                        "FSC2C2205"
                        CompilationPhase.SymbolicEmission
                        $"emission.debug-document-count={debugDocumentPaths.Length}"

            testCase "unsigned mode with a key stops at FinalLinking"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (SigningOptions.Create(SigningMode.Unsigned, [| 1uy |]))
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2301"
                    CompilationPhase.FinalLinking
                    "signing.unsigned-key=present"

            testCase "invalid signing key stops at FinalLinking"
            <| fun _ ->
                for mode, key in
                    [
                        SigningMode.DelaySign, [||]
                        SigningMode.PublicSign, [||]
                        SigningMode.FullSign, [||]
                        SigningMode.DelaySign, [| 0uy |]
                        SigningMode.PublicSign, [| 0uy |]
                        SigningMode.FullSign, [| 0uy |]
                    ] do
                    let request =
                        createRequest
                            (defaultSemanticOptions ())
                            (defaultDiagnosticOptions ())
                            (defaultEmissionOptions ())
                            (SigningOptions.Create(mode, key))
                            (emptyResources ())
                            defaultRequestedArtifacts
                            "module Tracer\nlet answer () = 42\n"

                    compileRequest request
                    |> assertUnsupported
                        "FSC2C2302"
                        CompilationPhase.FinalLinking
                        "signing.key=invalid"

            testCase "custom artifact stops as unsupported at Source"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        [| RequestedArtifact.Custom(StableIdentity.create "custom:test") |]
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2401"
                    CompilationPhase.Source
                    "artifact.custom=custom:test"

            testCase "multiple managed resources stop at FinalLinking"
            <| fun _ ->
                let managedResource index =
                    let content = [| byte index |]

                    ManagedResourceSnapshot.Create(
                        StableIdentity.create $"resource:managed:{index}",
                        $"resource-{index}",
                        ResourceVisibility.Public,
                        content,
                        fingerprint content
                    )

                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (ResourceInputs.Create(
                            [|
                                managedResource 1
                                managedResource 2
                            |],
                            [||]
                        ))
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2501"
                    CompilationPhase.FinalLinking
                    "resources.managed-count=2"

            testCase "multiple native resources stop at FinalLinking"
            <| fun _ ->
                let nativeResource index =
                    let content = [| byte index |]

                    NativeResourceSnapshot.Create(
                        StableIdentity.create $"resource:native:{index}",
                        content,
                        fingerprint content
                    )

                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (ResourceInputs.Create(
                            [||],
                            [|
                                nativeResource 1
                                nativeResource 2
                            |]
                        ))
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                compileRequest request
                |> assertUnsupported
                    "FSC2C2502"
                    CompilationPhase.FinalLinking
                    "resources.native-count=2"

            testCase "diagnostic policy preserves errors for every supported option"
            <| fun _ ->
                for warningLevel in
                    [
                        None
                        Some 0
                        Some 1
                        Some 2
                        Some 3
                        Some 4
                        Some 5
                    ] do
                    let request =
                        createRequest
                            (defaultSemanticOptions ())
                            (DiagnosticOptions.Create(
                                warningLevel,
                                [| "FS0001" |],
                                true,
                                [| "FS0001" |]
                            ))
                            (defaultEmissionOptions ())
                            (defaultSigningOptions ())
                            (emptyResources ())
                            defaultRequestedArtifacts
                            "module Tracer\nlet answer: int = \"text\"\n"

                    let result = compileRequest request
                    Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                    let diagnostic =
                        result.Diagnostics
                        |> Seq.exactlyOne

                    Expect.equal diagnostic.Code "FS0001" "The error code must be preserved."

                    Expect.equal
                        diagnostic.Severity
                        DiagnosticSeverity.Error
                        "Diagnostic policy must not suppress or demote an error."

            testCase "pre-cancelled compilation returns Source cancellation and no artifacts"
            <| fun _ ->
                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                use cancellation = new CancellationTokenSource()
                cancellation.Cancel()
                let result = Compiler().Compile(request, cancellation.Token)

                match result.Outcome with
                | CompilationOutcome.Cancelled observed ->
                    Expect.equal
                        observed.RequestIdentity
                        request.RequestIdentity
                        "Cancellation must preserve the request identity."

                    Expect.equal
                        observed.ObservedPhase
                        CompilationPhase.Source
                        "Pre-cancellation must stop at Source."
                | outcome -> failtestf "Expected a cancelled result, but received %A." outcome

                Expect.isEmpty
                    result.Artifacts
                    "A cancelled compilation must not contain an artifact."

            testCase "direct CLI service and MSBuild routes only through Compiler.Compile"
            <| fun _ ->
                let coreCalls = methodCalls typeof<Compiler>.Assembly.Location
                let testCalls = methodCalls (Assembly.GetExecutingAssembly().Location)

                Expect.isTrue
                    (hasCall
                        "fsharp2.Tests.CompilerContractTests"
                        "compileRequest"
                        "FSharp2.Compiler.Compiler"
                        "Compile"
                        testCalls)
                    "The direct test adapter must call Compiler.Compile."

                Expect.isTrue
                    (hasCall
                        "FSharp2.Compiler.CompilerHost"
                        "compileLocally"
                        "FSharp2.Compiler.Compiler"
                        "Compile"
                        coreCalls)
                    "The standalone CLI adapter must call Compiler.Compile."

                Expect.isTrue
                    (hasCall
                        "FSharp2.Compiler.ServiceHost"
                        "runServer"
                        "FSharp2.Compiler.Compiler"
                        "Compile"
                        coreCalls)
                    "The protocol v8 service adapter must call Compiler.Compile."

                Expect.isTrue
                    (hasCall
                        "FSharp2.Compiler.Compiler"
                        "Compile"
                        "FSharp2.Compiler.CompilationPipeline"
                        "compileRequest"
                        coreCalls)
                    "Compiler.Compile must own the internal pipeline entry."

                let adapterTypes =
                    set [
                        "FSharp2.Compiler.CompilerHost"
                        "FSharp2.Compiler.ServiceHost"
                    ]

                let forbiddenCallees =
                    set [
                        "FSharp2.Compiler.CompilerService", "Compile"
                        "FSharp2.Compiler.CompilationPipeline", "compile"
                        "FSharp2.Compiler.CompilationPipeline", "compileRequest"
                        "FSharp2.Compiler.Linker", "link"
                    ]

                let forbiddenCalls =
                    coreCalls
                    |> Array.filter (fun call ->
                        adapterTypes.Contains(call.CallerType)
                        && forbiddenCallees.Contains(call.CalleeType, call.CalleeMethod)
                    )

                Expect.isEmpty
                    forbiddenCalls
                    "No standalone or service adapter may bypass Compiler.Compile."

                let repositoryRoot =
                    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

                let targets =
                    File.ReadAllText(
                        Path.Combine(
                            repositoryRoot,
                            "src",
                            "FSharp2.Compiler.MSBuild",
                            "buildTransitive",
                            "FSharp2.Compiler.MSBuild.targets"
                        )
                    )

                let packageProject =
                    File.ReadAllText(
                        Path.Combine(
                            repositoryRoot,
                            "src",
                            "FSharp2.Compiler.MSBuild",
                            "FSharp2.Compiler.MSBuild.csproj"
                        )
                    )

                Expect.stringContains
                    targets
                    "tools/$(FSharp2CompilerHostRuntimeIdentifier)/fsc2.exe"
                    "The MSBuild target must select the packaged fsc2 adapter."

                Expect.stringContains
                    targets
                    "<FscToolExe>$([System.IO.Path]::GetFileName('$(FSharp2CompilerHostPath)'))</FscToolExe>"
                    "CoreCompile must invoke the selected packaged adapter."

                Expect.stringContains
                    packageProject
                    "PackagePath=\"tools/$(FSharp2CompilerHostRuntimeIdentifier)/fsc2.exe\""
                    "The package must place fsc2 at the MSBuild target path."
        ]
