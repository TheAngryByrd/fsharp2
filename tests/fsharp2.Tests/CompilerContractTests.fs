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

    let private referenceTypeAssembly attributes namespaceName typeName =
        let metadata = MetadataBuilder()
        let firstField = MetadataTokens.FieldDefinitionHandle(1)
        let firstMethod = MetadataTokens.MethodDefinitionHandle(1)

        metadata.AddModule(
            0,
            metadata.GetOrAddString("SuggestionReference.dll"),
            metadata.GetOrAddGuid(Guid.NewGuid()),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        metadata.AddAssembly(
            metadata.GetOrAddString("SuggestionReference"),
            System.Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.None
        )
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString("<Module>"),
            Unchecked.defaultof<EntityHandle>,
            firstField,
            firstMethod
        )
        |> ignore

        metadata.AddTypeDefinition(
            attributes,
            metadata.GetOrAddString(namespaceName),
            metadata.GetOrAddString(typeName),
            Unchecked.defaultof<EntityHandle>,
            firstField,
            firstMethod
        )
        |> ignore

        let image = BlobBuilder()

        ManagedPEBuilder(
            PEHeaderBuilder.CreateLibraryHeader(),
            MetadataRootBuilder(metadata),
            BlobBuilder()
        )
            .Serialize(image)
        |> ignore

        image.ToArray()

    let rec private metadataTypeName (metadata: MetadataReader) (handle: EntityHandle) =
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

            let name = metadata.GetString(reference.Name)

            if reference.ResolutionScope.Kind = HandleKind.TypeReference then
                metadataTypeName metadata reference.ResolutionScope
                + "+"
                + name
            else
                fullName (metadata.GetString(reference.Namespace)) name
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

        let fsharpCoreImage =
            File.ReadAllBytes(typeof<Microsoft.FSharp.Core.EntryPointAttribute>.Assembly.Location)

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
                TargetReferenceSnapshot.Create(
                    StableIdentity.create "reference:FSharp.Core",
                    "FSharp.Core.dll",
                    fsharpCoreImage,
                    fingerprint fsharpCoreImage
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

    let private compileSourcesWithReferences
        (sources: (string * string * string) array)
        (additionalReferences: TargetReferenceSnapshot array)
        =
        let _, _, firstSourceText = Array.head sources

        let baseline =
            createRequest
                (defaultSemanticOptions ())
                (defaultDiagnosticOptions ())
                (EmissionOptions.Create(
                    true,
                    false,
                    DebugFormat.None,
                    [||],
                    sources
                    |> Array.map (fun (_, logicalPath, _) -> logicalPath),
                    [||]
                ))
                (defaultSigningOptions ())
                (emptyResources ())
                defaultRequestedArtifacts
                firstSourceText

        let source stableId logicalPath (text: string) =
            let content = Text.Encoding.UTF8.GetBytes(text)

            SourceSnapshot.Create(
                StableIdentity.create stableId,
                logicalPath,
                text,
                fingerprint content
            )

        CompilationRequest.Create(
            baseline.ContractVersion,
            baseline.RequestIdentity,
            baseline.AssemblyIdentity,
            sources
            |> Array.map (fun (stableId, logicalPath, text) -> source stableId logicalPath text),
            Array.append
                (baseline.TargetReferences
                 |> Seq.toArray)
                additionalReferences,
            baseline.SemanticOptions,
            baseline.DiagnosticOptions,
            baseline.EmissionOptions,
            baseline.SigningOptions,
            baseline.Resources,
            baseline.RequestedArtifacts
            |> Seq.toArray
        )
        |> compileRequest

    let private compileSources sources =
        compileSourcesWithReferences sources [||]

    let private snapshotOfFile stableId (path: string) forwardingImplementations =
        let image = File.ReadAllBytes(path)

        TargetReferenceSnapshot.Create(
            StableIdentity.create stableId,
            Path.GetFileName(path),
            image,
            fingerprint image,
            forwardingImplementations
        )

    let private systemRuntimeSnapshot forwardingImplementations =
        snapshotOfFile
            "reference:System.Runtime"
            (Assembly.Load("System.Runtime").Location)
            forwardingImplementations

    let private coreLibrarySnapshot () =
        snapshotOfFile
            "reference:System.Runtime/forwarding:System.Private.CoreLib.dll"
            typeof<obj>.Assembly.Location
            [||]

    let private fsharpCoreSnapshot () =
        snapshotOfFile
            "reference:FSharp.Core"
            typeof<Microsoft.FSharp.Core.EntryPointAttribute>.Assembly.Location
            [||]

    let private compileWithReferences (references: TargetReferenceSnapshot array) sourceText =
        let baseline =
            createRequest
                (defaultSemanticOptions ())
                (defaultDiagnosticOptions ())
                (defaultEmissionOptions ())
                (defaultSigningOptions ())
                (emptyResources ())
                defaultRequestedArtifacts
                sourceText

        CompilationRequest.Create(
            baseline.ContractVersion,
            baseline.RequestIdentity,
            baseline.AssemblyIdentity,
            baseline.Sources
            |> Seq.toArray,
            references,
            baseline.SemanticOptions,
            baseline.DiagnosticOptions,
            baseline.EmissionOptions,
            baseline.SigningOptions,
            baseline.Resources,
            baseline.RequestedArtifacts
            |> Seq.toArray
        )
        |> compileRequest

    let private emittedAssemblyReferences (result: CompilationResult) =
        let implementation =
            result.Artifacts
            |> Seq.find (fun artifact -> artifact.Kind = RequestedArtifact.ImplementationAssembly)

        use pe = new PEReader(implementation.Bytes)
        let metadata = pe.GetMetadataReader()

        metadata.AssemblyReferences
        |> Seq.map (fun handle -> metadata.GetString(metadata.GetAssemblyReference(handle).Name))
        |> Seq.sort
        |> List.ofSeq

    let private phaseOutputFingerprint phase (result: CompilationResult) =
        result.PhaseResults
        |> Seq.find (fun phaseResult -> phaseResult.Phase = phase)
        |> _.OutputFingerprint

    let private forwardedMemberSource =
        "module Tracer\nlet completed (source: System.Threading.Tasks.ValueTask) = source.IsCompletedSuccessfully\n"

    [<Tests>]
    let tests =
        testList "CompilerContract" [
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
                    let systemRuntimeReferencePath = Path.Combine(root, "System.Runtime.dll")
                    let fsharpCoreReferencePath = Path.Combine(root, "FSharp.Core.dll")
                    let sourceText = "module Tracer\nlet answer () = 42\n"

                    let systemRuntimeReferenceImage =
                        File.ReadAllBytes(Assembly.Load("System.Runtime").Location)

                    let fsharpCoreReferenceImage =
                        File.ReadAllBytes(typeof<int list>.Assembly.Location)

                    File.WriteAllText(sourcePath, sourceText)
                    File.WriteAllBytes(systemRuntimeReferencePath, systemRuntimeReferenceImage)
                    File.WriteAllBytes(fsharpCoreReferencePath, fsharpCoreReferenceImage)

                    let sourceSnapshot =
                        SourceSnapshot.Create(
                            StableIdentity.create "source:tracer",
                            "Tracer.fs",
                            File.ReadAllText(sourcePath),
                            fingerprint (Text.Encoding.UTF8.GetBytes(sourceText))
                        )

                    let systemRuntimeReferenceSnapshot =
                        let capturedImage = File.ReadAllBytes(systemRuntimeReferencePath)

                        TargetReferenceSnapshot.Create(
                            StableIdentity.create "reference:System.Runtime",
                            "System.Runtime.dll",
                            capturedImage,
                            fingerprint capturedImage
                        )

                    let fsharpCoreReferenceSnapshot =
                        let capturedImage = File.ReadAllBytes(fsharpCoreReferencePath)

                        TargetReferenceSnapshot.Create(
                            StableIdentity.create "reference:FSharp.Core",
                            "FSharp.Core.dll",
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
                            [|
                                systemRuntimeReferenceSnapshot
                                fsharpCoreReferenceSnapshot
                            |],
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
                    File.WriteAllBytes(systemRuntimeReferencePath, [| 0uy |])
                    File.WriteAllBytes(fsharpCoreReferencePath, [| 0uy |])

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

            testCase "entry-point methods preserve Oracle metadata attributes"
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
                    $"The entry-point fixture must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

                let implementation =
                    result.Artifacts
                    |> Seq.find (fun artifact ->
                        artifact.Kind = RequestedArtifact.ImplementationAssembly
                    )

                let emittedAssembly = Assembly.Load(bytes implementation.Bytes)

                let mainMethod =
                    emittedAssembly
                        .GetType("Program", true)
                        .GetMethod(
                            "main",
                            BindingFlags.Public
                            ||| BindingFlags.NonPublic
                            ||| BindingFlags.Static
                        )

                let entryPointAttributes =
                    mainMethod.CustomAttributes
                    |> Seq.filter (fun attribute ->
                        attribute.AttributeType.FullName = "Microsoft.FSharp.Core.EntryPointAttribute"
                    )
                    |> Seq.toList

                Expect.equal
                    entryPointAttributes.Length
                    1
                    "Program.main must have one Microsoft.FSharp.Core.EntryPointAttribute."

                let entryPointAttribute = List.exactlyOne entryPointAttributes

                Expect.isEmpty
                    entryPointAttribute.ConstructorArguments
                    "The entry-point attribute must use its zero-argument constructor."

            testCase "module types preserve Oracle compilation mapping metadata"
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
                    $"The module metadata fixture must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

                let implementation =
                    result.Artifacts
                    |> Seq.find (fun artifact ->
                        artifact.Kind = RequestedArtifact.ImplementationAssembly
                    )

                let emittedAssembly = Assembly.Load(bytes implementation.Bytes)
                let programType = emittedAssembly.GetType("Program", true)

                let compilationMappingAttributes =
                    programType.CustomAttributes
                    |> Seq.filter (fun attribute ->
                        attribute.AttributeType.FullName = "Microsoft.FSharp.Core.CompilationMappingAttribute"
                    )
                    |> Seq.toList

                Expect.equal
                    compilationMappingAttributes.Length
                    1
                    "Program must have one Microsoft.FSharp.Core.CompilationMappingAttribute."

                let compilationMappingAttribute = List.exactlyOne compilationMappingAttributes

                Expect.equal
                    compilationMappingAttribute.ConstructorArguments.Count
                    1
                    "The compilation mapping attribute must have one semantic constructor argument."

                let sourceConstruct = compilationMappingAttribute.ConstructorArguments.[0]

                Expect.equal
                    sourceConstruct.ArgumentType
                    typeof<Microsoft.FSharp.Core.SourceConstructFlags>
                    "The compilation mapping argument must use SourceConstructFlags."

                Expect.equal
                    (enum<Microsoft.FSharp.Core.SourceConstructFlags> (
                        Convert.ToInt32(sourceConstruct.Value)
                    ))
                    Microsoft.FSharp.Core.SourceConstructFlags.Module
                    "The compilation mapping attribute must identify a module construct."

            testCase "entry-point wildcard parameters use Oracle metadata names"
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
                    $"The wildcard parameter fixture must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

                let implementation =
                    result.Artifacts
                    |> Seq.find (fun artifact ->
                        artifact.Kind = RequestedArtifact.ImplementationAssembly
                    )

                let emittedAssembly = Assembly.Load(bytes implementation.Bytes)

                let mainMethod =
                    emittedAssembly
                        .GetType("Program", true)
                        .GetMethod(
                            "main",
                            BindingFlags.Public
                            ||| BindingFlags.NonPublic
                            ||| BindingFlags.Static
                        )

                let parameters = mainMethod.GetParameters()

                Expect.equal
                    parameters.Length
                    1
                    "Program.main must preserve its single source parameter."

                Expect.equal
                    parameters.[0].Name
                    "_arg1"
                    "The entry-point wildcard must use the Oracle metadata name."

            testCase "invalid warning directive arguments stop the compilation"
            <| fun _ ->
                let result = compile "module Program\n#nowarn \"abc\"\nlet answer () = 42\n"

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Failed
                    "The Compatibility Oracle reports FS0203 as an error"

                Expect.isEmpty result.Artifacts "A failed compilation publishes no artifacts"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code,
                         diagnostic.OriginalSeverity,
                         diagnostic.Stage,
                         diagnostic.Message,
                         diagnostic.Range
                         |> Option.map (fun range -> range.Start.Line, range.Start.Column)
                     ))
                    [
                        "FS0203",
                        DiagnosticSeverity.Error,
                        DiagnosticStage.Compilation CompilationPhase.Source,
                        "Invalid warning number 'abc'",
                        Some(2, 9)
                    ]
                    "The invalid argument is a source-phase error at the Oracle position"

            testCase "an outer clause bar between two match columns gets no layout FS0058"
            <| fun _ ->
                let result =
                    compile
                        "namespace N\n\nopen System\n\ntype H() =\n    static member Run(a: int, b: int) : int =\n        match a with\n        | x ->\n            match b with\n            | y -> x + y\n          | z -> z\n"

                Expect.isFalse
                    (result.Diagnostics
                     |> Seq.exists (fun diagnostic -> diagnostic.Code = "FS0058"))
                    "The Compatibility Oracle accepts the bar as a clause of the outer match"

                if result.Outcome = CompilationOutcome.Succeeded then
                    let run =
                        (Assembly.Load(bytes (Seq.exactlyOne result.Artifacts).Bytes))
                            .GetType("N.H", true)
                            .GetMethod("Run")

                    Expect.sequenceEqual
                        ([
                            0, 0
                            1, 2
                            2, 1
                         ]
                         |> List.map (fun (a, b) ->
                             run.Invoke(
                                 null,
                                 [|
                                     box a
                                     box b
                                 |]
                             )
                             :?> int
                         ))
                        [
                            0
                            3
                            3
                        ]
                        "The Compatibility Oracle program returns 0, 3, and 3"
                else
                    Expect.isEmpty result.Artifacts "A failed compilation publishes no artifacts"

            testList
                "a first clause left of its match after a with at a line end fails and emits no assembly"
                [
                    for name, source, line, column in
                        [
                            "inner with line, bar at the outer clause column",
                            "namespace N\n\nopen System\n\ntype H() =\n    static member Run(a: int, b: int) : int =\n        match a with\n        | x ->\n            match\n                b\n              with\n        | y -> y\n",
                            12,
                            9
                            "inner with at a line end, bar at the outer clause column",
                            "namespace N\n\ntype H() =\n    static member Run(a: int, b: int) : int =\n        match a with\n        | x -> match b with\n        | y -> y\n",
                            7,
                            9
                            "inner with at a line end, bar between the match columns",
                            "namespace N\n\ntype H() =\n    static member Run(a: int, b: int) : int =\n        match a with\n        | x -> match b with\n             | y -> y\n",
                            7,
                            14
                        ] ->
                        testCase name
                        <| fun _ ->
                            let result = compile source

                            Expect.notEqual
                                result.Outcome
                                CompilationOutcome.Succeeded
                                "The Compatibility Oracle rejects the clause bar with FS0058"

                            Expect.isEmpty
                                result.Artifacts
                                "A failed compilation publishes no artifacts"

                            Expect.equal
                                (result.Diagnostics
                                 |> Seq.map (fun diagnostic ->
                                     diagnostic.Code,
                                     diagnostic.Range
                                     |> Option.map (fun range ->
                                         range.Start.Line, range.Start.Column
                                     )
                                 )
                                 |> Seq.tryHead)
                                (Some("FS0058", Some(line, column)))
                                "The first diagnostic is FS0058 at the Compatibility Oracle position"
                ]

            testCase "a second with after a match fails and emits no assembly"
            <| fun _ ->
                let result =
                    compile
                        "namespace N\n\ntype H() =\n    static member Run(a: int, b: int) : int =\n        match\n            a\n          with\n          | _ -> 1\n          with _ -> 2\n"

                Expect.notEqual
                    result.Outcome
                    CompilationOutcome.Succeeded
                    "The Compatibility Oracle rejects the second with with FS0058 and FS0010 at (9,11)"

                Expect.isEmpty result.Artifacts "A failed compilation publishes no artifacts"

            testCase "a with line between the match and its input compiles with the Oracle results"
            <| fun _ ->
                let result =
                    compile
                        "namespace N\n\ntype H() =\n    static member Run(a: int, b: int) : int =\n        match\n            a\n          with\n        | _ -> 1\n"

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map (fun diagnostic -> $"{diagnostic.Code}: {diagnostic.Message}")
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"The Compatibility Oracle accepts the with line. Diagnostics:{Environment.NewLine}{diagnostics}"

                let assembly =
                    result.Artifacts
                    |> Seq.exactlyOne
                    |> fun artifact -> Assembly.Load(bytes artifact.Bytes)

                let run = assembly.GetType("N.H", true).GetMethod("Run")

                Expect.sequenceEqual
                    ([
                        0, 0
                        1, 2
                        2, 1
                     ]
                     |> List.map (fun (a, b) ->
                         run.Invoke(
                             null,
                             [|
                                 box a
                                 box b
                             |]
                         )
                         :?> int
                     ))
                    [
                        1
                        1
                        1
                    ]
                    "The Compatibility Oracle program returns 1 for each input"

            testCase "a nested if with a second else fails and emits no assembly"
            <| fun _ ->
                let result =
                    compile
                        "namespace N\n\ntype H() =\n    static member Run(a: bool, b: bool) : int =\n        if a then\n            if b then 1\n          else 3\n        else 4\n"

                Expect.notEqual
                    result.Outcome
                    CompilationOutcome.Succeeded
                    "The Compatibility Oracle rejects the second else with FS0010 at (8,9)"

                Expect.isEmpty result.Artifacts "A failed compilation publishes no artifacts"

            testCase "an else line right of the if column compiles with the Oracle results"
            <| fun _ ->
                let result =
                    compile
                        "namespace N\n\ntype H() =\n    static member Run(a: bool, b: bool) : int =\n        if a then\n            1\n           else 2\n"

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map (fun diagnostic -> $"{diagnostic.Code}: {diagnostic.Message}")
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"The Compatibility Oracle accepts the else line. Diagnostics:{Environment.NewLine}{diagnostics}"

                let assembly =
                    result.Artifacts
                    |> Seq.exactlyOne
                    |> fun artifact -> Assembly.Load(bytes artifact.Bytes)

                let run = assembly.GetType("N.H", true).GetMethod("Run")

                Expect.sequenceEqual
                    ([
                        true, true
                        true, false
                        false, true
                        false, false
                     ]
                     |> List.map (fun (a, b) ->
                         run.Invoke(
                             null,
                             [|
                                 box a
                                 box b
                             |]
                         )
                         :?> int
                     ))
                    [
                        1
                        1
                        2
                        2
                    ]
                    "The Compatibility Oracle program returns 1, 1, 2, and 2"

            testCase
                "an undented infix line is an unsupported syntax diagnostic, not a layout error"
            <| fun _ ->
                let result = compile "module Program\nlet f a b =\n      a\n    + b\n"

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Failed
                    "The prototype front end does not compile an infix line"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code,
                         diagnostic.Stage,
                         diagnostic.Message,
                         diagnostic.Range
                         |> Option.map (fun range -> range.Start.Line, range.Start.Column)
                     ))
                    [
                        "FSC2P1001",
                        DiagnosticStage.Compilation CompilationPhase.Syntax,
                        "unsupported token",
                        Some(4, 5)
                    ]
                    "The Compatibility Oracle accepts the line, so the layout reports no FS0058"

            testCase "lexical errors are reported in source order"
            <| fun _ ->
                let result = compile "module Program\nlet answer () = 12abc\n#nowarn \"abc\"\n"

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Failed
                    "Lexical errors stop the compilation"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code,
                         diagnostic.Range
                         |> Option.map (fun range -> range.Start.Line)
                     ))
                    [
                        "FS1156", Some 2
                        "FS0203", Some 3
                    ]
                    "The Compatibility Oracle reports both errors in source order"

            testList
                "a numeric literal outside its range or a reserved identifier is a lexical error"
                [
                    for text, code, message, position in
                        [
                            "module Program\nlet answer () = 2147483649\n",
                            "FS1147",
                            "This number is outside the allowable range for 32-bit signed integers",
                            (2, 17)
                            "module Program\nlet answer () = -0x123456789lf\n",
                            "FS1155",
                            "This number is outside the allowable range for 32-bit floats",
                            (2, 18)
                            "module Program\nlet answer x = x!\n",
                            "FS1141",
                            "Identifiers followed by '!' are reserved for future use",
                            (2, 16)
                        ] ->
                        testCase code
                        <| fun _ ->
                            let result = compile text

                            Expect.equal
                                result.Outcome
                                CompilationOutcome.Failed
                                "A lexical error stops the compilation"

                            Expect.sequenceEqual
                                (result.Diagnostics
                                 |> Seq.map (fun diagnostic ->
                                     diagnostic.Code,
                                     diagnostic.Message,
                                     diagnostic.Range
                                     |> Option.map (fun range ->
                                         range.Start.Line, range.Start.Column
                                     )
                                 ))
                                [ code, message, Some position ]
                                "The diagnostic matches the Compatibility Oracle"
                ]

            testList
                "a numeric literal that the prototype front end cannot read fails with a diagnostic, not an exception"
                [
                    for name, text, message in
                        [
                            "at the 32-bit limit (Oracle FS1147)",
                            "module Program\nlet answer () = 2147483648\n",
                            "integer literal outside the 32-bit range"
                            "with a 64-bit suffix (Oracle accepts)",
                            "module Program\nlet answer () = 2147483648L\n",
                            "numeric literal other than a decimal integer"
                            "with a radix prefix (Oracle accepts)",
                            "module Program\nlet answer () = 0x1\n",
                            "numeric literal other than a decimal integer"
                            "before member access",
                            "module Program\nlet answer () = 0x1.A\n",
                            "numeric literal other than a decimal integer"
                        ] ->
                        testCase name
                        <| fun _ ->
                            let result = compile text

                            Expect.equal
                                result.Outcome
                                CompilationOutcome.Failed
                                "The prototype front end reads only decimal 32-bit integer literals"

                            Expect.sequenceEqual
                                (result.Diagnostics
                                 |> Seq.map (fun diagnostic ->
                                     diagnostic.Code,
                                     diagnostic.Stage,
                                     diagnostic.Message,
                                     diagnostic.Range
                                     |> Option.map (fun range ->
                                         range.Start.Line, range.Start.Column
                                     )
                                 ))
                                [
                                    "FSC2P1001",
                                    DiagnosticStage.Compilation CompilationPhase.Syntax,
                                    message,
                                    Some(2, 17)
                                ]
                                "The literal gets an explicit prototype diagnostic"
                ]

            testCase "an invalid reference image fails with a diagnostic"
            <| fun _ ->
                let baseline =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Program\nlet answer () = 42\n"

                let invalidImage = [|
                    1uy
                    2uy
                    3uy
                |]

                let request =
                    CompilationRequest.Create(
                        baseline.ContractVersion,
                        baseline.RequestIdentity,
                        baseline.AssemblyIdentity,
                        baseline.Sources
                        |> Seq.toArray,
                        [|
                            yield! baseline.TargetReferences
                            TargetReferenceSnapshot.Create(
                                StableIdentity.create "reference:invalid",
                                "Invalid.dll",
                                invalidImage,
                                fingerprint invalidImage
                            )
                        |],
                        baseline.SemanticOptions,
                        baseline.DiagnosticOptions,
                        baseline.EmissionOptions,
                        baseline.SigningOptions,
                        baseline.Resources,
                        baseline.RequestedArtifacts
                        |> Seq.toArray
                    )

                let result = compileRequest request

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Failed
                    "The invalid reference stops the compilation"

                Expect.isEmpty result.Artifacts "A failed compilation publishes no artifacts"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code, diagnostic.LogicalPath, diagnostic.Stage
                     ))
                    [
                        "FSC2P1003",
                        Some "Invalid.dll",
                        DiagnosticStage.Compilation CompilationPhase.ResolvedSymbols
                    ]
                    "The invalid reference is named in a reference diagnostic"

                Expect.stringStarts
                    (Seq.exactlyOne result.Diagnostics).Message
                    "Error opening binary file 'Invalid.dll': "
                    "The message follows the Compatibility Oracle wording"

            testCase "reused parsing rebinds the physical source checksum"
            <| fun _ ->
                let sourceText = "module Program\nlet answer () = 42\n"

                let baseline =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            true,
                            false,
                            DebugFormat.Portable,
                            [||],
                            [| "Program.fs" |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        [|
                            RequestedArtifact.ImplementationAssembly
                            RequestedArtifact.PortablePdb
                        |]
                        sourceText

                let utf16Bytes = Text.Encoding.Unicode.GetBytes(sourceText)

                let reboundSource =
                    SourceSnapshot.Create(
                        baseline.Sources[0].StableId,
                        baseline.Sources[0].LogicalPath,
                        sourceText,
                        fingerprint utf16Bytes
                    )

                let rebound =
                    CompilationRequest.Create(
                        baseline.ContractVersion,
                        baseline.RequestIdentity,
                        baseline.AssemblyIdentity,
                        [| reboundSource |],
                        baseline.TargetReferences
                        |> Seq.toArray,
                        baseline.SemanticOptions,
                        baseline.DiagnosticOptions,
                        baseline.EmissionOptions,
                        baseline.SigningOptions,
                        baseline.Resources,
                        baseline.RequestedArtifacts
                        |> Seq.toArray
                    )

                let compiler = Compiler()
                let first = compiler.Compile(baseline, CancellationToken.None)
                let second = compiler.Compile(rebound, CancellationToken.None)

                Expect.equal
                    first.Outcome
                    CompilationOutcome.Succeeded
                    "The first compilation succeeds"

                Expect.equal
                    second.Outcome
                    CompilationOutcome.Succeeded
                    "The reused compilation succeeds"

                let portablePdb =
                    second.Artifacts
                    |> Seq.find (fun artifact -> artifact.Kind = RequestedArtifact.PortablePdb)

                use pdbStream = new MemoryStream(bytes portablePdb.Bytes, false)
                use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                let pdb = pdbProvider.GetMetadataReader()

                let document =
                    pdb.Documents
                    |> Seq.exactlyOne
                    |> pdb.GetDocument

                Expect.sequenceEqual
                    (pdb.GetBlobBytes(document.Hash))
                    (SHA256.HashData utf16Bytes)
                    "The reused PDB uses the current physical source checksum"

            testCase "portable PDB preserves Oracle FSharp import scopes"
            <| fun _ ->
                let sourceText =
                    """module Program

let answer () = 42

[<EntryPoint>]
let main _ =
    printfn "%d" (answer ())
    0
"""

                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            true,
                            false,
                            DebugFormat.Portable,
                            [||],
                            [| "Program.fs" |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        [|
                            RequestedArtifact.ImplementationAssembly
                            RequestedArtifact.PortablePdb
                        |]
                        sourceText

                let result = compileRequest request

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map (fun diagnostic -> diagnostic.Message)
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"The portable PDB fixture must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

                let implementation =
                    result.Artifacts
                    |> Seq.find (fun artifact ->
                        artifact.Kind = RequestedArtifact.ImplementationAssembly
                    )

                let portablePdb =
                    result.Artifacts
                    |> Seq.find (fun artifact -> artifact.Kind = RequestedArtifact.PortablePdb)

                use implementationStream = new MemoryStream(bytes implementation.Bytes, false)

                use pe = new PEReader(implementationStream)
                let metadata = pe.GetMetadataReader()

                use pdbStream = new MemoryStream(bytes portablePdb.Bytes, false)
                use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                let pdb = pdbProvider.GetMetadataReader()

                let imports importScopeHandle =
                    let importScope = pdb.GetImportScope(importScopeHandle)

                    let definitions =
                        importScope.GetImports()
                        |> Seq.toList

                    let namespaces =
                        definitions
                        |> List.choose (fun definition ->
                            if definition.Kind = ImportDefinitionKind.ImportNamespace then
                                definition.TargetNamespace
                                |> pdb.GetBlobBytes
                                |> Text.Encoding.UTF8.GetString
                                |> Some
                            else
                                None
                        )

                    let types =
                        definitions
                        |> List.choose (fun definition ->
                            if definition.Kind = ImportDefinitionKind.ImportType then
                                definition.TargetType
                                |> metadataTypeName metadata
                                |> Some
                            else
                                None
                        )

                    importScope, namespaces, types

                let orderedImports importScopeHandle =
                    let importScope = pdb.GetImportScope(importScopeHandle)

                    importScope.GetImports()
                    |> Seq.map (fun definition ->
                        match definition.Kind with
                        | ImportDefinitionKind.ImportNamespace ->
                            definition.TargetNamespace
                            |> pdb.GetBlobBytes
                            |> Text.Encoding.UTF8.GetString
                            |> sprintf "namespace:%s"
                        | ImportDefinitionKind.ImportType ->
                            definition.TargetType
                            |> metadataTypeName metadata
                            |> sprintf "type:%s"
                        | kind -> failtestf "Unsupported import definition kind %A." kind
                    )
                    |> Seq.toList

                let expectedNamespaces = [
                    "Microsoft"
                    "Microsoft.FSharp"
                    "Microsoft.FSharp.Core"
                    "Microsoft.FSharp.Collections"
                    "Microsoft.FSharp.Control"
                ]

                let expectedTypes = [
                    "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicOperators"
                    "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority"
                    "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPlusPriority"
                    "Microsoft.FSharp.Control.TaskBuilderExtensions.MediumPriority"
                    "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority"
                    "Microsoft.FSharp.Linq.QueryRunExtensions.LowPriority"
                    "Microsoft.FSharp.Linq.QueryRunExtensions.HighPriority"
                ]

                let expectedImportOrder = [
                    "namespace:Microsoft"
                    "namespace:Microsoft.FSharp"
                    "type:Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicOperators"
                    "namespace:Microsoft.FSharp.Core"
                    "namespace:Microsoft.FSharp.Collections"
                    "namespace:Microsoft.FSharp.Control"
                    "type:Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority"
                    "type:Microsoft.FSharp.Control.TaskBuilderExtensions.LowPlusPriority"
                    "type:Microsoft.FSharp.Control.TaskBuilderExtensions.MediumPriority"
                    "type:Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority"
                    "type:Microsoft.FSharp.Linq.QueryRunExtensions.LowPriority"
                    "type:Microsoft.FSharp.Linq.QueryRunExtensions.HighPriority"
                ]

                let importScopes =
                    pdb.ImportScopes
                    |> Seq.map imports
                    |> Seq.toList

                let rootScopes =
                    importScopes
                    |> List.filter (fun (scope, _, _) -> scope.Parent.IsNil)

                Expect.equal
                    rootScopes.Length
                    1
                    "The portable PDB must contain one semantic root import scope."

                let _, rootNamespaces, rootTypes = List.exactlyOne rootScopes

                Expect.sequenceEqual
                    rootNamespaces
                    []
                    "The root import scope must contain no namespace imports."

                Expect.sequenceEqual
                    rootTypes
                    []
                    "The root import scope must contain no type imports."

                let childScopes =
                    importScopes
                    |> List.filter (fun (scope, _, _) -> not scope.Parent.IsNil)

                Expect.equal
                    childScopes.Length
                    1
                    "The portable PDB must contain one semantic child import scope."

                let childScopeHandle =
                    pdb.ImportScopes
                    |> Seq.filter (fun handle ->
                        let scope = pdb.GetImportScope(handle)
                        not scope.Parent.IsNil
                    )
                    |> Seq.exactlyOne

                Expect.sequenceEqual
                    (orderedImports childScopeHandle)
                    expectedImportOrder
                    "The child import scope must preserve the Oracle import order."

                let childScope, childNamespaces, childTypes = List.exactlyOne childScopes

                let parentScope, parentNamespaces, parentTypes = imports childScope.Parent

                Expect.isTrue
                    parentScope.Parent.IsNil
                    "The child import scope parent must be the semantic root."

                Expect.sequenceEqual
                    parentNamespaces
                    []
                    "The child import scope parent must contain no namespaces."

                Expect.sequenceEqual
                    parentTypes
                    []
                    "The child import scope parent must contain no types."

                Expect.sequenceEqual
                    childNamespaces
                    expectedNamespaces
                    "The child import scope must preserve the Oracle namespace imports."

                Expect.sequenceEqual
                    childTypes
                    expectedTypes
                    "The child import scope must preserve the Oracle type imports."

                let methodScopes =
                    pdb.LocalScopes
                    |> Seq.map (fun handle ->
                        let localScope = pdb.GetLocalScope(handle)
                        let methodDefinition = metadata.GetMethodDefinition(localScope.Method)

                        let declaringType =
                            methodDefinition.GetDeclaringType()
                            |> typeDefinitionEntityHandle
                            |> metadataTypeName metadata

                        let methodName =
                            declaringType
                            + "."
                            + metadata.GetString(methodDefinition.Name)

                        Expect.isFalse
                            localScope.ImportScope.IsNil
                            $"{methodName} must have an import-scope association."

                        let methodImportScope, namespaces, types = imports localScope.ImportScope

                        Expect.isFalse
                            methodImportScope.Parent.IsNil
                            $"{methodName} must use the semantic child import scope."

                        let methodParent, methodParentNamespaces, methodParentTypes =
                            imports methodImportScope.Parent

                        methodName,
                        namespaces,
                        types,
                        methodParent.Parent.IsNil,
                        methodParentNamespaces,
                        methodParentTypes
                    )
                    |> Seq.toList

                Expect.sequenceEqual
                    (methodScopes
                     |> List.map (fun (methodName, _, _, _, _, _) -> methodName)
                     |> List.sort)
                    [
                        "Program.answer"
                        "Program.main"
                    ]
                    "Both emitted methods must have portable PDB local scopes."

                for methodName, namespaces, types, parentIsRoot, parentNamespaces, parentTypes in
                    methodScopes do
                    Expect.sequenceEqual
                        namespaces
                        expectedNamespaces
                        $"{methodName} must use the Oracle namespace imports."

                    Expect.sequenceEqual
                        types
                        expectedTypes
                        $"{methodName} must use the Oracle type imports."

                    Expect.isTrue
                        parentIsRoot
                        $"{methodName} imports must have the semantic root parent."

                    Expect.sequenceEqual
                        parentNamespaces
                        []
                        $"{methodName} import parent must contain no namespaces."

                    Expect.sequenceEqual
                        parentTypes
                        []
                        $"{methodName} import parent must contain no types."

            testCase "inferred generic top-level functions compile through the public contract"
            <| fun _ ->
                let sourceText =
                    """module Program

let identity value = value

[<EntryPoint>]
let main _ =
    let number = identity 42
    let text = identity "forty-two"
    printfn "%d,%s" number text
    0
"""

                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            true,
                            false,
                            DebugFormat.Portable,
                            [||],
                            [| "Program.fs" |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        [|
                            RequestedArtifact.ImplementationAssembly
                            RequestedArtifact.PortablePdb
                        |]
                        sourceText

                let result = compileRequest request

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map (fun diagnostic ->
                        let range =
                            diagnostic.Range
                            |> Option.map (fun range ->
                                $"{range.Start.Line}:{range.Start.Column}-{range.End.Line}:{range.End.Column}"
                            )
                            |> Option.defaultValue "no-range"

                        $"{diagnostic.Code} {range}: {diagnostic.Message}"
                    )
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"Inferred generic top-level functions must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

                let portablePdb =
                    result.Artifacts
                    |> Seq.find (fun artifact -> artifact.Kind = RequestedArtifact.PortablePdb)

                use pdbStream = new MemoryStream(bytes portablePdb.Bytes, false)
                use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                let pdb = pdbProvider.GetMetadataReader()

                let hasReturnSequencePoint =
                    pdb.MethodDebugInformation
                    |> Seq.collect (fun handle ->
                        pdb.GetMethodDebugInformation(handle).GetSequencePoints()
                    )
                    |> Seq.exists (fun point ->
                        not point.IsHidden
                        && point.StartLine = 10
                        && point.StartColumn = 5
                        && point.EndLine = 10
                        && point.EndColumn = 6
                    )

                Expect.isTrue
                    hasReturnSequencePoint
                    "The portable PDB must map the final return expression to its source range."

            testCase "inferred generic top-level function mismatches reach type checking"
            <| fun _ ->
                let sourceText =
                    """module Program

let identity value = value
let number: int = identity 42
let text: int = identity "forty-two"
"""

                let result = compile sourceText

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Failed
                    "The incompatible inferred application must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.equal
                    diagnostic.Code
                    "FS0001"
                    $"The diagnostic code must match the Oracle. Actual message: {diagnostic.Message}"

                Expect.equal
                    diagnostic.Message
                    (String.concat Environment.NewLine [
                        "This expression was expected to have type"
                        "    'int'    "
                        "but here has type"
                        "    'string'"
                    ])
                    "The type mismatch must match the Oracle."

                let range =
                    diagnostic.Range
                    |> Option.get

                Expect.equal range.Start.Line 5 "The mismatch must start on line 5."
                Expect.equal range.Start.Column 26 "The mismatch must start at the string literal."
                Expect.equal range.End.Line 5 "The mismatch must end on line 5."
                Expect.equal range.End.Column 37 "The mismatch must include the string literal."

            testCase "ordered source modules resolve through the public contract"
            <| fun _ ->
                let firstText =
                    """module First

let first = 42
"""

                let programText =
                    """module Program

[<EntryPoint>]
let main _ =
    printfn "%d" First.first
    0
"""

                let result =
                    compileSources [|
                        "source:first", "First.fs", firstText
                        "source:program", "Program.fs", programText
                    |]

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map (fun diagnostic -> diagnostic.Message)
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"Ordered source modules must compile. Diagnostics:{Environment.NewLine}{diagnostics}"

            testCase "later source modules are unresolved through the public contract"
            <| fun _ ->
                let programText =
                    """module Program

[<EntryPoint>]
let main _ =
    printfn "%d" First.first
    0
"""

                let firstText =
                    """module First

let first = 42
"""

                let baseline =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (EmissionOptions.Create(
                            true,
                            false,
                            DebugFormat.None,
                            [||],
                            [|
                                "Program.fs"
                                "First.fs"
                            |],
                            [||]
                        ))
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        programText

                let source stableId logicalPath (text: string) =
                    let content = Text.Encoding.UTF8.GetBytes(text)

                    SourceSnapshot.Create(
                        StableIdentity.create stableId,
                        logicalPath,
                        text,
                        fingerprint content
                    )

                let request =
                    CompilationRequest.Create(
                        baseline.ContractVersion,
                        baseline.RequestIdentity,
                        baseline.AssemblyIdentity,
                        [|
                            source "source:program" "Program.fs" programText
                            source "source:first" "First.fs" firstText
                        |],
                        baseline.TargetReferences
                        |> Seq.toArray,
                        baseline.SemanticOptions,
                        baseline.DiagnosticOptions,
                        baseline.EmissionOptions,
                        baseline.SigningOptions,
                        baseline.Resources,
                        baseline.RequestedArtifacts
                        |> Seq.toArray
                    )

                let result = compileRequest request
                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.equal
                    diagnostic.Code
                    "FS0039"
                    "The diagnostic code must match the Compatibility Oracle."

                Expect.equal
                    diagnostic.Message
                    "The value, namespace, type or module 'First' is not defined. Maybe you want one of the following:\u001d   fst\u001d   List\u001d   list"
                    "The diagnostic message must match the Compatibility Oracle."

                Expect.equal
                    diagnostic.LogicalPath
                    (Some "Program.fs")
                    "The diagnostic path must identify Program.fs."

                let range =
                    diagnostic.Range
                    |> Option.defaultWith (fun () ->
                        failtest "The diagnostic must have a source range."
                    )

                Expect.equal range.Start.Line 5 "The diagnostic must start on the qualified access."
                Expect.equal range.Start.Column 18 "The diagnostic must start at First."
                Expect.equal range.End.Line 5 "The diagnostic must end on the qualified access."
                Expect.equal range.End.Column 23 "The diagnostic must include First."

            testCase "later-module suggestions come from target references"
            <| fun _ ->
                let programText =
                    """module Program

[<EntryPoint>]
let main _ =
    printfn "%d" Prinntf.first
    0
"""

                let laterModuleText =
                    """module Prinntf

let first = 42
"""

                let result =
                    compileSources [|
                        "source:program", "Program.fs", programText
                        "source:prinntf", "Prinntf.fs", laterModuleText
                    |]

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.equal diagnostic.Code "FS0039" "The later module must be unresolved."

                Expect.stringContains
                    diagnostic.Message
                    "printf"
                    "Suggestions must include a nearby source name from the target references."

            testCase "later-module suggestions exclude inaccessible reference types"
            <| fun _ ->
                let programText =
                    """module Program

[<EntryPoint>]
let main _ =
    printfn "%d" HiddenTargte.first
    0
"""

                let laterModuleText =
                    """module HiddenTargte

let first = 42
"""

                let referenceImage =
                    referenceTypeAssembly TypeAttributes.NotPublic "Suggestion" "HiddenTargetModule"

                let result =
                    compileSourcesWithReferences [|
                        "source:program", "Program.fs", programText
                        "source:hidden-target", "HiddenTargte.fs", laterModuleText
                    |] [|
                        TargetReferenceSnapshot.Create(
                            StableIdentity.create "reference:suggestion",
                            "SuggestionReference.dll",
                            referenceImage,
                            fingerprint referenceImage
                        )
                    |]

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.equal diagnostic.Code "FS0039" "The later module must be unresolved."

                Expect.isFalse
                    (diagnostic.Message.Contains("\u001d   HiddenTarget", StringComparison.Ordinal))
                    "Suggestions must exclude inaccessible reference types."

            testCase "bound receivers are not treated as later source modules"
            <| fun _ ->
                let programText =
                    """module Program

[<EntryPoint>]
let main First =
    First.Length
"""

                let laterModuleText =
                    """module First

let first = 42
"""

                let result =
                    compileSources [|
                        "source:program", "Program.fs", programText
                        "source:first", "First.fs", laterModuleText
                    |]

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.notEqual
                    diagnostic.Code
                    "FS0039"
                    "A bound receiver must not be diagnosed as a later source module."

                Expect.stringContains
                    diagnostic.Message
                    "no readable field or property 'Length'"
                    "Typing must continue with the bound receiver."

            testCase "preceding values are not treated as later source modules"
            <| fun _ ->
                let programText =
                    """module Program

let First = "value"

[<EntryPoint>]
let main _ =
    printfn "%d" First.Length
    0
"""

                let laterModuleText =
                    """module First

let first = 42
"""

                let result =
                    compileSources [|
                        "source:program", "Program.fs", programText
                        "source:first", "First.fs", laterModuleText
                    |]

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.notEqual
                    diagnostic.Code
                    "FS0039"
                    "A preceding value must not be diagnosed as a later source module."

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

            testCase "supported language versions complete the source phase"
            <| fun _ ->
                for mode in
                    [
                        "preview"
                        "default"
                        "latest"
                        "latestmajor"
                        "4.6"
                        "4.7"
                        "5.0"
                        "6.0"
                        "7.0"
                        "8.0"
                        "9.0"
                        "10.0"
                    ] do
                    let request =
                        createRequest
                            (SemanticOptions.Create(
                                [||],
                                Some mode,
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

                    let result = compileRequest request
                    Expect.equal result.Outcome CompilationOutcome.Succeeded $"Language mode {mode}"

                    Expect.equal
                        (result.PhaseResults
                         |> Seq.find (fun phase -> phase.Phase = CompilationPhase.Source)
                         |> _.Status)
                        PhaseStatus.Completed
                        $"Source phase {mode}"

            testCase "unknown language version stops at Syntax"
            <| fun _ ->
                let request =
                    createRequest
                        (SemanticOptions.Create(
                            [||],
                            Some "11.0",
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
                    "semantic.language-version=11.0"

            testCase "signature source inputs stop at Syntax as unsupported"
            <| fun _ ->
                let result =
                    compileSources [|
                        "source:public-api-signature",
                        "/src/PublicApi.fsi",
                        "namespace SignatureContract\n\nmodule PublicApi =\n    val answer: unit -> int\n"
                        "source:public-api-implementation",
                        "/src/PublicApi.fs",
                        "namespace SignatureContract\n\nmodule PublicApi =\n    let answer () = \"text\"\n"
                    |]

                match result.Outcome with
                | CompilationOutcome.Unsupported failure ->
                    Expect.equal failure.Code "FSC2C2004" "The failure code must be stable."

                    Expect.equal
                        failure.StoppingPhase
                        CompilationPhase.Syntax
                        "Signature input must stop at Syntax."

                    Expect.equal
                        failure.UnsupportedValueIdentity
                        "source.signature=/src/PublicApi.fsi"
                        "The signature source identity must be stable."
                | outcome -> failtestf "Expected an unsupported result, but received %A." outcome

                let phase phase =
                    result.PhaseResults
                    |> Seq.find (fun phaseResult -> phaseResult.Phase = phase)

                let sourcePhase = phase CompilationPhase.Source
                let syntaxPhase = phase CompilationPhase.Syntax

                Expect.equal
                    sourcePhase.Status
                    PhaseStatus.Completed
                    "Source normalization must complete before signature classification."

                Expect.isSome
                    sourcePhase.InputFingerprint
                    "The completed Source phase must record its input."

                Expect.isSome
                    sourcePhase.OutputFingerprint
                    "The completed Source phase must record its output."

                Expect.equal
                    syntaxPhase.Status
                    PhaseStatus.Unsupported
                    "Syntax must record the unsupported signature boundary."

                Expect.equal
                    syntaxPhase.InputFingerprint
                    sourcePhase.OutputFingerprint
                    "Syntax must consume the normalized Source output."

                Expect.isNone
                    syntaxPhase.OutputFingerprint
                    "Unsupported Syntax must not publish output."

                result.PhaseResults
                |> Seq.filter (fun phaseResult ->
                    phaseResult.Phase
                    <> CompilationPhase.Source
                    && phaseResult.Phase
                       <> CompilationPhase.Syntax
                )
                |> Seq.iter (fun phaseResult ->
                    Expect.equal
                        phaseResult.Status
                        PhaseStatus.NotStarted
                        $"{phaseResult.Phase} must not run after unsupported Syntax."

                    Expect.isNone
                        phaseResult.InputFingerprint
                        $"{phaseResult.Phase} must not record an input."

                    Expect.isNone
                        phaseResult.OutputFingerprint
                        $"{phaseResult.Phase} must not record an output."
                )

                Expect.isEmpty
                    result.Diagnostics
                    "Unsupported signature input must be diagnostic-free."

                Expect.isEmpty
                    result.Artifacts
                    "Unsupported signature input must publish no artifacts."

                Expect.isEmpty
                    result.Fingerprints
                    "Unsupported signature input must publish no artifact fingerprints."

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

            testCase "Compiler Contract.diagnostic v2 preserves every occurrence and related fact"
            <| fun _ ->
                Expect.equal
                    CompilerContract.Version
                    2
                    "The diagnostic contract must increment exactly once."

                let diagnosticProperties =
                    typeof<CompilationDiagnostic>.GetProperties()
                    |> Array.map _.Name
                    |> Set.ofArray

                for propertyName in
                    [
                        "Occurrence"
                        "Code"
                        "NumericCode"
                        "Subcategory"
                        "Stage"
                        "OriginalSeverity"
                        "EffectiveSeverity"
                        "Disposition"
                        "Suppression"
                        "Message"
                        "LogicalPath"
                        "Range"
                        "RelatedInformation"
                        "Suggestions"
                        "Stream"
                    ] do
                    Expect.isTrue
                        (diagnosticProperties.Contains(propertyName))
                        $"CompilationDiagnostic must expose {propertyName}."

                Expect.isFalse
                    (diagnosticProperties.Contains("Severity"))
                    "The v2 contract must not retain the ambiguous v1 severity field."

                let result = compile "module Tracer\nlet answer: int = \"text\"\n"

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.equal diagnostic.Occurrence 0L "Occurrence order must be stable."
                Expect.equal diagnostic.Code "FS0001" "The code must be preserved."
                Expect.equal diagnostic.NumericCode 1 "The numeric code must be preserved."
                Expect.isNone diagnostic.Subcategory "The current diagnostic has no subcategory."

                Expect.equal
                    diagnostic.Stage
                    (DiagnosticStage.Compilation CompilationPhase.TypedDeclarations)
                    "The diagnostic stage must identify the failing phase."

                Expect.equal
                    diagnostic.OriginalSeverity
                    DiagnosticSeverity.Error
                    "Original severity must be preserved."

                Expect.equal
                    diagnostic.EffectiveSeverity
                    DiagnosticSeverity.Error
                    "Effective severity must be evaluated."

                Expect.equal
                    diagnostic.Disposition
                    DiagnosticDisposition.Emitted
                    "The error must be emitted."

                Expect.isNone
                    diagnostic.Suppression
                    "An emitted error must not have a suppression reason."

                Expect.equal
                    diagnostic.Stream
                    (Some DiagnosticStream.StandardError)
                    "The error must select standard error."

                Expect.isEmpty
                    diagnostic.RelatedInformation
                    "The empty related facts must be retained."

                Expect.isEmpty diagnostic.Suggestions "The empty suggestions must be retained."

                let relatedInformation = [|
                    DiagnosticRelatedInformation.Create(
                        "The declaration is here.",
                        Some "Library.fs",
                        None
                    )
                |]

                let suggestions = [| "Use a compatible value." |]

                let warning =
                    CompilationDiagnostic.Create(
                        4L,
                        "FS0020",
                        20,
                        Some "typecheck",
                        DiagnosticStage.Compilation CompilationPhase.TypedDeclarations,
                        DiagnosticSeverity.Warning,
                        DiagnosticSeverity.Error,
                        DiagnosticDisposition.Emitted,
                        None,
                        "The result is ignored.",
                        Some "Tracer.fs",
                        None,
                        relatedInformation,
                        suggestions,
                        Some DiagnosticStream.StandardError
                    )

                relatedInformation[0] <-
                    DiagnosticRelatedInformation.Create("Changed.", Some "Changed.fs", None)

                suggestions[0] <- "Changed."

                Expect.equal
                    warning.RelatedInformation[0].Message
                    "The declaration is here."
                    "Related information must be defensively copied."

                Expect.equal
                    warning.Suggestions[0]
                    "Use a compatible value."
                    "Suggestions must be defensively copied."

                let disabledWarnings = [| "FS0020" |]
                let enabledWarnings = [| "FS1182" |]
                let warningsAsErrors = [| "FS0044" |]
                let warningsNotAsErrors = [| "FS0025" |]

                let localWarningDirectives = [|
                    LocalWarningDirective.Create(
                        3L,
                        LocalWarningDirectiveAction.Disable,
                        "FS0020",
                        "Tracer.fs",
                        None
                    )
                |]

                let completeOptions =
                    DiagnosticOptions.Create(
                        Some 4,
                        disabledWarnings,
                        enabledWarnings,
                        true,
                        warningsAsErrors,
                        warningsNotAsErrors,
                        Some 100,
                        true,
                        Some "fr-FR",
                        localWarningDirectives,
                        true,
                        true,
                        true,
                        DiagnosticStyle.Gcc,
                        ConsoleColorMode.Disabled,
                        Some 1036,
                        Some "fr",
                        true,
                        true,
                        true
                    )

                disabledWarnings[0] <- "FS9999"
                enabledWarnings[0] <- "FS9999"
                warningsAsErrors[0] <- "FS9999"
                warningsNotAsErrors[0] <- "FS9999"

                localWarningDirectives[0] <-
                    LocalWarningDirective.Create(
                        9L,
                        LocalWarningDirectiveAction.Enable,
                        "FS9999",
                        "Changed.fs",
                        None
                    )

                Expect.sequenceEqual
                    completeOptions.DisabledWarnings
                    [ "FS0020" ]
                    "NoWarn must be copied."

                Expect.sequenceEqual
                    completeOptions.EnabledWarnings
                    [ "FS1182" ]
                    "WarnOn must be copied."

                Expect.sequenceEqual
                    completeOptions.WarningsAsErrors
                    [ "FS0044" ]
                    "WarnAsError must be copied."

                Expect.sequenceEqual
                    completeOptions.WarningsNotAsErrors
                    [ "FS0025" ]
                    "WarnAsWarn must be copied."

                Expect.equal
                    completeOptions.LocalWarningDirectives[0].Order
                    3L
                    "Local directives must be copied."

                Expect.equal
                    completeOptions.WarningLevel
                    (Some 4)
                    "Warning level must be normalized."

                Expect.isTrue completeOptions.TreatWarningsAsErrors "WarnAsError must be retained."

                Expect.equal
                    completeOptions.MaximumErrors
                    (Some 100)
                    "Maximum errors must be retained."

                Expect.isTrue completeOptions.AbortOnError "Abort-on-error must be retained."

                Expect.equal
                    completeOptions.PreferredUICulture
                    (Some "fr-FR")
                    "The normalized UI culture must be retained."

                Expect.isTrue completeOptions.FullPaths "Full-path output must be retained."
                Expect.isTrue completeOptions.FlatErrors "Flat-message output must be retained."
                Expect.isTrue completeOptions.Utf8Output "UTF-8 output must be retained."

                Expect.equal
                    completeOptions.DiagnosticStyle
                    DiagnosticStyle.Gcc
                    "GCC style must be retained."

                Expect.equal
                    completeOptions.ConsoleColorMode
                    ConsoleColorMode.Disabled
                    "Console color mode must be retained."

                Expect.equal completeOptions.LCID (Some 1036) "The LCID input must be retained."

                Expect.equal
                    completeOptions.PreferredUILanguage
                    (Some "fr")
                    "The preferred UI language input must be retained."

                Expect.isTrue
                    completeOptions.TestParserErrorRecovery
                    "Parser recovery input must be retained."

                Expect.isTrue
                    completeOptions.StandardOutputRedirected
                    "Standard-output redirection must be retained."

                Expect.isTrue
                    completeOptions.StandardErrorRedirected
                    "Standard-error redirection must be retained."

                let errorOccurrence =
                    CompilationDiagnostic.Create(
                        5L,
                        "FS0001",
                        1,
                        None,
                        DiagnosticStage.Compilation CompilationPhase.TypedDeclarations,
                        DiagnosticSeverity.Error,
                        DiagnosticSeverity.Error,
                        DiagnosticDisposition.Emitted,
                        None,
                        "The types do not match.",
                        Some "Tracer.fs",
                        None,
                        [||],
                        [||],
                        Some DiagnosticStream.StandardError
                    )

                let policyType =
                    typeof<Compiler>.Assembly.GetType("FSharp2.Compiler.DiagnosticPolicy", true)

                let applyPolicy =
                    policyType.GetMethod(
                        "apply",
                        BindingFlags.Static
                        ||| BindingFlags.Public
                        ||| BindingFlags.NonPublic
                    )

                Expect.isNotNull applyPolicy "Diagnostic policy evaluation must exist."

                let occurrences: seq<CompilationDiagnostic> = [
                    warning
                    errorOccurrence
                ]

                let evaluated =
                    applyPolicy.Invoke(
                        null,
                        [|
                            box (DiagnosticOptions.Create(Some 0, [||], false, [||]))
                            box occurrences
                        |]
                    )
                    :?> Collections.Immutable.ImmutableArray<CompilationDiagnostic>

                Expect.sequenceEqual
                    (evaluated
                     |> Seq.map _.Occurrence)
                    [
                        4L
                        5L
                    ]
                    "Evaluation must retain every occurrence in stable order."

                Expect.equal
                    evaluated[0].Disposition
                    DiagnosticDisposition.Suppressed
                    "Warning level zero must retain a suppressed occurrence."

                Expect.equal
                    evaluated[0].EffectiveSeverity
                    DiagnosticSeverity.Hidden
                    "A suppressed occurrence must have Hidden effective severity."

                Expect.equal
                    evaluated[0].Suppression
                    (Some DiagnosticSuppression.WarningLevel)
                    "The suppression reason must be retained."

                Expect.isNone
                    evaluated[0].Stream
                    "A suppressed occurrence must not select a stream."

                Expect.equal
                    evaluated[0].RelatedInformation[0].Message
                    "The declaration is here."
                    "Evaluation must preserve related facts."

                Expect.equal
                    evaluated[0].Suggestions[0]
                    "Use a compatible value."
                    "Evaluation must preserve suggestions."

                Expect.equal
                    evaluated[1].Disposition
                    DiagnosticDisposition.Emitted
                    "The error occurrence must remain emitted."

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
                        diagnostic.EffectiveSeverity
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
                    "tools/$(FSharp2CompilerHostRuntimeIdentifier)/$(FSharp2CompilerHostExecutableName)"
                    "The MSBuild target must select the packaged fsc2 adapter."

                Expect.stringContains
                    targets
                    "StartsWith('win'))\">fsc2.exe</FSharp2CompilerHostExecutableName>"
                    "The Windows packaged adapter must be fsc2.exe."

                Expect.stringContains
                    targets
                    "<FSharp2CompilerHostExecutableName Condition=\"'$(FSharp2CompilerHostExecutableName)' == ''\">fsc2</FSharp2CompilerHostExecutableName>"
                    "The non-Windows packaged adapter must be fsc2."

                Expect.stringContains
                    targets
                    "<FscToolExe>$([System.IO.Path]::GetFileName('$(FSharp2CompilerHostPath)'))</FscToolExe>"
                    "CoreCompile must invoke the selected packaged adapter."

                Expect.stringContains
                    packageProject
                    "Include=\"$(FSharp2CompilerHostPublishDirectory)$(FSharp2CompilerHostExecutableName)\""
                    "The package must pack the published fsc2 adapter."

                Expect.stringContains
                    packageProject
                    "PackagePath=\"tools/$(FSharp2CompilerHostRuntimeIdentifier)\""
                    "The package must place fsc2 at the MSBuild target path."

            testCase "forwarded types resolve through a reference's forwarding implementation"
            <| fun _ ->
                let result =
                    compileWithReferences
                        [|
                            systemRuntimeSnapshot [| coreLibrarySnapshot () |]
                            fsharpCoreSnapshot ()
                        |]
                        forwardedMemberSource

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map _.Message
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"The forwarded member must resolve. Diagnostics:{Environment.NewLine}{diagnostics}"

                Expect.equal
                    (emittedAssemblyReferences result)
                    [
                        "FSharp.Core"
                        "System.Runtime"
                    ]
                    "The forwarding implementation must not become an emitted assembly reference."

            testCase
                "a facade without its forwarding implementation keeps the resolution diagnostic"
            <| fun _ ->
                let result =
                    compileWithReferences
                        [|
                            systemRuntimeSnapshot [||]
                            fsharpCoreSnapshot ()
                        |]
                        forwardedMemberSource

                Expect.equal result.Outcome CompilationOutcome.Failed "Compilation must fail."

                let diagnostic =
                    result.Diagnostics
                    |> Seq.exactlyOne

                Expect.stringContains
                    diagnostic.Message
                    "has no readable field or property 'IsCompletedSuccessfully'"
                    "The forwarded member must stay unresolved without the implementation image."

                Expect.isEmpty result.Artifacts "A failed compilation must not contain an artifact."

            testCase "a changed forwarding implementation changes the resolved-symbols fingerprint"
            <| fun _ ->
                let compileWithImplementationFingerprint contentFingerprint =
                    let implementation = coreLibrarySnapshot ()

                    compileWithReferences
                        [|
                            systemRuntimeSnapshot [|
                                TargetReferenceSnapshot.Create(
                                    implementation.StableId,
                                    implementation.LogicalPath,
                                    bytes implementation.PeImage,
                                    contentFingerprint
                                )
                            |]
                            fsharpCoreSnapshot ()
                        |]
                        forwardedMemberSource

                let first = compileWithImplementationFingerprint "implementation:first"
                let again = compileWithImplementationFingerprint "implementation:first"
                let changed = compileWithImplementationFingerprint "implementation:second"

                for result in
                    [
                        first
                        again
                        changed
                    ] do
                    Expect.equal
                        result.Outcome
                        CompilationOutcome.Succeeded
                        "Compilation must succeed."

                let resolved = phaseOutputFingerprint CompilationPhase.ResolvedSymbols

                Expect.equal
                    (resolved again)
                    (resolved first)
                    "The same forwarding implementation must give the same fingerprint."

                Expect.notEqual
                    (resolved changed)
                    (resolved first)
                    "A changed forwarding implementation must change the fingerprint."

            testCase "a request without an FSharp.Core reference links to FSharp.Core"
            <| fun _ ->
                let result =
                    compileWithReferences
                        [| systemRuntimeSnapshot [| coreLibrarySnapshot () |] |]
                        "module Tracer\nlet answer () = 42\n"

                let diagnostics =
                    result.Diagnostics
                    |> Seq.map _.Message
                    |> String.concat Environment.NewLine

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Succeeded
                    $"The module must compile without an explicit FSharp.Core reference. Diagnostics:{Environment.NewLine}{diagnostics}"

                Expect.equal
                    (emittedAssemblyReferences result)
                    [
                        "FSharp.Core"
                        "System.Runtime"
                    ]
                    "The compilation mapping attributes must reference FSharp.Core."

            testCase "a forwarding implementation without a logical path fails request validation"
            <| fun _ ->
                let broken = {
                    coreLibrarySnapshot () with
                        LogicalPath = String.Empty
                }

                let request =
                    createRequest
                        (defaultSemanticOptions ())
                        (defaultDiagnosticOptions ())
                        (defaultEmissionOptions ())
                        (defaultSigningOptions ())
                        (emptyResources ())
                        defaultRequestedArtifacts
                        "module Tracer\nlet answer () = 42\n"

                let error =
                    try
                        CompilationRequest.Create(
                            request.ContractVersion,
                            request.RequestIdentity,
                            request.AssemblyIdentity,
                            request.Sources
                            |> Seq.toArray,
                            [|
                                systemRuntimeSnapshot [| broken |]
                                fsharpCoreSnapshot ()
                            |],
                            request.SemanticOptions,
                            request.DiagnosticOptions,
                            request.EmissionOptions,
                            request.SigningOptions,
                            request.Resources,
                            request.RequestedArtifacts
                            |> Seq.toArray
                        )
                        |> ignore

                        None
                    with :? ArgumentException as error ->
                        Some error.Message

                match error with
                | None -> failtest "A nested snapshot without a logical path is invalid."
                | Some message ->
                    Expect.stringContains
                        message
                        "targetReferences.ForwardingImplementations.LogicalPath"
                        "The validation error must name the nested snapshot field."
        ]
