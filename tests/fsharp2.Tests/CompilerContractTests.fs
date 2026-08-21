namespace fsharp2.Tests

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Threading
open Expecto
open FSharp2.Compiler

module CompilerContractTests =
    let private bytes (values: seq<byte>) =
        values
        |> Seq.toArray

    let private fingerprint (values: byte array) =
        values
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let private compileWithOptimization optimization (sourceText: string) =
        let sourceBytes = Text.Encoding.UTF8.GetBytes(sourceText)
        let referenceImage = File.ReadAllBytes(Assembly.Load("System.Runtime").Location)

        let request =
            CompilationRequest.Create(
                CompilerContract.Version,
                StableIdentity.create "request:tracer",
                CompilationAssemblyIdentity.Create(
                    StableIdentity.create "assembly:Tracer",
                    "Tracer"
                ),
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
                SemanticOptions.Create([||], None, optimization, false, false, None),
                DiagnosticOptions.Create(None, [||], false, [||]),
                EmissionOptions.Create(true, false, DebugFormat.None, [||], [| "Tracer.fs" |], [||]),
                SigningOptions.Create(SigningMode.Unsigned, [||]),
                ResourceInputs.Create([||], [||]),
                [| RequestedArtifact.ImplementationAssembly |]
            )

        Compiler().Compile(request, CancellationToken.None)

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
        ]
