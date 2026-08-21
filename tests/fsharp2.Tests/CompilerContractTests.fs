namespace fsharp2.Tests

open System
open Expecto
open FSharp2.Compiler

module CompilerContractTests =
    let private bytes (values: seq<byte>) =
        values
        |> Seq.toArray

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
        ]
