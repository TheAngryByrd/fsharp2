namespace FSharp2.Compiler

open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text

type internal CoreCompilation = {
    Query: CompilerQueryResult
    SymbolicAssembly: SymbolicAssembly
    Artifacts: LinkedArtifacts
    LinkElapsedMicroseconds: int64
} with

    override _.ToString() = "CoreCompilation"

/// The host-independent compile/link/publish path shared by standalone and
/// persistent-service execution.
module internal CompilationPipeline =
    let private elapsedMicroseconds started =
        Stopwatch.GetElapsedTime(started).Ticks
        / 10L

    let private fingerprint (bytes: byte array) =
        bytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let private signingMode =
        function
        | StrongNameMode.Unsigned -> SigningMode.Unsigned
        | StrongNameMode.DelaySign -> SigningMode.DelaySign
        | StrongNameMode.PublicSign -> SigningMode.PublicSign
        | StrongNameMode.FullSign -> SigningMode.FullSign

    let createRequest (invocation: CompilerInvocation) (sources: SourceInput list) =
        let assemblyName = Path.GetFileNameWithoutExtension(invocation.AssemblyPath)

        let sourceSnapshots =
            sources
            |> List.mapi (fun index source ->
                let content = Encoding.UTF8.GetBytes(source.Text)

                SourceSnapshot.Create(
                    StableIdentity.create $"source:{index}:{Path.GetFileName(source.Path)}",
                    source.Path,
                    source.Text,
                    fingerprint content
                )
            )
            |> List.toArray

        let targetReferences =
            invocation.ReferencePaths
            |> List.mapi (fun index path ->
                let image = File.ReadAllBytes(path)

                TargetReferenceSnapshot.Create(
                    StableIdentity.create $"reference:{index}:{Path.GetFileName(path)}",
                    Path.GetFileName(path),
                    image,
                    fingerprint image
                )
            )
            |> List.toArray

        let managedResources =
            invocation.ManagedResource
            |> Option.map (fun resource ->
                ManagedResourceSnapshot.Create(
                    StableIdentity.create "resource:managed:0",
                    resource.LogicalName,
                    (if resource.IsPublic then
                         ResourceVisibility.Public
                     else
                         ResourceVisibility.Private),
                    resource.Data,
                    fingerprint resource.Data
                )
            )
            |> Option.toArray

        let nativeResources =
            if invocation.NativeResourceData.Length = 0 then
                [||]
            else
                [|
                    NativeResourceSnapshot.Create(
                        StableIdentity.create "resource:native:0",
                        invocation.NativeResourceData,
                        fingerprint invocation.NativeResourceData
                    )
                |]

        let requestedArtifacts = [|
            RequestedArtifact.ImplementationAssembly

            if invocation.PortablePdb then
                RequestedArtifact.PortablePdb

            if invocation.ReferenceAssemblyPath.IsSome then
                RequestedArtifact.ReferenceAssembly

            if invocation.DocumentationPath.IsSome then
                RequestedArtifact.Documentation
        |]

        CompilationRequest.Create(
            CompilerContract.Version,
            StableIdentity.create $"request:{Guid.NewGuid():N}",
            CompilationAssemblyIdentity.Create(
                StableIdentity.create $"assembly:{assemblyName}",
                assemblyName
            ),
            sourceSnapshots,
            targetReferences,
            SemanticOptions.Create(
                List.toArray invocation.Defines,
                invocation.LanguageVersion,
                (if invocation.Optimize then
                     OptimizationMode.Enabled
                 else
                     OptimizationMode.Disabled),
                invocation.CheckNulls,
                invocation.NoFramework,
                invocation.TargetProfile
            ),
            DiagnosticOptions.Create(
                invocation.WarningLevel,
                List.toArray invocation.DisabledWarnings,
                invocation.TreatWarningsAsErrors,
                List.toArray invocation.WarningsAsErrors
            ),
            EmissionOptions.Create(
                invocation.Deterministic,
                invocation.HighEntropyVA,
                (if invocation.PortablePdb then
                     DebugFormat.Portable
                 else
                     DebugFormat.None),
                [||],
                List.toArray invocation.DebugDocumentPaths,
                invocation.SourceLinkJson
            ),
            SigningOptions.Create(signingMode invocation.StrongNameMode, invocation.StrongNameKey),
            ResourceInputs.Create(managedResources, nativeResources),
            requestedArtifacts
        )

    let private diagnostic code message = {
        Code = code
        Message = message
        Path = None
        Range = None
    }

    let compileRequest (service: CompilerService) (request: CompilationRequest) =
        match ReferenceTypeIndex.Create(request.TargetReferences) with
        | Error message -> Error(diagnostic "FSC2P1001" message)
        | Ok references ->
            let sources =
                request.Sources
                |> Seq.map (fun source -> {
                    Path = source.LogicalPath
                    Text = source.Text
                })
                |> List.ofSeq

            match
                service.Compile(
                    request.AssemblyIdentity.Name,
                    List.ofSeq request.SemanticOptions.Defines,
                    references,
                    sources
                )
            with
            | Error compilerDiagnostic -> Error compilerDiagnostic
            | Ok query ->
                match request.SemanticOptions.Optimization with
                | OptimizationMode.Enabled ->
                    Error(
                        diagnostic
                            "FSC2C2002"
                            "Enabled optimization is not supported by this compiler contract."
                    )
                | OptimizationMode.Disabled ->
                    try
                        let symbolic = SymbolicEmission.emit query.LoweredCompilation
                        let linkStarted = Stopwatch.GetTimestamp()
                        let artifacts = Linker.link request symbolic

                        Ok {
                            Query = query
                            SymbolicAssembly = symbolic
                            Artifacts = artifacts
                            LinkElapsedMicroseconds = elapsedMicroseconds linkStarted
                        }
                    with ex ->
                        Error(diagnostic "FSC2P9999" ex.Message)

    let private failure started message = {
        ExitCode = 1
        Error = message
        ServiceProcessId = Environment.ProcessId
        QuerySchema = CompilerSchema.Query
        NodeKind = "source"
        ContentFingerprint = String.Empty
        PreviousContentFingerprint = String.Empty
        InvalidationReason = "bypass"
        ParseKey = String.Empty
        CheckKey = String.Empty
        LowerKey = String.Empty
        DependencyCount = 0
        ParseDecision = "bypass"
        CheckDecision = "bypass"
        LowerDecision = "bypass"
        ParseElapsedMicroseconds = 0L
        CheckElapsedMicroseconds = 0L
        LowerElapsedMicroseconds = 0L
        LinkElapsedMicroseconds = 0L
        PublishElapsedMicroseconds = 0L
        CompileElapsedMicroseconds = elapsedMicroseconds started
        ExportFingerprint = String.Empty
        FragmentHash = String.Empty
        Emitted = false
    }

    let fragmentHash (symbolic: SymbolicAssembly) =
        match
            [
                yield!
                    symbolic.AssemblyAttributes
                    |> List.map _.ContentHash

                yield!
                    symbolic.Module.TypeAbbreviations
                    |> List.map _.ContentHash

                for typeFragment in symbolic.Module.Types do
                    yield!
                        typeFragment.LiteralFields
                        |> List.map _.ContentHash

                    yield!
                        typeFragment.Methods
                        |> List.map _.ContentHash
            ]
        with
        | [ contentHash ] -> contentHash
        | contentHashes -> String.concat "|" contentHashes

    let completeInvocation
        compileStarted
        (invocation: CompilerInvocation)
        (result: CompilationResult)
        =
        let traceValues =
            result.Traces
            |> Seq.map (fun value ->
                let separator = value.IndexOf('=')

                value[.. separator
                         - 1],
                value[separator
                      + 1 ..]
            )
            |> Map.ofSeq

        let trace key = traceValues[key]

        let failureFromResult () =
            match result.Outcome with
            | CompilationOutcome.Failed ->
                match Seq.tryHead result.Diagnostics with
                | Some diagnostic ->
                    diagnostic
                    |> DiagnosticFormatter.formatCompilationDiagnostic invocation
                    |> failure compileStarted
                | None ->
                    failure compileStarted "FSC2P9999: compilation failed without a diagnostic"
            | CompilationOutcome.Unsupported unsupported ->
                failure compileStarted $"{unsupported.Code}: {unsupported.Message}"
            | CompilationOutcome.Cancelled cancellation ->
                failure
                    compileStarted
                    $"FSC2P1002: request '{cancellation.RequestIdentity.Value}' was cancelled at {cancellation.ObservedPhase}"
            | CompilationOutcome.Succeeded ->
                failure compileStarted "FSC2P9999: successful compilation was mapped as a failure"

        match result.Outcome with
        | CompilationOutcome.Succeeded ->
            let artifactBytes kind =
                result.Artifacts
                |> Seq.tryFind (fun artifact -> artifact.Kind = kind)
                |> Option.map (fun artifact -> Seq.toArray artifact.Bytes)

            match artifactBytes RequestedArtifact.ImplementationAssembly with
            | None ->
                failure compileStarted "FSC2P9999: compilation produced no implementation artifact"
            | Some implementation ->
                let linkedArtifacts = {
                    Implementation = implementation
                    PortablePdb =
                        artifactBytes RequestedArtifact.PortablePdb
                        |> Option.defaultValue Array.empty
                    ReferenceAssembly =
                        artifactBytes RequestedArtifact.ReferenceAssembly
                        |> Option.defaultValue Array.empty
                    Documentation =
                        artifactBytes RequestedArtifact.Documentation
                        |> Option.defaultValue Array.empty
                }

                let publishStarted = Stopwatch.GetTimestamp()
                Linker.publishTransactionally invocation linkedArtifacts
                let publishElapsedMicroseconds = elapsedMicroseconds publishStarted

                {
                    ExitCode = 0
                    Error = String.Empty
                    ServiceProcessId = Environment.ProcessId
                    QuerySchema = Int32.Parse(trace "querySchema")
                    NodeKind = trace "nodeKind"
                    ContentFingerprint = trace "contentFingerprint"
                    PreviousContentFingerprint = trace "previousContentFingerprint"
                    InvalidationReason = trace "invalidationReason"
                    ParseKey = trace "parseKey"
                    CheckKey = trace "checkKey"
                    LowerKey = trace "lowerKey"
                    DependencyCount = Int32.Parse(trace "dependencyCount")
                    ParseDecision = trace "parse"
                    CheckDecision = trace "check"
                    LowerDecision = trace "lower"
                    ParseElapsedMicroseconds = Int64.Parse(trace "parseElapsedMicroseconds")
                    CheckElapsedMicroseconds = Int64.Parse(trace "checkElapsedMicroseconds")
                    LowerElapsedMicroseconds = Int64.Parse(trace "lowerElapsedMicroseconds")
                    LinkElapsedMicroseconds = Int64.Parse(trace "linkElapsedMicroseconds")
                    PublishElapsedMicroseconds = publishElapsedMicroseconds
                    CompileElapsedMicroseconds = elapsedMicroseconds compileStarted
                    ExportFingerprint = trace "exportFingerprint"
                    FragmentHash = trace "fragmentHash"
                    Emitted = Boolean.Parse(trace "emitted")
                }
        | CompilationOutcome.Failed
        | CompilationOutcome.Unsupported _
        | CompilationOutcome.Cancelled _ -> failureFromResult ()

    let private compileCore
        (service: CompilerService)
        (invocation: CompilerInvocation)
        (sources: SourceInput list)
        =
        let compileStarted = Stopwatch.GetTimestamp()

        try
            let request = createRequest invocation sources

            match compileRequest service request with
            | Error compilerDiagnostic ->
                compilerDiagnostic
                |> DiagnosticFormatter.format invocation
                |> failure compileStarted
            | Ok compilation ->
                let publishStarted = Stopwatch.GetTimestamp()
                Linker.publishTransactionally invocation compilation.Artifacts
                let publishElapsedMicroseconds = elapsedMicroseconds publishStarted
                let query = compilation.Query

                {
                    ExitCode = 0
                    Error = String.Empty
                    ServiceProcessId = Environment.ProcessId
                    QuerySchema = query.QuerySchema
                    NodeKind = query.NodeKind
                    ContentFingerprint = query.ContentFingerprint
                    PreviousContentFingerprint = query.PreviousContentFingerprint
                    InvalidationReason = query.InvalidationReason
                    ParseKey = query.ParseKey
                    CheckKey = query.CheckKey
                    LowerKey = query.LowerKey
                    DependencyCount = query.DependencyCount
                    ParseDecision = query.ParseDecision
                    CheckDecision = query.CheckDecision
                    LowerDecision = query.LowerDecision
                    ParseElapsedMicroseconds = query.ParseElapsedMicroseconds
                    CheckElapsedMicroseconds = query.CheckElapsedMicroseconds
                    LowerElapsedMicroseconds = query.LowerElapsedMicroseconds
                    LinkElapsedMicroseconds = compilation.LinkElapsedMicroseconds
                    PublishElapsedMicroseconds = publishElapsedMicroseconds
                    CompileElapsedMicroseconds = elapsedMicroseconds compileStarted
                    ExportFingerprint = compilation.SymbolicAssembly.PublicFingerprint
                    FragmentHash = fragmentHash compilation.SymbolicAssembly
                    Emitted = true
                }
        with ex ->
            "FSC2P9999: "
            + ex.Message
            |> failure compileStarted

    let compile
        (service: CompilerService)
        (invocation: CompilerInvocation)
        (sources: SourceInput list)
        =
        try
            compileCore service invocation sources
        finally
            if invocation.StrongNameKey.Length > 0 then
                CryptographicOperations.ZeroMemory(invocation.StrongNameKey.AsSpan())
