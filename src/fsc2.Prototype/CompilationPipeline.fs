namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text

type internal CoreCompilation = {
    Query: CompilerQueryResult
    SymbolicAssembly: SymbolicAssembly
    Artifacts: LinkedArtifacts
    Diagnostics: ImmutableArray<CompilationDiagnostic>
    DiagnosticOptions: DiagnosticOptions
    LinkElapsedMicroseconds: int64
} with

    override _.ToString() = "CoreCompilation"

/// The host-independent compile/link/publish path shared by standalone and
/// persistent-service execution.
type internal RequestFailure = {
    Errors: CompilerDiagnostic list
    Warnings: CompilationDiagnostic list
}

module internal CompilationPipeline =
    type Response = {
        ExitCode: int
        DiagnosticOptions: DiagnosticOptions
        Diagnostics: ImmutableArray<CompilationDiagnostic>
        ServiceProcessId: int
        QuerySchema: int
        NodeKind: string
        ContentFingerprint: string
        PreviousContentFingerprint: string
        InvalidationReason: string
        ParseKey: string
        CheckKey: string
        LowerKey: string
        DependencyCount: int
        ParseDecision: string
        CheckDecision: string
        LowerDecision: string
        ParseElapsedMicroseconds: int64
        CheckElapsedMicroseconds: int64
        LowerElapsedMicroseconds: int64
        LinkElapsedMicroseconds: int64
        PublishElapsedMicroseconds: int64
        CompileElapsedMicroseconds: int64
        ExportFingerprint: string
        FragmentHash: string
        Emitted: bool
    }

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

    let defaultDiagnosticOptions () =
        DiagnosticOptions.Create(
            None,
            [||],
            [||],
            false,
            [||],
            [||],
            None,
            false,
            None,
            [||],
            false,
            false,
            false,
            DiagnosticStyle.Default,
            ConsoleColorMode.Automatic,
            None,
            None,
            false,
            false,
            false
        )

    let diagnosticOptions (invocation: CompilerInvocation) (renderingOptions: DiagnosticOptions) =
        DiagnosticOptions.Create(
            invocation.WarningLevel,
            List.toArray invocation.DisabledWarnings,
            List.toArray invocation.EnabledWarnings,
            invocation.TreatWarningsAsErrors,
            List.toArray invocation.WarningsAsErrors,
            List.toArray invocation.WarningsNotAsErrors,
            invocation.MaximumErrors,
            invocation.AbortOnError,
            renderingOptions.PreferredUICulture,
            Seq.toArray renderingOptions.LocalWarningDirectives,
            renderingOptions.FullPaths,
            renderingOptions.FlatErrors,
            renderingOptions.Utf8Output,
            renderingOptions.DiagnosticStyle,
            renderingOptions.ConsoleColorMode,
            renderingOptions.LCID,
            renderingOptions.PreferredUILanguage,
            renderingOptions.TestParserErrorRecovery,
            renderingOptions.StandardOutputRedirected,
            renderingOptions.StandardErrorRedirected
        )

    let createRequestWithDiagnosticOptions
        (invocation: CompilerInvocation)
        (diagnosticOptions: DiagnosticOptions)
        (sources: SourceInput list)
        =
        let assemblyName = Path.GetFileNameWithoutExtension(invocation.AssemblyPath)

        let sourceSnapshots =
            sources
            |> List.mapi (fun index source ->
                SourceSnapshot.Create(
                    StableIdentity.create $"source:{index}:{Path.GetFileName(source.Path)}",
                    source.Path,
                    source.Text,
                    source.ContentFingerprint
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
            diagnosticOptions,
            EmissionOptions.Create(
                invocation.Target,
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

    let createRequest (invocation: CompilerInvocation) (sources: SourceInput list) =
        defaultDiagnosticOptions ()
        |> diagnosticOptions invocation
        |> fun options -> createRequestWithDiagnosticOptions invocation options sources

    let private diagnostic code message = {
        Code = code
        Message = message
        Path = None
        Range = None
    }

    let private sourceDiagnosticOptions
        (request: CompilationRequest)
        (preparedSources: LexicalDocument list)
        =
        let existing = request.DiagnosticOptions.LocalWarningDirectives

        let firstSourceOrder =
            existing
            |> Seq.map _.Order
            |> Seq.fold max -1L
            |> (+) 1L

        let sourceDirectives =
            preparedSources
            |> Seq.collect _.WarningDirectives
            |> Seq.mapi (fun index directive -> {
                directive with
                    Order =
                        firstSourceOrder
                        + int64 index
            })

        {
            request.DiagnosticOptions with
                LocalWarningDirectives =
                    Seq.append existing sourceDirectives
                    |> ImmutableArray.CreateRange
        }

    let private sourceWarnings (service: CompilerService) (preparedSources: LexicalDocument list) =
        preparedSources
        |> Seq.collect service.LexicalWarnings
        |> Seq.mapi (fun order warning ->
            CompilationDiagnostic.Create(
                int64 order,
                warning.Code,
                Int32.Parse(warning.Code.Substring(2)),
                None,
                DiagnosticStage.Compilation CompilationPhase.Source,
                DiagnosticSeverity.Warning,
                DiagnosticSeverity.Warning,
                DiagnosticDisposition.Emitted,
                None,
                warning.Message,
                warning.Path,
                warning.Range,
                [||],
                [||],
                Some DiagnosticStream.StandardError
            )
        )
        |> Seq.toList

    let private compileRequestCore (service: CompilerService) (request: CompilationRequest) =
        match
            ReferenceTypeIndex.Create(request.TargetReferences),
            LanguageVersion.normalize request.SemanticOptions.LanguageVersion
        with
        | Error message, _ -> Error [ diagnostic "FSC2P1001" message ]
        | _, Error message -> Error [ diagnostic "FSC2C2001" message ]
        | Ok references, Ok language ->
            let sources =
                request.Sources
                |> List.ofSeq

            let defines = List.ofSeq request.SemanticOptions.Defines

            let preparedSources =
                sources
                |> List.map (fun source ->
                    service.PrepareSource(language, defines, source).Document
                )

            let lexicalDiagnostics =
                preparedSources
                |> Seq.collect service.LexicalDiagnostics
                |> Seq.toList

            if not lexicalDiagnostics.IsEmpty then
                Error lexicalDiagnostics
            else
                match
                    service.Compile(
                        request.AssemblyIdentity.Name,
                        language,
                        defines,
                        references,
                        sources
                    )
                with
                | Error compilerDiagnostic -> Error [ compilerDiagnostic ]
                | Ok query ->
                    match request.SemanticOptions.Optimization with
                    | OptimizationMode.Enabled ->
                        Error [
                            diagnostic
                                "FSC2C2002"
                                "Enabled optimization is not supported by this compiler contract."
                        ]
                    | OptimizationMode.Disabled ->
                        try
                            let symbolic = SymbolicEmission.emit query.LoweredCompilation

                            let entryPointCount =
                                symbolic.Module.Types
                                |> List.collect _.Methods
                                |> List.filter (fun methodFragment ->
                                    methodFragment.Kind = EntryPoint
                                )
                                |> List.length

                            match request.EmissionOptions.Target, entryPointCount with
                            | CompilationTarget.Executable, 0 ->
                                Error [
                                    diagnostic
                                        "FSC2P1001"
                                        "An executable compilation requires one entry point."
                                ]
                            | CompilationTarget.Executable, count when count > 1 ->
                                Error [
                                    diagnostic
                                        "FSC2P1001"
                                        "An executable compilation cannot contain more than one entry point."
                                ]
                            | _ ->
                                let linkStarted = Stopwatch.GetTimestamp()
                                let artifacts = Linker.link request symbolic

                                let diagnosticOptions =
                                    sourceDiagnosticOptions request preparedSources

                                let diagnostics =
                                    sourceWarnings service preparedSources
                                    |> Seq.map (DiagnosticPolicy.input None false true)
                                    |> DiagnosticPolicy.apply diagnosticOptions
                                    |> Seq.filter (fun diagnostic ->
                                        diagnostic.Disposition = DiagnosticDisposition.Emitted
                                    )
                                    |> ImmutableArray.CreateRange

                                if
                                    diagnostics
                                    |> Seq.exists DiagnosticPolicy.isEffectiveError
                                then
                                    Error []
                                else
                                    Ok {
                                        Query = query
                                        SymbolicAssembly = symbolic
                                        Artifacts = artifacts
                                        Diagnostics = diagnostics
                                        DiagnosticOptions = diagnosticOptions
                                        LinkElapsedMicroseconds = elapsedMicroseconds linkStarted
                                    }
                        with ex ->
                            Error [ diagnostic "FSC2P9999" ex.Message ]

    let compileRequest (service: CompilerService) (request: CompilationRequest) =
        match compileRequestCore service request with
        | Ok compilation -> Ok compilation
        | Error errors ->
            let warnings =
                match LanguageVersion.normalize request.SemanticOptions.LanguageVersion with
                | Error _ -> []
                | Ok language ->
                    let defines = List.ofSeq request.SemanticOptions.Defines

                    request.Sources
                    |> Seq.map (fun source ->
                        service.PrepareSource(language, defines, source).Document
                    )
                    |> Seq.toList
                    |> sourceWarnings service

            Error { Errors = errors; Warnings = warnings }

    let private responseDiagnostic code numericCode stage message =
        CompilationDiagnostic.Create(
            0L,
            code,
            numericCode,
            None,
            stage,
            DiagnosticSeverity.Error,
            DiagnosticSeverity.Error,
            DiagnosticDisposition.Emitted,
            None,
            message,
            None,
            None,
            [||],
            [||],
            Some DiagnosticStream.StandardError
        )

    let private structuredCompilerDiagnostic (diagnostic: CompilerDiagnostic) =
        let numericText =
            if diagnostic.Code.StartsWith("FS", StringComparison.Ordinal) then
                diagnostic.Code.AsSpan(2)
            elif diagnostic.Code.StartsWith("FSC2P", StringComparison.Ordinal) then
                diagnostic.Code.AsSpan(5)
            else
                ReadOnlySpan<char>()

        let numericCode =
            match Int32.TryParse(numericText) with
            | true, value -> value
            | false, _ -> 0

        CompilationDiagnostic.Create(
            0L,
            diagnostic.Code,
            numericCode,
            None,
            DiagnosticStage.Compilation CompilationPhase.Syntax,
            DiagnosticSeverity.Error,
            DiagnosticSeverity.Error,
            DiagnosticDisposition.Emitted,
            None,
            diagnostic.Message,
            diagnostic.Path,
            diagnostic.Range,
            [||],
            [||],
            Some DiagnosticStream.StandardError
        )

    let private failure started options diagnostics = {
        ExitCode = 1
        DiagnosticOptions = options
        Diagnostics = ImmutableArray.CreateRange diagnostics
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

    let serviceFailure options code numericCode message =
        responseDiagnostic code numericCode DiagnosticStage.Host message
        |> Array.singleton
        |> failure (Stopwatch.GetTimestamp()) options

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
        (diagnosticOptions: DiagnosticOptions)
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
        let effectiveOutcome = DiagnosticPolicy.outcome result.Outcome result.Diagnostics

        let failureFromResult () =
            match effectiveOutcome with
            | CompilationOutcome.Failed ->
                if
                    result.Diagnostics
                    |> Seq.exists DiagnosticPolicy.isEffectiveError
                then
                    failure compileStarted diagnosticOptions result.Diagnostics
                else
                    responseDiagnostic
                        "FSC2P9999"
                        9999
                        DiagnosticStage.Host
                        "compilation failed without a diagnostic"
                    |> Array.singleton
                    |> failure compileStarted diagnosticOptions
            | CompilationOutcome.Unsupported unsupported ->
                responseDiagnostic
                    unsupported.Code
                    0
                    (DiagnosticStage.Compilation unsupported.StoppingPhase)
                    unsupported.Message
                |> Array.singleton
                |> failure compileStarted diagnosticOptions
            | CompilationOutcome.Cancelled cancellation ->
                responseDiagnostic
                    "FSC2P1002"
                    1002
                    DiagnosticStage.Host
                    $"request '{cancellation.RequestIdentity.Value}' was cancelled at {cancellation.ObservedPhase}"
                |> Array.singleton
                |> failure compileStarted diagnosticOptions
            | CompilationOutcome.Succeeded ->
                responseDiagnostic
                    "FSC2P9999"
                    9999
                    DiagnosticStage.Host
                    "successful compilation was mapped as a failure"
                |> Array.singleton
                |> failure compileStarted diagnosticOptions

        match effectiveOutcome with
        | CompilationOutcome.Succeeded ->
            let artifactBytes kind =
                result.Artifacts
                |> Seq.tryFind (fun artifact -> artifact.Kind = kind)
                |> Option.map (fun artifact -> Seq.toArray artifact.Bytes)

            match artifactBytes RequestedArtifact.ImplementationAssembly with
            | None ->
                responseDiagnostic
                    "FSC2P9999"
                    9999
                    DiagnosticStage.Publication
                    "compilation produced no implementation artifact"
                |> Array.singleton
                |> failure compileStarted diagnosticOptions
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
                    DiagnosticOptions = diagnosticOptions
                    Diagnostics = result.Diagnostics
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

        let options =
            defaultDiagnosticOptions ()
            |> diagnosticOptions invocation

        try
            let request = createRequestWithDiagnosticOptions invocation options sources

            match compileRequest service request with
            | Error requestFailure ->
                Seq.append
                    requestFailure.Warnings
                    (requestFailure.Errors
                     |> Seq.map structuredCompilerDiagnostic)
                |> failure compileStarted options
            | Ok compilation ->
                let publishStarted = Stopwatch.GetTimestamp()
                Linker.publishTransactionally invocation compilation.Artifacts
                let publishElapsedMicroseconds = elapsedMicroseconds publishStarted
                let query = compilation.Query

                let diagnostics =
                    compilation.Diagnostics
                    |> Seq.map (DiagnosticPolicy.input None false true)
                    |> DiagnosticPolicy.apply compilation.DiagnosticOptions

                {
                    ExitCode = 0
                    DiagnosticOptions = compilation.DiagnosticOptions
                    Diagnostics = diagnostics
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
            responseDiagnostic "FSC2P9999" 9999 DiagnosticStage.Host ex.Message
            |> Array.singleton
            |> failure compileStarted options

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
