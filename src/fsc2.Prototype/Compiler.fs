namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Security.Cryptography
open System.Text
open System.Threading

type Compiler() =
    let service = CompilerService()

    let fingerprint (bytes: byte array) =
        bytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let textFingerprint (value: string) =
        value
        |> Encoding.UTF8.GetBytes
        |> fingerprint

    let phases = [|
        CompilationPhase.Source
        CompilationPhase.Syntax
        CompilationPhase.ResolvedSymbols
        CompilationPhase.TypedDeclarations
        CompilationPhase.LoweredCode
        CompilationPhase.OptimizedCode
        CompilationPhase.SymbolicEmission
        CompilationPhase.FinalLinking
    |]

    let sourceFingerprint (request: CompilationRequest) =
        [
            request.RequestIdentity.Value

            yield!
                request.Sources
                |> Seq.collect (fun source -> [
                    source.StableId.Value
                    source.ContentFingerprint
                ])
        ]
        |> String.concat "|"
        |> textFingerprint

    let semanticFingerprints (request: CompilationRequest) =
        let source = sourceFingerprint request
        let syntax = textFingerprint $"syntax|{source}"

        let resolved =
            [
                syntax

                yield!
                    request.TargetReferences
                    |> Seq.collect (fun reference -> [
                        reference.StableId.Value
                        reference.ContentFingerprint
                    ])
            ]
            |> String.concat "|"
            |> textFingerprint

        let typed = textFingerprint $"typed|{resolved}"

        let lowered =
            textFingerprint $"lowered|{typed}|{request.AssemblyIdentity.StableId.Value}"

        source, syntax, resolved, typed, lowered

    let phaseResult phase status inputFingerprint outputFingerprint = {
        Phase = phase
        Status = status
        InputFingerprint = inputFingerprint
        OutputFingerprint = outputFingerprint
        TraceValues = ImmutableArray.Empty
    }

    let failurePhaseResults request failedPhase =
        let sourceOutput, syntaxOutput, resolvedOutput, _, _ = semanticFingerprints request

        let failedIndex =
            phases
            |> Array.findIndex ((=) failedPhase)

        phases
        |> Array.mapi (fun index phase ->
            if index > failedIndex then
                phaseResult phase PhaseStatus.NotStarted None None
            elif index = failedIndex then
                let inputFingerprint =
                    match phase with
                    | CompilationPhase.Source -> Some request.RequestIdentity.Value
                    | CompilationPhase.Syntax -> Some sourceOutput
                    | CompilationPhase.ResolvedSymbols -> Some syntaxOutput
                    | CompilationPhase.TypedDeclarations -> Some resolvedOutput
                    | CompilationPhase.LoweredCode
                    | CompilationPhase.OptimizedCode
                    | CompilationPhase.SymbolicEmission
                    | CompilationPhase.FinalLinking -> None

                phaseResult phase PhaseStatus.Failed inputFingerprint None
            else
                match phase with
                | CompilationPhase.Source ->
                    phaseResult
                        phase
                        PhaseStatus.Completed
                        (Some request.RequestIdentity.Value)
                        (Some sourceOutput)
                | CompilationPhase.Syntax ->
                    phaseResult phase PhaseStatus.Completed (Some sourceOutput) (Some syntaxOutput)
                | CompilationPhase.ResolvedSymbols ->
                    phaseResult
                        phase
                        PhaseStatus.Completed
                        (Some syntaxOutput)
                        (Some resolvedOutput)
                | CompilationPhase.TypedDeclarations
                | CompilationPhase.LoweredCode
                | CompilationPhase.OptimizedCode
                | CompilationPhase.SymbolicEmission
                | CompilationPhase.FinalLinking ->
                    phaseResult phase PhaseStatus.Completed None None
        )
        |> ImmutableArray.CreateRange

    let unsupportedPhaseResults request stoppingPhase =
        let source, syntax, resolved, typed, lowered = semanticFingerprints request
        let symbolic = textFingerprint $"symbolic|{lowered}"

        let fingerprints =
            function
            | CompilationPhase.Source -> Some request.RequestIdentity.Value, Some source
            | CompilationPhase.Syntax -> Some source, Some syntax
            | CompilationPhase.ResolvedSymbols -> Some syntax, Some resolved
            | CompilationPhase.TypedDeclarations -> Some resolved, Some typed
            | CompilationPhase.LoweredCode -> Some typed, Some lowered
            | CompilationPhase.OptimizedCode -> Some lowered, Some lowered
            | CompilationPhase.SymbolicEmission -> Some lowered, Some symbolic
            | CompilationPhase.FinalLinking -> Some symbolic, None

        let stoppingIndex =
            phases
            |> Array.findIndex ((=) stoppingPhase)

        phases
        |> Array.mapi (fun index phase ->
            if index > stoppingIndex then
                phaseResult phase PhaseStatus.NotStarted None None
            else
                let inputFingerprint, outputFingerprint = fingerprints phase

                if index = stoppingIndex then
                    phaseResult phase PhaseStatus.Unsupported inputFingerprint None
                elif phase = CompilationPhase.OptimizedCode then
                    phaseResult phase PhaseStatus.Skipped inputFingerprint outputFingerprint
                else
                    phaseResult phase PhaseStatus.Completed inputFingerprint outputFingerprint
        )
        |> ImmutableArray.CreateRange

    let prevalidatedUnsupportedPhaseResults stoppingPhase =
        phases
        |> Array.map (fun phase ->
            if phase = stoppingPhase then
                phaseResult phase PhaseStatus.Unsupported None None
            else
                phaseResult phase PhaseStatus.NotStarted None None
        )
        |> ImmutableArray.CreateRange

    let unsupportedFailure code message phase identity = {
        Code = code
        Message = message
        StoppingPhase = phase
        UnsupportedValueIdentity = identity
    }

    let tryUnsupportedEnvelope (request: CompilationRequest) =
        let customArtifact =
            request.RequestedArtifacts
            |> Seq.tryPick (
                function
                | RequestedArtifact.Custom identity -> Some identity
                | _ -> None
            )

        match request.DiagnosticOptions.WarningLevel, customArtifact with
        | Some warningLevel, _ when
            warningLevel < 0
            || warningLevel > 5
            ->
            Some(
                unsupportedFailure
                    "FSC2C2101"
                    $"Warning level '{warningLevel}' is outside the supported range."
                    CompilationPhase.Source
                    $"diagnostics.warning-level={warningLevel}"
            )
        | _, Some identity ->
            Some(
                unsupportedFailure
                    "FSC2C2401"
                    $"Custom artifact '{identity.Value}' is not supported."
                    CompilationPhase.Source
                    $"artifact.custom={identity.Value}"
            )
        | _ ->
            match request.SemanticOptions.LanguageVersion with
            | Some languageVersion when
                languageVersion
                <> "9.0"
                ->
                Some(
                    unsupportedFailure
                        "FSC2C2001"
                        $"Language version '{languageVersion}' is not supported."
                        CompilationPhase.Syntax
                        $"semantic.language-version={languageVersion}"
                )
            | _ ->
                match request.SemanticOptions.TargetProfile with
                | Some targetProfile when
                    targetProfile
                    <> "netcore"
                    ->
                    Some(
                        unsupportedFailure
                            "FSC2C2003"
                            $"Target profile '{targetProfile}' is not supported."
                            CompilationPhase.ResolvedSymbols
                            $"semantic.target-profile={targetProfile}"
                    )
                | _ ->
                    let requestsPortablePdb =
                        request.RequestedArtifacts
                        |> Seq.contains RequestedArtifact.PortablePdb

                    if
                        request.EmissionOptions.DebugFormat = DebugFormat.None
                        && requestsPortablePdb
                    then
                        Some(
                            unsupportedFailure
                                "FSC2C2203"
                                "Portable PDB output requires the portable debug format."
                                CompilationPhase.SymbolicEmission
                                "emission.debug-format=none+pdb"
                        )
                    elif not request.EmissionOptions.EmbeddedSourceIdentities.IsEmpty then
                        let identity = request.EmissionOptions.EmbeddedSourceIdentities[0]

                        Some(
                            unsupportedFailure
                                "FSC2C2204"
                                $"Embedded source '{identity.Value}' is not supported."
                                CompilationPhase.SymbolicEmission
                                $"emission.embedded-source={identity.Value}"
                        )
                    elif
                        request.EmissionOptions.DebugDocumentPaths.Length
                        <> request.Sources.Length
                    then
                        let count = request.EmissionOptions.DebugDocumentPaths.Length

                        Some(
                            unsupportedFailure
                                "FSC2C2205"
                                $"Debug document count '{count}' does not match the source count."
                                CompilationPhase.SymbolicEmission
                                $"emission.debug-document-count={count}"
                        )
                    elif not request.EmissionOptions.Deterministic then
                        Some(
                            unsupportedFailure
                                "FSC2C2201"
                                "Nondeterministic emission is not supported."
                                CompilationPhase.FinalLinking
                                "emission.deterministic=false"
                        )
                    elif
                        request.EmissionOptions.HighEntropyVirtualAddress
                        && request.EmissionOptions.Target = CompilationTarget.Library
                    then
                        Some(
                            unsupportedFailure
                                "FSC2C2202"
                                "High-entropy virtual addresses are not supported."
                                CompilationPhase.FinalLinking
                                "emission.high-entropy-va=true"
                        )
                    elif
                        request.SigningOptions.Mode = SigningMode.Unsigned
                        && not request.SigningOptions.Key.IsEmpty
                    then
                        Some(
                            unsupportedFailure
                                "FSC2C2301"
                                "Unsigned output must not include a signing key."
                                CompilationPhase.FinalLinking
                                "signing.unsigned-key=present"
                        )
                    elif not (Linker.hasValidSigningKey request.SigningOptions) then
                        Some(
                            unsupportedFailure
                                "FSC2C2302"
                                "The signing key is empty or malformed."
                                CompilationPhase.FinalLinking
                                "signing.key=invalid"
                        )
                    elif request.Resources.Managed.Length > 1 then
                        let count = request.Resources.Managed.Length

                        Some(
                            unsupportedFailure
                                "FSC2C2501"
                                $"Managed resource count '{count}' is not supported."
                                CompilationPhase.FinalLinking
                                $"resources.managed-count={count}"
                        )
                    elif request.Resources.Native.Length > 1 then
                        let count = request.Resources.Native.Length

                        Some(
                            unsupportedFailure
                                "FSC2C2502"
                                $"Native resource count '{count}' is not supported."
                                CompilationPhase.FinalLinking
                                $"resources.native-count={count}"
                        )
                    else
                        None

    let unsupportedResult failure phaseResults = {
        Outcome = CompilationOutcome.Unsupported failure
        Diagnostics = ImmutableArray.Empty
        Artifacts = ImmutableArray.Empty
        Fingerprints = ImmutableArray.Empty
        PhaseResults = phaseResults
        Traces = ImmutableArray.Empty
    }

    let cancelledResult request = {
        Outcome =
            CompilationOutcome.Cancelled {
                RequestIdentity = request.RequestIdentity
                ObservedPhase = CompilationPhase.Source
            }
        Diagnostics = ImmutableArray.Empty
        Artifacts = ImmutableArray.Empty
        Fingerprints = ImmutableArray.Empty
        PhaseResults =
            phases
            |> Array.map (fun phase ->
                if phase = CompilationPhase.Source then
                    phaseResult
                        phase
                        PhaseStatus.Cancelled
                        (Some request.RequestIdentity.Value)
                        None
                else
                    phaseResult phase PhaseStatus.NotStarted None None
            )
            |> ImmutableArray.CreateRange
        Traces = ImmutableArray.Empty
    }

    let successPhaseResults request compilation compilationArtifacts =
        let source, syntax, resolved, typed, lowered = semanticFingerprints request

        let symbolic =
            textFingerprint $"symbolic|{lowered}|{compilation.SymbolicAssembly.PublicFingerprint}"

        let linked =
            compilationArtifacts
            |> Seq.map _.Fingerprint
            |> String.concat "|"
            |> textFingerprint

        [|
            phaseResult
                CompilationPhase.Source
                PhaseStatus.Completed
                (Some request.RequestIdentity.Value)
                (Some source)
            phaseResult CompilationPhase.Syntax PhaseStatus.Completed (Some source) (Some syntax)
            phaseResult
                CompilationPhase.ResolvedSymbols
                PhaseStatus.Completed
                (Some syntax)
                (Some resolved)
            phaseResult
                CompilationPhase.TypedDeclarations
                PhaseStatus.Completed
                (Some resolved)
                (Some typed)
            phaseResult
                CompilationPhase.LoweredCode
                PhaseStatus.Completed
                (Some typed)
                (Some lowered)
            phaseResult
                CompilationPhase.OptimizedCode
                PhaseStatus.Skipped
                (Some lowered)
                (Some lowered)
            phaseResult
                CompilationPhase.SymbolicEmission
                PhaseStatus.Completed
                (Some lowered)
                (Some symbolic)
            phaseResult
                CompilationPhase.FinalLinking
                PhaseStatus.Completed
                (Some symbolic)
                (Some linked)
        |]
        |> ImmutableArray.CreateRange

    let artifact request kind suffix bytes = {
        Kind = kind
        StableId =
            StableIdentity.create $"{request.AssemblyIdentity.StableId.Value}/artifact:{suffix}"
        Fingerprint = fingerprint bytes
        Bytes = ImmutableArray.CreateRange<byte>(bytes)
    }

    let artifacts request linked =
        request.RequestedArtifacts
        |> Seq.choose (
            function
            | RequestedArtifact.ImplementationAssembly ->
                Some(
                    artifact
                        request
                        RequestedArtifact.ImplementationAssembly
                        "implementation"
                        linked.Implementation
                )
            | RequestedArtifact.PortablePdb ->
                Some(
                    artifact request RequestedArtifact.PortablePdb "portable-pdb" linked.PortablePdb
                )
            | RequestedArtifact.ReferenceAssembly ->
                Some(
                    artifact
                        request
                        RequestedArtifact.ReferenceAssembly
                        "reference"
                        linked.ReferenceAssembly
                )
            | RequestedArtifact.Documentation ->
                Some(
                    artifact
                        request
                        RequestedArtifact.Documentation
                        "documentation"
                        linked.Documentation
                )
            | RequestedArtifact.Custom _ -> None
        )
        |> ImmutableArray.CreateRange

    let successTraces compilation =
        let query = compilation.Query

        [|
            $"querySchema={query.QuerySchema}"
            $"nodeKind={query.NodeKind}"
            $"contentFingerprint={query.ContentFingerprint}"
            $"previousContentFingerprint={query.PreviousContentFingerprint}"
            $"invalidationReason={query.InvalidationReason}"
            $"parseKey={query.ParseKey}"
            $"checkKey={query.CheckKey}"
            $"lowerKey={query.LowerKey}"
            $"dependencyCount={query.DependencyCount}"
            $"parse={query.ParseDecision}"
            $"check={query.CheckDecision}"
            $"lower={query.LowerDecision}"
            $"parseElapsedMicroseconds={query.ParseElapsedMicroseconds}"
            $"checkElapsedMicroseconds={query.CheckElapsedMicroseconds}"
            $"lowerElapsedMicroseconds={query.LowerElapsedMicroseconds}"
            $"linkElapsedMicroseconds={compilation.LinkElapsedMicroseconds}"
            $"exportFingerprint={compilation.SymbolicAssembly.PublicFingerprint}"
            $"fragmentHash={CompilationPipeline.fragmentHash compilation.SymbolicAssembly}"
            "emitted=true"
        |]
        |> ImmutableArray.CreateRange

    let failureDiagnostic (diagnostic: CompilerDiagnostic) = {
        Code = diagnostic.Code
        Severity = DiagnosticSeverity.Error
        Message = diagnostic.Message
        LogicalPath = diagnostic.Path
        Range = diagnostic.Range
    }

    member _.Compile(request: CompilationRequest, cancellationToken: CancellationToken) =
        if cancellationToken.IsCancellationRequested then
            cancelledResult request
        else
            match tryUnsupportedEnvelope request with
            | Some failure ->
                unsupportedResult
                    failure
                    (prevalidatedUnsupportedPhaseResults failure.StoppingPhase)
            | None ->
                match CompilationPipeline.compileRequest service request with
                | Error diagnostic when diagnostic.Code = "FSC2C2002" ->
                    let failure = {
                        Code = diagnostic.Code
                        Message = diagnostic.Message
                        StoppingPhase = CompilationPhase.OptimizedCode
                        UnsupportedValueIdentity = "semantic.optimization=enabled"
                    }

                    unsupportedResult
                        failure
                        (unsupportedPhaseResults request failure.StoppingPhase)
                | Error diagnostic ->
                    let failedPhase =
                        if diagnostic.Code = "FS0001" then
                            CompilationPhase.TypedDeclarations
                        elif diagnostic.Code = "FS0039" then
                            CompilationPhase.ResolvedSymbols
                        else
                            CompilationPhase.Syntax

                    let diagnostics =
                        [| failureDiagnostic diagnostic |]
                        |> DiagnosticPolicy.apply request.DiagnosticOptions

                    {
                        Outcome = CompilationOutcome.Failed
                        Diagnostics = diagnostics
                        Artifacts = ImmutableArray.Empty
                        Fingerprints = ImmutableArray.Empty
                        PhaseResults = failurePhaseResults request failedPhase
                        Traces = ImmutableArray.Empty
                    }
                | Ok compilation ->
                    let compilationArtifacts = artifacts request compilation.Artifacts
                    let phaseResults = successPhaseResults request compilation compilationArtifacts

                    {
                        Outcome = CompilationOutcome.Succeeded
                        Diagnostics = ImmutableArray.Empty
                        Artifacts = compilationArtifacts
                        Fingerprints =
                            compilationArtifacts
                            |> Seq.map _.Fingerprint
                            |> ImmutableArray.CreateRange
                        PhaseResults = phaseResults
                        Traces = successTraces compilation
                    }
