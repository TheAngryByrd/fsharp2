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

        let failedIndex = phases |> Array.findIndex ((=) failedPhase)

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
                    phaseResult
                        phase
                        PhaseStatus.Completed
                        (Some sourceOutput)
                        (Some syntaxOutput)
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

    let unsupportedOptimizationPhaseResults request =
        let source, syntax, resolved, typed, lowered = semanticFingerprints request

        [|
            phaseResult
                CompilationPhase.Source
                PhaseStatus.Completed
                (Some request.RequestIdentity.Value)
                (Some source)
            phaseResult
                CompilationPhase.Syntax
                PhaseStatus.Completed
                (Some source)
                (Some syntax)
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
                PhaseStatus.Unsupported
                (Some lowered)
                None
            phaseResult CompilationPhase.SymbolicEmission PhaseStatus.NotStarted None None
            phaseResult CompilationPhase.FinalLinking PhaseStatus.NotStarted None None
        |]
        |> ImmutableArray.CreateRange

    let successPhaseResults request compilation compilationArtifacts =
        let source, syntax, resolved, typed, lowered = semanticFingerprints request

        let symbolic =
            textFingerprint
                $"symbolic|{lowered}|{compilation.SymbolicAssembly.PublicFingerprint}"

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
            phaseResult
                CompilationPhase.Syntax
                PhaseStatus.Completed
                (Some source)
                (Some syntax)
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

    let artifact request kind suffix bytes =
        {
            Kind = kind
            StableId =
                StableIdentity.create $"{request.AssemblyIdentity.StableId.Value}/artifact:{suffix}"
            Fingerprint = fingerprint bytes
            Bytes = ImmutableArray.CreateRange<byte>(bytes)
        }

    let artifacts request linked =
        request.RequestedArtifacts
        |> Seq.choose (function
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
                    artifact request RequestedArtifact.Documentation "documentation" linked.Documentation
                )
            | RequestedArtifact.Custom _ -> None)
        |> ImmutableArray.CreateRange

    let failureDiagnostic (diagnostic: CompilerDiagnostic) = {
        Code = diagnostic.Code
        Severity = DiagnosticSeverity.Error
        Message = diagnostic.Message
        LogicalPath = diagnostic.Path
        Range = diagnostic.Range
    }

    member _.Compile(request: CompilationRequest, cancellationToken: CancellationToken) =
        cancellationToken
        |> ignore

        match CompilationPipeline.compileRequest service request with
        | Error diagnostic when diagnostic.Code = "FSC2C2002" ->
            {
                Outcome =
                    CompilationOutcome.Unsupported {
                        Code = diagnostic.Code
                        Message = diagnostic.Message
                        StoppingPhase = CompilationPhase.OptimizedCode
                        UnsupportedValueIdentity = "semantic.optimization=enabled"
                    }
                Diagnostics = ImmutableArray.Empty
                Artifacts = ImmutableArray.Empty
                Fingerprints = ImmutableArray.Empty
                PhaseResults = unsupportedOptimizationPhaseResults request
                Traces = ImmutableArray.Empty
            }
        | Error diagnostic ->
            let failedPhase =
                if diagnostic.Code = "FS0001" then
                    CompilationPhase.TypedDeclarations
                elif diagnostic.Code = "FS0039" then
                    CompilationPhase.ResolvedSymbols
                else
                    CompilationPhase.Syntax

            {
                Outcome = CompilationOutcome.Failed
                Diagnostics = ImmutableArray.Create(failureDiagnostic diagnostic)
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
                Traces = ImmutableArray.Empty
            }
