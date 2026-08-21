namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Security.Cryptography
open System.Threading

type Compiler() =
    let service = CompilerService()

    let fingerprint (bytes: byte array) =
        bytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

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
        | Error diagnostic ->
            {
                Outcome = CompilationOutcome.Failed
                Diagnostics = ImmutableArray.Create(failureDiagnostic diagnostic)
                Artifacts = ImmutableArray.Empty
                Fingerprints = ImmutableArray.Empty
                PhaseResults = ImmutableArray.Empty
                Traces = ImmutableArray.Empty
            }
        | Ok compilation ->
            let compilationArtifacts = artifacts request compilation.Artifacts

            {
                Outcome = CompilationOutcome.Succeeded
                Diagnostics = ImmutableArray.Empty
                Artifacts = compilationArtifacts
                Fingerprints =
                    compilationArtifacts
                    |> Seq.map _.Fingerprint
                    |> ImmutableArray.CreateRange
                PhaseResults = ImmutableArray.Empty
                Traces = ImmutableArray.Empty
            }
