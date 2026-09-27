namespace FSharp2.Conformance.Tests

open System
open System.Collections.Immutable
open System.Threading
open Expecto
open FSharp2.Compiler
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module CompilerContractEvidenceTests =
    let private phase phase status input output traces : PhaseResult = {
        Phase = phase
        Status = status
        InputFingerprint = input
        OutputFingerprint = output
        TraceValues = immutableArray traces
    }

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Contract" [
            testCase "Conformance Contract maps one case to CompilationRequest"
            <| fun _ ->
                withCopiedRoot
                    "contract-request"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById repository "language.bindings.value-function-positive"
                            )

                        let request = CompilerContractProbe.CreateRequest(materialized)

                        Expect.equal
                            request.ContractVersion
                            CompilerContract.Version
                            "Contract version is public and stable"

                        Expect.equal
                            request.RequestIdentity.Value
                            materialized.CaseId
                            "Case id becomes request identity"

                        Expect.equal request.Sources.Length 1 "One source snapshot is mapped"

                        Expect.equal
                            request.Sources[0].StableId.Value
                            "source.program"
                            "Source identity is path-free"

                        Expect.equal
                            request.Sources[0].LogicalPath
                            "/src/Program.fs"
                            "Logical source path is preserved"

                        Expect.stringContains
                            request.Sources[0].Text
                            "let answer () = 42"
                            "Source bytes map to source text"

                        Expect.equal
                            request.TargetReferences.Length
                            materialized.TargetReferences.Length
                            "The complete target-reference closure is mapped"

                        Expect.equal
                            request.SemanticOptions.LanguageVersion
                            None
                            "The default language version leaves the compiler contract unspecified"

                        Expect.equal
                            request.DiagnosticOptions.WarningLevel
                            (Some 5)
                            "Diagnostic options map to the compiler contract"

                        Expect.isTrue
                            request.EmissionOptions.Deterministic
                            "Emission options map to the compiler contract"

                        Expect.equal
                            request.EmissionOptions.Target
                            CompilationTarget.Executable
                            "The positive request selects executable output"

                        Expect.sequenceEqual
                            request.RequestedArtifacts
                            [
                                RequestedArtifact.ImplementationAssembly
                                RequestedArtifact.PortablePdb
                            ]
                            "Requested artifacts map in declared order"

                        let result = Compiler().Compile(request, CancellationToken.None)

                        let diagnostics =
                            result.Diagnostics
                            |> Seq.map (fun diagnostic ->
                                $"{diagnostic.Code}|{diagnostic.EffectiveSeverity}|{diagnostic.Message}|{diagnostic.LogicalPath}"
                            )
                            |> String.concat "\n"

                        Expect.equal
                            result.Outcome
                            CompilationOutcome.Succeeded
                            $"The positive direct-core request succeeds. Diagnostics:\n{diagnostics}"

                        Expect.isTrue
                            (result.Artifacts
                             |> Seq.exists (fun artifact ->
                                 artifact.Kind = RequestedArtifact.ImplementationAssembly
                             ))
                            "The positive direct-core request publishes an implementation assembly"
                    )

            testCase "Conformance Contract maps invalid UTF-8 with replacement decoding"
            <| fun _ ->
                withCopiedRoot
                    "contract-invalid-utf8"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById
                                    repository
                                    "language.source.encoding-invalid-utf8-negative"
                            )

                        let request = CompilerContractProbe.CreateRequest(materialized)

                        Expect.stringContains
                            request.Sources[0].Text
                            "\uFFFD("
                            "Invalid UTF-8 uses replacement decoding"
                    )

            testCase "Conformance Contract preserves separated lexical diagnostics"
            <| fun _ ->
                withCopiedRoot
                    "contract-lexical-recovery"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById repository "language.source.lexical-recovery-negative"
                            )

                        let result =
                            Compiler()
                                .Compile(
                                    CompilerContractProbe.CreateRequest(materialized),
                                    CancellationToken.None
                                )

                        Expect.sequenceEqual
                            (result.Diagnostics
                             |> Seq.map (fun diagnostic ->
                                 let range = diagnostic.Range.Value

                                 diagnostic.Code,
                                 range.Start.Offset,
                                 range.Start.Line,
                                 range.Start.Column,
                                 range.End.Offset,
                                 range.End.Line,
                                 range.End.Column
                             )
                             |> Seq.toArray)
                            [|
                                "FS0010", 28, 2, 14, 29, 2, 15
                                "FS0010", 45, 3, 16, 46, 3, 17
                            |]
                            "Separated lexical diagnostics remain ordered"
                    )

            testCase "Conformance Contract records ordered phase evidence"
            <| fun _ ->
                let result: CompilationResult = {
                    Outcome = CompilationOutcome.Succeeded
                    Diagnostics = ImmutableArray.Empty
                    Artifacts = ImmutableArray.Empty
                    Fingerprints =
                        immutableArray [
                            "syntax-output"
                            "source-output"
                        ]
                    PhaseResults =
                        immutableArray [
                            phase
                                CompilationPhase.Source
                                PhaseStatus.Completed
                                (Some "source-input")
                                (Some "source-output")
                                [ "source-trace" ]
                            phase
                                CompilationPhase.Syntax
                                PhaseStatus.Completed
                                (Some "syntax-input")
                                (Some "syntax-output")
                                [ "syntax-trace" ]
                        ]
                    Traces = immutableArray [ "request-trace" ]
                }

                let evidence = CoreEvidenceWriter.Create(result)

                Expect.equal
                    evidence.ContractVersion
                    CompilerContract.Version
                    "Evidence records the contract version"

                Expect.equal evidence.Outcome "succeeded" "Evidence records the public outcome"

                Expect.sequenceEqual
                    (evidence.Phases
                     |> Seq.map (fun value -> value.Phase))
                    [
                        "source"
                        "syntax"
                    ]
                    "Phase evidence preserves public compiler order"

                Expect.sequenceEqual
                    evidence.Phases[1].TraceValues
                    [ "syntax-trace" ]
                    "Phase-local trace values are durable"

            testCase "Conformance Contract rejects successful artifact publication on failure"
            <| fun _ ->
                let artifact: CompilationArtifact = {
                    Kind = RequestedArtifact.ImplementationAssembly
                    StableId = StableIdentity.create "artifact:failed-output"
                    Fingerprint = "sha256:failed-output"
                    Bytes = bytes "invalid published artifact"
                }

                let result: CompilationResult = {
                    Outcome = CompilationOutcome.Failed
                    Diagnostics = ImmutableArray.Empty
                    Artifacts = immutableArray [ artifact ]
                    Fingerprints = ImmutableArray.Empty
                    PhaseResults =
                        immutableArray [
                            phase
                                CompilationPhase.Source
                                PhaseStatus.Failed
                                (Some "source-input")
                                None
                                [ "failure" ]
                        ]
                    Traces = immutableArray [ "failed" ]
                }

                Expect.throwsT<ConformanceContractException>
                    (fun () ->
                        CoreEvidenceWriter.Create(result)
                        |> ignore
                    )
                    "Failed compilation results cannot publish a successful artifact"
        ]
