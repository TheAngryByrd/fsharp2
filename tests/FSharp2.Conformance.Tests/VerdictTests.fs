namespace FSharp2.Conformance.Tests

open Expecto
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module VerdictTests =
    let private passingComparison =
        ComparisonResult("exact", 1, "exact", "exact-v1", true, null)

    let private failingComparison =
        ComparisonResult("exact", 1, "exact", "exact-v1", false, "diagnostic code changed")

    let private passingProbe =
        ProbeEvidence("managed-load", true, json """{"loaded":true}""", null)

    let private verdict
        declaredUnsupported
        infrastructureFailed
        fallbackDetected
        publishedSuccessfulArtifact
        comparisons
        probes
        missingEvidence
        =
        VerdictEngine.Decide(
            VerdictInput(
                declaredUnsupported,
                infrastructureFailed,
                fallbackDetected,
                publishedSuccessfulArtifact,
                immutableArray comparisons,
                immutableArray probes,
                immutableArray missingEvidence
            )
        )

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Verdict" [
            testCase "Conformance Verdict preserves pass fail unsupported and infra-error"
            <| fun _ ->
                let pass = verdict false false false false [ passingComparison ] [ passingProbe ] []
                let fail = verdict false false false false [ failingComparison ] [ passingProbe ] []
                let unsupported = verdict true false false false [] [] []

                let unsupportedWithDifference =
                    verdict true false false false [ failingComparison ] [ passingProbe ] []

                let unsupportedWithMissingEvidence =
                    verdict true false false false [] [] [ "required portable PDB" ]

                let infraError = verdict false true false false [] [] []

                Expect.equal
                    pass.Verdict
                    ConformanceVerdict.Pass
                    "Complete matching evidence passes"

                Expect.equal fail.Verdict ConformanceVerdict.Fail "An observable difference fails"

                Expect.equal
                    unsupported.Verdict
                    ConformanceVerdict.Unsupported
                    "A declared out-of-envelope case stays unsupported"

                Expect.equal
                    unsupportedWithDifference.Verdict
                    ConformanceVerdict.Fail
                    "A declared unsupported case cannot hide a comparison failure"

                Expect.equal
                    unsupportedWithMissingEvidence.Verdict
                    ConformanceVerdict.Fail
                    "A declared unsupported case cannot hide missing required evidence"

                Expect.equal
                    infraError.Verdict
                    ConformanceVerdict.InfraError
                    "Harness infrastructure failure stays infra-error"

                Expect.equal
                    ([
                        pass.Verdict
                        fail.Verdict
                        unsupported.Verdict
                        infraError.Verdict
                     ]
                     |> Set.ofList
                     |> Set.count)
                    4
                    "The four verdicts remain distinct"

            testCase "Conformance Verdict rejects unsupported output publication"
            <| fun _ ->
                let result = verdict true false false true [] [] []

                Expect.equal
                    result.Verdict
                    ConformanceVerdict.Fail
                    "Unsupported input cannot publish a successful artifact"

                Expect.isFalse
                    result.CountsAsCoverage
                    "Invalid publication cannot count as coverage"

            testCase "Conformance Verdict does not count unsupported as coverage"
            <| fun _ ->
                let pass = verdict false false false false [ passingComparison ] [ passingProbe ] []
                let unsupported = verdict true false false false [] [] []

                Expect.isTrue
                    pass.CountsAsCoverage
                    "A complete pass counts as compatibility coverage"

                Expect.isFalse
                    unsupported.CountsAsCoverage
                    "Unsupported is an envelope fact, not compatibility coverage"
        ]
