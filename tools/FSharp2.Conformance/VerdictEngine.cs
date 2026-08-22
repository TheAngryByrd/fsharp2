namespace FSharp2.Conformance;

public static class VerdictEngine
{
    public static VerdictResult Decide(VerdictInput input)
    {
        ArgumentNullException.ThrowIfNull(input);
        var missingEvidence = input.MissingEvidence.IsDefault ? [] : input.MissingEvidence;
        if (input.InfrastructureFailed)
        {
            return Result(ConformanceVerdict.InfraError, ["Harness infrastructure failed."], missingEvidence);
        }
        if (input.DeclaredUnsupported && input.PublishedSuccessfulArtifact)
        {
            return Result(
                ConformanceVerdict.Fail,
                ["A declared unsupported case published a successful artifact."],
                missingEvidence);
        }
        if (input.FallbackDetected)
        {
            return Result(ConformanceVerdict.Fail, ["A forbidden compiler fallback was detected."], missingEvidence);
        }
        var reasons = new List<string>();
        reasons.AddRange(missingEvidence.Select(static value => $"Missing evidence: {value}"));
        if (!input.Comparisons.IsDefault)
        {
            reasons.AddRange(
                input.Comparisons
                    .Where(static comparison => !comparison.Passed)
                    .Select(static comparison =>
                        $"Comparison '{comparison.ComparatorId}' failed: {comparison.Difference ?? "no difference was recorded"}"));
        }
        if (!input.Probes.IsDefault)
        {
            reasons.AddRange(
                input.Probes
                    .Where(static probe => !probe.Passed)
                    .Select(static probe =>
                        $"Probe '{probe.Kind}' failed: {probe.Difference ?? "no difference was recorded"}"));
        }
        if (!input.DeclaredUnsupported
            && input.Comparisons.IsDefaultOrEmpty
            && input.Probes.IsDefaultOrEmpty)
        {
            reasons.Add("No comparison or probe evidence was recorded.");
        }

        if (reasons.Count != 0)
        {
            return Result(ConformanceVerdict.Fail, [.. reasons], missingEvidence);
        }
        return input.DeclaredUnsupported
            ? Result(
                ConformanceVerdict.Unsupported,
                ["The case is outside the declared compiler envelope."],
                missingEvidence)
            : Result(ConformanceVerdict.Pass, [], missingEvidence);
    }

    private static VerdictResult Result(
        ConformanceVerdict verdict,
        System.Collections.Immutable.ImmutableArray<string> reasons,
        System.Collections.Immutable.ImmutableArray<string> missingEvidence) =>
        new(verdict, reasons, missingEvidence, verdict == ConformanceVerdict.Pass);
}
