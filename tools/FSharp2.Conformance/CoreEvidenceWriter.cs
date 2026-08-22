using FSharp2.Compiler;
using System.Text;

namespace FSharp2.Conformance;

public static class CoreEvidenceWriter
{
    public static CoreEvidence Create(CompilationResult result)
    {
        ArgumentNullException.ThrowIfNull(result);
        var outcome = OutcomeName(result.Outcome);
        if (!string.Equals(outcome, "succeeded", StringComparison.Ordinal)
            && !result.Artifacts.IsDefaultOrEmpty)
        {
            throw new ConformanceContractException(
                [new(
                    "artifact-publication",
                    "CompilationResult.Artifacts",
                    $"A '{outcome}' compilation result cannot publish artifacts.")]);
        }

        return new CoreEvidence(
            CompilerContract.Version,
            outcome,
            [.. result.PhaseResults.Select(static phase =>
                new CorePhaseEvidence(
                    Kebab(phase.Phase.ToString()),
                    Kebab(phase.Status.ToString()),
                    NormalizeFingerprint(OptionValue(phase.InputFingerprint)),
                    NormalizeFingerprint(OptionValue(phase.OutputFingerprint)),
                    [.. phase.TraceValues]))],
            [.. result.Artifacts.Select(static artifact =>
                new CoreArtifactEvidence(
                    ArtifactName(artifact.Kind),
                    artifact.StableId.Value,
                    NormalizeFingerprint(artifact.Fingerprint)!,
                    [.. artifact.Bytes]))],
            [.. result.Traces]);
    }

    private static string OutcomeName(CompilationOutcome outcome) => outcome.Tag switch
    {
        CompilationOutcome.Tags.Succeeded => "succeeded",
        CompilationOutcome.Tags.Failed => "failed",
        CompilationOutcome.Tags.Unsupported => "unsupported",
        CompilationOutcome.Tags.Cancelled => "cancelled",
        _ => throw new ArgumentOutOfRangeException(nameof(outcome)),
    };

    private static string ArtifactName(RequestedArtifact artifact) => artifact.Tag switch
    {
        RequestedArtifact.Tags.ImplementationAssembly => "implementation-assembly",
        RequestedArtifact.Tags.PortablePdb => "portable-pdb",
        RequestedArtifact.Tags.ReferenceAssembly => "reference-assembly",
        RequestedArtifact.Tags.Documentation => "xml-documentation",
        RequestedArtifact.Tags.Custom => "custom",
        _ => throw new ArgumentOutOfRangeException(nameof(artifact)),
    };

    private static string? OptionValue(Microsoft.FSharp.Core.FSharpOption<string> value) =>
        value is null ? null : value.Value;

    private static string? NormalizeFingerprint(string? value)
    {
        if (value is null)
        {
            return null;
        }
        if (value.Length == 64 && value.All(char.IsAsciiHexDigit))
        {
            return $"sha256:{value.ToLowerInvariant()}";
        }
        if (value.Length == 71
            && value.StartsWith("sha256:", StringComparison.OrdinalIgnoreCase)
            && value.AsSpan(7).ToString().All(char.IsAsciiHexDigit))
        {
            return value.ToLowerInvariant();
        }
        return Hashing.Sha256(Encoding.UTF8.GetBytes(value));
    }

    private static string Kebab(string value)
    {
        var result = new System.Text.StringBuilder(value.Length + 4);
        for (var index = 0; index < value.Length; index++)
        {
            var character = value[index];
            if (index > 0 && char.IsUpper(character))
            {
                result.Append('-');
            }
            result.Append(char.ToLowerInvariant(character));
        }
        return result.ToString();
    }
}
