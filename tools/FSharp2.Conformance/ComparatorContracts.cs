using System.Collections.Immutable;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public sealed record ComparisonResult(
    string ComparatorId,
    int Version,
    string Class,
    string Rule,
    bool Passed,
    string? Difference)
{
    public ImmutableArray<string> RawHashes { get; init; } = [];
    public ImmutableArray<string> CanonicalValues { get; init; } = [];
}

internal sealed record ComparatorContract(string Id, int Version, string Class);

public sealed record BehaviorObservation
{
    public BehaviorObservation(
        int exitCode,
        string standardOutput,
        string standardError,
        JsonElement structuredValue,
        string? exception)
    {
        ExitCode = exitCode;
        StandardOutput = standardOutput;
        StandardError = standardError;
        StructuredValue = structuredValue.Clone();
        Exception = exception;
    }

    public int ExitCode { get; }
    public string StandardOutput { get; }
    public string StandardError { get; }
    public JsonElement StructuredValue { get; }
    public string? Exception { get; }
}

public sealed record ProbeRequest
{
    public ProbeRequest(
        string kind,
        string artifactPath,
        JsonElement configuration,
        TimeSpan timeout)
    {
        Kind = kind;
        ArtifactPath = artifactPath;
        Configuration = configuration.Clone();
        Timeout = timeout;
    }

    public string Kind { get; }
    public string ArtifactPath { get; }
    public JsonElement Configuration { get; }
    public TimeSpan Timeout { get; }
}

public sealed record ProbeEvidence
{
    public ProbeEvidence(
        string kind,
        bool passed,
        JsonElement observation,
        string? difference)
    {
        Kind = kind;
        Passed = passed;
        Observation = observation.Clone();
        Difference = difference;
    }

    public string Kind { get; }
    public bool Passed { get; }
    public JsonElement Observation { get; }
    public string? Difference { get; }
}

public sealed record VerdictInput(
    bool DeclaredUnsupported,
    bool InfrastructureFailed,
    bool FallbackDetected,
    bool PublishedSuccessfulArtifact,
    ImmutableArray<ComparisonResult> Comparisons,
    ImmutableArray<ProbeEvidence> Probes,
    ImmutableArray<string> MissingEvidence);

public sealed record VerdictResult(
    ConformanceVerdict Verdict,
    ImmutableArray<string> Reasons,
    ImmutableArray<string> MissingEvidence,
    bool CountsAsCoverage);

internal static class ComparisonSupport
{
    public static ImmutableArray<ComparatorContract> BuiltInComparatorContracts { get; } =
    [
        new("diagnostic-exact", 1, "exact"),
        new("path-newline-normalized", 1, "normalized"),
        new("managed-metadata-semantic", 1, "canonical-semantic"),
        new("portable-pdb-semantic", 1, "canonical-semantic"),
        new("runtime-behavior", 1, "behavioral"),
        new("artifact-repeat", 1, "within-compiler-exact"),
    ];

    public static ComparisonResult Create(
        string comparatorId,
        string comparisonClass,
        string rule,
        bool passed,
        string? difference) =>
        new(comparatorId, 1, comparisonClass, rule, passed, difference);

    public static ComparisonResult Create(
        string comparatorId,
        string comparisonClass,
        string rule,
        bool passed,
        string? difference,
        ReadOnlySpan<byte> expectedRaw,
        ReadOnlySpan<byte> actualRaw,
        ReadOnlySpan<byte> expectedCanonical,
        ReadOnlySpan<byte> actualCanonical) =>
        new ComparisonResult(comparatorId, 1, comparisonClass, rule, passed, difference)
        {
            RawHashes = [Hashing.Sha256(expectedRaw), Hashing.Sha256(actualRaw)],
            CanonicalValues = [
                Encoding.UTF8.GetString(expectedCanonical),
                Encoding.UTF8.GetString(actualCanonical),
            ],
        };

    public static string RuleId(JsonElement rule, string fallback) =>
        rule.ValueKind == JsonValueKind.Object
        && rule.TryGetProperty("id", out var id)
        && id.ValueKind == JsonValueKind.String
        && !string.IsNullOrWhiteSpace(id.GetString())
            ? id.GetString()!
            : fallback;

    public static string RuleClass(JsonElement rule, string fallback) =>
        rule.ValueKind == JsonValueKind.Object
        && rule.TryGetProperty("class", out var comparisonClass)
        && comparisonClass.ValueKind == JsonValueKind.String
        && !string.IsNullOrWhiteSpace(comparisonClass.GetString())
            ? comparisonClass.GetString()!
            : fallback;

    public static bool JsonEquals(JsonElement expected, JsonElement actual) =>
        CanonicalJson.Canonicalize(expected).AsSpan()
            .SequenceEqual(CanonicalJson.Canonicalize(actual));

    public static string HashDifference(ReadOnlySpan<byte> expected, ReadOnlySpan<byte> actual) =>
        $"Expected {Hashing.Sha256(expected)}, but observed {Hashing.Sha256(actual)}.";

    public static string TextDifference(
        IEnumerable<string> missing,
        IEnumerable<string> unexpected)
    {
        var missingValues = string.Join(", ", missing);
        var unexpectedValues = string.Join(", ", unexpected);
        var difference = new StringBuilder();
        if (missingValues.Length > 0)
        {
            difference.Append("Missing: ").Append(missingValues).Append('.');
        }
        if (unexpectedValues.Length > 0)
        {
            if (difference.Length > 0)
            {
                difference.Append(' ');
            }
            difference.Append("Unexpected: ").Append(unexpectedValues).Append('.');
        }
        return difference.ToString();
    }
}
