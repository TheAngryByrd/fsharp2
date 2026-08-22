using System.Collections.Immutable;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class DiagnosticComparator
{
    public static ComparisonResult Compare(JsonElement expected, JsonElement actual, JsonElement rule)
    {
        var expectedBytes = CanonicalJson.Canonicalize(expected);
        var actualBytes = CanonicalJson.Canonicalize(actual);
        var expectedRaw = Encoding.UTF8.GetBytes(expected.GetRawText());
        var actualRaw = Encoding.UTF8.GetBytes(actual.GetRawText());
        var passed = expectedBytes.AsSpan().SequenceEqual(actualBytes);
        return ComparisonSupport.Create(
            "diagnostic-exact",
            "exact",
            ComparisonSupport.RuleId(rule, "exact-v1"),
            passed,
            passed ? null : ComparisonSupport.HashDifference(expectedBytes, actualBytes),
            expectedRaw,
            actualRaw,
            expectedBytes,
            actualBytes);
    }

    public static ComparisonResult CompareStreams(
        int expectedExitCode,
        ImmutableArray<byte> expectedStandardOutput,
        ImmutableArray<byte> expectedStandardError,
        int actualExitCode,
        ImmutableArray<byte> actualStandardOutput,
        ImmutableArray<byte> actualStandardError,
        JsonElement rule)
    {
        var expected = JsonSerializer.SerializeToElement(new
        {
            exitCode = expectedExitCode,
            standardOutput = ExtractDiagnosticText(expectedStandardOutput),
            standardError = ExtractDiagnosticText(expectedStandardError),
        });
        var actual = JsonSerializer.SerializeToElement(new
        {
            exitCode = actualExitCode,
            standardOutput = ExtractDiagnosticText(actualStandardOutput),
            standardError = ExtractDiagnosticText(actualStandardError),
        });
        return ArtifactComparator.Compare(expected, actual, rule);
    }

    private static string ExtractDiagnosticText(ImmutableArray<byte> stream)
    {
        var text = Encoding.UTF8.GetString(stream.AsSpan());
        return string.Join(
            "\n",
            text.Replace("\r\n", "\n", StringComparison.Ordinal)
                .Split('\n')
                .Where(ContainsDiagnosticCode));
    }

    private static bool ContainsDiagnosticCode(string value)
    {
        for (var index = 0; index + 5 < value.Length; index++)
        {
            if (value[index] == 'F'
                && value[index + 1] == 'S'
                && char.IsAsciiDigit(value[index + 2])
                && char.IsAsciiDigit(value[index + 3])
                && char.IsAsciiDigit(value[index + 4])
                && char.IsAsciiDigit(value[index + 5]))
            {
                return true;
            }
        }
        return false;
    }
}
