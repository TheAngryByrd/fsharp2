using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class BehaviorComparator
{
    public static ComparisonResult Compare(
        BehaviorObservation expected,
        BehaviorObservation actual)
    {
        var differences = new List<string>();
        if (expected.ExitCode != actual.ExitCode)
        {
            differences.Add("exit code");
        }
        if (!string.Equals(expected.StandardOutput, actual.StandardOutput, StringComparison.Ordinal))
        {
            differences.Add("standard output");
        }
        if (!string.Equals(expected.StandardError, actual.StandardError, StringComparison.Ordinal))
        {
            differences.Add("standard error");
        }
        AddStructuredDifferences(expected.StructuredValue, actual.StructuredValue, differences);
        if (!string.Equals(expected.Exception, actual.Exception, StringComparison.Ordinal))
        {
            differences.Add("exception");
        }

        var passed = differences.Count == 0;
        var expectedValue = JsonSerializer.SerializeToElement(expected);
        var actualValue = JsonSerializer.SerializeToElement(actual);
        var expectedRaw = Encoding.UTF8.GetBytes(expectedValue.GetRawText());
        var actualRaw = Encoding.UTF8.GetBytes(actualValue.GetRawText());
        var expectedCanonical = CanonicalJson.Canonicalize(expectedValue);
        var actualCanonical = CanonicalJson.Canonicalize(actualValue);
        return ComparisonSupport.Create(
            "runtime-behavior",
            "behavioral",
            "behavior-v1",
            passed,
            passed ? null : $"Behavior changed: {string.Join(", ", differences)}.",
            expectedRaw,
            actualRaw,
            expectedCanonical,
            actualCanonical);
    }

    private static void AddStructuredDifferences(
        JsonElement expected,
        JsonElement actual,
        ICollection<string> differences)
    {
        if (expected.ValueKind != JsonValueKind.Object || actual.ValueKind != JsonValueKind.Object)
        {
            if (!ComparisonSupport.JsonEquals(expected, actual))
            {
                differences.Add("structured value");
            }
            return;
        }

        CompareProperty(expected, actual, "returnValue", "return value", differences);
        CompareProperty(expected, actual, "resources", "resources", differences);
        CompareProperty(expected, actual, "debugger", "debugger observations", differences);
        CompareProperty(expected, actual, "debuggerObservations", "debugger observations", differences);

        var excluded = new HashSet<string>(
            ["returnValue", "resources", "debugger", "debuggerObservations"],
            StringComparer.Ordinal);
        var expectedRemainder = WithoutProperties(expected, excluded);
        var actualRemainder = WithoutProperties(actual, excluded);
        if (!ComparisonSupport.JsonEquals(expectedRemainder, actualRemainder))
        {
            differences.Add("structured value");
        }
    }

    private static void CompareProperty(
        JsonElement expected,
        JsonElement actual,
        string propertyName,
        string difference,
        ICollection<string> differences)
    {
        var hasExpected = expected.TryGetProperty(propertyName, out var expectedValue);
        var hasActual = actual.TryGetProperty(propertyName, out var actualValue);
        if (hasExpected != hasActual
            || hasExpected && !ComparisonSupport.JsonEquals(expectedValue, actualValue))
        {
            differences.Add(difference);
        }
    }

    private static JsonElement WithoutProperties(JsonElement value, ISet<string> excluded)
    {
        using var stream = new MemoryStream();
        using (var writer = new Utf8JsonWriter(stream))
        {
            writer.WriteStartObject();
            foreach (var property in value.EnumerateObject())
            {
                if (!excluded.Contains(property.Name))
                {
                    property.WriteTo(writer);
                }
            }
            writer.WriteEndObject();
        }
        using var document = JsonDocument.Parse(stream.ToArray());
        return document.RootElement.Clone();
    }
}
