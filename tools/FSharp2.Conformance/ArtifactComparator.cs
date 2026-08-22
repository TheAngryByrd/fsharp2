using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class ArtifactComparator
{
    public static ComparisonResult Compare(JsonElement expected, JsonElement actual, JsonElement rule)
    {
        var comparisonClass = ComparisonSupport.RuleClass(rule, "exact");
        var expectedRaw = Encoding.UTF8.GetBytes(expected.GetRawText());
        var actualRaw = Encoding.UTF8.GetBytes(actual.GetRawText());
        var expectedBytes = Normalize(expected, rule);
        var actualBytes = Normalize(actual, rule);
        var passed = expectedBytes.AsSpan().SequenceEqual(actualBytes);
        return ComparisonSupport.Create(
            comparisonClass == "normalized" ? "path-newline-normalized" : "artifact-exact",
            comparisonClass,
            ComparisonSupport.RuleId(rule, comparisonClass == "normalized" ? "normalized-v1" : "exact-v1"),
            passed,
            passed ? null : ComparisonSupport.HashDifference(expectedBytes, actualBytes),
            expectedRaw,
            actualRaw,
            expectedBytes,
            actualBytes);
    }

    private static byte[] Normalize(JsonElement value, JsonElement rule)
    {
        var pathMap = ReadPathMap(rule);
        var normalizeNewlines =
            rule.ValueKind == JsonValueKind.Object
            && rule.TryGetProperty("newline", out var newline)
            && newline.ValueKind == JsonValueKind.String
            && string.Equals(newline.GetString(), "lf", StringComparison.Ordinal);
        using var stream = new MemoryStream();
        using (var writer = new Utf8JsonWriter(stream))
        {
            Write(writer, value, pathMap, normalizeNewlines);
        }
        return stream.ToArray();
    }

    private static KeyValuePair<string, string>[] ReadPathMap(JsonElement rule)
    {
        if (rule.ValueKind != JsonValueKind.Object
            || !rule.TryGetProperty("pathMap", out var pathMap)
            || pathMap.ValueKind != JsonValueKind.Object)
        {
            return [];
        }

        return [.. pathMap.EnumerateObject()
            .Where(static item => item.Value.ValueKind == JsonValueKind.String)
            .Select(static item => new KeyValuePair<string, string>(item.Name, item.Value.GetString()!))
            .OrderByDescending(static item => item.Key.Length)
            .ThenBy(static item => item.Key, StringComparer.Ordinal)];
    }

    private static void Write(
        Utf8JsonWriter writer,
        JsonElement value,
        KeyValuePair<string, string>[] pathMap,
        bool normalizeNewlines)
    {
        switch (value.ValueKind)
        {
            case JsonValueKind.Object:
                writer.WriteStartObject();
                foreach (var property in value.EnumerateObject()
                    .Select(item => new
                    {
                        Name = NormalizeString(item.Name, pathMap, normalizeNewlines),
                        item.Value,
                    })
                    .OrderBy(static item => item.Name, StringComparer.Ordinal))
                {
                    writer.WritePropertyName(property.Name);
                    Write(writer, property.Value, pathMap, normalizeNewlines);
                }
                writer.WriteEndObject();
                break;
            case JsonValueKind.Array:
                writer.WriteStartArray();
                foreach (var item in value.EnumerateArray())
                {
                    Write(writer, item, pathMap, normalizeNewlines);
                }
                writer.WriteEndArray();
                break;
            case JsonValueKind.String:
                writer.WriteStringValue(NormalizeString(value.GetString()!, pathMap, normalizeNewlines));
                break;
            case JsonValueKind.Number:
                writer.WriteRawValue(value.GetRawText(), skipInputValidation: true);
                break;
            case JsonValueKind.True:
                writer.WriteBooleanValue(true);
                break;
            case JsonValueKind.False:
                writer.WriteBooleanValue(false);
                break;
            case JsonValueKind.Null:
                writer.WriteNullValue();
                break;
            default:
                throw new JsonException($"Unsupported JSON value kind '{value.ValueKind}'.");
        }
    }

    private static string NormalizeString(
        string value,
        KeyValuePair<string, string>[] pathMap,
        bool normalizeNewlines)
    {
        var result = normalizeNewlines ? value.Replace("\r\n", "\n", StringComparison.Ordinal) : value;
        foreach (var mapping in pathMap)
        {
            var target = mapping.Value.TrimEnd('/', '\\');
            result = result.Replace(mapping.Key.TrimEnd('/', '\\') + "\\", target + "/", StringComparison.Ordinal);
            result = result.Replace(mapping.Key.TrimEnd('/', '\\') + "/", target + "/", StringComparison.Ordinal);
            result = result.Replace(mapping.Key, mapping.Value, StringComparison.Ordinal);
        }
        return result;
    }
}
