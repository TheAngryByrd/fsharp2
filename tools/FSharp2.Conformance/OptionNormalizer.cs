using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class OptionNormalizer
{
    public static NormalizedOptionGroups Normalize(JsonElement options)
    {
        if (options.ValueKind != JsonValueKind.Object)
        {
            throw Invalid("The options value must be an object.");
        }

        return new NormalizedOptionGroups(
            CloneGroup(options, "semantic"),
            CloneGroup(options, "diagnostic"),
            CloneGroup(options, "emission"),
            CloneGroup(options, "signing"),
            CloneGroup(options, "resources"),
            options
                .GetProperty("compilerArguments")
                .EnumerateArray()
                .Select(static item => item.GetString()!)
                .ToImmutableArray());
    }

    private static JsonElement CloneGroup(JsonElement options, string name)
    {
        if (!options.TryGetProperty(name, out var group) || group.ValueKind != JsonValueKind.Object)
        {
            throw Invalid($"The options value must contain the '{name}' object.");
        }

        return group.Clone();
    }

    private static ConformanceContractException Invalid(string message) =>
        new([new("options", "options", message)]);
}
