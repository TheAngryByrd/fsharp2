using System.Text.Json;
using System.Text.Json.Nodes;

namespace FSharp2.Conformance;

internal static class BundleRunResultSeal
{
    public const string Placeholder = "sha256:0000000000000000000000000000000000000000000000000000000000000000";

    public static bool IsSealable(JsonElement runResult) =>
        TryGetBundleHash(runResult, out var value)
        && string.Equals(value, Placeholder, StringComparison.Ordinal);

    public static byte[] HashBytes(JsonElement runResult)
    {
        var normalized = ReplaceBundleHash(runResult, Placeholder);
        return CanonicalJson.Canonicalize(normalized);
    }

    public static byte[] Seal(JsonElement runResult, string bundleHash)
    {
        var sealedResult = ReplaceBundleHash(runResult, bundleHash);
        return CanonicalJson.Canonicalize(sealedResult);
    }

    public static string VerifyAndHash(string path, string bundleHash)
    {
        using var document = JsonDocument.Parse(File.ReadAllBytes(path));
        if (!TryGetBundleHash(document.RootElement, out var recordedHash)
            || !string.Equals(recordedHash, bundleHash, StringComparison.Ordinal))
        {
            throw new ConformanceContractException(
                [new(
                    "bundle-seal",
                    "run-result.json/verdict/bundleHash",
                    $"The sealed run result does not name bundle hash '{bundleHash}'.")]);
        }
        return Hashing.Sha256(HashBytes(document.RootElement));
    }

    private static bool TryGetBundleHash(JsonElement runResult, out string? value)
    {
        value = null;
        return runResult.ValueKind == JsonValueKind.Object
               && runResult.TryGetProperty("verdict", out var verdict)
               && verdict.ValueKind == JsonValueKind.Object
               && verdict.TryGetProperty("bundleHash", out var bundleHash)
               && bundleHash.ValueKind == JsonValueKind.String
               && (value = bundleHash.GetString()) is not null;
    }

    private static JsonElement ReplaceBundleHash(JsonElement runResult, string bundleHash)
    {
        var root = JsonNode.Parse(runResult.GetRawText())?.AsObject()
                   ?? throw new JsonException("The run result must be a JSON object.");
        var verdict = root["verdict"]?.AsObject()
                      ?? throw new JsonException("The run result verdict must be a JSON object.");
        verdict["bundleHash"] = bundleHash;
        return JsonSerializer.SerializeToElement(root);
    }
}
