using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class BundleReader
{
    public static VerifiedBundle ReadAndVerify(string bundleRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(bundleRoot);
        var fullRoot = Path.GetFullPath(bundleRoot);
        var bundlePath = Path.Combine(fullRoot, "bundle.json");
        var bundleHashPath = Path.Combine(fullRoot, "bundle.sha256");
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        if (!File.Exists(bundlePath))
        {
            issues.Add(new("bundle-file", "bundle.json", "The bundle manifest does not exist."));
        }
        if (!File.Exists(bundleHashPath))
        {
            issues.Add(new("bundle-file", "bundle.sha256", "The bundle manifest hash does not exist."));
        }
        if (issues.Count > 0)
        {
            throw new ConformanceContractException(issues.ToImmutable());
        }

        JsonElement bundle;
        try
        {
            using var document = JsonDocument.Parse(File.ReadAllBytes(bundlePath));
            bundle = document.RootElement.Clone();
        }
        catch (JsonException exception)
        {
            throw new ConformanceContractException(
                [new("bundle-json", "bundle.json", $"The bundle manifest is invalid: {exception.Message}")]);
        }
        issues.AddRange(BundleSchema.Validate(bundle).Issues);
        var actualBundleHash = Hashing.Sha256(CanonicalJson.Canonicalize(bundle));
        var expectedBundleHash = File.ReadAllText(bundleHashPath).Trim();
        if (!string.Equals(actualBundleHash, expectedBundleHash, StringComparison.Ordinal))
        {
            issues.Add(new(
                "bundle-hash",
                "bundle.json",
                $"The bundle hash is '{actualBundleHash}'. Expected '{expectedBundleHash}'."));
        }

        var runId = RequireString(bundle, "runId", issues);
        string? parentRunId = null;
        if (bundle.ValueKind == JsonValueKind.Object
            && bundle.TryGetProperty("parentRunId", out var parent)
            && parent.ValueKind == JsonValueKind.String)
        {
            parentRunId = parent.GetString();
        }
        var requiredPaths = ReadRequiredPaths(bundle, issues);
        var recordedPaths = new HashSet<string>(BundlePath.Comparer);
        var fileHashes = ImmutableDictionary.CreateBuilder<string, string>(BundlePath.Comparer);
        if (bundle.ValueKind != JsonValueKind.Object
            || !bundle.TryGetProperty("files", out var files)
            || files.ValueKind != JsonValueKind.Array)
        {
            issues.Add(new("bundle-files", "files", "The bundle file hash map is missing."));
        }
        else
        {
            foreach (var file in ReadFiles(files, issues))
            {
                var expectedHash = file.Sha256;
                if (string.IsNullOrWhiteSpace(expectedHash))
                {
                    issues.Add(new("bundle-file-hash", file.Path, "The recorded file hash is invalid."));
                    continue;
                }
                try
                {
                    var normalizedPath = BundlePath.Normalize(file.Path);
                    if (!recordedPaths.Add(normalizedPath))
                    {
                        issues.Add(new(
                            "bundle-path-duplicate",
                            normalizedPath,
                            "The normalized bundle file path is declared more than once."));
                        continue;
                    }
                    var listedAsRequired = requiredPaths.Contains(normalizedPath);
                    if (file.Required != listedAsRequired)
                    {
                        issues.Add(new(
                            "bundle-file-required",
                            normalizedPath,
                            $"The file required value is '{file.Required}', expected '{listedAsRequired}'."));
                    }
                    var path = BundlePath.Resolve(fullRoot, normalizedPath);
                    if (!File.Exists(path))
                    {
                        issues.Add(new("bundle-file", normalizedPath, "The recorded bundle file does not exist."));
                        continue;
                    }
                    var actualLength = new FileInfo(path).Length;
                    if (actualLength != file.ByteLength)
                    {
                        issues.Add(new(
                            "bundle-file-length",
                            normalizedPath,
                            $"The file length is '{actualLength}', expected '{file.ByteLength}'."));
                        continue;
                    }
                    var actualHash = normalizedPath == "run-result.json"
                                     && actualLength == file.ByteLength
                        ? SealedOrRawRunResultHash(path, actualBundleHash, expectedHash)
                        : Hashing.Sha256File(path);
                    if (!string.Equals(actualHash, expectedHash, StringComparison.Ordinal))
                    {
                        issues.Add(new(
                            "bundle-file-hash",
                            normalizedPath,
                            $"The file hash is '{actualHash}'. Expected '{expectedHash}'."));
                        continue;
                    }
                    fileHashes[normalizedPath] = expectedHash;
                }
                catch (ConformanceContractException exception)
                {
                    issues.AddRange(exception.Issues);
                }
            }
        }
        foreach (var requiredPath in requiredPaths.Except(recordedPaths, BundlePath.Comparer))
        {
            issues.Add(new(
                "bundle-required-path",
                requiredPath,
                "The required bundle path does not have a file entry."));
        }
        if (issues.Count > 0)
        {
            throw new ConformanceContractException(issues.ToImmutable());
        }
        recordedPaths = fileHashes.Keys.ToHashSet(BundlePath.Comparer);
        foreach (var path in Directory.EnumerateFiles(fullRoot, "*", SearchOption.AllDirectories))
        {
            var relativePath = Path.GetRelativePath(fullRoot, path).Replace('\\', '/');
            if (relativePath is not ("bundle.json" or "bundle.sha256")
                && !recordedPaths.Contains(relativePath))
            {
                issues.Add(new("bundle-file-unrecorded", relativePath, "The bundle contains an unrecorded file."));
            }
        }
        if (issues.Count > 0)
        {
            throw new ConformanceContractException(issues.ToImmutable());
        }
        return new VerifiedBundle(
            fullRoot,
            runId!,
            parentRunId,
            actualBundleHash,
            fileHashes.ToImmutable(),
            bundle);
    }

    private static string SealedOrRawRunResultHash(
        string path,
        string bundleHash,
        string expectedHash)
    {
        var rawHash = Hashing.Sha256File(path);
        if (string.Equals(rawHash, expectedHash, StringComparison.Ordinal))
        {
            return rawHash;
        }
        try
        {
            return BundleRunResultSeal.VerifyAndHash(path, bundleHash);
        }
        catch (JsonException exception)
        {
            throw new ConformanceContractException(
                [new("bundle-json", "run-result.json", $"The sealed run result is invalid: {exception.Message}")]);
        }
    }

    private static IEnumerable<BundleFile> ReadFiles(
        JsonElement files,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        foreach (var file in files.EnumerateArray())
        {
            if (file.ValueKind != JsonValueKind.Object
                || !file.TryGetProperty("path", out var path)
                || path.ValueKind != JsonValueKind.String
                || !file.TryGetProperty("sha256", out var sha256)
                || sha256.ValueKind != JsonValueKind.String
                || !file.TryGetProperty("byteLength", out var byteLength)
                || !byteLength.TryGetInt64(out var length)
                || !file.TryGetProperty("required", out var required)
                || required.ValueKind is not (JsonValueKind.True or JsonValueKind.False))
            {
                issues.Add(new("bundle-file", "files", "A bundle file entry is invalid."));
                continue;
            }
            yield return new BundleFile(path.GetString()!, sha256.GetString(), length, required.GetBoolean());
        }
    }

    private static HashSet<string> ReadRequiredPaths(
        JsonElement bundle,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        var requiredPaths = new HashSet<string>(BundlePath.Comparer);
        if (bundle.ValueKind != JsonValueKind.Object
            || !bundle.TryGetProperty("requiredPaths", out var paths)
            || paths.ValueKind != JsonValueKind.Array)
        {
            issues.Add(new("bundle-required-path", "requiredPaths", "The required path list is missing."));
            return requiredPaths;
        }

        foreach (var path in paths.EnumerateArray())
        {
            if (path.ValueKind != JsonValueKind.String)
            {
                continue;
            }
            try
            {
                var normalizedPath = BundlePath.Normalize(path.GetString()!);
                if (!requiredPaths.Add(normalizedPath))
                {
                    issues.Add(new(
                        "bundle-required-path",
                        normalizedPath,
                        "The normalized required path is declared more than once."));
                }
            }
            catch (ConformanceContractException exception)
            {
                issues.AddRange(exception.Issues);
            }
        }
        return requiredPaths;
    }

    private static string? RequireString(
        JsonElement value,
        string name,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (value.ValueKind == JsonValueKind.Object
            && value.TryGetProperty(name, out var property)
            && property.ValueKind == JsonValueKind.String
            && !string.IsNullOrWhiteSpace(property.GetString()))
        {
            return property.GetString();
        }
        issues.Add(new("bundle-property", name, $"The bundle property '{name}' is missing."));
        return null;
    }

    private sealed record BundleFile(string Path, string? Sha256, long ByteLength, bool Required);
}
