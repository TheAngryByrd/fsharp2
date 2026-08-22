using System.Collections.Immutable;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class BundleWriter
{
    public static BundleReceipt Write(string runRoot, BundleInput input)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(runRoot);
        ArgumentNullException.ThrowIfNull(input);
        ArgumentException.ThrowIfNullOrWhiteSpace(input.RunId);
        var fullRunRoot = Path.GetFullPath(runRoot);
        var bundleRoot = Path.Combine(fullRunRoot, "bundle");
        if (Directory.Exists(bundleRoot)
            && Directory.EnumerateFileSystemEntries(bundleRoot).Any())
        {
            throw new ConformanceContractException(
                [new("bundle-exists", bundleRoot, "The bundle output directory is not empty.")]);
        }
        Directory.CreateDirectory(bundleRoot);

        var fileHashes = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);
        var fileLengths = ImmutableDictionary.CreateBuilder<string, long>(StringComparer.Ordinal);
        if (input.Files is not null)
        {
            foreach (var item in input.Files.OrderBy(static item => item.Key, StringComparer.Ordinal))
            {
                WritePayload(bundleRoot, item.Key, item.Value.AsSpan(), fileHashes, fileLengths);
            }
        }

        var sealableRunResult = BundleRunResultSeal.IsSealable(input.RunResult);
        var runResultHashBytes = sealableRunResult
            ? BundleRunResultSeal.HashBytes(input.RunResult)
            : CanonicalJson.Canonicalize(input.RunResult);
        var runResultPath = BundlePath.Resolve(bundleRoot, "run-result.json");
        fileHashes["run-result.json"] = Hashing.Sha256(runResultHashBytes);
        fileLengths["run-result.json"] = runResultHashBytes.Length;

        var bundle = JsonSerializer.SerializeToElement(new
        {
            schemaVersion = 1,
            bundleId = $"bundle:{input.RunId}",
            runId = input.RunId,
            parentRunId = input.ParentRunId,
            caseId = CaseId(input),
            createdAt = DateTimeOffset.UtcNow,
            requiredPaths = fileHashes.Keys.OrderBy(static path => path, StringComparer.Ordinal),
            files = fileHashes
                .OrderBy(static item => item.Key, StringComparer.Ordinal)
                .Select(item => new
                {
                    path = item.Key,
                    sha256 = item.Value,
                    byteLength = fileLengths[item.Key],
                    required = true,
                }),
        });
        var bundleBytes = CanonicalJson.Canonicalize(bundle);
        var bundleHash = Hashing.Sha256(bundleBytes);
        var runResultBytes = sealableRunResult
            ? BundleRunResultSeal.Seal(input.RunResult, bundleHash)
            : runResultHashBytes;
        File.WriteAllBytes(runResultPath, runResultBytes);
        if (runResultBytes.Length != fileLengths["run-result.json"])
        {
            throw new ConformanceContractException(
                [new("bundle-seal", "run-result.json", "The sealed run result byte length changed.")]);
        }
        File.WriteAllBytes(Path.Combine(bundleRoot, "bundle.json"), bundleBytes);
        File.WriteAllText(
            Path.Combine(bundleRoot, "bundle.sha256"),
            bundleHash + "\n",
            new UTF8Encoding(encoderShouldEmitUTF8Identifier: false));
        return new BundleReceipt(bundleRoot, bundleHash, fileHashes.ToImmutable());
    }

    private static void WritePayload(
        string bundleRoot,
        string relativePath,
        ReadOnlySpan<byte> bytes,
        ImmutableDictionary<string, string>.Builder fileHashes,
        ImmutableDictionary<string, long>.Builder fileLengths)
    {
        var normalizedPath = BundlePath.Normalize(relativePath);
        if (normalizedPath is "bundle.json" or "bundle.sha256"
            || fileHashes.ContainsKey(normalizedPath))
        {
            throw new ConformanceContractException(
                [new("bundle-path", relativePath, "The bundle payload path is reserved or duplicated.")]);
        }
        var path = BundlePath.Resolve(bundleRoot, normalizedPath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllBytes(path, bytes);
        var hash = Hashing.Sha256(bytes);
        var writtenHash = Hashing.Sha256File(path);
        if (!string.Equals(hash, writtenHash, StringComparison.Ordinal))
        {
            throw new ConformanceContractException(
                [new("bundle-file-hash", normalizedPath, $"The written hash is '{writtenHash}'. Expected '{hash}'.")]);
        }
        fileHashes[normalizedPath] = hash;
        fileLengths[normalizedPath] = bytes.Length;
    }

    private static string CaseId(BundleInput input) =>
        input.RunResult.ValueKind == JsonValueKind.Object
        && input.RunResult.TryGetProperty("identity", out var identity)
        && identity.ValueKind == JsonValueKind.Object
        && identity.TryGetProperty("caseId", out var caseId)
        && caseId.ValueKind == JsonValueKind.String
        && !string.IsNullOrWhiteSpace(caseId.GetString())
            ? caseId.GetString()!
            : input.RunId;
}
