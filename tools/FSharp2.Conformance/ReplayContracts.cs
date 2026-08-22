using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public sealed record CleanupPolicy(
    bool DeleteRunRoot,
    ImmutableArray<string> RetainRelativePaths,
    TimeSpan Timeout);

public sealed record CleanupReceipt(
    string RunRoot,
    ImmutableArray<int> KilledProcessIds,
    ImmutableArray<string> RetainedPaths,
    ImmutableArray<string> RemovedPaths,
    ImmutableArray<string> RemainingPaths,
    ImmutableArray<string> OpenHandles);

public sealed record BundleInput
{
    public BundleInput(
        string runId,
        string? parentRunId,
        ImmutableDictionary<string, ImmutableArray<byte>> files,
        JsonElement runResult)
    {
        ArgumentNullException.ThrowIfNull(files);
        RunId = runId;
        ParentRunId = parentRunId;
        Files = files;
        RunResult = runResult.Clone();
    }

    public string RunId { get; }
    public string? ParentRunId { get; }
    public ImmutableDictionary<string, ImmutableArray<byte>> Files { get; }
    public JsonElement RunResult { get; }
}

public sealed record BundleReceipt(
    string BundleRoot,
    string BundleHash,
    ImmutableDictionary<string, string> FileHashes);

public sealed record VerifiedBundle
{
    public VerifiedBundle(
        string bundleRoot,
        string runId,
        string? parentRunId,
        string bundleHash,
        ImmutableDictionary<string, string> fileHashes,
        JsonElement bundle)
    {
        BundleRoot = bundleRoot;
        RunId = runId;
        ParentRunId = parentRunId;
        BundleHash = bundleHash;
        FileHashes = fileHashes;
        Bundle = bundle.Clone();
    }

    public string BundleRoot { get; }
    public string RunId { get; }
    public string? ParentRunId { get; }
    public string BundleHash { get; }
    public ImmutableDictionary<string, string> FileHashes { get; }
    public JsonElement Bundle { get; }
}

public sealed record ReplayReceipt(
    string RunId,
    string ParentRunId,
    string OutputRoot,
    string BundleHash);

internal static class BundlePath
{
    public static string Normalize(string path)
    {
        if (string.IsNullOrWhiteSpace(path))
        {
            throw new ConformanceContractException(
                [new("bundle-path", path ?? string.Empty, "The bundle path must contain text.")]);
        }
        var normalized = path.Replace('\\', '/').Trim('/');
        if (normalized.Length == 0
            || Path.IsPathRooted(path)
            || normalized.Split('/').Any(static segment => segment is "" or "." or ".."))
        {
            throw new ConformanceContractException(
                [new("bundle-path", path, "The bundle path must be a relative path below the bundle root.")]);
        }
        return normalized;
    }

    public static StringComparer Comparer =>
        OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal;

    public static string Resolve(string bundleRoot, string relativePath)
    {
        var fullRoot = Path.GetFullPath(bundleRoot)
            .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        var fullPath = Path.GetFullPath(Path.Combine(
            fullRoot,
            relativePath.Replace('/', Path.DirectorySeparatorChar)));
        var comparison = OperatingSystem.IsWindows()
            ? StringComparison.OrdinalIgnoreCase
            : StringComparison.Ordinal;
        if (!fullPath.StartsWith(fullRoot + Path.DirectorySeparatorChar, comparison))
        {
            throw new ConformanceContractException(
                [new("bundle-path", relativePath, "The bundle path is outside the bundle root.")]);
        }
        return fullPath;
    }
}
