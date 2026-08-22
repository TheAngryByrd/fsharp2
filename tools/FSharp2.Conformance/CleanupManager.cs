using System.Collections.Immutable;
using System.Diagnostics;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class CleanupManager
{
    public static Task<CleanupReceipt> CleanupAsync(
        string runRoot,
        ImmutableArray<ProcessObservation> recordedDescendants,
        CleanupPolicy policy,
        CancellationToken cancellationToken) =>
        CleanupCoreAsync(runRoot, recordedDescendants, policy, cancellationToken);

    public static Task<CleanupReceipt> CleanupAsync(
        string runRoot,
        ImmutableArray<int> recordedDescendantProcessIds,
        CleanupPolicy policy,
        CancellationToken cancellationToken)
    {
        if (!recordedDescendantProcessIds.IsDefaultOrEmpty)
        {
            throw new ConformanceContractException(
                [new("cleanup-process-identity", "processes", "Cleanup requires the recorded executable path and start time for each process.")]);
        }
        return CleanupCoreAsync(runRoot, [], policy, cancellationToken);
    }

    private static async Task<CleanupReceipt> CleanupCoreAsync(
        string runRoot,
        ImmutableArray<ProcessObservation> recordedDescendants,
        CleanupPolicy policy,
        CancellationToken cancellationToken)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(runRoot);
        ArgumentNullException.ThrowIfNull(policy);
        if (policy.Timeout <= TimeSpan.Zero)
        {
            throw new ArgumentOutOfRangeException(nameof(policy), "The cleanup timeout must be positive.");
        }

        var fullRoot = Path.GetFullPath(runRoot)
            .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        RejectReparsePoint(fullRoot, fullRoot);
        ValidateMarker(fullRoot);
        var tree = ReadTree(fullRoot);
        var retainedRelativePaths = NormalizeRetainedPaths(fullRoot, policy.RetainRelativePaths);
        var killedProcessIds = ImmutableArray.CreateBuilder<int>();
        var retainedPaths = ImmutableArray.CreateBuilder<string>();
        var removedPaths = ImmutableArray.CreateBuilder<string>();
        var openHandles = ImmutableArray.CreateBuilder<string>();
        using var timeout = new CancellationTokenSource(policy.Timeout);
        using var linked = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeout.Token);
        await StopProcessesAsync(
            recordedDescendants,
            killedProcessIds,
            linked.Token).ConfigureAwait(false);

        foreach (var path in tree.Files.OrderBy(static value => value, PathComparer()))
        {
            linked.Token.ThrowIfCancellationRequested();
            var relativePath = RelativePath(fullRoot, path);
            if (IsRetained(relativePath, retainedRelativePaths))
            {
                retainedPaths.Add(relativePath);
                continue;
            }
            try
            {
                File.Delete(path);
                removedPaths.Add(relativePath);
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                openHandles.Add($"{relativePath}: {exception.Message}");
            }
        }

        foreach (var path in tree.Directories
                     .OrderByDescending(static value => value.Length)
                     .ThenBy(static value => value, PathComparer()))
        {
            linked.Token.ThrowIfCancellationRequested();
            try
            {
                if (!Directory.EnumerateFileSystemEntries(path).Any())
                {
                    Directory.Delete(path);
                    removedPaths.Add(RelativePath(fullRoot, path) + "/");
                }
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                openHandles.Add($"{RelativePath(fullRoot, path)}/: {exception.Message}");
            }
        }

        if (policy.DeleteRunRoot && retainedRelativePaths.Length == 0 && Directory.Exists(fullRoot))
        {
            try
            {
                Directory.Delete(fullRoot, recursive: false);
                removedPaths.Add(".");
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                openHandles.Add($".: {exception.Message}");
            }
        }

        var remainingPaths = ImmutableArray<string>.Empty;
        if (Directory.Exists(fullRoot))
        {
            var remainingTree = ReadTree(fullRoot);
            remainingPaths = remainingTree.Files.Concat(remainingTree.Directories)
                .Select(path => RelativePath(fullRoot, path))
                .OrderBy(static value => value, StringComparer.Ordinal)
                .ToImmutableArray();
        }
        return new CleanupReceipt(
            fullRoot,
            killedProcessIds.ToImmutable(),
            retainedPaths.Distinct(StringComparer.Ordinal).OrderBy(static value => value, StringComparer.Ordinal).ToImmutableArray(),
            removedPaths.Distinct(StringComparer.Ordinal).OrderBy(static value => value, StringComparer.Ordinal).ToImmutableArray(),
            remainingPaths,
            openHandles.ToImmutable());
    }

    private static void ValidateMarker(string runRoot)
    {
        var markerPath = Path.Combine(runRoot, ".fsharp2-conformance-root");
        if (!File.Exists(markerPath))
        {
            throw MarkerError(markerPath, "The run root ownership marker does not exist.");
        }
        try
        {
            using var marker = JsonDocument.Parse(File.ReadAllBytes(markerPath));
            if (!marker.RootElement.TryGetProperty("root", out var root)
                || root.ValueKind != JsonValueKind.String
                || !PathEquals(runRoot, Path.GetFullPath(root.GetString()!)))
            {
                throw MarkerError(markerPath, "The run root ownership marker does not match the cleanup root.");
            }
        }
        catch (JsonException exception)
        {
            throw MarkerError(markerPath, $"The run root ownership marker is invalid: {exception.Message}");
        }
    }

    private static ConformanceContractException MarkerError(string markerPath, string message) =>
        new([new("run-root-marker", markerPath, message)]);

    private static ImmutableArray<string> NormalizeRetainedPaths(
        string runRoot,
        ImmutableArray<string> retainedPaths)
    {
        if (retainedPaths.IsDefaultOrEmpty)
        {
            return [];
        }
        return [.. retainedPaths
            .Select(path =>
            {
                ArgumentException.ThrowIfNullOrWhiteSpace(path);
                var fullPath = Path.GetFullPath(Path.Combine(
                    runRoot,
                    path.Replace('/', Path.DirectorySeparatorChar)));
                EnsureUnderRoot(runRoot, fullPath, path);
                return RelativePath(runRoot, fullPath).TrimEnd('/');
            })
            .Distinct(StringComparer.Ordinal)
            .OrderBy(static value => value, StringComparer.Ordinal)];
    }

    private static async Task StopProcessesAsync(
        ImmutableArray<ProcessObservation> observations,
        ImmutableArray<int>.Builder killedProcessIds,
        CancellationToken cancellationToken)
    {
        if (observations.IsDefaultOrEmpty)
        {
            return;
        }
        var processes = ValidateProcessIdentities(observations);
        try
        {
            foreach (var process in processes)
            {
                cancellationToken.ThrowIfCancellationRequested();
                if (process.HasExited)
                {
                    continue;
                }
                process.Kill(entireProcessTree: true);
                await process.WaitForExitAsync(cancellationToken).ConfigureAwait(false);
                killedProcessIds.Add(process.Id);
            }
        }
        finally
        {
            foreach (var process in processes)
            {
                process.Dispose();
            }
        }
    }

    private static List<Process> ValidateProcessIdentities(
        ImmutableArray<ProcessObservation> observations)
    {
        var processes = new List<Process>();
        try
        {
            foreach (var observation in observations
                         .DistinctBy(static item => item.ProcessId)
                         .OrderBy(static item => item.ProcessId))
            {
                if (observation.ProcessId <= 0
                    || observation.ProcessId == Environment.ProcessId
                    || !observation.StartedAt.HasValue
                    || string.IsNullOrWhiteSpace(observation.ExecutablePath))
                {
                    throw ProcessIdentityError(observation.ProcessId, "The recorded process identity is incomplete.");
                }
                Process process;
                try
                {
                    process = Process.GetProcessById(observation.ProcessId);
                }
                catch (ArgumentException)
                {
                    continue;
                }
                try
                {
                    if (process.HasExited)
                    {
                        process.Dispose();
                        continue;
                    }
                    var actualStartedAt = new DateTimeOffset(process.StartTime).ToUniversalTime();
                    var actualExecutablePath = process.MainModule?.FileName ?? process.ProcessName;
                    if (actualStartedAt.UtcTicks != observation.StartedAt.Value.ToUniversalTime().UtcTicks
                        || !PathEquals(actualExecutablePath, observation.ExecutablePath))
                    {
                        process.Dispose();
                        throw ProcessIdentityError(observation.ProcessId, "The running process does not match the recorded executable path and start time.");
                    }
                }
                catch (Exception exception) when (
                    exception is ArgumentException
                    or InvalidOperationException
                    or System.ComponentModel.Win32Exception)
                {
                    process.Dispose();
                    throw ProcessIdentityError(observation.ProcessId, $"The recorded process identity could not be verified: {exception.Message}");
                }
                processes.Add(process);
            }
            return processes;
        }
        catch
        {
            foreach (var process in processes)
            {
                process.Dispose();
            }
            throw;
        }
    }

    private static ConformanceContractException ProcessIdentityError(int processId, string message) =>
        new([new("cleanup-process-identity", processId.ToString(System.Globalization.CultureInfo.InvariantCulture), message)]);

    private static TreeSnapshot ReadTree(string root)
    {
        var files = ImmutableArray.CreateBuilder<string>();
        var directories = ImmutableArray.CreateBuilder<string>();
        var pending = new Stack<string>();
        pending.Push(root);
        while (pending.TryPop(out var directory))
        {
            foreach (var path in Directory.EnumerateFileSystemEntries(directory, "*", SearchOption.TopDirectoryOnly))
            {
                RejectReparsePoint(root, path);
                if ((File.GetAttributes(path) & FileAttributes.Directory) != 0)
                {
                    directories.Add(path);
                    pending.Push(path);
                }
                else
                {
                    files.Add(path);
                }
            }
        }
        return new TreeSnapshot(files.ToImmutable(), directories.ToImmutable());
    }

    private static void RejectReparsePoint(string root, string path)
    {
        if ((File.GetAttributes(path) & FileAttributes.ReparsePoint) != 0)
        {
            throw new ConformanceContractException(
                [new("cleanup-reparse-point", RelativePath(root, path), "Cleanup does not traverse reparse points.")]);
        }
    }

    private static bool IsRetained(string path, ImmutableArray<string> retainedPaths) =>
        retainedPaths.Any(retained =>
            path.Equals(retained, PathComparison())
            || path.StartsWith(retained + "/", PathComparison()));

    private static void EnsureUnderRoot(string root, string path, string declaredPath)
    {
        var prefix = root + Path.DirectorySeparatorChar;
        if (!path.StartsWith(prefix, PathComparison()))
        {
            throw new ConformanceContractException(
                [new("cleanup-path", declaredPath, "The retained path is outside the run root.")]);
        }
    }

    private static string RelativePath(string root, string path) =>
        Path.GetRelativePath(root, path).Replace('\\', '/');

    private static bool PathEquals(string first, string second) =>
        first.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
            .Equals(
                second.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar),
                PathComparison());

    private static StringComparison PathComparison() =>
        OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;

    private static StringComparer PathComparer() =>
        OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal;

    private sealed record TreeSnapshot(
        ImmutableArray<string> Files,
        ImmutableArray<string> Directories);
}
