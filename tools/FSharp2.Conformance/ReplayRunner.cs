using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class ReplayRunner
{
    private static readonly JsonSerializerOptions EvidenceJson = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        PropertyNameCaseInsensitive = true,
    };

    public static async Task<ReplayReceipt> ReplayAsync(
        VerifiedBundle bundle,
        string dotnetRoot,
        string outputRoot,
        CancellationToken cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(bundle);
        ArgumentException.ThrowIfNullOrWhiteSpace(dotnetRoot);
        ArgumentException.ThrowIfNullOrWhiteSpace(outputRoot);
        cancellationToken.ThrowIfCancellationRequested();
        var fullDotnetRoot = Path.GetFullPath(dotnetRoot);
        var dotnetPath = Path.Combine(
            fullDotnetRoot,
            OperatingSystem.IsWindows() ? "dotnet.exe" : "dotnet");
        if (!File.Exists(dotnetPath))
        {
            throw new ConformanceContractException(
                [new("dotnet-root", fullDotnetRoot, "The replay dotnet root does not contain the dotnet host.")]);
        }

        var refreshed = BundleReader.ReadAndVerify(bundle.BundleRoot);
        if (!string.Equals(refreshed.BundleHash, bundle.BundleHash, StringComparison.Ordinal))
        {
            throw new ConformanceContractException(
                [new("bundle-hash", bundle.BundleRoot, "The source bundle changed after verification.")]);
        }
        var runId = $"replay-{refreshed.BundleHash[7..23]}-{Guid.NewGuid():N}";
        var fullOutputRoot = Path.GetFullPath(outputRoot);
        Directory.CreateDirectory(fullOutputRoot);
        var replayRoot = Path.Combine(fullOutputRoot, runId);
        var roots = LaneRoots.Create(replayRoot, runId);
        var link = JsonSerializer.SerializeToElement(new
        {
            schemaVersion = 1,
            runId,
            parentRunId = refreshed.RunId,
            sourceBundleRoot = refreshed.BundleRoot,
            sourceBundleHash = refreshed.BundleHash,
            dotnetRoot = fullDotnetRoot,
        });
        await File.WriteAllBytesAsync(
            Path.Combine(replayRoot, "replay-link.json"),
            CanonicalJson.Canonicalize(link),
            cancellationToken).ConfigureAwait(false);
        await ReplayLaneAsync(
            refreshed,
            "oracle",
            LaneKind.Oracle,
            roots.Oracle,
            fullDotnetRoot,
            dotnetPath,
            replayRoot,
            cancellationToken).ConfigureAwait(false);
        await ReplayLaneAsync(
            refreshed,
            "fsharp2",
            LaneKind.FSharp2,
            roots.FSharp2,
            fullDotnetRoot,
            dotnetPath,
            replayRoot,
            cancellationToken).ConfigureAwait(false);
        return new ReplayReceipt(runId, refreshed.RunId, replayRoot, refreshed.BundleHash);
    }

    private static async Task ReplayLaneAsync(
        VerifiedBundle bundle,
        string name,
        LaneKind kind,
        LaneRoot root,
        string dotnetRoot,
        string dotnetPath,
        string replayRoot,
        CancellationToken cancellationToken)
    {
        var invocationRelativePath = $"{name}/invocation.json";
        if (!bundle.FileHashes.ContainsKey(invocationRelativePath))
        {
            return;
        }

        var recorded = ReadInvocation(bundle, invocationRelativePath);
        if (recorded.Kind != kind)
        {
            throw new ConformanceContractException(
                [new("replay-invocation", invocationRelativePath, $"The recorded lane kind is '{recorded.Kind}', expected '{kind}'.")]);
        }

        CopyWorkFiles(bundle, name, root.Work);
        var packageRoot = CopyPackages(bundle, replayRoot);
        File.WriteAllBytes(Path.Combine(root.Work, "NuGet.Config"), NuGetConfigWriter.CreateBytes(packageRoot));
        var projectPath = Path.Combine(root.Work, Path.GetFileName(recorded.ProjectPath));
        if (!File.Exists(projectPath)
            || !File.ReadAllBytes(projectPath).AsSpan().SequenceEqual(recorded.ProjectBytes.AsSpan()))
        {
            throw new ConformanceContractException(
                [new("replay-project", invocationRelativePath, "The replay project does not match the recorded project bytes.")]);
        }

        var invocation = RebaseInvocation(
            bundle,
            recorded,
            root,
            projectPath,
            packageRoot,
            dotnetRoot,
            dotnetPath,
            replayRoot);
        var result = await CoreCompileRunner.RunAsync(invocation, cancellationToken).ConfigureAwait(false);
        var evidenceRoot = Path.Combine(replayRoot, name);
        Directory.CreateDirectory(evidenceRoot);
        await WriteJsonAsync(
            Path.Combine(evidenceRoot, "invocation.json"),
            invocation,
            cancellationToken).ConfigureAwait(false);
        await WriteJsonAsync(
            Path.Combine(evidenceRoot, "processes.json"),
            result.Process.Processes,
            cancellationToken).ConfigureAwait(false);
        await WriteJsonAsync(
            Path.Combine(evidenceRoot, "binlog-evidence.json"),
            result.Binlog,
            cancellationToken).ConfigureAwait(false);
        await File.WriteAllBytesAsync(
            Path.Combine(evidenceRoot, "stdout.bin"),
            result.Process.StandardOutput.ToArray(),
            cancellationToken).ConfigureAwait(false);
        await File.WriteAllBytesAsync(
            Path.Combine(evidenceRoot, "stderr.bin"),
            result.Process.StandardError.ToArray(),
            cancellationToken).ConfigureAwait(false);
        if (File.Exists(root.Binlog))
        {
            File.Copy(root.Binlog, Path.Combine(evidenceRoot, "corecompile.binlog"), overwrite: false);
        }
    }

    private static LaneInvocation ReadInvocation(VerifiedBundle bundle, string relativePath)
    {
        try
        {
            return JsonSerializer.Deserialize<LaneInvocation>(
                       File.ReadAllBytes(BundlePath.Resolve(bundle.BundleRoot, relativePath)),
                       EvidenceJson)
                   ?? throw new JsonException("The lane invocation is null.");
        }
        catch (JsonException exception)
        {
            throw new ConformanceContractException(
                [new("replay-invocation", relativePath, $"The lane invocation is invalid: {exception.Message}")]);
        }
    }

    private static void CopyWorkFiles(VerifiedBundle bundle, string name, string workRoot)
    {
        var prefix = $"{name}/work/";
        foreach (var relativePath in bundle.FileHashes.Keys
                     .Where(path => path.StartsWith(prefix, StringComparison.Ordinal))
                     .OrderBy(static path => path, StringComparer.Ordinal))
        {
            var targetRelativePath = relativePath[prefix.Length..];
            var targetPath = BundlePath.Resolve(workRoot, targetRelativePath);
            Directory.CreateDirectory(Path.GetDirectoryName(targetPath)!);
            File.Copy(BundlePath.Resolve(bundle.BundleRoot, relativePath), targetPath, overwrite: false);
        }
    }

    private static string CopyPackages(VerifiedBundle bundle, string replayRoot)
    {
        const string prefix = "inputs/packages/";
        var packagePaths = bundle.FileHashes.Keys
            .Where(path => path.StartsWith(prefix, StringComparison.Ordinal))
            .OrderBy(static path => path, StringComparer.Ordinal)
            .ToArray();
        if (packagePaths.Length == 0)
        {
            throw new ConformanceContractException(
                [new("replay-package", prefix, "The replay bundle does not contain a compiler integration package.")]);
        }

        var packageRoot = Path.Combine(replayRoot, "immutable", "packages");
        Directory.CreateDirectory(packageRoot);
        foreach (var relativePath in packagePaths)
        {
            var targetPath = BundlePath.Resolve(packageRoot, relativePath[prefix.Length..]);
            if (File.Exists(targetPath))
            {
                if (!string.Equals(
                        Hashing.Sha256File(targetPath),
                        bundle.FileHashes[relativePath],
                        StringComparison.Ordinal))
                {
                    throw new ConformanceContractException(
                        [new("replay-package", targetPath, "The materialized replay package does not match the verified bundle hash.")]);
                }
                continue;
            }
            File.Copy(BundlePath.Resolve(bundle.BundleRoot, relativePath), targetPath, overwrite: false);
        }
        return packageRoot;
    }

    private static LaneInvocation RebaseInvocation(
        VerifiedBundle bundle,
        LaneInvocation recorded,
        LaneRoot root,
        string projectPath,
        string packageRoot,
        string dotnetRoot,
        string dotnetPath,
        string replayRoot)
    {
        var properties = recorded.Properties.ToBuilder();
        properties["BaseIntermediateOutputPath"] = root.Intermediate + Path.DirectorySeparatorChar;
        properties["IntermediateOutputPath"] = root.Intermediate + Path.DirectorySeparatorChar;
        properties["MSBuildProjectExtensionsPath"] = root.Intermediate;
        properties["BaseOutputPath"] = root.Output + Path.DirectorySeparatorChar;
        properties["OutputPath"] = root.Output + Path.DirectorySeparatorChar;
        properties["RestorePackagesPath"] = root.Cache;
        properties["RestoreConfigFile"] = Path.Combine(root.Work, "NuGet.Config");
        properties["FSharp2ConformancePackageSource"] = packageRoot;
        properties["FSharp2CompilerServerName"] = $"fsharp2-conformance-{Path.GetFileName(root.Root)}-{Guid.NewGuid():N}";
        properties["FSharp2CompilerTracePath"] = Path.Combine(root.Service, "compiler.trace");

        string? compilerHostPath = null;
        if (recorded.CompilerHostPath is not null)
        {
            var relativeHostPath = $"tools/{Path.GetFileName(recorded.CompilerHostPath)}";
            if (!bundle.FileHashes.ContainsKey(relativeHostPath))
            {
                throw new ConformanceContractException(
                    [new("replay-tool", relativeHostPath, "The replay bundle does not contain the recorded compiler host.")]);
            }
            var toolRoot = Path.Combine(replayRoot, "tools");
            Directory.CreateDirectory(toolRoot);
            compilerHostPath = Path.Combine(toolRoot, Path.GetFileName(recorded.CompilerHostPath));
            File.Copy(
                BundlePath.Resolve(bundle.BundleRoot, relativeHostPath),
                compilerHostPath,
                overwrite: false);
            properties["FSharp2CompilerHostPath"] = compilerHostPath;
            properties["FscToolPath"] = toolRoot;
            properties["FscToolExe"] = Path.GetFileName(compilerHostPath);
        }

        var environment = recorded.Environment.ToBuilder();
        environment["DOTNET_ROOT"] = dotnetRoot;
        environment["DOTNET_HOST_PATH"] = dotnetPath;
        environment["DOTNET_MULTILEVEL_LOOKUP"] = "0";
        environment["PATH"] = dotnetRoot
                              + Path.PathSeparator
                              + (Environment.GetEnvironmentVariable("PATH") ?? string.Empty);
        environment["TEMP"] = root.Temp;
        environment["TMP"] = root.Temp;
        environment["NUGET_PACKAGES"] = root.Cache;

        var arguments = ImmutableArray.CreateBuilder<string>();
        arguments.Add("msbuild");
        arguments.Add(projectPath);
        arguments.Add("/restore");
        arguments.Add("/t:Build");
        arguments.Add("/m:1");
        arguments.Add("/nodeReuse:false");
        arguments.Add($"/bl:{root.Binlog}");
        foreach (var property in properties.OrderBy(static item => item.Key, StringComparer.Ordinal))
        {
            arguments.Add($"/p:{property.Key}={property.Value}");
        }

        return new LaneInvocation(
            recorded.Kind,
            projectPath,
            recorded.ProjectBytes,
            recorded.Inputs,
            properties.ToImmutable(),
            environment.ToImmutable(),
            arguments.ToImmutable(),
            compilerHostPath);
    }

    private static Task WriteJsonAsync(
        string path,
        object value,
        CancellationToken cancellationToken) =>
        File.WriteAllBytesAsync(
            path,
            CanonicalJson.Canonicalize(JsonSerializer.SerializeToElement(value, EvidenceJson)),
            cancellationToken);
}
