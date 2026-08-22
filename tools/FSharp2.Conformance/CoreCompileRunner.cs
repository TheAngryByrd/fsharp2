using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class CoreCompileRunner
{
    public static CoreCompilePlan CreatePlan(
        MaterializedCase materializedCase,
        LaneRootSet roots,
        SdkSelectionResult sdk,
        string fsharp2Host)
    {
        ArgumentNullException.ThrowIfNull(materializedCase);
        ArgumentNullException.ThrowIfNull(roots);
        ArgumentNullException.ThrowIfNull(sdk);
        ArgumentException.ThrowIfNullOrWhiteSpace(fsharp2Host);
        var package = PackageMaterializer.Materialize(roots.RunRoot);
        var projectBytes = MsBuildProjectWriter.CreateProjectBytes(materializedCase);
        var restoreConfigBytes = NuGetConfigWriter.CreateBytes(package.SourceRoot);
        var inputs = materializedCase.Sources
            .Select(static source => new MaterializedInputHash(source.LogicalPath, source.Sha256))
            .Concat(materializedCase.TargetReferences.Select(static reference =>
                new MaterializedInputHash(reference.LogicalPath, reference.Sha256)))
            .Append(new MaterializedInputHash(
                $"/packages/{Path.GetFileName(package.PackagePath)}",
                package.Sha256))
            .Append(new MaterializedInputHash(
                "/restore/NuGet.Config",
                Hashing.Sha256(restoreConfigBytes)))
            .ToImmutableArray();
        var targetFramework = materializedCase.ResolvedDocument
            .GetProperty("envelope")
            .GetProperty("targetFrameworks")[0]
            .GetString()!;
        var configuration = ProjectProperty(materializedCase, "Configuration", "Debug");
        return new CoreCompilePlan(
            CreateInvocation(
                LaneKind.Oracle,
                roots.Oracle,
                sdk,
                projectBytes,
                inputs,
                materializedCase,
                targetFramework,
                configuration,
                package.SourceRoot,
                restoreConfigBytes,
                null),
            CreateInvocation(
                LaneKind.FSharp2,
                roots.FSharp2,
                sdk,
                projectBytes,
                inputs,
                materializedCase,
                targetFramework,
                configuration,
                package.SourceRoot,
                restoreConfigBytes,
                Path.GetFullPath(fsharp2Host)));
    }

    public static ValidationResult ValidateLaneEquality(CoreCompilePlan plan)
    {
        ArgumentNullException.ThrowIfNull(plan);
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        if (!plan.Oracle.ProjectBytes.SequenceEqual(plan.FSharp2.ProjectBytes))
        {
            issues.Add(new("lane-project", "ProjectBytes", "The lane project bytes differ."));
        }
        if (!plan.Oracle.Inputs.SequenceEqual(plan.FSharp2.Inputs))
        {
            issues.Add(new("lane-inputs", "Inputs", "The lane immutable input manifests differ."));
        }

        var allowedDifferences = new HashSet<string>(
            [
                "UseFSharp2Compiler",
                "FSharp2CompilerHostPath",
                "DisableAutoSetFscCompilerPath",
                "FscToolPath",
                "FscToolExe",
                "DotnetFscCompilerPath",
                "BaseIntermediateOutputPath",
                "IntermediateOutputPath",
                "BaseOutputPath",
                "OutputPath",
                "MSBuildProjectExtensionsPath",
                "RestorePackagesPath",
                "RestoreConfigFile",
                "FSharp2CompilerServerName",
                "FSharp2CompilerTracePath",
            ],
            StringComparer.Ordinal);
        var propertyNames = plan.Oracle.Properties.Keys
            .Concat(plan.FSharp2.Properties.Keys)
            .Distinct(StringComparer.Ordinal)
            .Where(name => !allowedDifferences.Contains(name));
        foreach (var name in propertyNames)
        {
            plan.Oracle.Properties.TryGetValue(name, out var oracleValue);
            plan.FSharp2.Properties.TryGetValue(name, out var fsharp2Value);
            if (!string.Equals(oracleValue, fsharp2Value, StringComparison.Ordinal))
            {
                issues.Add(new(
                    "lane-property",
                    name,
                    $"The Oracle value '{oracleValue}' differs from the FSharp2 value '{fsharp2Value}'."));
            }
        }
        return new ValidationResult(issues.Count == 0, issues.ToImmutable());
    }

    public static RestoredDependencyEvidence CaptureRestoredDependencies(CoreCompilePlan plan)
    {
        ArgumentNullException.ThrowIfNull(plan);
        var paths = new[]
        {
            ("lanes/oracle/work/packages.lock.json", PackageLockPath(plan.Oracle)),
            ("lanes/oracle/obj/project.assets.json", ProjectAssetsPath(plan.Oracle)),
            ("lanes/fsharp2/work/packages.lock.json", PackageLockPath(plan.FSharp2)),
            ("lanes/fsharp2/obj/project.assets.json", ProjectAssetsPath(plan.FSharp2)),
        };
        var missing = paths.Where(static item => !File.Exists(item.Item2)).ToArray();
        if (missing.Length != 0)
        {
            throw new ConformanceContractException(missing.Select(static item =>
                new ValidationIssue(
                    "restored-dependency",
                    item.Item1,
                    $"The restored dependency evidence file does not exist: {item.Item2}")).ToImmutableArray());
        }

        var inputs = paths.Select(static item =>
            new MaterializedInputHash(item.Item1, Hashing.Sha256File(item.Item2))).ToImmutableArray();
        var oracleLock = File.ReadAllBytes(paths[0].Item2);
        var oracleAssets = File.ReadAllBytes(paths[1].Item2);
        var fsharp2Lock = File.ReadAllBytes(paths[2].Item2);
        var fsharp2Assets = File.ReadAllBytes(paths[3].Item2);
        var oracleEvidence = DependencyEvidence(oracleLock, oracleAssets);
        var fsharp2Evidence = DependencyEvidence(fsharp2Lock, fsharp2Assets);
        var pathMap = new Dictionary<string, string>(StringComparer.Ordinal)
        {
            [LaneRoot(plan.Oracle)] = "/_fsharp2_conformance/lane",
            [LaneRoot(plan.FSharp2)] = "/_fsharp2_conformance/lane",
        };
        var rule = JsonSerializer.SerializeToElement(new
        {
            id = "logical-root-v1",
            @class = "normalized",
            pathMap,
        });
        var comparison = ArtifactComparator.Compare(oracleEvidence, fsharp2Evidence, rule);
        return new RestoredDependencyEvidence(inputs, comparison);
    }

    public static ConformanceVerdict Classify(ProcessResult result)
    {
        ArgumentNullException.ThrowIfNull(result);
        if (!result.Processes.IsDefaultOrEmpty
            && result.Processes.Any(static process =>
                IsSdkFsc(process.ExecutablePath)
                || process.Arguments.Any(IsSdkFsc)))
        {
            return ConformanceVerdict.Fail;
        }
        return result.TimedOut ? ConformanceVerdict.InfraError : result.ExitCode == 0
            ? ConformanceVerdict.Pass
            : ConformanceVerdict.Fail;
    }

    public static async Task<CoreCompileLaneResult> RunAsync(
        LaneInvocation invocation,
        CancellationToken cancellationToken)
    {
        var laneRoot = Directory.GetParent(invocation.WorkingDirectory())!.FullName;
        var process = await ProcessRunner.RunAsync(
            new ProcessSpec(
                invocation.Environment["DOTNET_HOST_PATH"],
                invocation.WorkingDirectory(),
                invocation.Arguments,
                invocation.Environment,
                TimeSpan.FromMinutes(2),
                Path.Combine(laneRoot, "stdout.bin"),
                Path.Combine(laneRoot, "stderr.bin")),
            cancellationToken).ConfigureAwait(false);
        var binlogPath = InvocationBinlog(invocation);
        var binlog = File.Exists(binlogPath)
            ? MsBuildBinlogReader.Read(binlogPath)
            : new BinlogEvidence(0, true, []);
        return new CoreCompileLaneResult(invocation.Kind, process, binlog);
    }

    private static LaneInvocation CreateInvocation(
        LaneKind kind,
        LaneRoot root,
        SdkSelectionResult sdk,
        byte[] projectBytes,
        ImmutableArray<MaterializedInputHash> inputs,
        MaterializedCase materializedCase,
        string targetFramework,
        string configuration,
        string packageSource,
        byte[] restoreConfigBytes,
        string? compilerHostPath)
    {
        MaterializeInputs(root.Work, projectBytes, restoreConfigBytes, materializedCase);
        var projectPath = Path.Combine(root.Work, "Conformance.fsproj");
        var properties = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);
        properties["TargetFramework"] = targetFramework;
        properties["Configuration"] = configuration;
        properties["UseFSharp2Compiler"] = kind == LaneKind.FSharp2 ? "true" : "false";
        properties["BaseIntermediateOutputPath"] = root.Intermediate + Path.DirectorySeparatorChar;
        properties["IntermediateOutputPath"] = root.Intermediate + Path.DirectorySeparatorChar;
        properties["MSBuildProjectExtensionsPath"] = root.Intermediate;
        properties["BaseOutputPath"] = root.Output + Path.DirectorySeparatorChar;
        properties["OutputPath"] = root.Output + Path.DirectorySeparatorChar;
        properties["RestorePackagesPath"] = root.Cache;
        properties["RestoreConfigFile"] = Path.Combine(root.Work, "NuGet.Config");
        properties["FSharp2ConformancePackageSource"] = packageSource;
        properties["FSharp2CompilerServerName"] = $"fsharp2-conformance-{Path.GetFileName(root.Root)}-{Guid.NewGuid():N}";
        properties["FSharp2CompilerTracePath"] = Path.Combine(root.Service, "compiler.trace");
        if (compilerHostPath is not null)
        {
            properties["FSharp2CompilerHostPath"] = compilerHostPath;
            properties["DisableAutoSetFscCompilerPath"] = "true";
            properties["FscToolPath"] = Path.GetDirectoryName(compilerHostPath)!;
            properties["FscToolExe"] = Path.GetFileName(compilerHostPath);
            properties["DotnetFscCompilerPath"] = string.Empty;
        }

        var environment = sdk.Environment.ToBuilder();
        environment["TEMP"] = root.Temp;
        environment["TMP"] = root.Temp;
        environment["NUGET_PACKAGES"] = root.Cache;
        environment["MSBUILDPRESERVETOOLTEMPFILES"] = "1";
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
            kind,
            projectPath,
            [.. projectBytes],
            inputs,
            properties.ToImmutable(),
            environment.ToImmutable(),
            arguments.ToImmutable(),
            compilerHostPath);
    }

    private static void MaterializeInputs(
        string workRoot,
        byte[] projectBytes,
        byte[] restoreConfigBytes,
        MaterializedCase materializedCase)
    {
        WriteVerified(Path.Combine(workRoot, "Conformance.fsproj"), projectBytes, Hashing.Sha256(projectBytes));
        WriteVerified(
            Path.Combine(workRoot, "NuGet.Config"),
            restoreConfigBytes,
            Hashing.Sha256(restoreConfigBytes));
        foreach (var source in materializedCase.Sources)
        {
            WriteVerified(ResolveUnderRoot(workRoot, source.LogicalPath), source.Bytes.ToArray(), source.Sha256);
        }
        foreach (var reference in materializedCase.TargetReferences)
        {
            WriteVerified(ResolveUnderRoot(workRoot, reference.LogicalPath), reference.Bytes.ToArray(), reference.Sha256);
        }
    }

    private static string PackageLockPath(LaneInvocation invocation) =>
        Path.Combine(Path.GetDirectoryName(invocation.ProjectPath)!, "packages.lock.json");

    private static string ProjectAssetsPath(LaneInvocation invocation) =>
        Path.Combine(invocation.Properties["MSBuildProjectExtensionsPath"], "project.assets.json");

    private static string LaneRoot(LaneInvocation invocation) =>
        Directory.GetParent(Path.GetDirectoryName(invocation.ProjectPath)!)!.FullName;

    private static JsonElement DependencyEvidence(byte[] packageLock, byte[] projectAssets)
    {
        using var lockDocument = JsonDocument.Parse(packageLock);
        using var assetsDocument = JsonDocument.Parse(projectAssets);
        return JsonSerializer.SerializeToElement(new
        {
            packageLock = lockDocument.RootElement,
            projectAssets = assetsDocument.RootElement,
        });
    }

    private static void WriteVerified(string path, byte[] bytes, string expectedHash)
    {
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllBytes(path, bytes);
        var actualHash = Hashing.Sha256File(path);
        if (!string.Equals(actualHash, expectedHash, StringComparison.Ordinal))
        {
            throw new ConformanceContractException(
                [new("materialized-hash", path, $"The materialized hash is '{actualHash}'. Expected '{expectedHash}'.")]);
        }
    }

    private static string ResolveUnderRoot(string root, string logicalPath)
    {
        var fullRoot = Path.GetFullPath(root);
        var path = Path.GetFullPath(Path.Combine(fullRoot, logicalPath.TrimStart('/', '\\')));
        var prefix = fullRoot.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                     + Path.DirectorySeparatorChar;
        if (!path.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
        {
            throw new ConformanceContractException(
                [new("path-outside-root", logicalPath, "The logical input path is outside the lane work root.")]);
        }
        return path;
    }

    private static string ProjectProperty(MaterializedCase materializedCase, string name, string fallback) =>
        materializedCase.ResolvedDocument
            .GetProperty("project")
            .GetProperty("properties")
            .EnumerateArray()
            .Where(item => string.Equals(item.GetProperty("name").GetString(), name, StringComparison.Ordinal))
            .Select(static item => item.GetProperty("value").ToString())
            .FirstOrDefault() ?? fallback;

    private static string InvocationBinlog(LaneInvocation invocation) =>
        invocation.Arguments
            .First(static argument => argument.StartsWith("/bl:", StringComparison.Ordinal))[4..];

    private static string WorkingDirectory(this LaneInvocation invocation) =>
        Path.GetDirectoryName(invocation.ProjectPath)!;

    private static bool IsSdkFsc(string value)
    {
        if (string.IsNullOrWhiteSpace(value))
        {
            return false;
        }
        var normalized = value.Replace('\\', '/');
        return normalized.EndsWith("/FSharp/fsc.dll", StringComparison.OrdinalIgnoreCase)
               && normalized.Contains("/sdk/", StringComparison.OrdinalIgnoreCase);
    }
}
