using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Runtime.Loader;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class ProbeRunner
{
    public static async Task<ProbeEvidence> VerifyAsync(
        ProbeRequest request,
        CancellationToken cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(request);
        cancellationToken.ThrowIfCancellationRequested();
        if (request.Timeout <= TimeSpan.Zero)
        {
            throw new ArgumentOutOfRangeException(nameof(request), "The probe timeout must be positive.");
        }

        var artifactPath = Path.GetFullPath(request.ArtifactPath);
        if (!File.Exists(artifactPath))
        {
            return Failed(request.Kind, $"The probe artifact does not exist: {artifactPath}");
        }

        return request.Kind switch
        {
            "ilverify" => await VerifyIlAsync(request, artifactPath, cancellationToken).ConfigureAwait(false),
            "managed-load" => VerifyManagedLoad(request.Kind, artifactPath),
            "public-api" => VerifyPublicApi(request.Kind, artifactPath),
            "metadata" => VerifyMetadata(request.Kind, artifactPath),
            "portable-pdb" => VerifyPortablePdb(request.Kind, artifactPath),
            "runtime" => await VerifyRuntimeAsync(request, artifactPath, cancellationToken).ConfigureAwait(false),
            "downstream-fsharp" => await VerifyDownstreamAsync(
                request,
                artifactPath,
                "fsharp",
                ".fsproj",
                ".fs",
                cancellationToken).ConfigureAwait(false),
            "downstream-csharp" => await VerifyDownstreamAsync(
                request,
                artifactPath,
                "csharp",
                ".csproj",
                ".cs",
                cancellationToken).ConfigureAwait(false),
            _ => Failed(request.Kind, $"Unsupported probe kind '{request.Kind}'."),
        };
    }

    private static async Task<ProbeEvidence> VerifyIlAsync(
        ProbeRequest request,
        string artifactPath,
        CancellationToken cancellationToken)
    {
        var dotnetPath = ResolveDotnetPath(request.Configuration);
        var toolRoot = FindToolRoot();
        var references = ResolveReferences(request.Configuration, artifactPath);
        var arguments = ImmutableArray.CreateBuilder<string>();
        arguments.Add("tool");
        arguments.Add("run");
        arguments.Add("ilverify");
        arguments.Add("--");
        arguments.Add(artifactPath);
        arguments.Add("--system-module");
        arguments.Add("System.Private.CoreLib");
        foreach (var reference in references)
        {
            arguments.Add("--reference");
            arguments.Add(reference);
        }

        var probeRoot = CreateProbeRoot();
        try
        {
            var result = await ProcessRunner.RunAsync(
                new ProcessSpec(
                    dotnetPath,
                    toolRoot,
                    arguments.ToImmutable(),
                    ImmutableDictionary<string, string>.Empty
                        .Add("DOTNET_ROOT", Path.GetDirectoryName(dotnetPath)!),
                    request.Timeout,
                    Path.Combine(probeRoot, "stdout.bin"),
                    Path.Combine(probeRoot, "stderr.bin")),
                cancellationToken).ConfigureAwait(false);
            var standardOutput = Decode(result.StandardOutput);
            var standardError = Decode(result.StandardError);
            var passed = !result.TimedOut && result.ExitCode == 0;
            return new ProbeEvidence(
                request.Kind,
                passed,
                Observation(new
                {
                    result.ExitCode,
                    result.TimedOut,
                    standardOutput,
                    standardError,
                    referenceCount = references.Length,
                }),
                passed
                    ? null
                    : result.TimedOut
                        ? "ILVerify timed out."
                        : $"ILVerify exited with code {result.ExitCode}: {FirstNonEmpty(standardError, standardOutput)}");
        }
        finally
        {
            DeleteProbeRoot(probeRoot);
        }
    }

    private static ProbeEvidence VerifyManagedLoad(string kind, string artifactPath)
    {
        var (evidence, loadContext) = LoadManagedArtifact(kind, artifactPath);
        // A collectible context releases the mapped artifact file only after the GC collects it. The
        // deterministic repeat rebuilds the same artifact next, so the probe waits for the release.
        for (var attempt = 0; loadContext.IsAlive && attempt < 10; attempt++)
        {
            GC.Collect();
            GC.WaitForPendingFinalizers();
        }
        return loadContext.IsAlive
            ? Failed(kind, "The managed load context did not unload, so the artifact file stays locked.")
            : evidence;
    }

    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]
    private static (ProbeEvidence Evidence, WeakReference LoadContext) LoadManagedArtifact(string kind, string artifactPath)
    {
        var loadContext = new ProbeLoadContext(artifactPath);
        try
        {
            var assembly = loadContext.LoadFromAssemblyPath(artifactPath);
            var exportedTypes = assembly.GetExportedTypes()
                .Select(static type => type.FullName ?? type.Name)
                .OrderBy(static name => name, StringComparer.Ordinal)
                .ToArray();
            return (Passed(kind, new
            {
                assembly = assembly.GetName().Name,
                exportedTypes,
            }), new WeakReference(loadContext));
        }
        catch (Exception exception) when (
            exception is BadImageFormatException
            or FileLoadException
            or ReflectionTypeLoadException)
        {
            return (Failed(kind, $"The managed assembly could not be loaded: {exception.Message}"), new WeakReference(loadContext));
        }
        finally
        {
            loadContext.Unload();
        }
    }

    private static ProbeEvidence VerifyPublicApi(string kind, string artifactPath)
    {
        try
        {
            var identities = ManagedMetadataComparator.ReadPublicTypeIdentities(
                [.. File.ReadAllBytes(artifactPath)]);
            return Passed(kind, new { publicTypes = identities });
        }
        catch (BadImageFormatException exception)
        {
            return Failed(kind, $"The public API metadata could not be read: {exception.Message}");
        }
    }

    private static ProbeEvidence VerifyMetadata(string kind, string artifactPath)
    {
        try
        {
            using var stream = File.OpenRead(artifactPath);
            using var peReader = new PEReader(stream);
            if (!peReader.HasMetadata)
            {
                return Failed(kind, "The PE image does not contain managed metadata.");
            }
            var metadata = peReader.GetMetadataReader();
            return Passed(kind, new
            {
                metadata.IsAssembly,
                typeDefinitions = metadata.TypeDefinitions.Count,
                methodDefinitions = metadata.MethodDefinitions.Count,
                assemblyReferences = metadata.AssemblyReferences.Count,
            });
        }
        catch (BadImageFormatException exception)
        {
            return Failed(kind, $"The managed metadata could not be read: {exception.Message}");
        }
    }

    private static ProbeEvidence VerifyPortablePdb(string kind, string artifactPath)
    {
        try
        {
            using var stream = File.OpenRead(artifactPath);
            using var provider = MetadataReaderProvider.FromPortablePdbStream(stream);
            var metadata = provider.GetMetadataReader();
            return Passed(kind, new
            {
                documents = metadata.Documents.Count,
                methodDebugInformation = metadata.MethodDebugInformation.Count,
                localScopes = metadata.LocalScopes.Count,
            });
        }
        catch (BadImageFormatException exception)
        {
            return Failed(kind, $"The Portable PDB metadata could not be read: {exception.Message}");
        }
    }

    private static async Task<ProbeEvidence> VerifyRuntimeAsync(
        ProbeRequest request,
        string artifactPath,
        CancellationToken cancellationToken)
    {
        var configuration = request.Configuration;
        var dotnetPath = RequireString(configuration, "dotnetPath");
        var arguments = ImmutableArray.CreateBuilder<string>();
        arguments.Add(artifactPath);
        if (configuration.TryGetProperty("arguments", out var configuredArguments)
            && configuredArguments.ValueKind == JsonValueKind.Array)
        {
            foreach (var argument in configuredArguments.EnumerateArray())
            {
                arguments.Add(argument.GetString() ?? "");
            }
        }

        var expectedExitCode =
            configuration.TryGetProperty("expectedExitCode", out var exitCode)
            && exitCode.TryGetInt32(out var parsedExitCode)
                ? parsedExitCode
                : 0;
        var expectedOutput = ExpectedText(configuration, "expectedStdout");
        var expectedError = ExpectedText(configuration, "expectedStderr");
        var probeRoot = CreateProbeRoot();
        try
        {
            var result = await ProcessRunner.RunAsync(
                new ProcessSpec(
                    dotnetPath,
                    Path.GetDirectoryName(artifactPath)!,
                    arguments.ToImmutable(),
                    ReadEnvironment(configuration),
                    request.Timeout,
                    Path.Combine(probeRoot, "stdout.bin"),
                    Path.Combine(probeRoot, "stderr.bin")),
                cancellationToken).ConfigureAwait(false);
            var standardOutput = NormalizeNewlines(Decode(result.StandardOutput));
            var standardError = NormalizeNewlines(Decode(result.StandardError));
            var differences = new List<string>();
            if (result.TimedOut)
            {
                differences.Add("process timed out");
            }
            if (result.ExitCode != expectedExitCode)
            {
                differences.Add($"exit code expected {expectedExitCode} observed {result.ExitCode}");
            }
            if (!string.Equals(standardOutput, expectedOutput, StringComparison.Ordinal))
            {
                differences.Add("standard output changed");
            }
            if (!string.Equals(standardError, expectedError, StringComparison.Ordinal))
            {
                differences.Add("standard error changed");
            }
            return new ProbeEvidence(
                request.Kind,
                differences.Count == 0,
                Observation(new
                {
                    result.ExitCode,
                    result.TimedOut,
                    standardOutput,
                    standardError,
                }),
                differences.Count == 0 ? null : string.Join("; ", differences));
        }
        finally
        {
            DeleteProbeRoot(probeRoot);
        }
    }

    private static async Task<ProbeEvidence> VerifyDownstreamAsync(
        ProbeRequest request,
        string artifactPath,
        string language,
        string projectExtension,
        string sourceExtension,
        CancellationToken cancellationToken)
    {
        var configuration = request.Configuration;
        var dotnetPath = RequireString(configuration, "dotnetPath");
        var targetFramework = RequireText(configuration, "targetFramework");
        var source = RequireText(configuration, "source");
        var expectedExitCode =
            configuration.TryGetProperty("expectedExitCode", out var configuredExitCode)
            && configuredExitCode.TryGetInt32(out var exitCode)
                ? exitCode
                : 0;
        var probeRoot = CreateProbeRoot();
        var projectPath = Path.Combine(probeRoot, "DownstreamConsumer" + projectExtension);
        var sourcePath = Path.Combine(probeRoot, "Consumer" + sourceExtension);
        try
        {
            await File.WriteAllTextAsync(
                projectPath,
                ConsumerProject(targetFramework, artifactPath, Path.GetFileName(sourcePath)),
                new UTF8Encoding(encoderShouldEmitUTF8Identifier: false),
                cancellationToken).ConfigureAwait(false);
            await File.WriteAllTextAsync(
                sourcePath,
                source,
                new UTF8Encoding(encoderShouldEmitUTF8Identifier: false),
                cancellationToken).ConfigureAwait(false);

            var build = await ProcessRunner.RunAsync(
                new ProcessSpec(
                    dotnetPath,
                    probeRoot,
                    [
                        "build",
                        projectPath,
                        "--configuration",
                        "Release",
                        "--nologo",
                        "--verbosity",
                        "minimal",
                        "--disable-build-servers",
                    ],
                    ReadEnvironment(configuration),
                    request.Timeout,
                    Path.Combine(probeRoot, "build.stdout.bin"),
                    Path.Combine(probeRoot, "build.stderr.bin")),
                cancellationToken).ConfigureAwait(false);

            ProcessResult? run = null;
            if (!build.TimedOut && build.ExitCode == 0)
            {
                var consumerPath = Path.Combine(
                    probeRoot,
                    "bin",
                    "Release",
                    targetFramework,
                    "DownstreamConsumer.dll");
                run = await ProcessRunner.RunAsync(
                    new ProcessSpec(
                        dotnetPath,
                        Path.GetDirectoryName(consumerPath)!,
                        [consumerPath],
                        ReadEnvironment(configuration),
                        request.Timeout,
                        Path.Combine(probeRoot, "run.stdout.bin"),
                        Path.Combine(probeRoot, "run.stderr.bin")),
                    cancellationToken).ConfigureAwait(false);
            }

            var timedOut = build.TimedOut || run?.TimedOut == true;
            var standardOutput = Decode(run?.StandardOutput ?? build.StandardOutput);
            var standardError = Decode(run?.StandardError ?? build.StandardError);
            var passed = !timedOut
                         && build.ExitCode == 0
                         && run is not null
                         && run.ExitCode == expectedExitCode;
            return new ProbeEvidence(
                request.Kind,
                passed,
                Observation(new
                {
                    language,
                    targetFramework,
                    buildExitCode = build.ExitCode,
                    runExitCode = run?.ExitCode ?? -1,
                    timedOut,
                    standardOutput,
                    standardError,
                }),
                passed
                    ? null
                    : DownstreamDifference(
                        language,
                        build,
                        run,
                        expectedExitCode,
                        standardError,
                        standardOutput));
        }
        finally
        {
            DeleteProbeRoot(probeRoot);
        }
    }

    private static string ConsumerProject(
        string targetFramework,
        string artifactPath,
        string sourceFileName)
    {
        var escapedTargetFramework = System.Security.SecurityElement.Escape(targetFramework);
        var escapedArtifactPath = System.Security.SecurityElement.Escape(artifactPath);
        var escapedSourceFileName = System.Security.SecurityElement.Escape(sourceFileName);
        return $"""
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <OutputType>Exe</OutputType>
                <TargetFramework>{escapedTargetFramework}</TargetFramework>
                <AssemblyName>DownstreamConsumer</AssemblyName>
                <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
              </PropertyGroup>
              <ItemGroup>
                <Compile Include="{escapedSourceFileName}" />
                <Reference Include="ConformanceArtifact">
                  <HintPath>{escapedArtifactPath}</HintPath>
                  <Private>true</Private>
                </Reference>
              </ItemGroup>
            </Project>
            """;
    }

    private static string DownstreamDifference(
        string language,
        ProcessResult build,
        ProcessResult? run,
        int expectedExitCode,
        string standardError,
        string standardOutput)
    {
        if (build.TimedOut)
        {
            return $"The downstream {language} consumer build timed out.";
        }
        if (build.ExitCode != 0)
        {
            return $"The downstream {language} consumer build exited with code {build.ExitCode}: {FirstNonEmpty(standardError, standardOutput)}";
        }
        if (run is null)
        {
            return $"The downstream {language} consumer did not run.";
        }
        if (run.TimedOut)
        {
            return $"The downstream {language} consumer run timed out.";
        }
        return $"The downstream {language} consumer exited with code {run.ExitCode}, expected {expectedExitCode}: {FirstNonEmpty(standardError, standardOutput)}";
    }

    private static ImmutableArray<string> ResolveReferences(JsonElement configuration, string artifactPath)
    {
        var references = new SortedDictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        if (configuration.ValueKind == JsonValueKind.Object
            && configuration.TryGetProperty("references", out var configuredReferences)
            && configuredReferences.ValueKind == JsonValueKind.Array)
        {
            foreach (var reference in configuredReferences.EnumerateArray())
            {
                var path = reference.GetString();
                if (!string.IsNullOrWhiteSpace(path))
                {
                    AddReference(references, Path.GetFullPath(path), replace: true);
                }
            }
        }
        AddReferenceDirectory(references, Path.GetDirectoryName(artifactPath)!);
        AddReferenceDirectory(references, Path.GetDirectoryName(typeof(object).Assembly.Location)!);
        references.Remove(Path.GetFileNameWithoutExtension(artifactPath));
        return [.. references.Values.OrderBy(static value => value, StringComparer.Ordinal)];
    }

    private static void AddReferenceDirectory(IDictionary<string, string> references, string directory)
    {
        foreach (var path in Directory.EnumerateFiles(directory, "*.dll", SearchOption.TopDirectoryOnly))
        {
            AddReference(references, path, replace: false);
        }
    }

    private static void AddReference(IDictionary<string, string> references, string path, bool replace)
    {
        try
        {
            var name = AssemblyName.GetAssemblyName(path).Name;
            if (string.IsNullOrWhiteSpace(name))
            {
                return;
            }
            if (replace || !references.ContainsKey(name))
            {
                references[name] = Path.GetFullPath(path);
            }
        }
        catch (BadImageFormatException)
        {
        }
    }

    private static string ResolveDotnetPath(JsonElement configuration)
    {
        if (configuration.ValueKind == JsonValueKind.Object
            && configuration.TryGetProperty("dotnetPath", out var configuredPath)
            && configuredPath.ValueKind == JsonValueKind.String
            && !string.IsNullOrWhiteSpace(configuredPath.GetString()))
        {
            return Path.GetFullPath(configuredPath.GetString()!);
        }
        foreach (var rootName in new[] { "FSHARP2_DOTNET_ROOT", "DOTNET_ROOT" })
        {
            var root = Environment.GetEnvironmentVariable(rootName);
            if (!string.IsNullOrWhiteSpace(root))
            {
                var candidate = Path.Combine(root, OperatingSystem.IsWindows() ? "dotnet.exe" : "dotnet");
                if (File.Exists(candidate))
                {
                    return Path.GetFullPath(candidate);
                }
            }
        }
        var processPath = Environment.ProcessPath;
        if (!string.IsNullOrWhiteSpace(processPath)
            && string.Equals(
                Path.GetFileNameWithoutExtension(processPath),
                "dotnet",
                StringComparison.OrdinalIgnoreCase))
        {
            return Path.GetFullPath(processPath);
        }
        throw new ConformanceContractException(
            [new("dotnet-path", "probe", "The probe could not resolve an absolute dotnet path.")]);
    }

    private static string FindToolRoot()
    {
        foreach (var start in new[] { Directory.GetCurrentDirectory(), AppContext.BaseDirectory })
        {
            var directory = new DirectoryInfo(Path.GetFullPath(start));
            while (directory is not null)
            {
                if (File.Exists(Path.Combine(directory.FullName, ".config", "dotnet-tools.json")))
                {
                    return directory.FullName;
                }
                directory = directory.Parent;
            }
        }
        throw new ConformanceContractException(
            [new("tool-manifest", "ilverify", "The repository dotnet tool manifest could not be found.")]);
    }

    private static ImmutableDictionary<string, string> ReadEnvironment(JsonElement configuration)
    {
        var environment = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.OrdinalIgnoreCase);
        if (configuration.ValueKind == JsonValueKind.Object
            && configuration.TryGetProperty("environment", out var variables)
            && variables.ValueKind == JsonValueKind.Object)
        {
            foreach (var variable in variables.EnumerateObject())
            {
                environment[variable.Name] = variable.Value.GetString() ?? "";
            }
        }
        return environment.ToImmutable();
    }

    private static string RequireString(JsonElement configuration, string name)
    {
        if (configuration.ValueKind == JsonValueKind.Object
            && configuration.TryGetProperty(name, out var value)
            && value.ValueKind == JsonValueKind.String
            && !string.IsNullOrWhiteSpace(value.GetString()))
        {
            return Path.GetFullPath(value.GetString()!);
        }
        throw new ConformanceContractException(
            [new("probe-configuration", name, $"Runtime probe configuration requires '{name}'.")]);
    }

    private static string RequireText(JsonElement configuration, string name)
    {
        if (configuration.ValueKind == JsonValueKind.Object
            && configuration.TryGetProperty(name, out var value)
            && value.ValueKind == JsonValueKind.String
            && !string.IsNullOrWhiteSpace(value.GetString()))
        {
            return value.GetString()!;
        }
        throw new ConformanceContractException(
            [new("probe-configuration", name, $"The downstream probe configuration requires '{name}'.")]);
    }

    private static string ExpectedText(JsonElement configuration, string name)
    {
        if (configuration.ValueKind != JsonValueKind.Object
            || !configuration.TryGetProperty(name, out var value)
            || value.ValueKind != JsonValueKind.String)
        {
            return "";
        }
        return NormalizeNewlines(value.GetString()!)
            .Replace("\\r\\n", "\n", StringComparison.Ordinal)
            .Replace("\\n", "\n", StringComparison.Ordinal)
            .Replace("\\r", "\r", StringComparison.Ordinal);
    }

    private static string NormalizeNewlines(string value) =>
        value.Replace("\r\n", "\n", StringComparison.Ordinal);

    private static string Decode(ImmutableArray<byte> bytes) =>
        Encoding.UTF8.GetString(bytes.AsSpan());

    private static JsonElement Observation<T>(T value) =>
        JsonSerializer.SerializeToElement(value);

    private static ProbeEvidence Passed<T>(string kind, T observation) =>
        new(kind, true, Observation(observation), null);

    private static ProbeEvidence Failed(string kind, string difference) =>
        new(kind, false, Observation(new { }), difference);

    private static string FirstNonEmpty(string first, string second) =>
        !string.IsNullOrWhiteSpace(first) ? first.Trim() : second.Trim();

    private static string CreateProbeRoot()
    {
        var root = Path.Combine(
            Path.GetTempPath(),
            "fsharp2-conformance-probes",
            Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        return root;
    }

    private static void DeleteProbeRoot(string root)
    {
        if (Directory.Exists(root))
        {
            Directory.Delete(root, recursive: true);
        }
    }

    private sealed class ProbeLoadContext(string componentAssemblyPath)
        : AssemblyLoadContext(isCollectible: true)
    {
        private readonly AssemblyDependencyResolver resolver = new(componentAssemblyPath);

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            var path = resolver.ResolveAssemblyToPath(assemblyName);
            return path is null ? null : LoadFromAssemblyPath(path);
        }
    }
}
