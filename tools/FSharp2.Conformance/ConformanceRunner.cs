using System.Collections.Immutable;
using System.Text.Json;
using FSharp2.Compiler;

namespace FSharp2.Conformance;

public static class ConformanceRunner
{
    private sealed record RepeatRunEvidence(
        CoreCompilePlan Plan,
        CoreCompileLaneResult Oracle,
        CoreCompileLaneResult FSharp2);

    private static readonly JsonSerializerOptions EvidenceJson = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        WriteIndented = true,
    };

    public static async Task<int> CaptureOracleAsync(
        ImmutableDictionary<string, string> options,
        CancellationToken cancellationToken)
    {
        Program.RejectUnknown(options, ["root", "case", "output-root"]);
        var conformanceRoot = Program.Require(options, "root");
        var caseId = Program.Require(options, "case");
        var outputRoot = Path.GetFullPath(Program.Require(options, "output-root"));
        RejectCheckedInOutput(conformanceRoot, outputRoot);
        var repository = ManifestLoader.Load(conformanceRoot);
        var conformanceCase = repository.Cases.FirstOrDefault(item =>
            string.Equals(item.CaseId, caseId, StringComparison.Ordinal));
        if (conformanceCase is null)
        {
            throw new ConformanceContractException(
                [new("case-missing", caseId, "The selected conformance case does not exist.")]);
        }

        var materialized = CaseMaterializer.Materialize(repository, conformanceCase);
        var sdkRoot = Environment.GetEnvironmentVariable("FSHARP2_DOTNET_ROOT");
        if (string.IsNullOrWhiteSpace(sdkRoot))
        {
            sdkRoot = Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                $"fsharp2-sdk-{repository.Toolchain.GetProperty("sdkVersion").GetString()}");
        }
        var sdk = SdkSelection.Resolve(conformanceRoot, sdkRoot, null);
        var runId = $"oracle-{SafeName(caseId)}-{DateTimeOffset.UtcNow:yyyyMMddTHHmmssfffZ}-{Guid.NewGuid():N}";
        var runRoot = Path.Combine(outputRoot, runId);
        var roots = LaneRoots.Create(runRoot, runId);
        var plan = CoreCompileRunner.CreatePlan(
            materialized,
            roots,
            sdk,
            Environment.ProcessPath ?? sdk.DotnetPath);
        var lane = await CoreCompileRunner.RunAsync(plan.Oracle, cancellationToken).ConfigureAwait(false);
        await WriteOracleEvidenceAsync(
            runRoot,
            repository,
            materialized,
            sdk,
            plan.Oracle,
            lane,
            cancellationToken).ConfigureAwait(false);
        Console.WriteLine($"captured oracle {caseId} at {runRoot}");
        return lane.Process.TimedOut ? 3 : 0;
    }

    public static async Task<int> RunAsync(
        ImmutableDictionary<string, string> options,
        CancellationToken cancellationToken)
    {
        Program.RejectUnknown(options, ["root", "case", "dotnet-root", "fsharp2-host", "output-root"]);
        var conformanceRoot = Path.GetFullPath(Program.Require(options, "root"));
        var caseId = Program.Require(options, "case");
        var dotnetRoot = Program.Require(options, "dotnet-root");
        var fsharp2Host = Path.GetFullPath(Program.Require(options, "fsharp2-host"));
        var outputRoot = Path.GetFullPath(Program.Require(options, "output-root"));
        RejectCheckedInOutput(conformanceRoot, outputRoot);
        ThrowIfInvalid(NativeAotHostValidator.Validate(fsharp2Host));
        ThrowIfInvalid(ProductionGraphGuard.Validate(FindRepositoryRoot()));

        var repository = ManifestLoader.Load(conformanceRoot);
        var conformanceCase = RequireCase(repository, caseId);
        var materialized = CaseMaterializer.Materialize(repository, conformanceCase);
        var sdk = SdkSelection.Resolve(conformanceRoot, dotnetRoot, null);
        var runId = $"run-{SafeName(caseId)}-{DateTimeOffset.UtcNow:yyyyMMddTHHmmssfffZ}-{Guid.NewGuid():N}";
        var runRoot = Path.Combine(outputRoot, runId);
        var roots = LaneRoots.Create(runRoot, runId);
        var fallbackReceiptPath = FallbackSentinel.CreateReceipt(runRoot);
        var compilation = new FSharp2.Compiler.Compiler().Compile(
            CompilerContractProbe.CreateRequest(materialized),
            cancellationToken);
        var coreEvidence = CoreEvidenceWriter.Create(compilation);
        var declaredUnsupported = string.Equals(
            materialized.ResolvedDocument.GetProperty("envelope").GetProperty("state").GetString(),
            "unsupported",
            StringComparison.Ordinal);

        CoreCompilePlan? plan = null;
        CoreCompileLaneResult? oracle = null;
        CoreCompileLaneResult? fsharp2 = null;
        RepeatRunEvidence? repeat = null;
        var comparisons = ImmutableArray.CreateBuilder<ComparisonResult>();
        var probes = ImmutableArray.CreateBuilder<ProbeEvidence>();
        var missingEvidence = ImmutableArray.CreateBuilder<string>();
        var artifacts = ImmutableDictionary.CreateBuilder<string, ImmutableArray<byte>>(StringComparer.Ordinal);
        var fallbackDetected = false;
        var infrastructureFailed = false;

        comparisons.Add(CompareExpectedDiagnostics(repository, materialized, compilation));
        if (declaredUnsupported)
        {
            if (!string.Equals(coreEvidence.Outcome, "unsupported", StringComparison.Ordinal))
            {
                missingEvidence.Add($"direct compiler outcome was '{coreEvidence.Outcome}', expected 'unsupported'");
            }
            if (!coreEvidence.Artifacts.IsDefaultOrEmpty)
            {
                missingEvidence.Add("declared unsupported direct compilation published artifacts");
            }
        }
        else
        {
            plan = CoreCompileRunner.CreatePlan(materialized, roots, sdk, fsharp2Host);
            ThrowIfInvalid(CoreCompileRunner.ValidateLaneEquality(plan));
            var laneTasks = new[]
            {
                CoreCompileRunner.RunAsync(plan.Oracle, cancellationToken),
                CoreCompileRunner.RunAsync(plan.FSharp2, cancellationToken),
            };
            var lanes = await Task.WhenAll(laneTasks).ConfigureAwait(false);
            oracle = lanes[0];
            fsharp2 = lanes[1];
            var restoredDependencies = CoreCompileRunner.CaptureRestoredDependencies(plan);
            plan = plan with { RestoredDependencyInputs = restoredDependencies.Inputs };
            comparisons.Add(restoredDependencies.Comparison);
            infrastructureFailed = oracle.Process.TimedOut || fsharp2.Process.TimedOut;
            RequirePhysicalCompilerTask("oracle", oracle, missingEvidence);
            RequirePhysicalCompilerTask("fsharp2", fsharp2, missingEvidence);
            fallbackDetected = DetectFallback(plan.FSharp2, fsharp2);
            comparisons.Add(CompareLaneDiagnostics(roots, oracle, fsharp2));

            var positive = string.Equals(
                materialized.ResolvedDocument.GetProperty("polarity").GetString(),
                "positive",
                StringComparison.Ordinal);
            var oraclePaths = ArtifactPaths(roots.Oracle, materialized);
            var fsharp2Paths = ArtifactPaths(roots.FSharp2, materialized);
            ComparePublication(materialized, oraclePaths, fsharp2Paths, comparisons, missingEvidence);
            AddArtifactFiles("oracle", oraclePaths, artifacts);
            AddArtifactFiles("fsharp2", fsharp2Paths, artifacts);

            if (positive)
            {
                if (!string.Equals(coreEvidence.Outcome, "succeeded", StringComparison.Ordinal))
                {
                    missingEvidence.Add($"direct compiler outcome was '{coreEvidence.Outcome}', expected 'succeeded'");
                }
                repeat = await AddPositiveEvidenceAsync(
                    runRoot,
                    runId,
                    repository,
                    materialized,
                    sdk,
                    fsharp2Host,
                    oraclePaths,
                    fsharp2Paths,
                    comparisons,
                    probes,
                    artifacts,
                    missingEvidence,
                    cancellationToken).ConfigureAwait(false);
            }
            else
            {
                if (!string.Equals(coreEvidence.Outcome, "failed", StringComparison.Ordinal))
                {
                    missingEvidence.Add($"direct compiler outcome was '{coreEvidence.Outcome}', expected 'failed'");
                }
                if (oracle.Process.ExitCode == 0 || fsharp2.Process.ExitCode == 0)
                {
                    missingEvidence.Add("a supported negative lane exited successfully");
                }
            }
        }

        FallbackSentinel.RetainSelectedReceipt(fallbackReceiptPath);
        fallbackDetected |= !FallbackSentinel.ValidateReceipt(fallbackReceiptPath).IsValid;
        var publishedSuccessfulArtifact = coreEvidence.Artifacts.Any()
                                          && !string.Equals(coreEvidence.Outcome, "succeeded", StringComparison.Ordinal);
        var verdict = VerdictEngine.Decide(new VerdictInput(
            declaredUnsupported,
            infrastructureFailed,
            fallbackDetected,
            publishedSuccessfulArtifact,
            comparisons.ToImmutable(),
            probes.ToImmutable(),
            missingEvidence.ToImmutable()));
        if (!declaredUnsupported && missingEvidence.Count != 0 && verdict.Verdict == ConformanceVerdict.Pass)
        {
            verdict = verdict with
            {
                Verdict = ConformanceVerdict.Fail,
                Reasons = [.. missingEvidence.Select(static value => $"Missing evidence: {value}")],
                CountsAsCoverage = false,
            };
        }
        if (declaredUnsupported
            && (!string.Equals(coreEvidence.Outcome, "unsupported", StringComparison.Ordinal)
                || !coreEvidence.Artifacts.IsDefaultOrEmpty))
        {
            verdict = verdict with
            {
                Verdict = ConformanceVerdict.Fail,
                Reasons = ["The declared unsupported case did not produce the required unsupported outcome."],
                CountsAsCoverage = false,
            };
        }

        await WriteRunAsync(
            runRoot,
            runId,
            repository,
            materialized,
            sdk,
            fsharp2Host,
            compilation,
            coreEvidence,
            plan,
            oracle,
            fsharp2,
            repeat,
            probes.ToImmutable(),
            comparisons.ToImmutable(),
            verdict,
            artifacts.ToImmutable(),
            cancellationToken).ConfigureAwait(false);
        Console.WriteLine($"{VerdictName(verdict.Verdict)} {caseId} at {runRoot}");
        return verdict.Verdict switch
        {
            ConformanceVerdict.Pass or ConformanceVerdict.Unsupported => 0,
            ConformanceVerdict.Fail => 1,
            _ => 3,
        };
    }

    public static async Task<int> ReplayAsync(
        ImmutableDictionary<string, string> options,
        CancellationToken cancellationToken)
    {
        Program.RejectUnknown(options, ["bundle", "dotnet-root", "output-root"]);
        var suppliedRoot = Path.GetFullPath(Program.Require(options, "bundle"));
        var bundleRoot = File.Exists(Path.Combine(suppliedRoot, "bundle.json"))
            ? suppliedRoot
            : Path.Combine(suppliedRoot, "bundle");
        var dotnetRoot = Path.GetFullPath(Program.Require(options, "dotnet-root"));
        var outputRoot = Path.GetFullPath(Program.Require(options, "output-root"));
        try
        {
            var bundle = BundleReader.ReadAndVerify(bundleRoot);
            var replay = await ReplayRunner.ReplayAsync(
                bundle,
                dotnetRoot,
                outputRoot,
                cancellationToken).ConfigureAwait(false);
            Console.WriteLine($"replayed {replay.ParentRunId} at {replay.OutputRoot}");
            return 0;
        }
        catch (ConformanceContractException exception)
        {
            foreach (var issue in exception.Issues)
            {
                Console.Error.WriteLine($"infra-error {issue.Code} {issue.Path}: {issue.Message}");
            }
            return 3;
        }
    }

    private static async Task<RepeatRunEvidence?> AddPositiveEvidenceAsync(
        string runRoot,
        string runId,
        ConformanceRepository repository,
        MaterializedCase materialized,
        SdkSelectionResult sdk,
        string fsharp2Host,
        ArtifactPathSet oraclePaths,
        ArtifactPathSet fsharp2Paths,
        ImmutableArray<ComparisonResult>.Builder comparisons,
        ImmutableArray<ProbeEvidence>.Builder probes,
        ImmutableDictionary<string, ImmutableArray<byte>>.Builder artifacts,
        ImmutableArray<string>.Builder missingEvidence,
        CancellationToken cancellationToken)
    {
        if (!File.Exists(oraclePaths.Implementation) || !File.Exists(fsharp2Paths.Implementation))
        {
            missingEvidence.Add("a positive lane did not publish its implementation assembly");
            return null;
        }
        comparisons.Add(ManagedMetadataComparator.Compare(
            [.. File.ReadAllBytes(oraclePaths.Implementation)],
            [.. File.ReadAllBytes(fsharp2Paths.Implementation)]));
        if (File.Exists(oraclePaths.PortablePdb) && File.Exists(fsharp2Paths.PortablePdb))
        {
            comparisons.Add(PortablePdbComparator.Compare(
                [.. File.ReadAllBytes(oraclePaths.Implementation)],
                [.. File.ReadAllBytes(oraclePaths.PortablePdb)],
                [.. File.ReadAllBytes(fsharp2Paths.Implementation)],
                [.. File.ReadAllBytes(fsharp2Paths.PortablePdb)],
                JsonSerializer.SerializeToElement(new { id = "portable-pdb-v1" })));
        }
        else
        {
            missingEvidence.Add("a positive lane did not publish its portable PDB");
        }

        var oracleProbes = await RunDeclaredProbesAsync(
            materialized,
            oraclePaths,
            sdk,
            cancellationToken).ConfigureAwait(false);
        var fsharp2Probes = await RunDeclaredProbesAsync(
            materialized,
            fsharp2Paths,
            sdk,
            cancellationToken).ConfigureAwait(false);
        probes.AddRange(fsharp2Probes);
        foreach (var oracleProbe in oracleProbes.Where(static probe => !probe.Passed))
        {
            missingEvidence.Add($"Oracle probe '{oracleProbe.Kind}' failed: {oracleProbe.Difference}");
        }

        var oracleRuntime = oracleProbes.FirstOrDefault(static probe => probe.Kind == "runtime");
        var fsharp2Runtime = fsharp2Probes.FirstOrDefault(static probe => probe.Kind == "runtime");
        if (oracleRuntime is not null && fsharp2Runtime is not null)
        {
            comparisons.Add(BehaviorComparator.Compare(
                Behavior(oracleRuntime),
                Behavior(fsharp2Runtime)));
        }
        else
        {
            missingEvidence.Add("runtime behavior comparison evidence is missing");
        }

        var oracleSnapshot = SnapshotArtifacts(materialized.RequestedArtifacts, oraclePaths);
        var fsharp2Snapshot = SnapshotArtifacts(materialized.RequestedArtifacts, fsharp2Paths);
        var repeatRoots = LaneRoots.Create(runRoot, runId);
        var repeatPlan = CreateRepeatPlan(
            CoreCompileRunner.CreatePlan(materialized, repeatRoots, sdk, fsharp2Host));
        ThrowIfInvalid(CoreCompileRunner.ValidateLaneEquality(repeatPlan));
        var repeatTasks = new[]
        {
            CoreCompileRunner.RunAsync(repeatPlan.Oracle, cancellationToken),
            CoreCompileRunner.RunAsync(repeatPlan.FSharp2, cancellationToken),
        };
        var repeatLanes = await Task.WhenAll(repeatTasks).ConfigureAwait(false);
        var oracleRepeat = repeatLanes[0];
        var fsharp2Repeat = repeatLanes[1];
        RequirePhysicalCompilerTask("oracle-repeat", oracleRepeat, missingEvidence);
        RequirePhysicalCompilerTask("fsharp2-repeat", fsharp2Repeat, missingEvidence);
        if (oracleRepeat.Process.TimedOut || fsharp2Repeat.Process.TimedOut)
        {
            missingEvidence.Add("a deterministic repeat lane timed out");
        }
        if (DetectFallback(repeatPlan.FSharp2, fsharp2Repeat))
        {
            missingEvidence.Add("the FSharp2 deterministic repeat selected a forbidden compiler fallback");
        }
        var oracleRepeatPaths = ArtifactPaths(repeatRoots.Oracle, materialized);
        var fsharp2RepeatPaths = ArtifactPaths(repeatRoots.FSharp2, materialized);
        var repeatPassed = true;
        var repeatHashes = new Dictionary<string, string>(StringComparer.Ordinal);
        CompareRepeatArtifacts(
            "oracle",
            materialized.RequestedArtifacts,
            oracleSnapshot,
            oracleRepeatPaths,
            comparisons,
            missingEvidence,
            repeatHashes,
            ref repeatPassed);
        CompareRepeatArtifacts(
            "fsharp2",
            materialized.RequestedArtifacts,
            fsharp2Snapshot,
            fsharp2RepeatPaths,
            comparisons,
            missingEvidence,
            repeatHashes,
            ref repeatPassed);
        probes.Add(new ProbeEvidence(
            "within-compiler-repeat",
            repeatPassed,
            JsonSerializer.SerializeToElement(repeatHashes, EvidenceJson),
            repeatPassed ? null : "A deterministic repeat changed or omitted a requested artifact."));
        AddArtifactFiles("oracle-repeat", oracleRepeatPaths, artifacts);
        AddArtifactFiles("fsharp2-repeat", fsharp2RepeatPaths, artifacts);
        return new RepeatRunEvidence(repeatPlan, oracleRepeat, fsharp2Repeat);
    }

    private static ImmutableDictionary<string, ImmutableArray<byte>> SnapshotArtifacts(
        ImmutableArray<string> requestedArtifacts,
        ArtifactPathSet paths)
    {
        var snapshot = ImmutableDictionary.CreateBuilder<string, ImmutableArray<byte>>(StringComparer.Ordinal);
        foreach (var kind in requestedArtifacts)
        {
            var path = paths.Path(kind);
            if (File.Exists(path))
            {
                snapshot[kind] = [.. File.ReadAllBytes(path)];
            }
        }
        return snapshot.ToImmutable();
    }

    private static CoreCompilePlan CreateRepeatPlan(CoreCompilePlan plan) => plan with
    {
        Oracle = CreateRepeatInvocation(plan.Oracle),
        FSharp2 = CreateRepeatInvocation(plan.FSharp2),
    };

    private static LaneInvocation CreateRepeatInvocation(LaneInvocation invocation)
    {
        var laneRoot = Directory.GetParent(Path.GetDirectoryName(invocation.ProjectPath)!)!.FullName;
        var laneName = invocation.Kind == LaneKind.Oracle ? "oracle" : "fsharp2";
        var repeatBinlog = Path.Combine(laneRoot, $"{laneName}-repeat.binlog");
        var arguments = invocation.Arguments.Select(argument =>
        {
            if (string.Equals(argument, "/t:Build", StringComparison.Ordinal))
            {
                return "/t:Rebuild";
            }
            return argument.StartsWith("/bl:", StringComparison.Ordinal)
                ? $"/bl:{repeatBinlog}"
                : argument;
        }).ToImmutableArray();
        return invocation with { Arguments = arguments };
    }

    private static void CompareRepeatArtifacts(
        string lane,
        ImmutableArray<string> requestedArtifacts,
        ImmutableDictionary<string, ImmutableArray<byte>> first,
        ArtifactPathSet second,
        ImmutableArray<ComparisonResult>.Builder comparisons,
        ImmutableArray<string>.Builder missingEvidence,
        IDictionary<string, string> hashes,
        ref bool repeatPassed)
    {
        foreach (var kind in requestedArtifacts)
        {
            var secondPath = second.Path(kind);
            if (!first.TryGetValue(kind, out var firstBytes) || !File.Exists(secondPath))
            {
                var difference = $"The {lane} deterministic repeat did not publish requested artifact '{kind}'.";
                var firstEvidence = CanonicalJson.Canonicalize(JsonSerializer.SerializeToElement(new
                {
                    kind,
                    present = first.ContainsKey(kind),
                }));
                var secondEvidence = CanonicalJson.Canonicalize(JsonSerializer.SerializeToElement(new
                {
                    kind,
                    present = File.Exists(secondPath),
                }));
                comparisons.Add(ComparisonSupport.Create(
                    "artifact-repeat",
                    "within-compiler-exact",
                    "repeat-v1",
                    false,
                    difference,
                    firstEvidence,
                    secondEvidence,
                    firstEvidence,
                    secondEvidence));
                missingEvidence.Add(difference);
                repeatPassed = false;
                continue;
            }

            var comparison = DeterminismComparator.Compare(
                firstBytes,
                [.. File.ReadAllBytes(secondPath)]);
            comparisons.Add(comparison);
            repeatPassed &= comparison.Passed;
            hashes[$"{lane}:{kind}:first"] = Hashing.Sha256(firstBytes.AsSpan());
            hashes[$"{lane}:{kind}:second"] = Hashing.Sha256File(secondPath);
        }
    }

    private static async Task<ImmutableArray<ProbeEvidence>> RunDeclaredProbesAsync(
        MaterializedCase materialized,
        ArtifactPathSet paths,
        SdkSelectionResult sdk,
        CancellationToken cancellationToken)
    {
        var timeout = TimeSpan.FromMilliseconds(
            materialized.ResolvedDocument.GetProperty("timeout").GetProperty("probeMs").GetInt32());
        var evidence = ImmutableArray.CreateBuilder<ProbeEvidence>();
        foreach (var probe in materialized.ResolvedDocument.GetProperty("probes").EnumerateArray())
        {
            var kind = probe.GetProperty("kind").GetString()!;
            if (kind == "within-compiler-repeat")
            {
                continue;
            }
            var artifactPath = kind == "portable-pdb" ? paths.PortablePdb : paths.Implementation;
            evidence.Add(await ProbeRunner.VerifyAsync(
                new ProbeRequest(
                    kind,
                    artifactPath,
                    ProbeConfiguration(probe.GetProperty("configuration"), sdk),
                    timeout),
                cancellationToken).ConfigureAwait(false));
        }
        return evidence.ToImmutable();
    }

    private static JsonElement ProbeConfiguration(JsonElement values, SdkSelectionResult sdk)
    {
        var configuration = new Dictionary<string, object?>(StringComparer.Ordinal);
        foreach (var item in values.EnumerateArray())
        {
            configuration[item.GetProperty("name").GetString()!] = JsonValue(item.GetProperty("value"));
        }
        configuration["dotnetPath"] = sdk.DotnetPath;
        configuration["environment"] = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            ["DOTNET_ROOT"] = sdk.Root,
            ["DOTNET_MULTILEVEL_LOOKUP"] = "0",
            ["PATH"] = sdk.Environment["PATH"],
        };
        return JsonSerializer.SerializeToElement(configuration, EvidenceJson);
    }

    private static object? JsonValue(JsonElement value) => value.ValueKind switch
    {
        JsonValueKind.String => value.GetString(),
        JsonValueKind.Number when value.TryGetInt64(out var integer) => integer,
        JsonValueKind.Number => value.GetDouble(),
        JsonValueKind.True => true,
        JsonValueKind.False => false,
        JsonValueKind.Null => null,
        _ => value.Clone(),
    };

    private static BehaviorObservation Behavior(ProbeEvidence probe)
    {
        var observation = probe.Observation;
        return new BehaviorObservation(
            ReadInt32(observation, "ExitCode", "exitCode"),
            ReadString(observation, "standardOutput"),
            ReadString(observation, "standardError"),
            StructuredObservation(observation),
            ReadNullableString(observation, "exception", "Exception"));
    }

    private static JsonElement StructuredObservation(JsonElement observation)
    {
        var excluded = new HashSet<string>(
            ["ExitCode", "exitCode", "standardOutput", "standardError", "exception", "Exception"],
            StringComparer.Ordinal);
        using var stream = new MemoryStream();
        using (var writer = new Utf8JsonWriter(stream))
        {
            writer.WriteStartObject();
            foreach (var property in observation.EnumerateObject())
            {
                if (!excluded.Contains(property.Name))
                {
                    property.WriteTo(writer);
                }
            }
            writer.WriteEndObject();
        }
        using var document = JsonDocument.Parse(stream.ToArray());
        return document.RootElement.Clone();
    }

    private static int ReadInt32(JsonElement value, string first, string second) =>
        value.TryGetProperty(first, out var property) && property.TryGetInt32(out var result)
            ? result
            : value.TryGetProperty(second, out property) && property.TryGetInt32(out result)
                ? result
                : 0;

    private static string ReadString(JsonElement value, string name) =>
        value.TryGetProperty(name, out var property) && property.ValueKind == JsonValueKind.String
            ? property.GetString()!
            : string.Empty;

    private static string? ReadNullableString(JsonElement value, string first, string second) =>
        value.TryGetProperty(first, out var property) && property.ValueKind == JsonValueKind.String
            ? property.GetString()
            : value.TryGetProperty(second, out property) && property.ValueKind == JsonValueKind.String
                ? property.GetString()
                : null;

    private static ComparisonResult CompareExpectedDiagnostics(
        ConformanceRepository repository,
        MaterializedCase materialized,
        CompilationResult compilation)
    {
        var expected = ExpectedDiagnostics(repository, materialized.ResolvedDocument);
        var actual = JsonSerializer.SerializeToElement(
            compilation.Diagnostics.Select((diagnostic, index) => DiagnosticFact.FromCompilation(diagnostic, index)),
            EvidenceJson);
        return DiagnosticComparator.Compare(
            expected,
            actual,
            JsonSerializer.SerializeToElement(new { id = "exact-v1" }));
    }

    private static JsonElement ExpectedDiagnostics(
        ConformanceRepository repository,
        JsonElement conformanceCase)
    {
        var declaration = conformanceCase.GetProperty("expectedDiagnostics");
        var mode = declaration.GetProperty("mode").GetString();
        if (mode == "none")
        {
            return JsonSerializer.SerializeToElement(Array.Empty<DiagnosticFact>(), EvidenceJson);
        }
        if (mode == "inline")
        {
            return JsonSerializer.SerializeToElement(
                declaration.GetProperty("inlineFacts").EnumerateArray().Select(DiagnosticFact.FromInline),
                EvidenceJson);
        }
        var relativePath = declaration.GetProperty("lockPath").GetString()!;
        using var document = JsonDocument.Parse(File.ReadAllBytes(Path.Combine(
            repository.Root,
            relativePath.Replace('/', Path.DirectorySeparatorChar))));
        return JsonSerializer.SerializeToElement(
            document.RootElement.GetProperty("diagnostics").EnumerateArray().Select(DiagnosticFact.FromLock),
            EvidenceJson);
    }

    private static ComparisonResult CompareLaneDiagnostics(
        LaneRootSet roots,
        CoreCompileLaneResult oracle,
        CoreCompileLaneResult fsharp2)
    {
        var pathMap = new Dictionary<string, string>(StringComparer.Ordinal)
        {
            [roots.Oracle.Work] = "/_fsharp2_conformance/root",
            [roots.FSharp2.Work] = "/_fsharp2_conformance/root",
        };
        var rule = JsonSerializer.SerializeToElement(new
        {
            id = "logical-root-v1",
            @class = "normalized",
            pathMap,
            newline = "lf",
        });
        return DiagnosticComparator.CompareStreams(
            oracle.Process.ExitCode,
            oracle.Process.StandardOutput,
            oracle.Process.StandardError,
            fsharp2.Process.ExitCode,
            fsharp2.Process.StandardOutput,
            fsharp2.Process.StandardError,
            rule);
    }

    private static void ComparePublication(
        MaterializedCase materialized,
        ArtifactPathSet oracle,
        ArtifactPathSet fsharp2,
        ImmutableArray<ComparisonResult>.Builder comparisons,
        ImmutableArray<string>.Builder missingEvidence)
    {
        var expectedStates = materialized.ResolvedDocument.GetProperty("expectedArtifacts")
            .EnumerateArray()
            .Select(artifact => new ArtifactPublication(
                artifact.GetProperty("kind").GetString()!,
                artifact.GetProperty("state").GetString()! == "required" ? "present" : "absent",
                artifact.GetProperty("state").GetString()! == "required" ? "present" : "absent"))
            .ToArray();
        var actualStates = expectedStates.Select(expected => new ArtifactPublication(
            expected.Kind,
            File.Exists(oracle.Path(expected.Kind)) ? "present" : "absent",
            File.Exists(fsharp2.Path(expected.Kind)) ? "present" : "absent"))
            .ToArray();
        comparisons.Add(DiagnosticComparator.Compare(
            JsonSerializer.SerializeToElement(expectedStates, EvidenceJson),
            JsonSerializer.SerializeToElement(actualStates, EvidenceJson),
            JsonSerializer.SerializeToElement(new { id = "publication-v1" })));
        foreach (var mismatch in expectedStates.Zip(actualStates).Where(static pair => pair.First != pair.Second))
        {
            missingEvidence.Add($"artifact publication changed for '{mismatch.First.Kind}'");
        }
    }

    private static void RequirePhysicalCompilerTask(
        string name,
        CoreCompileLaneResult lane,
        ImmutableArray<string>.Builder missingEvidence)
    {
        if (lane.Binlog.CoreCompileInvocationCount != 1 || lane.Binlog.CoreCompileSkipped)
        {
            missingEvidence.Add(
                $"{name} recorded {lane.Binlog.CoreCompileInvocationCount} physical compiler tasks and skipped={lane.Binlog.CoreCompileSkipped}");
        }
    }

    private static bool DetectFallback(LaneInvocation invocation, CoreCompileLaneResult lane)
    {
        var values = invocation.Arguments
            .Concat(lane.Process.Processes.Select(static process => process.ExecutablePath))
            .Concat(lane.Process.Processes.SelectMany(static process => process.Arguments))
            .Concat(lane.Binlog.CompilerCommandLines);
        return values.Any(IsSdkFsc);
    }

    private static bool IsSdkFsc(string value)
    {
        var normalized = value.Replace('\\', '/');
        return normalized.Contains("/sdk/", StringComparison.OrdinalIgnoreCase)
               && normalized.Contains("/FSharp/fsc.dll", StringComparison.OrdinalIgnoreCase);
    }

    private static ArtifactPathSet ArtifactPaths(LaneRoot root, MaterializedCase materialized)
    {
        var assemblyName = materialized.ResolvedDocument.GetProperty("project").GetProperty("assemblyName").GetString()!;
        return new ArtifactPathSet(
            Path.Combine(root.Output, assemblyName + ".dll"),
            Path.Combine(root.Output, assemblyName + ".pdb"),
            Path.Combine(root.Intermediate, "ref", assemblyName + ".dll"),
            Path.Combine(root.Output, assemblyName + ".xml"));
    }

    private static void AddArtifactFiles(
        string lane,
        ArtifactPathSet paths,
        ImmutableDictionary<string, ImmutableArray<byte>>.Builder artifacts)
    {
        foreach (var (name, path) in paths.All())
        {
            if (File.Exists(path))
            {
                artifacts[$"artifacts/{lane}/{name}"] = [.. File.ReadAllBytes(path)];
            }
        }
    }

    private static async Task WriteRunAsync(
        string runRoot,
        string runId,
        ConformanceRepository repository,
        MaterializedCase materialized,
        SdkSelectionResult sdk,
        string fsharp2Host,
        CompilationResult compilation,
        CoreEvidence coreEvidence,
        CoreCompilePlan? plan,
        CoreCompileLaneResult? oracle,
        CoreCompileLaneResult? fsharp2,
        RepeatRunEvidence? repeat,
        ImmutableArray<ProbeEvidence> probes,
        ImmutableArray<ComparisonResult> comparisons,
        VerdictResult verdict,
        ImmutableDictionary<string, ImmutableArray<byte>> artifacts,
        CancellationToken cancellationToken)
    {
        var comparison = JsonSerializer.SerializeToElement(new
        {
            schemaVersion = 1,
            comparisons,
            probes,
            sealingRule = "run-result-v1-normalized-bundle-hash",
        }, EvidenceJson);
        var files = ImmutableDictionary.CreateBuilder<string, ImmutableArray<byte>>(StringComparer.Ordinal);
        files["resolved-manifest.json"] = [.. CanonicalJson.Canonicalize(repository.Manifest)];
        files["resolved-case.json"] = [.. CanonicalJson.Canonicalize(materialized.ResolvedDocument)];
        files["comparison-policy.json"] = [.. CanonicalJson.Canonicalize(repository.ComparisonPolicy)];
        files["toolchain.json"] = [.. CanonicalJson.Canonicalize(repository.Toolchain)];
        files["core-evidence.json"] = [.. CanonicalJson.Canonicalize(
            JsonSerializer.SerializeToElement(coreEvidence, EvidenceJson))];
        files["comparison.json"] = [.. CanonicalJson.Canonicalize(comparison)];
        files["bundle-sealing.json"] = [.. CanonicalJson.Canonicalize(JsonSerializer.SerializeToElement(new
        {
            schemaVersion = 1,
            rule = "Hash canonical run-result.json with verdict.bundleHash set to the all-zero SHA-256 placeholder. Hash the bundle manifest next. Store that manifest hash in verdict.bundleHash and verify both values together.",
        }))];
        var fallbackReceiptPath = Path.Combine(runRoot, "fallback-sentinel.receipt");
        if (File.Exists(fallbackReceiptPath))
        {
            files["fallback-sentinel.receipt"] = [.. File.ReadAllBytes(fallbackReceiptPath)];
        }
        files[$"tools/{Path.GetFileName(fsharp2Host)}"] = [.. File.ReadAllBytes(fsharp2Host)];
        foreach (var source in materialized.Sources)
        {
            files[$"inputs/sources/{source.StableId}-{Path.GetFileName(source.LogicalPath)}"] = source.Bytes;
        }
        foreach (var reference in materialized.TargetReferences)
        {
            files[$"inputs/references/{reference.StableId}-{Path.GetFileName(reference.LogicalPath)}"] = reference.Bytes;
        }
        foreach (var artifact in artifacts)
        {
            files[artifact.Key] = artifact.Value;
        }
        AddLaneFiles(files, "oracle", plan?.Oracle, oracle);
        AddLaneFiles(files, "fsharp2", plan?.FSharp2, fsharp2);
        AddLaneFiles(files, "oracle-repeat", repeat?.Plan.Oracle, repeat?.Oracle);
        AddLaneFiles(files, "fsharp2-repeat", repeat?.Plan.FSharp2, repeat?.FSharp2);
        var oracleCleanup = await CleanupLaneAsync(
            materialized,
            plan?.Oracle,
            oracle,
            repeat?.Oracle,
            cancellationToken).ConfigureAwait(false);
        var fsharp2Cleanup = await CleanupLaneAsync(
            materialized,
            plan?.FSharp2,
            fsharp2,
            repeat?.FSharp2,
            cancellationToken).ConfigureAwait(false);
        AddCleanupReceipt(files, "oracle", oracleCleanup);
        AddCleanupReceipt(files, "fsharp2", fsharp2Cleanup);
        var runResult = RunResultWriter.Create(
            runId,
            null,
            repository,
            materialized,
            sdk,
            fsharp2Host,
            compilation,
            coreEvidence,
            plan,
            oracle,
            fsharp2,
            probes,
            comparisons,
            verdict,
            artifacts,
            oracleCleanup,
            fsharp2Cleanup);

        var receipt = BundleWriter.Write(
            runRoot,
            new BundleInput(runId, null, files.ToImmutable(), runResult));
        var verified = BundleReader.ReadAndVerify(receipt.BundleRoot);
        if (!string.Equals(receipt.BundleHash, verified.BundleHash, StringComparison.Ordinal))
        {
            throw new ConformanceContractException(
                [new("bundle-hash", receipt.BundleRoot, "The written bundle did not verify with its receipt hash.")]);
        }
        File.Copy(
            Path.Combine(receipt.BundleRoot, "run-result.json"),
            Path.Combine(runRoot, "run-result.json"),
            overwrite: true);
        await File.WriteAllBytesAsync(
            Path.Combine(runRoot, "comparison.json"),
            CanonicalJson.Canonicalize(comparison),
            cancellationToken).ConfigureAwait(false);
    }

    private static async Task<CleanupReceipt?> CleanupLaneAsync(
        MaterializedCase materialized,
        LaneInvocation? invocation,
        CoreCompileLaneResult? result,
        CoreCompileLaneResult? repeatResult,
        CancellationToken cancellationToken)
    {
        if (invocation is null || result is null)
        {
            return null;
        }
        var workRoot = Path.GetDirectoryName(invocation.ProjectPath)!;
        var laneRoot = Directory.GetParent(workRoot)!.FullName;
        var deleteRoots = LaneDeleteRoots(materialized.ResolvedDocument);
        var retainedPaths = Directory.EnumerateFileSystemEntries(laneRoot, "*", SearchOption.TopDirectoryOnly)
            .Select(Path.GetFileName)
            .Where(static path => !string.IsNullOrEmpty(path))
            .Select(static path => path!)
            .Where(path => path != ".fsharp2-conformance-root" && !deleteRoots.Contains(path, StringComparer.Ordinal))
            .OrderBy(static path => path, StringComparer.Ordinal)
            .ToImmutableArray();
        var timeout = TimeSpan.FromMilliseconds(
            materialized.ResolvedDocument.GetProperty("timeout").GetProperty("cleanupMs").GetInt32());
        return await CleanupManager.CleanupAsync(
            laneRoot,
            result.Process.Processes
                .Concat(repeatResult?.Process.Processes ?? [])
                .ToImmutableArray(),
            new CleanupPolicy(false, retainedPaths, timeout),
            cancellationToken).ConfigureAwait(false);
    }

    private static ImmutableArray<string> LaneDeleteRoots(JsonElement resolvedCase)
    {
        const string prefix = "lanes/*/";
        var roots = ImmutableArray.CreateBuilder<string>();
        foreach (var item in resolvedCase.GetProperty("cleanup").GetProperty("delete").EnumerateArray())
        {
            var path = item.GetString()!;
            if (!path.StartsWith(prefix, StringComparison.Ordinal)
                || path[prefix.Length..].Contains('/'))
            {
                throw new ConformanceContractException(
                    [new("cleanup-path", path, "A lane cleanup path must identify one lane-root directory.")]);
            }
            roots.Add(path[prefix.Length..]);
        }
        return roots.ToImmutable();
    }

    private static void AddCleanupReceipt(
        ImmutableDictionary<string, ImmutableArray<byte>>.Builder files,
        string name,
        CleanupReceipt? receipt)
    {
        if (receipt is null)
        {
            return;
        }
        files[$"{name}/cleanup-receipt.json"] = [.. CanonicalJson.Canonicalize(
            JsonSerializer.SerializeToElement(receipt, EvidenceJson))];
    }

    private static void AddLaneFiles(
        ImmutableDictionary<string, ImmutableArray<byte>>.Builder files,
        string name,
        LaneInvocation? invocation,
        CoreCompileLaneResult? result)
    {
        if (invocation is null || result is null)
        {
            return;
        }
        files[$"{name}/invocation.json"] = [.. CanonicalJson.Canonicalize(
            JsonSerializer.SerializeToElement(invocation, EvidenceJson))];
        files[$"{name}/processes.json"] = [.. CanonicalJson.Canonicalize(
            JsonSerializer.SerializeToElement(result.Process.Processes, EvidenceJson))];
        files[$"{name}/binlog-evidence.json"] = [.. CanonicalJson.Canonicalize(
            JsonSerializer.SerializeToElement(result.Binlog, EvidenceJson))];
        files[$"{name}/stdout.bin"] = result.Process.StandardOutput;
        files[$"{name}/stderr.bin"] = result.Process.StandardError;
        var workRoot = Path.GetDirectoryName(invocation.ProjectPath)!;
        foreach (var path in Directory.EnumerateFiles(workRoot, "*", SearchOption.AllDirectories))
        {
            var relativePath = Path.GetRelativePath(workRoot, path).Replace('\\', '/');
            files[$"{name}/work/{relativePath}"] = [.. File.ReadAllBytes(path)];
        }
        if (invocation.Properties.TryGetValue("MSBuildProjectExtensionsPath", out var intermediateRoot))
        {
            var projectAssetsPath = Path.Combine(intermediateRoot, "project.assets.json");
            if (File.Exists(projectAssetsPath))
            {
                files[$"{name}/obj/project.assets.json"] = [.. File.ReadAllBytes(projectAssetsPath)];
            }
        }
        if (invocation.Properties.TryGetValue("FSharp2ConformancePackageSource", out var packageSource)
            && Directory.Exists(packageSource))
        {
            foreach (var path in Directory.EnumerateFiles(packageSource, "*", SearchOption.TopDirectoryOnly))
            {
                files[$"inputs/packages/{Path.GetFileName(path)}"] = [.. File.ReadAllBytes(path)];
            }
        }
        var binlogArgument = invocation.Arguments.FirstOrDefault(static argument =>
            argument.StartsWith("/bl:", StringComparison.Ordinal));
        if (binlogArgument is not null && File.Exists(binlogArgument[4..]))
        {
            files[$"{name}/corecompile.binlog"] = [.. File.ReadAllBytes(binlogArgument[4..])];
        }
        if (invocation.Properties.TryGetValue("RestoreConfigFile", out var restoreConfigPath)
            && File.Exists(restoreConfigPath))
        {
            files[$"{name}/NuGet.Config"] = [.. File.ReadAllBytes(restoreConfigPath)];
        }
    }

    private static ConformanceCase RequireCase(ConformanceRepository repository, string caseId) =>
        repository.Cases.FirstOrDefault(item => string.Equals(item.CaseId, caseId, StringComparison.Ordinal))
        ?? throw new ConformanceContractException(
            [new("case-missing", caseId, "The selected conformance case does not exist.")]);

    private static void ThrowIfInvalid(ValidationResult result)
    {
        if (!result.IsValid)
        {
            throw new ConformanceContractException(result.Issues);
        }
    }

    private static string FindRepositoryRoot()
    {
        var directory = new DirectoryInfo(Directory.GetCurrentDirectory());
        while (directory is not null)
        {
            if (File.Exists(Path.Combine(directory.FullName, "fsharp2.sln")))
            {
                return directory.FullName;
            }
            directory = directory.Parent;
        }
        throw new ConformanceContractException(
            [new("repository-root", Directory.GetCurrentDirectory(), "The repository root could not be found.")]);
    }

    private static string VerdictName(ConformanceVerdict verdict) => verdict switch
    {
        ConformanceVerdict.Pass => "pass",
        ConformanceVerdict.Fail => "fail",
        ConformanceVerdict.Unsupported => "unsupported",
        ConformanceVerdict.InfraError => "infra-error",
        _ => throw new ArgumentOutOfRangeException(nameof(verdict)),
    };

    private sealed record ArtifactPathSet(
        string Implementation,
        string PortablePdb,
        string ReferenceAssembly,
        string Documentation)
    {
        public string Path(string kind) => kind switch
        {
            "implementation-assembly" => Implementation,
            "portable-pdb" => PortablePdb,
            "reference-assembly" => ReferenceAssembly,
            "xml-documentation" => Documentation,
            _ => string.Empty,
        };

        public IEnumerable<(string Name, string Path)> All()
        {
            yield return ("implementation.dll", Implementation);
            yield return ("portable.pdb", PortablePdb);
            yield return ("reference.dll", ReferenceAssembly);
            yield return ("documentation.xml", Documentation);
        }
    }

    private sealed record ArtifactPublication(string Kind, string Oracle, string FSharp2);

    private sealed record DiagnosticFact(
        string Code,
        string Severity,
        string Message,
        string Source,
        int? StartLine,
        int? StartColumn,
        int? EndLine,
        int? EndColumn,
        string Stream,
        int Sequence)
    {
        public static DiagnosticFact FromCompilation(CompilationDiagnostic diagnostic, int sequence)
        {
            var projection = DiagnosticProjection.Create(diagnostic);
            var range = projection.Range;
            return new DiagnosticFact(
                projection.Code,
                projection.EffectiveSeverity switch
                {
                    "information" => "info",
                    var value => value,
                },
                Flatten(projection.Message),
                projection.LogicalPath ?? string.Empty,
                range?.Start.Line,
                range?.Start.Column,
                range?.End.Line,
                range?.End.Column,
                projection.Stream ?? string.Empty,
                sequence);
        }

        public static DiagnosticFact FromInline(JsonElement diagnostic) => new(
            diagnostic.GetProperty("code").GetString()!,
            diagnostic.GetProperty("severity").GetString()!,
            diagnostic.GetProperty("message").GetString()!,
            diagnostic.GetProperty("source").GetString()!,
            diagnostic.GetProperty("startLine").GetInt32(),
            diagnostic.GetProperty("startColumn").GetInt32(),
            diagnostic.GetProperty("endLine").GetInt32(),
            diagnostic.GetProperty("endColumn").GetInt32(),
            diagnostic.GetProperty("stream").GetString()!,
            diagnostic.GetProperty("sequence").GetInt32());

        public static DiagnosticFact FromLock(JsonElement diagnostic)
        {
            var range = diagnostic.GetProperty("range");
            return new DiagnosticFact(
                diagnostic.GetProperty("code").GetString()!,
                diagnostic.GetProperty("adjustedSeverity").GetString()!,
                diagnostic.GetProperty("message").GetString()!,
                diagnostic.GetProperty("logicalSource").GetString()!,
                range.GetProperty("startLine").GetInt32(),
                range.GetProperty("startColumn").GetInt32(),
                range.GetProperty("endLine").GetInt32(),
                range.GetProperty("endColumn").GetInt32(),
                diagnostic.GetProperty("stream").GetString()!,
                diagnostic.GetProperty("sequence").GetInt32());
        }

        private static string Flatten(string value) =>
            value.Replace("\r\n", "\n", StringComparison.Ordinal)
                .Replace('\r', '\n')
                .Replace('\n', '\u001d');
    }

    private static async Task WriteOracleEvidenceAsync(
        string runRoot,
        ConformanceRepository repository,
        MaterializedCase materialized,
        SdkSelectionResult sdk,
        LaneInvocation invocation,
        CoreCompileLaneResult lane,
        CancellationToken cancellationToken)
    {
        var oracleRoot = Path.Combine(runRoot, "oracle");
        Directory.CreateDirectory(oracleRoot);
        await File.WriteAllBytesAsync(
            Path.Combine(runRoot, "resolved-manifest.json"),
            CanonicalJson.Canonicalize(repository.Manifest),
            cancellationToken).ConfigureAwait(false);
        await File.WriteAllBytesAsync(
            Path.Combine(runRoot, "resolved-case.json"),
            CanonicalJson.Canonicalize(materialized.ResolvedDocument),
            cancellationToken).ConfigureAwait(false);
        await WriteJsonAsync(
            Path.Combine(runRoot, "environment.json"),
            new
            {
                sdk.Root,
                sdk.DotnetPath,
                sdk.Version,
                sdk.Environment,
            },
            cancellationToken).ConfigureAwait(false);
        await WriteJsonAsync(
            Path.Combine(oracleRoot, "invocation.json"),
            new
            {
                lane = invocation.Kind.ToString().ToLowerInvariant(),
                invocation.ProjectPath,
                invocation.Inputs,
                invocation.Properties,
                invocation.Environment,
                invocation.Arguments,
            },
            cancellationToken).ConfigureAwait(false);
        await File.WriteAllBytesAsync(
            Path.Combine(oracleRoot, "stdout.bin"),
            lane.Process.StandardOutput.ToArray(),
            cancellationToken).ConfigureAwait(false);
        await File.WriteAllBytesAsync(
            Path.Combine(oracleRoot, "stderr.bin"),
            lane.Process.StandardError.ToArray(),
            cancellationToken).ConfigureAwait(false);
        await WriteJsonAsync(
            Path.Combine(oracleRoot, "processes.json"),
            lane.Process.Processes,
            cancellationToken).ConfigureAwait(false);
        await WriteJsonAsync(
            Path.Combine(runRoot, "run-result.json"),
            new
            {
                schemaVersion = 1,
                runId = Path.GetFileName(runRoot),
                materialized.CaseId,
                capturedAt = DateTimeOffset.UtcNow,
                process = new
                {
                    lane.Process.ExitCode,
                    lane.Process.TimedOut,
                    lane.Process.StartedAt,
                    lane.Process.EndedAt,
                },
                binlog = lane.Binlog,
                verdict = CoreCompileRunner.Classify(lane.Process).ToString().ToLowerInvariant(),
            },
            cancellationToken).ConfigureAwait(false);
    }

    private static Task WriteJsonAsync(
        string path,
        object value,
        CancellationToken cancellationToken) =>
        File.WriteAllBytesAsync(
            path,
            JsonSerializer.SerializeToUtf8Bytes(value, EvidenceJson),
            cancellationToken);

    private static void RejectCheckedInOutput(string conformanceRoot, string outputRoot)
    {
        var root = Path.GetFullPath(conformanceRoot)
            .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        if (outputRoot.Equals(root, StringComparison.OrdinalIgnoreCase)
            || outputRoot.StartsWith(root + Path.DirectorySeparatorChar, StringComparison.OrdinalIgnoreCase))
        {
            throw new ConformanceContractException(
                [new(
                    "output-root",
                    outputRoot,
                    "Oracle capture output must be outside the checked-in conformance root.")]);
        }
    }

    private static string SafeName(string value) =>
        string.Concat(value.Select(static character =>
            char.IsAsciiLetterOrDigit(character) || character is '-' or '_' ? character : '-'));
}
