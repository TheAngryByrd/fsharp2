using System.Collections.Immutable;
using System.Runtime.InteropServices;
using System.Text.Json;
using FSharp2.Compiler;

namespace FSharp2.Conformance;

public static class RunResultWriter
{
    private static readonly JsonSerializerOptions Json = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
    };

    public static JsonElement Create(
        string runId,
        string? parentRunId,
        ConformanceRepository repository,
        MaterializedCase materialized,
        SdkSelectionResult sdk,
        string fsharp2Host,
        CompilationResult compilation,
        CoreEvidence coreEvidence,
        CoreCompilePlan? plan,
        CoreCompileLaneResult? oracle,
        CoreCompileLaneResult? fsharp2,
        ImmutableArray<ProbeEvidence> probes,
        ImmutableArray<ComparisonResult> comparisons,
        VerdictResult verdict,
        ImmutableDictionary<string, ImmutableArray<byte>> artifacts,
        CleanupReceipt? oracleCleanup = null,
        CleanupReceipt? fsharp2Cleanup = null)
    {
        var contractHashes = new[]
        {
            CreateHashEntry("manifest", CanonicalJson.Canonicalize(repository.Manifest)),
            CreateHashEntry("comparison-policy", CanonicalJson.Canonicalize(repository.ComparisonPolicy)),
            CreateHashEntry("toolchain", CanonicalJson.Canonicalize(repository.Toolchain)),
            CreateHashEntry("case", CanonicalJson.Canonicalize(materialized.ResolvedDocument)),
        };
        var tools = new[] { CreateHashEntry("fsharp2-host", File.ReadAllBytes(fsharp2Host)) };
        var environmentValue = new
        {
            operatingSystem = OperatingSystemName(),
            rid = RuntimeInformation.RuntimeIdentifier,
            architecture = RuntimeInformation.ProcessArchitecture.ToString().ToLowerInvariant(),
            sdkVersion = sdk.Version,
            sdkCommit = repository.Toolchain.GetProperty("sdkCommit").GetString(),
            tools,
            culture = "en-US",
            encoding = "utf-8",
        };
        var sources = materialized.Sources.Select(static source => new HashEntry(source.StableId, source.Sha256));
        var references = materialized.TargetReferences.Select(static reference => new HashEntry(reference.StableId, reference.Sha256));
        var optionsHash = HashValue(new
        {
            materialized.Options.Semantic,
            materialized.Options.Diagnostic,
            materialized.Options.Emission,
            materialized.Options.Signing,
            materialized.Options.Resources,
            materialized.Options.CompilerArguments,
        });
        var materializedHash = HashValue(new
        {
            materialized.CaseId,
            sources,
            references,
            optionsHash,
            materialized.RequestedArtifacts,
        });
        var artifactEntries = artifacts
            .OrderBy(static item => item.Key, StringComparer.Ordinal)
            .Select(static item => CreateHashEntry(item.Key, item.Value.AsSpan()))
            .ToArray();
        var probeEntries = probes
            .Select((probe, index) => CreateHashEntry($"{probe.Kind}:{index}", CanonicalJson.Canonicalize(probe.Observation)))
            .ToArray();
        var packageLocks = plan?.RestoredDependencyInputs
            .Select(static input => new HashEntry(input.LogicalPath, input.Sha256))
            .ToArray()
            ?? [];
        var comparisonEntries = comparisons.Select(comparison => new
        {
            comparison.ComparatorId,
            comparison.Version,
            comparison.Class,
            rawHashes = comparison.RawHashes,
            canonicalValues = comparison.CanonicalValues,
            comparison.Rule,
            comparison.Passed,
            comparison.Difference,
        });
        var result = JsonSerializer.SerializeToElement(new
        {
            schemaVersion = 1,
            identity = new
            {
                runId,
                caseId = materialized.CaseId,
                attempt = 1,
                parentRunId,
                recordedAt = DateTimeOffset.UtcNow,
                contractHashes,
            },
            environment = new
            {
                environmentValue.operatingSystem,
                environmentValue.rid,
                environmentValue.architecture,
                environmentValue.sdkVersion,
                environmentValue.sdkCommit,
                environmentValue.tools,
                environmentValue.culture,
                environmentValue.encoding,
                environmentHash = HashValue(environmentValue),
            },
            inputs = new
            {
                resolvedCaseHash = Hashing.Sha256(CanonicalJson.Canonicalize(materialized.ResolvedDocument)),
                sources,
                references,
                optionsHash,
                packageLocks,
                materializedHash,
            },
            coreEvidence = new
            {
                contractVersion = coreEvidence.ContractVersion,
                outcome = coreEvidence.Outcome,
                diagnostics = compilation.Diagnostics.Select(FormatDiagnostic),
                artifacts = coreEvidence.Artifacts.Select(static artifact =>
                    new HashEntry(artifact.StableId, Hashing.Sha256(artifact.Bytes.AsSpan()))),
                fingerprints = coreEvidence.Artifacts.Select(static artifact =>
                    new HashEntry(artifact.Kind, artifact.Fingerprint)),
                phases = coreEvidence.Phases.Select(static phase => new
                {
                    phase.Phase,
                    status = PhaseStatus(phase.Status),
                    phase.InputFingerprint,
                    phase.OutputFingerprint,
                    traces = phase.TraceValues,
                }),
                traces = coreEvidence.Traces,
            },
            lanes = new
            {
                oracle = Lane("oracle", plan?.Oracle, oracle, oracleCleanup),
                fsharp2 = Lane("fsharp2-nativeaot", plan?.FSharp2, fsharp2, fsharp2Cleanup),
            },
            observations = new
            {
                diagnostics = LaneStreams(oracle, fsharp2),
                artifacts = artifactEntries,
                probes = probeEntries,
                runtime = probeEntries.Where((_, index) => probes[index].Kind == "runtime"),
                consumer = probeEntries.Where((_, index) =>
                    probes[index].Kind is "downstream-fsharp" or "downstream-csharp"),
                il = probeEntries.Where((_, index) => probes[index].Kind == "ilverify"),
                metadata = probeEntries.Where((_, index) =>
                    probes[index].Kind is "managed-load" or "public-api" or "metadata"),
                pdb = probeEntries.Where((_, index) => probes[index].Kind == "portable-pdb"),
                determinism = comparisons
                    .Where(static comparison => comparison.ComparatorId == "artifact-repeat")
                    .Select(static comparison => CreateHashEntry(comparison.ComparatorId, CanonicalJson.Canonicalize(
                        JsonSerializer.SerializeToElement(comparison, Json)))),
            },
            comparisons = comparisonEntries,
            verdict = new
            {
                state = VerdictName(verdict.Verdict),
                verdict.Reasons,
                verdict.MissingEvidence,
                bundleHash = BundleRunResultSeal.Placeholder,
            },
        }, Json);
        result = AddDiagnosticCompatibility(result, materialized.ResolvedDocument);

        var validation = new SchemaStore(repository.Root).Validate(
            "run-result.schema.json",
            "run-result.json",
            result);
        if (!validation.IsValid)
        {
            throw new ConformanceContractException(validation.Issues);
        }
        return result;
    }

    private static JsonElement AddDiagnosticCompatibility(
        JsonElement runResult,
        JsonElement resolvedCase)
    {
        if (!resolvedCase.TryGetProperty("diagnosticCompatibility", out var compatibility))
        {
            return runResult;
        }

        using var stream = new MemoryStream();
        using (var writer = new Utf8JsonWriter(stream))
        {
            writer.WriteStartObject();
            foreach (var property in runResult.EnumerateObject())
            {
                property.WriteTo(writer);
            }
            writer.WritePropertyName("diagnosticCompatibility");
            compatibility.WriteTo(writer);
            writer.WriteEndObject();
        }
        using var document = JsonDocument.Parse(stream.ToArray());
        return document.RootElement.Clone();
    }

    private static object Lane(
        string compilerSelection,
        LaneInvocation? invocation,
        CoreCompileLaneResult? result,
        CleanupReceipt? cleanup)
    {
        if (invocation is null || result is null)
        {
            var notRunHash = HashValue(new { state = "not-run", compilerSelection });
            return new
            {
                compilerSelection,
                rootHash = notRunHash,
                evaluationHash = notRunHash,
                invocationHash = notRunHash,
                processHash = notRunHash,
                cleanupHash = notRunHash,
            };
        }
        ArgumentNullException.ThrowIfNull(cleanup);
        return new
        {
            compilerSelection,
            rootHash = HashValue(Path.GetDirectoryName(invocation.ProjectPath)!),
            evaluationHash = HashValue(invocation.Properties),
            invocationHash = HashValue(new { invocation.Arguments, invocation.Environment }),
            processHash = HashValue(result.Process),
            cleanupHash = HashValue(cleanup),
        };
    }

    private static HashEntry[] LaneStreams(
        CoreCompileLaneResult? oracle,
        CoreCompileLaneResult? fsharp2)
    {
        var entries = new List<HashEntry>();
        AddStreams(entries, "oracle", oracle);
        AddStreams(entries, "fsharp2", fsharp2);
        return [.. entries];
    }

    private static void AddStreams(
        ICollection<HashEntry> entries,
        string name,
        CoreCompileLaneResult? result)
    {
        if (result is null)
        {
            return;
        }
        entries.Add(CreateHashEntry($"{name}:stdout", result.Process.StandardOutput.AsSpan()));
        entries.Add(CreateHashEntry($"{name}:stderr", result.Process.StandardError.AsSpan()));
    }

    private static string FormatDiagnostic(CompilationDiagnostic diagnostic) =>
        DiagnosticProjection.Serialize(diagnostic);

    private static string PhaseStatus(string value) => value switch
    {
        "completed" => "completed",
        "failed" => "failed",
        "unsupported" => "unsupported",
        _ => "not-run",
    };

    private static string VerdictName(ConformanceVerdict verdict) => verdict switch
    {
        ConformanceVerdict.Pass => "pass",
        ConformanceVerdict.Fail => "fail",
        ConformanceVerdict.Unsupported => "unsupported",
        ConformanceVerdict.InfraError => "infra-error",
        _ => throw new ArgumentOutOfRangeException(nameof(verdict)),
    };

    private static string OperatingSystemName() =>
        OperatingSystem.IsWindows() ? "windows"
        : OperatingSystem.IsLinux() ? "linux"
        : OperatingSystem.IsMacOS() ? "macos"
        : "unknown";

    private static HashEntry CreateHashEntry(string id, ReadOnlySpan<byte> bytes) =>
        new(id, Hashing.Sha256(bytes));

    private static string HashValue<T>(T value) =>
        Hashing.Sha256(CanonicalJson.Canonicalize(JsonSerializer.SerializeToElement(value, Json)));

    private sealed record HashEntry(string Id, string Sha256);
}
