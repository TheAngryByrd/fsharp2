using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public sealed record ValidationIssue(string Code, string Path, string Message);

public sealed record ValidationResult(bool IsValid, ImmutableArray<ValidationIssue> Issues)
{
    public static ValidationResult Valid { get; } = new(true, []);

    public static ValidationResult Invalid(params ValidationIssue[] issues) =>
        new(false, [.. issues]);
}

public sealed record ConformanceCase(
    string CaseId,
    string FilePath,
    ImmutableArray<string> Tags,
    JsonElement Document)
{
    public ConformanceCase Snapshot() => this with { Document = Document.Clone() };
}

public sealed record ConformanceRepository(
    string Root,
    JsonElement Manifest,
    JsonElement ComparisonPolicy,
    JsonElement Toolchain,
    ImmutableArray<ConformanceCase> Cases)
{
    public ImmutableArray<JsonElement> Inventories { get; init; } = [];

    public ConformanceRepository Snapshot() =>
        this with
        {
            Manifest = Manifest.Clone(),
            ComparisonPolicy = ComparisonPolicy.Clone(),
            Toolchain = Toolchain.Clone(),
            Cases = [.. Cases.Select(static item => item.Snapshot())],
            Inventories = [.. Inventories.Select(static item => item.Clone())],
        };
}

public sealed record SdkSelectionResult(
    string Root,
    string DotnetPath,
    string Version,
    ImmutableDictionary<string, string> Environment);

public sealed record MaterializedSource(
    string StableId,
    string Kind,
    string LogicalPath,
    string Sha256,
    ImmutableArray<byte> Bytes);

public sealed record MaterializedTargetReference(
    string StableId,
    string LogicalPath,
    string Sha256,
    string AssemblyName,
    string Version,
    string Culture,
    string PublicKeyToken,
    string EvaluatedItemSource,
    ImmutableArray<byte> Bytes);

public sealed record NormalizedOptionGroups(
    JsonElement Semantic,
    JsonElement Diagnostic,
    JsonElement Emission,
    JsonElement Signing,
    JsonElement Resources,
    ImmutableArray<string> CompilerArguments);

public sealed record MaterializedCase(
    string CaseId,
    ImmutableArray<MaterializedSource> Sources,
    ImmutableArray<MaterializedTargetReference> TargetReferences,
    NormalizedOptionGroups Options,
    ImmutableArray<string> RequestedArtifacts,
    JsonElement ResolvedDocument);

public sealed record CorePhaseEvidence(
    string Phase,
    string Status,
    string? InputFingerprint,
    string? OutputFingerprint,
    ImmutableArray<string> TraceValues);

public sealed record CoreArtifactEvidence(
    string Kind,
    string StableId,
    string Fingerprint,
    ImmutableArray<byte> Bytes);

public sealed record CoreEvidence(
    int ContractVersion,
    string Outcome,
    ImmutableArray<CorePhaseEvidence> Phases,
    ImmutableArray<CoreArtifactEvidence> Artifacts,
    ImmutableArray<string> Traces);

public sealed class ConformanceContractException : Exception
{
    public ConformanceContractException(ImmutableArray<ValidationIssue> issues)
        : base(issues.IsDefaultOrEmpty ? "The conformance contract is invalid." : issues[0].Message)
    {
        Issues = issues.IsDefault ? [] : issues;
    }

    public ImmutableArray<ValidationIssue> Issues { get; }
}
