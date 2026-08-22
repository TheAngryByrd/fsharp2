using System.Collections.Immutable;
using System.Text.Json.Serialization;

namespace FSharp2.DiagnosticInventory;

public sealed record GenerationRequest(
    string SourceRepository,
    string SourceCommit,
    string ConformanceRoot,
    string CaptureOutput,
    string InventoryOutput,
    string ClosureOutput,
    string OracleLockOutput);

public sealed record GenerationResult(int CandidateCount, int VariantCount, int SourceEntryCount);

internal sealed record SourceEntry(
    string FamilyId,
    string SourceKind,
    string EvidenceKind,
    string Source,
    string Sha256,
    string ProductionKey,
    string MessageIdentity,
    string? Message,
    string? Culture,
    string? CaseId,
    bool Reserved,
    [property: JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        string? DynamicConstructor = null);

internal sealed record OpenDomain(
    string Constructor,
    string Source,
    string Sha256,
    string Boundary,
    string BoundaryDescription,
    string BlockingIssue);

internal sealed record CandidateCapture(
    int SchemaVersion,
    string SourceCommit,
    ImmutableArray<SourceEntry> Entries,
    ImmutableArray<OpenDomain> OpenDomains,
    object Review);

internal sealed record GrepMatch(string Path, int Line, string Text);

internal sealed record VariantOutput(
    string VariantId,
    string Disposition,
    string? BlockingIssue,
    string[] EvidenceHashes,
    object Value);

internal sealed record FamilyOutput(
    string FamilyId,
    string Disposition,
    string? BlockingIssue,
    string[] EvidenceHashes,
    ImmutableArray<VariantOutput> Variants,
    object Family);
