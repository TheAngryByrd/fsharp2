using System.Collections.Immutable;

namespace FSharp2.Conformance;

public enum LaneKind
{
    Oracle,
    FSharp2,
}

public enum ConformanceVerdict
{
    Pass,
    Fail,
    Unsupported,
    InfraError,
}

public sealed record LaneRoot(
    LaneKind Kind,
    string Root,
    string Work,
    string Temp,
    string Intermediate,
    string Output,
    string Service,
    string Cache,
    string ResponseFile,
    string Binlog);

public sealed record LaneRootSet(
    string RunId,
    string RunRoot,
    LaneRoot Oracle,
    LaneRoot FSharp2);

public sealed record MaterializedInputHash(string LogicalPath, string Sha256);

public sealed record LaneInvocation(
    LaneKind Kind,
    string ProjectPath,
    ImmutableArray<byte> ProjectBytes,
    ImmutableArray<MaterializedInputHash> Inputs,
    ImmutableDictionary<string, string> Properties,
    ImmutableDictionary<string, string> Environment,
    ImmutableArray<string> Arguments,
    string? CompilerHostPath);

public sealed record CoreCompilePlan(
    LaneInvocation Oracle,
    LaneInvocation FSharp2)
{
    public ImmutableArray<MaterializedInputHash> RestoredDependencyInputs { get; init; } = [];
}

public sealed record RestoredDependencyEvidence(
    ImmutableArray<MaterializedInputHash> Inputs,
    ComparisonResult Comparison);

public sealed record ProcessSpec(
    string FileName,
    string WorkingDirectory,
    ImmutableArray<string> Arguments,
    ImmutableDictionary<string, string> Environment,
    TimeSpan Timeout,
    string StandardOutputPath,
    string StandardErrorPath);

public sealed record ProcessObservation(
    int ProcessId,
    int? ParentProcessId,
    string ExecutablePath,
    ImmutableArray<string> Arguments)
{
    public DateTimeOffset? StartedAt { get; init; }
}

public sealed record ProcessResult(
    int ExitCode,
    bool TimedOut,
    DateTimeOffset StartedAt,
    DateTimeOffset EndedAt,
    ImmutableArray<byte> StandardOutput,
    ImmutableArray<byte> StandardError,
    ImmutableArray<ProcessObservation> Processes);

public sealed record BinlogEvidence(
    int CoreCompileInvocationCount,
    bool CoreCompileSkipped,
    ImmutableArray<string> CompilerCommandLines);

public sealed record CoreCompileLaneResult(
    LaneKind Kind,
    ProcessResult Process,
    BinlogEvidence Binlog);
