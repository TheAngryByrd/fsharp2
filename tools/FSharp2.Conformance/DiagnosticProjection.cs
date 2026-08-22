using System.Collections.Immutable;
using System.Text;
using System.Text.Json;
using FSharp2.Compiler;

namespace FSharp2.Conformance;

internal sealed record DiagnosticPositionProjection(
    int Offset,
    int Line,
    int Column);

internal sealed record DiagnosticRangeProjection(
    DiagnosticPositionProjection Start,
    DiagnosticPositionProjection End);

internal sealed record DiagnosticStageProjection(
    string Kind,
    string? Phase);

internal sealed record DiagnosticRelatedInformationProjection(
    string Message,
    string? LogicalPath,
    DiagnosticRangeProjection? Range);

internal sealed record CompilationDiagnosticProjection(
    long Occurrence,
    string Code,
    int NumericCode,
    string? Subcategory,
    DiagnosticStageProjection Stage,
    string OriginalSeverity,
    string EffectiveSeverity,
    string Disposition,
    string? Suppression,
    string Message,
    string? LogicalPath,
    DiagnosticRangeProjection? Range,
    ImmutableArray<DiagnosticRelatedInformationProjection> RelatedInformation,
    ImmutableArray<string> Suggestions,
    string? Stream);

internal static class DiagnosticProjection
{
    private static readonly JsonSerializerOptions Json = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
    };

    public static CompilationDiagnosticProjection Create(CompilationDiagnostic diagnostic)
    {
        ArgumentNullException.ThrowIfNull(diagnostic);
        return new CompilationDiagnosticProjection(
            diagnostic.Occurrence,
            diagnostic.Code,
            diagnostic.NumericCode,
            diagnostic.Subcategory is null ? null : diagnostic.Subcategory.Value,
            Stage(diagnostic.Stage),
            Severity(diagnostic.OriginalSeverity),
            Severity(diagnostic.EffectiveSeverity),
            Disposition(diagnostic.Disposition),
            diagnostic.Suppression is null ? null : Suppression(diagnostic.Suppression.Value),
            diagnostic.Message,
            diagnostic.LogicalPath is null ? null : diagnostic.LogicalPath.Value,
            diagnostic.Range is null ? null : Range(diagnostic.Range.Value),
            [.. diagnostic.RelatedInformation.Select(static information =>
                new DiagnosticRelatedInformationProjection(
                    information.Message,
                    information.LogicalPath is null ? null : information.LogicalPath.Value,
                    information.Range is null ? null : Range(information.Range.Value)))],
            [.. diagnostic.Suggestions],
            diagnostic.Stream is null ? null : Stream(diagnostic.Stream.Value));
    }

    public static string Serialize(CompilationDiagnostic diagnostic) =>
        Encoding.UTF8.GetString(CanonicalJson.Canonicalize(
            JsonSerializer.SerializeToElement(Create(diagnostic), Json)));

    private static DiagnosticStageProjection Stage(DiagnosticStage stage)
    {
        ArgumentNullException.ThrowIfNull(stage);
        if (stage.IsCommandLine)
        {
            return new("command-line", null);
        }
        if (stage.IsCompilation)
        {
            return new("compilation", Phase(((DiagnosticStage.Compilation)stage).Item));
        }
        if (stage.IsPublication)
        {
            return new("publication", null);
        }
        if (stage.IsHost)
        {
            return new("host", null);
        }
        throw new ArgumentOutOfRangeException(nameof(stage));
    }

    private static string Phase(CompilationPhase phase)
    {
        ArgumentNullException.ThrowIfNull(phase);
        return phase.IsSource ? "source"
            : phase.IsSyntax ? "syntax"
            : phase.IsResolvedSymbols ? "resolved-symbols"
            : phase.IsTypedDeclarations ? "typed-declarations"
            : phase.IsLoweredCode ? "lowered-code"
            : phase.IsOptimizedCode ? "optimized-code"
            : phase.IsSymbolicEmission ? "symbolic-emission"
            : phase.IsFinalLinking ? "final-linking"
            : throw new ArgumentOutOfRangeException(nameof(phase));
    }

    private static string Severity(DiagnosticSeverity severity)
    {
        ArgumentNullException.ThrowIfNull(severity);
        return severity.IsHidden ? "hidden"
            : severity.IsInformation ? "information"
            : severity.IsWarning ? "warning"
            : severity.IsError ? "error"
            : throw new ArgumentOutOfRangeException(nameof(severity));
    }

    private static string Disposition(DiagnosticDisposition disposition)
    {
        ArgumentNullException.ThrowIfNull(disposition);
        return disposition.IsEmitted ? "emitted"
            : disposition.IsSuppressed ? "suppressed"
            : throw new ArgumentOutOfRangeException(nameof(disposition));
    }

    private static string Suppression(DiagnosticSuppression suppression)
    {
        ArgumentNullException.ThrowIfNull(suppression);
        return suppression.IsWarningLevel ? "warning-level"
            : suppression.IsGlobalNowarn ? "global-nowarn"
            : suppression.IsLocalNowarn ? "local-nowarn"
            : suppression.IsOffByDefault ? "off-by-default"
            : suppression.IsLanguageFeature ? "language-feature"
            : suppression.IsMaximumErrors ? "maximum-errors"
            : suppression.IsAbortBoundary ? "abort-boundary"
            : throw new ArgumentOutOfRangeException(nameof(suppression));
    }

    private static string Stream(DiagnosticStream stream)
    {
        ArgumentNullException.ThrowIfNull(stream);
        return stream.IsStandardOutput ? "stdout"
            : stream.IsStandardError ? "stderr"
            : throw new ArgumentOutOfRangeException(nameof(stream));
    }

    private static DiagnosticRangeProjection Range(SourceRange range) =>
        new(Position(range.Start), Position(range.End));

    private static DiagnosticPositionProjection Position(SourcePosition position) =>
        new(position.Offset, position.Line, position.Column);
}
