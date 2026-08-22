using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class CoverageValidator
{
    public static ValidationResult Validate(ConformanceRepository repository)
    {
        ArgumentNullException.ThrowIfNull(repository);
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        var languageCases = repository.Cases
            .Where(static item => GetString(item.Document, "kind") == "language")
            .ToArray();
        var integrationCases = repository.Cases
            .Where(static item => GetString(item.Document, "kind") == "integration")
            .ToArray();

        var featureRows = repository.Manifest.GetProperty("featureRows").EnumerateArray().ToArray();
        foreach (var featureRow in featureRows)
        {
            var id = GetString(featureRow, "id")!;
            if (id.Contains("icedtasks", StringComparison.OrdinalIgnoreCase)
                || (GetString(featureRow, "title") ?? string.Empty)
                    .Contains("icedtasks", StringComparison.OrdinalIgnoreCase))
            {
                issues.Add(new("feature-corpus-name", id, "Primary feature rows must be corpus-neutral."));
            }

            var matching = languageCases
                .Where(item => GetString(item.Document, "featureRow") == id)
                .ToArray();
            foreach (var polarity in featureRow.GetProperty("requiredPolarities").EnumerateArray())
            {
                var expected = polarity.GetString();
                if (!matching.Any(item => GetString(item.Document, "polarity") == expected))
                {
                    issues.Add(new("coverage-polarity", id, $"Feature row '{id}' lacks '{expected}' coverage."));
                }
            }

            foreach (var phase in featureRow.GetProperty("requiredPhases").EnumerateArray())
            {
                var expected = phase.GetString();
                var observed = matching.Any(item => HasPhaseProof(item.Document, expected!));
                if (!observed)
                {
                    issues.Add(new("coverage-phase", id, $"Feature row '{id}' lacks evidence for '{expected}'."));
                }
            }
        }

        var corpusIds = repository.Manifest
            .GetProperty("integrationCorpora")
            .EnumerateArray()
            .Select(static item => GetString(item, "id"))
            .Where(static item => item is not null)
            .ToHashSet(StringComparer.Ordinal);
        foreach (var item in integrationCases)
        {
            if (item.Document.TryGetProperty("featureRow", out _))
            {
                issues.Add(new("integration-feature-row", item.FilePath, "An integration case cannot declare featureRow."));
            }
            var corpus = GetString(item.Document, "integrationCorpus");
            if (corpus is null || !corpusIds.Contains(corpus))
            {
                issues.Add(new("integration-corpus", item.FilePath, "The integration corpus is not declared."));
            }
        }

        foreach (var item in languageCases)
        {
            if (item.Document.TryGetProperty("integrationCorpus", out _))
            {
                issues.Add(new("language-integration-corpus", item.FilePath, "A language case cannot declare integrationCorpus."));
            }
        }

        var coverage = repository.Manifest.GetProperty("coverage");
        RequireCoverage(
            coverage,
            "languageVersions",
            "coverage-language-version",
            "language version",
            "case",
            repository.Cases.SelectMany(static item => Strings(item.Document, "envelope", "languageVersions")),
            issues);
        RequireCoverage(
            coverage,
            "optionCells",
            "coverage-option",
            "option cell",
            "inventory",
            repository.Inventories.SelectMany(static inventory => Strings(inventory, "optionCells")),
            issues);
        RequireCoverage(
            coverage,
            "targetFrameworks",
            "coverage-target-framework",
            "target framework",
            "case",
            repository.Cases.SelectMany(static item => Strings(item.Document, "envelope", "targetFrameworks")),
            issues);
        RequireCoverage(
            coverage,
            "configurations",
            "coverage-configuration",
            "configuration",
            "case",
            repository.Cases.SelectMany(static item => Strings(item.Document, "matrix", "configurations", "required")),
            issues);
        RequireCoverage(
            coverage,
            "artifactKinds",
            "coverage-artifact",
            "artifact kind",
            "case",
            repository.Cases.SelectMany(static item => ObjectStrings(item.Document, "expectedArtifacts", "kind")),
            issues);
        RequireCoverage(
            coverage,
            "probeKinds",
            "coverage-probe",
            "probe kind",
            "case",
            repository.Cases.SelectMany(static item => RequiredProbeKinds(item.Document)),
            issues);

        return new(issues.Count == 0, issues.ToImmutable());
    }

    private static void RequireCoverage(
        JsonElement coverage,
        string propertyName,
        string code,
        string description,
        string source,
        IEnumerable<string> observedValues,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        var observed = observedValues.ToHashSet(StringComparer.Ordinal);
        foreach (var required in coverage.GetProperty(propertyName)
                     .EnumerateArray()
                     .Where(static item => GetString(item, "status") == "required")
                     .Select(static item => GetString(item, "id")!)
                     .Where(required => !observed.Contains(required))
                     .Order(StringComparer.Ordinal))
        {
            issues.Add(new(
                code,
                required,
                $"Required {description} '{required}' has no {source} coverage."));
        }
    }

    private static IEnumerable<string> Strings(JsonElement document, params string[] propertyPath)
    {
        var value = document;
        foreach (var propertyName in propertyPath)
        {
            if (value.ValueKind != JsonValueKind.Object
                || !value.TryGetProperty(propertyName, out value))
            {
                yield break;
            }
        }
        if (value.ValueKind != JsonValueKind.Array)
        {
            yield break;
        }
        foreach (var item in value.EnumerateArray())
        {
            if (item.ValueKind == JsonValueKind.String)
            {
                yield return item.GetString()!;
            }
        }
    }

    private static IEnumerable<string> ObjectStrings(
        JsonElement document,
        string arrayProperty,
        string valueProperty)
    {
        if (!document.TryGetProperty(arrayProperty, out var values)
            || values.ValueKind != JsonValueKind.Array)
        {
            yield break;
        }
        foreach (var item in values.EnumerateArray())
        {
            var value = GetString(item, valueProperty);
            if (value is not null)
            {
                yield return value;
            }
        }
    }

    private static IEnumerable<string> RequiredProbeKinds(JsonElement document)
    {
        if (!document.TryGetProperty("probes", out var probes)
            || probes.ValueKind != JsonValueKind.Array)
        {
            yield break;
        }
        foreach (var probe in probes.EnumerateArray())
        {
            if (probe.TryGetProperty("required", out var required)
                && required.ValueKind == JsonValueKind.True)
            {
                var kind = GetString(probe, "kind");
                if (kind is not null)
                {
                    yield return kind;
                }
            }
        }
    }

    private static bool HasPhaseProof(JsonElement document, string phase)
    {
        if (!document.TryGetProperty("phaseExpectations", out var expectations)
            || !expectations.TryGetProperty("phases", out var phases))
        {
            return false;
        }

        return phases.EnumerateArray().Any(item =>
            GetString(item, "phase") == phase
            && item.TryGetProperty("observableProofs", out var proofs)
            && proofs.ValueKind == JsonValueKind.Array
            && proofs.GetArrayLength() > 0);
    }

    private static string? GetString(JsonElement value, string propertyName) =>
        value.TryGetProperty(propertyName, out var property) && property.ValueKind == JsonValueKind.String
            ? property.GetString()
            : null;
}
