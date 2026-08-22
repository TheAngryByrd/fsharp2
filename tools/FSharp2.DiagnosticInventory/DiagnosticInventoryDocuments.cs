using System.Collections.Immutable;
using System.Globalization;

namespace FSharp2.DiagnosticInventory;

public static partial class DiagnosticInventoryGenerator
{
    private static ImmutableArray<FamilyOutput> BuildFamilies(
        ImmutableArray<SourceEntry> entries,
        ImmutableArray<OpenDomain> domains) =>
        entries
            .GroupBy(static item => item.FamilyId, StringComparer.Ordinal)
            .OrderBy(static group => group.Key, StringComparer.Ordinal)
            .Select(group => BuildFamily(group, domains))
            .ToImmutableArray();

    private static FamilyOutput BuildFamily(
        IGrouping<string, SourceEntry> group,
        ImmutableArray<OpenDomain> domains)
    {
        var entries = group.ToImmutableArray();
        var reserved = entries.Any(static item => item.Reserved)
            && entries.All(static item => item.Reserved || item.SourceKind == "upstream-baseline");
        var disposition = reserved ? "reserved" : "untriggered";
        var blockingIssue = reserved ? null : "#27";
        var evidenceHashes = entries
            .Select(static item => item.Sha256)
            .Distinct(StringComparer.Ordinal)
            .Order(StringComparer.Ordinal)
            .ToArray();
        var variants = BuildVariants(group.Key, entries, disposition, blockingIssue);
        var family = new
        {
            familyId = group.Key,
            numericCode = int.Parse(group.Key.AsSpan(2), CultureInfo.InvariantCulture),
            candidateStatus = "candidate",
            sourceMappingEvidence = Evidence(entries, "source-mapping"),
            neutralResourceEvidence = Evidence(entries, "neutral-resource"),
            satelliteResourceEvidence = Evidence(entries, "satellite-resource"),
            upstreamBaselineEvidence = Evidence(entries, "upstream-baseline"),
            dynamicNumberSource = BuildDynamicNumberSource(entries, domains),
            variantIds = variants.Select(static item => item.VariantId).ToArray(),
            reachabilityReview = new
            {
                disposition,
                evidenceHashes,
            },
            blockingFeatureIssue = blockingIssue,
            variants = variants.Select(static item => item.Value).ToArray(),
        };
        return new(group.Key, disposition, blockingIssue, evidenceHashes, variants, family);
    }

    private static object BuildDynamicNumberSource(
        ImmutableArray<SourceEntry> entries,
        ImmutableArray<OpenDomain> domains)
    {
        var constructors = entries
            .Select(static item => item.DynamicConstructor)
            .Where(static item => item is not null)
            .Select(static item => item!)
            .Distinct(StringComparer.Ordinal)
            .Order(StringComparer.Ordinal)
            .ToArray();
        if (constructors.Length == 0)
        {
            return new { boundary = "not-applicable" };
        }
        if (constructors.Length != 1)
        {
            throw new InvalidDataException(
                $"A diagnostic family uses more than one runtime-number constructor: {string.Join(", ", constructors)}.");
        }

        var domain = domains
            .Where(item => string.Equals(item.Constructor, constructors[0], StringComparison.Ordinal))
            .OrderBy(static item => item.Source, StringComparer.Ordinal)
            .FirstOrDefault()
            ?? throw new InvalidDataException(
                $"Runtime-number constructor '{constructors[0]}' has no open-domain evidence.");
        return new
        {
            source = domain.Source,
            sha256 = domain.Sha256,
            boundary = domain.Boundary,
            boundaryDescription = domain.BoundaryDescription,
            blockingIssue = domain.BlockingIssue,
        };
    }

    private static object[] Evidence(ImmutableArray<SourceEntry> entries, string evidenceKind) =>
        entries
            .Where(item => string.Equals(item.EvidenceKind, evidenceKind, StringComparison.Ordinal))
            .Select(static item => new { source = item.Source, sha256 = item.Sha256 })
            .Distinct()
            .OrderBy(static item => item.source, StringComparer.Ordinal)
            .ThenBy(static item => item.sha256, StringComparer.Ordinal)
            .Cast<object>()
            .ToArray();

    private static ImmutableArray<VariantOutput> BuildVariants(
        string familyId,
        ImmutableArray<SourceEntry> entries,
        string disposition,
        string? blockingIssue)
    {
        var candidates = entries
            .Where(static item => item.EvidenceKind is "neutral-resource" or "source-mapping")
            .GroupBy(static item => item.ProductionKey, StringComparer.Ordinal)
            .Select(static group => group.First())
            .ToList();
        if (candidates.Count == 0)
        {
            candidates.Add(entries[0]);
        }

        var usedIds = new HashSet<string>(StringComparer.Ordinal);
        var variants = ImmutableArray.CreateBuilder<VariantOutput>();
        foreach (var entry in candidates.OrderBy(static item => item.ProductionKey, StringComparer.Ordinal))
        {
            var variantId = $"{familyId}.{Slug(entry.MessageIdentity)}";
            if (!usedIds.Add(variantId))
            {
                variantId += $"-{ShortHash(entry.ProductionKey)}";
                usedIds.Add(variantId);
            }
            var cultures = new List<object>
            {
                new { culture = "en-US", available = true, fallbackCulture = (string?)null },
            };
            foreach (var culture in SatelliteCultures)
            {
                var available = entries.Any(item =>
                    item.EvidenceKind == "satellite-resource"
                    && item.ProductionKey == entry.ProductionKey
                    && item.Culture == culture);
                cultures.Add(new
                {
                    culture,
                    available,
                    fallbackCulture = available ? null : "en-US",
                });
            }
            var sourceHashes = entries
                .Where(item => item.ProductionKey == entry.ProductionKey)
                .Select(static item => item.Sha256)
                .Distinct(StringComparer.Ordinal)
                .Order(StringComparer.Ordinal)
                .ToArray();
            var value = new
            {
                variantId,
                productionKey = entry.ProductionKey,
                messageIdentity = entry.MessageIdentity,
                placeholders = Placeholders(entry.Message),
                originalSeverities = new[] { "information", "warning", "error" },
                effectiveSeverityCases = Array.Empty<object>(),
                stages = new object[] { new { kind = "host" } },
                subcategories = Array.Empty<string>(),
                languageVersions = new[] { "default" },
                warningLevel = (int?)null,
                offByDefault = false,
                optionSensitiveBehavior = Array.Empty<string>(),
                cultures,
                triggerCaseIds = Array.Empty<string>(),
                recoveryShape = new { recoveryGroup = "none", parentOccurrence = "none" },
                relatedInformationShape = "none",
                oracleEvidenceHashes = Array.Empty<string>(),
            };
            variants.Add(new(variantId, disposition, blockingIssue, sourceHashes, value));
        }
        return variants.ToImmutable();
    }
}
