using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.DiagnosticInventory;

public static partial class DiagnosticInventoryGenerator
{
    private const string PinnedCommit = "b611d184a141d1ad1994ff59c01fecc10f787a89";
    private const string FsCompPath = "src/Compiler/FSComp.txt";
    private const string CompilerDiagnosticsPath = "src/Compiler/Driver/CompilerDiagnostics.fs";
    private static readonly string[] SatelliteCultures =
    [
        "cs",
        "de",
        "es",
        "fr",
        "it",
        "ja",
        "ko",
        "pl",
        "pt-BR",
        "ru",
        "tr",
        "zh-Hans",
        "zh-Hant",
    ];
    private static readonly JsonSerializerOptions Json = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        WriteIndented = true,
    };

    public static GenerationResult Generate(GenerationRequest request)
    {
        ArgumentNullException.ThrowIfNull(request);
        if (!string.Equals(request.SourceCommit, PinnedCommit, StringComparison.Ordinal))
        {
            throw new ArgumentException($"The source commit must be '{PinnedCommit}'.");
        }

        var sourceRepository = Path.GetFullPath(request.SourceRepository);
        var conformanceRoot = Path.GetFullPath(request.ConformanceRoot);
        RequireDirectory(sourceRepository, "source repository");
        RequireDirectory(conformanceRoot, "conformance root");
        var git = new GitReader(sourceRepository, request.SourceCommit);
        git.RequireCommit();

        var entries = ImmutableArray.CreateBuilder<SourceEntry>();
        var domains = ImmutableArray.CreateBuilder<OpenDomain>();
        CaptureResources(git, entries);
        CaptureCompilerMappings(git, entries);
        CaptureRuntimeNumberConstructors(git, entries, domains);
        CaptureUpstreamTests(git, entries);
        CaptureDynamicObservations(conformanceRoot, entries);

        var orderedEntries = entries
            .DistinctBy(static item =>
                (item.FamilyId, item.SourceKind, item.EvidenceKind, item.Source, item.ProductionKey),
                EqualityComparer<(string, string, string, string, string)>.Default)
            .OrderBy(static item => item.FamilyId, StringComparer.Ordinal)
            .ThenBy(static item => item.SourceKind, StringComparer.Ordinal)
            .ThenBy(static item => item.EvidenceKind, StringComparer.Ordinal)
            .ThenBy(static item => item.Source, StringComparer.Ordinal)
            .ThenBy(static item => item.ProductionKey, StringComparer.Ordinal)
            .ToImmutableArray();
        var orderedDomains = domains
            .DistinctBy(static item => (item.Constructor, item.Source))
            .OrderBy(static item => item.Constructor, StringComparer.Ordinal)
            .ThenBy(static item => item.Source, StringComparer.Ordinal)
            .ToImmutableArray();

        var capture = new CandidateCapture(
            1,
            request.SourceCommit,
            orderedEntries,
            orderedDomains,
            new
            {
                owner = "compiler-compatibility",
                sourceIssue = "#27",
                reviewCondition = "Refresh when the pinned source commit or a discovery rule changes.",
            });
        WriteJson(request.CaptureOutput, capture);

        var families = BuildFamilies(orderedEntries, orderedDomains);
        var caseIds = orderedEntries
            .Select(static item => item.CaseId)
            .Where(static item => item is not null)
            .Select(static item => item!)
            .Distinct(StringComparer.Ordinal)
            .Order(StringComparer.Ordinal)
            .ToArray();
        if (caseIds.Length == 0)
        {
            throw new InvalidDataException("At least one dynamic Oracle case is required.");
        }

        var inventory = new
        {
            documentKind = "family-inventory-v1",
            schemaVersion = 1,
            inventoryId = "diagnostics.fs-families.inventory",
            family = "diagnostics",
            caseIds,
            positiveCaseIds = Array.Empty<string>(),
            negativeCaseIds = caseIds,
            featureRows = Array.Empty<string>(),
            languageVersions = new[] { "default" },
            optionCells = new[] { "noframework-simpleresolution-fullpaths-flaterrors-consolecolors-minus" },
            artifactKinds = Array.Empty<string>(),
            probeKinds = new[] { "within-compiler-repeat" },
            review = new
            {
                owner = "compiler-compatibility",
                sourceIssue = "#27",
                reviewCondition = "Refresh when the pinned Oracle, source commit, discovery inputs, or working disposition changes.",
            },
            diagnosticFamilies = families.Select(static item => item.Family).ToArray(),
        };
        WriteJson(request.InventoryOutput, inventory);

        WriteJson(request.ClosureOutput, BuildClosure(conformanceRoot, families));
        WriteOracleLock(conformanceRoot, request.OracleLockOutput);

        return new(
            families.Length,
            families.Sum(static item => item.Variants.Length),
            orderedEntries.Length);
    }
}
