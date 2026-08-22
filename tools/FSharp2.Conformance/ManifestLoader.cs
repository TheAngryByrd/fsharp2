using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class ManifestLoader
{
    public static ValidationResult Validate(string root)
    {
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        if (string.IsNullOrWhiteSpace(root))
        {
            issues.Add(new("root", string.Empty, "The conformance root must contain text."));
            return new(false, issues.ToImmutable());
        }

        var conformanceRoot = Path.GetFullPath(root);
        if (!Directory.Exists(conformanceRoot))
        {
            issues.Add(new("root-missing", conformanceRoot, "The conformance root does not exist."));
            return new(false, issues.ToImmutable());
        }

        var schemaStore = new SchemaStore(conformanceRoot);
        var documents = new Dictionary<string, JsonElement>(PathComparer);

        var manifestPath = Path.Combine(conformanceRoot, "manifest.json");
        var manifest = ReadAndValidate(schemaStore, "manifest.schema.json", manifestPath, issues, documents);

        foreach (var casePath in CaseDiscovery.Discover(conformanceRoot))
        {
            ReadAndValidate(schemaStore, "case.schema.json", casePath, issues, documents);
        }

        ValidateFiles(
            conformanceRoot,
            "locks",
            "*.oracle-lock.json",
            "oracle-lock.schema.json",
            schemaStore,
            issues,
            documents);
        ValidateFiles(
            conformanceRoot,
            "inventories",
            "*.inventory.json",
            "inventory.schema.json",
            schemaStore,
            issues,
            documents);
        ValidateFiles(
            conformanceRoot,
            "expectations",
            "*.closure.json",
            "closure.schema.json",
            schemaStore,
            issues,
            documents);

        if (manifest is { } manifestValue)
        {
            ValidateReferencedDocument(
                conformanceRoot,
                manifestValue,
                "comparisonPolicy",
                "comparison-policy.schema.json",
                schemaStore,
                issues,
                documents);
            ValidateComparisonPolicy(conformanceRoot, manifestValue, issues, documents);
            ValidateToolchain(conformanceRoot, manifestValue, issues, documents);
        }

        ValidateCases(conformanceRoot, documents, issues);
        ValidateOracleLocks(documents, issues);
        ValidateDiagnosticOccurrenceOrder(documents, issues);
        ValidateForbiddenFields(documents, issues);

        return new(issues.Count == 0, issues.ToImmutable());
    }

    private static void ValidateDiagnosticOccurrenceOrder(
        Dictionary<string, JsonElement> documents,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        foreach (var (path, document) in documents
                     .Where(static item =>
                         item.Key.EndsWith(".case.json", StringComparison.Ordinal)
                         || item.Key.EndsWith(".oracle-lock.json", StringComparison.Ordinal)))
        {
            if (!document.TryGetProperty("diagnosticCompatibility", out var compatibility)
                || compatibility.ValueKind != JsonValueKind.Object
                || !compatibility.TryGetProperty("occurrences", out var occurrences)
                || occurrences.ValueKind != JsonValueKind.Array)
            {
                continue;
            }

            long? previous = null;
            var index = 0;
            foreach (var occurrence in occurrences.EnumerateArray())
            {
                if (occurrence.ValueKind == JsonValueKind.Object
                    && occurrence.TryGetProperty("occurrence", out var value)
                    && value.TryGetInt64(out var current))
                {
                    if (previous is not null && current <= previous.Value)
                    {
                        issues.Add(new(
                            "diagnostic-occurrence-order",
                            $"{path}/diagnosticCompatibility/occurrences/{index}/occurrence",
                            "Diagnostic occurrence values must increase in array order."));
                    }
                    previous = current;
                }
                index++;
            }
        }
    }

    public static ConformanceRepository Load(string root)
    {
        var result = Validate(root);
        if (!result.IsValid)
        {
            throw new ConformanceContractException(result.Issues);
        }

        var conformanceRoot = Path.GetFullPath(root);
        var manifest = ReadJson(Path.Combine(conformanceRoot, "manifest.json"));
        var comparisonPolicy = ReadReference(conformanceRoot, manifest, "comparisonPolicy");
        var toolchain = ReadReference(conformanceRoot, manifest, "toolchain");
        var inventoriesRoot = Path.Combine(conformanceRoot, "inventories");
        var inventories = Directory.Exists(inventoriesRoot)
            ? Directory.EnumerateFiles(inventoriesRoot, "*.inventory.json", SearchOption.AllDirectories)
                .OrderBy(static path => path, StringComparer.Ordinal)
                .Select(ReadJson)
                .ToImmutableArray()
            : [];
        var cases =
            CaseDiscovery
                .Discover(conformanceRoot)
                .Select(path =>
                {
                    var document = ReadJson(path);
                    var tags =
                        document.TryGetProperty("tags", out var tagsValue)
                            ? tagsValue
                                .EnumerateArray()
                                .Select(static item => item.GetString() ?? string.Empty)
                                .ToImmutableArray()
                            : [];
                    return new ConformanceCase(
                        document.GetProperty("caseId").GetString()!,
                        path,
                        tags,
                        document);
                })
                .ToImmutableArray();

        return new ConformanceRepository(
            conformanceRoot,
            manifest,
            comparisonPolicy,
            toolchain,
            cases)
        {
            Inventories = inventories,
        }.Snapshot();
    }

    private static JsonElement? ReadAndValidate(
        SchemaStore schemaStore,
        string schemaName,
        string path,
        ImmutableArray<ValidationIssue>.Builder issues,
        Dictionary<string, JsonElement> documents)
    {
        if (!File.Exists(path))
        {
            issues.Add(new("document-missing", path, "The required contract document does not exist."));
            return null;
        }

        JsonElement document;
        try
        {
            document = ReadJson(path);
        }
        catch (Exception exception) when (exception is IOException or JsonException)
        {
            issues.Add(new("json", path, exception.Message));
            return null;
        }

        documents[path] = document;
        issues.AddRange(schemaStore.Validate(schemaName, path, document).Issues);
        return document;
    }

    private static void ValidateFiles(
        string conformanceRoot,
        string directory,
        string pattern,
        string schemaName,
        SchemaStore schemaStore,
        ImmutableArray<ValidationIssue>.Builder issues,
        Dictionary<string, JsonElement> documents)
    {
        var documentRoot = Path.Combine(conformanceRoot, directory);
        if (!Directory.Exists(documentRoot))
        {
            return;
        }

        foreach (var path in Directory.EnumerateFiles(documentRoot, pattern, SearchOption.AllDirectories)
                     .OrderBy(static item => item, StringComparer.Ordinal))
        {
            ReadAndValidate(schemaStore, schemaName, path, issues, documents);
        }
    }

    private static void ValidateReferencedDocument(
        string conformanceRoot,
        JsonElement manifest,
        string propertyName,
        string schemaName,
        SchemaStore schemaStore,
        ImmutableArray<ValidationIssue>.Builder issues,
        Dictionary<string, JsonElement> documents)
    {
        if (!TryGetReference(conformanceRoot, manifest, propertyName, issues, out var path, out var expectedHash))
        {
            return;
        }

        var document = ReadAndValidate(schemaStore, schemaName, path, issues, documents);
        if (document is { } value)
        {
            ValidateCanonicalHash(path, value, expectedHash, issues);
        }
    }

    private static void ValidateToolchain(
        string conformanceRoot,
        JsonElement manifest,
        ImmutableArray<ValidationIssue>.Builder issues,
        Dictionary<string, JsonElement> documents)
    {
        if (!TryGetReference(conformanceRoot, manifest, "toolchain", issues, out var path, out var expectedHash))
        {
            return;
        }

        if (!File.Exists(path))
        {
            issues.Add(new("document-missing", path, "The referenced toolchain document does not exist."));
            return;
        }

        JsonElement document;
        try
        {
            document = ReadJson(path);
        }
        catch (Exception exception) when (exception is IOException or JsonException)
        {
            issues.Add(new("json", path, exception.Message));
            return;
        }

        documents[path] = document;
        ValidateCanonicalHash(path, document, expectedHash, issues);
        RequireToolchainValue(document, path, "schemaVersion", JsonValueKind.Number, "1", issues);
        RequireToolchainValue(document, path, "toolchainId", JsonValueKind.String, "dotnet-sdk-10.0.110", issues);
        RequireToolchainValue(document, path, "sdkVersion", JsonValueKind.String, "10.0.110", issues);
        RequireToolchainValue(
            document,
            path,
            "sdkCommit",
            JsonValueKind.String,
            "f7d90799ce4ef09a0bb257852a57248d2a8fb8dd",
            issues);
        RequireToolchainValue(document, path, "selectionMode", JsonValueKind.String, "explicit-root", issues);
        RequireToolchainValue(document, path, "multilevelLookup", JsonValueKind.False, "false", issues);

        var allowed = new HashSet<string>(
            ["schemaVersion", "toolchainId", "sdkVersion", "sdkCommit", "selectionMode", "multilevelLookup"],
            StringComparer.Ordinal);
        foreach (var property in document.EnumerateObject())
        {
            if (!allowed.Contains(property.Name))
            {
                issues.Add(new("toolchain-unknown", $"{path}/{property.Name}", "The toolchain field is not allowed."));
            }
        }
    }

    private static void ValidateComparisonPolicy(
        string conformanceRoot,
        JsonElement manifest,
        ImmutableArray<ValidationIssue>.Builder issues,
        Dictionary<string, JsonElement> documents)
    {
        if (!TryGetReference(
                conformanceRoot,
                manifest,
                "comparisonPolicy",
                issues,
                out var path,
                out _)
            || !documents.TryGetValue(path, out var policy)
            || !policy.TryGetProperty("comparatorVersions", out var comparatorVersions)
            || comparatorVersions.ValueKind != JsonValueKind.Array)
        {
            return;
        }

        var contracts = ComparisonSupport.BuiltInComparatorContracts
            .ToDictionary(static contract => contract.Id, StringComparer.Ordinal);
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var index = 0;
        foreach (var comparator in comparatorVersions.EnumerateArray())
        {
            var comparatorPath = $"{path}/comparatorVersions/{index}";
            index++;
            if (comparator.ValueKind != JsonValueKind.Object
                || !comparator.TryGetProperty("id", out var idValue)
                || idValue.ValueKind != JsonValueKind.String)
            {
                continue;
            }

            var id = idValue.GetString()!;
            if (!contracts.TryGetValue(id, out var contract))
            {
                issues.Add(new(
                    "comparison-policy-comparator-id",
                    $"{comparatorPath}/id",
                    $"Comparator '{id}' is not a built-in comparator."));
                continue;
            }
            if (!seen.Add(id))
            {
                issues.Add(new(
                    "comparison-policy-comparator-duplicate",
                    $"{comparatorPath}/id",
                    $"Comparator '{id}' is declared more than once."));
            }
            if (comparator.TryGetProperty("version", out var versionValue)
                && versionValue.ValueKind == JsonValueKind.Number
                && versionValue.TryGetInt32(out var version)
                && version != contract.Version)
            {
                issues.Add(new(
                    "comparison-policy-comparator-version",
                    $"{comparatorPath}/version",
                    $"Comparator '{id}' version is '{version}', expected '{contract.Version}'."));
            }
            if (comparator.TryGetProperty("class", out var classValue)
                && classValue.ValueKind == JsonValueKind.String
                && !string.Equals(classValue.GetString(), contract.Class, StringComparison.Ordinal))
            {
                issues.Add(new(
                    "comparison-policy-comparator-class",
                    $"{comparatorPath}/class",
                    $"Comparator '{id}' class is '{classValue.GetString()}', expected '{contract.Class}'."));
            }
        }

        foreach (var missing in contracts.Keys.Except(seen, StringComparer.Ordinal).Order(StringComparer.Ordinal))
        {
            issues.Add(new(
                "comparison-policy-comparator-missing",
                $"{path}/comparatorVersions",
                $"Built-in comparator '{missing}' is not declared."));
        }
    }

    private static void ValidateCases(
        string conformanceRoot,
        Dictionary<string, JsonElement> documents,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        var cases = documents
            .Where(static item => item.Key.EndsWith(".case.json", StringComparison.Ordinal))
            .OrderBy(static item => item.Key, StringComparer.Ordinal)
            .ToArray();
        var duplicateCaseIds = cases
            .Where(static item => item.Value.TryGetProperty("caseId", out _))
            .GroupBy(static item => item.Value.GetProperty("caseId").GetString(), StringComparer.Ordinal)
            .Where(static group => group.Key is not null && group.Count() > 1);
        foreach (var duplicate in duplicateCaseIds)
        {
            issues.Add(new("case-duplicate", conformanceRoot, $"Duplicate case id '{duplicate.Key}'."));
        }

        foreach (var (casePath, document) in cases)
        {
            ValidateCaseSources(conformanceRoot, casePath, document, issues);
            ValidateCaseReferences(conformanceRoot, casePath, document, issues);
            ValidateCaseLock(conformanceRoot, casePath, document, issues);
        }
    }

    private static void ValidateCaseSources(
        string conformanceRoot,
        string casePath,
        JsonElement document,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (!document.TryGetProperty("sources", out var sources) || sources.ValueKind != JsonValueKind.Array)
        {
            return;
        }

        ValidateUnique(sources, casePath, "order", "source-order-duplicate", issues);
        ValidateUnique(sources, casePath, "stableId", "source-id-duplicate", issues);
        ValidateUnique(sources, casePath, "logicalPath", "source-path-duplicate", issues);

        foreach (var source in sources.EnumerateArray())
        {
            if (!source.TryGetProperty("fixturePath", out var fixtureProperty)
                || !source.TryGetProperty("sha256", out var hashProperty))
            {
                continue;
            }

            if (source.TryGetProperty("generation", out _))
            {
                continue;
            }

            var fixturePath = ResolveRelative(conformanceRoot, fixtureProperty.GetString()!);
            if (!File.Exists(fixturePath))
            {
                issues.Add(new("source-missing", fixturePath, "The source fixture does not exist."));
                continue;
            }

            var actual = Hashing.Sha256File(fixturePath);
            var expected = hashProperty.GetString();
            if (!string.Equals(actual, expected, StringComparison.Ordinal))
            {
                issues.Add(
                    new(
                        "source-hash",
                        fixturePath,
                        $"The source hash is '{actual}', expected '{expected}'."));
            }
        }
    }

    private static void ValidateCaseReferences(
        string conformanceRoot,
        string casePath,
        JsonElement document,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (!document.TryGetProperty("targetReferences", out var references)
            || !references.TryGetProperty("closurePath", out var closureProperty))
        {
            return;
        }

        var closurePath = ResolveRelative(conformanceRoot, closureProperty.GetString()!);
        if (!File.Exists(closurePath))
        {
            issues.Add(new("closure-missing", casePath, $"The reference closure '{closurePath}' does not exist."));
        }
    }

    private static void ValidateCaseLock(
        string conformanceRoot,
        string casePath,
        JsonElement document,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (!document.TryGetProperty("expectedDiagnostics", out var diagnostics)
            || !diagnostics.TryGetProperty("mode", out var mode)
            || mode.GetString() != "oracle-lock"
            || !diagnostics.TryGetProperty("lockPath", out var lockProperty)
            || lockProperty.ValueKind != JsonValueKind.String)
        {
            return;
        }

        var lockPath = ResolveRelative(conformanceRoot, lockProperty.GetString()!);
        if (!File.Exists(lockPath))
        {
            issues.Add(new("lock-missing", casePath, $"The Oracle lock '{lockPath}' does not exist."));
            return;
        }

        var lockDocument = ReadJson(lockPath);
        if (lockDocument.TryGetProperty("caseHash", out var lockHash))
        {
            var actual = Hashing.Sha256(CanonicalJson.Canonicalize(document));
            if (!string.Equals(actual, lockHash.GetString(), StringComparison.Ordinal))
            {
                issues.Add(new("lock-case-hash", lockPath, $"The Oracle lock does not match '{casePath}'."));
            }
        }
    }

    private static void ValidateOracleLocks(
        Dictionary<string, JsonElement> documents,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        var cases = new Dictionary<string, JsonElement>(StringComparer.Ordinal);
        foreach (var (_, document) in documents
                     .Where(static item => item.Key.EndsWith(".case.json", StringComparison.Ordinal))
                     .OrderBy(static item => item.Key, StringComparer.Ordinal))
        {
            if (document.TryGetProperty("caseId", out var caseIdProperty)
                && caseIdProperty.ValueKind == JsonValueKind.String)
            {
                cases.TryAdd(caseIdProperty.GetString()!, document);
            }
        }

        foreach (var (path, document) in documents.Where(
                     static item => item.Key.EndsWith(".oracle-lock.json", StringComparison.Ordinal)))
        {
            if (!document.TryGetProperty("caseId", out var caseIdProperty)
                || !document.TryGetProperty("caseHash", out var caseHashProperty))
            {
                continue;
            }

            var caseId = caseIdProperty.GetString()!;
            if (!cases.TryGetValue(caseId, out var conformanceCase))
            {
                issues.Add(new("lock-case-missing", path, $"The lock case '{caseId}' does not exist."));
                continue;
            }

            var actual = Hashing.Sha256(CanonicalJson.Canonicalize(conformanceCase));
            if (!string.Equals(actual, caseHashProperty.GetString(), StringComparison.Ordinal))
            {
                issues.Add(new("lock-case-hash", path, $"The lock case hash is stale for '{caseId}'."));
            }
        }
    }

    private static void ValidateForbiddenFields(
        Dictionary<string, JsonElement> documents,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        foreach (var (path, document) in documents)
        {
            VisitForbiddenFields(document, path, issues);
        }
    }

    private static void VisitForbiddenFields(
        JsonElement value,
        string path,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (value.ValueKind == JsonValueKind.Object)
        {
            foreach (var property in value.EnumerateObject())
            {
                if (property.Name.Contains("performance", StringComparison.OrdinalIgnoreCase)
                    || property.Name.Contains("statistic", StringComparison.OrdinalIgnoreCase))
                {
                    issues.Add(
                        new(
                            "performance-field",
                            $"{path}/{property.Name}",
                            "Performance schedules and statistics belong to issue #24."));
                }
                VisitForbiddenFields(property.Value, $"{path}/{property.Name}", issues);
            }
        }
        else if (value.ValueKind == JsonValueKind.Array)
        {
            var index = 0;
            foreach (var item in value.EnumerateArray())
            {
                VisitForbiddenFields(item, $"{path}/{index}", issues);
                index++;
            }
        }
    }

    private static void ValidateUnique(
        JsonElement array,
        string path,
        string propertyName,
        string code,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        var values = array
            .EnumerateArray()
            .Where(item => item.TryGetProperty(propertyName, out _))
            .Select(item => item.GetProperty(propertyName).GetRawText())
            .ToArray();
        if (values.Distinct(StringComparer.Ordinal).Count() != values.Length)
        {
            issues.Add(new(code, path, $"Source field '{propertyName}' contains a duplicate."));
        }
    }

    private static bool TryGetReference(
        string root,
        JsonElement manifest,
        string propertyName,
        ImmutableArray<ValidationIssue>.Builder issues,
        out string path,
        out string expectedHash)
    {
        path = string.Empty;
        expectedHash = string.Empty;
        if (!manifest.TryGetProperty(propertyName, out var reference)
            || !reference.TryGetProperty("path", out var pathProperty)
            || !reference.TryGetProperty("sha256", out var hashProperty))
        {
            return false;
        }

        try
        {
            path = ResolveRelative(root, pathProperty.GetString()!);
        }
        catch (IOException exception)
        {
            issues.Add(new("path", root, exception.Message));
            return false;
        }
        expectedHash = hashProperty.GetString()!;
        return true;
    }

    private static void ValidateCanonicalHash(
        string path,
        JsonElement document,
        string expectedHash,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        var actual = Hashing.Sha256(CanonicalJson.Canonicalize(document));
        if (!string.Equals(actual, expectedHash, StringComparison.Ordinal))
        {
            issues.Add(new("document-hash", path, $"The canonical hash is '{actual}', expected '{expectedHash}'."));
        }
    }

    private static void RequireToolchainValue(
        JsonElement document,
        string path,
        string propertyName,
        JsonValueKind kind,
        string expected,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (!document.TryGetProperty(propertyName, out var value)
            || value.ValueKind != kind
            || value.GetRawText().Trim('"') != expected)
        {
            issues.Add(new("toolchain", $"{path}/{propertyName}", $"Expected '{expected}'."));
        }
    }

    private static JsonElement ReadReference(string root, JsonElement manifest, string propertyName)
    {
        var relative = manifest.GetProperty(propertyName).GetProperty("path").GetString()!;
        return ReadJson(ResolveRelative(root, relative));
    }

    private static JsonElement ReadJson(string path)
    {
        using var document = JsonDocument.Parse(
            File.ReadAllBytes(path),
            new JsonDocumentOptions { CommentHandling = JsonCommentHandling.Disallow, AllowTrailingCommas = false });
        return document.RootElement.Clone();
    }

    private static string ResolveRelative(string root, string relativePath)
    {
        if (Path.IsPathRooted(relativePath)
            || relativePath.Contains("\\", StringComparison.Ordinal))
        {
            throw new IOException($"Contract path '{relativePath}' must be a forward-slash relative path.");
        }

        var canonicalRoot = Path.GetFullPath(root);
        var path = Path.GetFullPath(Path.Combine(canonicalRoot, relativePath.Replace('/', Path.DirectorySeparatorChar)));
        var prefix = canonicalRoot.EndsWith(Path.DirectorySeparatorChar)
            ? canonicalRoot
            : canonicalRoot + Path.DirectorySeparatorChar;
        if (!path.StartsWith(prefix, PathComparison))
        {
            throw new IOException($"Contract path '{relativePath}' is outside '{canonicalRoot}'.");
        }
        return path;
    }

    private static StringComparer PathComparer =>
        OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal;

    private static StringComparison PathComparison =>
        OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
}
