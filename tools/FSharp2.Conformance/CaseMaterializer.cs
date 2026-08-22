using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class CaseMaterializer
{
    public static MaterializedCase Materialize(
        ConformanceRepository repository,
        ConformanceCase conformanceCase)
    {
        ArgumentNullException.ThrowIfNull(repository);
        ArgumentNullException.ThrowIfNull(conformanceCase);

        var sources = ImmutableArray.CreateBuilder<MaterializedSource>();
        foreach (var source in conformanceCase.Document
                     .GetProperty("sources")
                     .EnumerateArray()
                     .OrderBy(static item => item.GetProperty("order").GetInt32()))
        {
            var fixturePath = source.GetProperty("fixturePath").GetString()!;
            var physicalPath = ResolveUnderRoot(repository.Root, fixturePath);
            if (!File.Exists(physicalPath))
            {
                throw Invalid("fixture-missing", fixturePath, "The source fixture does not exist.");
            }

            var bytes = File.ReadAllBytes(physicalPath);
            var actualHash = Hashing.Sha256(bytes);
            var expectedHash = source.GetProperty("sha256").GetString()!;
            if (!string.Equals(actualHash, expectedHash, StringComparison.Ordinal))
            {
                throw Invalid(
                    "fixture-hash",
                    fixturePath,
                    $"The source fixture hash is '{actualHash}'. Expected '{expectedHash}'.");
            }

            sources.Add(
                new MaterializedSource(
                    source.GetProperty("stableId").GetString()!,
                    source.GetProperty("kind").GetString()!,
                    source.GetProperty("logicalPath").GetString()!,
                    actualHash,
                    [.. bytes]));
        }

        var requestedArtifacts = conformanceCase.Document
            .GetProperty("expectedArtifacts")
            .EnumerateArray()
            .Where(static item =>
                string.Equals(item.GetProperty("state").GetString(), "required", StringComparison.Ordinal))
            .Select(static item => item.GetProperty("kind").GetString()!)
            .ToImmutableArray();

        return new MaterializedCase(
            conformanceCase.CaseId,
            sources.ToImmutable(),
            ReferenceClosure.Resolve(repository, conformanceCase),
            OptionNormalizer.Normalize(conformanceCase.Document.GetProperty("options")),
            requestedArtifacts,
            conformanceCase.Document.Clone());
    }

    private static string ResolveUnderRoot(string root, string relativePath)
    {
        var fullRoot = Path.GetFullPath(root);
        var path = Path.GetFullPath(Path.Combine(fullRoot, relativePath));
        var prefix = fullRoot.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                     + Path.DirectorySeparatorChar;
        if (!path.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
        {
            throw Invalid("path-outside-root", relativePath, "The fixture path is outside the conformance root.");
        }

        return path;
    }

    private static ConformanceContractException Invalid(string code, string path, string message) =>
        new([new(code, path, message)]);
}
