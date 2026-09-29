using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class ReferenceClosure
{
    public static ImmutableArray<MaterializedTargetReference> Resolve(
        ConformanceRepository repository,
        ConformanceCase conformanceCase)
    {
        var closureReference = conformanceCase.Document.GetProperty("targetReferences");
        var closurePath = ResolveUnderRoot(
            repository.Root,
            closureReference.GetProperty("closurePath").GetString()!);
        if (!File.Exists(closurePath))
        {
            throw Invalid("reference-closure-missing", closurePath, "The target-reference closure does not exist.");
        }

        using var closureDocument = JsonDocument.Parse(File.ReadAllBytes(closurePath));
        var sdkRoot = SdkSelection.DefaultRoot(
            repository.Toolchain.GetProperty("sdkVersion").GetString()!);
        var sdk = SdkSelection.Resolve(repository.Root, sdkRoot, null);
        var references = ImmutableArray.CreateBuilder<MaterializedTargetReference>();
        foreach (var reference in closureDocument.RootElement
                     .GetProperty("references")
                     .EnumerateArray()
                     .OrderBy(static item => item.GetProperty("order").GetInt32()))
        {
            var assemblyName = reference.GetProperty("assemblyName").GetString()!;
            if (!reference.GetProperty("sha256BySdkRid").TryGetProperty(sdk.Rid, out var hashProperty))
            {
                throw Invalid(
                    "reference-hash-rid",
                    reference.GetProperty("logicalPath").GetString()!,
                    $"The closure does not record a '{assemblyName}' hash for SDK runtime identifier '{sdk.Rid}'.");
            }
            var expectedHash = hashProperty.GetString()!;
            var physicalPath = ResolveReferencePath(sdk, assemblyName, expectedHash);
            var bytes = File.ReadAllBytes(physicalPath);
            var actualHash = Hashing.Sha256(bytes);
            if (!string.Equals(actualHash, expectedHash, StringComparison.Ordinal))
            {
                throw Invalid(
                    "reference-hash",
                    reference.GetProperty("logicalPath").GetString()!,
                    $"The target reference hash is '{actualHash}'. Expected '{expectedHash}'.");
            }

            references.Add(
                new MaterializedTargetReference(
                    reference.GetProperty("stableId").GetString()!,
                    reference.GetProperty("logicalPath").GetString()!,
                    actualHash,
                    assemblyName,
                    reference.GetProperty("version").GetString()!,
                    reference.GetProperty("culture").GetString()!,
                    reference.GetProperty("publicKeyToken").GetString()!,
                    reference.GetProperty("evaluatedItemSource").GetString()!,
                    [.. bytes]));
        }

        return references.ToImmutable();
    }

    private static string ResolveReferencePath(
        SdkSelectionResult sdk,
        string assemblyName,
        string expectedHash)
    {
        if (string.Equals(assemblyName, "FSharp.Core", StringComparison.Ordinal))
        {
            return RequireHashMatch(
                Path.Combine(sdk.Root, "sdk", sdk.Version, "FSharp", "FSharp.Core.dll"),
                expectedHash);
        }

        var packRoot = Path.Combine(sdk.Root, "packs", "Microsoft.NETCore.App.Ref");
        if (!Directory.Exists(packRoot))
        {
            throw Invalid("reference-pack-missing", packRoot, "The selected SDK does not contain the reference pack.");
        }

        var fileName = $"{assemblyName}.dll";
        var matches = Directory
            .EnumerateFiles(packRoot, fileName, SearchOption.AllDirectories)
            .Where(path => string.Equals(Hashing.Sha256File(path), expectedHash, StringComparison.Ordinal))
            .OrderBy(static path => path, StringComparer.Ordinal)
            .ToArray();
        return matches.Length switch
        {
            0 => throw Invalid(
                "reference-hash",
                fileName,
                $"The selected SDK does not contain a '{fileName}' file with hash '{expectedHash}'."),
            _ => matches[0],
        };
    }

    private static string RequireHashMatch(string path, string expectedHash)
    {
        if (!File.Exists(path))
        {
            throw Invalid("reference-missing", path, "The selected SDK target reference does not exist.");
        }

        var actualHash = Hashing.Sha256File(path);
        if (!string.Equals(actualHash, expectedHash, StringComparison.Ordinal))
        {
            throw Invalid(
                "reference-hash",
                path,
                $"The target reference hash is '{actualHash}'. Expected '{expectedHash}'.");
        }

        return path;
    }

    private static string ResolveUnderRoot(string root, string relativePath)
    {
        var fullRoot = Path.GetFullPath(root);
        var path = Path.GetFullPath(Path.Combine(fullRoot, relativePath));
        var prefix = fullRoot.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                     + Path.DirectorySeparatorChar;
        if (!path.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
        {
            throw Invalid("path-outside-root", relativePath, "The reference path is outside the conformance root.");
        }

        return path;
    }

    private static ConformanceContractException Invalid(string code, string path, string message) =>
        new([new(code, path, message)]);
}
