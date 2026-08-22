using System.Collections.Immutable;

namespace FSharp2.Conformance;

public static class CaseDiscovery
{
    public static ImmutableArray<string> Discover(string root)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(root);
        var conformanceRoot = Path.GetFullPath(root);
        var casesRoot = Path.GetFullPath(Path.Combine(conformanceRoot, "cases"));
        if (!Directory.Exists(casesRoot))
        {
            return [];
        }

        var prefix = casesRoot.EndsWith(Path.DirectorySeparatorChar)
            ? casesRoot
            : casesRoot + Path.DirectorySeparatorChar;

        return
        [
            .. Directory
                .EnumerateFiles(casesRoot, "*.case.json", SearchOption.AllDirectories)
                .Select(Path.GetFullPath)
                .Where(path => path.StartsWith(prefix, PathComparison))
                .Where(path => path.EndsWith(".case.json", StringComparison.Ordinal))
                .OrderBy(path => IsIntegrationCase(casesRoot, path))
                .ThenBy(static path => path, StringComparer.Ordinal),
        ];
    }

    private static bool IsIntegrationCase(string casesRoot, string path)
    {
        var relative = Path.GetRelativePath(casesRoot, path);
        return relative.StartsWith(
            $"integration{Path.DirectorySeparatorChar}",
            PathComparison);
    }

    private static StringComparison PathComparison =>
        OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
}
