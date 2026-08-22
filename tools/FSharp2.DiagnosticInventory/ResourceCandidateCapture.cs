using System.Collections.Immutable;
using System.Globalization;
using System.Xml.Linq;

namespace FSharp2.DiagnosticInventory;

public static partial class DiagnosticInventoryGenerator
{
    private static void CaptureResources(GitReader git, ImmutableArray<SourceEntry>.Builder entries)
    {
        var fsComp = git.Show(FsCompPath);
        var resources = new List<(int Number, string Key, string Message, int Line, string Text)>();
        var lines = SplitLines(fsComp);
        for (var index = 0; index < lines.Length; index++)
        {
            var matched = NumberedResource().Match(lines[index]);
            if (!matched.Success)
            {
                continue;
            }
            resources.Add((
                int.Parse(matched.Groups[1].Value, CultureInfo.InvariantCulture),
                matched.Groups[2].Value,
                matched.Groups[3].Value,
                index + 1,
                lines[index]));
        }
        if (resources.Count != 1238 || resources.Select(static item => item.Number).Distinct().Count() != 1172)
        {
            throw new InvalidDataException("The pinned FSComp.txt numbered resource count changed.");
        }

        foreach (var resource in resources)
        {
            var familyId = FamilyId(resource.Number);
            var productionKey = $"FSComp.SR.{resource.Key}";
            var source = SourceLocation(git.Commit, FsCompPath, resource.Line);
            var hash = HashLine(resource.Text);
            entries.Add(new(
                familyId,
                "compiler-mapping",
                "source-mapping",
                source,
                hash,
                productionKey,
                resource.Key,
                resource.Message,
                null,
                null,
                false));
            entries.Add(new(
                familyId,
                "resource-inventory",
                "neutral-resource",
                source,
                hash,
                productionKey,
                resource.Key,
                resource.Message,
                "en-US",
                null,
                false));
        }

        foreach (var culture in SatelliteCultures)
        {
            var path = $"src/Compiler/xlf/FSComp.txt.{culture}.xlf";
            var text = git.Show(path);
            var hash = HashText(text);
            var document = XDocument.Parse(text, LoadOptions.PreserveWhitespace);
            var keys = document
                .Descendants()
                .Where(static element => element.Name.LocalName == "trans-unit")
                .Select(static element => element.Attribute("id")?.Value)
                .Where(static value => value is not null)
                .Select(static value => value!)
                .ToHashSet(StringComparer.Ordinal);
            foreach (var resource in resources.Where(item => keys.Contains(item.Key)))
            {
                entries.Add(new(
                    FamilyId(resource.Number),
                    "resource-inventory",
                    "satellite-resource",
                    $"dotnet/fsharp@{git.Commit}:{path}#trans-unit={resource.Key}",
                    hash,
                    $"FSComp.SR.{resource.Key}",
                    resource.Key,
                    null,
                    culture,
                    null,
                    false));
            }
        }
    }

    private static void CaptureCompilerMappings(
        GitReader git,
        ImmutableArray<SourceEntry>.Builder entries)
    {
        var text = git.Show(CompilerDiagnosticsPath);
        var start = text.IndexOf("member exn.DiagnosticNumber =", StringComparison.Ordinal);
        var end = text.IndexOf("member x.Number =", start, StringComparison.Ordinal);
        if (start < 0 || end <= start)
        {
            throw new InvalidDataException("The pinned compiler diagnostic mapping was not found.");
        }
        var startLine = text.AsSpan(0, start).Count('\n') + 1;
        var lines = SplitLines(text[start..end]);
        for (var index = 0; index < lines.Length; index++)
        {
            var line = lines[index];
            var mapped = FixedMapping().Match(line);
            if (mapped.Success)
            {
                AddMapping(
                    entries,
                    git,
                    int.Parse(mapped.Groups[2].Value, CultureInfo.InvariantCulture),
                    mapped.Groups[1].Value,
                    line,
                    startLine + index,
                    false);
            }

            var reserved = ReservedNumber().Match(line);
            if (reserved.Success)
            {
                AddMapping(
                    entries,
                    git,
                    int.Parse(reserved.Groups[1].Value, CultureInfo.InvariantCulture),
                    $"reserved-{reserved.Groups[1].Value}",
                    line,
                    startLine + index,
                    true);
            }
        }
        for (var number = 94; number <= 100; number++)
        {
            var sourceLine = Array.FindIndex(lines, static line => line.Contains("avoid 94-100", StringComparison.Ordinal));
            AddMapping(entries, git, number, $"reserved-{number}", lines[sourceLine], startLine + sourceLine, true);
        }
    }

    private static void AddMapping(
        ImmutableArray<SourceEntry>.Builder entries,
        GitReader git,
        int number,
        string key,
        string line,
        int lineNumber,
        bool reserved)
    {
        entries.Add(new(
            FamilyId(number),
            "compiler-mapping",
            "source-mapping",
            SourceLocation(git.Commit, CompilerDiagnosticsPath, lineNumber),
            HashLine(line),
            $"CompilerDiagnostics.{Slug(key)}",
            Slug(key),
            null,
            null,
            null,
            reserved));
    }
}
