using System.Collections.Immutable;
using System.Globalization;
using System.Text.Json;
using System.Text.RegularExpressions;
using FSharp2.Conformance;

namespace FSharp2.DiagnosticInventory;

public static partial class DiagnosticInventoryGenerator
{
    private static void CaptureRuntimeNumberConstructors(
        GitReader git,
        ImmutableArray<SourceEntry>.Builder entries,
        ImmutableArray<OpenDomain>.Builder domains)
    {
        var output = git.Grep(
            "UserCompilerMessage|DiagnosticWithText|DiagnosticWithSuggestions",
            "src/Compiler",
            "src/FSharp.Build/Fsc.fs");
        foreach (var match in ParseGrep(output))
        {
            var constructor = match.Text.Contains("UserCompilerMessage", StringComparison.Ordinal)
                ? "UserCompilerMessage"
                : match.Text.Contains("DiagnosticWithSuggestions", StringComparison.Ordinal)
                    ? "DiagnosticWithSuggestions"
                    : "DiagnosticWithText";
            var literal = LiteralDiagnosticWithText().Match(match.Text);
            if (!literal.Success)
            {
                literal = LiteralUserCompilerMessage().Match(match.Text);
            }
            if (literal.Success)
            {
                entries.Add(new(
                    FamilyId(int.Parse(literal.Groups[1].Value, CultureInfo.InvariantCulture)),
                    "runtime-number-constructor",
                    "source-mapping",
                    SourceLocation(git.Commit, match.Path, match.Line),
                    HashLine(match.Text),
                    $"runtime-constructor.{Slug(match.Path)}.{match.Line}",
                    $"runtime-constructor-{match.Line}",
                    null,
                    null,
                    null,
                    false,
                    constructor));
            }

            if (match.Text.Contains("number: int", StringComparison.Ordinal)
                || DynamicConstructorMapping().IsMatch(match.Text))
            {
                domains.Add(new(
                    constructor,
                    SourceLocation(git.Commit, match.Path, match.Line),
                    HashLine(match.Text),
                    "open",
                    "The constructor accepts an integer supplied by compiler or external integration code.",
                    "#27"));
            }
        }
    }

    private static void CaptureUpstreamTests(GitReader git, ImmutableArray<SourceEntry>.Builder entries)
    {
        var exactFsCodes = git.Grep(@"FS[0-9]{4}([^0-9]|$)", "tests");
        foreach (var match in ParseGrep(exactFsCodes))
        {
            foreach (Match code in DiagnosticCode().Matches(match.Text))
            {
                AddBaseline(entries, git, match, int.Parse(code.Groups[1].Value, CultureInfo.InvariantCulture));
            }
        }

        string[] warningPaths =
        [
            "tests/FSharp.Compiler.ComponentTests/CompilerOptions/fsc/warn",
            "tests/FSharp.Compiler.ComponentTests/CompilerOptions/fsc/warnon",
            "tests/FSharp.Compiler.ComponentTests/CompilerDirectives/Nowarn.fs",
            "tests/fsharp/Compiler/Warnings",
        ];
        var warnings = git.Grep(
            @"FS[0-9]{4}([^0-9]|$)|#(no)?warn|--(no)?warn|with(Warning|Error)Code|\b(Warning|Error|Information)[[:space:]]+[0-9]{1,4}",
            warningPaths);
        foreach (var match in ParseGrep(warnings))
        {
            foreach (Match code in DiagnosticCode().Matches(match.Text))
            {
                AddBaseline(entries, git, match, int.Parse(code.Groups[1].Value, CultureInfo.InvariantCulture));
            }
            foreach (Match code in WarningNumber().Matches(match.Text))
            {
                AddBaseline(entries, git, match, int.Parse(code.Groups[1].Value, CultureInfo.InvariantCulture));
            }
        }
    }

    private static void AddBaseline(
        ImmutableArray<SourceEntry>.Builder entries,
        GitReader git,
        GrepMatch match,
        int number)
    {
        entries.Add(new(
            FamilyId(number),
            "upstream-baseline",
            "upstream-baseline",
            SourceLocation(git.Commit, match.Path, match.Line),
            HashLine(match.Text),
            $"upstream-test.{Slug(match.Path)}.{match.Line}",
            $"upstream-test-{match.Line}",
            match.Text,
            null,
            null,
            false));
    }

    private static void CaptureDynamicObservations(
        string conformanceRoot,
        ImmutableArray<SourceEntry>.Builder entries)
    {
        var locksRoot = Path.Combine(conformanceRoot, "locks");
        foreach (var path in Directory.EnumerateFiles(
                     locksRoot,
                     "*.oracle-lock.json",
                     SearchOption.AllDirectories).Order(StringComparer.Ordinal))
        {
            if (Path.GetRelativePath(locksRoot, path)
                .Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)[0]
                .Equals("diagnostics", StringComparison.Ordinal))
            {
                continue;
            }

            using var document = JsonDocument.Parse(File.ReadAllBytes(path));
            if (!document.RootElement.TryGetProperty("caseId", out var caseIdProperty)
                || !document.RootElement.TryGetProperty("diagnostics", out var diagnostics))
            {
                continue;
            }
            var caseId = caseIdProperty.GetString()!;
            var lockHash = Hashing.Sha256File(path);
            var relative = Path.GetRelativePath(conformanceRoot, path).Replace('\\', '/');
            var index = 0;
            foreach (var diagnostic in diagnostics.EnumerateArray())
            {
                var code = diagnostic.GetProperty("code").GetString();
                if (code is not null && DiagnosticCodeFull().IsMatch(code))
                {
                    entries.Add(new(
                        code,
                        "dynamic-oracle",
                        "dynamic-oracle",
                        $"tests/conformance/{relative}#diagnostic={index}",
                        lockHash,
                        $"oracle.{caseId}.{index}",
                        $"oracle-{caseId}-{index}",
                        diagnostic.GetProperty("message").GetString(),
                        null,
                        caseId,
                        false));
                }
                index++;
            }
        }
    }
}
