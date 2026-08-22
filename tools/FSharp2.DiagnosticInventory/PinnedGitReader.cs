using System.Diagnostics;
using System.Globalization;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;

namespace FSharp2.DiagnosticInventory;

public static partial class DiagnosticInventoryGenerator
{
    private static IEnumerable<GrepMatch> ParseGrep(string output)
    {
        foreach (var line in SplitLines(output))
        {
            var matched = GrepOutput().Match(line);
            if (matched.Success)
            {
                yield return new(
                    matched.Groups[1].Value,
                    int.Parse(matched.Groups[2].Value, CultureInfo.InvariantCulture),
                    matched.Groups[3].Value);
            }
        }
    }

    private static string[] SplitLines(string value) =>
        value.Split('\n').Select(static line => line.TrimEnd('\r')).ToArray();

    private static string FamilyId(int number) => $"FS{number:0000}";

    private static string SourceLocation(string commit, string path, int line) =>
        $"dotnet/fsharp@{commit}:{path}:{line}";

    private static string HashLine(string line) => HashText(line);

    private static string HashText(string text) =>
        $"sha256:{Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(text))).ToLowerInvariant()}";

    private static string ShortHash(string text) =>
        Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(text))).ToLowerInvariant()[..8];

    private static string Slug(string value)
    {
        var slug = SlugCharacters().Replace(value, "-").Trim('-').ToLowerInvariant();
        return string.IsNullOrEmpty(slug) ? "candidate" : slug;
    }

    private static void RequireDirectory(string path, string description)
    {
        if (!Directory.Exists(path))
        {
            throw new ArgumentException($"The {description} '{path}' does not exist.");
        }
    }

    [GeneratedRegex("""^(\d{1,4}),([^,]+),"(.*)"$""", RegexOptions.CultureInvariant)]
    private static partial Regex NumberedResource();

    [GeneratedRegex(@"^\s*\|\s*(.*?)\s*->\s*(\d{1,4})\s*$", RegexOptions.CultureInvariant)]
    private static partial Regex FixedMapping();

    [GeneratedRegex(@"//\s*(\d{1,4}) cannot be reused", RegexOptions.CultureInvariant)]
    private static partial Regex ReservedNumber();

    [GeneratedRegex(@"(?:DiagnosticWithText|DiagnosticWithSuggestions)\s*\(\s*(\d{1,4})\s*,", RegexOptions.CultureInvariant)]
    private static partial Regex LiteralDiagnosticWithText();

    [GeneratedRegex(@"UserCompilerMessage\s*\(.*?,\s*(\d{1,4})\s*,", RegexOptions.CultureInvariant)]
    private static partial Regex LiteralUserCompilerMessage();

    [GeneratedRegex(@"(?:UserCompilerMessage|DiagnosticWithText|DiagnosticWithSuggestions)\s*\([^)]*\bn\b", RegexOptions.CultureInvariant)]
    private static partial Regex DynamicConstructorMapping();

    [GeneratedRegex(@"FS(\d{4})(?!\d)", RegexOptions.CultureInvariant)]
    private static partial Regex DiagnosticCode();

    [GeneratedRegex(@"^FS\d{4}$", RegexOptions.CultureInvariant)]
    private static partial Regex DiagnosticCodeFull();

    [GeneratedRegex("""(?:#(?:nowarn|warnon)|--(?:nowarn|warnon|warnaserror-?)\s*:|with(?:Warning|Error)Code|\b(?:Warning|Error|Information))\s*"?(?:FS)?(\d{1,4})""", RegexOptions.CultureInvariant)]
    private static partial Regex WarningNumber();

    [GeneratedRegex(@"^[^:]+:(.*?):(\d+):(.*)$", RegexOptions.CultureInvariant)]
    private static partial Regex GrepOutput();

    [GeneratedRegex(@"(?<!%)%(?:[-+0 #]*\d*(?:\.\d+)?)?([a-zA-Z])", RegexOptions.CultureInvariant)]
    private static partial Regex FormatPlaceholder();

    [GeneratedRegex(@"[^a-zA-Z0-9.-]+", RegexOptions.CultureInvariant)]
    private static partial Regex SlugCharacters();
}

internal sealed class GitReader(string repository, string commit)
{
    public string Commit { get; } = commit;

    public void RequireCommit()
    {
        var type = Run("cat-file", "-t", Commit).Trim();
        if (!string.Equals(type, "commit", StringComparison.Ordinal))
        {
            throw new InvalidDataException($"Git object '{Commit}' is not a commit.");
        }
    }

    public string Show(string path) => Run("show", $"{Commit}:{path}");

    public string Grep(string pattern, params string[] paths)
    {
        var arguments = new List<string>
        {
            "grep",
            "-I",
            "-n",
            "-E",
            pattern,
            Commit,
            "--",
        };
        arguments.AddRange(paths);
        return Run(arguments.ToArray());
    }

    private string Run(params string[] arguments)
    {
        var startInfo = new ProcessStartInfo("git")
        {
            WorkingDirectory = repository,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        foreach (var argument in arguments)
        {
            startInfo.ArgumentList.Add(argument);
        }
        using var process = Process.Start(startInfo)
            ?? throw new InvalidOperationException("Git did not start.");
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();
        process.WaitForExit();
        if (process.ExitCode != 0)
        {
            throw new InvalidDataException(
                $"Git exited with code {process.ExitCode}: {error.Result.Trim()}");
        }
        return output.Result;
    }
}
