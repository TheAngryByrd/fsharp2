using System.Collections.Immutable;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class Program
{
    public static async Task<int> Main(string[] args)
    {
        if (args.Length == 0)
        {
            WriteUsage();
            return 2;
        }

        try
        {
            var command = args[0];
            var options = ParseOptions(args.AsSpan(1));
            return command switch
            {
                "validate" => Validate(options),
                "list" => List(options),
                "capture-oracle" => await ConformanceRunner.CaptureOracleAsync(options, CancellationToken.None),
                "run" => await ConformanceRunner.RunAsync(options, CancellationToken.None),
                "replay" => await ConformanceRunner.ReplayAsync(options, CancellationToken.None),
                _ => UsageError($"Unknown command '{command}'."),
            };
        }
        catch (ConformanceContractException exception)
        {
            WriteIssues(exception.Issues);
            return 2;
        }
        catch (ArgumentException exception)
        {
            Console.Error.WriteLine(exception.Message);
            return 2;
        }
        catch (Exception exception)
        {
            Console.Error.WriteLine($"infra-error: {exception.Message}");
            return 3;
        }
    }

    private static int Validate(ImmutableDictionary<string, string> options)
    {
        RejectUnknown(options, ["root"]);
        var root = Require(options, "root");
        var result = ManifestLoader.Validate(root);
        if (!result.IsValid)
        {
            WriteIssues(result.Issues);
            return 2;
        }

        var repository = ManifestLoader.Load(root);
        var coverage = CoverageValidator.Validate(repository);
        if (!coverage.IsValid)
        {
            WriteIssues(coverage.Issues);
            return 2;
        }

        Console.WriteLine($"validated {repository.Cases.Length} cases");
        return 0;
    }

    private static int List(ImmutableDictionary<string, string> options)
    {
        RejectUnknown(options, ["root", "case", "tag"]);
        var root = Require(options, "root");
        var repository = ManifestLoader.Load(root);
        IEnumerable<ConformanceCase> cases = repository.Cases;
        if (options.TryGetValue("case", out var caseId))
        {
            cases = cases.Where(item => string.Equals(item.CaseId, caseId, StringComparison.Ordinal));
        }
        if (options.TryGetValue("tag", out var tag))
        {
            cases = cases.Where(item => item.Tags.Contains(tag, StringComparer.Ordinal));
        }

        var selected = cases.ToArray();
        if (options.ContainsKey("case") && selected.Length == 0)
        {
            throw new ConformanceContractException(
                [new("case-missing", options["case"], "The selected conformance case does not exist.")]);
        }

        foreach (var item in selected)
        {
            var kind = GetString(item.Document, "kind");
            var state = item.Document.GetProperty("envelope").GetProperty("state").GetString();
            Console.WriteLine($"{item.CaseId}\t{kind}\t{state}\t{string.Join(',', item.Tags)}");
        }
        return 0;
    }

    internal static ImmutableDictionary<string, string> ParseOptions(ReadOnlySpan<string> args)
    {
        var options = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);
        for (var index = 0; index < args.Length; index += 2)
        {
            var name = args[index];
            if (!name.StartsWith("--", StringComparison.Ordinal) || name.Length == 2)
            {
                throw new ArgumentException($"Expected an option name, received '{name}'.");
            }
            if (index + 1 >= args.Length)
            {
                throw new ArgumentException($"Option '{name}' requires a value.");
            }
            if (!options.TryAdd(name[2..], args[index + 1]))
            {
                throw new ArgumentException($"Option '{name}' was specified more than once.");
            }
        }
        return options.ToImmutable();
    }

    internal static string Require(ImmutableDictionary<string, string> options, string name) =>
        options.TryGetValue(name, out var value) && !string.IsNullOrWhiteSpace(value)
            ? value
            : throw new ArgumentException($"Option '--{name}' is required.");

    internal static void RejectUnknown(
        ImmutableDictionary<string, string> options,
        ImmutableArray<string> allowed)
    {
        foreach (var name in options.Keys)
        {
            if (!allowed.Contains(name, StringComparer.Ordinal))
            {
                throw new ArgumentException($"Option '--{name}' is not valid for this command.");
            }
        }
    }

    private static string? GetString(JsonElement value, string propertyName) =>
        value.TryGetProperty(propertyName, out var property) ? property.GetString() : null;

    private static void WriteIssues(ImmutableArray<ValidationIssue> issues)
    {
        foreach (var issue in issues)
        {
            Console.Error.WriteLine($"{issue.Code} {issue.Path}: {issue.Message}");
        }
    }

    private static int UsageError(string message)
    {
        Console.Error.WriteLine(message);
        WriteUsage();
        return 2;
    }

    private static void WriteUsage()
    {
        Console.Error.WriteLine("FSharp2.Conformance validate --root <tests/conformance>");
        Console.Error.WriteLine("FSharp2.Conformance list --root <tests/conformance> [--case <case-id>] [--tag <tag>]");
        Console.Error.WriteLine("FSharp2.Conformance capture-oracle --root <tests/conformance> --case <case-id> --output-root <artifacts/conformance>");
        Console.Error.WriteLine("FSharp2.Conformance run --root <tests/conformance> --case <case-id> --dotnet-root <sdk-root> --fsharp2-host <host> --output-root <artifacts/conformance>");
        Console.Error.WriteLine("FSharp2.Conformance replay --bundle <bundle-directory> --dotnet-root <sdk-root> --output-root <artifacts/conformance>");
    }
}
