namespace FSharp2.DiagnosticInventory;

public static class Program
{
    public static int Main(string[] args)
    {
        try
        {
            if (args.Length == 0 || !string.Equals(args[0], "generate", StringComparison.Ordinal))
            {
                return Usage();
            }

            var options = ParseOptions(args.AsSpan(1));
            RejectUnknown(options);
            var request = new GenerationRequest(
                Require(options, "source-repository"),
                Require(options, "source-commit"),
                Require(options, "conformance-root"),
                Require(options, "capture-output"),
                Require(options, "inventory-output"),
                Require(options, "closure-output"),
                Require(options, "oracle-lock-output"));
            var result = DiagnosticInventoryGenerator.Generate(request);
            Console.WriteLine(
                $"generated {result.CandidateCount} candidates, {result.VariantCount} variants, "
                + $"and {result.SourceEntryCount} source entries");
            return 0;
        }
        catch (Exception exception) when (exception is ArgumentException or InvalidDataException)
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

    private static Dictionary<string, string> ParseOptions(ReadOnlySpan<string> args)
    {
        var options = new Dictionary<string, string>(StringComparer.Ordinal);
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
        return options;
    }

    private static string Require(Dictionary<string, string> options, string name) =>
        options.TryGetValue(name, out var value) && !string.IsNullOrWhiteSpace(value)
            ? value
            : throw new ArgumentException($"Option '--{name}' is required.");

    private static void RejectUnknown(Dictionary<string, string> options)
    {
        string[] allowed =
        [
            "source-repository",
            "source-commit",
            "conformance-root",
            "capture-output",
            "inventory-output",
            "closure-output",
            "oracle-lock-output",
        ];
        foreach (var name in options.Keys)
        {
            if (!allowed.Contains(name, StringComparer.Ordinal))
            {
                throw new ArgumentException($"Option '--{name}' is not valid for this command.");
            }
        }
    }

    private static int Usage()
    {
        Console.Error.WriteLine(
            "FSharp2.DiagnosticInventory generate "
            + "--source-repository <dotnet-fsharp-repository> "
            + "--source-commit <commit> "
            + "--conformance-root <tests/conformance> "
            + "--capture-output <candidate-sources.json> "
            + "--inventory-output <inventory.json> "
            + "--closure-output <closure.json> "
            + "--oracle-lock-output <oracle-lock.json>");
        return 2;
    }
}
