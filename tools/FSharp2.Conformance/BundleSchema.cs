using System.Collections.Immutable;
using System.Reflection;
using System.Text.Json;
using System.Threading;
using Json.Schema;

namespace FSharp2.Conformance;

internal static class BundleSchema
{
    private static readonly Lazy<JsonSchema> Schema = new(
        Load,
        LazyThreadSafetyMode.ExecutionAndPublication);

    public static ValidationResult Validate(JsonElement bundle)
    {
        var options = new EvaluationOptions
        {
            OutputFormat = OutputFormat.List,
            IncludeApplicatorErrors = true,
            RequireFormatValidation = true,
        };
        var result = Schema.Value.Evaluate(bundle, options);
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        AddIssues(result, issues);
        return new(issues.Count == 0, issues.ToImmutable());
    }

    private static JsonSchema Load()
    {
        using var stream = Assembly.GetExecutingAssembly()
            .GetManifestResourceStream("FSharp2.Conformance.bundle.schema.json")
            ?? throw new JsonSchemaException("The embedded bundle schema does not exist.");
        using var reader = new StreamReader(stream);
        return JsonSchema.FromText(reader.ReadToEnd());
    }

    private static void AddIssues(
        EvaluationResults result,
        ImmutableArray<ValidationIssue>.Builder issues)
    {
        if (result.IsValid)
        {
            return;
        }
        if (result.Errors is { Count: > 0 })
        {
            foreach (var error in result.Errors.OrderBy(static item => item.Key, StringComparer.Ordinal))
            {
                issues.Add(new(
                    "bundle-schema",
                    $"bundle.json{result.InstanceLocation}",
                    $"{error.Key}: {error.Value}"));
            }
        }
        if (result.Details is null)
        {
            return;
        }
        foreach (var detail in result.Details
                     .Where(static item => !item.IsValid)
                     .OrderBy(static item => item.InstanceLocation.ToString(), StringComparer.Ordinal))
        {
            AddIssues(detail, issues);
        }
    }
}
