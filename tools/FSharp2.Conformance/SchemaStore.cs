using System.Collections.Immutable;
using System.Collections.Concurrent;
using System.Text.Json;
using System.Threading;
using Json.Schema;

namespace FSharp2.Conformance;

public sealed class SchemaStore
{
    private readonly string _schemaRoot;
    private static readonly ConcurrentDictionary<string, Lazy<JsonSchema>> Schemas =
        new(StringComparer.Ordinal);

    public SchemaStore(string conformanceRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(conformanceRoot);
        _schemaRoot = Path.GetFullPath(Path.Combine(conformanceRoot, "schemas", "v1"));
    }

    public ValidationResult Validate(string schemaName, string instancePath, JsonElement instance)
    {
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        JsonSchema schema;
        try
        {
            schema = Load(schemaName);
        }
        catch (Exception exception) when (exception is IOException or JsonException or JsonSchemaException)
        {
            issues.Add(new("schema-load", instancePath, exception.Message));
            return new(false, issues.ToImmutable());
        }

        var options = new EvaluationOptions
        {
            OutputFormat = OutputFormat.List,
            IncludeApplicatorErrors = true,
            RequireFormatValidation = true,
        };
        var result = schema.Evaluate(instance, options);
        AddIssues(result, instancePath, issues);
        return new(issues.Count == 0, issues.ToImmutable());
    }

    private JsonSchema Load(string schemaName)
    {
        var schemaPath = Path.GetFullPath(Path.Combine(_schemaRoot, schemaName));
        EnsureDescendant(_schemaRoot, schemaPath);
        return Schemas.GetOrAdd(
            schemaName,
            _ => new Lazy<JsonSchema>(
                () => JsonSchema.FromText(File.ReadAllText(schemaPath)),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;
    }

    private static void AddIssues(
        EvaluationResults result,
        string instancePath,
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
                issues.Add(
                    new(
                        "schema",
                        $"{instancePath}{result.InstanceLocation}",
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
            AddIssues(detail, instancePath, issues);
        }
    }

    private static void EnsureDescendant(string root, string path)
    {
        var prefix = root.EndsWith(Path.DirectorySeparatorChar) ? root : root + Path.DirectorySeparatorChar;
        var comparison =
            OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
        if (!path.StartsWith(prefix, comparison))
        {
            throw new IOException($"Schema path '{path}' is outside '{root}'.");
        }
    }
}
