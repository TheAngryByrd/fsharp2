using System.Collections.Immutable;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using FSharp2.Conformance;

namespace FSharp2.DiagnosticInventory;

public static partial class DiagnosticInventoryGenerator
{
    private static object[] Placeholders(string? message)
    {
        if (message is null)
        {
            return [];
        }
        var placeholders = new List<object>();
        var index = 1;
        foreach (Match match in FormatPlaceholder().Matches(message))
        {
            var type = match.Groups[1].Value switch
            {
                "d" or "i" or "u" or "x" or "X" => "int",
                "f" or "F" or "e" or "E" or "g" or "G" => "float",
                "b" => "bool",
                "c" => "char",
                _ => "string",
            };
            placeholders.Add(new { name = $"arg{index}", type });
            index++;
        }
        return placeholders.ToArray();
    }

    private static object BuildClosure(string conformanceRoot, ImmutableArray<FamilyOutput> families)
    {
        var templatePath = Path.Combine(
            conformanceRoot,
            "expectations",
            "bindings",
            "net10.0.closure.json");
        using var template = JsonDocument.Parse(File.ReadAllBytes(templatePath));
        var references = template.RootElement.GetProperty("references").Clone();
        var dispositions = families
            .SelectMany(static family => family.Variants.Select(variant => new
            {
                familyId = family.FamilyId,
                variantId = variant.VariantId,
                workingDisposition = variant.Disposition,
                finalDisposition = (string?)null,
                blockingIssue = variant.BlockingIssue,
                evidenceHashes = variant.EvidenceHashes,
            }))
            .OrderBy(static item => item.familyId, StringComparer.Ordinal)
            .ThenBy(static item => item.variantId, StringComparer.Ordinal)
            .ToArray();
        return new
        {
            documentKind = "closure-expectation-v1",
            schemaVersion = 1,
            closureId = "diagnostics-fs-families-working-closure",
            family = "diagnostics",
            targetFramework = "net10.0",
            references,
            comparison = "exact-ordered-closure",
            review = new
            {
                owner = "compiler-compatibility",
                sourceIssue = "#27",
                refreshCondition = "Refresh when the diagnostic inventory hash or any working disposition changes.",
            },
            diagnosticClosure = new
            {
                state = "working",
                dispositions,
            },
        };
    }

    private static void WriteOracleLock(string conformanceRoot, string outputPath)
    {
        var sourceLockPath = Path.Combine(
            conformanceRoot,
            "locks",
            "bindings",
            "value-function-parse-negative.oracle-lock.json");
        var root = JsonNode.Parse(File.ReadAllBytes(sourceLockPath))?.AsObject()
            ?? throw new InvalidDataException("The source Oracle lock is invalid.");
        var caseId = root["caseId"]?.GetValue<string>()
            ?? throw new InvalidDataException("The source Oracle lock has no caseId.");
        var casePath = Directory.EnumerateFiles(
                Path.Combine(conformanceRoot, "cases"),
                "*.case.json",
                SearchOption.AllDirectories)
            .Single(path =>
            {
                using var document = JsonDocument.Parse(File.ReadAllBytes(path));
                return document.RootElement.GetProperty("caseId").GetString() == caseId;
            });
        using var caseDocument = JsonDocument.Parse(File.ReadAllBytes(casePath));
        var expectedOptionHash = Hashing.Sha256(
            CanonicalJson.Canonicalize(caseDocument.RootElement.GetProperty("options")));
        var actualOptionHash = root["oracleIdentity"]?["optionHash"]?.GetValue<string>();
        if (!string.Equals(expectedOptionHash, actualOptionHash, StringComparison.Ordinal))
        {
            throw new InvalidDataException(
                $"The source Oracle option hash is '{actualOptionHash}', expected '{expectedOptionHash}'.");
        }

        root["lockId"] = "diagnostics.oracle-identity";
        root["family"] = "diagnostics";
        root["review"] = new JsonObject
        {
            ["owner"] = "compiler-compatibility",
            ["sourceIssue"] = "#27",
            ["capturedAt"] = "2026-08-21T09:47:00Z",
            ["expiryCondition"] = "Refresh when the case hash, Oracle SDK identity, complete options, culture, or encoding changes.",
        };
        WriteJson(outputPath, root);
    }

    private static void WriteJson(string path, object value)
    {
        var fullPath = Path.GetFullPath(path);
        Directory.CreateDirectory(Path.GetDirectoryName(fullPath)!);
        var bytes = JsonSerializer.SerializeToUtf8Bytes(value, Json);
        var output = new byte[bytes.Length + 1];
        bytes.CopyTo(output, 0);
        output[^1] = (byte)'\n';
        File.WriteAllBytes(fullPath, output);
    }
}
