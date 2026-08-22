using System.Collections.Immutable;
using System.Text;
using System.Text.Json;
using FSharp2.Compiler;
using Microsoft.FSharp.Core;

namespace FSharp2.Conformance;

public static class CompilerContractProbe
{
    private static readonly UTF8Encoding StrictUtf8 = new(false, true);

    public static CompilationRequest CreateRequest(MaterializedCase materializedCase)
    {
        ArgumentNullException.ThrowIfNull(materializedCase);
        var document = materializedCase.ResolvedDocument;
        var project = document.GetProperty("project");
        var assemblyName = project.GetProperty("assemblyName").GetString()!;
        var semantic = materializedCase.Options.Semantic;
        var diagnostic = materializedCase.Options.Diagnostic;
        var emission = materializedCase.Options.Emission;
        var signing = materializedCase.Options.Signing;
        var msbuild = document.GetProperty("options").GetProperty("msbuild");

        var sources = materializedCase.Sources
            .Select(source =>
                SourceSnapshot.Create(
                    StableIdentity.create(source.StableId),
                    source.LogicalPath,
                    StrictUtf8.GetString(source.Bytes.AsSpan()),
                    source.Sha256))
            .ToArray();
        var references = materializedCase.TargetReferences
            .Select(reference =>
                TargetReferenceSnapshot.Create(
                    StableIdentity.create(reference.StableId),
                    reference.LogicalPath,
                    reference.Bytes.ToArray(),
                    reference.Sha256))
            .ToArray();
        var semanticOptions = SemanticOptions.Create(
            ReadStrings(semantic, "defines"),
            LanguageVersion(semantic),
            ParseOptimization(semantic.GetProperty("optimization").GetString()!),
            GetBoolean(semantic, "checkNulls"),
            GetBoolean(msbuild, "noFramework"),
            OptionalString(semantic, "targetProfile"));
        var diagnosticOptions = DiagnosticOptions.Create(
            diagnostic.TryGetProperty("warningLevel", out var warningLevel)
                ? FSharpOption<int>.Some(warningLevel.GetInt32())
                : FSharpOption<int>.None,
            ReadStrings(diagnostic, "noWarn"),
            false,
            ReadStrings(diagnostic, "warnAsError"));
        var debugFormat = emission.TryGetProperty("debugFormat", out var format)
                          && string.Equals(format.GetString(), "portable", StringComparison.Ordinal)
            ? DebugFormat.Portable
            : DebugFormat.None;
        var emissionOptions = EmissionOptions.Create(
            ParseCompilationTarget(emission.GetProperty("outputType").GetString()!),
            GetBoolean(emission, "deterministic"),
            GetBoolean(emission, "highEntropyVirtualAddress"),
            debugFormat,
            [],
            materializedCase.Sources.Select(static source => source.LogicalPath).ToArray(),
            []);
        var signingOptions = SigningOptions.Create(
            ParseSigningMode(signing.GetProperty("mode").GetString()!),
            []);
        var requestedArtifacts = materializedCase.RequestedArtifacts
            .Select(ParseRequestedArtifact)
            .ToArray();

        return CompilationRequest.Create(
            CompilerContract.Version,
            StableIdentity.create(materializedCase.CaseId),
            CompilationAssemblyIdentity.Create(
                StableIdentity.create($"assembly:{materializedCase.CaseId}"),
                assemblyName),
            sources,
            references,
            semanticOptions,
            diagnosticOptions,
            emissionOptions,
            signingOptions,
            ResourceInputs.Create([], []),
            requestedArtifacts);
    }

    private static FSharpOption<string> OptionalString(JsonElement value, string name) =>
        value.TryGetProperty(name, out var property) && property.ValueKind == JsonValueKind.String
            ? FSharpOption<string>.Some(property.GetString()!)
            : FSharpOption<string>.None;

    private static FSharpOption<string> LanguageVersion(JsonElement semantic)
    {
        if (!semantic.TryGetProperty("languageVersion", out var value)
            || value.ValueKind != JsonValueKind.String
            || string.Equals(value.GetString(), "default", StringComparison.Ordinal))
        {
            return FSharpOption<string>.None;
        }
        return FSharpOption<string>.Some(value.GetString()!);
    }

    private static bool GetBoolean(JsonElement value, string name) =>
        value.TryGetProperty(name, out var property) && property.ValueKind == JsonValueKind.True;

    private static string[] ReadStrings(JsonElement value, string name) =>
        value.TryGetProperty(name, out var property) && property.ValueKind == JsonValueKind.Array
            ? property.EnumerateArray().Select(static item => item.GetString()!).ToArray()
            : [];

    private static OptimizationMode ParseOptimization(string value) => value switch
    {
        "disabled" => OptimizationMode.Disabled,
        "enabled" => OptimizationMode.Enabled,
        _ => throw Invalid("optimization", value),
    };

    private static SigningMode ParseSigningMode(string value) => value switch
    {
        "none" => SigningMode.Unsigned,
        "delay" => SigningMode.DelaySign,
        "public" => SigningMode.PublicSign,
        "full" => SigningMode.FullSign,
        _ => throw Invalid("signing", value),
    };

    private static CompilationTarget ParseCompilationTarget(string value) => value switch
    {
        "library" => CompilationTarget.Library,
        "exe" => CompilationTarget.Executable,
        _ => throw Invalid("outputType", value),
    };

    private static RequestedArtifact ParseRequestedArtifact(string value) => value switch
    {
        "implementation-assembly" => RequestedArtifact.ImplementationAssembly,
        "portable-pdb" => RequestedArtifact.PortablePdb,
        "reference-assembly" => RequestedArtifact.ReferenceAssembly,
        "xml-documentation" => RequestedArtifact.Documentation,
        _ => RequestedArtifact.NewCustom(StableIdentity.create($"artifact:{value}")),
    };

    private static ConformanceContractException Invalid(string field, string value) =>
        new([new("compiler-contract", field, $"The value '{value}' cannot map to the compiler contract.")]);
}
