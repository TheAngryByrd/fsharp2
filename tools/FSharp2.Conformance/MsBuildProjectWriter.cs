using System.Collections.Immutable;
using System.Security;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class MsBuildProjectWriter
{
    public static byte[] CreateProjectBytes(MaterializedCase materializedCase)
    {
        ArgumentNullException.ThrowIfNull(materializedCase);
        var project = materializedCase.ResolvedDocument.GetProperty("project");
        var properties = project.GetProperty("properties")
            .EnumerateArray()
            .ToDictionary(
                static item => item.GetProperty("name").GetString()!,
                static item => item.GetProperty("value").ToString(),
                StringComparer.Ordinal);
        var targetFramework = materializedCase.ResolvedDocument
            .GetProperty("envelope")
            .GetProperty("targetFrameworks")[0]
            .GetString()!;
        var semantic = materializedCase.Options.Semantic;
        var diagnostic = materializedCase.Options.Diagnostic;
        var emission = materializedCase.Options.Emission;
        var signing = materializedCase.Options.Signing;
        var resources = materializedCase.Options.Resources;
        var outputType = string.Equals(
            String(emission, "outputType") ?? project.GetProperty("projectType").GetString(),
            "exe",
            StringComparison.Ordinal)
            ? "Exe"
            : "Library";
        var otherFlags = OtherFlags(materializedCase);
        var builder = new StringBuilder();
        builder.AppendLine("<Project Sdk=\"Microsoft.NET.Sdk\">");
        builder.AppendLine("  <PropertyGroup>");
        Property(builder, "TargetFramework", targetFramework);
        Property(builder, "OutputType", outputType);
        Property(builder, "AssemblyName", project.GetProperty("assemblyName").GetString()!);
        Property(builder, "EnableDefaultCompileItems", "false");
        Property(builder, "DisableImplicitFSharpCoreReference", "true");
        Property(builder, "GenerateAssemblyInfo", "false");
        Property(builder, "GenerateTargetFrameworkAttribute", "false");
        Property(builder, "EnableSourceLink", "false");
        Property(builder, "EmbedUntrackedSources", "false");
        Property(builder, "DebugType", String(emission, "debugFormat") ?? Get(properties, "DebugType", "portable"));
        Property(builder, "DebugSymbols", "true");
        Property(builder, "PdbFile", "$(AssemblyName).pdb");
        Property(builder, "Deterministic", BooleanText(emission, "deterministic", Get(properties, "Deterministic", "true")));
        Property(builder, "LangVersion", LanguageVersion(semantic));
        Property(builder, "Optimize", EnabledText(semantic, "optimization", "false"));
        Property(builder, "Tailcalls", EnabledText(semantic, "tailCalls", "false"));
        Property(builder, "TargetProfile", String(semantic, "targetProfile") ?? string.Empty);
        Property(builder, "WarningLevel", diagnostic.GetProperty("warningLevel").ToString());
        Property(builder, "NoWarn", JoinFirst(diagnostic, "disabledWarnings", "noWarn"));
        Property(builder, "WarnOn", Join(diagnostic, "enabledWarnings"));
        Property(builder, "TreatWarningsAsErrors", BooleanText(diagnostic, "treatWarningsAsErrors", "false"));
        Property(builder, "WarningsAsErrors", JoinFirst(diagnostic, "warningsAsErrors", "warnAsError"));
        Property(builder, "WarningsNotAsErrors", JoinFirst(diagnostic, "warningsNotAsErrors", "warnAsWarn"));
        Property(builder, "LCID", ValueText(diagnostic, "lcid"));
        Property(builder, "PreferredUILang", PreferredUiLanguage(diagnostic));
        Property(builder, "Utf8Output", BooleanText(diagnostic, "utf8Output", "false"));
        Property(builder, "VisualStudioStyleErrors", VisualStudioStyleErrors(diagnostic));
        Property(builder, "DefineConstants", Join(semantic, "defines"));
        Property(builder, "PathMap", PathMap(emission));
        Property(builder, "GenerateDocumentationFile", BooleanText(emission, "documentation", "false"));
        Property(builder, "ProduceReferenceAssembly", BooleanText(emission, "referenceAssembly", "false"));
        Property(builder, "SignAssembly", SigningText(signing, "sign"));
        Property(builder, "DelaySign", SigningText(signing, "delay"));
        Property(builder, "PublicSign", BooleanText(signing, "publicSign", SigningText(signing, "public")));
        Property(builder, "AssemblyOriginatorKeyFile", String(signing, "keyFile") ?? string.Empty);
        Property(builder, "Win32Resource", First(resources, "native"));
        Property(builder, "OtherFlags", string.Join(' ', otherFlags));
        Property(builder, "ManagePackageVersionsCentrally", "false");
        Property(builder, "RestorePackagesWithLockFile", "true");
        Property(builder, "RestoreSources", "$(FSharp2ConformancePackageSource)");
        Property(builder, "RestoreIgnoreFailedSources", "true");
        builder.AppendLine("  </PropertyGroup>");
        builder.AppendLine("  <ItemGroup>");
        builder.Append("    <PackageReference Include=\"FSharp2.Compiler.MSBuild\" Version=\"")
            .Append(PackageMaterializer.PackageVersion)
            .AppendLine("\" PrivateAssets=\"all\" />");
        foreach (var source in materializedCase.Sources)
        {
            builder.Append("    <Compile Include=\"")
                .Append(Escape(Relative(source.LogicalPath)))
                .AppendLine("\" />");
        }
        foreach (var resource in Strings(resources, "embedded"))
        {
            builder.Append("    <EmbeddedResource Include=\"")
                .Append(Escape(Relative(resource)))
                .AppendLine("\" />");
        }
        foreach (var reference in materializedCase.TargetReferences)
        {
            builder.Append("    <Reference Include=\"")
                .Append(Escape(reference.AssemblyName))
                .AppendLine("\">");
            builder.Append("      <HintPath>")
                .Append(Escape(Relative(reference.LogicalPath)))
                .AppendLine("</HintPath>");
            builder.Append("      <Private>")
                .Append(string.Equals(reference.AssemblyName, "FSharp.Core", StringComparison.Ordinal)
                    ? "true"
                    : "false")
                .AppendLine("</Private>");
            builder.AppendLine("    </Reference>");
        }
        builder.AppendLine("  </ItemGroup>");
        builder.AppendLine("</Project>");
        return new UTF8Encoding(false).GetBytes(builder.ToString().Replace("\r\n", "\n", StringComparison.Ordinal));
    }

    private static void Property(StringBuilder builder, string name, string value) =>
        builder.Append("    <")
            .Append(name)
            .Append('>')
            .Append(Escape(value))
            .Append("</")
            .Append(name)
            .AppendLine(">");

    private static string Get(Dictionary<string, string> properties, string name, string fallback) =>
        properties.TryGetValue(name, out var value) ? value : fallback;

    private static string Join(JsonElement value, string name) =>
        value.TryGetProperty(name, out var items)
            ? string.Join(';', items.EnumerateArray().Select(static item => item.GetString()))
            : string.Empty;

    private static string JoinFirst(JsonElement value, params string[] names)
    {
        foreach (var name in names)
        {
            if (value.TryGetProperty(name, out _))
            {
                return Join(value, name);
            }
        }
        return string.Empty;
    }

    private static IEnumerable<string> Strings(JsonElement value, string name) =>
        value.TryGetProperty(name, out var items) && items.ValueKind == JsonValueKind.Array
            ? items.EnumerateArray().Select(static item => item.GetString()!)
            : [];

    private static string First(JsonElement value, string name) =>
        Strings(value, name).FirstOrDefault() ?? string.Empty;

    private static string? String(JsonElement value, params string[] names)
    {
        foreach (var name in names)
        {
            if (value.TryGetProperty(name, out var property) && property.ValueKind == JsonValueKind.String)
            {
                return property.GetString();
            }
        }
        return null;
    }

    private static string ValueText(JsonElement value, string name) =>
        value.TryGetProperty(name, out var property) && property.ValueKind != JsonValueKind.Null
            ? property.ToString()
            : string.Empty;

    private static string BooleanText(JsonElement value, string name, string fallback) =>
        value.TryGetProperty(name, out var property)
        && property.ValueKind is JsonValueKind.True or JsonValueKind.False
            ? property.GetBoolean().ToString().ToLowerInvariant()
            : fallback;

    private static string EnabledText(JsonElement value, string name, string fallback) =>
        String(value, name) switch
        {
            "enabled" => "true",
            "disabled" => "false",
            _ => fallback,
        };

    private static string LanguageVersion(JsonElement semantic)
    {
        var value = String(semantic, "languageVersion");
        return string.Equals(value, "default", StringComparison.Ordinal)
            ? string.Empty
            : value ?? string.Empty;
    }

    private static string SigningText(JsonElement signing, string property) =>
        (String(signing, "mode"), property) switch
        {
            ("delay", "sign") or ("public", "sign") or ("full", "sign") => "true",
            ("delay", "delay") => "true",
            ("public", "public") => "true",
            _ => "false",
        };

    private static string VisualStudioStyleErrors(JsonElement diagnostic) =>
        String(diagnostic, "diagnosticStyle") switch
        {
            "visual-studio" => "true",
            "gcc" or "rich" or "flat" => "false",
            _ => string.Empty,
        };

    private static string PreferredUiLanguage(JsonElement diagnostic)
    {
        var value = String(diagnostic, "preferredUILanguage", "culture");
        return string.Equals(value, "en-US", StringComparison.OrdinalIgnoreCase)
            ? string.Empty
            : value ?? string.Empty;
    }

    private static string PathMap(JsonElement emission)
    {
        var logicalRoot = "/_fsharp2_conformance/root";
        if (emission.TryGetProperty("pathMap", out var pathMap) && pathMap.ValueKind == JsonValueKind.Array)
        {
            logicalRoot = pathMap.EnumerateArray()
                .Select(static item => item.GetProperty("value").GetString())
                .FirstOrDefault(static value => !string.IsNullOrWhiteSpace(value))
                ?? logicalRoot;
        }
        return $"$(MSBuildProjectDirectory)={logicalRoot}";
    }

    private static ImmutableArray<string> OtherFlags(MaterializedCase materializedCase)
    {
        var semantic = materializedCase.Options.Semantic;
        var diagnostic = materializedCase.Options.Diagnostic;
        var resources = materializedCase.Options.Resources;
        var values = new List<string>();
        if (semantic.TryGetProperty("checkedArithmetic", out var checkedArithmetic)
            && checkedArithmetic.ValueKind == JsonValueKind.True)
        {
            values.Add("--checked+");
        }
        AddBooleanFlag(values, diagnostic, "fullPaths", "--fullpaths");
        AddBooleanFlag(values, diagnostic, "flatErrors", "--flaterrors");
        if (diagnostic.TryGetProperty("consoleColors", out var consoleColors)
            && consoleColors.ValueKind == JsonValueKind.True)
        {
            values.Add("--consolecolors+");
        }
        values.AddRange(Strings(resources, "linked").Select(static value => $"--linkresource:{value}"));
        values.AddRange(materializedCase.Options.CompilerArguments.Where(static value => !IsNormalizedAlias(value)));
        return [.. values.Distinct(StringComparer.Ordinal)];
    }

    private static bool IsNormalizedAlias(string value)
    {
        var option = value.Split(':', 2)[0];
        return option.ToLowerInvariant() is
            "--abortonerror"
            or "--checked+"
            or "--checked-"
            or "--consolecolors+"
            or "--consolecolors-"
            or "--debug"
            or "--deterministic+"
            or "--deterministic-"
            or "--flaterrors"
            or "--fullpaths"
            or "--gccerrors"
            or "--gnu-style-errors"
            or "--langversion"
            or "--lcid"
            or "--max-errors"
            or "--maxerrors"
            or "--nowarn"
            or "--optimize+"
            or "--optimize-"
            or "--preferreduilang"
            or "--richerrors"
            or "--tailcalls+"
            or "--tailcalls-"
            or "--targetprofile"
            or "--testparsererrorrecovery"
            or "--utf8output"
            or "--vserrors"
            or "--warn"
            or "--warnaserror"
            or "--warnaserror+"
            or "--warnaserror-"
            or "--warnon";
    }

    private static void AddBooleanFlag(
        ICollection<string> values,
        JsonElement source,
        string propertyName,
        string flag)
    {
        if (source.TryGetProperty(propertyName, out var property) && property.ValueKind == JsonValueKind.True)
        {
            values.Add(flag);
        }
    }

    private static string Relative(string logicalPath) => logicalPath.TrimStart('/', '\\').Replace('\\', '/');

    private static string Escape(string value) => SecurityElement.Escape(value) ?? string.Empty;
}
