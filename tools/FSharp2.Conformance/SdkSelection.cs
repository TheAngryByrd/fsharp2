using System.Collections.Immutable;
using System.Diagnostics;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class SdkSelection
{
    public static SdkSelectionResult Resolve(
        string conformanceRoot,
        string? explicitRoot,
        string? environmentRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(conformanceRoot);
        var toolchainPath = Path.Combine(Path.GetFullPath(conformanceRoot), "toolchains", "dotnet-sdk.json");
        if (!File.Exists(toolchainPath))
        {
            throw Invalid(toolchainPath, "The SDK toolchain lock does not exist.");
        }

        using var document = JsonDocument.Parse(File.ReadAllBytes(toolchainPath));
        var expectedVersion = document.RootElement.GetProperty("sdkVersion").GetString()!;
        var selectedRoot =
            FirstText(
                explicitRoot,
                environmentRoot,
                Environment.GetEnvironmentVariable("FSHARP2_DOTNET_ROOT"));
        if (selectedRoot is null)
        {
            throw Invalid(
                toolchainPath,
                "Set --dotnet-root or FSHARP2_DOTNET_ROOT to the locked SDK root.");
        }

        selectedRoot = Path.GetFullPath(selectedRoot);
        var dotnetPath = Path.Combine(selectedRoot, OperatingSystem.IsWindows() ? "dotnet.exe" : "dotnet");
        if (!File.Exists(dotnetPath))
        {
            throw Invalid(dotnetPath, "The selected SDK does not contain the dotnet executable.");
        }

        var environment = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.OrdinalIgnoreCase);
        environment["DOTNET_ROOT"] = selectedRoot;
        environment["DOTNET_MULTILEVEL_LOOKUP"] = "0";
        environment["DOTNET_HOST_PATH"] = dotnetPath;
        environment["PATH"] =
            selectedRoot
            + Path.PathSeparator
            + (Environment.GetEnvironmentVariable("PATH") ?? string.Empty);

        var version = ReadVersion(dotnetPath, environment.ToImmutable());
        if (!string.Equals(version, expectedVersion, StringComparison.Ordinal))
        {
            throw Invalid(
                dotnetPath,
                $"Selected dotnet reported '{version}'. Expected '{expectedVersion}'.");
        }

        var rid = ReadRid(selectedRoot, version);
        return new(selectedRoot, dotnetPath, version, rid, environment.ToImmutable());
    }

    private static string ReadRid(string sdkRoot, string version)
    {
        // The SDK .version file lists the commit, the version, and the SDK runtime identifier on the first three lines.
        var versionFilePath = Path.Combine(sdkRoot, "sdk", version, ".version");
        if (!File.Exists(versionFilePath))
        {
            throw Invalid(versionFilePath, "The selected SDK does not contain its .version file.");
        }

        var lines = File.ReadAllLines(versionFilePath);
        if (lines.Length < 3 || string.IsNullOrWhiteSpace(lines[2]))
        {
            throw Invalid(versionFilePath, "The selected SDK .version file does not declare a runtime identifier.");
        }
        return lines[2].Trim();
    }

    public static string DefaultRoot(string sdkVersion)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(sdkVersion);
        return FirstText(
                Environment.GetEnvironmentVariable("FSHARP2_DOTNET_ROOT"),
                Environment.GetEnvironmentVariable("DOTNET_ROOT"))
            ?? Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                $"fsharp2-sdk-{sdkVersion}");
    }

    private static string ReadVersion(
        string dotnetPath,
        ImmutableDictionary<string, string> environment)
    {
        var startInfo = new ProcessStartInfo(dotnetPath)
        {
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
        };
        startInfo.ArgumentList.Add("--version");
        foreach (var item in environment)
        {
            startInfo.Environment[item.Key] = item.Value;
        }

        using var process = new Process { StartInfo = startInfo };
        process.Start();
        var output = process.StandardOutput.ReadToEndAsync();
        var error = process.StandardError.ReadToEndAsync();
        if (!process.WaitForExit(30_000))
        {
            process.Kill(entireProcessTree: true);
            throw Invalid(dotnetPath, "The selected SDK version check timed out.");
        }

        if (process.ExitCode != 0)
        {
            throw Invalid(dotnetPath, $"The selected SDK version check failed: {error.Result.Trim()}");
        }
        return output.Result.Trim();
    }

    private static string? FirstText(params string?[] values) =>
        values.FirstOrDefault(static value => !string.IsNullOrWhiteSpace(value));

    private static ConformanceContractException Invalid(string path, string message) =>
        new([new("sdk-selection", path, message)]);
}
