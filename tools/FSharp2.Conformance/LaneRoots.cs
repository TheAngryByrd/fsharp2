using System.Text.Json;
using System.Text.Json.Serialization;

namespace FSharp2.Conformance;

public static class LaneRoots
{
    public static LaneRootSet Create(string runRoot, string runId)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(runRoot);
        ArgumentException.ThrowIfNullOrWhiteSpace(runId);
        var fullRunRoot = Path.GetFullPath(runRoot);
        Directory.CreateDirectory(fullRunRoot);
        EnsureMarker(fullRunRoot, runId);
        return new LaneRootSet(
            runId,
            fullRunRoot,
            CreateLane(fullRunRoot, runId, LaneKind.Oracle, "oracle"),
            CreateLane(fullRunRoot, runId, LaneKind.FSharp2, "fsharp2"));
    }

    private static LaneRoot CreateLane(string runRoot, string runId, LaneKind kind, string name)
    {
        var root = Path.Combine(runRoot, "lanes", name);
        Directory.CreateDirectory(root);
        EnsureMarker(root, $"{runId}:{name}");
        var work = CreateDirectory(root, "work");
        var temp = CreateDirectory(root, "tmp");
        var intermediate = CreateDirectory(root, "obj");
        var output = CreateDirectory(root, "bin");
        var service = CreateDirectory(root, "service");
        var cache = CreateDirectory(root, "cache");
        return new LaneRoot(
            kind,
            root,
            work,
            temp,
            intermediate,
            output,
            service,
            cache,
            Path.Combine(root, $"{name}.rsp"),
            Path.Combine(root, $"{name}.binlog"));
    }

    private static string CreateDirectory(string root, string name)
    {
        var path = Path.Combine(root, name);
        Directory.CreateDirectory(path);
        return path;
    }

    private static void EnsureMarker(string runRoot, string runId)
    {
        var markerPath = Path.Combine(runRoot, ".fsharp2-conformance-root");
        if (File.Exists(markerPath))
        {
            return;
        }

        var marker = JsonSerializer.SerializeToUtf8Bytes(
            new RunMarker(runId, runRoot),
            new JsonSerializerOptions
            {
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
            });
        File.WriteAllBytes(markerPath, marker);
    }

    private sealed record RunMarker(string RunId, string Root);
}
