using System.Collections.Immutable;
using System.Reflection.PortableExecutable;

namespace FSharp2.Conformance;

public static class NativeAotHostValidator
{
    public static ValidationResult Validate(string hostPath)
    {
        if (string.IsNullOrWhiteSpace(hostPath))
        {
            return Invalid(hostPath ?? string.Empty, "The FSharp2 host path must contain text.");
        }

        var path = Path.GetFullPath(hostPath);
        if (!File.Exists(path))
        {
            return Invalid(path, "The FSharp2 host does not exist.");
        }
        if (string.Equals(Path.GetExtension(path), ".dll", StringComparison.OrdinalIgnoreCase))
        {
            return Invalid(path, "The FSharp2 host must be a native executable, not a managed assembly.");
        }

        try
        {
            using var stream = File.OpenRead(path);
            using var reader = new PEReader(stream);
            if (reader.PEHeaders.PEHeader is null)
            {
                return Invalid(path, "The FSharp2 host does not contain a PE executable header.");
            }
            if (reader.HasMetadata || reader.PEHeaders.CorHeader is not null)
            {
                return Invalid(path, "The FSharp2 host contains a managed CLR entry point.");
            }
        }
        catch (BadImageFormatException exception)
        {
            return Invalid(path, $"The FSharp2 host is not a valid native PE executable: {exception.Message}");
        }

        var stem = Path.Combine(Path.GetDirectoryName(path)!, Path.GetFileNameWithoutExtension(path));
        var managedSidecars = new[] { stem + ".dll", stem + ".deps.json", stem + ".runtimeconfig.json" };
        var sidecar = managedSidecars.FirstOrDefault(File.Exists);
        return sidecar is null
            ? ValidationResult.Valid
            : Invalid(sidecar, "The FSharp2 host has a managed application sidecar.");
    }

    private static ValidationResult Invalid(string path, string message) =>
        new(false, ImmutableArray.Create(new ValidationIssue("nativeaot-host", path, message)));
}
