using System.Buffers.Binary;
using System.Collections.Immutable;
using System.Reflection.PortableExecutable;

namespace FSharp2.Conformance;

public static class NativeAotHostValidator
{
    private const int ElfExecutable = 2;
    private const int ElfSharedObject = 3;
    private const uint MachOExecutable = 2;

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

        var header = ReadHeader(path);
        var image = header.Length >= 4 && header[0] == 0x7F && header[1] == (byte)'E' && header[2] == (byte)'L' && header[3] == (byte)'F'
            ? ValidateElf(path, header)
            : header.Length >= 4 && IsMachOMagic(BinaryPrimitives.ReadUInt32LittleEndian(header))
                ? ValidateMachO(path, header)
                : ValidatePe(path);
        if (image is not null)
        {
            return image;
        }

        var stem = Path.Combine(Path.GetDirectoryName(path)!, Path.GetFileNameWithoutExtension(path));
        var managedSidecars = new[] { stem + ".dll", stem + ".deps.json", stem + ".runtimeconfig.json" };
        var sidecar = managedSidecars.FirstOrDefault(File.Exists);
        return sidecar is null
            ? ValidationResult.Valid
            : Invalid(sidecar, "The FSharp2 host has a managed application sidecar.");
    }

    private static byte[] ReadHeader(string path)
    {
        using var stream = File.OpenRead(path);
        var buffer = new byte[20];
        var read = stream.ReadAtLeast(buffer, buffer.Length, throwOnEndOfStream: false);
        return buffer[..read];
    }

    private static ValidationResult? ValidatePe(string path)
    {
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
            return Invalid(path, $"The FSharp2 host is not a native PE, ELF, or Mach-O executable: {exception.Message}");
        }
        return null;
    }

    private static ValidationResult? ValidateElf(string path, byte[] header)
    {
        if (header.Length < 18)
        {
            return Invalid(path, "The FSharp2 host ELF header is truncated.");
        }
        var type = header[5] == 2
            ? BinaryPrimitives.ReadUInt16BigEndian(header.AsSpan(16, 2))
            : BinaryPrimitives.ReadUInt16LittleEndian(header.AsSpan(16, 2));
        return type is ElfExecutable or ElfSharedObject
            ? null
            : Invalid(path, $"The FSharp2 host ELF type is {type}. Expected an executable.");
    }

    private static ValidationResult? ValidateMachO(string path, byte[] header)
    {
        if (header.Length < 16)
        {
            return Invalid(path, "The FSharp2 host Mach-O header is truncated.");
        }
        var magic = BinaryPrimitives.ReadUInt32LittleEndian(header);
        var fileType = magic is 0xFEEDFACE or 0xFEEDFACF
            ? BinaryPrimitives.ReadUInt32LittleEndian(header.AsSpan(12, 4))
            : BinaryPrimitives.ReadUInt32BigEndian(header.AsSpan(12, 4));
        return fileType == MachOExecutable
            ? null
            : Invalid(path, $"The FSharp2 host Mach-O file type is {fileType}. Expected an executable.");
    }

    private static bool IsMachOMagic(uint magic) =>
        magic is 0xFEEDFACE or 0xFEEDFACF or 0xCEFAEDFE or 0xCFFAEDFE;

    private static ValidationResult Invalid(string path, string message) =>
        new(false, ImmutableArray.Create(new ValidationIssue("nativeaot-host", path, message)));
}
