using System.Security.Cryptography;

namespace FSharp2.Conformance;

public static class Hashing
{
    public static string Sha256(ReadOnlySpan<byte> value) =>
        $"sha256:{Convert.ToHexString(SHA256.HashData(value)).ToLowerInvariant()}";

    public static string Sha256File(string path)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(path);
        using var stream = File.OpenRead(Path.GetFullPath(path));
        return $"sha256:{Convert.ToHexString(SHA256.HashData(stream)).ToLowerInvariant()}";
    }
}
