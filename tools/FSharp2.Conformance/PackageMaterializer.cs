using System.IO.Compression;
using System.Text;

namespace FSharp2.Conformance;

public sealed record MaterializedPackage(
    string SourceRoot,
    string PackagePath,
    string PackageVersion,
    string Sha256);

public static class PackageMaterializer
{
    public const string PackageVersion = "0.0.0-conformance";
    private const string PackageId = "FSharp2.Compiler.MSBuild";

    public static MaterializedPackage Materialize(string runRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(runRoot);
        var repositoryRoot = FindRepositoryRoot();
        var propsPath = Path.Combine(
            repositoryRoot,
            "src",
            PackageId,
            "buildTransitive",
            $"{PackageId}.props");
        var targetsPath = Path.Combine(
            repositoryRoot,
            "src",
            PackageId,
            "buildTransitive",
            $"{PackageId}.targets");
        if (!File.Exists(propsPath) || !File.Exists(targetsPath))
        {
            throw new ConformanceContractException(
                [new("package-input", PackageId, "The FSharp2 MSBuild integration assets do not exist.")]);
        }

        var sourceRoot = Path.Combine(Path.GetFullPath(runRoot), "immutable", "packages");
        Directory.CreateDirectory(sourceRoot);
        var packagePath = Path.Combine(sourceRoot, $"{PackageId}.{PackageVersion}.nupkg");
        var packageBytes = CreatePackage(
            File.ReadAllBytes(propsPath),
            File.ReadAllBytes(targetsPath));
        var expectedHash = Hashing.Sha256(packageBytes);
        if (File.Exists(packagePath))
        {
            var actualHash = Hashing.Sha256File(packagePath);
            if (!string.Equals(actualHash, expectedHash, StringComparison.Ordinal))
            {
                throw new ConformanceContractException(
                    [new("package-hash", packagePath, $"The existing package hash is '{actualHash}'. Expected '{expectedHash}'.")]);
            }
        }
        else
        {
            File.WriteAllBytes(packagePath, packageBytes);
        }
        return new MaterializedPackage(sourceRoot, packagePath, PackageVersion, expectedHash);
    }

    private static byte[] CreatePackage(byte[] props, byte[] targets)
    {
        using var stream = new MemoryStream();
        using (var archive = new ZipArchive(stream, ZipArchiveMode.Create, leaveOpen: true))
        {
            WriteEntry(
                archive,
                $"{PackageId}.nuspec",
                Utf8($"""
                <?xml version="1.0" encoding="utf-8"?>
                <package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">
                  <metadata>
                    <id>{PackageId}</id>
                    <version>{PackageVersion}</version>
                    <authors>TheAngryByrd</authors>
                    <description>Conformance-local FSharp2 MSBuild integration.</description>
                    <developmentDependency>true</developmentDependency>
                  </metadata>
                </package>
                """));
            WriteEntry(
                archive,
                "[Content_Types].xml",
                Utf8("""
                <?xml version="1.0" encoding="utf-8"?>
                <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
                  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml" />
                  <Default Extension="psmdcp" ContentType="application/vnd.openxmlformats-package.core-properties+xml" />
                  <Default Extension="props" ContentType="application/octet" />
                  <Default Extension="targets" ContentType="application/octet" />
                  <Default Extension="nuspec" ContentType="application/octet" />
                </Types>
                """));
            WriteEntry(
                archive,
                "_rels/.rels",
                Utf8($"""
                <?xml version="1.0" encoding="utf-8"?>
                <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
                  <Relationship Type="http://schemas.microsoft.com/packaging/2010/07/manifest" Target="/{PackageId}.nuspec" Id="R1" />
                </Relationships>
                """));
            WriteEntry(archive, $"buildTransitive/{PackageId}.props", props);
            WriteEntry(archive, $"buildTransitive/{PackageId}.targets", targets);
        }
        return stream.ToArray();
    }

    private static void WriteEntry(ZipArchive archive, string path, byte[] bytes)
    {
        var entry = archive.CreateEntry(path, CompressionLevel.NoCompression);
        entry.LastWriteTime = new DateTimeOffset(1980, 1, 1, 0, 0, 0, TimeSpan.Zero);
        using var target = entry.Open();
        target.Write(bytes);
    }

    private static byte[] Utf8(string value) =>
        new UTF8Encoding(encoderShouldEmitUTF8Identifier: false).GetBytes(
            value.Replace("\r\n", "\n", StringComparison.Ordinal));

    private static string FindRepositoryRoot()
    {
        foreach (var start in new[] { Directory.GetCurrentDirectory(), AppContext.BaseDirectory })
        {
            var directory = new DirectoryInfo(Path.GetFullPath(start));
            while (directory is not null)
            {
                if (File.Exists(Path.Combine(directory.FullName, "fsharp2.sln")))
                {
                    return directory.FullName;
                }
                directory = directory.Parent;
            }
        }
        throw new ConformanceContractException(
            [new("repository-root", PackageId, "The repository root could not be found.")]);
    }
}
