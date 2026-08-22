using System.Security;
using System.Text;

namespace FSharp2.Conformance;

public static class NuGetConfigWriter
{
    public static byte[] CreateBytes(string packageSource)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(packageSource);
        var escapedSource = SecurityElement.Escape(Path.GetFullPath(packageSource)) ?? string.Empty;
        return new UTF8Encoding(encoderShouldEmitUTF8Identifier: false).GetBytes(
            $"""
            <?xml version="1.0" encoding="utf-8"?>
            <configuration>
              <packageSources>
                <clear />
                <add key="conformance" value="{escapedSource}" />
              </packageSources>
              <packageSourceMapping>
                <clear />
                <packageSource key="conformance">
                  <package pattern="FSharp2.Compiler.MSBuild" />
                </packageSource>
              </packageSourceMapping>
            </configuration>
            """.Replace("\r\n", "\n", StringComparison.Ordinal));
    }
}
