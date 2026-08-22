using System.Collections.Immutable;
using System.Xml.Linq;

namespace FSharp2.Conformance;

public static class ProductionGraphGuard
{
    private static readonly ImmutableArray<string> ProductionProjects =
    [
        "src/FSharp2.Compiler.Prototype.Core/FSharp2.Compiler.Prototype.Core.fsproj",
        "src/fsc2.Prototype/fsc2.Prototype.fsproj",
        "src/FSharp2.Compiler.MSBuild/FSharp2.Compiler.MSBuild.csproj",
    ];

    public static ValidationResult Validate(string repositoryRoot)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(repositoryRoot);
        var fullRoot = Path.GetFullPath(repositoryRoot);
        var issues = ImmutableArray.CreateBuilder<ValidationIssue>();
        foreach (var relativePath in ProductionProjects)
        {
            var path = Path.Combine(fullRoot, relativePath.Replace('/', Path.DirectorySeparatorChar));
            if (!File.Exists(path))
            {
                issues.Add(new("production-project", relativePath, "The required production project does not exist."));
                continue;
            }
            try
            {
                var project = XDocument.Load(path, LoadOptions.PreserveWhitespace);
                foreach (var element in project.Descendants())
                {
                    var dependency = DependencyIdentity(element);
                    if (dependency is null
                        || string.Equals(dependency, "FSharp.Core", StringComparison.OrdinalIgnoreCase))
                    {
                        continue;
                    }
                    if (dependency.Contains("FSharp.Compiler.Service", StringComparison.OrdinalIgnoreCase))
                    {
                        issues.Add(new(
                            "production-fcs-dependency",
                            relativePath,
                            $"Production dependency '{dependency}' includes FSharp.Compiler.Service."));
                    }
                    if (dependency.Contains("Oracle", StringComparison.OrdinalIgnoreCase))
                    {
                        issues.Add(new(
                            "production-oracle-dependency",
                            relativePath,
                            $"Production dependency '{dependency}' includes an Oracle project or executable."));
                    }
                    if (dependency.Contains("fsc.dll", StringComparison.OrdinalIgnoreCase))
                    {
                        issues.Add(new(
                            "production-fsc-dependency",
                            relativePath,
                            $"Production dependency '{dependency}' includes SDK fsc.dll."));
                    }
                }
            }
            catch (Exception exception) when (exception is IOException or System.Xml.XmlException)
            {
                issues.Add(new("production-project", relativePath, $"The production project could not be read: {exception.Message}"));
            }
        }
        return new ValidationResult(issues.Count == 0, issues.ToImmutable());
    }

    private static string? DependencyIdentity(XElement element)
    {
        if (element.Name.LocalName is not ("PackageReference" or "ProjectReference" or "Reference"))
        {
            return null;
        }
        return element.Attribute("Include")?.Value
               ?? element.Attribute("Update")?.Value
               ?? element.Value;
    }
}
