using System.Collections.Immutable;

namespace FSharp2.Conformance;

public static class ProcessLineage
{
    public static ImmutableArray<ProcessObservation> Read(ProcessResult result)
    {
        ArgumentNullException.ThrowIfNull(result);
        return result.Processes.IsDefault ? [] : [.. result.Processes];
    }
}
