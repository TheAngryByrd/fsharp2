using System.Collections.Immutable;
using System.Text;

namespace FSharp2.Conformance;

public static class DeterminismComparator
{
    public static ComparisonResult Compare(
        ImmutableArray<byte> first,
        ImmutableArray<byte> second)
    {
        var passed = first.AsSpan().SequenceEqual(second.AsSpan());
        var firstHash = Hashing.Sha256(first.AsSpan());
        var secondHash = Hashing.Sha256(second.AsSpan());
        return ComparisonSupport.Create(
            "artifact-repeat",
            "within-compiler-exact",
            "repeat-v1",
            passed,
            passed ? null : ComparisonSupport.HashDifference(first.AsSpan(), second.AsSpan()),
            first.AsSpan(),
            second.AsSpan(),
            Encoding.UTF8.GetBytes(firstHash),
            Encoding.UTF8.GetBytes(secondHash));
    }
}
