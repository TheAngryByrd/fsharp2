# Preserve compatible optimization modes

The compiler will honor the existing F# optimization, debugging, tail-call, and related build switches rather than applying an aggressive optimizer unconditionally. New optimizations must preserve the compatible default behavior and earn inclusion through differential correctness plus compile-time and generated-code benchmarks.
