# Use F# with benchmark-justified C# kernels

The semantic compiler pipeline will be authored primarily in F#. Narrow C# kernels may implement span- and ref-struct-heavy parsing, pooled storage, hashing, or metadata emission only when benchmarks demonstrate a material advantage; language boundaries are not introduced speculatively.
