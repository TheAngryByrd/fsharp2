# Build an independent source-informed F# compiler

The compiler will be independently authored rather than forked from or built by copying the official `dotnet/fsharp` implementation. Contributors may study that implementation to understand observable behavior and compatibility requirements, while all production code in this repository must be newly written.

ADR 0028 amends this decision: FSharp2 can vendor third-party source under a compatible license, and production code stays independent of `dotnet/fsharp`.
