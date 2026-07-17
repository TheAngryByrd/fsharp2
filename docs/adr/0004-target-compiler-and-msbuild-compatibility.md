# Target compiler and MSBuild compatibility

The core compatibility destination is a drop-in replacement for the F# compiler as invoked by MSBuild: task inputs, command-line options, diagnostics, exit behavior, and emitted build artifacts must be compatible. Reproducing the public `FSharp.Compiler.Service` API and its type identities is outside this core destination.
