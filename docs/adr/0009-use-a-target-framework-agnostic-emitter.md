# Use a target-framework-agnostic emitter

The compiler host runs on modern .NET, but output targeting is driven by the reference assemblies and options supplied to a Compiler Target Invocation. The first vertical milestone covers IcedTasks' `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0` targets; the final compatibility gate expands to the broader target matrix accepted by the official compiler.
