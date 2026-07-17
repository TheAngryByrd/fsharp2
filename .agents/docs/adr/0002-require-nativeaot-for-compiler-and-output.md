# Require NativeAOT compatibility for the compiler and its output

NativeAOT compatibility applies on both sides of compilation: the compiler host must publish and execute as a NativeAOT binary, and the assemblies it emits must remain valid inputs to the .NET NativeAOT toolchain. Both capabilities are release gates rather than later optimizations.
