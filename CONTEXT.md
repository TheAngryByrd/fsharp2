# F# Compiler Replacement

This context describes an independently implemented F# compiler that can replace the compiler target used by existing MSBuild projects.

## Language

**FSharp2**:
The independently implemented F# batch compiler developed in this repository.
_Avoid_: New compiler, alternative fsc

**Compiler Target Invocation**:
The compilation work requested by one evaluated project and target framework through the replacement MSBuild target.
_Avoid_: Whole build, MSBuild time

**Cold Compilation**:
A Compiler Target Invocation with no reusable compiler state from an earlier invocation.
_Avoid_: Clean solution build

**Warm Compilation**:
A Compiler Target Invocation that follows an earlier compilation and may reuse still-valid compiler state.
_Avoid_: Cached build

**Behavioral Compatibility**:
Equivalence in the compiler behavior and build artifacts observable by source programs, build tooling, debuggers, and consumers, without requiring byte-identical output.
_Avoid_: Binary identity

**FS Diagnostic Compatibility**:
Behavioral equivalence across the complete `FSxxxx` error and warning surface of the Compatibility Oracle, including occurrence, code, effective severity, message, source range, ordering, stream, suppression, promotion, and exit behavior.
_Avoid_: Representative diagnostics, error-code coverage, similar messages

**Compatibility Oracle**:
The compiler shipped in the latest .NET 10 SDK, whose observable behavior resolves compatibility questions not settled by the written F# specification.
_Avoid_: Reference implementation

**Provider Broker**:
The optional managed build-time sidecar that executes legacy F# type providers for FSharp2 while the NativeAOT compiler retains all compiler semantics, diagnostics, incremental state, and artifact ownership.
_Avoid_: Managed compiler fallback, type-provider sandbox

**Experimental Vertical Milestone**:
An opt-in release that performs the complete compilation path for a declared compatibility envelope that is narrower than the final destination.
_Avoid_: Partial compiler

**IcedTasks Compatibility Corpus**:
The pinned IcedTasks library source and unchanged tests used as FSharp2's first real-world corpus. Compatibility Oracle and FSharp2 producer lanes differ only in compiler selection; harness-owned probes fill declared coverage gaps, and reversible source edits exist only as isolated incremental-compilation inputs.
_Avoid_: IcedTasks fixture, reduced IcedTasks slice, patched IcedTasks

**Compatibility Gate**:
The evidence threshold that must be met before the replacement compiler target is presented as drop-in compatible rather than experimental.
_Avoid_: Feature complete

**Performance Tripwire**:
The three-second Compiler Target Invocation threshold beyond which the compiler must explain its critical path and resource costs.
_Avoid_: Compilation timeout

**Linux Deployment Floor**:
The versioned conjunction of distro line, architecture, libc ABI, loader/native-dependency manifest, and native clean-image execution that a shipped FSharp2 Linux host must satisfy.
_Avoid_: Linux kernel minimum, RID support
