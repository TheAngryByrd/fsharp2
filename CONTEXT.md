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

**Compatibility Oracle**:
The compiler shipped in the latest .NET 10 SDK, whose observable behavior resolves compatibility questions not settled by the written F# specification.
_Avoid_: Reference implementation

**Experimental Vertical Milestone**:
An opt-in release that performs the complete compilation path for a declared compatibility envelope that is narrower than the final destination.
_Avoid_: Partial compiler

**Compatibility Gate**:
The evidence threshold that must be met before the replacement compiler target is presented as drop-in compatible rather than experimental.
_Avoid_: Feature complete

**Performance Tripwire**:
The three-second Compiler Target Invocation threshold beyond which the compiler must explain its critical path and resource costs.
_Avoid_: Compilation timeout
