# Minimum end-to-end FSharp2 prototype result

Status: measured prototype result, 2026-07-19

## Question

Does the issue #8 tracer prove the complete pinned IcedTasks Compatibility Corpus, both NativeAOT gates, and the Compiler Target Invocation budget end to end?

## Verdict

No. The tracer proves several architecture seams, but it does not compile the IcedTasks Compatibility Corpus. All four FSharp2 producer cases fail closed on the first SDK-generated source input. The failure occurs before FSharp2 parses any of the 17 authored IcedTasks files. The failed cases do not produce valid Cold Compilation or Warm Compilation samples.

The prototype does prove these narrower results on Windows x64:

- The NuGet-delivered MSBuild integration selects the packaged FSharp2 compiler host for a real `CoreCompile` invocation.
- The NativeAOT compiler host starts and reports a FSharp2-owned diagnostic without invoking the Compatibility Oracle.
- The host reports 18 evaluated source inputs for each IcedTasks target framework and reports reference cardinalities of 165, 160, 122, and 117 for `net9.0`, `net6.0`, `netstandard2.1`, and `netstandard2.0`, respectively.
- The symbolic-fragment and fresh-linker path emits a synthetic DLL and portable PDB.
- The .NET NativeAOT toolchain accepts the synthetic FSharp2-emitted DLL, and the published native consumer runs it.
- The retained compiler service reuses synthetic query state in the existing repository tests.
- The existing repository tests compare the occurrence, code, error severity, message, source range, ordering, stream, exit behavior, and artifact-publication behavior of the `FS0010` and `FS0001` milestone probes under their tested compiler flags. The tests do not prove family-wide suppression or promotion behavior.

These results do not prove Behavioral Compatibility for the complete pinned IcedTasks Compatibility Corpus. They also do not prove the IcedTasks performance gates, the required warm-edit behavior, or complete FS Diagnostic Compatibility.

## Result by issue #8 seam

| Seam named by issue #8 | Measured disposition |
| --- | --- |
| Host | Proven for the packaged Windows x64 NativeAOT host. The complete host RID matrix is not proven. |
| Frontend | Failed on the dot in `namespace Microsoft.BuildSettings` in the first SDK-generated input. FSharp2 did not reach authored IcedTasks source. |
| Typed representation | Proven only for the synthetic integer-declaration model. The IcedTasks producer cases did not reach type checking. |
| Optimizer seam | Not implemented as an optimization pass. The command line accepts `--optimize`, but the prototype does not apply or trace an optimization decision. |
| Emitter | Proven for the synthetic DLL, reference DLL, portable PDB, SourceLink, resource, signing, and deterministic-output probes in the repository tests. No IcedTasks artifact exists. |
| Differential harness | The Oracle lane and four fail-closed FSharp2 producer attempts ran on the same isolated project. The probe did not compare candidate artifacts because FSharp2 emitted none. The full versioned differential harness is not implemented. |
| NativeAOT publication | The Windows x64 compiler-host gate and the synthetic emitted-output gate passed. The IcedTasks emitted-output gate and the complete RID matrices remain unproven. |
| Compiler Target Invocation budget | Not measured. All candidate attempts failed correctness admission, so no duration is a valid Cold Compilation or Warm Compilation sample. |

## Evidence identity

| Input | Value |
| --- | --- |
| FSharp2 commit | `ea26599` |
| IcedTasks commit | `ba4e932b71bfde354f0e2561c2519b282fe56ff9` |
| IcedTasks tree | `20abf5280d4630db438c7c6f06838105df12c765` |
| .NET SDK | `10.0.110` |
| Host cell | Windows x64 |
| FSharp2 package | `FSharp2.Compiler.MSBuild` version `0.0.0-issue8` |

The probe used an isolated clone of the pinned IcedTasks commit. The probe changed only `Directory.Build.props` and `Directory.Packages.props` to select the local FSharp2 compiler package. `git status --short` reported only those two files. The probe did not change IcedTasks product source, project source, or tests.

## Repository baseline

The repository baseline passed before the corpus probe:

```text
dotnet build fsharp2.sln -c Release --no-restore -v minimal
Build succeeded. 0 Warning(s), 0 Error(s).

dotnet test tests\fsharp2.Tests\fsharp2.Tests.fsproj -c Release --no-build --no-restore -v minimal
Passed: 17, Failed: 0, Skipped: 0.
```

The 17 tests cover the synthetic compiler target, artifact, diagnostic, retained-service, and MSBuild integration seams. They do not compile the pinned IcedTasks corpus.

## IcedTasks differential result

The Compatibility Oracle built the same isolated project for all four producer target frameworks:

```text
dotnet build src\IcedTasks\IcedTasks.fsproj -c Release --no-restore -v minimal -p:UseFSharp2Compiler=false
Build succeeded. 0 Warning(s), 0 Error(s).
```

The FSharp2 lane failed for each target framework:

| Target framework | Physical sources | References | First FSharp2 diagnostic | Emitted |
| --- | ---: | ---: | --- | --- |
| `net9.0` | 18 | 165 | `.NETCoreApp,Version=v9.0.AssemblyAttributes.fs(1,20): error FSC2P1001: unsupported token` | `false` |
| `net6.0` | 18 | 160 | `.NETCoreApp,Version=v6.0.AssemblyAttributes.fs(1,20): error FSC2P1001: unsupported token` | `false` |
| `netstandard2.1` | 18 | 122 | `.NETStandard,Version=v2.1.AssemblyAttributes.fs(1,20): error FSC2P1001: unsupported token` | `false` |
| `netstandard2.0` | 18 | 117 | `.NETStandard,Version=v2.0.AssemblyAttributes.fs(1,20): error FSC2P1001: unsupported token` | `false` |

The 18 physical sources are one SDK-generated target-framework attribute file plus the 17 authored IcedTasks files. Each trace recorded `invalidationReason=bypass`, `parse=bypass`, `check=bypass`, `lower=bypass`, and `emitted=false`. The prototype tokenizer rejects the dot in the qualified namespace `Microsoft.BuildSettings` on line 1 of the generated first input. The parser therefore does not reach the target-framework attribute on line 2.

The FSharp2 failures occurred in less than the Performance Tripwire, but those durations are not performance evidence. Correctness and successful artifact checks are admission requirements. No FSharp2 producer artifact exists for downstream tests, the IcedTasks emitted-output NativeAOT gate, or either mandatory Warm edit.

## NativeAOT results

### Compiler host gate

`dotnet pack` published the compiler host with `PublishAot=true` and produced a 3,841,536-byte `tools/win-x64/fsc2.exe`. The four IcedTasks `CoreCompile` requests executed that packaged host. Each request produced a FSharp2 trace with a distinct native process identifier and the fail-closed `FSC2P1001` result.

This result passes the Windows x64 host publication-and-execution check for the prototype. It does not pass the complete host RID matrix.

### Emitted-output gate

The packaged NativeAOT host compiled the repository's synthetic `Tracer.fs` input. The probe then published `FSharp2.Prototype.NativeConsumer` with `PublishAot=true` against the emitted `Tracer.dll`. The native executable exited with code `0` and wrote `42`.

This result proves that the .NET NativeAOT toolchain accepts the synthetic output envelope. It does not prove the IcedTasks emitted-output gate because FSharp2 did not produce an IcedTasks assembly.

## Architecture consequences

Keep these validated seams:

- Keep the opt-in MSBuild package and the real `CoreCompile` boundary.
- Keep the statically closed NativeAOT host and the retained service protocol.
- Keep immutable symbolic emission fragments and a fresh public-SRM link for every successful request.
- Keep transactional artifact publication and explicit failure for unsupported input.
- Keep the Compatibility Oracle outside the production compiler graph.

Replace or deepen these prototype parts:

1. Replace the one-declaration tokenizer and parser with an ordered syntax model. The next Experimental Vertical Milestone must parse SDK-generated attributes, namespaces, modules, attributes, and multiple declarations before it can enter authored IcedTasks code.
2. Add the metadata-only reference index, name resolution, inference, constraint solving, and typed declaration checkpoints required by the complete IcedTasks source.
3. Create a line-addressed language-construct inventory for the pinned IcedTasks tree before lowering work. Use that inventory to define the lowering requirements. The known requirements include object and member declarations, generics, computation expressions, resumable state machines, exception handling, and closures.
4. Separate exported, implementation, debug, and diagnostic fingerprints. Track declaration dependencies and F# source-order checkpoints explicitly.
5. Retain compiler state in the NativeAOT service and use content-addressed semantic query keys. Do not treat a daemon, an unchanged MSBuild skip, or a whole-project cache hit as Warm Compilation.
6. Admit performance samples only after correctness, artifact, no-fallback, state-lineage, and evidence checks pass. The first governed runner cell must report the current candidate as a correctness failure with no valid timing sample.
7. Keep `FS0010` and `FS0001` as milestone probes. Expand to the complete `FSxxxx` error-and-warning surface only through the final family-wide FS Diagnostic Compatibility gate.

These consequences apply the comparative compiler findings in [Fast compiler design lessons for FSharp2](fast-fsharp-compiler-cold-and-incremental.md). In particular, the next frontend and semantic work must preserve declaration-level query identities, exported semantic fingerprints, explicit invalidation, and a fresh final link. File timestamps and process persistence are not sufficient evidence of safe reuse.

## Issue #8 answer

The issue #8 prototype question has a negative answer for the complete pinned IcedTasks Compatibility Corpus. The current tracer validates the host and MSBuild selection path, synthetic retained-service behavior, failure-trace plumbing, symbolic emission for the synthetic envelope, and the two synthetic NativeAOT checks. It does not validate the governed cache-evidence trace or provide Behavioral Compatibility for the declared IcedTasks envelope.

The negative result resolves the prototype question, but it does not satisfy the first Experimental Vertical Milestone or the issue #1 destination. Follow-on implementation must close the frontend, semantic, lowering, artifact, differential, and performance gaps above.
