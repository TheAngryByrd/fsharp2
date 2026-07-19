# Cross-platform debugger automation driver for FSharp2

Status: decision research for [Choose the cross-platform debugger automation driver][issue-18], supporting the debugger-data gate in the [differential compatibility and performance harness](differential-compatibility-performance-harness.md)

Observed: 2026-07-19

Governing decisions: [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0006](../adr/0006-require-behavioral-not-byte-identical-output.md), [ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md), [ADR 0021](../adr/0021-use-public-srm-behind-symbolic-emission-fragments.md), and [ADR 0027](../adr/0027-use-dap-with-a-gated-netcoredbg-driver.md)

## Answer

Use the **Debug Adapter Protocol (DAP)** as the versioned harness protocol, pinned to schema commit `e34479c39ed4973210115872c8e118c097a50d4a`. Use Samsung `netcoredbg` `3.2.0-1092`, source commit `9744e1f051866215611b8440c638042aa2aa2f72`, as the first redistributable driver candidate.

That candidate is not accepted merely because it starts or negotiates DAP. Each OS/RID/runtime cell must first pass an F#-specific driver-conformance probe covering ordinary functions, lambdas/closures, computation expressions, async/task state machines, breakpoint binding, stepping, source identity, locals, and stack frames. A failed or missing probe is `infra-error`; it cannot become a FSharp2 compatibility failure, a pass, or an omitted matrix cell.

This gate is necessary because the pinned driver has a documented F# computation-expression breakpoint defect: breakpoints may slide or fail when one source method maps to multiple generated methods. The release also lacks official macOS x64, Windows Arm64, and musl assets, and `netcoredbg` does not implement `sourceFileMap` or exact-source enforcement. The final Compatibility Gate therefore cannot claim complete cross-platform debugger coverage until those cells have a hash-pinned, notice-complete driver that passes the same probe. This may be a reproducibly locked FSharp2-owned build or a later reviewed replacement; silently building the current floating source graph is not sufficient.

`vsdbg` is technically stronger but is not an acceptable standalone harness dependency: Microsoft publishes it under a proprietary license restricted to Microsoft IDE use and forbids redistributing it as a separate offering. MIEngine/OpenDebugAD7 and `lldb-dap` do not provide primary-source evidence of managed F# Portable PDB behavior. They are not substitutes for a passing managed-driver probe.

## Why DAP

The pinned DAP contract exposes the complete observation sequence required by issue #18:

1. `initialize` negotiates capabilities and line/column/path conventions.
2. `launch` or `attach` starts the declared debuggee.
3. `setBreakpoints` returns whether each requested breakpoint was verified and its actual source location.
4. `configurationDone` releases startup once configuration is complete.
5. `stopped` establishes a stopped-state observation boundary.
6. `threads`, `stackTrace`, `scopes`, and `variables` expose the stack and locals while references are valid.
7. `next`, `stepIn`, `stepOut`, and `continue` resume execution and invalidate stopped-state handles.
8. `disconnect` terminates or detaches according to the case contract.

The harness sends only one mutating request at a time, awaits its response and the expected event transition, captures all stopped-state observations before resuming, and applies a timeout to every transition. DAP sequence ids, process ids, thread ids, frame ids, and `variablesReference` values are correlation handles, not cross-run identities.

Primary contract: [pinned DAP schema][dap-schema] and [pinned DAP lifecycle/stack/variable overview][dap-overview].

## Pinned driver and supply-chain evidence

Pin both the release identity and source identity:

- release: [`3.2.0-1092`][netcoredbg-release], GitHub release API id `344761429`;
- source commit: [`9744e1f051866215611b8440c638042aa2aa2f72`][netcoredbg-commit];
- license: MIT, with redistribution notice required; and
- protocol mode: `netcoredbg --interpreter=vscode`.

The official release assets observed on 2026-07-19 were:

| Harness cell | Asset | Bytes | SHA-256 |
| --- | --- | ---: | --- |
| Linux x64, glibc | `netcoredbg-linux-amd64.tar.gz` | 3,510,717 | `080eb3b2d2152465f599d3b33d1ee6e747794e11cc0a3773ec689f5e5f2c5afa` |
| Linux Arm64, glibc | `netcoredbg-linux-arm64.tar.gz` | 3,424,271 | `065ff49badec8a695dbea2de6ab6a330c774a191e426a217ab8cc05250627ccb` |
| macOS Arm64 | `netcoredbg-osx-arm64.zip` | 3,389,504 | `f4fa33b3ff874910cc184b4bb3b9c56d0abdf5c6521cee0b144d7c6e4a6e59ea` |
| Windows x64 | `netcoredbg-win64.zip` | 3,524,161 | `3c410a45fa502415203a94fcb88654af65bf8e3dac158a5527a722e7a6b9274a` |

The release is not marked immutable. Harness bootstrap therefore never downloads an unqualified tag into a run. Mirror the reviewed bytes into a controlled tool store keyed by SHA-256, verify length and digest before extraction, record the extracted-file manifest, and fail closed on any mismatch. The archives include `netcoredbg`, its managed component, `dbgshim`, and Roslyn assemblies but omit the root license file; any retained or redistributed mirror must add the pinned MIT license and audited third-party notices.

The release does not publish macOS x64, Windows Arm64, or musl assets. The macOS Arm64 build is documented as community-supported and potentially failing. Those facts are explicit matrix gaps rather than inferred support.

Do not treat an ad hoc source build as an equivalent pin. At the selected commit, the build follows a moving .NET 10 branch/channel and contains floating NuGet and DbgShim inputs. A FSharp2-owned build becomes eligible only when its SDK, source submodules, packages, native toolchain/sysroot, runtime/debug-shim inputs, patches, notices, output manifest, and hashes are locked and the resulting artifact passes the same probe on real target hardware.

Primary evidence: [MIT license][netcoredbg-license], [build inputs][netcoredbg-cmake], [managed dependency inputs][netcoredbg-managed-project], and [platform support statement][netcoredbg-platforms].

## Required driver-conformance probe

The driver probe is independent of Oracle-versus-FSharp2 comparison. It proves that the observation instrument can see the behavior the case asks it to compare.

Each declared driver cell compiles and debugs a small pinned F# program containing:

- a module function with parameters and locals;
- a lambda that captures at least one local;
- an F# record, discriminated union, list, tuple, option, and exception value;
- `async` and `task` computation expressions with a real suspension/resumption point;
- a nested function and at least one source declaration that lowers to multiple methods; and
- path-mapped source plus a separate attach-ready process.

The probe must demonstrate:

1. launch and attach both reach the requested program;
2. every breakpoint response is `verified` and reports the expected canonical document and actual line;
3. the stopped event is attributable to that breakpoint by the normalized top frame, because the pinned driver does not return `hitBreakpointIds`;
4. step-in, step-over, and step-out visit the expected normalized source sequence without hanging or silently continuing;
5. stack frames preserve declared F# source documents, ranges, and the expected semantic call chain;
6. scopes and locals expose the required source values with bounded, cycle-safe variable expansion;
7. repeated runs produce the same normalized transcript;
8. the probe works without network access after tool/corpus materialization; and
9. all requested processes exit and the run-scoped process tree is empty.

The known computation-expression defect is the decisive candidate gate, not a waived difference. Issue `Samsung/netcoredbg#221` records missed or displaced F# computation-expression breakpoints, and the implementation still contains constructor-specific assumptions where F# source can map to multiple generated methods. Until the exact pinned driver passes the probe, its result for that platform/runtime is `infra-error` and no Portable PDB behavioral verdict is issued.

Primary evidence: [F# breakpoint defect][netcoredbg-fsharp-breakpoints], [maintainer analysis][netcoredbg-fsharp-analysis], and [pinned method-resolution code][netcoredbg-method-resolution].

## Portable PDB and source contract

`netcoredbg` has first-party implementation evidence for external, in-memory, and embedded Portable PDB reading, PE/PDB identity checks, local scopes, and async metadata. This makes it a plausible behavioral observer rather than the PDB authority. The harness-owned SRM comparator from issue #5 remains responsible for canonical document, sequence-point, local/import-scope, SourceLink, and custom-debug-information evidence.

For each debugger case:

1. Build Oracle and FSharp2 outputs from isolated producer roots with the same `PathMap` target.
2. Materialize a read-only, hash-verified debug source tree at that exact mapped path before launching the driver.
3. Independently verify each Portable PDB document checksum against the materialized bytes.
4. Reject a missing, mismatched, or unmappable source before starting the debug session.
5. Pass only artifact paths, program arguments, working directory, and allowed environment to the driver.

The pinned driver has no `sourceFileMap` implementation and no `requireExactSource` equivalent. The common mapped source tree and harness checksum check are therefore required controls, not optional normalization. A source mismatch is `infra-error`; the harness never lets a debugger silently display different source.

Primary evidence: [pinned Portable PDB symbol reader][netcoredbg-symbol-reader], [`sourceFileMap` limitation][netcoredbg-source-map], and [official F# `--pathmap` option][fsharp-pathmap].

## Versioned observation schema

Store the raw DAP byte stream and a canonical semantic projection. A `debug-session-v1` record contains:

| Section | Required evidence |
| --- | --- |
| Identity | case/run/attempt ids; Oracle or FSharp2 producer; source/artifact hashes; DAP schema commit; driver release/source/asset hashes; runtime identity; OS/RID/architecture. |
| Bootstrap | verified mirror entry; extraction manifest; executable and dependency hashes; license/notice ids; offline flag. |
| Request | launch/attach mode; program, arguments, working directory, environment allowlist; requested breakpoints; timeout policy. |
| Source | raw and normalized document paths; PDB checksum algorithm/value; source-byte hash; PathMap rule id. |
| Transcript | raw framed DAP input/output with timestamps plus request/response/event correlation. |
| Breakpoint | requested and actual document/line/column; verified/message; normalized top-frame attribution. |
| Stop | semantic reason; normalized thread role; normalized stack; source range; instruction address retained only as audit data. |
| Variables | scope kind; source variable path; display name, declared/runtime type, normalized value; presentation hints; bounded child projection. |
| Step | operation, starting location, ending location, intervening events, timeout/termination result. |
| Cleanup | disconnect result, debuggee/driver exits, descendant process inventory, leftover-file manifest. |
| Verdict | driver-probe verdict, debugger-comparison verdict, failure class, missing evidence, policy rule ids. |

Normalize run-specific DAP sequence ids, PIDs, thread/frame/reference handles, timestamps, randomized ports/pipes, and physical roots. Preserve raw values in the restricted evidence bundle. Do not normalize away source documents/ranges, breakpoint verification, stop reasons, semantic frame ordering, local names/types/values, thrown exception identity, or termination behavior.

Generated frame and variable names may differ between compilers without being user-visible source concepts. Any projection from generated names to semantic F# frames/locals is a versioned comparator rule backed by PDB/source evidence; a new unmapped shape fails closed. The Compatibility Oracle output establishes expected observable behavior, while the driver probe establishes that the observer itself is capable.

## Verdict and failure taxonomy

The driver layer reports separately from compiler compatibility:

| Driver result | Meaning |
| --- | --- |
| `pass` | The pinned driver/protocol/tool asset completed the required F# probe and produced complete normalized evidence. |
| `fail` | The probe executed and demonstrated a driver defect or nondeterministic observation. This blocks the matrix cell but is not charged to FSharp2. |
| `infra-error` | Driver/tool/source/runtime setup, missing asset, timeout, crash, checksum, cleanup, or evidence completeness prevented the probe. |

Only a driver `pass` permits the harness to issue its existing `pass`/`fail`/`unsupported`/`infra-error` compiler verdict for debugger behavior. Compiler differences are then classified as `pdb-structure`, `breakpoint-binding`, `source-mapping`, `stepping`, `stack`, `locals`, `exception`, or `termination`. A driver regression never becomes an intentional compiler difference.

## Platform and CI policy

Run a short driver probe before debugger comparison on every worker image/runtime combination. Cache a passing result only by the complete tuple of driver asset hash, extracted dependency manifest, runtime hash, OS image, RID/architecture, kernel/libc identity where applicable, and probe hash.

The current candidate matrix is:

| Platform | Candidate state |
| --- | --- |
| Windows x64 | Official asset exists; exact F# probe required. |
| Linux glibc x64 | Official asset exists; exact F# probe required. |
| Linux glibc Arm64 | Official asset exists; exact F# probe required on real hardware. |
| macOS Arm64 | Official community-supported asset exists; exact F# probe required on real hardware. |
| Windows Arm64 | No official asset; final cell missing. |
| macOS x64 | No official asset; final cell missing. |
| Linux musl x64/Arm64 | No supported asset; final cells missing and upstream musl support remains unresolved. |

Ordinary pull requests may run the available x64 smoke cells. Nightly/release runs execute every declared eligible real-hardware cell. Release cannot claim the final debugger matrix while a required cell is missing or its probe is not `pass`.

## Security and isolation

The debugger can read target memory and control execution, so treat it as privileged test infrastructure:

- run only hash-pinned harness-owned debuggees in a disposable worker/job sandbox;
- deny network after inputs are materialized;
- use a minimal environment allowlist and no inherited user secrets;
- never attach to an arbitrary or pre-existing process;
- scope launch/attach by run-owned PID and executable hash;
- bound variable recursion, string/collection lengths, output size, and session time;
- kill only the verified run-scoped process tree after the graceful deadline;
- retain raw transcripts under the secure evidence policy from issue #16; and
- export only the allowlisted normalized semantic projection.

The driver, DAP transcript, and debuggee are test-only. They never enter the NativeAOT compiler graph, emitted artifact, NuGet integration package, compiler service, or cache schema.

## Relationship to cold and incremental compilation

The [fast compiler research](fast-fsharp-compiler-cold-and-incremental.md) affects this decision only at the boundary: debugger automation must not lengthen a Compiler Target Invocation or become a dependency of the persistent compiler service. FSharp2 still emits the symbolic debug contributions and Portable PDB required by ADR 0021; the harness runs the debugger afterward against immutable artifacts.

Debugger time, launch cost, stopped-state inspection, and transcript normalization are recorded as harness time, never cold/warm compiler time. The debugger may validate baseline and edited artifacts, but it cannot prove semantic cache reuse, invalidation, or sub-second Warm Compilation. Conversely, an incremental edit that reuses an incorrect stale PDB must fail the ordinary PDB/debugger comparison even if the compiler timing gate passes.

## Alternatives considered

### Microsoft `vsdbg`

`vsdbg` supports the required managed-debugger behavior and a broader platform surface, but its published license permits use only with Microsoft Visual Studio products and forbids offering the debugger separately. A standalone open-source FSharp2 harness cannot redistribute or automate it as its general CI driver. Developers may use it interactively outside authoritative evidence, but such runs do not satisfy this contract.

Primary evidence: [Microsoft debugger licensing notice][vsdbg-notice] and [pinned runtime license][vsdbg-license].

### MIEngine/OpenDebugAD7 and `lldb-dap`

These projects expose debug-adapter or MI infrastructure around native GDB/LLDB scenarios. The reviewed primary sources do not establish managed Portable PDB/F# breakpoint, locals, async, or stack behavior. Adding one of them in front of a managed engine would add another moving layer without solving the managed-driver requirement.

### Harness-owned `ICorDebug` driver

An independently authored test-only driver over pinned runtime debugging interfaces could eventually cover the missing RIDs and avoid third-party breakpoint policy. It would also have to own process control, Portable PDB-to-IL mapping, stepping, value inspection, exception behavior, runtime/version support, and native interop across every platform. That is a new implementation project, not a smaller dependency choice. Revisit it if the netcoredbg conformance/matrix gaps cannot be closed with a reproducibly locked, notice-complete build.

## Implementation order

1. Check in `debug-session-v1`, transcript framing, normalization, and deliberate comparator mutations.
2. Materialize the four reviewed driver assets through a digest-addressed offline tool store with notices.
3. Implement the standalone F# driver-conformance probe and run it on each published-asset cell.
4. Treat the computation-expression breakpoint result as a go/no-go gate for the candidate.
5. Add Oracle-versus-Oracle debugger self-tests before comparing FSharp2.
6. Add baseline, lambda, async/task, exception, source-map, launch, and attach cases.
7. Add the IcedTasks Compatibility Corpus only after the driver passes the matching F# probe.
8. Close missing final platform cells with reproducibly locked artifacts or a reviewed replacement; do not infer them from source portability.

## Primary-source index

- [Pinned DAP schema][dap-schema]
- [Pinned DAP lifecycle and stopped-state queries][dap-overview]
- [Pinned netcoredbg release][netcoredbg-release] and [source commit][netcoredbg-commit]
- [Pinned netcoredbg DAP request implementation][netcoredbg-protocol]
- [Pinned Portable PDB symbol reader][netcoredbg-symbol-reader]
- [Pinned netcoredbg MIT license][netcoredbg-license]
- [Pinned platform statement][netcoredbg-platforms]
- [F# computation-expression breakpoint defect][netcoredbg-fsharp-breakpoints]
- [`sourceFileMap` limitation][netcoredbg-source-map]
- [Microsoft `vsdbg` licensing notice][vsdbg-notice] and [runtime license][vsdbg-license]

[issue-18]: https://github.com/TheAngryByrd/fsharp2/issues/18
[dap-schema]: https://github.com/microsoft/debug-adapter-protocol/blob/e34479c39ed4973210115872c8e118c097a50d4a/debugAdapterProtocol.json
[dap-overview]: https://github.com/microsoft/debug-adapter-protocol/blob/e34479c39ed4973210115872c8e118c097a50d4a/overview.md#L110-L219
[netcoredbg-release]: https://github.com/Samsung/netcoredbg/releases/tag/3.2.0-1092
[netcoredbg-commit]: https://github.com/Samsung/netcoredbg/commit/9744e1f051866215611b8440c638042aa2aa2f72
[netcoredbg-protocol]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/src/protocols/vscodeprotocol.cpp#L484-L718
[netcoredbg-symbol-reader]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/src/managed/SymbolReader.cs#L1131-L1320
[netcoredbg-license]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/LICENSE#L1-L13
[netcoredbg-cmake]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/CMakeLists.txt#L10-L13
[netcoredbg-managed-project]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/src/managed/ManagedPart.csproj#L29-L42
[netcoredbg-platforms]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/README.md#L107-L114
[netcoredbg-fsharp-breakpoints]: https://github.com/Samsung/netcoredbg/issues/221
[netcoredbg-fsharp-analysis]: https://github.com/Samsung/netcoredbg/issues/221#issuecomment-4184832575
[netcoredbg-method-resolution]: https://github.com/Samsung/netcoredbg/blob/9744e1f051866215611b8440c638042aa2aa2f72/src/metadata/modules_sources.cpp#L194-L203
[netcoredbg-source-map]: https://github.com/Samsung/netcoredbg/issues/78#issuecomment-991565053
[fsharp-pathmap]: https://github.com/dotnet/fsharp/blob/5928e91b5f701586690562ce10bd639357fff50b/src/Compiler/Driver/CompilerOptions.fs#L591-L594
[vsdbg-notice]: https://github.com/dotnet/vscode-csharp/blob/b645dec498f3639c6bbe7be654e1cc88044f51fe/docs/debugger/Microsoft-.NET-Core-Debugger-licensing-and-Microsoft-Visual-Studio-Code.md#L3-L7
[vsdbg-license]: https://github.com/dotnet/vscode-csharp/blob/b645dec498f3639c6bbe7be654e1cc88044f51fe/RuntimeLicenses/license.txt#L9-L13
