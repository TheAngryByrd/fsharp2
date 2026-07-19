# Warm-edit corpus and cache-evidence contract

Status: issue [#19](https://github.com/TheAngryByrd/fsharp2/issues/19) research decision, 2026-07-18

Corpus: IcedTasks commit `ba4e932b71bfde354f0e2561c2519b282fe56ff9`, tree `20abf5280d4630db438c7c6f06838105df12c765`

Governing decisions: [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), [ADR 0016](../adr/0016-explain-slow-compilations-with-structured-traces.md), [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md), and [ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md)

## Decision

Use two line-count-preserving edits to the pinned IcedTasks source in isolated, hash-verified materializations:

1. `AsyncEx.AwaitTask(Task<'T>)` is the implementation-only case. It changes the emitted method body while preserving its managed and F# exported meaning and its returned value.
2. `ParallelAsyncBuilderBase.BindReturn` is the consumer-visible inline case. Its managed signature remains stable, but its public inline body and F# optimization payload change.

The corpus is not merely two patches: forced compiler-contact/no-op, identical replay, exact baseline restore, diagnostic-failure recovery, and unknown-impact widening are mandatory companion controls. A measured sample primes once and then applies its edit immediately; no control invocation may warm extra state before the timed `CoreCompile`, and replay hits or MSBuild skips cannot enter the Warm Compilation timing distribution.

Both edits add a `GC.KeepAlive` lifetime marker to an object already retained by the returned `Async`. This gives the harness an emitted call which the optimizer cannot erase as a source-only no-op, without using APIs missing from `netstandard2.0` or changing the observed result. `GC.KeepAlive(Object)` is present in the pinned `netstandard2.0` reference assembly and is documented as extending object liveness to the call site.[^keep-alive]

The Warm Compilation gate primes one compiler service, applies exactly one edit, then performs a real `CoreCompile` through the same healthy service PID and cache epoch. It compares the edited result with the Compatibility Oracle and requires per-node reuse and invalidation evidence. An unchanged MSBuild skip, a timestamp touch, wall time alone, or a whole-project cache miss does not satisfy the gate.

This applies the query/action-key, exported-interface, and dependency-graph lessons from the [comparative compiler research](fast-fsharp-compiler-cold-and-incremental.md): cache semantic computations by stable inputs, compare consumer-visible fingerprints before invalidating dependants, and retain F# file-order checkpoints. Rust's query model and Swift's dependency-driven incremental compilation are the closest primary-source analogues; MSBuild's timestamp target skip remains only the outer guard.[^rust-query][^swift-incremental][^msbuild-incremental]

## Source inspection boundary

The sibling IcedTasks checkout was not modified. Its working tree had unrelated local changes, so all inspection used immutable Git objects at the pinned commit. The commit resolves to the tree required by ADR 0020. The project lists these 17 authored files in order:[^icedtasks-project]

```text
 1 AssemblyInfo.fs
 2 Nullness.fs
 3 TaskLike.fs
 4 TaskBuilderBase.fs
 5 ValueTask.fs
 6 PoolingValueTask.fs
 7 ValueTaskUnit.fs
 8 TaskUnit.fs
 9 Task.fs
10 AsyncEx.fs
11 ParallelAsync.fs
12 ColdTask.fs
13 CancellableTaskBuilderBase.fs
14 CancellableValueTask.fs
15 CancellablePoolingValueTask.fs
16 CancellableTask.fs
17 AutoOpens.fs
```

Files 10 and 11 are genuinely in the middle of the ordered compilation. The project targets `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0`; `net9.0` changes FSharp.Core, language-version, and nullness inputs, while the project also requests graph checking, parallel IL generation/optimization, warning-as-error, and reference-assembly production.[^icedtasks-project][^icedtasks-props]

## Mandatory edit 1: implementation changes, exported meaning does not

Edit [pinned `AsyncEx.fs` lines 86-87][icedtasks-asyncex]:

```diff
 static member AwaitTask(task: Task<'T>) : Async<'T> =
-    AsyncEx.AwaitAwaiter(Awaitable.GetTaskAwaiter task)
+    let computation = AsyncEx.AwaitAwaiter(Awaitable.GetTaskAwaiter task) in GC.KeepAlive(task); computation
```

Why this is the implementation-only case:

- The same `TaskAwaiter<'T>` and `Async<'T>` are created before the added call, and the exact `computation` object is returned.
- The returned async computation already captures the awaiter, which retains the task. The lifetime marker therefore adds no effective lifetime or result change.
- The managed signature, accessibility, generic constraints, attributes, and non-inline F# contract do not change.
- Official-compiler IL inspection shows a new `call System.GC::KeepAlive(object)` and a non-tail return, so this is a real lowered-body change rather than whitespace which may disappear before emission.
- Keeping the replacement on one line prevents unrelated declarations later in `AsyncEx.fs` from moving. The changed declaration's own source range, PDB contribution, and implementation fragment still must refresh.

Required warm assertions:

1. `AsyncEx.fs` content and parse keys change; the file is reparsed.
2. The `AwaitTask(Task<'T>)` declaration is rechecked, re-lowered, and re-emitted; its implementation/fragment fingerprint changes.
3. Its exported semantic fingerprint does not change. Raw F# metadata-resource bytes are audit data, not this fingerprint, because source ranges and debug contributions can perturb opaque blobs without changing consumer typing.
4. Unaffected declarations and files 11-17 reuse valid typed checkpoints. Uncertainty may widen this set, but the trace must name the widening reason; silent project-wide invalidation fails the intended proof.
5. The final assembly and PDB are freshly linked/published. The managed API/reference surface and downstream consumer-typing key remain unchanged.
6. Oracle and FSharp2 outputs pass the full artifact, unchanged-test, consumer, and runtime gates declared by ADR 0020.

## Mandatory edit 2: public inline meaning changes

Edit [pinned `ParallelAsync.fs` line 95][icedtasks-parallelasync]:

```diff
-member inline _.BindReturn(x: Async<'T>, f) = Async.map f x
+member inline _.BindReturn(x: Async<'T>, f) = GC.KeepAlive(f); Async.map f x
```

Why this is consumer-visible even though ApiCompat is stable:

- `BindReturn` is public and inline. F# consumers can receive its body through `FSharpOptimizationData`; the body is part of the consumer semantic boundary even though its ECMA-335 signature does not change.
- `Async.map f x` returns an async computation which captures `f`. Calling `KeepAlive(f)` immediately before creating that computation does not invoke `f` or change the result, but it survives in emitted IL and in the inline optimization payload.
- The existing tests and harness consumers can use identical source on both sides of the edit. No synthetic new API or edited IcedTasks test is needed.

Required warm assertions:

1. `ParallelAsync.fs` and the `BindReturn` declaration reparse/recheck/re-lower/re-emit.
2. The managed API fingerprint remains equal, while the decoded F# inline/optimization semantic fingerprint changes.
3. Declarations and external consumer queries which depend on this inline body invalidate transitively. The trace names the exact dependency edge and old/new fingerprint.
4. Consumers which did not depend on the changed declaration may reuse. Source order alone preserves a checkpoint boundary; it is not permission to recheck every later declaration after equivalence is known.
5. Final link and publish still occur, and the unchanged IcedTasks tests and runtime observers agree with the Compatibility Oracle.

### Mandatory FSharp2 inline consumer

The harness manifest names a `net9.0` project `Issue19.BindReturn.Consumer` with this unchanged source:

```fsharp
module Issue19.BindReturn.Consumer

open IcedTasks

let increment (input: Async<int>) =
    parallelAsync {
        let! value = input
        return value + 1
    }
```

Compile this project with FSharp2 against the prebuilt producer artifact in the ADR 0020 producer-consumer matrix; execute `increment (async.Return 41)` and require `42`. The dependency graph must contain an `inline-body` edge from `Issue19.BindReturn.Consumer.increment` to `IcedTasks.ParallelAsync.ParallelAsyncBuilderBase.BindReturn`. The implementation-only producer edit keeps the consumer project reusable, while the `BindReturn` edit invalidates, rechecks, and re-emits that declaration; a clean FSharp2 consumer compile and the Compatibility Oracle must agree on diagnostics, artifacts, and runtime behavior. The unchanged IcedTasks test projects remain Oracle-compiled compatibility evidence and do not substitute for this FSharp2 consumer edge.

## Measurement lane and mandatory controls

### Measured warm-edit lane

Each TFM, edit, and timing sample starts from its own newly materialized baseline. Reset the service and cache, perform exactly one untimed prime compile, retain that healthy service PID/epoch, then immediately apply one content-addressed patch and measure the next non-skipped `CoreCompile`. No no-op, inspection, replay, or other compiler request may occur between prime and edit. The invocation must satisfy its edit-specific assertions to enter the warm timing distribution.

### Companion controls

Controls are real Compiler Target Invocations but are excluded from the timing distribution. Each starts from its own reset-and-prime baseline unless the control explicitly tests a replay or recovery chain:

1. **Compiler-contact/no-op:** force `CoreCompile` without changing source. The request must reach the retained service and validate the prior project state. Relevant semantic nodes report hits/reuse, while the deterministic final link still executes. An MSBuild up-to-date skip fails this control.
2. **Identical replay:** after a non-sampled edit invocation, force `CoreCompile` again without changing the edited bytes. Semantic action keys remain equal and valid nodes hit/reuse; a fresh final link must produce output byte-identical to a clean standalone FSharp2 compile of the same edited inputs.
3. **Baseline restore:** after replay, restore the exact pinned bytes without reformatting, then force `CoreCompile`. The reverse change must be explained, the current PDB/source data must refer to the restored source, and output must match a clean baseline compile.
4. **Diagnostic failure and recovery:** from equivalent valid prestate, run the same harness-owned exact `FS0010` or `FS0001` failure through FSharp2 and the Compatibility Oracle, compare their normalized post-failure leftover artifact sets, then recover to the last valid content. The failed FSharp2 request must not advance the last-successful semantic project epoch/action key or poison subsequent reuse. Do not impose a blanket retain-or-delete policy on disk: match the Oracle artifact behavior separately from retained compiler state.
5. **Unknown-impact widening:** inject the harness-owned missing-dependency-proof case from a fresh prime. FSharp2 must report `widened-uncertain`, recompute the containing scope required for correctness, and agree with a clean compile; an unexplained reuse or silent whole-project miss fails.
6. **Cold bypass:** start without reusable live or disk state. Every lookup reports bypass rather than a hit, and the invocation remains a real compiler target execution.

Successful baseline, edit, replay, and restore lanes remain diagnostic-free under the pinned warning-as-error policy. Failure lanes require exact FS Diagnostic Compatibility and normalized leftover-artifact equivalence with the Compatibility Oracle. These probes do not narrow the user's family-wide requirement: the complete `FSxxxx` error-and-warning surface remains mandatory at the final Compatibility Gate.

## Oracle validation of the exact edits

The exact two one-line edits above were validated in an isolated materialization of the pinned commit with the .NET SDK `10.0.110` Compatibility Oracle:

- Release `Rebuild` succeeded for `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0` with `0` warnings and `0` errors after restoring the repo-local tools.
- SDK-shipped ApiCompat passed for all four TFMs: the managed public contract did not change.
- The unchanged `net9.0` `IcedTasks.Tests` suite passed `796/796`.
- `dotnet-inspect` confirmed that generic `AsyncEx.AwaitTask` kept the same signature and gained `System.GC.KeepAlive(object)` in IL.
- `ParallelAsyncBuilderBase.BindReturn` gained the same IL call. Its `FSharpOptimizationCompressedData.IcedTasks` payload changed hash and length while ApiCompat stayed clean.
- Rebuilding the unchanged consumer against the inline edit placed `GC.KeepAlive` calls in three `IcedTasks.Tests.ParallelAsyncTests` methods. This proves that the edited F# optimization payload reaches consumers, rather than inferring consumer visibility from opaque resource bytes alone.

These checks establish that the edit corpus is viable. They do not establish FSharp2's incremental behavior; that evidence must come from the FSharp2 warm lane described below.

## Cache-validity inputs

An entry is reusable only when every input relevant to that node is equal. If equality cannot be established, recompute or widen. Timestamps may trigger a lookup but never establish semantic validity.

| Validity domain | Inputs which must participate |
| --- | --- |
| Compiler and schema | FSharp2 build/content identity; protocol, query, fingerprint, symbolic-emission, linker, and persisted-cache schema versions; compatibility-envelope version. A mismatch rejects the entry rather than attempting best-effort decoding. |
| Evaluated invocation | Target framework/profile and target kind; assembly identity; language/nullness/conditional-symbol modes; optimization, debug, tail-call, checked, deterministic and path-map switches; warning level, suppression and promotion policy; UI culture; reference/output/XML/resource/signing/SourceLink switches. FS Diagnostic Compatibility makes diagnostic policy a correctness input, not presentation metadata. |
| Ordered source graph | Ordered logical source paths, exact content bytes/encoding/BOM, generated inputs, `#line` effects, file index, and the semantic fingerprint of each required predecessor checkpoint. Physical roots normalize only under the declared path-map/filesystem policy. |
| References and tools | Content and assembly identities for the complete reference closure, FSharp.Core identity, decoded F# signature/optimization fingerprints, friend-assembly context, analyzer identities/configuration, and provider identity/output/invalidation tokens once providers are supported. |
| Declaration semantics | Stable symbol identity; accessibility and compiled name; inferred types, members and constraints; literals; type abbreviations; union/record/active-pattern shape; attributes and generated-member shape; inline and SRTP bodies; resolved-name and overload dependencies. |
| Implementation and emission | Typed implementation fingerprint, optimizer inputs, symbolic fragment schema/dependencies, debug source checksum/range, resource/signing inputs, and target-reference identity. Final metadata handles, row numbers, offsets, MVIDs, and PDB identities never enter a reusable symbolic fragment, per ADR 0021. |
| Environment where observable | Filesystem case/path semantics, working directory for relative resolution, explicitly allowed environment variables, encoding and diagnostic culture. Irrelevant temp/output roots must not destroy reuse when path mapping makes them non-observable. |

Use separate fingerprints for:

- `content`: exact source/query input;
- `export`: consumer-visible F# and managed meaning, including inline/SRTP/abbreviation/attribute/generated-member information;
- `implementation`: non-exported typed body and symbolic method fragment;
- `debug`: document, checksum, range, scope, and sequence-point contribution;
- `diagnostic`: ordered diagnostics under the complete severity/suppression/culture policy.

A single hash of the DLL, reference DLL, or opaque F# metadata resource is too coarse. The implementation-only edit is expected to change implementation/debug artifacts while keeping `export` equal. The inline edit is expected to keep managed API equal while changing the F# portion of `export`.

## Evidence schema

The current prototype already exposes `querySchema`, node kind, content fingerprints, invalidation reason, parse/check/lower keys and decisions, dependency count, phase timings, export fingerprint, fragment hash, and whole-output `emitted` state.[^prototype-model][^prototype-trace] Existing tests prove the retained service, implementation-edit fingerprint split, cache-key replay, current PDB, fresh link, and last-good-output behavior for the current tracer; general negative-output behavior remains Oracle-derived.[^prototype-tests]

Issue #19 extends that vocabulary; it does not make physical cache paths, table/dictionary layout, serialized compiler objects, locking, eviction, or compaction policy a compatibility contract.

### Invocation record

Record at least:

- trace/run/case/edit ids and schema versions;
- corpus commit/tree, ordered file hashes, before/after edited-file SHA-256, canonical patch SHA-256, and TFM;
- compiler/package/protocol/query/emitter/linker identities;
- service PID, service start identity, service epoch, cache epoch/root/schema, and health before/after;
- project-state epoch and last-successful project action key before/after the request; failed requests must leave both unchanged;
- explicit Cold/Warm mode and live/disk cache allow/bypass policy;
- previous/current project action key, option-set hash, reference-set hash, and ordered-source-list hash;
- baseline/current managed API, decoded F# export, implementation, debug, diagnostic, reference-artifact, and final-artifact fingerprints;
- normalized pre/post artifact manifests plus the Compatibility Oracle leftover comparison for failed invocations; and
- real `CoreCompile`, request/RPC, phase, link, publish, and target-finish timings.

### Per-node event

Each considered node emits:

| Field | Meaning |
| --- | --- |
| Identity | Stable `nodeId`, kind (`syntax`, `file-checkpoint`, `declaration`, `diagnostic`, `fragment`, `link`, `consumer-project`), logical file/order, and declaration/symbol id where applicable. |
| Keys | Hashed action key plus old/new content, export, implementation, debug, and diagnostic fingerprints. Raw source or secrets do not belong in shareable traces. |
| Decision | `bypass`, `hit-live`, `hit-disk`, `miss-no-entry`, `miss-key-changed`, `miss-schema`, `miss-corrupt`, `rechecked-equivalent`, `invalidated-direct`, `invalidated-transitive`, `widened-uncertain`, `reused`, `re-lowered`, `re-emitted`, or `relinked`. |
| Cause | Stable reason code, direct `invalidatedBy` node ids, dependency edge kind, transitive depth, and widened scope/reason. |
| Work | Reused/reparsed/rechecked/re-lowered/re-emitted status, old/new fragment hash, cache level, bytes read/written, dependency count, and elapsed microseconds. |
| Outcome | Success/diagnostic/failure, artifact contribution, whether last-successful semantic state advanced, the normalized pre/post artifact-set delta, and the Oracle leftover-comparison result. |

The trace may additionally carry a dependency-edge digest when the complete edge list is too large, but every invalidated node must still identify its direct cause. A count without causality cannot prove transitive invalidation.

### Auditable counters

Aggregate by node kind, phase, cache level, file, and project:

- lookups, live hits, disk hits, misses, bypasses, schema rejects, corrupt rejects;
- files/syntax trees reused and reparsed;
- file checkpoints reused, rebuilt, and conservatively widened;
- declarations considered, reused, rechecked, export-unchanged, export-changed, directly invalidated, and transitively invalidated;
- dependency edges visited, invalidation fan-out, maximum transitive depth, and widening counts by reason;
- fragments reused, re-lowered, re-emitted, and final links performed;
- downstream consumer projects reused, rechecked, and invalidated; and
- diagnostics reused/recomputed; artifacts published, retained, modified, or removed after failure; and normalized Oracle-leftover matches/mismatches.

Require conservation checks such as:

```text
lookups = liveHits + diskHits + misses + bypasses
declarationsConsidered = declarationsReused + declarationsRechecked
successfulRechecks = exportUnchanged + exportChanged
fragmentsConsidered = fragmentsReused + fragmentsRelowered
```

The invocation is `infra-error` if required node events are missing, aggregate counters do not reconcile, the service/cache epoch changed unexpectedly, or trace and binlog/protocol evidence disagree.

## Adversarial invalidation corpus

The two mandatory edits are performance-gated. The remaining cases are correctness tracers and need not participate in the sub-second statistic. Run each from a freshly primed baseline so one edit cannot mask another.

| Construct | Pinned-corpus evidence and exact candidate | Required result |
| --- | --- | --- |
| Non-inline implementation | Final `AsyncEx.AwaitTask` edit above. | Export equal; method implementation/debug refresh; unaffected declarations and consumers reuse. |
| Public inline body | Final `ParallelAsyncBuilderBase.BindReturn` edit above. | Managed API equal; F# inline export changes; exact inline consumers invalidate. |
| Type abbreviation | `ColdTask<'T>` and `ColdTask` are public abbreviations at [`ColdTask.fs` lines 33-35][icedtasks-coldtask]. In a separate materialization, add `type Issue19ColdTaskAlias<'T> = ColdTask<'T>` after line 35. | Decoded F# signature/export changes even though a CLR type need not appear; F# consumer checks the alias; unrelated managed consumers may reuse. |
| SRTP | `Awaiter<'Awaiter,'TResult>` carries member constraints at [`TaskLike.fs` lines 17-20][icedtasks-tasklike], and public inline dispatchers occupy lines 25-145. In a separate tracer, insert `GC.KeepAlive(awaiter);` before the existing `awaiter.GetResult()` at lines 31-34, keeping the signature unchanged. | SRTP/inline export changes; consumers which specialize `GetResult` invalidate; other `Awaiter` members do not invalidate merely because they share a type. |
| Attributes | Assembly attributes are explicit at [`AssemblyInfo.fs` lines 5-12][icedtasks-assemblyinfo], while `AutoOpen`, `Struct`, `NoEquality`, `NoComparison`, and `DefaultValue` occur throughout. Add `[<assembly: AssemblyMetadataAttribute("Issue19WarmEdit","1")>]` after line 12 in a separate materialization. | Assembly-attribute/export and final metadata change; broad project relink is expected, but typing invalidation still follows actual attribute observers. |
| Union cases | No source-defined discriminated union occurs in the 17 authored files; `Choice` cases in `AsyncEx.fs` are FSharp.Core uses, not corpus declarations. | Harness-owned two-file probe: add a case to a DU and require the consumer's exhaustive match to recheck, including exact oracle warning/exit behavior when applicable. |
| Active patterns | `git grep` finds no active-pattern definition in the pinned authored files. | Harness-owned two-file probe changes total/partial shape or case set; exact pattern consumers recheck. |
| Generated members | IcedTasks has compiler-recognized resumable state-machine structs, but its state data explicitly uses `NoEquality`/`NoComparison`; it is not a clean ordinary record/union generated-member tracer.[^icedtasks-taskbase] | Harness-owned record/DU probe changes `[<CLIMutable>]`, fields, or cases and verifies generated constructor/property/equality/tag shape plus consumers. Do not mutate IcedTasks' state-machine representation for this probe. |
| Provider-sensitive | The pinned project/source has no type-provider reference or use. Provider execution is outside the current milestone envelope. | [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) selects the managed broker boundary, but the harness-owned provider probe remains `unsupported` until its declared broker gate passes. Once supported, provider dependency closure/configuration/output identity and invalidation generation are mandatory cache inputs; unknown effects widen conservatively. |
| Unknown effect | No source construct should be mislabeled inherently unknown merely to make a test pass. | Harness-owned fault-injection node deliberately withholds dependency proof. FSharp2 must emit `widened-uncertain`, expand to the containing file/project as needed, and produce oracle-correct output; reuse with missing proof fails. |

These probes are additions to the complete IcedTasks corpus, not patches to the IcedTasks product or tests. Their source, patch, and expected invalidation closure live in the harness manifest and are restored by discarding the isolated materialization.

## Acceptance sequence

For each TFM and edit:

1. Verify the immutable corpus commit/tree, ordered source list, per-file hashes, options, references, and patch preimage.
2. Reset service and cache, perform exactly one untimed prime build, and capture the baseline artifact/fingerprint graph.
3. Without an intervening compiler request, retain the service PID/epoch, apply one content-addressed patch, verify its postimage hash, and measure one non-skipped `CoreCompile` through IPC, invalidation, check/lower/emission, atomic publish, response, and target finish.
4. Run compiler-contact/no-op, identical replay, exact baseline restore, diagnostic failure/recovery, unknown-impact, and cold controls in the separate lanes defined above; none enter the timing sample.
5. Rebuild every valid source state with a clean standalone FSharp2 compiler and rebuild the edited state with the Compatibility Oracle; run all comparators required by ADR 0020, including unchanged tests and the named FSharp2 inline consumer.
6. Assert the edit-specific fingerprint and invalidation closure, counter conservation, no fallback, current PDB/source data, last-successful-state isolation, Oracle-matched failure leftovers, and fresh final link.
7. Restore by discarding the isolated materialization. The pinned IcedTasks checkout and product source remain untouched.

The controlled-runner series follows the differential harness: 15 valid samples initially, 30+ for release, cold median at most 3,000 ms, and warm median below 1,000 ms. Instrumented traces explain failures but do not mix with the uninstrumented timing distribution.

## Uncertainties and follow-on implementation work

- The current prototype's `parse`, `check`, and `lower` decisions are aggregate and its dependency count is zero for the tracer language. Issue #19 requires declaration/file-checkpoint events and causal edges before the IcedTasks warm proof can pass.
- [ADR 0023](../adr/0023-keep-the-fsharp-metadata-harness-inspector-envelope-only.md) keeps the harness inspector at the exact resource-envelope layer and any private-pickle decoder diagnostic-only. FSharp2's production NativeAOT importer and semantic/API fingerprints remain separate compiler implementation work; raw resource hashes must be retained, but they cannot substitute for the semantic fingerprint split required here.
- Exact transitive consumers of `BindReturn` must come from the implemented dependency graph, not a hard-coded file count. `Issue19.BindReturn.Consumer.increment` is the manifest's minimum required `inline-body` edge; additional invalidation is allowed only with an explicit conservative-widening reason.
- Type-provider invalidation remains deliberately unsupported in the current milestone until the [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) broker envelope passes its declared gate. This is a declared harness-owned gap, not evidence of compatibility; the selected final boundary and cache rules are defined by the [issue #13 research](type-provider-compatibility-boundary.md).
- GitHub issue text could not be refreshed during this research because the configured `gh` authentication returned HTTP 401. The checked-in differential-harness decision and ADR 0020 were therefore treated as the authoritative issue contract.

## Primary sources and repo evidence

- [Comparative fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md)
- [Differential compatibility and performance harness](differential-compatibility-performance-harness.md)
- [ADR 0020 IcedTasks milestone](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md)
- [Current prototype cache owner](../../../src/fsc2.Prototype/CompilerService.fs)
- [Current trace writer](../../../src/FSharp2.Compiler.Prototype.Core/CompilerHost.fs)
- [Current retained-service/cache proof](../../../tests/fsharp2.Tests/CompilerTargetTests.fs)

[^keep-alive]: [.NET `GC.KeepAlive` API and semantics](https://learn.microsoft.com/en-us/dotnet/api/system.gc.keepalive).
[^rust-query]: [rustc query system](https://rustc-dev-guide.rust-lang.org/query.html) and [incremental compilation](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html).
[^swift-incremental]: [Swift incremental compilation design](https://github.com/swiftlang/swift/blob/main/docs/IncrementalCompilation.md).
[^msbuild-incremental]: [MSBuild incremental builds](https://learn.microsoft.com/en-us/visualstudio/msbuild/incremental-builds).
[^icedtasks-project]: [Pinned `IcedTasks.fsproj`](https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/IcedTasks.fsproj#L1-L30).
[^icedtasks-props]: [Pinned `Directory.Build.props`](https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/Directory.Build.props#L1-L36).
[^prototype-model]: [`CompilerQueryResult`, response, and statistics fields](../../../src/fsc2.Prototype/Model.fs#L216-L273).
[^prototype-trace]: [Current line-oriented trace fields](../../../src/FSharp2.Compiler.Prototype.Core/CompilerHost.fs#L8-L70).
[^prototype-tests]: [Current retained-service query proof](../../../tests/fsharp2.Tests/CompilerTargetTests.fs#L1929-L2163).
[^icedtasks-taskbase]: [Pinned state-machine data attributes](https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/TaskBuilderBase.fs#L20-L44).

[icedtasks-asyncex]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/AsyncEx.fs#L75-L104
[icedtasks-parallelasync]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/ParallelAsync.fs#L67-L105
[icedtasks-coldtask]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/ColdTask.fs#L32-L56
[icedtasks-tasklike]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/TaskLike.fs#L16-L48
[icedtasks-assemblyinfo]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/AssemblyInfo.fs#L1-L23
