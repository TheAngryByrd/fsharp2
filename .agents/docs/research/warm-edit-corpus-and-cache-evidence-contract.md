# Warm-edit corpus and cache-evidence contract

Status: issue [#19](https://github.com/TheAngryByrd/fsharp2/issues/19) research decision, refreshed 2026-07-19 against the live issue question

Document purpose: normative harness-contract reference backed by research; it is not an implementation tutorial or a cache-layout/API specification.

Corpus: IcedTasks commit `ba4e932b71bfde354f0e2561c2519b282fe56ff9`, tree `20abf5280d4630db438c7c6f06838105df12c765`

Governing decisions: [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), [ADR 0016](../adr/0016-explain-slow-compilations-with-structured-traces.md), [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md), and [ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md)

Related authority: [differential-harness verdicts](differential-compatibility-performance-harness.md), [`perf-governance-v1`](compiler-performance-runner-and-regression-governance.md), and [`evidence-governance-v1`](secure-harness-evidence-retention-and-redaction.md)

## Decision

Use two line-count-preserving edits to the pinned IcedTasks source in isolated, hash-verified materializations:

1. `AsyncEx.AwaitTask(Task<'T>)` is the implementation-only case. It changes the emitted method body while preserving its managed and F# exported meaning and its returned value.
2. `ParallelAsyncBuilderBase.BindReturn` is the consumer-visible inline case. Its managed signature remains stable, but its public inline body and F# optimization payload change.

The corpus is not merely two patches: forced compiler-contact/no-op, a comment-only content edit, identical replay, exact baseline restore, diagnostic add/fix, compiler-option/reference/source-order key mutations, A -> B -> A recovery, stale-state races, corrupt/schema-rejected entries, and unknown-impact widening are mandatory companion controls. A measured sample primes once and then applies its edit immediately; no control invocation may warm extra state before the timed `CoreCompile`, and replay hits or MSBuild skips cannot enter the Warm Compilation timing distribution.

Both edits add a `GC.KeepAlive` lifetime marker to an object already retained by the returned `Async`. This gives the harness an emitted call which the optimizer cannot erase as a source-only no-op, without using APIs missing from `netstandard2.0` or changing the observed result. `GC.KeepAlive(Object)` is present in the pinned `netstandard2.0` reference assembly and is documented as extending object liveness to the call site.[^keep-alive]

The Warm Compilation gate primes one compiler service, applies exactly one edit, then performs a real `CoreCompile` through the same healthy service instance, live-state epoch, and cache-namespace epoch. It compares the edited result with the Compatibility Oracle and requires per-node reuse and invalidation evidence. An unchanged MSBuild skip, a timestamp touch, wall time alone, or a whole-project cache miss does not satisfy the gate.

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

### Cold, warm, replay, and skip are different states

- **Cold Compilation** executes one real `CoreCompile` while the request explicitly bypasses both live and disk compiler state. It has no semantic reuse. OS page-cache warmth does not change this classification.
- **Measured Warm Compilation** starts from one successful prime, retains the same service instance and live-state epoch, changes declared content, and executes the first non-skipped post-edit `CoreCompile`. Its event stream must prove both valid reuse and the work caused by the edit. For this milestone, a disk hit after a process restart is a useful recovery control but is not a measured Warm sample.
- **Compiler-contact/no-op** and **identical replay** force a real `CoreCompile` with the same semantic input action key. They prove lookup and reuse behavior but do not enter the Warm-edit timing distribution.
- **Unchanged-target skip** means MSBuild did not execute `CoreCompile`. It contains no compiler request, query, reuse, or timing evidence and is neither Cold nor Warm Compilation. MSBuild officially defines this outer target skip from declared inputs and outputs; it does not establish semantic reuse inside the compiler.[^msbuild-incremental]

The measured interval is `CoreCompile` `TargetStarted` through its matching `TargetFinished`, including the FSharp2 task/client, service request, validation, incremental work, fresh link/publish, response, and target completion. It excludes restore, project evaluation, materialization, cache reset, prime compilation, patching, Oracle execution, artifact comparison, consumers, debugger automation, and heavy diagnostic replay. No debugger or harness post-processing time may be placed inside Compiler Target Invocation. The ordinary bounded decision stream and work summary are part of the request; EventPipe or another heavy profiler is a separate replay and never enters the timing distribution.

### Measured warm-edit lane

Each TFM, edit, and timing sample starts from its own newly materialized baseline. Reset the service and cache, perform exactly one untimed prime compile, retain that healthy service instance/live-state/cache-namespace lineage, then immediately apply one content-addressed patch and measure the next non-skipped `CoreCompile`. No no-op, inspection, replay, or other compiler request may occur between prime and edit. The invocation must satisfy its edit-specific assertions to enter the warm timing distribution.

### Companion controls

Controls are real Compiler Target Invocations but are excluded from the timing distribution. Each starts from its own reset-and-prime baseline unless the control explicitly tests a replay or recovery chain:

1. **Compiler-contact/no-op:** force `CoreCompile` without changing source bytes or the project action key. The request must reach the retained service and validate the prior project state. Relevant semantic nodes report hits/reuse, while the deterministic final link still executes. An MSBuild up-to-date skip fails this control.
2. **Comment-only content edit:** in an isolated materialization, change the one-line `AsyncEx.fs` comment `// Why not handle TaskCanceledException/OperationCanceledException?` to `// Why handle neither TaskCanceledException nor OperationCanceledException?`. Exact content and the PDB document checksum change, but declaration export, implementation, and diagnostic fingerprints remain equal. The source-content/debug contribution must refresh; semantic checkpoints may reuse only after equivalence is established. Restore the exact original bytes afterward.
3. **Identical replay:** after a non-sampled edit invocation, force `CoreCompile` again without changing the edited bytes. Semantic action keys remain equal and valid nodes hit/reuse; a fresh final link must produce output byte-identical to a clean standalone FSharp2 compile of the same edited inputs.
4. **Baseline restore and A -> B -> A:** after replay, restore the exact pinned bytes without reformatting, then force `CoreCompile`. The action key returns to A but the last-successful project epoch advances to a new state rather than rolling backward. A content-addressed A result may hit, but it must be revalidated against the current dependency graph; current PDB/source data must refer to restored source and output must match a clean baseline compile.
5. **Diagnostic add/fix:** append the exact harness-owned [`UnexpectedToken.fs`](../../../tests/FSharp2.Prototype.Diagnostics/UnexpectedToken.fs) (`FS0010`) or [`TypeMismatch.fs`](../../../tests/FSharp2.Prototype.Diagnostics/TypeMismatch.fs) (`FS0001`) source after `AutoOpens.fs` as a temporary ordered `Compile` input to the full IcedTasks project, compare FSharp2 and Oracle diagnostics and normalized post-failure artifact sets, remove it, and recover. The failed request must not advance last-successful project epoch/action key, publish a successful artifact, or poison subsequent reuse. Do not impose a blanket retain-or-delete policy on disk: match Oracle artifact behavior separately from retained compiler state.
6. **Key mutation:** execute each compiler-option, reference-identity, and ordered-source-list A -> B -> A control below. A timestamp-only mutation does not count.
7. **Unknown-impact widening:** inject the harness-owned missing-dependency-proof case from a fresh prime. FSharp2 must report `widened-uncertain`, recompute the containing scope required for correctness, and agree with a clean compile; an unexplained reuse or silent whole-project miss fails.
8. **Adversarial stale state:** use deterministic harness barriers at the project-state commit seam to exercise an out-of-order commit race; use a versioned test cache adapter to return a logically schema-mismatched candidate and an integrity-failing candidate without depending on physical cache files; and exercise an unexpected service restart. No old attempt or entry may publish over a newer successful state. The restart makes a measured Warm row `product-fail`; a separately declared recovery control may start a new service and prove safe disk reuse. Sleeping, racing by chance, or flipping bytes in an implementation-specific cache file does not satisfy this control.
9. **Cold bypass:** start with explicit live/disk bypass. Cache-eligible nodes report bypass rather than lookup hits, reuse is zero, and the invocation remains a real compiler target execution.

Successful baseline, edit, replay, and restore lanes remain diagnostic-free under the pinned warning-as-error policy. Failure lanes require exact FS Diagnostic Compatibility and normalized leftover-artifact equivalence with the Compatibility Oracle. These probes do not narrow the user's family-wide requirement: the complete `FSxxxx` error-and-warning surface remains mandatory at the final Compatibility Gate.

### Exact key-mutation controls

F# compiler options define references, conditional symbols, optimization, debug behavior, and ordered source processing, so the evaluated option set, reference closure, and source order are semantic inputs rather than cache-location details.[^fsharp-options]

| Control | A -> B -> A materialization | Required evidence |
| --- | --- | --- |
| Compiler option | Compile Release A with the pinned `Optimize=true`; compile B with only the evaluated `Optimize=false`/`--optimize-` value changed; restore A. | `optionSetHash` changes and returns exactly. Syntax and typing may reuse because optimization is not their input; optimization/lowering/emission nodes whose behavior depends on the switch must not reuse B from A. Every state matches a clean same-option FSharp2 build and the Oracle's runtime/API behavior. |
| Reference content and semantic identity | Through a harness-owned temporary targets overlay, reference a deterministic C# assembly `Issue19.ReferenceProbe` targeting `netstandard2.0`, assembly name/version `Issue19.ReferenceProbe, 1.0.0.0`. Variant A exposes `public const int Marker = 1`; B uses the same logical item/path/assembly identity but `Marker = 2`; then restore A. IcedTasks source does not use the probe. | Reference bytes and decoded export fingerprints change even though path/name/version do not. The reference-import node recomputes. IcedTasks declarations may reuse only when the graph proves no observed edge; otherwise widening must be explicit. The restored content-addressed A candidate is revalidated, never trusted by path or timestamp. |
| Ordered source list | Add two harness-owned internal files between `Task.fs` and `AsyncEx.fs`. Each begins with `namespace IcedTasks`; `Issue19OrderA.fs` then defines `module internal Issue19OrderA` and `let marker = 1`, while `Issue19OrderB.fs` defines the corresponding B module/value `2`. A orders A then B; B swaps only those two `Compile` items; restore A. | The file-content set stays identical while `orderedSourceListHash` changes and returns. Checkpoints before the first marker reuse; both marker-order checkpoints recompute. Later checkpoints may reuse only after the cumulative environment is proved equivalent. Final link/debug ordering refreshes and each state matches a clean compile. |

Each control uses a fresh prime and its own service/cache lineage. It is correctness evidence, not a Warm performance sample. The manifest records exact overlay, source, option, and reference hashes; discarding the materialization removes every IcedTasks-adjacent change.

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

- trace/run/case/edit ids, one logical `requestId`, one unique execution `attemptId`, attempt ordinal/parent/retry reason, and schema versions;
- corpus commit/tree, ordered file hashes, before/after edited-file SHA-256, canonical patch SHA-256, and TFM;
- compiler/package/protocol/query/emitter/linker identities;
- opaque service-instance id, live-state epoch, cache-namespace id/epoch/schema, and health before/after;
- base and last-successful project-state epoch/action key before/after the attempt, plus atomic commit/superseded state;
- explicit Cold/Warm mode and live/disk cache allow/bypass policy;
- previous/current project action key, option-set hash, reference-set hash, and ordered-source-list hash;
- baseline/current managed API, decoded F# export, implementation, debug, diagnostic, reference-artifact, and final-artifact fingerprints;
- normalized pre/post artifact manifests plus the Compatibility Oracle leftover comparison for failed invocations; and
- real `CoreCompile`, request/RPC, phase, link, publish, and target-finish timings; and
- attempt outcome (`valid`, `product-fail`, or `infra-invalid`), four-state case verdict, stable reasons, and the evidence proving any infrastructure attribution.

### Identity, epoch, failure, and retry rules

These identities are deliberately independent:

| Identity | Contract |
| --- | --- |
| Logical request | `requestId` names one evaluated Compiler Target Invocation. A transport retry for that invocation keeps the request id but receives a new, never-reused `attemptId`; a replacement performance row is a new request linked by `replacementForAttemptId`, not another attempt of the original invocation. |
| Service instance | `serviceInstanceId` is random and unique for one process lifetime. PID and process start identity corroborate it only in restricted evidence; PID alone is not identity because operating systems reuse it. |
| Live-state epoch | `serviceStateEpoch` is monotonic within one service instance and changes only when the in-memory query graph is reset, quarantined, or wholesale-recovered. Normal successful requests do not increment it. Prime and measured edit require the same service instance and state epoch. |
| Disk namespace epoch | `cacheNamespaceId` identifies one compiler/query/cache-schema compatibility namespace; `cacheNamespaceEpoch` changes when that namespace is cleared, migrated, quarantined, or replaced. A service restart may retain this pair, which permits a separate disk-recovery control, but it does not preserve the measured live Warm lineage. |
| Project state | `lastSuccessfulProjectEpoch` is monotonic for one logical project/TFM within a service-state epoch. It advances exactly once when a successful attempt atomically commits a different project action key/state; an identical replay republishes output without advancing semantic state. `lastSuccessfulProjectActionKey` names the committed inputs. |

Every attempt records `baseProjectEpoch`. Commit is compare-and-swap against that base. A diagnostic or compiler failure, cancelled/superseded attempt, malformed candidate trace, or unsuccessful publication leaves the committed project epoch, action key, live project state, and last-good artifact pointer unchanged; speculative failure-key entries may remain only if isolated and never masquerade as last-successful state. For retries, at most one attempt may commit; later completion of an older attempt is `superseded` and cannot publish. In A -> B -> A, the action key returns to A while epochs move `e -> e+1 -> e+2`; this is a new validated state, never an epoch rollback. An unexpected service-instance/state-epoch or cache-namespace-epoch change is `product-fail` when candidate-owned and `infra-invalid` only when independent runner evidence proves the runner caused it.

### Per-node event

Each considered node emits:

| Field | Meaning |
| --- | --- |
| Identity | Stable `nodeId`, kind (`syntax`, `file-checkpoint`, `declaration`, `diagnostic`, `fragment`, `link`, `consumer-project`), logical file/order, and declaration/symbol id where applicable. |
| Keys | Hashed action key plus old/new content, export, implementation, debug, and diagnostic fingerprints. Only approved public/harness fingerprints may be shareable; private fingerprints are R2 and raw source/secrets are never shareable. |
| Cache lookup | Exactly one of `not-eligible`, `bypass-policy`, `hit-live`, `hit-disk`, `miss-no-entry`, `miss-key-changed`, `reject-schema`, or `reject-corrupt`. A hit means only that a candidate was found; it is not proof of validity or reuse. |
| Validity/invalidation | Exactly one of `no-prior`, `valid`, `invalid-direct`, `invalid-transitive`, or `widened-uncertain`, with stable reason code, direct `invalidatedBy` ids, dependency-edge kind, transitive depth, and widened scope/reason. |
| Semantic comparison | Exactly one of `not-compared`, `equal`, `changed`, or `unavailable`. `equal` after executed work is rechecked-equivalent, not reuse. `unavailable` covers a diagnostic/compiler failure before a comparable result exists. |
| Actual work | For each applicable phase (`source-read`, `parse`, `check`, `lower`, `emit`, `link`), exactly one of `not-applicable`, `reused`, `executed-success`, `executed-diagnostic`, or `executed-failed`, plus bytes read/written, dependency count, and elapsed microseconds. Old/new fragment hashes accompany applicable emission work. |
| Outcome | `success`, `diagnostic`, or `compiler-failure`; artifact contribution; commit/superseded state; normalized pre/post artifact-set delta; and Oracle leftover-comparison result. |

These axes are orthogonal. For example, a node can report `hit-live`, `invalid-transitive`, `equal`, and `check=executed-success`: the old candidate was found, could not initially be reused because a dependency changed, and was rechecked to equivalent meaning. Collapsing that event to either "hit" or "rechecked" loses required evidence.

The restricted trace carries the complete dependency graph and direct-cause ids as R2. The shareable trace carries allowed public-corpus node identities, reason codes, dependency counts/depths, approved fingerprints, and reconciliation verdicts, not raw edges. Every invalidated node must still have a resolvable direct cause in authoritative evidence. A count without causality cannot prove transitive invalidation.

### Auditable counters

Aggregate by node kind, phase, cache level, file, and project:

- cache-eligible nodes, lookup attempts, live hits, disk hits, no-entry misses, key-change misses, bypasses, schema rejects, and corrupt rejects;
- files/syntax trees reused and reparsed;
- file checkpoints reused, rebuilt, and conservatively widened;
- declarations considered, reused, rechecked, export-unchanged, export-changed, directly invalidated, and transitively invalidated;
- dependency edges visited, invalidation fan-out, maximum transitive depth, and widening counts by reason;
- fragments reused, re-lowered, re-emitted, and final links performed;
- downstream consumer projects reused, rechecked, and invalidated; and
- diagnostics reused/recomputed; artifacts published, retained, modified, or removed after failure; and normalized Oracle-leftover matches/mismatches.

Require conservation checks such as:

```text
cacheEligibleNodes = bypasses + lookupAttempts
lookupAttempts = liveHits + diskHits + noEntryMisses + keyChangedMisses + schemaRejects + corruptRejects
hits = liveHits + diskHits
misses = noEntryMisses + keyChangedMisses
rejects = schemaRejects + corruptRejects

priorSemanticNodes = validNodes + directInvalidations + transitiveInvalidations + widenedInvalidations
invalidations = directInvalidations + transitiveInvalidations + widenedInvalidations
successfulSemanticRecomputations = semanticEqual + semanticChanged

phaseEligible = phaseReused + phaseExecutedSuccess + phaseExecutedDiagnostic + phaseExecutedFailed
phaseExecuted = phaseExecutedSuccess + phaseExecutedDiagnostic + phaseExecutedFailed
declarationsConsidered = declarationsReused + declarationsRecheckedSuccess + declarationsRecheckedDiagnostic + declarationsRecheckedFailed
fragmentsConsidered = fragmentsReused + fragmentsReloweredSuccess + fragmentsReloweredDiagnostic + fragmentsReloweredFailed
```

Every aggregate is derived from the same canonical event set, not separately incremented mutable counters. A reused phase must have a valid hit; a miss/reject cannot reuse that candidate; an invalidated affected phase cannot be marked reused; and a semantic `equal` reached through execution counts as recomputation. On Cold Compilation, lookup attempts and reuse are zero while every cache-eligible node is bypassed. On a successful supported invocation, the final link executes exactly once and cannot be reported as a whole-output cache hit. A measured Warm edit has both nonzero required work for the changed closure and nonzero proven semantic reuse; silent whole-project recomputation fails the edit-specific proof even if output and time pass.

Missing/malformed candidate-emitted node events, unreconciled counters, candidate-owned epoch drift, or disagreement with binlog/protocol evidence is `product-fail`, producing case verdict `fail`. It is `infra-invalid` only when independent supervisor evidence proves that correct candidate evidence existed and runner-side collection lost or corrupted it; unresolved attribution fails toward the candidate. This aligns with `perf-governance-v1`: performance attempts cannot retry away required-evidence failures.

### Evidence visibility: semantics are contractual; cache layout is not

Apply `evidence-governance-v1` field by field:

- **R3 Restricted Raw:** source/protocol/binlog bytes, service PID and raw process-start identity, physical cache root and cache files, cache snapshots, raw traces, and complete machine/process inventories.
- **R2 Controlled Derived:** complete/raw dependency edges, raw-to-shareable id mapping, internal unmapped paths, and private-source/input fingerprints or hashes.
- **S1 Shareable:** schema/compiler/policy ids; public corpus and harness-fixture hashes; logical path/node identities; run-scoped opaque service/cache/project epoch ids; hit/miss/reuse/invalidation reason codes; dependency/work counters; timings; approved public-corpus fingerprints; equality and reconciliation verdicts. Raw dependency edges remain R2.

The local verifier uses R3/R2 to prove equality relationships such as `sameServiceInstance=true`, `sameServiceStateEpoch=true`, and `sameCacheNamespaceEpoch=true`; S1 need not reveal the PID, process start value, cache root, or raw private hashes. A physical root, filename, table/dictionary shape, serialization object, sharding, locking, eviction/compaction policy, key encoding, or memory address is never a compatibility or performance-pass condition. Cache implementations may change without changing this contract as long as logical identity, validity, causality, work, artifacts, and verdict evidence still reconcile.

### Attempt outcome, case verdict, and failure reasons

Each baseline/candidate attempt has one `perf-governance-v1` outcome:

| Attempt outcome | Meaning |
| --- | --- |
| `valid` | State controls, execution, correctness, outputs, and required evidence are trustworthy. A valid timing may still contribute to a series-level performance `fail`. |
| `product-fail` | FSharp2 caused a correctness, diagnostic, fallback, artifact, state-control, timeout/hang, stale-reuse, malformed/missing-evidence, or publication failure. It terminates the required cell as `fail` and is not replacement-eligible. |
| `infra-invalid` | Independent observations prove admission noise, runner/tool failure, or runner-side evidence collection—not compiler behavior or emission—invalidated the attempt. It is preserved and replacement-eligible only within the predeclared cap. |

Aggregate cases retain the differential harness's exact four verdicts: `pass`, `fail`, `unsupported`, and `infra-error`. `unsupported` is valid only for a manifest cell explicitly outside the current milestone and rejected before successful output; none of the two mandatory IcedTasks edits or key controls may use it. The type-provider tracer remains `unsupported` only until its ADR 0025 broker gate passes. `infra-error` means infrastructure prevented a trustworthy required verdict; it never converts to pass. Family-wide FS Diagnostic Compatibility remains required at the final gate, so an unimplemented FSxxxx family is `fail` there, not `unsupported`.

Stable failure reasons include `corpus-precondition`, `target-skipped`, `wrong-mode`, `service-lineage-changed`, `cache-epoch-changed`, `stale-commit`, `stale-reuse`, `key-input-omitted`, `unexplained-widening`, `over-invalidation`, `counter-nonconservation`, `causality-missing`, `diagnostic-mismatch`, `artifact-mismatch`, `runtime-mismatch`, `oracle-leftover-mismatch`, `compiler-crash`, `compiler-timeout`, `performance-threshold`, `runner-admission`, `runner-collection-loss`, and `oracle-or-tool-unavailable`. The reason refines but never weakens the attempt outcome or four-state verdict.

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

## Acceptance requirements

Every TFM/edit cell must satisfy this contract:

| Boundary | Required evidence |
| --- | --- |
| Immutable inputs | Corpus commit/tree, ordered source list, per-file hashes, options, references, patch preimage, and patch postimage match the manifest. |
| Prime lineage | Exactly one untimed prime follows the service/cache reset and captures the baseline artifact/fingerprint graph. |
| Measured transition | With no intervening compiler request, the same service-instance/live-state/cache-namespace lineage receives one content-addressed edit and executes one non-skipped `CoreCompile` covering IPC, invalidation, check/lower/emission, atomic publish, response, and target finish. |
| Separate controls | Compiler-contact/no-op, comment-only content, identical replay, exact baseline restore/A -> B -> A, diagnostic add/fix, option/reference/order key mutations, stale-state, unknown-impact, and Cold controls run in their declared independent lanes and never enter the timing sample. |
| Independent correctness | Every valid source state matches a clean standalone FSharp2 compile; the edited state matches the Compatibility Oracle and all ADR 0020 comparators, unchanged tests, runtime probes, and the named FSharp2 inline consumer. |
| Reconciliation | Edit-specific fingerprints and invalidation closure, request/attempt and epoch invariants, counter conservation, no fallback, current PDB/source data, last-successful-state isolation, Oracle-matched failure leftovers, fresh final link, evidence classification, attempt outcome, and four-state verdict all reconcile. |
| Isolation and restore | The case ends by discarding its isolated materialization; the pinned IcedTasks checkout and product source remain untouched. |

The controlled-runner series follows `perf-governance-v1`: 15 complete valid pairs within at most 18 scheduled pairs for controlled PR/nightly runs, and 30 within at most 36 for release; cold median is at most 3,000 ms and warm median is below 1,000 ms, with the separate release p95 and relative-regression gates still mandatory. Instrumented traces explain failures but do not mix with the uninstrumented timing distribution.

## Uncertainties and follow-on implementation work

- The current prototype's `parse`, `check`, and `lower` decisions are aggregate and its dependency count is zero for the tracer language. Issue #19 requires declaration/file-checkpoint events and causal edges before the IcedTasks warm proof can pass.
- [ADR 0023](../adr/0023-keep-the-fsharp-metadata-harness-inspector-envelope-only.md) keeps the harness inspector at the exact resource-envelope layer and any private-pickle decoder diagnostic-only. FSharp2's production NativeAOT importer and semantic/API fingerprints remain separate compiler implementation work; raw resource hashes must be retained, but they cannot substitute for the semantic fingerprint split required here.
- Exact transitive consumers of `BindReturn` must come from the implemented dependency graph, not a hard-coded file count. `Issue19.BindReturn.Consumer.increment` is the manifest's minimum required `inline-body` edge; additional invalidation is allowed only with an explicit conservative-widening reason.
- Type-provider invalidation remains deliberately unsupported in the current milestone until the [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) broker envelope passes its declared gate. This is a declared harness-owned gap, not evidence of compatibility; the selected final boundary and cache rules are defined by the [issue #13 research](type-provider-compatibility-boundary.md).
- The implementation ticket must choose and version canonical stable symbol/node identities and semantic fingerprint encodings. This note constrains their observable equality and invalidation behavior, not their byte layout.
- The service/cache implementation must make project-state compare-and-swap, cache namespace quarantine, and crash recovery atomic enough to satisfy the stale-attempt tests. The on-disk journal/index strategy remains private.
- The harness implementation must supply the temporary targets overlay, two reference variants, two order-marker files, exact patch hashes, and an independent event-to-counter reconciler. Until deliberate event mutation proves every conservation/failure path, a green compiler test is not cache-evidence proof.
- The live issue asks only for the controlled Warm edits and epoch/hit/miss/reuse/invalidation/work proof. That research question is resolved here; implementing the graph, schemas, controls, and complete final-gate FSxxxx corpus remains follow-on engineering under issue #1.

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
[^fsharp-options]: [Official F# compiler options](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-options), including `--define`, `--reference`, `--optimize`, debug controls, and command-line source inputs.
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
