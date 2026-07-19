# Fast compiler design lessons for FSharp2

Status: research snapshot, 2026-07-18

## Executive conclusion

Fast compilers usually win through a combination of scope, representation, and reuse:

1. They make the hot path small: a native executable or long-lived daemon, low startup ceremony, compact data structures, and lazy loading.
2. They cache semantic work by stable inputs, not merely by file timestamps. Query/action caches make unchanged work reusable and make cache invalidation explicit.
3. They make invalidation narrower than a file or project whenever correctness permits: exported interfaces, declarations, compiler queries, or action keys become the cache boundary.
4. They schedule the remaining work in parallel, while preserving the language's required ordering constraints.

For FSharp2, the decisive design is a persistent NativeAOT compiler service with a content-addressed, declaration-aware query graph. A file edit in the middle of an F# project should reparse and recheck that file, then invalidate only declarations whose exported meaning changed and the transitive consumers of those declarations. A changed implementation with the same exported semantic fingerprint should not force downstream projects to recompile.

This is a recommendation for FSharp2, not a claim that every cited compiler implements the complete design. The existing [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md) already establishes the key local direction: preserve F# source-order checkpoints, track semantic dependencies explicitly, and widen invalidation when impact cannot be proven.

## FSharp2 constraints and vocabulary

The repository defines a Compiler Target Invocation separately from a whole solution build, calls an invocation without reusable compiler state a Cold Compilation, and reserves Warm Compilation for a later invocation that actually reuses valid compiler state. `CONTEXT.md` sets a three-second Performance Tripwire for every Compiler Target Invocation. Existing research records that the shipped compiler launches a fresh, non-incremental `fsc` process for the measured path, while an unchanged MSBuild build is a skipped no-op rather than a warm compiler measurement.

[ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md) applies these distinctions to the complete pinned IcedTasks corpus. It requires the warm lane to distinguish an implementation-only edit from a consumer-visible middle-of-order edit; the follow-on warm-edit decision owns the exact reversible changes and cache-evidence contract.

The independent, source-informed implementation and NativeAOT constraints matter. Contributors may study the official compiler as the Compatibility Oracle, but FSharp2 cannot copy or reuse its production implementation or load it as a runtime dependency. A production cache therefore needs a closed, versioned schema and metadata readers that do not load arbitrary target assemblies into the compiler host.

## Evidence from primary sources

### OCaml and Dune: compilation units and explicit build caching

OCaml's compilation model makes `.mli` interfaces and compiled interfaces (`.cmi`) first-class artifacts. The compiler manual documents separate compilation of implementation and interface units and the need to compile in dependency order. This is a useful analogue for F#: a stable public signature is a much better downstream cache key than the source timestamp.

Dune documents a build cache keyed by the contents and relevant action context, with local and shared-cache modes. That demonstrates the practical value of treating compilation as reproducible actions with explicit inputs rather than as an imperative sequence of commands.

Evidence: [OCaml compiler manual, compilation units](https://ocaml.org/manual/5.2/comp.html), [Dune cache](https://dune.readthedocs.io/en/stable/cache.html).

### Zig: explicit incremental compilation and lazy semantic work

Zig's language reference exposes incremental compilation as a compiler feature and describes the compiler's ability to preserve compilation state across edits. The Zig source has a central compilation state and explicit invalidation/re-analysis machinery rather than assuming that a new process must rebuild the world. Its design is especially relevant to FSharp2's warm target: a service must own the state that a one-shot command-line compiler throws away.

Evidence: [Zig language reference, Incremental Compilation](https://ziglang.org/documentation/master/#Incremental-Compilation), [Zig compilation implementation](https://github.com/ziglang/zig/blob/master/src/Compilation.zig).

The lesson is architectural, not a promise that Zig's exact invalidation rules fit F#. F# inference and source-order semantics require a richer dependency summary and more conservative fallback.

### Odin: small compiler surface and deliberate simplicity

Odin's official FAQ attributes its fast compilation experience to a deliberately simple language/compiler model and a compiler written as a native executable; its repository exposes the compiler as a direct, self-contained tool rather than a large managed toolchain. This is evidence for reducing startup and per-file overhead, not evidence for fine-grained incremental checking: Odin is not a drop-in model for F#'s inferred, ordered module system.

Evidence: [Odin FAQ](https://odin-lang.org/docs/faq/), [Odin compiler source](https://github.com/odin-lang/Odin/tree/master/src).

### Go: action keys, build IDs, and a persistent build cache

The Go command documents build caching for package actions. The Go source implements cache keys/action IDs and records outputs so unchanged package actions can be reused. Go's package API boundary is comparatively strong: a package's compiled export data is a natural boundary for avoiding work in consumers.

Evidence: [Go build and test caching](https://go.dev/cmd/go/#hdr-Build_and_test_caching), [Go build cache source](https://go.dev/src/cmd/go/internal/cache/cache.go), [Go build ID source](https://go.dev/src/cmd/go/internal/work/buildid.go).

The F# adaptation is to use an action key containing compiler/schema version, options, target framework/reference fingerprints, ordered source content, and imported semantic fingerprints. Do not copy Go's package-level granularity as the only level: it is too coarse for the requested middle-of-DAG edit.

### Rust: memoized queries, dependency tracking, and incremental sessions

The rustc developer guide describes the compiler as a demand-driven query system. Queries memoize results, track dependencies, and can reuse results across a compilation session. Its incremental-compilation documentation explains serialization and reuse of query results between sessions, with dependency validation determining whether a result remains valid.

This is the closest published architecture analogue for FSharp2. The key is not “cache every phase”; it is to make semantic computations named queries with explicit inputs and dependencies. A changed declaration then invalidates the smallest query frontier that depends on its changed fingerprint.

Evidence: [rustc query system](https://rustc-dev-guide.rust-lang.org/query.html), [rustc incremental compilation](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html), [rustc incremental codegen option](https://doc.rust-lang.org/rustc/codegen-options/index.html#incremental).

### Swift: dependency scanning, explicit modules, and driver-managed incrementality

Swift's driver and compiler documentation describe incremental compilation as a dependency-driven process and document explicit module/dependency scanning work. The driver can avoid recompiling unaffected source files when it can determine that a dependency's relevant interface has not changed. The explicit-modules work also reduces repeated module discovery and serialization overhead.

Evidence: [Swift incremental compilation design](https://github.com/swiftlang/swift/blob/main/docs/IncrementalCompilation.md), [Swift dependency scanning](https://github.com/swiftlang/swift-driver/tree/main/docs), [Swift explicit modules](https://github.com/swiftlang/swift/blob/main/docs/ExplicitModules.md).

The direct F# lesson is to separate project graph discovery from compiler execution and to persist a normalized dependency graph. The compiler should receive evaluated, ordered inputs from the MSBuild integration, rather than rediscovering the whole project on every warm request.

### Bazel and Buck2: Skyframe/action graphs, remote reuse, and daemons

Bazel's Skyframe documentation describes an incremental evaluation graph whose nodes represent computations and whose dependencies determine invalidation. Bazel's remote-cache documentation describes cacheable actions keyed by inputs and action configuration. Buck2 documents a daemon and an incremental build model built around a graph of rules and cached computation.

Evidence: [Bazel Skyframe](https://bazel.build/reference/skyframe), [Bazel remote caching](https://bazel.build/remote/caching), [Buck2 incremental builds](https://buck2.build/docs/concepts/incremental/), [Buck2 daemon](https://buck2.build/docs/concepts/daemon/).

These systems show that “incremental compiler” and “incremental build” are separate layers. FSharp2 needs both: semantic compiler queries inside a project, and MSBuild/project action keys across projects. A daemon alone only removes startup; a build cache alone only reuses complete actions.

### MSBuild: timestamp/output inference is useful but insufficient

MSBuild's official incremental-build documentation describes target skipping through inputs and outputs. This is an important outer guard, but it cannot answer whether a changed middle F# file changed an exported declaration or only an implementation. FSharp2 needs a semantic layer below MSBuild's file/output checks.

Evidence: [MSBuild incremental builds](https://learn.microsoft.com/en-us/visualstudio/msbuild/incremental-builds).

## Applying the lessons to FSharp2

### Cold start: make the first invocation a bounded, mostly static path

Recommended cold-start architecture:

- Ship a NativeAOT host with a narrow dependency closure and no runtime assembly loading. Keep protocol, cache, diagnostics, and metadata-reader code statically reachable.
- Prefer a persistent compiler service for normal editor/MSBuild sessions. Make the one-shot host a thin client or a process that can optionally host the same service core.
- Benchmark argument, response-file, and path handling on representative inputs. Use allocation-light or span-oriented code only where it outperforms ordinary managed alternatives without sacrificing correctness, NativeAOT compatibility, or maintainability; avoid repeated normalization and splitting where measurement identifies it as material.
- Build a versioned framework/reference index once per SDK/reference-set fingerprint. Read assembly metadata lazily and cache only the tables needed by name/type resolution.
- Persist a project-input manifest: ordered source paths, content hashes, flags, target framework, references, analyzers/providers where supported, and compiler schema/version. Do not make the compiler rediscover or rehash irrelevant files.
- Record phase timings and cache hit/miss/bypass counters. Any invocation over three seconds should explain startup, graph, parse, check, optimize, emit, and I/O costs in the structured trace already required by the repository.

Evidence: NativeAOT constraints and the IcedTasks baseline are local research evidence; the daemon/query/cache pattern is supported by the Rust, Zig, Bazel, Buck2, and Go sources above. The specific NativeAOT implementation plan is a FSharp2 recommendation.

### Middle-of-DAG edits: use two graphs and two fingerprints

F# source order means a file's earlier declarations are inputs to later declarations even when a conventional import graph does not show the edge. Model that explicitly:

```text
source file -> ordered declaration checkpoints -> semantic queries -> emitted fragments
                                  \-> exported semantic fingerprint
project -> ordered source list + reference fingerprints + options -> artifact
```

For each declaration checkpoint, retain at least:

- parsed syntax/green-tree identity;
- bound names and resolved declaration identities;
- inferred type and member shape;
- diagnostics and source ranges;
- a compact exported fingerprint; and
- dependencies on imported declarations and compiler/environment inputs.

For each project, retain a public/API fingerprint based on F# metadata and the emitted reference surface. Include constructs whose implementation can affect consumers: inline bodies, statically resolved type parameters, active patterns, type abbreviations, union cases, accessibility, attributes, constants, and generated members. Treat unknown or reflection/provider-sensitive effects as an invalidation widening point.

On a middle-file edit:

1. Hash and parse only changed content; reuse unchanged syntax trees.
2. Recheck from the earliest changed source-order checkpoint in that file, reusing earlier checkpoints.
3. Compare old and new declaration fingerprints. If a declaration's exported meaning is unchanged, retain its downstream query results even if its implementation or source span changed.
4. Propagate invalidation over declaration-level edges. Recheck only consumers whose queried input fingerprint changed.
5. Re-emit changed method fragments and relink the artifact. If the project API fingerprint is unchanged, downstream projects remain valid; if it changed, invalidate only projects that consume the changed API.
6. If the compiler cannot prove equivalence, widen to the containing file, then project, and report the widening reason in the trace.

This is stronger than file-level incremental compilation and safer than guessing from syntax. It also preserves F#'s order semantics: a later file is never reused against a stale earlier checkpoint.

### Project boundaries

Use two artifacts and two validity decisions:

- an internal incremental state containing declaration/query caches for warm work inside the project;
- a stable reference/API summary containing the public F# metadata surface and all consumer-visible implementation payloads (for example inline code).

An implementation-only edit can reuse downstream project compilation when the reference/API summary is byte- or canonically-semantic-identical and the project options/reference set are unchanged. A public type, union case, inline body, constraint, attribute, or generated member change must invalidate consumers that observe it. This avoids the common mistake of treating “DLL timestamp changed” as “every dependent project must be recompiled.”

### Parallelism without violating F# order

Preserve sequential checking at the declaration/file boundary where the language requires it, but parallelize independent work: parsing unchanged/new files, metadata indexing, independent project graph nodes, code generation for already-checked fragments, and cache reads. Use a work-stealing scheduler only after the dependency graph proves independence. Parallelism is a multiplier; it cannot compensate for rebuilding invalidated semantic state.

## Proposed proof plan

The performance work should be accepted only with correctness evidence:

- Cold: bypass live and disk compiler state explicitly, publish/run the NativeAOT host, and measure Compiler Target Invocation time. Test first on the pinned IcedTasks project/TFM matrix selected by [ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md).
- Warm no-op control: prove that the compiler service was contacted and that retained state was checked; an MSBuild `OutputsUpToDate` skip is not a warm compilation.
- Warm implementation edit: change a body whose exported fingerprint is unchanged; prove the changed file is rechecked/emitted and downstream files/projects are reused.
- Warm middle-DAG API edit: change a declaration and prove only the transitive declaration/project consumers recheck.
- Adversarial edits: inline body, union-case addition, type abbreviation, active pattern, SRTP constraint, attribute, generated member, provider-sensitive code, and a deliberately unknown effect. Each must either invalidate the required consumers or fail closed by widening.
- Compare compiler outputs and diagnostics against the Compatibility Oracle; compare cache hit/miss counters and per-phase traces, not just wall time.

The minimum useful trace fields are cache key/schema, node kind, old/new fingerprint, invalidation reason, reused/rechecked/emitted status, dependency count, and elapsed time. This turns a failed three-second target into an actionable dependency or startup diagnosis.

The exact pinned IcedTasks edits, forced compiler-contact/replay/recovery controls, complete action-key inputs, and reconciled per-node evidence are fixed by [ADR 0022](../adr/0022-define-the-warm-edit-corpus-and-cache-evidence-contract.md) and the [issue #19 research](warm-edit-corpus-and-cache-evidence-contract.md).

The corresponding reference-metadata authority split is fixed by [ADR 0023](../adr/0023-keep-the-fsharp-metadata-harness-inspector-envelope-only.md) and the [issue #17 research](independent-fsharp-metadata-decoder.md): the harness owns exact envelope inspection, four-way producer/consumer behavior remains the compatibility authority, and the production NativeAOT importer/reference index/API fingerprints remain separate compiler components.

## What not to copy blindly

- Go's package-level export boundary is excellent for a simple package graph but too coarse for declaration-level F# reuse.
- Zig's incremental state and Odin's simple compiler model are valuable architectural evidence, not proof that their invalidation rules preserve F# semantics.
- Build-system remote caches can reproduce complete actions, but they do not replace semantic dependency tracking inside a project.
- Timestamp/output checks are a cheap outer optimization, not evidence that F# semantic reuse is safe.
- NativeAOT reduces startup and deployment overhead but does not automatically provide an incremental compiler; the query graph and cache schema remain the core design.

## Source index

All external claims above point to first-party documentation or source repositories. Local claims point to the checked-in FSharp2 context/research/ADR documents.

- [FSharp2 context](../../../CONTEXT.md)
- [ADR 0018: declaration-granularity incremental dependencies](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md)
- [IcedTasks official compiler target baseline](icedtasks-official-compiler-target-baseline.md)
- [NativeAOT compiler-host constraints](dotnet-10-nativeaot-compiler-host-constraints.md)
- [Differential compatibility/performance harness](differential-compatibility-performance-harness.md)
