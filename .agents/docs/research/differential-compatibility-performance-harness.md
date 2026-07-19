# Differential compatibility and performance harness

Status: harness decision for [Define the differential compatibility and performance harness][issue-5], supporting the destination in [Build FSharp2: a NativeAOT, incremental, drop-in F# compiler][issue-1]

Observed: 2026-07-18 on Windows x64

Evidence baseline: [.NET 10 compiler/MSBuild contract](dotnet-10-fsharp-compiler-msbuild-contract.md), [official IcedTasks target baseline](icedtasks-official-compiler-target-baseline.md), and [.NET 10 NativeAOT compiler-host constraints](dotnet-10-nativeaot-compiler-host-constraints.md)

Governing decisions: [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md), [ADR 0004](../adr/0004-target-compiler-and-msbuild-compatibility.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0006](../adr/0006-require-behavioral-not-byte-identical-output.md), [ADR 0008](../adr/0008-deliver-through-experimental-vertical-milestones.md), [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), [ADR 0016](../adr/0016-explain-slow-compilations-with-structured-traces.md), [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md), and [ADR 0019](../adr/0019-distribute-opt-in-msbuild-integration-by-nuget.md)

## Answer

FSharp2 should build one versioned, test-only **differential harness** with four independently reportable gates:

1. **Compiler-target compatibility** runs the Compatibility Oracle and FSharp2 through the same evaluated `CoreCompile` boundary in isolated process/output/cache roots, captures each physical invocation, and compares diagnostics and compiler-owned artifacts.
2. **Consumer compatibility** validates API/metadata/PDB semantics and exercises both outputs through runtime, project-reference, package, debugger-data, and bidirectional F# consumption.
3. **NativeAOT compatibility** separately proves the native FSharp2 host and NativeAOT consumption of FSharp2-emitted managed output.
4. **Compiler-target performance** measures uninstrumented, real Cold and Warm Compiler Target Invocations. It never includes restore/evaluation and never calls an unchanged skipped target "warm."

A case receives one of exactly four verdicts:

| Verdict | Meaning |
| --- | --- |
| `pass` | Every comparator and gate required by the case's declared compatibility envelope passed, or a difference matched a narrow, active, reviewed comparison-policy rule. |
| `fail` | A required observable diverged, an artifact was invalid, fallback evidence fired, deterministic repeatability failed, or a required absolute/relative performance gate failed. |
| `unsupported` | The case is outside the declared Experimental Vertical Milestone and FSharp2 rejected it before producing successful output, with the declared unsupported-envelope diagnostic and no oracle fallback. This is expected behavior but **not compatibility coverage**. |
| `infra-error` | The requested comparison was not made: setup, restore, runner, tool, timeout cleanup, missing evidence, or measurement validity failed. It is never converted to pass or silently discarded. |

The overall run succeeds only when every required in-envelope cell is `pass`, every required out-of-envelope cell produces its declared `unsupported` verdict, and no required matrix cell is missing or `infra-error`. An `unsupported` verdict is expected behavior only for a manifest cell explicitly outside the current envelope; it is never counted as compatibility coverage. A new divergence defaults to `fail`. Updating oracle goldens or adding an intentional-difference rule is a reviewed versioned change, never an automatic test update.

The harness deliberately requires behavioral compatibility rather than byte-identical output. Cross-compiler metadata tokens, heap offsets, row order, method-body IL, MVIDs, PE timestamps, PDB ids, and artifact hashes are not exact by default. The harness compares their canonical semantic identity and observable behavior. In contrast, two deterministic runs of **the same compiler** with identical/path-mapped inputs must reproduce byte-identical compiler-owned artifacts, including its own MVID and PDB identity.

## Comparison classes

Every result field declares one of these comparison classes; there is no unrecorded "close enough" fallback.

| Class | Rule | Examples |
| --- | --- | --- |
| **Exact** | Values must be identical after decoding, with no policy normalization. | Exit code; adjusted diagnostic severity; diagnostic number; stream; source span numbers; assembly identity; resource logical name/visibility; requested artifact set; target skip state. |
| **Normalized exact** | Apply only named, deterministic transformations, then require equality. Preserve raw values in the bundle. | Replace isolated roots with `<ROOT>`, canonicalize path separators/case according to recorded filesystem semantics, normalize CRLF/LF, strip ANSI only when color is not under test, parse and canonicalize JSON/XML. |
| **Canonical semantic** | Decode representation-specific handles into stable names/signatures/relationships and compare that model. | ECMA-335 definitions/references, public API, PDB methods joined to semantic method identity, SourceLink mappings, local/import scope trees. |
| **Behavioral** | Run the same declared observer and compare its structured observations. | Return values, console streams, exception/resource behavior, project-reference/pack/use, native consumer output, debugger-visible source mapping. |
| **Within-compiler exact** | Repeat one compiler under deterministic inputs and compare artifact bytes/hashes to itself, not to the other compiler. | DLL, reference DLL, portable/embedded PDB, XML docs/signature/resource outputs. |
| **Measured** | Compare a declared statistic from valid uninstrumented samples under one policy and machine class. | `CoreCompile` duration, service/RPC duration, threshold-exceedance count, cold/warm medians and p95. |

Normalization is part of the versioned comparison policy. A comparator emits raw value, normalized value, rule id, and rule version so a policy change cannot rewrite history.

## Harness topology and trust boundary

The harness is a managed test/tool graph. It may reference SDK-owned ApiCompat, `System.Reflection.Metadata`, ILVerify, an MSBuild binlog reader, test frameworks, and oracle helpers. None may become reachable from a production host or compiler project.

The NativeAOT constraints already fix the production closure: official `fsc`/FCS, oracle adapters, harnesses, and benchmark frameworks are test-only and the native compiler reads target assemblies as metadata rather than loading them. The harness enforces that boundary by:

1. inspecting evaluated `ProjectReference`/`PackageReference`, `project.assets.json`, native publish inputs, and installed package payloads for forbidden production dependencies;
2. launching oracle and FSharp2 as separate child processes with distinct working, temporary, output, and cache roots; and
3. making no-fallback evidence executable.

For the third check, the FSharp2 lane overrides every standard official-compiler seam (`FSharpBuildAssemblyFile`, `FscToolPath`, `FscToolExe` where safe, and `DotnetFscCompilerPath`) with a run-specific sentinel that records invocation and exits nonzero. The harness records the selected compiler executable hash and complete descendant process lineage. A FSharp2 success fails if the sentinel ran, a descendant command line selected SDK `fsc.dll`, or the native host/package graph contains FCS/official compiler assemblies. Matching output alone cannot prove [no production fallback](../adr/0010-forbid-production-fallback-to-the-official-compiler.md).

The two compiler lanes share only immutable, hash-verified source/reference/package inputs. They never share `obj`, `bin`, temp, service, or mutable cache directories. Case materialization creates:

```text
<run>/
  corpus/                 immutable materialized source snapshot
  oracle/{work,tmp,obj,bin}
  fsharp2/{work,tmp,obj,bin,cache}
  consumers/<matrix-cell>/
  evidence/raw/
  evidence/shareable/
```

Both work roots map to the same logical source root with `PathMap`. This removes physical checkout paths without hiding real source-order, reference, resource, culture, or option differences. Restore happens before comparison into a pinned package cache; lock/assets hashes become manifest evidence.

## Corpus and compatibility-envelope manifest

The source of truth is a checked-in UTF-8 JSON document validated against a checked-in versioned schema. JSON avoids a new YAML parser in the harness graph and permits canonical hashing. Comments live in adjacent Markdown, not in the machine contract.

The top-level manifest contains:

```json
{
  "schemaVersion": 1,
  "corpusVersion": "sha256:...",
  "comparisonPolicy": { "id": "compat-v1", "sha256": "..." },
  "oraclePolicy": { "sdkBand": "10.0", "refresh": "reviewed-servicing-update" },
  "matrix": {
    "hostRids": ["win-x64", "win-arm64", "linux-x64", "linux-arm64", "osx-x64", "osx-arm64"],
    "cultures": ["en-US", "<declared-non-default>", "<missing-culture-fallback>"]
  },
  "cases": []
}
```

Each case contains at least:

| Field | Required content |
| --- | --- |
| Identity | Stable `caseId`, title, owner, rationale, tags, and the issue/ADR requirement it traces. |
| Source snapshot | Repository URL, commit and tree, project/subdirectory, ordered file list, per-file SHA-256, generated inputs or generation command, and dirty-tree prohibition. Inline fixtures record content hashes. |
| Envelope | Envelope id/version, `supported` or `unsupported`, language version/features, target/profile, TFM, configuration, compiler/task options, references/tools/providers/resources/signing/debug modes, and explicit exclusions. |
| Evaluation | Project path, target, global properties, expected ordered `Compile`/reference/resource items, expected compiler-owned outputs, and SDK-owned downstream outputs. |
| Comparison profile | Required diagnostic, PE/API, metadata, IL, F# metadata, PDB, runtime, downstream, determinism, cleanup, NativeAOT, and performance comparators. |
| Runtime observer | Driver project/hash, entry point, arguments/stdin/environment, timeout, cultures, structured assertions, expected resources/exceptions, and normalization rules. |
| Edit sequence | Baseline plus content-addressed controlled edits, expected semantic impact/invalidation class, output expectations, and restore action. This drives Warm Compilation; a timestamp-only touch is insufficient. |
| Matrix selectors | Host OS/RID/architecture, downstream runtime RID, TFM, UI culture, path-shape, Debug/Release, portable/embedded PDB, and required/excluded cells. |
| Policy bounds | Oracle SDK identity range, allowed comparison-policy ids, expected unsupported diagnostic if applicable, and expiry/review owner for any intentional difference. |

A compatibility envelope narrows **cases and values**, not the path through the compiler. Every `supported` case traverses evaluation, invocation, parsing, checking, requested optimization, emission, loading/consumption, and execution where applicable. Source order, resolved-reference consumption, task failure propagation, output ownership, no fallback, and requested artifact completeness are never optional.

[Select the first IcedTasks vertical slice][issue-6] owns the first concrete IcedTasks case selection. This note fixes the schema and rules, not which IcedTasks declarations form that slice.

## Oracle identity and invocation capture

Every run resolves and records:

- `dotnet --info`, SDK/MSBuild/runtime version and repository commit;
- `fsc` banner/product/file version and SHA-256;
- hashes of `fsc.dll`, `FSharp.Build.dll`, `FSharp.Compiler.Service.dll`, `Microsoft.FSharp.Targets`, `Microsoft.FSharp.NetSdk.props`, and `Microsoft.FSharp.NetSdk.targets`;
- FSharp.Core version/hash;
- OS, architecture, RID, cultures, encoding, environment allowlist, working directory, filesystem case behavior, CPU/memory/power-plan, and runner image; and
- FSharp2 source commit, package/host/compiler/protocol hashes, native host RID, service/cache schema versions, and comparison-tool versions.

The repo requests SDK `10.0.100` with feature roll-forward, so servicing can change the selected oracle; record the resolved SDK rather than treating `global.json` as the exact compiler build.[^repo-sdk]

For each compiler lane, the harness:

1. sets `MSBUILDPRESERVETOOLTEMPFILES=1` and lane-specific `TEMP`/`TMP` before starting a fresh MSBuild process;
2. invokes one inner project/TFM with `-t:Compile`, a run-specific `-bl`, `-maxcpucount:1`, `-nodeReuse:false`, and `BuildProjectReferences=false`;
3. replays the binlog with the first-party `BinaryLogReplayEventSource` and pairs target/task events by `BuildEventContext`;
4. preserves the physical response file or FSharp2 protocol request after argument construction;
5. records evaluated task properties/items, ordered sources/references/resources, returned `FscCommandLineArgs`, physical arguments, selected executable, working directory/environment, output paths, and target/task events; and
6. canonicalizes paths only in the derived comparison record.

The official returned `FscCommandLineArgs` omits later `--refout`/`--refonly` construction while the physical response contains it. The binlog/response pair is authoritative; neither alone is complete.[^fsc-capture]

The existing FAKE helpers disable internal binlogs and tests use `--no-build`, so the harness needs a separate capture lane rather than the current helpers.[^repo-capture-gap] The current workflow watches `main` while the repository base is `master`; absence of checks is evidence to fix, not a passing harness result.[^repo-ci-gap]

MSBuild binlogs may contain command lines, environment-derived properties, paths, and imported project content.[^binlog-content] Raw bundles are potentially sensitive and remain controlled/local. A shareable export uses an explicit allowlist/redaction policy, carries its own hash and redaction report, and never replaces the authoritative raw result.

## Diagnostics, streams, and exit

The diagnostic comparator captures stdout and stderr independently as bytes plus decoded text. For a pinned culture/encoding/color profile it compares:

- diagnostic identity/code, adjusted severity, subcategory, and related-diagnostic relationship;
- normalized source document and exact one-based start/end line and column;
- message text exactly after only declared root, newline, and ANSI normalization;
- ordering and stream (`stdout` versus `stderr`);
- process exit, MSBuild task/target result, cancellation/timeout classification; and
- suppressed, promoted, warning-level, language-version, and culture variants as separate cases.

Default target cases use `--fullpaths --flaterrors --consolecolors-` and pinned `PreferredUILang`. Paths map to logical roots; wording is otherwise exact for that oracle identity. There is no fuzzy substring/edit-distance comparator. Servicing/culture refresh changes text only through reviewed oracle evidence.

Help/version remain stdout; diagnostics remain stderr; warning promotion changes effective severity and exit. Raw bytes remain so encoding or stream regressions cannot be hidden.[^diagnostic-contract]

An out-of-envelope case does not compare a FSharp2-specific unsupported message to oracle success. It requires the declared unsupported identity, nonzero exit, no successful output, no fallback sentinel/process, and cleanup policy. It yields `unsupported`, never `pass`.

## Managed artifact and ECMA-335 comparison

Every managed PE first passes `PEReader`/`MetadataReader` structural reading. ILVerify then runs against the exact invocation reference closure. When the oracle verifies cleanly or the case declares verifiable IL, FSharp2 must verify cleanly; a case whose oracle intentionally emits unverifiable IL instead compares the exact verifier findings under a reviewed case policy. ILVerify is evidence about metadata/IL validity, not an API or behavior oracle.[^ilverify]

The public API lane uses the **SDK-shipped `Microsoft.DotNet.ApiCompatibility` task assembly pinned by the oracle manifest**, not a floating latest package. Assembly validation runs both directions:

- oracle contract -> FSharp2 implementation detects missing/incompatible API; and
- FSharp2 contract -> oracle implementation detects unexpected additions or shape differences.

ApiCompat is an independent general-purpose check. Its suppressions are versioned policy inputs. It cannot see F# opaque resources, PDB semantics, runtime behavior, or every ECMA-335 detail, so it does not replace the harness-owned model.[^api-compat]

The canonical metadata fingerprint follows ECMA-335 identities, not row numbers.[^ecma-335] It includes:

- assembly/module name and identity, culture, version, public key/token, flags, hash algorithm, entry point, target kind, machine, CorFlags, subsystem, and relevant PE characteristics;
- type namespace/nesting/name/generic arity, visibility, layout, base, interfaces, generic constraints, method implementations, declarative security, and layout;
- field/method/property/event semantic keys, decoded signatures, accessibility/attributes/implementation flags, parameter/default/marshal data, overrides, P/Invoke, and custom modifiers;
- references and specifications decoded to stable semantic identities;
- custom attributes keyed by constructor identity with canonical fixed/named arguments when valid, retaining raw blobs;
- resource logical name, visibility, embedded/linked form, and content policy;
- reference-assembly and implementation-assembly surfaces separately; and
- signing/public-key/strong-name semantics and requested XML documentation/signature artifacts.

Metadata tokens, heap offsets, row order, MVID, PE timestamp, and raw method-body bytes remain audit data but are excluded from default cross identity. Different valid IL/optimization choices are allowed. Bodies must verify, reference the correct target closure, preserve observable requested modes, and pass runtime/downstream behavior.

User-supplied resource payloads are cross-compiler byte-exact when merely embedded/linked. Compiler-generated resources use their declared semantic policy.

## F# metadata and cross-compiler consumption

The implementation must contain expected logical F# metadata resources (`FSharpSignatureData*` and `FSharpOptimizationData*` forms for the declared mode), with correct visibility/presence and compatible `--nointerfacedata`, `--nooptimizationdata`, and `--compressmetadata` behavior. Opaque payload bytes are:

- exact within deterministic repeats of one compiler;
- retained and hashed cross-compiler; but
- not required to equal oracle bytes merely because both are valid.

Cross-compiler validity uses this producer/consumer matrix where applicable:

| Producer artifact | Consumer compiler |
| --- | --- |
| Oracle | Oracle |
| Oracle | FSharp2 |
| FSharp2 | Oracle |
| FSharp2 | FSharp2 |

Consumers compile against producer implementation/reference artifacts without rebuilding the producer. Compare consumer diagnostics/API/runtime behavior and requested optimization-mode results. This catches signature/optimization-resource interoperability bidirectionally without imposing opaque byte identity. Exact resource names/presence remain required. Add a standard C# consumer where the API is managed-visible.

## Portable and embedded PDBs, SourceLink, and debugging data

`PEReader.ReadDebugDirectory` identifies CodeView, checksum, reproducible-build, and embedded-PDB entries; portable PDB readers expose documents, method debug information, sequence points, scopes/imports, state-machine mapping, and custom debug information.[^pdb-api]

For each requested portable or embedded mode, the harness:

1. locates the associated/embedded PDB through the PE debug directory and validates requested shape;
2. joins each PDB method to its PE method and canonical semantic method identity—never raw handle/token;
3. canonicalizes documents by logical path, language, hash algorithm/checksum, and embedded-source content/checksum;
4. compares ordered document transitions, visible source spans, hidden markers, and relative point order while retaining raw IL offsets;
5. compares local-scope nesting, local names/index/attributes, constants, import chains/imports, kickoff/move-next relationships, async stepping/hoisted-scope data, and default namespace where emitted;
6. parses SourceLink JSON and compares canonical document-pattern -> URL mappings; optional network verification fetches and checksum-validates source; and
7. runs declared stack/source-location and breakpoint-source probes for representative cases.

Raw IL offsets, method handles, PDB id/stamp, CodeView path, and PDB hash are not cross exact because valid IL layout may differ. Documents/checksums, visible/hidden spans, scope/import semantics, state-machine association, embedded source, and SourceLink resolution are requirements. Within each compiler's deterministic repeat, PE debug directory and PDB bytes/ids are identical.

## Runtime, exceptions, and resources

Runtime observers execute out of process against each output under the same runtime/RID/culture and identical driver hash. Each case defines structured JSON plus raw stdout/stderr/exit. Compare:

- return values and public data using a case-owned canonical serializer;
- stdout/stderr, exit, cancellation, and timeout behavior;
- exception type, parameter, inner/aggregate shape, HResult when contractual, and message under pinned culture;
- declared stack/source frames after path/address normalization, not raw IL/native offsets;
- resource values/fallback across default, one non-default culture, and missing-culture fallback;
- module/type initialization and observable side effects; and
- repeated/concurrent runs when async, disposal, determinism, or thread safety is under test.

Timestamps, random values, PIDs, addresses, scheduling order, and stack offsets normalize only when the case declares why. A crash, hang, malformed result, resource failure, or different exception is `fail`. Use `infra-error` only when evidence proves the observer/runner—not the compiled program—failed.

## Deterministic-repeat gate

For every `Deterministic=true` case, each compiler runs at least two clean invocations in different roots with identical content, logical paths, references, options, environment/culture, and path map. No output/compiler cache crosses repeats.

For each compiler independently require:

- identical requested artifact set;
- byte-identical implementation/reference DLL, PDB, XML docs/signature, and generated-resource outputs;
- identical SHA-256, MVID, debug-directory identity, PDB id, diagnostics, exit, and fingerprints; and
- no undeclared physical path in metadata, PDB, SourceLink, or embedded source.

Oracle A compares only to oracle B; FSharp2 A only to FSharp2 B. Cross comparison remains semantic/behavioral. Self-mismatch is correctness failure even if one runtime probe agrees.

## Downstream build, project-reference, package, and use

After direct comparison:

1. build consumers against each implementation and reference assembly without rebuilding producer;
2. execute the four-way F# matrix plus standard C# consumer;
3. build a real `ProjectReference` graph, confirming compile-time reference selection and runtime implementation copy;
4. `pack` each producer, restore into a clean cache, build/run/test a consumer, and compare package roles semantically rather than nupkg ZIP bytes;
5. validate docs, resources, SourceLink, signing identity, and SDK-owned `.deps.json`/copy shape at their owning layer; and
6. run Debug/Release and requested optimization/tail-call/debug variants separately.

Compiler-owned outputs compare directly; SDK/package layers compare final usability/shape. Do not fail FSharp2 for SDK ZIP timestamps or allow a missing reference assembly because direct runtime load succeeded.

## Two NativeAOT gates

NativeAOT remains two independent matrices.

### Gate A: native compiler host

For `win-x64`, `win-arm64`, `linux-x64`, `linux-arm64`, `osx-x64`, and `osx-arm64`:

- publish with `PublishAot=true` and strict unsuppressed ILLink/ILCompiler warnings;
- inspect production/native inputs for forbidden oracle/test dependencies;
- install/restore the real RID payload and launch it directly from an unrelated working directory;
- run a real declared compilation through standalone and, once present, service paths;
- assert dynamic code support false, valid requested outputs, and no fallback evidence; and
- record native toolchain, OS floor/dependencies, symbols, executable hash, and lineage.

Host RID is the build-machine RID, independent of output TFM/runtime RID.

### Gate B: NativeAOT consumption of emitted output

For each declared envelope/downstream RID:

- consume the FSharp2-emitted managed artifact from a standard SDK `net10.0` driver;
- publish it with strict `PublishAot=true` warnings;
- run the native consumer and compare API/runtime/exception/resource observations with oracle output;
- include negative AOT-warning tracers; and
- prove emitted metadata references target inputs, not host assemblies.

A native host that does not compile, a managed output merely loaded by CoreCLR, or one combined checkbox cannot satisfy both gates.[^nativeaot-gates]

## Cold and Warm Compiler Target Invocation timing

Correctness gates run before performance. A failing result has no publishable performance win.

### Canonical interval and validity

Canonical duration is one `CoreCompile` `TargetStarted` to matching `TargetFinished`, paired by `BuildEventContext`, for one project/TFM. Compiler task/RPC and whole process wall are secondary. Evaluation, restore, generated-input targets, analyzers, copy, pack, test, and outer startup are excluded.[^msbuild-evaluation]

A sample is valid only when:

- restore completed before the series and timed command did not request it;
- exactly one non-skipped `CoreCompile` succeeded;
- `SkipCompilerExecution=false`;
- exactly one real oracle `Fsc` process or FSharp2 request executed;
- expected implementation/reference outputs refreshed and validated;
- binlog and service/protocol evidence agree;
- no correctness/fallback/timeout/noise/evidence failure occurred; and
- instrumentation is `none`.

`TargetSkippedEventArgs.SkipReason=OutputsUpToDate` plus zero compiler execution is the unchanged no-op control. Output inference means a target node alone is insufficient. A skipped build is never Warm Compilation.[^msbuild-skip]

Baseline command:

```powershell
dotnet msbuild <project> `
  -t:Compile `
  -p:TargetFramework=<tfm> `
  -p:Configuration=Release `
  -p:BuildProjectReferences=false `
  -maxcpucount:1 `
  -nodeReuse:false `
  -nologo `
  -verbosity:minimal `
  -bl:<run.binlog>
```

### Cold scenario

For each independent cold FSharp2 sample:

1. stop service and verify exit;
2. enable an explicit cold mode that bypasses both live memory state and content-addressed disk-cache lookups, regardless of cache contents;
3. delete only outputs required to force `CoreCompile`;
4. start the measured invocation in that cold mode; and
5. prove both cache levels were bypassed and no compiler state was reused through counters.

Oracle CLI `Fsc` starts a fresh process and reaches the non-incremental entry point. Later samples may benefit from OS/filesystem caches; call it compiler-state cold, not OS-page-cache cold.[^oracle-cold]

`dotnet build-server shutdown` and `-nodeReuse:false` do not prove custom service/cache reset. An empty cache epoch proves only that no prior entry was present, not that lookup was bypassed. Record service PID/start, explicit cold-bypass state, cache epoch/schema/root, compiler/protocol hash, and hit/miss/bypass/reuse counters.

### Warm scenario

Each independent warm FSharp2 sample:

1. resets service/cache;
2. performs one untimed prime compile;
3. retains the same healthy service PID/cache epoch;
4. applies one manifest-defined content edit with known invalidation footprint;
5. measures a real target through service IPC, invalidation, checking/emission, atomic output commit, response, and target finish; and
6. validates edited outputs and counters.

Timestamp-only touch or identical replay may be a separate cache-hit scenario but cannot replace changed-input Warm. Oracle supplies correctness for edited source but has no retained-state warm comparator; FSharp2's sub-second goal is independent.

### Sample order, statistics, and noise

Start with 15 valid samples for each applicable compiler/TFM/scenario: paired oracle/FSharp2 samples for Cold Compilation, and FSharp2 samples for retained-state Warm Compilation. Oracle edited-source correctness remains required, but it is not mislabeled as a retained-state warm series. Interleave paired comparisons using a recorded random seed and rotate TFM/edit order per round. Use one invocation at a time on a quiet fixed-power machine. Keep SDK, binlog, affinity policy, service mode, package cache, filesystem, security-scanner policy, and runner image constant.

Persist every attempt, including invalid rows, timeouts, and upper outliers. Never silently retry, trim, winsorize, or delete. A rerun is a linked new attempt.

Report:

- valid count and invalid reasons;
- minimum, median, mean, maximum, median absolute deviation, and threshold-exceedance count;
- p95 only with at least 20 samples; release uses 30+;
- median/p95 delta and ratio versus same-machine baseline; and
- seeded bootstrap 95% confidence interval for median delta/ratio when gating.

Absolute controlled-runner gates are cold median `<= 3,000 ms` and warm median `< 1,000 ms`; release also applies these to p95 with 30+ samples. Every invocation over three seconds emits the Performance Tripwire warning/structured trace and is counted; one noisy local exceedance does not alter correctness unless CI opts in.

Initial relative regression is `>= 10%` **and** `>= 50 ms` median slowdown with bootstrap interval wholly above zero, reviewed after dedicated-runner variance. Relative speedup never excuses an absolute failure, and oracle slowness never relaxes budgets.

Hosted runners run correctness, not gating performance. Phase/allocation/EventPipe/working-set/traces use a separate `instrumentation != none` profile. They explain but never replace/mix with uninstrumented samples; no guessed cost is subtracted.[^benchmark-method]

## Failure cleanup and unsupported envelopes

Each compiler gets a fresh output root. Before/after, record recursive path/type/length/timestamp (audit only)/SHA-256/open status/owner. Supported negative cases compare oracle/FSharp2 leftovers by normalized path and content policy. Do not impose blanket cleanup: the oracle can leave reference/XML outputs after late failure.

On timeout/cancellation/harness failure:

1. stop request;
2. kill run-scoped tree/service after graceful deadline;
3. poison and never reuse cache epoch;
4. collect streams, binlog, protocol, lineage, file manifest, and dumps;
5. hash/copy evidence; then
6. remove only the verified run root.

Cleanup inability is `infra-error`, not success.

Unsupported input must be detected before successful publication, emit declared FSharp2 unsupported identity, exit nonzero, leave only declared failure evidence, and never invoke/load oracle. As support expands, changing `unsupported` to `supported` adds full comparator coverage rather than deleting the negative.

## Result and reproduction-bundle schemas

`run-result.json` contains:

| Section | Content |
| --- | --- |
| Identity | Schema/run/case/attempt ids, UTC, harness/source/policy/corpus hashes, parent run. |
| Environment | OS/image/RID/architecture, CPU/memory/power, filesystem, SDK/runtime/tools, cultures/encoding, environment and package/lock hashes. |
| Compiler | Role, executable/package/source hash, lineage, graph result, sentinel, service PID/epoch/protocol/cache counters. |
| Invocation | Evaluation, raw/canonical arguments, ordered inputs/hashes, roots, target/task/RPC events, skip reason. |
| Observations | Diagnostics/streams/exit, file hashes, PE/API/metadata/IL/PDB/F# fingerprints, runtime/downstream/AOT. |
| Determinism | Repeat ids and per-compiler artifact/hash/identity comparison. |
| Performance | Scenario/edit, instrumentation, validity, raw durations, statistics, tripwire/trace ids, baseline. |
| Comparisons | Comparator id/version/class, raw/normalized values, diff, policy rule, pass/fail. |
| Verdict | Four-state verdict, reasons, missing cells, evidence bundle hash. |

Bundle:

```text
resolved-manifest.json
comparison-policy.json
environment.json
commands.json
inputs/{manifest.json,patches/}
oracle/{evaluation.json,invocation.json,response.rsp,stdout.bin,stderr.bin,processes.json}
fsharp2/{evaluation.json,invocation.json,request.bin,stdout.bin,stderr.bin,processes.json}
artifacts/{raw-manifest.json,semantic-fingerprints/,pdb/,api-compat/,ilverify/}
consumers/<cell>/
nativeaot/<gate>/<rid>/
timing/{attempts.csv,summary.json,traces/}
comparison.json
run-result.json
bundle.sha256
redaction-report.json
```

Repro requires exact source/commit/tree and ordered-input hashes, SDK/oracle/compiler/tool identities, assets/locks, evaluation/physical invocation, culture/environment/path map, raw streams, artifact hashes, policy, lineage, cleanup, and verdict. A prose command is insufficient.

One-command replay by `caseId`/`runId` verifies hashes first and emits a linked result; it never overwrites original.

## CI matrix and scheduling

| Lane | Matrix/purpose | Performance |
| --- | --- | --- |
| Pull request | x64 Windows/Linux/macOS; selected positive/unsupported/negative; affected Debug/Release; portable PDB; deterministic repeat; graph/sentinel. | Smoke numbers only. |
| Nightly | Full declared corpus across IcedTasks `netstandard2.0`, `netstandard2.1`, `net6.0`, `net9.0`; language/options, portable/embedded PDB, paths, en-US + non-default + fallback, consumers/cleanup. | 15 samples on characterized runner. |
| Release | All declared cells, clean package use, signing/resources, deterministic repeats, eight host RIDs and declared downstream AOT RIDs on target hardware/OS floors. | 30+, absolute median/p95 and relative policy. |
| Oracle refresh | Candidate latest .NET 10 servicing SDK versus prior over complete corpus before manifest update. | Rebaseline only after correctness review. |
| Diagnostic | Tripwire-selected phase/allocation/GC/working-set/structured traces. | Instrumented, non-comparable. |

Eight host RIDs are exhaustive: Windows and macOS x64/Arm64 plus separate glibc and musl Linux x64/Arm64 payloads. Cases declare applicable TFM/culture/debug/runtime cells so the harness does not claim an unexecuted Cartesian product. [Select the first IcedTasks vertical slice][issue-6] supplies the first envelope; the final gate expands to the then-current .NET 10 surface.

Implementation first corrects current `main`/`master` CI trigger mismatch.[^repo-ci-gap]

## Triage and intentional differences

Classify a failed comparison, without changing verdict, as `fsharp2-correctness`, `fsharp2-performance`, `fallback`, `determinism`, `harness-comparator`, `infrastructure`, `oracle-servicing-drift`, `envelope-gap`, or `candidate-intentional-difference`.

A candidate stays failed until a reviewed policy entry records:

- stable id/version and exact case/dimension/semantic selector;
- raw example/canonical diff;
- why ADR 0006 permits it;
- API/runtime/debugger/downstream evidence of no lost observable;
- SDK bounds, author/reviewer, creation/expiry, linked decision; and
- whether cross-compiler only or also deterministic.

Rules may allow different IL sequences, row/token order, or cross MVID/PDB identities. They may not broadly suppress diagnostics, missing API/resources, invalid metadata, runtime differences, fallback evidence, deterministic self-mismatch, or AOT failure. ApiCompat suppressions follow the same contract.

Oracle refresh runs old/candidate oracle, reviews every corpus/golden/policy diff, updates binary hashes and evidence together, and retains both bundles. "Accept new output" is never a test action.

## Incremental rollout and ownership

1. **Harness self-test:** schemas, hashing, isolation, binlog replay, oracle-vs-oracle deterministic/canonical comparison; deliberately mutate every comparator.
2. **Minimal dual lane:** one positive, diagnostic, late-failure cleanup, unsupported, and deterministic case through oracle and stub/early FSharp2.
3. **First IcedTasks envelope:** [Select the first IcedTasks vertical slice][issue-6] selects and owns the smallest concrete source/feature/diagnostic/artifact cases without weakening full-path invariants.
4. **Emitter integration:** [Select the metadata, IL, and PDB emitter architecture][issue-7] chooses the NativeAOT-compatible ECMA-335/IL/PDB/signing/resource/deterministic emitter; comparator convenience cannot dictate byte-identical oracle layout.
5. **End-to-end proof:** [Prototype the minimum end-to-end FSharp2 compiler][issue-8] runs the selected host/frontend/IR/optimizer/emitter through this harness, including consumers, both AOT gates, cold/warm, tripwire, and no fallback.
6. **Expansion:** add versions/options/diagnostics/artifacts/failures/cultures/signing/resources/graphs/RIDs to final gate.

[Define the differential compatibility and performance harness][issue-5] establishes the harness contract, schemas, verdict, and measurement policy. It does not choose the source slice owned by [Select the first IcedTasks vertical slice][issue-6], the emitter owned by [Select the metadata, IL, and PDB emitter architecture][issue-7], or claim the evidence required by [Prototype the minimum end-to-end FSharp2 compiler][issue-8]. The harness decision directly unblocks both selection tickets; the prototype also depends on [Establish the NativeAOT constraints for the compiler host][issue-4].

## Ticketed follow-on decisions

1. **Opaque F# metadata:** [ADR 0023](../adr/0023-keep-the-fsharp-metadata-harness-inspector-envelope-only.md) adds an exact, test-only resource-envelope inspector while keeping four-way producer/consumer behavior authoritative; the [issue #17 research](independent-fsharp-metadata-decoder.md) keeps any private-pickle semantic decoder diagnostic-only and separates production importer/fingerprint work.
2. **Debugger automation:** canonical PDB/source/stack evidence is fixed; [Choose the cross-platform debugger automation driver][issue-18] owns the pinned breakpoint/stepping driver and reproducibility contract.
3. **Warm edit corpus:** after [Select the first IcedTasks vertical slice][issue-6] fixes the source envelope, [Define the warm-edit corpus and cache-evidence contract][issue-19] owns representative edits and epoch/hit/miss/reuse/invalidation evidence without making cache layout a compatibility contract.
4. **Performance runner:** [Define compiler performance runner and regression governance][issue-12] owns machine classes, quietness thresholds, scanner exclusions, variance policy, trace schema, and the evidence required before tightening the 10%/50 ms gate.
5. **Type providers:** [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) selects an optional managed broker for final legacy-provider compatibility and keeps a dependency-declaring snapshot contract additive; the [issue #13 research](type-provider-compatibility-boundary.md) defines the protocol, invalidation, cache, diagnostic, and gate contract. Provider cases remain explicitly unsupported until their declared broker envelope passes, not because the boundary is undecided.
6. **Linux deployment:** [Establish Linux runtime and deployment compatibility floors][issue-14] owns separate glibc and musl floors; `linux-*` cannot imply both.
7. **Localization/signing:** [Define localization, signing, and native-resource compatibility policy][issue-15] owns the final culture/ICU matrix, key custody, and native-resource/signing probes.
8. **Sensitive evidence:** [Define secure harness evidence retention and redaction][issue-16] owns CI retention, access, redaction, and deletion policy before raw binlogs or source-bearing bundles are uploaded.

## Primary sources and repo evidence

- [Pinned F# `CoreCompile` target and task inputs][fsharp-corecompile]
- [Pinned F# argument capture and response generation][fsc-capture-source]
- [Pinned diagnostic formatting/severity/entry behavior][fsharp-diagnostics]
- [MSBuild evaluation versus execution][msbuild-evaluation]
- [MSBuild incremental skipping/output inference][msbuild-incremental]
- [Pinned structured target-skip event][target-skipped]
- [Pinned first-party binlog replay API][binlog-replay]
- [MSBuild binlog contents and overhead][binlog-content]
- [ECMA-335 sixth edition][ecma-335]
- [Pinned `PEReader`][pe-reader] and [`MetadataReader`][metadata-reader]
- [Pinned portable PDB specification/debug API][portable-pdb] [pdb-api]
- [Pinned SDK ApiCompat targets/task][api-compat-target] [api-compat-task]
- [Pinned ILVerify implementation][ilverify]
- [Official NativeAOT contract][nativeaot-overview]
- [Official benchmark isolation/distribution/outlier guidance][benchmark-guidance]
- [Current repo capture/test seam][repo-build]
- [Current CI workflow][repo-workflow]
- [Current test project][repo-tests]

[^repo-sdk]: See the [repo SDK request][repo-global-json] and resolved snapshot in the [compiler/MSBuild contract](dotnet-10-fsharp-compiler-msbuild-contract.md#oracle-snapshot).
[^fsc-capture]: The [pinned `Fsc` source][fsc-capture-source] snapshots ordinary arguments/sources before later ref-output switches and writes the completed response separately; see the [local contract](dotnet-10-fsharp-compiler-msbuild-contract.md#standard-sdk-invocation).
[^repo-capture-gap]: [`disableBinLog` sets `DisableInternalBinLog=true`][repo-build-binlog], and [`DotnetTest` passes `--no-build`][repo-build-test].
[^repo-ci-gap]: The workflow selects [`main` for push and pull requests][repo-workflow-branches]; the branch inspected for this note was `master`.
[^binlog-content]: Microsoft documents [binlog contents and possible performance cost][binlog-content].
[^diagnostic-contract]: See [pinned diagnostic sources][fsharp-diagnostics] and the [local diagnostics/exit inventory](dotnet-10-fsharp-compiler-msbuild-contract.md#diagnostics-and-exit-contract).
[^ilverify]: ILVerify is the runtime repository's [ECMA-335 verification tool][ilverify]; it validates IL/metadata against references rather than behavior.
[^api-compat]: The SDK exposes [assembly/package validation tasks][api-compat-target] and their [left/right/suppression inputs][api-compat-task].
[^ecma-335]: ECMA-335 defines CLI metadata and signatures.[ecma-335]
[^pdb-api]: See [PE/metadata sources][pe-reader], [portable PDB spec][portable-pdb], and [method debug API][pdb-api].
[^nativeaot-gates]: See the [two-gate protocol](dotnet-10-nativeaot-compiler-host-constraints.md#two-distinct-nativeaot-release-gates) and [official NativeAOT contract][nativeaot-overview].
[^msbuild-evaluation]: MSBuild separates [evaluation and execution][msbuild-evaluation]; the F# source defines the [target boundary][fsharp-corecompile].
[^msbuild-skip]: MSBuild documents [skipping/output inference][msbuild-incremental], and the event exposes `OutputsUpToDate`.[target-skipped]
[^oracle-cold]: The CLI reaches the [non-incremental entry point][fsc-nonincremental]; see [measured interpretation](icedtasks-official-compiler-target-baseline.md#cold-and-warm-interpretation).
[^benchmark-method]: Follow pinned [benchmark distribution/isolation/outlier guidance][benchmark-guidance]; keep explanatory instrumentation separate as established by the [IcedTasks baseline](icedtasks-official-compiler-target-baseline.md#allocation-observer).

[issue-1]: https://github.com/TheAngryByrd/fsharp2/issues/1
[issue-4]: https://github.com/TheAngryByrd/fsharp2/issues/4
[issue-5]: https://github.com/TheAngryByrd/fsharp2/issues/5
[issue-6]: https://github.com/TheAngryByrd/fsharp2/issues/6
[issue-7]: https://github.com/TheAngryByrd/fsharp2/issues/7
[issue-8]: https://github.com/TheAngryByrd/fsharp2/issues/8
[issue-12]: https://github.com/TheAngryByrd/fsharp2/issues/12
[issue-14]: https://github.com/TheAngryByrd/fsharp2/issues/14
[issue-15]: https://github.com/TheAngryByrd/fsharp2/issues/15
[issue-16]: https://github.com/TheAngryByrd/fsharp2/issues/16
[issue-17]: https://github.com/TheAngryByrd/fsharp2/issues/17
[issue-18]: https://github.com/TheAngryByrd/fsharp2/issues/18
[issue-19]: https://github.com/TheAngryByrd/fsharp2/issues/19
[repo-global-json]: ../../../global.json#L1-L6
[repo-build]: ../../../build/build.fs
[repo-build-binlog]: ../../../build/build.fs#L306-L307
[repo-build-test]: ../../../build/build.fs#L406-L431
[repo-workflow]: ../../../.github/workflows/build.yml
[repo-workflow-branches]: ../../../.github/workflows/build.yml#L3-L9
[repo-tests]: ../../../tests/fsharp2.Tests/fsharp2.Tests.fsproj#L1-L19
[fsharp-corecompile]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.Targets#L279-L426
[fsc-capture-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L340-L365
[fsharp-diagnostics]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerDiagnostics.fs#L1981-L2330
[fsc-nonincremental]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/fsc.fs#L1212-L1245
[msbuild-evaluation]: https://github.com/MicrosoftDocs/visualstudio-docs/blob/551384461e5bfa0f343017cefcf18f0976c858fa/docs/msbuild/build-process-overview.md#L19-L22
[msbuild-incremental]: https://github.com/MicrosoftDocs/visualstudio-docs/blob/551384461e5bfa0f343017cefcf18f0976c858fa/docs/msbuild/incremental-builds.md#L16-L25
[target-skipped]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/msbuild/src/Framework/TargetSkippedEventArgs.cs#L16-L44
[binlog-replay]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/msbuild/src/Build/Logging/BinaryLogger/BinaryLogReplayEventSource.cs#L57-L105
[binlog-content]: https://github.com/MicrosoftDocs/visualstudio-docs/blob/761a454fa5ff25bd85cd322e052851a04d88aaa2/docs/msbuild/obtaining-build-logs-with-msbuild.md#L80-L102
[ecma-335]: https://ecma-international.org/wp-content/uploads/ECMA-335_6th_edition_june_2012.pdf
[pe-reader]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/PEReader.cs
[metadata-reader]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/MetadataReader.cs
[portable-pdb]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/specs/PortablePdb-Metadata.md
[pdb-api]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/PortablePdb/MethodDebugInformation.cs
[api-compat-target]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.ApiCompat.Common.targets#L15-L16
[api-compat-task]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Compatibility/ApiCompat/Microsoft.DotNet.ApiCompat.Task/ValidateAssembliesTask.cs#L13-L58
[ilverify]: https://github.com/dotnet/runtime/tree/v10.0.10/src/coreclr/tools/ILVerify
[nativeaot-overview]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md
[benchmark-guidance]: https://github.com/dotnet/performance/blob/f82aa4641b85fe406bd50d0b9989d07a15cebbcb/docs/microbenchmark-design-guidelines.md#L51-L56
