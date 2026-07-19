# Compiler performance runner and regression governance

Status: issue [#12](https://github.com/TheAngryByrd/fsharp2/issues/12) research decision, 2026-07-19

Scope: reproducible Cold and Warm Compiler Target Invocation measurements for the complete pinned IcedTasks Compatibility Corpus

Governing decisions: [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md), [ADR 0007](../adr/0007-use-fsharp-with-benchmark-justified-csharp-kernels.md), [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), [ADR 0016](../adr/0016-explain-slow-compilations-with-structured-traces.md), [ADR 0017](../adr/0017-select-memory-techniques-by-benchmark.md), [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md), [ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md), [ADR 0022](../adr/0022-define-the-warm-edit-corpus-and-cache-evidence-contract.md), and [ADR 0024](../adr/0024-use-calibrated-dedicated-runners-for-performance-gates.md)

Related evidence: [differential harness](differential-compatibility-performance-harness.md), [official IcedTasks baseline](icedtasks-official-compiler-target-baseline.md), [warm-edit contract](warm-edit-corpus-and-cache-evidence-contract.md), [`evidence-governance-v1`](secure-harness-evidence-retention-and-redaction.md), [fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md), and [NativeAOT host constraints](dotnet-10-nativeaot-compiler-host-constraints.md)

## Decision

FSharp2 owns a versioned `perf-governance-v1` runner policy. Authoritative performance results come only from dedicated, characterized runners. A gate compares an accepted FSharp2 baseline artifact with the candidate in same-machine, same-job, seed-balanced pairs. Compatibility Oracle timings remain useful context; they are not the relative-regression baseline.

The policy preserves every attempt and every valid upper outlier. It fixes the required valid count before the run and caps total attempts, preventing silent retries and optional stopping. Each authoritative baseline/candidate measured attempt is `valid`, `product-fail`, or `infra-invalid`. Only an independently proved admission, noise, runner, or evidence-collection fault is `infra-invalid` and replacement-eligible. Candidate-owned correctness, state-control, timeout, output, or required-evidence failures are `product-fail`, end the cell as `fail`, and cannot be retried away. An equivalent failure from the accepted baseline ends the series as `infra-error` and quarantines its lineage unless independent infrastructure evidence explains it. Slowness by itself never invalidates a row.

The initial gates remain:

- Cold median `<= 3,000 ms` and Warm median `< 1,000 ms` on controlled runners;
- release Cold p95 `<= 3,000 ms` and Warm p95 `< 1,000 ms` with 30 valid samples;
- relative failure when median slowdown is both `>= 10%` and `>= 50 ms`, and the seeded paired-bootstrap 95% intervals for median delta and ratio are both wholly on the slower side; and
- an additional heavy diagnostic replay for every invocation above three seconds and for every failed performance series.

These numeric quietness, variance, attempt-cap, and relative-regression rules are FSharp2 decisions, not values claimed by the external sources. They are versioned policy inputs and may be tightened only through the baseline-governance process below.

Where the older differential-harness performance section is less specific or conflicts, this issue #12 decision supersedes it: authoritative relative comparison is accepted-FSharp2-baseline versus candidate, p95 is reported and gated only at exactly 30 valid rows, and the canonical statistic/bootstrap definitions below apply.

Correctness remains prior. A candidate diagnostic or artifact divergence is `fail` even when no publishable timing series exists. This note neither narrows family-wide FS Diagnostic Compatibility at the final Compatibility Gate nor permits production fallback to official `fsc` or FCS.

## Compiler-design evidence carried into the gate

The [comparative fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md) identifies four recurring sources of speed: a small native or persistent hot path, semantic caches keyed by stable inputs, invalidation narrower than a file or project when correctness permits, and parallel scheduling of work whose independence is proven. Those findings justify the shape of FSharp2's evidence; they do not provide portable numeric thresholds or make another compiler's timings a FSharp2 baseline.

The gate therefore measures outcomes and verifies the work which produced them:

- A Cold row runs the bounded NativeAOT path and records startup/service, evaluated-input manifest, reference-index, parse, check, optimize/lower, emit, link/publish, and I/O summaries while proving that reusable live and disk compiler state was bypassed.
- A Warm row proves more than contact with a daemon. ADR 0022 action/query keys, source-order checkpoints, separate export/implementation/debug/diagnostic fingerprints, causal per-node decisions, and reconciled counters must show the expected narrow reuse or invalidation. An MSBuild target skip, complete-action hit, silent whole-project miss, or duration alone cannot pass.
- The evaluated ordered project inputs and compiler/environment inputs which participate in semantic validity are hashed into the attempt evidence. Timestamp/output inference remains an outer MSBuild optimization, not semantic-reuse proof.
- The runner retains the complete reserved processor set so independent parse, metadata-index, project, code-generation, and cache work may execute in parallel. F# source-order checking is not weakened to manufacture parallelism, and a policy change cannot force single-core execution to hide a scheduler regression.

Every valid FSharp2 baseline/candidate row carries a bounded `work-summary-v1` produced by the ordinary uninstrumented compiler request. It includes the normalized input/action-key identity, monotonic phase durations, allocation and GC activity, input scale, reference-index and cache/query decisions and counters, invalidation/reuse totals, critical-path summary, and the hash plus reconciliation result of the normal ADR 0022 per-node decision stream. It does not enable EventPipe or another heavy profiler. An invocation over three seconds itself emits the ADR 0016 MSBuild performance warning and retains this structured tripwire trace; CI may promote that warning to an error. The heavier `diagnostic-v1` replay is additional and remains separate below.

Baseline and candidate must emit the same policy-versioned evidence shape. Missing, malformed, or unreconciled candidate-emitted evidence is `product-fail`; independently proved runner-side collection loss is `infra-invalid`. The same emission failure from the accepted baseline is `infra-error` and rejects the lineage. Wall time never stands alone.

## What the sources establish

The .NET performance repository treats benchmark results as distributions and uses out-of-process isolation. Its `RecommendedConfig` explicitly configures one warm-up followed by 15-20 measured iterations and disables BenchmarkDotNet power-plan enforcement.[^dotnet-benchmark-guidance] [^dotnet-recommended-config] Its comparer combines a statistical equivalence test with separately configured practical and noise thresholds rather than treating a ratio alone as dispositive.[^dotnet-results-comparer] FSharp2 adopts the distribution, isolation, and practical-plus-statistical principles, but not BenchmarkDotNet's upper-outlier removal. FSharp2's fixed-power admission rules are local policy rather than a BenchmarkDotNet default, and a multi-second Compiler Target Invocation can be slow because of real compiler, cache, scheduler, storage, or service behavior that the gate must retain.

GitHub-hosted runner images are updated weekly. Self-hosted runners offer control over hardware, OS, and tools, but GitHub does not promise a clean instance for each self-hosted job.[^github-hosted] [^github-self-hosted] Those facts support using ordinary hosted runners for correctness and smoke numbers while making FSharp2 responsible for reimaging and characterizing authoritative runners.

NativeAOT supports EventPipe when `EventSourceSupport=true`; `dotnet-trace` can launch a child from startup, select providers and buffer size, and retain the original NetTrace when converting to lossy viewer formats.[^nativeaot-eventpipe] [^dotnet-trace] These capabilities support the diagnostic format below. They do not make traced durations comparable with uninstrumented samples.

Microsoft documents that antivirus exclusions are a protection gap, should be narrow and audited, and can be enumerated or directly validated. Defender and ClamAV expose folder/path exclusion mechanisms on their supported platforms.[^defender-policy] [^defender-windows] [^defender-macos] [^defender-linux] [^clamav] Scanner controls below are therefore limited to a disposable benchmark root and fail closed when the installed product cannot be audited.

## Runner classes and ownership

| Class | Owner and isolation | Authority |
| --- | --- | --- |
| `hosted-correctness` | Ordinary GitHub-hosted VM, exact runner image recorded | Correctness and non-gating smoke numbers only |
| `local-diagnostic` | Developer-owned machine, environment manifest required | Local diagnosis only; never updates or satisfies a CI gate |
| `controlled-nightly` | FSharp2-owned dedicated physical host or exclusive, non-burstable VM restored from a pinned image; one job at a time | 15-sample nightly and approved-PR gates |
| `controlled-release-<rid>` | Same controls, with one qualified machine class for each required host RID | 30-sample release median/p95 gate |

The eight release RIDs are `win-x64`, `win-arm64`, `linux-x64`, `linux-arm64`, `linux-musl-x64`, `linux-musl-arm64`, `osx-x64`, and `osx-arm64`. Samples never pool across RIDs, libc families, hosts, or machine-class revisions.

A controlled VM also requires provider/host evidence that its CPU, memory, and benchmark storage allocation is exclusive and non-burstable for the complete job, plus preflight and continuous hypervisor steal/ready and host-storage-contention observations. Guest-idle CPU and I/O counters alone cannot qualify a VM because they do not prove that the host did not deschedule or contend the guest. If the provider or host adapter cannot expose equivalent auditable evidence, that VM class is non-gating.

Each controlled class has a stable id derived from:

- physical host id or exclusive-VM host/instance identity;
- OS image digest, kernel/build, architecture, and virtualization state;
- CPU vendor/family/model/stepping, microcode/firmware, physical/logical count, SMT state, and reserved processor set;
- installed memory, storage device/firmware/filesystem, and benchmark volume;
- power plan/governor, AC state, cooling policy, and runner-agent version; and
- SDK, MSBuild, runtime, runner/harness, scanner/indexer policy, and measurement-tool hashes. The accepted-baseline and candidate compiler artifacts are attempt identities, not machine-class identities.

Changing any identity field creates a new machine class and baseline lineage. Nominally identical hosts qualify separately before a reviewed pool can route jobs to either one.

The performance runner group accepts only trusted commits and reviewed artifacts. It never executes arbitrary public-fork code. The image is restored before a run, the benchmark queue has concurrency one, package/network acquisition finishes before admission, and the network is idle or disabled during sampling. Baseline and candidate use separate work, temp, output, service, and cache roots under one disposable benchmark volume.

Use normal process priority. High and real-time priority are forbidden. Each platform adapter records both the requested policy and the effective OS-native priority for the target, client, persistent service, and descendants; on Unix this includes scheduler policy and nice value because the .NET `ProcessPriorityClass` mapping is explicitly platform-specific rather than a portable numeric identity.[^unix-priority] Windows and Linux use the complete recorded reserved CPU set so compiler parallelism remains part of the real invocation; a policy change may not force a single core. A Windows controlled class must either keep that set within one processor group of at most 64 logical processors or use and version a processor-group-aware adapter which applies and verifies every process/thread. `Process.ProcessorAffinity`/the process affinity mask alone is insufficient evidence above one group.[^processor-affinity] macOS records affinity as unsupported rather than simulating a different contract. Baseline and candidate always receive the same observed effective priority and processor policy.

## Admission and continuous quietness

Every numeric limit in this section belongs to `perf-governance-v1`.

Before warm-up, the platform adapter records 20 consecutive one-second windows. Admission requires:

- median non-runner busy CPU below `0.10` core and p95 below `0.25` core;
- no other build, benchmark, update, backup, package, telemetry-upload, or scheduled-maintenance job;
- no swap-in/swap-out during the window and available memory of at least `max(8 GiB, 4 * the machine class's qualified p95 peak working set)`;
- p95 background storage throughput below `1 MiB/s` and no background access to the run, cache, temp, or output roots;
- for a controlled VM, passing provider/host exclusivity plus hypervisor steal/ready and host-storage-contention observations;
- the declared AC/fixed-power state, processor set, clock/governor policy, and thermal-throttle check; and
- every scanner/indexer audit below passing with its configuration fingerprint equal to the machine-class manifest.

The runner repeats the process and system observations during each invocation. CPU use from the complete run-scoped process lineage is excluded; unrelated work is not. A breach makes that attempt `infra-invalid`, preserves its measurements and observations, and consumes one attempt from the cap. A slow duration without an independently observed breach stays valid.

For a new lineage with no qualified working-set observation, the machine manifest declares a conservative bound before the first run. Qualification may raise that bound and rerun; it cannot lower the already-used admission floor retroactively.

An unsupported counter is not treated as zero. The machine-class adapter must provide an equivalent audited observation or the class is non-gating; this includes required provider/host observations for a VM. Cumulative CPU counters are sampled as deltas; Linux `iowait` is never used by itself as a quietness oracle because the kernel documentation says it is not reliable in isolation, and guest `steal` is retained as a host-descheduling signal rather than subtracted from a row.[^linux-proc]

## Scanner and indexer controls

Only the canonical physical, non-symlink disposable benchmark root is excluded. The adapter resolves and records the physical path before applying policy and rejects symlink/reparse aliases or a product which would follow an exclusion outside that root. Do not exclude a user's checkout, an entire source drive, the SDK, the FSharp2 executable, extensions, or broad compiler processes. Inputs are fetched before the exclusion becomes relevant and hash-verified on materialization. Exclusion ownership, reason, creation, expiry, and approver are part of the machine manifest. In `finally`, the adapter removes every run-created scanner/indexer exclusion, directly proves it is absent, records a post-run audit/receipt, and then destroys the root; removal or verification failure quarantines the runner.

| Platform | Required scanner control | Required indexer control | Audit evidence |
| --- | --- | --- | --- |
| Windows | Defender folder exclusion for the exact benchmark root; no process/extension exclusion | Benchmark root carries `NotContentIndexed`, or `WSearch` is disabled in the pinned image | `Get-MpComputerStatus`; `Get-MpPreference`; direct `MpCmdRun.exe -CheckExclusion -Path <root>`; root attributes; `WSearch` start/status; policy hashes |
| macOS | Installed Defender or other endpoint product has an exact managed folder exclusion | Dedicated benchmark volume is in Spotlight Privacy or has indexing disabled | applied profile/plist hash, product health and direct exclusion qualification; `mdutil -s <volume>`; `mds`/scanner process deltas |
| Linux | Defender `epp` exact physical folder exclusion for the AV lane, or ClamAV `OnAccessExcludePath` only when the audited active on-access mode supports it; otherwise the installed product's exact equivalent | Known miners/indexers are absent/disabled or the root is explicitly ignored | resolved non-symlink path; `mdatp exclusion` or `clamconf`; active mode/configuration hash/log; continuous scanner/EDR access observations; service/process inventory; LocalSearch/Tracker settings and marker files when installed |

Windows policy can hide entries from local enumeration, so direct path validation is mandatory. A Defender for Endpoint `epp` exclusion suppresses the antivirus lane but does not establish that EDR inspection is absent; EDR remains enabled, characterized in the machine class, and continuously observed unless a separately reviewed exact-root product policy proves otherwise.[^defender-linux] ClamAV `OnAccessExcludePath` is accepted only when the active on-access mode permits that directive; an incompatible mount-path mode requires a product-specific exact-root equivalent or rejects admission.[^clamav] macOS has no universal per-path XProtect or third-party scanner interface, and Linux has no universal scanner/indexer policy. Each image therefore carries an allowlisted product manifest plus executable audit. An unknown active product, unauditable managed policy, unexpected file access, expired exclusion, or missing post-run removal receipt rejects admission rather than becoming noise tolerance.

## Cold and Warm state controls

The measured interval remains the real `CoreCompile` `TargetStarted` to matching `TargetFinished` for one evaluated project/TFM. Restore, evaluation, outer build, and any heavy `diagnostic-v1` replay remain outside the sample. The ordinary `work-summary-v1` and conditional tripwire warning/trace are part of the uninstrumented request. Exactly one non-skipped target and one real FSharp2 request must execute, produce and validate the expected outputs, and agree with binlog/protocol evidence.

Cold samples:

1. stop the FSharp2 service and verify exit;
2. select explicit cold mode which bypasses live and content-addressed disk state;
3. force `CoreCompile` without `Rebuild` or restore;
4. execute through the real target; and
5. prove zero reuse and lookup bypass through service/cache counters.

This is compiler-state cold, not an OS-page-cache-cold claim.

Warm samples follow ADR 0022 exactly:

1. create a fresh hash-verified materialization of the pinned IcedTasks Compatibility Corpus;
2. reset service/cache and perform one untimed prime compile;
3. retain the same healthy opaque service instance, live-state epoch, cache-namespace id, and cache-namespace epoch;
4. immediately apply exactly one content-addressed implementation-only or public-inline patch;
5. measure the next real `CoreCompile`; and
6. validate output, fingerprint, reuse, invalidation, and consumer evidence.

No inspection, replay, no-op, or other compiler request may occur between prime and measured edit. An unchanged MSBuild skip, compiler-contact no-op, identical replay, or timestamp touch is a control, never a Warm timing sample. The pinned IcedTasks product checkout remains unchanged; edits exist only in isolated disposable materializations.

The prime and measured target each receive one logical `requestId` and every execution attempt receives a unique `attemptId`. The prime records the service/cache lineage and last-successful project epoch/action key. The measured edit names that project epoch as its compare-and-swap base and, on success, advances the project epoch exactly once while publishing the edited state. A failed, cancelled, retried, or superseded attempt cannot advance or publish over it. PID and process-start identity are restricted corroboration only; PID reuse cannot define service identity. Candidate-owned service, live-state, cache-namespace, or project-commit drift is `product-fail`; only independent supervisor evidence can classify the drift as `infra-invalid`.

## Warm-up, order, and exact sample counts

An authoritative cell fixes one machine-class revision, IcedTasks case and TFM, and scenario (`Cold` or one named Warm edit). The accepted baseline and candidate are paired roles inside that cell; compiler artifacts are not part of the cell identity.

After admission, run one untimed stabilization invocation for each role in every applicable cell. Cold stabilization still bypasses compiler state. Each Warm stabilization includes its own untimed prime and edit. Warm-up results are retained as `warmup` attempts but never enter statistics or the replacement cap.

Authoritative relative runs pair the accepted FSharp2 baseline and candidate on the same machine, boot, job, policy, and round. A recorded seed determines:

- a balanced `baseline -> candidate` / `candidate -> baseline` order (exactly balanced for 30; differing by one for 15);
- a round-robin rotation of TFM and Warm edit order; and
- the separate bootstrap seed.

Before the first warm-up, the runner materializes and hashes the complete round schedule, including role order, TFM/edit rotation, stabilization rows, and the separately named bootstrap seed. `series.json` records that schedule, its generator id/version, and its hash; a library PRNG default or an ordering seed without the materialized schedule is not sufficient reproduction evidence.

The Compatibility Oracle remains required for correctness of every source state. Its Cold timing may be interleaved and reported as context, but it does not enter the accepted-baseline pair, relative ratio, confidence interval, or threshold. There is no retained-state Oracle Warm comparator.

| Lane | Required observations per cell | Predeclared cap | Gate |
| --- | ---: | ---: | --- |
| Hosted PR smoke | 1 candidate row | 1 candidate attempt | None |
| Approved PR controlled run | 15 complete baseline/candidate pairs, 15 valid rows per role | 18 scheduled pairs | Absolute median and relative median |
| Nightly | 15 complete baseline/candidate pairs, 15 valid rows per role | 18 scheduled pairs | Absolute median and relative median |
| Release | 30 complete baseline/candidate pairs, 30 valid rows per role | 36 scheduled pairs | Absolute median/p95 and relative median |

The valid-pair target and scheduled-pair cap are declared before the first warm-up. In the absence of a terminal `product-fail`, baseline-lineage failure, or runner quarantine, sampling stops only at the fixed complete-pair target or the cap. Statistical significance, an apparent pass, or proximity to a threshold never stops sampling early.

## Validity, outliers, retries, and variance

Persist every attempt in order. A measured row is valid when state controls, correctness, target execution, outputs, uninstrumented profile, admission/continuous quietness, and required evidence all pass.

A candidate correctness, fallback, diagnostic, artifact, candidate-caused state-control, compiler timeout/hang, or candidate-emitted evidence failure is `product-fail`: retain it, end the cell immediately as `fail`, and do not schedule a replacement pair. The equivalent accepted-baseline failure is `infra-error` for the comparison and quarantines that baseline lineage. A row is `infra-invalid` only when independent observations prove that admission, ambient noise, the runner, or runner-side evidence collection—not either compiler artifact—caused the failure.

Do not trim, winsorize, replace, or relabel upper outliers. Do not use an interquartile, standard-deviation, Tukey, or BenchmarkDotNet label as an invalidity reason. Completed long rows and tripwire exceedances stay valid. A compiler-owned timeout or hang is `fail` and ends the cell; an independently proved runner/infrastructure timeout is `infra-invalid`. Neither is silently replaced as ordinary noise.

If either role in a scheduled pair is `infra-invalid`, that pair does not enter authoritative statistics; both rows remain evidence, and a valid half-pair is never matched with another round. Infra-invalid pairs may be replaced only until the fixed cap: at most three replacements for a 15-pair series and six for a 30-pair series. Failure to reach the target is `infra-error`. Three consecutive infra-invalid pairs immediately quarantine the runner and end the series as `infra-error`.

A complete valid failing series is not automatically retried. One linked confirmation run may be requested after review, but it cannot erase the first result. A pass/fail disagreement is runner or product instability requiring triage; it is not a pass.

For every series report valid, product-fail, and infra-invalid counts and reasons, minimum, median, arithmetic mean, maximum, MAD, robust dispersion `1.4826 * MAD / median`, threshold exceedance count, and raw ordered rows. Report p95 only with 30 valid values.

### Canonical statistics and bootstrap

Every statistic consumes the exact positive rational duration stored for each valid row. Given integer monotonic `startTicks`, `finishTicks`, and positive `frequencyHz`, the canonical nanosecond value is the unreduced rational `(finishTicks - startTicks) * 1,000,000,000 / frequencyHz`; `attempt.json` stores its reduced numerator and denominator along with the raw ticks, frequency, clock identity, and platform-adapter id/version. Rounded integer nanoseconds or milliseconds are presentation only and never enter a gate.

For sorted values `x[0] <= ... <= x[n-1]`, `perf-governance-v1` defines `Q(p)` by `h = (n - 1) * p`, `j = floor(h)`, `g = h - j`, and `Q(p) = (1 - g) * x[j] + g * x[min(j + 1, n - 1)]`, all in exact rational arithmetic. Therefore:

- median is `Q(0.5)`, including the arithmetic midpoint for an even sample;
- p95 is `Q(0.95)` and this same convention applies to admission/quietness windows;
- MAD is `Q(0.5)` over `abs(x[i] - median)`;
- mean is the exact sum divided by `n`; and
- robust dispersion uses exact `1.4826 = 7413 / 5000`, then divides by the median.

Threshold comparisons use the exact rationals, with multiplication/cross-multiplication rather than binary floating-point or rounded display values. `series.json` retains numerator/denominator forms plus rendered units, the statistic-definition id, and the ordered input row ids. A nonpositive canonical duration or zero denominator is invalid evidence, not a value silently repaired by the statistics layer.

Runner qualification requires three 30-pair A/A control series on separate days, with the same immutable artifact in both roles. For each required release cell, each role's robust dispersion must be `<= 5%`, and the seed-balanced A/A comparison must not trigger the relative rule. During an ordinary gate:

- noisy accepted baseline with candidate also noisy is `infra-error` and quarantines the runner;
- accepted baseline dispersion above 5% is `infra-error` even when the candidate appears fast; and
- candidate dispersion above 5% while the accepted baseline remains qualified is `fail: fsharp2-performance-variance`.

## Regression gates and verdicts

For each required cell, candidate absolute gates are independent of relative performance:

- Cold median `<= 3,000 ms`;
- Warm median `< 1,000 ms`; and
- on release, Cold p95 `<= 3,000 ms` and Warm p95 `< 1,000 ms`.

Relative comparison uses exactly 10,000 paired percentile-bootstrap resamples of the complete baseline/candidate round pairs. The bootstrap seed is exactly 32 bytes, generated and sealed before warm-up and stored as 64 lowercase hexadecimal characters. The resampler does not use a runtime/library PRNG. For resample `b = 0..9999`, draw `d = 0..n-1`, and rejection counter `r = 0..`, it hashes the ASCII domain `FSharp2/perf-governance-v1/bootstrap` followed by one `0x00` byte, the 32 seed bytes, and `UInt32BE(b) || UInt32BE(d) || UInt32BE(r)` with SHA-256.[^sha256] Interpret the first eight digest bytes as unsigned big-endian `u`; let `limit = 2^64 - (2^64 mod n)`. Accept when `u < limit` and select pair index `u mod n`; otherwise increment `r`. The selected index resamples the baseline and candidate row together.

For each resample, compute the candidate and baseline medians separately with `Q(0.5)`, then record their difference and ratio. Sort the 10,000 bootstrap values and form the two-sided percentile interval `[Q(0.025), Q(0.975)]` with the same quantile definition. No bias correction, acceleration, studentization, or library-default interval is applied. The lower bounds in the rule below are the exact `Q(0.025)` values and the comparisons to zero and one are strict. A cell fails relative governance only when:

```text
candidate median - baseline median >= 50 ms
and candidate median / baseline median >= 1.10
and the paired-delta 95% lower bound is above 0 ms
and the paired-ratio 95% lower bound is above 1.0
```

A relative speedup never excuses an absolute failure. Oracle slowness never relaxes a budget. A result within the relative guard band still reports its signed delta, ratio, and interval.

Use the harness's four verdicts:

| Verdict | Performance meaning |
| --- | --- |
| `pass` | Correctness passed, the fixed valid count was reached, evidence reconciled, and every applicable absolute and relative gate passed. |
| `fail` | A valid series violated an absolute, relative, or candidate-variance gate, or required compiler behavior/correctness failed. |
| `unsupported` | The cell is explicitly outside the current Experimental Vertical Milestone; it is not performance coverage. |
| `infra-error` | Admission, attempt cap, baseline stability, scanner/indexer audit, runner, or independently proved runner-side trace/evidence loss prevented a trustworthy verdict. Candidate-owned behavior and evidence failures are `fail`, not `infra-error`. |

A required missing cell blocks the aggregate gate. Neither `unsupported` nor `infra-error` is converted to pass.

## PR, nightly, and release policy

| Trigger | Matrix and action |
| --- | --- |
| Every PR | Run correctness on hosted Windows/Linux/macOS. Optional one-row timing is clearly non-gating. Do not upload it into controlled baselines. |
| Approved performance PR | On affected controlled classes, run 15 valid same-machine pairs for all affected IcedTasks TFMs and both Warm edits. Block on absolute/relative failure. |
| Nightly | Run 15 valid pairs over the complete four-TFM IcedTasks Cold/Warm matrix on the designated Windows, Linux, and macOS sentinel classes. Quarantine rather than rebaseline noisy runners. |
| Release | Run 30 valid pairs over every required cell on all eight host-RID classes. Require median and p95 absolute gates, relative median gate, and complete diagnostic evidence. |
| Oracle refresh | Compare old and candidate Oracle for correctness over the complete corpus. Record timing context; do not replace the accepted FSharp2 baseline. |

## Baseline governance

An accepted performance baseline is an immutable FSharp2 package/native-host artifact plus its source commit, protocol/cache/query/emitter schema ids, SDK/tool hashes, machine-class id, policy id, corpus/edit manifest, and complete raw result bundle. It is never “the latest successful job.”

Baseline promotion requires a reviewed change that includes:

1. correctness and no-fallback gates for the proposed artifact;
2. old-baseline/candidate paired results on every affected controlled class;
3. all absolute/relative verdicts and diagnostic replays;
4. the reason for promotion and linked issue/decision; and
5. explicit reviewer approval from compiler and performance owners.

A failing candidate cannot become passing by refreshing its baseline. Promotion occurs only after the regression is fixed or a reviewed product-budget/policy decision changes the gate. Old evidence remains addressable.

OS image, firmware, CPU, storage, SDK, harness, scanner/indexer, or policy changes create a new baseline lineage. Where possible, run the same immutable accepted artifact on old and new classes to produce a bridge report. Never compare candidate on one class with baseline on another.

## Timing and trace evidence schema

The uninstrumented `attempt.json` records:

- schema/policy/run/series/round/pair ids, one logical request id, one unique execution-attempt id, attempt ordinal/parent/retry reason, replacement-attempt linkage, and UTC plus monotonic clock identity;
- corpus commit/tree, case/TFM/edit, normalized evaluated-input manifest and action-key identity, and all input hashes;
- baseline/candidate/oracle role, executable/package/source/protocol/cache/query/emitter hashes;
- machine-class/image/boot ids, OS/CPU/memory/storage/power/affinity/priority and tool hashes;
- scanner/indexer inventory, policy fingerprint, direct audit, preflight and continuous quietness samples;
- Cold/Warm mode; opaque service-instance id and health; service-state epoch; cache-namespace id/epoch/schema; base and last-successful project epoch/action key before/after; commit/superseded state; live/disk policy; prime/edit ids; input-scale measures; orthogonal reference-index/cache lookup, validity, semantic-comparison, and actual-work decisions; and reconciled cache/reuse/invalidation/work counters;
- target/task/RPC start/finish timestamps, raw monotonic ticks/frequency and canonical-duration numerator/denominator, monotonic startup/service/input/reference-index/parse/check/optimize/lower/emit/link/publish/I/O durations, allocation/GC activity, working-set observations, critical-path summary, normal decision-stream hash and reconciliation result, output/artifact hashes, and correctness result; and
- attempt outcome (`valid`, `product-fail`, or `infra-invalid`), independently observed reasons, tripwire warning/CI-promotion state, linked diagnostic replay, and attempt-chain ids.

The compiler-internal `work-summary-v1` fields are mandatory for FSharp2 baseline/candidate roles. A contextual Compatibility Oracle timing row marks those fields `not-applicable`; it never satisfies or enters the authoritative FSharp2 gate.

`series.json` records the declared valid target/cap, immutable materialized schedule/generator/hash, exact ordering/bootstrap seeds, every request/attempt id, ordered valid-row ids, canonical-statistic and bootstrap algorithm ids, exact-rational statistics and paired deltas/ratios/intervals, baseline id, absolute/relative gate evaluations, and four-state verdict.

### Evidence classification, linkage, and retention

The files above describe the authoritative logical record; they do not imply that every field is shareable. Apply `evidence-governance-v1` before persistence and field by field:

- **R3 Restricted Raw** includes raw binlog/protocol/stream/artifact/trace bytes, raw service PID/process-start and boot identity, physical work/cache/scanner-exclusion paths, complete machine/process/environment inventories, cache snapshots, and raw NetTrace or dumps.
- **R2 Controlled Derived** includes complete dependency edges, raw-to-shareable identity mappings, internal unmapped paths, private-corpus/input fingerprints and hashes, and restricted access/deletion/linkage detail.
- **S1 Shareable** may include policy/schema/tool ids, public-corpus and harness-fixture hashes, logical or run-scoped opaque machine/service/cache/project identities, safe reason/verdict codes, aggregate quietness/statistic/work counters and timings, approved public-corpus fingerprints, reconciliation verdicts, and the random export receipt. It never contains a raw bundle id/hash/locator, physical path, PID, private-source digest, or raw dependency edge.

Because the authoritative `attempt.json` contains R3 fields, its container is R3. A shareable attempt or series is reconstructed from the closed S1 schema rather than redacted in place. The capture manifest classifies every field/file; an R2 linkage record binds the raw bundle id/hash to a random export receipt and S1 manifest/hash, while the S1 redaction report exposes only that random receipt and safe verification counts. Accepted-baseline R3/R2 remains retained while its lineage is active and for 400 days after supersession; release evidence remains for 400 days, and other lanes follow the exact retention table in `evidence-governance-v1`. Missing authoritative raw evidence, linkage/export reports, access audit, expiry, or deletion schedule blocks verdict publication and baseline promotion.

Heavy instrumentation is a distinct `diagnostic-v1` profile. The normal `work-summary-v1`, same-invocation tripwire warning/trace, and ADR 0022 decision evidence above remain part of every uninstrumented row; they are not an EventPipe sample. For a diagnostic replay, the exact same role artifact as the triggering attempt is run with the versioned `FSharp2-Compiler` plus runtime allocation/GC providers. A persistent client/service invocation requires one EventPipe session and raw NetTrace for every participating .NET compiler process, or a separately versioned multi-process collector proved to cover the same lineage; `dotnet-trace -- <command>` alone records only the first .NET process and cannot establish complete client/service evidence.[^dotnet-trace] Every per-process trace must report `EventsLost = 0`. The replay manifest correlates all process traces with logical request id, unique attempt id, activity/parent ids, and a recorded monotonic-clock-domain calibration/uncertainty before producing any cross-process critical path. Events include invocation and phase start/stop, service/RPC, cache lookup, node decision, direct/transitive invalidation, conservative widening, optimizer/lowering/emission/link/publish, artifact commit, tripwire, allocations, and GC pauses. Each event carries schema, activity/parent ids, process/thread, monotonic timestamp, case/TFM/edit, node/phase, decision/reason, old/new fingerprints, counts/bytes, and elapsed microseconds. ADR 0022 per-node events and reconciliation counters remain mandatory in both profiles at their specified detail.

The diagnostic manifest records parent timing-attempt id, replay input/state hashes, provider/keyword/level/arguments, `dotnet-trace` and runtime versions, buffer size, raw NetTrace hash, summary-tool hash, and `EventsLost`. `EventsLost` must be zero. Raw NetTrace, its locator/hash, and direct Speedscope/Chromium conversions remain R3. A canonical JSON summary is R2 unless the S1 exporter reconstructs only explicitly allowed public-corpus identities, counters, timings, reason codes, and reconciliation results. Every form follows the retention/linkage policy above.

Any valid uninstrumented invocation over three seconds or failed series schedules an additional fresh linked replay of the relevant role artifact and scenario:

- Cold replay recreates explicit cold bypass;
- Warm replay resets, primes, reapplies the exact patch, and traces the edited invocation; and
- the replay duration never replaces, adjusts, or joins the original timing distribution.

Required-replay attribution follows the same fail-closed rule as timing evidence. Missing or malformed candidate-emitted events, candidate-owned state mismatch, or a candidate replay failure is `product-fail` and makes the cell `fail`; independently proved runner/tool/collector loss is `infra-error`. Unresolved missing required candidate evidence fails toward the candidate rather than becoming replaceable noise. Neither classification deletes the slow row or replaces an already-proved valid `fail`; a nominally passing series whose required tripwire replay is absent has no publishable performance verdict.

## Remaining calibration work

The first controlled runners must qualify the provisional `0.10`/`0.25` core quietness bounds, 5% robust-dispersion bound, and 10%/50 ms regression rule. A policy update may change them only with archived same-artifact controls and known-change trials demonstrating the false-positive and false-negative boundary across every affected machine class. Tightening cannot precede that evidence; loosening cannot relax the absolute product budgets and requires a separately reviewed rationale.

macOS and Linux endpoint products remain vendor-specific. The portable contract is the allowlisted manifest, exact-root exclusion, executable audit, and fail-closed admission rule—not a claim that one command controls every scanner or indexer.

The current prototype's line-oriented trace is evidence of the available phase/cache vocabulary, not the final schema. Implementing `attempt.json`, `series.json`, EventSource events, platform adapters, and runner qualification remains follow-on engineering; this issue fixes their authority and acceptance contract.

The current `.github/workflows/build.yml` now runs hosted correctness/smoke jobs for `master`; it remains non-authoritative for performance. Controlled machine classes plus this policy and `evidence-governance-v1` must be implemented and qualified before CI can publish a performance verdict.

## Primary sources and repo evidence

- [Comparative fast-compiler design research](fast-fsharp-compiler-cold-and-incremental.md)
- [Differential compatibility and performance harness](differential-compatibility-performance-harness.md)
- [Warm-edit and cache-evidence contract](warm-edit-corpus-and-cache-evidence-contract.md)
- [Secure evidence retention and redaction](secure-harness-evidence-retention-and-redaction.md)
- [ADR 0024 calibrated dedicated runners](../adr/0024-use-calibrated-dedicated-runners-for-performance-gates.md)
- [.NET benchmark distribution and isolation guidance][dotnet-benchmark-guidance]
- [.NET performance recommended configuration][dotnet-recommended-config]
- [.NET performance results comparer][dotnet-results-comparer]
- [GitHub-hosted runners][github-hosted] and [self-hosted runners][github-self-hosted]
- [NativeAOT EventPipe support][nativeaot-eventpipe] and [`dotnet-trace` contract][dotnet-trace]
- [Windows Defender exclusion policy][defender-policy], [Windows controls][defender-windows], [macOS controls][defender-macos], and [Linux controls][defender-linux]
- [ClamAV on-access exclusion contract][clamav]
- [Linux process/statistics contract][linux-proc]
- [.NET/Windows processor-affinity boundary][processor-affinity]
- [.NET Unix priority mapping][unix-priority]
- [NIST SHA-256][sha256]
- [Current hosted-only workflow](../../../.github/workflows/build.yml)
- [Current prototype trace writer](../../../src/FSharp2.Compiler.Prototype.Core/CompilerHost.fs)
- [Current prototype query/response schema](../../../src/fsc2.Prototype/Model.fs)

[^dotnet-benchmark-guidance]: The official .NET performance guidance describes distributions and out-of-process isolation and notes its microbenchmark outlier policy: [pinned source](https://github.com/dotnet/performance/blob/0e8ef382d7a881f6145a8ffdd6e410ace6b5ddd4/docs/microbenchmark-design-guidelines.md#L51-L89).
[^dotnet-recommended-config]: The .NET performance repository's `RecommendedConfig` selects one warm-up and 15-20 iterations and sets `PowerPlanMode.None`: [pinned source](https://github.com/dotnet/performance/blob/0e8ef382d7a881f6145a8ffdd6e410ace6b5ddd4/src/harness/BenchmarkDotNet.Extensions/RecommendedConfig.cs#L31-L43).
[^dotnet-results-comparer]: The official comparer applies Mann-Whitney TOST with separate practical and noise thresholds: [pinned source](https://github.com/dotnet/performance/blob/0e8ef382d7a881f6145a8ffdd6e410ace6b5ddd4/src/tools/ResultsComparer/TwoInputsComparer.cs#L35-L51).
[^github-hosted]: GitHub documents hosted VMs, weekly image updates, and exact-image links in logs: [pinned source](https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/concepts/runners/github-hosted-runners.md#L30-L63).
[^github-self-hosted]: GitHub documents self-hosted hardware/software control and that a clean instance per job is not required: [pinned source](https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/concepts/runners/self-hosted-runners.md#L18-L32).
[^nativeaot-eventpipe]: Microsoft documents NativeAOT EventPipe support, its optional `EventSourceSupport` switch, and supported tools: [pinned source](https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/deploying/native-aot/diagnostics.md#L12-L34).
[^dotnet-trace]: Microsoft documents startup launch, provider/buffer/format selection, that `dotnet-trace -- <command>` traces only the first .NET process, and preservation of the original NetTrace during lossy conversion: [pinned source](https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/diagnostics/dotnet-trace.md#L82-L111), [first-process limit](https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/diagnostics/dotnet-trace.md#L257-L263), [conversion](https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/diagnostics/dotnet-trace.md#L781-L786).
[^defender-policy]: Microsoft warns exclusions lower protection and requires narrow, reviewed exclusions: [pinned source](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/configure-exclusions-microsoft-defender-antivirus.md#L55-L70).
[^defender-windows]: Microsoft documents path exclusions and retrieval through Defender PowerShell: [pinned source](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/configure-extension-file-exclusions-microsoft-defender-antivirus.md#L145-L173), [direct `MpCmdRun -CheckExclusion` validation](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/configure-extension-file-exclusions-microsoft-defender-antivirus.md#L315-L326), and [retrieval](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/configure-extension-file-exclusions-microsoft-defender-antivirus.md#L332-L345).
[^defender-macos]: Microsoft documents exact macOS folder/path exclusions, their audit/expiry expectations, and managed configuration: [pinned source](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/mac-exclusions.md#L56-L100).
[^defender-linux]: Microsoft distinguishes Linux antivirus from global exclusions and documents `mdatp` folder exclusions: [pinned source](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/linux-exclusions.md#L26-L70), [CLI](https://github.com/MicrosoftDocs/defender-docs/blob/b45f8e2303d7c2ee3c414ad8b3bd08280a9e765d/defender-endpoint/linux-exclusions.md#L254-L281).
[^clamav]: ClamAV documents the on-access mode constraint, recursive `OnAccessExcludePath`, and explicit logging: [pinned source](https://github.com/Cisco-Talos/clamav-documentation/blob/5b31b7e1d5d142a04fd40d4cc8a4d1473e44aa6d/src/manual/OnAccess.md#L100-L140), [logging](https://github.com/Cisco-Talos/clamav-documentation/blob/5b31b7e1d5d142a04fd40d4cc8a4d1473e44aa6d/src/manual/OnAccess.md#L150-L162).
[^linux-proc]: The Linux kernel documents cumulative `/proc/stat` counters and cautions that `iowait` is not reliable by itself: [kernel documentation](https://docs.kernel.org/filesystems/proc.html#miscellaneous-kernel-statistics-in-proc-stat).
[^processor-affinity]: .NET uses the Win32 process-affinity mask on Windows, while Win32 documents its processor-group behavior on systems with more than 64 processors: [.NET source](https://github.com/dotnet/runtime/blob/c1b09d933f7d720f7ae50f80b4ec6980bb93d96f/src/libraries/System.Diagnostics.Process/src/System/Diagnostics/Process.Windows.cs#L271-L295), [Win32 source](https://github.com/MicrosoftDocs/sdk-api/blob/3149ba735faa96ac629b1e628df64b407e661257/sdk-api-src/content/winbase/nf-winbase-getprocessaffinitymask.md#L82-L93).
[^unix-priority]: The .NET Unix implementation calls its `ProcessPriorityClass` to nice-value mapping "relatively arbitrary" and maps `Normal` to nice zero: [pinned source](https://github.com/dotnet/runtime/blob/c1b09d933f7d720f7ae50f80b4ec6980bb93d96f/src/libraries/System.Diagnostics.Process/src/System/Diagnostics/Process.Unix.cs#L293-L342).
[^sha256]: NIST FIPS 180-4 defines SHA-256: [standard](https://csrc.nist.gov/pubs/fips/180-4/upd1/final).
