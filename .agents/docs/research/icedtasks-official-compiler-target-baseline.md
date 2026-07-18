# Official F# compiler target baseline for IcedTasks

Status: versioned research snapshot for [Measure the official compiler target on IcedTasks][issue-3], supporting the destination in [Build FSharp2: a NativeAOT, incremental, drop-in F# compiler][issue-1]

Observed: 2026-07-18 on Windows x64

Scope: IcedTasks commit `ba4e932b71bfde354f0e2561c2519b282fe56ff9`, `src/IcedTasks/IcedTasks.fsproj`, Release, one Compiler Target Invocation for each of `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0`

Governing decisions: [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md), [ADR 0016](../adr/0016-explain-slow-compilations-with-structured-traces.md), [ADR 0017](../adr/0017-select-memory-techniques-by-benchmark.md), and [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md)

## Answer

The reproducible official-compiler baseline for FSharp2 is the real Release `CoreCompile`/`Fsc` path for the four inner builds of IcedTasks' library project, evaluated from a real Git checkout at the pinned commit and compiled with the .NET 10.0.110 SDK oracle. It is not a solution-build time, restore time, analyzer time, direct-CLI substitute, or unchanged incremental no-op.

On this machine, all four median Compiler Target Invocations exceed FSharp2's three-second Performance Tripwire:

| TFM | Samples | Minimum ms | Median ms | Mean ms | Maximum ms | Median over tripwire |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `netstandard2.0` | 15 | 3,258.384 | **3,513.742** | 3,558.570 | 4,004.107 | 17.1% |
| `netstandard2.1` | 15 | 3,337.597 | **3,507.558** | 3,626.058 | 4,108.579 | 16.9% |
| `net6.0` | 15 | 3,857.566 | **4,257.558** | 4,219.921 | 4,677.481 | 41.9% |
| `net9.0` | 15 | 3,721.937 | **4,306.148** | 4,527.297 | 7,169.007 | 43.5% |

Across the samples, the median per-invocation `Fsc`/`CoreCompile` share is 99.959% to 99.968%. The median per-invocation target-minus-task gaps are only 1.325, 1.514, 1.349, and 1.474 ms in the table's TFM order. Four `netstandard2.1` samples have larger 22.975-29.663 ms gaps; even the worst observed row still spends 99.148% of `CoreCompile` in `Fsc`.

These are Cold Compilations in the repository vocabulary: every executed CLI `Fsc` task launches a fresh `dotnet fsc.dll` process and the shipped compiler's entry point is explicitly non-incremental. The measurements do not claim an OS-page-cache-cold machine. An unchanged second `Compile` skipped `CoreCompile` for all four TFMs and launched zero `Fsc` tasks. That is an incremental no-op, not a Warm Compilation. The official path therefore supplies no retained-compiler-state warm timing against which FSharp2 can compare; FSharp2's warm sub-second goal remains an independent requirement.

One separately instrumented replay per TFM shows where the official compiler spends time. Optimization is the largest named phase at 1,079.8-1,380.8 ms, followed by typechecking at 786.9-927.6 ms. Approximate allocation-observer replays saw 1,240-1,595 MiB in `GCAllocationTick_V4.AllocationAmount64` chunks with zero lost events. Those phase and allocation observations are deliberately separate from the baseline: both instrumentation modes perturb execution, and the allocation sum is not an exact uninstrumented lifetime allocation count.

FSharp2 should preserve this tuple as the first baseline manifest:

```text
IcedTasks commit/tree      ba4e932b71bfde354f0e2561c2519b282fe56ff9
                          20abf5280d4630db438c7c6f06838105df12c765
Project/configuration     src/IcedTasks/IcedTasks.fsproj / Release
Inner builds              netstandard2.0; netstandard2.1; net6.0; net9.0
Oracle                    SDK 10.0.110 / fsc 14.0.110.0 / win-x64
Argument evidence         preserved physical Fsc response file per TFM
Timing evidence           15 round-robin real CoreCompile/Fsc invocations per TFM
Warm evidence             unchanged target skip only; no official Warm Compilation
Observer evidence         one --times replay and one AllocationTick replay per TFM
```

The official medians are observations, not FSharp2's performance budget. In particular, they do not relax the three-second tripwire or the sub-second Warm Compilation destination.

## Scope and exclusions

[ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md) fixes the first IcedTasks milestone to the four target frameworks on [`src/IcedTasks/IcedTasks.fsproj`][icedtasks-project]. This note measures exactly those four inner builds.

The IcedTasks solution also contains a FAKE build project, three test projects, examples, and a benchmark project. They are useful later as downstream compatibility consumers, but they are not part of this ADR-defined compiler baseline and are not silently folded into its timings. Restore, NuGet resolution, generated-input targets, common SDK copy/package work, formatting, and `_FSharpAnalyzerAfterBuild` are also outside a Compiler Target Invocation. The analyzer packages remain evaluated project inputs, but analyzer paths do not occur in any physical `fsc` response file; their separate target is not compiler time.

Release is intentional. [`src/Directory.Build.props`][icedtasks-src-props] configures Release SourceLink inputs and settings, and the package-producing path requests portable PDB, XML documentation, deterministic output, and reference assemblies. A Git-less source export is not equivalent: it causes SourceLink to evaluate differently and can change untracked-source embedding. The measurement tree was therefore a clean local Git clone, not `git archive` output.

Debug is not measured here. Its option and SourceLink differences should be a separate compatibility tracer rather than mixed into one performance distribution.

## Oracle and machine snapshot

| Input | Observed value |
| --- | --- |
| IcedTasks origin | `https://github.com/TheAngryByrd/IcedTasks.git` |
| IcedTasks commit | `ba4e932b71bfde354f0e2561c2519b282fe56ff9` |
| IcedTasks tree | `20abf5280d4630db438c7c6f06838105df12c765` |
| IcedTasks SDK request | `10.0.100-rc.1.25451.107`, `rollForward: latestMinor` |
| Selected SDK | `10.0.110`, commit `f7d90799ce4ef09a0bb257852a57248d2a8fb8dd` |
| MSBuild | `18.0.11+f7d90799c` |
| Runtime / RID | .NET `10.0.10` / `win-x64` |
| Compiler | `Microsoft (R) F# Compiler version 14.0.110.0 for F# 10.0` |
| `fsc.dll` SHA-256 | `8CB573BE4B7F70886929A572024AB26FC1893CFDD795EF124C58627610DDBF22` |
| OS | Windows 11 Pro `10.0.26200`, x64 |
| CPU | Intel Core i9-12900F rechecked after capture; 24 logical processors retained at the timing gate; physical-core count not retained |
| Memory | 25,586.5 MiB free at timing gate; installed total not retained |
| Power scheme | High performance when rechecked after capture; timing-gate scheme not retained |
| Quiet gate | five seconds; 14% instantaneous CPU load; zero CPU-time delta in observed `dotnet` and unrelated F# MSBuild nodes |

The IcedTasks [`global.json`][icedtasks-global] requests the prerelease SDK shown above with `rollForward: latestMinor`; the captured environment selected installed SDK `10.0.110`. This note records that resolved result rather than attributing it to a single environment variable. The compiler identity and installed-file hashes match the preceding [.NET 10 F# compiler and MSBuild contract](dotnet-10-fsharp-compiler-msbuild-contract.md); this note does not redefine that wider compatibility boundary.

The clone was clean and detached at the commit and tree above. Its generated SourceLink JSON mapped the clone root to:

```text
https://raw.githubusercontent.com/TheAngryByrd/IcedTasks/ba4e932b71bfde354f0e2561c2519b282fe56ff9/*
```

One restore completed before measurement and was excluded from every reported duration.

## Work inside each invocation

### Ordered sources and language surface

Every invocation receives 18 positional F# inputs in order: one generated target-framework attribute file followed by these 17 authored files. The authored corpus is 5,766 physical lines and 268,326 bytes at the pinned tree.

| Order | Authored source | Lines |
| ---: | --- | ---: |
| 1 | `AssemblyInfo.fs` | 23 |
| 2 | `Nullness.fs` | 27 |
| 3 | `TaskLike.fs` | 145 |
| 4 | `TaskBuilderBase.fs` | 475 |
| 5 | `ValueTask.fs` | 320 |
| 6 | `PoolingValueTask.fs` | 266 |
| 7 | `ValueTaskUnit.fs` | 157 |
| 8 | `TaskUnit.fs` | 243 |
| 9 | `Task.fs` | 247 |
| 10 | `AsyncEx.fs` | 364 |
| 11 | `ParallelAsync.fs` | 134 |
| 12 | `ColdTask.fs` | 891 |
| 13 | `CancellableTaskBuilderBase.fs` | 863 |
| 14 | `CancellableValueTask.fs` | 495 |
| 15 | `CancellablePoolingValueTask.fs` | 516 |
| 16 | `CancellableTask.fs` | 579 |
| 17 | `AutoOpens.fs` | 21 |

The workload exercises a demanding, connected language envelope rather than isolated syntax:

- namespaces, modules, assembly attributes, literals, XML documentation, accessibility, and [auto-open metadata][icedtasks-autoopens];
- generic classes, structs, type aliases, tuples and struct tuples, overloaded members, extension members, and type/member constraints;
- [statically resolved member constraints and byref operations][icedtasks-tasklike], extensive `inline`/`InlineIfLambda` use, and [`NoEagerConstraintApplication`][icedtasks-taskbuilderbase];
- [`ResumableCode`, `ResumableStateMachine`, and custom computation-expression builders][icedtasks-taskbuilderbase], plus compiler-recognized resumable intrinsics and [dynamic fallbacks][icedtasks-coldtask];
- `Task`, `ValueTask`, [pooled value-task sources][icedtasks-pooling], `Async`, `IAsyncEnumerable`, cancellation, exception/finally/using control flow, loops, and [parallel `MergeSources` paths][icedtasks-cancellable-pooling]; and
- conditional compilation and nullness syntax that change the active source envelope by TFM.

All four response files still list all 17 authored files. Conditional compilation makes their active content differ:

- [`PoolingValueTask.fs`][icedtasks-pooling] and [`CancellablePoolingValueTask.fs`][icedtasks-cancellable-pooling] put their implementations behind `NET6_0_OR_GREATER`, so those implementations are active for `net6.0` and `net9.0` but not the two .NET Standard targets.
- [`Nullness.fs`][icedtasks-nullness] uses `Exception | null`, `IDisposable | null`, and `IAsyncDisposable | null` only when `NULLABLE` is defined. That occurs only for `net9.0`, where the project evaluates `Nullable=enable` and the compiler receives `--checknulls+`.

This is an inventory of what the real project asks the oracle to compile, not a claim that every F# feature used by IcedTasks must be the first Experimental Vertical Milestone. [Select the first IcedTasks vertical slice](https://github.com/TheAngryByrd/fsharp2/issues/6) owns that narrower selection.

### Physical compiler arguments and artifacts

`MSBUILDPRESERVETOOLTEMPFILES=1` was set before starting a fresh MSBuild process for each TFM. The pinned `ToolTask` source reads that environment variable once, writes its generated response commands to UTF-8 `.rsp`, and skips deletion when its value is exactly `1`. This preserves the physical invocation generated from the [completed response builder][fsc-physical-response], including `--refout`. The [`@(FscCommandLineArgs)` snapshot precedes the `--refout` addition][fsc-argument-snapshot] and is [returned separately][fsc-provided-args], so it is an incomplete replay vector and omits that option.

| TFM | Response lines | SHA-256 | References | Defines | Language / profile | FSharp.Core |
| --- | ---: | --- | ---: | ---: | --- | --- |
| `netstandard2.0` | 172 | `71ac51802c153e296f938d7d6ccfb229d397a6bba8fdd66fcb6961d1d4882b9c` | 117 | 12 | `8.0` / `netstandard` | `6.0.4` (`netstandard2.0`) |
| `netstandard2.1` | 178 | `20d38daaa0aa46e220cb8e7e4f3fa0e04a33409b9cb3c4143de75a5b8d0af1e3` | 122 | 13 | `8.0` / `netstandard` | `6.0.4` (`netstandard2.1`) |
| `net6.0` | 217 | `dc71f67866c568a0d834f83391a6c34662105a91be314b2f3f09e27d392de9e5` | 160 | 14 | `8.0` / `netcore` | `6.0.4` (`netstandard2.1`) |
| `net9.0` | 227 | `107714cbdf61edf08729a9a6ee57b96f79f2cff02c59b7e57593c494ec396c2f` | 165 | 18 | `9.0` / `netcore` | `9.0.300` (`netstandard2.1`) |

Every physical response contains:

```text
--debug:portable
--noframework
--doc:<TFM-specific IcedTasks.xml>
--optimize+
--target:library
--nowarn:FS0057,NU5104,FS3513
--warn:3
--warnaserror
--warnaserror:3239
--fullpaths
--flaterrors
--highentropyva+
--nocopyfsharpcore
--deterministic+
--simpleresolution
--test:GraphBasedChecking
--test:ParallelIlxGen
--test:ParallelOptimization
--sourcelink:<TFM-specific IcedTasks.sourcelink.json>
--refout:<TFM-specific refint/IcedTasks.dll>
```

The exact target profile, language version, defines, output paths, references, and generated inputs vary as shown. All four responses contain two `--embed` inputs: the generated target-framework attribute source and `ILLink.Substitutions.xml`. There are no compiler resources and no analyzer-path arguments. `netstandard2.0` additionally resolves `Microsoft.Bcl.AsyncInterfaces` and package-supplied façade references. Only `net9.0` receives `--checknulls+` and `--define:NULLABLE`.

Each successful compiler invocation produces or refreshes:

```text
obj/Release/<tfm>/IcedTasks.dll
obj/Release/<tfm>/IcedTasks.pdb
obj/Release/<tfm>/IcedTasks.xml
obj/Release/<tfm>/refint/IcedTasks.dll
```

All four TFMs receive `--refout` because IcedTasks explicitly sets `ProduceReferenceAssembly=true` in its root [`Directory.Build.props`][icedtasks-root-props]. This overrides the neutral SDK default described in the preceding contract note.

## Timing baseline

### Method

The timed command was the real target path:

```powershell
dotnet msbuild src/IcedTasks/IcedTasks.fsproj `
  -t:Compile `
  -p:TargetFramework=<tfm> `
  -p:Configuration=Release `
  -p:BuildProjectReferences=false `
  -maxcpucount:1 `
  -nodeReuse:false `
  -nologo `
  -verbosity:minimal `
  -bl:<per-run.binlog>
```

Before each run, the harness deleted only the TFM's implementation output and `refint` output. That forces the shipped incremental target to execute without invoking `Rebuild` or including restore, analyzer, copy, pack, or unrelated build work. Each binlog was then inspected for the `CoreCompile` target and its `Fsc` task. `Process wall ms` below is retained as audit context; the baseline statistic is `CoreCompile ms`.

There were 15 rounds. TFM order rotated once per round to spread drift, the build was single-node with node reuse disabled, instrumentation was off, all 60 processes exited zero, and every expected implementation/reference output existed. The raw CSV has SHA-256:

```text
7278314E3C4E3441DD08DE48AA4BCBB26650CC83F43C864BB1A74AB6836E2455
```

With only 15 samples per TFM, nearest-rank p95 is just the maximum and is not reported as a separate statistic.

### Raw observations

| Round | Order | TFM | CoreCompile ms | Fsc ms | Process wall ms |
| ---: | ---: | --- | ---: | ---: | ---: |
| 1 | 1 | `netstandard2.0` | 3685.131 | 3683.872 | 4338.921 |
| 1 | 2 | `netstandard2.1` | 3544.645 | 3543.195 | 4222.016 |
| 1 | 3 | `net6.0` | 4677.481 | 4676.298 | 5529.893 |
| 1 | 4 | `net9.0` | 4531.583 | 4530.039 | 5318.494 |
| 2 | 1 | `netstandard2.1` | 3827.976 | 3826.462 | 4646.695 |
| 2 | 2 | `net6.0` | 4464.528 | 4463.097 | 5176.633 |
| 2 | 3 | `net9.0` | 4480.088 | 4478.614 | 5200.462 |
| 2 | 4 | `netstandard2.0` | 3513.742 | 3512.336 | 4204.192 |
| 3 | 1 | `net6.0` | 4200.520 | 4199.171 | 4985.356 |
| 3 | 2 | `net9.0` | 4264.341 | 4263.013 | 4973.728 |
| 3 | 3 | `netstandard2.0` | 3732.689 | 3730.430 | 4418.563 |
| 3 | 4 | `netstandard2.1` | 3910.384 | 3886.962 | 4761.748 |
| 4 | 1 | `net9.0` | 4616.923 | 4615.429 | 5379.747 |
| 4 | 2 | `netstandard2.0` | 3884.566 | 3883.093 | 4590.827 |
| 4 | 3 | `netstandard2.1` | 3997.206 | 3995.763 | 4715.450 |
| 4 | 4 | `net6.0` | 4312.787 | 4311.443 | 5075.050 |
| 5 | 1 | `netstandard2.0` | 3386.577 | 3385.263 | 4036.452 |
| 5 | 2 | `netstandard2.1` | 3337.597 | 3336.378 | 4015.771 |
| 5 | 3 | `net6.0` | 4102.359 | 4100.682 | 4840.654 |
| 5 | 4 | `net9.0` | 7169.007 | 7167.443 | 7972.120 |
| 6 | 1 | `netstandard2.1` | 4108.579 | 4106.710 | 5020.066 |
| 6 | 2 | `net6.0` | 4448.910 | 4447.362 | 5195.145 |
| 6 | 3 | `net9.0` | 4306.148 | 4304.787 | 5023.956 |
| 6 | 4 | `netstandard2.0` | 4004.107 | 4002.832 | 4667.567 |
| 7 | 1 | `net6.0` | 4457.251 | 4455.933 | 5257.565 |
| 7 | 2 | `net9.0` | 4067.332 | 4066.037 | 4736.258 |
| 7 | 3 | `netstandard2.0` | 3258.384 | 3257.229 | 3880.651 |
| 7 | 4 | `netstandard2.1` | 3481.055 | 3479.791 | 4141.909 |
| 8 | 1 | `net9.0` | 4092.811 | 4091.223 | 4760.204 |
| 8 | 2 | `netstandard2.0` | 3341.078 | 3339.823 | 4000.870 |
| 8 | 3 | `netstandard2.1` | 3481.090 | 3451.427 | 4244.017 |
| 8 | 4 | `net6.0` | 3862.883 | 3861.237 | 4509.419 |
| 9 | 1 | `netstandard2.0` | 3370.269 | 3368.831 | 4010.005 |
| 9 | 2 | `netstandard2.1` | 3400.805 | 3399.579 | 4045.773 |
| 9 | 3 | `net6.0` | 3986.266 | 3985.057 | 4723.912 |
| 9 | 4 | `net9.0` | 4784.778 | 4783.257 | 5537.856 |
| 10 | 1 | `netstandard2.1` | 3760.671 | 3759.417 | 4661.178 |
| 10 | 2 | `net6.0` | 4046.076 | 4044.718 | 4792.892 |
| 10 | 3 | `net9.0` | 4174.024 | 4172.819 | 4841.661 |
| 10 | 4 | `netstandard2.0` | 3896.195 | 3894.826 | 4588.305 |
| 11 | 1 | `net6.0` | 4290.932 | 4289.194 | 5323.561 |
| 11 | 2 | `net9.0` | 4929.021 | 4927.541 | 5701.398 |
| 11 | 3 | `netstandard2.0` | 3707.379 | 3705.323 | 4422.919 |
| 11 | 4 | `netstandard2.1` | 3507.558 | 3481.548 | 4319.611 |
| 12 | 1 | `net9.0` | 3721.937 | 3720.714 | 4363.744 |
| 12 | 2 | `netstandard2.0` | 3315.412 | 3314.064 | 3917.856 |
| 12 | 3 | `netstandard2.1` | 3375.725 | 3352.750 | 4088.934 |
| 12 | 4 | `net6.0` | 4010.504 | 4009.332 | 4721.018 |
| 13 | 1 | `netstandard2.0` | 3372.254 | 3371.087 | 4017.490 |
| 13 | 2 | `netstandard2.1` | 3815.495 | 3813.860 | 4476.632 |
| 13 | 3 | `net6.0` | 4323.193 | 4321.865 | 5028.798 |
| 13 | 4 | `net9.0` | 4432.241 | 4430.821 | 5287.341 |
| 14 | 1 | `netstandard2.1` | 3467.100 | 3465.898 | 4099.754 |
| 14 | 2 | `net6.0` | 3857.566 | 3856.282 | 4517.902 |
| 14 | 3 | `net9.0` | 4254.211 | 4252.589 | 4957.181 |
| 14 | 4 | `netstandard2.0` | 3550.577 | 3549.252 | 4256.692 |
| 15 | 1 | `net6.0` | 4257.558 | 4255.415 | 5168.477 |
| 15 | 2 | `net9.0` | 4085.010 | 4083.756 | 4763.079 |
| 15 | 3 | `netstandard2.0` | 3360.184 | 3358.993 | 3994.221 |
| 15 | 4 | `netstandard2.1` | 3374.990 | 3373.393 | 4043.694 |

### Cold and warm interpretation

`Fsc` inherits MSBuild `ToolTask`. In a normal CLI build its host object is null and it calls `base.ExecuteTool`, which starts an executable; the compiler source names its path the non-incremental compilation entry point. Consequently:

1. Every measured invocation starts a fresh compiler process with no reusable compiler state from an earlier invocation.
2. Later rounds can benefit from OS/filesystem caches, but that is not Warm Compilation as defined in this repository.
3. An ordinary unchanged `Compile` skips the compiler instead of warming it.

The unchanged follow-up probes all exited zero:

| TFM | `CoreCompile` status | Considered/skipped ms | `Fsc` task count |
| --- | --- | ---: | ---: |
| `netstandard2.0` | skipped | 1.791 | 0 |
| `netstandard2.1` | skipped | 4.877 | 0 |
| `net6.0` | skipped | 5.449 | 0 |
| `net9.0` | skipped | 6.119 | 0 |

Those small numbers are target-consideration evidence only. They must not be advertised as warm compiler timings. FSharp2's Warm Compilation benchmark must force a real Compiler Target Invocation whose inputs changed in a controlled way and prove that retained state is valid under [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md).

## Phase observer

One direct replay of each preserved response file added:

```text
--times --warnaserror-:75
```

`--times` is a shipped hidden observer; the [bare form installs the console listener][times-listener]. `--warnaserror-:75` is an observer-only override required because IcedTasks treats warnings as errors and the hidden option emits FS0075; neither option occurs in the baseline. Direct replay measures `fsc`, not the real `CoreCompile`/`ToolTask` boundary.

The official phase rows are intervals. [`ReportTime` closes the current activity and starts the named next activity][phase-transition], and nested `>` writer rows overlap `Write .NET Binary`. The table below sums only top-level rows once. The nine highlighted rows do not cover all 15 top-level activities; `Other` is their remainder.

| TFM | Import core | Parse | Import refs | Typecheck | Optimize | OptData | Tail | TAST to IL | Write binary | Other |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `netstandard2.0` | 266.2 | 80.7 | 11.0 | 801.4 | 1,116.6 | 198.9 | 19.8 | 480.3 | 819.4 | 59.8 |
| `netstandard2.1` | 259.0 | 76.0 | 10.5 | 786.9 | 1,079.8 | 214.6 | 22.1 | 611.6 | 673.0 | 62.0 |
| `net6.0` | 324.2 | 72.3 | 27.1 | 829.5 | 1,242.6 | 214.6 | 24.6 | 583.4 | 702.1 | 60.6 |
| `net9.0` | 369.9 | 70.8 | 20.2 | 927.6 | 1,380.8 | 312.9 | 27.8 | 600.8 | 679.7 | 68.9 |

All values are milliseconds from one instrumented observation, not distribution estimates or evidence that a TFM alone caused the difference.

| TFM | All 15 top-level rows ms | Last reported elapsed ms | External process wall ms | Maximum sampled phase-end WS MiB |
| --- | ---: | ---: | ---: | ---: |
| `netstandard2.0` | 3,854.1 | 3,994.2 | 4,176.953 | 504 |
| `netstandard2.1` | 3,795.5 | 3,942.3 | 4,118.504 | 472 |
| `net6.0` | 4,081.0 | 4,221.4 | 4,426.172 | 536 |
| `net9.0` | 4,459.4 | 4,602.7 | 4,793.339 | 524 |

`WS(MB)` is `Process.WorkingSet64` sampled when a phase stops. The maximum table value is therefore the maximum sampled phase-end working set, **not** `PeakWorkingSet64` and not a true process peak. GC0/GC1/GC2 columns in the shipped table are collection-count deltas, not allocation bytes. Startup before the first phase report and teardown after `Exiting` are outside the phase table.

The durable phase CSV used for this summary had SHA-256 `F1A89765F5A96DB24A5AE4CE520716CB34BE5858754EBFB1F66BFD7E27F979B5`.

## Allocation observer

One separate direct replay per TFM ran under:

```powershell
dotnet-trace collect `
  --format NetTrace `
  --buffersize 256 `
  --providers Microsoft-Windows-DotNETRuntime:0x1:5 `
  --show-child-io `
  -- dotnet <sdk>/FSharp/fsc.dll @<preserved-response-file>
```

The GC keyword is `0x1` and level 5 is Verbose, which is required for `GCAllocationTick_V4`. In [launch mode][dotnet-trace-child], `dotnet-trace` started the `dotnet fsc.dll` command suspended until EventPipe attached. Each trace contained one process id and each replay exited zero. Collection used `dotnet-trace` `6.0.328102`; parsing used `Microsoft.Diagnostics.Tracing.TraceEvent` `3.2.0`.

| TFM | Allocation ticks | Sum `AllocationAmount64` bytes | MiB | Trace wall ms | Events lost |
| --- | ---: | ---: | ---: | ---: | ---: |
| `netstandard2.0` | 12,008 | 1,300,448,184 | 1,240.204 | 3,787.391 | 0 |
| `netstandard2.1` | 12,028 | 1,302,969,944 | 1,242.609 | 3,717.145 | 0 |
| `net6.0` | 15,372 | 1,662,601,576 | 1,585.580 | 4,862.221 | 0 |
| `net9.0` | 15,436 | 1,672,418,496 | 1,594.943 | 4,807.426 | 0 |

The runtime sets the AllocationTick threshold to [100 KiB][allocation-threshold], then [accumulates allocated bytes and emits a tick after the accumulator exceeds it][allocation-accumulation]. [`AllocationAmount64` is that accumulated amount, while `ObjectSize` is only the allocation that triggered the tick][allocation-trigger] and must not be summed as total volume. A final sub-threshold remainder is absent at process exit, and event timestamps smear the preceding accumulated allocations across phase boundaries.

The sum is therefore an approximate allocation-volume observation for the **instrumented replay**. Zero lost events removes one undercount source, but it does not make the sum an exact lifetime count. EventPipe instrumentation can itself change allocations, so the value is neither an exact count nor a strict lower bound for an uninstrumented Compiler Target Invocation. It cannot be attributed to individual compiler phases from these traces.

The allocation summary CSV had SHA-256 `70D3BC3CE3DB747EA45627EFFB334781A8E473B5E1360804A45EDCD89D737373`.

## Reproduction protocol

1. Clone `https://github.com/TheAngryByrd/IcedTasks.git`, detach at `ba4e932b71bfde354f0e2561c2519b282fe56ff9`, and assert tree `20abf5280d4630db438c7c6f06838105df12c765`. Do not substitute a Git-less archive because Release SourceLink is part of the target inputs.
2. Record `dotnet --info`, the compiler banner/hash, OS/RID, CPU, memory, power plan, working directory, environment, and Git status. Resolve SDK `10.0.110` deliberately.
3. Restore `src/IcedTasks/IcedTasks.fsproj` once. Do not include restore in compiler measurements.
4. Set `MSBUILDPRESERVETOOLTEMPFILES=1`, `TEMP`, and `TMP` before launching a fresh MSBuild process. Run one forced Release `Compile` per TFM with a binlog, locate the preserved `.rsp` through its low-importance log entry, and assert Release, SourceLink, two embeds, `--refout`, output existence, and exit zero.
5. Gate on a quiet machine. Run 15 round-robin repetitions. Before each, delete only `obj/Release/<tfm>/IcedTasks.dll` and `obj/Release/<tfm>/refint/IcedTasks.dll`; then run the timing command above. Persist each raw row and binlog immediately.
6. Calculate min, arithmetic mean, median at sorted zero-based index 7 for 15 values, and max from `CoreCompile`. Do not use index 8. Report p95 only with a larger sample or state that nearest-rank p95 equals max at this sample count.
7. Run the same `Compile` without output deletion and verify from the binlog that `CoreCompile` was skipped and no `Fsc` task ran. Do not label the result Warm Compilation.
8. For phases, replay each exact physical response once with `--times --warnaserror-:75`; retain the raw phase table, exclude overlapping nested rows from top-level sums, and call working set sampled rather than peak.
9. For allocations, replay each exact physical response under the provider above, retain the `.nettrace`, assert zero lost events, and sum only `GCAllocationTick_V4.AllocationAmount64` for the compiler process. Keep the observer result separate from timing.

The local evidence set used during this investigation lived under `C:\tmp\fsharp2-issue3-evidence-20260718`. The repo artifact is this note, so the raw timing table, commands, identities, and key hashes are embedded here rather than depending on that temporary path.

## Interpretation and follow-on decisions

1. **The tripwire is already live.** Every median exceeds three seconds, so FSharp2's structured performance trace must be useful on this first real project rather than deferred to a larger corpus.
2. **Compiler work dominates the target.** Optimization, typechecking, IL lowering, and binary writing are the first official-oracle cost landmarks. They are not a mandate to copy the oracle's internal architecture.
3. **Target-framework differences are material inputs.** Reference count, FSharp.Core version, defines, active conditional source, nullable checking, profile, and emitted metadata differ. A single representative TFM cannot stand in for all four.
4. **Reference assemblies are required four times.** IcedTasks' explicit property means every first-milestone invocation exercises implementation plus reference emission.
5. **There is no official warm compiler comparator.** The compatibility oracle still defines warm output behavior, but FSharp2's persistent service and cache invalidation need their own real-invocation benchmark and correctness gates.
6. **Allocation evidence is a regression observer, not a budget.** A future harness may choose more exact allocation instrumentation, but it must retain the distinction between sampled/traced replay and the uninstrumented Compiler Target Invocation.
7. **Scope expansion stays explicit.** Tests, examples, packing, analyzers, NativeAOT gates, negative diagnostics, and the exact first language slice remain owned by their existing follow-on tickets rather than being smuggled into this baseline.

## Primary sources

- [IcedTasks project and pinned source tree][icedtasks-project]
- [IcedTasks inherited compiler properties][icedtasks-root-props]
- [IcedTasks Release SourceLink and analyzer package properties][icedtasks-src-props]
- [Pinned F# `CoreCompile` target and physical `Fsc` task inputs][fsharp-corecompile]
- [Pinned `Fsc` task execution and host-object split][fsc-execution]
- [Pinned `Fsc` argument snapshot and later `--refout` addition][fsc-argument-snapshot]
- [Pinned physical response-file generation and exposed command-line arguments][fsc-physical-response]
- [Pinned non-incremental `fsc` entry point][fsc-nonincremental]
- [Pinned `--times` option][times-option] and [console listener][times-listener]
- [Pinned phase-transition semantics][phase-transition] and [metric implementation][activity-source]
- [Pinned runtime AllocationTick threshold][allocation-threshold], [accumulation][allocation-accumulation], and [trigger payload][allocation-trigger]
- [Pinned AllocationTick V4 event schema][allocation-schema]
- [Official `dotnet-trace` provider, buffer, and loss behavior][dotnet-trace-doc]
- [Official `dotnet-trace` launch-mode behavior][dotnet-trace-child]
- [Pinned MSBuild response-file preservation behavior][tooltask-preserve]

[issue-1]: https://github.com/TheAngryByrd/fsharp2/issues/1
[issue-3]: https://github.com/TheAngryByrd/fsharp2/issues/3
[icedtasks-global]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/global.json
[icedtasks-root-props]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/Directory.Build.props
[icedtasks-src-props]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/Directory.Build.props
[icedtasks-project]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/IcedTasks.fsproj
[icedtasks-tasklike]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/TaskLike.fs#L59-L145
[icedtasks-taskbuilderbase]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/TaskBuilderBase.fs#L20-L224
[icedtasks-coldtask]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/ColdTask.fs#L37-L224
[icedtasks-pooling]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/PoolingValueTask.fs#L1-L266
[icedtasks-cancellable-pooling]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/CancellablePoolingValueTask.fs#L1-L516
[icedtasks-nullness]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/Nullness.fs#L1-L27
[icedtasks-autoopens]: https://github.com/TheAngryByrd/IcedTasks/blob/ba4e932b71bfde354f0e2561c2519b282fe56ff9/src/IcedTasks/AutoOpens.fs#L1-L21
[fsharp-corecompile]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.Targets#L279-L426
[fsc-execution]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L690-L778
[fsc-argument-snapshot]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L340-L365
[fsc-provided-args]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L721-L728
[fsc-physical-response]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L780-L804
[fsc-nonincremental]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/fsc.fs#L1212-L1245
[times-option]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L1810-L1826
[times-listener]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/fsc.fs#L571-L585
[phase-transition]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L2441-L2458
[activity-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Utilities/Activity.fs#L164-L252
[allocation-threshold]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/coreclr/gc/gc.cpp#L2024-L2026
[allocation-accumulation]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/coreclr/gc/gc.cpp#L19246-L19261
[allocation-trigger]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/coreclr/gc/gcee.cpp#L333-L344
[allocation-schema]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/coreclr/vm/ClrEtwAll.man#L1179-L1202
[dotnet-trace-doc]: https://github.com/dotnet/docs/blob/fbeb2689ff0e2098ae5041aa0be08c30c1c5cd45/docs/core/diagnostics/dotnet-trace.md#L112-L139
[dotnet-trace-child]: https://github.com/dotnet/docs/blob/fbeb2689ff0e2098ae5041aa0be08c30c1c5cd45/docs/core/diagnostics/dotnet-trace.md#L246-L262
[tooltask-preserve]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/msbuild/src/Utilities/ToolTask.cs#L54-L64
