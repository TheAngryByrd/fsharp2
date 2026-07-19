# .NET 10 NativeAOT compiler-host constraints

Status: versioned constraints snapshot for [Establish the NativeAOT constraints for the compiler host][issue-4], supporting the destination in [Build FSharp2: a NativeAOT, incremental, drop-in F# compiler][issue-1]

Observed: 2026-07-18 on Windows x64

Version baseline: .NET SDK `10.0.110` at `f7d90799ce4ef09a0bb257852a57248d2a8fb8dd`, runtime and ILCompiler `10.0.10`, and FSharp.Core `10.0.100` at `b0f34d51fccc69fd334253924abd8d6853fad7aa`

Governing decisions: [ADR 0001](../adr/0001-independent-source-informed-compiler.md), [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0007](../adr/0007-use-fsharp-with-benchmark-justified-csharp-kernels.md), [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md), and [ADR 0017](../adr/0017-select-memory-techniques-by-benchmark.md)

## Answer

FSharp2 must begin as a **closed-world NativeAOT product**, not as a managed compiler that will be made AOT-compatible later. Its production executable, compiler libraries, FSharp.Core dependency, optional C# kernels, resource set, and every callable implementation must be known when each RID-specific host is published. The official compiler and `FSharp.Compiler.Service` belong in a separate oracle/test graph and may be invoked only as test processes. Target references are untrusted metadata inputs read with `System.Reflection.Metadata`; they are never loaded into the host runtime.

That boundary has five immediate consequences:

1. Every production project is AOT- and trim-compatible by policy, but an F# `dotnet build` is not sufficient evidence. The pinned SDK imports the ILLink Roslyn analyzers for C# and Visual Basic only, so the warning-free RID-specific NativeAOT publish is the authoritative F# gate.
2. Runtime assembly loading and runtime code generation are forbidden in the production closure. Reflection is not categorically forbidden: small, statically analyzable uses may remain, but `Type`-driven discovery, `Assembly.Load*`, `AssemblyLoadContext` plugins, `Reflection.Emit`, and open-ended runtime generic construction cannot define compiler architecture.
3. FSharp.Core is usable, not blanket-approved. A minimal F# host using lists published and ran warning-free, while adding only `printfn` made the strict publish fail inside FSharp.Core's printf and structured-reflection implementation. Production F# code therefore uses a proved subset and keeps Printf, structured formatting, FSharp.Reflection, and quotation evaluation outside the default path.
4. The six required host artifacts are `win-x64`, `win-arm64`, `linux-x64`, `linux-arm64`, `osx-x64`, and `osx-arm64`. They are separately published, packaged, installed, and executed. The integration target selects the **build machine** RID, never the target project's `RuntimeIdentifier`.
5. [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md) creates two independent release gates: one proves the compiler host itself is native; the other proves that a normal .NET NativeAOT consumer can publish and execute against FSharp2's managed output. Neither gate stands in for the other.

The sharp compatibility boundary is arbitrary F# type providers. The public FSharp.Core contract describes executable provider classes that the compiler constructs and calls, exchanging runtime `Type`, `Assembly`, `MethodBase`, and quotation objects. An arbitrary existing provider cannot be loaded into a closed-world NativeAOT host. Experimental envelopes must reject provider inputs explicitly until their declared provider gate passes. [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) now resolves the follow-on decision: final legacy-provider compatibility uses an optional out-of-process managed broker, while a dependency-declaring snapshot contract remains additive. The detailed boundary is captured in the [issue #13 research](type-provider-compatibility-boundary.md).

## Evidence vocabulary

- **.NET 10 fact** means behavior owned by pinned Microsoft documentation, SDK/runtime source, shipped targets, package metadata, or a recorded local probe.
- **Fixed project decision** means an accepted ADR or repository domain definition that this ticket must preserve.
- **Issue #4 boundary** means a recommendation established here so implementation tickets can be judged consistently.
- **Probe** means one dated observation on Windows x64. It is evidence for that input and cannot prove another RID or the entire FSharp.Core surface.

## Seven-dimension constraint summary

| Dimension | Proven .NET 10 fact | Fixed project decision | Boundary established here |
| --- | --- | --- | --- |
| NativeAOT | A native publish is self-contained, JIT-free, trimmed, single-file, and RID-specific. The runtime reports dynamic code unsupported. [The publisher analyzes the whole closure.][nativeaot-limitations] | Both the host and the host's managed output have NativeAOT release gates. | Treat AOT as a project-graph invariant. Put `PublishAot` in the host project, publish each shipped RID, and run the resulting native file. |
| Trimming/analyzers | `IsAotCompatible` defaults `IsTrimmable`, trim, single-file, and AOT analyzers. `PublishAot` implies `PublishTrimmed`. The pinned SDK imports those source analyzers only for C#/VB. | Correctness and NativeAOT remain requirements even when a memory/performance technique wins a benchmark. | No suppressed IL warning baseline. F# relies on final ILLink/ILCompiler publish analysis; C# source warnings are an earlier extra gate. |
| FSharp.Core | Package `10.0.100` contains `netstandard2.0`/`netstandard2.1` assets, no RID asset, no `IsAotCompatible` metadata, broad reflection annotations, and reflection-heavy formatting paths. | The semantic pipeline is primarily F#. | Pin one package version for the host, audit every used API through strict publish, and quarantine Printf/structured reflection/quotation evaluation until separately proved. |
| Reflection | Static reflection can be preserved; `DynamicallyAccessedMembers` expresses a closed member requirement. Broad `All` requirements retain code and can expose more warnings. | The compiler is independently authored and does not need official compiler runtime types. | Represent target types with FSharp2 metadata IDs/handles, not `System.Type`. Permit reflection only in small reviewed adapters with exact annotations and tests. |
| Dynamic code/loading | NativeAOT has no dynamic assembly loading or `Reflection.Emit`; runtime-created generic instantiations can warn or fail when code was not generated. LINQ expressions use interpretation. | Production may not load or fall back to the official compiler/FCS. | No production `Assembly.Load*`, plugin `AssemblyLoadContext`, `DynamicMethod`, `Reflection.Emit`, or unbounded `MakeGenericType`/`MakeGenericMethod`. Oracle and output execution stay out of process. |
| Platform | [.NET 9+ publicly supports][nativeaot-platforms] Windows x64/Arm64, Linux x64/Arm64, and macOS x64/Arm64. Native toolchains are OS-specific; cross-OS compilation is unsupported and same-OS x64/Arm64 cross-compilation needs the target toolchain. | All six desktop OS/architecture pairs are required host gates. | Prefer native target-architecture runners; allow a same-OS cross lane only after its linker/sysroot/toolchain and produced artifact pass the same execution gate. Build Linux on the oldest supported deployment baseline. |
| Deployment | Native output is self-contained and single-file; `Assembly.Location` is empty. Native symbols are separate by default. Full globalization can require ICU; satellite languages are publish inputs. | Experimental adoption is one NuGet/MSBuild seam with RID-specific native hosts. | Select by SDK host RID, use `AppContext.BaseDirectory` only for adjacent host assets, preserve invocation working-directory semantics, package declared cultures deliberately, and test from the installed package in a clean environment. |

## Required production and oracle graph

```text
normal .fsproj / FSharp2 MSBuild target
  -> RID selector based on NETCoreSdkPortableRuntimeIdentifier
  -> FSharp2.Host.<host-rid>              NativeAOT executable
       -> FSharp2.Compiler                F# semantic pipeline
       -> FSharp2.Metadata + emitter      metadata-only target universe
       -> proved C# kernels               optional, benchmark-justified
       -> pinned FSharp.Core              production dependency

differential/integration tests only
  -> invoke the native FSharp2 host as a process
  -> invoke pinned SDK fsc as a separate oracle process
  -> optional managed FCS/oracle helpers, unreachable from production
  -> compare diagnostics, managed artifacts, publish results, and behavior
```

### Production closure

**Fixed project decision.** [ADR 0001](../adr/0001-independent-source-informed-compiler.md) requires newly authored production code. [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md) permits the official compiler and FCS only in differential-test infrastructure. [The compiler/MSBuild contract](dotnet-10-fsharp-compiler-msbuild-contract.md) further fixes the SDK target boundary: MSBuild already resolves target references and supplies concrete paths.

**Issue #4 boundary.** The NativeAOT host project may reference only production compiler projects and reviewed AOT-compatible packages. Its transitive restore graph and native publish inputs must contain none of:

- `FSharp.Compiler.Service`, `FSharp.Build`, the SDK's `fsc.dll`, or an official-compiler implementation assembly;
- oracle adapters, baseline harnesses, test frameworks, or benchmark runners;
- runtime-discovered compiler phases, serializers, dependency-injection registrations, plugins, or target assemblies; or
- a managed fallback host selected when the native host rejects an input.

The gate inspects evaluated `ProjectReference`/`PackageReference` items, `project.assets.json`, the NativeAOT input list, and the installed NuGet payload. A source-level namespace search alone is not proof of graph separation.

Oracle comparison remains out of process. This avoids pulling official compiler types into the native closure, preserves [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), and makes process identity, arguments, culture, environment, outputs, and exit behavior independently recordable.

### Production project policy

Every production library targets `net10.0` and declares `IsAotCompatible=true`. The executable additionally keeps `PublishAot=true` in the project file; Microsoft recommends the project property because it controls analysis outside the publish command. Test, oracle, and benchmark projects do not become production references merely because they use the same domain types.

The policy baseline is conceptually:

```xml
<PropertyGroup>
  <TargetFramework>net10.0</TargetFramework>
  <IsAotCompatible>true</IsAotCompatible>
  <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  <SuppressTrimAnalysisWarnings>false</SuppressTrimAnalysisWarnings>
  <SuppressAotAnalysisWarnings>false</SuppressAotAnalysisWarnings>
</PropertyGroup>
```

The host adds `PublishAot`. Release CI also sets `ILLinkTreatWarningsAsErrors=true`, `IlcTreatWarningsAsErrors=true`, and `TrimmerSingleWarn=false` explicitly so SDK default drift cannot weaken the gate and dependency warnings retain their individual codes.

## AOT and trimming analyzer policy

### What the pinned SDK actually does

The SDK's `Microsoft.NET.Publish.targets` makes `PublishAot` imply trimming and makes `IsAotCompatible` imply `IsTrimmable`.[^sdk-aot] Its analyzer target derives `EnableTrimAnalyzer`, `EnableSingleFileAnalyzer`, and `EnableAotAnalyzer` from those properties. However, `Microsoft.NET.Sdk.targets` imports `Microsoft.NET.Sdk.Analyzers.targets` only when `$(Language)` is C# or VB; the F# target import is separate.[^fsharp-analyzer-gap]

That yields a deliberate asymmetry:

- a C# kernel must be source-analyzer clean during ordinary build **and** clean in the final native publish;
- an F# project can be green in ordinary build without having received the Roslyn trim/AOT analyzers, so only the final post-IL publish can close its gate; and
- a green editor/build result never substitutes for a publish of the complete F# host closure.

`TreatWarningsAsErrors` flows to ILLink by default in the pinned `10.0.10` targets, but the explicit release properties above preserve intent. `TrimmerSingleWarn` defaults to `true` for package assemblies and can collapse actionable warnings into a package-level warning; the diagnostic lane sets it to `false`.[^illink-warning-target]

### Warning resolution hierarchy

The required order is:

1. remove runtime discovery or reflection;
2. replace it with a static table, source/generated registration, or metadata model;
3. if reflection has a closed type flow, use the narrowest `DynamicallyAccessedMembers` requirement;
4. if a truly incompatible API belongs only outside production, move it behind the oracle/test boundary;
5. use `DynamicDependency` only as a last resort with a direct behavioral test and size review; and
6. use `UnconditionalSuppressMessage` only at the smallest call site when the preserved target is independently proven.

Microsoft's trimming guidance explicitly says `DynamicDependency` is a last resort and a suppression is valid only when the reflected members are visible preservation targets.[^trim-annotations] Therefore:

- there is no repository-wide `NoWarn` for `IL2xxx` or `IL3xxx`;
- `SuppressTrimAnalysisWarnings` and `SuppressAotAnalysisWarnings` remain false;
- an accepted suppression records the exact warning, target, justification, and tracer; and
- a suppression is re-audited on every SDK/FSharp.Core update.

### Dependency metadata is an inventory signal, not proof

.NET 10 introduced `VerifyReferenceAotCompatibility=true`, which produces IL3058 when a referenced assembly lacks `IsAotCompatible` metadata. Microsoft keeps it opt-in because compatible libraries may not yet carry the metadata and pre-.NET-10-targeted assemblies cannot carry the new marker.[^verify-reference-aot]

FSharp.Core `10.0.100` has no such assembly metadata in the observed binary. FSharp2 should run reference verification in a dependency-inventory lane with a reviewed allowlist, but the real strict publish and runtime tracer decide whether a dependency is usable. Neither a missing marker nor a marker by itself proves runtime behavior.

## Reflection, runtime code generation, and assembly loading

### Production stance

| Mechanism | Production stance | Reason/gate |
| --- | --- | --- |
| `System.Reflection.Metadata`, `PEReader`, portable-PDB readers | **Required** | Reads arbitrary target PE/metadata without binding or executing it. |
| Statically closed reflection over host-owned types | **Exceptional** | Allowed only with narrow annotations, strict publish, behavior test, and size review. NativeAOT retains statically used members.[^static-reflection] |
| `Microsoft.FSharp.Reflection.FSharpType` / `FSharpValue` | **Quarantined** | Their public `Type` parameters broadly require `DynamicallyAccessedMemberTypes.All`; do not base semantic data structures, logging, or serializers on them. |
| `Assembly.Load*`, `AssemblyLoadContext.LoadFrom*` | **Forbidden** | NativeAOT has no dynamic assembly loading; trim analysis cannot see an arbitrary plugin closure. |
| `System.Reflection.Emit`, `DynamicMethod` | **Forbidden** | NativeAOT has no runtime code generation. |
| runtime `MakeGenericType` / `MakeGenericMethod` from open input | **Forbidden by default** | NativeAOT must pre-generate generic instantiations; an unknown instantiation can warn IL3050 or fail at runtime.[^aot-generic-warning] |
| LINQ expression compilation | **Interpreted and isolated** | NativeAOT uses the interpreted form. It may be correct but is not an implicit performance/codegen escape hatch. |
| target/output execution | **Out of process in tests** | The production compiler emits managed files; a managed runner or downstream native app loads and executes them. |

`RuntimeFeature.IsDynamicCodeSupported` and `RuntimeFeature.IsDynamicCodeCompiled` are hard-coded false in the NativeAOT runtime.[^runtime-feature] Reflection is narrower rather than absent: the runtime's usage-based metadata policy retains statically used types and methods, while NativeAOT documentation prohibits dynamic loading and runtime code generation.

### Target-reference metadata boundary

**Fixed project decision.** [ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md) says output targeting follows the references and options supplied to a Compiler Target Invocation. The .NET 10 compiler/MSBuild contract proves that normal SDK builds supply concrete resolved reference-assembly paths; FSharp2 does not own NuGet or TFM selection.

**Issue #4 boundary.** Reference, resource, compiler-tool, portable-PDB, and prior-output files are byte/metadata inputs. The reader layer:

- opens managed PE files with `PEReader` and obtains `MetadataReader` tables for definitions, references, attributes, signatures, resources, and debug information;
- maps assembly identity plus metadata handles into an FSharp2-owned target type universe;
- never represents target types with the host runtime's `System.Type` or target assemblies with loaded `Assembly` objects;
- never executes module initializers, attribute constructors, type-provider code, or target methods while indexing;
- keeps host FSharp.Core identity separate from the FSharp.Core reference supplied for the target TFM; and
- preserves file/path/content identity so caches can be invalidated without changing compilation semantics.

`PEReader` and `MetadataReader` expose exactly the metadata-only operations required here, including assembly references, custom attributes, type/member definitions, manifest resources, and embedded/associated portable PDB access.[^metadata-reader]

This boundary is also a security boundary. A malformed or hostile reference must be able to fail as an input diagnostic without becoming executable code in the compiler process.

## FSharp.Core risk surface

### Pinned package facts

The production-language dependency inspected with `dotnet-inspect` was exactly:

```powershell
dnx dotnet-inspect -y -- package FSharp.Core@10.0.100 -v:d
dnx dotnet-inspect -y -- library FSharp.Core --package FSharp.Core@10.0.100
dnx dotnet-inspect -y -- member Microsoft.FSharp.Reflection.FSharpType --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- member Microsoft.FSharp.Reflection.FSharpValue --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- member Microsoft.FSharp.Core.CompilerServices.ITypeProvider --package FSharp.Core@10.0.100 --oneline
```

Observed package/binary facts:

| Fact | Value |
| --- | --- |
| Package repository/commit | `dotnet/dotnet` / `b0f34d51fccc69fd334253924abd8d6853fad7aa` |
| Compile assets | `netstandard2.0`, `netstandard2.1`; AnyCPU; no RID asset |
| Assembly identity | `FSharp.Core, Version=10.0.0.0` |
| Package SHA-256 | `B2D5E20EE3683B4C58B3CDAF9BA08D59B4ECC5412B6D67358928AB5328947570` |
| `netstandard2.1` DLL SHA-256 | `09730E5BA269E2A255588DA8839FB5CFF43899DFAFBED0D64E905A6F4171EC5B` |
| Localized satellites | `cs`, `de`, `es`, `fr`, `it`, `ja`, `ko`, `pl`, `pt-BR`, `ru`, `tr`, `zh-Hans`, `zh-Hant` for both TFMs |
| Assembly AOT/trim marker | no `AssemblyMetadata("IsAotCompatible", ...)` or `AssemblyMetadata("IsTrimmable", ...)` observed |

The owning project targets only .NET Standard 2.0/2.1.[^fsharp-core-project] Its reflection signature annotates broad `Type` parameters with `DynamicallyAccessedMemberTypes.All`, and its printf implementation constructs generic methods through reflection.[^fsharp-core-reflection] [^fsharp-core-printf]

The official F# SDK also generates and embeds `ILLink.Substitutions.xml` to remove F# metadata resources during trimming.[^fsharp-illink-substitutions] FSharp2's downstream emission must preserve the compatible substitution/resource contract supplied by MSBuild; the native host must not assume that its own trimmed runtime metadata is an adequate model of target F# metadata.

### Narrow NativeAOT probes

The probe directory was `C:\tmp\fsharp2-issue4-main-probe`. It used SDK `10.0.110`, runtime `10.0.10`, `win-x64`, `net10.0`, an explicit FSharp.Core `10.0.100` reference with the SDK implicit reference disabled, `PublishAot=true`, `IsAotCompatible=true`, and warnings as errors.

Strict command:

```powershell
dotnet publish Probe.fsproj -c Release -r win-x64 --self-contained true -v minimal
```

Diagnostic expansion:

```powershell
dotnet publish Probe.fsproj -c Release -r win-x64 --self-contained true -v minimal `
  -p:TreatWarningsAsErrors=false -p:TrimmerSingleWarn=false
```

Results:

1. `[1..100] |> List.sum` plus `printfn` failed the strict publish with aggregate IL3053/IL2104. Expanded output exposed IL3050, IL2060, IL2067, IL2070, IL2072, IL2075, IL2080, and IL2055 in FSharp.Core Printf/structured-formatting paths, including `MakeGenericMethod` and `MakeGenericType`.
2. Replacing only `printfn` with `System.Console.WriteLine` published with zero warnings. The native executable printed `RuntimeFeature.IsDynamicCodeSupported=false`, `RuntimeFeature.IsDynamicCodeCompiled=false`, and the sum `5050`.
3. The publish directory contained `Probe.exe` (2,515,968 bytes) and native `Probe.pdb` (14,348,288 bytes). This matches the documented Windows native-symbol shape, not a portable managed PDB assumption.
4. Binary annotation inspection found 12 public `FSharpType` methods and 12 public `FSharpValue` methods with at least one `Type` parameter annotated `DynamicallyAccessedMembers(All)`.
5. A standalone SDK-style F# probe implicitly selected FSharp.Core `10.0.110`; adding `10.0.100` explicitly without disabling the implicit reference produced NU1504. Exact host dependency pinning therefore requires an early `DisableImplicitFSharpCoreReference=true` plus one explicit package reference.

These observations prove that ordinary F# data structures can participate in one warning-free native closure and that `printfn` cannot enter the production baseline unchanged. They do **not** certify all list/option/async/quotation/reflection APIs or another RID.

### Boundary for production F# code

- F# records, discriminated unions, options, lists, arrays, functions, tasks, and ordinary pattern matching are allowed when the final strict host publish remains clean.
- Production diagnostics and tracing do not use `printf`, `printfn`, `sprintf`, `%A`, or general structured formatting until a specific implementation and format corpus passes the gate. Prefer direct `Console`/writer calls, ordinal/invariant formatting, and explicit structured fields.
- `FSharpType`, `FSharpValue`, `LeafExpressionConverter.EvaluateQuotation`, and runtime quotation evaluation stay out of the default compiler pipeline. A later use needs its own warning/size/behavior proof.
- Host code must not use FSharp.Core reflection to infer the compiler's own record/union schema. Static functions and explicit tables are clearer AOT roots.
- FSharp.Core updates are deliberate compatibility changes: record package version, repository commit, hashes, warning diff, size diff, and all six host gates.
- The FSharp.Core referenced by emitted code comes from the target invocation's resolved references. It is not the compiler host's pinned runtime dependency.

## F# and C# implementation seam

**Fixed project decision.** [ADR 0007](../adr/0007-use-fsharp-with-benchmark-justified-csharp-kernels.md) keeps the semantic pipeline in F#. A C# layer is not introduced merely because C# receives earlier Roslyn AOT diagnostics.

**Issue #4 boundary.** The initial host and semantic pipeline remain F#. The final native publish normalizes safety evidence across both languages. A C# project is added only for a narrow benchmark-proved kernel under ADR 0007, and then:

- it independently sets `IsAotCompatible=true` and is source-analyzer clean;
- it is statically referenced by the host, never discovered by assembly name;
- it exposes a small static API over owned values or bounded `Span`/`ReadOnlySpan` calls;
- ref structs, pinned memory, and borrowed spans do not escape into stored F# semantic state;
- it accepts FSharp2 metadata IDs/value models rather than `Assembly`, `Type`, `MethodInfo`, or provider objects; and
- it has a behaviorally equivalent portable fallback when it uses hardware intrinsics or platform-specific acceleration, as required by [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md).

The same rule applies in reverse: a convenient F# wrapper cannot hide a C# reflection/dynamic-code implementation from the native publisher.

## Arbitrary type providers and plugins

The FSharp.Core contract is executable, not metadata-only:

- `TypeProviderAttribute` marks a class that extends the compiler;
- `TypeProviderAssemblyAttribute` names a corresponding design-time assembly, which may differ from the runtime assembly;
- the compiler constructs a provider with `TypeProviderConfig`, including reference paths, resolution/temp folders, and runtime information; and
- `ITypeProvider` returns runtime `Type` values, accepts `MethodBase`/`Assembly`, returns quotations and generated assembly bytes, and raises invalidation events.[^type-provider-contract]

Those requirements collide directly with NativeAOT's closed world and prohibition on arbitrary dynamic assembly loading. Consequently:

1. The production host has no generic plugin loader and never treats `--compilertool` or a referenced `TypeProviderAssemblyAttribute` as permission to call `Assembly.Load`.
2. Every Experimental Vertical Milestone declares whether type providers are excluded. When excluded, detection produces a clear unsupported-envelope diagnostic before partial output is published.
3. Provider assemblies remain metadata-only inputs to the NativeAOT host. In an enabled [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) envelope, the managed Provider Broker alone loads the selected design-time assembly as executable user input; provider objects never enter the native process.
4. Oracle infrastructure may run providers through the official managed compiler for differential evidence, but production may not delegate a successful compilation to that compiler.

### Resolved follow-on decision

[ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) selects a separate, managed, out-of-process **Provider Broker** for final legacy-provider compatibility. The NativeAOT compiler remains closed-world and owns every compiler decision; the broker executes only the public provider contract and has no FCS/compiler fallback path. Provider inputs remain fail-explicitly unsupported in experimental envelopes until the broker's declared gate passes. A new ahead-of-time provider snapshot contract may improve deterministic cross-session reuse, but is additive rather than a substitute for existing providers. See the [type-provider boundary research](type-provider-compatibility-boundary.md) for trust, protocol, invalidation, cache, diagnostics, performance, and platform constraints.

## Supported build and execution matrix

The [public .NET 9+ NativeAOT table][nativeaot-platforms] includes all six pairs fixed by [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md). SDK pack availability for additional RIDs is not a FSharp2 support promise. Microsoft's required native build tools are Visual Studio C++ on Windows, Clang/linker/zlib development packages on Linux, and Xcode Command Line Tools on macOS.[^nativeaot-prerequisites]

| Required pair | Host RID | Required native toolchain | Release evidence |
| --- | --- | --- | --- |
| Windows x64 | `win-x64` | Visual Studio 2022+ Desktop development with C++ | strict publish; install; native execute; compile tracer; symbols/dependency manifest |
| Windows ARM64 | `win-arm64` | VS 2022+ C++ target toolchain for ARM64 | same on ARM64, or a separately proved Windows x64-to-ARM64 cross lane plus ARM64 execution |
| Linux x64 | `linux-x64` | `clang`, linker/build tools, zlib development package, glibc sysroot | build on oldest supported distro baseline; install/run in clean x64 image |
| Linux ARM64 | `linux-arm64` | target-compatible `clang`/linker, C runtime objects, zlib, `objcopy`/`strip` | same on ARM64, or proved Linux cross lane plus ARM64 execution |
| macOS x64 | `osx-x64` | current supported Xcode Command Line Tools | strict publish/install/native execution on supported x64 macOS |
| macOS ARM64 | `osx-arm64` | current supported Xcode Command Line Tools | same on Apple Silicon |

NativeAOT does not cross-compile across operating systems. Microsoft documents limited same-OS x64/Arm64 cross-compilation when the target toolchain is present.[^nativeaot-cross] The pinned SDK's RID-specific tool packaging target nevertheless refuses to orchestrate AOT child packages in one cross-platform/cross-architecture pack invocation.[^packtool-aot] FSharp2 release automation should therefore stage one independently gated publish per RID and assemble integration packages only after all required artifacts exist.

The Linux support name needs precision. `linux-x64`/`linux-arm64` are the initial glibc-family assets. Alpine/musl requires distinct `linux-musl-*` assets; SDK pack existence is not proof that FSharp2 supports them. Microsoft also warns that a native Linux binary generally runs only on the same or a newer distribution baseline than the build machine.[^nativeaot-linux-baseline] The minimum glibc/distribution floor and whether musl joins the product matrix need a follow-on deployment decision and clean-image evidence.

No release publish uses a “native/current CPU” instruction-set setting for a portable asset. Platform acceleration must be guarded and behaviorally equivalent, and the portable fallback runs in the matrix.

## RID-specific packaging and deployment

### Host selection

The compiler runs on the build machine while its managed output may target a different OS, architecture, or TFM. The MSBuild integration therefore selects its executable from `$(NETCoreSdkPortableRuntimeIdentifier)` (with a tested mapping/failure policy), not from the consuming project's `$(RuntimeIdentifier)`, `$(RuntimeIdentifiers)`, `TargetFramework`, or `PlatformTarget`. The SDK itself uses that property when it infers the current runtime identifier.[^sdk-host-rid]

For example, an `win-x64` SDK compiling a project for `linux-arm64` must execute the `win-x64` FSharp2 host and consume Linux/ARM64 target references. Selecting `linux-arm64` as the compiler executable would confuse host deployment with output targeting and violate [ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md).

The package must:

- contain or resolve exactly one native host payload for each supported build-host RID;
- fail clearly for an unsupported/missing host RID rather than falling back to managed FSharp2, SDK `fsc`, or FCS;
- preserve executable permissions and select `.exe` only on Windows;
- verify payload hashes and invoke the installed NuGet-cache asset in tests, not only a publish-directory copy;
- keep native debug symbols as release/debug artifacts with the [documented `.pdb`, `.dbg`, or `.dSYM` shape][nativeaot-symbols]; and
- record SDK/runtime/ILCompiler version, source commit, RID, build OS baseline, native toolchain, publish properties, native dependencies, and hashes in the release manifest.

NativeAOT tool packages are platform-specific executables in the SDK's own packaging model, but its automatic multi-RID pack orchestration is not an AOT cross-build solution. ADR 0019's integration package may use one package containing staged custom RID assets or an outer integration package with RID payload packages; the physical topology remains packaging implementation work. The invariant established here is build-host-RID selection with no fallback.

### Paths and working directory

NativeAOT inherits single-file API behavior: `Assembly.Location` is empty, `Assembly.GetFile(s)` fails, and module names are not file discovery APIs. Microsoft directs apps to `AppContext.BaseDirectory` for files beside the executable and `Environment.ProcessPath` for the executable path.[^single-file-paths]

FSharp2 separates three path domains:

1. **Invocation paths** — response files, sources, references, resources, outputs, and relative command-line paths are interpreted under the caller/MSBuild working-directory contract. The host must not change the current directory to its install directory.
2. **Host assets** — co-packaged read-only data is addressed from an explicit path passed by the target or from `AppContext.BaseDirectory`; prefer embedded resources when practical.
3. **Mutable state** — temporary files and content-addressed caches use explicit, versioned locations and never write beside the installed native executable.

Tests launch the installed host from a different working directory, with spaces/non-ASCII path segments, platform separators, relative response-file inputs, read-only install assets, and a separately configured cache root. Path keys do not globally lowercase names: actual filesystem semantics and content hashes decide identity.

### Culture, resources, and globalization

`InvariantGlobalization=true` removes culture-specific data and ICU dependence, but it also changes culture creation, parsing, formatting, and comparison behavior.[^invariant-globalization] The compiler target accepts `--preferreduilang`, and the compatibility contract includes culture as oracle evidence. The production host therefore keeps full globalization (`InvariantGlobalization=false`) unless a later compatibility decision proves an invariant-only envelope.

Implementation rules are:

- language parsing, metadata names, cache keys, protocol fields, hashes, and machine-readable diagnostics use ordinal/invariant rules;
- UI culture affects only explicitly localizable human text, never diagnostic identity, severity, range, exit code, or artifact semantics;
- the requested/preferred UI culture is an explicit invocation and trace input;
- compiler-owned resources use a declared fallback culture, and [`SatelliteResourceLanguages`][satellite-languages] is set deliberately rather than inheriting every transitive package satellite by accident; and
- FSharp.Core implementation exception/resource text is not exposed as stable compiler diagnostics.

The NativeAOT build passes satellite resources to ILCompiler and roots their metadata, so localization is supported only when the intended resources are actually in the publish inputs.[^native-satellites] Release tests include `en-US`, one non-default shipped culture, and missing-culture fallback on every OS family.

Full globalization still has native deployment implications. Linux terminates if it cannot load required ICU and invariant mode is off; Windows can fall back to NLS, which can produce different collation behavior.[^globalization-icu] A release manifest therefore records whether each payload relies on system ICU or deliberately ships app-local ICU, and a clean-image smoke proves that choice. Compiler semantic operations must remain invariant so ICU/NLS/CLDR differences cannot alter output.

## Two distinct NativeAOT release gates

### Gate A — native compiler host

Purpose: prove the **compiler process and its complete production closure** are NativeAOT-safe for a build-host RID.

For each of the six RIDs:

```text
dotnet publish <FSharp2.Host.fsproj> -c Release -r <host-rid> --self-contained true
  -p:PublishAot=true
  -p:TreatWarningsAsErrors=true
  -p:ILLinkTreatWarningsAsErrors=true
  -p:IlcTreatWarningsAsErrors=true
  -p:SuppressTrimAnalysisWarnings=false
  -p:SuppressAotAnalysisWarnings=false
  -p:TrimmerSingleWarn=false
```

Required evidence:

1. restore/project/native-input graph contains only approved production dependencies and no official compiler/FCS fallback;
2. publish exits zero with no trim, single-file, AOT, or reference-compatibility warning hidden by suppression;
3. the RID-specific native executable is packaged, restored/installed through the real integration package, and launched directly—not with `dotnet <dll>`;
4. a test-only native diagnostic proves dynamic code support/compilation false;
5. from an unrelated working directory, the host performs a real declared Compiler Target Invocation and produces all requested managed artifacts;
6. path, resource/UI-culture, cancellation, failure cleanup, and native dependency smokes pass in a clean target environment; and
7. once the persistent service exists, standalone and service NativeAOT paths both pass and produce equivalent observable results under [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md).

### Gate B — NativeAOT consumption of emitted managed output

Purpose: prove FSharp2's **managed PE/PDB/metadata output** remains a valid input to the standard .NET 10 NativeAOT toolchain.

For every declared compatibility envelope:

1. invoke FSharp2 through its real MSBuild target and emit a managed library/application plus the requested PDB, reference assembly, resources, F# metadata, and trimming substitutions;
2. inspect that output as metadata and assert it references the invocation's target framework/FSharp.Core, not compiler-host assemblies;
3. consume the emitted managed artifact from a small standard-SDK `net10.0` driver (a C# driver is useful to isolate the FSharp2-produced assembly from a second F# compile);
4. publish that driver with the same strict NativeAOT warning policy for each downstream runtime RID;
5. execute each native consumer and compare public API, runtime behavior, resources, exceptions, and deterministic inputs with the same source built by the pinned Compatibility Oracle; and
6. include negative tracers where source or a dependency is intentionally AOT-incompatible, verifying FSharp2 preserves the relevant code-analysis attributes and yields the same downstream warning class rather than claiming every source program is warning-free.

Gate B does not ask FSharp2 to emit native machine code. The SDK/ILCompiler owns that transformation. It asks FSharp2 to emit valid managed IL and metadata without compiler-injected dynamic requirements, missing preservation annotations, invalid generic metadata, host-framework leakage, or broken resources.

The six host publishes and six downstream native-consumer publishes are separately reported. A single `PublishAot passed` checkbox, a native host that never compiles, or an emitted DLL that was merely loaded by CoreCLR is insufficient evidence.

## What issue #4 establishes

The following are now constraints rather than open implementation preferences:

1. closed production graph; official compiler/FCS only in out-of-process test/oracle infrastructure;
2. `net10.0`, `IsAotCompatible`, strict unsuppressed ILLink/ILCompiler policy for every production assembly;
3. final NativeAOT publish as the authoritative F# analyzer gate;
4. metadata-only target-reference universe based on `System.Reflection.Metadata`;
5. no dynamic assembly loading, runtime code generation, or generic plugin discovery in production;
6. proved-subset FSharp.Core policy, with Printf/structured reflection excluded from the baseline;
7. primarily F# implementation with only benchmark-justified, statically linked C# kernels;
8. six build-host RID payloads selected from the SDK host RID and independently executed;
9. full-globalization baseline with invariant compiler semantics and explicit satellite/ICU evidence; and
10. separate host and emitted-output NativeAOT matrices.

## Deferred to later map work

This constraints ticket does not implement the host or copy the official F# compiler. Later map tickets retain ownership of:

- the differential compatibility and performance harness ([issue #5][issue-5]);
- the exact first IcedTasks language/diagnostic/artifact envelope ([issue #6][issue-6]);
- the metadata, IL, and PDB emitter architecture ([issue #7][issue-7]);
- the minimum end-to-end compiler prototype ([issue #8][issue-8]);
- parser, typechecker, optimizer, metadata reader, remaining emitter, and diagnostic implementation;
- standalone/persistent service protocol, cache representation, invalidation, and equivalence gates;
- benchmark evidence for any C# kernel, spans/pools/arenas/unsafe code, or platform acceleration;
- the NuGet props/targets and physical RID-package topology required by ADR 0019;
- the full differential corpus for hidden options, resources, signing, PDBs, F# metadata, and failure cleanup from the compiler/MSBuild contract;
- release engineering, signing/notarization, SBOM/provenance, and supported OS-version floors; and
- implementation and compatibility evidence for the selected Provider Broker policy, plus the Linux glibc/musl support decision.

Those tickets must inherit the boundaries above; they may refine implementations and evidence, but cannot replace a native host with a managed fallback or collapse the two NativeAOT gates.

## Residual uncertainties and refresh triggers

1. **Servicing drift.** The repo asks for SDK `10.0.100` with feature roll-forward; this observation selected `10.0.110`. Refresh target source, analyzer behavior, runtime packs, FSharp.Core closure, warnings, sizes, and hashes for each servicing update.
2. **F# analyzer integration.** SDK `10.0.110` does not import the ILLink Roslyn analyzer target for F#. A later SDK/F# compiler may add equivalent analysis. Keep the publish gate even if earlier diagnostics improve.
3. **FSharp.Core coverage.** The successful collection probe is narrow. Each newly used FSharp.Core subsystem needs warning/behavior/size coverage; lack of an AOT marker is not proof of failure, and one successful publish is not blanket proof.
4. **Type providers.** [ADR 0025](../adr/0025-use-a-managed-broker-for-legacy-type-providers.md) resolves the execution boundary; exact Oracle probing/load behavior, provider-visible configuration, and the final compatibility corpus remain implementation evidence to discover rather than architecture still to choose.
5. **Linux portability.** The glibc/distribution floor and musl support are not settled by the abstract word “Linux.”
6. **Localization.** Satellite publication is supported, but the exact final oracle culture set and ICU deployment strategy still require cross-platform runtime evidence.
7. **Cross-architecture builds.** Microsoft supports limited same-OS x64/Arm64 cross-compilation with target toolchains; FSharp2 should not claim a cross lane until the resulting target artifact runs on real target hardware.

## Primary sources

- [.NET 10 NativeAOT deployment, analyzers, prerequisites, limitations, platform matrix, Linux baseline, and native symbols][nativeaot-overview]
- [Pinned NativeAOT build contract and invalid property/OS combinations][nativeaot-publish-target]
- [Pinned runtime dynamic-code feature values][runtime-feature]
- [Pinned NativeAOT static-reflection metadata policy][static-reflection]
- [Official AOT warning guidance for runtime generic construction][aot-generic-warning]
- [Official trimming warning and annotation guidance][trim-warning-guide]
- [Pinned SDK publish and analyzer-property inference][sdk-publish-target] [sdk-analyzer-target]
- [Pinned SDK language-conditional analyzer import][sdk-main-target]
- [Pinned ILLink warning/single-warning defaults][illink-warning-target]
- [Pinned SDK NativeAOT RID-specific tool packaging behavior][packtool-aot]
- [Official NativeAOT cross-compilation constraints][nativeaot-cross]
- [Pinned F# ILLink substitutions target][fsharp-illink-substitutions]
- [FSharp.Core target frameworks, reflection annotations, printf reflection, and type-provider contract][fsharp-core-project] [fsharp-core-reflection] [fsharp-core-printf] [type-provider-contract]
- [Pinned metadata-only PE reader source][metadata-reader]
- [Official single-file path/API incompatibilities][single-file-paths]
- [Official globalization/ICU behavior and invariant-mode design][globalization-icu] [invariant-globalization]
- [Official satellite-language publish property and NativeAOT satellite inputs][satellite-languages] [native-satellites]
- [Versioned FSharp2 compiler/MSBuild contract](dotnet-10-fsharp-compiler-msbuild-contract.md)
- [Versioned official IcedTasks compiler baseline](icedtasks-official-compiler-target-baseline.md)

[^sdk-aot]: [`PublishAot` implies trimming and `IsAotCompatible` implies `IsTrimmable`][sdk-publish-target]; [the documented four analyzer defaults][nativeaot-analyzers].
[^fsharp-analyzer-gap]: [SDK F#/ILCompiler/ILLink imports and C#/VB-only analyzer import][sdk-main-target]; [the analyzer inference target][sdk-analyzer-target].
[^illink-warning-target]: [ILLink warning task inputs][illink-warning-target] and [warning/single-warning defaults][illink-warning-defaults].
[^trim-annotations]: [Library trimming guidance on reflection, annotations, suppressions, and `DynamicDependency`][trim-annotations].
[^verify-reference-aot]: [`.NET 10` reference AOT metadata verification and caveats][verify-reference-aot].
[^static-reflection]: [NativeAOT's usage-based metadata rule for statically used types/methods][static-reflection].
[^aot-generic-warning]: [NativeAOT IL3050 and missing runtime generic code guidance][aot-generic-warning].
[^runtime-feature]: [NativeAOT `RuntimeFeature` implementation][runtime-feature].
[^metadata-reader]: [`PEReader` source][pe-reader] and [`MetadataReader` source][metadata-reader].
[^fsharp-core-project]: [Pinned FSharp.Core project target frameworks][fsharp-core-project].
[^fsharp-core-reflection]: [Pinned broad FSharp.Reflection annotations][fsharp-core-reflection] and [implementation generic construction][fsharp-core-reflect-impl].
[^fsharp-core-printf]: [Pinned FSharp.Core printf generic reflection][fsharp-core-printf].
[^fsharp-illink-substitutions]: [Pinned F# SDK generation of ILLink substitutions][fsharp-illink-substitutions].
[^type-provider-contract]: [Pinned FSharp.Core type-provider attribute/config/interface contract][type-provider-contract].
[^nativeaot-cross]: [Official same-OS and target-toolchain cross-compilation limits][nativeaot-cross].
[^packtool-aot]: [Pinned SDK platform-specific executable selection][packtool-aot] and [AOT multi-RID orchestration limit][packtool-aot-orchestrator].
[^nativeaot-prerequisites]: [Official Windows, Linux, and macOS native toolchain prerequisites][nativeaot-prerequisites].
[^nativeaot-linux-baseline]: [Official Linux deployment-baseline warning][nativeaot-linux-baseline].
[^sdk-host-rid]: [Pinned SDK current-host RID inference][sdk-rid-inference].
[^single-file-paths]: [Official single-file incompatible APIs and path alternatives][single-file-paths].
[^invariant-globalization]: [Pinned invariant-globalization behavior][invariant-globalization].
[^native-satellites]: [Pinned ILCompiler satellite input][native-satellites] and [satellite metadata rooting][native-satellite-root].
[^globalization-icu]: [Pinned platform ICU resolution behavior][globalization-icu].

[issue-1]: https://github.com/TheAngryByrd/fsharp2/issues/1
[issue-4]: https://github.com/TheAngryByrd/fsharp2/issues/4
[issue-5]: https://github.com/TheAngryByrd/fsharp2/issues/5
[issue-6]: https://github.com/TheAngryByrd/fsharp2/issues/6
[issue-7]: https://github.com/TheAngryByrd/fsharp2/issues/7
[issue-8]: https://github.com/TheAngryByrd/fsharp2/issues/8
[nativeaot-overview]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md
[nativeaot-analyzers]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L91-L114
[verify-reference-aot]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L127-L148
[nativeaot-limitations]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L166-L183
[nativeaot-prerequisites]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L17-L61
[nativeaot-linux-baseline]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L77-L89
[nativeaot-symbols]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L150-L163
[nativeaot-platforms]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L203-L215
[nativeaot-publish-target]: https://github.com/dotnet/runtime/blob/v10.0.10/src/coreclr/nativeaot/BuildIntegration/Microsoft.NETCore.Native.Publish.targets#L42-L67
[runtime-feature]: https://github.com/dotnet/runtime/blob/v10.0.10/src/coreclr/nativeaot/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeFeature.NativeAot.cs#L8-L14
[static-reflection]: https://github.com/dotnet/runtime/blob/v10.0.10/src/coreclr/tools/aot/ILCompiler.Compiler/Compiler/UsageBasedMetadataManager.cs#L33-L35
[aot-generic-warning]: https://github.com/dotnet/docs/blob/020cd4a325f2f29d82412789c9d679f2940272ae/docs/core/deploying/native-aot/fixing-warnings.md#L38-L63
[trim-warning-guide]: https://github.com/dotnet/docs/blob/5aa08ccf9eb8d89860d21a4ccb3e12212e19d8f9/docs/core/deploying/trimming/fixing-warnings.md#L12-L33
[trim-annotations]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/deploying/trimming/prepare-libraries-for-trimming.md#L134-L229
[sdk-publish-target]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.Publish.targets#L20-L38
[sdk-analyzer-target]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.Sdk.Analyzers.targets#L75-L108
[sdk-main-target]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.Sdk.targets#L1388-L1393
[illink-warning-target]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/tools/illink/src/ILLink.Tasks/build/Microsoft.NET.ILLink.targets#L168-L173
[illink-warning-defaults]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/tools/illink/src/ILLink.Tasks/build/Microsoft.NET.ILLink.targets#L239-L241
[packtool-aot]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.PackTool.targets#L145-L163
[packtool-aot-orchestrator]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.PackTool.targets#L424-L431
[sdk-rid-inference]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.RuntimeIdentifierInference.targets#L103-L123
[nativeaot-cross]: https://github.com/dotnet/docs/blob/e12212912072f74edfac055eb7cffce11d328136/docs/core/deploying/native-aot/cross-compile.md#L12-L40
[fsharp-illink-substitutions]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.NetSdk.targets#L157-L167
[fsharp-core-project]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/FSharp.Core/FSharp.Core.fsproj#L1-L15
[fsharp-core-reflection]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/FSharp.Core/reflect.fsi#L689-L887
[fsharp-core-reflect-impl]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/FSharp.Core/reflect.fs#L708-L775
[fsharp-core-printf]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/FSharp.Core/printf.fs#L997-L1133
[type-provider-contract]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/FSharp.Core/fslib-extra-pervasives.fsi#L387-L566
[pe-reader]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/PEReader.cs
[metadata-reader]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/MetadataReader.cs
[single-file-paths]: https://github.com/dotnet/docs/blob/6e25c3be15d3ec2a0f0441614e95c8da356d8dc2/docs/core/deploying/single-file/overview.md#L187-L210
[satellite-languages]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/project-sdk/msbuild-props.md#L658-L673
[native-satellites]: https://github.com/dotnet/runtime/blob/v10.0.10/src/coreclr/nativeaot/BuildIntegration/Microsoft.NETCore.Native.targets#L233-L236
[native-satellite-root]: https://github.com/dotnet/runtime/blob/v10.0.10/src/coreclr/tools/aot/ILCompiler.Compiler/Compiler/DependencyAnalysis/ModuleMetadataNode.cs#L47-L52
[globalization-icu]: https://github.com/dotnet/docs/blob/156931bb4ec1e81b028c76ea983553f2e9778bdd/docs/core/extensions/globalization-icu.md#L331-L353
[invariant-globalization]: https://github.com/dotnet/runtime/blob/v10.0.10/docs/design/features/globalization-invariant-mode.md#L31-L45
