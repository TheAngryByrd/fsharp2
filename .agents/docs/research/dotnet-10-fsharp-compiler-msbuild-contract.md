# .NET 10 F# compiler and MSBuild contract

Status: versioned research snapshot for [issue #2](https://github.com/TheAngryByrd/fsharp2/issues/2), supporting the destination in [issue #1](https://github.com/TheAngryByrd/fsharp2/issues/1)
Observed: 2026-07-17 on Windows x64
Compatibility target: [ADR 0004](../adr/0004-target-compiler-and-msbuild-compatibility.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), and [ADR 0006](../adr/0006-require-behavioral-not-byte-identical-output.md)

## Answer

FSharp2's final Compatibility Gate must reproduce the behavior visible at the .NET 10 F# **compiler target boundary**, not the internal architecture of the official compiler. That boundary includes:

- the SDK's `.fsproj` selection seam and `CoreCompile` target contract;
- the `Fsc` MSBuild task inputs, command construction, command-line capture, skip behavior, and failure propagation;
- all standard MSBuild-emitted switches and ordered source/reference/resource inputs;
- the public `fsc` option surface, response-file grammar, accepted .NET 10 language-version matrix, diagnostic identity/severity/location behavior, and process exit behavior;
- compiler-owned DLL/EXE, portable or embedded PDB, reference assembly, XML documentation/signature, resource, metadata, signing, and determinism behavior; and
- the resulting SDK build artifact shape, while retaining the ownership distinction between compiler outputs and downstream SDK outputs such as `.deps.json`.

Every Experimental Vertical Milestone must traverse that complete target-to-artifact path for a **declared subset** of language versions, inputs, switches, references, diagnostics, and artifacts. Integration invariants such as source order, resolved-reference consumption, task failure propagation, and [no production fallback](../adr/0010-forbid-production-fallback-to-the-official-compiler.md) are not optional subsets. [ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md) already fixes the first IcedTasks target-framework slice as `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0`; the language/input/diagnostic/artifact subset beyond those TFMs still has to be measured and declared by issue #6. The final Compatibility Gate covers the full, then-current .NET 10 oracle surface.

This is a snapshot, not timeless truth. The repo asks for SDK `10.0.100` with `rollForward: feature`, but that SDK is not installed. On the observation date, the resolver selected installed SDK `10.0.110`. A compatibility run therefore needs a recorded oracle manifest and a deliberate refresh whenever the latest .NET 10 servicing SDK changes.

## Evidence vocabulary

- **Observed** means a command was run against the installed binaries or an evaluated build was inspected locally.
- **Shipped contract** means behavior encoded by files installed with the selected SDK and corroborated by first-party source at the binary's recorded repository commit.
- **Public contract** means the surface printed by `fsc --help`/`--full-help` or selected by the standard SDK targets.
- **Implementation detail** means accepted hidden/test switches, host-object behavior, temporary response-file mechanics, or another behavior not advertised by public help or standard targets. It is still inventoried so the project can make an explicit compatibility decision.

## Oracle snapshot

The authoritative local command was:

```powershell
dotnet --info
dotnet "C:\Program Files\dotnet\sdk\10.0.110\FSharp\fsc.dll" --version
dotnet "C:\Program Files\dotnet\sdk\10.0.110\FSharp\fsc.dll" --langversion:?
```

| Component | Observed identity |
| --- | --- |
| Repo request | `global.json`: `10.0.100`, `rollForward: feature` |
| Selected SDK | `10.0.110`, commit `f7d90799ce4ef09a0bb257852a57248d2a8fb8dd` |
| MSBuild | `18.0.11+f7d90799c` |
| Host/runtime | .NET host and `Microsoft.NETCore.App` `10.0.10`, RID `win-x64` |
| Compiler banner | `Microsoft (R) F# Compiler version 14.0.110.0 for F# 10.0` |
| Compiler runtime/dependency identity | `fsc.runtimeconfig.json`: `net10.0` / `Microsoft.NETCore.App` `10.0.10`; `fsc.deps.json`: `Microsoft.FSharp.Compiler` `14.0.110-servicing.26326.116` |
| `fsc.dll` | file version `14.1.1026.32716`; product `14.0.110-servicing.26326.116+f7d90799...`; SHA-256 `8CB573BE4B7F70886929A572024AB26FC1893CFDD795EF124C58627610DDBF22` |
| `FSharp.Build.dll` | same file/product version family; SHA-256 `AB62157B2111AE5F172E863D086F7035503F83C0BDB9A9EFC2E2450891DF6E1A` |
| `FSharp.Compiler.Service.dll` | file `43.1001.1026.32716`; product `43.10.110-servicing.26326.116+f7d90799...`; SHA-256 `783F1BB555880B64E0BA3D571D65AB220EADF2C34DCC0A8DB1CC6DEF8ED3C181` |
| Bundled FSharp.Core package version | `FSCorePackageVersion=10.0.110` in `Microsoft.FSharp.Core.NetSdk.props` |

The local oracle root is exactly `C:\Program Files\dotnet\sdk\10.0.110\FSharp`. The three build files most directly defining the boundary have these hashes:

| Installed file | SHA-256 |
| --- | --- |
| `Microsoft.FSharp.Targets` | `FE0F39FF97942B5250280C071339B1DD5BF22C6488077CAF3295DC70B1DA80C1` |
| `Microsoft.FSharp.NetSdk.targets` | `D0A8BB234D720395FE609DCFD6FB75828C9ED60789F845A50EEBE1F02DB91B87` |
| `Microsoft.FSharp.NetSdk.props` | `7729DB5F3C8A28E25BBA5D1A600F2EFB65E9A943C66799EE6856DD5D3D00632F` |

The compiler assemblies record `https://github.com/dotnet/dotnet` and the commit above. The matching VMR source is therefore pinned throughout this note rather than using `dotnet/fsharp` main. See the [pinned `fsc` task][fsc-task-source], [compiler option definitions][compiler-options-source], and [compiler entry point][fsc-main-source].

## End-to-end build boundary

```text
.fsproj
  -> Microsoft.NET.Sdk F# props/target shims
  -> Microsoft.FSharp.NetSdk.props / .targets
  -> restore + ResolveReferences + reference-assembly selection
  -> generated F# sources/resources/SourceLink/path map
  -> Microsoft.FSharp.Targets::CoreCompile
  -> FSharp.Build.dll::Fsc
  -> dotnet.exe  <sdk>\FSharp\fsc.dll  @<temporary-response-file>
  -> compiler-owned obj artifacts
  -> common SDK copy/reference/deps targets
  -> final bin artifacts
```

This division matters. FSharp2 must consume the same evaluated inputs and produce compatible compiler outputs; it should not reimplement NuGet asset selection or claim ownership of `.deps.json` generation.

## Command-line contract

### Standard SDK invocation

For a .NET 10 SDK-style `.fsproj`, the installed props set:

```text
FscToolPath             = directory containing DOTNET_HOST_PATH
FscToolExe              = dotnet.exe
DotnetFscCompilerPath   = "<sdk>\FSharp\fsc.dll"
```

The `Fsc` task puts the quoted `fsc.dll` path on the tool command line and puts compiler arguments in a response file. The compiler response parser expands an argument beginning with `@`; it reads with BOM detection, ignores empty lines and lines whose first character is `#`, trims every other line, and recursively processes the resulting arguments. Option parsing accepts `-`/`--` and `/` prefixes, uses `:` between option and value, accepts `+`/`-` boolean suffixes, and splits list values on comma or semicolon. Sources are otherwise positional and order-sensitive. These are shipped behaviors, not proposed FSharp2 syntax. [The exact parser is pinned here.][response-parser-source]

An evaluated Release build of `src/fsharp2/fsharp2.fsproj` logged this shape (reference arguments abbreviated):

```text
C:\Program Files\dotnet\dotnet.exe
  "C:\Program Files\dotnet\sdk\10.0.110\FSharp\fsc.dll"
  -o:obj\Release\net10.0\fsharp2.dll
  --debug:portable
  --embed:<generated-tfm-attributes.fs>
  --embed:<ILLink.Substitutions.xml>
  --sourcelink:<project.sourcelink.json>
  --noframework
  --define:TRACE --define:RELEASE --define:NET --define:NET10_0 ...
  --doc:obj\Release\net10.0\fsharp2.xml
  --optimize+
  -r:<one concrete path for every ReferencePathWithRefAssemblies item> ...
  --target:library
  --warn:3
  --warnaserror:3239
  --fullpaths --flaterrors --highentropyva+
  --targetprofile:netcore
  --nocopyfsharpcore
  --deterministic+
  --simpleresolution
  --refout:obj\Release\net10.0\refint\fsharp2.dll
  <ordered source files>
```

The task source calls `AppendFileNames` before it calls `AppendSwitch` for `--refout`/`--refonly`, but `FSharpCommandLineBuilder` keeps filenames separate and emits them physically last; the observed invocation therefore has ref-output switches before ordered source filenames as shown above. More subtly, `ProvideCommandLineArgs=true` snapshots ordinary arguments before those calls, snapshots filenames separately, and returns the two snapshots; it does **not** include the later `--refout`/`--refonly`. [See command construction and capture.][fsc-task-source]

That is a task quirk rather than compiler semantics, but it is still required target compatibility. The shipped `CoreCompile` explicitly `Returns="@(FscCommandLineArgs)"`, so build tooling can observe both the omission and order. This local target-result probe forced a non-incremental Release compile and inspected the returned items:

```powershell
$json = dotnet msbuild src/fsharp2/fsharp2.fsproj `
  -t:Rebuild -p:Configuration=Release -p:ProvideCommandLineArgs=true `
  -getTargetResult:CoreCompile -verbosity:quiet
$ids = @((ConvertFrom-Json ($json -join [Environment]::NewLine)).TargetResults.CoreCompile.Items.Identity)
'count={0}' -f $ids.Count
'refout={0}' -f (($ids | Where-Object { $_ -like '--refout:*' }).Count)
'refonly={0}' -f (($ids | Where-Object { $_ -eq '--refonly' }).Count)
$ids | Select-Object -Last 5
```

Observed result: `count=206`, `refout=0`, `refonly=0`; the last five items were `--deterministic+`, `--simpleresolution`, generated TFM attributes, `AssemblyInfo.fs`, and `Library.fs`. The successful build nevertheless produced `obj/Release/net10.0/refint/fsharp2.dll`, corroborating the task source: the physical compiler invocation received `--refout`, while the returned list omitted it and kept sources last.

This output has a first-party consumer: Visual Studio's F# `CompileDesignTime` target returns `@(FscCommandLineArgs)` to the language service, whose parser separates references, sources, and remaining options from that ordered list. [See the design-time target][fsharp-design-time-target-source] and [command-line parser service.][fsharp-command-line-parser-source] A replacement target must therefore preserve the exact returned-list order and `refout`/`refonly` omission as well as the physical compiler argument order, unless it deliberately versions and validates a replacement design-time contract.

Debug configuration changes the relevant code-generation input rather than choosing a different compiler path: the observed Debug invocation included `-g --debug:portable --optimize- --tailcalls- --deterministic+`; Release used `--debug:portable --optimize+`, left tail calls at the compiler default-on setting, and retained determinism.

### Public option inventory for 14.0.110

The following spellings are the complete categories printed by both `--help` and `--full-help` in this snapshot. The generated help is backed by the public option blocks in [`CompilerOptions.fs`][public-option-blocks].

| Category | Accepted public options |
| --- | --- |
| Output | `--out:<file>` (`-o`); `--target:exe`; `--target:winexe`; `--target:library` (`-a`); `--target:module`; `--delaysign[+|-]`; `--publicsign[+|-]`; `--doc:<file>`; `--keyfile:<file>`; `--platform:<string>`; `--compressmetadata[+|-]`; `--nooptimizationdata`; `--nointerfacedata`; `--sig:<file>`; `--allsigs`; `--nocopyfsharpcore`; `--refonly[+|-]`; `--refout:<file>` |
| Input | `--reference:<file>` (`-r`); `--compilertool:<file>` (`-t`); positional source filenames |
| Resources | `--win32icon:<file>`; `--win32res:<file>`; `--win32manifest:<file>`; `--nowin32manifest`; `--resource:<file>[,<name>[,public|private]]`; `--linkresource:<file>[,<name>[,public|private]]` |
| Code generation | `--debug[+|-]` (`-g`); `--debug:{full|pdbonly|portable|embedded}`; `--embed[+|-]`; `--embed:<file;...>`; `--sourcelink:<file>`; `--optimize[+|-]` (`-O`); `--tailcalls[+|-]`; `--deterministic[+|-]`; `--realsig[+|-]`; `--pathmap:<path=sourcePath;...>`; `--crossoptimize[+|-]`; `--reflectionfree` |
| Errors/warnings | `--warnaserror[+|-]`; `--warnaserror[+|-]:<warn;...>`; `--warn:<0-5>`; `--nowarn:<warn;...>`; `--warnon:<warn;...>`; `--checknulls[+|-]`; `--consolecolors[+|-]` |
| Language | `--langversion:?`; `--langversion:{version|latest|preview}`; `--checked[+|-]`; `--define:<string>` (`-d`); `--mlcompatibility`; `--strict-indentation[+|-]` |
| Miscellaneous | `--nologo`; `--version`; `--help` (`-?`); `@<file>` |
| Advanced | `--codepage:<n>`; `--utf8output`; `--preferreduilang:<culture>`; `--fullpaths`; `--lib:<dir;...>` (`-I`); `--simpleresolution`; `--targetprofile:{mscorlib|netcore|netstandard}`; `--baseaddress:<address>`; `--checksumalgorithm:{SHA1|SHA256}`; `--noframework`; `--standalone`; `--staticlink:<assembly>`; `--pdb:<file>`; `--highentropyva[+|-]`; `--subsystemversion:<string>`; `--quotations-debug[+|-]` |

The same help reports these direct-CLI defaults: platform `anycpu`; compressed metadata, optimization, tail calls, cross-module optimization, and console colors on; reference-only output, debug information, source embedding, deterministic output, real-signature visibility, warnings-as-errors, null checks, overflow checks, strict-indentation override, high-entropy VA, and quotation debug information off; target profile `mscorlib`; PDB checksum algorithm SHA256. The standard NetSdk target deliberately overrides several of these, notably target profile, determinism, framework/reference selection, optimization by configuration, and diagnostic formatting.

`--langversion:?` exited 0 and printed exactly:

```text
preview
default
latest
latestmajor
4.6
4.7
5.0
6.0
7.0
8.0
9.0
10.0 (Default)
```

The final gate must cover parsing, typing, feature gates, warnings, and output behavior for every accepted value, consistent with [ADR 0014](../adr/0014-support-the-full-dotnet-10-language-version-matrix.md). An experimental envelope may enumerate fewer values, but it cannot silently reinterpret an accepted value inside its declaration.

### Hidden accepted options

`--dumpAllCommandLineOptions` is itself hidden and exits 1 without an input after printing the inventory. It reveals additional accepted names. Some are used by the standard task (`flaterrors`, `vserrors`, `LCID`, `versionfile`, `parallelcompilation`); some are aliases or legacy compatibility switches; many are compiler tests/debug knobs.

The owning source makes that split explicit: option blocks are `PublicOptions` or `PrivateOptions`; generated help enumerates only the public blocks, while parsing flattens both. The private `fsc` surface is composed from internal, abbreviated/alias, deprecated, and testing/QA groups. See the [block model][option-block-model-source], [help filter][option-help-filter-source], [parser flattening][option-parser-flatten-source], and [private composition][private-option-composition-source]. Acceptance is not synonymous with silence: the parser applies each option's warning policy, including `FS0075` for internal/deprecated uses, while some tooling compatibility flags such as `LCID` and `validate-type-providers` are intentional no-ops in this oracle. See the [private tooling block][private-option-behavior-source] and [warning mapping.][private-option-warning-source]

The full hidden-name snapshot is:

```text
typedtree typedtreefile typedtreestamps typedtreeranges typedtreetypes
typedtreevalreprinfo pause bufferwidth detuple simulateException
stackReserveSize tlr finalSimplify parseonly typecheckonly ast tokenize
tokenize-debug tokenize-unfiltered testInteractionParser testparsererrorrecovery
inlinethreshold extraoptimizationloops abortonerror implicitresolution
resolutions resolutionframeworkregistrybase resolutionassemblyfoldersuffix
resolutionassemblyfoldersconditions msbuildresolution alwayscallvirt nodebugdata
parallelreferenceresolution parallelcompilation test vserrors richerrors
validate-type-providers LCID flaterrors sqmsessionguid gccerrors exename maxerrors
noconditionalerasure ignorelinedirectives jit localoptimize splitting versionfile
times showextensionresolution metadataversion d O g i r I o a ? help full-help
light indentation-syntax no-indentation-syntax cliroot jit-optimize
no-jit-optimize jit-tracking no-jit-tracking progress compiling-fslib
compiling-fslib-20 compiling-fslib-40 compiling-fslib-nobigint version
local-optimize no-local-optimize cross-optimize no-cross-optimize
no-string-interning statistics generate-filter-blocks max-errors debug-file
no-debug-file Ooff keycontainer ml-keywords gnu-style-errors
dumpAllCommandLineOptions
```

The accepted names divide into two implementation tiers, but both are observable at the compiler-target boundary:

1. **Task-mapped hidden options** are options the public `Fsc` task can emit from a named property: `flaterrors`, `vserrors`, `LCID`, `versionfile`, and `parallelcompilation`. They are required in every declared envelope that accepts the corresponding task property.
2. **`OtherFlags` hidden options** are the remaining hidden/test/legacy names and aliases. `OtherFlags` is a public task escape hatch that appends its contents to the real compiler invocation, so the official target accepts these names and exposes their effects to an MSBuild caller. [The task source documents this pass-through seam.][fsc-other-flags-source] Under [ADR 0004](../adr/0004-target-compiler-and-msbuild-compatibility.md), the final Compatibility Gate must therefore accept every name in this snapshot and reproduce its observable effect, including a no-op only where the pinned oracle itself is a no-op. An experimental envelope may reject an undeclared hidden option clearly; it may not silently ignore or reinterpret one it claims to accept.

The distinction is useful for milestone ordering, not for removing the second tier from final target compatibility. `--dumpAllCommandLineOptions` itself belongs to the second tier: its inventory text and exit behavior are observable when supplied through `OtherFlags`.

## MSBuild integration contract

### SDK selection seam

The selected SDK recognizes `.fsproj` and imports installed F# shims. In this snapshot:

- `...\Sdks\Microsoft.NET.Sdk\targets\Microsoft.NET.Sdk.FSharp.props` defaults `UseBundledFSharpTargets=true` unless the older `FSharp.NET.Sdk` tool/task path properties are present, resolves `FSharpPropsShim`, and imports `FSharp\Microsoft.FSharp.NetSdk.props`.
- `...\Sdks\Microsoft.NET.Sdk\targets\Microsoft.NET.Sdk.FSharpTargetsShim.targets` resolves `FSharpTargetsShim` to `$(MSBuildToolsPath)\FSharp\Microsoft.FSharp.NetSdk.targets` for a per-TFM build; cross-targeting builds import common cross-targeting targets instead.
- `Microsoft.FSharp.NetSdk.targets` imports `Microsoft.FSharp.Targets`; later SDK targets import `Microsoft.FSharp.Overrides.NetSdk.targets` for generated F# assembly-info source.

Observable override points are `UseBundledFSharpTargets`, `FSharpPropsShim`, `FSharpTargetsShim`, `FSharpOverridesTargetsShim`, `DisableAutoSetFscCompilerPath`, `FSharpBuildAssemblyFile`, `FscToolPath`, `FscToolExe`, and `DotnetFscCompilerPath`. [ADR 0019](../adr/0019-distribute-opt-in-msbuild-integration-by-nuget.md) should plug into an intentional seam here and differential-test the evaluated project, rather than editing the SDK installation.

### Props and generated inputs

`Microsoft.FSharp.NetSdk.props` establishes build-visible defaults including:

- no implicit globbing of F# compile or none items (`EnableDefaultCompileItems=false`, `EnableDefaultNoneItems=false`), so source order remains project data;
- `TRACE`, `Prefer32Bit=false`, `TreatWarningsAsErrors=false`, `WarningLevel=3`, and warning `3239` in `WarningsAsErrors`;
- Debug defaults `DebugSymbols=true`, `Optimize=false`, `Tailcalls=false`; Release defaults `DebugSymbols=false`, `Optimize=true`, `Tailcalls=true`;
- `CompileFirst`, `CompileBefore`, `CompileAfter`, and `CompileLast` item definitions; and
- an implicit `FSharp.Core` package at `10.0.110` unless disabled or an explicit implicit-package version is supplied. Central package management causes the SDK to default the implicit reference off.

These values come from the [pinned NetSdk props][netsdk-props-source] and [FSharp.Core props][fsharp-core-props-source]. This repo additionally sets `DisableImplicitFSharpCoreReference=true`; that is a repo customization, not the general SDK default.

The target graph also generates or orders compiler inputs:

- `GenerateFSharpTextResources` turns `.resx`/embedded text into generated CompileBefore source and resources;
- generated TFM and assembly-info F# source enters before user source;
- `GenerateFSharpILLinkSubstitutions` supplies an embedded `ILLink.Substitutions.xml` unless disabled or compiling FSharp.Core;
- deterministic SourceRoot mapping computes `PathMap`; and
- `FSharpSourceCodeCompileOrder` rebuilds `@(Compile)` in this exact group order: CompileFirst metadata, `@(CompileBefore)`, CompileBefore metadata, ordinary Compile items, CompileAfter metadata, `@(CompileAfter)`, CompileLast metadata. [See the shipped ordering target.][fsharp-targets-order]

Every envelope must preserve this ordering for the item classes it accepts. F# file order changes binding visibility and is semantic, not cosmetic.

### `CoreCompile` target

The F# target replaces the common language `CoreCompile` target. Its shipped signature is:

| Facet | Exact surface |
| --- | --- |
| Name | `CoreCompile` |
| Depends on | `$(CoreCompileDependsOn);FSharpSourceCodeCompileOrder`, where the default F# value is `_ComputeNonExistentFileProperty` |
| Inputs | `$(MSBuildAllProjects)`; `@(CompileBefore)`; `@(Compile)`; `@(CompileAfter)`; `@(FscCompilerTools)`; `@(_CoreCompileResourceInputs)`; `@(ManifestNonResxWithNoCultureOnDisk)`; `$(ApplicationIcon)`; `$(AssemblyOriginatorKeyFile)`; `@(ReferencePathWithRefAssemblies)`; `@(CompiledLicenseFile)`; `@(EmbeddedDocumentation)`; `$(Win32Resource)`; `$(Win32Manifest)`; `@(CustomAdditionalCompileInputs)`; `$(VersionFile)`; `$(KeyOriginatorFile)`; `$(UseSource)`; `$(LoadSource)`; `$(SourceLink)` |
| Outputs | `@(DocFileItem)`; `@(IntermediateAssembly)`; `@(IntermediateRefAssembly)`; `@(_DebugSymbolsIntermediatePath)`; `$(NonExistentFile)`; custom compile outputs |
| Returns | `@(FscCommandLineArgs)` |
| Post-compile hook | `$(TargetsTriggeredByCompilation)` |

The exact XML and `Fsc` invocation are pinned in [`Microsoft.FSharp.Targets` lines 279-426][fsharp-targets-corecompile]. The Inputs/Outputs list is also the MSBuild incremental-build contract: a replacement package must preserve correct invalidation and declared writes, even if it implements warm compilation with a persistent service internally.

### `Fsc` task input/output surface

`Microsoft.FSharp.Targets` loads `Fsc` from `$(FSharpBuildAssemblyFile)`, defaulting to the installed `FSharp.Build.dll`. The task's public properties and argument mapping are pinned in the [task source][fsc-task-properties]. The target supplies the following groups:

| Task input group | Properties/items | Emitted behavior |
| --- | --- | --- |
| Tool selection/control | `ToolPath`, `ToolExe`, `DotnetFscCompilerPath`, `ProvideCommandLineArgs`, `SkipCompilerExecution` | Invoke `dotnet fsc.dll`; optionally capture args; skip returns 0 without running compiler |
| Primary/ref output | `OutputAssembly`, `OutputRefAssembly`, `RefOnly`, `TargetType` | `-o:`, `--refout:`, `--refonly`, canonical `--target:` |
| Text/docs/signatures | `CodePage`, `DocumentationFile`, `GenerateInterfaceFile` | `--codepage:`, `--doc:`, `--sig:` |
| Debug/determinism | `DebugSymbols`, `DebugType`, `PdbFile`, `ChecksumAlgorithm`, `EmbedAllSources`, `Embed`/`EmbeddedFiles`, `SourceLink`, `PathMap`, `Deterministic` | `-g`, debug/PDB/source embedding/SourceLink/path-map/deterministic switches |
| Language/codegen | `LangVersion`, `DefineConstants`, `Optimize`, `Tailcalls`, `ParallelCompilation`, `ReflectionFree`, `RealSig`, `NoInterfaceData`, `NoOptimizationData`, `CompressMetadata`, `Nullable` | Language/define/optimization/metadata/null-check switches; enabling `Nullable` also defines `NULLABLE` |
| References/tools | `References`, `ReferencePath`, `CompilerTools`, `NoFramework`, `TargetProfile` | one `-r:` per reference, `--lib:`, one `--compilertool:` per item, `--noframework`, target profile |
| Diagnostics | `WarningLevel`, `DisabledWarnings`, `WarnOn`, `TreatWarningsAsErrors`, `WarningsAsErrors`, `WarningsNotAsErrors`, `VisualStudioStyleErrors`, `LCID`, `PreferredUILang`, `Utf8Output` | warning filters/promotion, diagnostic style/culture, UTF-8 streams |
| Resources/platform/signing | `Resources`, `UseStandardResourceNames`, `Win32IconFile`, `Win32ResourceFile`, `Win32ManifestFile`, `Platform`, `Prefer32Bit`, `BaseAddress`, `SubsystemVersion`, `HighEntropyVA`, `KeyFile`, `DelaySign`, `PublicSign`, `VersionFile` | resource metadata, PE/platform/Win32/signing/version switches |
| Escape hatch | `OtherFlags` | appended unquoted after normal task-generated options and before positional sources |
| Sources | `Sources` | positional filenames, preserving task input order and emitted physically last |
| Task output | `[Output] CommandLineArgs` -> `@(FscCommandLineArgs)` | captured ordinary args plus sources when `ProvideCommandLineArgs=true`; see the `refout` caveat above |

The task always adds `--fullpaths`, `--flaterrors`, and `--nocopyfsharpcore`; always materializes `--optimize+` or `--optimize-` and `--highentropyva+` or `--highentropyva-`; and only emits `--tailcalls-` when tail calls are disabled. Standard .NET SDK targets set `SimpleResolution=true`, which prefixes `OtherFlags` with `--simpleresolution`. [See exact argument generation.][fsc-task-args]

This mapping is the most direct FSharp2 adapter contract. The replacement can provide its own task or compiler host, but the evaluated property/item behavior and returned command-line items must remain compatible for build tooling.

## Reference-resolution contract

### Standard MSBuild path

Reference selection is upstream MSBuild work:

1. Restore and SDK targets select framework-pack, package, project, and explicit assembly assets for the current TFM/RID/project graph.
2. `FindReferenceAssembliesForReferences` creates `@(ReferencePathWithRefAssemblies)`. When `CompileUsingReferenceAssemblies != false`, it uses each resolved reference's `ReferenceAssembly` metadata when present; otherwise it uses the implementation path. [This selection is in the pinned common targets.][msbuild-reference-selection]
3. F# `CoreCompile` declares that item list as an input and binds `Fsc.References="@(ReferencePathWithRefAssemblies)"`; the task emits one concrete `-r:<path>` per item. The XML also binds `Fsc.ReferencePath="$(ReferencePathWithRefAssemblies)"` as a property expression, but the observed standard build emitted no `--lib:` argument; standalone task callers can populate `ReferencePath` explicitly.
4. Standard NetSdk compilation also passes `--noframework --simpleresolution`, so `fsc` does not invent a default framework closure and resolves the supplied names with directory rules.

The observed library build consumed the .NET 10 reference pack (`Microsoft.NETCore.App.Ref/10.0.10/ref/net10.0`) as explicit `-r:` paths. The observed test-project build additionally consumed selected NuGet assets (including FSharp.Core and test libraries) and the referenced project's **reference assembly**, not its implementation assembly. This proves the replacement compiler's job is to read the exact selected files and preserve their order/identity; it must not redo NuGet or TFM selection.

### Direct-CLI fallback path (inventoried, outside the core gate)

Without `--noframework`, `fsc` can choose default framework/FSharp.Core references and branch between simple and legacy environment-sensitive resolution. [See default-reference insertion and resolver selection.][default-reference-source] For simple resolution, the source search order is target-framework directories, `--lib` include directories, the implicit include/project directory, and the compiler binaries directory; `#r` additionally searches the referring source's directory. Names without an extension are tried as `.dll`, `.exe`, then `.netmodule`. [See the pinned simple resolver.][simple-resolution-source]

The direct invocation below exited 0 without any explicit `-r:` arguments and emitted an assembly referencing `FSharp.Core`, `mscorlib`, and `netstandard`, demonstrating the implicit-reference path in this oracle:

```powershell
dotnet "C:\Program Files\dotnet\sdk\10.0.110\FSharp\fsc.dll" `
  --nologo --target:library `
  --out:src\fsharp2\obj\Release\net10.0\oracle-direct-default.dll `
  src\fsharp2\Library.fs
```

`--standalone` does **not** mean “invoke the compiler without MSBuild.” The oracle help defines it as statically linking FSharp.Core and every referenced DLL that depends on it into the generated assembly; on the same input it exited 1 with `FS2008: Static linking may not include a mixed managed/unmanaged DLL`. `--noframework` separately means “Do not reference the default CLI assemblies by default,” and `--simpleresolution` selects directory-based rather than MSBuild resolution.

Environment-selected direct-CLI default discovery remains inventoried because it shares the resolver, but [ADR 0004](../adr/0004-target-compiler-and-msbuild-compatibility.md) defines the core destination as the compiler **as invoked by MSBuild**. That implicit-discovery path is therefore not a gate for issue #2 or the final compiler-target Compatibility Gate unless a later decision deliberately broadens the product surface. Public option effects such as `--standalone` **are** still required when a project routes them through `OtherFlags`; only the no-target invocation/discovery promise is excluded. The mandatory reference contract is the standard target path above: consume the exact concrete references and reference assemblies selected by MSBuild, in their supplied order, with no replacement TFM/NuGet discovery.

## Diagnostics and exit contract

### Diagnostic structure

With the task-forced `--fullpaths --flaterrors`, a normal ranged diagnostic has the MSBuild-readable form:

```text
<absolute-path>(<line>,<column>): <warning|error> FS<four-digits>: <message>
```

The compiler owns diagnostic numbers, severity, source range, message text, related diagnostics, language-version gating, warning level, suppression, promotion, style, culture, and coloring. The formatter uses `FS%04d`; default style uses one-based MSBuild-compatible locations, `--vserrors` adds the diagnostic subcategory and end range, and `--flaterrors` normalizes multiline messages. [See formatting and severity adjustment.][diagnostic-format-source]

Console diagnostics are written to stderr; banner/help/version text is written to stdout. `--preferreduilang`, `--LCID`, `--utf8output`, and `--consolecolors` affect the observable stream. Differential tests should pin UI culture and disable colors unless color itself is under test, then compare structured fields plus text for the pinned oracle version. Exact localized wording should not be assumed stable across a servicing update without refreshing the oracle corpus.

### Observed severity/exit matrix

| Probe | Diagnostic/output | Process result |
| --- | --- | --- |
| `--version`, `--help`, `--full-help`, `--langversion:?` | requested text on stdout | 0 |
| Non-exhaustive match | `warning FS0025` on diagnostic stderr | 0 |
| Same with `--nowarn:25` | suppressed | 0 |
| Same with `--warnaserror+` or `--warnaserror:25` | `error FS0025` | 1 |
| Missing source | `error FS0225` | 1 |
| Syntax/indentation error with task formatting flags | ranged `FS0058`/`FS0010`; multiline guidance flattened | 1 |
| Unknown option with no source | `FS0243` plus `FS0207` | 1 |

The compiler logger increments its error count only after warning policy adjusts severity and `AbortOnError` exits 1 when that count is nonzero. The process entry point returns 0 on normal completion and 1 on last-chance failure. [See the console logger/abort logic][fsc-driver-errors] and [entry point][fsc-main-source].

The `Fsc` task forwards the compiler process result through MSBuild `ToolTask`: nonzero invokes task-error handling and returns false; zero returns true. A failed task therefore fails `CoreCompile` and the build. `SkipCompilerExecution=true` is the deliberate exception: the F# task returns 0 without launching the compiler, principally so tooling can request/capture a command line. [See `Fsc.ExecuteTool`][fsc-task-execute] and [MSBuild `ToolTask.Execute`][tooltask-execute]. A hosted IDE cancellation may return `-1`; that host-object path is an implementation detail but should not turn cancellation into success.

### Observed failure leftovers

The oracle does **not** provide an atomic all-or-nothing output guarantee. The following representative probes used four isolated directories, a `Probe.fs` containing respectively an incomplete binding, an `int`/`string` type mismatch, an unresolved `Missing.Namespace.value`, and the valid binding `let value = 42`. The first three used this command; the last changed only `--out:Probe.dll` to `--out:.` so primary-PE creation would fail at emission:

```powershell
dotnet "C:\Program Files\dotnet\sdk\10.0.110\FSharp\fsc.dll" `
  --nologo --target:library --debug:portable --pdb:Probe.pdb `
  --doc:Probe.xml --refout:Probe.ref.dll --out:Probe.dll Probe.fs
```

| Failure phase | Oracle result | Requested artifacts left after exit |
| --- | --- | --- |
| Parse/indentation | exit 1, `FS0058` + `FS0010` | none |
| Type checking | exit 1, `FS0001` | none |
| Name/reference checking | exit 1, `FS0039` | none |
| Primary PE write (`--out:.`) after valid analysis | exit 1, `FS2014` | `Probe.ref.dll` (4,608 bytes) and `Probe.xml` (114 bytes); no primary DLL or PDB |

This matrix is deliberately representative, not exhaustive across every emitter and signing failure. It establishes the compatibility rule that cleanup is phase- and artifact-dependent: FSharp2 must compare leftovers for each failure tracer it claims, and must not impose a blanket cleanup policy that erases oracle-produced reference/doc artifacts or retain files where the oracle leaves none. Exact behavior beyond these probes remains an oracle-corpus item.

## Artifact contract

### Compiler-owned outputs

Depending on switches, `fsc` owns:

| Output/behavior | Trigger and compatibility obligation |
| --- | --- |
| Primary PE output | `-o` plus `--target:{library|exe|winexe|module}`; preserve public API/assembly shape where applicable, metadata, runtime semantics, target/platform characteristics, F# signature/optimization resources, and supported optimization-mode behavior |
| Debug information | `--debug:*`, `-g`, `--pdb`, checksum algorithm, embedded source, SourceLink, path map; preserve debugger-visible sequence points/documents/checksums/embedded data. `embedded` places PDB data in the PE; other supported modes may create a separate PDB |
| Reference assembly | `--refout` in normal builds or `--refonly`; preserve the compile-time API surface expected by downstream project references |
| XML documentation | `--doc:<file>` |
| Inferred signatures | `--sig:<file>` or `--allsigs` |
| Managed/linked/Win32 resources and manifest/icon | resource and Win32 switches; preserve logical names, visibility, and embedding/link behavior |
| Signing | key file, delay sign, public sign; preserve assembly identity/signature semantics |
| Deterministic identity | `--deterministic+` plus path map/source inputs; repeated equivalent invocations must be repeatable. Local repeated deterministic DLL/PDB probes produced identical hashes |
| Optional FSharp.Core copy | direct compiler default can copy FSharp.Core; the standard task always passes `--nocopyfsharpcore`, leaving copy-local behavior to SDK targets |

Byte identity with the oracle is not required. [ADR 0006](../adr/0006-require-behavioral-not-byte-identical-output.md) requires behavioral equivalence across public API, metadata/runtime semantics, diagnostics, debugging, signing, resources, and deterministic repeatability while allowing different valid IL and optimization choices.

### SDK-owned and intermediate outputs

In the observed Release build, `fsc` directly targeted:

```text
obj/Release/net10.0/fsharp2.dll
obj/Release/net10.0/fsharp2.pdb
obj/Release/net10.0/fsharp2.xml
obj/Release/net10.0/refint/fsharp2.dll
```

It consumed generated TFM attributes, SourceLink JSON, and `ILLink.Substitutions.xml`. Common SDK targets then copied the implementation DLL/PDB/XML to `bin/Release/net10.0`, copied the reference assembly to `obj/Release/net10.0/ref`, and generated `bin/Release/net10.0/fsharp2.deps.json`. `CoreCompile`'s declared outputs are the intermediate assembly, intermediate reference assembly, debug symbols, documentation, non-existent sentinel, and custom outputs—not `.deps.json`. This ownership split follows the shipped [`CoreCompile` target][fsharp-targets-corecompile] and observed build tree.

FSharp2's differential gate should compare compiler-owned artifacts directly, then separately build through the SDK and compare final build usability/shape. It should not require the compiler process itself to generate SDK-owned files.

### IcedTasks target frameworks and `--refout`

[ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md) fixes the first IcedTasks milestone to `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0`. It is therefore incorrect to describe its TFM set as TBD or to assume that every one of those inner builds requests a reference assembly. The .NET 10 SDK defaults `ProduceReferenceAssembly=true` for an F# project only for `.NETCoreApp` 7.0 or newer; other project types/versions require an explicit override. [See the pinned inference rule.][produce-reference-assembly-source]

This evaluation used the repo project as a neutral F# project and changed only the TFM:

```powershell
$tfms = 'netstandard2.0','netstandard2.1','net6.0','net9.0'
foreach ($tfm in $tfms) {
  $json = dotnet msbuild src/fsharp2/fsharp2.fsproj `
    -p:TargetFramework=$tfm -p:Configuration=Release `
    -getProperty:ProduceReferenceAssembly,TargetFrameworkIdentifier,TargetFrameworkVersion `
    -getItem:IntermediateRefAssembly -verbosity:quiet
  $evaluation = ConvertFrom-Json ($json -join [Environment]::NewLine)
  $intermediateRefAssemblies = @($evaluation.Items.IntermediateRefAssembly)
  '{0}: ProduceReferenceAssembly={1}; IntermediateRefAssemblyCount={2}' -f `
    $tfm,$evaluation.Properties.ProduceReferenceAssembly,$intermediateRefAssemblies.Count
}
```

Observed: `false/0` for `netstandard2.0`, `netstandard2.1`, and `net6.0`; `true/1` for `net9.0`. Under the unmodified SDK defaults, only the `net9.0` IcedTasks inner build therefore supplies `OutputRefAssembly` and physically invokes `--refout`. If IcedTasks explicitly overrides `ProduceReferenceAssembly`, the evaluated project wins and the oracle manifest must record that. All four TFMs still exercise the same requirement from ADR 0009: output targeting follows their MSBuild-selected reference assemblies and options rather than the compiler host TFM.

## Compatibility-envelope reproduction matrix

There are currently no fully named compatibility envelopes. “Experimental” below means the explicit subset declared by a particular Experimental Vertical Milestone. The first IcedTasks TFMs are fixed by ADR 0009; issue #6 still has to select the remaining language/input/diagnostic/artifact subset.

| Contract dimension | Every Experimental Vertical Milestone | First IcedTasks envelope | Final Compatibility Gate |
| --- | --- | --- | --- |
| SDK opt-in/selection seam | **Required.** Normal `.fsproj` evaluation selects FSharp2 with the promised one-property/package seam and leaves unrelated SDK behavior intact | Required; exact package/property still follows ADR 0019 implementation | Full transparent/drop-in target integration |
| No official-compiler production fallback | **Required.** Oracle may run only in tests/infrastructure | Required | Required |
| `CoreCompile` task success/failure and target graph | **Required** for all declared invocations, including incremental invalidation and declared outputs | Required for the IcedTasks graph | Full shipped target contract |
| Source ordering/generated source | **Required** for all accepted item classes; never a semantic subset | Required for every IcedTasks/generated source | Full CompileFirst/Before/ordinary/After/Last behavior |
| Reference consumption | **Required.** Consume MSBuild-selected concrete paths and reference assemblies; no replacement TFM/NuGet selection | Required for IcedTasks' packages/projects/framework refs across `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0` | Full reference/resource/compiler-tool item behavior; environment-selected direct-CLI discovery remains outside this target gate |
| Task property/CLI mapping | Reproduce every standard task property and switch exercised by the declared envelope; reject an out-of-envelope value clearly rather than reinterpret it | Exact set beyond the four fixed TFMs is TBD until the IcedTasks envelope is measured | All standard task mappings and every option accepted through `OtherFlags`, public or hidden, including oracle warning/no-op behavior |
| Response file and positional inputs | **Required** for standard task invocation and declared paths/quoting | Required | Full public parser/response behavior |
| Language versions/features | Explicitly enumerate supported `--langversion` values/features; oracle-equivalent within that subset | TBD; derive from IcedTasks plus a deliberate feature tracer | All `preview`, aliases, and versions 4.6 through 10.0 accepted by the then-current .NET 10 oracle |
| Parsing/typechecking/optimization/emission | Complete path for declared constructs—an envelope may be narrow, but not stop before emission | Complete path for IcedTasks | Full .NET 10 F# corpus and supported optimization modes |
| Diagnostics | Correct code, severity, range, suppression/promotion, stream, and exit for every diagnostic reachable in the supported subset | IcedTasks diagnostics plus negative tracers | Full oracle diagnostic behavior across languages/options/cultures in the compatibility corpus |
| Primary assembly behavior | DLL/EXE kind, metadata, API, and runtime semantics for declared targets/platforms | At least the exact artifacts IcedTasks requires; selection TBD | All supported target/platform/profile/signing/resource modes |
| F# metadata/cross-module behavior | Compatible signature and essential optimization data for declared cross-project uses | Required if IcedTasks crosses F# assembly boundaries | Full signature/optimization metadata semantics, including no-data/compression modes |
| PDB/debug/SourceLink | Explicit artifact declaration; reproduce every enabled mode in that declaration | Exact IcedTasks debug profile plus negative tracer; TBD | Full portable/embedded/other supported debug modes, path map, checksum, embedded source, SourceLink |
| Reference assembly | Required whenever the evaluated target supplies `OutputRefAssembly` or `RefOnly`; do not infer it merely from the existence of a project reference | Under unmodified SDK defaults, required for IcedTasks `net9.0`; not requested for its `netstandard2.0`, `netstandard2.1`, or `net6.0` inner builds | Full `refout`/`refonly`, exact task capture quirk, and downstream compile compatibility |
| XML docs/signatures/resources/signing | Declare each supported artifact class and reproduce it completely when enabled | IcedTasks set plus selected tracers; TBD | Full public/task surface |
| Determinism | Required whenever `Deterministic=true`, including declared DLL/PDB/ref outputs | Required under normal SDK deterministic defaults | Full repeatability matrix with path mapping and source embedding |
| SDK final artifact usability | Normal SDK copy/package/run/test consumers must work for declared target types | IcedTasks must build and execute its real workflow | Full final build/pack/project-reference compatibility |
| NativeAOT host/output gates | **Required release gates under ADR 0002.** Publish and execute the compiler host as NativeAOT, and prove a program consuming emitted managed artifacts can publish and execute through the .NET NativeAOT toolchain for the declared supported subset | Both gates are required for the first IcedTasks baseline; its four TFMs do not waive the host gate, and the output gate uses a declared NativeAOT-consumable tracer | Both gates remain required across the final supported host/output matrix |
| Cold/Warm performance | Measure the real Compiler Target Invocation; performance does not excuse incompatibility | Measure the real IcedTasks path | Cold at/below the three-second tripwire for normal projects; Warm sub-second, per issue #1 |

The first-baseline NativeAOT evidence is two separate end-to-end proofs, not a compiler switch: (1) `dotnet publish -p:PublishAot=true` produces a native FSharp2 host that executes a real declared compilation, and (2) a downstream application consuming FSharp2's emitted **managed** PE/PDB publishes with `PublishAot=true` and its behavioral smoke runs. Official .NET documentation defines NativeAOT as whole-program publishing to a self-contained native executable and records its static-analysis/runtime restrictions; it does not redefine the compiler's managed output format. [See the NativeAOT deployment contract.][nativeaot-deployment-source] The exact supported RIDs, warning policy, and constraint corpus belong to issue #4; issue #2 only records that [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md) makes both proofs release gates from the first baseline onward.

The important rule is: a narrower envelope narrows **values and cases**, not the completeness of the pipeline. For example, “F# 10 library + portable PDB + unsigned + net10.0” may be a valid declaration; returning a DLL without the requested reference assembly, diagnostic behavior, or source-order semantics is not.

## Required oracle manifest and probes

Each differential run should record at least:

```text
SDK version + commit
MSBuild version
host/runtime version and RID/OS
fsc banner/product version
hashes of fsc.dll, FSharp.Build.dll, Microsoft.FSharp.Targets,
  Microsoft.FSharp.NetSdk.props, and Microsoft.FSharp.NetSdk.targets
evaluated task properties/items
actual response-file argument vector and ordered source vector
UI culture, encoding, color setting, working directory, and relevant environment
input/reference/resource hashes
exit code and stdout/stderr bytes
all produced artifact paths and hashes
```

Minimum contract tracers should include:

- command success/help/version/unknown/missing-parameter/response-file failures;
- warning, suppressed warning, warning-as-error, syntax/type/reference/emission failures;
- Debug and Release task mappings;
- all accepted language-version values;
- ordered `.fsi`/`.fs`, generated source, project/package/framework references, and compiler tools/type-provider inputs;
- library/exe, implementation/reference assemblies, portable and embedded PDB, docs, resources, signing, SourceLink/path map, deterministic repeats, and a failure-cleanup probe; and
- both direct compiler-owned artifact comparison and downstream SDK project-reference/run/test/pack use.

## Residual decisions and uncertainties

1. **Moving oracle:** 10.0.110 is the latest installed oracle on 2026-07-17, not a permanent target. The final gate targets the then-latest .NET 10 servicing SDK and must refresh the manifest/corpus deliberately.
2. **Hidden-option corpus:** scope is resolved—every option accepted through the target's `OtherFlags` seam is in the final gate—but observable behavior for the internal/deprecated/test options still needs a generated differential corpus. The source establishes warning/no-op categories; it does not by itself enumerate every output side effect.
3. **Failure leftovers:** the representative matrix proves cleanup is not atomic, but it is not an authoritative matrix for every signing/resource/debug/emitter failure phase. Extend it only as each failure class enters a declared envelope.
4. **Localization:** diagnostic identity/severity/range are stable compatibility dimensions for a pinned oracle; localized message bytes may change with culture or servicing and need a pinned-culture corpus.
5. **IcedTasks envelope:** ADR 0009 fixes its four TFMs, and the SDK rule fixes the default `--refout` implication. Issue #6 still must derive the exact language/options/artifact/reference cases from an evaluated IcedTasks build rather than guesses.
6. **NativeAOT and performance:** ADR 0002 fixes both NativeAOT release gates, but their detailed RID/warning/constraint corpus belongs to issue #4; performance needs separate evidence as well. Neither can substitute for compatibility-oracle evidence.

## Primary sources

Installed sources are under `C:\Program Files\dotnet\sdk\10.0.110\FSharp` and `C:\Program Files\dotnet\sdk\10.0.110\Sdks\Microsoft.NET.Sdk\targets`. Stable first-party source links below are pinned to the repository commit recorded in the installed compiler product versions.

- [`Fsc` task argument generation, public properties, capture, skip, and execution][fsc-task-source]
- [Public/internal compiler option definitions and help blocks][compiler-options-source]
- [Visual Studio F# design-time target and returned-command parser][fsharp-design-time-target-source]
- [Console diagnostics, adjusted severities, and abort-on-error][fsc-driver-errors]
- [Process entry point and 0/1 return behavior][fsc-main-source]
- [Diagnostic formatting and codes][diagnostic-format-source]
- [F# target source ordering and `CoreCompile`][fsharp-targets-source]
- [NetSdk defaults, tool paths, and implicit FSharp.Core reference][netsdk-props-source]
- [NetSdk target profile, SimpleResolution, SourceRoot/path map, and ILLink input][netsdk-targets-source]
- [Simple reference-resolution search behavior][simple-resolution-source]
- [MSBuild reference-assembly selection][msbuild-reference-selection]
- [SDK default reference-assembly inference for F#][produce-reference-assembly-source]
- [MSBuild `ToolTask` failure propagation][tooltask-execute]
- [Official NativeAOT deployment contract][nativeaot-deployment-source]

[fsc-task-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L117-L804
[fsc-other-flags-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L14-L17
[fsc-task-args]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L117-L365
[fsc-task-properties]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L367-L688
[fsc-task-execute]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Fsc.fs#L691-L804
[compiler-options-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs
[option-block-model-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L47-L88
[option-help-filter-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L167-L194
[option-parser-flatten-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L260-L264
[private-option-composition-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L2325-L2348
[private-option-behavior-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L1463-L1477
[private-option-warning-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerDiagnostics.fs#L280-L303
[response-parser-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L229-L365
[public-option-blocks]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L763-L1398
[fsc-driver-errors]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/fsc.fs#L61-L139
[fsc-main-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/fsc/fscmain.fs#L22-L97
[diagnostic-format-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerDiagnostics.fs#L1981-L2330
[fsharp-targets-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.Targets
[fsharp-targets-order]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.Targets#L258-L277
[fsharp-targets-corecompile]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.Targets#L279-L426
[fsharp-design-time-target-source]: https://github.com/dotnet/project-system/blob/1379b2dce76234664ad1b970e0bd72dc95c75e68/src/Microsoft.VisualStudio.ProjectSystem.Managed/ProjectSystem/DesignTimeTargets/Microsoft.FSharp.DesignTime.targets#L62-L75
[fsharp-command-line-parser-source]: https://github.com/dotnet/project-system/blob/1379b2dce76234664ad1b970e0bd72dc95c75e68/src/Microsoft.VisualStudio.ProjectSystem.Managed/ProjectSystem/LanguageServices/FSharp/FSharpCommandLineParserService.cs#L24-L95
[netsdk-props-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.NetSdk.props#L19-L150
[fsharp-core-props-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.Core.NetSdk.props#L23-L26
[netsdk-targets-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/Microsoft.FSharp.NetSdk.targets#L14-L208
[default-reference-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L806-L887
[simple-resolution-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L523-L750
[msbuild-reference-selection]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/msbuild/src/Tasks/Microsoft.Common.CurrentVersion.targets#L2511-L2535
[produce-reference-assembly-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/sdk/src/Tasks/Microsoft.NET.Build.Tasks/targets/Microsoft.NET.TargetFrameworkInference.targets#L280-L282
[tooltask-execute]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/msbuild/src/Utilities/ToolTask.cs#L1341-L1603
[nativeaot-deployment-source]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/deploying/native-aot/index.md
