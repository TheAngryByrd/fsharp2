# Linux runtime and deployment compatibility floors

Status: decision research, 2026-07-19

Governing decisions: [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0008](../adr/0008-deliver-through-experimental-vertical-milestones.md), [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md), [ADR 0019](../adr/0019-distribute-opt-in-msbuild-integration-by-nuget.md), and [ADR 0024](../adr/0024-use-calibrated-dedicated-runners-for-performance-gates.md)

## Answer

FSharp2 supports two Linux libc families on both required architectures. The release matrix therefore contains four Linux compiler-host payloads:

- `linux-x64` and `linux-arm64` for glibc;
- `linux-musl-x64` and `linux-musl-arm64` for musl.

The minimum certified execution environments are **RHEL 8** for the glibc pair and **Alpine 3.21** for the musl pair. Both versions and both architectures are in Microsoft's current .NET 10 supported-OS table.[^supported-os] The corresponding Microsoft-published libc prerequisites are glibc 2.27 and musl 1.2.3.[^supported-libc] FSharp2 adopts those numbers as build-time ABI ceilings: glibc artifacts may require no symbol newer than `GLIBC_2.27`, and musl artifacts use a musl 1.2.3 sysroot plus native execution proof because musl has no equivalent GLIBC symbol-version ceiling. These are different claims:

- the distro/version is FSharp2's minimum clean-image execution gate;
- the libc number is a necessary .NET prerequisite and FSharp2 build/inspection bound;
- neither one turns every arbitrary distribution with a sufficiently new libc into a supported FSharp2 environment.

There is no Microsoft-published universal numeric Linux-kernel floor for .NET 10, so FSharp2 does not invent one. Support is the conjunction of a named, publisher-supported distro line, architecture, libc family, the artifact's recorded ELF/native dependency contract, and successful native execution. Distro kernels routinely backport features, making a bare kernel number weaker than the supported-OS contract.

The four payloads are built in controlled, digest-pinned Linux builders with the target libc/sysroot and then tested on real x64 or Arm64 hardware in the minimum clean images. QEMU or cross-architecture container emulation may be useful for developer diagnostics but never counts as release evidence; Microsoft's .NET 10 matrix explicitly says QEMU execution is unsupported. Same-OS x64/Arm64 cross-publication is permitted only with the target tools/sysroot, followed by native target-architecture execution. Windows-to-Linux publication is not supported.

This decision expands [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md)'s compiler-host package set from six to eight payloads. It does not expand the architecture promise to Arm32, ppc64le, or s390x, and it does not make every RID with an available runtime pack a product promise.

## Why these floors

Microsoft's versioned .NET 10 matrix currently supports RHEL 8, 9, and 10 and Alpine 3.21, 3.22, and 3.23 on both x64 and Arm64.[^supported-os] RHEL 8 is the oldest listed long-lived glibc distro line that covers both required architectures; Alpine 3.21 is the oldest listed musl line. Both therefore give the release gate a concrete, reproducible bottom edge without claiming that an out-of-support Ubuntu 18.04 or Alpine 3.17 installation is supported merely because it supplied Microsoft's libc-minimum observation.

Microsoft recommends portable, non-versioned, non-distro RIDs and documents the exact four RIDs selected above.[^rid-catalog] Official NativeAOT runtime packs are published for all four through .NET 10.0.10.[^nativeaot-packs] Pack availability proves that the SDK can target the RID. It does not prove FSharp2's actual binary works on a particular distro, loader, native dependency set, or older userspace.

Microsoft also warns that a NativeAOT Linux binary built on one Linux version generally runs only on the same or a newer Linux version.[^nativeaot-overview] Consequently, publishing on an arbitrary current hosted runner is invalid floor evidence. A controlled floor builder or explicit sysroot owns the link contract, and the minimum runtime image owns the execution contract. Microsoft's sample cross-build containers illustrate explicit sysroots, but the samples expressly call those images unsupported; FSharp2 must own and qualify its builders rather than inheriting that sample as a support claim.[^nativeaot-containers]

## Versioned release matrix

| Payload | Libc family | Product ABI ceiling | Minimum certified runtime | Required release execution |
| --- | --- | --- | --- | --- |
| `linux-x64` | glibc | maximum required symbol `GLIBC_2.27` | RHEL 8 x64 | native x64 clean-image host smoke and real Compiler Target Invocation |
| `linux-arm64` | glibc | maximum required symbol `GLIBC_2.27` | RHEL 8 Arm64 | native Arm64 clean-image host smoke and real Compiler Target Invocation |
| `linux-musl-x64` | musl | musl 1.2.3 sysroot plus execution proof | Alpine 3.21 x64 | native x64 clean-image host smoke and real Compiler Target Invocation |
| `linux-musl-arm64` | musl | musl 1.2.3 sysroot plus execution proof | Alpine 3.21 Arm64 | native Arm64 clean-image host smoke and real Compiler Target Invocation |

The release manifest records the exact base-image digest, SDK, runtime, ILCompiler, compiler/linker/binutils versions, sysroot identity, RID, architecture, source commit, publish properties, hashes, ELF observations, installed native packages, and test result. A moving tag such as `ubi8`, `alpine:3.21`, or `latest` is not sufficient evidence by itself.

The matrix is a floor, not the complete compatibility sample. Each release also runs a current supported glibc image and a current supported Alpine image per architecture so a binary constrained to the floor is not accidentally coupled only to the oldest environment.

### Loader and native-dependency floor

The required program interpreters are exact:

| Payload | Required `PT_INTERP` |
| --- | --- |
| `linux-x64` | `/lib64/ld-linux-x86-64.so.2` |
| `linux-arm64` | `/lib/ld-linux-aarch64.so.1` |
| `linux-musl-x64` | `/lib/ld-musl-x86_64.so.1` |
| `linux-musl-arm64` | `/lib/ld-musl-aarch64.so.1` |

The initial closed runtime dependency envelope is:

- the selected libc and its loader-provided math, dynamic-loading, threading, and realtime facilities;
- GCC runtime and C++ runtime support (`libgcc` and `libstdc++`);
- zlib;
- full ICU common, internationalization, and data libraries plus timezone data; and
- OpenSSL `libssl`/`libcrypto`.

An artifact may omit an unused member after NativeAOT trimming, but it may not add a native dependency family outside this list. Such an addition fails the release until a reviewed compatibility/security decision expands the envelope. The distro package names, exact resolved SONAMEs, and versions are recorded from the digest-pinned RHEL 8 or Alpine 3.21 image because packaging names and servicing revisions differ; that manifest is evidence for one release, while the closed family list above is the product contract. Network clients, UI stacks, shell utilities, Git, and the official compiler/FCS are not runtime dependencies of the compiler host.

## ELF and native dependency contract

Each shipped file is inspected before execution with equivalent evidence to:

```text
file <host>
readelf -h <host>
readelf -l <host>
readelf -d <host>
readelf --version-info <host>
```

The gate records and verifies:

1. ELF class, endianness, machine, and executable type match the declared RID.
2. `PT_INTERP` names the intended libc-family loader, not the builder's accidental loader.
3. every `DT_NEEDED` entry belongs to the closed native-dependency envelope, appears with its resolved SONAME/package in the release manifest, and exists in the clean runtime image;
4. glibc symbol-version requirements do not exceed `GLIBC_2.27` and musl payloads were linked against the recorded 1.2.3 sysroot;
5. no absolute builder/sysroot path, undeclared runtime library, or host-architecture binary leaks into the package; and
6. packaged executable bits, hash/provenance data, and optional native symbols survive the real NuGet install path.

The musl ABI specifies an absolute dynamic-interpreter path of the form `$(syslibdir)/ld-musl-$(ARCH).so.1`; ELF specifies that `DT_NEEDED` entries drive runtime dependency loading.[^musl-loader] [^elf-dynamic] These specifications explain what to inspect. They do not establish a .NET support floor. In particular, musl's Linux 2.6.39 statement is about musl POSIX conformance and is not a .NET 10 or FSharp2 kernel promise.

The resolved allowlist is the artifact's intersection with the closed dependency envelope, rather than a list copied wholesale from a framework-dependent container. Microsoft's current Ubuntu Noble runtime-deps image includes libc, libgcc, ICU, OpenSSL, libstdc++, and timezone data, while its Alpine image includes libgcc, OpenSSL, libstdc++, and zlib and opts into invariant globalization.[^runtime-deps-images] Those manifests support the selected dependency families and provide useful clean-image inputs, but do not prove that every FSharp2 NativeAOT artifact needs every member forever.

## Globalization and other native dependencies

The existing compiler-host decision keeps full globalization because compiler UI culture is observable and invariant mode changes culture creation, casing, sorting, normalization, IDN handling, and timezone names. On Linux a non-invariant .NET application fails at startup when it cannot load ICU.[^globalization]

Therefore every minimum image gate does one of the following explicitly:

- installs and records the distro ICU package used by the host; or
- packages a separately reviewed app-local/static ICU payload and proves its behavior and servicing process.

The default gate uses the distro's system ICU. Static ICU is not an escape hatch for cross-publication: the NativeAOT documentation limits that feature to Linux and says it is not supported while cross-compiling.[^nativeaot-static-libs] Alpine's official runtime-deps image chooses invariant mode because it omits ICU, so it cannot serve unchanged as FSharp2's full-globalization proof image.

OpenSSL, zlib, libgcc, libstdc++, timezone data, and certificate bundles are included only when the artifact or exercised compiler behavior requires them. A package being installed in a generic .NET image is not enough to put it in FSharp2's permanent contract. Conversely, a library found in `DT_NEEDED`, opened dynamically, or required by a full compiler invocation must be present in the clean-image manifest and covered by servicing policy. Static OpenSSL is an advanced Linux-only option with an explicit security warning and is not supported during cross-compilation, so it is not the default.[^nativeaot-static-libs]

## Required runtime and deployment gates

For each of the four Linux RIDs, a release candidate must pass all of the following on real target-architecture hardware:

1. **Strict publish:** publish the NativeAOT host with no suppressed trim/AOT warnings in the qualified floor builder or sysroot.
2. **Artifact inspection:** validate ELF architecture, interpreter, dependencies, symbol versions, hashes, and provenance as described above.
3. **Clean install:** restore the real FSharp2 integration package into a clean, digest-pinned minimum image and select the build-host RID, never the target project's output RID.
4. **Native host smoke:** launch the installed native executable directly from an unrelated working directory and prove it is not `dotnet <managed-dll>` or an official-compiler fallback.
5. **Real compilation:** run a declared Compiler Target Invocation through the FSharp2 MSBuild target, including response files, paths with spaces and non-ASCII characters, diagnostics, PDB/DLL creation, cancellation, and failure cleanup.
6. **Globalization:** run the baseline UI-culture and invariant-semantic probes with the declared ICU strategy.
7. **Persistent service:** run start/contact/compile/restart/stale-daemon recovery and clean shutdown under both libc families. Every supported release must pass this gate; an earlier Experimental Vertical Milestone without the service is an explicitly narrower, non-release envelope rather than a passing release cell.
8. **Cache portability:** produce and consume portable persisted compiler state across qualified Linux hosts when build inputs are portable; reject, re-key, or recompute any host-native state.
9. **Emitted-output AOT:** separately publish an Oracle and FSharp2 consumer for each of the four downstream Linux RIDs, inspect each native consumer against the same architecture, exact `PT_INTERP`, `GLIBC_2.27` or musl 1.2.3, closed dependency, globalization, and provenance rules, then execute and compare it in the corresponding RHEL 8 or Alpine 3.21 minimum image. This remains distinct from proving the compiler host and applies even when the compiler itself ran on another build-host RID.
10. **Current-image control:** repeat the host and compilation smoke in a current supported image of the same libc family.

The minimum-image compatibility gate is not a performance lane. [ADR 0024](../adr/0024-use-calibrated-dedicated-runners-for-performance-gates.md)'s Cold Compilation and Warm Compilation timing distributions run only on qualified dedicated machine classes. Compatibility must pass first; QEMU timings, container startup, whole-build time, and an MSBuild no-op cannot satisfy either gate.

## Applying the fast-compiler research

The [fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md) selects a persistent NativeAOT service with a content-addressed, declaration-aware query graph. Three deployment consequences apply here:

1. **The service is part of every supported host.** A one-shot smoke is insufficient for a supported release. The minimum glibc and musl images must exercise the persistent service core, protocol, cancellation, recovery, and long-lived resource behavior used by normal MSBuild sessions.
2. **Persisted state remains portable where inputs are portable.** Semantic/query cache records cannot embed native pointers, loader paths, libc-dependent bytes, or host endianness/layout assumptions. Architecture/libc identity is included in a key only when the cached result genuinely depends on it; otherwise the identical versioned content-addressed schema is reusable across qualified hosts.
3. **Cold Compilation and Warm Compilation evidence remains semantic.** NativeAOT startup, daemon contact, a disk-cache hit, or Linux page-cache warmth does not itself prove Warm Compilation. The ADR 0022 work counters and fingerprints must demonstrate valid declaration-level reuse. Conversely, compatibility on one Linux floor does not prove the three-second Performance Tripwire for Cold Compilation or the sub-second Warm Compilation target.

These findings reinforce the deployment matrix, but they never substitute for native artifact inspection and real clean-image execution.

## Servicing and lifecycle policy

NativeAOT is self-contained deployment. Microsoft states that self-contained applications do not roll forward to .NET security patches and that updating the included runtime requires publishing a new application.[^self-contained-servicing] FSharp2 therefore:

- rebuilds, re-inspects, retests, and redeploys all four Linux payloads for every adopted .NET servicing/security update;
- rebuilds and reruns the affected clean-image gates when the builder, sysroot, base image, ICU, OpenSSL, zlib, compiler, linker, or loader receives a relevant security update;
- stops claiming a distro version after its publisher support ends, even if an old image still boots;
- refreshes the floor decision when Microsoft's .NET supported-OS/libc table changes; and
- retains the previous and replacement manifests so a floor move is an auditable compatibility change.

Official .NET container images cover only a subset of the supported OS matrix and stop receiving updates at the earlier of .NET EOL or base-image EOL.[^container-policy] Container availability therefore cannot silently redefine the FSharp2 support matrix.

## Rejected alternatives

- **Only glibc Linux:** fails the explicit musl deployment decision and excludes a current .NET 10 family for which official NativeAOT packs exist.
- **One `linux-*` binary for glibc and musl:** the families use different loaders and native dependency contracts; they require distinct RIDs and artifacts.
- **Support any distro above a libc number:** ignores publisher lifecycle, loader/dependency differences, and Microsoft's distro-based support policy.
- **Publish on the newest hosted runner:** can introduce newer symbols and violates the same-or-newer NativeAOT warning.
- **Use an old unsupported distro as the builder and support floor:** confuses a toolchain technique with a supported deployment promise.
- **Declare a kernel version from an ELF header or musl manual:** neither is a Microsoft .NET 10 kernel contract.
- **Use QEMU as Arm64 release evidence:** Microsoft explicitly excludes QEMU execution from support.
- **Copy a runtime-deps Dockerfile as the permanent dependency list:** image manifests drift and do not describe the exact NativeAOT artifact.
- **Use invariant globalization to make Alpine easy:** changes observable culture behavior and conflicts with the existing full-globalization baseline.

## Residual work and refresh triggers

This ticket fixes the support policy and proof shape; implementation must still capture the first real artifacts. Before the first public Linux package ships, release automation must confirm the observed interpreters match the four fixed paths, record the dependency envelope's exact resolved SONAME/package versions, prove the `GLIBC_2.27` or musl 1.2.3 ceiling, retain image digests, and capture host plus emitted-output clean-image results for all four RIDs. A failure to meet the selected RHEL 8 or Alpine 3.21 floor is a release failure or a new reviewed floor decision, not a reason to weaken the evidence silently.

Refresh this research when:

- the pinned .NET 10 SDK/runtime/ILCompiler changes;
- Microsoft changes the .NET 10 supported distro or libc matrix;
- RHEL 8 or Alpine 3.21 approaches publisher EOL;
- the host adds a native library, static-link option, or globalization strategy;
- the service or cache schema begins retaining host-dependent data; or
- the build moves between native and cross-architecture publication.

## Primary sources

- [.NET 10 supported operating systems and libc prerequisites][supported-os] [supported-libc]
- [.NET OS lifecycle policy][os-lifecycle]
- [.NET Runtime Identifier catalog][rid-catalog]
- [NativeAOT deployment, prerequisites, target RIDs, and Linux same-or-newer rule][nativeaot-overview]
- [NativeAOT same-OS cross-compilation requirements][nativeaot-cross]
- [NativeAOT controlled sysroot/container examples and limitations][nativeaot-containers]
- [NativeAOT globalization and static native-library options][globalization] [nativeaot-static-libs]
- [.NET self-contained and NativeAOT servicing behavior][self-contained-servicing]
- [Official .NET container support policy and current runtime-deps examples][container-policy] [runtime-deps-images]
- [musl dynamic loader contract][musl-loader]
- [ELF dynamic-section contract][elf-dynamic]

[^supported-os]: [.NET 10 supported distro/architecture table][supported-os].
[^supported-libc]: [.NET 10 libc prerequisites and QEMU restriction][supported-libc].
[^rid-catalog]: [Portable Linux RID guidance and catalog][rid-catalog].
[^nativeaot-packs]: Official NuGet indexes for [`linux-x64`][pack-linux-x64], [`linux-arm64`][pack-linux-arm64], [`linux-musl-x64`][pack-linux-musl-x64], and [`linux-musl-arm64`][pack-linux-musl-arm64].
[^nativeaot-overview]: [NativeAOT deployment prerequisites, RID targeting, and Linux same-or-newer warning][nativeaot-overview].
[^nativeaot-containers]: [Official NativeAOT sysroot/cross-container examples and their unsupported status][nativeaot-containers].
[^musl-loader]: [musl dynamic-linking and filesystem-layout contract][musl-loader].
[^elf-dynamic]: [ELF dynamic-section and `DT_NEEDED` contract][elf-dynamic].
[^runtime-deps-images]: [Ubuntu Noble runtime-deps manifest][runtime-deps-ubuntu] and [Alpine 3.23 runtime-deps manifest][runtime-deps-alpine].
[^globalization]: [NativeAOT globalization behavior][globalization] and [missing-ICU startup behavior][globalization-icu].
[^nativeaot-static-libs]: [NativeAOT static ICU and OpenSSL constraints][nativeaot-static-libs].
[^self-contained-servicing]: [.NET self-contained servicing][self-contained-servicing] and [NativeAOT deployment mode][nativeaot-scd].
[^container-policy]: [Official .NET container platform and lifecycle policy][container-policy].

[supported-os]: https://github.com/dotnet/core/blob/20e72eb1b769d71b4dd208419d66d8a0ef3b1961/release-notes/10.0/supported-os.md#L33-L49
[supported-libc]: https://github.com/dotnet/core/blob/20e72eb1b769d71b4dd208419d66d8a0ef3b1961/release-notes/10.0/supported-os.md#L99-L114
[os-lifecycle]: https://github.com/dotnet/core/blob/712e32ba92b26ed6a0bb7f2777a4ce4f1c897f20/os-lifecycle-policy.md#L31-L43
[rid-catalog]: https://github.com/dotnet/docs/blob/33797fd40ff9e1409face480e4d65164031552a4/docs/core/rid-catalog.md#L83-L122
[pack-linux-x64]: https://api.nuget.org/v3-flatcontainer/microsoft.netcore.app.runtime.nativeaot.linux-x64/index.json
[pack-linux-arm64]: https://api.nuget.org/v3-flatcontainer/microsoft.netcore.app.runtime.nativeaot.linux-arm64/index.json
[pack-linux-musl-x64]: https://api.nuget.org/v3-flatcontainer/microsoft.netcore.app.runtime.nativeaot.linux-musl-x64/index.json
[pack-linux-musl-arm64]: https://api.nuget.org/v3-flatcontainer/microsoft.netcore.app.runtime.nativeaot.linux-musl-arm64/index.json
[nativeaot-overview]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L9-L87
[nativeaot-cross]: https://github.com/dotnet/runtime/blob/release/10.0/src/coreclr/nativeaot/docs/compiling.md#L36-L65
[nativeaot-containers]: https://github.com/dotnet/runtime/blob/83a533e0a284f056e304d6dcef2f0fcb1297e88c/src/coreclr/nativeaot/docs/containers.md#L13-L90
[globalization]: https://github.com/dotnet/runtime/blob/release/10.0/docs/design/features/globalization-invariant-mode.md#L1-L17
[globalization-icu]: https://github.com/dotnet/runtime/blob/release/10.0/docs/design/features/globalization-invariant-mode.md#L127-L134
[nativeaot-static-libs]: https://github.com/dotnet/runtime/blob/release/10.0/src/coreclr/nativeaot/docs/compiling.md#L68-L143
[runtime-deps-ubuntu]: https://github.com/dotnet/dotnet-docker/blob/b3c1717e6ce45ac6f59c940774bf81e4bad2e2c5/src/runtime-deps/10.0/noble/amd64/Dockerfile#L11-L23
[runtime-deps-alpine]: https://github.com/dotnet/dotnet-docker/blob/87bbc768dfb7661d9d4e9b814fdba1d45c12ef0b/src/runtime-deps/10.0/alpine3.23/amd64/Dockerfile#L1-L22
[self-contained-servicing]: https://github.com/dotnet/docs/blob/2369e0c61088c5ed4ce5a7e5c44fe693331fbd6b/docs/core/deploying/index.md#L250-L264
[nativeaot-scd]: https://github.com/dotnet/docs/blob/2369e0c61088c5ed4ce5a7e5c44fe693331fbd6b/docs/core/deploying/index.md#L352-L368
[container-policy]: https://github.com/dotnet/dotnet-docker/blob/820c26d10c441961868d47bc54cea0e67d974ae0/documentation/supported-platforms.md#L1-L30
[musl-loader]: https://musl.libc.org/doc/1.1.24/manual.html
[elf-dynamic]: https://refspecs.linuxfoundation.org/elf/gabi4+/ch5.dynamic.html
