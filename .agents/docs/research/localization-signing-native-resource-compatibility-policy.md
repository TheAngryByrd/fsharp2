# Localization, signing, and native-resource compatibility policy

Status: issue [#15](https://github.com/TheAngryByrd/fsharp2/issues/15) research decision, 2026-07-19

Governing decisions: [ADRs 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [0004](../adr/0004-target-compiler-and-msbuild-compatibility.md), [0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [0006](../adr/0006-require-behavioral-not-byte-identical-output.md), [0009](../adr/0009-use-a-target-framework-agnostic-emitter.md), [0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md), [0014](../adr/0014-support-the-full-dotnet-10-language-version-matrix.md), [0019](../adr/0019-distribute-opt-in-msbuild-integration-by-nuget.md), [0021](../adr/0021-use-public-srm-behind-symbolic-emission-fragments.md), [0023](../adr/0023-keep-the-fsharp-metadata-harness-inspector-envelope-only.md), and [0026](../adr/0026-support-versioned-glibc-and-musl-linux-floors.md)

## Answer

FSharp2 preserves the Compatibility Oracle's complete compiler-target behavior for localization, strong-name signing, and native Win32 resources. These are final-gate compatibility surfaces, not optional packaging details.

- Every shipped FSharp2 compiler host uses full globalization. Compiler semantics, cache identities, metadata, ordering, and generated artifacts use ordinal or explicitly invariant operations; UI culture may change only localizable human text.
- The diagnostic gate covers neutral English and every satellite language shipped by the resolved .NET 10 F# oracle. The user's family-wide requirement remains literal: every FSxxxx error and warning case in the complete generated diagnostic corpus is run in every declared UI culture, with exact code, effective severity, range, message, ordering, stream, suppression/promotion, and exit behavior.
- Unsigned, delay-signed, public-signed, and fully signed key-file outputs are core final-gate modes on every supported host RID. Key-container input follows the .NET 10 CoreCLR oracle and fails with its exact unsupported behavior; FSharp2 does not add a managed fallback or silently reinterpret it.
- The --win32res, --win32manifest, --nowin32manifest, and --win32icon options, including compiler-generated version resources and every collision combination, are core final-gate inputs on every host OS. Output targeting follows the invocation, not the compiler host OS.
- Authenticode signing and macOS code signing/notarization belong to release packaging after compiler determinism and strong-name checks. They are not compiler-output features and never excuse missing strong-name or resource behavior.
- Every release exercises these surfaces in both NativeAOT directions: the NativeAOT compiler host must compile them, and a separately NativeAOT-published consumer must preserve the emitted output's signing identity and resources.

The observed oracle is the SDK selected by the repository on 2026-07-19: .NET SDK 10.0.110, F# compiler 14.0.110.0, VMR commit f7d90799ce4ef09a0bb257852a57248d2a8fb8dd. global.json requests 10.0.100 with feature roll-forward, so every servicing update creates a new reviewed oracle lineage rather than inheriting these bytes indefinitely.

## Localization and globalization

### Exact UI-language corpus

The pinned compiler has neutral English resources and exactly these thirteen satellite directories:

~~~text
cs
de
es
fr
it
ja
ko
pl
pt-BR
ru
tr
zh-Hans
zh-Hant
~~~

The final diagnostic matrix therefore uses these exact PreferredUILang values plus en-US for the neutral-English lane. It also includes:

- de-DE, proving parent fallback to the de satellite;
- ga-IE, proving fallback to neutral English when no satellite exists;
- an invalid preferred-language value, preserving the oracle diagnostic and exit behavior; and
- a missing/corrupt satellite case for every packaged host, proving the oracle-equivalent fallback or failure rather than silently mixing languages.

The source of the language list is the pinned F# [Compiler/xlf tree][fsharp-xlf], corroborated by the installed FSharp.Compiler.Service.resources.dll and FSharp.Build.resources.dll directories. F# formats resource strings through ResourceManager.GetString with CurrentUICulture.[^fsharp-resource-manager] Satellite presence alone is not proof: each release records the culture name, satellite hash, resolved resource assembly, fallback result, and exact UTF-8 output.

All diagnostic cases pass PreferredUILang explicitly, request UTF-8, disable console colors except in color-specific cases, and capture stdout and stderr independently as raw bytes plus decoded text. There is no fuzzy message comparator and no normalization beyond declared logical-root, newline, and ANSI handling.

### Family-wide FS Diagnostic Compatibility

The final gate generates an inventory from the resolved oracle's diagnostic definitions and accepted warning/error surface, then requires at least one reachable positive occurrence for every inventory member. Intentional aliases or unreachable internal identities need a reviewed oracle record; absence from a handwritten corpus is not a pass.

Every reachable case is crossed with all fourteen language lanes above. For each occurrence, FSharp2 must match:

1. occurrence count and FS%04d identity;
2. original and effective severity, including warning level, --nowarn, --warnon, selective/global --warnaserror, and language-version effects;
3. exact one-based start/end range, related diagnostics, and ordering;
4. exact localized message after only the declared normalizations;
5. stdout versus stderr and raw UTF-8 bytes; and
6. compiler exit, Fsc task result, CoreCompile result, and phase-specific leftover artifacts.

The formatter keeps code/range/severity separate from resource lookup.[^fsharp-diagnostic-structure] A culture change may change message text only. It must never change parsing, name resolution, inference, optimization, symbol identity, metadata, signing, resources, artifact hashes, exit behavior for the same effective diagnostics, or invalidation reach.

Experimental Vertical Milestones may enumerate fewer diagnostic cases only when they declare those inputs unsupported before successful publication. They do not shrink the final family-wide gate. The existing FS0010 and FS0001 tracers remain milestone probes, not evidence for the final surface.

### Required runtime matrix

| Host family | Required globalization lanes |
| --- | --- |
| Windows x64 and Arm64 | Default .NET ICU, forced NLS, pinned app-local ICU, missing-system-ICU fallback to NLS, and missing requested app-local ICU failure. |
| macOS x64 and Arm64 | System ICU with recorded runtime/OS identity, all UI-language lanes, and a controlled missing/unloadable ICU startup failure where the platform permits isolation. |
| RHEL 8 x64 and Arm64 | Recorded system ICU and pinned app-local ICU, each in the minimum clean image and a current supported control image; missing ICU must terminate before compilation. |
| Alpine 3.21 x64 and Arm64 | Recorded system ICU and pinned app-local ICU under the musl payload, each on real hardware; the invariant official runtime-deps image is not valid evidence. |

Windows uses NLS when system ICU is unavailable, while an explicitly requested app-local ICU that cannot be loaded is fatal.[^windows-globalization] Unix has no NLS fallback and terminates when full globalization cannot load ICU.[^unix-globalization] Linux probes record the selected library/SO names and ICU version because the runtime searches supported versions and may select the highest installed one.[^icu-selection]

Every backend runs an invariant-semantics sentinel containing at least Turkish-I casing, Unicode normalization, ordinal identifier/name comparisons, numeric/path formatting, stable metadata/resource ordering, and non-ASCII source/path inputs. Oracle and FSharp2 run in the same cell. Across cells, all non-message compiler observations and deterministic artifacts must remain identical.

InvariantGlobalization=true and DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 are negative release lanes. They must not produce a successful FSharp2 Compiler Target Invocation, reusable cache state, or compiler-owned artifacts. The harness distinguishes a runtime startup failure from an FS diagnostic; missing ICU and invariant-runtime failures are process/configuration evidence, not invented FSxxxx messages. This is necessary because invariant mode changes culture creation, casing, sorting, normalization, formatting, and other observable behavior.[^invariant-globalization]

### Globalization and incremental state

The [fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md) applies directly:

- syntax, binding, type inference, lowering, exported fingerprints, and symbolic emission fragments are culture-independent and reusable when their semantic inputs are unchanged;
- requested UI culture, resource-set identity, output encoding, and formatting flags key diagnostic-rendering results;
- a culture-only change invalidates diagnostic rendering and any culture-bearing trace/output, not semantic queries or emitted code;
- ICU/NLS backend and version are recorded evidence and refresh inputs. They enter a cache key only for a result that can actually observe them;
- satellite/resource hashes enter the compiler-host package identity; and
- traces report culture/backend changes and the exact reused/recomputed boundary without allowing localized text to become a semantic fingerprint.

Unknown culture dependence widens invalidation and records the reason. Reuse is never inferred from equal final text alone.

## Strong-name signing and key custody

### Supported modes

FSharp2 implements the oracle's four explicit artifact modes:

| Mode | Input | Assembly public key | Signature reservation | StrongNameSigned | Required action |
| --- | --- | --- | --- | --- | --- |
| Unsigned | No key | Absent | None | Clear | Deterministic serialization only. |
| Delay sign | Public or private key file accepted by the oracle plus --delaysign+ | Present | Modulus-sized, zero-filled | Clear | Validate identity and leave the slot zero. |
| Public sign | Public or private key file accepted by the oracle plus --publicsign+ | Present | Modulus-sized, zero-filled | Set | Validate identity and leave the slot zero. |
| Full sign | Private key file, with neither delay nor public mode selected | Present | Modulus-sized, populated | Set | Apply CLR strong-name signing after deterministic identity is fixed. |

The pinned compiler separates full key-pair signing from public/delay input handling.[^fsharp-signing-modes] Its writer distinguishes the signed flag and zero/populated slots for delay, public, and full signing.[^fsharp-signing-writer] Full CLR signing uses SHA-1 with RSA PKCS#1 v1.5, reverses the stored signature bytes, and excludes the PE checksum, Authenticode/security-directory fields, and strong-name slot from the signed content.[^fsharp-strong-name-algorithm] SHA-1 is confined to this compatibility protocol; content-addressed state and evidence use SHA-256 or stronger.

--keycontainer is accepted through the compiler-target option surface but is unsupported by the pinned CoreCLR compiler path. The final gate preserves its exact FS3393 failure, stream, exit, and leftovers on every FSharp2 host. FSharp2 does not load a provider, start official fsc, invoke a framework-only signing API, or add an out-of-process fallback. If a future .NET 10 servicing oracle changes that behavior, this decision is refreshed before support changes.

The harness generates the full boolean/input matrix: no key, public-only key, private key, missing file, unreadable file, malformed/truncated/unsupported key bytes, mismatched key material, delay/public flags on and off in every combination, key container, and signing failure after earlier artifacts were requested. Exact precedence, diagnostic, and leftover behavior comes from the resolved oracle manifest; no first-wins/last-wins rule is inferred from option order or source inspection.

### Key custody

Strong names provide assembly identity, not a modern security boundary.[^strong-name-purpose] Nevertheless, private keys are secrets and follow this exact custody rule:

- unsigned, delay, and public-sign positive fixtures use checked-in public-only test material;
- a full-sign positive run receives a short-lived CI secret through a run-scoped file with least-privilege ACLs; the file is deleted after evidence capture and is never placed in the repository, package cache, response-file archive, or reproduction bundle;
- the compiler reads private bytes only after command validation and immediately before link/sign validation, derives the public identity, and clears owned private buffers when the platform permits;
- private bytes, private parameters, secret environment values, original secret paths, and private-key hashes never enter logs, traces, binlogs, crash dumps, protocol recordings, live/disk semantic state, or shareable evidence;
- persistent action identity uses signing mode plus the derived public-key blob/token/fingerprint. A full-sign request parses and validates its private input before any artifact-cache reuse so a malformed or unavailable key cannot be hidden by a hit;
- semantic/query caches may be reused across signing changes. Signing mode/public identity invalidates the final-link/artifact action only; and
- traces record mode, public token/fingerprint, parse/sign elapsed time, and a redacted invalidation reason, never secret material.

General evidence retention, access, and deletion remain owned by issue #16. This issue fixes the non-negotiable redaction boundary so issue #16 cannot choose to retain private keys.

### Determinism, interoperability, and release signing

For each key-file mode, two clean, cache-isolated, different-root invocations must produce byte-identical pre-Authenticode implementation DLL, reference DLL, PDB, XML/signature files, and resources. The full signature must verify with the public key; assembly name/public key/token and downstream F#/C# reference binding must match the oracle behavior. A different key must change the declared identity and signing observations without changing unrelated semantics.

Tamper probes flip bytes in signed content, the strong-name slot, and excluded PE fields and verify the same strong-name validation boundary as the oracle. Invalid-key and injected-signing failures compare exact diagnostics and phase-specific leftover artifacts; they may not publish a successful mixed artifact set or poison the last successful service state.

Authenticode, Windows native-host signing, macOS code signing/notarization, and package signing run after compiler-owned deterministic comparisons. Their credentials never enter the compiler service. Release evidence records the unsigned/pre-Authenticode artifact hash and the outer signed package/host hash separately. FSharp2 does not Authenticode-sign a user's emitted assembly merely because the compiler host itself is release-signed. The PE Authenticode hash has a different exclusion contract from the CLR strong name.[^pe-authenticode]

## Native Win32 resources

### Core option and composition surface

The final gate supports the public compiler-target inputs --win32res, --win32manifest, --nowin32manifest, and --win32icon, plus compiler-generated version information, for libraries, executables, and Windows executables wherever the Compatibility Oracle accepts them. All eight compiler-host payloads must parse and emit the same target PE resource tree from the same evaluated inputs; Linux or macOS hosts may not reject an otherwise supported Windows-target resource request merely because they cannot use Win32 runtime APIs locally.

The pinned oracle establishes these baseline composition rules:

- a raw --win32res input conflicts with an explicit --win32manifest;
- raw .res input suppresses the default manifest and a separately supplied icon;
- compiler-generated version resources remain part of the plan; and
- without raw .res, the selected default/explicit/no-manifest, icon, and version inputs are merged into one resource tree.[^fsharp-native-resource-plan]

These rules do not settle duplicate precedence. The pinned writer has no explicit duplicate (type, name, language) rejection, and its observed ordering comparator does not include language.[^fsharp-native-resource-writer] FSharp2 therefore freezes collision behavior only from executed oracle cases. It must not invent first-wins, last-wins, deduplication, case folding, or language precedence from the data structure.

### Required resource corpus

Harness-owned fixtures cover:

1. default manifest, explicit UTF-8 and UTF-16 manifests, --nowin32manifest, icon-only, version-only, and their valid combinations;
2. raw .res containing numeric and string type/name identifiers, neutral and non-neutral language IDs, code pages, aligned odd-sized payloads, RCDATA, manifest, icon/group-icon, version, and unknown resource types;
3. exact duplicate, case-only string-name, numeric-versus-string, same type/name with different language, raw-versus-generated version, raw-versus-default/explicit manifest, and raw-versus-icon collisions;
4. empty, truncated, misaligned, oversized, invalid-offset, invalid-string, malformed icon, malformed manifest, missing/unreadable file, and conflicting-option failures;
5. deterministic repeats in different roots and input-order permutations where the oracle accepts more than one resource; and
6. managed-resource controls proving that .mresource/F# metadata resources remain separate from the PE .rsrc tree.

For each cell, the harness records the ordered evaluated inputs and hashes, raw diagnostic streams/exit, all leftovers, and a structural resource observation keyed by (type, name, language, code page, payload hash). It compares manifest XML semantics and bytes where the oracle preserves bytes, icon/group links and image payloads, version fields/translations/string tables, directory ordering/alignment, PE resource directory bounds, and runtime lookup/fallback. Behavioral compatibility does not require byte-identical directory offsets when both trees and consumers are equivalent, while each FSharp2 deterministic repeat remains byte-identical to itself.

Windows consumers additionally use native resource APIs to observe manifests, icons, version data, and language fallback. Cross-platform structural inspection uses independently authored PE/resource parsing and never loads target code into the NativeAOT compiler. C# and F# downstream consumers verify assembly identity plus managed-resource behavior. A successful parse with a missing or shadowed resource is a failure.

### Resource incrementality

Resource content, logical/native identity, language, code page, option composition, generated version inputs, target kind/platform, and resource-schema version are final-link action inputs. A resource-only edit:

- reuses unchanged parsing, checking, lowering, exported semantic fingerprints, and method fragments;
- rebuilds the affected resource contribution and performs a fresh deterministic link unless the complete artifact action key is an exact hit;
- leaves downstream semantic compilation valid when the public assembly surface is unchanged, while packaging/runtime resource observers still rerun; and
- emits a structured resource-input-changed decision with only non-sensitive identities/hashes.

Unknown collision or target-platform impact widens the link action and reports why. A successful prior artifact remains the service recovery point after a malformed-resource or signing failure.

## Release-owned probe matrix

The following are required release evidence, not optional diagnostics:

| Probe family | Required evidence |
| --- | --- |
| Oracle identity | Resolved SDK/F# hashes and commit, host/runtime/RID/OS, satellite hashes, culture/encoding, globalization backend and ICU/NLS/CLDR identity, compiler/package/protocol/cache schema. |
| Diagnostics | Complete family-wide FSxxxx corpus crossed with all fourteen UI-language lanes, parent/neutral/missing fallback, warning controls, streams, exits, and leftovers. |
| Semantic invariance | Same source under every OS/backend/culture cell; identical non-message diagnostics, semantic fingerprints, deterministic artifacts, and runtime behavior. |
| Globalization failures | Invariant-mode rejection, Unix missing ICU, Windows missing-system-ICU NLS fallback, missing app-local ICU, corrupt/missing satellite, and changed-ICU refresh. |
| Signing positive | Unsigned/delay/public/full key-file outputs, deterministic repeats, metadata/flag/slot inspection, cryptographic verification, four-way F#/C# consumers, and package/project-reference use. |
| Signing negative/tamper | Full flag/key matrix, key-container FS3393, malformed/missing keys, tampered regions, injected late failure, exact diagnostics, and phase-specific leftovers. |
| Native resources | Every valid composition and invalid/collision case above, structural comparison, Windows native observation, cross-host equivalence, deterministic repeats, and downstream package/runtime use. |
| Persistent service | Culture-only, resource-only, signing-only, replay, failure/recovery, restart, and stale-cache cases with reconciled query/link counters and no secret persistence. |
| NativeAOT Gate A | Each of the eight RID-specific native compiler hosts runs the globalization, diagnostic, signing, and resource compiler cases from an unrelated working directory with no fallback. |
| NativeAOT Gate B | Standard SDK NativeAOT consumers of Oracle and FSharp2 emitted outputs preserve identity, resources, exceptions/messages under pinned culture, and strict AOT warnings on every declared downstream RID. |

Gate A and Gate B remain independent: NativeAOT turns a managed application into a RID-specific native executable at publish time; FSharp2 itself still emits managed PE/PDB artifacts.[^nativeaot-contract] A native compiler that never compiles these cases or an emitted DLL merely loaded by CoreCLR proves neither gate.

Raw authoritative evidence remains local/controlled. Shareable results may include public-key blobs/tokens, public fixture keys, resource fixture bytes, satellite hashes, redacted logical paths, and exact localized diagnostics. They must exclude private-key material and paths, secret environment/config values, response files or binlog fields containing secret paths, source-bearing payloads not approved for export, and dumps from a signing process. A redaction report and hash accompany every derived export; issue #16 decides retention and access without weakening these exclusions.

## Refresh triggers and residual implementation evidence

Refresh this decision and regenerate the affected oracle matrix when:

- the resolved .NET 10 SDK, F# compiler, FSharp.Core, SRM, NativeAOT runtime, or servicing commit changes;
- the shipped F# satellite set/resource hashes or diagnostic inventory changes;
- an OS/runtime changes ICU, CLDR, NLS selection, app-local ICU, invariant-mode, or satellite publication behavior;
- an accepted compiler/MSBuild signing or Win32-resource option changes;
- the public SRM signing/resource seam or the oracle key/resource writer changes;
- the eight host RID/floor matrix, target-platform matrix, or package layout changes; or
- release Authenticode/notarization tooling moves across the pre/post-determinism boundary.

Two result tables still require executed oracle population before their implementation can pass: every invalid signing flag/key combination, and every raw/generated native-resource collision. Source inspection fixes the dimensions and baseline rules but is not authority for accidental precedence. The checked-in generated manifests must contain the exact diagnostic, exit, artifact, and winner/tree observation for the resolved oracle. Until then, those cases are explicitly unimplemented; they cannot be treated as compatible, silently normalized, or removed from the final gate.

This remaining evidence does not reopen the policy: key-file modes are core, key containers match the CoreCLR unsupported result, the complete localized diagnostic family and Win32 resource surface are mandatory, private keys are never retained, and both NativeAOT gates must pass.

## Primary sources and repo evidence

- [Pinned F# compiler satellite resource tree][fsharp-xlf]
- [Pinned F# resource lookup][fsharp-resource-manager]
- [Pinned F# diagnostic identity/severity/range model][fsharp-diagnostic-structure]
- [Pinned .NET Windows and Unix globalization initialization][windows-globalization] [unix-globalization]
- [Pinned native ICU selection logic][icu-selection]
- [.NET invariant-globalization design][invariant-globalization]
- [Pinned F# signing-mode selection and strong-name writer][fsharp-signing-modes] [fsharp-signing-writer]
- [Strong-name purpose and limits][strong-name-purpose]
- [PE/COFF Authenticode hashing contract][pe-authenticode]
- [Pinned F# native-resource plan and writer][fsharp-native-resource-plan] [fsharp-native-resource-writer]
- [Public SRM ResourceSectionBuilder and ManagedPEBuilder seams][resource-section-builder] [managed-pe-builder]
- [.NET NativeAOT deployment contract][nativeaot-contract]
- [FSharp2 compiler/MSBuild contract](dotnet-10-fsharp-compiler-msbuild-contract.md)
- [Differential compatibility/performance harness](differential-compatibility-performance-harness.md)
- [NativeAOT compiler-host constraints](dotnet-10-nativeaot-compiler-host-constraints.md)
- [Emitter architecture](nativeaot-metadata-il-pdb-emitter-architecture.md)
- [Linux runtime/deployment floors](linux-runtime-and-deployment-compatibility-floors.md)
- [Comparative fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md)

[^fsharp-resource-manager]: The pinned F# resource helper resolves strings through ResourceManager.GetString with CurrentUICulture: [source][fsharp-resource-manager].
[^fsharp-diagnostic-structure]: The pinned diagnostic model keeps numbered warnings/errors, ranges, and severity adjustment distinct from localized message lookup: [source][fsharp-diagnostic-structure].
[^windows-globalization]: The pinned Windows runtime initializes app-local/system ICU and falls back to NLS only for the system-ICU path: [source][windows-globalization].
[^unix-globalization]: The pinned Unix runtime has no NLS fallback and fails initialization when requested ICU cannot be loaded: [source][unix-globalization].
[^icu-selection]: The pinned native shim probes supported ICU names/versions and can select the highest available version: [source][icu-selection].
[^invariant-globalization]: Invariant mode intentionally changes culture availability and semantic operations: [design][invariant-globalization].
[^fsharp-signing-modes]: The pinned F# driver selects key-pair versus public-key signing state from key file and flags: [source][fsharp-signing-modes].
[^fsharp-signing-writer]: The pinned writer records public/delay/full flag and slot behavior: [source][fsharp-signing-writer].
[^fsharp-strong-name-algorithm]: The pinned writer implements CLR SHA-1/RSA PKCS#1 v1.5 signing and the PE exclusion/reversal rules: [hash selection][fsharp-strong-name-hash] and [signing][fsharp-strong-name-sign].
[^strong-name-purpose]: Microsoft documents strong names as unique assembly identity, not trusted security evidence: [documentation][strong-name-purpose].
[^pe-authenticode]: The PE/COFF specification defines the distinct Authenticode hashing exclusions: [specification][pe-authenticode].
[^fsharp-native-resource-plan]: The pinned F# emission path composes raw resources, manifests, icons, and version information: [source][fsharp-native-resource-plan].
[^fsharp-native-resource-writer]: The pinned native-resource writer exposes the ordering/duplicate ambiguity that executed oracle probes must settle: [source][fsharp-native-resource-writer].
[^nativeaot-contract]: Microsoft defines NativeAOT as RID-specific publish-time native compilation and documents its deployment/limitations: [documentation][nativeaot-contract].

[fsharp-xlf]: https://github.com/dotnet/dotnet/tree/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/xlf
[fsharp-resource-manager]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/FSharp.Build/FSharpEmbedResourceText.fs#L278-L290
[fsharp-diagnostic-structure]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerDiagnostics.fs#L210-L432
[windows-globalization]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/libraries/System.Private.CoreLib/src/System/Globalization/GlobalizationMode.Windows.cs#L7-L49
[unix-globalization]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/libraries/System.Private.CoreLib/src/System/Globalization/GlobalizationMode.Unix.cs#L11-L84
[icu-selection]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/runtime/src/native/libs/System.Globalization.Native/pal_icushim.c#L233-L404
[invariant-globalization]: https://github.com/dotnet/runtime/blob/v10.0.10/docs/design/features/globalization-invariant-mode.md#L31-L45
[fsharp-signing-modes]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CreateILModule.fs#L65-L149
[fsharp-signing-writer]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/AbstractIL/ilsign.fs#L343-L396
[fsharp-strong-name-hash]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/AbstractIL/ilsign.fs#L56-L125
[fsharp-strong-name-sign]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/AbstractIL/ilsign.fs#L264-L303
[strong-name-purpose]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/standard/assembly/strong-named.md#L12-L39
[pe-authenticode]: https://github.com/MicrosoftDocs/win32/blob/1fe27142f7f97c3baf3ae3b71549238e6b2b3647/desktop-src/Debug/pe-format.md#L279-L315
[fsharp-native-resource-plan]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CreateILModule.fs#L487-L645
[fsharp-native-resource-writer]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/AbstractIL/ilnativeres.fs#L714-L808
[resource-section-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/ResourceSectionBuilder.cs
[managed-pe-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/ManagedPEBuilder.cs
[nativeaot-contract]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/deploying/native-aot/index.md#L9-L15
