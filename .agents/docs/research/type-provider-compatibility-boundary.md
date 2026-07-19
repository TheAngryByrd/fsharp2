# Type-provider compatibility boundary

Status: decision research for the type-provider frontier, supporting the final drop-in compiler/MSBuild destination

Observed: 2026-07-19 on Windows x64 with .NET SDK `10.0.110`, FSharp.Core package `10.0.100`, and the locally installed `FSharp.Compiler.Service` `43.10.110.0`

Governing decisions: [ADR 0001](../adr/0001-independent-source-informed-compiler.md), [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0003](../adr/0003-use-a-persistent-incremental-compiler-service.md), [ADR 0004](../adr/0004-target-compiler-and-msbuild-compatibility.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0008](../adr/0008-deliver-through-experimental-vertical-milestones.md), [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), [ADR 0011](../adr/0011-support-nativeaot-hosts-across-desktop-platforms.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), and [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md)

Related evidence: [.NET 10 NativeAOT compiler-host constraints](dotnet-10-nativeaot-compiler-host-constraints.md), [differential harness](differential-compatibility-performance-harness.md), [warm-edit/cache contract](warm-edit-corpus-and-cache-evidence-contract.md), and [fast-compiler design lessons](fast-fsharp-compiler-cold-and-incremental.md)

## Decision

The final type-provider compatibility boundary is an **optional, out-of-process managed provider broker** beside the NativeAOT FSharp2 compiler:

- The NativeAOT process remains the compiler. It parses, checks, optimizes, lowers, emits, reports diagnostics, owns incremental dependencies, and never loads a provider assembly.
- The broker does only the executable legacy-provider work: locate and instantiate user-supplied provider assemblies, call the public FSharp.Core provider contract, inspect the returned reflection/quotation objects, and translate them to a FSharp2-owned protocol.
- The broker is independently authored from the public contract and oracle behavior. Its own project and package graph may reference the matching public `FSharp.Core`, but must not reference or call `FSharp.Compiler.Service`, `FSharp.Build`, official `fsc`, or official compiler implementation code.
- The protocol carries session-scoped opaque handles and closed tagged descriptors for the reflection and quotation graph. It does not attempt to serialize CLR object references or expose provider objects to the native process.
- Legacy-provider-derived compiler state is request-scoped by default. Reuse across requests in one live broker session requires Oracle-compatible invalidation semantics or another complete freshness proof; cross-session reuse requires an explicit, versioned provider dependency/snapshot contract.
- Provider inputs remain explicitly `unsupported` in Experimental Vertical Milestones until the broker's declared compatibility envelope passes the provider gate below. `Unsupported` is not final compatibility coverage.

An ahead-of-time snapshot contract is worth building as an **optional FSharp2-native provider model and broker optimization**, but it cannot replace the managed broker for arbitrary existing providers. Declaring all providers unsupported is acceptable only for experimental envelopes; it is incompatible with the stated final drop-in compiler/MSBuild destination.

```text
normal evaluated .fsproj
  -> FSharp2 NativeAOT host
       -> metadata-only provider preflight
       -> independently authored compiler and emitter
       -> private inherited-pipe protocol
            -> managed provider broker (only when providers are present)
                 -> matching public FSharp.Core contract
                 -> user-supplied design-time provider + dependency closure

never in the NativeAOT compiler or broker implementation graph
  -> official fsc / FSharp.Compiler.Service / FSharp.Build
```

This is not a managed compiler fallback. If the broker cannot represent a provider interaction, the compilation fails; it never delegates F# parsing, checking, lowering, emission, or diagnostics to the official compiler.

## Why this is a special compatibility boundary

Type providers are executable build inputs, not metadata-only references. Microsoft documents that a provider assembly runs merely to populate editor/compiler type information, can connect to external data sources, and can compromise the machine if malicious.[^provider-security] The type-provider overview distinguishes erased types, which are ephemeral and not written as provided types to the consuming assembly, from generative types, which produce .NET types consumable by other assemblies.[^provider-kinds] The creation guide also describes the compiler constructing a public `[<TypeProvider>]` class, optionally with `TypeProviderConfig`, and providers depending on schemas, URLs, services, or other information sources.[^provider-creation]

The official FSharp.Core `10.0.100` API is an object protocol, not a data-transfer protocol. The observed signed Microsoft package exposes:[^api-snapshot]

| Surface | Values crossing the compiler/provider boundary |
| --- | --- |
| `IProvidedNamespace` | nested namespace objects and `System.Type` objects |
| `ITypeProvider.GetNamespaces` | namespace objects |
| `GetStaticParameters` / `ApplyStaticArguments` | `ParameterInfo[]`, `System.Type`, paths, and `object[]` static values |
| `ITypeProvider2` | method-level `MethodBase`, `ParameterInfo[]`, and `object[]` static values |
| `GetInvokerExpression` | `MethodBase`, `FSharpExpr[]`, and a returned `FSharpExpr` |
| `GetGeneratedAssemblyContents` | a logical `Assembly` object and returned physical assembly bytes |
| `Invalidate` | a live event on the provider instance |
| `TypeProviderConfig` | referenced-assembly paths, resolution and temporary folders, runtime-assembly path, target system-runtime version/type test, hosted-execution flag, and invalidation support flag |

The transitive surface is much larger than those method signatures. Public quotation patterns contain calls, witnesses, fields, properties, constructors, types, variables/binders, loops, exceptions, records, unions, delegates, and `Value`/`WithValue` nodes containing `object`; those nodes themselves refer back to reflection objects.[^quotation-snapshot] Provider-specific attributes can carry XML documentation, definition file/line/column, editor hiding, erased/relocation flags, and runtime-to-design-time assembly mapping.[^attribute-snapshot] The locally installed official FCS binary correspondingly contains distinct wrappers for provided assemblies, types, members, parameters, methods, fields, properties, events, constructors, expressions, and variables. That inventory is corroborating scope evidence only, not production code or a contract to copy.[^fcs-inventory]

NativeAOT cannot host that arbitrary executable object graph directly. Microsoft's NativeAOT contract explicitly excludes dynamic assembly loading such as `Assembly.LoadFile` and runtime code generation such as `System.Reflection.Emit`, and publishing analyzes the complete application dependency closure.[^nativeaot] Statically linking a known provider at FSharp2 publish time can work only for a closed, AOT-compatible provider set known per shipped RID; it cannot accept an arbitrary provider restored after the compiler was published.

## Options considered

| Option | Existing-provider compatibility | NativeAOT compiler boundary | Isolation and cache consequences | Decision |
| --- | --- | --- | --- | --- |
| Load providers in the NativeAOT compiler | Cannot support arbitrary post-publish assemblies because NativeAOT has no dynamic loading.[^nativeaot] A finite statically linked provider set is not drop-in compatibility. | Violates the metadata-only target universe and makes provider reflection/dynamic behavior part of the native closure. | Provider faults and global state share the compiler process; every linked provider expands the AOT/RID matrix. | **Reject as the general boundary.** A closed proof may be useful only as an experiment, never the final contract. |
| Out-of-process managed broker preserving the legacy contract | Can load the same executable provider contract on CoreCLR, while translating its object graph for FSharp2. | Native compiler stays closed-world; no provider or FCS object enters it. | Process death is containable; live state and invalidation stay session-scoped. A process is not by itself a malicious-code sandbox. | **Select for final legacy-provider compatibility.** |
| AOT-generated snapshot/new provider contract | Excellent for providers authored to declare complete inputs and emit canonical types/quotations/artifacts. | Native compiler consumes pure data and may persist it safely when freshness is proved. | Deterministic and cacheable by design, but a snapshot must be regenerated for target config/static arguments/external changes. | **Select as an optional new contract and optimization, not as the legacy boundary.** |
| Providers unsupported | Simple, secure, and honest for early milestones. | Preserves NativeAOT. | No provider execution or cache ambiguity. | **Select only before the broker gate; reject for the final drop-in claim.** |

An eager snapshot of an arbitrary existing provider is not equivalent to the existing contract. Providers may generate types on demand, accept type- and method-level static arguments, return invoker quotations, return generated assembly bytes, and signal invalidation after construction.[^api-snapshot] External schemas also may change without automatic recognition; Microsoft's troubleshooting guidance tells users to clean or rebuild to reset provider state and reconnect.[^provider-troubleshooting] A snapshot can therefore be authoritative only when its producer declares and fingerprints every relevant dependency and configuration input, or when it is treated as an ephemeral transcript of one live broker session.

## Broker production contract

### Responsibilities and clean-room boundary

The broker implementation may:

1. read provider/runtime assembly metadata to find the public provider attributes;
2. load the selected user-supplied design-time assembly and its declared dependency closure on managed CoreCLR;
3. construct the public `TypeProviderConfig` for the evaluated project/TFM;
4. discover public provider classes, instantiate them, and call `ITypeProvider`, optional `ITypeProvider2`, `IProvidedNamespace`, reflection, custom-attribute, and quotation APIs;
5. normalize successful results, exceptions, events, and generated bytes into the FSharp2 protocol; and
6. dispose provider instances and terminate when its owning session ends.

It may not contain copied official provider-loader/consumer implementation, use official internal type-provider wrappers, ask FCS to interpret reflection or quotations, or invoke official compilation. The Compatibility Oracle remains a separate test process. Installed FCS metadata was inspected only to size the compatibility surface; no implementation source or binary becomes a production reference.

User-supplied provider binaries are executable build inputs, not FSharp2 implementation. FSharp2 ships none of their source or implementation code. Executing the provider is the purpose of the broker and occurs only after the build has selected/trusted that provider.

### May a provider bring FSharp.Compiler.Service?

The **broker itself may not reference FCS**. Its project graph, restore assets, deployed files, startup loads, and callable implementation are audited to prove that.

An arbitrary provider may, however, bring FCS in its own dependency closure. Categorically rejecting that closure would reject an otherwise oracle-loadable existing provider and would narrow the promised compatibility surface. Such a dependency is allowed only under all of these rules:

- it was supplied and selected as part of the provider's own resolved input closure; FSharp2 never substitutes the SDK's FCS or adds one on the provider's behalf;
- it is loaded only in the provider's managed isolation context and is hashed as untrusted provider input;
- the broker implementation has no compile-time or reflective call path to FCS APIs;
- no official compiler process is spawned, and no provider-owned FCS object crosses the wire; and
- the same provider/closure must be loadable by the pinned Compatibility Oracle on that host, with observable candidate behavior compared in the same controlled case.

This is analogous to a provider bringing any other implementation dependency. It does not authorize FCS fallback. If a provider deliberately uses FCS internally, that execution is provider behavior; the no-fallback gate still proves that FSharp2 itself performed the compilation.

### Discovery, probing, and assembly loading

The NativeAOT process performs only PE/metadata preflight. It records the exact runtime reference, any `TypeProviderAssemblyAttribute` mapping, target-framework/architecture metadata, file hashes, and evaluated resolution inputs without loading code. The public attribute explicitly allows runtime and design-time assemblies to be the same or different.[^attribute-snapshot]

The managed broker performs executable probing. It must preserve the oracle-observable relationship among:

- the referenced runtime assembly that caused provider creation (`RuntimeAssembly`);
- the mapped design-time provider assembly;
- `ResolutionFolder`, the full base path for relative file-name arguments;
- `TemporaryFolder`;
- the complete ordered `ReferencedAssemblies` input;
- target system-runtime version and type-presence answers; and
- the provider-observable `IsHostedExecution` and `IsInvalidationSupported` values for the exact Oracle build shape.[^api-snapshot]

The broker uses a deliberate `AssemblyLoadContext` dependency policy and shares only the public contract identity needed to cast provider instances. Microsoft documents that one load context admits one assembly version per simple name, that multiple contexts can hold conflicting dependency versions, and that the isolation is name/loading isolation rather than binary isolation.[^alc] Therefore provider dependency islands must not be silently unified merely for convenience, and an `AssemblyLoadContext` is not the security boundary. The process is the fault/lifetime boundary; the build's OS identity or external sandbox is the permission boundary.

Provider-visible hosting and invalidation flags are Compatibility Oracle behavior, not values inferred from broker lifetime. Microsoft's authoring guidance says `fsc` and FSI ignore provider invalidation events, which makes `IsInvalidationSupported=false` the one-shot hypothesis, but it does not record the value passed by the pinned compiler.[^provider-invalidation] An instrumented provider must capture both flags for standalone and every declared MSBuild/service build shape before that shape becomes supported. FSharp2 reproduces the observed values. It may advertise invalidation support only when the matching compatibility evidence permits it and the broker subscription, provider epoch, dependency edges, cancellation, and recheck path are already active; losing that machinery invalidates the complete epoch. If the Oracle-compatible value is `false`, a persistent service must not silently change it to `true` and must not rely on an event the provider was told is unsupported. Provider-derived state is then request-scoped unless another complete freshness proof justifies reuse.

Exact probing order, FSharp.Core binding redirects/unification, native-library lookup, provider-visible hosting/invalidation flags, and exception timing are oracle-owned compatibility details. They must be discovered by differential probes rather than copied from official loader code.

### Wire protocol

The production transport is a private pair of inherited local pipe handles. It has no listening TCP endpoint, and provider stdout/stderr are captured separately so provider writes cannot corrupt framing. Every frame is length-prefixed, bounded, request-correlated, cancellable, and covered by a session nonce.

The initial handshake records and negotiates:

- protocol major/minor and descriptor/quotation schema versions;
- FSharp2 and broker build identities;
- public FSharp.Core contract identity;
- host OS, architecture, runtime version, and broker bitness;
- target TFM/runtime identity from evaluated references; and
- capabilities and hard message/object/depth/byte limits.

A major/schema mismatch fails before provider code loads. Minor evolution is additive only after capability negotiation; unknown required tags fail closed. The canonical encoding never uses CLR object serialization.

Provider objects remain in the broker. The native process sees opaque `(session, generation, kind, id)` handles plus immutable descriptors. At minimum the protocol must cover:

- namespace, assembly/module, type and constructed-type identity;
- generic parameters/constraints, base/interface/nesting relationships;
- constructors, methods, fields, properties, events, accessors, parameters, defaults, calling conventions, modifiers, and custom attributes;
- type- and method-level static-parameter queries and applications;
- every quotation shape accepted by the oracle, with stable variable/binder identities, member/type handles, literal/value representations, and custom attributes;
- generated-assembly identity and bytes; and
- invalidation, provider output, exception, cancellation, disposal, and process-exit events.

The public quotation surface admits `object` values, so a JSON-primitive-only codec is insufficient.[^quotation-snapshot] The exact oracle-accepted constant/static-argument universe and failure point are a required probe result. Values which cannot be canonicalized remain broker handles until validation; the native compiler must reproduce the oracle diagnostic rather than inventing successful semantics.

Handles are never persisted or reused after broker restart. A lost connection invalidates the entire handle space and all dependent compiler nodes. The broker must not automatically replay a mutating provider call after a crash or timeout because replay can duplicate external effects.

### Security and isolation

Microsoft's provider security guidance says provider code runs during design/compilation and a malicious provider can compromise the machine.[^provider-security] The selected process boundary improves compiler integrity and fault containment, but does **not** make arbitrary provider code safe:

- trusted/drop-in mode runs under the build user's identity with the working directory, environment, filesystem, network, credentials, and native-library access needed by the same provider under the oracle;
- hardened CI may place the entire build/broker in a container, job object, namespace, low-privilege account, or network/filesystem policy, but restrictions which change provider behavior define a separate declared envelope;
- provider processes are never globally shared across repositories or trust domains;
- each provider closure/session receives a private temp root and transactional output root;
- message count, size, recursion, generated-assembly bytes, CPU/time, and memory are observable and cancellable; and
- cancellation, broker crash, protocol violation, or limit breach kills the affected broker, deletes uncommitted outputs, invalidates its complete provider epoch, and reports a structured operational failure.

There should be no deceptively named “sandboxed” mode based only on `AssemblyLoadContext`. The official loading documentation says contexts do not provide binary isolation, and the provider security documentation treats trust as a prerequisite.[^alc] [^provider-security]

### Determinism, invalidation, and caches

Provider-dependent state has a separate `providerEpoch` from ordinary reference/source epochs. This applies the [fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md) rule that semantic reuse is keyed by stable inputs and explicit dependencies rather than timestamps: provider identity, configuration, output, and invalidation generation are compiler-query inputs, not ancillary logging. The epoch contains:

- hashes and assembly identities for runtime assembly, design-time assembly, and complete managed/native dependency closure;
- broker, protocol, descriptor, quotation, and public-contract versions;
- target TFM/runtime facts, referenced assemblies, resolution/temp/working-directory policy, culture, host OS/RID/architecture, and relevant environment configuration;
- static argument values and every provider query/result descriptor;
- generated assembly byte hashes; and
- the current provider instance plus invalidation generation.

When `Invalidate` fires in a persistent service, the bridge acknowledges it before accepting more results, increments the generation, cancels outstanding requests, discards all handles/snapshots from the old generation, and conservatively invalidates every declaration/artifact derived from that provider. The public config exposes whether the host responds to invalidation, but the event carries no dependency-level payload, so narrowing below the provider contribution requires separately proved dependencies.[^api-snapshot] If FSharp2 cannot prove the affected provider query/declaration frontier, it widens first to every node derived from that provider and then to the containing project as needed, emitting the widening reason. It never reuses a result across an unknown provider effect.

Default cache policy for legacy providers is:

| State | Reuse policy |
| --- | --- |
| Reflection objects, provider instances, opaque handles | Never persisted; live broker session only as an object-lifetime bound |
| Normalized descriptors/quotations and generated bytes | Reusable across requests in a live session only when the Oracle-compatible invalidation mode or another complete freshness proof keeps the provider epoch valid |
| Compiler declarations/artifacts depending on provider output | Reusable only within a proved-valid provider epoch; otherwise request-scoped and discarded on invalidation, broker death/restart, clean/rebuild, config/closure change, or unknown effect |
| Persistent disk snapshot | Disabled unless a versioned provider-declared dependency contract proves all external inputs/freshness and the snapshot passes deterministic replay validation |

Providers can consult schemas, files, URLs, services, credentials, time, environment, or arbitrary code, and Microsoft notes that schema changes may not automatically refresh until clean/rebuild.[^provider-creation] [^provider-troubleshooting] Hashing provider DLLs alone is therefore insufficient proof for cross-session reuse. Clean/rebuild must terminate the old provider session and establish a fresh epoch. A future snapshot contract can make declared files/URLs/schema tokens/expiry or content digests explicit and thereby unlock portable content-addressed reuse; undeclared legacy behavior stays conservative.

Provider performance remains attributable rather than hidden inside `check` or a cache verdict. Every invocation records broker launch/runtime selection, handshake, metadata preflight, dependency probing/load, construction, provider query category/count, descriptor/quotation normalization, generated bytes, invalidation/cancellation, IPC bytes/latency, queue time, and provider execution wall/CPU where observable. Cache evidence records the provider epoch and exact hit/miss/bypass/widening reason. The normal three-second Performance Tripwire and structured trace include those bounded provider/RPC phases; a persistent broker or descriptor hit is not evidence of valid Warm Compilation unless the epoch and provider-dependent semantic nodes were actually validated. This is the provider-specific application of the [fast-compiler design lessons](fast-fsharp-compiler-cold-and-incremental.md) and the repository's performance governance.

### Diagnostics and failure semantics

Provider behavior is part of family-wide FS Diagnostic Compatibility. For every provider discovery, constructor, namespace, reflection, static-argument, quotation, generated-assembly, resolution, and disposal failure, the pinned Compatibility Oracle decides:

- whether a diagnostic occurs;
- exact `FSxxxx` code, effective severity, message, range, ordering, stream, suppression/promotion, exit behavior, and partial-output cleanup; and
- which provider exception details, paths, or inner exceptions are exposed.

The broker returns structured exception type/assembly, message, stack frames, operation, provider identity, and correlation data separately from user-facing text. FSharp2—not the broker—formats the oracle-compatible diagnostic. Protocol corruption, broker launch failure, cancellation, product timeout, or resource-limit termination has no presumed FS mapping: it is an explicit operational failure unless an oracle probe proves an equivalent compiler diagnostic. It is never reported as `unsupported`, cached as a provider result, or silently retried.

Same-invocation differential evidence is authoritative. Each oracle/candidate pair uses the same evaluated project inputs and equivalent isolated copies of provider data/services, environment, culture, working directory, and target references. The provider may not share mutable state between the two lanes. The gate compares diagnostics, compiler-owned artifacts, generated provider artifacts, downstream compilation, and runtime behavior; a previous broker snapshot cannot replace provider execution in the evidence-producing candidate invocation.

### Emitted and runtime artifacts

Provider hosting must leave no broker implementation detail in output:

- For an erased provider, provided types remain compile-time-only; FSharp2 lowers the returned invoker quotation to normal IL references against the provider's runtime representation, and emits no broker/protocol/FCS reference merely because the broker hosted it.[^provider-kinds]
- For a generative provider, `GetGeneratedAssemblyContents` returns the physical bytes of a logical provided assembly.[^api-snapshot] Microsoft's authoring guide says ordinary compilation statically links the backing DLL's IL definitions and managed resources into the final DLL/EXE, while hosted FSI loads the backing DLL.[^provider-backing-assembly] FSharp2 validates the returned bytes and reproduces the oracle's relocation/`SuppressRelocate`, assembly-reference, resource, PDB, signing, cleanup, and downstream-consumption behavior; it must not assume that copying a separate DLL is equivalent.
- When runtime and design-time assemblies differ, only the oracle-required runtime/generated artifacts flow to build output; the design-time implementation stays a compiler input.[^attribute-snapshot]
- Provider-generated output participates in deterministic and NativeAOT-consumer gates exactly like other managed output. FSharp2 must not rewrite nondeterministic provider bytes and call them compatible; fixtures must control external provider inputs and compare oracle-observable results.

The broker is a build-time sidecar. Applications compiled with an erased provider do not need it at runtime; generated/runtime provider artifacts follow the same deployment behavior as oracle output.

### Cross-platform and RID behavior

The managed broker runs on the **build host**, not the target application's `RuntimeIdentifier`. Its runtime/architecture and provider native dependencies therefore follow the current Windows/Linux/macOS x64/Arm64 build host, while `TypeProviderConfig` target-runtime facts come from the evaluated target references. NativeAOT itself publishes RID-specific applications for those six desktop OS/architecture pairs; [ADR 0026](../adr/0026-support-versioned-glibc-and-musl-linux-floors.md) splits Linux by glibc/musl into eight FSharp2 host payloads.[^nativeaot]

The integration package should carry one platform-neutral broker payload plus the existing RID-selected native host where feasible, launched by the same SDK-selected .NET host/runtime family as MSBuild. Any broker apphost or native dependency is selected by build-host RID. A provider with OS/architecture-specific design-time or native dependencies is supported only in cells where the pinned oracle loads it; “works on win-x64” does not prove another host. Provider-generated managed artifacts still run through the separate downstream RID/NativeAOT consumer matrix.

Protocol encoding and provider snapshots are platform-neutral only for fields whose underlying inputs are portable. Host paths, filesystem case behavior, runtime/native dependency identities, and provider output remain explicit validity inputs rather than normalized away.

## Milestone boundary

Provider support progresses without weakening the final decision:

1. **Before a broker exists:** any provider-marked runtime/design-time reference or provider use fails early with the declared experimental unsupported-envelope diagnostic; no official compiler fallback and no output success.
2. **Protocol/probe milestone:** a newly authored synthetic provider exercises namespace/type/member reflection, type and method static parameters, quotations, invalidation, exceptions, generated bytes, cancellation, and process loss. This proves the boundary, not arbitrary-provider compatibility.
3. **Legacy-provider experimental envelope:** selected real erased and generative providers pass the full same-invocation oracle matrix on declared hosts/TFMs. Anything outside the manifest remains explicitly unsupported.
4. **Final provider gate:** arbitrary existing providers are no longer excluded as a class. The matrix below passes for the declared final platform/TFM surface, and unsupported is not an accepted verdict for an oracle-successful provider case.

In parallel, FSharp2 may publish a new snapshot-provider SDK whose output is the same canonical descriptor/quotation/artifact schema plus a dependency manifest. That path can run without the legacy broker when a project selects such a provider, but it is additive and must not be confused with evidence for legacy `ITypeProvider` compatibility.

## Required compatibility probes and final gate

The provider corpus needs small independently authored contract probes plus pinned real packages. Each row runs through normal evaluated MSBuild and compares oracle and FSharp2 on the same host:

| Area | Minimum proof |
| --- | --- |
| Discovery/mapping | Provider assembly same as runtime assembly; separate runtime/design-time mapping; multiple provider classes/namespaces; absent/invalid attributes; dependency version conflict |
| Namespace/type reflection | Nested/lazy namespaces and types; arrays/byrefs/pointers/generics; base/interfaces/nested types; all member kinds; overloads/accessors; attributes, modifiers, defaults, erased/relocation flags |
| Static parameters | Type and `ITypeProvider2` method parameters; every oracle-accepted literal/default/optional/error form; relative file paths resolved from the exact `ResolutionFolder` |
| Quotations | Every oracle-accepted quotation pattern, witness call, binder identity, member/type reference, literal/value kind, custom attribute, invalid quotation, and provider exception |
| Erased runtime | Emitted IL contains correct runtime calls/references, no provided type leakage, and matches downstream F#/C# and NativeAOT behavior |
| Generative runtime | Generated bytes, assembly identity/reference/copy behavior, deterministic repeats where inputs are controlled, downstream F#/C# use, and NativeAOT consumption |
| Invalidation/cache | Instrumented provider records `IsHostedExecution` and `IsInvalidationSupported` for standalone and every declared MSBuild/service shape; candidate matches the Oracle values; true-capability cases cover events during idle/in-flight requests and repeated events; false-capability cases prove no event-dependent reuse; provider/data/config/closure change, clean/rebuild, broker restart/crash, fail-closed widening, and proof that stale handles and disk entries cannot hit |
| Diagnostics | Discovery, load, constructor, query, reflection, static-argument, quotation, generated-byte, dependency/native-load, dispose, cancellation, and broker operational failures with full FS Diagnostic Compatibility where the oracle owns an FS diagnostic |
| Isolation/abuse | Provider writes stdout/stderr, exits/crashes, hangs, allocates excessively, sends deep/large graphs, uses path traversal/temp files, starts children, and performs network/file access under declared trust policy |
| Dependency closure | Older/newer FSharp.Core contracts, private dependency conflicts, native libraries, and a provider-owned FCS dependency proving it remains opaque and cannot become fallback |
| Build shapes | Multi-targeting, parallel projects, persistent service versus standalone, clean/rebuild, Debug/Release, path/culture/environment variation, and transactional failure cleanup |
| Host matrix | Oracle-successful provider cases across win-x64, win-arm64, linux-x64, linux-arm64, osx-x64, and osx-arm64; explicit oracle-matched failures for host-incompatible providers |

The final gate additionally proves:

1. native-host publish/execution remains strict and warning-clean on all eight host RIDs with no provider/FCS assembly in its closure;
2. broker project/package graph contains public FSharp.Core but no FCS, FSharp.Build, official compiler, or oracle implementation;
3. process lineage and sentinels prove no official compiler invocation;
4. no provider, reflection, quotation, or provider-owned FCS object enters the NativeAOT process;
5. all required provider cells pass diagnostics, artifacts, downstream behavior, cleanup, invalidation, and cache evidence; and
6. provider-enabled compiler invocations still satisfy the repository's correctness-first performance governance, with broker startup, handshake, probing/load, provider query/normalization, generated-byte, invalidation, and RPC work separately attributed rather than hidden.

## Residual uncertainties

1. **Exact consumed reflection surface.** The public object graph is broad, and API signatures alone do not establish every property/query or its ordering. Build an audited query ledger from differential probes; missing operations fail closed.
2. **Quotation/static-value universe.** Public quotation patterns expose `object`, but the oracle accepts only whatever its compiler path can lower. The exact value kinds and diagnostic points require probes.
3. **Assembly resolution and type identity.** Provider packages span FSharp.Core versions and may carry conflicting managed/native dependencies. Exact same-host oracle behavior, not an invented binding policy, decides compatibility.
4. **Provider-owned FCS dependencies.** The boundary permits them only as opaque user input. At least one real probe must prove shared FSharp.Core identity, isolation, no broker-to-FCS call path, and no fallback.
5. **Security.** No in-process managed loading mechanism makes malicious provider code safe. A hardened OS sandbox can change provider behavior and therefore needs an explicit compatibility envelope.
6. **Nondeterministic/external providers.** Some providers cannot supply a complete freshness proof. Correctness requires live-session-only reuse and controlled oracle fixtures even if that costs warm performance.
7. **Platform coverage.** A useful real-provider corpus with native dependencies on all eight hosts has not yet been selected. Final support is limited to oracle-successful same-host cells, not a claim that every provider is portable.
8. **Editor/FSI behavior.** This decision sets the batch compiler/MSBuild boundary; an instrumented provider must still capture the exact Oracle `IsHostedExecution` value for each declared build shape. FSI and FCS/editor API compatibility remain outside the core destination; a future editor host may reuse the protocol but needs its own lifecycle/security decision.

## Primary sources and reproducibility

- [Microsoft Learn: Type Providers][provider-overview]
- [Microsoft Learn: Tutorial — Create a Type Provider][provider-create]
- [Microsoft Learn: Type Provider Security][provider-security-source]
- [Microsoft Learn: Troubleshooting Type Providers][provider-troubleshooting-source]
- [Microsoft Learn: Native AOT deployment and limitations][nativeaot-source]
- [Microsoft Learn: `AssemblyLoadContext` loading and isolation model][alc-source]
- [Microsoft-verified FSharp.Core `10.0.100` package][fsharp-core-package]
- Locally installed, signed official FSharp.Core/FCS assemblies from .NET SDK `10.0.110`, inspected as API metadata only

The API snapshot is reproducible with:

```powershell
dnx dotnet-inspect -y -- package FSharp.Core@10.0.100 -v:d
dnx dotnet-inspect -y -- member Microsoft.FSharp.Core.CompilerServices.ITypeProvider --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- member Microsoft.FSharp.Core.CompilerServices.ITypeProvider2 --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- member Microsoft.FSharp.Core.CompilerServices.IProvidedNamespace --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- member Microsoft.FSharp.Core.CompilerServices.TypeProviderConfig --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- member Microsoft.FSharp.Quotations.PatternsModule --package FSharp.Core@10.0.100 --oneline
dnx dotnet-inspect -y -- find '*TypeProvider*Attribute*' --package FSharp.Core@10.0.100 --all --table
dnx dotnet-inspect -y -- find '*TypeProvider*' --library 'C:\Program Files\dotnet\sdk\10.0.110\FSharp\FSharp.Compiler.Service.dll' --all --table
```

Observed local identities:

| File | Assembly/informational version | SHA-256 |
| --- | --- | --- |
| `FSharp.Core.dll` | `10.0.0.0` / `10.0.110-servicing.26326.116+f7d90799ce4ef09a0bb257852a57248d2a8fb8dd` | `FF83CEAD5566C9111B69E920C2703E6D8D125305D6AC1AC08692CAFEE3890968` |
| `FSharp.Compiler.Service.dll` | `43.10.110.0` / `43.10.110-servicing.26326.116+f7d90799ce4ef09a0bb257852a57248d2a8fb8dd` | `783F1BB555880B64E0BA3D571D65AB220EADF2C34DCC0A8DB1CC6DEF8ED3C181` |

[^provider-security]: Microsoft states that provider DLL code runs during browsing/design-time, may connect to external sources, and can compromise the machine if malicious: [Type Provider Security][provider-security-source].
[^provider-kinds]: Microsoft distinguishes generative types written as .NET types for downstream consumption from erased ephemeral types not written as provided types to the assembly: [Type Providers][provider-overview].
[^provider-creation]: Microsoft documents provider discovery/creation, optional `TypeProviderConfig`, and schema/service-driven use: [Create a Type Provider][provider-create].
[^api-snapshot]: Observed in the signed Microsoft FSharp.Core `10.0.100` package using the commands above; package provenance is recorded at [NuGet][fsharp-core-package]. `dotnet-inspect` descriptions identify `ResolutionFolder` and `TemporaryFolder` as full paths, `RuntimeAssembly` as the referenced assembly which caused creation, and `IsInvalidationSupported`/`IsHostedExecution` as host capability/context flags.
[^quotation-snapshot]: Observed from `Microsoft.FSharp.Quotations.FSharpExpr` and `PatternsModule` in the signed FSharp.Core `10.0.100` package using the command above.
[^attribute-snapshot]: Observed from `TypeProviderAssemblyAttribute`, `TypeProviderDefinitionLocationAttribute`, `TypeProviderXmlDocAttribute`, `TypeProviderEditorHideMethodsAttribute`, and `TypeProviderTypeAttributes` in the signed FSharp.Core `10.0.100` package. The assembly attribute's official XML documentation says runtime and design-time assemblies may be the same.
[^fcs-inventory]: Observed by type-name/API-metadata inspection of the locally installed signed `FSharp.Compiler.Service.dll` identified above. This evidence was not derived from implementation source and does not authorize a production reference.
[^nativeaot]: Microsoft documents NativeAOT's closed publish model, RID-specific output, supported desktop OS/architecture matrix, and explicit lack of dynamic loading and runtime code generation: [Native AOT deployment][nativeaot-source].
[^provider-troubleshooting]: Microsoft says a changed schema is not necessarily recognized automatically and advises clean/rebuild to reset provider state and reconnect: [Troubleshooting Type Providers][provider-troubleshooting-source].
[^provider-invalidation]: Microsoft's authoring tutorial documents provider invalidation and says Visual Studio responds by re-typechecking while FSI and `fsc` ignore the event: [Create a Type Provider][provider-create].
[^provider-backing-assembly]: Microsoft's authoring tutorial documents ordinary compilation relocation/static linking and hosted FSI loading in its “Backing Assembly” section: [Create a Type Provider][provider-create].
[^alc]: Microsoft documents per-context version rules, conflicting-version contexts, shared dependencies/type identity, and that contexts have no binary isolation: [`AssemblyLoadContext`][alc-source].

[provider-overview]: https://learn.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/
[provider-create]: https://learn.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/creating-a-type-provider
[provider-security-source]: https://learn.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/type-provider-security
[provider-troubleshooting-source]: https://learn.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/troubleshooting-type-providers
[nativeaot-source]: https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/
[alc-source]: https://learn.microsoft.com/en-us/dotnet/core/dependency-loading/understanding-assemblyloadcontext
[fsharp-core-package]: https://www.nuget.org/packages/FSharp.Core/10.0.100
