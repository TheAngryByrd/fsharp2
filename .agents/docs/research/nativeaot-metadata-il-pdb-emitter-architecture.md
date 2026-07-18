# NativeAOT metadata, IL, and portable-PDB emitter architecture

Status: versioned research decision for [Select the metadata, IL, and PDB emitter architecture][issue-7], supporting [Build FSharp2: a NativeAOT, incremental, drop-in F# compiler][issue-1]

Observed: 2026-07-18

Version baseline: .NET SDK `10.0.110` at `f7d90799ce4ef09a0bb257852a57248d2a8fb8dd`, runtime and `System.Reflection.Metadata` `10.0.10`, and F# compiler/FSharp.Core `10.0.100` at `b0f34d51fccc69fd334253924abd8d6853fad7aa`

Issue question: **Which NativeAOT-compatible ECMA-335 metadata, IL, portable-PDB, signing, resource, and deterministic-emission architecture gives FSharp2 the best correctness and measured throughput without constraining later incremental fragment reuse?**

Governing decisions: [ADR 0001](../adr/0001-independent-source-informed-compiler.md), [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0006](../adr/0006-require-behavioral-not-byte-identical-output.md), [ADR 0007](../adr/0007-use-fsharp-with-benchmark-justified-csharp-kernels.md), [ADR 0009](../adr/0009-use-a-target-framework-agnostic-emitter.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), [ADR 0017](../adr/0017-select-memory-techniques-by-benchmark.md), [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md), [ADR 0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md), and [ADR 0021](../adr/0021-use-public-srm-behind-symbolic-emission-fragments.md)

## Answer

FSharp2 should own a **versioned, immutable symbolic emission graph** and use a thin final linker built only on the public .NET 10 `System.Reflection.Metadata`, `System.Reflection.Metadata.Ecma335`, and `System.Reflection.PortableExecutable` APIs.

The durable incremental boundary is a method/declaration fragment containing symbolic metadata identities, symbolic IL operands and labels, exception regions, locals, sequence-point anchors, and resource/debug contributions. It contains **no** `MetadataBuilder`, `EntityHandle`, metadata token, heap offset, method-body offset, RVA, PE/PDB content id, or strong-name signature. Those are final-artifact facts and are assigned by a fresh deterministic link plan for each implementation or reference assembly.

The final linker:

1. closes and canonically orders the selected symbolic fragments;
2. plans metadata rows and resolves symbolic identities to final handles;
3. encodes signatures, user strings, method bodies, exception regions, mapped data, and managed resources;
4. constructs portable PDB metadata from final method order and final IL offsets;
5. constructs the debug directory and managed PE;
6. applies deterministic identities, requested signing, native resources, and checksums; and
7. validates and atomically publishes the complete requested artifact set.

This is the best correctness architecture available from the evidence. It uses the ECMA-335 implementation maintained with .NET, keeps the production dependency closure small, needs no target-assembly loading or runtime code generation, and leaves FSharp2—not a serializer library—in control of declaration-level fragment identity. It is **not** a claim that direct SRM emission is already measured fastest. No primary source contains an apples-to-apples FSharp2/IcedTasks benchmark. Final-link throughput, allocation strategy, branch shrinking, pooling, parallel encoding, token-neutral byte reuse, and any C# kernel remain benchmark questions under ADRs 0007 and 0017.

## Why the boundary must sit above SRM

The public SRM surface is deliberately a final encoder, not an incremental compiler database:

- `MetadataBuilder.Add*` appends rows and returns handles whose row ids depend on final insertion order.[^metadata-builder]
- `InstructionEncoder.Token(EntityHandle)` immediately writes the numeric token, and `LoadString` immediately writes a user-string token.[^instruction-token]
- `ControlFlowBuilder` records method-local labels, branches, and exception regions. The public `MethodBodyStreamEncoder.AddMethodBody` invokes its branch fixup and returns the method-body stream offset.[^branch-fixup]
- `AddMethodDefinition` stores an offset within the IL stream. `MetadataRootBuilder.Serialize` later adds the section RVA; field-data offsets are resolved the same way.[^rva-fixup]
- `PortablePdbBuilder` needs the final type-system table row counts, while the portable-PDB `MethodDebugInformation` table is indexed by final `MethodDef` row id.[^portable-pdb-builder] [^portable-pdb-spec]

Persisting any of those final handles or offsets would make a cached method depend on unrelated row, heap, section, or branch-layout decisions. A preceding declaration could then move a token and silently corrupt an otherwise unchanged cached body or PDB record.

The cache-safe representation is therefore symbolic:

```text
typed declaration
  -> stable declaration/emission identity
  -> SymbolicMethodFragment
       IL opcodes + symbolic metadata operands
       local labels + exception-region labels
       local signature + max-stack facts
       sequence-point/scope anchors tied to symbolic instructions
       mapped-data/resource/debug contributions
       schema version + dependency ids + content hash
  -> final artifact link
       metadata row/heap plan
       handles/tokens + branch distances
       method offsets + RVAs
       PDB rows/IL offsets
       PE/PDB ids + signature + checksums
```

The [comparative compiler research](fast-fsharp-compiler-cold-and-incremental.md) supports exactly this shape: reuse work by stable semantic inputs, make invalidation explicit, and keep the reusable unit narrower than a file when correctness permits. ADR 0018 already requires emitted method-fragment reuse. This decision makes that fragment honest: symbolic state is reusable; linked bytes are reusable only when a later relocation-aware optimization proves every final dependency unchanged.

## Recommended production modules

The names below describe responsibilities, not mandatory project names.

| Module | Owns | Must not own |
| --- | --- | --- |
| Symbolic emission model | Stable definition/reference ids; signatures; method/debug/resource fragments; schema and hashes | SRM builders, handles, row ids, offsets, paths not normalized by invocation policy |
| Deterministic link planner | Closure; artifact mode; canonical table/heap/resource/document order; planned row handles; final dependency map | PE byte serialization or secret key material |
| SRM metadata/IL encoder | Ephemeral builders; signature blobs; token resolution; method-local branch/EH encoding; body and mapped-data offsets | Persistent cache identities or semantic invalidation |
| Portable-PDB encoder | Public debug-table rows plus FSharp2-owned sequence-point/import/custom-debug blob codecs; documents, checksums, scopes, SourceLink, embedded source | Source parsing or method identity based only on row number |
| Resource encoder | FSharp2-owned length-prefixed managed-resource stream and native `ResourceSectionBuilder`; linked-resource metadata and F# payloads | Runtime `ResourceManager` loading of target artifacts |
| Deterministic identity/signing | PDB/PE content-id providers, reserved-MVID patch, PDB checksum, strong-name mode/key adapter | Key bytes in persistent compiler caches |
| Artifact transaction | Temporary outputs, structural validation, signing, complete-set commit, failure cleanup | Partial publication after any required artifact fails |

The symbolic model is FSharp2-owned and versioned. The SRM adapter is replaceable. If a future benchmark establishes that another final encoder materially wins without weakening correctness or NativeAOT, only the adapter changes; cached semantic/emission identities do not become third-party object identities.

## Deterministic final-link protocol

### 1. Freeze the artifact request

Create a normalized link request from the evaluated Compiler Target Invocation:

- implementation versus reference assembly;
- target kind, platform, PE header flags, entry point, base address, subsystem, and high-entropy/32-bit options;
- exact target reference identities selected by MSBuild, never host-runtime `System.Type` values;
- ordered selected definitions and method fragments;
- optimization/debug/tail-call/reflection-free/F# metadata modes;
- managed, linked, Win32, F# metadata, SourceLink, embedded-source, and XML/signature outputs;
- path map, checksum algorithm, deterministic mode, and signing mode; and
- compiler/emission schema version.

The link request and each selected fragment are content-addressed. The link action key includes every output-affecting normalized input, target-reference fingerprint, resource hash, artifact kind, public signing-key fingerprint, and compiler/schema version. It never includes a private key byte or a physical root erased by the declared path map.

### 2. Plan rows before encoding token operands

Close all definitions, references, specifications, standalone signatures, user strings, resources, documents, and custom-debug records. Assign a canonical order that:

- preserves F# declaration/source order wherever order is observable or defines table ownership;
- meets ECMA-335 sorted-table and contiguous-list requirements;
- uses stable semantic identity as the deterministic tie-breaker; and
- never depends on dictionary/hash-set enumeration, process-randomized hashes, culture, worker completion order, or filesystem enumeration.

The planner can construct predicted handles with the public `MetadataTokens.*Handle(rowNumber)` methods. When rows are later added, every returned handle is asserted against the plan. `MetadataRootBuilder` retains order validation; `SuppressValidation` stays false.[^metadata-order]

Definitions such as `MethodDef` need final body offsets even though their tokens are operands in other bodies. Predicted handles break that cycle without leaking row numbers into the persistent fragments.

### 3. Populate deterministic heaps and encode signatures

Use a fresh `MetadataBuilder` per artifact. Intern strings, blobs, GUIDs, and user strings in the link plan's deterministic traversal order. Encode ECMA-335 signatures through the public blob encoders using planned handles.

Reserve the MVID in the fresh metadata builder with `MetadataBuilder.ReserveGuid`, put its returned handle in the module row, and leave the reserved 16 bytes zero while the deterministic PE id is computed. After `ManagedPEBuilder.Serialize` returns the PE `BlobContentId`, patch the reserved MVID blob with `peContentId.Guid`; perform any full strong-name signature only after that patch. This is the public, cycle-breaking deterministic pipeline used as behavioral evidence by the official compiler: the PE id hashes a canonical image with the reserved MVID/signature slots zero, while the final MVID is reproducibly derived from that id. SRM does not invent or populate the module MVID automatically.[^reserved-guid]

### 4. Encode methods locally, then add method rows

For each method in final `MethodDef` order:

1. resolve symbolic metadata operands to planned handles;
2. resolve user strings and local signatures to their final handles;
3. create an independent `BlobBuilder`, `ControlFlowBuilder`, and `InstructionEncoder`;
4. emit labels, long/short branches, exception regions, and instructions;
5. call public `MethodBodyStreamEncoder.AddMethodBody`;
6. retain its returned IL-stream offset and a mapping from symbolic instruction anchors to final IL offsets; and
7. add the `MethodDef` row at its planned row id, asserting the returned handle.

SRM fixes branch operands when the body is copied to the method stream, but it does not automatically promote an out-of-range short branch. The first correct implementation should emit a long branch whenever distance is not already proven. An optional branch-size fixed-point pass is a generated-code size/throughput benchmark question, not an architectural requirement.[^branch-fixup]

FSharp2 owns max-stack calculation and stack/type correctness. SRM accepts the supplied max stack; ILVerify and runtime/consumer gates remain required.

Independent methods may be encoded in parallel **after** the global handle plan is frozen, then concatenated in deterministic method order. Whether parallel encoding beats scheduling/allocation cost on IcedTasks must be measured. Worker completion order can never affect bytes.

### 5. Complete metadata and let SRM assign final RVAs

Add remaining rows in their planned table order, including parameters, types, semantics, generic constraints, custom attributes, manifest resources, and field RVAs. Method-body and mapped-field values remain offsets relative to their supplied streams. `MetadataRootBuilder` and `ManagedPEBuilder` add final section RVAs during serialization; FSharp2 does not patch PE RVAs itself.[^rva-fixup]

Create `MetadataRootBuilder` only after the metadata builder is complete. Builders are single-link mutable state and never enter live or disk caches.

## Portable PDB and SourceLink

The public boundary is lower-level than the table names suggest. `MetadataBuilder` exposes debug-table row methods, but .NET 10 exposes no public `SequencePointsEncoder` and no public encoder for the import-definition blob. `AddMethodDebugInformation`, `AddImportScope`, and `AddCustomDebugInformation` accept caller-created blob handles. FSharp2 must therefore own small, pinned-spec codecs for sequence-point streams, import definitions, and the value payloads/convention GUIDs for SourceLink, embedded source, async/state-machine and required F# custom debug information. SRM still owns table and portable-PDB container serialization.[^pdb-blob-gap]

Each symbolic method fragment carries debug information by stable method id and symbolic instruction anchor, not token or byte offset. The final PDB pass:

1. canonically interns path-mapped documents with requested SHA-1/SHA-256 checksums;
2. resolves sequence-point anchors after final method branch layout;
3. emits one `MethodDebugInformation` row per final `MethodDef` row, including empty rows where required;
4. emits local variables/constants/scopes, import-scope chains, state-machine kickoff/move-next relations, and required F# custom debug records;
5. attaches embedded source to its document and SourceLink JSON to the module using the standardized custom-debug GUIDs;
6. constructs `PortablePdbBuilder` with the final PE metadata row counts; and
7. serializes with a content-hash `BlobContentId` provider.

The portable-PDB specification defines the document checksum GUIDs, one-to-one `MethodDebugInformation`/MethodDef indexing, sequence-point and import blobs, local/import/state-machine tables, embedded-source record, and module-level SourceLink record.[^portable-pdb-spec] The official .NET 10 F# compiler uses the public `MetadataBuilder` and `PortablePdbBuilder`, path-maps document and CodeView paths, emits SourceLink as module custom debug information, and calculates PDB content id/checksum from the requested SHA-1 or SHA-256 algorithm.[^fsharp-pdb]

For an external portable PDB:

- have the deterministic id provider hash the serialized PDB blobs while the 20-byte PDB-id slot is still zero, retain that digest, and derive the `BlobContentId` from it;
- let `PortablePdbBuilder.Serialize` patch that content id into the PDB and return it;
- use the retained pre-id digest for the PDB-checksum debug entry; hashing the already-patched PDB would introduce a self-reference and would not match the oracle pipeline;
- add CodeView, PDB-checksum, and—when deterministic—reproducible entries through `DebugDirectoryBuilder`.

For an embedded portable PDB, add the same serialized PDB through `AddEmbeddedPortablePdbEntry`; the public implementation writes the standard header and deterministic deflate payload.[^debug-directory]

The CodeView path is path-mapped and root-independent. PDB ids, hashes, raw method handles, and IL offsets need not match the Compatibility Oracle, but FSharp2's own identical deterministic repeats must be byte-identical and all debugger-visible semantics must pass the differential harness.

## Managed, native, and custom resources

### Managed and F# metadata resources

`ManagedPEBuilder` accepts a caller-built managed-resource blob, while `MetadataBuilder.AddManifestResource` records logical name, visibility, implementation, and offset. SRM provides no public managed-resource stream builder.[^managed-pe] FSharp2 therefore owns a small deterministic builder for the standard length-prefixed stream:

- embedded resource offsets point to the standard length-prefixed payload;
- linked resources use the correct `AssemblyFile`/implementation handle and file hash;
- logical order, names, visibility, and duplicate behavior follow the Compatibility Oracle;
- user-supplied payload bytes remain exact; and
- generated `FSharpSignatureData*` and `FSharpOptimizationData*` payloads enter through the same explicit resource plan, with their presence/compression modes owned by the F# metadata serializer.

Do not use `ResourceManager`, load the emitted assembly, or discover resources by reflection in the compiler host.

### Native Win32 resources

`ManagedPEBuilder` accepts a public but abstract `ResourceSectionBuilder`; it is only an extension seam, whose protected override receives the final section location so an implementation can serialize correct RVAs.[^native-resource-builder] SRM does not supply a concrete native-resource builder, `.res` parser, or icon/manifest merger. FSharp2 therefore owns a narrow, deterministic native-resource model and `ResourceSectionBuilder` subclass that:

- parses the accepted Win32 `.res` form without loading target code;
- converts icon, manifest, version, and raw Win32-resource inputs into one resource tree;
- applies the ordering, alignment, code-page, language, name/id, and duplicate rules observed from the Compatibility Oracle; and
- serializes the final tree only after its section location is known.

This is a bounded format adapter, not a reason to write a complete PE serializer. IcedTasks itself has no compiler resource or signing arguments, so harness-owned positive and negative resource probes are mandatory; the corpus alone cannot close this requirement.

### Portable-PDB custom records

SourceLink, embedded source, async stepping data, and other language-specific debugger payloads are portable-PDB `CustomDebugInformation`, not PE managed resources. Imports and state-machine associations use their dedicated `ImportScope` and `StateMachineMethod` tables, with caller-encoded import/custom-debug blobs where the specification requires them. Keep the custom-record GUID/schema registry in the FSharp2 PDB module, sourced from the pinned portable-PDB specification rather than Roslyn internal constants.

## Deterministic PE identity and strong-name signing

### PE and PDB content ids

`PEBuilder` uses the supplied id provider over serialized blobs and patches the COFF timestamp from the resulting `BlobContentId`; absence of a provider selects a time/GUID provider. `PortablePdbBuilder` similarly requires a deterministic provider for deterministic PDB bytes. `BlobContentId.FromHash` accepts a hash of at least 20 bytes and derives the content GUID/stamp.[^content-id]

For `Deterministic=true`, use incremental SHA-256 for the PE id and the requested PDB checksum algorithm (SHA-256 by default, with SHA-1 only when the compiler contract explicitly requests it), in this order:

1. serialize the PDB through an id provider that retains the digest computed with the PDB-id slot zero, then retain the returned content id and that digest as the checksum;
2. build CodeView, PDB-checksum, and reproducible debug entries;
3. serialize the PE while its reserved MVID and any strong-name slot are zero;
4. patch the reserved MVID with the returned PE content-id GUID; and
5. apply the full strong-name signature last, which also patches the PE checksum.

Hash incrementally over the provided blobs; do not concatenate the whole artifact merely to hash it. PE and PDB ids are separate domains and are not assumed equal. For nondeterministic mode, follow the requested oracle behavior rather than accidentally claiming deterministic evidence.

### Signing modes

Model signing as four explicit modes:

| Mode | Assembly public key | Reserved signature | `CorFlags.StrongNameSigned` | Final action |
| --- | --- | --- | --- | --- |
| Unsigned | No | 0 bytes | Off | Serialize only |
| Delay sign | Yes | Key modulus size, zero-filled | Off | Serialize only |
| Public sign | Yes | Key modulus size, zero-filled | On | Serialize only |
| Full sign | Yes | Key modulus size | On | Call public `ManagedPEBuilder.Sign` |

The official F# source distinguishes public/delay/full signing this way and uses SHA-1 plus RSA PKCS#1 v1.5 with reversed signature bytes for a full CLR strong name.[^fsharp-signing] This is behavioral/algorithm evidence only; ADR 0001 forbids copying its implementation.

The FSharp2 signer:

- parses only declared key formats into a new FSharp2 key model;
- derives and validates the CLR public-key blob and signature size before PE construction;
- passes `strongNameSignatureSize` explicitly—`0` for unsigned output and the validated key modulus size for delay/public/full signing—rather than accepting `ManagedPEBuilder`'s 128-byte default;
- sets assembly public-key metadata and `CorFlags.StrongNameSigned` according to the selected mode before serialization, because `ManagedPEBuilder.Sign` does neither;
- passes a pure signature callback to public `ManagedPEBuilder.Sign`;
- uses the strong-name-required SHA-1 only for the strong-name protocol, never as the content-addressed cache hash;
- clears private-key buffers where the platform permits and never persists them in compiler state; and
- differentially tests key file, delay-sign, public-sign, invalid-key, and identity behavior.

`ManagedPEBuilder.Sign` owns the exact hash exclusions, signature-slot patch, and final PE checksum. FSharp2's callback owns key use, RSA PKCS#1/SHA-1 policy, and the CLR-required signature byte order. The underlying content-selection helper is internal and must not be called or reflected over.[^pe-sign]

Key-container compatibility is not silently solved here. A platform key-container implementation cannot become a dynamic plugin or a managed fallback in the NativeAOT host. A later compatibility decision may add a statically linked platform adapter or an explicit out-of-process signing step, but key-file signing is the first portable implementation.

## Public API boundary

The production adapter may depend on these public .NET 10 surfaces:

| Public surface | Use |
| --- | --- |
| `MetadataBuilder`, `MetadataRootBuilder`, `MetadataTokens`, blob/signature encoders | ECMA-335 tables/heaps/signatures and planned handles |
| `InstructionEncoder`, `ControlFlowBuilder`, `MethodBodyStreamEncoder` | Final method-local IL, branches, exception regions, and body offsets |
| `PortablePdbBuilder` plus debug-table `MetadataBuilder` methods | Portable-PDB rows/container; caller supplies sequence-point/import/custom-debug blobs |
| `PEHeaderBuilder`, `ManagedPEBuilder`, `DebugDirectoryBuilder`, abstract `ResourceSectionBuilder` | PE layout, debug directory, caller-built managed/native resource sections, signing slot |
| `BlobBuilder`, `BlobContentId`, `IncrementalHash`, `RSA` | Segmented serialization, deterministic ids, checksums, strong-name callback |

This is a deliberate split: public SRM owns final ECMA-335 table, IL, portable-PDB container, and PE serialization; FSharp2 owns the specification-defined blob/layout gaps. Those owned codecs are portable-PDB sequence points/import definitions/custom-debug payloads, the length-prefixed managed-resource stream, the native resource tree/section subclass, key parsing, and strong-name policy. None may reach runtime-internal encoders.

The adapter must not depend on:

- runtime-internal SRM helpers such as branch-copy/fixup, metadata-table writers, PE content-to-sign selection, or validation internals;
- Roslyn/F# compiler internal PE writers, strong-name providers, debug GUID constants, or metadata models;
- private reflection into any builder;
- `System.Reflection.Emit`, `DynamicMethod`, runtime expression compilation, `Assembly.Load*`, or target `System.Type`; or
- a third-party emitter's mutable model as the persistent FSharp2 fragment schema.

The inspected public builder constructors and methods have no dynamic-code contract and operate on values, handles, and bytes. That makes the design compatible with the closed-world NativeAOT architecture, but only strict warning-free publish and execution of the complete host closure can prove the implementation on each required RID.

## Alternatives considered and rejected

### Copy or wrap the official F# PE writer

Rejected. The official writer is an internal custom table/PE serializer. It records string/data/branch fixups, assigns metadata rows, patches deterministic MVID/timestamp, writes resources/signature slots, and separately uses SRM for portable PDBs.[^fsharp-writer] That is valuable behavioral evidence that a symbolic IL layer must precede token/layout fixups. It is neither a public stable API nor independently authored FSharp2 code. Copying it violates ADR 0001; referencing FCS violates the production closure and ADR 0010.

### Build a new complete PE/metadata serializer now

Rejected. ECMA-335 table encoding, heap-width transitions, sorted-table validation, method/RVA layout, PE directories, PDB association, signing exclusions, and checksums are already covered by public runtime-owned builders. A new full serializer multiplies correctness and servicing risk without measured FSharp2 evidence. Keep the FSharp2-owned layer at symbolic planning/resource/signing gaps. Revisit a custom final serializer only if a representative NativeAOT benchmark identifies an SRM bottleneck or a required format feature that cannot be expressed through its public surface.

### AsmResolver 6.0 as the production backend

Rejected as the default, but retained as the strongest benchmark challenger. Its official v6.0 source targets modern frameworks, declares `IsTrimmable=true`, exposes a comprehensive managed PE model, assigns new tokens through a mapping, and can patch or reassemble method bodies; its release notes explicitly claim improved AOT trimming and performance work.[^asmresolver]

Those are credible capabilities, not FSharp2 release evidence. The project does not declare `IsAotCompatible`, and `IsTrimmable` plus release notes do not prove the six-RID strict NativeAOT host gate, exact IcedTasks PDB/F# metadata behavior, or lower final-link cost. Its own PDB guide calls support work in progress, describes reading and only sometimes writing debug data, and warns that APIs can change.[^asmresolver-pdb] Adopting its whole mutable module/file model would add a broad production dependency and tempt incremental state to couple to library objects/tokens. The symbolic FSharp2 boundary makes an isolated AsmResolver adapter possible for a future apples-to-apples benchmark without making it the architecture.

### Mono.Cecil 0.11.6 or dnlib 4.5

Rejected for production. Both are credible assembly rewriting ecosystems with IL, resources, signing, and portable-PDB support, but their pinned project files expose broad whole-module libraries without `IsAotCompatible` or strict NativeAOT evidence. Cecil 0.11.6 still targets `netstandard2.0;net40` and contains portable-PDB, signing, and deterministic-MVID machinery, but it makes no declared whole-PE determinism or NativeAOT compatibility claim; dnlib additionally references `System.Reflection.Emit` packages for its netstandard target.[^cecil] [^dnlib] Their strengths are round-trip rewriting and inspection, not a minimal closed-world compiler emitter with a FSharp2-owned fragment schema. They may be test-only comparison tools only if the harness dependency boundary permits them.

### Reflection.Emit or PersistedAssemblyBuilder

Rejected. NativeAOT explicitly does not support runtime code generation such as `System.Reflection.Emit`, and the repository already forbids a `System.Type`/runtime-loading target universe.[^nativeaot-limitations] It also does not provide the required explicit token/PDB/resource/signing seam for declaration-fragment reuse.

### Patch a template assembly or mutate the previous output

Rejected as the correctness baseline. Metadata tokens, heap widths, branch distances, method offsets, RVAs, PDB method rows/offsets, content ids, signatures, and checksums are coupled. Post-hoc mutation makes completeness and determinism harder to prove. A future relocation-aware reuse optimization may reuse token-neutral byte runs only after the final plan proves every relocation and debug anchor; it remains an optimization behind the same clean-link oracle.

## NativeAOT and trimming consequences

The chosen architecture preserves the issue #4 production boundary:

- target references are read as metadata and represented by FSharp2 identities, never loaded;
- no emitter phase is discovered by reflection or dependency injection scanning;
- all resource, debug-record, signature, and key-format handlers are statically registered;
- no dynamic code or runtime assembly loading is needed;
- the public SRM adapter, FSharp2 resource/signing code, and any benchmark-justified C# kernel are in the strict NativeAOT closure; and
- the official compiler and all alternative emitters remain outside production unless a separately reviewed decision changes the backend.

Acceptance still requires warning-free NativeAOT publish and execution for `win-x64`, `win-arm64`, `linux-x64`, `linux-arm64`, `osx-x64`, and `osx-arm64`, plus NativeAOT consumers of the emitted managed output. Architectural absence of dynamic code is necessary, not sufficient.

## Incremental and throughput consequences

The [comparative fast-compiler study](fast-fsharp-compiler-cold-and-incremental.md) does not select an emitter library. It determines where the reusable boundary sits: stable semantic inputs and explicit dependencies produce FSharp2-owned symbolic fragments, while each final artifact link is a reproducible project action. That keeps the persistent live/disk cache independent of SRM's mutable final-layout objects.

Its findings apply as follows:

- a declaration query produces a content-addressed symbolic emission fragment;
- unchanged fragments remain valid when their semantic/emission dependency fingerprints remain valid;
- an implementation-only edit changes only the affected method/debug fragments and artifact link, while an unchanged exported API fingerprint keeps downstream projects valid;
- an API or inline-body change invalidates the exact semantic consumers required by ADR 0018;
- any equivalence the compiler cannot prove widens invalidation to the containing declaration/file/project and records the reason;
- the final artifact link is a reproducible action over the selected fragment set;
- during Warm Compilation, an exact final-link action-key hit may reuse the whole artifact set; Cold Compilation bypasses both fragment and final-artifact caches, and an MSBuild timestamp skip is neither fragment reuse nor Warm Compilation; and
- structured traces report fragment reuse separately from final-link work.

Minimum emitter trace fields are artifact kind, schema/action key, fragment counts and bytes by reused/rebuilt status, planned table/heap/resource/document sizes, method encode and PDB/PE/resource/sign durations, strong-sign duration excluding secret data, allocation/GC counters, peak working set when available, and any invalidation or layout fallback reason.

The first implementation should prefer ordinary F# and public builders. The following are explicitly benchmark questions:

- pooled versus fresh `BlobBuilder`/temporary collections;
- F# versus a narrow C# span/ref-struct encoder kernel;
- serial versus parallel method/signature encoding;
- eager versus lazy signature/resource interning;
- long-branch baseline versus a branch-shrinking pass;
- symbolic instruction re-encoding versus token-neutral byte runs plus relocation tables;
- full clean final link versus stable-row/tombstone strategies;
- contiguous output versus segmented/temp-file staging; and
- direct SRM versus an isolated AsmResolver adapter.

Measure them in the NativeAOT host on the complete IcedTasks four-TFM producer matrix and on harness probes that stress large metadata, PDBs, resources, and signing. Report Cold and Warm Compiler Target Invocation impact, not a serializer microbenchmark alone. No optimization may change artifact semantics, deterministic repeats, strict publish results, or failure cleanup.

### Minimal local feasibility smoke

A disposable `win-x64` proof under `C:\tmp\fsharp2-issue7-emitter-proof` tested only whether the selected public surface survives NativeAOT and can emit a loadable minimal artifact. A C# host using public `MetadataBuilder`, `BlobEncoder`, `InstructionEncoder`, `MethodBodyStreamEncoder`, `PortablePdbBuilder`, `DebugDirectoryBuilder`, and `ManagedPEBuilder` was published with `PublishAot=true`. Its native executable emitted a DLL containing `FSharp2.Proof.Answer() = 42` and a readable portable PDB twice. The two output pairs were independently rehashed in this research pass:

- DLL SHA-256: `A2C5A2FCE7072E77DBE3226CF3FD2BA2DFA9AB1D58BCC24AFD0C74199C758C30`
- PDB SHA-256: `5095F5B91F09BB4B5B0B104739CFDE2B4F4BA60DD26C992F961BCECA5444D50B`

A separate `net10.0` consumer referencing that DLL also published with NativeAOT and returned `42` with exit code 0. This is minimal local feasibility evidence, not primary-source proof and not the issue gate. The disposable proof has no F# metadata, sequence points, import/custom-debug blobs, resources, signing, exception handling, reference assembly, IcedTasks input, cross-root repeat, or six-RID matrix. It does not replace any row below.

## Required proof before implementation is accepted

| Requirement | Authoritative evidence |
| --- | --- |
| Public-only backend | Production dependency/API audit; no internal/private-reflection call; clean strict NativeAOT publish |
| ECMA-335 metadata/IL | `PEReader`/`MetadataReader` structural read, exact reference closure, ILVerify, canonical metadata comparison, runtime/downstream matrix |
| Target-framework agnosticism | Same host emits the IcedTasks `netstandard2.0`, `netstandard2.1`, `net6.0`, and `net9.0` artifacts from supplied references/options |
| Implementation and reference assemblies | Separate semantic/API validation and bidirectional oracle/FSharp2 consumers without rebuilding producer |
| Portable/embedded PDB | Spec-table and raw blob validation for one-row-per-MethodDef, sequence points, imports, custom-debug records, PDB id/pre-id checksum association, SourceLink/embedded source, and debugger-visible scopes/state machines |
| Determinism | Two clean, cache-isolated, different-root repeats per compiler; byte-identical DLL/ref DLL/PDB/XML/signature/resource artifacts and identical PDB id/checksum, PE id/debug entries, and late-patched MVID |
| Strong name | Explicit zero/key-sized reservation; unsigned/delay/public/full key-file flags; MVID-before-sign ordering; identity/token/signature/checksum verification; invalid-key/failure cleanup; oracle differential behavior |
| Managed/F# metadata resources | Exact user payloads, logical names/visibility/linking, F# resource presence/modes, four-way F# consumer matrix |
| Native resources | Icon/manifest/version/raw `.res` probes on each relevant target/platform shape and structural/native observer validation |
| Incremental fragments | Implementation-only and middle-DAG API edits prove reused/rebuilt fragment identities and clean-link byte equivalence |
| NativeAOT | Six native host RID gates plus strict NativeAOT consumers of FSharp2-emitted output |
| Throughput | Controlled IcedTasks Cold/Warm measurements and phase/allocation traces; no unchanged MSBuild skip counted as Warm Compilation |
| Transactionality | Injected failure at metadata, PDB, resource, signing, validation, and publish stages leaves no successful partial artifact set |

The clean-link path is the correctness oracle for every incremental emission optimization: given the same normalized link request and selected symbolic fragments, incremental and standalone paths must produce byte-identical FSharp2 artifacts.

## Residual questions and refresh triggers

1. **Native resource fidelity.** The public section seam is sufficient, but the exact cross-platform `.res`/icon/manifest/version rules need oracle probes before implementing the adapter.
2. **Strong-name key containers.** Key-file modes have a portable design; key-container behavior needs an explicit NativeAOT/platform decision.
3. **F# custom debug information.** The portable-PDB core is fixed; exact F#-specific records and suppression modes expand with the compatibility envelope and oracle corpus.
4. **Metadata ordering observability.** The default planner preserves declaration/source order plus spec-required sorting. Any token-preservation/tombstone policy needs reflection/consumer evidence and benchmarks.
5. **Servicing drift.** Reinspect public API annotations/source and rerun strict publish on every .NET 10 servicing update.
6. **Performance.** Direct SRM is selected as the correctness backend, not declared the permanent fastest backend. The adapter seam and benchmark matrix deliberately leave that claim falsifiable.

## Decision for the next prototype

[Issue #8][issue-8] should implement the smallest end-to-end trace through this exact seam:

- one FSharp2-owned symbolic module/type/method model with stable ids;
- deterministic row planning and public SRM metadata/IL emission;
- portable PDB with FSharp2-owned spec codecs for sequence points/imports/custom-debug payloads, plus path-mapped documents, SourceLink, CodeView/checksum/reproducible entries;
- deterministic PDB first, debug directory second, zero-reserved-MVID PE serialization third, MVID patch fourth, and strong-name signing last;
- one managed resource and one harness-native-resource path;
- all four signing modes at adapter-test level;
- structural/ILVerify/runtime/deterministic-repeat checks; and
- strict NativeAOT host publish plus NativeAOT consumption of the emitted assembly.

The prototype should not add AsmResolver, Cecil, dnlib, FCS, Roslyn compiler internals, or a custom full PE writer to production. Unsupported rows/options fail explicitly and transactionally until later vertical slices expand the symbolic model and public SRM adapter.

## Primary sources and repo evidence

- [ECMA-335 sixth edition][ecma-335]
- [Pinned public metadata table builder][metadata-tables], [heap builder][metadata-heaps], [metadata-root/RVA serialization][metadata-root], and [metadata-token factories][metadata-tokens]
- [Pinned public IL instruction encoder][instruction-encoder], [branch/EH builder][control-flow], and [method-body encoder][method-body-encoder]
- [Pinned public managed PE][managed-pe-builder], [PE deterministic/signing implementation][pe-builder], [native resource extension seam][resource-section], and [content-id implementation][blob-content-id]
- [Pinned public portable-PDB builder][portable-pdb-builder], [Portable PDB specification][portable-pdb-spec], and [debug-directory builders][debug-directory-builder] [embedded-pdb-builder]
- [Pinned official F# abstract-IL/custom writer][fsharp-il-writer], [portable-PDB writer][fsharp-pdb-writer], [IL generation boundary][fsharp-ilxgen], and [strong-name behavior][fsharp-sign-writer] — behavioral evidence only
- [Official NativeAOT limitations][nativeaot-overview]
- [AsmResolver 6.0 release/project/builder evidence][asmresolver-release] [asmresolver-project] [asmresolver-builder] [asmresolver-token-map] [asmresolver-body], [advanced builder guide][asmresolver-advanced], and [PDB status][asmresolver-pdb]
- [Mono.Cecil 0.11.6 project/writer][cecil-project] [cecil-writer] and [dnlib 4.5 project][dnlib-project]
- [FSharp2 context](../../../CONTEXT.md)
- [NativeAOT compiler-host constraints](dotnet-10-nativeaot-compiler-host-constraints.md)
- [Compiler/MSBuild artifact contract](dotnet-10-fsharp-compiler-msbuild-contract.md)
- [Differential compatibility/performance harness](differential-compatibility-performance-harness.md)
- [Complete IcedTasks milestone](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md)
- [Public SRM behind symbolic emission fragments](../adr/0021-use-public-srm-behind-symbolic-emission-fragments.md)
- [Comparative fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md)

[^metadata-builder]: The public [metadata row methods][metadata-tables] return append-order handles; see `AddMethodDefinition` and `AddTypeDefinition`.
[^instruction-token]: [`InstructionEncoder.Token` and `LoadString`][instruction-encoder] write final numeric tokens immediately.
[^branch-fixup]: [`ControlFlowBuilder` records labels/branches and patches distances][control-flow]; [`MethodBodyStreamEncoder.AddMethodBody` invokes it][method-body-encoder]. The source explicitly declines automatic short-to-long promotion.
[^rva-fixup]: [`AddMethodDefinition` documents an IL-stream-relative body offset][metadata-tables], and [`MetadataRootBuilder.Serialize` adds method/mapped-field stream RVAs][metadata-root].
[^portable-pdb-builder]: [`PortablePdbBuilder` requires final type-system row counts and an id provider][portable-pdb-builder].
[^portable-pdb-spec]: The [Portable PDB specification][portable-pdb-spec] defines its tables, MethodDef indexing, sequence points, checksums, SourceLink, and embedded source.
[^pdb-blob-gap]: The public debug-row methods in [`MetadataBuilder`][metadata-tables] accept caller-supplied blob handles; the public .NET 10 surface contains no `SequencePointsEncoder` or import-definition encoder. The [Portable PDB specification][portable-pdb-spec] defines those blobs and the custom-debug formats/GUIDs that FSharp2 must encode.
[^metadata-order]: [`MetadataRootBuilder.Serialize` validates required table order unless explicitly suppressed][metadata-root]; FSharp2 keeps validation enabled.
[^reserved-guid]: [`MetadataBuilder.ReserveGuid` returns a handle and writable reserved blob][metadata-heaps]; [`PEBuilder.Serialize` returns the computed content id][pe-builder]. The pinned official F# [writer reserves the MVID, serializes deterministically, patches it from the PE id, and signs afterward][fsharp-il-writer].
[^fsharp-pdb]: The pinned official F# [portable-PDB generator][fsharp-pdb-writer] uses public builders, path mapping, document hashes, and SourceLink. Its id provider retains the digest computed before `PortablePdbBuilder.Serialize` patches the PDB-id slot; that same digest supplies the debug-directory PDB checksum.
[^debug-directory]: The public builder exposes [CodeView, PDB checksum, and reproducible entries][debug-directory-builder] and [standard embedded portable-PDB compression][embedded-pdb-builder].
[^managed-pe]: The public [`ManagedPEBuilder` constructor][managed-pe-builder] accepts a caller-built managed-resource blob, IL, mapped fields, native resources, debug directory, signing size/flags, and deterministic id provider; [manifest-resource rows][metadata-tables] carry logical metadata and offsets. No public API builds the length-prefixed managed-resource stream.
[^native-resource-builder]: [`ResourceSectionBuilder`][resource-section] is an abstract public extension point called with final section location, not a concrete native-resource compiler.
[^content-id]: [`PEBuilder.Serialize` applies its id provider][pe-builder], [`PortablePdbBuilder` requires a deterministic provider for deterministic output][portable-pdb-builder], and [`BlobContentId.FromHash`][blob-content-id] derives the id from at least 20 hash bytes.
[^fsharp-signing]: The official F# [strong-name source][fsharp-sign-writer] records public/delay/full mode, SHA-1/RSA-PKCS1 signing, reversed signature bytes, and the signed flag.
[^pe-sign]: Public [`ManagedPEBuilder.Sign`][managed-pe-builder] delegates to PE signing; the pinned [PEBuilder implementation][pe-builder] excludes the signature slot, writes the returned signature, and calculates the PE checksum. It does not create assembly public-key metadata or select `CorFlags.StrongNameSigned`; its constructor's signature reservation default is 128 bytes, so FSharp2 passes every mode explicitly.
[^fsharp-writer]: The official F# [internal IL/PE writer][fsharp-il-writer] has explicit row caches and string/data/branch fixups, then patches deterministic identity/layout. Its [IL generator][fsharp-ilxgen] produces an abstract IL boundary before that writer.
[^asmresolver]: [AsmResolver 6.0 release notes][asmresolver-release], inherited [target/trimming properties][asmresolver-project], [PE image builder][asmresolver-builder], [token mapping][asmresolver-token-map], and [method-body patch/reassembly][asmresolver-body].
[^asmresolver-pdb]: AsmResolver's [advanced image-building guide][asmresolver-advanced] demonstrates its broad PE seam; its own [PDB guide][asmresolver-pdb] describes support as work in progress, with reading and sometimes writing and unstable APIs.
[^cecil]: [Mono.Cecil 0.11.6 project targets and source inventory][cecil-project] plus its [assembly writer][cecil-writer].
[^dnlib]: [dnlib 4.5 project targets, package description, and Reflection.Emit references][dnlib-project].
[^nativeaot-limitations]: The official [.NET NativeAOT limitations][nativeaot-overview] exclude dynamic loading and runtime code generation such as Reflection.Emit.

[issue-1]: https://github.com/TheAngryByrd/fsharp2/issues/1
[issue-7]: https://github.com/TheAngryByrd/fsharp2/issues/7
[issue-8]: https://github.com/TheAngryByrd/fsharp2/issues/8
[ecma-335]: https://ecma-international.org/wp-content/uploads/ECMA-335_6th_edition_june_2012.pdf
[metadata-tables]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/MetadataBuilder.Tables.cs
[metadata-heaps]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/MetadataBuilder.Heaps.cs
[metadata-root]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/MetadataRootBuilder.cs
[metadata-tokens]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/MetadataTokens.cs
[instruction-encoder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/Encoding/InstructionEncoder.cs
[control-flow]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/Encoding/ControlFlowBuilder.cs
[method-body-encoder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/Encoding/MethodBodyStreamEncoder.cs
[managed-pe-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/ManagedPEBuilder.cs
[pe-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/PEBuilder.cs
[resource-section]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/ResourceSectionBuilder.cs
[blob-content-id]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/BlobContentId.cs
[portable-pdb-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/Ecma335/PortablePdbBuilder.cs
[portable-pdb-spec]: https://github.com/dotnet/runtime/blob/v10.0.10/docs/design/specs/PortablePdb-Metadata.md
[debug-directory-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/DebugDirectory/DebugDirectoryBuilder.cs
[embedded-pdb-builder]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/DebugDirectory/DebugDirectoryBuilder.EmbeddedPortablePdb.cs
[fsharp-il-writer]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/Compiler/AbstractIL/ilwrite.fs
[fsharp-pdb-writer]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/Compiler/AbstractIL/ilwritepdb.fs
[fsharp-ilxgen]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/Compiler/CodeGen/IlxGen.fs
[fsharp-sign-writer]: https://github.com/dotnet/dotnet/blob/b0f34d51fccc69fd334253924abd8d6853fad7aa/src/fsharp/src/Compiler/AbstractIL/ilsign.fs
[nativeaot-overview]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md#L166-L183
[asmresolver-release]: https://github.com/Washi1337/AsmResolver/releases/tag/v6.0.0
[asmresolver-project]: https://github.com/Washi1337/AsmResolver/blob/v6.0.0/src/Directory.Build.props
[asmresolver-builder]: https://github.com/Washi1337/AsmResolver/blob/v6.0.0/src/AsmResolver.DotNet/Builder/ManagedPEImageBuilder.cs
[asmresolver-token-map]: https://github.com/Washi1337/AsmResolver/blob/v6.0.0/src/AsmResolver.DotNet/Builder/TokenMapping.cs
[asmresolver-body]: https://github.com/Washi1337/AsmResolver/blob/v6.0.0/src/AsmResolver.DotNet/Code/Cil/CilMethodBodySerializer.cs
[asmresolver-advanced]: https://docs.washi.dev/asmresolver/guides/dotnet/advanced-pe-image-building.html
[asmresolver-pdb]: https://docs.washi.dev/asmresolver/guides/pdb/index.html
[cecil-project]: https://github.com/jbevain/cecil/blob/0.11.6/Mono.Cecil.csproj
[cecil-writer]: https://github.com/jbevain/cecil/blob/0.11.6/Mono.Cecil/AssemblyWriter.cs
[dnlib-project]: https://github.com/0xd4d/dnlib/blob/v4.5.0/src/dnlib.csproj
