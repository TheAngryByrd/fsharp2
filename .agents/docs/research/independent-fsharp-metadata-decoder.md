# Independent F# metadata decoder boundary

Status: decision for [Decide whether the harness needs an independent F# metadata decoder][issue-17]

Observed: 2026-07-19 against the .NET 10 source snapshot already pinned by the differential-harness research

Governing decisions: [ADR 0001](../adr/0001-independent-source-informed-compiler.md), [ADR 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [ADR 0004](../adr/0004-target-compiler-and-msbuild-compatibility.md), [ADR 0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [ADR 0006](../adr/0006-require-behavioral-not-byte-identical-output.md), [ADR 0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), [ADR 0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), and [ADR 0018](../adr/0018-track-incremental-dependencies-at-declaration-granularity.md)

Related evidence: [differential compatibility and performance harness](differential-compatibility-performance-harness.md), [warm-edit/cache-evidence contract](warm-edit-corpus-and-cache-evidence-contract.md), and [fast F# compiler cold and incremental research](fast-fsharp-compiler-cold-and-incremental.md)

## Decision

Do **not** make a harness-owned, full semantic decoder of the official compiler's private F# signature and optimization pickles a Compatibility Gate oracle.

Do add a small, independently authored, test-only **F# metadata envelope inspector**. It should use public `System.Reflection.Metadata` and `System.IO.Compression` APIs to inventory, extract, bound, inflate, pair, and hash the F# resources without loading the target assembly. Its exact observations are authoritative only for the envelope properties already required by the harness: resource family, logical name and CCU suffix, visibility/storage, compression mode, A/B pairing, format-version attribute, payload bounds, and successful Deflate decoding.

The existing four-way producer/consumer matrix remains the semantic authority:

| Producer | Consumer | What it proves |
| --- | --- | --- |
| Compatibility Oracle | Compatibility Oracle | The case and observer are valid for the pinned SDK. |
| Compatibility Oracle | FSharp2 | FSharp2's production importer understands the Oracle artifact. |
| FSharp2 | Compatibility Oracle | The independent emitter produced metadata accepted by the shipped compiler. |
| FSharp2 | FSharp2 | FSharp2's producer and importer are internally consistent. |

The standard C# consumer, managed API comparison, runtime observers, and inline F# consumer remain part of the surrounding gate. A decoder disagreement cannot overrule successful bidirectional compilation and observation, and a decoder agreement cannot rescue a consumer failure. The latest compiler shipped with the pinned .NET 10 SDK remains the Compatibility Oracle.

The inspector never short-circuits a compiler or consumer lane. Malformed, missing, or incompatible metadata cases still run through the Compatibility Oracle and FSharp2 so the harness can compare the complete `FSxxxx` code, severity, message, range, ordering, stream, suppression/promotion, exit, and artifact behavior required by FS Diagnostic Compatibility. An envelope finding supplements that evidence; it does not replace it or narrow the family-wide final gate.

This is a deliberate split:

- **Yes** to an independent envelope inspector and optional semantic-fingerprint diagnostics.
- **No** to a second implementation of the whole private pickle graph as a pass/fail oracle now.
- **Yes** to a production FSharp2 importer, because the compiler must consume Oracle and FSharp2 references. That importer is production code, independently authored under ADR 0001 and NativeAOT-compatible under ADR 0002; it is not the harness's independent judge.

## Why a full decoder should not be authoritative

### The payload is compiler-private graph serialization, not an interchange schema

The official implementation labels `TypedTreePickle` as an internal module. It serializes shared and interned graph nodes, dangling compilation-unit references, lazy values, F# types and entities, and typed expressions. Signature data ends in a pickled `PickledCcuInfo`; optimization data serializes the optimizer's `LazyModuleInfo` graph.[^private-pickle] The optimization graph includes value references and typed expression bodies, so it is not equivalent to ordinary ECMA-335 public metadata.[^optimization-graph]

The encoding also carries compatibility machinery that a faithful clone would have to reproduce:

- compact integers use a custom implementation modeled on CLI compressed integers;
- a second `B` stream is optional, and absent `B` bytes are interpreted as zero or empty;
- reserved bytes in the main stream can later acquire meanings; and
- graph tables and fixups are reconstructed while unpickling.[^pickle-evolution]

The assembly-level revision in the pinned source is the coarse value `2.0.0.0`. The emitter stamps that revision with the signature-data version attribute, and the importer requires a matching attribute before using F# signature data.[^format-version] The optional `B` stream and reserved-space behavior show that evolution can occur inside that coarse revision. A full harness decoder would therefore track private compiler history, graph invariants, and compatibility conventions rather than a small, independently specified wire contract.

No normative, first-party specification for the complete signature/optimization pickle was identified in the sources examined for this decision. The first-party implementation found was the same official compiler writer/reader; `System.Reflection.Metadata` exposes the surrounding CLI resource metadata, not F# typed-tree semantics. This is negative search evidence, not proof that no other first-party implementation exists.

### A clone would add correlated confidence, not an independent oracle

A decoder derived from the same official source can be useful for localization, but it is not independent semantic authority in the same sense as an Oracle compiler successfully consuming FSharp2 output. It can:

- repeat the same mistaken interpretation as FSharp2's importer;
- reject an alternative but valid representation that the Oracle accepts;
- lag a servicing change while both real compilers interoperate; or
- report equal normalized graphs while hiding an executable consumer difference.

ADR 0006 explicitly permits different valid representation and optimization choices. Behavioral interoperability is therefore the stronger gate. The decoder's value is explaining *where* a failed interchange differs, not deciding whether accepted interchange is compatible.

## Exact resource and option behavior to inspect

The official source defines separate names for uncompressed, compressed, and `B` resources. The ordinary prefixes are:

| Kind | Uncompressed A | Uncompressed B | Compressed A | Compressed B |
| --- | --- | --- | --- | --- |
| Signature | `FSharpSignatureData.` | `FSharpSignatureDataB.` | `FSharpSignatureCompressedData.` | `FSharpSignatureCompressedDataB.` |
| Optimization | `FSharpOptimizationData.` | `FSharpOptimizationDataB.` | `FSharpOptimizationCompressedData.` | `FSharpOptimizationCompressedDataB.` |

The suffix is the compilation-unit/assembly name. Uncompressed FSharp.Core A resources have the historical prefixes `FSharpSignatureInfo.` and `FSharpOptimizationInfo.` so older compilers do not mistake them for the ordinary form.[^resource-names]

The writer makes resources public. It Deflate-compresses each stream independently when compression is enabled and omits the `B` resource when that stream is empty. The reader recognizes both A and B prefixes, pairs them by suffix, inflates compressed resources with `DeflateStream`, and supplies an empty B stream when none exists.[^resource-read-write]

The option cases must be modeled separately:

| Invocation mode | Oracle behavior in the pinned source | Required harness evidence |
| --- | --- | --- |
| Default / `--compressmetadata+` | Compression defaults on; signature and optimization A resources use compressed prefixes, with B resources only when non-empty. | Exact inventory/version/visibility, bounded Deflate success, A/B suffix pairing, and all applicable consumers. |
| `--compressmetadata-` | The same logical data is emitted under uncompressed prefixes. | Exact inventory and direct payload extraction; consumers must agree with the compressed case's semantics. |
| `--nooptimizationdata` | It does **not** remove optimization metadata. It sets `onlyEssentialOptimizationData`, and the writer reduces the graph to information essential for implementing inlined constructs before still emitting the resource. | An inline consumer is mandatory; resource presence alone cannot prove the essential subset is correct. |
| `--nointerfacedata` | It sets `noSignatureData`; `GenerateSignatureData` becomes false, and `GenerateOptimizationData` is tied to the same value, so both F# resource families and the signature-version attribute are omitted. The official importer then uses its IL view rather than F# signature data. | Exact absence plus Oracle/FSharp2 and FSharp2/Oracle consumers for a case whose expected behavior is pinned. Do not silently treat the IL view as equivalent to full F# metadata. |

These option semantics come from the option definitions and generation predicates, not from the option names alone.[^metadata-options] In particular, interpreting `--nooptimizationdata` as “no optimization resource” would encode a false gate.

Signature and optimization observations must remain separate. Signature data carries the F# view needed for types, modules, values, constraints, and other F#-specific surface. Optimization data carries consumer-visible implementation payloads such as inline bodies. A non-inline consumer can validate signature interchange while never exercising the optimization payload; the harness therefore needs an explicitly inline consumer for every mode in which essential or full optimization data is expected.

## Envelope inspector contract

The inspector belongs only in the harness/tool graph. It must not be referenced by the NativeAOT compiler host, emitter, production importer, or shipped MSBuild package.

Public SRM supplies the required outer boundary: `MetadataReader.ManifestResources` enumerates manifest rows; `ManifestResource` exposes name, attributes, implementation, and offset; and `PEReader`/`BlobReader` expose the PE section bytes and bounded primitive reads.[^srm-resources] The inspector should not call `Assembly.Load`, instantiate target types, or reference FCS/the official compiler.

For each assembly, emit this minimal canonical **envelope model**:

```text
assembly identity
signature-data version attribute: absent | version tuple | malformed
resource[]:
  semantic family: signature | optimization | unknown-fsharp
  stream: A | B | unknown
  encoding: deflate | raw | unknown
  logical CCU suffix
  manifest name, visibility, embedded/linked implementation
  raw length and SHA-256
  decoded status, bounded decoded length and SHA-256
  pair status: matched | missing-optional-B | duplicate | conflicting
```

Canonical ordering is `(family, CCU suffix, stream, manifest name)`, not metadata row number. Raw row/token/offset values remain audit evidence. Bounds are versioned harness policy inputs; an oversized or malformed Deflate stream must not allocate without limit.

The inspector may additionally record a same-compiler raw/decoded hash for deterministic repeats. Cross-compiler hash inequality is diagnostic only because ADR 0006 does not require byte-identical payloads.

### Authority and verdict boundary

| Observation | Authority | Verdict effect |
| --- | --- | --- |
| Expected resource name/family/suffix/visibility/presence for a pinned mode | Envelope inspector plus case manifest | Mismatch is `fail`. |
| Known compressed resource cannot be boundedly inflated, has duplicate/conflicting pairing, or violates the pinned version attribute | Envelope inspector | `fail`; this is malformed required structure, not a semantic opinion. |
| Within-compiler deterministic raw payload mismatch | Existing deterministic-artifact comparator | `fail`. |
| Cross-compiler raw or decoded payload hash mismatch | Audit only | No verdict effect by itself. |
| Optional decoder cannot understand a known/private payload node | Diagnostic status `decoder-error` or `unsupported-format` | No Compatibility Gate effect by itself. It must not turn a passing consumer matrix into `fail` or `infra-error`. |
| Optional semantic fingerprint differs while every required consumer/observer passes | Diagnostic disagreement | Case remains `pass`; retain both fingerprints and open triage evidence. |
| Any required producer/consumer cell, API observer, runtime observer, or inline-mode observer differs | Compatibility Oracle and behavioral matrix | `fail`, whether or not the decoder agrees. |
| A diagnostic decoder is unavailable after a consumer failure | Consumer failure remains authoritative | Still `fail`, with weaker localization rather than `infra-error`. |

Envelope failures are additional comparator findings, not permission to skip the declared compiler/consumer request. Negative cases retain the exact FS Diagnostic Compatibility and cleanup evidence from both compiler lanes before the harness computes its verdict.

An Oracle refresh that introduces an unknown resource family or version is reviewed like any other comparison-policy change. The inspector records the unknown bytes without interpreting them as the current format. Until the case manifest and exact-envelope policy are reviewed, the refresh lane is incomplete; an optional payload decoder's lack of support is not itself a claim that the new Oracle artifact is incompatible.

## Optional semantic decoder and minimal model

A payload decoder may be added later as a **diagnostic plugin** if real failures show that the envelope plus four-way matrix cannot localize defects cheaply. If built, it remains test-only, independently authored, explicitly versioned, and non-authoritative.

Its minimally useful canonical model is not the official compiler's full object graph:

### Signature summary

- CCU identity and nested namespace/module/entity paths;
- entity kind, accessibility, representation, generic parameters and constraints;
- record fields, union cases, exception shape, abbreviations, interfaces and generated members;
- value/member stable F# identity, logical and compiled names, normalized F# type/arity, accessibility, inline/member flags, constants, and consumer-relevant attributes; and
- references resolved to stable assembly/entity/value keys rather than pickle indexes or compiler stamps.

### Optimization summary

- exported inline-capable value identity and type/arity;
- the normalized inline expression needed by a downstream compiler, including calls, type/value references, constants, data construction, control/decision structure, and static constraints; and
- referenced entities/values keyed by the same stable semantic identities as the signature summary.

Source ranges, object-sharing indexes, pickle table order, stream offsets, unique compiler stamps, resource compression, and incidental graph allocation order are provenance rather than semantic identity. Unknown tags/fields must be retained as raw-span hashes when they can be skipped safely; a positional construct that cannot be skipped makes that decoder version `unsupported-format` rather than guessed-compatible.

That summary is deliberately just large enough to answer “is the failure in exported F# shape, an inline body, a reference identity, or only serialization?” It must not become a requirement that FSharp2 serialize the Oracle's internal typed tree.

## Incremental fingerprints and production separation

The parallel compiler-speed research recommends a versioned, lazily read framework/reference index and two different fingerprints: declaration-level exported semantic fingerprints and a project public/API fingerprint that includes consumer-visible implementation payloads such as inline bodies.[^incremental-research] That recommendation applies here, but it does not justify making the harness decoder an oracle.

Keep three schemas and responsibilities distinct:

1. **Production reference index.** FSharp2's NativeAOT importer owns lazy reading of referenced assemblies. Index validity includes assembly content identity, resolved reference-set fingerprint, target/options, F# metadata format observation, and FSharp2 reader/index schema version. It eagerly validates the envelope, then loads only the tables or F# payload portions demanded by name/type resolution where the private format permits; a positional dependency that cannot be skipped is decoded conservatively rather than guessed.
2. **Production semantic/API fingerprint.** FSharp2 computes this from its own checked semantic model before serialization and from its independently imported model for references. It includes public F# shape and consumer-visible bodies/constraints. Unknown impact widens invalidation; it is never inferred solely from equality of opaque resource bytes.
3. **Harness diagnostic fingerprint.** A future decoder may independently summarize Oracle and FSharp2 payloads to localize a failed matrix cell or cross-check cache evidence. It is versioned separately and cannot make a production cache entry valid or a compatibility case pass.

This preserves the useful pattern seen in first-party compiler/build designs: action and query caches are keyed by explicit inputs and schema versions, while downstream reuse follows a stable interface fingerprint rather than a timestamp.[^cache-primary-sources] It also avoids coupling production correctness to a test tool derived from the Oracle's private serializer.

The production importer is unavoidable functionality, not fallback. It may study official source as behavioral/format evidence under ADR 0001, but it must contain newly authored code and may not load or invoke FCS/fsc. Four-way interoperability is what independently tests that implementation.

## Revisit triggers

Reopen the full diagnostic-decoder decision only when at least one concrete trigger occurs:

1. repeated four-way failures remain expensive to localize after envelope, diagnostic, API, IL, and minimized-consumer evidence is collected;
2. a production semantic/API fingerprint is about to authorize cross-project reuse and the existing controlled-edit/clean-rebuild evidence cannot distinguish signature versus inline-body mistakes;
3. the .NET 10 Oracle changes the revision, resource families, B-stream conventions, or pickle behavior during a reviewed refresh;
4. a normative F# metadata format or genuinely independent first-party decoder becomes available; or
5. the corpus gains F# constructs whose resource semantics cannot be isolated by focused signature-only and inline consumers.

Before promoting a decoder beyond diagnostics, require a separate ADR with a version table, independently authored fixtures from both producers, mutation/fuzz tests, unknown-field rules, NativeAOT dependency audit, and evidence that its canonical model predicts real consumer behavior without rejecting Oracle-accepted alternatives. Nothing in this decision permits it to replace the Compatibility Oracle.

## Uncertainties

- The complete private pickle does not have a format specification among the first-party sources examined. Source study can establish current behavior but not a stable forward-compatibility promise.
- The pinned `2.0.0.0` revision is coarser than the observed B-stream/reserved-space evolution. The exact historical compatibility matrix should be measured with real SDK-produced artifacts if a payload decoder is later approved.
- No second independent first-party semantic decoder was identified. This should be rechecked if Microsoft publishes a new metadata/tooling package.
- The minimal semantic summary above is a proposed localization model, not proof that every F# consumer-visible behavior can be canonically represented without executing a consumer. That is why it remains non-authoritative.
- `--nointerfacedata` fallback behavior can expose less F# information than the normal mode. Compatibility expectations must be pinned per corpus case rather than inferred from managed API equality alone.

## Primary sources

- [Pinned F# resource names][resource-names-source]
- [Pinned F# resource reader, Deflate handling, writer, and version attribute][compiler-imports-source]
- [Pinned metadata option definitions][compiler-options-source] and [generation predicates/defaults][compiler-config-source]
- [Pinned private typed-tree pickle implementation][typed-tree-pickle-source]
- [Pinned optimization graph types][optimizer-types-source], [essential-only filter][optimizer-essential-source], and [pickle implementation][optimizer-pickle-source]
- [Pinned F# binary metadata revision][compiler-location-source]
- [`System.Reflection.Metadata` `MetadataReader` source][metadata-reader-source], [`ManifestResource` source][manifest-resource-source], [`PEReader` source][pe-reader-source], and [`BlobReader` source][blob-reader-source]
- [Official NativeAOT deployment contract][nativeaot-source]
- [Official rustc query/incremental model][rustc-query] and [Go build-cache/action-key implementation][go-cache]

[^private-pickle]: The [pinned internal `TypedTreePickle` module][typed-tree-pickle-source] defines the reader/writer graph machinery and signature root; the [signature writer/reader][compiler-imports-source] calls it for `PickledCcuInfo`.
[^optimization-graph]: The pinned [optimizer graph types][optimizer-types-source] define `LazyModuleInfo`, value references, and typed expression payloads; the [essential-only filter][optimizer-essential-source] retains inline values for `--nooptimizationdata`; and the [pickle functions][optimizer-pickle-source] serialize and deserialize that graph.
[^pickle-evolution]: See the [compressed integer primitives][typed-tree-pickle-integers], [lazy/optional-B defaults][typed-tree-pickle-b-stream], and [reserved-space/fixup machinery][typed-tree-pickle-reserved] in `TypedTreePickle`.
[^format-version]: The revision is fixed in [CompilerLocation][compiler-location-source]; `CompilerImports` [emits the signature-data version attribute][signature-version-emit], [checks it on import][signature-version-match], and [reports a different-version error before using the IL import path][signature-version-import].
[^resource-names]: The exact prefixes and historical FSharp.Core names are defined in [PrettyNaming][resource-names-source].
[^resource-read-write]: [CompilerImports lines 53-199][resource-read-write-source] recognize, pair, inflate, compress, and emit public A/B resources; [lines 201-343][resource-encode-source] select names and generate signature/optimization data.
[^metadata-options]: [CompilerOptions][compiler-options-source] defines the three switches. [CompilerConfig defaults][compiler-config-source] make compression true and metadata generation enabled; its [generation predicates][compiler-generation-source] make `noSignatureData` suppress signature generation and tie optimization generation to signature generation. [CompilerImports][resource-encode-source] reduces rather than removes optimization data when `onlyEssentialOptimizationData` is set.
[^srm-resources]: The public surface is defined by the pinned runtime sources for [`MetadataReader`][metadata-reader-source], [`ManifestResource`][manifest-resource-source], [`PEReader`][pe-reader-source], and [`BlobReader`][blob-reader-source].
[^incremental-research]: See the local [versioned reference-index and semantic/API fingerprint recommendation](fast-fsharp-compiler-cold-and-incremental.md#cold-start-make-the-first-invocation-a-bounded-mostly-static-path) and its [two-graph/two-fingerprint model](fast-fsharp-compiler-cold-and-incremental.md#middle-of-dag-edits-use-two-graphs-and-two-fingerprints).
[^cache-primary-sources]: The official [rustc query and incremental-compilation documentation][rustc-query] describes dependency-tracked memoized queries; the first-party [Go cache implementation][go-cache] and [build-ID implementation][go-build-id] expose content/action-keyed reuse.

[issue-17]: https://github.com/TheAngryByrd/fsharp2/issues/17
[resource-names-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/SyntaxTree/PrettyNaming.fs#L1104-L1133
[compiler-imports-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L53-L343
[resource-read-write-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L53-L199
[resource-encode-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L201-L343
[compiler-options-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerOptions.fs#L941-L968
[compiler-config-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerConfig.fs#L764-L780
[typed-tree-pickle-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/TypedTree/TypedTreePickle.fs
[typed-tree-pickle-integers]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/TypedTree/TypedTreePickle.fs#L224-L474
[typed-tree-pickle-b-stream]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/TypedTree/TypedTreePickle.fs#L679-L780
[typed-tree-pickle-reserved]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/TypedTree/TypedTreePickle.fs#L927-L1056
[optimizer-types-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Optimize/Optimizer.fs#L62-L179
[optimizer-essential-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Optimize/Optimizer.fs#L1415-L1421
[optimizer-pickle-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Optimize/Optimizer.fs#L4421-L4490
[compiler-location-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Facilities/CompilerLocation.fs#L38-L45
[signature-version-emit]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L300-L316
[signature-version-match]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L1050-L1056
[signature-version-import]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerImports.fs#L2304-L2315
[compiler-generation-source]: https://github.com/dotnet/dotnet/blob/f7d90799ce4ef09a0bb257852a57248d2a8fb8dd/src/fsharp/src/Compiler/Driver/CompilerConfig.fs#L1517-L1520
[metadata-reader-source]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/MetadataReader.cs
[manifest-resource-source]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/TypeSystem/ManifestResource.cs
[pe-reader-source]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/PortableExecutable/PEReader.cs
[blob-reader-source]: https://github.com/dotnet/runtime/blob/v10.0.10/src/libraries/System.Reflection.Metadata/src/System/Reflection/Metadata/BlobReader.cs
[nativeaot-source]: https://github.com/dotnet/docs/blob/e61848719410e9bbe577f57e8359af802c5deba6/docs/core/deploying/native-aot/index.md
[rustc-query]: https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation.html
[go-cache]: https://go.dev/src/cmd/go/internal/cache/cache.go
[go-build-id]: https://go.dev/src/cmd/go/internal/work/buildid.go
