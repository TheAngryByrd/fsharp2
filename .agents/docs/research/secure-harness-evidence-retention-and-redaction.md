# Secure harness evidence retention and redaction

Status: issue [#16](https://github.com/TheAngryByrd/fsharp2/issues/16) research decision, 2026-07-19

Policy id: **evidence-governance-v1**

Governing decisions: [ADRs 0002](../adr/0002-require-nativeaot-for-compiler-and-output.md), [0005](../adr/0005-use-the-shipped-compiler-as-compatibility-oracle.md), [0010](../adr/0010-forbid-production-fallback-to-the-official-compiler.md), [0013](../adr/0013-use-memory-and-content-addressed-disk-caches.md), [0016](../adr/0016-explain-slow-compilations-with-structured-traces.md), [0020](../adr/0020-use-the-complete-icedtasks-corpus-for-the-first-milestone.md), [0022](../adr/0022-define-the-warm-edit-corpus-and-cache-evidence-contract.md), and [0024](../adr/0024-use-calibrated-dedicated-runners-for-performance-gates.md)

Related evidence: [differential harness](differential-compatibility-performance-harness.md), [performance governance](compiler-performance-runner-and-regression-governance.md), [warm-edit contract](warm-edit-corpus-and-cache-evidence-contract.md), [localization/signing/resource policy](localization-signing-native-resource-compatibility-policy.md), and [comparative fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md)

## Answer

FSharp2 keeps two deliberately different evidence products:

1. an authoritative, encrypted **Restricted Raw Bundle** in a dedicated evidence store for trusted diagnostic, nightly, release, oracle-refresh, and accepted-baseline runs; and
2. a reconstructed, allowlist-only **Shareable Evidence Bundle** that is safe even if the repository or release becomes public.

Raw source-bearing evidence never goes to GitHub Actions logs, workflow artifacts, caches, job summaries, pull-request comments, releases, or package feeds. GitHub documents that repository readers can download workflow artifacts and that pull requests can access applicable caches; repository privacy is therefore not a sufficient raw-evidence access boundary.[^github-artifact-readers] [^github-cache-secrets]

The raw bundle remains authoritative. Redaction never mutates or replaces it. A separate exporter reads the raw manifest, constructs a new typed bundle from an explicit field allowlist, validates canary absence and schema closure, and writes an independently hashed redaction report. A verdict cannot be published, promoted as a performance baseline, or used to claim the Compatibility Gate when required raw evidence, the export report, or deletion/access evidence is missing.

This policy cannot weaken issue #15:

- private strong-name key bytes, private parameters, private-key hashes, original secret paths, secret environment/configuration values, and signing-process dumps are **Prohibited Evidence** and are never retained;
- every reachable FSxxxx error and warning remains covered in every declared language lane with exact raw messages, streams, ranges, ordering, effective severity, exit, and leftovers;
- exact message text may be shared only for the public pinned IcedTasks corpus or a reviewed harness-owned corpus; a private corpus retains exact text only in restricted evidence and shares the equality verdict without the text; and
- FSharp2 and the emitted-output NativeAOT gates remain independent and both require reconciled evidence.

## Threat and trust boundary

The evidence pipeline assumes that all of the following can contain source, paths, credentials, or other sensitive state:

- MSBuild binlogs and imported project files;
- physical response files and FSharp2 protocol requests;
- raw stdout/stderr and exact diagnostics;
- source trees, patches, generated inputs, PDBs, Source Link data, assemblies, resources, and package graphs;
- compiler-service memory/disk state and incremental cache snapshots;
- EventPipe/NetTrace, heap graphs, crash dumps, runner logs, and process/environment inventories; and
- temporary files left by MSBuild, the Compatibility Oracle, FSharp2, consumers, debuggers, or NativeAOT publishing.

This is not hypothetical. MSBuild's binary logger embeds project/import source by default and records environment variables used by MSBuild; its own documentation says the reduced environment capture does not eliminate sensitive-data leakage.[^msbuild-binlog] .NET documentation warns that dumps can contain full process memory, and even its triage redaction is not guaranteed to satisfy a privacy requirement by itself.[^dotnet-dumps] [^dotnet-triage-redaction]

The tested compiler, Compatibility Oracle, downstream consumer, debugger, and NativeAOT process are untrusted with respect to evidence credentials. None receives an evidence-store token, key-encryption permission, GitHub write token, or general network egress. A candidate that writes prohibited material into its own trace is a product security failure; a harness or runner that mishandles otherwise valid restricted data is an evidence security failure. Neither can be retried away as performance noise.

Only trusted commits and reviewed compiler artifacts may run the privileged raw-capture lane. Fork or otherwise untrusted pull-request code receives no evidence-store identity, release/signing secret, or persistent self-hosted runner. GitHub explicitly warns that privileged pull_request_target/workflow_run combinations and self-hosted runners can expose secrets or the runner environment to untrusted changes.[^github-secure-use]

## Classification

Classification is field- and file-specific; a container inherits the highest class of any member. Unknown collected material defaults to **R3 Restricted Raw**. Unknown material in a shareable export is an error, not an implicit redaction.

| Class | Meaning | Examples | Permitted destinations |
| --- | --- | --- | --- |
| **P0 Prohibited Evidence** | Material that the harness may use transiently only where the compiler contract requires it, but may never collect, hash, log, upload, cache, dump, or retain. | Private signing bytes/parameters/hash; original private-key path; GitHub/cloud/package tokens; secret environment/config values; signing-process dump; plaintext data-encryption keys. | The minimum process memory or run-scoped secret file needed for the operation. No evidence destination. |
| **R3 Restricted Raw** | Authoritative source-bearing or memory-/environment-bearing evidence. | Binlog and ProjectImports; response/protocol bytes; raw streams; source/patches; raw PDB/PE/resources; complete invocation/environment; NetTrace; non-signing dump; service/cache snapshot; raw artifact tree. | Per-run encrypted runner volume and dedicated restricted evidence store only. |
| **R2 Controlled Derived** | Structured data derived from R3 that is useful for diagnosis but is not automatically safe to disclose. | Canonical comparison records; exact diagnostics from an unapproved corpus; private-source hashes; dependency graphs; internal paths not yet mapped; access/deletion audit details. | Restricted evidence store; approved diagnostic workspace. |
| **S1 Shareable Evidence** | A closed-schema, allowlisted export proved free of P0/R3/R2 fields and suitable for repository readers or public release. | Policy/tool/oracle identities; logical-root paths; verdict and reason codes; public input/artifact hashes; exact messages for approved public/harness sources; aggregate timings; safe fingerprints; redaction report and export hash. | Actions logs/artifacts, job summary, PR comment, release, package provenance, public documentation. |

The following rules prevent classification shortcuts:

- Encryption does not make R3 shareable; it only protects R3 in its allowed store.
- Masking does not reclassify a log line. GitHub masking is defense in depth for a value already known to the workflow, not permission to print raw material.
- A SHA-256 digest of P0 is still P0. A digest of private/proprietary source remains R2 unless disclosure of that exact corpus was approved. Public IcedTasks and harness-owned fixture hashes may be S1.
- Compiler caches are product state, not evidence storage. A captured cache image is R3 and never enters actions/cache. GitHub itself warns against sensitive cache contents because repository readers and applicable fork pull requests can access caches.[^github-cache-secrets]
- A Triage dump remains R3. Built-in path redaction does not turn it into S1.[^dotnet-triage-redaction]

## Capture and storage architecture

### Per-run capture

Every evidence-producing job uses a run-specific encrypted volume and assigns all work, TEMP/TMP, compiler-service, cache, output, response-file, binlog, trace, dump, and consumer roots beneath it. Access is restricted to the job identity. The volume is never reused across jobs.

The supervisor starts the tested processes with an explicit environment allowlist and no evidence-store credential. It records the complete authoritative bundle required by the differential harness, including raw byte streams and the physical binlog/response pair, but prevents P0 from entering that bundle. Private signing material is mounted only for the signing action, read after command validation, and removed before any dump or evidence walk can run.

A capture manifest records:

- policy, schema, run, case, attempt, and parent ids;
- producer/tool/compiler/oracle identities;
- logical path, media type, classification, byte length, and retention class for every entry;
- SHA-256 for R3/R2/S1 entries, but never for P0;
- expected source/publication authority and approved-corpus id;
- collection start/end and cleanup state; and
- raw bundle id, encryption object ids, expiry, key id, and integrity status.

The manifest must reconcile exactly with the differential bundle schema. Missing and extra entries are failures.

### Restricted evidence store

R3/R2 data uses a dedicated object store outside GitHub Actions artifacts, caches, logs, releases, and packages. Required store capabilities are:

- object version enumeration and deletion;
- per-object expiry/lifecycle metadata;
- immutable access and deletion audit events;
- deny-by-default network and identity policy;
- customer-controlled key wrapping and revocation;
- no anonymous/public access or repository-wide read grant; and
- a deletion API whose result can be verified and recorded.

The ordinary CI upload identity is write-only to one run prefix. It cannot read, list, overwrite, extend retention, change classification, or decrypt. A separate lifecycle identity may enumerate expiry and delete. A separate reader role may decrypt only a specifically approved bundle.

GitHub Actions artifacts are allowed only for S1. GitHub documents that any signed-in repository reader can download artifacts, that logs/artifacts default to 90 days, and that individual artifact retention can be configured.[^github-artifact-readers] [^github-artifact-deletion] Every S1 upload therefore declares retention explicitly; this policy never relies on repository defaults.

### Access

Restricted access is case-scoped, time-bound, and audited:

1. A request names the run/case, reason, minimum objects, intended local workspace, and deletion deadline.
2. An evidence custodian approves it. The requestor cannot self-approve when another custodian is available; a single-maintainer break-glass access records that fact and receives retrospective review.
3. The reader receives a short-lived role limited to those object ids and their wrapping keys. No directory-wide list or public URL is issued.
4. Download, decrypt, copy, and delete events are written to the audit ledger.
5. The diagnostic workspace is an encrypted disposable volume, has no source-control/package publication credential, and is destroyed by its deadline.

Routine repository read, issue triage, CI administration, and package publication do not grant R3/R2 read access. GitHub environments may add required review for privileged jobs, but the raw store's IAM/KMS policy remains the authority.[^github-secure-use]

## Encryption and integrity

R3/R2 objects use envelope encryption:

- a fresh random 256-bit data-encryption key per run;
- AES-256-GCM with a unique nonce per encrypted object;
- associated data binding repository id, run id, case/attempt id, policy/schema, classification, logical object id, and plaintext SHA-256;
- the data key wrapped by a versioned KMS key whose decrypt permission is separate from object read;
- TLS for every upload/download; and
- immediate clearing/destruction of owned plaintext key material after use.

AES and GCM follow the NIST AES and GCM specifications.[^nist-aes] [^nist-gcm] A storage provider may add server-side encryption, but it does not replace the per-run envelope boundary.

CI obtains its narrow upload/KMS-wrap identity through GitHub OIDC with repository, workflow, ref/environment, and audience conditions. GitHub recommends OIDC for short-lived, well-scoped workflow credentials instead of long-lived cloud secrets.[^github-oidc]

Integrity is independent of confidentiality:

- the raw manifest and bundle hash are calculated before encryption;
- each encrypted object is authenticated by GCM and checked after an authorized download;
- the S1 export has its own manifest and SHA-256, never reuses the raw bundle hash as its content identity;
- the redaction report binds raw bundle id/hash, exporter/policy ids, S1 manifest/hash, and verifier result; and
- accepted baseline/release summaries carry a signed CI provenance statement for the S1 hash.

The provenance and hashes prove which bytes were reviewed. They do not make restricted bytes safe to publish.

## Retention

Retention is assigned when the run is created. An object without a known class, owner, and expiresAt is rejected before upload. The values below are policy choices, not GitHub defaults.

| Evidence purpose | R3/R2 retention | S1 retention |
| --- | --- | --- |
| Runner plaintext and temporary files | Delete in finally; hard deadline one hour after job termination. Never uploaded as plaintext. | Not applicable. |
| Successful trusted PR smoke run | Do not upload raw; destroy after S1 verification. | 30 days. |
| Failed trusted PR or requested diagnostic run | 14 days. | 30 days. |
| Nightly compatibility/performance run | 30 days. | 90 days. |
| Release gate or oracle-refresh run | 400 days. | Repository/release lifetime. |
| Accepted performance-baseline raw bundle | While the baseline lineage is active, then 400 days after supersession. | Repository lifetime. |
| Rejected baseline candidate | 90 days unless it is also a release/oracle-refresh run. | Repository lifetime for the verdict/summary; 90 days for bulky derived detail. |
| Access audit, redaction report, deletion receipt, and incident id | Repository lifetime, with sensitive request text kept R2. | Repository lifetime for the S1 projection. |

A legal/security hold may extend R3/R2 only through an approved record with owner, reason, object ids, new expiry, and review no later than every 90 days. It can never retain P0, remove encryption, broaden access, or turn an indefinite hold into the default.

Performance policy requires the complete raw result bundle while a baseline can gate candidates and says old evidence remains addressable. The active-lineage rule preserves that authority; the 400-day post-supersession window preserves bridge/regression diagnosis without retaining source-bearing data forever. After raw expiry, the S1 manifest, verdict, provenance, and deletion receipt remain addressable and explicitly state that raw replay has expired.

## Shareable export and redaction

### Reconstruction, not in-place editing

The exporter does not copy the raw directory and then search/replace. It parses approved formats and creates a new directory from an S1 schema whose fields have explicit source and transformation rules. Entire opaque formats are denied from S1:

- binlog and ProjectImports archive;
- response files and binary protocol requests;
- source, source patches, compiler/service cache images, and raw environment;
- NetTrace, heap graph, crash dump, and signing-process diagnostics;
- uninspected PDB/PE/resource/package files; and
- any file or field unknown to the policy version.

Safe observations are reconstructed as typed JSON/CSV or reviewed public artifacts. Path canonicalization uses the differential harness's declared logical roots. It never rewrites the authoritative raw bytes. Environment output is a positive allowlist of named non-secret facts; absence of a name from the list means omission.

### Exact diagnostics and FSxxxx coverage

The family-wide Compatibility Gate compares exact messages in R3 for every reachable FSxxxx error/warning, every declared UI culture/fallback, warning control, stream, range, ordering, exit, and leftover case.

For the pinned public IcedTasks tree and harness-owned diagnostic fixtures, the reviewed corpus manifest permits exact localized messages, canonical logical paths, source hashes, and fixture snippets in S1. For any private or third-party corpus without publication approval:

- exact raw messages and source-bearing streams remain R3/R2;
- S1 records code, effective severity, canonical range shape where safe, message-equal boolean, occurrence/order/stream/exit results, and an opaque run-scoped observation id;
- a source/content/message hash is not exported merely because it is one-way; and
- omission from S1 never weakens the raw comparison or its pass/fail authority.

Redaction may not turn a mismatch into a match. Comparison completes against raw observations before export.

### Incremental and performance evidence

The fast-compiler work requires stable query/action keys, declaration dependencies, invalidation reasons, cache decisions, counters, and phase timings. S1 may contain schema ids, node kinds, logical public-corpus identities, hit/miss/reuse decisions, reason codes, dependency counts, timings, and approved fingerprints. It does not contain raw source, serialized compiler objects, private-source fingerprints, physical cache paths, or secret-bearing diagnostic text.

A culture-only change continues to invalidate diagnostic rendering rather than semantic state, and signing/resource changes remain final-link inputs as issue #15 defines. Evidence classification does not enter a compiler semantic cache key. It is a harness/export concern and cannot force recompilation or make localized text part of a semantic fingerprint.

### Redaction report

redaction-report.json contains:

- raw bundle id/hash and policy/exporter/verifier identities;
- one rule result per expected raw manifest entry: passed, transformed, summarized, or omitted;
- rule id, classification, destination logical path, and S1 output hash where one exists;
- declared root/environment/corpus transformations;
- unknown-field/file count, which must be zero;
- prohibited/sensitive canary scan results;
- S1 manifest/hash and byte count; and
- final pass/fail with reasons.

It never contains the removed value, private-key/secret hash, original secret path, or a snippet from an omitted binary.

### Independent validation

Every exporter test corpus injects distinct synthetic canaries into:

- repository/home/temp/secret-file physical paths;
- an environment secret, response argument, imported project property, source string, diagnostic argument, trace payload, and cache record;
- a dump from a non-signing process; and
- a private-key fixture path and a separate fake secret marker.

The S1 verifier scans bytes and parsed fields, reconciles the closed schema, checks the report and hashes, and requires all canaries absent. The real signing key is not copied into the exporter just to scan for it; P0 exclusion is structural and tested with synthetic material. A new schema field, file type, culture lane, trace event, diagnostic shape, or bundle member fails export until policy explicitly classifies it.

Only after verification may S1 be written to Actions output. A missing/lossy/mismatched report blocks evidence publication and baseline/release promotion.

## Deletion and incident response

### Normal deletion

Cleanup runs on success, failure, timeout, and cancellation:

1. stop compiler/consumer/debugger processes;
2. remove run-scoped secret files without recording their paths or hashes;
3. destroy the plaintext capture/diagnostic volume and its data key;
4. verify no R3/R2 object was uploaded to GitHub Actions, cache, release, package, or job log;
5. expire/delete restricted objects and every version/replica at the assigned deadline;
6. revoke/destroy the wrapped per-run data key after the last object is gone; and
7. write a non-secret deletion receipt containing object ids, version count, policy/retention class, provider result, key-destruction result, UTC, actor, and exceptions.

Failure to clean a dedicated runner quarantines it before another job. Destruction of a per-run encrypted volume/key is the normal runner sanitization boundary; retired physical media follows the owning provider's sanitization process.

GitHub documents that deleted workflow artifacts cannot be restored and exposes expiration through its artifact API.[^github-artifact-deletion] That mechanism applies only to S1 here. R3/R2 deletion is verified against the dedicated store, including object versions; deleting a workflow run cannot stand in for it.

### Security incident

If P0 is suspected in any evidence destination:

- stop publication and access immediately;
- delete/quarantine every contaminated local, restricted, and GitHub object;
- revoke affected object/KMS/session credentials and rotate the exposed secret/key;
- quarantine the runner and preserve only non-secret incident metadata;
- audit downloads and derived copies; and
- require security review before the corpus/workflow runs again.

Issue #15's no-retention rule wins over diagnostic convenience: a contaminated signing bundle or signing-process dump is not kept as incident evidence.

If R3/R2 is exposed through GitHub or another over-broad channel, delete the artifact/workflow output, revoke links/credentials, audit readers, purge derived copies, and regenerate S1 only after the exporter defect is fixed. The run is evidence-security-failed and cannot support a pass even if compiler outputs were correct.

Verdict ownership remains precise:

- candidate-emitted prohibited data or missing required candidate evidence is product-fail;
- runner/exporter collection loss or leak is security-error/infra-error and blocks publication;
- an already proved compiler mismatch remains fail even if a later evidence security error occurs; and
- no security or infrastructure classification permits replacement sampling to erase a completed slow or failing row.

## NativeAOT, no-fallback, and process separation

Evidence upload, encryption, KMS/OIDC, redaction, and retention tooling belong to the harness and CI control plane, not FSharp2.Compiler or its persistent service. This preserves the closed NativeAOT production graph and avoids giving the compiler cloud SDKs, credentials, or an exfiltration path.

The compiler emits only its versioned structured trace/protocol results to the run root. The supervisor proves selected executable, process graph, no official fsc/FCS fallback, and both NativeAOT gates before the evidence exporter runs. An evidence-store outage cannot cause production fallback or change compiler semantics; it yields an unpublishable evidence result.

## Required policy probes

Before any raw CI upload is enabled, the harness must automate and retain safe results for:

| Probe | Required result |
| --- | --- |
| Classification closure | Every differential bundle member and field has one policy rule; an unknown member fails before upload/export. |
| GitHub boundary | R3/R2/P0 are absent from Actions artifacts, caches, logs, summaries, comments, releases, and packages. |
| Binlog | Default embedded imports and used environment values are detected as R3; ProjectImports=None remains R3 because events/arguments/environment can still disclose data. |
| Signing | Real P0 is absent structurally; synthetic key/secret/path canaries never reach raw evidence or S1; no signing-process dump is captured. |
| Diagnostics | Family-wide raw exactness passes; public/harness corpus messages export exactly, while an unapproved-corpus message remains restricted without changing the verdict. |
| Trace/cache | Private source/cache/NetTrace remains R3; only approved structured decisions/fingerprints enter S1. |
| Dump | Non-signing dump is R3 only; Triage is not accepted as S1; signing dump collection is refused. |
| Fork/untrusted | No OIDC/raw-store/signing credential is issued and no persistent runner is exposed. |
| Encryption | Ciphertext/authentication/AAD and wrapped-key checks pass; wrong run/classification/key fails closed. |
| Access | Case-scoped reader grant expires; object/KMS/download audit reconciles with the request. |
| Retention | Synthetic objects expire at each class deadline; all versions and the wrapped key are gone; deletion receipt reconciles. |
| Export | Closed-schema rebuild, canary scan, raw/S1 hashes, and redaction report pass; tampering or missing report blocks publication. |
| Failure recovery | Cancellation, timeout, exporter failure, object-store failure, and cleanup failure leave no plaintext reuse and quarantine when required. |

Release evidence records the policy version, exact store/IAM/KMS configuration identity, workflow/action SHAs, runner image, corpus approval, raw bundle id/hash/expiry, S1 hash/provenance, redaction report, access audit status, and deletion schedule. Actions in a privileged evidence workflow are pinned to immutable commit SHAs, matching GitHub's secure-use guidance.[^github-secure-use]

## Current repository gap and implementation boundary

The current repository does not yet implement this evidence pipeline:

- build.yml watches main while the default branch is master;
- the FAKE helpers disable internal binlogs rather than running the differential capture lane;
- build logging masks only the known GitHub and NuGet token values;
- no restricted evidence store, OIDC policy, encryption/export schema, retention job, deletion receipt, or policy probe exists; and
- the existing workflows use action version tags rather than immutable SHAs.

That is an explicit gap, not a policy pass. Until evidence-governance-v1 and its probes exist, CI must not upload raw binlogs, response/protocol files, dumps, traces, cache snapshots, or source-bearing reproduction bundles. Local controlled evidence may continue to support development, and S1-only ordinary build/test/package artifacts may continue under their existing workflows.

This issue fixes the policy and acceptance contract. Implementing the harness/store/exporter and wiring release gates remains follow-on engineering under issue #1. No later ticket may weaken P0, treat repository privacy/masking/encryption as redaction, or publish a compatibility/performance verdict without the required raw and S1 linkage.

## Refresh triggers

Review and version this policy when:

- the differential bundle, trace, cache, diagnostic, PDB/resource, dump, or reproduction schema changes;
- a new corpus is not public/harness-owned or its publication authority changes;
- GitHub changes artifact/cache/log access, retention, deletion, OIDC, or runner security behavior;
- the evidence store, KMS, IAM, runner provider, encryption format, or workflow trust model changes;
- a compiler or provider-broker feature introduces a new secret/source-bearing input;
- diagnostic localization, signing, native resources, type providers, debugger automation, or NativeAOT adds evidence types;
- a security incident shows that a classification/export/deletion probe was incomplete; or
- law, contract, or data-owner requirements demand a shorter retention or stronger access boundary.

A refresh increments the policy/schema id and re-runs all policy probes. Existing objects keep their original policy and may only receive a shorter expiry unless an approved hold applies.

## Primary sources and repo evidence

- [GitHub artifact download permissions and default retention][github-artifact-readers]
- [GitHub artifact deletion, custom retention, and expires_at][github-artifact-deletion]
- [GitHub cache warning for sensitive data and fork/read access][github-cache-secrets]
- [GitHub secure-use guidance for secrets, OIDC, privileged triggers, actions, and self-hosted runners][github-secure-use]
- [GitHub OIDC short-lived credential model][github-oidc]
- [MSBuild binary-log project/import and environment capture][msbuild-binlog]
- [.NET dump sensitivity warning][dotnet-dumps]
- [.NET Triage-dump redaction limitation][dotnet-triage-redaction]
- [NIST AES specification][nist-aes]
- [NIST GCM specification][nist-gcm]
- [FSharp2 differential harness](differential-compatibility-performance-harness.md)
- [FSharp2 issue #15 policy](localization-signing-native-resource-compatibility-policy.md)
- [FSharp2 performance governance](compiler-performance-runner-and-regression-governance.md)
- [FSharp2 warm-edit evidence contract](warm-edit-corpus-and-cache-evidence-contract.md)
- [FSharp2 comparative fast-compiler research](fast-fsharp-compiler-cold-and-incremental.md)
- [Current build workflow](../../../.github/workflows/build.yml)
- [Current build secret masking](../../../build/build.fs)

[^github-artifact-readers]: GitHub states that signed-in users with repository read access can download workflow artifacts and that logs/artifacts default to 90 days: [official docs source][github-artifact-readers].
[^github-artifact-deletion]: GitHub documents irreversible artifact deletion, configurable per-artifact retention, and the REST expires_at field: [official docs source][github-artifact-deletion].
[^github-cache-secrets]: GitHub says not to store sensitive information in caches because repository readers and applicable fork pull requests can access cache contents: [official docs source][github-cache-secrets].
[^github-secure-use]: GitHub documents required review for environment secrets, privileged-trigger and self-hosted-runner risks, OIDC guidance, and immutable action-SHA guidance: [official docs source][github-secure-use].
[^github-oidc]: GitHub OIDC exchanges a workflow identity for short-lived, scoped cloud access rather than requiring a long-lived cloud credential: [official docs source][github-oidc].
[^msbuild-binlog]: MSBuild documents that ProjectImports=Embed is the default, embeds project/import source, and that used environment-variable capture reduces but does not eliminate sensitive-data leakage: [official source][msbuild-binlog].
[^dotnet-dumps]: .NET warns that a dump can contain full process memory and sensitive information: [official docs source][dotnet-dumps].
[^dotnet-triage-redaction]: .NET states that Triage dump redaction is not guaranteed by itself to satisfy a privacy law or standard: [official docs source][dotnet-triage-redaction].
[^nist-aes]: NIST FIPS 197 defines the Advanced Encryption Standard: [standard][nist-aes].
[^nist-gcm]: NIST SP 800-38D defines Galois/Counter Mode authenticated encryption: [standard][nist-gcm].

[github-artifact-readers]: https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/how-tos/manage-workflow-runs/download-workflow-artifacts.md
[github-artifact-deletion]: https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/how-tos/manage-workflow-runs/remove-workflow-artifacts.md
[github-cache-secrets]: https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/reference/workflows-and-actions/dependency-caching.md
[github-secure-use]: https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/reference/security/secure-use.md
[github-oidc]: https://github.com/github/docs/blob/27a4008f193706042a40cbb6c71cf85633249e79/content/actions/concepts/security/openid-connect.md
[msbuild-binlog]: https://github.com/dotnet/msbuild/blob/5d474cda0a6bbfc0414bdf5e05298846bb06f462/documentation/wiki/Binary-Log.md
[dotnet-dumps]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/diagnostics/dumps.md
[dotnet-triage-redaction]: https://github.com/dotnet/docs/blob/4fb491cfa55c31357bc569ad01c67db836c1f895/docs/core/diagnostics/microsoft-diagnostics-netcore-client.md
[nist-aes]: https://csrc.nist.gov/pubs/fips/197/final
[nist-gcm]: https://csrc.nist.gov/pubs/sp/800/38/d/final
