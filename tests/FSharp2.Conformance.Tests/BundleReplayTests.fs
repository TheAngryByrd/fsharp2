namespace FSharp2.Conformance.Tests

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open Expecto
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module BundleReplayTests =
    let private bundleFiles values =
        values
        |> Seq.map (fun (path, content) ->
            KeyValuePair<string, ImmutableArray<byte>>(path, bytes content)
        )
        |> ImmutableDictionary.CreateRange

    let private bundleInput runId =
        BundleInput(
            runId,
            null,
            bundleFiles [
                "inputs/source.fs", "module ReplayFixture\nlet value = 42\n"
                "inputs/toolchain.json", "{\"sdkVersion\":\"10.0.110\"}\n"
                "comparison-policy.json", "{\"policyId\":\"conformance-comparison-v1\"}\n"
                "oracle/evaluation.json", "{\"compiler\":\"sdk-fsc\"}\n"
                "tools/fsc2-host.bin", "fsharp2-host-fixture"
            ],
            json """{"verdict":{"state":"pass"}}"""
        )

    let private snapshotFiles root =
        Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
        |> Seq.map (fun path ->
            Path.GetRelativePath(root, path).Replace('\\', '/'), Hashing.Sha256File(path)
        )
        |> Map.ofSeq

    let private fileHashes (values: ImmutableDictionary<string, string>) =
        values
        |> Seq.map (fun item -> item.Key, item.Value)
        |> Map.ofSeq

    let private mutateBundle (bundleRoot: string) mutation =
        let bundlePath = Path.Combine(bundleRoot, "bundle.json")
        mutateJson bundlePath (fun bundle -> mutation (bundle.AsObject()))

        let bundleHash =
            bundlePath
            |> readJson
            |> CanonicalJson.Canonicalize
            |> Hashing.Sha256

        File.WriteAllText(
            Path.Combine(bundleRoot, "bundle.sha256"),
            bundleHash
            + Environment.NewLine,
            UTF8Encoding(false)
        )

    let private expectBundleIssue code bundleRoot =
        try
            BundleReader.ReadAndVerify(bundleRoot)
            |> ignore

            failtest $"Bundle verification accepted missing issue '{code}'"
        with :? ConformanceContractException as error ->
            Expect.isTrue
                (error.Issues
                 |> Seq.exists (fun issue -> issue.Code = code))
                $"Bundle verification must report '{code}'"

    let private replayPlanBundleInput runId runRoot (plan: CoreCompilePlan) =
        let files =
            ImmutableDictionary.CreateBuilder<string, ImmutableArray<byte>>(StringComparer.Ordinal)

        let evidenceJson =
            JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

        let addLane name (invocation: LaneInvocation) =
            let workRoot = Path.GetDirectoryName(invocation.ProjectPath)

            files[$"{name}/invocation.json"] <-
                JsonSerializer.SerializeToElement(invocation, evidenceJson)
                |> CanonicalJson.Canonicalize
                |> immutableArray

            for path in Directory.EnumerateFiles(workRoot, "*", SearchOption.AllDirectories) do
                let relativePath = Path.GetRelativePath(workRoot, path).Replace('\\', '/')
                files[$"{name}/work/{relativePath}"] <- immutableArray (File.ReadAllBytes(path))

            if not (String.IsNullOrWhiteSpace(invocation.CompilerHostPath)) then
                files[$"tools/{Path.GetFileName(invocation.CompilerHostPath)}"] <-
                    immutableArray (File.ReadAllBytes(invocation.CompilerHostPath))

        addLane "oracle" plan.Oracle
        addLane "fsharp2" plan.FSharp2

        let packagePath =
            Path.Combine(
                runRoot,
                "immutable",
                "packages",
                "FSharp2.Compiler.MSBuild.0.0.0-conformance.nupkg"
            )

        files["inputs/packages/FSharp2.Compiler.MSBuild.0.0.0-conformance.nupkg"] <-
            immutableArray (File.ReadAllBytes(packagePath))

        BundleInput(
            runId,
            null,
            files.ToImmutable(),
            JsonSerializer.SerializeToElement(
                {|
                    identity = {|
                        caseId = "language.bindings.value-function-positive"
                    |}
                    verdict = {| state = "pass" |}
                |},
                evidenceJson
            )
        )

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Replay" [
            testCase "Conformance Runner writes a verified bundle for declared unsupported cases"
            <| fun _ ->
                withCopiedRoot
                    "runner-write-bundle"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let outputRoot = Path.Combine(runRoot, "runs")
                        let hostPath = Path.Combine(runRoot, "native", "fsc2.exe")
                        writeNativePeHost hostPath
                        expectValid (NativeAotHostValidator.Validate(hostPath))

                        let lockHashes () =
                            Directory.EnumerateFiles(
                                root,
                                "*.oracle-lock.json",
                                SearchOption.AllDirectories
                            )
                            |> Seq.map (fun path ->
                                Path.GetRelativePath(root, path).Replace('\\', '/'),
                                Hashing.Sha256File(path)
                            )
                            |> Map.ofSeq

                        let checkedInLocks = lockHashes ()
                        let oracleOutputRoot = Path.Combine(runRoot, "oracle-captures")

                        let captureExitCode =
                            ConformanceRunner
                                .CaptureOracleAsync(
                                    immutableDictionary [
                                        "root", root
                                        "case", "language.bindings.value-function-positive"
                                        "output-root", oracleOutputRoot
                                    ],
                                    CancellationToken.None
                                )
                                .GetAwaiter()
                                .GetResult()

                        Expect.equal captureExitCode 0 "The public Oracle capture succeeds"

                        let captureRoots = Directory.GetDirectories(oracleOutputRoot)
                        Expect.hasLength captureRoots 1 "Oracle capture emits one run root"

                        for path in
                            [
                                Path.Combine(captureRoots[0], "resolved-manifest.json")
                                Path.Combine(captureRoots[0], "resolved-case.json")
                                Path.Combine(captureRoots[0], "environment.json")
                                Path.Combine(captureRoots[0], "oracle", "invocation.json")
                                Path.Combine(captureRoots[0], "oracle", "stdout.bin")
                                Path.Combine(captureRoots[0], "oracle", "stderr.bin")
                                Path.Combine(captureRoots[0], "oracle", "processes.json")
                                Path.Combine(captureRoots[0], "run-result.json")
                            ] do
                            Expect.isTrue (File.Exists(path)) $"Oracle capture emits {path}"

                        Expect.equal
                            (lockHashes ())
                            checkedInLocks
                            "Oracle capture does not update checked-in locks"

                        let checkedInOutput = Path.Combine(root, "artifacts", "conformance")

                        let outputRejection =
                            try
                                ConformanceRunner
                                    .CaptureOracleAsync(
                                        immutableDictionary [
                                            "root", root
                                            "case", "language.bindings.value-function-positive"
                                            "output-root", checkedInOutput
                                        ],
                                        CancellationToken.None
                                    )
                                    .GetAwaiter()
                                    .GetResult()
                                |> ignore

                                None
                            with :? ConformanceContractException as error ->
                                Some error

                        Expect.isSome
                            outputRejection
                            "Oracle capture rejects output inside the checked-in conformance root"

                        Expect.isTrue
                            (outputRejection.Value.Issues
                             |> Seq.exists (fun issue -> issue.Code = "output-root"))
                            "Oracle capture reports the checked-in output-root issue"

                        Expect.isFalse
                            (Directory.Exists(checkedInOutput))
                            "Rejected Oracle capture creates no checked-in output"

                        let run caseId output =
                            try
                                ConformanceRunner
                                    .RunAsync(
                                        immutableDictionary [
                                            "root", root
                                            "case", caseId
                                            "dotnet-root", sdkRoot
                                            "fsharp2-host", hostPath
                                            "output-root", output
                                        ],
                                        CancellationToken.None
                                    )
                                    .GetAwaiter()
                                    .GetResult()
                            with :? ConformanceContractException as error ->
                                let details =
                                    error.Issues
                                    |> Seq.map (fun issue ->
                                        $"{issue.Code} {issue.Path}: {issue.Message}"
                                    )
                                    |> String.concat Environment.NewLine

                                failtest
                                    $"The public run path failed:{Environment.NewLine}{details}"

                        let exitCode = run "language.signatures.contract-positive" outputRoot

                        Expect.equal
                            exitCode
                            0
                            "A declared unsupported case completes the public run path"

                        let emittedRunRoots = Directory.GetDirectories(outputRoot)
                        Expect.hasLength emittedRunRoots 1 "The public run path emits one run root"

                        let emittedRunRoot = emittedRunRoots[0]

                        for path in
                            [
                                Path.Combine(emittedRunRoot, "run-result.json")
                                Path.Combine(emittedRunRoot, "comparison.json")
                                Path.Combine(emittedRunRoot, "bundle", "bundle.json")
                                Path.Combine(emittedRunRoot, "bundle", "run-result.json")
                            ] do
                            Expect.isTrue (File.Exists(path)) $"The public run path emits {path}"

                        let laneOutputRoot = Path.Combine(runRoot, "lane-runs")

                        let laneExitCode =
                            run "language.bindings.value-function-positive" laneOutputRoot

                        Expect.notEqual
                            laneExitCode
                            0
                            "The unmanaged fixture cannot publish the declared positive artifacts"

                        let laneRunRoots = Directory.GetDirectories(laneOutputRoot)
                        Expect.hasLength laneRunRoots 1 "The lane run emits one run root"
                        let laneRunRoot = laneRunRoots[0]
                        let bundleRoot = Path.Combine(laneRunRoot, "bundle")

                        let receiptHash lane =
                            let receiptPath =
                                Path.Combine(bundleRoot, lane, "cleanup-receipt.json")

                            Expect.isTrue
                                (File.Exists(receiptPath))
                                $"The bundle contains the {lane} cleanup receipt"

                            use receipt = JsonDocument.Parse(File.ReadAllBytes(receiptPath))
                            Hashing.Sha256(CanonicalJson.Canonicalize(receipt.RootElement))

                        use runResult =
                            JsonDocument.Parse(
                                File.ReadAllBytes(Path.Combine(bundleRoot, "run-result.json"))
                            )

                        let lanes = runResult.RootElement.GetProperty("lanes")

                        for lane in
                            [
                                "oracle"
                                "fsharp2"
                            ] do
                            Expect.equal
                                (lanes.GetProperty(lane).GetProperty("cleanupHash").GetString())
                                (receiptHash lane)
                                $"The {lane} cleanupHash covers its bundled cleanup receipt"
                    )

            testCase "Conformance Replay verifies every input and tool hash"
            <| fun _ ->
                withRoot
                    "replay-hash-verification"
                    (fun runRoot ->
                        let input = bundleInput "source-run"
                        let receipt = BundleWriter.Write(runRoot, input)
                        let verified = BundleReader.ReadAndVerify(receipt.BundleRoot)

                        Expect.equal
                            verified.RunId
                            input.RunId
                            "The verified bundle retains its run id"

                        Expect.equal
                            verified.BundleHash
                            receipt.BundleHash
                            "The verified bundle retains its canonical bundle hash"

                        Expect.equal
                            (fileHashes verified.FileHashes)
                            (fileHashes receipt.FileHashes)
                            "Every recorded file hash verifies"

                        for item in input.Files do
                            Expect.isTrue
                                (verified.FileHashes.ContainsKey(item.Key))
                                $"The bundle records the hash for {item.Key}"

                            Expect.equal
                                verified.FileHashes[item.Key]
                                (Hashing.Sha256(item.Value.AsSpan()))
                                $"The recorded hash matches {item.Key}"

                        Expect.isTrue
                            (verified.FileHashes.ContainsKey("tools/fsc2-host.bin"))
                            "The bundle verifies the compiler tool hash"

                        for requiredPath in
                            [
                                "inputs/source.fs"
                                "inputs/toolchain.json"
                                "comparison-policy.json"
                                "oracle/evaluation.json"
                                "tools/fsc2-host.bin"
                            ] do
                            Expect.isTrue
                                (verified.FileHashes.ContainsKey(requiredPath))
                                $"Replay verifies the required input hash for {requiredPath}"
                    )

            testCase "Conformance Replay rejects one mutated byte"
            <| fun _ ->
                withRoot
                    "replay-mutated-byte"
                    (fun runRoot ->
                        let receipt = BundleWriter.Write(runRoot, bundleInput "mutated-source-run")
                        let toolPath = Path.Combine(receipt.BundleRoot, "tools", "fsc2-host.bin")
                        let mutated = File.ReadAllBytes(toolPath)

                        mutated[0] <-
                            mutated[0]
                            ^^^ 0xffuy

                        File.WriteAllBytes(toolPath, mutated)

                        try
                            BundleReader.ReadAndVerify(receipt.BundleRoot)
                            |> ignore

                            failtest "Bundle verification accepted a mutated tool byte"
                        with :? ConformanceContractException as error ->
                            let details =
                                error.Issues
                                |> Seq.map (fun issue ->
                                    $"{issue.Code} {issue.Path} {issue.Message}"
                                )
                                |> String.concat Environment.NewLine

                            Expect.isTrue
                                (details.Contains("hash", StringComparison.OrdinalIgnoreCase))
                                "Replay rejects the bundle because a recorded hash changed"
                    )

            testCase "Conformance Replay rejects invalid bundle schema fields and types"
            <| fun _ ->
                withRoot
                    "replay-schema"
                    (fun runRoot ->
                        let receipt = BundleWriter.Write(runRoot, bundleInput "schema-run")

                        mutateBundle
                            receipt.BundleRoot
                            (fun bundle ->
                                bundle["schemaVersion"] <- JsonValue.Create("one")
                                bundle["unexpectedField"] <- JsonValue.Create(true)
                            )

                        expectBundleIssue "bundle-schema" receipt.BundleRoot
                    )

            testCase "Conformance Replay rejects missing required paths"
            <| fun _ ->
                withRoot
                    "replay-required-paths"
                    (fun runRoot ->
                        let receipt = BundleWriter.Write(runRoot, bundleInput "required-paths-run")

                        mutateBundle
                            receipt.BundleRoot
                            (fun bundle ->
                                bundle.Remove("requiredPaths")
                                |> ignore
                            )

                        expectBundleIssue "bundle-required-path" receipt.BundleRoot
                    )

            testCase "Conformance Replay rejects mismatched required entries"
            <| fun _ ->
                withRoot
                    "replay-required-entry"
                    (fun runRoot ->
                        let receipt = BundleWriter.Write(runRoot, bundleInput "required-entry-run")

                        mutateBundle
                            receipt.BundleRoot
                            (fun bundle ->
                                let files = bundle["files"].AsArray()
                                let entry = files[0].AsObject()
                                entry["required"] <- JsonValue.Create(false)
                            )

                        expectBundleIssue "bundle-file-required" receipt.BundleRoot
                    )

            testCase "Conformance Replay rejects duplicate normalized file paths"
            <| fun _ ->
                withRoot
                    "replay-duplicate-path"
                    (fun runRoot ->
                        let receipt = BundleWriter.Write(runRoot, bundleInput "duplicate-path-run")

                        mutateBundle
                            receipt.BundleRoot
                            (fun bundle ->
                                let files = bundle["files"].AsArray()
                                let duplicate = JsonNode.Parse(files[0].ToJsonString()).AsObject()
                                let path = duplicate["path"].GetValue<string>()
                                duplicate["path"] <- JsonValue.Create(path + "/")
                                files.Add(duplicate)
                            )

                        expectBundleIssue "bundle-path-duplicate" receipt.BundleRoot
                    )

            testCase "Conformance Replay rejects incorrect payload lengths"
            <| fun _ ->
                withRoot
                    "replay-payload-length"
                    (fun runRoot ->
                        let receipt = BundleWriter.Write(runRoot, bundleInput "payload-length-run")

                        mutateBundle
                            receipt.BundleRoot
                            (fun bundle ->
                                let files = bundle["files"].AsArray()
                                let entry = files[0].AsObject()
                                let byteLength = entry["byteLength"].GetValue<int64>()

                                entry["byteLength"] <-
                                    JsonValue.Create(
                                        byteLength
                                        + 1L
                                    )
                            )

                        expectBundleIssue "bundle-file-length" receipt.BundleRoot
                    )

            testCase "Conformance Replay reruns both compilers under a realistic long output root"
            <| fun _ ->
                withCopiedRoot
                    "replay"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById repository "language.bindings.value-function-positive"
                            )

                        let roots = LaneRoots.Create(runRoot, "replay-source-plan")
                        let sdk = SdkSelection.Resolve(root, sdkRoot, null)

                        let plan =
                            CoreCompileRunner.CreatePlan(
                                materialized,
                                roots,
                                sdk,
                                fsharp2CompilerHostPath ()
                            )

                        let parentRunId =
                            "run-language-bindings-value-function-positive-20260822T020851245Z-76e6b1aabf6241679cf41846872e2ab3"

                        let receipt =
                            BundleWriter.Write(
                                runRoot,
                                replayPlanBundleInput parentRunId runRoot plan
                            )

                        let outputRoot =
                            Path.Combine(
                                runRoot,
                                "replays",
                                "realistic-customer-output-root",
                                "retained-conformance-evidence"
                            )

                        let exitCode =
                            ConformanceRunner
                                .ReplayAsync(
                                    immutableDictionary [
                                        "bundle", receipt.BundleRoot
                                        "dotnet-root", sdkRoot
                                        "output-root", outputRoot
                                    ],
                                    CancellationToken.None
                                )
                                .GetAwaiter()
                                .GetResult()

                        Expect.equal exitCode 0 "The public two-lane replay succeeds"

                        let replayRoots = Directory.GetDirectories(outputRoot)

                        Expect.hasLength
                            replayRoots
                            1
                            "The public replay emits one linked run root"

                        let replayRoot = replayRoots[0]

                        let link = readJson (Path.Combine(replayRoot, "replay-link.json"))

                        Expect.equal
                            (link.GetProperty("parentRunId").GetString())
                            parentRunId
                            "The bounded replay identity retains the complete parent run id"

                        let packageRoot = Path.Combine(replayRoot, "immutable", "packages")

                        Expect.hasLength
                            (Directory.GetFiles(
                                packageRoot,
                                "*.nupkg",
                                SearchOption.TopDirectoryOnly
                            ))
                            1
                            "Both lanes share one immutable package materialization"

                        let mutableLaneRoots =
                            [
                                "oracle"
                                "fsharp2"
                            ]
                            |> List.map (fun lane ->
                                let laneRoot = Path.Combine(replayRoot, "lanes", lane)
                                let workRoot = Path.Combine(laneRoot, "work")

                                Expect.isTrue
                                    (workRoot.Length < 260)
                                    $"The {lane} replay work path is bounded: {workRoot.Length} characters"

                                for path in
                                    [
                                        Path.Combine(replayRoot, lane, "processes.json")
                                        Path.Combine(replayRoot, lane, "binlog-evidence.json")
                                    ] do
                                    Expect.isTrue
                                        (File.Exists(path))
                                        $"Replay publishes fresh {lane} evidence: {path}"

                                laneRoot
                            )

                        Expect.notEqual
                            mutableLaneRoots[0]
                            mutableLaneRoots[1]
                            "Oracle and FSharp2 replays use separate mutable lane roots"
                    )

            testCase "Conformance Replay creates a linked result without overwrite"
            <| fun _ ->
                withRoot
                    "replay-linked-result"
                    (fun runRoot ->
                        let receipt =
                            BundleWriter.Write(runRoot, bundleInput "original-source-run")

                        let verified = BundleReader.ReadAndVerify(receipt.BundleRoot)
                        let sourceBefore = snapshotFiles receipt.BundleRoot
                        let outputRoot = Path.Combine(runRoot, "replays")

                        let replay =
                            ReplayRunner
                                .ReplayAsync(verified, sdkRoot, outputRoot, CancellationToken.None)
                                .GetAwaiter()
                                .GetResult()

                        let sourceAfter = snapshotFiles receipt.BundleRoot

                        Expect.equal
                            replay.ParentRunId
                            verified.RunId
                            "The replay links to the source run"

                        Expect.notEqual
                            replay.RunId
                            verified.RunId
                            "The replay creates a new run id"

                        Expect.equal
                            replay.BundleHash
                            verified.BundleHash
                            "The replay records the verified source bundle hash"

                        Expect.equal
                            sourceAfter
                            sourceBefore
                            "The replay leaves every source bundle byte unchanged"

                        Expect.isTrue
                            (Directory.Exists(replay.OutputRoot))
                            "The replay creates linked output in the requested output root"

                        Expect.notEqual
                            (Path.GetFullPath(replay.OutputRoot))
                            (Path.GetFullPath(receipt.BundleRoot))
                            "The replay does not overwrite the source bundle"
                    )
        ]
