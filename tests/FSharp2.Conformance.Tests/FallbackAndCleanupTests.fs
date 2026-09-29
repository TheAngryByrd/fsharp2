namespace FSharp2.Conformance.Tests

open System
open System.Collections.Immutable
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Threading
open Expecto
open FSharp2.Compiler
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module FallbackAndCleanupTests =
    let private processResult processes =
        let observedAt = DateTimeOffset.UtcNow

        ProcessResult(
            0,
            false,
            observedAt,
            observedAt,
            bytes "",
            bytes "",
            immutableArray processes
        )

    let private withLaneCleanupReceipt name action =
        let _, laneRoot = createRunRoot name
        let retainedPath = Path.Combine(laneRoot, "evidence", "keep.bin")
        let removedPath = Path.Combine(laneRoot, "work", "remove.bin")

        Path.GetDirectoryName(retainedPath)
        |> ensureDirectory
        |> ignore

        Path.GetDirectoryName(removedPath)
        |> ensureDirectory
        |> ignore

        File.WriteAllBytes(retainedPath, [| 1uy |])
        File.WriteAllBytes(removedPath, [| 2uy |])

        try
            let receipt =
                CleanupManager
                    .CleanupAsync(
                        laneRoot,
                        ImmutableArray<ProcessObservation>.Empty,
                        CleanupPolicy(
                            false,
                            immutableArray [ "evidence/keep.bin" ],
                            TimeSpan.FromSeconds(10.0)
                        ),
                        CancellationToken.None
                    )
                    .GetAwaiter()
                    .GetResult()

            Expect.sequenceEqual
                receipt.RetainedPaths
                [ "evidence/keep.bin" ]
                "Lane cleanup retains only the declared evidence file"

            Expect.sequenceEqual
                receipt.RemovedPaths
                [
                    ".fsharp2-conformance-root"
                    "work/"
                    "work/remove.bin"
                ]
                "Lane cleanup removes its marker and transient work"

            Expect.sequenceEqual
                receipt.RemainingPaths
                [
                    "evidence"
                    "evidence/keep.bin"
                ]
                "Lane cleanup records only retained evidence as remaining"

            Expect.isEmpty receipt.KilledProcessIds "The fixture has no lane process to kill"
            Expect.isEmpty receipt.OpenHandles "The fixture cleanup leaves no open handles"
            action laneRoot receipt
        finally
            if Directory.Exists(laneRoot) then
                cleanupRoot name laneRoot
                |> ignore

    let private cleanupReceiptHash (receipt: CleanupReceipt) =
        JsonSerializer.SerializeToElement(
            receipt,
            JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)
        )
        |> CanonicalJson.Canonicalize
        |> Hashing.Sha256

    let private writeProject repositoryRoot relativePath (contents: string) =
        let path = Path.Combine(repositoryRoot, relativePath)

        Path.GetDirectoryName(path)
        |> ensureDirectory
        |> ignore

        File.WriteAllText(path, contents)

    let private isProcessRunning processId =
        try
            use childProcess = Process.GetProcessById(processId)
            not childProcess.HasExited
        with :? ArgumentException ->
            false

    let private stopProcessId processId =
        if processId > 0 then
            try
                use childProcess = Process.GetProcessById(processId)

                if not childProcess.HasExited then
                    childProcess.Kill(true)

                childProcess.WaitForExit(10000)
                |> ignore
            with :? ArgumentException ->
                ()

    let private isObservedProcessRunning (observation: ProcessObservation) =
        try
            use childProcess = Process.GetProcessById(observation.ProcessId)

            not childProcess.HasExited
            && observation.StartedAt.HasValue
            && DateTimeOffset(childProcess.StartTime).ToUniversalTime().UtcTicks = observation
                .StartedAt.Value
                .ToUniversalTime()
                .UtcTicks
            && String.Equals(
                Path.GetFullPath(childProcess.MainModule.FileName),
                Path.GetFullPath(observation.ExecutablePath),
                StringComparison.OrdinalIgnoreCase
            )
        with
        | :? ArgumentException
        | :? InvalidOperationException
        | :? Win32Exception -> false

    let private stopObservedProcess (observation: ProcessObservation) =
        if isObservedProcessRunning observation then
            try
                use childProcess = Process.GetProcessById(observation.ProcessId)
                childProcess.Kill(true)

                childProcess.WaitForExit(10000)
                |> ignore
            with
            | :? ArgumentException
            | :? InvalidOperationException
            | :? Win32Exception -> ()

    let private readProcessId path =
        let timer = Stopwatch.StartNew()
        let mutable processId: int option = None

        while processId.IsNone
              && timer.Elapsed < TimeSpan.FromSeconds(10.0) do
            try
                use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.None)
                use reader = new StreamReader(stream)

                match Int32.TryParse(reader.ReadToEnd()) with
                | true, value -> processId <- Some value
                | false, _ -> ()
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

            if processId.IsNone then
                Thread.Sleep(20)

        processId
        |> Option.defaultWith (fun () -> failtest $"Timed out reading process id from {path}")

    let private observedProcess (childProcess: Process) executablePath startedAt =
        ProcessObservation(
            childProcess.Id,
            Nullable(Environment.ProcessId),
            executablePath,
            immutableArray [
                testAssemblyPath
                "--conformance-test-child"
            ],
            StartedAt = Nullable(startedAt)
        )

    let private expectProcessIdentityRefusal
        (name: string)
        (observation: Process -> ProcessObservation)
        =
        withRoot
            name
            (fun runRoot ->
                let receiptPath = Path.Combine(runRoot, "child.pid")
                use childProcess = startTestChild receiptPath
                waitForFile receiptPath (TimeSpan.FromSeconds(10.0))

                try
                    let mutable refusal: ConformanceContractException option = None

                    try
                        CleanupManager
                            .CleanupAsync(
                                runRoot,
                                immutableArray [ observation childProcess ],
                                CleanupPolicy(false, immutableArray [], TimeSpan.FromSeconds(10.0)),
                                CancellationToken.None
                            )
                            .GetAwaiter()
                            .GetResult()
                        |> ignore
                    with :? ConformanceContractException as error ->
                        refusal <- Some error

                    Expect.isSome refusal "Cleanup accepted a process with changed identity"

                    Expect.isTrue
                        (refusal.Value.Issues
                         |> Seq.exists (fun issue -> issue.Code = "cleanup-process-identity"))
                        "The refusal identifies the changed process identity"

                    Expect.isTrue
                        (isProcessRunning childProcess.Id)
                        "Cleanup refuses the changed identity before it kills the process"
                finally
                    stopProcess childProcess

                    let cleanupReceipt =
                        writeCleanupReceipt name {|
                            processId = childProcess.Id
                            runningAfterCleanup = isProcessRunning childProcess.Id
                            recordedAt = DateTimeOffset.UtcNow
                        |}

                    Expect.isFalse
                        (isProcessRunning childProcess.Id)
                        $"Process identity cleanup receipt: {cleanupReceipt}"
            )

    let private createDirectoryLink linkPath targetPath =
        if OperatingSystem.IsWindows() then
            let startInfo = ProcessStartInfo("cmd.exe")
            startInfo.UseShellExecute <- false
            startInfo.CreateNoWindow <- true
            startInfo.RedirectStandardOutput <- true
            startInfo.RedirectStandardError <- true
            startInfo.ArgumentList.Add("/d")
            startInfo.ArgumentList.Add("/c")
            startInfo.ArgumentList.Add("mklink")
            startInfo.ArgumentList.Add("/J")
            startInfo.ArgumentList.Add(linkPath)
            startInfo.ArgumentList.Add(targetPath)

            use linkProcess = Process.Start(startInfo)
            let standardOutput = linkProcess.StandardOutput.ReadToEndAsync()
            let standardError = linkProcess.StandardError.ReadToEndAsync()

            if not (linkProcess.WaitForExit(10000)) then
                linkProcess.Kill(true)

                if not (linkProcess.WaitForExit(10000)) then
                    failtest $"mklink process {linkProcess.Id} did not exit"

                failtest "mklink did not exit within 10 seconds"

            let output = standardOutput.GetAwaiter().GetResult()
            let error = standardError.GetAwaiter().GetResult()

            Expect.equal linkProcess.ExitCode 0 $"Could not create junction: {output}{error}"
        else
            Directory.CreateSymbolicLink(linkPath, targetPath)
            |> ignore

    let private removeDirectoryLink (linkPath: string) =
        try
            let attributes = File.GetAttributes(linkPath)

            if attributes.HasFlag(FileAttributes.Directory) then
                Directory.Delete(linkPath)
            else
                File.Delete(linkPath)
        with
        | :? FileNotFoundException
        | :? DirectoryNotFoundException -> ()

    let private withParentProcess
        (name: string)
        (runRoot: string)
        (childArgument: string)
        (action: int -> int -> ProcessResult -> unit)
        =
        let parentReceiptPath = Path.Combine(runRoot, "parent.pid")
        let childReceiptPath = Path.Combine(runRoot, "descendant.pid")
        let childArgumentPath = Path.Combine(runRoot, "child-argument.txt")
        let standardOutputPath = Path.Combine(runRoot, "parent-stdout.bin")
        let standardErrorPath = Path.Combine(runRoot, "parent-stderr.bin")
        File.WriteAllText(childArgumentPath, childArgument)

        let environment =
            immutableDictionary [
                "DOTNET_ROOT", sdkRoot
                "DOTNET_MULTILEVEL_LOOKUP", "0"
                "PATH",
                sdkRoot
                + string Path.PathSeparator
                + Environment.GetEnvironmentVariable("PATH")
            ]

        let specification =
            ProcessSpec(
                dotnetPath,
                runRoot,
                immutableArray [
                    testAssemblyPath
                    "--conformance-test-parent"
                    parentReceiptPath
                    childReceiptPath
                    childArgumentPath
                ],
                environment,
                TimeSpan.FromSeconds(2.0),
                standardOutputPath,
                standardErrorPath
            )

        let runTask = ProcessRunner.RunAsync(specification, CancellationToken.None)
        let mutable parentProcessId = 0
        let mutable childProcessId = 0
        let mutable completedResult: ProcessResult option = None

        try
            waitForFile parentReceiptPath (TimeSpan.FromSeconds(10.0))
            waitForFile childReceiptPath (TimeSpan.FromSeconds(10.0))
            parentProcessId <- readProcessId parentReceiptPath
            childProcessId <- readProcessId childReceiptPath
            let result = runTask.GetAwaiter().GetResult()
            completedResult <- Some result
            action parentProcessId childProcessId result
        finally
            try
                let result = runTask.GetAwaiter().GetResult()

                if completedResult.IsNone then
                    completedResult <- Some result
            with _ ->
                ()

            if
                parentProcessId = 0
                && File.Exists(parentReceiptPath)
            then
                parentProcessId <- readProcessId parentReceiptPath

            if
                childProcessId = 0
                && File.Exists(childReceiptPath)
            then
                childProcessId <- readProcessId childReceiptPath

            let observation processId =
                completedResult
                |> Option.bind (fun result ->
                    result.Processes
                    |> Seq.tryFind (fun candidate -> candidate.ProcessId = processId)
                )

            let stopRecordedProcess processId =
                match observation processId with
                | Some recorded -> stopObservedProcess recorded
                | None -> stopProcessId processId

            let isRecordedProcessRunning processId =
                match observation processId with
                | Some recorded -> isObservedProcessRunning recorded
                | None -> isProcessRunning processId

            stopRecordedProcess childProcessId
            stopRecordedProcess parentProcessId

            let parentRunningAfterCleanup = isRecordedProcessRunning parentProcessId
            let childRunningAfterCleanup = isRecordedProcessRunning childProcessId

            let cleanupReceipt =
                writeCleanupReceipt name {|
                    parentProcessId = parentProcessId
                    childProcessId = childProcessId
                    parentRunningAfterCleanup = parentRunningAfterCleanup
                    childRunningAfterCleanup = childRunningAfterCleanup
                    recordedAt = DateTimeOffset.UtcNow
                |}

            Expect.isFalse
                parentRunningAfterCleanup
                $"Parent process cleanup receipt: {cleanupReceipt}"

            Expect.isFalse
                childRunningAfterCleanup
                $"Descendant process cleanup receipt: {cleanupReceipt}"

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Fallback and Cleanup" [
            testCase "Conformance Fallback fails when the sentinel runs"
            <| fun _ ->
                withCopiedRoot
                    "fallback-sentinel"
                    (fun root ->
                        removeCaseProbe
                            root
                            "language.bindings.value-function-positive"
                            "managed-load"

                        let runRoot = Directory.GetParent(root).FullName
                        let outputRoot = Path.Combine(runRoot, "runs")

                        let appHostPath =
                            nativeExecutablePath
                                (Path.GetDirectoryName(testAssemblyPath))
                                (Path.GetFileNameWithoutExtension(testAssemblyPath))

                        let sentinelPath =
                            nativeExecutablePath
                                (Path.GetDirectoryName(testAssemblyPath))
                                $"fallback-sentinel-{Guid.NewGuid():N}"

                        let sentinelReceiptPath =
                            Path.Combine(runRoot, "sentinel-arguments.receipt")

                        let environmentName = "FSharp2FallbackSentinelReceiptPath"

                        let previousReceiptPath =
                            Environment.GetEnvironmentVariable(environmentName)

                        File.Copy(appHostPath, sentinelPath)

                        try
                            expectValid (NativeAotHostValidator.Validate(sentinelPath))

                            Environment.SetEnvironmentVariable(
                                environmentName,
                                sentinelReceiptPath
                            )

                            let exitCode =
                                ConformanceRunner
                                    .RunAsync(
                                        immutableDictionary [
                                            "root", root
                                            "case", "language.bindings.value-function-positive"
                                            "dotnet-root", sdkRoot
                                            "fsharp2-host", sentinelPath
                                            "output-root", outputRoot
                                        ],
                                        CancellationToken.None
                                    )
                                    .GetAwaiter()
                                    .GetResult()

                            Expect.isTrue
                                (File.Exists(sentinelReceiptPath))
                                "The selected fallback sentinel must execute"

                            let receivedArguments = File.ReadAllLines(sentinelReceiptPath)

                            Expect.isNonEmpty
                                receivedArguments
                                "The fallback sentinel must append its received arguments"

                            Expect.equal
                                exitCode
                                1
                                "A real fallback-sentinel invocation must fail the public run"

                            let emittedRunRoots = Directory.GetDirectories(outputRoot)
                            Expect.hasLength emittedRunRoots 1 "The public run emits one run root"

                            use runResult =
                                JsonDocument.Parse(
                                    File.ReadAllBytes(
                                        Path.Combine(emittedRunRoots[0], "run-result.json")
                                    )
                                )

                            let reasons =
                                runResult.RootElement
                                    .GetProperty("verdict")
                                    .GetProperty("reasons")
                                    .EnumerateArray()
                                |> Seq.map (fun value -> value.GetString())
                                |> String.concat Environment.NewLine

                            Expect.stringContains
                                reasons
                                "fallback"
                                "The public verdict identifies the fallback-sentinel invocation"
                        finally
                            Environment.SetEnvironmentVariable(
                                environmentName,
                                previousReceiptPath
                            )

                            if File.Exists(sentinelPath) then
                                File.Delete(sentinelPath)
                    )

            testCase "Conformance Fallback fails when lineage contains SDK fsc"
            <| fun _ ->
                let sdkFsc = Path.Combine(sdkRoot, "sdk", "10.0.110", "FSharp", "fsc.dll")

                let result =
                    processResult [
                        ProcessObservation(
                            43210,
                            Nullable<int>(),
                            sdkFsc,
                            immutableArray [
                                "dotnet"
                                sdkFsc
                                "Program.fs"
                            ]
                        )
                    ]

                Expect.equal
                    (CoreCompileRunner.Classify(result))
                    ConformanceVerdict.Fail
                    "SDK fsc in the FSharp2 lineage is a fallback failure"

            testCase "Conformance Fallback rejects production Oracle and FCS dependencies"
            <| fun _ ->
                withRoot
                    "fallback-production-graph"
                    (fun repositoryRoot ->
                        writeProject
                            repositoryRoot
                            (Path.Combine(
                                "src",
                                "FSharp2.Compiler.Prototype.Core",
                                "FSharp2.Compiler.Prototype.Core.fsproj"
                            ))
                            """<Project><ItemGroup><PackageReference Include="FSharp.Compiler.Service" /></ItemGroup></Project>"""

                        writeProject
                            repositoryRoot
                            (Path.Combine("src", "fsc2.Prototype", "fsc2.Prototype.fsproj"))
                            """<Project><ItemGroup><ProjectReference Include="..\CompatibilityOracle\CompatibilityOracle.fsproj" /></ItemGroup></Project>"""

                        writeProject
                            repositoryRoot
                            (Path.Combine(
                                "src",
                                "FSharp2.Compiler.MSBuild",
                                "FSharp2.Compiler.MSBuild.csproj"
                            ))
                            "<Project />"

                        let result = ProductionGraphGuard.Validate(repositoryRoot)

                        let details =
                            result.Issues
                            |> Seq.map (fun issue -> $"{issue.Path} {issue.Message}")
                            |> String.concat Environment.NewLine

                        Expect.isFalse
                            result.IsValid
                            "Production projects cannot depend on the Oracle or FCS"

                        Expect.stringContains
                            details
                            "FSharp.Compiler.Service"
                            "The graph result identifies the FCS dependency"

                        Expect.stringContains
                            details
                            "Oracle"
                            "The graph result identifies the Oracle dependency"
                    )

            testCase "Conformance Cleanup retains declared late-failure artifacts"
            <| fun _ ->
                withRoot
                    "cleanup-retained-artifacts"
                    (fun runRoot ->
                        let retainedPath = Path.Combine(runRoot, "artifacts", "retained.bin")
                        let removedPath = Path.Combine(runRoot, "work", "discard.bin")

                        Path.GetDirectoryName(retainedPath)
                        |> ensureDirectory
                        |> ignore

                        Path.GetDirectoryName(removedPath)
                        |> ensureDirectory
                        |> ignore

                        File.WriteAllBytes(
                            retainedPath,
                            [|
                                1uy
                                2uy
                                3uy
                            |]
                        )

                        File.WriteAllBytes(
                            removedPath,
                            [|
                                4uy
                                5uy
                                6uy
                            |]
                        )

                        let policy =
                            CleanupPolicy(
                                false,
                                immutableArray [ "artifacts/retained.bin" ],
                                TimeSpan.FromSeconds(10.0)
                            )

                        let receipt =
                            CleanupManager
                                .CleanupAsync(
                                    runRoot,
                                    immutableArray ([]: int list),
                                    policy,
                                    CancellationToken.None
                                )
                                .GetAwaiter()
                                .GetResult()

                        Expect.isTrue
                            (File.Exists(retainedPath))
                            "A declared late-failure artifact remains available"

                        Expect.isFalse
                            (File.Exists(removedPath))
                            "An undeclared work artifact is removed"

                        Expect.contains
                            receipt.RetainedPaths
                            "artifacts/retained.bin"
                            "The cleanup receipt records the retained artifact"

                        Expect.contains
                            receipt.RemovedPaths
                            "work/discard.bin"
                            "The cleanup receipt records the removed work artifact"

                        Expect.contains
                            receipt.RemainingPaths
                            "artifacts/retained.bin"
                            "The cleanup receipt records the retained artifact as remaining"

                        Expect.isFalse
                            (receipt.RemainingPaths
                             |> Seq.contains "work/discard.bin")
                            "The cleanup receipt excludes removed work from remaining paths"
                    )

            testCase "Conformance Cleanup kills recorded descendants and no unrelated process"
            <| fun _ ->
                withRoot
                    "cleanup-recorded-descendants"
                    (fun runRoot ->
                        let recordedPath = Path.Combine(runRoot, "recorded.pid")
                        let unrelatedPath = Path.Combine(runRoot, "unrelated.pid")
                        use recordedProcess = startTestChild recordedPath
                        use unrelatedProcess = startTestChild unrelatedPath

                        waitForFile recordedPath (TimeSpan.FromSeconds(10.0))
                        waitForFile unrelatedPath (TimeSpan.FromSeconds(10.0))

                        try
                            let policy =
                                CleanupPolicy(false, immutableArray [], TimeSpan.FromSeconds(10.0))

                            let recordedObservation =
                                observedProcess
                                    recordedProcess
                                    recordedProcess.MainModule.FileName
                                    (DateTimeOffset(recordedProcess.StartTime).ToUniversalTime())

                            let receipt =
                                CleanupManager
                                    .CleanupAsync(
                                        runRoot,
                                        immutableArray [ recordedObservation ],
                                        policy,
                                        CancellationToken.None
                                    )
                                    .GetAwaiter()
                                    .GetResult()

                            Expect.isFalse
                                (isProcessRunning recordedProcess.Id)
                                "The recorded descendant process is stopped"

                            Expect.isTrue
                                (isProcessRunning unrelatedProcess.Id)
                                "An unrelated process remains alive"

                            Expect.contains
                                receipt.KilledProcessIds
                                recordedProcess.Id
                                "The cleanup receipt records the stopped descendant"

                            Expect.isFalse
                                (receipt.KilledProcessIds
                                 |> Seq.contains unrelatedProcess.Id)
                                "The cleanup receipt excludes the unrelated process"
                        finally
                            stopProcess recordedProcess
                            stopProcess unrelatedProcess

                            let receiptPath =
                                writeCleanupReceipt "cleanup-recorded-descendants-processes" {|
                                    recordedProcessId = recordedProcess.Id
                                    unrelatedProcessId = unrelatedProcess.Id
                                    recordedRunningAfterCleanup =
                                        isProcessRunning recordedProcess.Id
                                    unrelatedRunningAfterCleanup =
                                        isProcessRunning unrelatedProcess.Id
                                    recordedAt = DateTimeOffset.UtcNow
                                |}

                            Expect.isFalse
                                (isProcessRunning recordedProcess.Id)
                                $"Recorded descendant cleanup receipt: {receiptPath}"

                            Expect.isFalse
                                (isProcessRunning unrelatedProcess.Id)
                                $"Unrelated process cleanup receipt: {receiptPath}"
                    )

            testCase "Conformance Cleanup refuses a root without its marker"
            <| fun _ ->
                let runRoot = createUnmarkedRoot "cleanup-unowned-root"

                try
                    try
                        CleanupManager
                            .CleanupAsync(
                                runRoot,
                                immutableArray ([]: int list),
                                CleanupPolicy(true, immutableArray [], TimeSpan.FromSeconds(10.0)),
                                CancellationToken.None
                            )
                            .GetAwaiter()
                            .GetResult()
                        |> ignore

                        failtest "Cleanup accepted a root without its ownership marker"
                    with :? ConformanceContractException as error ->
                        Expect.isTrue
                            (error.Issues
                             |> Seq.exists (fun issue -> issue.Code = "run-root-marker"))
                            "The refusal identifies the missing ownership marker"

                        Expect.isTrue
                            (Directory.Exists(runRoot))
                            "Cleanup refuses the unowned root before deletion"
                finally
                    deleteOwnedRoot runRoot

                    let receiptPath =
                        writeCleanupReceipt "cleanup-unowned-root" {|
                            root = runRoot
                            existsAfterCleanup = Directory.Exists(runRoot)
                            recordedAt = DateTimeOffset.UtcNow
                        |}

                    Expect.isFalse
                        (Directory.Exists(runRoot))
                        $"Markerless test-root cleanup receipt: {receiptPath}"

            testList "Conformance Cleanup Process Safety" [
                testCase "Conformance Cleanup rejects reparse-point traversal"
                <| fun _ ->
                    withRoot
                        "cleanup-reparse-point"
                        (fun runRoot ->
                            let _, targetRoot = createRunRoot "cleanup-reparse-target"
                            let targetPath = Path.Combine(targetRoot, "outside.bin")
                            let linkPath = Path.Combine(runRoot, "escape")

                            File.WriteAllBytes(
                                targetPath,
                                [|
                                    1uy
                                    2uy
                                    3uy
                                |]
                            )

                            createDirectoryLink linkPath targetRoot

                            try
                                let mutable refusal: ConformanceContractException option = None

                                try
                                    CleanupManager
                                        .CleanupAsync(
                                            runRoot,
                                            immutableArray ([]: ProcessObservation list),
                                            CleanupPolicy(
                                                false,
                                                immutableArray [],
                                                TimeSpan.FromSeconds(10.0)
                                            ),
                                            CancellationToken.None
                                        )
                                        .GetAwaiter()
                                        .GetResult()
                                    |> ignore
                                with :? ConformanceContractException as error ->
                                    refusal <- Some error

                                Expect.isSome refusal "Cleanup accepted a reparse point"

                                Expect.isTrue
                                    (refusal.Value.Issues
                                     |> Seq.exists (fun issue ->
                                         issue.Code = "cleanup-reparse-point"
                                     ))
                                    "The refusal identifies reparse-point traversal"

                                Expect.isTrue
                                    (File.Exists(targetPath))
                                    "Cleanup refuses before it changes the linked target"
                            finally
                                removeDirectoryLink linkPath
                                deleteOwnedRoot targetRoot

                                let cleanupReceipt =
                                    writeCleanupReceipt "cleanup-reparse-target" {|
                                        root = targetRoot
                                        linkExistsAfterCleanup =
                                            Directory.Exists(linkPath)
                                            || File.Exists(linkPath)
                                        targetExistsAfterCleanup = Directory.Exists(targetRoot)
                                        recordedAt = DateTimeOffset.UtcNow
                                    |}

                                Expect.isFalse
                                    (Directory.Exists(targetRoot))
                                    $"Reparse target cleanup receipt: {cleanupReceipt}"
                        )

                testCase "Conformance Cleanup rejects changed process identity"
                <| fun _ ->
                    expectProcessIdentityRefusal
                        "cleanup-process-executable-identity"
                        (fun childProcess ->
                            let startedAt =
                                DateTimeOffset(childProcess.StartTime).ToUniversalTime()

                            observedProcess childProcess testAssemblyPath startedAt
                        )

                    expectProcessIdentityRefusal
                        "cleanup-process-start-time-identity"
                        (fun childProcess ->
                            let startedAt =
                                DateTimeOffset(childProcess.StartTime)
                                    .ToUniversalTime()
                                    .AddDays(-1.0)

                            observedProcess childProcess childProcess.MainModule.FileName startedAt
                        )

                testCase "Conformance Cleanup captures complete real process lineage"
                <| fun _ ->
                    withRoot
                        "cleanup-process-lineage"
                        (fun runRoot ->
                            withParentProcess
                                "cleanup-process-lineage"
                                runRoot
                                ""
                                (fun parentProcessId childProcessId result ->
                                    let lineage = ProcessLineage.Read(result)

                                    Expect.isGreaterThanOrEqual
                                        lineage.Length
                                        2
                                        "The lineage contains the launched process and a descendant"

                                    for observation in lineage do
                                        Expect.isTrue
                                            observation.StartedAt.HasValue
                                            $"Process {observation.ProcessId} has no start time"

                                    let parent =
                                        lineage
                                        |> Seq.tryFind (fun observation ->
                                            observation.ProcessId = parentProcessId
                                        )
                                        |> Option.defaultWith (fun () ->
                                            failtest "The lineage omitted the launched process"
                                        )

                                    let child =
                                        lineage
                                        |> Seq.tryFind (fun observation ->
                                            observation.ProcessId = childProcessId
                                        )
                                        |> Option.defaultWith (fun () ->
                                            failtest "The lineage omitted the real descendant"
                                        )

                                    Expect.isTrue
                                        child.ParentProcessId.HasValue
                                        "The descendant records its parent process"

                                    Expect.equal
                                        child.ParentProcessId.Value
                                        parent.ProcessId
                                        "The lineage contains the real parent-child edge"
                                )
                        )

                testCase "Conformance Cleanup complete lineage detects descendant SDK fsc"
                <| fun _ ->
                    withRoot
                        "cleanup-lineage-sdk-fsc"
                        (fun runRoot ->
                            let sdkFsc =
                                Path.Combine(sdkRoot, "sdk", "10.0.110", "FSharp", "fsc.dll")

                            withParentProcess
                                "cleanup-lineage-sdk-fsc"
                                runRoot
                                sdkFsc
                                (fun parentProcessId childProcessId result ->
                                    let lineage = ProcessLineage.Read(result)

                                    let parent =
                                        lineage
                                        |> Seq.find (fun observation ->
                                            observation.ProcessId = parentProcessId
                                        )

                                    Expect.isFalse
                                        (parent.Arguments
                                         |> Seq.contains sdkFsc)
                                        "The launched process does not contain the fallback marker"

                                    let child =
                                        lineage
                                        |> Seq.tryFind (fun observation ->
                                            observation.ProcessId = childProcessId
                                        )
                                        |> Option.defaultWith (fun () ->
                                            failtest "The lineage omitted the marked descendant"
                                        )

                                    Expect.contains
                                        child.Arguments
                                        sdkFsc
                                        "The descendant contains the SDK fsc marker"

                                    Expect.equal
                                        (CoreCompileRunner.Classify(result))
                                        ConformanceVerdict.Fail
                                        "Complete lineage classifies descendant SDK fsc as fallback"
                                )
                        )

                testCase "Conformance Cleanup receipt matches real operations"
                <| fun _ ->
                    withRoot
                        "cleanup-receipt-real-operations"
                        (fun runRoot ->
                            let retainedPath = Path.Combine(runRoot, "artifacts", "keep.bin")
                            let removedPath = Path.Combine(runRoot, "work", "remove.bin")
                            let lockedPath = Path.Combine(runRoot, "locked.bin")
                            let childReceiptPath = Path.Combine(runRoot, "child.pid")

                            Path.GetDirectoryName(retainedPath)
                            |> ensureDirectory
                            |> ignore

                            Path.GetDirectoryName(removedPath)
                            |> ensureDirectory
                            |> ignore

                            File.WriteAllBytes(retainedPath, [| 1uy |])
                            File.WriteAllBytes(removedPath, [| 2uy |])
                            File.WriteAllBytes(lockedPath, [| 3uy |])

                            use lockedHandle =
                                new FileStream(
                                    lockedPath,
                                    FileMode.Open,
                                    FileAccess.ReadWrite,
                                    FileShare.None
                                )

                            use childProcess = startTestChild childReceiptPath
                            waitForFile childReceiptPath (TimeSpan.FromSeconds(10.0))

                            let startedAt =
                                DateTimeOffset(childProcess.StartTime).ToUniversalTime()

                            let observation =
                                observedProcess
                                    childProcess
                                    childProcess.MainModule.FileName
                                    startedAt

                            try
                                let receipt =
                                    CleanupManager
                                        .CleanupAsync(
                                            runRoot,
                                            immutableArray [ observation ],
                                            CleanupPolicy(
                                                false,
                                                immutableArray [ "artifacts/keep.bin" ],
                                                TimeSpan.FromSeconds(10.0)
                                            ),
                                            CancellationToken.None
                                        )
                                        .GetAwaiter()
                                        .GetResult()

                                let lockedPathRemains = File.Exists(lockedPath)

                                let expectedRemovedPaths = [
                                    ".fsharp2-conformance-root"
                                    "child.pid"

                                    if not lockedPathRemains then
                                        "locked.bin"

                                    "work/"
                                    "work/remove.bin"
                                ]

                                let expectedRemainingPaths = [
                                    "artifacts"
                                    "artifacts/keep.bin"

                                    if lockedPathRemains then
                                        "locked.bin"
                                ]

                                Expect.equal
                                    receipt.RunRoot
                                    (Path.GetFullPath(runRoot))
                                    "The receipt records the actual cleanup root"

                                Expect.sequenceEqual
                                    receipt.KilledProcessIds
                                    [ childProcess.Id ]
                                    "The receipt records exactly the killed process"

                                Expect.sequenceEqual
                                    receipt.RetainedPaths
                                    [ "artifacts/keep.bin" ]
                                    "The receipt records exactly the retained file"

                                Expect.sequenceEqual
                                    receipt.RemovedPaths
                                    expectedRemovedPaths
                                    "The receipt records exactly the removed files and directory"

                                Expect.sequenceEqual
                                    receipt.RemainingPaths
                                    expectedRemainingPaths
                                    "The receipt records exactly the remaining paths"

                                Expect.isTrue
                                    (File.Exists(retainedPath))
                                    "The retained receipt entry exists"

                                Expect.isFalse
                                    (File.Exists(removedPath))
                                    "The removed receipt entry does not exist"

                                Expect.isFalse
                                    (isProcessRunning childProcess.Id)
                                    "The killed receipt entry is not running"

                                if lockedPathRemains then
                                    Expect.equal
                                        receipt.OpenHandles.Length
                                        1
                                        "The receipt records exactly one open handle"

                                    Expect.isTrue
                                        (receipt.OpenHandles[0]
                                            .StartsWith("locked.bin: ", StringComparison.Ordinal))
                                        "The open-handle receipt identifies the locked file"
                                else
                                    Expect.isEmpty
                                        receipt.OpenHandles
                                        "The receipt has no open handle when deletion succeeds"

                                if OperatingSystem.IsWindows() then
                                    Expect.isTrue
                                        lockedPathRemains
                                        "The Windows open handle prevents deletion"
                            finally
                                stopProcess childProcess

                                let cleanupReceipt =
                                    writeCleanupReceipt "cleanup-receipt-real-operations-process" {|
                                        processId = childProcess.Id
                                        runningAfterCleanup = isProcessRunning childProcess.Id
                                        recordedAt = DateTimeOffset.UtcNow
                                    |}

                                Expect.isFalse
                                    (isProcessRunning childProcess.Id)
                                    $"Real operation process cleanup receipt: {cleanupReceipt}"
                        )

                testCase "RunResultWriter hashes actual lane cleanup receipts"
                <| fun _ ->
                    withCopiedRoot
                        "run-result-cleanup-hashes"
                        (fun root ->
                            let repository = ManifestLoader.Load(root)

                            let materialized =
                                CaseMaterializer.Materialize(
                                    repository,
                                    caseById repository "language.bindings.value-function-positive"
                                )

                            let sdk = SdkSelection.Resolve(root, sdkRoot, null)
                            let request = CompilerContractProbe.CreateRequest(materialized)
                            let compilation = Compiler().Compile(request, CancellationToken.None)
                            let coreEvidence = CoreEvidenceWriter.Create(compilation)

                            let verdict =
                                VerdictResult(
                                    ConformanceVerdict.Pass,
                                    ImmutableArray<string>.Empty,
                                    ImmutableArray<string>.Empty,
                                    true
                                )

                            let invocation kind laneRoot =
                                LaneInvocation(
                                    kind,
                                    Path.Combine(laneRoot, "Project.fsproj"),
                                    bytes "<Project />",
                                    ImmutableArray<MaterializedInputHash>.Empty,
                                    immutableDictionary [ "Configuration", "Release" ],
                                    immutableDictionary [ "DOTNET_ROOT", sdkRoot ],
                                    immutableArray [ "build" ],
                                    null
                                )

                            let laneResult kind =
                                CoreCompileLaneResult(
                                    kind,
                                    processResult [],
                                    BinlogEvidence(
                                        1,
                                        false,
                                        immutableArray [ "fsc @compiler.rsp" ]
                                    )
                                )

                            withLaneCleanupReceipt
                                "run-result-oracle-cleanup"
                                (fun oracleRoot oracleReceipt ->
                                    withLaneCleanupReceipt
                                        "run-result-fsharp2-cleanup"
                                        (fun fsharp2Root fsharp2Receipt ->
                                            let plan =
                                                CoreCompilePlan(
                                                    invocation LaneKind.Oracle oracleRoot,
                                                    invocation LaneKind.FSharp2 fsharp2Root
                                                )

                                            let runResult =
                                                RunResultWriter.Create(
                                                    "run-cleanup-hashes",
                                                    null,
                                                    repository,
                                                    materialized,
                                                    sdk,
                                                    testAssemblyPath,
                                                    compilation,
                                                    coreEvidence,
                                                    plan,
                                                    laneResult LaneKind.Oracle,
                                                    laneResult LaneKind.FSharp2,
                                                    ImmutableArray<ProbeEvidence>.Empty,
                                                    ImmutableArray<ComparisonResult>.Empty,
                                                    verdict,
                                                    ImmutableDictionary<
                                                        string,
                                                        ImmutableArray<byte>
                                                     >.Empty,
                                                    oracleReceipt,
                                                    fsharp2Receipt
                                                )

                                            let lanes = runResult.GetProperty("lanes")

                                            Expect.equal
                                                (lanes
                                                    .GetProperty("oracle")
                                                    .GetProperty("cleanupHash")
                                                    .GetString())
                                                (cleanupReceiptHash oracleReceipt)
                                                "Oracle cleanupHash covers the actual Oracle receipt"

                                            Expect.equal
                                                (lanes
                                                    .GetProperty("fsharp2")
                                                    .GetProperty("cleanupHash")
                                                    .GetString())
                                                (cleanupReceiptHash fsharp2Receipt)
                                                "FSharp2 cleanupHash covers the actual FSharp2 receipt"
                                        )
                                )
                        )
            ]
        ]
