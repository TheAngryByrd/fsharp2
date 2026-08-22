namespace FSharp2.Conformance.Tests

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Threading
open Expecto
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module LaneIsolationTests =
    let private createPlan conformanceRoot runRoot runId =
        let repository = ManifestLoader.Load(conformanceRoot)

        let materialized =
            CaseMaterializer.Materialize(
                repository,
                caseById repository "language.bindings.value-function-positive"
            )

        let roots = LaneRoots.Create(runRoot, runId)
        let sdk = SdkSelection.Resolve(conformanceRoot, sdkRoot, null)
        let compilerHostPath = fsharp2CompilerHostPath ()
        let plan = CoreCompileRunner.CreatePlan(materialized, roots, sdk, compilerHostPath)
        roots, sdk, compilerHostPath, plan

    let private isProcessRunning processId =
        try
            use childProcess = Process.GetProcessById(processId)
            not childProcess.HasExited
        with :? ArgumentException ->
            false

    let private runLane (invocation: LaneInvocation) =
        CoreCompileRunner.RunAsync(invocation, CancellationToken.None).GetAwaiter().GetResult()

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Lanes" [
            testCase "Conformance Lanes use byte-identical projects and immutable inputs"
            <| fun _ ->
                withCopiedRoot
                    "lanes-identical-inputs"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let runId = Path.GetFileName(runRoot)
                        let _, _, _, plan = createPlan root runRoot runId

                        Expect.sequenceEqual
                            plan.Oracle.ProjectBytes
                            plan.FSharp2.ProjectBytes
                            "Both lanes must receive byte-identical project files"

                        Expect.sequenceEqual
                            plan.Oracle.Inputs
                            plan.FSharp2.Inputs
                            "Both lanes must receive the same ordered immutable input hashes"

                        Expect.isNonEmpty
                            plan.Oracle.ProjectBytes
                            "The generated project must not be empty"

                        Expect.isNonEmpty
                            plan.Oracle.Inputs
                            "The immutable input manifest must not be empty"
                    )

            testCase "Conformance Lanes share no mutable root"
            <| fun _ ->
                withCopiedRoot
                    "lanes-root-isolation"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let runId = Path.GetFileName(runRoot)
                        let roots, _, _, _ = createPlan root runRoot runId

                        let oraclePaths = [
                            roots.Oracle.Work
                            roots.Oracle.Temp
                            roots.Oracle.Intermediate
                            roots.Oracle.Output
                            roots.Oracle.Service
                            roots.Oracle.Cache
                            roots.Oracle.ResponseFile
                            roots.Oracle.Binlog
                        ]

                        let fsharp2Paths = [
                            roots.FSharp2.Work
                            roots.FSharp2.Temp
                            roots.FSharp2.Intermediate
                            roots.FSharp2.Output
                            roots.FSharp2.Service
                            roots.FSharp2.Cache
                            roots.FSharp2.ResponseFile
                            roots.FSharp2.Binlog
                        ]

                        let allPaths =
                            oraclePaths
                            @ fsharp2Paths
                            |> List.map Path.GetFullPath

                        Expect.equal
                            (allPaths
                             |> Set.ofList
                             |> Set.count)
                            allPaths.Length
                            "Every mutable lane path is unique"

                        for path in oraclePaths do
                            Expect.isTrue
                                ((Path.GetFullPath(path))
                                    .StartsWith(
                                        Path.GetFullPath(roots.Oracle.Root)
                                        + string Path.DirectorySeparatorChar,
                                        StringComparison.OrdinalIgnoreCase
                                    ))
                                $"Oracle path escaped its lane root: {path}"

                        for path in fsharp2Paths do
                            Expect.isTrue
                                ((Path.GetFullPath(path))
                                    .StartsWith(
                                        Path.GetFullPath(roots.FSharp2.Root)
                                        + string Path.DirectorySeparatorChar,
                                        StringComparison.OrdinalIgnoreCase
                                    ))
                                $"FSharp2 path escaped its lane root: {path}"

                        for expectedRunId, expectedRoot in
                            [
                                runId, runRoot
                                $"{runId}:oracle", roots.Oracle.Root
                                $"{runId}:fsharp2", roots.FSharp2.Root
                            ] do
                            let markerPath =
                                Path.Combine(expectedRoot, ".fsharp2-conformance-root")

                            Expect.isTrue
                                (File.Exists(markerPath))
                                $"The owned root has its marker: {expectedRoot}"

                            use marker = JsonDocument.Parse(File.ReadAllBytes(markerPath))

                            Expect.equal
                                (marker.RootElement.GetProperty("runId").GetString())
                                expectedRunId
                                "The marker records the exact run ID"

                            Expect.equal
                                (Path.GetFullPath(
                                    marker.RootElement.GetProperty("root").GetString()
                                ))
                                (Path.GetFullPath(expectedRoot))
                                "The marker records the canonical owned root"
                    )

            testCase "Conformance Lanes differ only by compiler selection and isolation roots"
            <| fun _ ->
                withCopiedRoot
                    "lanes-equality-guard"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let runId = Path.GetFileName(runRoot)
                        let _, _, compilerHostPath, plan = createPlan root runRoot runId

                        CoreCompileRunner.ValidateLaneEquality(plan)
                        |> expectValid

                        Expect.equal
                            plan.Oracle.Properties["UseFSharp2Compiler"]
                            "false"
                            "The Oracle lane selects the Compatibility Oracle"

                        Expect.equal
                            plan.FSharp2.Properties["UseFSharp2Compiler"]
                            "true"
                            "The FSharp2 lane selects FSharp2"

                        Expect.equal
                            plan.FSharp2.Properties["DisableAutoSetFscCompilerPath"]
                            "true"
                            "The FSharp2 lane disables SDK compiler-path selection"

                        Expect.equal
                            plan.FSharp2.Properties["FscToolPath"]
                            (Path.GetDirectoryName(compilerHostPath))
                            "The FSharp2 lane selects the strict host directory"

                        Expect.equal
                            plan.FSharp2.Properties["FscToolExe"]
                            (Path.GetFileName(compilerHostPath))
                            "The FSharp2 lane selects the strict host file"

                        Expect.equal
                            plan.FSharp2.Properties["FSharp2CompilerHostPath"]
                            compilerHostPath
                            "The FSharp2 lane retains the strict host path"

                        Expect.equal
                            plan.FSharp2.Properties["DotnetFscCompilerPath"]
                            ""
                            "The FSharp2 lane clears the SDK compiler path"

                        let mutatedInvocation =
                            LaneInvocation(
                                plan.FSharp2.Kind,
                                plan.FSharp2.ProjectPath,
                                plan.FSharp2.ProjectBytes,
                                plan.FSharp2.Inputs,
                                plan.FSharp2.Properties.SetItem("TargetFramework", "net9.0"),
                                plan.FSharp2.Environment,
                                plan.FSharp2.Arguments,
                                plan.FSharp2.CompilerHostPath
                            )

                        let mutated = CoreCompilePlan(plan.Oracle, mutatedInvocation)
                        let result = CoreCompileRunner.ValidateLaneEquality(mutated)

                        Expect.isFalse
                            result.IsValid
                            "A non-isolation property changed in one lane"

                        Expect.isNonEmpty
                            result.Issues
                            "The lane equality guard must record the difference"
                    )

            testCase "Conformance Lanes invoke one physical CoreCompile compiler task"
            <| fun _ ->
                withCopiedRoot
                    "lanes-one-corecompile"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let runId = Path.GetFileName(runRoot)

                        File.WriteAllText(
                            Path.Combine(runRoot, "Directory.Packages.props"),
                            """
<Project>
  <PropertyGroup>
    <ManagePackageVersionsCentrally>true</ManagePackageVersionsCentrally>
  </PropertyGroup>
</Project>
"""
                        )

                        File.WriteAllText(
                            Path.Combine(runRoot, "NuGet.Config"),
                            """
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
  <packageSourceMapping>
    <packageSource key="nuget.org">
      <package pattern="*" />
    </packageSource>
  </packageSourceMapping>
</configuration>
"""
                        )

                        let _, _, compilerHostPath, plan = createPlan root runRoot runId

                        let invocations = [
                            plan.Oracle
                            plan.FSharp2
                        ]

                        Expect.sequenceEqual
                            (invocations
                             |> List.map (fun invocation -> invocation.Kind))
                            [
                                LaneKind.Oracle
                                LaneKind.FSharp2
                            ]
                            "The physical CoreCompile test executes both external lanes"

                        for invocation in invocations do
                            let result = runLane invocation

                            try
                                Expect.equal
                                    result.Kind
                                    invocation.Kind
                                    "The result identifies the executed lane"

                                Expect.equal
                                    result.Process.ExitCode
                                    0
                                    $"The {invocation.Kind} CoreCompile process succeeds"

                                Expect.isFalse
                                    result.Process.TimedOut
                                    $"The {invocation.Kind} CoreCompile process does not time out"

                                Expect.equal
                                    result.Binlog.CoreCompileInvocationCount
                                    1
                                    $"The {invocation.Kind} binlog contains one physical compiler invocation"

                                Expect.isFalse
                                    result.Binlog.CoreCompileSkipped
                                    $"The {invocation.Kind} CoreCompile was not an MSBuild skip"

                                if invocation.Kind = LaneKind.FSharp2 then
                                    let lineage = ProcessLineage.Read(result.Process)

                                    let compilerProcesses =
                                        lineage
                                        |> Seq.filter (fun observation ->
                                            String.Equals(
                                                Path.GetFullPath(observation.ExecutablePath),
                                                compilerHostPath,
                                                StringComparison.OrdinalIgnoreCase
                                            )
                                        )
                                        |> Seq.toArray

                                    Expect.hasLength
                                        compilerProcesses
                                        1
                                        "The physical FSharp2 compiler invocation uses the selected strict host once"

                                    Expect.equal
                                        (CoreCompileRunner.Classify(result.Process))
                                        ConformanceVerdict.Pass
                                        "The physical FSharp2 compiler invocation does not use an SDK fsc fallback"

                                    Expect.isFalse
                                        (lineage
                                         |> Seq.exists (fun observation ->
                                             String.Equals(
                                                 Path.GetFullPath(observation.ExecutablePath),
                                                 testAssemblyPath,
                                                 StringComparison.OrdinalIgnoreCase
                                             )
                                             || (observation.Arguments
                                                 |> Seq.exists (fun argument ->
                                                     String.Equals(
                                                         argument,
                                                         testAssemblyPath,
                                                         StringComparison.OrdinalIgnoreCase
                                                     )
                                                 ))
                                         ))
                                        "The physical FSharp2 compiler invocation does not use the managed test assembly"
                            finally
                                stopRecordedProcesses result.Process.Processes
                    )

            testCase "Conformance Lanes select SDK 10.0.110 in every child"
            <| fun _ ->
                withCopiedRoot
                    "lanes-sdk-selection"
                    (fun root ->
                        let runRoot = Directory.GetParent(root).FullName
                        let runId = Path.GetFileName(runRoot)
                        let _, sdk, _, plan = createPlan root runRoot runId

                        let invocations = [
                            plan.Oracle
                            plan.FSharp2
                        ]

                        Expect.equal sdk.Version "10.0.110" "The locked SDK version is selected"

                        Expect.sequenceEqual
                            (invocations
                             |> List.map (fun invocation -> invocation.Kind))
                            [
                                LaneKind.Oracle
                                LaneKind.FSharp2
                            ]
                            "The SDK lineage test executes both external lanes"

                        for invocation in invocations do
                            Expect.equal
                                invocation.Environment["DOTNET_ROOT"]
                                sdk.Root
                                "Each lane inherits the selected SDK root"

                            Expect.equal
                                invocation.Environment["DOTNET_MULTILEVEL_LOOKUP"]
                                "0"
                                "Each lane disables multilevel lookup"

                            Expect.isTrue
                                (invocation.Environment["PATH"]
                                    .StartsWith(
                                        sdk.Root
                                        + string Path.PathSeparator,
                                        StringComparison.OrdinalIgnoreCase
                                    ))
                                "Each lane puts the selected SDK first on PATH"

                        for invocation in invocations do
                            let result = runLane invocation

                            try
                                Expect.equal
                                    result.Kind
                                    invocation.Kind
                                    "The result identifies the executed lane"

                                let lineage = ProcessLineage.Read(result.Process)

                                Expect.isNonEmpty
                                    lineage
                                    $"The {invocation.Kind} lane records process lineage"

                                let dotnetChildren =
                                    lineage
                                    |> Seq.filter (fun observation ->
                                        String.Equals(
                                            Path.GetFileName(observation.ExecutablePath),
                                            Path.GetFileName(sdk.DotnetPath),
                                            StringComparison.OrdinalIgnoreCase
                                        )
                                    )
                                    |> Seq.toArray

                                Expect.isNonEmpty
                                    dotnetChildren
                                    $"The {invocation.Kind} lane records the selected dotnet child"

                                for observation in dotnetChildren do
                                    Expect.equal
                                        (Path.GetFullPath(observation.ExecutablePath))
                                        (Path.GetFullPath(sdk.DotnetPath))
                                        $"The {invocation.Kind} dotnet child uses the selected SDK executable"
                            finally
                                stopRecordedProcesses result.Process.Processes
                    )

            testCase "Conformance Lanes classify a timeout as infra-error"
            <| fun _ ->
                withRoot
                    "lanes-timeout"
                    (fun runRoot ->
                        let childReceipt = Path.Combine(runRoot, "child.pid")
                        let standardOutput = Path.Combine(runRoot, "stdout.bin")
                        let standardError = Path.Combine(runRoot, "stderr.bin")

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
                                    "--conformance-test-child"
                                    childReceipt
                                ],
                                environment,
                                TimeSpan.FromMilliseconds(250.0),
                                standardOutput,
                                standardError
                            )

                        let result =
                            ProcessRunner
                                .RunAsync(specification, CancellationToken.None)
                                .GetAwaiter()
                                .GetResult()

                        try
                            Expect.isTrue result.TimedOut "The child exceeded the declared timeout"

                            Expect.equal
                                (CoreCompileRunner.Classify(result))
                                ConformanceVerdict.InfraError
                                "A harness timeout is infra-error"

                            for observation in result.Processes do
                                Expect.isFalse
                                    (isProcessRunning observation.ProcessId)
                                    $"Timed-out process remains alive: {observation.ProcessId}"
                        finally
                            stopRecordedProcesses result.Processes
                    )
        ]
