namespace FSharp2.Conformance.Tests

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open Expecto
open FSharp2.Conformance

type TestAssemblyMarker = private TestAssemblyMarker of unit

type private NativePeBuilder() =
    inherit PEBuilder(PEHeaderBuilder.CreateExecutableHeader(), null)

    override _.CreateSections() =
        ImmutableArray.Create(
            PEBuilder.Section(
                ".text",
                SectionCharacteristics.ContainsCode
                ||| SectionCharacteristics.MemExecute
                ||| SectionCharacteristics.MemRead
            )
        )

    override _.SerializeSection(_, _) =
        let section = BlobBuilder()
        section.WriteByte(0xC3uy)
        section

    override _.GetDirectories() = PEDirectoriesBuilder()

module TestSupport =
    let repositoryRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    let conformanceRoot = Path.Combine(repositoryRoot, "tests", "conformance")

    let evidenceRoot =
        Path.Combine(
            repositoryRoot,
            ".omo",
            "teams",
            "team-cd15850d",
            "artifacts",
            "conformance-harness-tests"
        )

    let sdkRoot =
        [
            Environment.GetEnvironmentVariable("FSHARP2_DOTNET_ROOT")
            Environment.GetEnvironmentVariable("DOTNET_ROOT")
            Path.Combine(
                Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
                "fsharp2-sdk-10.0.110"
            )
        ]
        |> List.filter (
            String.IsNullOrWhiteSpace
            >> not
        )
        |> List.tryFind Directory.Exists
        |> Option.defaultWith (fun () ->
            failtest
                "No .NET SDK root is available through FSHARP2_DOTNET_ROOT, DOTNET_ROOT, or the local Issue #26 SDK root."
        )

    let dotnetPath =
        if OperatingSystem.IsWindows() then
            Path.Combine(sdkRoot, "dotnet.exe")
        else
            Path.Combine(sdkRoot, "dotnet")

    let testAssemblyPath = typeof<TestAssemblyMarker>.Assembly.Location

    let nativeExecutablePath directory stem =
        Path.Combine(
            directory,
            (if OperatingSystem.IsWindows() then
                 stem
                 + ".exe"
             else
                 stem)
        )

    let physicalLaneTestCase name body =
        testList "Conformance Lanes" [ testCase name body ]

    let ensureDirectory path =
        Directory.CreateDirectory(path)
        |> ignore

        path

    let private safeName (value: string) =
        value.Replace(' ', '-').Replace('/', '-').Replace('\\', '-').ToLowerInvariant()

    let createRunRoot name =
        let runId = $"test-{safeName name}-{Guid.NewGuid():N}"

        let runRoot =
            Path.Combine(Path.GetTempPath(), "fsharp2-conformance-tests", runId)
            |> Path.GetFullPath

        Directory.CreateDirectory(runRoot)
        |> ignore

        let marker = JsonSerializer.Serialize({| runId = runId; root = runRoot |})

        File.WriteAllText(
            Path.Combine(runRoot, ".fsharp2-conformance-root"),
            marker,
            UTF8Encoding(false)
        )

        runId, runRoot

    let createUnmarkedRoot name =
        let root =
            Path.Combine(
                Path.GetTempPath(),
                "fsharp2-conformance-tests",
                $"unmarked-{safeName name}-{Guid.NewGuid():N}"
            )
            |> Path.GetFullPath

        Directory.CreateDirectory(root)
        |> ignore

        root

    let rec copyDirectory source destination =
        Directory.CreateDirectory(destination)
        |> ignore

        for file in Directory.EnumerateFiles(source) do
            File.Copy(file, Path.Combine(destination, Path.GetFileName(file)), true)

        for directory in Directory.EnumerateDirectories(source) do
            copyDirectory directory (Path.Combine(destination, Path.GetFileName(directory)))

    let copyConformanceRoot name =
        if not (Directory.Exists(conformanceRoot)) then
            failtest $"Conformance data root does not exist: {conformanceRoot}"

        let _, runRoot = createRunRoot name
        let copiedRoot = Path.Combine(runRoot, "conformance")
        copyDirectory conformanceRoot copiedRoot
        runRoot, copiedRoot

    let deleteOwnedRoot root =
        if Directory.Exists(root) then
            let fullRoot = Path.GetFullPath(root)

            let expectedParent =
                Path.Combine(Path.GetTempPath(), "fsharp2-conformance-tests")
                |> Path.GetFullPath

            if not (fullRoot.StartsWith(expectedParent, StringComparison.OrdinalIgnoreCase)) then
                failtest $"Refusing to delete test root outside '{expectedParent}': {fullRoot}"

            Directory.Delete(fullRoot, true)

    let writeCleanupReceipt name facts =
        ensureDirectory evidenceRoot
        |> ignore

        let path =
            Path.Combine(evidenceRoot, $"cleanup-{safeName name}-{Guid.NewGuid():N}.json")

        File.WriteAllText(path, JsonSerializer.Serialize(facts), UTF8Encoding(false))
        path

    let cleanupRoot name root =
        let fullRoot = Path.GetFullPath(root)
        deleteOwnedRoot fullRoot

        let receipt =
            writeCleanupReceipt name {|
                root = fullRoot
                existsAfterCleanup = Directory.Exists(fullRoot)
                recordedAt = DateTimeOffset.UtcNow
            |}

        Expect.isFalse (Directory.Exists(fullRoot)) $"Cleanup receipt: {receipt}"
        receipt

    let readJson path =
        use document = JsonDocument.Parse(File.ReadAllBytes(path))
        document.RootElement.Clone()

    let writeJson path (value: JsonNode) =
        let options = JsonSerializerOptions(WriteIndented = true)

        File.WriteAllText(
            path,
            value.ToJsonString(options)
            + Environment.NewLine,
            UTF8Encoding(false)
        )

    let mutateJson path mutation =
        let root = JsonNode.Parse(File.ReadAllText(path))

        if isNull root then
            failtest $"JSON document is empty: {path}"

        mutation root
        writeJson path root

    let removeProperty path propertyName =
        mutateJson
            path
            (fun root ->
                match root with
                | :? JsonObject as value ->
                    value.Remove(propertyName)
                    |> ignore
                | _ -> failtest $"Expected an object root in {path}"
            )

    let addStringProperty (path: string) (propertyName: string) (propertyValue: string) =
        mutateJson
            path
            (fun root ->
                match root with
                | :? JsonObject as value -> value[propertyName] <- JsonValue.Create(propertyValue)
                | _ -> failtest $"Expected an object root in {path}"
            )

    let replaceString (path: string) (propertyName: string) (propertyValue: string) =
        mutateJson
            path
            (fun root ->
                match root with
                | :? JsonObject as value -> value[propertyName] <- JsonValue.Create(propertyValue)
                | _ -> failtest $"Expected an object root in {path}"
            )

    let firstCasePath root =
        let path =
            CaseDiscovery.Discover(root)
            |> Seq.tryHead
            |> Option.defaultWith (fun () ->
                failtest $"No conformance cases were discovered in {root}"
            )

        if Path.IsPathRooted(path) then
            path
        else
            Path.Combine(root, path)

    let caseById (repository: ConformanceRepository) caseId =
        repository.Cases
        |> Seq.tryFind (fun candidate -> candidate.CaseId = caseId)
        |> Option.defaultWith (fun () -> failtest $"Case '{caseId}' was not loaded")

    let removeCaseProbe root caseId probeKind =
        let casePath =
            Directory.EnumerateFiles(root, "*.case.json", SearchOption.AllDirectories)
            |> Seq.tryFind (fun path ->
                String.Equals(
                    (readJson path).GetProperty("caseId").GetString(),
                    caseId,
                    StringComparison.Ordinal
                )
            )
            |> Option.defaultWith (fun () -> failtest $"Case '{caseId}' was not found")

        mutateJson
            casePath
            (fun node ->
                let probes = ((node.AsObject())["probes"]).AsArray()

                let index =
                    probes
                    |> Seq.findIndex (fun probe ->
                        String.Equals(
                            ((probe.AsObject())["kind"]).GetValue<string>(),
                            probeKind,
                            StringComparison.Ordinal
                        )
                    )

                probes.RemoveAt(index)
            )

    let expectInvalidWith code (result: ValidationResult) =
        Expect.isFalse result.IsValid $"Expected validation issue '{code}'"

        result.Issues
        |> Seq.exists (fun issue -> issue.Code = code)
        |> fun found -> Expect.isTrue found $"Expected validation issue '{code}'"

    let expectValid (result: ValidationResult) =
        let details =
            result.Issues
            |> Seq.map (fun issue -> $"{issue.Code} {issue.Path}: {issue.Message}")
            |> String.concat Environment.NewLine

        Expect.isTrue result.IsValid details

    let fsharp2CompilerHostPath () =
        let configuredPath = Environment.GetEnvironmentVariable("FSharp2CompilerHostPath")

        if String.IsNullOrWhiteSpace(configuredPath) then
            failtest
                "Set FSharp2CompilerHostPath to the published NativeAOT fsc2 host before running physical lane tests."

        let hostPath = Path.GetFullPath(configuredPath)

        NativeAotHostValidator.Validate(hostPath)
        |> expectValid

        hostPath

    let immutableArray (values: 'T seq) = ImmutableArray.CreateRange(values)

    let immutableDictionary (values: (string * string) seq) =
        values
        |> Seq.map (fun (key, value) -> KeyValuePair<string, string>(key, value))
        |> ImmutableDictionary.CreateRange

    let bytes (value: string) =
        value
        |> Encoding.UTF8.GetBytes
        |> ImmutableArray.CreateRange

    let json (value: string) =
        use document = JsonDocument.Parse(value)
        document.RootElement.Clone()

    let jsonNodeToElement (value: JsonNode) = json (value.ToJsonString())

    let metadataAssembly typeNames =
        let metadata = MetadataBuilder()
        let moduleName = metadata.GetOrAddString("ComparatorFixture.dll")

        let moduleVersionId =
            metadata.GetOrAddGuid(Guid.Parse("25a828f2-f428-45a2-9393-72ed04ac513f"))

        metadata.AddModule(
            0,
            moduleName,
            moduleVersionId,
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        metadata.AddAssembly(
            metadata.GetOrAddString("ComparatorFixture"),
            System.Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.Sha256
        )
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString("<Module>"),
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle(1),
            MetadataTokens.MethodDefinitionHandle(1)
        )
        |> ignore

        for typeName in typeNames do
            metadata.AddTypeDefinition(
                TypeAttributes.Public
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.BeforeFieldInit,
                metadata.GetOrAddString("ComparatorFixture"),
                metadata.GetOrAddString(typeName),
                Unchecked.defaultof<EntityHandle>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )
            |> ignore

        let peBuilder =
            ManagedPEBuilder(
                PEHeaderBuilder.CreateLibraryHeader(),
                MetadataRootBuilder(metadata),
                BlobBuilder(),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly,
                null
            )

        let result = BlobBuilder()

        peBuilder.Serialize(result)
        |> ignore

        result.ToImmutableArray()

    let writeNativePeHost (path: string) =
        ensureDirectory (Path.GetDirectoryName(path))
        |> ignore

        let image = BlobBuilder()

        NativePeBuilder().Serialize(image)
        |> ignore

        File.WriteAllBytes(
            path,
            image.ToImmutableArray()
            |> Seq.toArray
        )

    let startTestChild receiptPath =
        let startInfo = ProcessStartInfo(dotnetPath)
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true
        startInfo.ArgumentList.Add(testAssemblyPath)
        startInfo.ArgumentList.Add("--conformance-test-child")
        startInfo.ArgumentList.Add(receiptPath)
        Process.Start(startInfo)

    let private hasObservedIdentity (observation: ProcessObservation) (childProcess: Process) =
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

    let stopRecordedProcesses observations =
        for observation: ProcessObservation in observations do
            try
                use childProcess = Process.GetProcessById(observation.ProcessId)

                if hasObservedIdentity observation childProcess then
                    childProcess.Kill(true)

                    if not (childProcess.WaitForExit(10000)) then
                        failtest $"Process {childProcess.Id} did not exit"
            with
            | :? ArgumentException
            | :? InvalidOperationException
            | :? System.ComponentModel.Win32Exception -> ()

    let waitForFile path timeout =
        let timer = Stopwatch.StartNew()

        while not (File.Exists(path))
              && timer.Elapsed < timeout do
            Thread.Sleep(20)

        Expect.isTrue (File.Exists(path)) $"Timed out waiting for {path}"

    let stopProcess (childProcess: Process) =
        if not childProcess.HasExited then
            childProcess.Kill(true)

        if not (childProcess.WaitForExit(10000)) then
            failtest $"Process {childProcess.Id} did not exit"

    let withRoot name action =
        let _, root = createRunRoot name

        try
            action root
        finally
            if Directory.Exists(root) then
                cleanupRoot name root
                |> ignore

    let withCopiedRoot name action =
        let runRoot, root = copyConformanceRoot name

        try
            action root
        finally
            if Directory.Exists(runRoot) then
                cleanupRoot name runRoot
                |> ignore
