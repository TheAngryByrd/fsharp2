namespace fsharp2.Tests

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open System.Text
open Expecto

module CompilerTargetTests =
    type private InvocationResult = {
        ExitCode: int
        StandardOutput: string
        StandardError: string
    }

    let private repositoryRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    let private configuration =
        let releaseSegment =
            $"{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}"

        if
            AppContext.BaseDirectory.Contains(releaseSegment, StringComparison.OrdinalIgnoreCase)
        then
            "Release"
        else
            "Debug"

    let private compilerStartInfo arguments =
        let startInfo = ProcessStartInfo()
        let nativeExecutable = Environment.GetEnvironmentVariable("FSC2_EXECUTABLE")

        if String.IsNullOrWhiteSpace(nativeExecutable) then
            startInfo.FileName <- "dotnet"

            for argument in
                [
                    "run"
                    "--no-build"
                    "--configuration"
                    configuration
                    "--project"
                    $"{repositoryRoot}/src/fsc2.Prototype/fsc2.Prototype.fsproj"
                    "--"
                ] do
                startInfo.ArgumentList.Add(argument)
        else
            startInfo.FileName <- Path.GetFullPath(nativeExecutable)

        for argument in arguments do
            startInfo.ArgumentList.Add(argument)

        startInfo

    let private invokeFsc2 workingDirectory responsePath =
        let startInfo =
            compilerStartInfo [
                "@"
                + responsePath
            ]

        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEnd()
        let standardError = child.StandardError.ReadToEnd()

        if not (child.WaitForExit(30_000)) then
            child.Kill(true)
            failtest "fsc2 did not exit within 30 seconds"

        {
            ExitCode = child.ExitCode
            StandardOutput = standardOutput
            StandardError = standardError
        }

    let private invokeConsumer workingDirectory assemblyPath expected =
        let startInfo =
            ProcessStartInfo(
                "dotnet",
                $"run --no-build --configuration {configuration} --project \"{repositoryRoot}/tests/FSharp2.Prototype.Consumer/FSharp2.Prototype.Consumer.csproj\" -- \"{assemblyPath}\" {expected}"
            )

        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEnd()
        let standardError = child.StandardError.ReadToEnd()

        if not (child.WaitForExit(30_000)) then
            child.Kill(true)
            failtest "the downstream consumer did not exit within 30 seconds"

        {
            ExitCode = child.ExitCode
            StandardOutput = standardOutput
            StandardError = standardError
        }

    let private invokeIlVerify assemblyPath =
        let startInfo = ProcessStartInfo("dotnet")
        startInfo.ArgumentList.Add("ilverify")
        startInfo.ArgumentList.Add(assemblyPath)
        startInfo.ArgumentList.Add("--reference")

        startInfo.ArgumentList.Add(Path.Combine(RuntimeEnvironment.GetRuntimeDirectory(), "*.dll"))

        startInfo.WorkingDirectory <- repositoryRoot
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEnd()
        let standardError = child.StandardError.ReadToEnd()

        if not (child.WaitForExit(30_000)) then
            child.Kill(true)
            failtest "ILVerify did not exit within 30 seconds"

        {
            ExitCode = child.ExitCode
            StandardOutput = standardOutput
            StandardError = standardError
        }

    let private startCompilerService workingDirectory pipeName =
        let startInfo =
            compilerStartInfo [
                "--fsharp2-serve:"
                + pipeName
            ]

        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        let child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let ready = child.StandardOutput.ReadLineAsync()

        if not (ready.Wait(10_000)) then
            child.Kill(true)
            child.Dispose()
            failtest "the compiler service did not become ready within 10 seconds"

        Expect.equal
            ready.Result
            ("ready="
             + pipeName)
            "the compiler service should identify its endpoint"

        child

    let private readTrace path =
        File.ReadAllLines(path)
        |> Array.map (fun line ->
            let separator = line.IndexOf('=')

            line.Substring(0, separator),
            line.Substring(
                separator
                + 1
            )
        )
        |> Map.ofArray

    [<Tests>]
    let tests =
        testList "Compiler Target Invocation" [
            testCase "compiles a typed module with source-mapped debug output and executes it"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "Tracer.fs")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal result.ExitCode 0 result.StandardError

                    Expect.isTrue
                        (File.Exists outputPath)
                        "fsc2 should emit the implementation assembly"

                    Expect.isTrue
                        (File.Exists pdbPath)
                        "fsc2 should emit the requested portable PDB"

                    use pdbStream = File.OpenRead(pdbPath)
                    use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                    let pdb = pdbProvider.GetMetadataReader()
                    Expect.equal pdb.Documents.Count 1 "the PDB should contain the source document"

                    let document =
                        pdb.Documents
                        |> Seq.exactlyOne
                        |> pdb.GetDocument

                    Expect.equal
                        (pdb.GetString(document.Name))
                        "Tracer.fs"
                        "the document path should be root-independent"

                    let sequencePoints =
                        pdb
                            .GetMethodDebugInformation(MetadataTokens.MethodDefinitionHandle(1))
                            .GetSequencePoints()
                        |> Seq.toArray

                    Expect.equal
                        sequencePoints.Length
                        1
                        "the emitted declaration should have one sequence point"

                    Expect.equal
                        sequencePoints.[0].StartLine
                        2
                        "the sequence point should map to the declaration"

                    Expect.equal
                        sequencePoints.[0].StartColumn
                        5
                        "the sequence point should begin at the declaration name"

                    Expect.equal
                        sequencePoints.[0].EndLine
                        2
                        "the sequence point should end on the declaration line"

                    Expect.equal
                        sequencePoints.[0].EndColumn
                        19
                        "the sequence point should include the expression"

                    let consumer = invokeConsumer root outputPath 42
                    Expect.equal consumer.ExitCode 0 consumer.StandardError

                    Expect.equal
                        (consumer.StandardOutput.Trim())
                        "42"
                        "a downstream process should execute the emitted method"
                finally
                    Directory.Delete(root, true)

            testCase "emits structurally valid and verifiable IL"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "Tracer.fs")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    use implementationStream = File.OpenRead(outputPath)
                    use pe = new PEReader(implementationStream)
                    Expect.isTrue pe.HasMetadata "the implementation should contain CLI metadata"

                    Expect.isTrue
                        (pe.PEHeaders.CorHeader.Flags.HasFlag(CorFlags.ILOnly))
                        "the implementation should declare IL-only managed code"

                    let metadata = pe.GetMetadataReader()
                    Expect.isTrue metadata.IsAssembly "the implementation should be an assembly"

                    Expect.equal
                        (metadata.GetString(metadata.GetAssemblyDefinition().Name))
                        "Tracer"
                        "the assembly identity should match the requested output"

                    Expect.equal
                        metadata.TypeDefinitions.Count
                        2
                        "the tracer should have a module and one type"

                    Expect.equal
                        metadata.MethodDefinitions.Count
                        1
                        "the tracer should have one method"

                    let methodDefinition =
                        metadata.MethodDefinitions
                        |> Seq.exactlyOne
                        |> metadata.GetMethodDefinition

                    Expect.equal
                        (metadata.GetString(methodDefinition.Name))
                        "answer"
                        "the emitted method should preserve its source declaration name"

                    Expect.isTrue
                        (methodDefinition.Attributes.HasFlag(MethodAttributes.Public))
                        "the emitted method should be public"

                    Expect.isTrue
                        (methodDefinition.Attributes.HasFlag(MethodAttributes.Static))
                        "the emitted method should be static"

                    let methodBody = pe.GetMethodBody(methodDefinition.RelativeVirtualAddress)

                    Expect.isGreaterThanOrEqual
                        methodBody.MaxStack
                        1
                        "the method body should declare a sufficient max stack"

                    Expect.isTrue
                        methodBody.LocalSignature.IsNil
                        "the tracer method should have no locals"

                    Expect.equal
                        (methodBody.GetILBytes()
                         |> Array.last)
                        0x2auy
                        "the emitted method body should end in ret"

                    let verification = invokeIlVerify outputPath
                    Expect.equal verification.ExitCode 0 verification.StandardError

                    Expect.stringContains
                        verification.StandardOutput
                        "All Classes and Methods"
                        "ILVerify should verify every emitted type and method"
                finally
                    Directory.Delete(root, true)

            testCase "emits SourceLink as module custom debug information"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "Tracer.fs")
                    let sourceLinkPath = Path.Combine(root, "Tracer.sourcelink.json")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    let sourceLinkJson =
                        "{\"documents\":{\"Tracer.fs\":\"https://example.invalid/Tracer.fs\"}}"

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")
                    File.WriteAllText(sourceLinkPath, sourceLinkJson)

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--sourcelink:{sourceLinkPath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    use pdbStream = File.OpenRead(pdbPath)
                    use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                    let pdb = pdbProvider.GetMetadataReader()

                    let customDebugInformation =
                        pdb.CustomDebugInformation
                        |> Seq.map pdb.GetCustomDebugInformation
                        |> Seq.toArray

                    Expect.equal
                        customDebugInformation.Length
                        1
                        "the PDB should contain one SourceLink custom debug row"

                    let sourceLink = customDebugInformation.[0]

                    Expect.equal
                        sourceLink.Parent.Kind
                        HandleKind.ModuleDefinition
                        "SourceLink should be attached to the module"

                    Expect.equal
                        (pdb.GetGuid(sourceLink.Kind))
                        (Guid("cc110556-a091-4d38-9fec-25ab9a351a6a"))
                        "SourceLink should use the portable-PDB convention GUID"

                    Expect.equal
                        (pdb.GetBlobBytes(sourceLink.Value)
                         |> Encoding.UTF8.GetString)
                        sourceLinkJson
                        "the SourceLink payload should be preserved exactly"
                finally
                    Directory.Delete(root, true)

            testCase "emits byte-identical implementation and PDB artifacts across physical roots"
            <| fun _ ->
                let parent =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                let compile rootName =
                    let root = Path.Combine(parent, rootName)

                    Directory.CreateDirectory(root)
                    |> ignore

                    let sourcePath = Path.Combine(root, "Tracer.fs")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let tracePath = Path.Combine(root, "compile.trace")
                    let sourceLinkPath = Path.Combine(root, "Tracer.sourcelink.json")

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    File.WriteAllText(
                        sourceLinkPath,
                        "{\"documents\":{\"Tracer.fs\":\"https://example.invalid/Tracer.fs\"}}"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--sourcelink:{sourceLinkPath}"
                            $"--fsharp2-trace:{tracePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    File.ReadAllBytes(outputPath), File.ReadAllBytes(pdbPath), readTrace tracePath

                try
                    let leftAssembly, leftPdb, leftTrace = compile "left"
                    let rightAssembly, rightPdb, rightTrace = compile "right"

                    Expect.sequenceEqual
                        rightAssembly
                        leftAssembly
                        "physical source roots must not affect deterministic implementation bytes"

                    Expect.sequenceEqual
                        rightPdb
                        leftPdb
                        "physical source roots must not affect deterministic portable PDB bytes"

                    Expect.equal
                        rightTrace.["parseKey"]
                        leftTrace.["parseKey"]
                        "logical source identity and content should define the parsing action key"

                    Expect.equal
                        rightTrace.["checkKey"]
                        leftTrace.["checkKey"]
                        "semantic content should define the checking action key"

                    Expect.equal
                        rightTrace.["lowerKey"]
                        leftTrace.["lowerKey"]
                        "symbolic lowering should be independent of the physical source root"
                finally
                    Directory.Delete(parent, true)

            testCase "matches the milestone FS0010 and FS0001 oracle diagnostics"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                let runProbe sourceName expectedDiagnostic =
                    let sourcePath =
                        Path.Combine(
                            repositoryRoot,
                            "tests",
                            "FSharp2.Prototype.Diagnostics",
                            sourceName
                            + ".fs"
                        )

                    let outputPath =
                        Path.Combine(
                            root,
                            sourceName
                            + ".dll"
                        )

                    let pdbPath =
                        Path.Combine(
                            root,
                            sourceName
                            + ".pdb"
                        )

                    let responsePath =
                        Path.Combine(
                            root,
                            sourceName
                            + ".rsp"
                        )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--nologo"
                            "--target:library"
                            "--fullpaths"
                            "--flaterrors"
                            "--utf8output"
                            "--deterministic+"
                            "--debug:portable"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    let expected =
                        "\n"
                        + sourcePath
                        + expectedDiagnostic
                        + Environment.NewLine

                    Expect.equal
                        result.ExitCode
                        1
                        "the diagnostic should fail the Compiler Target Invocation"

                    Expect.equal
                        result.StandardOutput
                        String.Empty
                        "diagnostics should not be written to stdout"

                    Expect.equal
                        result.StandardError
                        expected
                        "the FS diagnostic should match the Compatibility Oracle"

                    Expect.isFalse
                        (File.Exists outputPath)
                        "a frontend error should not leave an implementation assembly"

                    Expect.isFalse
                        (File.Exists pdbPath)
                        "a frontend error should not leave a portable PDB"

                try
                    runProbe
                        "UnexpectedToken"
                        "(3,13): error FS0010: Unexpected symbol ')' in binding"

                    runProbe
                        "TypeMismatch"
                        "(3,18): error FS0001: This expression was expected to have type\u001d    'int'    \u001dbut here has type\u001d    'string'"
                finally
                    Directory.Delete(root, true)

            testCase "persistent service rechecks edits and reuses identical semantic queries"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                let pipeName =
                    "fsharp2-"
                    + Guid.NewGuid().ToString("N")

                use service = startCompilerService root pipeName

                try
                    let sourcePath = Path.Combine(root, "Tracer.fs")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")

                    let compile value traceName =
                        let responsePath =
                            Path.Combine(
                                root,
                                traceName
                                + ".rsp"
                            )

                        let tracePath =
                            Path.Combine(
                                root,
                                traceName
                                + ".trace"
                            )

                        File.WriteAllText(sourcePath, $"module Tracer\nlet answer () = {value}\n")

                        File.WriteAllLines(
                            responsePath,
                            [|
                                $"--fsharp2-server:{pipeName}"
                                $"--fsharp2-trace:{tracePath}"
                                "--target:library"
                                "--deterministic+"
                                "--debug:portable"
                                $"--out:{outputPath}"
                                $"--pdb:{pdbPath}"
                                sourcePath
                            |]
                        )

                        let result = invokeFsc2 root responsePath
                        Expect.equal result.ExitCode 0 result.StandardError
                        readTrace tracePath

                    let baseline = compile 42 "baseline"
                    let edited = compile 43 "edited"
                    let replay = compile 43 "replay"

                    Expect.equal
                        baseline.["querySchema"]
                        "1"
                        "query cache evidence should be versioned"

                    Expect.equal
                        baseline.["nodeKind"]
                        "source"
                        "the trace should identify the invalidated node kind"

                    Expect.equal baseline.["parse"] "miss" "the first source query should miss"
                    Expect.equal baseline.["check"] "miss" "the first typed query should miss"
                    Expect.equal baseline.["lower"] "miss" "the first lowering query should miss"

                    Expect.equal
                        baseline.["invalidationReason"]
                        "no-prior-state"
                        "the first request has no reusable semantic state"

                    Expect.equal edited.["parse"] "miss" "changed content should reparse"
                    Expect.equal edited.["check"] "miss" "changed content should recheck"
                    Expect.equal edited.["lower"] "miss" "changed implementation should re-lower"

                    Expect.equal
                        edited.["invalidationReason"]
                        "source-content-changed"
                        "the trace should explain why semantic queries missed"

                    Expect.equal
                        edited.["exportFingerprint"]
                        baseline.["exportFingerprint"]
                        "an implementation edit should preserve the exported fingerprint"

                    Expect.notEqual
                        edited.["fragmentHash"]
                        baseline.["fragmentHash"]
                        "an implementation edit should change the emitted fragment"

                    Expect.equal
                        edited.["previousContentFingerprint"]
                        baseline.["contentFingerprint"]
                        "the edit should name the semantic state it replaced"

                    Expect.equal replay.["parse"] "hit" "identical content should reuse parsing"
                    Expect.equal replay.["check"] "hit" "identical content should reuse checking"

                    Expect.equal
                        replay.["lower"]
                        "hit"
                        "identical content should reuse symbolic lowering"

                    Expect.equal
                        replay.["invalidationReason"]
                        "unchanged"
                        "an identical request should explain why queries were reusable"

                    Expect.equal
                        replay.["previousContentFingerprint"]
                        edited.["contentFingerprint"]
                        "replay should validate against the last successful content"

                    Expect.equal
                        replay.["parseKey"]
                        edited.["parseKey"]
                        "identical parsing inputs should have the same action key"

                    Expect.equal
                        replay.["checkKey"]
                        edited.["checkKey"]
                        "identical checking inputs should have the same action key"

                    Expect.equal
                        replay.["lowerKey"]
                        edited.["lowerKey"]
                        "identical lowering inputs should have the same action key"

                    Expect.equal
                        replay.["dependencyCount"]
                        "0"
                        "the tracer declaration has no semantic dependencies"

                    Expect.equal
                        replay.["emitted"]
                        "true"
                        "a cache hit still requires a fresh final SRM link"

                    for phase in
                        [
                            "parse"
                            "check"
                            "lower"
                            "link"
                            "publish"
                            "compile"
                        ] do
                        let elapsed =
                            Int64.Parse(
                                replay.[phase
                                        + "ElapsedMicroseconds"]
                            )

                        Expect.isGreaterThanOrEqual
                            elapsed
                            0L
                            $"{phase} timing should be non-negative"

                    let consumer = invokeConsumer root outputPath 43
                    Expect.equal consumer.ExitCode 0 consumer.StandardError

                    Expect.equal
                        (consumer.StandardOutput.Trim())
                        "43"
                        "the edited implementation must not reuse stale typed code"
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

                    Directory.Delete(root, true)
        ]
