namespace fsharp2.Tests

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open System.Security.Cryptography
open System.Text
open Expecto

module CompilerTargetTests =
    type private InvocationResult = {
        ExitCode: int
        StandardOutput: string
        StandardError: string
    }

    type private StrongNameArtifact = {
        Image: byte array
        PublicKey: byte array
        CorFlags: CorFlags
        SignatureOffset: int
        Signature: byte array
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

    let private invokeProcessStartInfo
        (timeoutMilliseconds: int)
        timeoutMessage
        (startInfo: ProcessStartInfo)
        =
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEndAsync()
        let standardError = child.StandardError.ReadToEndAsync()

        if not (child.WaitForExit(timeoutMilliseconds)) then
            child.Kill(true)
            failtest timeoutMessage

        {
            ExitCode = child.ExitCode
            StandardOutput = standardOutput.Result
            StandardError = standardError.Result
        }

    let private invokeFsc2 workingDirectory responsePath =
        let startInfo =
            compilerStartInfo [
                "@"
                + responsePath
            ]

        startInfo.WorkingDirectory <- workingDirectory

        invokeProcessStartInfo 30_000 "fsc2 did not exit within 30 seconds" startInfo

    let private invokeProcess workingDirectory timeoutMilliseconds fileName arguments =
        let startInfo = ProcessStartInfo(fileName)
        startInfo.WorkingDirectory <- workingDirectory

        for argument in arguments do
            startInfo.ArgumentList.Add(argument)

        invokeProcessStartInfo
            timeoutMilliseconds
            $"{fileName} did not exit within {timeoutMilliseconds} milliseconds"
            startInfo

    let private invokeConsumer workingDirectory assemblyPath expected =
        let startInfo =
            ProcessStartInfo(
                "dotnet",
                $"run --no-build --configuration {configuration} --project \"{repositoryRoot}/tests/FSharp2.Prototype.Consumer/FSharp2.Prototype.Consumer.csproj\" -- \"{assemblyPath}\" {expected}"
            )

        startInfo.WorkingDirectory <- workingDirectory

        invokeProcessStartInfo
            30_000
            "the downstream consumer did not exit within 30 seconds"
            startInfo

    let private invokeIlVerify assemblyPath =
        let startInfo = ProcessStartInfo("dotnet")
        startInfo.ArgumentList.Add("ilverify")
        startInfo.ArgumentList.Add(assemblyPath)
        startInfo.ArgumentList.Add("--reference")

        startInfo.ArgumentList.Add(Path.Combine(RuntimeEnvironment.GetRuntimeDirectory(), "*.dll"))

        startInfo.WorkingDirectory <- repositoryRoot
        invokeProcessStartInfo 30_000 "ILVerify did not exit within 30 seconds" startInfo

    let private startCompilerServiceProcess
        workingDirectory
        pipeName
        (startInfo: ProcessStartInfo)
        =
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

    let private startCompilerService workingDirectory pipeName =
        let startInfo =
            compilerStartInfo [
                "--fsharp2-serve:"
                + pipeName
            ]

        startCompilerServiceProcess workingDirectory pipeName startInfo

    let private startPackagedCompilerService workingDirectory executablePath pipeName =
        let startInfo = ProcessStartInfo(Path.GetFullPath(executablePath))

        startInfo.ArgumentList.Add(
            "--fsharp2-serve:"
            + pipeName
        )

        startCompilerServiceProcess workingDirectory pipeName startInfo

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

    let private rvaToFileOffset (headers: PEHeaders) relativeVirtualAddress =
        headers.SectionHeaders
        |> Seq.find (fun section ->
            let size = max section.VirtualSize section.SizeOfRawData

            relativeVirtualAddress
            >= section.VirtualAddress
            && relativeVirtualAddress < section.VirtualAddress
                                        + size
        )
        |> fun section ->
            relativeVirtualAddress
            - section.VirtualAddress
            + section.PointerToRawData

    let private readStrongNameArtifact path =
        let image = File.ReadAllBytes(path)
        use stream = new MemoryStream(image, false)
        use pe = new PEReader(stream)
        let metadata = pe.GetMetadataReader()
        let assembly = metadata.GetAssemblyDefinition()
        let directory = pe.PEHeaders.CorHeader.StrongNameSignatureDirectory

        let signatureOffset, signature =
            if directory.Size = 0 then
                0, Array.empty
            else
                let offset = rvaToFileOffset pe.PEHeaders directory.RelativeVirtualAddress

                offset,
                image.[offset .. offset
                                 + directory.Size
                                 - 1]

        {
            Image = image
            PublicKey = metadata.GetBlobBytes(assembly.PublicKey)
            CorFlags = pe.PEHeaders.CorHeader.Flags
            SignatureOffset = signatureOffset
            Signature = signature
        }

    let private verifyStrongNameSignature (rsa: RSA) artifact =
        let image = Array.copy artifact.Image
        Array.Clear(image, artifact.SignatureOffset, artifact.Signature.Length)

        let peHeaderOffset = BitConverter.ToInt32(image, 0x3c)

        let optionalHeaderOffset =
            peHeaderOffset
            + 24

        let optionalHeaderMagic = BitConverter.ToUInt16(image, optionalHeaderOffset)

        let dataDirectoriesOffset =
            if optionalHeaderMagic = 0x20bus then
                optionalHeaderOffset
                + 112
            else
                optionalHeaderOffset
                + 96

        Array.Clear(
            image,
            optionalHeaderOffset
            + 64,
            4
        )

        Array.Clear(
            image,
            dataDirectoriesOffset
            + 32,
            8
        )

        let optionalHeaderSize =
            int (
                BitConverter.ToUInt16(
                    image,
                    peHeaderOffset
                    + 20
                )
            )

        let sectionCount =
            int (
                BitConverter.ToUInt16(
                    image,
                    peHeaderOffset
                    + 6
                )
            )

        let sectionTableOffset =
            optionalHeaderOffset
            + optionalHeaderSize

        let peHeadersSize =
            sectionTableOffset
            + sectionCount
              * 40

        let firstSectionOffset =
            BitConverter.ToInt32(
                image,
                sectionTableOffset
                + 20
            )

        use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA1)
        hash.AppendData(image.AsSpan(0, peHeadersSize))

        hash.AppendData(
            image.AsSpan(
                firstSectionOffset,
                artifact.SignatureOffset
                - firstSectionOffset
            )
        )

        let signatureEnd =
            artifact.SignatureOffset
            + artifact.Signature.Length

        hash.AppendData(image.AsSpan(signatureEnd))
        let digest = hash.GetHashAndReset()
        let signature = Array.rev artifact.Signature

        rsa.VerifyHash(digest, signature, HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1)

    let private writeCapiPrivateKey path (parameters: RSAParameters) =
        let writeLittleEndian (writer: BinaryWriter) (bytes: byte array) =
            writer.Write(Array.rev bytes)

        let exponent =
            parameters.Exponent
            |> Array.fold
                (fun value part ->
                    (value
                     <<< 8)
                    ||| uint32 part
                )
                0u

        use stream = File.Create(path)
        use writer = new BinaryWriter(stream)
        writer.Write(0x07uy)
        writer.Write(0x02uy)
        writer.Write(0us)
        writer.Write(0x00002400u)
        writer.Write(0x32415352u)

        writer.Write(
            uint32 (
                parameters.Modulus.Length
                * 8
            )
        )

        writer.Write(exponent)
        writeLittleEndian writer parameters.Modulus
        writeLittleEndian writer parameters.P
        writeLittleEndian writer parameters.Q
        writeLittleEndian writer parameters.DP
        writeLittleEndian writer parameters.DQ
        writeLittleEndian writer parameters.InverseQ
        writeLittleEndian writer parameters.D

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

                    Expect.equal
                        result.StandardOutput
                        String.Empty
                        "successful compilation should be silent"

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

            testCase
                "accepts the IcedTasks CoreCompile argument subset and preserves ordered modules"
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
                    let firstSourcePath = Path.Combine(root, "First.fs")
                    let tracerSourcePath = Path.Combine(root, "Tracer.fs")
                    let sourceLinkPath = Path.Combine(root, "Tracer.sourcelink.json")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let referenceOutputPath = Path.Combine(root, "ref", "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let documentationPath = Path.Combine(root, "Tracer.xml")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let tracePath = Path.Combine(root, "compile.trace")

                    File.WriteAllText(firstSourcePath, "module First\nlet first () = 1\n")
                    File.WriteAllText(tracerSourcePath, "module Tracer\nlet answer () = 42\n")

                    File.WriteAllText(
                        sourceLinkPath,
                        "{\"documents\":{\"*.fs\":\"https://example.invalid/*.fs\"}}"
                    )

                    let systemRuntime =
                        Path.Combine(RuntimeEnvironment.GetRuntimeDirectory(), "System.Runtime.dll")

                    File.WriteAllLines(
                        responsePath,
                        [|
                            $"-o:{outputPath}"
                            "--debug:portable"
                            $"--embed:{firstSourcePath}"
                            $"--sourcelink:{sourceLinkPath}"
                            "--langversion:9.0"
                            "--noframework"
                            "--define:TRACE"
                            "--define:RELEASE"
                            $"--doc:{documentationPath}"
                            "--optimize+"
                            "--checknulls+"
                            "--define:NULLABLE"
                            $"-r:{systemRuntime}"
                            "--target:library"
                            "--nowarn:FS0057,NU5104,FS3513"
                            "--warn:3"
                            "--warnaserror"
                            "--warnaserror:3239"
                            "--fullpaths"
                            "--flaterrors"
                            "--highentropyva+"
                            "--targetprofile:netcore"
                            "--nocopyfsharpcore"
                            "--deterministic+"
                            "--simpleresolution"
                            "--test:GraphBasedChecking"
                            "--test:ParallelIlxGen"
                            "--test:ParallelOptimization"
                            $"--refout:{referenceOutputPath}"
                            $"--fsharp2-trace:{tracePath}"
                            firstSourcePath
                            tracerSourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError
                    Expect.isTrue (File.Exists outputPath) "the implementation DLL should exist"
                    Expect.isTrue (File.Exists pdbPath) "the portable PDB should exist"

                    Expect.isTrue
                        (File.Exists referenceOutputPath)
                        "the requested reference assembly should exist"

                    Expect.isTrue
                        (File.Exists documentationPath)
                        "the requested XML documentation should exist"

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let typeNames =
                        metadata.TypeDefinitions
                        |> Seq.map (fun handle ->
                            handle
                            |> metadata.GetTypeDefinition
                            |> fun definition -> metadata.GetString(definition.Name)
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        typeNames
                        [|
                            "<Module>"
                            "First"
                            "Tracer"
                        |]
                        "source order should determine emitted module-type order"

                    use referenceStream = File.OpenRead(referenceOutputPath)
                    use referenceAssembly = new PEReader(referenceStream)

                    Expect.equal
                        (referenceAssembly.GetMetadataReader().TypeDefinitions.Count)
                        3
                        "the reference output should expose both compiled modules"

                    use pdbStream = File.OpenRead(pdbPath)
                    use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                    let pdb = pdbProvider.GetMetadataReader()

                    Expect.equal
                        pdb.Documents.Count
                        2
                        "each ordered source should have a portable-PDB document"

                    let documentNames =
                        pdb.Documents
                        |> Seq.map (
                            pdb.GetDocument
                            >> fun document -> pdb.GetString(document.Name)
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        documentNames
                        [|
                            "First.fs"
                            "Tracer.fs"
                        |]
                        "PDB documents should preserve source order"

                    let methodDocumentNames = [|
                        for methodRow in 1 .. metadata.MethodDefinitions.Count do
                            let methodDebugInformation =
                                MetadataTokens.MethodDefinitionHandle(methodRow)
                                |> pdb.GetMethodDebugInformation

                            methodDebugInformation.Document
                            |> pdb.GetDocument
                            |> fun document -> pdb.GetString(document.Name)
                    |]

                    Expect.sequenceEqual
                        methodDocumentNames
                        documentNames
                        "method debug rows should point to their source-order documents"

                    let trace = readTrace tracePath

                    Expect.equal
                        trace.["sourceCount"]
                        "2"
                        "the trace should record source cardinality"

                    Expect.equal
                        trace.["referenceCount"]
                        "1"
                        "the trace should record the explicit reference closure"

                    Expect.stringContains
                        (File.ReadAllText(documentationPath))
                        "<name>Tracer</name>"
                        "XML documentation should identify the compiled assembly"

                    let consumer = invokeConsumer root outputPath 42
                    Expect.equal consumer.ExitCode 0 consumer.StandardError
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
                            $"--pathmap:{root}=/mapped"
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

                    let document =
                        pdb.Documents
                        |> Seq.exactlyOne
                        |> pdb.GetDocument

                    Expect.equal
                        (pdb.GetString(document.Name))
                        "/mapped/Tracer.fs"
                        "the document path should honor the requested path map"

                    let importScopeHandle =
                        pdb.ImportScopes
                        |> Seq.exactlyOne

                    let importScope = pdb.GetImportScope(importScopeHandle)

                    let importDefinition =
                        importScope.GetImports()
                        |> Seq.exactlyOne

                    Expect.equal
                        importDefinition.Kind
                        ImportDefinitionKind.ImportNamespace
                        "the PDB should encode the imported namespace kind"

                    Expect.equal
                        (pdb.GetBlobBytes(importDefinition.TargetNamespace)
                         |> Encoding.UTF8.GetString)
                        "System"
                        "the PDB should preserve the imported namespace"

                    let localScope =
                        pdb.LocalScopes
                        |> Seq.exactlyOne
                        |> pdb.GetLocalScope

                    Expect.equal
                        localScope.ImportScope
                        importScopeHandle
                        "the method scope should reference the encoded imports"

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

                    let pdbBytes = File.ReadAllBytes(pdbPath)

                    let pdbIdBytes =
                        pdb.DebugMetadataHeader.Id
                        |> Seq.toArray

                    let pdbIdOffsets =
                        [|
                            0 .. pdbBytes.Length
                                 - pdbIdBytes.Length
                        |]
                        |> Array.filter (fun offset ->
                            [|
                                0 .. pdbIdBytes.Length
                                     - 1
                            |]
                            |> Array.forall (fun index ->
                                pdbBytes.[offset
                                          + index] = pdbIdBytes.[index]
                            )
                        )

                    Expect.equal
                        pdbIdOffsets.Length
                        1
                        "the portable PDB should contain one patchable content-id slot"

                    let preIdPdb = Array.copy pdbBytes
                    Array.Clear(preIdPdb, pdbIdOffsets.[0], pdbIdBytes.Length)
                    let preIdDigest = SHA256.HashData(preIdPdb)
                    let expectedPdbId = BlobContentId.FromHash(preIdDigest)

                    Expect.equal
                        (Guid(pdbIdBytes.[0..15]))
                        expectedPdbId.Guid
                        "the PDB header id should derive from the pre-id digest"

                    Expect.equal
                        (BitConverter.ToUInt32(pdbIdBytes, 16))
                        expectedPdbId.Stamp
                        "the PDB header stamp should derive from the pre-id digest"

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)

                    let debugEntries =
                        implementation.ReadDebugDirectory()
                        |> Seq.toArray

                    let debugEntry entryType =
                        debugEntries
                        |> Seq.filter (fun entry -> entry.Type = entryType)
                        |> Seq.exactlyOne

                    let codeViewEntry = debugEntry DebugDirectoryEntryType.CodeView
                    let codeView = implementation.ReadCodeViewDebugDirectoryData(codeViewEntry)

                    Expect.equal
                        codeView.Path
                        "Tracer.pdb"
                        "CodeView should name the root-independent PDB"

                    Expect.equal
                        codeView.Guid
                        expectedPdbId.Guid
                        "CodeView should reference the emitted portable PDB id"

                    Expect.equal
                        codeViewEntry.Stamp
                        expectedPdbId.Stamp
                        "CodeView should carry the emitted portable PDB stamp"

                    let checksumEntry = debugEntry DebugDirectoryEntryType.PdbChecksum

                    let checksum = implementation.ReadPdbChecksumDebugDirectoryData(checksumEntry)

                    Expect.equal
                        checksum.AlgorithmName
                        "SHA256"
                        "the debug directory should declare the requested checksum algorithm"

                    Expect.sequenceEqual
                        (checksum.Checksum
                         |> Seq.toArray)
                        preIdDigest
                        "the debug-directory checksum should preserve the pre-id PDB digest"

                    let reproducibleEntry = debugEntry DebugDirectoryEntryType.Reproducible

                    Expect.equal
                        reproducibleEntry.DataSize
                        0
                        "the deterministic image should carry an empty reproducible marker"
                finally
                    Directory.Delete(root, true)

            testCase "emits managed and harness-native resources"
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
                    let managedResourcePath = Path.Combine(root, "payload.bin")
                    let nativeResourcePath = Path.Combine(root, "native.bin")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    let managedPayload = [|
                        0x46uy
                        0x53uy
                        0x32uy
                        0x4duy
                    |]

                    let nativePayload = [|
                        0x46uy
                        0x53uy
                        0x32uy
                        0x4euy
                    |]

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")
                    File.WriteAllBytes(managedResourcePath, managedPayload)
                    File.WriteAllBytes(nativeResourcePath, nativePayload)

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--resource:{managedResourcePath},Tracer.Payload,public"
                            $"--fsharp2-native-resource:{nativeResourcePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    use stream = File.OpenRead(outputPath)
                    use pe = new PEReader(stream)
                    let metadata = pe.GetMetadataReader()

                    let resource =
                        metadata.ManifestResources
                        |> Seq.exactlyOne
                        |> metadata.GetManifestResource

                    Expect.equal
                        (metadata.GetString(resource.Name))
                        "Tracer.Payload"
                        "the managed resource should retain its logical name"

                    Expect.equal
                        resource.Attributes
                        ManifestResourceAttributes.Public
                        "the managed resource should retain its visibility"

                    Expect.isTrue
                        resource.Implementation.IsNil
                        "the managed resource should be embedded"

                    let managedDirectory = pe.PEHeaders.CorHeader.ResourcesDirectory

                    let managedSection =
                        pe
                            .GetSectionData(managedDirectory.RelativeVirtualAddress)
                            .GetContent(0, managedDirectory.Size)
                        |> Seq.toArray

                    Expect.equal
                        (BitConverter.ToInt32(managedSection, int resource.Offset))
                        managedPayload.Length
                        "the managed stream should prefix the payload length"

                    Expect.sequenceEqual
                        managedSection.[int resource.Offset
                                        + 4 .. int resource.Offset
                                               + 3
                                               + managedPayload.Length]
                        managedPayload
                        "the managed payload should be preserved exactly"

                    let nativeDirectory = pe.PEHeaders.PEHeader.ResourceTableDirectory

                    let nativeSection =
                        pe
                            .GetSectionData(nativeDirectory.RelativeVirtualAddress)
                            .GetContent(0, nativeDirectory.Size)
                        |> Seq.toArray

                    let readUInt32 offset =
                        BitConverter.ToUInt32(nativeSection, offset)

                    Expect.equal (readUInt32 16) 10u "the native resource type should be RCDATA"

                    let typeDirectoryOffset =
                        int (
                            readUInt32 20
                            &&& 0x7fffffffu
                        )

                    Expect.equal
                        (readUInt32 (
                            typeDirectoryOffset
                            + 16
                        ))
                        1u
                        "the native resource should use the harness name id"

                    let nameDirectoryOffset =
                        int (
                            readUInt32 (
                                typeDirectoryOffset
                                + 20
                            )
                            &&& 0x7fffffffu
                        )

                    Expect.equal
                        (readUInt32 (
                            nameDirectoryOffset
                            + 16
                        ))
                        0u
                        "the native resource should use the neutral language id"

                    let dataEntryOffset =
                        int (
                            readUInt32 (
                                nameDirectoryOffset
                                + 20
                            )
                            &&& 0x7fffffffu
                        )

                    let payloadOffset =
                        int (readUInt32 dataEntryOffset)
                        - nativeDirectory.RelativeVirtualAddress

                    let payloadLength =
                        int (
                            readUInt32 (
                                dataEntryOffset
                                + 4
                            )
                        )

                    Expect.sequenceEqual
                        nativeSection.[payloadOffset .. payloadOffset
                                                        + payloadLength
                                                        - 1]
                        nativePayload
                        "the native payload should be preserved exactly"
                finally
                    Directory.Delete(root, true)

            testCase "emits unsigned, delay-signed, public-signed, and fully signed assemblies"
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
                    let keyPath = Path.Combine(root, "Tracer.snk")
                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    use rsa = RSA.Create()
                    rsa.KeySize <- 2048
                    writeCapiPrivateKey keyPath (rsa.ExportParameters(true))

                    let compile name extraArguments =
                        let outputPath =
                            Path.Combine(
                                root,
                                name
                                + ".dll"
                            )

                        let pdbPath =
                            Path.Combine(
                                root,
                                name
                                + ".pdb"
                            )

                        let responsePath =
                            Path.Combine(
                                root,
                                name
                                + ".rsp"
                            )

                        let arguments =
                            [|
                                "--target:library"
                                "--deterministic+"
                                "--debug:portable"
                                $"--out:{outputPath}"
                                $"--pdb:{pdbPath}"
                            |]
                            |> Array.append (Array.ofList extraArguments)
                            |> Array.append [| sourcePath |]

                        File.WriteAllLines(responsePath, arguments)
                        let result = invokeFsc2 root responsePath
                        Expect.equal result.ExitCode 0 result.StandardError
                        outputPath

                    let unsignedPath = compile "unsigned" []

                    let delayPath =
                        compile "delay" [
                            $"--keyfile:{keyPath}"
                            "--delaysign+"
                        ]

                    let publicPath =
                        compile "public" [
                            $"--keyfile:{keyPath}"
                            "--publicsign+"
                        ]

                    let fullPath = compile "full" [ $"--keyfile:{keyPath}" ]
                    let firstFullImage = File.ReadAllBytes(fullPath)
                    let fullRepeatPath = compile "full" [ $"--keyfile:{keyPath}" ]
                    let unsigned = readStrongNameArtifact unsignedPath
                    let delay = readStrongNameArtifact delayPath
                    let publicSigned = readStrongNameArtifact publicPath
                    let full = readStrongNameArtifact fullPath

                    Expect.isEmpty
                        unsigned.PublicKey
                        "unsigned output should not carry a public key"

                    Expect.isEmpty
                        unsigned.Signature
                        "unsigned output should not reserve a signature"

                    for artifact in
                        [
                            delay
                            publicSigned
                            full
                        ] do
                        Expect.isGreaterThan
                            artifact.PublicKey.Length
                            0
                            "signed modes should carry the complete CLR public key"

                        Expect.equal
                            artifact.Signature.Length
                            256
                            "signed modes should reserve the key modulus size"

                    Expect.sequenceEqual
                        delay.PublicKey
                        full.PublicKey
                        "delay and full signing should use the same assembly identity"

                    Expect.sequenceEqual
                        publicSigned.PublicKey
                        full.PublicKey
                        "public and full signing should use the same assembly identity"

                    Expect.isTrue
                        (delay.Signature
                         |> Array.forall ((=) 0uy))
                        "delay signing should leave the signature slot zeroed"

                    Expect.isTrue
                        (publicSigned.Signature
                         |> Array.forall ((=) 0uy))
                        "public signing should leave the signature slot zeroed"

                    Expect.isTrue
                        (full.Signature
                         |> Array.exists ((<>) 0uy))
                        "full signing should populate the signature slot"

                    Expect.isFalse
                        (delay.CorFlags.HasFlag(CorFlags.StrongNameSigned))
                        "delay signing should leave the strong-name flag clear"

                    Expect.isTrue
                        (publicSigned.CorFlags.HasFlag(CorFlags.StrongNameSigned))
                        "public signing should set the strong-name flag"

                    Expect.isTrue
                        (full.CorFlags.HasFlag(CorFlags.StrongNameSigned))
                        "full signing should set the strong-name flag"

                    Expect.isTrue
                        (verifyStrongNameSignature rsa full)
                        "the full strong-name signature should verify with the key"

                    Expect.sequenceEqual
                        firstFullImage
                        (File.ReadAllBytes(fullRepeatPath))
                        "full signing should remain deterministic"

                    let consumer = invokeConsumer root fullPath 42
                    Expect.equal consumer.ExitCode 0 consumer.StandardError
                finally
                    Directory.Delete(root, true)

            testCase
                "rejects malformed strong-name keys transactionally and keeps the service healthy"
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
                    let validKeyPath = Path.Combine(root, "valid.snk")
                    let truncatedKeyPath = Path.Combine(root, "truncated.snk")
                    let trailingKeyPath = Path.Combine(root, "trailing.snk")
                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    use rsa = RSA.Create()
                    rsa.KeySize <- 2048
                    writeCapiPrivateKey validKeyPath (rsa.ExportParameters(true))
                    let validKey = File.ReadAllBytes(validKeyPath)

                    File.WriteAllBytes(
                        truncatedKeyPath,
                        validKey.[.. validKey.Length
                                     - 2]
                    )

                    File.WriteAllBytes(trailingKeyPath, Array.append validKey [| 0uy |])

                    let compile name keyPath =
                        let outputPath =
                            Path.Combine(
                                root,
                                name
                                + ".dll"
                            )

                        let pdbPath =
                            Path.Combine(
                                root,
                                name
                                + ".pdb"
                            )

                        let responsePath =
                            Path.Combine(
                                root,
                                name
                                + ".rsp"
                            )

                        File.WriteAllLines(
                            responsePath,
                            [|
                                $"--fsharp2-server:{pipeName}"
                                "--target:library"
                                "--deterministic+"
                                "--debug:portable"
                                $"--keyfile:{keyPath}"
                                $"--out:{outputPath}"
                                $"--pdb:{pdbPath}"
                                sourcePath
                            |]
                        )

                        outputPath, pdbPath, invokeFsc2 root responsePath

                    for name, keyPath in
                        [
                            "truncated", truncatedKeyPath
                            "trailing", trailingKeyPath
                        ] do
                        let outputPath, pdbPath, result = compile name keyPath

                        Expect.equal
                            result.ExitCode
                            1
                            $"the {name} private key should fail explicitly"

                        Expect.stringContains
                            result.StandardError
                            "strong-name key"
                            $"the {name} private-key failure should identify the rejected input"

                        Expect.isFalse
                            (File.Exists outputPath)
                            $"the {name} private-key failure must not publish an assembly"

                        Expect.isFalse
                            (File.Exists pdbPath)
                            $"the {name} private-key failure must not publish a PDB"

                    let validOutput, _, validResult = compile "valid" validKeyPath
                    Expect.equal validResult.ExitCode 0 validResult.StandardError

                    let consumer = invokeConsumer root validOutput 42
                    Expect.equal consumer.ExitCode 0 consumer.StandardError
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

                    Directory.Delete(root, true)

            testCase "a late publish failure restores the previous artifact set"
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
                    let referenceOutputPath = Path.Combine(root, "ref", "Tracer.dll")
                    let documentationPath = Path.Combine(root, "Tracer.xml")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    let originalPdb = [|
                        0x46uy
                        0x53uy
                        0x32uy
                    |]

                    let originalReference = [|
                        0x52uy
                        0x45uy
                        0x46uy
                    |]

                    let originalDocumentation = "<original />"

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    Directory.CreateDirectory(outputPath)
                    |> ignore

                    Directory.CreateDirectory(Path.GetDirectoryName(referenceOutputPath))
                    |> ignore

                    File.WriteAllBytes(pdbPath, originalPdb)
                    File.WriteAllBytes(referenceOutputPath, originalReference)
                    File.WriteAllText(documentationPath, originalDocumentation)

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            $"--refout:{referenceOutputPath}"
                            $"--doc:{documentationPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 1 "the invalid final assembly target should fail"

                    Expect.stringContains
                        result.StandardError
                        "FSC2P9999"
                        "the publish failure should be explicit"

                    Expect.isTrue
                        (Directory.Exists outputPath)
                        "the pre-existing target directory should remain"

                    Expect.sequenceEqual
                        (File.ReadAllBytes(pdbPath))
                        originalPdb
                        "the previous PDB should be restored when assembly publication fails"

                    Expect.sequenceEqual
                        (File.ReadAllBytes(referenceOutputPath))
                        originalReference
                        "the previous reference DLL should be restored when assembly publication fails"

                    Expect.equal
                        (File.ReadAllText(documentationPath))
                        originalDocumentation
                        "the previous XML documentation should be restored when assembly publication fails"

                    let transactionFiles =
                        Directory.GetFiles(root, "*", SearchOption.AllDirectories)
                        |> Array.filter (fun path ->
                            path.Contains(".fsc2-", StringComparison.Ordinal)
                        )

                    Expect.isEmpty
                        transactionFiles
                        "failed publication should clean temporary and backup files"
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
                            $"--pathmap:{root}=/mapped"
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

            testCase
                "standalone and service failures expose equivalent diagnostics and trace decisions"
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

                let compile name serverName =
                    let sourcePath =
                        Path.Combine(
                            repositoryRoot,
                            "tests",
                            "FSharp2.Prototype.Diagnostics",
                            "UnexpectedToken.fs"
                        )

                    let responsePath =
                        Path.Combine(
                            root,
                            name
                            + ".rsp"
                        )

                    let tracePath =
                        Path.Combine(
                            root,
                            name
                            + ".trace"
                        )

                    let outputPath =
                        Path.Combine(
                            root,
                            name
                            + ".dll"
                        )

                    let pdbPath =
                        Path.Combine(
                            root,
                            name
                            + ".pdb"
                        )

                    let arguments = ResizeArray<string>()

                    serverName
                    |> Option.iter (fun value -> arguments.Add($"--fsharp2-server:{value}"))

                    for argument in
                        [
                            "--nologo"
                            "--target:library"
                            "--fullpaths"
                            "--flaterrors"
                            "--utf8output"
                            "--deterministic+"
                            "--debug:portable"
                            $"--fsharp2-trace:{tracePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        ] do
                        arguments.Add(argument)

                    File.WriteAllLines(responsePath, arguments)
                    invokeFsc2 root responsePath, readTrace tracePath

                try
                    let standalone, standaloneTrace = compile "standalone" None
                    let remote, remoteTrace = compile "remote" (Some pipeName)

                    Expect.equal remote.ExitCode standalone.ExitCode "failure exits should agree"

                    Expect.equal
                        remote.StandardOutput
                        standalone.StandardOutput
                        "failure stdout should agree"

                    Expect.equal
                        remote.StandardError
                        standalone.StandardError
                        "failure stderr should agree"

                    for field in
                        [
                            "nodeKind"
                            "invalidationReason"
                            "parse"
                            "check"
                            "lower"
                            "emitted"
                        ] do
                        Expect.equal
                            remoteTrace.[field]
                            standaloneTrace.[field]
                            $"failure trace field '{field}' should agree across execution paths"
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

                    Directory.Delete(root, true)

            testCase "persistent service cache reuse keeps diagnostic paths request-local"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-prototype",
                        Guid.NewGuid().ToString("N")
                    )

                let firstRoot = Path.Combine(root, "first")
                let secondRoot = Path.Combine(root, "second")

                Directory.CreateDirectory(firstRoot)
                |> ignore

                Directory.CreateDirectory(secondRoot)
                |> ignore

                let pipeName =
                    "fsharp2-"
                    + Guid.NewGuid().ToString("N")

                use service = startCompilerService root pipeName

                let compile sourceRoot =
                    let sourcePath = Path.Combine(sourceRoot, "TypeMismatch.fs")
                    let outputPath = Path.Combine(sourceRoot, "TypeMismatch.dll")
                    let pdbPath = Path.Combine(sourceRoot, "TypeMismatch.pdb")
                    let responsePath = Path.Combine(sourceRoot, "compile.rsp")

                    File.WriteAllText(
                        sourcePath,
                        "module NegativeDiagnostics\n\nlet value: int = \"text\"\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            $"--fsharp2-server:{pipeName}"
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

                    sourcePath, invokeFsc2 sourceRoot responsePath

                try
                    let firstSourcePath, first = compile firstRoot
                    let secondSourcePath, second = compile secondRoot

                    Expect.equal first.ExitCode 1 "the first semantic failure should be reported"
                    Expect.equal second.ExitCode 1 "the reused semantic failure should be reported"

                    Expect.stringContains
                        first.StandardError
                        firstSourcePath
                        "the first request should use its own physical source path"

                    Expect.stringContains
                        second.StandardError
                        secondSourcePath
                        "the second request should rebind diagnostics to its physical source path"

                    Expect.isFalse
                        (second.StandardError.Contains(firstSourcePath, StringComparison.Ordinal))
                        "root-neutral cached syntax must not leak the first request path"
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

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

                    let compile (sourceText: string) traceName =
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

                        File.WriteAllText(sourcePath, sourceText)

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

                    let baselineSource = "module Tracer\nlet answer () = 42\n"
                    let editedSource = "module Tracer\nlet answer () = 43\n"
                    let relocatedSource = "module Tracer\n\nlet answer () = 43\n"
                    let baseline = compile baselineSource "baseline"
                    let edited = compile editedSource "edited"
                    let replay = compile editedSource "replay"
                    let relocated = compile relocatedSource "relocated"

                    let servicePid = baseline.["servicePid"]

                    Expect.notEqual
                        servicePid
                        (Environment.ProcessId.ToString())
                        "warm requests should be handled outside the compiler client process"

                    for traceName, trace in
                        [
                            "baseline", baseline
                            "edited", edited
                            "replay", replay
                            "relocated", relocated
                        ] do
                        Expect.equal
                            trace.["servicePid"]
                            servicePid
                            $"the {traceName} request should be handled by the retained compiler service"

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
                        relocated.["parse"]
                        "miss"
                        "a layout edit should reparse the changed source"

                    Expect.equal
                        relocated.["check"]
                        "miss"
                        "a layout edit should recheck the changed source"

                    Expect.equal
                        relocated.["lower"]
                        "miss"
                        "a layout edit should rebuild its changed debug contribution"

                    Expect.notEqual
                        relocated.["lowerKey"]
                        replay.["lowerKey"]
                        "debug-output inputs must participate in the lowering action key"

                    Expect.equal
                        relocated.["exportFingerprint"]
                        replay.["exportFingerprint"]
                        "a layout edit should preserve the exported semantic fingerprint"

                    Expect.equal
                        relocated.["fragmentHash"]
                        replay.["fragmentHash"]
                        "a layout edit should preserve the method implementation hash"

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

                    use pdbStream = File.OpenRead(pdbPath)
                    use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                    let pdb = pdbProvider.GetMetadataReader()

                    let document =
                        pdb.Documents
                        |> Seq.exactlyOne
                        |> pdb.GetDocument

                    let expectedChecksum =
                        relocatedSource
                        |> Encoding.UTF8.GetBytes
                        |> SHA256.HashData

                    Expect.sequenceEqual
                        (pdb.GetBlobBytes(document.Hash))
                        expectedChecksum
                        "the warm PDB should hash the current source content"

                    let relocatedSequencePoint =
                        pdb
                            .GetMethodDebugInformation(MetadataTokens.MethodDefinitionHandle(1))
                            .GetSequencePoints()
                        |> Seq.exactlyOne

                    Expect.equal
                        relocatedSequencePoint.StartLine
                        3
                        "the warm PDB should use the current declaration range"

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

            testCase
                "NuGet integration retains the NativeAOT service across CoreCompile edits and failures"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-msbuild",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let packageSource = Path.Combine(root, "packages")
                    let packageCache = Path.Combine(root, "cache")
                    let projectRoot = Path.Combine(root, "project")
                    let projectPath = Path.Combine(projectRoot, "Tracer.fsproj")
                    let firstSourcePath = Path.Combine(projectRoot, "First.fs")
                    let sourcePath = Path.Combine(projectRoot, "Tracer.fs")
                    let tracePath = Path.Combine(projectRoot, "obj", "fsharp2.trace")
                    let packageVersion = "0.0.0-integration"

                    let pipeName =
                        "fsharp2-msbuild-"
                        + Guid.NewGuid().ToString("N")

                    Directory.CreateDirectory(packageSource)
                    |> ignore

                    Directory.CreateDirectory(projectRoot)
                    |> ignore

                    let packageProject =
                        Path.Combine(
                            repositoryRoot,
                            "src",
                            "FSharp2.Compiler.MSBuild",
                            "FSharp2.Compiler.MSBuild.csproj"
                        )

                    let pack =
                        invokeProcess repositoryRoot 180_000 "dotnet" [
                            "pack"
                            packageProject
                            "--configuration"
                            configuration
                            "--output"
                            packageSource
                            $"-p:PackageVersion={packageVersion}"
                            "--no-restore"
                            "--verbosity"
                            "minimal"
                        ]

                    Expect.equal
                        pack.ExitCode
                        0
                        (pack.StandardOutput
                         + pack.StandardError)

                    File.WriteAllText(firstSourcePath, "module First\nlet first () = 1\n")
                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    File.WriteAllText(
                        projectPath,
                        $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <OutputType>Library</OutputType>
    <AssemblyName>Tracer</AssemblyName>
    <UseFSharp2Compiler>true</UseFSharp2Compiler>
    <FSharp2CompilerServerName>{pipeName}</FSharp2CompilerServerName>
    <FSharp2CompilerTracePath>{tracePath}</FSharp2CompilerTracePath>
    <DisableImplicitFSharpCoreReference>true</DisableImplicitFSharpCoreReference>
    <GenerateAssemblyInfo>false</GenerateAssemblyInfo>
    <GenerateTargetFrameworkAttribute>false</GenerateTargetFrameworkAttribute>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
    <ProduceReferenceAssembly>true</ProduceReferenceAssembly>
    <EnableSourceLink>false</EnableSourceLink>
    <EmbedUntrackedSources>false</EmbedUntrackedSources>
    <DebugSymbols>false</DebugSymbols>
    <DebugType>portable</DebugType>
    <Deterministic>true</Deterministic>
    <RestoreSources>{packageSource}</RestoreSources>
    <RestorePackagesPath>{packageCache}</RestorePackagesPath>
    <RestoreIgnoreFailedSources>true</RestoreIgnoreFailedSources>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="FSharp2.Compiler.MSBuild" Version="{packageVersion}" />
    <Compile Include="First.fs" />
    <Compile Include="Tracer.fs" />
  </ItemGroup>
</Project>
"""
                    )

                    let restore =
                        invokeProcess projectRoot 60_000 "dotnet" [
                            "restore"
                            projectPath
                            "--source"
                            packageSource
                            "--packages"
                            packageCache
                            "--ignore-failed-sources"
                            "--verbosity"
                            "minimal"
                        ]

                    Expect.equal
                        restore.ExitCode
                        0
                        (restore.StandardOutput
                         + restore.StandardError)

                    let packagedHostPath =
                        Path.Combine(
                            packageCache,
                            "fsharp2.compiler.msbuild",
                            packageVersion,
                            "tools",
                            "win-x64",
                            "fsc2.exe"
                        )

                    use service = startPackagedCompilerService projectRoot packagedHostPath pipeName

                    try
                        let build () =
                            invokeProcess projectRoot 60_000 "dotnet" [
                                "build"
                                projectPath
                                "--configuration"
                                "Release"
                                "--no-restore"
                                "--verbosity"
                                "minimal"
                            ]

                        let expectMsbuildDiagnostic code expectedLine result =
                            Expect.equal
                                result.StandardError
                                String.Empty
                                $"{code} should remain on MSBuild's standard output stream"

                            let lines =
                                result.StandardOutput.Split(
                                    [|
                                        "\r\n"
                                        "\n"
                                    |],
                                    StringSplitOptions.RemoveEmptyEntries
                                )
                                |> Array.filter (fun line ->
                                    line.Contains(
                                        " "
                                        + code
                                        + ":",
                                        StringComparison.Ordinal
                                    )
                                )

                            Expect.sequenceEqual
                                lines
                                [|
                                    expectedLine
                                    expectedLine
                                |]
                                $"{code} should match the oracle occurrence, text, and ordering"

                        let baselineBuild = build ()

                        Expect.equal
                            baselineBuild.ExitCode
                            0
                            (baselineBuild.StandardOutput
                             + baselineBuild.StandardError)

                        Expect.isTrue (File.Exists tracePath) "CoreCompile should execute fsc2"

                        let baselineTrace = readTrace tracePath

                        Expect.equal
                            baselineTrace.["sourceCount"]
                            "2"
                            "MSBuild should preserve the evaluated source list"

                        Expect.isGreaterThan
                            (Int32.Parse(baselineTrace.["referenceCount"]))
                            0
                            "CoreCompile should supply evaluated framework references"

                        Expect.equal
                            baselineTrace.["servicePid"]
                            (service.Id.ToString())
                            "CoreCompile should connect to the selected retained compiler service"

                        Expect.equal
                            baselineTrace.["emitted"]
                            "true"
                            "fsc2 should own the successful output"

                        let outputPath =
                            Path.Combine(projectRoot, "bin", "Release", "net10.0", "Tracer.dll")

                        let pdbPath =
                            Path.Combine(projectRoot, "bin", "Release", "net10.0", "Tracer.pdb")

                        let referencePath =
                            Path.Combine(
                                projectRoot,
                                "obj",
                                "Release",
                                "net10.0",
                                "refint",
                                "Tracer.dll"
                            )

                        let documentationPath =
                            Path.Combine(projectRoot, "bin", "Release", "net10.0", "Tracer.xml")

                        let requestedArtifacts = [
                            outputPath
                            pdbPath
                            referencePath
                            documentationPath
                        ]

                        for path in requestedArtifacts do
                            Expect.isTrue (File.Exists path) $"CoreCompile should produce {path}"

                        let baselineConsumer = invokeConsumer root outputPath 42
                        Expect.equal baselineConsumer.ExitCode 0 baselineConsumer.StandardError

                        File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 43\n")

                        let editedBuild = build ()

                        Expect.equal
                            editedBuild.ExitCode
                            0
                            (editedBuild.StandardOutput
                             + editedBuild.StandardError)

                        let editedTrace = readTrace tracePath

                        Expect.equal
                            editedTrace.["servicePid"]
                            baselineTrace.["servicePid"]
                            "a source edit should reuse the retained compiler service"

                        Expect.equal
                            editedTrace.["previousContentFingerprint"]
                            baselineTrace.["contentFingerprint"]
                            "the warm invocation should validate the prior project content"

                        Expect.equal
                            editedTrace.["invalidationReason"]
                            "source-content-changed"
                            "the warm trace should explain the invalidated semantic work"

                        Expect.equal
                            editedTrace.["exportFingerprint"]
                            baselineTrace.["exportFingerprint"]
                            "an implementation edit should preserve the exported fingerprint"

                        let editedConsumer = invokeConsumer root outputPath 43
                        Expect.equal editedConsumer.ExitCode 0 editedConsumer.StandardError

                        let retainedArtifactHashes =
                            requestedArtifacts
                            |> List.map (fun path ->
                                path,
                                path
                                |> File.ReadAllBytes
                                |> SHA256.HashData
                            )

                        File.WriteAllText(sourcePath, "module Tracer\nlet answer () = )\n")

                        let failedBuild = build ()

                        Expect.equal
                            failedBuild.ExitCode
                            1
                            "an FS diagnostic should fail CoreCompile and the project build"

                        expectMsbuildDiagnostic
                            "FS0010"
                            ($"{sourcePath}(2,17): error FS0010: Unexpected symbol ')' in binding [{projectPath}]")
                            failedBuild

                        let failedTrace = readTrace tracePath

                        Expect.equal
                            failedTrace.["servicePid"]
                            baselineTrace.["servicePid"]
                            "diagnostic requests should use the same retained service"

                        Expect.equal
                            failedTrace.["emitted"]
                            "false"
                            "a frontend failure must not publish a successful artifact"

                        File.WriteAllText(sourcePath, "module Tracer\nlet answer: int = \"text\"\n")

                        let typeMismatchBuild = build ()

                        Expect.equal
                            typeMismatchBuild.ExitCode
                            1
                            "the milestone type error should fail CoreCompile and the project build"

                        expectMsbuildDiagnostic
                            "FS0001"
                            ($"{sourcePath}(2,19): error FS0001: This expression was expected to have type\u001d    'int'    \u001dbut here has type\u001d    'string' [{projectPath}]")
                            typeMismatchBuild

                        let typeMismatchTrace = readTrace tracePath

                        Expect.equal
                            typeMismatchTrace.["servicePid"]
                            baselineTrace.["servicePid"]
                            "type diagnostics should use the same retained service"

                        Expect.equal
                            typeMismatchTrace.["emitted"]
                            "false"
                            "a type failure must not publish a successful artifact"

                        for path, expectedHash in retainedArtifactHashes do
                            Expect.isTrue (File.Exists path) $"a failed edit should preserve {path}"

                            Expect.sequenceEqual
                                (path
                                 |> File.ReadAllBytes
                                 |> SHA256.HashData)
                                expectedHash
                                $"a failed edit should preserve the last successful bytes for {path}"

                        let retainedConsumer = invokeConsumer root outputPath 43

                        Expect.equal
                            retainedConsumer.ExitCode
                            0
                            "a failed edit should preserve the last successful artifact set"
                    finally
                        if not service.HasExited then
                            service.Kill(true)

                            service.WaitForExit(10_000)
                            |> ignore
                finally
                    Directory.Delete(root, true)
        ]
