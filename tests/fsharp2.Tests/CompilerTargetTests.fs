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
                    let responsePath = Path.Combine(root, "compile.rsp")

                    let originalPdb = [|
                        0x46uy
                        0x53uy
                        0x32uy
                    |]

                    File.WriteAllText(sourcePath, "module Tracer\nlet answer () = 42\n")

                    Directory.CreateDirectory(outputPath)
                    |> ignore

                    File.WriteAllBytes(pdbPath, originalPdb)

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

                    let transactionFiles =
                        Directory.GetFiles(root)
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
