namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography

type internal LinkedArtifacts = {
    Implementation: byte array
    PortablePdb: byte array
}

with
    override _.ToString() = "LinkedArtifacts"

module internal Linker =
    let private sha256DocumentHashAlgorithm =
        Guid("8829d00f-11b8-4213-878b-770e8597ac16")

    let private fsharpLanguage = Guid("ab4f38c9-b6e6-43ba-be3b-58080b2ccce3")

    let private sourceLinkKind = Guid("cc110556-a091-4d38-9fec-25ab9a351a6a")

    let private immutableBytes (bytes: byte array) = ImmutableArray.CreateRange<byte>(bytes)

    let private contentId (captureDigest: byte array -> unit) (blobs: IEnumerable<Blob>) =
        use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)

        for blob in blobs do
            hash.AppendData(blob.GetBytes().AsSpan())

        let digest = hash.GetHashAndReset()
        captureDigest digest
        BlobContentId.FromHash(digest)

    let private encodeMethodBody
        (stream: MethodBodyStreamEncoder)
        (methodFragment: SymbolicMethodFragment)
        =
        let code = BlobBuilder()
        let instructions = InstructionEncoder(code)

        for instruction in methodFragment.Instructions do
            match instruction with
            | LoadInt32 value -> instructions.LoadConstantI4(value)
            | Return -> instructions.OpCode(ILOpCode.Ret)

        stream.AddMethodBody(instructions, maxStack = 1)

    let private encodeSignature () =
        let signature = BlobBuilder()

        BlobEncoder(signature)
            .MethodSignature(isInstanceMethod = false)
            .Parameters(0, (fun returnType -> returnType.Type().Int32()), (fun _ -> ()))

        signature

    let private encodeSequencePoint (methodFragment: SymbolicMethodFragment) =
        let sequencePoints = BlobBuilder()
        let range = methodFragment.Range

        let deltaLines =
            range.End.Line
            - range.Start.Line

        let deltaColumns =
            range.End.Column
            - range.Start.Column

        // Local-signature row id, followed by one sequence point at IL offset 0.
        sequencePoints.WriteCompressedInteger(0)
        sequencePoints.WriteCompressedInteger(0)
        sequencePoints.WriteCompressedInteger(deltaLines)

        if deltaLines = 0 then
            sequencePoints.WriteCompressedInteger(deltaColumns)
        else
            sequencePoints.WriteCompressedSignedInteger(deltaColumns)

        // The first non-hidden point stores an absolute start line and column.
        sequencePoints.WriteCompressedInteger(range.Start.Line)
        sequencePoints.WriteCompressedInteger(range.Start.Column)
        sequencePoints

    let link (invocation: CompilerInvocation) (symbolic: SymbolicAssembly) =
        if
            symbolic.Methods.Length
            <> 1
        then
            invalidOp "the first linker tracer supports exactly one method fragment"

        let methodFragment = symbolic.Methods.Head
        let metadata = MetadataBuilder()
        let ilStream = BlobBuilder()
        let methodBodies = MethodBodyStreamEncoder(ilStream)
        let bodyOffset = encodeMethodBody methodBodies methodFragment
        let signature = encodeSignature ()

        let publicKeyToken =
            [|
                0xb0uy
                0x3fuy
                0x5fuy
                0x7fuy
                0x11uy
                0xd5uy
                0x0auy
                0x3auy
            |]
            |> immutableBytes
            |> metadata.GetOrAddBlob

        let systemRuntime =
            metadata.AddAssemblyReference(
                metadata.GetOrAddString("System.Runtime"),
                Version(10, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                publicKeyToken,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<BlobHandle>
            )

        let systemObject =
            metadata.AddTypeReference(
                systemRuntime,
                metadata.GetOrAddString("System"),
                metadata.GetOrAddString("Object")
            )

        let firstField = MetadataTokens.FieldDefinitionHandle(1)
        let firstMethod = MetadataTokens.MethodDefinitionHandle(1)
        let firstParameter = MetadataTokens.ParameterHandle(1)
        let reservedMvid = metadata.ReserveGuid()

        metadata.AddModule(
            0,
            metadata.GetOrAddString(
                symbolic.AssemblyName
                + ".dll"
            ),
            reservedMvid.Handle,
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        metadata.AddAssembly(
            metadata.GetOrAddString(symbolic.AssemblyName),
            Version(1, 0, 0, 0),
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
            firstField,
            firstMethod
        )
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.BeforeFieldInit,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString(symbolic.ModuleName),
            systemObject,
            firstField,
            firstMethod
        )
        |> ignore

        metadata.AddMethodDefinition(
            MethodAttributes.Public
            ||| MethodAttributes.Static
            ||| MethodAttributes.HideBySig,
            MethodImplAttributes.IL
            ||| MethodImplAttributes.Managed,
            metadata.GetOrAddString(methodFragment.Name),
            metadata.GetOrAddBlob(signature),
            bodyOffset,
            firstParameter
        )
        |> ignore

        let pdbMetadata = MetadataBuilder()

        let documentName =
            methodFragment.DocumentPath
            |> Path.GetFileName
            |> pdbMetadata.GetOrAddDocumentName

        let document =
            pdbMetadata.AddDocument(
                documentName,
                pdbMetadata.GetOrAddGuid(sha256DocumentHashAlgorithm),
                pdbMetadata.GetOrAddBlob(methodFragment.DocumentChecksum),
                pdbMetadata.GetOrAddGuid(fsharpLanguage)
            )

        let sequencePoints =
            methodFragment
            |> encodeSequencePoint
            |> pdbMetadata.GetOrAddBlob

        pdbMetadata.AddMethodDebugInformation(document, sequencePoints)
        |> ignore

        if invocation.SourceLinkJson.Length > 0 then
            pdbMetadata.AddCustomDebugInformation(
                MetadataTokens.EntityHandle(TableIndex.Module, 1),
                pdbMetadata.GetOrAddGuid(sourceLinkKind),
                pdbMetadata.GetOrAddBlob(invocation.SourceLinkJson)
            )
            |> ignore

        let mutable pdbDigest = Array.empty<byte>

        let pdbBuilder =
            PortablePdbBuilder(
                pdbMetadata,
                metadata.GetRowCounts(),
                Unchecked.defaultof<MethodDefinitionHandle>,
                (fun blobs -> contentId (fun digest -> pdbDigest <- digest) blobs)
            )

        let pdbBlob = BlobBuilder()
        let pdbId = pdbBuilder.Serialize(pdbBlob)
        let debugDirectory = DebugDirectoryBuilder()

        debugDirectory.AddCodeViewEntry(
            symbolic.AssemblyName
            + ".pdb",
            pdbId,
            pdbBuilder.FormatVersion
        )

        debugDirectory.AddPdbChecksumEntry("SHA256", immutableBytes pdbDigest)
        debugDirectory.AddReproducibleEntry()

        let metadataRoot = MetadataRootBuilder(metadata)

        let deterministicIdProvider =
            Func<IEnumerable<Blob>, BlobContentId>(fun blobs -> contentId ignore blobs)

        let peBuilder =
            ManagedPEBuilder(
                PEHeaderBuilder.CreateLibraryHeader(),
                metadataRoot,
                ilStream,
                debugDirectoryBuilder = debugDirectory,
                strongNameSignatureSize = 0,
                deterministicIdProvider = deterministicIdProvider
            )

        let peBlob = BlobBuilder()
        let peId = peBuilder.Serialize(peBlob)
        let mvidWriter = reservedMvid.CreateWriter()
        mvidWriter.WriteGuid(peId.Guid)

        {
            Implementation = peBlob.ToArray()
            PortablePdb = pdbBlob.ToArray()
        }

    let publishTransactionally invocation artifacts =
        let outputDirectory = Path.GetDirectoryName(invocation.AssemblyPath)
        let pdbDirectory = Path.GetDirectoryName(invocation.PdbPath)

        Directory.CreateDirectory(outputDirectory)
        |> ignore

        Directory.CreateDirectory(pdbDirectory)
        |> ignore

        let nonce = Guid.NewGuid().ToString("N")

        let temporaryAssembly =
            invocation.AssemblyPath
            + ".fsc2-"
            + nonce
            + ".tmp"

        let temporaryPdb =
            invocation.PdbPath
            + ".fsc2-"
            + nonce
            + ".tmp"

        try
            File.WriteAllBytes(temporaryAssembly, artifacts.Implementation)
            File.WriteAllBytes(temporaryPdb, artifacts.PortablePdb)
            File.Move(temporaryPdb, invocation.PdbPath, true)
            File.Move(temporaryAssembly, invocation.AssemblyPath, true)
        with _ ->
            if File.Exists(temporaryAssembly) then
                File.Delete(temporaryAssembly)

            if File.Exists(temporaryPdb) then
                File.Delete(temporaryPdb)

            reraise ()
