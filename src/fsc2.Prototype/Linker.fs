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
open System.Text

type internal LinkedArtifacts = {
    Implementation: byte array
    PortablePdb: byte array
    ReferenceAssembly: byte array
    Documentation: byte array
} with

    override _.ToString() = "LinkedArtifacts"

[<Sealed>]
type private PrototypeNativeResourceSection(payload: byte array) =
    inherit ResourceSectionBuilder()

    override _.Serialize(builder: BlobBuilder, location: SectionLocation) =
        if
            builder.Count
            <> 0
        then
            invalidOp "the native resource section must start empty"

        let writeDirectory () =
            builder.WriteUInt32(0u)
            builder.WriteUInt32(0u)
            builder.WriteUInt16(0us)
            builder.WriteUInt16(0us)
            builder.WriteUInt16(0us)
            builder.WriteUInt16(1us)

        let directoryFlag = 0x80000000u
        let typeDirectoryOffset = 24u
        let nameDirectoryOffset = 48u
        let dataEntryOffset = 72u
        let payloadOffset = 88

        writeDirectory ()
        builder.WriteUInt32(10u)

        builder.WriteUInt32(
            directoryFlag
            ||| typeDirectoryOffset
        )

        writeDirectory ()
        builder.WriteUInt32(1u)

        builder.WriteUInt32(
            directoryFlag
            ||| nameDirectoryOffset
        )

        writeDirectory ()
        builder.WriteUInt32(0u)
        builder.WriteUInt32(dataEntryOffset)

        builder.WriteUInt32(
            uint32 (
                location.RelativeVirtualAddress
                + payloadOffset
            )
        )

        builder.WriteUInt32(uint32 payload.Length)
        builder.WriteUInt32(0u)
        builder.WriteUInt32(0u)
        builder.WriteBytes(payload)
        builder.Align(4)

module internal Linker =
    let private sha256DocumentHashAlgorithm =
        Guid("8829d00f-11b8-4213-878b-770e8597ac16")

    let private fsharpLanguage = Guid("ab4f38c9-b6e6-43ba-be3b-58080b2ccce3")

    let private sourceLinkKind = Guid("cc110556-a091-4d38-9fec-25ab9a351a6a")

    let private immutableBytes (bytes: byte array) = ImmutableArray.CreateRange<byte>(bytes)

    let private addManagedResource
        (metadata: MetadataBuilder)
        (resource: ManagedResourceInput option)
        =
        match resource with
        | None -> Unchecked.defaultof<BlobBuilder>
        | Some resource ->
            let stream = BlobBuilder()
            let offset = uint32 stream.Count
            stream.WriteInt32(resource.Data.Length)
            stream.WriteBytes(resource.Data)

            let attributes =
                if resource.IsPublic then
                    ManifestResourceAttributes.Public
                else
                    ManifestResourceAttributes.Private

            metadata.AddManifestResource(
                attributes,
                metadata.GetOrAddString(resource.LogicalName),
                Unchecked.defaultof<EntityHandle>,
                offset
            )
            |> ignore

            stream

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

        let codeSize = code.Count
        stream.AddMethodBody(instructions, maxStack = 1), codeSize

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

    let private escapeXml (value: string) =
        value
            .Replace("&", "&amp;", StringComparison.Ordinal)
            .Replace("<", "&lt;", StringComparison.Ordinal)
            .Replace(">", "&gt;", StringComparison.Ordinal)
            .Replace("\"", "&quot;", StringComparison.Ordinal)
            .Replace("'", "&apos;", StringComparison.Ordinal)

    let private documentation (assemblyName: string) =
        String.concat
            "\n"
            [
                "<?xml version=\"1.0\"?>"
                "<doc>"
                "  <assembly>"
                "    <name>"
                + escapeXml assemblyName
                + "</name>"
                "  </assembly>"
                "  <members />"
                "</doc>"
                String.Empty
            ]
        |> UTF8Encoding(false).GetBytes

    let private linkWithStrongName
        (invocation: CompilerInvocation)
        (symbolic: SymbolicAssembly)
        (strongName: StrongNamePlan)
        =
        let typeFragments = symbolic.Module.Types

        let methodFragments =
            typeFragments
            |> List.collect _.Methods

        if
            symbolic.SchemaVersion
            <> symbolic.Module.SchemaVersion
            || (typeFragments
                |> List.exists (fun typeFragment ->
                    symbolic.SchemaVersion
                    <> typeFragment.SchemaVersion
                    || (typeFragment.Methods
                        |> List.exists (fun methodFragment ->
                            symbolic.SchemaVersion
                            <> methodFragment.SchemaVersion
                        ))
                ))
        then
            invalidOp "the symbolic emission graph uses inconsistent schema versions"

        if
            String.IsNullOrWhiteSpace(symbolic.StableId)
            || String.IsNullOrWhiteSpace(symbolic.Module.StableId)
            || (typeFragments
                |> List.exists (fun typeFragment ->
                    String.IsNullOrWhiteSpace(typeFragment.StableId)
                    || (typeFragment.Methods
                        |> List.exists (fun methodFragment ->
                            String.IsNullOrWhiteSpace(methodFragment.StableId)
                        ))
                ))
        then
            invalidOp "the symbolic emission graph contains an empty stable identity"

        if
            invocation.DebugDocumentPaths.Length
            <> typeFragments.Length
        then
            invalidOp "the symbolic emission graph must contain one debug document per source type"

        let documentChecksums =
            typeFragments
            |> List.mapi (fun documentIndex typeFragment ->
                match typeFragment.Methods with
                | [] -> invalidOp "a source type must contain at least one method for PDB emission"
                | firstMethod :: remainingMethods ->
                    if
                        firstMethod.DocumentIndex
                        <> documentIndex
                        || (remainingMethods
                            |> List.exists (fun methodFragment ->
                                methodFragment.DocumentIndex
                                <> documentIndex
                                || methodFragment.DocumentChecksum
                                   <> firstMethod.DocumentChecksum
                            ))
                    then
                        invalidOp "a source type has inconsistent debug-document metadata"

                    firstMethod.DocumentChecksum
            )

        let metadata = MetadataBuilder()
        let ilStream = BlobBuilder()
        let methodBodies = MethodBodyStreamEncoder(ilStream)

        let encodedMethods =
            methodFragments
            |> List.map (fun methodFragment ->
                let bodyOffset, codeSize = encodeMethodBody methodBodies methodFragment
                methodFragment, bodyOffset, codeSize
            )

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
            metadata.GetOrAddString(symbolic.Module.Name),
            reservedMvid.Handle,
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        let assemblyPublicKey =
            if strongName.PublicKey.Length = 0 then
                Unchecked.defaultof<BlobHandle>
            else
                strongName.PublicKey
                |> metadata.GetOrAddBlob

        metadata.AddAssembly(
            metadata.GetOrAddString(symbolic.AssemblyName),
            Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            assemblyPublicKey,
            StrongName.assemblyFlags strongName,
            StrongName.assemblyHashAlgorithm strongName
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

        let mutable nextMethodRow = 1

        for typeFragment in typeFragments do
            metadata.AddTypeDefinition(
                TypeAttributes.Public
                ||| TypeAttributes.Abstract
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.BeforeFieldInit,
                (if String.IsNullOrEmpty(typeFragment.Namespace) then
                     Unchecked.defaultof<StringHandle>
                 else
                     metadata.GetOrAddString(typeFragment.Namespace)),
                metadata.GetOrAddString(typeFragment.Name),
                systemObject,
                firstField,
                MetadataTokens.MethodDefinitionHandle(nextMethodRow)
            )
            |> ignore

            nextMethodRow <-
                nextMethodRow
                + typeFragment.Methods.Length

        let methodSignature = metadata.GetOrAddBlob(signature)

        for methodFragment, bodyOffset, _ in encodedMethods do
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                MethodImplAttributes.IL
                ||| MethodImplAttributes.Managed,
                metadata.GetOrAddString(methodFragment.Name),
                methodSignature,
                bodyOffset,
                firstParameter
            )
            |> ignore

        let pdbMetadata = MetadataBuilder()

        let documents =
            (invocation.DebugDocumentPaths, documentChecksums)
            ||> List.map2 (fun path checksum ->
                pdbMetadata.AddDocument(
                    pdbMetadata.GetOrAddDocumentName(path),
                    pdbMetadata.GetOrAddGuid(sha256DocumentHashAlgorithm),
                    pdbMetadata.GetOrAddBlob(checksum),
                    pdbMetadata.GetOrAddGuid(fsharpLanguage)
                )
            )

        for methodFragment in methodFragments do
            let sequencePoints =
                methodFragment
                |> encodeSequencePoint
                |> pdbMetadata.GetOrAddBlob

            pdbMetadata.AddMethodDebugInformation(
                documents.[methodFragment.DocumentIndex],
                sequencePoints
            )
            |> ignore

        let systemNamespace = pdbMetadata.GetOrAddBlobUTF8("System", false)
        let importDefinitions = BlobBuilder()

        importDefinitions.WriteCompressedInteger(int ImportDefinitionKind.ImportNamespace)

        systemNamespace
        |> MetadataTokens.GetHeapOffset
        |> importDefinitions.WriteCompressedInteger

        let importScope =
            pdbMetadata.AddImportScope(
                Unchecked.defaultof<ImportScopeHandle>,
                pdbMetadata.GetOrAddBlob(importDefinitions)
            )

        for methodIndex, (_, _, methodCodeSize) in List.indexed encodedMethods do
            pdbMetadata.AddLocalScope(
                MetadataTokens.MethodDefinitionHandle(methodIndex + 1),
                importScope,
                Unchecked.defaultof<LocalVariableHandle>,
                Unchecked.defaultof<LocalConstantHandle>,
                0,
                methodCodeSize
            )
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

        let managedResources = addManagedResource metadata invocation.ManagedResource
        let metadataRoot = MetadataRootBuilder(metadata)

        let nativeResources =
            if invocation.NativeResourceData.Length = 0 then
                Unchecked.defaultof<ResourceSectionBuilder>
            else
                PrototypeNativeResourceSection(invocation.NativeResourceData)
                :> ResourceSectionBuilder

        let deterministicIdProvider =
            Func<IEnumerable<Blob>, BlobContentId>(fun blobs -> contentId ignore blobs)

        let peBuilder =
            ManagedPEBuilder(
                PEHeaderBuilder.CreateLibraryHeader(),
                metadataRoot,
                ilStream,
                managedResources = managedResources,
                nativeResources = nativeResources,
                debugDirectoryBuilder = debugDirectory,
                strongNameSignatureSize = strongName.SignatureSize,
                flags = StrongName.corFlags strongName,
                deterministicIdProvider = deterministicIdProvider
            )

        let peBlob = BlobBuilder()
        let peId = peBuilder.Serialize(peBlob)
        let mvidWriter = reservedMvid.CreateWriter()
        mvidWriter.WriteGuid(peId.Guid)
        StrongName.sign peBuilder peBlob strongName

        let implementation = peBlob.ToArray()

        {
            Implementation = implementation
            PortablePdb = pdbBlob.ToArray()
            // The prototype has not yet split contract-only metadata emission
            // from implementation emission. Preserve the requested artifact
            // contract now; the compatibility gate still owns a true ref DLL.
            ReferenceAssembly = implementation
            Documentation = documentation symbolic.AssemblyName
        }

    let link (invocation: CompilerInvocation) (symbolic: SymbolicAssembly) =
        let strongName =
            StrongName.createPlan invocation.StrongNameMode invocation.StrongNameKey

        try
            linkWithStrongName invocation symbolic strongName
        finally
            StrongName.clearPlan strongName

    let publishTransactionally invocation artifacts =
        let nonce = Guid.NewGuid().ToString("N")

        let deleteIfPresent path =
            if File.Exists(path) then
                File.Delete(path)

        let requestedArtifacts =
            [
                match invocation.ReferenceAssemblyPath with
                | Some path -> yield path, artifacts.ReferenceAssembly
                | None -> ()

                match invocation.DocumentationPath with
                | Some path -> yield path, artifacts.Documentation
                | None -> ()

                yield invocation.PdbPath, artifacts.PortablePdb
                // The implementation DLL is deliberately last: it is the
                // transaction's externally visible commit point.
                yield invocation.AssemblyPath, artifacts.Implementation
            ]

        let distinctTargets = HashSet<string>(StringComparer.OrdinalIgnoreCase)

        for targetPath, _ in requestedArtifacts do
            if
                targetPath
                |> Path.GetFullPath
                |> distinctTargets.Add
                |> not
            then
                invalidOp "compiler output paths must be distinct"

        let states =
            requestedArtifacts
            |> List.mapi (fun index (targetPath, bytes) ->
                let directory = Path.GetDirectoryName(targetPath)

                if not (String.IsNullOrEmpty(directory)) then
                    Directory.CreateDirectory(directory)
                    |> ignore

                targetPath,
                bytes,
                targetPath
                + ".fsc2-"
                + nonce
                + "-"
                + index.ToString()
                + ".tmp",
                targetPath
                + ".fsc2-"
                + nonce
                + "-"
                + index.ToString()
                + ".backup",
                ref false,
                ref false
            )

        try
            for _, bytes, temporaryPath, _, _, _ in states do
                File.WriteAllBytes(temporaryPath, bytes)

            for targetPath, _, _, backupPath, backedUp, _ in states do
                if File.Exists(targetPath) then
                    File.Move(targetPath, backupPath)
                    backedUp.Value <- true

            for targetPath, _, temporaryPath, _, _, published in states do
                File.Move(temporaryPath, targetPath)
                published.Value <- true

            try
                for _, _, _, backupPath, _, _ in states do
                    deleteIfPresent backupPath
            with _ ->
                // The requested artifact set is already complete. A backup
                // cleanup failure must not roll back successfully published
                // outputs into a partial set.
                ()
        with _ ->
            for targetPath, _, _, _, _, published in states do
                if published.Value then
                    deleteIfPresent targetPath

            for targetPath, _, _, backupPath, backedUp, _ in states do
                if backedUp.Value then
                    File.Move(backupPath, targetPath, true)

            for _, _, temporaryPath, backupPath, _, _ in states do
                deleteIfPresent temporaryPath
                deleteIfPresent backupPath

            reraise ()
