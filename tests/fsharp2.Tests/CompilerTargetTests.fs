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

    type private MethodMetadataShape = {
        Attributes: MethodAttributes
        ImplementationAttributes: MethodImplAttributes
        Signature: string
        Parameters: (int * string * ParameterAttributes * (string * string * string) array) array
        GenericParameters: (int * string * GenericParameterAttributes) array
        GenericParameterConstraints: (int * string array) array
        CustomAttributes: (string * string * string) array
        SequencePoints: string array
    }

    type private ObjectTypeMetadataShape = {
        Attributes: TypeAttributes
        BaseType: string
        DeclaringType: string
        CustomAttributes: (string * string * string) array
        Constructor: MethodMetadataShape
        InstanceMember: MethodMetadataShape
    }

    type private ResumableTryFinallyProbe() =
        member val ComputationInvocations = 0 with get, set

        member this.InvokeComputation
            (_stateMachine: byref<Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine<int>>)
            =
            this.ComputationInvocations <-
                this.ComputationInvocations
                + 1

            true

    type private SignatureShapeProvider() as this =
        let fullTypeName
            (metadata: MetadataReader)
            (namespaceHandle: StringHandle)
            (nameHandle: StringHandle)
            =
            let namespaceName = metadata.GetString(namespaceHandle)
            let name = metadata.GetString(nameHandle)

            if String.IsNullOrEmpty(namespaceName) then
                name
            else
                namespaceName
                + "."
                + name

        let localScopeName = "<local>"

        let rec resolutionScopeName (metadata: MetadataReader) (handle: EntityHandle) =
            match handle.Kind with
            | HandleKind.AssemblyReference ->
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.AssemblyReferenceHandle
                |> metadata.GetAssemblyReference
                |> _.Name
                |> metadata.GetString
            | HandleKind.TypeReference ->
                let declaringType =
                    handle
                    |> MetadataTokens.GetRowNumber
                    |> MetadataTokens.TypeReferenceHandle
                    |> metadata.GetTypeReference

                resolutionScopeName metadata declaringType.ResolutionScope
            | HandleKind.ModuleDefinition -> localScopeName
            | HandleKind.ModuleReference ->
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.ModuleReferenceHandle
                |> metadata.GetModuleReference
                |> _.Name
                |> metadata.GetString
            | _ -> handle.Kind.ToString()

        static member Format(signature: MethodSignature<string>) =
            String.concat "|" [
                signature.Header.CallingConvention.ToString()
                $"generic:{signature.GenericParameterCount}"
                $"required:{signature.RequiredParameterCount}"
                "return:"
                signature.ReturnType
                "parameters:"
                yield! signature.ParameterTypes
            ]

        interface ISignatureTypeProvider<string, unit> with
            member _.GetArrayType(elementType, shape) = $"array:{shape.Rank}:{elementType}"

            member _.GetByReferenceType(elementType) =
                "byref:"
                + elementType

            member _.GetFunctionPointerType(signature) =
                "function-pointer:"
                + SignatureShapeProvider.Format(signature)

            member _.GetGenericInstantiation(genericType, typeArguments) =
                String.concat "" [
                    genericType
                    "<"
                    typeArguments
                    |> String.concat ","
                    ">"
                ]

            member _.GetGenericMethodParameter(_, index) = $"!!{index}"

            member _.GetGenericTypeParameter(_, index) = $"!{index}"

            member _.GetModifiedType(modifier, unmodifiedType, isRequired) =
                String.concat ":" [
                    if isRequired then "modreq" else "modopt"
                    modifier
                    unmodifiedType
                ]

            member _.GetPinnedType(elementType) =
                "pinned:"
                + elementType

            member _.GetPointerType(elementType) =
                "pointer:"
                + elementType

            member _.GetPrimitiveType(code) = code.ToString()

            member _.GetSZArrayType(elementType) =
                "szarray:"
                + elementType

            member _.GetTypeFromDefinition(metadata, handle, rawTypeKind) =
                let definition = metadata.GetTypeDefinition(handle)

                String.concat "" [
                    $"{rawTypeKind:X2}:"
                    fullTypeName metadata definition.Namespace definition.Name
                    "@"
                    localScopeName
                ]

            member _.GetTypeFromReference(metadata, handle, rawTypeKind) =
                let reference = metadata.GetTypeReference(handle)

                String.concat "" [
                    $"{rawTypeKind:X2}:"
                    fullTypeName metadata reference.Namespace reference.Name
                    "@"
                    resolutionScopeName metadata reference.ResolutionScope
                ]

            member _.GetTypeFromSpecification(metadata, context, handle, _) =
                metadata.GetTypeSpecification(handle).DecodeSignature(this, context)

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

    let private compatibilityOraclePath () =
        let versionResult = invokeProcess repositoryRoot 10_000 "dotnet" [ "--version" ]

        Expect.equal
            versionResult.ExitCode
            0
            (versionResult.StandardOutput
             + versionResult.StandardError)

        let version = versionResult.StandardOutput.Trim()
        let sdkList = invokeProcess repositoryRoot 10_000 "dotnet" [ "--list-sdks" ]

        Expect.equal
            sdkList.ExitCode
            0
            (sdkList.StandardOutput
             + sdkList.StandardError)

        let prefix =
            version
            + " ["

        let sdkRoot =
            sdkList.StandardOutput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryPick (fun line ->
                let line = line.Trim()

                if
                    line.StartsWith(prefix, StringComparison.Ordinal)
                    && line.EndsWith(']')
                then
                    Some(
                        line.Substring(
                            prefix.Length,
                            line.Length
                            - prefix.Length
                            - 1
                        )
                    )
                else
                    None
            )
            |> Option.defaultWith (fun () ->
                failtestf "the active .NET SDK '%s' was not present in dotnet --list-sdks" version
            )

        let compilerPath = Path.Combine(sdkRoot, version, "FSharp", "fsc.dll")

        Expect.isTrue
            (File.Exists(compilerPath))
            $"the Compatibility Oracle should exist at {compilerPath}"

        compilerPath

    let private invokeCompatibilityOracle workingDirectory responsePath (arguments: string list) =
        File.WriteAllLines(responsePath, arguments)

        invokeProcess workingDirectory 30_000 "dotnet" [
            compatibilityOraclePath ()
            "@"
            + responsePath
        ]

    let private objectTypeMetadataShape assemblyPath pdbPath instanceMemberName =
        use implementationStream = File.OpenRead(assemblyPath)
        use implementation = new PEReader(implementationStream)
        let metadata = implementation.GetMetadataReader()
        use pdbStream = File.OpenRead(pdbPath)
        use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
        let pdb = pdbProvider.GetMetadataReader()

        let typeName (handle: EntityHandle) =
            match handle.Kind with
            | HandleKind.TypeDefinition ->
                let definition =
                    handle
                    |> MetadataTokens.GetRowNumber
                    |> MetadataTokens.TypeDefinitionHandle
                    |> metadata.GetTypeDefinition

                let namespaceName = metadata.GetString(definition.Namespace)
                let name = metadata.GetString(definition.Name)

                if String.IsNullOrEmpty(namespaceName) then
                    name
                else
                    namespaceName
                    + "."
                    + name
            | HandleKind.TypeReference ->
                let reference =
                    handle
                    |> MetadataTokens.GetRowNumber
                    |> MetadataTokens.TypeReferenceHandle
                    |> metadata.GetTypeReference

                let namespaceName = metadata.GetString(reference.Namespace)
                let name = metadata.GetString(reference.Name)

                if String.IsNullOrEmpty(namespaceName) then
                    name
                else
                    namespaceName
                    + "."
                    + name
            | HandleKind.TypeSpecification ->
                handle
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.TypeSpecificationHandle
                |> metadata.GetTypeSpecification
                |> fun specification -> specification.DecodeSignature(SignatureShapeProvider(), ())
            | kind -> failtestf "unsupported metadata type handle %A" kind

        let customAttributeShape handle =
            let attribute = metadata.GetCustomAttribute(handle)

            let attributeTypeName, constructorSignature =
                match attribute.Constructor.Kind with
                | HandleKind.MemberReference ->
                    let constructor =
                        attribute.Constructor
                        |> MetadataTokens.GetRowNumber
                        |> MetadataTokens.MemberReferenceHandle
                        |> metadata.GetMemberReference

                    typeName constructor.Parent,
                    (constructor.DecodeMethodSignature(SignatureShapeProvider(), ())
                     |> SignatureShapeProvider.Format)
                | kind -> failtestf "unsupported custom-attribute constructor %A" kind

            attributeTypeName,
            constructorSignature,
            Convert.ToHexString(metadata.GetBlobBytes(attribute.Value))

        let typeHandle, typeDefinition =
            metadata.TypeDefinitions
            |> Seq.map (fun handle -> handle, metadata.GetTypeDefinition(handle))
            |> Seq.find (fun (_, definition) ->
                metadata.GetString(definition.Name) = "TaskBuilderBase"
            )

        let methodShape methodName =
            let methodHandle, definition =
                typeDefinition.GetMethods()
                |> Seq.map (fun handle -> handle, metadata.GetMethodDefinition(handle))
                |> Seq.find (fun (_, definition) ->
                    metadata.GetString(definition.Name) = methodName
                )

            let debugInformation = pdb.GetMethodDebugInformation(methodHandle)

            {
                Attributes = definition.Attributes
                ImplementationAttributes = definition.ImplAttributes
                Signature =
                    definition.DecodeSignature(SignatureShapeProvider(), ())
                    |> SignatureShapeProvider.Format
                Parameters =
                    definition.GetParameters()
                    |> Seq.map (fun handle ->
                        let parameter = metadata.GetParameter(handle)

                        int parameter.SequenceNumber,
                        metadata.GetString(parameter.Name),
                        parameter.Attributes,
                        (parameter.GetCustomAttributes()
                         |> Seq.map customAttributeShape
                         |> Seq.toArray)
                    )
                    |> Seq.toArray
                GenericParameters =
                    definition.GetGenericParameters()
                    |> Seq.map (fun handle ->
                        let parameter = metadata.GetGenericParameter(handle)

                        parameter.Index, metadata.GetString(parameter.Name), parameter.Attributes
                    )
                    |> Seq.toArray
                GenericParameterConstraints =
                    definition.GetGenericParameters()
                    |> Seq.map (fun handle ->
                        let parameter = metadata.GetGenericParameter(handle)

                        int parameter.Index,
                        parameter.GetConstraints()
                        |> Seq.map (fun constraintHandle ->
                            metadata.GetGenericParameterConstraint(constraintHandle).Type
                            |> typeName
                        )
                        |> Seq.toArray
                    )
                    |> Seq.toArray
                CustomAttributes =
                    definition.GetCustomAttributes()
                    |> Seq.map customAttributeShape
                    |> Seq.toArray
                SequencePoints =
                    debugInformation.GetSequencePoints()
                    |> Seq.map (fun point ->
                        if point.IsHidden then
                            "hidden"
                        else
                            $"{point.StartLine}:{point.StartColumn}-{point.EndLine}:{point.EndColumn}"
                    )
                    |> Seq.toArray
            }

        let declaringType =
            let declaringHandle = typeDefinition.GetDeclaringType()

            if declaringHandle.IsNil then
                String.Empty
            else
                MetadataTokens.EntityHandle(
                    TableIndex.TypeDef,
                    MetadataTokens.GetRowNumber(declaringHandle)
                )
                |> typeName

        {
            Attributes = typeDefinition.Attributes
            BaseType = typeName typeDefinition.BaseType
            DeclaringType = declaringType
            CustomAttributes =
                typeDefinition.GetCustomAttributes()
                |> Seq.map customAttributeShape
                |> Seq.toArray
            Constructor = methodShape ".ctor"
            InstanceMember = methodShape instanceMemberName
        }

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

    let private writeReferenceTypeAssembly path assemblyName namespaceName typeName =
        let metadata = MetadataBuilder()
        let firstField = MetadataTokens.FieldDefinitionHandle(1)
        let firstMethod = MetadataTokens.MethodDefinitionHandle(1)

        metadata.AddModule(
            0,
            metadata.GetOrAddString(
                assemblyName
                + ".dll"
            ),
            metadata.GetOrAddGuid(Guid.NewGuid()),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        metadata.AddAssembly(
            metadata.GetOrAddString(assemblyName),
            System.Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.None
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
            TypeAttributes.Public,
            metadata.GetOrAddString(namespaceName),
            metadata.GetOrAddString(typeName),
            Unchecked.defaultof<EntityHandle>,
            firstField,
            firstMethod
        )
        |> ignore

        let image = BlobBuilder()

        ManagedPEBuilder(
            PEHeaderBuilder.CreateLibraryHeader(),
            MetadataRootBuilder(metadata),
            BlobBuilder()
        )
            .Serialize(image)
        |> ignore

        File.WriteAllBytes(path, image.ToArray())

    let private compileForExportFingerprint
        root
        responsePath
        sourcePath
        outputStem
        additionalReferences
        =
        let outputPath =
            Path.Combine(
                root,
                outputStem
                + ".dll"
            )

        let pdbPath =
            Path.Combine(
                root,
                outputStem
                + ".pdb"
            )

        let tracePath =
            Path.Combine(
                root,
                outputStem
                + ".trace"
            )

        let systemRuntimePath = Assembly.Load("System.Runtime").Location

        let arguments =
            [
                [
                    "--target:library"
                    "--deterministic+"
                    "--debug:portable"
                    $"--reference:{typeof<Microsoft.FSharp.Core.StructAttribute>.Assembly.Location}"
                    $"--reference:{systemRuntimePath}"
                ]
                additionalReferences
                |> List.map (fun referencePath -> $"--reference:{referencePath}")
                [
                    $"--out:{outputPath}"
                    $"--pdb:{pdbPath}"
                    $"--fsharp2-trace:{tracePath}"
                    sourcePath
                ]
            ]
            |> List.concat

        File.WriteAllLines(responsePath, arguments)

        let result = invokeFsc2 root responsePath

        Expect.equal
            result.ExitCode
            0
            (result.StandardOutput
             + result.StandardError)

        outputPath, (readTrace tracePath).["exportFingerprint"]

    let private findAssemblyAttributes (metadata: MetadataReader) expectedTypeName =
        metadata.CustomAttributes
        |> Seq.choose (fun handle ->
            let attribute = metadata.GetCustomAttribute(handle)

            if
                attribute.Parent.Kind
                <> HandleKind.AssemblyDefinition
                || attribute.Constructor.Kind
                   <> HandleKind.MemberReference
            then
                None
            else
                let constructor =
                    attribute.Constructor
                    |> MetadataTokens.GetRowNumber
                    |> MetadataTokens.MemberReferenceHandle
                    |> metadata.GetMemberReference

                if
                    constructor.Parent.Kind
                    <> HandleKind.TypeReference
                then
                    None
                else
                    let attributeType =
                        constructor.Parent
                        |> MetadataTokens.GetRowNumber
                        |> MetadataTokens.TypeReferenceHandle
                        |> metadata.GetTypeReference

                    let fullName =
                        metadata.GetString(attributeType.Namespace)
                        + "."
                        + metadata.GetString(attributeType.Name)

                    if
                        fullName
                        <> expectedTypeName
                        || attributeType.ResolutionScope.Kind
                           <> HandleKind.AssemblyReference
                    then
                        None
                    else
                        let scope =
                            attributeType.ResolutionScope
                            |> MetadataTokens.GetRowNumber
                            |> MetadataTokens.AssemblyReferenceHandle
                            |> metadata.GetAssemblyReference

                        Some(attribute, constructor, scope)
        )
        |> Seq.toArray

    let private findAssemblyAttribute (metadata: MetadataReader) expectedTypeName =
        findAssemblyAttributes metadata expectedTypeName
        |> Seq.exactlyOne

    let private readCustomAttributes
        (metadata: MetadataReader)
        (handles: CustomAttributeHandleCollection)
        =
        handles
        |> Seq.map (fun handle ->
            let attribute = metadata.GetCustomAttribute(handle)

            if
                attribute.Constructor.Kind
                <> HandleKind.MemberReference
            then
                failtest "the custom attribute constructor should be a member reference"

            let constructor =
                attribute.Constructor
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.MemberReferenceHandle
                |> metadata.GetMemberReference

            if
                constructor.Parent.Kind
                <> HandleKind.TypeReference
            then
                failtest "the custom attribute should be declared by a referenced type"

            let attributeType =
                constructor.Parent
                |> MetadataTokens.GetRowNumber
                |> MetadataTokens.TypeReferenceHandle
                |> metadata.GetTypeReference

            let fullName =
                metadata.GetString(attributeType.Namespace)
                + "."
                + metadata.GetString(attributeType.Name)

            fullName,
            metadata.GetBlobBytes(constructor.Signature),
            metadata.GetBlobBytes(attribute.Value)
        )
        |> Seq.toArray

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

    let private withObjectMemberDifferential
        (temporaryDirectoryName: string)
        (sourceText: string)
        (memberName: string)
        (assertRuntimeBehavior: string -> string -> unit)
        =
        let root =
            Path.Combine(Path.GetTempPath(), temporaryDirectoryName, Guid.NewGuid().ToString("N"))

        Directory.CreateDirectory(root)
        |> ignore

        try
            let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
            File.WriteAllText(sourcePath, sourceText)

            let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
            let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
            let oracleResponsePath = Path.Combine(root, "oracle.rsp")

            let oracleResult =
                invokeCompatibilityOracle root oracleResponsePath [
                    "--target:library"
                    "--targetprofile:netcore"
                    "--deterministic+"
                    "--debug:portable"
                    "--optimize-"
                    $"--out:{oracleOutputPath}"
                    $"--pdb:{oraclePdbPath}"
                    sourcePath
                ]

            Expect.equal
                oracleResult.ExitCode
                0
                (oracleResult.StandardOutput
                 + oracleResult.StandardError)

            let responsePath = Path.Combine(root, "fsharp2.rsp")

            let outputPath, _ =
                compileForExportFingerprint root responsePath sourcePath "TaskBuilderBase-fsharp2" []

            let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

            Expect.equal
                (objectTypeMetadataShape outputPath pdbPath memberName)
                (objectTypeMetadataShape oracleOutputPath oraclePdbPath memberName)
                $"the '{memberName}' member should match the Compatibility Oracle's CLR and portable-PDB surface"

            assertRuntimeBehavior oracleOutputPath outputPath
        finally
            Directory.Delete(root, true)

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

                    File.WriteAllText(
                        sourcePath,
                        "// Auto-generated source\nmodule Tracer\nlet answer () = 42\n"
                    )

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
                        3
                        "the sequence point should map to the declaration"

                    Expect.equal
                        sequencePoints.[0].StartColumn
                        5
                        "the sequence point should begin at the declaration name"

                    Expect.equal
                        sequencePoints.[0].EndLine
                        3
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

            testCase "uses the invocation define set when selecting conditional source"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-conditionals",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "Tracer.fs")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    File.WriteAllText(
                        sourcePath,
                        "module Tracer\n#if FEATURE\nlet answer () = )\n#else\nlet answer () = 42\n#endif\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            "--define:OTHER"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    let consumer = invokeConsumer root outputPath 42
                    Expect.equal consumer.ExitCode 0 consumer.StandardError
                finally
                    Directory.Delete(root, true)

            testCase "keeps nullable type abbreviations in the semantic surface only"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-type-abbreviations",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "Nullness.fs")
                    let outputPath = Path.Combine(root, "Aliases.dll")
                    let pdbPath = Path.Combine(root, "Aliases.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace Aliases\n\nopen System\n\ntype ExceptionNull =\n#if NULLABLE\n    Exception | null\n#else\n    Exception\n#endif\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            "--define:NULLABLE"
                            $"--reference:{typeof<Exception>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let typeNames =
                        metadata.TypeDefinitions
                        |> Seq.map (fun handle ->
                            let definition = metadata.GetTypeDefinition(handle)
                            metadata.GetString(definition.Name)
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        typeNames
                        [| "<Module>" |]
                        "an F# type abbreviation should not add a CLI runtime type"
                finally
                    Directory.Delete(root, true)

            testCase "generic type abbreviations preserve parameter and constraint semantics"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-generic-type-abbreviations",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let tracePath = Path.Combine(root, "compile.trace")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<System.Runtime.CompilerServices.ICriticalNotifyCompletion>.Assembly.Location}"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            $"--fsharp2-trace:{tracePath}"
                            sourcePath
                        |]
                    )

                    let compile (source: string) =
                        File.WriteAllText(sourcePath, source)
                        let result = invokeFsc2 root responsePath

                        Expect.equal
                            result.ExitCode
                            0
                            (result.StandardOutput
                             + result.StandardError)

                        (readTrace tracePath).["exportFingerprint"]

                    let multilineSource =
                        "namespace IcedTasks.TaskLike\n\nopen System.Runtime.CompilerServices\n\ntype Awaiter<'Awaiter, 'TResult\n    when 'Awaiter :> ICriticalNotifyCompletion\n    and 'Awaiter: (member get_IsCompleted: unit -> bool)\n    and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter\n"

                    let compactSource =
                        "namespace IcedTasks.TaskLike\nopen System.Runtime.CompilerServices\ntype Awaiter<'Awaiter, 'TResult when 'Awaiter :> ICriticalNotifyCompletion and 'Awaiter: (member get_IsCompleted: unit -> bool) and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter\n"

                    let changedConstraintSource =
                        compactSource.Replace(
                            "ICriticalNotifyCompletion",
                            "INotifyCompletion",
                            StringComparison.Ordinal
                        )

                    let qualifiedSource =
                        compactSource
                            .Replace(
                                "ICriticalNotifyCompletion",
                                "System.Runtime.CompilerServices.ICriticalNotifyCompletion",
                                StringComparison.Ordinal
                            )
                            .Replace(
                                "unit ->",
                                "Microsoft.FSharp.Core.Unit ->",
                                StringComparison.Ordinal
                            )
                            .Replace("bool", "System.Boolean", StringComparison.Ordinal)

                    let multilineFingerprint = compile multilineSource
                    let compactFingerprint = compile compactSource
                    let qualifiedFingerprint = compile qualifiedSource
                    let changedConstraintFingerprint = compile changedConstraintSource

                    Expect.equal
                        compactFingerprint
                        multilineFingerprint
                        "formatting must not change a generic abbreviation's exported meaning"

                    Expect.equal
                        qualifiedFingerprint
                        compactFingerprint
                        "aliases and opened names must resolve to the same target-reference identities"

                    Expect.notEqual
                        changedConstraintFingerprint
                        compactFingerprint
                        "a changed generic constraint must change the exported semantic fingerprint"

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let typeNames =
                        metadata.TypeDefinitions
                        |> Seq.map (fun handle ->
                            let definition = metadata.GetTypeDefinition(handle)
                            metadata.GetString(definition.Name)
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        typeNames
                        [| "<Module>" |]
                        "an F# generic type abbreviation should not add a CLI runtime type"
                finally
                    Directory.Delete(root, true)

            testCase "emits the Oracle CLR stub for an inline statically resolved member"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-srtp-stub",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskLike\n\nopen System.Runtime.CompilerServices\n\ntype Awaiter<'Awaiter, 'TResult\n    when 'Awaiter :> ICriticalNotifyCompletion\n    and 'Awaiter: (member get_IsCompleted: unit -> bool)\n    and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter\n\ntype Awaiter =\n    static member inline IsCompleted<'Awaiter, 'TResult when Awaiter<'Awaiter, 'TResult>>\n        (awaiter: 'Awaiter)\n        =\n        awaiter.get_IsCompleted ()\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<System.Runtime.CompilerServices.ICriticalNotifyCompletion>.Assembly.Location}"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let awaiterType =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskLike"
                            && metadata.GetString(definition.Name) = "Awaiter"
                        )

                    Expect.equal
                        awaiterType.Attributes
                        (enum<TypeAttributes> 0x00002001)
                        "the CLR helper type attributes should match the Compatibility Oracle"

                    let methodDefinition =
                        awaiterType.GetMethods()
                        |> Seq.map metadata.GetMethodDefinition
                        |> Seq.find (fun methodDefinition ->
                            metadata.GetString(methodDefinition.Name) = "IsCompleted"
                        )

                    Expect.equal
                        methodDefinition.Attributes
                        (MethodAttributes.Public
                         ||| MethodAttributes.Static)
                        "the inline member's callable CLR stub should match the Oracle surface"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(methodDefinition.Signature))
                        (Convert.FromHexString("100201021E00"))
                        "the generic method signature should be bool IsCompleted<Awaiter,TResult>(Awaiter)"

                    let genericParameters =
                        methodDefinition.GetGenericParameters()
                        |> Seq.map (fun handle ->
                            let parameter = metadata.GetGenericParameter(handle)

                            parameter.Index,
                            metadata.GetString(parameter.Name),
                            parameter.Attributes
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        genericParameters
                        [|
                            0, "Awaiter", GenericParameterAttributes.None
                            1, "TResult", GenericParameterAttributes.None
                        |]
                        "the method generic parameters should preserve source order and names"

                    let parameters =
                        methodDefinition.GetParameters()
                        |> Seq.map (fun handle ->
                            let parameter = metadata.GetParameter(handle)

                            parameter.SequenceNumber,
                            metadata.GetString(parameter.Name),
                            parameter.Attributes
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        parameters
                        [| 1, "awaiter", ParameterAttributes.None |]
                        "the CLR parameter row should match the Compatibility Oracle"

                    let body = implementation.GetMethodBody(methodDefinition.RelativeVirtualAddress)
                    let il = body.GetILBytes()

                    Expect.sequenceEqual
                        [|
                            il.[0]
                            il.[5]
                            il.[10]
                        |]
                        [|
                            0x72uy
                            0x73uy
                            0x7auy
                        |]
                        "the CLR stub should load the message, construct the exception, and throw"

                    Expect.equal
                        il.Length
                        11
                        "the CLR stub should contain only the Oracle throw path"

                    let messageToken = BitConverter.ToInt32(il, 1)

                    let message =
                        messageToken
                        &&& 0x00ffffff
                        |> MetadataTokens.UserStringHandle
                        |> metadata.GetUserString

                    Expect.equal
                        message
                        "Dynamic invocation of get_IsCompleted is not supported"
                        "the dynamic-invocation failure message should match the Compatibility Oracle"

                    let constructor =
                        metadata.MemberReferences
                        |> Seq.exactlyOne
                        |> metadata.GetMemberReference

                    let declaringType =
                        constructor.Parent
                        |> MetadataTokens.GetRowNumber
                        |> MetadataTokens.TypeReferenceHandle
                        |> metadata.GetTypeReference

                    Expect.equal
                        (metadata.GetString(declaringType.Namespace),
                         metadata.GetString(declaringType.Name))
                        ("System", "NotSupportedException")
                        "the CLR stub should construct System.NotSupportedException"

                    Expect.equal
                        (metadata.GetString(constructor.Name))
                        ".ctor"
                        "the CLR stub should call the exception constructor"
                finally
                    Directory.Delete(root, true)

            testCase "emits tupled inline-member parameters in Oracle order"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-srtp-tuple-parameters",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskLike\n\nopen System.Runtime.CompilerServices\n\ntype Awaiter<'Awaiter, 'TResult\n    when 'Awaiter :> ICriticalNotifyCompletion\n    and 'Awaiter: (member get_IsCompleted: unit -> bool)\n    and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter\n\ntype Awaiter =\n    static member inline IsCompleted<'Awaiter, 'TResult when Awaiter<'Awaiter, 'TResult>>\n        (awaiter: 'Awaiter, context: 'TResult)\n        =\n        awaiter.get_IsCompleted ()\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<System.Runtime.CompilerServices.ICriticalNotifyCompletion>.Assembly.Location}"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let methodDefinition =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskLike"
                            && metadata.GetString(definition.Name) = "Awaiter"
                        )
                        |> _.GetMethods()
                        |> Seq.map metadata.GetMethodDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Name) = "IsCompleted"
                        )

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(methodDefinition.Signature))
                        (Convert.FromHexString("100202021E001E01"))
                        "the generic signature should preserve both tupled parameter types"

                    let parameters =
                        methodDefinition.GetParameters()
                        |> Seq.map (fun handle ->
                            let parameter = metadata.GetParameter(handle)

                            parameter.SequenceNumber,
                            metadata.GetString(parameter.Name),
                            parameter.Attributes
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        parameters
                        [|
                            1, "awaiter", ParameterAttributes.None
                            2, "context", ParameterAttributes.None
                        |]
                        "the CLR parameter rows should preserve Oracle source order and names"
                finally
                    Directory.Delete(root, true)

            testCase "emits the Oracle stub for a trait call with an argument"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-srtp-call-argument",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskLike\n\ntype Awaiter<'Awaiter, 'TResult\n    when 'Awaiter: (member Check: 'TResult -> bool)> = 'Awaiter\n\ntype Awaiter =\n    static member inline Check<'Awaiter, 'TResult when Awaiter<'Awaiter, 'TResult>>\n        (awaiter: 'Awaiter, context: 'TResult)\n        =\n        awaiter.Check(context)\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let methodDefinition =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskLike"
                            && metadata.GetString(definition.Name) = "Awaiter"
                        )
                        |> _.GetMethods()
                        |> Seq.map metadata.GetMethodDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Name) = "Check"
                        )

                    let il =
                        implementation
                            .GetMethodBody(methodDefinition.RelativeVirtualAddress)
                            .GetILBytes()

                    let message =
                        BitConverter.ToInt32(il, 1)
                        &&& 0x00ffffff
                        |> MetadataTokens.UserStringHandle
                        |> metadata.GetUserString

                    Expect.equal
                        message
                        "Dynamic invocation of Check is not supported"
                        "the one-argument trait call should preserve the Oracle member identity"
                finally
                    Directory.Delete(root, true)

            testCase "emits an inline stub from a direct member constraint"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-direct-member-constraint",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskLike\n\ntype MethodBuilder =\n    static member inline Check<'Builder, 'TResult\n        when 'Builder: (member Check: 'TResult -> bool)>\n        (builder: 'Builder, context: 'TResult)\n        =\n        builder.Check(context)\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let methodDefinition =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskLike"
                            && metadata.GetString(definition.Name) = "MethodBuilder"
                        )
                        |> _.GetMethods()
                        |> Seq.map metadata.GetMethodDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Name) = "Check"
                        )

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(methodDefinition.Signature))
                        (Convert.FromHexString("100202021E001E01"))
                        "the direct constraint should preserve the Oracle method signature"

                    let il =
                        implementation
                            .GetMethodBody(methodDefinition.RelativeVirtualAddress)
                            .GetILBytes()

                    let message =
                        BitConverter.ToInt32(il, 1)
                        &&& 0x00ffffff
                        |> MetadataTokens.UserStringHandle
                        |> metadata.GetUserString

                    Expect.equal
                        message
                        "Dynamic invocation of Check is not supported"
                        "the direct member constraint should resolve the Oracle trait identity"
                finally
                    Directory.Delete(root, true)

            testCase "emits the Oracle byref signature for an address-of trait call"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-address-of-trait-call",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskLike\n\ntype MethodBuilder =\n    static member inline Start<'Builder, 'TStateMachine\n        when 'Builder: (member Start: byref<'TStateMachine> -> unit)>\n        (builder: byref<'Builder>, stateMachine: byref<'TStateMachine>)\n        =\n        builder.Start(&stateMachine)\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let methodDefinition =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskLike"
                            && metadata.GetString(definition.Name) = "MethodBuilder"
                        )
                        |> _.GetMethods()
                        |> Seq.map metadata.GetMethodDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Name) = "Start"
                        )

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(methodDefinition.Signature))
                        (Convert.FromHexString("10020201101E00101E01"))
                        "the Start stub should preserve the Oracle void and byref signature"

                    let il =
                        implementation
                            .GetMethodBody(methodDefinition.RelativeVirtualAddress)
                            .GetILBytes()

                    let message =
                        BitConverter.ToInt32(il, 1)
                        &&& 0x00ffffff
                        |> MetadataTokens.UserStringHandle
                        |> metadata.GetUserString

                    Expect.equal
                        message
                        "Dynamic invocation of Start is not supported"
                        "the address-of argument should preserve the Oracle trait identity"
                finally
                    Directory.Delete(root, true)

            testCase "emits the Oracle stub for a tupled member-constraint domain"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-tupled-constraint-domain",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskLike.fs")
                    let outputPath = Path.Combine(root, "TaskLike.dll")
                    let pdbPath = Path.Combine(root, "TaskLike.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskLike\n\nopen System.Runtime.CompilerServices\n\ntype MethodBuilder =\n    static member inline AwaitUnsafeOnCompleted<'Builder, 'TAwaiter, 'TStateMachine\n        when 'Builder: (member AwaitUnsafeOnCompleted:\n            byref<'TAwaiter> * byref<'TStateMachine> -> unit)\n        and 'TAwaiter :> ICriticalNotifyCompletion\n        and 'TStateMachine :> IAsyncStateMachine>\n        (builder: byref<'Builder>, awaiter: byref<'TAwaiter>, stateMachine: byref<'TStateMachine>)\n        =\n        builder.AwaitUnsafeOnCompleted(&awaiter, &stateMachine)\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<System.Runtime.CompilerServices.ICriticalNotifyCompletion>.Assembly.Location}"
                            $"--reference:{typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let methodDefinition =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskLike"
                            && metadata.GetString(definition.Name) = "MethodBuilder"
                        )
                        |> _.GetMethods()
                        |> Seq.map metadata.GetMethodDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Name) = "AwaitUnsafeOnCompleted"
                        )

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(methodDefinition.Signature))
                        (Convert.FromHexString("10030301101E00101E01101E02"))
                        "the tupled constraint should preserve all three Oracle byref parameters"

                    let il =
                        implementation
                            .GetMethodBody(methodDefinition.RelativeVirtualAddress)
                            .GetILBytes()

                    let message =
                        BitConverter.ToInt32(il, 1)
                        &&& 0x00ffffff
                        |> MetadataTokens.UserStringHandle
                        |> metadata.GetUserString

                    Expect.equal
                        message
                        "Dynamic invocation of AwaitUnsafeOnCompleted is not supported"
                        "the tupled constraint should preserve the Oracle trait identity"
                finally
                    Directory.Delete(root, true)

            testCase "emits semicolon-separated type attributes on the IcedTasks state-data struct"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-icedtasks-struct-attributes",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
                    let outputPath = Path.Combine(root, "TaskBuilderBase.dll")
                    let pdbPath = Path.Combine(root, "TaskBuilderBase.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    [<Struct; NoComparison; NoEquality>]\n    type TaskBaseStateMachineData<'T, 'Builder> =\n        [<DefaultValue(false)>]\n        val mutable Result: 'T\n\n        [<DefaultValue(false)>]\n        val mutable MethodBuilder: 'Builder\n"
                    )

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<Microsoft.FSharp.Core.StructAttribute>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        0
                        (result.StandardOutput
                         + result.StandardError)

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let moduleHandle =
                        metadata.TypeDefinitions
                        |> Seq.find (fun handle ->
                            let definition = metadata.GetTypeDefinition(handle)

                            metadata.GetString(definition.Namespace) = "IcedTasks.TaskBase"
                            && metadata.GetString(definition.Name) = "TaskBase"
                        )

                    let moduleDefinition = metadata.GetTypeDefinition(moduleHandle)

                    Expect.equal
                        moduleDefinition.Attributes
                        (TypeAttributes.Public
                         ||| TypeAttributes.Abstract
                         ||| TypeAttributes.Sealed)
                        "the AutoOpen module container should match the Oracle flags"

                    let moduleAttributes =
                        moduleDefinition.GetCustomAttributes()
                        |> readCustomAttributes metadata

                    Expect.sequenceEqual
                        (moduleAttributes
                         |> Array.map (fun (name, _, _) -> name))
                        [|
                            "Microsoft.FSharp.Core.AutoOpenAttribute"
                            "Microsoft.FSharp.Core.CompilationMappingAttribute"
                        |]
                        "the module should preserve its authored and compiler mapping attributes"

                    Expect.sequenceEqual
                        (let _, _, value = moduleAttributes.[1] in value)
                        (Convert.FromHexString("0100070000000000"))
                        "the module mapping should use the Oracle Module construct flag"

                    let stateDataHandle =
                        metadata.TypeDefinitions
                        |> Seq.find (fun handle ->
                            let definition = metadata.GetTypeDefinition(handle)
                            metadata.GetString(definition.Name) = "TaskBaseStateMachineData`2"
                        )

                    let stateData = metadata.GetTypeDefinition(stateDataHandle)

                    Expect.isTrue
                        stateData.Namespace.IsNil
                        "the nested state-data type should not repeat its enclosing namespace"

                    Expect.equal
                        (stateData.GetDeclaringType())
                        moduleHandle
                        "the state-data struct should be nested under the TaskBase module"

                    Expect.equal
                        stateData.Attributes
                        (TypeAttributes.NestedPublic
                         ||| TypeAttributes.SequentialLayout
                         ||| TypeAttributes.Sealed
                         ||| enum<TypeAttributes> 0x00002000
                         ||| TypeAttributes.BeforeFieldInit)
                        "the state-data struct flags should match the Compatibility Oracle"

                    let typeParameters =
                        stateData.GetGenericParameters()
                        |> Seq.map (
                            metadata.GetGenericParameter
                            >> fun parameter ->
                                parameter.Index,
                                metadata.GetString(parameter.Name),
                                parameter.Attributes
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        typeParameters
                        [|
                            0, "T", GenericParameterAttributes.None
                            1, "Builder", GenericParameterAttributes.None
                        |]
                        "the nested struct generic parameters should preserve Oracle order"

                    let typeAttributes =
                        stateData.GetCustomAttributes()
                        |> readCustomAttributes metadata

                    Expect.sequenceEqual
                        (typeAttributes
                         |> Array.map (fun (name, _, _) -> name))
                        [|
                            "Microsoft.FSharp.Core.StructAttribute"
                            "Microsoft.FSharp.Core.NoComparisonAttribute"
                            "Microsoft.FSharp.Core.NoEqualityAttribute"
                            "Microsoft.FSharp.Core.CompilationMappingAttribute"
                        |]
                        "the semicolon-separated type attributes should preserve Oracle order"

                    Expect.sequenceEqual
                        (let _, _, value = typeAttributes.[3] in value)
                        (Convert.FromHexString("0100030000000000"))
                        "the state-data mapping should use the Oracle ObjectType construct flag"

                    let fields =
                        stateData.GetFields()
                        |> Seq.map (fun handle ->
                            let field = metadata.GetFieldDefinition(handle)

                            let customAttributes =
                                field.GetCustomAttributes()
                                |> readCustomAttributes metadata

                            metadata.GetString(field.Name),
                            field.Attributes,
                            metadata.GetBlobBytes(field.Signature),
                            customAttributes
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        (fields
                         |> Array.map (fun (name, attributes, signature, _) ->
                             name, attributes, Convert.ToHexString(signature)
                         ))
                        [|
                            "Result", FieldAttributes.Public, "061300"
                            "MethodBuilder", FieldAttributes.Public, "061301"
                        |]
                        "the public mutable fields should reference the enclosing generic parameters"

                    for _, _, _, customAttributes in fields do
                        Expect.sequenceEqual
                            (customAttributes
                             |> Array.map (fun (name, _, _) -> name))
                            [| "Microsoft.FSharp.Core.DefaultValueAttribute" |]
                            "each field should carry DefaultValueAttribute"

                        let _, constructor, value = customAttributes.[0]

                        Expect.sequenceEqual
                            constructor
                            (Convert.FromHexString("20010102"))
                            "DefaultValueAttribute should use its bool constructor"

                        Expect.sequenceEqual
                            value
                            (Convert.FromHexString("0100000000"))
                            "DefaultValue(false) should preserve the Oracle fixed argument"
                finally
                    Directory.Delete(root, true)

            testCase
                "keeps the mutually defined IcedTasks state-machine abbreviations out of CLR metadata"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-icedtasks-state-machine-abbreviations",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    let sourceText resumptionFunctionType =
                        $"namespace IcedTasks.TaskBase\n\nopen IcedTasks.Nullness\nopen IcedTasks.TaskLike\n\n[<AutoOpen>]\nmodule TaskBase =\n    open System\n    open System.Runtime.CompilerServices\n    open System.Threading.Tasks\n    open Microsoft.FSharp.Core\n    open Microsoft.FSharp.Core.CompilerServices\n    open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers\n    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators\n    open Microsoft.FSharp.Collections\n    open System.Collections.Generic\n    open System.Threading\n\n    [<Struct; NoComparison; NoEquality>]\n    type TaskBaseStateMachineData<'T, 'Builder> =\n        [<DefaultValue(false)>]\n        val mutable Result: 'T\n\n        [<DefaultValue(false)>]\n        val mutable MethodBuilder: 'Builder\n\n    and TaskBaseStateMachine<'TOverall, 'Builder> =\n        ResumableStateMachine<TaskBaseStateMachineData<'TOverall, 'Builder>>\n\n    and TaskBaseResumptionFunc<'TOverall, 'Builder> =\n        {resumptionFunctionType}<TaskBaseStateMachineData<'TOverall, 'Builder>>\n\n    and TaskBaseResumptionDynamicInfo<'TOverall, 'Builder> =\n        ResumptionDynamicInfo<TaskBaseStateMachineData<'TOverall, 'Builder>>\n\n    and TaskBaseCode<'TOverall, 'T, 'Builder> =\n        ResumableCode<TaskBaseStateMachineData<'TOverall, 'Builder>, 'T>\n"

                    let compile suffix resumptionFunctionType =
                        File.WriteAllText(sourcePath, sourceText resumptionFunctionType)

                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            $"TaskBuilderBase-{suffix}"
                            []

                    let outputPath, baselineFingerprint = compile "baseline" "ResumptionFunc"

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    let emittedTypeNames =
                        metadata.TypeDefinitions
                        |> Seq.map (
                            metadata.GetTypeDefinition
                            >> fun definition -> metadata.GetString(definition.Name)
                        )
                        |> Seq.toArray

                    Expect.contains
                        emittedTypeNames
                        "TaskBaseStateMachineData`2"
                        "the concrete state-data struct should remain in CLR metadata"

                    for abbreviation in
                        [|
                            "TaskBaseStateMachine`2"
                            "TaskBaseResumptionFunc`2"
                            "TaskBaseResumptionDynamicInfo`2"
                            "TaskBaseCode`3"
                        |] do
                        Expect.isFalse
                            (emittedTypeNames
                             |> Array.contains abbreviation)
                            $"the type abbreviation {abbreviation} should remain erased"

                    let _, editedFingerprint = compile "edited" "ResumptionDynamicInfo"

                    Expect.notEqual
                        editedFingerprint
                        baselineFingerprint
                        "an alias-target edit must invalidate the exported semantic fingerprint"
                finally
                    Directory.Delete(root, true)

            testCase "includes referenced declaration identity in alias export fingerprints"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-reference-type-identity",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "Alias.fs")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let firstReferencePath = Path.Combine(root, "FirstReference.dll")
                    let secondReferencePath = Path.Combine(root, "SecondReference.dll")

                    writeReferenceTypeAssembly
                        firstReferencePath
                        "FirstReference"
                        "Collision"
                        "Shared`1"

                    writeReferenceTypeAssembly
                        secondReferencePath
                        "SecondReference"
                        "Collision"
                        "Shared`1"

                    File.WriteAllText(
                        sourcePath,
                        "namespace Collision\ntype Alias<'T> = Shared<'T>\n"
                    )

                    let compile referencePath outputName =
                        compileForExportFingerprint root responsePath sourcePath outputName [
                            referencePath
                        ]
                        |> snd

                    let firstFingerprint = compile firstReferencePath "first"
                    let secondFingerprint = compile secondReferencePath "second"

                    Expect.notEqual
                        secondFingerprint
                        firstFingerprint
                        "the referenced assembly is part of an exported alias target's identity"
                finally
                    Directory.Delete(root, true)

            testCase
                "matches Oracle CLR metadata and portable-PDB shape for an IcedTasks-style object type"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-icedtasks-object-type",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    let sourceText =
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n"

                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Zero")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Zero")
                        "the emitted object type should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)

                    let zero =
                        builderType.GetMethod(
                            "Zero",
                            BindingFlags.Public
                            ||| BindingFlags.Instance
                        )

                    Expect.equal
                        (zero.Invoke(builder, Array.empty<obj>))
                        (box 0)
                        "the emitted object member should execute its F# body"
                finally
                    Directory.Delete(root, true)

            testCase "matches Oracle shape and executes a parameterized inline object member"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-parameterized-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Return(value: int) : int = value\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Return")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Return")
                        "the parameterized member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)
                    let returnMethod = builderType.GetMethod("Return")

                    Expect.equal
                        (returnMethod.Invoke(builder, [| box 37 |]))
                        (box 37)
                        "the emitted parameterized member should return its argument"
                finally
                    Directory.Delete(root, true)

            testCase "infers an inline object member generic parameter in Oracle order"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-generic-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Return(value: 'T) : 'T = value\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Return")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Return")
                        "the generic member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)

                    let returnMethod =
                        builderType.GetMethod("Return").MakeGenericMethod(typeof<int>)

                    Expect.equal
                        (returnMethod.Invoke(builder, [| box 37 |]))
                        (box 37)
                        "the emitted generic member should return its specialized argument"
                finally
                    Directory.Delete(root, true)

            testCase "matches the Oracle flexible-type constraint for an object member"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-flexible-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    let sourceText =
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Identity(resource: #System.IAsyncDisposable) = resource\n"

                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, baselineExportFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    let baselineTrace =
                        Path.Combine(root, "TaskBuilderBase-fsharp2.trace")
                        |> readTrace

                    Expect.equal
                        baselineTrace.["dependencyCount"]
                        "2"
                        "the object constructor and constrained interface should be explicit symbolic dependencies"

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Identity")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Identity")
                        "the flexible-type member should match the Compatibility Oracle's CLR constraints and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)

                    let identityMethod =
                        builderType.GetMethod("Identity").MakeGenericMethod(typeof<MemoryStream>)

                    use resource = new MemoryStream()

                    Expect.isTrue
                        (Object.ReferenceEquals(
                            identityMethod.Invoke(builder, [| resource |]),
                            resource
                        ))
                        "the emitted flexible-type member should return its constrained argument"

                    File.WriteAllText(
                        sourcePath,
                        sourceText.Replace(
                            "System.IAsyncDisposable",
                            "System.IDisposable",
                            StringComparison.Ordinal
                        )
                    )

                    let _, editedExportFingerprint =
                        compileForExportFingerprint
                            root
                            (Path.Combine(root, "fsharp2-edited.rsp"))
                            sourcePath
                            "TaskBuilderBase-fsharp2-edited"
                            []

                    let editedTrace =
                        Path.Combine(root, "TaskBuilderBase-fsharp2-edited.trace")
                        |> readTrace

                    Expect.notEqual
                        editedExportFingerprint
                        baselineExportFingerprint
                        "a changed flexible constraint should change consumer-visible export identity"

                    Expect.notEqual
                        editedTrace.["fragmentHash"]
                        baselineTrace.["fragmentHash"]
                        "a changed flexible constraint should invalidate the symbolic method fragment"

                    File.WriteAllText(
                        sourcePath,
                        sourceText.Replace(
                            "System.IAsyncDisposable",
                            "System.Collections.Generic.IEnumerable<System.IDisposable>",
                            StringComparison.Ordinal
                        )
                    )

                    let _ =
                        compileForExportFingerprint
                            root
                            (Path.Combine(root, "fsharp2-generic.rsp"))
                            sourcePath
                            "TaskBuilderBase-fsharp2-generic"
                            []

                    let genericTrace =
                        Path.Combine(root, "TaskBuilderBase-fsharp2-generic.trace")
                        |> readTrace

                    Expect.equal
                        genericTrace.["dependencyCount"]
                        "3"
                        "a generic flexible constraint should depend on its type definition and argument declarations"
                finally
                    Directory.Delete(root, true)

            testCase "matches the Oracle DefaultValue attribute on an object member"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-default-value-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    let attributedSource =
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        [<DefaultValue>]\n        member inline _.Zero() = 0\n"

                    File.WriteAllText(sourcePath, attributedSource)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, attributedFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Zero")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Zero")
                        "the attributed member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)
                    let zeroMethod = builderType.GetMethod("Zero")

                    Expect.equal
                        (zeroMethod.Invoke(builder, [||]))
                        (box 0)
                        "the attributed member should remain executable"

                    File.WriteAllText(
                        sourcePath,
                        attributedSource.Replace(
                            "        [<DefaultValue>]\n",
                            "\n        [<DefaultValue>]\n"
                        )
                    )

                    let _, relocatedFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-relocated-attribute"
                            []

                    Expect.equal
                        relocatedFingerprint
                        attributedFingerprint
                        "moving an attribute without changing its meaning should preserve the export fingerprint"

                    File.WriteAllText(
                        sourcePath,
                        attributedSource.Replace("        [<DefaultValue>]\n", String.Empty)
                    )

                    let _, unattributedFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-without-attribute"
                            []

                    Expect.notEqual
                        unattributedFingerprint
                        attributedFingerprint
                        "removing a consumer-visible member attribute should change the export fingerprint"
                finally
                    Directory.Delete(root, true)

            testCase "matches the Oracle InlineIfLambda attribute on an object-member parameter"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-if-lambda-parameter",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                let pipeName =
                    "fsharp2-"
                    + Guid.NewGuid().ToString("N")

                use service = startCompilerService root pipeName

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    let attributedSource =
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.While\n            (\n                [<InlineIfLambda>] guard: unit -> bool,\n                computation: int\n            ) : int =\n            computation\n"

                    File.WriteAllText(sourcePath, attributedSource)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, attributedFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "While")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "While")
                        "the attributed parameter should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)
                    let whileMethod = builderType.GetMethod("While")

                    Expect.equal
                        (whileMethod.Invoke(
                            builder,
                            [|
                                null
                                box 42
                            |]
                        ))
                        (box 42)
                        "the attributed parameter should remain executable"

                    File.WriteAllText(
                        sourcePath,
                        attributedSource.Replace(
                            "                [<InlineIfLambda>] guard",
                            "\n                [<InlineIfLambda>] guard"
                        )
                    )

                    let _, relocatedFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-relocated-parameter-attribute"
                            []

                    Expect.equal
                        relocatedFingerprint
                        attributedFingerprint
                        "moving a parameter attribute without changing its meaning should preserve the export fingerprint"

                    File.WriteAllText(
                        sourcePath,
                        attributedSource.Replace("[<InlineIfLambda>] ", String.Empty)
                    )

                    let _, unattributedFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-without-parameter-attribute"
                            []

                    Expect.notEqual
                        unattributedFingerprint
                        attributedFingerprint
                        "removing a consumer-visible parameter attribute should change the export fingerprint"

                    let retainedOutputPath = Path.Combine(root, "TaskBuilderBase-retained.dll")
                    let retainedPdbPath = Path.Combine(root, "TaskBuilderBase-retained.pdb")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    let compileRetained (sourceText: string) traceName =
                        let retainedResponsePath =
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
                            retainedResponsePath,
                            [|
                                $"--fsharp2-server:{pipeName}"
                                $"--fsharp2-trace:{tracePath}"
                                "--target:library"
                                "--deterministic+"
                                "--debug:portable"
                                $"--reference:{typeof<Microsoft.FSharp.Core.InlineIfLambdaAttribute>.Assembly.Location}"
                                $"--reference:{systemRuntimePath}"
                                $"--out:{retainedOutputPath}"
                                $"--pdb:{retainedPdbPath}"
                                sourcePath
                            |]
                        )

                        let result = invokeFsc2 root retainedResponsePath

                        Expect.equal
                            result.ExitCode
                            0
                            (result.StandardOutput
                             + result.StandardError)

                        readTrace tracePath

                    let unattributedSource =
                        attributedSource.Replace("[<InlineIfLambda>] ", String.Empty)

                    let retainedBaseline = compileRetained attributedSource "retained-baseline"
                    let retainedEdited = compileRetained unattributedSource "retained-edited"
                    let retainedReplay = compileRetained unattributedSource "retained-replay"

                    for traceName, trace in
                        [
                            "baseline", retainedBaseline
                            "edited", retainedEdited
                            "replay", retainedReplay
                        ] do
                        Expect.equal
                            trace.["servicePid"]
                            retainedBaseline.["servicePid"]
                            $"the retained {traceName} request should use the same compiler service"

                    Expect.notEqual
                        retainedEdited.["exportFingerprint"]
                        retainedBaseline.["exportFingerprint"]
                        "removing the parameter attribute should invalidate consumer-visible meaning"

                    Expect.notEqual
                        retainedEdited.["fragmentHash"]
                        retainedBaseline.["fragmentHash"]
                        "removing the emitted parameter attribute should invalidate the symbolic fragment"

                    Expect.equal
                        retainedEdited.["previousContentFingerprint"]
                        retainedBaseline.["contentFingerprint"]
                        "the attribute edit should identify the retained semantic state it replaced"

                    Expect.equal
                        retainedEdited.["invalidationReason"]
                        "source-content-changed"
                        "the trace should explain the attribute edit"

                    Expect.equal
                        retainedEdited.["parse"]
                        "miss"
                        "the attribute edit should be reparsed"

                    Expect.equal
                        retainedEdited.["check"]
                        "miss"
                        "the attribute edit should be rechecked"

                    Expect.equal
                        retainedEdited.["lower"]
                        "miss"
                        "the attribute edit should be re-emitted"

                    Expect.equal
                        retainedReplay.["parse"]
                        "hit"
                        "an identical attribute-free replay should reuse parsing"

                    Expect.equal
                        retainedReplay.["check"]
                        "hit"
                        "an identical attribute-free replay should reuse checking"

                    Expect.equal
                        retainedReplay.["lower"]
                        "hit"
                        "an identical attribute-free replay should reuse symbolic lowering"
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

                    Directory.Delete(root, true)

            testCase "accepts InlineIfLambda on an F# delegate parameter like the Oracle"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-inline-if-lambda-delegate-parameter",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let delegateSourcePath = Path.Combine(root, "DelegateLibrary.fs")
                    let delegateAssemblyPath = Path.Combine(root, "DelegateLibrary.dll")
                    let delegateResponsePath = Path.Combine(root, "delegate.rsp")

                    File.WriteAllText(
                        delegateSourcePath,
                        "namespace DelegateLibrary\n\ntype Callback = delegate of unit -> unit\n"
                    )

                    let delegateResult =
                        invokeCompatibilityOracle root delegateResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--optimize-"
                            $"--out:{delegateAssemblyPath}"
                            delegateSourcePath
                        ]

                    Expect.equal
                        delegateResult.ExitCode
                        0
                        (delegateResult.StandardOutput
                         + delegateResult.StandardError)

                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\nopen DelegateLibrary\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Delay([<InlineIfLambda>] callback: Callback) : int = 42\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--reference:{delegateAssemblyPath}"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            (Path.Combine(root, "fsharp2.rsp"))
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            [ delegateAssemblyPath ]

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Delay")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Delay")
                        "the F# delegate parameter should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let loadContext =
                        new System.Runtime.Loader.AssemblyLoadContext(
                            "fsharp2-inline-if-lambda-delegate",
                            isCollectible = true
                        )

                    use delegateStream = File.OpenRead(delegateAssemblyPath)
                    use outputStream = File.OpenRead(outputPath)

                    loadContext.LoadFromStream(delegateStream)
                    |> ignore

                    let emittedAssembly = loadContext.LoadFromStream(outputStream)

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)
                    let delayMethod = builderType.GetMethod("Delay")

                    Expect.equal
                        (delayMethod.Invoke(builder, [| null |]))
                        (box 42)
                        "an attributed F# delegate parameter should remain executable"

                    loadContext.Unload()
                finally
                    Directory.Delete(root, true)

            testCase "rejects InlineIfLambda on a non-function parameter like the Oracle"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-invalid-inline-if-lambda-parameter",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Return([<InlineIfLambda>] value: int) : int = value\n"
                    )

                    let fsharpCorePath =
                        typeof<Microsoft.FSharp.Core.InlineIfLambdaAttribute>.Assembly.Location

                    let systemRuntimePath = Assembly.Load("System.Runtime").Location
                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let commonArguments = [
                        "--nologo"
                        "--target:library"
                        "--targetprofile:netcore"
                        "--fullpaths"
                        "--flaterrors"
                        "--utf8output"
                        "--deterministic+"
                        "--debug:portable"
                        "--optimize-"
                        $"--reference:{fsharpCorePath}"
                        $"--reference:{systemRuntimePath}"
                    ]

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            yield! commonArguments
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        1
                        "the Compatibility Oracle should reject InlineIfLambda on an int parameter"

                    Expect.stringContains
                        oracleResult.StandardError
                        "FS3519"
                        "the Compatibility Oracle should report FS3519"

                    let outputPath = Path.Combine(root, "TaskBuilderBase-fsharp2.dll")
                    let pdbPath = Path.Combine(root, "TaskBuilderBase-fsharp2.pdb")
                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    File.WriteAllLines(
                        responsePath,
                        [|
                            yield! commonArguments
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath

                    Expect.equal
                        result.ExitCode
                        oracleResult.ExitCode
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    Expect.equal
                        result.StandardOutput
                        oracleResult.StandardOutput
                        "the invalid attribute should have the Oracle standard-output behavior"

                    Expect.equal
                        result.StandardError
                        oracleResult.StandardError
                        "the invalid attribute should have the exact Oracle diagnostic"

                    Expect.isFalse
                        (File.Exists outputPath)
                        "a rejected parameter attribute should not emit an implementation assembly"

                    Expect.isFalse
                        (File.Exists pdbPath)
                        "a rejected parameter attribute should not emit a portable PDB"
                finally
                    Directory.Delete(root, true)

            testCase "encodes an inline object member function parameter and return type"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-function-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Delay(generator: unit -> 'T) : unit -> 'T = generator\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Delay")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Delay")
                        "the function-typed member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)

                    let delayMethod = builderType.GetMethod("Delay").MakeGenericMethod(typeof<int>)

                    let generator () = 37

                    let returned =
                        delayMethod.Invoke(builder, [| box generator |]) :?> (unit -> int)

                    Expect.equal
                        (returned ())
                        37
                        "the emitted function-typed member should return its function argument"
                finally
                    Directory.Delete(root, true)

            testCase "encodes a referenced generic object member parameter and return type"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-referenced-generic-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Identity(values: System.Collections.Generic.IEnumerable<'T>) : System.Collections.Generic.IEnumerable<'T> = values\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Identity")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Identity")
                        "the referenced generic member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)

                    let identityMethod =
                        builderType.GetMethod("Identity").MakeGenericMethod(typeof<int>)

                    let values = [|
                        1
                        2
                        3
                    |]

                    Expect.isTrue
                        (Object.ReferenceEquals(
                            identityMethod.Invoke(builder, [| values |]),
                            values
                        ))
                        "the emitted generic member should return its referenced generic argument"
                finally
                    Directory.Delete(root, true)

            testCase "erases a local generic abbreviation in an object member signature"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-abbreviated-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type Values<'T> = System.Collections.Generic.IEnumerable<'T>\n\n    type TaskBuilderBase() =\n        member inline _.Identity(values: Values<'T>) : Values<'T> = values\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Identity")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Identity")
                        "the abbreviated member should match the Compatibility Oracle's erased CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)

                    let identityMethod =
                        builderType.GetMethod("Identity").MakeGenericMethod(typeof<int>)

                    let values = [|
                        1
                        2
                        3
                    |]

                    Expect.isTrue
                        (Object.ReferenceEquals(
                            identityMethod.Invoke(builder, [| values |]),
                            values
                        ))
                        "the emitted abbreviated member should return its erased generic argument"
                finally
                    Directory.Delete(root, true)

            testCase "encodes a nested local generic struct in an object member signature"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-local-struct-object-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    File.WriteAllText(
                        sourcePath,
                        "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    [<Struct; NoComparison; NoEquality>]\n    type Data<'T> =\n        [<DefaultValue(false)>]\n        val mutable Value: 'T\n\n    type Values<'T> = System.Collections.Generic.IEnumerable<Data<'T>>\n\n    type TaskBuilderBase() =\n        member inline _.Identity(values: Values<'T>) : Values<'T> = values\n"
                    )

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Identity")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Identity")
                        "the local-struct member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let dataTypeDefinition =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+Data`1",
                            throwOnError = true
                        )

                    let dataType = dataTypeDefinition.MakeGenericType(typeof<int>)

                    let values = Array.CreateInstance(dataType, 1)
                    let builder = Activator.CreateInstance(builderType)

                    let identityMethod =
                        builderType.GetMethod("Identity").MakeGenericMethod(typeof<int>)

                    Expect.isTrue
                        (Object.ReferenceEquals(
                            identityMethod.Invoke(builder, [| values |]),
                            values
                        ))
                        "the emitted member should return its nested local generic argument"
                finally
                    Directory.Delete(root, true)

            testCase "executes an IcedTasks resumable Return member"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-resumable-return-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    let sourceText =
                        "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    [<Struct; NoComparison; NoEquality>]\n    type Data<'T> =\n        [<DefaultValue(false)>]\n        val mutable Result: 'T\n\n    type Code<'T> = ResumableCode<Data<'T>, 'T>\n\n    type TaskBuilderBase() =\n        member inline _.Return(value: 'T) : Code<'T> =\n            Code<'T>(fun sm ->\n                sm.Data.Result <- value\n                true\n            )\n"

                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, baselineExportFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "Return")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "Return")
                        "the resumable Return member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let invokeReturn assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let dataTypeDefinition =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+Data`1",
                                throwOnError = true
                            )

                        let dataType = dataTypeDefinition.MakeGenericType(typeof<int>)

                        let stateMachineType =
                            typedefof<
                                Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine<_>
                             >
                                .MakeGenericType(dataType)

                        let builder = Activator.CreateInstance(builderType)

                        let returnMethod =
                            builderType.GetMethod("Return").MakeGenericMethod(typeof<int>)

                        let code = returnMethod.Invoke(builder, [| box 42 |])
                        let invokeMethod = code.GetType().GetMethod("Invoke")
                        let invokeArguments = [| Activator.CreateInstance(stateMachineType) |]

                        let completed = invokeMethod.Invoke(code, invokeArguments) :?> bool

                        let data = stateMachineType.GetField("Data").GetValue(invokeArguments.[0])

                        let result = dataType.GetField("Result").GetValue(data) :?> int
                        completed, result

                    let oracleBehavior = invokeReturn oracleOutputPath
                    let fsharp2Behavior = invokeReturn outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted resumable function should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (true, 42)
                        "Return should complete and store its captured value in the state-machine data"

                    File.WriteAllText(
                        sourcePath,
                        sourceText.Replace(
                            "    type TaskBuilderBase() =",
                            "\n    type TaskBuilderBase() =",
                            StringComparison.Ordinal
                        )
                    )

                    let _, shiftedExportFingerprint =
                        compileForExportFingerprint
                            root
                            (Path.Combine(root, "fsharp2-shifted.rsp"))
                            sourcePath
                            "TaskBuilderBase-fsharp2-shifted"
                            []

                    Expect.equal
                        shiftedExportFingerprint
                        baselineExportFingerprint
                        "a source-only line shift should preserve the resumable member's semantic export fingerprint"
                finally
                    Directory.Delete(root, true)

            testCase "executes a resumable member with a typed lambda parameter"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    [<Struct; NoComparison; NoEquality>]\n    type Data<'T> =\n        [<DefaultValue(false)>]\n        val mutable Result: 'T\n\n    type Code<'T> = ResumableCode<Data<'T>, 'T>\n\n    type TaskBuilderBase() =\n        member inline _.ReturnTyped(value: 'T) : Code<'T> =\n            Code<'T>(fun (sm: byref<ResumableStateMachine<Data<'T>>>) ->\n                sm.Data.Result <- value\n                true\n            )\n"

                withObjectMemberDifferential
                    "fsharp2-typed-resumable-lambda"
                    sourceText
                    "ReturnTyped"
                <| fun oracleOutputPath outputPath ->
                    let invokeReturnTyped assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let dataType =
                            emittedAssembly
                                .GetType("IcedTasks.TaskBase.TaskBase+Data`1", throwOnError = true)
                                .MakeGenericType(typeof<int>)

                        let stateMachineType =
                            typedefof<
                                Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine<_>
                             >
                                .MakeGenericType(dataType)

                        let builder = Activator.CreateInstance(builderType)

                        let code =
                            builderType
                                .GetMethod("ReturnTyped")
                                .MakeGenericMethod(typeof<int>)
                                .Invoke(builder, [| box 42 |])

                        let invokeArguments = [| Activator.CreateInstance(stateMachineType) |]

                        let completed =
                            code.GetType().GetMethod("Invoke").Invoke(code, invokeArguments)
                            :?> bool

                        let data = stateMachineType.GetField("Data").GetValue(invokeArguments.[0])
                        let result = dataType.GetField("Result").GetValue(data) :?> int
                        completed, result

                    let oracleBehavior = invokeReturnTyped oracleOutputPath
                    let fsharp2Behavior = invokeReturnTyped outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the typed resumable lambda should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (true, 42)
                        "ReturnTyped should complete and store its captured value"

            testCase "executes an IcedTasks resumable TryFinally member"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-resumable-try-finally-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")

                    let sourceText =
                        "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type Code<'T> = ResumableCode<int, 'T>\n\n    type TaskBuilderBase() =\n        member inline _.TryFinally\n            (\n                computation: Code<'T>,\n                [<InlineIfLambda>] compensation: unit -> unit\n            ) : Code<'T> =\n            ResumableCode.TryFinally(\n                computation,\n                ResumableCode<_, _>(fun _ ->\n                    compensation ()\n                    true\n                )\n            )\n"

                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oraclePdbPath = Path.Combine(root, "TaskBuilderBase-oracle.pdb")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            $"--pdb:{oraclePdbPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let pdbPath = Path.ChangeExtension(outputPath, ".pdb")

                    Expect.equal
                        (objectTypeMetadataShape outputPath pdbPath "TryFinally")
                        (objectTypeMetadataShape oracleOutputPath oraclePdbPath "TryFinally")
                        "the resumable TryFinally member should match the Compatibility Oracle's CLR and portable-PDB surface"

                    let invokeTryFinally assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let builder = Activator.CreateInstance(builderType)

                        let tryFinallyMethod =
                            builderType.GetMethod("TryFinally").MakeGenericMethod(typeof<int>)

                        let probe = ResumableTryFinallyProbe()

                        let resumableCodeType =
                            typedefof<Microsoft.FSharp.Core.CompilerServices.ResumableCode<_, _>>
                                .MakeGenericType(typeof<int>, typeof<int>)

                        let computation =
                            Delegate.CreateDelegate(
                                resumableCodeType,
                                probe,
                                typeof<ResumableTryFinallyProbe>
                                    .GetMethod(
                                        "InvokeComputation",
                                        BindingFlags.Instance
                                        ||| BindingFlags.Public
                                        ||| BindingFlags.NonPublic
                                    )
                            )

                        let mutable compensationInvocations = 0

                        let compensation () =
                            compensationInvocations <-
                                compensationInvocations
                                + 1

                        let code =
                            tryFinallyMethod.Invoke(
                                builder,
                                [|
                                    box computation
                                    box compensation
                                |]
                            )

                        let stateMachineType =
                            typedefof<
                                Microsoft.FSharp.Core.CompilerServices.ResumableStateMachine<_>
                             >
                                .MakeGenericType(typeof<int>)

                        let invokeArguments = [| Activator.CreateInstance(stateMachineType) |]

                        let completed =
                            code.GetType().GetMethod("Invoke").Invoke(code, invokeArguments)
                            :?> bool

                        completed, probe.ComputationInvocations, compensationInvocations

                    let oracleBehavior = invokeTryFinally oracleOutputPath
                    let fsharp2Behavior = invokeTryFinally outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted TryFinally function should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (true, 1, 1)
                        "TryFinally should execute both the computation and compensation once"
                finally
                    Directory.Delete(root, true)

            testCase "executes an attributed static inline object member"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Identity(value: 'T) : 'T = value\n"

                withObjectMemberDifferential
                    "fsharp2-attributed-static-object-member"
                    sourceText
                    "Identity"
                <| fun oracleOutputPath outputPath ->
                    let invokeIdentity assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType
                            .GetMethod("Identity")
                            .MakeGenericMethod(typeof<int>)
                            .Invoke(null, [| box 42 |])
                        :?> int

                    let oracleBehavior = invokeIdentity oracleOutputPath
                    let fsharp2Behavior = invokeIdentity outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted static member should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "Identity should return its argument"

            testCase "executes a static object member with a parenthesized function type"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Keep(continuation: ('T -> 'U)) : ('T -> 'U) = continuation\n"

                withObjectMemberDifferential "fsharp2-parenthesized-function-type" sourceText "Keep"
                <| fun oracleOutputPath outputPath ->
                    let invokeKeep assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let keepMethod =
                            builderType
                                .GetMethod("Keep")
                                .MakeGenericMethod(typeof<int>, typeof<string>)

                        let continuation (value: int) = string value

                        let returned =
                            keepMethod.Invoke(null, [| box continuation |]) :?> (int -> string)

                        returned 42

                    let oracleBehavior = invokeKeep oracleOutputPath
                    let fsharp2Behavior = invokeKeep outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted static member should preserve the function value"

                    Expect.equal fsharp2Behavior "42" "Keep should return its function argument"

            testCase "executes a static object member with a local let expression"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Alias(value: 'T) : 'T =\n            let result = value\n            result\n"

                withObjectMemberDifferential "fsharp2-local-let-expression" sourceText "Alias"
                <| fun oracleOutputPath outputPath ->
                    let invokeAlias assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType
                            .GetMethod("Alias")
                            .MakeGenericMethod(typeof<int>)
                            .Invoke(null, [| box 42 |])
                        :?> int

                    let oracleBehavior = invokeAlias oracleOutputPath
                    let fsharp2Behavior = invokeAlias outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted static member should preserve the locally bound value"

                    Expect.equal fsharp2Behavior 42 "Alias should return its local binding"

            testCase "executes assignment to a mutable local binding"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Replace(initial: int, replacement: int) : int =\n            let mutable result = initial\n            result <- replacement\n            result\n"

                withObjectMemberDifferential "fsharp2-mutable-local" sourceText "Replace"
                <| fun oracleOutputPath outputPath ->
                    let invokeReplace assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType
                            .GetMethod("Replace")
                            .Invoke(
                                null,
                                [|
                                    box 1
                                    box 42
                                |]
                            )
                        :?> int

                    let oracleBehavior = invokeReplace oracleOutputPath
                    let fsharp2Behavior = invokeReplace outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted mutable local should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "Replace should return the assigned value"

            testCase "executes a unit-valued if expression without else"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline MaybeReplace(condition: bool, initial: int, replacement: int) : int =\n            let mutable result = initial\n            if condition then\n                result <- replacement\n            result\n"

                withObjectMemberDifferential "fsharp2-if-without-else" sourceText "MaybeReplace"
                <| fun oracleOutputPath outputPath ->
                    let invokeMaybeReplace assemblyPath condition =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType
                            .GetMethod("MaybeReplace")
                            .Invoke(
                                null,
                                [|
                                    box condition
                                    box 1
                                    box 42
                                |]
                            )
                        :?> int

                    let oracleBehavior =
                        invokeMaybeReplace oracleOutputPath false,
                        invokeMaybeReplace oracleOutputPath true

                    let fsharp2Behavior =
                        invokeMaybeReplace outputPath false, invokeMaybeReplace outputPath true

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted unit-valued conditional should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (1, 42)
                        "MaybeReplace should mutate only on the true branch"

            testCase "executes a whitespace-applied static object member call"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type Helper() =\n        static member inline Identity(value: 'T) : 'T = value\n\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Alias(value: 'T) : 'T =\n            let result = Helper.Identity value\n            result\n"

                withObjectMemberDifferential "fsharp2-static-whitespace-call" sourceText "Alias"
                <| fun oracleOutputPath outputPath ->
                    let invokeAlias assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType
                            .GetMethod("Alias")
                            .MakeGenericMethod(typeof<int>)
                            .Invoke(null, [| box 42 |])
                        :?> int

                    let oracleBehavior = invokeAlias oracleOutputPath
                    let fsharp2Behavior = invokeAlias outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted static call should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "Alias should return the helper result"

            testCase "executes a grouped FSharp function application"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Apply(continuation: ('T -> 'U), value: 'T) : 'U =\n            (continuation value)\n"

                withObjectMemberDifferential
                    "fsharp2-grouped-function-application"
                    sourceText
                    "Apply"
                <| fun oracleOutputPath outputPath ->
                    let invokeApply assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let continuation: int -> string = fun value -> string value

                        builderType
                            .GetMethod("Apply")
                            .MakeGenericMethod(typeof<int>, typeof<string>)
                            .Invoke(
                                null,
                                [|
                                    box continuation
                                    box 42
                                |]
                            )
                        :?> string

                    let oracleBehavior = invokeApply oracleOutputPath
                    let fsharp2Behavior = invokeApply outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted function application should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior "42" "Apply should invoke its function argument"

            testCase "executes an ungrouped FSharp function application as a call argument"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type Helper() =\n        static member inline Identity(value: 'T) : 'T = value\n\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Apply(mapper: ('T -> 'U), value: 'T) : 'U =\n            Helper.Identity(mapper value)\n"

                withObjectMemberDifferential
                    "fsharp2-ungrouped-function-call-argument"
                    sourceText
                    "Apply"
                <| fun oracleOutputPath outputPath ->
                    let invokeApply assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let mapper: int -> string = fun value -> string value

                        builderType
                            .GetMethod("Apply")
                            .MakeGenericMethod(typeof<int>, typeof<string>)
                            .Invoke(
                                null,
                                [|
                                    box mapper
                                    box 42
                                |]
                            )
                        :?> string

                    let oracleBehavior = invokeApply oracleOutputPath
                    let fsharp2Behavior = invokeApply outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the call-argument application should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior "42" "Apply should pass the mapped value"

            testCase "executes a member call on a grouped expression"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Run(factory: ('T -> System.Func<'U>), value: 'T) : 'U =\n            (factory value).Invoke()\n"

                withObjectMemberDifferential "fsharp2-expression-member-call" sourceText "Run"
                <| fun oracleOutputPath outputPath ->
                    let invokeRun assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let factory: int -> Func<string> =
                            fun value -> Func<string>(fun () -> string value)

                        builderType
                            .GetMethod("Run")
                            .MakeGenericMethod(typeof<int>, typeof<string>)
                            .Invoke(
                                null,
                                [|
                                    box factory
                                    box 42
                                |]
                            )
                        :?> string

                    let oracleBehavior = invokeRun oracleOutputPath
                    let fsharp2Behavior = invokeRun outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted member call should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior "42" "Run should invoke the returned delegate"

            testCase "executes a postfix member call on a call result"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type Helper() =\n        static member inline Echo(value: System.Func<int>) : System.Func<int> = value\n\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Run(value: System.Func<int>) : int =\n            Helper.Echo(value).Invoke()\n"

                withObjectMemberDifferential "fsharp2-postfix-member-call" sourceText "Run"
                <| fun oracleOutputPath outputPath ->
                    let invokeRun assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType
                            .GetMethod("Run")
                            .Invoke(null, [| box (Func<int>(fun () -> 42)) |])
                        :?> int

                    let oracleBehavior = invokeRun oracleOutputPath
                    let fsharp2Behavior = invokeRun outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted postfix call should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "Run should invoke the returned delegate"

            testCase "executes an if-then-else expression"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Choose(condition: bool) : int =\n            if condition then 42 else 0\n"

                withObjectMemberDifferential "fsharp2-if-then-else" sourceText "Choose"
                <| fun oracleOutputPath outputPath ->
                    let invokeChoose assemblyPath condition =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType.GetMethod("Choose").Invoke(null, [| box condition |]) :?> int

                    let oracleBehavior =
                        invokeChoose oracleOutputPath false, invokeChoose oracleOutputPath true

                    let fsharp2Behavior =
                        invokeChoose outputPath false, invokeChoose outputPath true

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted conditional should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior (0, 42) "Choose should select one branch"

            testCase "executes Boolean negation"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Negate(value: bool) : bool =\n            not value\n"

                withObjectMemberDifferential "fsharp2-boolean-not" sourceText "Negate"
                <| fun oracleOutputPath outputPath ->
                    let invokeNegate assemblyPath value =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType.GetMethod("Negate").Invoke(null, [| box value |]) :?> bool

                    let oracleBehavior =
                        invokeNegate oracleOutputPath false, invokeNegate oracleOutputPath true

                    let fsharp2Behavior =
                        invokeNegate outputPath false, invokeNegate outputPath true

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted Boolean negation should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior (true, false) "Negate should invert its input"

            testCase "executes a multi-expression conditional branch"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline ChooseMany(condition: bool) : int =\n            if condition then\n                42\n            else\n                1\n                0\n"

                withObjectMemberDifferential
                    "fsharp2-multi-expression-conditional"
                    sourceText
                    "ChooseMany"
                <| fun oracleOutputPath outputPath ->
                    let invokeChooseMany assemblyPath condition =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType.GetMethod("ChooseMany").Invoke(null, [| box condition |])
                        :?> int

                    let oracleBehavior =
                        invokeChooseMany oracleOutputPath false,
                        invokeChooseMany oracleOutputPath true

                    let fsharp2Behavior =
                        invokeChooseMany outputPath false, invokeChooseMany outputPath true

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted branch sequence should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (0, 42)
                        "ChooseMany should return the final expression in its selected branch"

            testCase "executes a constrained generic upcast"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen System.Runtime.CompilerServices\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Upcast(awaiter: 'TAwaiter) : ICriticalNotifyCompletion =\n            (awaiter :> ICriticalNotifyCompletion)\n"

                withObjectMemberDifferential "fsharp2-generic-upcast" sourceText "Upcast"
                <| fun oracleOutputPath outputPath ->
                    let invokeUpcast assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let awaiter = System.Threading.Tasks.Task.CompletedTask.GetAwaiter()

                        let result =
                            builderType
                                .GetMethod("Upcast")
                                .MakeGenericMethod(
                                    typeof<System.Runtime.CompilerServices.TaskAwaiter>
                                )
                                .Invoke(null, [| box awaiter |])

                        result.GetType().FullName,
                        result :? System.Runtime.CompilerServices.ICriticalNotifyCompletion

                    let oracleBehavior = invokeUpcast oracleOutputPath
                    let fsharp2Behavior = invokeUpcast outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted upcast should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (typeof<System.Runtime.CompilerServices.TaskAwaiter>.FullName, true)
                        "Upcast should box the constrained value type as the target interface"

            testCase "executes an unparenthesized upcast in a local binding"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen System.Runtime.CompilerServices\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline UpcastLocal(awaiter: 'TAwaiter) : ICriticalNotifyCompletion =\n            let mutable boxed = awaiter :> ICriticalNotifyCompletion\n            boxed\n"

                withObjectMemberDifferential
                    "fsharp2-unparenthesized-upcast"
                    sourceText
                    "UpcastLocal"
                <| fun oracleOutputPath outputPath ->
                    let invokeUpcastLocal assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let awaiter = System.Threading.Tasks.Task.CompletedTask.GetAwaiter()

                        let result =
                            builderType
                                .GetMethod("UpcastLocal")
                                .MakeGenericMethod(
                                    typeof<System.Runtime.CompilerServices.TaskAwaiter>
                                )
                                .Invoke(null, [| box awaiter |])

                        result.GetType().FullName,
                        result :? System.Runtime.CompilerServices.ICriticalNotifyCompletion

                    let oracleBehavior = invokeUpcastLocal oracleOutputPath
                    let fsharp2Behavior = invokeUpcastLocal outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the local upcast should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        (typeof<System.Runtime.CompilerServices.TaskAwaiter>.FullName, true)
                        "UpcastLocal should retain the boxed constrained value"

            testCase "executes an internal inline instance member"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        member inline internal _.Hidden(value: int) : int = value\n"

                withObjectMemberDifferential "fsharp2-internal-instance-member" sourceText "Hidden"
                <| fun oracleOutputPath outputPath ->
                    let invokeHidden assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let instance = Activator.CreateInstance(builderType)

                        builderType
                            .GetMethod(
                                "Hidden",
                                BindingFlags.Instance
                                ||| BindingFlags.NonPublic
                            )
                            .Invoke(instance, [| box 42 |])
                        :?> int

                    let oracleBehavior = invokeHidden oracleOutputPath
                    let fsharp2Behavior = invokeHidden outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the internal member should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "Hidden should return its argument"

            testCase "returns a bound unit instance member as an FSharp function"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 42\n\n        member inline this.GetZero() : (unit -> int) = this.Zero\n"

                withObjectMemberDifferential
                    "fsharp2-bound-unit-instance-member"
                    sourceText
                    "GetZero"
                <| fun oracleOutputPath outputPath ->
                    let invokeBoundZero assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let instance = Activator.CreateInstance(builderType)

                        let zero =
                            try
                                builderType.GetMethod("GetZero").Invoke(instance, Array.empty)
                                :?> (unit -> int)
                            with :? TargetInvocationException as error when
                                not (isNull error.InnerException) ->
                                raise error.InnerException

                        zero ()

                    let oracleBehavior = invokeBoundZero oracleOutputPath
                    let fsharp2Behavior = invokeBoundZero outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the bound instance member should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "the bound Zero member should remain callable"

            testCase "returns a captured unit lambda as an FSharp function"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Make(value: int) : (unit -> int) =\n            fun () -> value\n"

                withObjectMemberDifferential "fsharp2-captured-unit-lambda" sourceText "Make"
                <| fun oracleOutputPath outputPath ->
                    let invokeMake assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let make =
                            builderType.GetMethod("Make").Invoke(null, [| box 42 |])
                            :?> (unit -> int)

                        make ()

                    let oracleBehavior = invokeMake oracleOutputPath
                    let fsharp2Behavior = invokeMake outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the captured unit lambda should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "the lambda should return its captured value"

            testCase "executes a static call with an address-of struct field"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    [<Struct; NoComparison; NoEquality>]\n    type Data<'T> =\n        [<DefaultValue(false)>]\n        val mutable Value: 'T\n\n    type Helper() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Ignore(value: byref<'T>) : int = 42\n\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        [<NoEagerConstraintApplication>]\n        static member inline Touch(data: byref<Data<'T>>) : int =\n            Helper.Ignore(&data.Value)\n"

                withObjectMemberDifferential "fsharp2-address-of-struct-field" sourceText "Touch"
                <| fun oracleOutputPath outputPath ->
                    let invokeTouch assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let dataType =
                            emittedAssembly
                                .GetType("IcedTasks.TaskBase.TaskBase+Data`1", throwOnError = true)
                                .MakeGenericType(typeof<int>)

                        let arguments = [| Activator.CreateInstance(dataType) |]

                        try
                            builderType
                                .GetMethod("Touch")
                                .MakeGenericMethod(typeof<int>)
                                .Invoke(null, arguments)
                            :?> int
                        with :? TargetInvocationException as error when
                            not (isNull error.InnerException) ->
                            raise error.InnerException

                    let oracleBehavior = invokeTouch oracleOutputPath
                    let fsharp2Behavior = invokeTouch outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the address-of field call should behave like the Compatibility Oracle"

                    Expect.equal fsharp2Behavior 42 "Touch should pass the field address to Helper"

            testCase "emits an attributed nested module type extension"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n    [<AutoOpen>]\n    module LowPriority =\n        type TaskBuilderBase with\n            member inline _.Source(value: 'T) : 'T = value\n"

                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-nested-extension-module",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let inspectAndInvoke assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let extensionModuleType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+LowPriority",
                                throwOnError = true
                            )

                        let extensionMethod =
                            extensionModuleType.GetMethod(
                                "TaskBuilderBase.Source",
                                BindingFlags.Public
                                ||| BindingFlags.Static
                            )

                        Expect.isNotNull
                            extensionMethod
                            "the nested module should expose the type-extension method"

                        let closedMethod = extensionMethod.MakeGenericMethod(typeof<int>)
                        let builder = Activator.CreateInstance(builderType)

                        extensionModuleType.Attributes,
                        (extensionModuleType.CustomAttributes
                         |> Seq.map _.AttributeType.FullName
                         |> Seq.sort
                         |> Seq.toArray),
                        extensionMethod.Attributes,
                        (extensionMethod.GetParameters()
                         |> Array.map _.ParameterType.ToString()),
                        closedMethod.Invoke(
                            null,
                            [|
                                builder
                                box 42
                            |]
                        )
                        :?> int

                    let oracleShape = inspectAndInvoke oracleOutputPath
                    let fsharp2Shape = inspectAndInvoke outputPath

                    Expect.equal
                        fsharp2Shape
                        oracleShape
                        "the nested extension module should match the Compatibility Oracle"

                    let _, _, _, _, result = fsharp2Shape
                    Expect.equal result 42 "the extension should return its argument"
                finally
                    Directory.Delete(root, true)

            testCase
                "preserves explicit generic parameters and constraints on a type extension member"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\nopen System.Runtime.CompilerServices\nopen Microsoft.FSharp.Core.CompilerServices\n\n[<AutoOpen>]\nmodule TaskBase =\n    type Awaiter<'Awaiter, 'TResult\n        when 'Awaiter :> ICriticalNotifyCompletion\n        and 'Awaiter: (member GetResult: unit -> 'TResult)> = 'Awaiter\n\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n    [<AutoOpen>]\n    module LowPriority =\n        type TaskBuilderBase with\n            [<NoEagerConstraintApplication>]\n            member inline _.Source<'TResult1, 'TResult2, 'Awaiter, 'TOverall\n                when Awaiter<'Awaiter, 'TResult1>>\n                (awaiter: 'Awaiter)\n                : 'Awaiter =\n                awaiter\n"

                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-constrained-extension-member",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "TaskBuilderBase-oracle.dll")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, _ =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "TaskBuilderBase-fsharp2"
                            []

                    let inspectAndInvoke assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        let extensionModuleType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+LowPriority",
                                throwOnError = true
                            )

                        let extensionMethod =
                            extensionModuleType.GetMethod(
                                "TaskBuilderBase.Source",
                                BindingFlags.Public
                                ||| BindingFlags.Static
                            )

                        Expect.isNotNull
                            extensionMethod
                            "the nested module should expose the constrained type-extension method"

                        let genericParameters =
                            extensionMethod.GetGenericArguments()
                            |> Array.map (fun parameter ->
                                parameter.GenericParameterPosition,
                                parameter.Name,
                                parameter.GenericParameterAttributes,
                                (parameter.GetGenericParameterConstraints()
                                 |> Array.map _.FullName)
                            )

                        let closedMethod =
                            extensionMethod.MakeGenericMethod(
                                typeof<int>,
                                typeof<string>,
                                typeof<System.Runtime.CompilerServices.TaskAwaiter<int>>,
                                typeof<bool>
                            )

                        let builder = Activator.CreateInstance(builderType)
                        let awaiter = System.Threading.Tasks.Task.FromResult(42).GetAwaiter()

                        let result =
                            closedMethod.Invoke(
                                null,
                                [|
                                    builder
                                    awaiter
                                |]
                            )
                            :?> System.Runtime.CompilerServices.TaskAwaiter<int>

                        genericParameters,
                        (extensionMethod.GetParameters()
                         |> Array.map (fun parameter ->
                             parameter.Name, parameter.ParameterType.ToString()
                         )),
                        extensionMethod.Attributes,
                        result.GetResult()

                    let oracleShape = inspectAndInvoke oracleOutputPath
                    let fsharp2Shape = inspectAndInvoke outputPath

                    Expect.equal
                        fsharp2Shape
                        oracleShape
                        "the constrained extension member should match the Compatibility Oracle"

                    let _, _, _, result = fsharp2Shape
                    Expect.equal result 42 "Source should return its awaiter argument"
                finally
                    Directory.Delete(root, true)

            testCase "emits a static type augmentation in the current module"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.ValueTasks\n\nopen System\nopen System.Threading\nopen System.Threading.Tasks\n\n[<AutoOpen>]\nmodule ValueTaskExtensions =\n    type String with\n        static member Echo(value: int) : int = value\n\n    type System.Object with\n        static member Kind() : int = 7\n\n    type Microsoft.FSharp.Control.Async with\n        static member inline AsValueTask(computation: Async<'T>) : ValueTask<'T> =\n            Async.StartImmediateAsTask(computation)\n            |> ValueTask<'T>\n\n    type ValueTask with\n        static member FromCanceled(cancellationToken) =\n            new ValueTask(Task.FromCanceled(cancellationToken))\n\n        static member FromCanceled<'T>(cancellationToken) =\n            new ValueTask<'T>(Task.FromCanceled<'T>(cancellationToken))\n"

                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-current-module-type-augmentation",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "ValueTask.fs")
                    File.WriteAllText(sourcePath, sourceText)

                    let oracleOutputPath = Path.Combine(root, "ValueTask-oracle.dll")
                    let oracleResponsePath = Path.Combine(root, "oracle.rsp")

                    let oracleResult =
                        invokeCompatibilityOracle root oracleResponsePath [
                            "--target:library"
                            "--targetprofile:netcore"
                            "--deterministic+"
                            "--debug:portable"
                            "--optimize-"
                            $"--out:{oracleOutputPath}"
                            sourcePath
                        ]

                    Expect.equal
                        oracleResult.ExitCode
                        0
                        (oracleResult.StandardOutput
                         + oracleResult.StandardError)

                    let responsePath = Path.Combine(root, "fsharp2.rsp")

                    let outputPath, originalExportFingerprint =
                        compileForExportFingerprint root responsePath sourcePath "ValueTask-fsharp2" [
                            typeof<System.Threading.Tasks.Task>.Assembly.Location
                        ]

                    let inspectAndInvoke assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let extensionModuleType =
                            emittedAssembly.GetType(
                                "IcedTasks.ValueTasks.ValueTaskExtensions",
                                throwOnError = true
                            )

                        let publicStaticMethods =
                            extensionModuleType.GetMethods(
                                BindingFlags.Public
                                ||| BindingFlags.Static
                            )

                        let augmentationMethod memberName =
                            publicStaticMethods
                            |> Array.tryFind (fun methodInfo ->
                                methodInfo.Name.Contains(
                                    "."
                                    + memberName,
                                    StringComparison.Ordinal
                                )
                            )
                            |> Option.defaultWith (fun () ->
                                publicStaticMethods
                                |> Array.map _.Name
                                |> String.concat ", "
                                |> failtestf
                                    "the source module should expose the static type-augmentation member; found: %s"
                            )

                        let methodShape (methodInfo: MethodInfo) arguments =
                            methodInfo.Name,
                            methodInfo.Attributes,
                            (methodInfo.GetParameters()
                             |> Array.map (fun parameter ->
                                 parameter.Name, parameter.ParameterType.FullName
                             )),
                            methodInfo.ReturnType.FullName,
                            (methodInfo.Invoke(null, arguments) :?> int)

                        let fromCanceledMethods =
                            publicStaticMethods
                            |> Array.filter (fun methodInfo ->
                                methodInfo.Name.Contains(".FromCanceled", StringComparison.Ordinal)
                            )

                        let fromCanceled =
                            fromCanceledMethods
                            |> Array.find (fun methodInfo ->
                                not methodInfo.IsGenericMethodDefinition
                            )

                        let genericFromCanceled =
                            fromCanceledMethods
                            |> Array.find _.IsGenericMethodDefinition
                            |> fun methodInfo -> methodInfo.MakeGenericMethod(typeof<int>)

                        let canceledTask =
                            fromCanceled.Invoke(
                                null,
                                [| box (System.Threading.CancellationToken(canceled = true)) |]
                            )
                            :?> System.Threading.Tasks.ValueTask

                        let canceledGenericTask =
                            try
                                genericFromCanceled.Invoke(
                                    null,
                                    [| box (System.Threading.CancellationToken(canceled = true)) |]
                                )
                                :?> System.Threading.Tasks.ValueTask<int>
                            with error ->
                                failtestf "%O" error

                        let asValueTask =
                            augmentationMethod "AsValueTask"
                            |> fun methodInfo -> methodInfo.MakeGenericMethod(typeof<int>)

                        let wrappedValueTask =
                            asValueTask.Invoke(null, [| box (async.Return 42) |])
                            :?> System.Threading.Tasks.ValueTask<int>

                        methodShape (augmentationMethod "Echo") [| box 42 |],
                        methodShape (augmentationMethod "Kind") Array.empty,
                        (fromCanceled.Name,
                         fromCanceled.Attributes,
                         (fromCanceled.GetParameters()
                          |> Array.map (fun parameter ->
                              parameter.Name, parameter.ParameterType.FullName
                          )),
                         fromCanceled.ReturnType.FullName,
                         canceledTask.IsCanceled),
                        (genericFromCanceled.Name,
                         genericFromCanceled.Attributes,
                         genericFromCanceled.GetGenericArguments().Length,
                         (genericFromCanceled.GetParameters()
                          |> Array.map (fun parameter ->
                              parameter.Name, parameter.ParameterType.FullName
                          )),
                         genericFromCanceled.ReturnType.FullName,
                         canceledGenericTask.IsCanceled),
                        (asValueTask.Name,
                         asValueTask.Attributes,
                         asValueTask.GetGenericArguments().Length,
                         (asValueTask.GetParameters()
                          |> Array.map (fun parameter ->
                              parameter.Name, parameter.ParameterType.FullName
                          )),
                         asValueTask.ReturnType.FullName,
                         wrappedValueTask.Result)

                    let oracleShape = inspectAndInvoke oracleOutputPath
                    let fsharp2Shape = inspectAndInvoke outputPath

                    Expect.equal
                        fsharp2Shape
                        oracleShape
                        "the static type augmentation should match the Compatibility Oracle"

                    let ((_, _, _, _, result),
                         (_, _, _, _, kind),
                         (_, _, _, _, isCanceled),
                         (_, _, _, _, _, isGenericCanceled),
                         (_, _, _, _, _, wrappedResult)) =
                        fsharp2Shape

                    Expect.equal result 42 "Echo should return its argument"
                    Expect.equal kind 7 "Kind should execute from the qualified augmentation"
                    Expect.isTrue isCanceled "FromCanceled should return a canceled ValueTask"

                    Expect.isTrue
                        isGenericCanceled
                        "generic FromCanceled should return a canceled ValueTask"

                    Expect.equal
                        wrappedResult
                        42
                        "AsValueTask should wrap the completed async computation"

                    File.WriteAllText(
                        sourcePath,
                        sourceText.Replace("= value", "= 43", StringComparison.Ordinal)
                    )

                    let changedOutputPath, changedExportFingerprint =
                        compileForExportFingerprint
                            root
                            responsePath
                            sourcePath
                            "ValueTask-changed-fsharp2"
                            [ typeof<System.Threading.Tasks.Task>.Assembly.Location ]

                    let (_, _, _, _, changedResult), _, _, _, _ = inspectAndInvoke changedOutputPath
                    Expect.equal changedResult 43 "the changed non-inline body should be emitted"

                    Expect.equal
                        changedExportFingerprint
                        originalExportFingerprint
                        "a non-inline body edit should preserve the consumer-visible export fingerprint"
                finally
                    Directory.Delete(root, true)

            testCase "emits and executes an object expression"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        static member inline Make() : System.Object =\n            { new System.Object() with\n                override _.ToString() = \"object-expression\" }\n"

                withObjectMemberDifferential "fsharp2-object-expression" sourceText "Make"
                <| fun oracleOutputPath outputPath ->
                    let invokeMake assemblyPath =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType.GetMethod("Make").Invoke(null, null).ToString()

                    let oracleBehavior = invokeMake oracleOutputPath
                    let fsharp2Behavior = invokeMake outputPath

                    Expect.equal
                        fsharp2Behavior
                        oracleBehavior
                        "the emitted object expression should behave like the Compatibility Oracle"

                    Expect.equal
                        fsharp2Behavior
                        "object-expression"
                        "the override should provide the object-expression result"

            testCase "emits and executes a runtime type-test match"
            <| fun _ ->
                let sourceText =
                    "namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = 0\n\n        static member inline Describe(value: System.Object) : System.String =\n            match value with\n            | :? System.String as text -> text\n            | _ -> \"other\"\n"

                withObjectMemberDifferential "fsharp2-type-test-match" sourceText "Describe"
                <| fun oracleOutputPath outputPath ->
                    let invokeDescribe assemblyPath value =
                        let emittedAssembly = Assembly.Load(File.ReadAllBytes(assemblyPath))

                        let builderType =
                            emittedAssembly.GetType(
                                "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                                throwOnError = true
                            )

                        builderType.GetMethod("Describe").Invoke(null, [| value |]) :?> string

                    for input, expected in
                        [
                            box "matched", "matched"
                            box 42, "other"
                        ] do
                        let oracleBehavior = invokeDescribe oracleOutputPath input
                        let fsharp2Behavior = invokeDescribe outputPath input

                        Expect.equal
                            fsharp2Behavior
                            oracleBehavior
                            "the runtime type-test match should behave like the Compatibility Oracle"

                        Expect.equal
                            fsharp2Behavior
                            expected
                            "the runtime type-test match should select the expected arm"

            testCase "retained service invalidates an IcedTasks inline object-member edit"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-icedtasks-object-type-edit",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                let pipeName =
                    "fsharp2-"
                    + Guid.NewGuid().ToString("N")

                use service = startCompilerService root pipeName

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
                    let outputPath = Path.Combine(root, "TaskBuilderBase.dll")
                    let pdbPath = Path.Combine(root, "TaskBuilderBase.pdb")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

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

                        File.WriteAllText(
                            sourcePath,
                            $"namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    type TaskBuilderBase() =\n        member inline _.Zero() = {value}\n"
                        )

                        File.WriteAllLines(
                            responsePath,
                            [|
                                $"--fsharp2-server:{pipeName}"
                                $"--fsharp2-trace:{tracePath}"
                                "--target:library"
                                "--deterministic+"
                                "--debug:portable"
                                $"--reference:{typeof<Microsoft.FSharp.Core.AutoOpenAttribute>.Assembly.Location}"
                                $"--reference:{systemRuntimePath}"
                                $"--out:{outputPath}"
                                $"--pdb:{pdbPath}"
                                sourcePath
                            |]
                        )

                        let result = invokeFsc2 root responsePath

                        Expect.equal
                            result.ExitCode
                            0
                            (result.StandardOutput
                             + result.StandardError)

                        readTrace tracePath

                    let baseline = compile 0 "baseline"
                    let edited = compile 1 "edited"
                    let replay = compile 1 "replay"

                    for traceName, trace in
                        [
                            "baseline", baseline
                            "edited", edited
                            "replay", replay
                        ] do
                        Expect.equal
                            trace.["servicePid"]
                            baseline.["servicePid"]
                            $"the {traceName} request should use the retained compiler service"

                    Expect.notEqual
                        edited.["exportFingerprint"]
                        baseline.["exportFingerprint"]
                        "an inline object-member edit should change consumer-visible F# export identity"

                    Expect.notEqual
                        edited.["fragmentHash"]
                        baseline.["fragmentHash"]
                        "the edited object-member implementation should have a new symbolic fragment hash"

                    Expect.equal edited.["parse"] "miss" "changed object source should be reparsed"
                    Expect.equal edited.["check"] "miss" "changed object source should be rechecked"

                    Expect.equal
                        edited.["lower"]
                        "miss"
                        "changed object source should be re-emitted"

                    Expect.equal
                        edited.["previousContentFingerprint"]
                        baseline.["contentFingerprint"]
                        "the edit should identify the retained semantic state it replaced"

                    Expect.equal
                        edited.["dependencyCount"]
                        "1"
                        "the object constructor should retain its symbolic System.Object constructor dependency"

                    Expect.equal edited.["emitted"] "true" "the edit should be linked and published"

                    Expect.equal
                        replay.["parse"]
                        "hit"
                        "identical object source should reuse parsing"

                    Expect.equal
                        replay.["check"]
                        "hit"
                        "identical object source should reuse checking"

                    Expect.equal
                        replay.["lower"]
                        "hit"
                        "identical object source should reuse lowering"

                    Expect.equal
                        replay.["fragmentHash"]
                        edited.["fragmentHash"]
                        "replay should retain the edited symbolic fragment"

                    let warmImplementation = File.ReadAllBytes(outputPath)
                    let warmPdb = File.ReadAllBytes(pdbPath)
                    let cleanResponsePath = Path.Combine(root, "clean.rsp")

                    File.WriteAllLines(
                        cleanResponsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--reference:{typeof<Microsoft.FSharp.Core.AutoOpenAttribute>.Assembly.Location}"
                            $"--reference:{systemRuntimePath}"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            sourcePath
                        |]
                    )

                    let cleanResult = invokeFsc2 root cleanResponsePath

                    Expect.equal
                        cleanResult.ExitCode
                        0
                        (cleanResult.StandardOutput
                         + cleanResult.StandardError)

                    Expect.sequenceEqual
                        (File.ReadAllBytes(outputPath))
                        warmImplementation
                        "the warm implementation should match a clean standalone compile of the edited input"

                    Expect.sequenceEqual
                        (File.ReadAllBytes(pdbPath))
                        warmPdb
                        "the warm PDB should match a clean standalone compile of the edited input"

                    let emittedAssembly = Assembly.Load(File.ReadAllBytes(outputPath))

                    let builderType =
                        emittedAssembly.GetType(
                            "IcedTasks.TaskBase.TaskBase+TaskBuilderBase",
                            throwOnError = true
                        )

                    let builder = Activator.CreateInstance(builderType)
                    let zero = builderType.GetMethod("Zero")

                    Expect.equal
                        (zero.Invoke(builder, Array.empty<obj>))
                        (box 1)
                        "the published warm artifact should execute the edited object-member body"
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

                    Directory.Delete(root, true)

            testCase "attribute edits change IcedTasks struct export fingerprints"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-icedtasks-struct-attribute-fingerprint",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "TaskBuilderBase.fs")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let systemRuntimePath = Assembly.Load("System.Runtime").Location

                    let compile suffix typeAttributes =
                        let outputPath = Path.Combine(root, $"TaskBuilderBase-{suffix}.dll")
                        let pdbPath = Path.Combine(root, $"TaskBuilderBase-{suffix}.pdb")
                        let tracePath = Path.Combine(root, $"TaskBuilderBase-{suffix}.trace")

                        File.WriteAllText(
                            sourcePath,
                            $"namespace IcedTasks.TaskBase\n\n[<AutoOpen>]\nmodule TaskBase =\n    [<{typeAttributes}>]\n    type TaskBaseStateMachineData<'T, 'Builder> =\n        [<DefaultValue(false)>]\n        val mutable Result: 'T\n"
                        )

                        File.WriteAllLines(
                            responsePath,
                            [|
                                "--target:library"
                                "--deterministic+"
                                "--debug:portable"
                                $"--reference:{typeof<Microsoft.FSharp.Core.StructAttribute>.Assembly.Location}"
                                $"--reference:{systemRuntimePath}"
                                $"--out:{outputPath}"
                                $"--pdb:{pdbPath}"
                                $"--fsharp2-trace:{tracePath}"
                                sourcePath
                            |]
                        )

                        let result = invokeFsc2 root responsePath

                        Expect.equal
                            result.ExitCode
                            0
                            (result.StandardOutput
                             + result.StandardError)

                        (readTrace tracePath).["exportFingerprint"]

                    let baseline = compile "baseline" "Struct; NoComparison; NoEquality"

                    let reordered = compile "reordered" "Struct; NoEquality; NoComparison"

                    Expect.notEqual
                        reordered
                        baseline
                        "consumer-visible attribute order must invalidate the exported semantic fingerprint"
                finally
                    Directory.Delete(root, true)

            testCase "namespace moves change same-named module export fingerprints"
            <| fun _ ->
                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-stable-identities",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                try
                    let sourcePath = Path.Combine(root, "AssemblyInfo.fs")
                    let outputPath = Path.Combine(root, "StableIdentity.dll")
                    let pdbPath = Path.Combine(root, "StableIdentity.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")
                    let tracePath = Path.Combine(root, "compile.trace")

                    File.WriteAllLines(
                        responsePath,
                        [|
                            "--target:library"
                            "--deterministic+"
                            "--debug:portable"
                            $"--out:{outputPath}"
                            $"--pdb:{pdbPath}"
                            $"--fsharp2-trace:{tracePath}"
                            sourcePath
                        |]
                    )

                    let compile namespaceName =
                        File.WriteAllText(
                            sourcePath,
                            $"namespace {namespaceName}\n[<assembly: System.Reflection.AssemblyTitleAttribute(\"same\")>]\ndo ()\nmodule internal Shared =\n    let [<Literal>] Value = \"same\"\n"
                        )

                        let result = invokeFsc2 root responsePath
                        Expect.equal result.ExitCode 0 result.StandardError
                        (readTrace tracePath).["exportFingerprint"]

                    let firstFingerprint = compile "First"
                    let secondFingerprint = compile "Second"

                    Expect.notEqual
                        secondFingerprint
                        firstFingerprint
                        "namespace identity is part of a declaration's exported meaning"
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

            testCase "emits generated and authored assembly metadata from the target reference set"
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
                    let targetFrameworkSourcePath =
                        Path.Combine(root, ".NETStandard,Version=v2.1.AssemblyAttributes.fs")

                    let assemblyInfoSourcePath = Path.Combine(root, "AssemblyInfo.fs")
                    let tracerSourcePath = Path.Combine(root, "Tracer.fs")
                    let outputPath = Path.Combine(root, "Tracer.dll")
                    let pdbPath = Path.Combine(root, "Tracer.pdb")
                    let responsePath = Path.Combine(root, "compile.rsp")

                    let dotnetRoot =
                        let rec findPacksRoot (directory: DirectoryInfo) =
                            if isNull directory then
                                None
                            elif Directory.Exists(Path.Combine(directory.FullName, "packs")) then
                                Some directory.FullName
                            else
                                findPacksRoot directory.Parent

                        RuntimeEnvironment.GetRuntimeDirectory()
                        |> DirectoryInfo
                        |> findPacksRoot
                        |> Option.defaultWith (fun () ->
                            failwith
                                "the active .NET runtime is not beneath an SDK installation containing packs"
                        )

                    let netstandardReferencePath =
                        Path.Combine(
                            dotnetRoot,
                            "packs",
                            "NETStandard.Library.Ref",
                            "2.1.0",
                            "ref",
                            "netstandard2.1",
                            "netstandard.dll"
                        )

                    Expect.isTrue
                        (File.Exists netstandardReferencePath)
                        "the installed .NET SDK should contain the netstandard2.1 reference assembly"

                    File.WriteAllText(
                        targetFrameworkSourcePath,
                        "namespace Microsoft.BuildSettings\n[<System.Runtime.Versioning.TargetFrameworkAttribute(\".NETStandard,Version=v2.1\", FrameworkDisplayName=\".NET Standard 2.1\")>]\ndo ()\n"
                    )

                    File.WriteAllText(
                        assemblyInfoSourcePath,
                        "// Auto-Generated by FAKE; do not edit\nnamespace System\nopen System.Reflection\n\n[<assembly: AssemblyTitleAttribute(\"IcedTasks\")>]\n[<assembly: AssemblyProductAttribute(\"IcedTasks\")>]\n[<assembly: AssemblyVersionAttribute(\"0.11.9\")>]\n[<assembly: AssemblyMetadataAttribute(\"ReleaseDate\",\"2025-09-05T00:00:00.0000000-04:00\")>]\n[<assembly: AssemblyFileVersionAttribute(\"0.11.9\")>]\n[<assembly: AssemblyInformationalVersionAttribute(\"0.11.9\")>]\n[<assembly: AssemblyMetadataAttribute(\"ReleaseChannel\",\"release\")>]\n[<assembly: AssemblyMetadataAttribute(\"GitHash\",\"ef640f5d11e4c7b50234dfe90f0e45d5976a9b2d\")>]\ndo ()\n\nmodule internal AssemblyVersionInformation =\n    let [<Literal>] AssemblyTitle = \"IcedTasks\"\n    let [<Literal>] AssemblyProduct = \"IcedTasks\"\n    let [<Literal>] AssemblyVersion = \"0.11.9\"\n    let [<Literal>] AssemblyMetadata_ReleaseDate = \"2025-09-05T00:00:00.0000000-04:00\"\n    let [<Literal>] AssemblyFileVersion = \"0.11.9\"\n    let [<Literal>] AssemblyInformationalVersion = \"0.11.9\"\n    let [<Literal>] AssemblyMetadata_ReleaseChannel = \"release\"\n    let [<Literal>] AssemblyMetadata_GitHash = \"ef640f5d11e4c7b50234dfe90f0e45d5976a9b2d\"\n"
                    )

                    File.WriteAllText(tracerSourcePath, "module Tracer\nlet answer () = 42\n")

                    File.WriteAllLines(
                        responsePath,
                        [|
                            $"-o:{outputPath}"
                            "--debug:portable"
                            "--noframework"
                            $"-r:{netstandardReferencePath}"
                            "--target:library"
                            "--deterministic+"
                            targetFrameworkSourcePath
                            assemblyInfoSourcePath
                            tracerSourcePath
                        |]
                    )

                    let result = invokeFsc2 root responsePath
                    Expect.equal result.ExitCode 0 result.StandardError

                    use implementationStream = File.OpenRead(outputPath)
                    use implementation = new PEReader(implementationStream)
                    let metadata = implementation.GetMetadataReader()

                    Expect.equal
                        (metadata.GetAssemblyDefinition().Version)
                        (System.Version(0, 11, 9, 0))
                        "AssemblyVersionAttribute should set the AssemblyDef version"

                    Expect.isEmpty
                        (findAssemblyAttributes
                            metadata
                            "System.Reflection.AssemblyVersionAttribute")
                        "AssemblyVersionAttribute should not remain as a custom attribute"

                    let (targetFrameworkAttribute, targetFrameworkConstructor, targetFrameworkScope) =
                        findAssemblyAttribute
                            metadata
                            "System.Runtime.Versioning.TargetFrameworkAttribute"

                    Expect.equal
                        (metadata.GetString(targetFrameworkScope.Name))
                        "netstandard"
                        "the attribute type should resolve through the supplied facade"

                    Expect.equal
                        targetFrameworkScope.Version
                        (System.Version(2, 1, 0, 0))
                        "the attribute scope should retain the supplied reference version"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(targetFrameworkScope.PublicKeyOrToken))
                        (Convert.FromHexString("CC7B13FFCD2DDD51"))
                        "the attribute scope should retain the supplied reference identity"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(targetFrameworkConstructor.Signature))
                        (Convert.FromHexString("2001010E"))
                        "the constructor signature should match the Compatibility Oracle"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(targetFrameworkAttribute.Value))
                        (Convert.FromHexString(
                            "0100192E4E45545374616E646172642C56657273696F6E3D76322E310100540E144672616D65776F726B446973706C61794E616D65112E4E4554205374616E6461726420322E31"
                        ))
                        "the attribute value should match the Compatibility Oracle"

                    let (assemblyTitleAttribute, assemblyTitleConstructor, assemblyTitleScope) =
                        findAssemblyAttribute metadata "System.Reflection.AssemblyTitleAttribute"

                    Expect.equal
                        (metadata.GetString(assemblyTitleScope.Name))
                        "netstandard"
                        "authored assembly attributes should resolve through the supplied facade"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(assemblyTitleConstructor.Signature))
                        (Convert.FromHexString("2001010E"))
                        "a one-string attribute constructor should match the Compatibility Oracle"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(assemblyTitleAttribute.Value))
                        (Convert.FromHexString("010009496365645461736B730000"))
                        "the assembly-title value should match the Compatibility Oracle"

                    let assemblyMetadataAttributes =
                        findAssemblyAttributes
                            metadata
                            "System.Reflection.AssemblyMetadataAttribute"

                    Expect.equal
                        assemblyMetadataAttributes.Length
                        3
                        "every repeated assembly-metadata attribute should be emitted"

                    let (releaseDateAttribute, releaseDateConstructor, _) =
                        assemblyMetadataAttributes.[0]

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(releaseDateConstructor.Signature))
                        (Convert.FromHexString("2002010E0E"))
                        "a two-string attribute constructor should match the Compatibility Oracle"

                    Expect.sequenceEqual
                        (metadata.GetBlobBytes(releaseDateAttribute.Value))
                        (Convert.FromHexString(
                            "01000B52656C656173654461746521323032352D30392D30355430303A30303A30302E303030303030302D30343A30300000"
                        ))
                        "the assembly-metadata value should match the Compatibility Oracle"

                    let assemblyVersionInformation =
                        metadata.TypeDefinitions
                        |> Seq.map metadata.GetTypeDefinition
                        |> Seq.find (fun definition ->
                            metadata.GetString(definition.Namespace) = "System"
                            && metadata.GetString(definition.Name) = "AssemblyVersionInformation"
                        )

                    Expect.equal
                        assemblyVersionInformation.Attributes
                        (TypeAttributes.Abstract
                         ||| TypeAttributes.Sealed)
                        "the internal F# module type should match the Compatibility Oracle"

                    let literalFields =
                        assemblyVersionInformation.GetFields()
                        |> Seq.map (fun handle ->
                            let field = metadata.GetFieldDefinition(handle)
                            let constant = metadata.GetConstant(field.GetDefaultValue())

                            metadata.GetString(field.Name),
                            field.Attributes,
                            metadata.GetBlobBytes(field.Signature),
                            (metadata.GetBlobBytes(constant.Value)
                             |> Encoding.Unicode.GetString)
                        )
                        |> Seq.toArray

                    let expectedLiterals = [|
                        "AssemblyTitle", "IcedTasks"
                        "AssemblyProduct", "IcedTasks"
                        "AssemblyVersion", "0.11.9"
                        "AssemblyMetadata_ReleaseDate", "2025-09-05T00:00:00.0000000-04:00"
                        "AssemblyFileVersion", "0.11.9"
                        "AssemblyInformationalVersion", "0.11.9"
                        "AssemblyMetadata_ReleaseChannel", "release"
                        "AssemblyMetadata_GitHash", "ef640f5d11e4c7b50234dfe90f0e45d5976a9b2d"
                    |]

                    Expect.equal
                        literalFields.Length
                        expectedLiterals.Length
                        "every literal declaration should produce one field"

                    for index in
                        0 .. expectedLiterals.Length
                             - 1 do
                        let expectedName, expectedValue = expectedLiterals.[index]
                        let actualName, attributes, signature, actualValue = literalFields.[index]

                        Expect.equal
                            actualName
                            expectedName
                            "literal fields should preserve source order"

                        Expect.equal
                            attributes
                            (FieldAttributes.Assembly
                             ||| FieldAttributes.Static
                             ||| FieldAttributes.Literal
                             ||| FieldAttributes.HasDefault)
                            "literal field attributes should match the Compatibility Oracle"

                        Expect.sequenceEqual
                            signature
                            (Convert.FromHexString("060E"))
                            "literal fields should use a string field signature"

                        Expect.equal
                            actualValue
                            expectedValue
                            "literal field constants should match the Compatibility Oracle"

                    let typeNames =
                        metadata.TypeDefinitions
                        |> Seq.map (fun handle ->
                            let definition = metadata.GetTypeDefinition(handle)

                            metadata.GetString(definition.Name)
                        )
                        |> Seq.toArray

                    Expect.sequenceEqual
                        typeNames
                        [|
                            "<Module>"
                            "AssemblyVersionInformation"
                            "Tracer"
                        |]
                        "only the authored assembly-info module should add a runtime type"

                    use pdbStream = File.OpenRead(pdbPath)
                    use pdbProvider = MetadataReaderProvider.FromPortablePdbStream(pdbStream)
                    let pdb = pdbProvider.GetMetadataReader()

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
                            ".NETStandard,Version=v2.1.AssemblyAttributes.fs"
                            "AssemblyInfo.fs"
                            "Tracer.fs"
                        |]
                        "the portable PDB should retain the generated source in order"
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
                        "48"
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

                    pdbProvider.Dispose()
                    pdbStream.Dispose()

                    let consumer = invokeConsumer root outputPath 43
                    Expect.equal consumer.ExitCode 0 consumer.StandardError

                    Expect.equal
                        (consumer.StandardOutput.Trim())
                        "43"
                        "the edited implementation must not reuse stale typed code"

                    let conditionalBaselineSource =
                        "module Tracer\n#if UNUSED\nlet hidden = 11\n#endif\nlet answer () = 43\n"

                    let conditionalEditedSource =
                        "module Tracer\n#if UNUSED\nlet hidden = 22\n#endif\nlet answer () = 43\n"

                    let conditionalBaseline =
                        compile conditionalBaselineSource "conditional-baseline"

                    let conditionalEdited = compile conditionalEditedSource "conditional-edited"

                    Expect.equal
                        conditionalEdited.["parse"]
                        "miss"
                        "an inactive-source edit should still refresh the physical source input"

                    Expect.equal
                        conditionalEdited.["check"]
                        "hit"
                        "an equal active program should reuse its semantic check"

                    Expect.equal
                        conditionalEdited.["lower"]
                        "miss"
                        "a changed source checksum should rebuild the debug fragment"

                    Expect.equal
                        conditionalEdited.["checkKey"]
                        conditionalBaseline.["checkKey"]
                        "inactive source text should not change the semantic check key"

                    Expect.notEqual
                        conditionalEdited.["lowerKey"]
                        conditionalBaseline.["lowerKey"]
                        "debug inputs should participate independently in the lowering key"

                    Expect.equal
                        conditionalEdited.["exportFingerprint"]
                        conditionalBaseline.["exportFingerprint"]
                        "inactive source text should not change exported meaning"

                    Expect.equal
                        conditionalEdited.["fragmentHash"]
                        conditionalBaseline.["fragmentHash"]
                        "inactive source text should not change implementation fragments"

                    do
                        use conditionalPdbStream = File.OpenRead(pdbPath)

                        use conditionalPdbProvider =
                            MetadataReaderProvider.FromPortablePdbStream(conditionalPdbStream)

                        let conditionalPdb = conditionalPdbProvider.GetMetadataReader()

                        let conditionalDocument =
                            conditionalPdb.Documents
                            |> Seq.exactlyOne
                            |> conditionalPdb.GetDocument

                        let conditionalChecksum =
                            conditionalEditedSource
                            |> Encoding.UTF8.GetBytes
                            |> SHA256.HashData

                        Expect.sequenceEqual
                            (conditionalPdb.GetBlobBytes(conditionalDocument.Hash))
                            conditionalChecksum
                            "semantic reuse must not retain the prior physical source checksum"
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
                            "3"
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

                        do
                            use implementationStream = File.OpenRead(outputPath)
                            use implementation = new PEReader(implementationStream)
                            let metadata = implementation.GetMetadataReader()

                            let (targetFrameworkAttribute,
                                 targetFrameworkConstructor,
                                 targetFrameworkScope) =
                                findAssemblyAttribute
                                    metadata
                                    "System.Runtime.Versioning.TargetFrameworkAttribute"

                            Expect.equal
                                (metadata.GetString(targetFrameworkConstructor.Name))
                                ".ctor"
                                "the target-framework attribute should use its string constructor"

                            Expect.sequenceEqual
                                (metadata.GetBlobBytes(targetFrameworkConstructor.Signature))
                                [|
                                    0x20uy
                                    0x01uy
                                    0x01uy
                                    0x0euy
                                |]
                                "the constructor signature should be instance void(string)"

                            Expect.equal
                                (metadata.GetString(targetFrameworkScope.Name))
                                "System.Runtime"
                                "the attribute type should resolve through the target reference set"

                            Expect.equal
                                targetFrameworkScope.Version
                                (System.Version(10, 0, 0, 0))
                                "the attribute scope should retain the target reference version"

                            Expect.sequenceEqual
                                (metadata.GetBlobBytes(targetFrameworkAttribute.Value))
                                (Convert.FromHexString(
                                    "0100192E4E4554436F72654170702C56657273696F6E3D7631302E300100540E144672616D65776F726B446973706C61794E616D65092E4E45542031302E30"
                                ))
                                "the target-framework attribute value should match the Compatibility Oracle"

                            let hasBuildSettingsType =
                                metadata.TypeDefinitions
                                |> Seq.exists (fun handle ->
                                    let definition = metadata.GetTypeDefinition(handle)

                                    metadata.GetString(definition.Namespace) = "Microsoft.BuildSettings"
                                )

                            Expect.isFalse
                                hasBuildSettingsType
                                "the generated assembly-attribute file should not add a runtime type"

                            use pdbStream = File.OpenRead(pdbPath)

                            use pdbProvider =
                                MetadataReaderProvider.FromPortablePdbStream(pdbStream)

                            let pdb = pdbProvider.GetMetadataReader()

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
                                    ".NETCoreApp,Version=v10.0.AssemblyAttributes.fs"
                                    "First.fs"
                                    "Tracer.fs"
                                |]
                                "the portable PDB should retain every evaluated source in order"

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
