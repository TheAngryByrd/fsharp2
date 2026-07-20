namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Globalization
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
    type private TargetReferenceIdentity = {
        Name: string
        Version: Version
        Culture: string
        PublicKeyToken: byte array
        Flags: AssemblyFlags
    }

    let private sha256DocumentHashAlgorithm =
        Guid("8829d00f-11b8-4213-878b-770e8597ac16")

    let private fsharpLanguage = Guid("ab4f38c9-b6e6-43ba-be3b-58080b2ccce3")

    let private sourceLinkKind = Guid("cc110556-a091-4d38-9fec-25ab9a351a6a")

    let private immutableBytes (bytes: byte array) = ImmutableArray.CreateRange<byte>(bytes)

    let private publicKeyToken (publicKey: byte array) =
        if publicKey.Length = 0 then
            Array.empty
        else
            let hash = SHA1.HashData(publicKey)

            Array.init
                8
                (fun index ->
                    hash.[hash.Length
                          - index
                          - 1]
                )

    let private tryReadTargetReference (expectedName: string) (path: string) =
        if
            not (
                String.Equals(
                    Path.GetFileNameWithoutExtension(path),
                    expectedName,
                    StringComparison.OrdinalIgnoreCase
                )
            )
        then
            None
        else
            use stream = File.OpenRead(path)
            use pe = new PEReader(stream)
            let metadata = pe.GetMetadataReader()
            let definition = metadata.GetAssemblyDefinition()
            let publicKey = metadata.GetBlobBytes(definition.PublicKey)

            Some {
                Name = metadata.GetString(definition.Name)
                Version = definition.Version
                Culture =
                    if definition.Culture.IsNil then
                        String.Empty
                    else
                        metadata.GetString(definition.Culture)
                PublicKeyToken = publicKeyToken publicKey
                Flags =
                    enum<AssemblyFlags> (
                        int definition.Flags
                        &&& ~~~(int AssemblyFlags.PublicKey)
                    )
            }

    let private defaultSystemRuntimeReference = {
        Name = "System.Runtime"
        Version = Version(10, 0, 0, 0)
        Culture = String.Empty
        PublicKeyToken = [|
            0xb0uy
            0x3fuy
            0x5fuy
            0x7fuy
            0x11uy
            0xd5uy
            0x0auy
            0x3auy
        |]
        Flags = enum<AssemblyFlags> 0
    }

    let private targetReferenceName (symbolic: SymbolicAssembly) =
        symbolic.AssemblyAttributes
        |> List.tryPick (fun attribute ->
            if
                attribute.Kind = TargetFrameworkAttribute
                && (attribute.ConstructorArguments
                    |> List.tryHead
                    |> Option.exists (fun argument ->
                        argument.StartsWith(".NETStandard,", StringComparison.Ordinal)
                    ))
            then
                Some "netstandard"
            else
                None
        )
        |> Option.defaultValue "System.Runtime"

    let private resolveTargetReference invocation symbolic =
        let expectedName = targetReferenceName symbolic

        match
            invocation.ReferencePaths
            |> List.tryPick (tryReadTargetReference expectedName)
        with
        | Some reference -> reference
        | None when
            List.isEmpty invocation.ReferencePaths
            && expectedName = "System.Runtime"
            ->
            // The original synthetic prototype accepted no explicit reference
            // closure and targets its own .NET 10 host profile. Real
            // Compiler Target Invocations always resolve from evaluated refs.
            defaultSystemRuntimeReference
        | None ->
            invalidOp (
                "the target reference set does not contain assembly '"
                + expectedName
                + "'"
            )

    let private resolveRequiredReference invocation expectedName =
        match
            invocation.ReferencePaths
            |> List.tryPick (tryReadTargetReference expectedName)
        with
        | Some reference -> reference
        | None ->
            invalidOp (
                "the target reference set does not contain assembly '"
                + expectedName
                + "'"
            )

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

    let rec private encodeCliType resolveTypeReference (encoder: SignatureTypeEncoder) =
        function
        | CliInt32 -> encoder.Int32()
        | CliBoolean -> encoder.Boolean()
        | CliString -> encoder.String()
        | CliObject -> encoder.Object()
        | CliNativeInt -> encoder.IntPtr()
        | CliTypeParameter index -> encoder.GenericTypeParameter(index)
        | CliMethodTypeParameter index -> encoder.GenericMethodTypeParameter(index)
        | CliVoid -> invalidOp "void is valid only as a method return type"
        | CliByRef _ -> invalidOp "byref must be encoded by a return or parameter encoder"
        | CliNamedType typeReference ->
            encoder.Type(resolveTypeReference typeReference, typeReference.IsValueType)
        | CliGenericType(typeReference, arguments) ->
            let argumentEncoder =
                encoder.GenericInstantiation(
                    resolveTypeReference typeReference,
                    arguments.Length,
                    typeReference.IsValueType
                )

            for argument in arguments do
                encodeCliType resolveTypeReference (argumentEncoder.AddArgument()) argument

    let private encodeReturnType resolveTypeReference (encoder: ReturnTypeEncoder) =
        function
        | CliVoid -> encoder.Void()
        | CliByRef elementType ->
            encodeCliType resolveTypeReference (encoder.Type(true)) elementType
        | returnType -> encodeCliType resolveTypeReference (encoder.Type(false)) returnType

    let private encodeParameterType resolveTypeReference (encoder: ParameterTypeEncoder) =
        function
        | CliVoid -> invalidOp "a method parameter cannot have type void"
        | CliByRef elementType ->
            encodeCliType resolveTypeReference (encoder.Type(true)) elementType
        | parameterType -> encodeCliType resolveTypeReference (encoder.Type(false)) parameterType

    type private MethodKindEncoding = {
        IsInstance: bool
        Attributes: MethodAttributes
    }

    let private methodKindEncoding =
        function
        | ModuleFunction -> {
            IsInstance = false
            Attributes =
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig
          }
        | TypeExtensionMember -> {
            IsInstance = false
            Attributes =
                MethodAttributes.Public
                ||| MethodAttributes.Static
          }
        | StaticInlineMemberStub -> {
            IsInstance = false
            Attributes =
                MethodAttributes.Public
                ||| MethodAttributes.Static
          }
        | InstanceConstructor -> {
            IsInstance = true
            Attributes =
                MethodAttributes.Public
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName
          }
        | InstanceInlineMember -> {
            IsInstance = true
            Attributes =
                MethodAttributes.Public
                ||| MethodAttributes.HideBySig
          }
        | InternalInstanceInlineMember -> {
            IsInstance = true
            Attributes =
                MethodAttributes.Assembly
                ||| MethodAttributes.HideBySig
          }
        | ClosureConstructor -> {
            IsInstance = true
            Attributes =
                MethodAttributes.Public
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName
          }
        | ClosureInvoke -> {
            IsInstance = true
            Attributes =
                MethodAttributes.Assembly
                ||| MethodAttributes.HideBySig
          }

    let private encodeCallableSignature
        resolveTypeReference
        genericParameterCount
        isInstanceMethod
        parameterTypes
        returnType
        =
        let signature = BlobBuilder()

        BlobEncoder(signature)
            .MethodSignature(
                genericParameterCount = genericParameterCount,
                isInstanceMethod = isInstanceMethod
            )
            .Parameters(
                parameterTypes
                |> List.length,
                (fun encoder -> encodeReturnType resolveTypeReference encoder returnType),
                (fun parameters ->
                    for parameterType in parameterTypes do
                        encodeParameterType
                            resolveTypeReference
                            (parameters.AddParameter())
                            parameterType
                )
            )

        signature

    let private encodeMethodSignature
        resolveTypeReference
        (methodFragment: SymbolicMethodFragment)
        =
        encodeCallableSignature
            resolveTypeReference
            methodFragment.GenericParameters.Length
            (methodKindEncoding methodFragment.Kind).IsInstance
            (methodFragment.Parameters
             |> List.map _.Type)
            methodFragment.ReturnType

    let private encodeFieldSignature resolveTypeReference fieldType =
        let signature = BlobBuilder()
        encodeCliType resolveTypeReference (BlobEncoder(signature).FieldSignature()) fieldType
        signature

    let private encodeLocalType resolveTypeReference (encoder: LocalVariableTypeEncoder) =
        function
        | CliVoid -> invalidOp "a local variable cannot have type void"
        | CliByRef elementType ->
            encodeCliType resolveTypeReference (encoder.Type(true, false)) elementType
        | localType -> encodeCliType resolveTypeReference (encoder.Type(false, false)) localType

    let private encodeLocalSignature resolveTypeReference (locals: SymbolicLocalFragment list) =
        let orderedLocals =
            locals
            |> List.sortBy _.Index

        orderedLocals
        |> List.iteri (fun expectedIndex local ->
            if
                local.Index
                <> expectedIndex
            then
                invalidOp "symbolic local indices must be contiguous and zero-based"
        )

        let signature = BlobBuilder()

        let variables = BlobEncoder(signature).LocalVariableSignature(orderedLocals.Length)

        for local in orderedLocals do
            encodeLocalType resolveTypeReference (variables.AddVariable()) local.Type

        signature

    let private encodeMethodReferenceSignature
        resolveTypeReference
        (methodReference: SymbolicMethodReference)
        =
        encodeCallableSignature
            resolveTypeReference
            methodReference.GenericArity
            methodReference.IsInstance
            methodReference.ParameterTypes
            methodReference.ReturnType

    let private encodeMethodBody
        (metadata: MetadataBuilder)
        (resolveDeclaringType: SymbolicDeclaringType -> EntityHandle)
        resolveTypeReference
        (stream: MethodBodyStreamEncoder)
        (methodFragment: SymbolicMethodFragment)
        =
        let code = BlobBuilder()
        let controlFlow = ControlFlowBuilder()
        let instructions = InstructionEncoder(code, controlFlow)
        let sequencePoints = ResizeArray<int * SourceRange option>()
        let labels = Dictionary<int, LabelHandle>()

        let resolveLabel label =
            match labels.TryGetValue(label) with
            | true, handle -> handle
            | false, _ ->
                let handle = instructions.DefineLabel()
                labels.Add(label, handle)
                handle

        let localSignature =
            if List.isEmpty methodFragment.Locals then
                Unchecked.defaultof<StandaloneSignatureHandle>
            else
                methodFragment.Locals
                |> encodeLocalSignature resolveTypeReference
                |> metadata.GetOrAddBlob
                |> metadata.AddStandaloneSignature

        let addMethodReference (methodReference: SymbolicMethodReference) =
            metadata.AddMemberReference(
                resolveDeclaringType methodReference.DeclaringType,
                metadata.GetOrAddString(methodReference.Name),
                encodeMethodReferenceSignature resolveTypeReference methodReference
                |> metadata.GetOrAddBlob
            )

        let addMethodSpecification
            (methodReference: SymbolicMethodReference)
            (genericArguments: CliType list)
            =
            let signature = BlobBuilder()

            let arguments =
                BlobEncoder(signature).MethodSpecificationSignature(genericArguments.Length)

            for argument in genericArguments do
                encodeCliType resolveTypeReference (arguments.AddArgument()) argument

            metadata.AddMethodSpecification(
                addMethodReference methodReference,
                metadata.GetOrAddBlob(signature)
            )

        let addFieldReference (fieldReference: SymbolicFieldReference) =
            metadata.AddMemberReference(
                resolveDeclaringType fieldReference.DeclaringType,
                metadata.GetOrAddString(fieldReference.Name),
                encodeFieldSignature resolveTypeReference fieldReference.FieldType
                |> metadata.GetOrAddBlob
            )

        for instruction in methodFragment.Instructions do
            match instruction with
            | MarkSequencePoint range -> sequencePoints.Add(instructions.Offset, Some range)
            | MarkHiddenSequencePoint -> sequencePoints.Add(instructions.Offset, None)
            | MarkLabel label -> instructions.MarkLabel(resolveLabel label)
            | BranchIfFalse label -> instructions.Branch(ILOpCode.Brfalse, resolveLabel label)
            | Branch label -> instructions.Branch(ILOpCode.Br, resolveLabel label)
            | Nop -> instructions.OpCode(ILOpCode.Nop)
            | CompareEqual -> instructions.OpCode(ILOpCode.Ceq)
            | Box cliType ->
                instructions.OpCode(ILOpCode.Box)
                instructions.Token(resolveDeclaringType (CliDeclaringType cliType))
            | LoadInt32 value -> instructions.LoadConstantI4(value)
            | LoadString value ->
                value
                |> metadata.GetOrAddUserString
                |> instructions.LoadString
            | LoadNull -> instructions.OpCode(ILOpCode.Ldnull)
            | LoadArgument index -> instructions.LoadArgument(index)
            | LoadArgumentAddress index -> instructions.LoadArgumentAddress(index)
            | LoadLocal index -> instructions.LoadLocal(index)
            | LoadLocalAddress index -> instructions.LoadLocalAddress(index)
            | StoreLocal index -> instructions.StoreLocal(index)
            | LoadField fieldReference ->
                instructions.OpCode(ILOpCode.Ldfld)
                instructions.Token(addFieldReference fieldReference)
            | LoadFieldAddress fieldReference ->
                instructions.OpCode(ILOpCode.Ldflda)
                instructions.Token(addFieldReference fieldReference)
            | StoreField fieldReference ->
                instructions.OpCode(ILOpCode.Stfld)
                instructions.Token(addFieldReference fieldReference)
            | CallMethod methodReference ->
                instructions.OpCode(ILOpCode.Call)
                instructions.Token(addMethodReference methodReference)
            | CallVirtualMethod methodReference ->
                instructions.OpCode(ILOpCode.Callvirt)
                instructions.Token(addMethodReference methodReference)
            | CallGenericMethod(methodReference, genericArguments) ->
                instructions.OpCode(ILOpCode.Call)
                instructions.Token(addMethodSpecification methodReference genericArguments)
            | LoadFunctionPointer methodReference ->
                instructions.OpCode(ILOpCode.Ldftn)
                instructions.Token(addMethodReference methodReference)
            | NewObject methodReference ->
                instructions.OpCode(ILOpCode.Newobj)
                instructions.Token(addMethodReference methodReference)
            | Pop -> instructions.OpCode(ILOpCode.Pop)
            | Throw -> instructions.OpCode(ILOpCode.Throw)
            | Return -> instructions.OpCode(ILOpCode.Ret)

        let codeSize = code.Count

        stream.AddMethodBody(
            instructions,
            maxStack = methodFragment.MaxStack,
            localVariablesSignature = localSignature
        ),
        codeSize,
        localSignature,
        List.ofSeq sequencePoints

    let private encodeStringConstructorSignature (parameterCount: int) =
        let signature = BlobBuilder()

        BlobEncoder(signature)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                parameterCount,
                (fun returnType -> returnType.Void()),
                (fun parameters ->
                    for _ in 1..parameterCount do
                        parameters.AddParameter().Type().String()
                )
            )

        signature

    let private encodeAssemblyAttributeValue (attribute: SymbolicAssemblyAttributeFragment) =
        let value = BlobBuilder()

        BlobEncoder(value)
            .CustomAttributeSignature(
                (fun fixedArguments ->
                    for argument in attribute.ConstructorArguments do
                        fixedArguments.AddArgument().Scalar().Constant(argument)
                ),
                (fun namedArguments ->
                    let arguments = namedArguments.Count(attribute.NamedArguments.Length)

                    for argument in attribute.NamedArguments do
                        arguments.AddArgument(
                            false,
                            (fun argumentType -> argumentType.ScalarType().String()),
                            (fun argumentName -> argumentName.Name(argument.Name)),
                            (fun literal -> literal.Scalar().Constant(argument.Value))
                        )
                )
            )

        value

    let private knownAttributeTypeName =
        function
        | AutoOpenAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "AutoOpenAttribute"
          }
        | StructAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "StructAttribute"
          }
        | NoComparisonAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "NoComparisonAttribute"
          }
        | NoEqualityAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "NoEqualityAttribute"
          }
        | DefaultValueAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "DefaultValueAttribute"
          }
        | InlineIfLambdaAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "InlineIfLambdaAttribute"
          }
        | NoEagerConstraintApplicationAttribute -> {
            Namespace = "Microsoft.FSharp.Core.CompilerServices"
            Name = "NoEagerConstraintApplicationAttribute"
          }
        | CompilationMappingAttribute -> {
            Namespace = "Microsoft.FSharp.Core"
            Name = "CompilationMappingAttribute"
          }

    let private encodeKnownAttributeConstructorSignature
        (sourceConstructFlags: TypeReferenceHandle)
        (attribute: SymbolicCustomAttributeFragment)
        =
        let signature = BlobBuilder()

        BlobEncoder(signature)
            .MethodSignature(isInstanceMethod = true)
            .Parameters(
                attribute.ConstructorArguments.Length,
                (fun returnType -> returnType.Void()),
                (fun parameters ->
                    for argument in attribute.ConstructorArguments do
                        let parameter = parameters.AddParameter().Type()

                        match attribute.Kind, argument with
                        | DefaultValueAttribute, TypedBooleanAttributeArgument _ ->
                            parameter.Boolean()
                        | CompilationMappingAttribute, TypedSourceConstructAttributeArgument _ ->
                            parameter.Type(sourceConstructFlags, true)
                        | _ ->
                            invalidOp
                                "the symbolic custom attribute has an invalid constructor argument"
                )
            )

        signature

    let private encodeKnownAttributeValue (attribute: SymbolicCustomAttributeFragment) =
        let value = BlobBuilder()

        BlobEncoder(value)
            .CustomAttributeSignature(
                (fun fixedArguments ->
                    for argument in attribute.ConstructorArguments do
                        let scalar = fixedArguments.AddArgument().Scalar()

                        match argument with
                        | TypedBooleanAttributeArgument argumentValue ->
                            scalar.Constant(argumentValue)
                        | TypedSourceConstructAttributeArgument sourceConstruct ->
                            let argumentValue =
                                match sourceConstruct with
                                | ObjectTypeConstruct -> 3
                                | ModuleConstruct -> 7

                            scalar.Constant(argumentValue)
                ),
                (fun namedArguments ->
                    namedArguments.Count(0)
                    |> ignore
                )
            )

        value

    let private encodeSequencePoints
        (localSignature: StandaloneSignatureHandle)
        (instructionSequencePoints: (int * SourceRange option) list)
        (methodFragment: SymbolicMethodFragment)
        =
        let points =
            match instructionSequencePoints, methodFragment.EmitDefaultSequencePoint with
            | [], true -> [ 0, Some methodFragment.Range ]
            | points, _ -> points

        match points with
        | [] -> BlobBuilder()
        | points ->
            let sequencePoints = BlobBuilder()

            let localSignatureRow =
                if localSignature.IsNil then
                    0
                else
                    MetadataTokens.GetRowNumber(localSignature)

            sequencePoints.WriteCompressedInteger(localSignatureRow)

            let mutable previousOffset = 0
            let mutable previousStartLine = 0
            let mutable previousStartColumn = 0
            let mutable hasPreviousVisiblePoint = false

            points
            |> List.iteri (fun index (offset, range) ->
                if
                    index > 0
                    && offset
                       <= previousOffset
                then
                    invalidOp "sequence-point offsets must be strictly increasing"

                let offsetDelta =
                    if index = 0 then
                        offset
                    else
                        offset
                        - previousOffset

                sequencePoints.WriteCompressedInteger(offsetDelta)

                match range with
                | None ->
                    sequencePoints.WriteCompressedInteger(0)
                    sequencePoints.WriteCompressedInteger(0)
                | Some range ->
                    let deltaLines =
                        range.End.Line
                        - range.Start.Line

                    let deltaColumns =
                        range.End.Column
                        - range.Start.Column

                    sequencePoints.WriteCompressedInteger(deltaLines)

                    if deltaLines = 0 then
                        sequencePoints.WriteCompressedInteger(deltaColumns)
                    else
                        sequencePoints.WriteCompressedSignedInteger(deltaColumns)

                    if not hasPreviousVisiblePoint then
                        sequencePoints.WriteCompressedInteger(range.Start.Line)
                        sequencePoints.WriteCompressedInteger(range.Start.Column)
                        hasPreviousVisiblePoint <- true
                    else
                        sequencePoints.WriteCompressedSignedInteger(
                            range.Start.Line
                            - previousStartLine
                        )

                        sequencePoints.WriteCompressedSignedInteger(
                            range.Start.Column
                            - previousStartColumn
                        )

                    previousStartLine <- range.Start.Line
                    previousStartColumn <- range.Start.Column

                previousOffset <- offset
            )

            sequencePoints

    let private escapeXml (value: string) =
        value
            .Replace("&", "&amp;", StringComparison.Ordinal)
            .Replace("<", "&lt;", StringComparison.Ordinal)
            .Replace(">", "&gt;", StringComparison.Ordinal)
            .Replace("\"", "&quot;", StringComparison.Ordinal)
            .Replace("'", "&apos;", StringComparison.Ordinal)

    let private documentation (assemblyName: string) =
        String.concat "\n" [
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
        let typeAbbreviationFragments = symbolic.Module.TypeAbbreviations

        let methodFragments =
            typeFragments
            |> List.collect _.Methods

        let customAttributeFragments = [
            for typeFragment in typeFragments do
                yield! typeFragment.Attributes

                for fieldFragment in typeFragment.InstanceFields do
                    yield! fieldFragment.Attributes

                for methodFragment in typeFragment.Methods do
                    yield! methodFragment.Attributes

                    for parameterFragment in methodFragment.Parameters do
                        yield! parameterFragment.Attributes
        ]

        let rec invalidTypeExpression =
            function
            | TypedNamedType resolvedType ->
                String.IsNullOrWhiteSpace(resolvedType.TypeName.Name)
                || String.IsNullOrWhiteSpace(resolvedType.DeclarationId)
            | TypedTypeParameter name -> String.IsNullOrWhiteSpace(name)
            | TypedGenericTypeApplication(genericType, arguments) ->
                invalidTypeExpression genericType
                || (arguments
                    |> List.exists invalidTypeExpression)
            | TypedByRefType elementType -> invalidTypeExpression elementType
            | TypedTupleType elements ->
                elements
                |> List.exists invalidTypeExpression
            | TypedFunctionType(domain, range) ->
                invalidTypeExpression domain
                || invalidTypeExpression range

        let invalidTypeConstraint =
            function
            | TypedSubtypeConstraint(typeParameter, superType) ->
                String.IsNullOrWhiteSpace(typeParameter)
                || invalidTypeExpression superType
            | TypedMemberConstraint(typeParameter, memberName, memberType) ->
                String.IsNullOrWhiteSpace(typeParameter)
                || String.IsNullOrWhiteSpace(memberName)
                || invalidTypeExpression memberType

        if
            symbolic.SchemaVersion
            <> symbolic.Module.SchemaVersion
            || (symbolic.Documents
                |> List.exists (fun document ->
                    symbolic.SchemaVersion
                    <> document.SchemaVersion
                ))
            || (symbolic.AssemblyAttributes
                |> List.exists (fun attribute ->
                    symbolic.SchemaVersion
                    <> attribute.SchemaVersion
                ))
            || (typeAbbreviationFragments
                |> List.exists (fun typeAbbreviation ->
                    symbolic.SchemaVersion
                    <> typeAbbreviation.SchemaVersion
                ))
            || (typeFragments
                |> List.exists (fun typeFragment ->
                    symbolic.SchemaVersion
                    <> typeFragment.SchemaVersion
                    || (typeFragment.Attributes
                        |> List.exists (fun attribute ->
                            symbolic.SchemaVersion
                            <> attribute.SchemaVersion
                        ))
                    || (typeFragment.LiteralFields
                        |> List.exists (fun fieldFragment ->
                            symbolic.SchemaVersion
                            <> fieldFragment.SchemaVersion
                        ))
                    || (typeFragment.InstanceFields
                        |> List.exists (fun fieldFragment ->
                            symbolic.SchemaVersion
                            <> fieldFragment.SchemaVersion
                            || (fieldFragment.Attributes
                                |> List.exists (fun attribute ->
                                    symbolic.SchemaVersion
                                    <> attribute.SchemaVersion
                                ))
                        ))
                    || (typeFragment.Methods
                        |> List.exists (fun methodFragment ->
                            symbolic.SchemaVersion
                            <> methodFragment.SchemaVersion
                            || (methodFragment.Attributes
                                |> List.exists (fun attribute ->
                                    symbolic.SchemaVersion
                                    <> attribute.SchemaVersion
                                ))
                            || (methodFragment.Parameters
                                |> List.exists (fun parameterFragment ->
                                    parameterFragment.Attributes
                                    |> List.exists (fun attribute ->
                                        symbolic.SchemaVersion
                                        <> attribute.SchemaVersion
                                    )
                                ))
                        ))
                ))
        then
            invalidOp "the symbolic emission graph uses inconsistent schema versions"

        if
            String.IsNullOrWhiteSpace(symbolic.StableId)
            || String.IsNullOrWhiteSpace(symbolic.Module.StableId)
            || (symbolic.Documents
                |> List.exists (fun document -> String.IsNullOrWhiteSpace(document.StableId)))
            || (symbolic.AssemblyAttributes
                |> List.exists (fun attribute -> String.IsNullOrWhiteSpace(attribute.StableId)))
            || (typeAbbreviationFragments
                |> List.exists (fun typeAbbreviation ->
                    String.IsNullOrWhiteSpace(typeAbbreviation.StableId)
                    || String.IsNullOrWhiteSpace(typeAbbreviation.Name)
                    || (typeAbbreviation.TypeParameters
                        |> List.exists String.IsNullOrWhiteSpace)
                    || (typeAbbreviation.Constraints
                        |> List.exists invalidTypeConstraint)
                    || invalidTypeExpression typeAbbreviation.TargetType
                ))
            || (typeFragments
                |> List.exists (fun typeFragment ->
                    String.IsNullOrWhiteSpace(typeFragment.StableId)
                    || (typeFragment.GenericParameters
                        |> List.exists String.IsNullOrWhiteSpace)
                    || (typeFragment.Attributes
                        |> List.exists (fun attribute ->
                            String.IsNullOrWhiteSpace(attribute.StableId)
                        ))
                    || (typeFragment.LiteralFields
                        |> List.exists (fun fieldFragment ->
                            String.IsNullOrWhiteSpace(fieldFragment.StableId)
                        ))
                    || (typeFragment.InstanceFields
                        |> List.exists (fun fieldFragment ->
                            String.IsNullOrWhiteSpace(fieldFragment.StableId)
                            || String.IsNullOrWhiteSpace(fieldFragment.Name)
                            || (fieldFragment.Attributes
                                |> List.exists (fun attribute ->
                                    String.IsNullOrWhiteSpace(attribute.StableId)
                                ))
                        ))
                    || (typeFragment.Methods
                        |> List.exists (fun methodFragment ->
                            String.IsNullOrWhiteSpace(methodFragment.StableId)
                            || (methodFragment.Attributes
                                |> List.exists (fun attribute ->
                                    String.IsNullOrWhiteSpace(attribute.StableId)
                                ))
                            || (methodFragment.Parameters
                                |> List.exists (fun parameterFragment ->
                                    parameterFragment.Attributes
                                    |> List.exists (fun attribute ->
                                        String.IsNullOrWhiteSpace(attribute.StableId)
                                    )
                                ))
                        ))
                ))
        then
            invalidOp "the symbolic emission graph contains an empty stable identity"

        let typeStableIds =
            HashSet<string>(
                typeFragments
                |> List.map _.StableId,
                StringComparer.Ordinal
            )

        if
            typeStableIds.Count
            <> typeFragments.Length
            || (typeFragments
                |> List.exists (fun typeFragment ->
                    match typeFragment.EnclosingTypeStableId with
                    | Some enclosingTypeStableId ->
                        not (typeStableIds.Contains(enclosingTypeStableId))
                    | None -> false
                ))
        then
            invalidOp "the symbolic type graph has duplicate or missing nesting identities"

        if
            invocation.DebugDocumentPaths.Length
            <> symbolic.Documents.Length
        then
            invalidOp "the symbolic emission graph must contain one debug document per source input"

        let documentChecksums =
            symbolic.Documents
            |> List.map _.Checksum

        if
            methodFragments
            |> List.exists (fun methodFragment ->
                methodFragment.DocumentIndex < 0
                || methodFragment.DocumentIndex
                   >= symbolic.Documents.Length
                || methodFragment.DocumentChecksum
                   <> symbolic.Documents.[methodFragment.DocumentIndex].Checksum
            )
        then
            invalidOp "a symbolic method has inconsistent debug-document metadata"

        let metadata = MetadataBuilder()
        let ilStream = BlobBuilder()
        let methodBodies = MethodBodyStreamEncoder(ilStream)

        let stringFieldSignature =
            let signature = BlobBuilder()
            BlobEncoder(signature).FieldSignature().String()
            signature

        let targetReference = resolveTargetReference invocation symbolic

        let addAssemblyReference (reference: TargetReferenceIdentity) =
            let culture =
                if String.IsNullOrEmpty(reference.Culture) then
                    Unchecked.defaultof<StringHandle>
                else
                    metadata.GetOrAddString(reference.Culture)

            metadata.AddAssemblyReference(
                metadata.GetOrAddString(reference.Name),
                reference.Version,
                culture,
                reference.PublicKeyToken
                |> immutableBytes
                |> metadata.GetOrAddBlob,
                reference.Flags,
                Unchecked.defaultof<BlobHandle>
            )

        let coreLibrary = addAssemblyReference targetReference

        let assemblyReferences =
            Dictionary<string, AssemblyReferenceHandle>(StringComparer.OrdinalIgnoreCase)

        assemblyReferences.Add(targetReference.Name, coreLibrary)

        let resolveAssemblyReference assemblyName =
            match assemblyReferences.TryGetValue(assemblyName) with
            | true, handle -> handle
            | false, _ ->
                let handle =
                    resolveRequiredReference invocation assemblyName
                    |> addAssemblyReference

                assemblyReferences.Add(assemblyName, handle)
                handle

        let coreTypeReferences = Dictionary<QualifiedTypeName, TypeReferenceHandle>()

        let resolveCoreTypeReference (typeName: QualifiedTypeName) =
            match coreTypeReferences.TryGetValue(typeName) with
            | true, handle -> handle
            | false, _ ->
                let handle =
                    metadata.AddTypeReference(
                        coreLibrary,
                        metadata.GetOrAddString(typeName.Namespace),
                        metadata.GetOrAddString(typeName.Name)
                    )

                coreTypeReferences.Add(typeName, handle)
                handle

        let cliTypeReferences = Dictionary<CliTypeReference, EntityHandle>()
        let mutable localTypeDefinitions: Map<string, EntityHandle> = Map.empty

        let resolveCliTypeReference (typeReference: CliTypeReference) =
            match cliTypeReferences.TryGetValue(typeReference) with
            | true, handle -> handle
            | false, _ ->
                let entityHandle =
                    if String.IsNullOrEmpty(typeReference.AssemblyName) then
                        match
                            localTypeDefinitions
                            |> Map.tryFind typeReference.DeclarationId
                        with
                        | Some handle -> handle
                        | None ->
                            invalidOp (
                                "the local CLI type '"
                                + typeReference.DeclarationId
                                + "' has no symbolic type definition"
                            )
                    else
                        let handle =
                            metadata.AddTypeReference(
                                resolveAssemblyReference typeReference.AssemblyName,
                                metadata.GetOrAddString(typeReference.TypeName.Namespace),
                                metadata.GetOrAddString(typeReference.TypeName.Name)
                            )

                        MetadataTokens.EntityHandle(
                            TableIndex.TypeRef,
                            MetadataTokens.GetRowNumber(handle)
                        )

                cliTypeReferences.Add(typeReference, entityHandle)
                entityHandle

        let cliTypeSpecifications = Dictionary<CliType, EntityHandle>()

        let resolveCliTypeEntity cliType =
            match cliType with
            | CliNamedType typeReference -> resolveCliTypeReference typeReference
            | CliGenericType _
            | CliTypeParameter _
            | CliMethodTypeParameter _ ->
                match cliTypeSpecifications.TryGetValue(cliType) with
                | true, handle -> handle
                | false, _ ->
                    let signature = BlobBuilder()

                    encodeCliType
                        resolveCliTypeReference
                        (BlobEncoder(signature).TypeSpecificationSignature())
                        cliType

                    let handle = metadata.AddTypeSpecification(metadata.GetOrAddBlob(signature))

                    let entityHandle =
                        MetadataTokens.EntityHandle(
                            TableIndex.TypeSpec,
                            MetadataTokens.GetRowNumber(handle)
                        )

                    cliTypeSpecifications.Add(cliType, entityHandle)
                    entityHandle
            | CliInt32
            | CliBoolean
            | CliString
            | CliObject
            | CliNativeInt
            | CliVoid
            | CliByRef _ -> invalidOp "a CLI type entity must be a named or constructed CLI type"

        let resolveDeclaringType =
            function
            | CoreDeclaringType typeName ->
                let handle = resolveCoreTypeReference typeName

                MetadataTokens.EntityHandle(TableIndex.TypeRef, MetadataTokens.GetRowNumber(handle))
            | CliDeclaringType cliType -> resolveCliTypeEntity cliType

        let fsharpCore =
            if List.isEmpty customAttributeFragments then
                Unchecked.defaultof<AssemblyReferenceHandle>
            elif targetReference.Name = "FSharp.Core" then
                coreLibrary
            else
                resolveAssemblyReference "FSharp.Core"

        let systemObject =
            resolveCoreTypeReference {
                Namespace = "System"
                Name = "Object"
            }

        let systemValueType =
            resolveCoreTypeReference {
                Namespace = "System"
                Name = "ValueType"
            }

        let sourceConstructFlags =
            if List.isEmpty customAttributeFragments then
                Unchecked.defaultof<TypeReferenceHandle>
            else
                metadata.AddTypeReference(
                    fsharpCore,
                    metadata.GetOrAddString("Microsoft.FSharp.Core"),
                    metadata.GetOrAddString("SourceConstructFlags")
                )

        let addKnownCustomAttribute
            (parent: EntityHandle)
            (attribute: SymbolicCustomAttributeFragment)
            =
            let attributeTypeName = knownAttributeTypeName attribute.Kind

            let attributeType =
                metadata.AddTypeReference(
                    fsharpCore,
                    metadata.GetOrAddString(attributeTypeName.Namespace),
                    metadata.GetOrAddString(attributeTypeName.Name)
                )

            let constructor =
                metadata.AddMemberReference(
                    attributeType,
                    metadata.GetOrAddString(".ctor"),
                    encodeKnownAttributeConstructorSignature sourceConstructFlags attribute
                    |> metadata.GetOrAddBlob
                )

            metadata.AddCustomAttribute(
                parent,
                constructor,
                attribute
                |> encodeKnownAttributeValue
                |> metadata.GetOrAddBlob
            )
            |> ignore

        let firstField = MetadataTokens.FieldDefinitionHandle(1)
        let firstMethod = MetadataTokens.MethodDefinitionHandle(1)
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

        let assemblyDefinition =
            metadata.AddAssembly(
                metadata.GetOrAddString(symbolic.AssemblyName),
                symbolic.AssemblyVersion,
                Unchecked.defaultof<StringHandle>,
                assemblyPublicKey,
                StrongName.assemblyFlags strongName,
                StrongName.assemblyHashAlgorithm strongName
            )

        for attribute in symbolic.AssemblyAttributes do
            let attributeType =
                metadata.AddTypeReference(
                    coreLibrary,
                    metadata.GetOrAddString(attribute.AttributeType.Namespace),
                    metadata.GetOrAddString(attribute.AttributeType.Name)
                )

            let constructor =
                let signature =
                    encodeStringConstructorSignature attribute.ConstructorArguments.Length
                    |> metadata.GetOrAddBlob

                metadata.AddMemberReference(
                    attributeType,
                    metadata.GetOrAddString(".ctor"),
                    signature
                )

            let value =
                attribute
                |> encodeAssemblyAttributeValue
                |> metadata.GetOrAddBlob

            metadata.AddCustomAttribute(assemblyDefinition, constructor, value)
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

        let mutable nextFieldRow = 1
        let mutable nextMethodRow = 1

        let typeDefinitionHandles =
            typeFragments
            |> List.mapi (fun index typeFragment ->
                typeFragment.StableId, MetadataTokens.TypeDefinitionHandle(index + 2)
            )
            |> Map.ofList

        let typeDefinitionEntities =
            typeFragments
            |> List.mapi (fun index typeFragment ->
                typeFragment.StableId, MetadataTokens.EntityHandle(TableIndex.TypeDef, index + 2)
            )
            |> Map.ofList

        localTypeDefinitions <- typeDefinitionEntities

        let encodedMethods =
            methodFragments
            |> List.map (fun methodFragment ->
                let bodyOffset, codeSize, localSignature, sequencePoints =
                    encodeMethodBody
                        metadata
                        resolveDeclaringType
                        resolveCliTypeReference
                        methodBodies
                        methodFragment

                methodFragment, bodyOffset, codeSize, localSignature, sequencePoints
            )

        for typeFragment in typeFragments do
            let visibility =
                match typeFragment.EnclosingTypeStableId, typeFragment.IsPublic with
                | Some _, true -> TypeAttributes.NestedPublic
                | Some _, false -> TypeAttributes.NestedAssembly
                | None, true -> TypeAttributes.Public
                | None, false -> TypeAttributes.NotPublic

            let attributes =
                match typeFragment.Kind with
                | ModuleContainer ->
                    if List.isEmpty typeFragment.Methods then
                        visibility
                        ||| TypeAttributes.Abstract
                        ||| TypeAttributes.Sealed
                    else
                        visibility
                        ||| TypeAttributes.Abstract
                        ||| TypeAttributes.Sealed
                        ||| TypeAttributes.BeforeFieldInit
                | ExtensionModuleContainer ->
                    visibility
                    ||| TypeAttributes.Abstract
                    ||| TypeAttributes.Sealed
                | StaticMemberContainer ->
                    visibility
                    ||| enum<TypeAttributes> 0x00002000
                | ObjectContainer ->
                    visibility
                    ||| enum<TypeAttributes> 0x00002000
                | StructContainer ->
                    visibility
                    ||| TypeAttributes.SequentialLayout
                    ||| TypeAttributes.Sealed
                    ||| enum<TypeAttributes> 0x00002000
                    ||| TypeAttributes.BeforeFieldInit
                | ClosureContainer ->
                    visibility
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SpecialName
                    ||| enum<TypeAttributes> 0x00002000
                    ||| TypeAttributes.BeforeFieldInit

            let name =
                if List.isEmpty typeFragment.GenericParameters then
                    typeFragment.Name
                else
                    typeFragment.Name
                    + "`"
                    + typeFragment.GenericParameters.Length.ToString(CultureInfo.InvariantCulture)

            let baseType =
                match typeFragment.Kind with
                | StructContainer -> systemValueType
                | ModuleContainer
                | ExtensionModuleContainer
                | StaticMemberContainer
                | ObjectContainer
                | ClosureContainer -> systemObject

            let typeDefinition =
                metadata.AddTypeDefinition(
                    attributes,
                    (if String.IsNullOrEmpty(typeFragment.Namespace) then
                         Unchecked.defaultof<StringHandle>
                     else
                         metadata.GetOrAddString(typeFragment.Namespace)),
                    metadata.GetOrAddString(name),
                    baseType,
                    MetadataTokens.FieldDefinitionHandle(nextFieldRow),
                    MetadataTokens.MethodDefinitionHandle(nextMethodRow)
                )

            if
                typeDefinition
                <> typeDefinitionHandles.[typeFragment.StableId]
            then
                invalidOp "the planned type-definition row does not match the emitted row"

            nextMethodRow <-
                nextMethodRow
                + typeFragment.Methods.Length

            nextFieldRow <-
                nextFieldRow
                + typeFragment.LiteralFields.Length
                + typeFragment.InstanceFields.Length

        for typeFragment in typeFragments do
            match typeFragment.EnclosingTypeStableId with
            | Some enclosingTypeStableId ->
                metadata.AddNestedType(
                    typeDefinitionHandles.[typeFragment.StableId],
                    typeDefinitionHandles.[enclosingTypeStableId]
                )
            | None -> ()

        let genericParameterOwners =
            [
                yield!
                    typeFragments
                    |> List.mapi (fun index typeFragment ->
                        let row = index + 2

                        row * 2,
                        MetadataTokens.EntityHandle(TableIndex.TypeDef, row),
                        typeFragment.GenericParameters,
                        []
                    )

                yield!
                    methodFragments
                    |> List.mapi (fun index methodFragment ->
                        let row = index + 1

                        row * 2
                        + 1,
                        MetadataTokens.EntityHandle(TableIndex.MethodDef, row),
                        methodFragment.GenericParameters,
                        methodFragment.GenericParameterConstraints
                    )
            ]
            |> List.sortBy (fun (codedOwner, _, _, _) -> codedOwner)

        for _, owner, genericParameters, constraints in genericParameterOwners do
            let parameterHandles =
                genericParameters
                |> List.mapi (fun index name ->
                    metadata.AddGenericParameter(
                        owner,
                        GenericParameterAttributes.None,
                        metadata.GetOrAddString(name),
                        index
                    )
                )

            for parameterIndex, constraintType in constraints do
                if
                    parameterIndex < 0
                    || parameterIndex
                       >= parameterHandles.Length
                then
                    invalidOp "a generic constraint has no matching generic parameter"

                metadata.AddGenericParameterConstraint(
                    parameterHandles.[parameterIndex],
                    resolveCliTypeEntity constraintType
                )
                |> ignore

        for typeFragment in typeFragments do
            let parent = typeDefinitionEntities.[typeFragment.StableId]

            for attribute in typeFragment.Attributes do
                addKnownCustomAttribute parent attribute

        let literalFieldSignature = metadata.GetOrAddBlob(stringFieldSignature)

        let fieldDefinitionEntities =
            Dictionary<string, EntityHandle>(StringComparer.Ordinal)

        for typeFragment in typeFragments do
            for fieldFragment in typeFragment.LiteralFields do
                let field =
                    metadata.AddFieldDefinition(
                        FieldAttributes.Assembly
                        ||| FieldAttributes.Static
                        ||| FieldAttributes.Literal
                        ||| FieldAttributes.HasDefault,
                        metadata.GetOrAddString(fieldFragment.Name),
                        literalFieldSignature
                    )

                fieldDefinitionEntities.Add(
                    fieldFragment.StableId,
                    MetadataTokens.EntityHandle(
                        TableIndex.Field,
                        metadata.GetRowCount(TableIndex.Field)
                    )
                )

                metadata.AddConstant(field, fieldFragment.Value)
                |> ignore

            for fieldFragment in typeFragment.InstanceFields do
                let field =
                    metadata.AddFieldDefinition(
                        FieldAttributes.Public,
                        metadata.GetOrAddString(fieldFragment.Name),
                        fieldFragment.Type
                        |> encodeFieldSignature resolveCliTypeReference
                        |> metadata.GetOrAddBlob
                    )

                fieldDefinitionEntities.Add(
                    fieldFragment.StableId,
                    MetadataTokens.EntityHandle(
                        TableIndex.Field,
                        metadata.GetRowCount(TableIndex.Field)
                    )
                )

        for typeFragment in typeFragments do
            for fieldFragment in typeFragment.InstanceFields do
                let parent = fieldDefinitionEntities.[fieldFragment.StableId]

                for attribute in fieldFragment.Attributes do
                    addKnownCustomAttribute parent attribute

        let mutable nextParameterRow = 1

        for methodFragment, bodyOffset, _, _, _ in encodedMethods do
            let attributes = (methodKindEncoding methodFragment.Kind).Attributes

            let methodDefinition =
                metadata.AddMethodDefinition(
                    attributes,
                    MethodImplAttributes.IL
                    ||| MethodImplAttributes.Managed,
                    metadata.GetOrAddString(methodFragment.Name),
                    methodFragment
                    |> encodeMethodSignature resolveCliTypeReference
                    |> metadata.GetOrAddBlob,
                    bodyOffset,
                    MetadataTokens.ParameterHandle(nextParameterRow)
                )

            let parent =
                MetadataTokens.EntityHandle(
                    TableIndex.MethodDef,
                    MetadataTokens.GetRowNumber(methodDefinition)
                )

            for attribute in methodFragment.Attributes do
                addKnownCustomAttribute parent attribute

            methodFragment.Parameters
            |> List.iteri (fun index parameter ->
                let parameterDefinition =
                    metadata.AddParameter(
                        ParameterAttributes.None,
                        metadata.GetOrAddString(parameter.Name),
                        index + 1
                    )

                let parent =
                    MetadataTokens.EntityHandle(
                        TableIndex.Param,
                        MetadataTokens.GetRowNumber(parameterDefinition)
                    )

                for attribute in parameter.Attributes do
                    addKnownCustomAttribute parent attribute

                nextParameterRow <-
                    nextParameterRow
                    + 1
            )

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

        for methodFragment, _, _, localSignature, instructionSequencePoints in encodedMethods do
            let sequencePoints =
                encodeSequencePoints localSignature instructionSequencePoints methodFragment
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

        for methodIndex, (methodFragment, _, methodCodeSize, _, _) in List.indexed encodedMethods do
            let localVariables =
                methodFragment.Locals
                |> List.sortBy _.Index
                |> List.map (fun local ->
                    pdbMetadata.AddLocalVariable(
                        LocalVariableAttributes.None,
                        local.Index,
                        pdbMetadata.GetOrAddString(local.Name)
                    )
                )

            let firstLocalVariable =
                localVariables
                |> List.tryHead
                |> Option.defaultValue Unchecked.defaultof<LocalVariableHandle>

            pdbMetadata.AddLocalScope(
                MetadataTokens.MethodDefinitionHandle(
                    methodIndex
                    + 1
                ),
                importScope,
                firstLocalVariable,
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

        let requestedArtifacts = [
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
