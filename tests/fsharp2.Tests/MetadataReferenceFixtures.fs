namespace FSharp2.Tests

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography
open FSharp2.Compiler

module internal MetadataReferenceFixtures =
    type Fixture = {
        Identity: string
        Snapshot: TargetReferenceSnapshot
    }

    type AttributeCorruption =
        | InvalidProlog
        | Truncated
        | InvalidBoxedTag
        | TruncatedEnum
        | TrailingData

    let private fingerprint (bytes: byte array) =
        bytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let snapshot identity logicalPath (bytes: byte array) = {
        Identity = identity
        Snapshot =
            TargetReferenceSnapshot.Create(
                StableIdentity.create identity,
                logicalPath,
                bytes,
                fingerprint bytes
            )
    }

    let private entity table row = MetadataTokens.EntityHandle(table, row)

    let private serializeWithResources (metadata: MetadataBuilder) (resources: BlobBuilder option) =
        let image = BlobBuilder()

        ManagedPEBuilder(
            PEHeaderBuilder.CreateLibraryHeader(),
            MetadataRootBuilder(metadata),
            BlobBuilder(),
            managedResources =
                (resources
                 |> Option.toObj)
        )
            .Serialize(image)
        |> ignore

        image.ToArray()

    let private serialize metadata = serializeWithResources metadata None

    let private addModule
        (metadata: MetadataBuilder)
        generation
        moduleName
        mvid
        generationId
        baseGenerationId
        =
        metadata.AddModule(
            generation,
            metadata.GetOrAddString(moduleName),
            metadata.GetOrAddGuid(mvid),
            generationId
            |> Option.map metadata.GetOrAddGuid
            |> Option.defaultValue Unchecked.defaultof<_>,
            baseGenerationId
            |> Option.map metadata.GetOrAddGuid
            |> Option.defaultValue Unchecked.defaultof<_>
        )

    let private addAssembly
        (metadata: MetadataBuilder)
        name
        version
        culture
        (publicKey: byte array)
        flags
        hashAlgorithm
        =
        metadata.AddAssembly(
            metadata.GetOrAddString(name),
            version,
            culture
            |> Option.map metadata.GetOrAddString
            |> Option.defaultValue Unchecked.defaultof<_>,
            publicKey
            |> metadata.GetOrAddBlob,
            flags,
            hashAlgorithm
        )

    let private addModuleType (metadata: MetadataBuilder) firstField firstMethod =
        metadata.AddTypeDefinition(
            TypeAttributes.NotPublic,
            Unchecked.defaultof<_>,
            metadata.GetOrAddString("<Module>"),
            Unchecked.defaultof<_>,
            firstField,
            firstMethod
        )

    let private fieldSignature (metadata: MetadataBuilder) (encode: SignatureTypeEncoder -> unit) =
        let value = BlobBuilder()
        encode (BlobEncoder(value).FieldSignature())
        metadata.GetOrAddBlob(value)

    let private methodSignature
        (metadata: MetadataBuilder)
        (convention: SignatureCallingConvention)
        (genericArity: int)
        (isInstance: bool)
        (parameterCount: int)
        (encodeReturn: ReturnTypeEncoder -> unit)
        (encodeParameters: ParametersEncoder -> unit)
        =
        let value = BlobBuilder()

        BlobEncoder(value)
            .MethodSignature(
                convention = convention,
                genericParameterCount = genericArity,
                isInstanceMethod = isInstance
            )
            .Parameters(parameterCount, encodeReturn, encodeParameters)

        metadata.GetOrAddBlob(value)

    let private voidSignature (metadata: MetadataBuilder) =
        methodSignature
            metadata
            SignatureCallingConvention.Default
            0
            true
            0
            (fun (returnType: ReturnTypeEncoder) -> returnType.Void())
            ignore

    let private voidStringSignature (metadata: MetadataBuilder) =
        methodSignature
            metadata
            SignatureCallingConvention.Default
            0
            true
            1
            (fun (returnType: ReturnTypeEncoder) -> returnType.Void())
            (fun parameters -> parameters.AddParameter().Type().String())

    let private emptyAttributeValue (metadata: MetadataBuilder) =
        let value = BlobBuilder()

        BlobEncoder(value)
            .CustomAttributeSignature(
                (fun (_: FixedArgumentsEncoder) -> ()),
                (fun (arguments: CustomAttributeNamedArgumentsEncoder) ->
                    arguments.Count(0)
                    |> ignore
                )
            )

        metadata.GetOrAddBlob(value)

    let identityAssembly () =
        let metadata = MetadataBuilder()
        let firstField = MetadataTokens.FieldDefinitionHandle(1)
        let firstMethod = MetadataTokens.MethodDefinitionHandle(1)

        addModule
            metadata
            7
            "Issue30.Identity.dll"
            (Guid.Parse("11111111-2222-3333-4444-555555555555"))
            (Some(Guid.Parse("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")))
            (Some(Guid.Parse("01234567-89ab-cdef-0123-456789abcdef")))
        |> ignore

        addAssembly
            metadata
            "Issue30.Identity"
            (Version(2, 3, 4, 5))
            (Some "en-US")
            [|
                1uy
                2uy
                3uy
                4uy
            |]
            AssemblyFlags.PublicKey
            AssemblyHashAlgorithm.Sha256
        |> ignore

        metadata.AddAssemblyReference(
            metadata.GetOrAddString("Issue30.Dependency"),
            Version(6, 7, 8, 9),
            metadata.GetOrAddString("fr-FR"),
            metadata.GetOrAddBlob(
                [|
                    10uy
                    11uy
                    12uy
                    13uy
                    14uy
                    15uy
                    16uy
                    17uy
                |]
            ),
            enum<AssemblyFlags> 0,
            metadata.GetOrAddBlob(
                [|
                    18uy
                    19uy
                |]
            )
        )
        |> ignore

        metadata.AddModuleReference(metadata.GetOrAddString("Issue30.Native"))
        |> ignore

        metadata.AddAssemblyFile(
            metadata.GetOrAddString("Issue30.Part.netmodule"),
            metadata.GetOrAddBlob(
                [|
                    20uy
                    21uy
                    22uy
                |]
            ),
            true
        )
        |> ignore

        addModuleType metadata firstField firstMethod
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.Public,
            metadata.GetOrAddString("Issue30"),
            metadata.GetOrAddString("IdentityType"),
            Unchecked.defaultof<_>,
            firstField,
            firstMethod
        )
        |> ignore

        serialize metadata
        |> snapshot "fixture:issue30.ecma.identity" "Issue30.Identity.dll"

    let invalidUtf8StringByteLength =
        System.Text.Encoding.UTF8.GetByteCount("Issue30.Identity.dll")

    let invalidUtf8String () =
        let source = identityAssembly ()

        let bytes =
            source.Snapshot.PeImage
            |> Seq.toArray

        let marker = System.Text.Encoding.UTF8.GetBytes("Issue30.Identity.dll")

        let matches = [
            for offset in
                0 .. bytes.Length
                     - marker.Length do
                if
                    marker
                    |> Array.mapi (fun index value ->
                        bytes.[offset
                               + index] = value
                    )
                    |> Array.forall id
                then
                    yield offset
        ]

        match matches with
        | [ offset ] -> bytes.[offset] <- 0xffuy
        | _ -> failwithf "Expected one module-name byte sequence but found %d." matches.Length

        bytes
        |> snapshot "fixture:issue30.ecma.heap.invalid-utf8" "Issue30.InvalidUtf8.dll"

    let netmodule () =
        let metadata = MetadataBuilder()
        let firstField = MetadataTokens.FieldDefinitionHandle(1)
        let firstMethod = MetadataTokens.MethodDefinitionHandle(1)

        addModule
            metadata
            3
            "Issue30.Part.netmodule"
            (Guid.Parse("99999999-8888-7777-6666-555555555555"))
            None
            None
        |> ignore

        addModuleType metadata firstField firstMethod
        |> ignore

        serialize metadata
        |> snapshot "fixture:issue30.ecma.identity.netmodule" "Issue30.Part.netmodule"

    let private richAttributeSignature
        (metadata: MetadataBuilder)
        (enumType: EntityHandle)
        (systemType: EntityHandle)
        =
        methodSignature
            metadata
            SignatureCallingConvention.Default
            0
            true
            6
            (fun (returnType: ReturnTypeEncoder) -> returnType.Void())
            (fun parameters ->
                parameters.AddParameter().Type().Boolean()
                parameters.AddParameter().Type().String()
                parameters.AddParameter().Type().Type(enumType, true)
                parameters.AddParameter().Type().Type(systemType, false)
                parameters.AddParameter().Type().Object()
                parameters.AddParameter().Type().SZArray().Int32()
            )

    let private richAttributeValue (metadata: MetadataBuilder) =
        let value = BlobBuilder()

        BlobEncoder(value)
            .CustomAttributeSignature(
                (fun (fixedArguments: FixedArgumentsEncoder) ->
                    fixedArguments.AddArgument().Scalar().Constant(true)
                    fixedArguments.AddArgument().Scalar().Constant("issue30-attribute-marker")
                    fixedArguments.AddArgument().Scalar().Constant(uint16 513)

                    fixedArguments
                        .AddArgument()
                        .Scalar()
                        .SystemType("Issue30.Outer+Inner, Issue30.Semantics")

                    fixedArguments
                        .AddArgument()
                        .TaggedScalar(
                            (fun valueType -> valueType.Int32()),
                            (fun scalar -> scalar.Constant(42))
                        )

                    let literals = fixedArguments.AddArgument().Vector().Count(2)
                    literals.AddLiteral().Scalar().Constant(7)
                    literals.AddLiteral().Scalar().Constant(9)
                ),
                (fun (namedArguments: CustomAttributeNamedArgumentsEncoder) ->
                    let arguments = namedArguments.Count(2)

                    arguments.AddArgument(
                        true,
                        (fun argumentType -> argumentType.ScalarType().Int32()),
                        (fun name -> name.Name("Number")),
                        (fun literal -> literal.Scalar().Constant(17))
                    )

                    arguments.AddArgument(
                        false,
                        (fun argumentType -> argumentType.ScalarType().String()),
                        (fun name -> name.Name("Text")),
                        (fun literal -> literal.Scalar().Constant("named-value"))
                    )
                )
            )

        metadata.GetOrAddBlob(value)

    let private semanticImage
        (attributeOverride: byte array option)
        (friendAssemblyIdentity: string)
        =
        let metadata = MetadataBuilder()

        let systemAssembly =
            metadata.AddAssemblyReference(
                metadata.GetOrAddString("System.Runtime"),
                Version(10, 0, 0, 0),
                Unchecked.defaultof<_>,
                Unchecked.defaultof<_>,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<_>
            )

        let systemScope =
            entity TableIndex.AssemblyRef (MetadataTokens.GetRowNumber(systemAssembly))

        let objectType =
            metadata.AddTypeReference(
                systemScope,
                metadata.GetOrAddString("System"),
                metadata.GetOrAddString("Object")
            )

        let attributeType =
            metadata.AddTypeReference(
                systemScope,
                metadata.GetOrAddString("System"),
                metadata.GetOrAddString("Attribute")
            )

        let systemType =
            metadata.AddTypeReference(
                systemScope,
                metadata.GetOrAddString("System"),
                metadata.GetOrAddString("Type")
            )

        let modifierType =
            metadata.AddTypeReference(
                systemScope,
                metadata.GetOrAddString("System.Runtime.CompilerServices"),
                metadata.GetOrAddString("IsReadOnlyAttribute")
            )

        let ivtType =
            metadata.AddTypeReference(
                systemScope,
                metadata.GetOrAddString("System.Runtime.CompilerServices"),
                metadata.GetOrAddString("InternalsVisibleToAttribute")
            )

        let moduleReference =
            metadata.AddModuleReference(metadata.GetOrAddString("Issue30.Native"))

        let genericFieldSignature =
            fieldSignature metadata (fun (fieldType: SignatureTypeEncoder) -> fieldType.Int32())

        let enumFieldSignature =
            fieldSignature metadata (fun (fieldType: SignatureTypeEncoder) -> fieldType.UInt16())

        let genericField =
            metadata.AddFieldDefinition(
                FieldAttributes.Public
                ||| FieldAttributes.HasDefault,
                metadata.GetOrAddString("Number"),
                genericFieldSignature
            )

        metadata.AddConstant(genericField, 42)
        |> ignore

        let enumField =
            metadata.AddFieldDefinition(
                FieldAttributes.Public
                ||| FieldAttributes.SpecialName
                ||| FieldAttributes.RTSpecialName,
                metadata.GetOrAddString("value__"),
                enumFieldSignature
            )

        let parameter =
            metadata.AddParameter(
                ParameterAttributes.HasDefault,
                metadata.GetOrAddString("value"),
                1
            )

        metadata.AddConstant(parameter, 7)
        |> ignore

        let contractSignature =
            methodSignature
                metadata
                SignatureCallingConvention.Default
                0
                true
                1
                (fun (returnType: ReturnTypeEncoder) -> returnType.Void())
                (fun parameters -> parameters.AddParameter().Type().Int32())

        let contractMethod =
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract,
                MethodImplAttributes.IL,
                metadata.GetOrAddString("Invoke"),
                contractSignature,
                0,
                MetadataTokens.ParameterHandle(1)
            )

        let exoticSignature =
            methodSignature
                metadata
                SignatureCallingConvention.VarArgs
                1
                true
                7
                (fun returnType -> returnType.Type().GenericMethodTypeParameter(0))
                (fun parameters ->
                    parameters.AddParameter().Type().GenericTypeParameter(0)
                    parameters.AddParameter().Type(true).Int32()
                    parameters.AddParameter().Type().Pointer().Byte()
                    parameters.AddParameter().Type().SZArray().String()

                    parameters
                        .AddParameter()
                        .Type()
                        .Array(
                            (fun elementType -> elementType.Int32()),
                            (fun shape ->
                                shape.Shape(
                                    2,
                                    ImmutableArray.CreateRange([| 3 |]),
                                    ImmutableArray.CreateRange([| 1 |])
                                )
                            )
                        )

                    let modified = parameters.AddParameter()

                    modified.CustomModifiers().AddModifier(entity TableIndex.TypeRef 4, false)
                    |> ignore

                    modified.Type().Int32()

                    let optionalParameters = parameters.StartVarArgs()

                    let pointer =
                        optionalParameters
                            .AddParameter()
                            .Type()
                            .FunctionPointer(SignatureCallingConvention.StdCall)

                    pointer.Parameters(
                        1,
                        (fun (returnType: ReturnTypeEncoder) -> returnType.Void()),
                        (fun functionParameters ->
                            functionParameters.AddParameter().Type().Int32()
                        )
                    )
                )

        let implementationMethod =
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.Virtual,
                MethodImplAttributes.IL,
                metadata.GetOrAddString("Invoke"),
                exoticSignature,
                0,
                MetadataTokens.ParameterHandle(1)
            )

        let getMethod =
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.SpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString("get_Value"),
                methodSignature
                    metadata
                    SignatureCallingConvention.Default
                    0
                    true
                    0
                    (fun value -> value.Type().Int32())
                    ignore,
                0,
                MetadataTokens.ParameterHandle(2)
            )

        let setMethod =
            metadata.AddMethodDefinition(
                MethodAttributes.Private
                ||| MethodAttributes.SpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString("set_Value"),
                contractSignature,
                0,
                MetadataTokens.ParameterHandle(2)
            )

        let addMethod =
            metadata.AddMethodDefinition(
                MethodAttributes.FamORAssem
                ||| MethodAttributes.SpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString("add_Changed"),
                contractSignature,
                0,
                MetadataTokens.ParameterHandle(2)
            )

        let removeMethod =
            metadata.AddMethodDefinition(
                MethodAttributes.FamANDAssem
                ||| MethodAttributes.SpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString("remove_Changed"),
                contractSignature,
                0,
                MetadataTokens.ParameterHandle(2)
            )

        let richConstructor =
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString(".ctor"),
                richAttributeSignature
                    metadata
                    (entity TableIndex.TypeDef 4)
                    (entity TableIndex.TypeRef 3),
                0,
                MetadataTokens.ParameterHandle(2)
            )

        let emptyConstructor =
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString(".ctor"),
                voidSignature metadata,
                0,
                MetadataTokens.ParameterHandle(2)
            )

        addModule
            metadata
            0
            "Issue30.Semantics.dll"
            (Guid.Parse("30000000-0000-0000-0000-000000000001"))
            None
            None
        |> ignore

        let assembly =
            addAssembly
                metadata
                "Issue30.Semantics"
                (Version(1, 0, 0, 0))
                None
                Array.empty
                (enum<AssemblyFlags> 0)
                AssemblyHashAlgorithm.Sha256

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let contractType =
            metadata.AddTypeDefinition(
                TypeAttributes.Public
                ||| TypeAttributes.Interface
                ||| TypeAttributes.Abstract,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("IContract"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )

        let genericType =
            metadata.AddTypeDefinition(
                TypeAttributes.NotPublic,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("GenericType`1"),
                entity TableIndex.TypeRef 1,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(2)
            )

        metadata.AddTypeDefinition(
            TypeAttributes.Public
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString("Issue30"),
            metadata.GetOrAddString("SmallEnum"),
            entity TableIndex.TypeRef 1,
            MetadataTokens.FieldDefinitionHandle(2),
            MetadataTokens.MethodDefinitionHandle(7)
        )
        |> ignore

        let richAttributeType =
            metadata.AddTypeDefinition(
                TypeAttributes.Public
                ||| TypeAttributes.Sealed,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("RichAttribute"),
                entity TableIndex.TypeRef 2,
                MetadataTokens.FieldDefinitionHandle(3),
                MetadataTokens.MethodDefinitionHandle(7)
            )

        let parent =
            metadata.AddTypeDefinition(
                TypeAttributes.NotPublic,
                metadata.GetOrAddString("Issue30.Colliding"),
                metadata.GetOrAddString("Parent"),
                entity TableIndex.TypeRef 1,
                MetadataTokens.FieldDefinitionHandle(3),
                MetadataTokens.MethodDefinitionHandle(9)
            )

        let nestedAccessibilities = [
            TypeAttributes.NestedPublic
            TypeAttributes.NestedPrivate
            TypeAttributes.NestedFamily
            TypeAttributes.NestedAssembly
            TypeAttributes.NestedFamANDAssem
            TypeAttributes.NestedFamORAssem
        ]

        nestedAccessibilities
        |> List.iteri (fun index visibility ->
            let nested =
                metadata.AddTypeDefinition(
                    visibility,
                    Unchecked.defaultof<_>,
                    metadata.GetOrAddString($"Nested{index}"),
                    entity TableIndex.TypeRef 1,
                    MetadataTokens.FieldDefinitionHandle(3),
                    MetadataTokens.MethodDefinitionHandle(9)
                )

            metadata.AddNestedType(nested, parent)
        )

        let interfaceImplementation =
            metadata.AddInterfaceImplementation(genericType, entity TableIndex.TypeDef 2)

        metadata.AddMethodImplementation(
            genericType,
            entity TableIndex.MethodDef (MetadataTokens.GetRowNumber(implementationMethod)),
            entity TableIndex.MethodDef (MetadataTokens.GetRowNumber(contractMethod))
        )
        |> ignore

        metadata.AddGenericParameter(
            entity TableIndex.MethodDef 2,
            GenericParameterAttributes.NotNullableValueTypeConstraint,
            metadata.GetOrAddString("TMethod"),
            0
        )
        |> ignore

        let genericParameter =
            metadata.AddGenericParameter(
                entity TableIndex.TypeDef 3,
                GenericParameterAttributes.Covariant
                ||| GenericParameterAttributes.ReferenceTypeConstraint
                ||| GenericParameterAttributes.DefaultConstructorConstraint,
                metadata.GetOrAddString("T"),
                0
            )

        metadata.AddGenericParameterConstraint(genericParameter, entity TableIndex.TypeRef 1)
        |> ignore

        let propertySignature =
            let value = BlobBuilder()

            BlobEncoder(value)
                .PropertySignature(true)
                .Parameters(0, (fun result -> result.Type().Int32()), ignore)

            metadata.GetOrAddBlob(value)

        let property =
            metadata.AddProperty(
                PropertyAttributes.HasDefault,
                metadata.GetOrAddString("Value"),
                propertySignature
            )

        metadata.AddConstant(property, 11)
        |> ignore

        metadata.AddPropertyMap(genericType, property)

        metadata.AddMethodSemantics(
            entity TableIndex.Property 1,
            MethodSemanticsAttributes.Getter,
            getMethod
        )

        metadata.AddMethodSemantics(
            entity TableIndex.Property 1,
            MethodSemanticsAttributes.Setter,
            setMethod
        )

        let eventDefinition =
            metadata.AddEvent(
                EventAttributes.SpecialName,
                metadata.GetOrAddString("Changed"),
                entity TableIndex.TypeRef 1
            )

        metadata.AddEventMap(genericType, eventDefinition)

        metadata.AddMethodSemantics(
            entity TableIndex.Event 1,
            MethodSemanticsAttributes.Adder,
            addMethod
        )

        metadata.AddMethodSemantics(
            entity TableIndex.Event 1,
            MethodSemanticsAttributes.Remover,
            removeMethod
        )

        let genericTypeSpec = BlobBuilder()

        let arguments =
            BlobEncoder(genericTypeSpec)
                .TypeSpecificationSignature()
                .GenericInstantiation(entity TableIndex.TypeDef 3, 1, false)

        arguments.AddArgument().Int32()

        let typeSpecification =
            metadata.AddTypeSpecification(metadata.GetOrAddBlob(genericTypeSpec))

        let memberRefParents = [
            entity TableIndex.TypeDef 3
            entity TableIndex.TypeRef 1
            entity TableIndex.ModuleRef (MetadataTokens.GetRowNumber(moduleReference))
            entity TableIndex.MethodDef 2
            entity TableIndex.TypeSpec (MetadataTokens.GetRowNumber(typeSpecification))
        ]

        let memberReferences =
            memberRefParents
            |> List.mapi (fun index memberParent ->
                metadata.AddMemberReference(
                    memberParent,
                    metadata.GetOrAddString($"Referenced{index}"),
                    if index = 2 then
                        genericFieldSignature
                    else
                        contractSignature
                )
            )

        let methodInstantiation = BlobBuilder()
        BlobEncoder(methodInstantiation).MethodSpecificationSignature(1).AddArgument().String()

        metadata.AddMethodSpecification(
            entity TableIndex.MethodDef 2,
            metadata.GetOrAddBlob(methodInstantiation)
        )
        |> ignore

        let locals = BlobBuilder()
        let variables = BlobEncoder(locals).LocalVariableSignature(3)
        variables.AddVariable().Type(false, true).Int32()
        variables.AddVariable().Type(true, false).String()
        variables.AddVariable().Type().Pointer().Byte()

        metadata.AddStandaloneSignature(metadata.GetOrAddBlob(locals))
        |> ignore

        metadata.AddStandaloneSignature(
            metadata.GetOrAddBlob(
                [|
                    0x06uy
                    0x1euy
                    0x00uy
                |]
            )
        )
        |> ignore

        metadata.AddStandaloneSignature(
            metadata.GetOrAddBlob(
                [|
                    0x60uy
                    0x00uy
                    0x01uy
                |]
            )
        )
        |> ignore

        let emptyAttribute = emptyAttributeValue metadata

        let emptyConstructorEntity =
            entity TableIndex.MethodDef (MetadataTokens.GetRowNumber(emptyConstructor))

        [
            entity TableIndex.TypeDef (MetadataTokens.GetRowNumber(genericType))
            entity TableIndex.MethodDef (MetadataTokens.GetRowNumber(implementationMethod))
            entity TableIndex.Field (MetadataTokens.GetRowNumber(genericField))
            entity TableIndex.Param (MetadataTokens.GetRowNumber(parameter))
            entity TableIndex.Property (MetadataTokens.GetRowNumber(property))
            entity TableIndex.Event (MetadataTokens.GetRowNumber(eventDefinition))
            entity TableIndex.InterfaceImpl (MetadataTokens.GetRowNumber(interfaceImplementation))
        ]
        |> List.iter (fun owner ->
            metadata.AddCustomAttribute(owner, emptyConstructorEntity, emptyAttribute)
            |> ignore
        )

        let richValue =
            match attributeOverride with
            | None -> richAttributeValue metadata
            | Some bytes -> metadata.GetOrAddBlob(bytes)

        metadata.AddCustomAttribute(
            assembly,
            entity TableIndex.MethodDef (MetadataTokens.GetRowNumber(richConstructor)),
            richValue
        )
        |> ignore

        let ivtConstructor =
            metadata.AddMemberReference(
                entity TableIndex.TypeRef (MetadataTokens.GetRowNumber(ivtType)),
                metadata.GetOrAddString(".ctor"),
                voidStringSignature metadata
            )

        let ivtValue = BlobBuilder()

        BlobEncoder(ivtValue)
            .CustomAttributeSignature(
                (fun fixedArguments ->
                    fixedArguments.AddArgument().Scalar().Constant(friendAssemblyIdentity)
                ),
                (fun namedArguments ->
                    namedArguments.Count(0)
                    |> ignore
                )
            )

        metadata.AddCustomAttribute(assembly, ivtConstructor, metadata.GetOrAddBlob(ivtValue))
        |> ignore

        metadata, richAttributeType, memberReferences

    let semanticAssembly () =
        let metadata, _, _ = semanticImage None "Issue30.Friend, PublicKey=01020304"

        serialize metadata
        |> snapshot "fixture:issue30.ecma.semantics" "Issue30.Semantics.dll"

    let private enumProvider assemblyName backingSignature =
        let metadata = MetadataBuilder()

        let systemAssembly =
            metadata.AddAssemblyReference(
                metadata.GetOrAddString("System.Runtime"),
                Version(10, 0, 0, 0),
                Unchecked.defaultof<_>,
                Unchecked.defaultof<_>,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<_>
            )

        let enumType =
            metadata.AddTypeReference(
                entity TableIndex.AssemblyRef (MetadataTokens.GetRowNumber(systemAssembly)),
                metadata.GetOrAddString("System"),
                metadata.GetOrAddString("Enum")
            )

        let backingField =
            metadata.AddFieldDefinition(
                FieldAttributes.Public
                ||| FieldAttributes.SpecialName
                ||| FieldAttributes.RTSpecialName,
                metadata.GetOrAddString("value__"),
                backingSignature metadata
            )

        addModule metadata 0 $"{assemblyName}.dll" (Guid.NewGuid()) None None
        |> ignore

        addAssembly
            metadata
            assemblyName
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType metadata backingField (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let parent =
            metadata.AddTypeDefinition(
                TypeAttributes.Public,
                metadata.GetOrAddString("Issue30.External"),
                metadata.GetOrAddString("Container"),
                Unchecked.defaultof<_>,
                backingField,
                MetadataTokens.MethodDefinitionHandle(1)
            )

        let nested =
            metadata.AddTypeDefinition(
                TypeAttributes.NestedPublic
                ||| TypeAttributes.Sealed,
                Unchecked.defaultof<_>,
                metadata.GetOrAddString("Choice"),
                entity TableIndex.TypeRef (MetadataTokens.GetRowNumber(enumType)),
                backingField,
                MetadataTokens.MethodDefinitionHandle(1)
            )

        metadata.AddNestedType(nested, parent)

        serialize metadata
        |> snapshot $"fixture:issue30.ecma.enum.{assemblyName}" $"{assemblyName}.dll"

    let private externalEnumProvider assemblyName useInt32 =
        enumProvider
            assemblyName
            (fun metadata ->
                fieldSignature
                    metadata
                    (fun fieldType -> if useInt32 then fieldType.Int32() else fieldType.UInt16())
            )

    let unusedMalformedEnum () =
        enumProvider
            "Issue30.UnusedMalformedEnum"
            (fun metadata -> metadata.GetOrAddBlob([| 0xffuy |]))

    let private externalEnumConsumer () =
        let metadata = MetadataBuilder()

        let providerAssembly =
            metadata.AddAssemblyReference(
                metadata.GetOrAddString("Issue30.ExternalEnum.Provider"),
                Version(1, 0, 0, 0),
                Unchecked.defaultof<_>,
                Unchecked.defaultof<_>,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<_>
            )

        let parentType =
            metadata.AddTypeReference(
                entity TableIndex.AssemblyRef (MetadataTokens.GetRowNumber(providerAssembly)),
                metadata.GetOrAddString("Issue30.External"),
                metadata.GetOrAddString("Container")
            )

        let enumType =
            metadata.AddTypeReference(
                entity TableIndex.TypeRef (MetadataTokens.GetRowNumber(parentType)),
                Unchecked.defaultof<_>,
                metadata.GetOrAddString("Choice")
            )

        let constructor =
            metadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.RTSpecialName,
                MethodImplAttributes.IL,
                metadata.GetOrAddString(".ctor"),
                methodSignature
                    metadata
                    SignatureCallingConvention.Default
                    0
                    true
                    1
                    (fun returnType -> returnType.Void())
                    (fun parameters ->
                        parameters
                            .AddParameter()
                            .Type()
                            .Type(
                                entity TableIndex.TypeRef (MetadataTokens.GetRowNumber(enumType)),
                                true
                            )
                    ),
                0,
                MetadataTokens.ParameterHandle(1)
            )

        addModule
            metadata
            0
            "Issue30.ExternalEnum.Consumer.dll"
            (Guid.Parse("e0000000-0000-0000-0000-000000000003"))
            None
            None
        |> ignore

        let assembly =
            addAssembly
                metadata
                "Issue30.ExternalEnum.Consumer"
                (Version(1, 0, 0, 0))
                None
                Array.empty
                (enum<AssemblyFlags> 0)
                AssemblyHashAlgorithm.Sha256

        addModuleType metadata (MetadataTokens.FieldDefinitionHandle(1)) constructor
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.Public
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString("Issue30.External"),
            metadata.GetOrAddString("ExternalEnumAttribute"),
            Unchecked.defaultof<_>,
            MetadataTokens.FieldDefinitionHandle(1),
            constructor
        )
        |> ignore

        let value = BlobBuilder()

        BlobEncoder(value)
            .CustomAttributeSignature(
                (fun fixedArguments -> fixedArguments.AddArgument().Scalar().Constant(uint16 513)),
                (fun namedArguments ->
                    namedArguments.Count(0)
                    |> ignore
                )
            )

        metadata.AddCustomAttribute(assembly, constructor, metadata.GetOrAddBlob(value))
        |> ignore

        serialize metadata
        |> snapshot "fixture:issue30.ecma.enum.consumer" "Issue30.ExternalEnum.Consumer.dll"

    let externalEnumUniverse () =
        externalEnumConsumer (),
        externalEnumProvider "Issue30.ExternalEnum.Decoy" true,
        externalEnumProvider "Issue30.ExternalEnum.Provider" false

    let invalidAttribute corruption =
        let bytes =
            match corruption with
            | InvalidProlog -> [|
                0uy
                0uy
              |]
            | Truncated -> [|
                1uy
                0uy
                1uy
              |]
            | InvalidBoxedTag -> [|
                1uy
                0uy
                1uy
                6uy
                0uy
                0uy
                0uy
                0uy
                0xffuy
              |]
            | TruncatedEnum -> [|
                1uy
                0uy
                1uy
                1uy
                2uy
              |]
            | TrailingData -> [|
                1uy
                0uy
                1uy
                6uy
                0uy
                0uy
                0uy
                0uy
                0xffuy
                0uy
              |]

        let metadata, _, _ = semanticImage (Some bytes) "Issue30.Friend, PublicKey=01020304"

        serialize metadata
        |> snapshot
            $"fixture:issue30.ecma.attributes.{corruption}"
            $"Issue30.Attributes.{corruption}.dll"

    let malformedFriendAssembly identity =
        let metadata, _, _ = semanticImage None identity
        let identityBytes = System.Text.Encoding.UTF8.GetBytes(identity)

        serialize metadata
        |> snapshot
            $"fixture:issue30.ecma.attributes.friend-{fingerprint identityBytes}"
            "Issue30.BadFriend.dll"

    let malformedSignature depth =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.Signature.dll"
            (Guid.Parse("30000000-0000-0000-0000-000000000010"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.Signature"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let signature = Array.append (Array.create depth 0x1duy) [| 0x08uy |]

        metadata.AddTypeSpecification(metadata.GetOrAddBlob(signature))
        |> ignore

        serialize metadata
        |> snapshot $"fixture:issue30.ecma.signature.depth-{depth}" "Issue30.Signature.dll"

    let invalidStandaloneSignature () =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.BadSignature.dll"
            (Guid.Parse("30000000-0000-0000-0000-000000000011"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.BadSignature"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        metadata.AddStandaloneSignature(metadata.GetOrAddBlob([| 0xffuy |]))
        |> ignore

        serialize metadata
        |> snapshot "fixture:issue30.ecma.signature.invalid-standalone" "Issue30.BadSignature.dll"

    let manyTypes count =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.ManyRows.dll"
            (Guid.Parse("30000000-0000-0000-0000-000000000012"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.ManyRows"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        for index in 1..count do
            metadata.AddTypeDefinition(
                TypeAttributes.Public,
                metadata.GetOrAddString("Issue30.Rows"),
                metadata.GetOrAddString($"Type{index}"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )
            |> ignore

        serialize metadata
        |> snapshot $"fixture:issue30.ecma.rows-{count}" "Issue30.ManyRows.dll"

    let duplicateProperty () =
        let metadata = MetadataBuilder()

        let signature =
            let value = BlobBuilder()

            BlobEncoder(value)
                .PropertySignature(true)
                .Parameters(0, (fun result -> result.Type().Int32()), ignore)

            metadata.GetOrAddBlob(value)

        let first =
            metadata.AddProperty(
                enum<PropertyAttributes> 0,
                metadata.GetOrAddString("Value"),
                signature
            )

        metadata.AddProperty(
            enum<PropertyAttributes> 0,
            metadata.GetOrAddString("Value"),
            signature
        )
        |> ignore

        addModule
            metadata
            0
            "Issue30.DuplicateProperty.dll"
            (Guid.Parse("d0000000-0000-0000-0000-000000000001"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.DuplicateProperty"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let owner =
            metadata.AddTypeDefinition(
                TypeAttributes.Public,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("DuplicateOwner"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )

        metadata.AddPropertyMap(owner, first)

        serialize metadata
        |> snapshot "fixture:issue30.ecma.duplicate.property" "Issue30.DuplicateProperty.dll"

    let duplicateEvent () =
        let metadata = MetadataBuilder()

        let systemAssembly =
            metadata.AddAssemblyReference(
                metadata.GetOrAddString("System.Runtime"),
                Version(10, 0, 0, 0),
                Unchecked.defaultof<_>,
                Unchecked.defaultof<_>,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<_>
            )

        let objectType =
            metadata.AddTypeReference(
                entity TableIndex.AssemblyRef (MetadataTokens.GetRowNumber(systemAssembly)),
                metadata.GetOrAddString("System"),
                metadata.GetOrAddString("Object")
            )

        let first =
            metadata.AddEvent(
                enum<EventAttributes> 0,
                metadata.GetOrAddString("Changed"),
                entity TableIndex.TypeRef (MetadataTokens.GetRowNumber(objectType))
            )

        metadata.AddEvent(
            enum<EventAttributes> 0,
            metadata.GetOrAddString("Changed"),
            entity TableIndex.TypeRef (MetadataTokens.GetRowNumber(objectType))
        )
        |> ignore

        addModule
            metadata
            0
            "Issue30.DuplicateEvent.dll"
            (Guid.Parse("d0000000-0000-0000-0000-000000000002"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.DuplicateEvent"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let owner =
            metadata.AddTypeDefinition(
                TypeAttributes.Public,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("DuplicateOwner"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )

        metadata.AddEventMap(owner, first)

        serialize metadata
        |> snapshot "fixture:issue30.ecma.duplicate.event" "Issue30.DuplicateEvent.dll"

    let private forwarderTarget () =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.ForwarderTarget.dll"
            (Guid.Parse("f0000000-0000-0000-0000-000000000001"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.ForwarderTarget"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let parent =
            metadata.AddTypeDefinition(
                TypeAttributes.Public,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("Forwarded"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )

        let nested =
            metadata.AddTypeDefinition(
                TypeAttributes.NestedPublic,
                Unchecked.defaultof<_>,
                metadata.GetOrAddString("Nested"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )

        metadata.AddNestedType(nested, parent)

        serialize metadata
        |> snapshot "fixture:issue30.ecma.forwarders.target" "Issue30.ForwarderTarget.dll"

    let private assemblyFacade name targetName duplicate =
        let metadata = MetadataBuilder()

        addModule metadata 0 $"{name}.dll" (Guid.NewGuid()) None None
        |> ignore

        addAssembly
            metadata
            name
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        let targetReference =
            metadata.AddAssemblyReference(
                metadata.GetOrAddString(targetName),
                Version(1, 0, 0, 0),
                Unchecked.defaultof<_>,
                Unchecked.defaultof<_>,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<_>
            )

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let forwarder = enum<TypeAttributes> 0x00200000

        let parent =
            metadata.AddExportedType(
                TypeAttributes.Public
                ||| forwarder,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("Forwarded"),
                entity TableIndex.AssemblyRef (MetadataTokens.GetRowNumber(targetReference)),
                0
            )

        metadata.AddExportedType(
            TypeAttributes.NestedPublic
            ||| forwarder,
            Unchecked.defaultof<_>,
            metadata.GetOrAddString("Nested"),
            entity TableIndex.ExportedType (MetadataTokens.GetRowNumber(parent)),
            0
        )
        |> ignore

        if duplicate then
            metadata.AddExportedType(
                TypeAttributes.Public
                ||| forwarder,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("Forwarded"),
                entity TableIndex.AssemblyRef (MetadataTokens.GetRowNumber(targetReference)),
                0
            )
            |> ignore

        serialize metadata
        |> snapshot $"fixture:issue30.ecma.forwarders.{name}" $"{name}.dll"

    let forwarderPair () =
        assemblyFacade "Issue30.ForwarderFacade" "Issue30.ForwarderTarget" false, forwarderTarget ()

    let forwarderMultiHop () =
        let first = assemblyFacade "Issue30.ForwarderFirst" "Issue30.ForwarderMiddle" false

        let middle =
            assemblyFacade "Issue30.ForwarderMiddle" "Issue30.ForwarderTarget" false

        first, middle, forwarderTarget ()

    let forwarderCycle () =
        assemblyFacade "Issue30.ForwarderCycleA" "Issue30.ForwarderCycleB" false,
        assemblyFacade "Issue30.ForwarderCycleB" "Issue30.ForwarderCycleA" false

    let duplicateForwarder () =
        assemblyFacade "Issue30.ForwarderDuplicate" "Issue30.ForwarderTarget" true

    let incompatibleForwarder () =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.EmptyTarget.dll"
            (Guid.Parse("f0000000-0000-0000-0000-000000000009"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.ForwarderTarget"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        let emptyTarget =
            serialize metadata
            |> snapshot
                "fixture:issue30.ecma.forwarders.incompatible-target"
                "Issue30.EmptyTarget.dll"

        assemblyFacade "Issue30.IncompatibleFacade" "Issue30.ForwarderTarget" false, emptyTarget

    let private fileForwarderWithHashAlgorithm hashAlgorithm =
        let targetModule identity mvid =
            let metadata = MetadataBuilder()

            addModule metadata 0 "Issue30.Part.netmodule" mvid None None
            |> ignore

            addModuleType
                metadata
                (MetadataTokens.FieldDefinitionHandle(1))
                (MetadataTokens.MethodDefinitionHandle(1))
            |> ignore

            metadata.AddTypeDefinition(
                TypeAttributes.Public,
                metadata.GetOrAddString("Issue30"),
                metadata.GetOrAddString("MissingFromNetmodule"),
                Unchecked.defaultof<_>,
                MetadataTokens.FieldDefinitionHandle(1),
                MetadataTokens.MethodDefinitionHandle(1)
            )
            |> ignore

            serialize metadata
            |> snapshot identity "Issue30.Part.netmodule"

        let target =
            targetModule
                "fixture:issue30.ecma.forwarders.file-target"
                (Guid.Parse("f0000000-0000-0000-0000-000000000011"))

        let mismatch =
            targetModule
                "fixture:issue30.ecma.forwarders.file-mismatch"
                (Guid.Parse("f0000000-0000-0000-0000-000000000012"))

        let targetBytes =
            target.Snapshot.PeImage
            |> Seq.toArray

        let targetHash = SHA256.HashData(targetBytes)
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.FileFacade.dll"
            (Guid.Parse("f0000000-0000-0000-0000-000000000010"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.FileFacade"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            hashAlgorithm
        |> ignore

        let file =
            metadata.AddAssemblyFile(
                metadata.GetOrAddString("Issue30.Part.netmodule"),
                metadata.GetOrAddBlob(targetHash),
                true
            )

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        metadata.AddExportedType(
            TypeAttributes.Public
            ||| enum<TypeAttributes> 0x00200000,
            metadata.GetOrAddString("Issue30"),
            metadata.GetOrAddString("MissingFromNetmodule"),
            entity TableIndex.File (MetadataTokens.GetRowNumber(file)),
            0
        )
        |> ignore

        serialize metadata
        |> snapshot "fixture:issue30.ecma.forwarders.file" "Issue30.FileFacade.dll",
        target,
        mismatch

    let fileForwarder () =
        fileForwarderWithHashAlgorithm AssemblyHashAlgorithm.Sha256

    let unsupportedFileHashAlgorithmForwarder () =
        fileForwarderWithHashAlgorithm (enum<AssemblyHashAlgorithm> 0x800D)

    let malformedImage () =
        [|
            0x49uy
            0x33uy
            0x30uy
        |]
        |> snapshot "fixture:issue30.ecma.malformed.invalid-pe" "invalid-pe.dll"

    let nativeImage () =
        let source = identityAssembly ()

        let bytes =
            source.Snapshot.PeImage
            |> Seq.toArray

        use pe = new PEReader(source.Snapshot.PeImage)

        let dataDirectoryStart =
            match pe.PEHeaders.PEHeader.Magic with
            | PEMagic.PE32 ->
                pe.PEHeaders.PEHeaderStartOffset
                + 96
            | PEMagic.PE32Plus ->
                pe.PEHeaders.PEHeaderStartOffset
                + 112
            | magic -> failwithf "Unexpected PE magic '%A'." magic

        let corHeaderDirectory =
            dataDirectoryStart
            + (14 * 8)

        Array.Clear(bytes, corHeaderDirectory, 8)

        bytes
        |> snapshot "fixture:issue30.ecma.malformed.no-cli" "no-cli.dll"

    let private replaceMetadataSignature replacement =
        let source = identityAssembly ()

        let bytes =
            source.Snapshot.PeImage
            |> Seq.toArray

        use pe = new PEReader(source.Snapshot.PeImage)
        bytes.[pe.PEHeaders.MetadataStartOffset] <- replacement
        bytes

    let corruptMetadataRoot () =
        replaceMetadataSignature 0uy
        |> snapshot "fixture:issue30.ecma.malformed.metadata-root" "bad-metadata-root.dll"

    let corruptStreamDirectory () =
        replaceMetadataSignature 1uy
        |> snapshot "fixture:issue30.ecma.malformed.stream-directory" "bad-stream-directory.dll"

    let invalidHeapIndex () =
        let source = identityAssembly ()

        let bytes =
            source.Snapshot.PeImage
            |> Seq.toArray

        use pe = new PEReader(source.Snapshot.PeImage)
        let metadata = pe.GetMetadataReader()

        let typeNameOffset =
            pe.PEHeaders.MetadataStartOffset
            + metadata.GetTableMetadataOffset(TableIndex.TypeDef)
            + metadata.GetTableRowSize(TableIndex.TypeDef)
            + 4

        bytes.[typeNameOffset] <- 0xffuy

        bytes.[typeNameOffset
               + 1] <- 0xffuy

        bytes
        |> snapshot "fixture:issue30.ecma.malformed.heap-index" "Issue30.BadHeap.dll"

    let invalidCodedIndex () =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.BadCodedIndex.dll"
            (Guid.Parse("30000000-0000-0000-0000-000000000021"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.BadCodedIndex"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        metadata.AddTypeReference(
            entity TableIndex.AssemblyRef 99,
            metadata.GetOrAddString("Issue30"),
            metadata.GetOrAddString("Missing")
        )
        |> ignore

        serialize metadata
        |> snapshot "fixture:issue30.ecma.malformed.coded-index" "Issue30.BadCodedIndex.dll"

    let invalidResource declaredLength =
        let metadata = MetadataBuilder()

        addModule
            metadata
            0
            "Issue30.BadResource.dll"
            (Guid.Parse("30000000-0000-0000-0000-000000000022"))
            None
            None
        |> ignore

        addAssembly
            metadata
            "Issue30.BadResource"
            (Version(1, 0, 0, 0))
            None
            Array.empty
            (enum<AssemblyFlags> 0)
            AssemblyHashAlgorithm.Sha256
        |> ignore

        addModuleType
            metadata
            (MetadataTokens.FieldDefinitionHandle(1))
            (MetadataTokens.MethodDefinitionHandle(1))
        |> ignore

        metadata.AddManifestResource(
            ManifestResourceAttributes.Public,
            metadata.GetOrAddString("Issue30.Resource"),
            Unchecked.defaultof<_>,
            0u
        )
        |> ignore

        let resources = BlobBuilder()
        resources.WriteUInt32(uint32 declaredLength)
        resources.WriteByte(1uy)

        serializeWithResources metadata (Some resources)
        |> snapshot
            $"fixture:issue30.ecma.malformed.resource-{declaredLength}"
            "Issue30.BadResource.dll"

    let request fixture = {
        Limits = ReferenceImportLimits.Production
        CancellationToken = Threading.CancellationToken.None
        Snapshots = [ fixture.Snapshot ]
    }

    let requestWithLimits limits fixture = {
        Limits = limits
        CancellationToken = Threading.CancellationToken.None
        Snapshots = [ fixture.Snapshot ]
    }

    let requestWithCancellation cancellationToken fixture = {
        Limits = ReferenceImportLimits.Production
        CancellationToken = cancellationToken
        Snapshots = [ fixture.Snapshot ]
    }

    let requestMany fixtures = {
        Limits = ReferenceImportLimits.Production
        CancellationToken = Threading.CancellationToken.None
        Snapshots =
            fixtures
            |> List.map _.Snapshot
    }

    let requestManyWithLimits limits fixtures = {
        Limits = limits
        CancellationToken = Threading.CancellationToken.None
        Snapshots =
            fixtures
            |> List.map _.Snapshot
    }
