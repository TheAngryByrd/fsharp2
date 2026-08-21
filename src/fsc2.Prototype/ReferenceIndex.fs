namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography

module private ReferenceFingerprint =
    let text (value: string) =
        value
        |> Text.Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun hash -> hash.ToLowerInvariant()

module internal ReferenceTypeName =
    let fullName (typeName: QualifiedTypeName) =
        if String.IsNullOrEmpty(typeName.Namespace) then
            typeName.Name
        else
            typeName.Namespace
            + "."
            + typeName.Name

    let private arityKey (name: string) genericArity = {
        Name = name
        GenericArity = genericArity
    }

    let simpleKey (name: string) (genericArity: int) = arityKey name genericArity

    let typeKey (typeName: QualifiedTypeName) genericArity =
        arityKey (fullName typeName) genericArity

    let parseMetadataName (metadataName: string) =
        let marker = metadataName.LastIndexOf('`')

        if marker > 0 then
            match Int32.TryParse(metadataName.Substring(marker + 1)) with
            | true, arity -> metadataName.Substring(0, marker), arity
            | false, _ -> metadataName, 0
        else
            metadataName, 0

type internal ReferenceMethodDefinition = {
    DeclaringType: CliTypeReference
    StableId: string
    Name: string
    IsStatic: bool
    GenericArity: int
    MetadataParameterNames: string list
    ParameterTypes: CliType list
    OptionalParameterCount: int
    ParamArrayElementType: CliType option
    ReturnType: CliType
} with

    override _.ToString() = "ReferenceMethodDefinition"

type internal ReferenceFieldDefinition = {
    DeclaringType: CliTypeReference
    StableId: string
    Name: string
    IsStatic: bool
    FieldType: CliType
} with

    override _.ToString() = "ReferenceFieldDefinition"

type private ReferenceTypeLocation = {
    ReferencePath: string
    TypeRow: int
    DeclaringType: CliTypeReference
} with

    override _.ToString() = "ReferenceTypeLocation"

module private ReferenceMethodKey =
    let create declarationId name isStatic =
        String.concat "\u001f" [
            declarationId
            name
            (if isStatic then "static" else "instance")
        ]

module private ReferenceFieldKey =
    let create declarationId name isStatic =
        String.concat "\u001f" [
            declarationId
            name
            (if isStatic then "static" else "instance")
        ]

type private ReferenceSignatureTypeProvider(types: Dictionary<TypeNameArity, ResolvedTypeName>) as this
    =
    let tryResolve namespaceName metadataName =
        let name, genericArity = ReferenceTypeName.parseMetadataName metadataName

        let key =
            ReferenceTypeName.typeKey
                {
                    Namespace = namespaceName
                    Name = name
                }
                genericArity

        match types.TryGetValue(key) with
        | true, resolved ->
            Some {
                DeclarationId = resolved.DeclarationId
                AssemblyName = resolved.AssemblyName
                TypeName = resolved.TypeName
                IsValueType = resolved.IsValueType
            }
        | false, _ -> None

    interface ISignatureTypeProvider<CliType option, unit> with
        member _.GetArrayType(_, _) = None

        member _.GetByReferenceType(elementType) =
            elementType
            |> Option.bind (
                function
                | CliVoid -> None
                | cliType -> Some(CliByRef cliType)
            )

        member _.GetFunctionPointerType(_) = None

        member _.GetGenericInstantiation(genericType, typeArguments) =
            match genericType with
            | Some(CliNamedType typeReference) when
                typeArguments
                |> Seq.forall Option.isSome
                ->
                let arguments =
                    typeArguments
                    |> Seq.choose id
                    |> List.ofSeq

                let genericReference = {
                    typeReference with
                        TypeName = {
                            typeReference.TypeName with
                                Name =
                                    typeReference.TypeName.Name
                                    + "`"
                                    + arguments.Length.ToString()
                        }
                }

                Some(CliGenericType(genericReference, arguments))
            | _ -> None

        member _.GetGenericMethodParameter(_, index) = Some(CliMethodTypeParameter index)

        member _.GetGenericTypeParameter(_, index) = Some(CliTypeParameter index)

        member _.GetModifiedType(_, unmodifiedType, _) = unmodifiedType

        member _.GetPinnedType(elementType) = elementType

        member _.GetPointerType(_) = None

        member _.GetPrimitiveType(code) =
            match code with
            | PrimitiveTypeCode.Boolean -> Some CliBoolean
            | PrimitiveTypeCode.Int32 -> Some CliInt32
            | PrimitiveTypeCode.IntPtr -> Some CliNativeInt
            | PrimitiveTypeCode.Object -> Some CliObject
            | PrimitiveTypeCode.String -> Some CliString
            | PrimitiveTypeCode.Void -> Some CliVoid
            | _ -> None

        member _.GetSZArrayType(elementType) =
            elementType
            |> Option.map CliArray

        member _.GetTypeFromDefinition(metadata, handle, _) =
            let definition = metadata.GetTypeDefinition(handle)

            tryResolve
                (metadata.GetString(definition.Namespace))
                (metadata.GetString(definition.Name))
            |> Option.map CliNamedType

        member _.GetTypeFromReference(metadata, handle, _) =
            let reference = metadata.GetTypeReference(handle)

            tryResolve
                (metadata.GetString(reference.Namespace))
                (metadata.GetString(reference.Name))
            |> Option.map CliNamedType

        member _.GetTypeFromSpecification(metadata, context, handle, _) =
            metadata.GetTypeSpecification(handle).DecodeSignature(this, context)

/// A metadata-only index of the evaluated target references. The index owns
/// name lookup and a content-addressed identity; it never loads target code.
type internal ReferenceTypeIndex
    private
    (
        fingerprint: string,
        types: Dictionary<TypeNameArity, ResolvedTypeName>,
        typesBySimpleName: Dictionary<TypeNameArity, ResolvedTypeName list>,
        fsharpDelegateDeclarationIds: HashSet<string>,
        typeLocations: Dictionary<string, ReferenceTypeLocation list>
    ) =
    let methodCache = Dictionary<string, ReferenceMethodDefinition list>()
    let fieldCache = Dictionary<string, ReferenceFieldDefinition list>()
    let baseTypeCache = Dictionary<string, CliType option>()
    let interfaceCache = Dictionary<string, CliType list>()

    let loadBaseType declarationId =
        let signatureProvider = ReferenceSignatureTypeProvider(types)

        let provider = signatureProvider :> ISignatureTypeProvider<CliType option, unit>

        let locations =
            match typeLocations.TryGetValue(declarationId) with
            | true, candidates -> candidates
            | false, _ -> []

        locations
        |> List.tryPick (fun location ->
            use metadataStream = File.OpenRead(location.ReferencePath)
            use pe = new PEReader(metadataStream)
            let metadata = pe.GetMetadataReader()

            let typeDefinition =
                location.TypeRow
                |> MetadataTokens.TypeDefinitionHandle
                |> metadata.GetTypeDefinition

            let handle = typeDefinition.BaseType

            if handle.IsNil then
                None
            else
                match handle.Kind with
                | HandleKind.TypeDefinition ->
                    provider.GetTypeFromDefinition(
                        metadata,
                        TypeDefinitionHandle.op_Explicit handle,
                        0uy
                    )
                | HandleKind.TypeReference ->
                    provider.GetTypeFromReference(
                        metadata,
                        TypeReferenceHandle.op_Explicit handle,
                        0uy
                    )
                | HandleKind.TypeSpecification ->
                    provider.GetTypeFromSpecification(
                        metadata,
                        (),
                        TypeSpecificationHandle.op_Explicit handle,
                        0uy
                    )
                | _ -> None
        )

    let loadInterfaces declarationId =
        let signatureProvider = ReferenceSignatureTypeProvider(types)

        let provider = signatureProvider :> ISignatureTypeProvider<CliType option, unit>

        let locations =
            match typeLocations.TryGetValue(declarationId) with
            | true, candidates -> candidates
            | false, _ -> []

        locations
        |> List.collect (fun location ->
            use metadataStream = File.OpenRead(location.ReferencePath)
            use pe = new PEReader(metadataStream)
            let metadata = pe.GetMetadataReader()

            let typeDefinition =
                location.TypeRow
                |> MetadataTokens.TypeDefinitionHandle
                |> metadata.GetTypeDefinition

            typeDefinition.GetInterfaceImplementations()
            |> Seq.choose (fun implementationHandle ->
                let handle = metadata.GetInterfaceImplementation(implementationHandle).Interface

                match handle.Kind with
                | HandleKind.TypeDefinition ->
                    provider.GetTypeFromDefinition(
                        metadata,
                        TypeDefinitionHandle.op_Explicit handle,
                        0uy
                    )
                | HandleKind.TypeReference ->
                    provider.GetTypeFromReference(
                        metadata,
                        TypeReferenceHandle.op_Explicit handle,
                        0uy
                    )
                | HandleKind.TypeSpecification ->
                    provider.GetTypeFromSpecification(
                        metadata,
                        (),
                        TypeSpecificationHandle.op_Explicit handle,
                        0uy
                    )
                | _ -> None
            )
            |> List.ofSeq
        )
        |> List.distinct

    let loadMethods declarationId name isStatic =
        let signatureProvider = ReferenceSignatureTypeProvider(types)

        let locations =
            match typeLocations.TryGetValue(declarationId) with
            | true, candidates -> candidates
            | false, _ -> []

        locations
        |> List.collect (fun location ->
            use metadataStream = File.OpenRead(location.ReferencePath)
            use pe = new PEReader(metadataStream)
            let metadata = pe.GetMetadataReader()

            let entityTypeName (handle: EntityHandle) =
                match handle.Kind with
                | HandleKind.TypeReference ->
                    let reference =
                        metadata.GetTypeReference(TypeReferenceHandle.op_Explicit handle)

                    Some(
                        metadata.GetString(reference.Namespace),
                        metadata.GetString(reference.Name)
                    )
                | HandleKind.TypeDefinition ->
                    let definition =
                        metadata.GetTypeDefinition(TypeDefinitionHandle.op_Explicit handle)

                    Some(
                        metadata.GetString(definition.Namespace),
                        metadata.GetString(definition.Name)
                    )
                | _ -> None

            let customAttributeTypeName (attribute: CustomAttribute) =
                match attribute.Constructor.Kind with
                | HandleKind.MemberReference ->
                    let constructor =
                        metadata.GetMemberReference(
                            MemberReferenceHandle.op_Explicit attribute.Constructor
                        )

                    entityTypeName constructor.Parent
                | HandleKind.MethodDefinition ->
                    let constructor =
                        metadata.GetMethodDefinition(
                            MethodDefinitionHandle.op_Explicit attribute.Constructor
                        )

                    let declaringType = metadata.GetTypeDefinition(constructor.GetDeclaringType())

                    Some(
                        metadata.GetString(declaringType.Namespace),
                        metadata.GetString(declaringType.Name)
                    )
                | _ -> None

            let hasCustomAttribute namespaceName typeName (parameter: Parameter) =
                parameter.GetCustomAttributes()
                |> Seq.exists (fun handle ->
                    handle
                    |> metadata.GetCustomAttribute
                    |> customAttributeTypeName
                    |> Option.exists (fun (attributeNamespace, attributeTypeName) ->
                        attributeNamespace = namespaceName
                        && attributeTypeName = typeName
                    )
                )

            let isFSharpOptionalParameter =
                hasCustomAttribute "Microsoft.FSharp.Core" "OptionalArgumentAttribute"

            let isParamArrayParameter = hasCustomAttribute "System" "ParamArrayAttribute"

            let typeDefinition =
                location.TypeRow
                |> MetadataTokens.TypeDefinitionHandle
                |> metadata.GetTypeDefinition

            typeDefinition.GetMethods()
            |> Seq.choose (fun methodHandle ->
                let methodDefinition = metadata.GetMethodDefinition(methodHandle)
                let methodName = metadata.GetString(methodDefinition.Name)

                let access =
                    methodDefinition.Attributes
                    &&& MethodAttributes.MemberAccessMask

                let methodIsStatic =
                    (methodDefinition.Attributes
                     &&& MethodAttributes.Static)
                    <> enum 0

                if
                    methodName
                    <> name
                    || access
                       <> MethodAttributes.Public
                    || methodIsStatic
                       <> isStatic
                then
                    None
                else
                    let signature = methodDefinition.DecodeSignature(signatureProvider, ())

                    if
                        signature.ReturnType.IsNone
                        || (signature.ParameterTypes
                            |> Seq.exists Option.isNone)
                    then
                        None
                    else
                        let parameterTypes =
                            signature.ParameterTypes
                            |> Seq.choose id
                            |> List.ofSeq

                        let returnType = signature.ReturnType.Value
                        let genericArity = signature.GenericParameterCount

                        let optionalParameters = Array.create parameterTypes.Length false

                        let paramArrayParameters = Array.create parameterTypes.Length false

                        let parameterNames = Array.create parameterTypes.Length String.Empty

                        for parameterHandle in methodDefinition.GetParameters() do
                            let parameter = metadata.GetParameter(parameterHandle)

                            let index =
                                parameter.SequenceNumber
                                - 1

                            if
                                index >= 0
                                && index < optionalParameters.Length
                            then
                                optionalParameters.[index] <- isFSharpOptionalParameter parameter

                                parameterNames.[index] <- metadata.GetString(parameter.Name)

                                paramArrayParameters.[index] <- isParamArrayParameter parameter

                        let optionalParameterCount =
                            optionalParameters
                            |> Array.rev
                            |> Seq.takeWhile id
                            |> Seq.length

                        let paramArrayElementType =
                            match List.tryLast parameterTypes with
                            | Some(CliArray elementType) when
                                paramArrayParameters.Length > 0
                                && paramArrayParameters.[paramArrayParameters.Length
                                                         - 1]
                                ->
                                Some elementType
                            | _ -> None

                        let stableId =
                            String.concat "|" [
                                location.DeclaringType.DeclarationId
                                "method"
                                methodName
                                "generic"
                                genericArity.ToString()
                                yield!
                                    parameterTypes
                                    |> List.map StableIdentity.cliType
                                "return"
                                StableIdentity.cliType returnType
                            ]

                        Some {
                            DeclaringType = location.DeclaringType
                            StableId = stableId
                            Name = methodName
                            IsStatic = methodIsStatic
                            GenericArity = genericArity
                            MetadataParameterNames =
                                parameterNames
                                |> Array.toList
                            ParameterTypes = parameterTypes
                            OptionalParameterCount = optionalParameterCount
                            ParamArrayElementType = paramArrayElementType
                            ReturnType = returnType
                        }
            )
            |> List.ofSeq
        )
        |> List.distinctBy _.StableId
        |> List.sortBy _.StableId

    let loadFields declarationId name isStatic =
        let signatureProvider = ReferenceSignatureTypeProvider(types)

        let locations =
            match typeLocations.TryGetValue(declarationId) with
            | true, candidates -> candidates
            | false, _ -> []

        locations
        |> List.collect (fun location ->
            use metadataStream = File.OpenRead(location.ReferencePath)
            use pe = new PEReader(metadataStream)
            let metadata = pe.GetMetadataReader()

            let typeDefinition =
                location.TypeRow
                |> MetadataTokens.TypeDefinitionHandle
                |> metadata.GetTypeDefinition

            typeDefinition.GetFields()
            |> Seq.choose (fun fieldHandle ->
                let fieldDefinition = metadata.GetFieldDefinition(fieldHandle)
                let fieldName = metadata.GetString(fieldDefinition.Name)

                let access =
                    fieldDefinition.Attributes
                    &&& FieldAttributes.FieldAccessMask

                let fieldIsStatic =
                    (fieldDefinition.Attributes
                     &&& FieldAttributes.Static)
                    <> enum 0

                if
                    fieldName
                    <> name
                    || access
                       <> FieldAttributes.Public
                    || fieldIsStatic
                       <> isStatic
                then
                    None
                else
                    match fieldDefinition.DecodeSignature(signatureProvider, ()) with
                    | None -> None
                    | Some fieldType ->
                        let stableId =
                            String.concat "|" [
                                location.DeclaringType.DeclarationId
                                "field"
                                fieldName
                                StableIdentity.cliType fieldType
                            ]

                        Some {
                            DeclaringType = location.DeclaringType
                            StableId = stableId
                            Name = fieldName
                            IsStatic = fieldIsStatic
                            FieldType = fieldType
                        }
            )
            |> List.ofSeq
        )
        |> List.distinctBy _.StableId
        |> List.sortBy _.StableId

    member _.Fingerprint = fingerprint

    member _.IsFSharpDelegate(declarationId: string) =
        fsharpDelegateDeclarationIds.Contains(declarationId)

    member _.Methods(declarationId: string, name: string, isStatic: bool) =
        let key = ReferenceMethodKey.create declarationId name isStatic

        lock
            methodCache
            (fun () ->
                match methodCache.TryGetValue(key) with
                | true, candidates -> candidates
                | false, _ ->
                    let candidates = loadMethods declarationId name isStatic
                    methodCache.Add(key, candidates)
                    candidates
            )

    member _.Fields(declarationId: string, name: string, isStatic: bool) =
        let key = ReferenceFieldKey.create declarationId name isStatic

        lock
            fieldCache
            (fun () ->
                match fieldCache.TryGetValue(key) with
                | true, candidates -> candidates
                | false, _ ->
                    let candidates = loadFields declarationId name isStatic
                    fieldCache.Add(key, candidates)
                    candidates
            )

    member _.BaseType(declarationId: string) =
        lock
            baseTypeCache
            (fun () ->
                match baseTypeCache.TryGetValue(declarationId) with
                | true, baseType -> baseType
                | false, _ ->
                    let baseType = loadBaseType declarationId
                    baseTypeCache.Add(declarationId, baseType)
                    baseType
            )

    member _.Interfaces(declarationId: string) =
        lock
            interfaceCache
            (fun () ->
                match interfaceCache.TryGetValue(declarationId) with
                | true, interfaces -> interfaces
                | false, _ ->
                    let interfaces = loadInterfaces declarationId
                    interfaceCache.Add(declarationId, interfaces)
                    interfaces
            )

    member _.Resolve
        (
            currentNamespace: string,
            openedNamespaces: string list,
            syntaxName: QualifiedTypeName,
            genericArity: int
        ) =
        let knownAlias =
            match syntaxName.Namespace, syntaxName.Name with
            | "", "int" -> Some { Namespace = "System"; Name = "Int32" }
            | "", "bool" ->
                Some {
                    Namespace = "System"
                    Name = "Boolean"
                }
            | "", "unit" ->
                Some {
                    Namespace = "Microsoft.FSharp.Core"
                    Name = "Unit"
                }
            | "", "exn" ->
                Some {
                    Namespace = "System"
                    Name = "Exception"
                }
            | "", "Async"
            | "Microsoft.FSharp.Control", "Async" ->
                Some {
                    Namespace = "Microsoft.FSharp.Control"
                    Name = "FSharpAsync"
                }
            | "", "Choice"
            | "Microsoft.FSharp.Core", "Choice" ->
                Some {
                    Namespace = "Microsoft.FSharp.Core"
                    Name = "FSharpChoice"
                }
            | _ -> None

        let tryResolve typeName =
            match types.TryGetValue(ReferenceTypeName.typeKey typeName genericArity) with
            | true, resolved -> Some resolved
            | false, _ -> None

        let resolvedAlias =
            knownAlias
            |> Option.bind tryResolve

        let exactCandidate =
            if String.IsNullOrEmpty(syntaxName.Namespace) then
                None
            else
                tryResolve syntaxName

        let contextualCandidate =
            if
                not (String.IsNullOrEmpty(syntaxName.Namespace))
                || knownAlias.IsSome
            then
                None
            else
                [
                    if not (String.IsNullOrEmpty(currentNamespace)) then
                        yield currentNamespace

                    yield! List.rev openedNamespaces
                ]
                |> List.tryPick (fun namespaceName ->
                    let candidate = {
                        Namespace = namespaceName
                        Name = syntaxName.Name
                    }

                    tryResolve candidate
                )

        match resolvedAlias, exactCandidate, contextualCandidate, knownAlias with
        | Some candidate, _, _, _ -> Ok candidate
        | _, Some candidate, _, _ -> Ok candidate
        | _, _, Some candidate, _ -> Ok candidate
        | None, None, None, Some candidate ->
            Error(
                $"the target reference set does not define '{ReferenceTypeName.fullName candidate}'"
            )
        | None, None, None, None ->
            match
                typesBySimpleName.TryGetValue(
                    ReferenceTypeName.simpleKey syntaxName.Name genericArity
                )
            with
            | true, [ candidate ] -> Ok candidate
            | true, candidates ->
                candidates
                |> List.map (fun candidate -> ReferenceTypeName.fullName candidate.TypeName)
                |> String.concat "', '"
                |> fun names ->
                    Error($"the type name '{syntaxName.Name}' is ambiguous between '{names}'")
            | false, _ ->
                Error(
                    $"the type '{ReferenceTypeName.fullName syntaxName}' is not defined by the target references"
                )

    static member Create(referencePaths: string list) =
        let normalizedReferencePaths =
            referencePaths
            |> List.map Path.GetFullPath
            |> List.distinct
            |> List.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))

        let types = Dictionary<TypeNameArity, ResolvedTypeName>()

        let bySimpleName = Dictionary<TypeNameArity, ResizeArray<ResolvedTypeName>>()

        let fsharpDelegateDeclarationIds = HashSet<string>(StringComparer.Ordinal)

        let typeLocations = Dictionary<string, ResizeArray<ReferenceTypeLocation>>()

        let referenceIdentities = ResizeArray<string>()

        let referenceContentHashes =
            Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        let addReferenceIdentity path =
            let normalizedPath = Path.GetFullPath(path)

            match referenceContentHashes.TryGetValue(normalizedPath) with
            | true, contentHash -> contentHash
            | false, _ ->
                use fingerprintStream = File.OpenRead(normalizedPath)

                let contentHash =
                    SHA256.HashData(fingerprintStream)
                    |> Convert.ToHexString
                    |> fun value -> value.ToLowerInvariant()

                referenceContentHashes.Add(normalizedPath, contentHash)

                referenceIdentities.Add(
                    normalizedPath
                    + "="
                    + contentHash
                )

                contentHash

        let addTypeWithArity
            declarationOwner
            assemblyName
            isValueType
            isFSharpDelegate
            (namespaceName: string)
            (name: string)
            genericArity
            =
            if
                not (String.IsNullOrEmpty(name))
                && name
                   <> "<Module>"
            then
                let qualifiedName = {
                    Namespace = namespaceName
                    Name = name
                }

                let key = ReferenceTypeName.typeKey qualifiedName genericArity

                let resolved: ResolvedTypeName = {
                    TypeName = qualifiedName
                    DeclarationId =
                        declarationOwner
                        + "/type:"
                        + ReferenceTypeName.fullName qualifiedName
                        + "`"
                        + genericArity.ToString()
                    AssemblyName = assemblyName
                    IsValueType = isValueType
                }

                if types.TryAdd(key, resolved) then
                    if isFSharpDelegate then
                        fsharpDelegateDeclarationIds.Add(resolved.DeclarationId)
                        |> ignore

                    let key = ReferenceTypeName.simpleKey name genericArity

                    match bySimpleName.TryGetValue(key) with
                    | true, candidates -> candidates.Add(resolved)
                    | false, _ ->
                        let candidates = ResizeArray<ResolvedTypeName>()
                        candidates.Add(resolved)
                        bySimpleName.Add(key, candidates)

        let addType
            declarationOwner
            assemblyName
            isValueType
            isFSharpDelegate
            (namespaceName: string)
            (metadataName: string)
            =
            let name, genericArity = ReferenceTypeName.parseMetadataName metadataName

            addTypeWithArity
                declarationOwner
                assemblyName
                isValueType
                isFSharpDelegate
                namespaceName
                name
                genericArity

        let forwardedAssemblyTypes =
            Dictionary<string, Dictionary<string * string, int * bool>>(
                StringComparer.OrdinalIgnoreCase
            )

        let forwardedTypesFor implementationPath =
            match forwardedAssemblyTypes.TryGetValue(implementationPath) with
            | true, forwardedTypes -> forwardedTypes
            | false, _ ->
                addReferenceIdentity implementationPath
                |> ignore

                let forwardedTypes = Dictionary<string * string, int * bool>()
                use implementationStream = File.OpenRead(implementationPath)
                use implementationPe = new PEReader(implementationStream)

                if implementationPe.HasMetadata then
                    let implementationMetadata = implementationPe.GetMetadataReader()

                    let implementationTypeName (handle: EntityHandle) =
                        if handle.IsNil then
                            None
                        else
                            match handle.Kind with
                            | HandleKind.TypeReference ->
                                let reference =
                                    implementationMetadata.GetTypeReference(
                                        handle
                                        |> MetadataTokens.GetRowNumber
                                        |> MetadataTokens.TypeReferenceHandle
                                    )

                                Some(
                                    implementationMetadata.GetString(reference.Namespace),
                                    implementationMetadata.GetString(reference.Name)
                                )
                            | HandleKind.TypeDefinition ->
                                let definition =
                                    implementationMetadata.GetTypeDefinition(
                                        handle
                                        |> MetadataTokens.GetRowNumber
                                        |> MetadataTokens.TypeDefinitionHandle
                                    )

                                Some(
                                    implementationMetadata.GetString(definition.Namespace),
                                    implementationMetadata.GetString(definition.Name)
                                )
                            | _ -> None

                    for handle in implementationMetadata.TypeDefinitions do
                        let definition = implementationMetadata.GetTypeDefinition(handle)

                        if not definition.IsNested then
                            let namespaceName =
                                implementationMetadata.GetString(definition.Namespace)

                            let metadataName = implementationMetadata.GetString(definition.Name)

                            let isValueType =
                                implementationTypeName definition.BaseType
                                |> Option.exists (fun (namespaceName, typeName) ->
                                    namespaceName = "System"
                                    && (typeName = "ValueType"
                                        || typeName = "Enum")
                                )

                            forwardedTypes.TryAdd(
                                (namespaceName, metadataName),
                                (MetadataTokens.GetRowNumber(handle), isValueType)
                            )
                            |> ignore

                forwardedAssemblyTypes.Add(implementationPath, forwardedTypes)
                forwardedTypes

        let addForwardedTypeLocation
            (facadePath: string)
            (facadeMetadata: MetadataReader)
            (exportedType: ExportedType)
            =
            if
                exportedType.Implementation.Kind
                <> HandleKind.AssemblyReference
            then
                ()
            else
                let assemblyReference =
                    exportedType.Implementation
                    |> AssemblyReferenceHandle.op_Explicit
                    |> facadeMetadata.GetAssemblyReference

                let implementationAssemblyName = facadeMetadata.GetString(assemblyReference.Name)

                let implementationPath =
                    Path.Combine(
                        Path.GetDirectoryName(facadePath),
                        implementationAssemblyName
                        + ".dll"
                    )

                if File.Exists(implementationPath) then
                    let namespaceName = facadeMetadata.GetString(exportedType.Namespace)
                    let metadataName = facadeMetadata.GetString(exportedType.Name)
                    let forwardedTypes = forwardedTypesFor implementationPath

                    match forwardedTypes.TryGetValue((namespaceName, metadataName)) with
                    | false, _ -> ()
                    | true, (typeRow, isValueType) ->
                        let name, genericArity = ReferenceTypeName.parseMetadataName metadataName

                        let typeKey =
                            ReferenceTypeName.typeKey
                                {
                                    Namespace = namespaceName
                                    Name = name
                                }
                                genericArity

                        match types.TryGetValue(typeKey) with
                        | false, _ -> ()
                        | true, resolved ->
                            let resolved = {
                                resolved with
                                    IsValueType = isValueType
                            }

                            types.[typeKey] <- resolved

                            let simpleKey = ReferenceTypeName.simpleKey name genericArity

                            match bySimpleName.TryGetValue(simpleKey) with
                            | true, candidates ->
                                for index = 0 to candidates.Count
                                                 - 1 do
                                    if
                                        candidates.[index].DeclarationId = resolved.DeclarationId
                                    then
                                        candidates.[index] <- resolved
                            | false, _ -> ()

                            let location = {
                                ReferencePath = implementationPath
                                TypeRow = typeRow
                                DeclaringType = {
                                    DeclarationId = resolved.DeclarationId
                                    AssemblyName = resolved.AssemblyName
                                    TypeName = resolved.TypeName
                                    IsValueType = resolved.IsValueType
                                }
                            }

                            match typeLocations.TryGetValue(resolved.DeclarationId) with
                            | true, locations -> locations.Add(location)
                            | false, _ ->
                                let locations = ResizeArray<ReferenceTypeLocation>()
                                locations.Add(location)
                                typeLocations.Add(resolved.DeclarationId, locations)

        try
            for path in normalizedReferencePaths do
                let contentHash = addReferenceIdentity path

                use metadataStream = File.OpenRead(path)
                use pe = new PEReader(metadataStream)

                if not pe.HasMetadata then
                    invalidOp $"the reference '{path}' does not contain CLI metadata"

                let metadata = pe.GetMetadataReader()

                let assemblyName, declarationOwner =
                    if metadata.IsAssembly then
                        let definition = metadata.GetAssemblyDefinition()
                        let assemblyName = metadata.GetString(definition.Name)

                        let culture =
                            if definition.Culture.IsNil then
                                String.Empty
                            else
                                metadata.GetString(definition.Culture)

                        let publicKey =
                            if definition.PublicKey.IsNil then
                                String.Empty
                            else
                                metadata.GetBlobBytes(definition.PublicKey)
                                |> Convert.ToHexString

                        assemblyName,
                        String.concat "|" [
                            "reference-assembly"
                            assemblyName
                            definition.Version.ToString()
                            culture
                            publicKey
                        ]
                    else
                        Path.GetFileNameWithoutExtension(path),
                        "reference-module|"
                        + contentHash

                let qualifiedTypeName namespaceName name : QualifiedTypeName = {
                    Namespace = namespaceName
                    Name = name
                }

                let typeReferenceName handle =
                    let typeReference =
                        handle
                        |> MetadataTokens.GetRowNumber
                        |> MetadataTokens.TypeReferenceHandle
                        |> metadata.GetTypeReference

                    qualifiedTypeName
                        (metadata.GetString(typeReference.Namespace))
                        (metadata.GetString(typeReference.Name))

                let typeDefinitionName handle =
                    let typeDefinition =
                        handle
                        |> MetadataTokens.GetRowNumber
                        |> MetadataTokens.TypeDefinitionHandle
                        |> metadata.GetTypeDefinition

                    qualifiedTypeName
                        (metadata.GetString(typeDefinition.Namespace))
                        (metadata.GetString(typeDefinition.Name))

                let entityTypeName (handle: EntityHandle) =
                    if handle.IsNil then
                        None
                    else
                        match handle.Kind with
                        | HandleKind.TypeReference -> Some(typeReferenceName handle)
                        | HandleKind.TypeDefinition -> Some(typeDefinitionName handle)
                        | _ -> None

                let baseTypeName (definition: TypeDefinition) = entityTypeName definition.BaseType

                let isValueType (definition: TypeDefinition) =
                    match baseTypeName definition with
                    | Some typeName when
                        typeName = qualifiedTypeName "System" "ValueType"
                        || typeName = qualifiedTypeName "System" "Enum"
                        ->
                        true
                    | _ -> false

                let customAttributeTypeName (attribute: CustomAttribute) =
                    match attribute.Constructor.Kind with
                    | HandleKind.MemberReference ->
                        let constructor =
                            attribute.Constructor
                            |> MetadataTokens.GetRowNumber
                            |> MetadataTokens.MemberReferenceHandle
                            |> metadata.GetMemberReference

                        entityTypeName constructor.Parent
                    | HandleKind.MethodDefinition ->
                        let constructor =
                            attribute.Constructor
                            |> MetadataTokens.GetRowNumber
                            |> MetadataTokens.MethodDefinitionHandle
                            |> metadata.GetMethodDefinition

                        let declaringType =
                            constructor.GetDeclaringType()
                            |> metadata.GetTypeDefinition

                        qualifiedTypeName
                            (metadata.GetString(declaringType.Namespace))
                            (metadata.GetString(declaringType.Name))
                        |> Some
                    | _ -> None

                let isFSharpDelegate (definition: TypeDefinition) =
                    match baseTypeName definition with
                    | Some baseType when
                        baseType.Namespace = "System"
                        && baseType.Name = "MulticastDelegate"
                        ->
                        definition.GetCustomAttributes()
                        |> Seq.exists (fun handle ->
                            handle
                            |> metadata.GetCustomAttribute
                            |> customAttributeTypeName
                            |> Option.exists (fun attributeType ->
                                attributeType.Namespace = "Microsoft.FSharp.Core"
                                && attributeType.Name = "CompilationMappingAttribute"
                            )
                        )
                    | _ -> false

                let rec metadataTypeName handle =
                    let definition = metadata.GetTypeDefinition(handle)
                    let metadataName = metadata.GetString(definition.Name)

                    if definition.IsNested then
                        let namespaceName, declaringName =
                            metadataTypeName (definition.GetDeclaringType())

                        namespaceName,
                        declaringName
                        + "+"
                        + metadataName
                    else
                        metadata.GetString(definition.Namespace), metadataName

                let indexedTypeName handle =
                    let definition = metadata.GetTypeDefinition(handle)
                    let namespaceName, metadataName = metadataTypeName handle

                    if definition.IsNested then
                        namespaceName,
                        metadataName,
                        (definition.GetGenericParameters()
                         |> Seq.length)
                    else
                        let name, genericArity = ReferenceTypeName.parseMetadataName metadataName

                        namespaceName, name, genericArity

                for handle in metadata.TypeDefinitions do
                    let definition = metadata.GetTypeDefinition(handle)
                    let namespaceName, name, genericArity = indexedTypeName handle

                    addTypeWithArity
                        declarationOwner
                        assemblyName
                        (isValueType definition)
                        (isFSharpDelegate definition)
                        namespaceName
                        name
                        genericArity

                    let key =
                        ReferenceTypeName.typeKey
                            {
                                Namespace = namespaceName
                                Name = name
                            }
                            genericArity

                    match types.TryGetValue(key) with
                    | false, _ -> ()
                    | true, resolved ->
                        let location = {
                            ReferencePath = path
                            TypeRow = MetadataTokens.GetRowNumber(handle)
                            DeclaringType = {
                                DeclarationId = resolved.DeclarationId
                                AssemblyName = resolved.AssemblyName
                                TypeName = resolved.TypeName
                                IsValueType = resolved.IsValueType
                            }
                        }

                        match typeLocations.TryGetValue(resolved.DeclarationId) with
                        | true, locations -> locations.Add(location)
                        | false, _ ->
                            let locations = ResizeArray<ReferenceTypeLocation>()
                            locations.Add(location)
                            typeLocations.Add(resolved.DeclarationId, locations)

                for handle in metadata.ExportedTypes do
                    let exportedType = metadata.GetExportedType(handle)

                    addType
                        declarationOwner
                        assemblyName
                        false
                        false
                        (metadata.GetString(exportedType.Namespace))
                        (metadata.GetString(exportedType.Name))

                    addForwardedTypeLocation path metadata exportedType

            let fingerprint =
                referenceIdentities
                |> String.concat "\n"
                |> ReferenceFingerprint.text

            let frozenBySimpleName = Dictionary<TypeNameArity, ResolvedTypeName list>()

            for KeyValue(name, candidates) in bySimpleName do
                frozenBySimpleName.Add(
                    name,
                    candidates
                    |> Seq.sortBy (fun candidate ->
                        candidate.TypeName.Namespace, candidate.DeclarationId
                    )
                    |> List.ofSeq
                )

            let frozenTypeLocations = Dictionary<string, ReferenceTypeLocation list>()

            for KeyValue(declarationId, locations) in typeLocations do
                frozenTypeLocations.Add(
                    declarationId,
                    locations
                    |> Seq.distinctBy (fun location -> location.ReferencePath, location.TypeRow)
                    |> Seq.sortBy (fun location -> location.ReferencePath, location.TypeRow)
                    |> List.ofSeq
                )

            Ok(
                ReferenceTypeIndex(
                    fingerprint,
                    types,
                    frozenBySimpleName,
                    fsharpDelegateDeclarationIds,
                    frozenTypeLocations
                )
            )
        with error ->
            Error error.Message
