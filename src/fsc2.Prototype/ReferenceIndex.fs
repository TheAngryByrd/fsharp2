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
    ParameterTypes: CliType list
    ReturnType: CliType
} with

    override _.ToString() = "ReferenceMethodDefinition"

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

type private ReferenceSignatureTypeProvider(types: Dictionary<TypeNameArity, ResolvedTypeName>) as this =
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
            |> Option.bind (function
                | CliVoid -> None
                | cliType -> Some(CliByRef cliType)
            )

        member _.GetFunctionPointerType(_) = None

        member _.GetGenericInstantiation(genericType, typeArguments) =
            match genericType with
            | Some(CliNamedType typeReference) when typeArguments |> Seq.forall Option.isSome ->
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

        member _.GetGenericMethodParameter(_, index) =
            Some(CliMethodTypeParameter index)

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

        member _.GetSZArrayType(_) = None

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
                    methodName <> name
                    || access <> MethodAttributes.Public
                    || methodIsStatic <> isStatic
                then
                    None
                else
                    let signature =
                        methodDefinition.DecodeSignature(signatureProvider, ())

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
                            ParameterTypes = parameterTypes
                            ReturnType = returnType
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

        lock methodCache (fun () ->
            match methodCache.TryGetValue(key) with
            | true, candidates -> candidates
            | false, _ ->
                let candidates = loadMethods declarationId name isStatic
                methodCache.Add(key, candidates)
                candidates
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

        let addType
            declarationOwner
            assemblyName
            isValueType
            isFSharpDelegate
            (namespaceName: string)
            (metadataName: string)
            =
            let name, genericArity = ReferenceTypeName.parseMetadataName metadataName

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

        try
            for path in normalizedReferencePaths do
                use fingerprintStream = File.OpenRead(path)

                let contentHash =
                    SHA256.HashData(fingerprintStream)
                    |> Convert.ToHexString
                    |> fun value -> value.ToLowerInvariant()

                referenceIdentities.Add(
                    path
                    + "="
                    + contentHash
                )

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

                for handle in metadata.TypeDefinitions do
                    let definition = metadata.GetTypeDefinition(handle)

                    if not definition.IsNested then
                        let namespaceName = metadata.GetString(definition.Namespace)
                        let metadataName = metadata.GetString(definition.Name)

                        addType
                            declarationOwner
                            assemblyName
                            (isValueType definition)
                            (isFSharpDelegate definition)
                            namespaceName
                            metadataName

                        let name, genericArity =
                            ReferenceTypeName.parseMetadataName metadataName

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
