namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection.Metadata
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

/// A metadata-only index of the evaluated target references. The index owns
/// name lookup and a content-addressed identity; it never loads target code.
type internal ReferenceTypeIndex
    private
    (
        fingerprint: string,
        types: Dictionary<TypeNameArity, ResolvedTypeName>,
        typesBySimpleName: Dictionary<TypeNameArity, ResolvedTypeName list>
    ) =
    member _.Fingerprint = fingerprint

    member _.Resolve
        (
            currentNamespace: string,
            openedNamespaces: string list,
            syntaxName: QualifiedTypeName,
            genericArity: int
        ) =
        let knownAlias =
            match syntaxName.Namespace, syntaxName.Name with
            | "", "int" ->
                Some {
                    Namespace = "System"
                    Name = "Int32"
                }
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
        let types = Dictionary<TypeNameArity, ResolvedTypeName>()

        let bySimpleName = Dictionary<TypeNameArity, ResizeArray<ResolvedTypeName>>()

        let referenceIdentities = ResizeArray<string>()

        let addType declarationOwner (namespaceName: string) (metadataName: string) =
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

                let resolved = {
                    TypeName = qualifiedName
                    DeclarationId =
                        declarationOwner
                        + "/type:"
                        + ReferenceTypeName.fullName qualifiedName
                        + "`"
                        + genericArity.ToString()
                }

                if types.TryAdd(key, resolved) then
                    let key = ReferenceTypeName.simpleKey name genericArity

                    match bySimpleName.TryGetValue(key) with
                    | true, candidates -> candidates.Add(resolved)
                    | false, _ ->
                        let candidates = ResizeArray<ResolvedTypeName>()
                        candidates.Add(resolved)
                        bySimpleName.Add(key, candidates)

        try
            for path in
                referencePaths
                |> List.map Path.GetFullPath
                |> List.distinct
                |> List.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right)) do
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

                let declarationOwner =
                    if metadata.IsAssembly then
                        let definition = metadata.GetAssemblyDefinition()

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

                        String.concat "|" [
                            "reference-assembly"
                            metadata.GetString(definition.Name)
                            definition.Version.ToString()
                            culture
                            publicKey
                        ]
                    else
                        "reference-module|"
                        + contentHash

                for handle in metadata.TypeDefinitions do
                    let definition = metadata.GetTypeDefinition(handle)

                    if not definition.IsNested then
                        addType
                            declarationOwner
                            (metadata.GetString(definition.Namespace))
                            (metadata.GetString(definition.Name))

                for handle in metadata.ExportedTypes do
                    let exportedType = metadata.GetExportedType(handle)

                    addType
                        declarationOwner
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

            Ok(ReferenceTypeIndex(fingerprint, types, frozenBySimpleName))
        with error ->
            Error error.Message
