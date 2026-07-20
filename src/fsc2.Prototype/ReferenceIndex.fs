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

/// A metadata-only index of the evaluated target references. The index owns
/// name lookup and a content-addressed identity; it never loads target code.
type internal ReferenceTypeIndex private (
    fingerprint: string,
    types: HashSet<string>,
    typesBySimpleName: Dictionary<string, QualifiedTypeName list>
) =
    let fullName (typeName: QualifiedTypeName) =
        if String.IsNullOrEmpty(typeName.Namespace) then
            typeName.Name
        else
            typeName.Namespace
            + "."
            + typeName.Name

    member _.Fingerprint = fingerprint

    member _.Resolve(
        currentNamespace: string,
        openedNamespaces: string list,
        syntaxName: QualifiedTypeName
    ) =
        let knownAlias =
            match syntaxName.Namespace, syntaxName.Name with
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

        let exactCandidate =
            if String.IsNullOrEmpty(syntaxName.Namespace) then
                None
            elif types.Contains(fullName syntaxName) then
                Some syntaxName
            else
                None

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

                    if types.Contains(fullName candidate) then
                        Some candidate
                    else
                        None
                )

        match knownAlias, exactCandidate, contextualCandidate with
        | Some candidate, _, _ when types.Contains(fullName candidate) -> Ok candidate
        | _, Some candidate, _ -> Ok candidate
        | _, _, Some candidate -> Ok candidate
        | Some candidate, _, _ ->
            Error($"the target reference set does not define '{fullName candidate}'")
        | None, None, None ->
            match typesBySimpleName.TryGetValue(syntaxName.Name) with
            | true, [ candidate ] -> Ok candidate
            | true, candidates ->
                candidates
                |> List.map fullName
                |> String.concat "', '"
                |> fun names -> Error($"the type name '{syntaxName.Name}' is ambiguous between '{names}'")
            | false, _ -> Error($"the type '{fullName syntaxName}' is not defined by the target references")

    static member Create(referencePaths: string list) =
        let types = HashSet<string>(StringComparer.Ordinal)
        let bySimpleName = Dictionary<string, ResizeArray<QualifiedTypeName>>(StringComparer.Ordinal)
        let referenceIdentities = ResizeArray<string>()

        let addType namespaceName name =
            if
                not (String.IsNullOrEmpty(name))
                && name <> "<Module>"
            then
                let qualifiedName = {
                    Namespace = namespaceName
                    Name = name
                }

                let fullName =
                    if String.IsNullOrEmpty(namespaceName) then
                        name
                    else
                        namespaceName
                        + "."
                        + name

                if types.Add(fullName) then
                    match bySimpleName.TryGetValue(name) with
                    | true, candidates -> candidates.Add(qualifiedName)
                    | false, _ ->
                        let candidates = ResizeArray<QualifiedTypeName>()
                        candidates.Add(qualifiedName)
                        bySimpleName.Add(name, candidates)

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

                referenceIdentities.Add(path + "=" + contentHash)

                use metadataStream = File.OpenRead(path)
                use pe = new PEReader(metadataStream)

                if not pe.HasMetadata then
                    invalidOp $"the reference '{path}' does not contain CLI metadata"

                let metadata = pe.GetMetadataReader()

                for handle in metadata.TypeDefinitions do
                    let definition = metadata.GetTypeDefinition(handle)

                    if not definition.IsNested then
                        addType
                            (metadata.GetString(definition.Namespace))
                            (metadata.GetString(definition.Name))

                for handle in metadata.ExportedTypes do
                    let exportedType = metadata.GetExportedType(handle)

                    addType
                        (metadata.GetString(exportedType.Namespace))
                        (metadata.GetString(exportedType.Name))

            let fingerprint =
                referenceIdentities
                |> String.concat "\n"
                |> ReferenceFingerprint.text

            let frozenBySimpleName =
                Dictionary<string, QualifiedTypeName list>(StringComparer.Ordinal)

            for KeyValue(name, candidates) in bySimpleName do
                frozenBySimpleName.Add(
                    name,
                    candidates
                    |> Seq.sortBy (fun candidate -> candidate.Namespace)
                    |> List.ofSeq
                )

            Ok(ReferenceTypeIndex(fingerprint, types, frozenBySimpleName))
        with error ->
            Error error.Message
