namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text

module private Fingerprint =
    let text (value: string) =
        value
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun hash -> hash.ToLowerInvariant()

    let parts (values: string seq) =
        values
        |> Seq.map (fun value ->
            value.Length.ToString(CultureInfo.InvariantCulture)
            + ":"
            + value
        )
        |> String.concat String.Empty
        |> text

module private TypeIdentity =
    let qualifiedName (typeName: QualifiedTypeName) =
        if String.IsNullOrEmpty(typeName.Namespace) then
            typeName.Name
        else
            typeName.Namespace
            + "."
            + typeName.Name

    let rec expression =
        function
        | TypedNamedType typeName ->
            Fingerprint.parts [
                "named"
                qualifiedName typeName
            ]
        | TypedTypeParameter name ->
            Fingerprint.parts [
                "parameter"
                name
            ]
        | TypedFunctionType(domain, range) ->
            Fingerprint.parts [
                "function"
                expression domain
                expression range
            ]

    let constraintIdentity =
        function
        | TypedSubtypeConstraint(typeParameter, superType) ->
            Fingerprint.parts [
                "subtype"
                typeParameter
                expression superType
            ]
        | TypedMemberConstraint(typeParameter, memberName, memberType) ->
            Fingerprint.parts [
                "member"
                typeParameter
                memberName
                expression memberType
            ]

/// A deliberately small in-memory query owner. Query identities and cached
/// values are semantic/compiler state; final SRM state never enters these maps.
type internal CompilerService() =
    let querySchema = CompilerSchema.Query
    let parseCache = Dictionary<string, ParsedModule>(StringComparer.Ordinal)
    let checkCache = Dictionary<string, TypedModule>(StringComparer.Ordinal)
    let lowerCache = Dictionary<string, SymbolicAssembly>(StringComparer.Ordinal)
    let lastSuccessfulContent = Dictionary<string, string>(StringComparer.Ordinal)
    let mutable parseHits = 0
    let mutable parseMisses = 0
    let mutable checkHits = 0
    let mutable checkMisses = 0
    let mutable lowerHits = 0
    let mutable lowerMisses = 0

    let elapsedMicroseconds started =
        Stopwatch.GetElapsedTime(started).Ticks
        / 10L

    let parse (defines: string list) (source: SourceInput) =
        let normalizedDefines =
            defines
            |> List.distinct
            |> List.sortWith (fun left right ->
                StringComparer.Ordinal.Compare(left, right)
            )

        let key =
            Fingerprint.parts [
                querySchema.ToString(CultureInfo.InvariantCulture)
                "defines"
                normalizedDefines.Length.ToString(CultureInfo.InvariantCulture)
                yield! normalizedDefines
                Path.GetFileName(source.Path)
                source.Text
            ]

        match parseCache.TryGetValue(key) with
        | true, parsed ->
            parseHits <-
                parseHits
                + 1

            Ok(parsed, key)
        | false, _ ->
            parseMisses <-
                parseMisses
                + 1

            match Frontend.parse normalizedDefines source with
            | Error error -> Error error
            | Ok parsed ->
                parseCache.Add(key, parsed)
                Ok(parsed, key)

    let check
        (references: ReferenceTypeIndex)
        (sourcePath: string)
        (parsed: ParsedModule)
        =
        let key =
            Fingerprint.parts [
                querySchema.ToString(CultureInfo.InvariantCulture)
                references.Fingerprint
                parsed.StableId
                parsed.ContentFingerprint
            ]

        match checkCache.TryGetValue(key) with
        | true, typed ->
            checkHits <-
                checkHits
                + 1

            Ok(
                {
                    typed with
                        SourceChecksum = parsed.SourceChecksum
                },
                key
            )
        | false, _ ->
            checkMisses <-
                checkMisses
                + 1

            let typeDeclaration =
                function
                | ParsedMethod declaration ->
                    match declaration.DeclaredType, declaration.Body with
                    | Some ParsedInt32, StringLiteral _ ->
                        Error {
                            Code = "FS0001"
                            Message =
                                String.concat Environment.NewLine [
                                    "This expression was expected to have type"
                                    "    'int'    "
                                    "but here has type"
                                    "    'string'"
                                ]
                            Path = Some sourcePath
                            Range = Some declaration.BodyRange
                        }

                    | _, IntegerLiteral value ->
                        let stableId =
                            parsed.StableId
                            + "/method:"
                            + declaration.Name
                            + (if declaration.IsUnitFunction then
                                   ":unit->int32"
                               else
                                   ":int32")

                        let exportFingerprint = Fingerprint.text stableId

                        Ok(
                            TypedMethod {
                                StableId = stableId
                                Name = declaration.Name
                                ReturnType = ValueType.Int32
                                Body = TypedIntegerLiteral value
                                ExportFingerprint = exportFingerprint
                                Range = declaration.Range
                            }
                        )
                    | None, StringLiteral _ ->
                        Error {
                            Code = "FSC2P1001"
                            Message =
                                "string-valued declarations are not yet supported by the prototype"
                            Path = Some sourcePath
                            Range = Some declaration.BodyRange
                        }
                | ParsedLiteralField declaration ->
                    let stableId =
                        parsed.StableId
                        + "/literal-field:"
                        + declaration.Name

                    Ok(
                        TypedLiteralField {
                            StableId = stableId
                            Name = declaration.Name
                            Value = declaration.Value
                            ExportFingerprint =
                                Fingerprint.text (
                                    stableId
                                    + "="
                                    + declaration.Value
                                )
                        }
                    )
                | ParsedTypeAbbreviation declaration ->
                    let stableId =
                        parsed.StableId
                        + "/type-abbreviation:"
                        + declaration.Name

                    let declaredParameters =
                        HashSet<string>(declaration.TypeParameters, StringComparer.Ordinal)

                    let diagnostic range message =
                        Error {
                            Code = "FSC2P1001"
                            Message = message
                            Path = Some sourcePath
                            Range = Some range
                        }

                    let rec resolveType =
                        function
                        | ParsedTypeParameter(name, range) ->
                            if declaredParameters.Contains(name) then
                                Ok(TypedTypeParameter name)
                            else
                                diagnostic range $"the type parameter '{name}' is not declared"
                        | ParsedNamedType(typeName, range) ->
                            match
                                references.Resolve(
                                    parsed.Namespace,
                                    parsed.OpenedNamespaces,
                                    typeName
                                )
                            with
                            | Ok resolved -> Ok(TypedNamedType resolved)
                            | Error message -> diagnostic range message
                        | ParsedFunctionType(domain, range, _) ->
                            match resolveType domain with
                            | Error error -> Error error
                            | Ok typedDomain ->
                                resolveType range
                                |> Result.map (fun typedRange ->
                                    TypedFunctionType(typedDomain, typedRange)
                                )

                    let resolveConstraint =
                        function
                        | ParsedSubtypeConstraint(typeParameter, superType, range) ->
                            if not (declaredParameters.Contains(typeParameter)) then
                                diagnostic
                                    range
                                    $"the constrained type parameter '{typeParameter}' is not declared"
                            else
                                resolveType superType
                                |> Result.map (fun typedSuperType ->
                                    TypedSubtypeConstraint(typeParameter, typedSuperType)
                                )
                        | ParsedMemberConstraint(typeParameter, memberName, memberType, range) ->
                            if not (declaredParameters.Contains(typeParameter)) then
                                diagnostic
                                    range
                                    $"the constrained type parameter '{typeParameter}' is not declared"
                            else
                                resolveType memberType
                                |> Result.map (fun typedMemberType ->
                                    TypedMemberConstraint(
                                        typeParameter,
                                        memberName,
                                        typedMemberType
                                    )
                                )

                    let rec resolveConstraints resolved =
                        function
                        | [] -> Ok(List.rev resolved)
                        | constraint' :: remaining ->
                            match resolveConstraint constraint' with
                            | Error error -> Error error
                            | Ok typedConstraint ->
                                resolveConstraints
                                    (typedConstraint :: resolved)
                                    remaining

                    if
                        declaredParameters.Count
                        <> declaration.TypeParameters.Length
                    then
                        diagnostic declaration.Range "generic type parameters must be unique"
                    else
                        match resolveConstraints [] declaration.Constraints with
                        | Error error -> Error error
                        | Ok constraints ->
                            match resolveType declaration.Target.Type with
                            | Error error -> Error error
                            | Ok targetType ->
                                let targetIdentity =
                                    Fingerprint.parts [
                                        stableId
                                        "parameters"
                                        yield! declaration.TypeParameters
                                        "constraints"

                                        yield!
                                            constraints
                                            |> List.map TypeIdentity.constraintIdentity

                                        "target"
                                        TypeIdentity.expression targetType
                                        if declaration.Target.AllowsNull then
                                            "null"
                                        else
                                            "non-null"
                                    ]

                                Ok(
                                    TypedTypeAbbreviation {
                                        StableId = stableId
                                        Name = declaration.Name
                                        TypeParameters = declaration.TypeParameters
                                        Constraints = constraints
                                        TargetType = targetType
                                        AllowsNull = declaration.Target.AllowsNull
                                        ExportFingerprint = targetIdentity
                                        Range = declaration.Range
                                    }
                                )

            let typeAssemblyAttribute index (attribute: ParsedAssemblyAttribute) =
                let attributeTypeName =
                    if String.IsNullOrEmpty(attribute.AttributeType.Namespace) then
                        attribute.AttributeType.Name
                    else
                        attribute.AttributeType.Namespace
                        + "."
                        + attribute.AttributeType.Name

                let typedAttribute kind =
                    let stableId =
                        parsed.StableId
                        + "/assembly-attribute:"
                        + index.ToString()
                        + ":"
                        + attributeTypeName

                    let exportFingerprint =
                        Fingerprint.parts [
                            attributeTypeName

                            yield! attribute.ConstructorArguments

                            yield!
                                attribute.NamedArguments
                                |> List.collect (fun argument -> [
                                    argument.Name
                                    argument.Value
                                ])
                        ]
                        |> Fingerprint.text

                    Ok {
                        StableId = stableId
                        Kind = kind
                        AttributeType = attribute.AttributeType
                        ConstructorArguments = attribute.ConstructorArguments
                        NamedArguments = attribute.NamedArguments
                        ExportFingerprint = exportFingerprint
                        Range = attribute.Range
                    }

                let tryAttributeKind =
                    function
                    | "System.Runtime.Versioning.TargetFrameworkAttribute" ->
                        Some TargetFrameworkAttribute
                    | "System.Reflection.AssemblyTitleAttribute" ->
                        Some AssemblyTitleAttribute
                    | "System.Reflection.AssemblyProductAttribute" ->
                        Some AssemblyProductAttribute
                    | "System.Reflection.AssemblyVersionAttribute" ->
                        Some AssemblyVersionAttribute
                    | "System.Reflection.AssemblyMetadataAttribute" ->
                        Some AssemblyMetadataAttribute
                    | "System.Reflection.AssemblyFileVersionAttribute" ->
                        Some AssemblyFileVersionAttribute
                    | "System.Reflection.AssemblyInformationalVersionAttribute" ->
                        Some AssemblyInformationalVersionAttribute
                    | _ -> None

                match
                    tryAttributeKind attributeTypeName,
                    attribute.ConstructorArguments,
                    attribute.NamedArguments
                with
                | Some TargetFrameworkAttribute,
                  [ _ ],
                  [ { Name = "FrameworkDisplayName" } ] ->
                    typedAttribute TargetFrameworkAttribute
                | Some AssemblyVersionAttribute, [ value ], [] ->
                    match Version.TryParse(value) with
                    | true, _ -> typedAttribute AssemblyVersionAttribute
                    | false, _ ->
                        Error {
                            Code = "FSC2P1001"
                            Message = "the assembly version is invalid"
                            Path = Some sourcePath
                            Range = Some attribute.Range
                        }
                | Some AssemblyTitleAttribute, [ _ ], [] ->
                    typedAttribute AssemblyTitleAttribute
                | Some AssemblyProductAttribute, [ _ ], [] ->
                    typedAttribute AssemblyProductAttribute
                | Some AssemblyFileVersionAttribute, [ _ ], [] ->
                    typedAttribute AssemblyFileVersionAttribute
                | Some AssemblyInformationalVersionAttribute, [ _ ], [] ->
                    typedAttribute AssemblyInformationalVersionAttribute
                | Some AssemblyMetadataAttribute, [ _; _ ], [] ->
                    typedAttribute AssemblyMetadataAttribute
                | Some TargetFrameworkAttribute, _, _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message =
                            "the generated target-framework attribute has unsupported named arguments"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }
                | Some _, _, _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message = "the assembly attribute has unsupported arguments"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }
                | None, _, _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message = "unsupported assembly attribute"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }

            let rec collectResults completed remaining =
                match remaining with
                | [] -> Ok(List.rev completed)
                | result :: tail ->
                    match result with
                    | Error diagnostic -> Error diagnostic
                    | Ok value ->
                        collectResults
                            (value
                             :: completed)
                            tail

            let typedAssemblyAttributes =
                parsed.AssemblyAttributes
                |> List.mapi typeAssemblyAttribute
                |> collectResults []

            match typedAssemblyAttributes with
            | Error diagnostic -> Error diagnostic
            | Ok assemblyAttributes ->
                let typedDeclarations =
                    parsed.Declarations
                    |> List.map typeDeclaration
                    |> collectResults []

                match typedDeclarations with
                | Error diagnostic -> Error diagnostic
                | Ok declarations ->
                    let typed = {
                        StableId = parsed.StableId
                        Namespace = parsed.Namespace
                        Name = parsed.Name
                        IsPublic = parsed.IsPublic
                        SourceChecksum = parsed.SourceChecksum
                        ContentFingerprint = parsed.ContentFingerprint
                        AssemblyAttributes = assemblyAttributes
                        Declarations = declarations
                        ExportFingerprint =
                            [
                                parsed.StableId

                                if parsed.IsPublic then
                                    "public"
                                else
                                    "internal"

                                yield!
                                    assemblyAttributes
                                    |> List.map _.ExportFingerprint

                                yield!
                                    declarations
                                    |> List.map _.ExportFingerprint
                            ]
                            |> String.concat "|"
                            |> Fingerprint.text
                    }

                    checkCache.Add(key, typed)
                    Ok(typed, key)

    let lower (assemblyName: string) (typedModules: TypedModule list) =
        let modulesWithContentHashes =
            typedModules
            |> List.map (fun typed ->
                let declarationsWithContentHashes =
                    typed.Declarations
                    |> List.map (fun declaration ->
                        let implementation =
                            match declaration with
                            | TypedMethod methodDeclaration ->
                                match methodDeclaration.Body with
                                | TypedIntegerLiteral value ->
                                    methodDeclaration.StableId
                                    + "="
                                    + value.ToString()
                            | TypedLiteralField fieldDeclaration ->
                                fieldDeclaration.StableId
                                + "="
                                + fieldDeclaration.Value
                            | TypedTypeAbbreviation typeDeclaration ->
                                Fingerprint.parts [
                                    typeDeclaration.StableId
                                    "parameters"
                                    yield! typeDeclaration.TypeParameters
                                    "constraints"

                                    yield!
                                        typeDeclaration.Constraints
                                        |> List.map TypeIdentity.constraintIdentity

                                    "target"
                                    TypeIdentity.expression typeDeclaration.TargetType

                                    if typeDeclaration.AllowsNull then
                                        "null"
                                    else
                                        "non-null"
                                ]

                        declaration, Fingerprint.text implementation
                    )

                typed, declarationsWithContentHashes
            )

        let assemblyAttributesWithContentHashes =
            typedModules
            |> List.collect (fun typed ->
                typed.AssemblyAttributes
                |> List.map (fun attribute -> attribute, attribute.ExportFingerprint)
            )

        let isAssemblyVersionAttribute (attribute: TypedAssemblyAttribute) =
            attribute.Kind = AssemblyVersionAttribute

        let assemblyVersion =
            assemblyAttributesWithContentHashes
            |> List.tryPick (fun (attribute, _) ->
                if isAssemblyVersionAttribute attribute then
                    attribute.ConstructorArguments
                    |> List.tryHead
                else
                    None
            )
            |> Option.map (fun value ->
                let parsed = Version.Parse(value)

                Version(parsed.Major, parsed.Minor, max 0 parsed.Build, max 0 parsed.Revision)
            )
            |> Option.defaultValue (Version(1, 0, 0, 0))

        let implementationFingerprint =
            modulesWithContentHashes
            |> List.collect (
                snd
                >> List.map snd
            )
            |> String.concat "|"
            |> Fingerprint.text

        let contentFingerprint =
            typedModules
            |> List.map _.ContentFingerprint
            |> String.concat "|"
            |> Fingerprint.text

        let debugFingerprint =
            typedModules
            |> List.map (fun typed ->
                typed.SourceChecksum
                |> Seq.toArray
                |> Convert.ToHexString
            )
            |> Fingerprint.parts

        let key =
            Fingerprint.parts [
                querySchema.ToString(CultureInfo.InvariantCulture)
                assemblyName
                implementationFingerprint
                contentFingerprint
                debugFingerprint
            ]

        match lowerCache.TryGetValue(key) with
        | true, symbolic ->
            lowerHits <-
                lowerHits
                + 1

            symbolic, key
        | false, _ ->
            lowerMisses <-
                lowerMisses
                + 1

            let assemblyStableId =
                "assembly:"
                + assemblyName

            let moduleName =
                assemblyName
                + ".dll"

            let moduleStableId =
                assemblyStableId
                + "/module:"
                + moduleName

            let documents =
                typedModules
                |> List.mapi (fun documentIndex typed -> {
                    SchemaVersion = querySchema
                    StableId =
                        moduleStableId
                        + "/document:"
                        + documentIndex.ToString()
                    Checksum = typed.SourceChecksum
                })

            let assemblyAttributes =
                assemblyAttributesWithContentHashes
                |> List.filter (
                    fst
                    >> isAssemblyVersionAttribute
                    >> not
                )
                |> List.map (fun (attribute, contentHash) -> {
                    SchemaVersion = querySchema
                    StableId = attribute.StableId
                    Kind = attribute.Kind
                    AttributeType = attribute.AttributeType
                    ConstructorArguments = attribute.ConstructorArguments
                    NamedArguments =
                        attribute.NamedArguments
                        |> List.map (fun argument -> {
                            Name = argument.Name
                            Value = argument.Value
                        })
                    ContentHash = contentHash
                })

            let typeAbbreviations =
                modulesWithContentHashes
                |> List.collect (fun (_, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, contentHash) ->
                        match declaration with
                        | TypedTypeAbbreviation typeDeclaration ->
                            Some {
                                SchemaVersion = querySchema
                                StableId = typeDeclaration.StableId
                                Name = typeDeclaration.Name
                                TypeParameters = typeDeclaration.TypeParameters
                                Constraints = typeDeclaration.Constraints
                                TargetType = typeDeclaration.TargetType
                                AllowsNull = typeDeclaration.AllowsNull
                                ContentHash = contentHash
                            }
                        | TypedMethod _
                        | TypedLiteralField _ -> None
                    )
                )

            let types =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let typeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let literalFields =
                        declarationsWithContentHashes
                        |> List.choose (fun (declaration, contentHash) ->
                            match declaration with
                            | TypedLiteralField fieldDeclaration ->
                                Some {
                                    SchemaVersion = querySchema
                                    StableId =
                                        typeStableId
                                        + "/field:"
                                        + fieldDeclaration.StableId
                                    Name = fieldDeclaration.Name
                                    Value = fieldDeclaration.Value
                                    ContentHash = contentHash
                                }
                            | TypedMethod _
                            | TypedTypeAbbreviation _ -> None
                        )

                    let methods =
                        declarationsWithContentHashes
                        |> List.choose (fun (declaration, contentHash) ->
                            match declaration with
                            | TypedMethod methodDeclaration ->
                                let value =
                                    match methodDeclaration.Body with
                                    | TypedIntegerLiteral value -> value

                                let instructions = [
                                    LoadInt32 value
                                    Return
                                ]

                                Some {
                                    SchemaVersion = querySchema
                                    StableId =
                                        typeStableId
                                        + "/method:"
                                        + methodDeclaration.StableId
                                    Name = methodDeclaration.Name
                                    ReturnType = methodDeclaration.ReturnType
                                    Instructions = instructions
                                    DependencyIds = []
                                    ContentHash = contentHash
                                    DocumentIndex = documentIndex
                                    DocumentChecksum = typed.SourceChecksum
                                    Range = methodDeclaration.Range
                                }
                            | TypedLiteralField _
                            | TypedTypeAbbreviation _ -> None
                        )

                    if List.isEmpty literalFields && List.isEmpty methods then
                        None
                    else
                        Some {
                            SchemaVersion = querySchema
                            StableId = typeStableId
                            Namespace = typed.Namespace
                            Name = typed.Name
                            IsPublic = typed.IsPublic
                            LiteralFields = literalFields
                            Methods = methods
                        }
                )
                |> List.choose id

            let symbolic: SymbolicAssembly = {
                SchemaVersion = querySchema
                StableId = assemblyStableId
                AssemblyName = assemblyName
                AssemblyVersion = assemblyVersion
                PublicFingerprint =
                    typedModules
                    |> List.map _.ExportFingerprint
                    |> String.concat "|"
                    |> Fingerprint.text
                Documents = documents
                AssemblyAttributes = assemblyAttributes
                Module = {
                    SchemaVersion = querySchema
                    StableId = moduleStableId
                    Name = moduleName
                    TypeAbbreviations = typeAbbreviations
                    Types = types
                }
            }

            lowerCache.Add(key, symbolic)
            symbolic, key

    member _.Compile(
        assemblyName: string,
        defines: string list,
        references: ReferenceTypeIndex,
        sources: SourceInput list
    ) =
        let combine values =
            match values with
            | [ value ] -> value
            | _ ->
                values
                |> String.concat "|"
                |> Fingerprint.text

        let stateKey =
            assemblyName
            + "\n"
            + references.Fingerprint
            + "\n"
            + (sources
               |> List.map _.Path
               |> String.concat "\n")

        let previousContentFingerprint =
            match lastSuccessfulContent.TryGetValue(stateKey) with
            | true, fingerprint -> fingerprint
            | false, _ -> String.Empty

        let before = {
            ParseHits = parseHits
            ParseMisses = parseMisses
            CheckHits = checkHits
            CheckMisses = checkMisses
            LowerHits = lowerHits
            LowerMisses = lowerMisses
        }

        let decision hitsBefore hitsAfter missesBefore missesAfter =
            let hitCount =
                hitsAfter
                - hitsBefore

            let missCount =
                missesAfter
                - missesBefore

            if
                hitCount > 0
                && missCount > 0
            then
                "partial"
            elif hitCount > 0 then
                "hit"
            else
                "miss"

        let rec parseAll parsed keys remaining =
            match remaining with
            | [] -> Ok(List.rev parsed, List.rev keys)
            | source :: tail ->
                match parse defines source with
                | Error diagnostic -> Error diagnostic
                | Ok(parsedModule, key) ->
                    parseAll
                        ((source, parsedModule)
                         :: parsed)
                        (key
                         :: keys)
                        tail

        let rec checkAll typed keys remaining =
            match remaining with
            | [] -> Ok(List.rev typed, List.rev keys)
            | (source, parsedModule) :: tail ->
                match check references source.Path parsedModule with
                | Error diagnostic -> Error diagnostic
                | Ok(typedModule, key) ->
                    checkAll
                        (typedModule
                         :: typed)
                        (key
                         :: keys)
                        tail

        let parseStarted = Stopwatch.GetTimestamp()

        match parseAll [] [] sources with
        | Error error -> Error error
        | Ok(parsedModules, parseKeys) ->
            let parseElapsedMicroseconds = elapsedMicroseconds parseStarted
            let checkStarted = Stopwatch.GetTimestamp()

            match checkAll [] [] parsedModules with
            | Error diagnostic -> Error diagnostic
            | Ok(typedModules, checkKeys) ->
                let checkElapsedMicroseconds = elapsedMicroseconds checkStarted
                let lowerStarted = Stopwatch.GetTimestamp()
                let symbolic, lowerKey = lower assemblyName typedModules
                let lowerElapsedMicroseconds = elapsedMicroseconds lowerStarted

                let contentFingerprint =
                    references.Fingerprint
                    :: (parsedModules
                        |> List.map (fun (_, parsedModule) -> parsedModule.ContentFingerprint))
                    |> combine

                let invalidationReason =
                    if previousContentFingerprint.Length = 0 then
                        "no-prior-state"
                    elif previousContentFingerprint = contentFingerprint then
                        "unchanged"
                    else
                        "source-content-changed"

                lastSuccessfulContent.[stateKey] <- contentFingerprint

                Ok {
                    SymbolicAssembly = symbolic
                    QuerySchema = querySchema
                    NodeKind = if sources.Length = 1 then "source" else "project"
                    ContentFingerprint = contentFingerprint
                    PreviousContentFingerprint = previousContentFingerprint
                    InvalidationReason = invalidationReason
                    ParseKey = combine parseKeys
                    CheckKey = combine checkKeys
                    LowerKey = lowerKey
                    DependencyCount =
                        symbolic.Module.TypeAbbreviations.Length
                        + (symbolic.Module.Types
                           |> List.sumBy (fun typeFragment ->
                               typeFragment.Methods
                               |> List.sumBy (fun methodFragment ->
                                   methodFragment.DependencyIds.Length
                               )
                            )
                          )
                    ParseDecision =
                        decision before.ParseHits parseHits before.ParseMisses parseMisses
                    CheckDecision =
                        decision before.CheckHits checkHits before.CheckMisses checkMisses
                    LowerDecision =
                        decision before.LowerHits lowerHits before.LowerMisses lowerMisses
                    ParseElapsedMicroseconds = parseElapsedMicroseconds
                    CheckElapsedMicroseconds = checkElapsedMicroseconds
                    LowerElapsedMicroseconds = lowerElapsedMicroseconds
                }

    member _.Statistics = {
        ParseHits = parseHits
        ParseMisses = parseMisses
        CheckHits = checkHits
        CheckMisses = checkMisses
        LowerHits = lowerHits
        LowerMisses = lowerMisses
    }
