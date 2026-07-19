namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Diagnostics
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

/// A deliberately small in-memory query owner. Query identities and cached
/// values are semantic/compiler state; final SRM state never enters these maps.
type internal CompilerService() =
    let querySchema = 1
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

    let parse (source: SourceInput) =
        let key =
            Fingerprint.text (
                querySchema.ToString()
                + "\n"
                + Path.GetFileName(source.Path)
                + "\n"
                + source.Text
            )

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

            match Frontend.parse source with
            | Error error -> Error error
            | Ok parsed ->
                parseCache.Add(key, parsed)
                Ok(parsed, key)

    let check (sourcePath: string) (parsed: ParsedModule) =
        let key =
            Fingerprint.text (
                querySchema.ToString()
                + "|"
                + parsed.Name
                + "|"
                + parsed.ContentFingerprint
            )

        match checkCache.TryGetValue(key) with
        | true, typed ->
            checkHits <-
                checkHits
                + 1

            Ok(typed, key)
        | false, _ ->
            checkMisses <-
                checkMisses
                + 1

            let typeDeclaration declaration =
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
                        parsed.Name
                        + "."
                        + declaration.Name
                        + (if declaration.IsUnitFunction then
                               ":unit->int32"
                           else
                               ":int32")

                    let exportFingerprint = Fingerprint.text stableId

                    Ok {
                        StableId = stableId
                        Name = declaration.Name
                        ReturnType = ValueType.Int32
                        Body = TypedIntegerLiteral value
                        ExportFingerprint = exportFingerprint
                        Range = declaration.Range
                    }
                | None, StringLiteral _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message =
                            "string-valued declarations are not yet supported by the prototype"
                        Path = Some sourcePath
                        Range = Some declaration.BodyRange
                    }

            let typeAssemblyAttribute index (attribute: ParsedAssemblyAttribute) =
                let attributeTypeName =
                    if String.IsNullOrEmpty(attribute.AttributeType.Namespace) then
                        attribute.AttributeType.Name
                    else
                        attribute.AttributeType.Namespace
                        + "."
                        + attribute.AttributeType.Name

                match attributeTypeName, attribute.NamedArguments with
                | "System.Runtime.Versioning.TargetFrameworkAttribute",
                  [ { Name = "FrameworkDisplayName" } ] ->
                    let stableId =
                        parsed.Name
                        + "/assembly-attribute:"
                        + index.ToString()
                        + ":"
                        + attributeTypeName

                    let exportFingerprint =
                        String.concat "\n" [
                            attributeTypeName
                            attribute.ConstructorArgument

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
                        AttributeType = attribute.AttributeType
                        ConstructorArgument = attribute.ConstructorArgument
                        NamedArguments = attribute.NamedArguments
                        ExportFingerprint = exportFingerprint
                        Range = attribute.Range
                    }
                | "System.Runtime.Versioning.TargetFrameworkAttribute", _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message =
                            "the generated target-framework attribute has unsupported named arguments"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }
                | _ ->
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
                        Name = parsed.Name
                        SourceChecksum = parsed.SourceChecksum
                        ContentFingerprint = parsed.ContentFingerprint
                        AssemblyAttributes = assemblyAttributes
                        Declarations = declarations
                        ExportFingerprint =
                            List.append
                                (assemblyAttributes
                                 |> List.map _.ExportFingerprint)
                                (declarations
                                 |> List.map _.ExportFingerprint)
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
                            match declaration.Body with
                            | TypedIntegerLiteral value ->
                                declaration.StableId
                                + "="
                                + value.ToString()

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

        let key =
            Fingerprint.text (
                querySchema.ToString()
                + "|"
                + assemblyName
                + "|"
                + implementationFingerprint
                + "|"
                + contentFingerprint
            )

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
                |> List.map (fun (attribute, contentHash) -> {
                    SchemaVersion = querySchema
                    StableId = attribute.StableId
                    AttributeType = attribute.AttributeType
                    ConstructorArgument = attribute.ConstructorArgument
                    NamedArguments =
                        attribute.NamedArguments
                        |> List.map (fun argument -> {
                            Name = argument.Name
                            Value = argument.Value
                        })
                    ContentHash = contentHash
                })

            let types =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    if List.isEmpty declarationsWithContentHashes then
                        None
                    else
                        let typeStableId =
                            moduleStableId
                            + "/type:"
                            + typed.Name

                        let methods =
                            declarationsWithContentHashes
                            |> List.map (fun (declaration, contentHash) ->
                                let instructions =
                                    match declaration.Body with
                                    | TypedIntegerLiteral value -> [
                                        LoadInt32 value
                                        Return
                                      ]

                                {
                                    SchemaVersion = querySchema
                                    StableId =
                                        typeStableId
                                        + "/method:"
                                        + declaration.StableId
                                    Name = declaration.Name
                                    ReturnType = declaration.ReturnType
                                    Instructions = instructions
                                    DependencyIds = []
                                    ContentHash = contentHash
                                    DocumentIndex = documentIndex
                                    DocumentChecksum = typed.SourceChecksum
                                    Range = declaration.Range
                                }
                            )

                        Some {
                            SchemaVersion = querySchema
                            StableId = typeStableId
                            Namespace = String.Empty
                            Name = typed.Name
                            Methods = methods
                        }
                )
                |> List.choose id

            let symbolic: SymbolicAssembly = {
                SchemaVersion = querySchema
                StableId = assemblyStableId
                AssemblyName = assemblyName
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
                    Types = types
                }
            }

            lowerCache.Add(key, symbolic)
            symbolic, key

    member _.Compile(assemblyName: string, sources: SourceInput list) =
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
                match parse source with
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
                match check source.Path parsedModule with
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
                    parsedModules
                    |> List.map (fun (_, parsedModule) -> parsedModule.ContentFingerprint)
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
                        symbolic.Module.Types
                        |> List.sumBy (fun typeFragment ->
                            typeFragment.Methods
                            |> List.sumBy (fun methodFragment ->
                                methodFragment.DependencyIds.Length
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
