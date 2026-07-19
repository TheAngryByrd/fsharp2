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

    let sourceStateKey assemblyName (source: SourceInput) =
        assemblyName
        + "\n"
        + source.Path

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

            let rec typeDeclarations typed remaining =
                match remaining with
                | [] -> Ok(List.rev typed)
                | declaration :: tail ->
                    match typeDeclaration declaration with
                    | Error diagnostic -> Error diagnostic
                    | Ok declaration ->
                        typeDeclarations
                            (declaration
                             :: typed)
                            tail

            match typeDeclarations [] parsed.Declarations with
            | Error diagnostic -> Error diagnostic
            | Ok declarations ->
                let typed = {
                    Name = parsed.Name
                    SourceChecksum = parsed.SourceChecksum
                    ContentFingerprint = parsed.ContentFingerprint
                    Declarations = declarations
                    ExportFingerprint =
                        declarations
                        |> List.map (fun declaration -> declaration.ExportFingerprint)
                        |> String.concat "|"
                        |> Fingerprint.text
                }

                checkCache.Add(key, typed)
                Ok(typed, key)

    let lower (assemblyName: string) (typed: TypedModule) =
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

        let implementationFingerprint =
            declarationsWithContentHashes
            |> List.map snd
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
                + typed.ContentFingerprint
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
                        DocumentChecksum = typed.SourceChecksum
                        Range = declaration.Range
                    }
                )

            let symbolic: SymbolicAssembly = {
                SchemaVersion = querySchema
                StableId = assemblyStableId
                AssemblyName = assemblyName
                PublicFingerprint = typed.ExportFingerprint
                Module = {
                    SchemaVersion = querySchema
                    StableId = moduleStableId
                    Name = moduleName
                    Types = [
                        {
                            SchemaVersion = querySchema
                            StableId = typeStableId
                            Namespace = String.Empty
                            Name = typed.Name
                            Methods = methods
                        }
                    ]
                }
            }

            lowerCache.Add(key, symbolic)
            symbolic, key

    member _.Compile(assemblyName: string, source: SourceInput) =
        let stateKey = sourceStateKey assemblyName source

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

        let parseStarted = Stopwatch.GetTimestamp()

        match parse source with
        | Error error -> Error error
        | Ok(parsed, parseKey) ->
            let parseElapsedMicroseconds = elapsedMicroseconds parseStarted
            let checkStarted = Stopwatch.GetTimestamp()

            match check source.Path parsed with
            | Error diagnostic -> Error diagnostic
            | Ok(typed, checkKey) ->
                let checkElapsedMicroseconds = elapsedMicroseconds checkStarted
                let lowerStarted = Stopwatch.GetTimestamp()
                let symbolic, lowerKey = lower assemblyName typed
                let lowerElapsedMicroseconds = elapsedMicroseconds lowerStarted

                let invalidationReason =
                    if previousContentFingerprint.Length = 0 then
                        "no-prior-state"
                    elif previousContentFingerprint = parsed.ContentFingerprint then
                        "unchanged"
                    else
                        "source-content-changed"

                lastSuccessfulContent.[stateKey] <- parsed.ContentFingerprint

                Ok {
                    SymbolicAssembly = symbolic
                    QuerySchema = querySchema
                    NodeKind = "source"
                    ContentFingerprint = parsed.ContentFingerprint
                    PreviousContentFingerprint = previousContentFingerprint
                    InvalidationReason = invalidationReason
                    ParseKey = parseKey
                    CheckKey = checkKey
                    LowerKey = lowerKey
                    DependencyCount =
                        symbolic.Module.Types
                        |> List.sumBy (fun typeFragment ->
                            typeFragment.Methods
                            |> List.sumBy (fun methodFragment ->
                                methodFragment.DependencyIds.Length
                            )
                        )
                    ParseDecision = if parseHits > before.ParseHits then "hit" else "miss"
                    CheckDecision = if checkHits > before.CheckHits then "hit" else "miss"
                    LowerDecision = if lowerHits > before.LowerHits then "hit" else "miss"
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
