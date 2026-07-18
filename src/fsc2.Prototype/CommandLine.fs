namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.IO

module internal CommandLine =
    let private trimQuotes (value: string) =
        if
            value.Length
            >= 2
            && value.[0] = '"'
            && value.[value.Length
                      - 1] = '"'
        then
            value.Substring(
                1,
                value.Length
                - 2
            )
        else
            value

    let rec private expandArgument (expanded: ResizeArray<string>) (argument: string) =
        if argument.StartsWith("@", StringComparison.Ordinal) then
            let responsePath =
                argument.Substring(1)
                |> trimQuotes
                |> Path.GetFullPath

            for line in File.ReadLines(responsePath) do
                if
                    line.Length > 0
                    && line.[0]
                       <> '#'
                then
                    let candidate = line.Trim()

                    if candidate.Length > 0 then
                        expandArgument expanded candidate
        else
            expanded.Add(argument)

    let private optionValue (prefix: string) (argument: string) =
        if argument.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
            Some(
                argument.Substring(prefix.Length)
                |> trimQuotes
            )
        else
            None

    let parse (arguments: string array) =
        try
            let expanded = ResizeArray<string>()

            for argument in arguments do
                expandArgument expanded argument

            let mutable assemblyPath: string option = None
            let mutable pdbPath: string option = None
            let mutable deterministic = false
            let mutable portablePdb = false
            let mutable sourceLinkPath: string option = None
            let mutable fullPaths = false
            let mutable flatErrors = false
            let mutable utf8Output = false
            let mutable targetIsLibrary = false
            let mutable serverName: string option = None
            let mutable tracePath: string option = None
            let sources = ResizeArray<string>()
            let unsupported = ResizeArray<string>()

            for argument in expanded do
                match optionValue "--fsharp2-server:" argument with
                | Some value -> serverName <- Some value
                | None ->
                    match optionValue "--fsharp2-trace:" argument with
                    | Some value -> tracePath <- Some(Path.GetFullPath(value))
                    | None ->
                        match optionValue "--sourcelink:" argument with
                        | Some value -> sourceLinkPath <- Some(Path.GetFullPath(value))
                        | None ->
                            match optionValue "--out:" argument with
                            | Some value -> assemblyPath <- Some(Path.GetFullPath(value))
                            | None ->
                                match optionValue "--pdb:" argument with
                                | Some value -> pdbPath <- Some(Path.GetFullPath(value))
                                | None when
                                    argument.Equals(
                                        "--target:library",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    targetIsLibrary <- true
                                | None when
                                    argument.Equals(
                                        "--deterministic+",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    deterministic <- true
                                | None when
                                    argument.Equals(
                                        "--debug:portable",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    portablePdb <- true
                                | None when
                                    argument.Equals(
                                        "--fullpaths",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    fullPaths <- true
                                | None when
                                    argument.Equals(
                                        "--flaterrors",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    flatErrors <- true
                                | None when
                                    argument.Equals(
                                        "--utf8output",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    utf8Output <- true
                                | None when
                                    argument.Equals(
                                        "--nologo",
                                        StringComparison.OrdinalIgnoreCase
                                    )
                                    ->
                                    ()
                                | None when argument.StartsWith("-", StringComparison.Ordinal) ->
                                    unsupported.Add(argument)
                                | None ->
                                    sources.Add(
                                        Path.GetFullPath(
                                            argument
                                            |> trimQuotes
                                        )
                                    )

            if unsupported.Count > 0 then
                Error(
                    "unsupported prototype option: "
                    + String.Join(", ", unsupported)
                )
            elif not targetIsLibrary then
                Error("the prototype supports only --target:library")
            elif not deterministic then
                Error("the prototype requires --deterministic+")
            elif not portablePdb then
                Error("the prototype requires --debug:portable")
            elif
                sources.Count
                <> 1
            then
                Error("the prototype currently requires exactly one source file")
            else
                match assemblyPath with
                | None -> Error("missing required --out:<path> option")
                | Some output ->
                    let pdb =
                        pdbPath
                        |> Option.defaultValue (Path.ChangeExtension(output, ".pdb"))

                    Ok {
                        AssemblyPath = output
                        PdbPath = pdb
                        SourcePaths = List.ofSeq sources
                        Deterministic = deterministic
                        PortablePdb = portablePdb
                        SourceLinkJson =
                            sourceLinkPath
                            |> Option.map File.ReadAllBytes
                            |> Option.defaultValue Array.empty
                        FullPaths = fullPaths
                        FlatErrors = flatErrors
                        Utf8Output = utf8Output
                        ServerName = serverName
                        TracePath = tracePath
                    }
        with ex ->
            Error ex.Message
