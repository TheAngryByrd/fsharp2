namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.IO

module internal CommandLine =
    type private PathMap = {
        SourceRoot: string
        TargetRoot: string
    }

    with
        override _.ToString() = "PathMap"

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

    let private parsePathMaps (pathMaps: ResizeArray<PathMap>) (value: string) =
        for mapping in value.Split(';', StringSplitOptions.RemoveEmptyEntries) do
            let separator = mapping.IndexOf('=')

            if
                separator <= 0
                || separator
                   = mapping.Length - 1
            then
                invalidArg "value" "path maps must use '<source>=<target>'"

            pathMaps.Add {
                SourceRoot =
                    mapping.Substring(0, separator)
                    |> Path.GetFullPath
                    |> fun root ->
                        root.TrimEnd(
                            Path.DirectorySeparatorChar,
                            Path.AltDirectorySeparatorChar
                        )
                TargetRoot = mapping.Substring(separator + 1).Replace('\\', '/')
            }

    let private mapDocumentPath
        (pathMaps: ResizeArray<PathMap>)
        (sourcePath: string)
        =
        let comparison =
            if OperatingSystem.IsWindows() then
                StringComparison.OrdinalIgnoreCase
            else
                StringComparison.Ordinal

        pathMaps
        |> Seq.sortByDescending (fun mapping -> mapping.SourceRoot.Length)
        |> Seq.tryPick (fun mapping ->
            let sourceRoot = mapping.SourceRoot

            let matches =
                String.Equals(sourcePath, sourceRoot, comparison)
                || (sourcePath.StartsWith(sourceRoot, comparison)
                    && sourcePath.Length > sourceRoot.Length
                    && (sourcePath.[sourceRoot.Length] = Path.DirectorySeparatorChar
                        || sourcePath.[sourceRoot.Length] = Path.AltDirectorySeparatorChar))

            if matches then
                let relativePath =
                    sourcePath
                        .Substring(sourceRoot.Length)
                        .TrimStart(
                            Path.DirectorySeparatorChar,
                            Path.AltDirectorySeparatorChar
                        )
                        .Replace('\\', '/')

                let targetRoot = mapping.TargetRoot.TrimEnd('/')

                if relativePath.Length = 0 then
                    Some targetRoot
                elif targetRoot.Length = 0 then
                    Some("/" + relativePath)
                else
                    Some(
                        targetRoot
                        + "/"
                        + relativePath
                    )
            else
                None
        )
        |> Option.defaultWith (fun () -> Path.GetFileName(sourcePath))

    let private parseManagedResource (value: string) =
        let parts = value.Split(',')

        if
            parts.Length = 0
            || parts.Length > 3
            || String.IsNullOrWhiteSpace(parts.[0])
        then
            invalidArg "value" "resources must use '<path>[,<name>[,public|private]]'"

        let path = Path.GetFullPath(parts.[0] |> trimQuotes)

        let logicalName =
            if
                parts.Length >= 2
                && not (String.IsNullOrWhiteSpace(parts.[1]))
            then
                parts.[1]
            else
                Path.GetFileName(path)

        let isPublic =
            if parts.Length < 3 then
                true
            elif parts.[2].Equals("public", StringComparison.OrdinalIgnoreCase) then
                true
            elif parts.[2].Equals("private", StringComparison.OrdinalIgnoreCase) then
                false
            else
                invalidArg "value" "resource visibility must be 'public' or 'private'"

        {
            LogicalName = logicalName
            IsPublic = isPublic
            Data = File.ReadAllBytes(path)
        }

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
            let mutable managedResource: ManagedResourceInput option = None
            let mutable nativeResourceData = Array.empty<byte>
            let mutable keyFilePath: string option = None
            let mutable delaySign = false
            let mutable publicSign = false
            let sources = ResizeArray<string>()
            let pathMaps = ResizeArray<PathMap>()
            let unsupported = ResizeArray<string>()

            for argument in expanded do
                match optionValue "--fsharp2-server:" argument with
                | Some value -> serverName <- Some value
                | None ->
                    match optionValue "--fsharp2-trace:" argument with
                    | Some value -> tracePath <- Some(Path.GetFullPath(value))
                    | None ->
                        match optionValue "--pathmap:" argument with
                        | Some value -> parsePathMaps pathMaps value
                        | None ->
                            match optionValue "--sourcelink:" argument with
                            | Some value -> sourceLinkPath <- Some(Path.GetFullPath(value))
                            | None ->
                                match optionValue "--resource:" argument with
                                | Some value -> managedResource <- Some(parseManagedResource value)
                                | None ->
                                    match optionValue "--fsharp2-native-resource:" argument with
                                    | Some value ->
                                        nativeResourceData <-
                                            value
                                            |> Path.GetFullPath
                                            |> File.ReadAllBytes
                                    | None ->
                                        match optionValue "--keyfile:" argument with
                                        | Some value -> keyFilePath <- Some(Path.GetFullPath(value))
                                        | None when
                                            argument.Equals(
                                                "--delaysign+",
                                                StringComparison.OrdinalIgnoreCase
                                            )
                                            ->
                                            delaySign <- true
                                        | None when
                                            argument.Equals(
                                                "--publicsign+",
                                                StringComparison.OrdinalIgnoreCase
                                            )
                                            ->
                                            publicSign <- true
                                        | None ->
                                            match optionValue "--out:" argument with
                                            | Some value ->
                                                assemblyPath <- Some(Path.GetFullPath(value))
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
                                                | None when
                                                    argument.StartsWith(
                                                        "-",
                                                        StringComparison.Ordinal
                                                    )
                                                    ->
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
            elif delaySign && publicSign then
                Error("--delaysign+ and --publicsign+ are mutually exclusive")
            elif
                (delaySign || publicSign)
                && keyFilePath.IsNone
            then
                Error("signing mode requires --keyfile:<path>")
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

                    let strongNameMode, strongNameKey =
                        match keyFilePath with
                        | None -> Unsigned, Array.empty
                        | Some path when delaySign -> DelaySign, File.ReadAllBytes(path)
                        | Some path when publicSign -> PublicSign, File.ReadAllBytes(path)
                        | Some path -> FullSign, File.ReadAllBytes(path)

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
                        DebugDocumentPath = mapDocumentPath pathMaps sources.[0]
                        ManagedResource = managedResource
                        NativeResourceData = nativeResourceData
                        StrongNameMode = strongNameMode
                        StrongNameKey = strongNameKey
                        FullPaths = fullPaths
                        FlatErrors = flatErrors
                        Utf8Output = utf8Output
                        ServerName = serverName
                        TracePath = tracePath
                    }
        with ex ->
            Error ex.Message
