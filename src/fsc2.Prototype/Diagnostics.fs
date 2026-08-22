namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Globalization
open System.IO
open System.Resources
open System.Text
open System.Text.RegularExpressions

module internal DiagnosticPolicy =
    type Input = {
        Fact: DiagnosticFact
        LanguageFeatureEnabled: bool
        Diagnostic: CompilationDiagnostic
    }

    let input warningLevel offByDefault languageFeatureEnabled (diagnostic: CompilationDiagnostic) = {
        Fact =
            DiagnosticFact.Create(
                DiagnosticKey.create diagnostic.Code,
                diagnostic.Stage,
                diagnostic.OriginalSeverity,
                warningLevel,
                offByDefault,
                diagnostic.LogicalPath,
                diagnostic.Range,
                [||],
                [||],
                [||],
                None,
                None,
                diagnostic.Occurrence
            )
        LanguageFeatureEnabled = languageFeatureEnabled
        Diagnostic = diagnostic
    }

    let private normalizeCode (code: string) =
        let value = code.Trim()

        let numeric =
            if value.StartsWith("FS", StringComparison.OrdinalIgnoreCase) then
                value[2..]
            else
                value

        match Int32.TryParse(numeric) with
        | true, number when
            number
            >= 0
            && number
               <= 9999
            ->
            Some number
        | _ -> None

    let private containsCode codes numericCode =
        codes
        |> Seq.exists (fun code -> normalizeCode code = Some numericCode)

    let private comparePosition left right =
        compare (left.Line, left.Column) (right.Line, right.Column)

    let private containsPosition (scope: SourceRange) (diagnosticRange: SourceRange) =
        comparePosition diagnosticRange.Start scope.Start > 0
        && comparePosition diagnosticRange.Start scope.End < 0

    let private localAction (options: DiagnosticOptions) (diagnostic: CompilationDiagnostic) =
        match diagnostic.LogicalPath with
        | None -> None
        | Some logicalPath ->
            options.LocalWarningDirectives
            |> Seq.filter (fun directive ->
                normalizeCode directive.Code = Some diagnostic.NumericCode
                && String.Equals(directive.LogicalPath, logicalPath, StringComparison.Ordinal)
                && match directive.Range, diagnostic.Range with
                   | None, _ -> true
                   | Some scope, Some diagnosticRange -> containsPosition scope diagnosticRange
                   | Some _, None -> false
            )
            |> Seq.sortByDescending _.Order
            |> Seq.tryHead
            |> Option.map _.Action

    let private emitted
        severity
        fallbackStream
        (diagnostic: CompilationDiagnostic)
        : CompilationDiagnostic =
        {
            diagnostic with
                EffectiveSeverity = severity
                Disposition = DiagnosticDisposition.Emitted
                Suppression = None
                Stream = Some fallbackStream
        }

    let private suppressed reason (diagnostic: CompilationDiagnostic) : CompilationDiagnostic = {
        diagnostic with
            EffectiveSeverity = DiagnosticSeverity.Hidden
            Disposition = DiagnosticDisposition.Suppressed
            Suppression = Some reason
            Stream = None
    }

    let isEffectiveError (diagnostic: CompilationDiagnostic) =
        diagnostic.Disposition = DiagnosticDisposition.Emitted
        && diagnostic.EffectiveSeverity = DiagnosticSeverity.Error

    let evaluate
        (options: DiagnosticOptions)
        diagnosticWarningLevel
        offByDefault
        languageFeatureEnabled
        (diagnostic: CompilationDiagnostic)
        =
        let numericCode = diagnostic.NumericCode
        let localDirective = localAction options diagnostic
        let localWarnon = localDirective = Some LocalWarningDirectiveAction.Enable
        let localNowarn = localDirective = Some LocalWarningDirectiveAction.Disable
        let commandLineWarnon = containsCode options.EnabledWarnings numericCode
        let commandLineNowarn = containsCode options.DisabledWarnings numericCode
        let perCodePromotion = containsCode options.WarningsAsErrors numericCode
        let perCodeDemotion = containsCode options.WarningsNotAsErrors numericCode

        let warnOff =
            commandLineNowarn
            && not localWarnon
            || localNowarn

        let warningLevel =
            options.WarningLevel
            |> Option.defaultValue 3

        let enabled =
            commandLineWarnon
            || (not offByDefault
                && languageFeatureEnabled
                && match diagnostic.OriginalSeverity with
                   | DiagnosticSeverity.Information -> warningLevel > 0
                   | DiagnosticSeverity.Warning ->
                       warningLevel
                       >= diagnosticWarningLevel
                   | _ -> false)

        let hiddenReason () =
            if localNowarn then
                DiagnosticSuppression.LocalNowarn
            elif
                commandLineNowarn
                && not localWarnon
            then
                DiagnosticSuppression.GlobalNowarn
            elif not languageFeatureEnabled then
                DiagnosticSuppression.LanguageFeature
            elif
                offByDefault
                && not commandLineWarnon
            then
                DiagnosticSuppression.OffByDefault
            else
                DiagnosticSuppression.WarningLevel

        match diagnostic.OriginalSeverity with
        | DiagnosticSeverity.Error ->
            emitted DiagnosticSeverity.Error DiagnosticStream.StandardError diagnostic
        | DiagnosticSeverity.Warning when
            enabled
            && ((options.TreatWarningsAsErrors
                 && not warnOff)
                || perCodePromotion
                   && not localNowarn)
            && not perCodeDemotion
            ->
            emitted DiagnosticSeverity.Error DiagnosticStream.StandardError diagnostic
        | DiagnosticSeverity.Information when
            perCodePromotion
            && not localNowarn
            ->
            emitted DiagnosticSeverity.Error DiagnosticStream.StandardError diagnostic
        | DiagnosticSeverity.Warning when
            enabled
            && not warnOff
            ->
            emitted DiagnosticSeverity.Warning DiagnosticStream.StandardError diagnostic
        | DiagnosticSeverity.Warning when localWarnon ->
            emitted DiagnosticSeverity.Warning DiagnosticStream.StandardError diagnostic
        | DiagnosticSeverity.Information when
            commandLineWarnon
            && not warnOff
            ->
            emitted DiagnosticSeverity.Warning DiagnosticStream.StandardError diagnostic
        | DiagnosticSeverity.Information when
            enabled
            && not warnOff
            ->
            emitted DiagnosticSeverity.Information DiagnosticStream.StandardOutput diagnostic
        | DiagnosticSeverity.Hidden ->
            suppressed
                (diagnostic.Suppression
                 |> Option.defaultValue DiagnosticSuppression.OffByDefault)
                diagnostic
        | _ -> suppressed (hiddenReason ()) diagnostic

    let apply (options: DiagnosticOptions) (values: System.Collections.IEnumerable) =
        let mutable effectiveErrorCount = 0

        let mutable boundary =
            match options.MaximumErrors with
            | Some 0 -> Some DiagnosticSuppression.MaximumErrors
            | _ -> None

        values
        |> Seq.cast<obj>
        |> Seq.map (
            function
            | :? Input as input -> input
            | :? CompilationDiagnostic as diagnostic -> input None false true diagnostic
            | _ ->
                invalidArg "values" "Policy inputs must be diagnostic occurrences or policy inputs."
        )
        |> Seq.map (fun input ->
            let diagnostic = input.Diagnostic

            match boundary with
            | Some reason -> suppressed reason diagnostic
            | None ->
                let evaluated =
                    evaluate
                        options
                        (input.Fact.WarningLevel
                         |> Option.defaultValue 2)
                        input.Fact.OffByDefault
                        input.LanguageFeatureEnabled
                        diagnostic

                if isEffectiveError evaluated then
                    effectiveErrorCount <-
                        effectiveErrorCount
                        + 1

                    if options.AbortOnError then
                        boundary <- Some DiagnosticSuppression.AbortBoundary
                    elif
                        options.MaximumErrors
                        |> Option.exists (fun maximum ->
                            effectiveErrorCount
                            >= maximum
                        )
                    then
                        boundary <- Some DiagnosticSuppression.MaximumErrors

                evaluated
        )
        |> ImmutableArray.CreateRange

    let outcome currentOutcome (diagnostics: seq<CompilationDiagnostic>) =
        match currentOutcome with
        | CompilationOutcome.Unsupported _
        | CompilationOutcome.Cancelled _ -> currentOutcome
        | CompilationOutcome.Succeeded
        | CompilationOutcome.Failed ->
            if
                diagnostics
                |> Seq.exists isEffectiveError
            then
                CompilationOutcome.Failed
            else
                CompilationOutcome.Succeeded

module internal DiagnosticFormatter =
    let private flattenMessage (message: string) =
        message
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Replace('\r', '\n')
            .Replace('\n', '\u001d')

    let private formatDiagnostic
        (invocation: CompilerInvocation)
        code
        diagnosticMessage
        path
        range
        =
        let message =
            if invocation.FlatErrors then
                flattenMessage diagnosticMessage
            else
                diagnosticMessage

        let location =
            match path, range with
            | Some path, Some range ->
                let displayedPath =
                    if invocation.FullPaths then
                        Path.GetFullPath(path)
                    else
                        path

                displayedPath
                + "("
                + range.Start.Line.ToString()
                + ","
                + range.Start.Column.ToString()
                + "): "
            | _ -> String.Empty

        "\n"
        + location
        + "error "
        + code
        + ": "
        + message

    let format (invocation: CompilerInvocation) (diagnostic: CompilerDiagnostic) =
        formatDiagnostic
            invocation
            diagnostic.Code
            diagnostic.Message
            diagnostic.Path
            diagnostic.Range

    let formatCompilationDiagnostic
        (invocation: CompilerInvocation)
        (diagnostic: CompilationDiagnostic)
        =
        formatDiagnostic
            invocation
            diagnostic.Code
            diagnostic.Message
            diagnostic.LogicalPath
            diagnostic.Range

type internal RenderedDiagnostic private (stream, bytes: byte array) =
    member _.Stream: DiagnosticStream option = stream
    member _.Bytes = Array.copy bytes

    static member Create(stream, bytes) =
        RenderedDiagnostic(stream, Array.copy bytes)

module internal DiagnosticResources =
    let resourceManager =
        ResourceManager(
            "FSharp2.Compiler.Resources.Diagnostics",
            typeof<CompilationDiagnostic>.Assembly
        )

    let private tryCulture (name: string) =
        try
            Some(CultureInfo.GetCultureInfo(name))
        with :? CultureNotFoundException ->
            None

    let private requestCulture (options: DiagnosticOptions) =
        options.PreferredUICulture
        |> Option.bind tryCulture
        |> Option.orElseWith (fun () ->
            options.PreferredUILanguage
            |> Option.bind tryCulture
        )
        |> Option.defaultValue CultureInfo.InvariantCulture

    let private tryGetString (resources: ResourceManager) (key: string) (culture: CultureInfo) =
        try
            resources.GetString(key, culture)
            |> Option.ofObj
        with
        | :? MissingManifestResourceException
        | :? MissingSatelliteAssemblyException
        | :? InvalidOperationException
        | :? BadImageFormatException
        | :? FileLoadException -> None

    let messageWithResources
        (resources: ResourceManager)
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        =
        let key =
            diagnostic.Code
            + ".Message"

        match tryGetString resources key CultureInfo.InvariantCulture with
        | Some neutral when String.Equals(neutral, diagnostic.Message, StringComparison.Ordinal) ->
            tryGetString resources key (requestCulture options)
            |> Option.defaultValue neutral
        | _ -> diagnostic.Message

    let message = messageWithResources resourceManager

module internal DiagnosticRendering =
    let private formattedError =
        Regex(
            "^\\n?(?:(?<path>.+)\\((?<line>[0-9]+),(?<column>[0-9]+)\\): )?error (?<code>[A-Z]+[0-9]+): (?<message>.*)$",
            RegexOptions.CultureInvariant
            ||| RegexOptions.Singleline
        )

    let private flattenMessage (message: string) =
        message
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Replace('\r', '\n')
            .Replace('\n', '\u001d')

    let private severityText (severity: DiagnosticSeverity) =
        match severity with
        | DiagnosticSeverity.Error -> "error"
        | DiagnosticSeverity.Warning -> "warning"
        | DiagnosticSeverity.Information -> "info"
        | DiagnosticSeverity.Hidden -> "hidden"

    let private colorCode (severity: DiagnosticSeverity) =
        match severity with
        | DiagnosticSeverity.Error -> "31"
        | DiagnosticSeverity.Warning -> "33"
        | DiagnosticSeverity.Information -> "36"
        | DiagnosticSeverity.Hidden -> "90"

    let private shouldUseColor (options: DiagnosticOptions) (stream: DiagnosticStream option) =
        match options.ConsoleColorMode with
        | ConsoleColorMode.Enabled -> true
        | ConsoleColorMode.Disabled -> false
        | ConsoleColorMode.Automatic ->
            match stream with
            | Some DiagnosticStream.StandardOutput -> not options.StandardOutputRedirected
            | Some DiagnosticStream.StandardError -> not options.StandardErrorRedirected
            | None -> false

    let private displayedPath (fullPaths: bool) (path: string) =
        if fullPaths then Path.GetFullPath(path) else path

    let private operatingSystemPath fullPaths path =
        (displayedPath fullPaths path).Replace('/', Path.DirectorySeparatorChar)

    let private visualStudioPath fullPaths path =
        (displayedPath fullPaths path).Replace('/', '\\')

    let private emacsPath fullPaths path =
        (displayedPath fullPaths path).Replace('\\', '/')

    let private diagnosticLabel (options: DiagnosticOptions) (diagnostic: CompilationDiagnostic) =
        let value =
            severityText diagnostic.EffectiveSeverity
            + " "
            + diagnostic.Code

        if shouldUseColor options diagnostic.Stream then
            "\u001b["
            + colorCode diagnostic.EffectiveSeverity
            + "m"
            + value
            + "\u001b[0m"
        else
            value

    let private standard
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        (message: string)
        =
        let location =
            match diagnostic.LogicalPath, diagnostic.Range with
            | Some path, Some range ->
                operatingSystemPath options.FullPaths path
                + "("
                + range.Start.Line.ToString(CultureInfo.InvariantCulture)
                + ","
                + range.Start.Column.ToString(CultureInfo.InvariantCulture)
                + "): "
            | _ -> String.Empty

        "\n"
        + location
        + diagnosticLabel options diagnostic
        + ": "
        + message
        + "\r\n"

    let private visualStudio
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        (message: string)
        =
        let location =
            match diagnostic.LogicalPath, diagnostic.Range with
            | Some path, Some range ->
                visualStudioPath options.FullPaths path
                + "("
                + range.Start.Line.ToString(CultureInfo.InvariantCulture)
                + ","
                + range.Start.Column.ToString(CultureInfo.InvariantCulture)
                + ","
                + range.End.Line.ToString(CultureInfo.InvariantCulture)
                + ","
                + range.End.Column.ToString(CultureInfo.InvariantCulture)
                + "): "
            | _ -> String.Empty

        let subcategory =
            diagnostic.Subcategory
            |> Option.map (fun value ->
                value
                + " "
            )
            |> Option.defaultValue String.Empty

        "\n"
        + location
        + subcategory
        + diagnosticLabel options diagnostic
        + ": "
        + message
        + "\r\n"

    let private gcc
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        (message: string)
        =
        let location =
            match diagnostic.LogicalPath, diagnostic.Range with
            | Some path, Some range ->
                operatingSystemPath options.FullPaths path
                + ":"
                + range.Start.Line.ToString(CultureInfo.InvariantCulture)
                + ":"
                + range.Start.Column.ToString(CultureInfo.InvariantCulture)
                + ": "
            | _ -> String.Empty

        "\n"
        + location
        + diagnosticLabel options diagnostic
        + ": "
        + message
        + "\r\n"

    let private emacs
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        (message: string)
        =
        let location =
            match diagnostic.LogicalPath, diagnostic.Range with
            | Some path, Some range ->
                "File \""
                + emacsPath options.FullPaths path
                + "\", line "
                + range.Start.Line.ToString(CultureInfo.InvariantCulture)
                + ", characters "
                + (max
                    0
                    (range.Start.Column
                     - 1))
                    .ToString(CultureInfo.InvariantCulture)
                + "-"
                + (max
                    0
                    (range.End.Column
                     - 1))
                    .ToString(CultureInfo.InvariantCulture)
                + ": "
            | _ -> String.Empty

        "\n"
        + location
        + diagnosticLabel options diagnostic
        + ": "
        + message
        + "\r\n"

    let private rich
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        (sourceLine: string option)
        (message: string)
        =
        let location =
            match diagnostic.LogicalPath, diagnostic.Range with
            | Some path, Some range ->
                let header =
                    "\n  --> "
                    + operatingSystemPath options.FullPaths path
                    + " ("
                    + range.Start.Line.ToString(CultureInfo.InvariantCulture)
                    + ","
                    + range.Start.Column.ToString(CultureInfo.InvariantCulture)
                    + ")"

                match sourceLine with
                | Some source ->
                    header
                    + "\n  "
                    + range.Start.Line.ToString(CultureInfo.InvariantCulture)
                    + " | "
                    + source
                    + "\n"
                    + String(
                        ' ',
                        6
                        + max
                            0
                            (range.Start.Column
                             - 1)
                    )
                    + String(
                        '^',
                        max
                            1
                            (range.End.Column
                             - range.Start.Column)
                    )
                | None -> header
            | _ -> String.Empty

        "\n"
        + diagnosticLabel options diagnostic
        + ": "
        + message
        + location
        + "\r\n"

    let private encoding (options: DiagnosticOptions) (stream: DiagnosticStream option) =
        if options.Utf8Output then
            UTF8Encoding(false) :> Encoding
        else
            match stream with
            | Some DiagnosticStream.StandardError -> Console.Error.Encoding
            | _ -> Console.Out.Encoding

    let renderWithResources
        (resources: ResourceManager)
        (options: DiagnosticOptions)
        (diagnostic: CompilationDiagnostic)
        (sourceLine: string option)
        =
        if diagnostic.Disposition = DiagnosticDisposition.Suppressed then
            RenderedDiagnostic.Create(None, Array.empty)
        else
            let localizedMessage =
                DiagnosticResources.messageWithResources resources options diagnostic

            let message =
                if
                    options.FlatErrors
                    || options.DiagnosticStyle = DiagnosticStyle.Flat
                then
                    flattenMessage localizedMessage
                else
                    localizedMessage

            let text =
                match options.DiagnosticStyle with
                | DiagnosticStyle.Default
                | DiagnosticStyle.Flat -> standard options diagnostic message
                | DiagnosticStyle.VisualStudio -> visualStudio options diagnostic message
                | DiagnosticStyle.Gcc -> gcc options diagnostic message
                | DiagnosticStyle.Emacs -> emacs options diagnostic message
                | DiagnosticStyle.Rich -> rich options diagnostic sourceLine message

            let bytes = (encoding options diagnostic.Stream).GetBytes(text)

            RenderedDiagnostic.Create(diagnostic.Stream, bytes)

    let render options diagnostic sourceLine =
        renderWithResources DiagnosticResources.resourceManager options diagnostic sourceLine

    let private sourceLine line (sourceText: string option) =
        sourceText
        |> Option.bind (fun text ->
            let lines =
                text
                    .Replace("\r\n", "\n", StringComparison.Ordinal)
                    .Replace('\r', '\n')
                    .Split('\n')

            if
                line > 0
                && line
                   <= lines.Length
            then
                Some lines[line - 1]
            else
                None
        )

    let renderFormattedError
        (options: DiagnosticOptions)
        (error: string)
        (sourceTextForPath: string -> string option)
        =
        let matched = formattedError.Match(error)

        if matched.Success then
            let code = matched.Groups["code"].Value

            let numericCode =
                if code.StartsWith("FS", StringComparison.Ordinal) then
                    match Int32.TryParse(code.AsSpan(2)) with
                    | true, value -> value
                    | false, _ -> 0
                else
                    0

            let path, range, line =
                if matched.Groups["path"].Success then
                    let startLine =
                        Int32.Parse(matched.Groups["line"].Value, CultureInfo.InvariantCulture)

                    let startColumn =
                        Int32.Parse(matched.Groups["column"].Value, CultureInfo.InvariantCulture)

                    Some matched.Groups["path"].Value,
                    Some {
                        Start = {
                            Offset = 0
                            Line = startLine
                            Column = startColumn
                        }
                        End = {
                            Offset = 0
                            Line = startLine
                            Column =
                                startColumn
                                + 1
                        }
                    },
                    Some startLine
                else
                    None, None, None

            let subcategory = if code = "FS0010" then Some "parse" else None

            let diagnostic =
                CompilationDiagnostic.Create(
                    0L,
                    code,
                    numericCode,
                    subcategory,
                    DiagnosticStage.Compilation CompilationPhase.Syntax,
                    DiagnosticSeverity.Error,
                    DiagnosticSeverity.Error,
                    DiagnosticDisposition.Emitted,
                    None,
                    matched.Groups["message"].Value,
                    path,
                    range,
                    [||],
                    [||],
                    Some DiagnosticStream.StandardError
                )

            let selectedSourceText =
                path
                |> Option.bind sourceTextForPath

            render
                options
                diagnostic
                (line
                 |> Option.bind (fun value -> sourceLine value selectedSourceText))
        else
            let text =
                if
                    error.EndsWith("\r\n", StringComparison.Ordinal)
                    || error.EndsWith("\n", StringComparison.Ordinal)
                then
                    error
                else
                    error
                    + "\r\n"

            let bytes = (encoding options (Some DiagnosticStream.StandardError)).GetBytes(text)

            RenderedDiagnostic.Create(Some DiagnosticStream.StandardError, bytes)
