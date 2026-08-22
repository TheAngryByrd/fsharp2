namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.IO

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
