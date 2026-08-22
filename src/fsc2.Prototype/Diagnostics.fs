namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.IO

module internal DiagnosticPolicy =
    let private canonicalCode (code: string) =
        let value = code.Trim()

        let numeric =
            if value.StartsWith("FS", StringComparison.OrdinalIgnoreCase) then
                value[2..]
            else
                value

        match Int32.TryParse(numeric) with
        | true, number -> number.ToString()
        | false, _ -> value.ToUpperInvariant()

    let private containsCode codes code =
        let expected = canonicalCode code

        codes
        |> Seq.exists (
            canonicalCode
            >> ((=) expected)
        )

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
                Stream =
                    diagnostic.Stream
                    |> Option.orElse (Some fallbackStream)
        }

    let private suppressed reason (diagnostic: CompilationDiagnostic) : CompilationDiagnostic = {
        diagnostic with
            EffectiveSeverity = DiagnosticSeverity.Hidden
            Disposition = DiagnosticDisposition.Suppressed
            Suppression = Some reason
            Stream = None
    }

    let apply (options: DiagnosticOptions) (diagnostics: seq<CompilationDiagnostic>) =
        diagnostics
        |> Seq.map (fun diagnostic ->
            match diagnostic.OriginalSeverity with
            | DiagnosticSeverity.Hidden ->
                suppressed
                    (diagnostic.Suppression
                     |> Option.defaultValue DiagnosticSuppression.OffByDefault)
                    diagnostic
            | DiagnosticSeverity.Information ->
                emitted DiagnosticSeverity.Information DiagnosticStream.StandardOutput diagnostic
            | DiagnosticSeverity.Error ->
                emitted DiagnosticSeverity.Error DiagnosticStream.StandardError diagnostic
            | DiagnosticSeverity.Warning ->
                if options.WarningLevel = Some 0 then
                    suppressed DiagnosticSuppression.WarningLevel diagnostic
                elif containsCode options.DisabledWarnings diagnostic.Code then
                    suppressed DiagnosticSuppression.GlobalNowarn diagnostic
                elif
                    options.TreatWarningsAsErrors
                    || containsCode options.WarningsAsErrors diagnostic.Code
                then
                    emitted DiagnosticSeverity.Error DiagnosticStream.StandardError diagnostic
                else
                    emitted DiagnosticSeverity.Warning DiagnosticStream.StandardError diagnostic
        )
        |> ImmutableArray.CreateRange

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
