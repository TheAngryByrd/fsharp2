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

    let apply (options: DiagnosticOptions) diagnostics =
        diagnostics
        |> Seq.choose (fun diagnostic ->
            match diagnostic.Severity with
            | DiagnosticSeverity.Information
            | DiagnosticSeverity.Error -> Some diagnostic
            | DiagnosticSeverity.Warning ->
                if
                    options.WarningLevel = Some 0
                    || containsCode options.DisabledWarnings diagnostic.Code
                then
                    None
                elif
                    options.TreatWarningsAsErrors
                    || containsCode options.WarningsAsErrors diagnostic.Code
                then
                    Some {
                        diagnostic with
                            Severity = DiagnosticSeverity.Error
                    }
                else
                    Some diagnostic
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
