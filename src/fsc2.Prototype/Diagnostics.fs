namespace FSharp2.Compiler

open System
open System.IO

module internal DiagnosticFormatter =
    let private flattenMessage (message: string) =
        message
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Replace('\r', '\n')
            .Replace('\n', '\u001d')

    let format (invocation: CompilerInvocation) (diagnostic: CompilerDiagnostic) =
        let message =
            if invocation.FlatErrors then
                flattenMessage diagnostic.Message
            else
                diagnostic.Message

        let location =
            match diagnostic.Path, diagnostic.Range with
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
        + diagnostic.Code
        + ": "
        + message
