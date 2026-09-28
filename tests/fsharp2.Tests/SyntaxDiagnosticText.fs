namespace fsharp2.Tests

open FSharp2.Compiler

module internal SyntaxDiagnosticText =
    let severity (diagnostic: SyntaxDiagnostic) =
        match diagnostic.Severity with
        | LexicalSeverity.Error -> "error"
        | LexicalSeverity.Warning -> "warning"
