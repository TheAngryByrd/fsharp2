namespace fsharp2.Tests

open FSharp2.Compiler

module internal SyntaxDiagnosticText =
    let severity (diagnostic: SyntaxDiagnostic) =
        match diagnostic.Severity with
        | DiagnosticSeverity.Error -> "error"
        | DiagnosticSeverity.Warning -> "warning"
        | DiagnosticSeverity.Information -> "info"
        | DiagnosticSeverity.Hidden -> "hidden"
