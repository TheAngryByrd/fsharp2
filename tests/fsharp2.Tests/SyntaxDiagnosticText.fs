namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module internal SyntaxDiagnosticText =
    let severity (diagnostic: SyntaxDiagnostic) =
        match diagnostic.Severity with
        | DiagnosticSeverity.Error -> "error"
        | DiagnosticSeverity.Warning -> "warning"
        | DiagnosticSeverity.Information -> "info"
        | DiagnosticSeverity.Hidden -> "hidden"

    let expectExplicitlyUnsupported
        (oracle: string list)
        (diagnostics: SyntaxDiagnostic seq)
        (lines: string list)
        =
        let diagnostics = Seq.toList diagnostics

        Expect.exists
            diagnostics
            (fun diagnostic -> diagnostic.Code = "FSC2P1001")
            "Unsupported syntax must report an explicit FSC2P1001 diagnostic"

        let oracleCount line =
            oracle
            |> List.filter ((=) line)
            |> List.length

        for line, occurrences in
            lines
            |> List.filter (fun line -> not (line.Contains ": error FSC2P1001: "))
            |> List.countBy id do
            Expect.isLessThanOrEqual
                occurrences
                (oracleCount line)
                $"Each FS diagnostic must be a diagnostic that the Compatibility Oracle reports, no more often than the Oracle: {line}"

        // The Compatibility Oracle reports FS0058 at the position of the diagnostic that follows it.
        let recoveryGroups =
            diagnostics
            |> List.filter (fun diagnostic ->
                diagnostic.Code
                <> "FS0058"
            )
            |> List.groupBy (fun diagnostic -> diagnostic.Code = "FSC2P1001")

        for _, group in recoveryGroups do
            Expect.equal
                (group
                 |> Seq.map _.Range.Start
                 |> Seq.distinct
                 |> Seq.length)
                group.Length
                "One recovery group reports one diagnostic at each position"
