module FSharp2.XParsec.ParseHost.Program

open System.IO
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let private parseFile (printTree: bool) (path: string) =
    let input = File.ReadAllText(path).Replace("\r\n", "\n")
    let lexed = Lexing.lexString input
    let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

    let result = FSharpAst.parse reader

    let outcome =
        match result with
        | Ok _ -> "tree"
        | Error _ -> "no-tree"

    let diagnostics = reader.State.Diagnostics

    stdout.WriteLine(
        Path.GetFileName path
        + " "
        + outcome
        + " diagnostics="
        + string diagnostics.Length
    )

    for diagnostic in List.rev diagnostics do
        let fsharp2Code =
            match DiagnosticCode.fsharp2Code diagnostic.Code with
            | ValueSome code -> " " + code
            | ValueNone -> ""

        stdout.WriteLine(
            "  "
            + DiagnosticCode.code diagnostic.Code
            + fsharp2Code
        )

    if printTree then
        let context = Debug.PrintContext(2)

        match result with
        | Ok ast -> Debug.printFSharpAst context lexed ast
        | Error _ -> ()

        Debug.printDiagnostics context input diagnostics
        context.Flush(stdout)

[<EntryPoint>]
let main argv =
    let printTree =
        argv.Length > 0
        && argv[0] = "--tree"

    for path in (if printTree then argv[1..] else argv) do
        parseFile printTree path

    0
