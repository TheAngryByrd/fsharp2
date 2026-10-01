module FSharp2.XParsec.ParseHost.Program

open System.IO
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let private parseFile (path: string) =
    let input = File.ReadAllText(path).Replace("\r\n", "\n")
    let lexed = Lexing.lexString input
    let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

    let outcome =
        match FSharpAst.parse reader with
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

[<EntryPoint>]
let main argv =
    for path in argv do
        parseFile path

    0
