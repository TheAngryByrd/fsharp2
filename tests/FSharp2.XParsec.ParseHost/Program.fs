module FSharp2.XParsec.ParseHost.Program

open System.IO
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

let private parseFile (path: string) =
    let input = File.ReadAllText(path).Replace("\r\n", "\n")
    let lexed = Lexing.lexString input
    let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)

    match FSharpAst.parse reader with
    | Ok _ ->
        let diagnostics = reader.State.Diagnostics

        stdout.WriteLine(
            Path.GetFileName path
            + " diagnostics="
            + string diagnostics.Length
        )

        for diagnostic in List.rev diagnostics do
            stdout.WriteLine(
                "  "
                + DiagnosticCode.code diagnostic.Code
            )
    | Error error ->
        stdout.WriteLine(
            Path.GetFileName path
            + " error="
            + ErrorFormatting.splitAndFormatTokenErrors error
        )

[<EntryPoint>]
let main argv =
    for path in argv do
        parseFile path

    0
