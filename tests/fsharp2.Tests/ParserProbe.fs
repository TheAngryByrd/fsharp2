namespace fsharp2.Tests

open System
open System.Collections.Immutable
open System.IO
open System.Text.Json
open Expecto
open FSharp2.Compiler

/// Parser side of tools/Issue29ParserProbe.
module ParserProbe =
    let private variable = "FSHARP2_PARSER_PROBE_DIRECTORY"

    let private probe (directory: string) =
        use cases =
            JsonDocument.Parse(File.ReadAllBytes(Path.Combine(directory, "cases.resolved.json")))

        let output = Collections.Generic.Dictionary<string, string list>()

        for case in cases.RootElement.EnumerateArray() do
            let name = case.GetProperty("name").GetString()

            let language =
                LanguageVersion.normalize (Some(case.GetProperty("languageVersion").GetString()))
                |> Result.defaultWith failtest

            let target =
                SyntaxCompilationTarget.tryParse (case.GetProperty("target").GetString())
                |> Option.defaultWith (fun () -> failtest $"Case '{name}' has an unknown target")

            let sources =
                case.GetProperty("files").EnumerateArray()
                |> Seq.map (fun file ->
                    let logicalPath = file.GetString()

                    {
                        Kind =
                            SyntaxSourceKind.parse logicalPath
                            |> Result.defaultWith (fun error ->
                                failtest
                                    $"Case '{name}' file '{logicalPath}' has no source kind: {error}"
                            )
                        Document =
                            SourceSnapshot.Create(
                                StableIdentity.create logicalPath,
                                logicalPath,
                                File.ReadAllText(Path.Combine(directory, logicalPath)),
                                "content"
                            )
                            |> LexicalPipeline.prepare language Array.empty
                    }
                )
                |> ImmutableArray.CreateRange

            output[name] <-
                (Parser.parseCompilation target sources).Diagnostics
                |> Seq.map (fun fileDiagnostic ->
                    let diagnostic = fileDiagnostic.Diagnostic
                    let range = diagnostic.Range

                    $"{fileDiagnostic.LogicalPath}({range.Start.Line},{range.Start.Column},{range.End.Line},{range.End.Column}): {SyntaxDiagnosticText.severity diagnostic} {diagnostic.Code}: {diagnostic.Message}"
                )
                |> Seq.toList

        File.WriteAllText(
            Path.Combine(directory, "parser.json"),
            JsonSerializer.Serialize(output, JsonSerializerOptions(WriteIndented = true))
        )

    [<Tests>]
    let tests =
        match Environment.GetEnvironmentVariable variable with
        | null
        | "" -> testList "Issue29.ParserProbe" []
        | directory ->
            testList "Issue29.ParserProbe" [
                testCase "writes the parser diagnostics of each probe case"
                <| fun _ -> probe directory
            ]
