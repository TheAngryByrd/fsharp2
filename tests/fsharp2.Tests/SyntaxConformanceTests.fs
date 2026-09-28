namespace fsharp2.Tests

open System
open System.Collections.Immutable
open System.IO
open System.Text.Json
open Expecto
open FSharp2.Compiler

module SyntaxConformanceTests =
    let private conformanceRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "conformance"))

    let private syntaxRows = [
        "syntax.implementation-declarations"
        "syntax.signature-declarations"
        "syntax.type-definitions"
        "syntax.expressions"
        "syntax.underscore-dot-shorthand"
        "syntax.successive-arguments"
        "syntax.ordered-files"
    ]

    let private json (path: string) =
        use document = JsonDocument.Parse(File.ReadAllBytes path)
        document.RootElement.Clone()

    let private text (element: JsonElement) (name: string) = element.GetProperty(name).GetString()

    let private syntaxCases () =
        Directory.GetFiles(
            Path.Combine(conformanceRoot, "cases", "language", "syntax"),
            "*.case.json"
        )
        |> Array.sort
        |> Array.map json

    let private parserDiagnostics language (case: JsonElement) =
        let target =
            let outputType =
                text (case.GetProperty("options").GetProperty("emission")) "outputType"

            SyntaxCompilationTarget.tryParse outputType
            |> Option.defaultWith (fun () -> failtest $"Unknown output type '{outputType}'")

        let documents =
            case.GetProperty("sources").EnumerateArray()
            |> Seq.sortBy (fun source -> source.GetProperty("order").GetInt32())
            |> Seq.map (fun source ->
                SourceSnapshot.Create(
                    StableIdentity.create (text source "stableId"),
                    text source "logicalPath",
                    File.ReadAllText(Path.Combine(conformanceRoot, text source "fixturePath")),
                    "content"
                )
                |> LexicalPipeline.prepare language Array.empty
            )
            |> ImmutableArray.CreateRange

        (Parser.parseCompilation target documents).Diagnostics
        |> Seq.map (fun fileDiagnostic ->
            let diagnostic = fileDiagnostic.Diagnostic

            fileDiagnostic.LogicalPath,
            diagnostic.Code,
            diagnostic.Message,
            diagnostic.Range.Start.Line,
            diagnostic.Range.Start.Column,
            diagnostic.Range.End.Line,
            diagnostic.Range.End.Column
        )
        |> Seq.toList

    let private oracleDiagnostics (case: JsonElement) =
        let expected = case.GetProperty("expectedDiagnostics")

        if text expected "mode" = "oracle-lock" then
            let lock = json (Path.Combine(conformanceRoot, text expected "lockPath"))

            lock.GetProperty("diagnostics").EnumerateArray()
            |> Seq.map (fun diagnostic ->
                let range = diagnostic.GetProperty("range")

                text diagnostic "logicalSource",
                text diagnostic "code",
                text diagnostic "message",
                range.GetProperty("startLine").GetInt32(),
                range.GetProperty("startColumn").GetInt32(),
                range.GetProperty("endLine").GetInt32(),
                range.GetProperty("endColumn").GetInt32()
            )
            |> Seq.toList
        else
            []

    [<Tests>]
    let tests =
        testList "Issue29.SyntaxConformance" [
            testCase "each syntax feature row has a positive and a negative Oracle case"
            <| fun _ ->
                let cases = syntaxCases ()

                for row in syntaxRows do
                    let polarities =
                        cases
                        |> Seq.filter (fun case -> text case "featureRow" = row)
                        |> Seq.map (fun case -> text case "polarity")
                        |> Set.ofSeq

                    Expect.equal
                        polarities
                        (set [
                            "negative"
                            "positive"
                        ])
                        $"Feature row '{row}' must have positive and negative cases"

            testCase "the syntax parser reports the Oracle diagnostics for each syntax case"
            <| fun _ ->
                let cases = syntaxCases ()

                Expect.isNonEmpty cases "The syntax family has cases"

                for case in cases do
                    let caseId = text case "caseId"

                    for mode in
                        case
                            .GetProperty("envelope")
                            .GetProperty("languageVersions")
                            .EnumerateArray() do
                        let mode = mode.GetString()

                        let language =
                            LanguageVersion.normalize (Some mode)
                            |> Result.defaultWith failtest

                        Expect.sequenceEqual
                            (parserDiagnostics language case)
                            (oracleDiagnostics case)
                            $"Case '{caseId}' with language version '{mode}' must match its Oracle lock"
        ]
