namespace fsharp2.Tests

open System
open System.IO
open System.Threading.Tasks
open Expecto
open FSharp2.Compiler

module ParserRobustnessTests =
    let private language =
        LanguageVersion.normalize (Some "10.0")
        |> Result.defaultWith failtest

    let private limit = TimeSpan.FromSeconds 10.0

    let private parse (logicalPath: string) document =
        if logicalPath.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
            Parser.parseSignatureFile document
            |> ignore
        else
            Parser.parseImplementationFile document
            |> ignore

    let private expectCompletes (logicalPath: string) (text: string) =
        let document =
            SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
            |> LexicalPipeline.prepare language Array.empty

        let work = Task.Run(fun () -> parse logicalPath document)

        let completed =
            try
                work.Wait limit
            with :? AggregateException as failure ->
                failtest
                    $"The parser threw {failure.InnerException.GetType().Name}: {failure.InnerException.Message}\nInput {logicalPath}:\n{text}"

        Expect.isTrue
            completed
            $"The parser must finish within {limit.TotalSeconds} seconds for {logicalPath}:\n{text}"

    let private fragments = [|
        "module"
        "namespace"
        "open"
        "let"
        "rec"
        "and"
        "do"
        "type"
        "val"
        "member"
        "static"
        "mutable"
        "private"
        "match"
        "with"
        "when"
        "if"
        "then"
        "elif"
        "else"
        "fun"
        "function"
        "of"
        "in"
        "end"
        "done"
        "x"
        "Some"
        "A.B"
        "_"
        "'a"
        "1"
        "\"s\""
        "true"
        "("
        ")"
        "["
        "]"
        "{"
        "}"
        "[<"
        ">]"
        "[|"
        "|]"
        "="
        ":"
        ";"
        ","
        "."
        "|"
        "->"
        "<-"
        "*"
        "+"
        "<"
        ">"
        "\n"
        "\n    "
        "\n        "
        "\n  "
    |]

    let private generated =
        let random = Random 29

        [
            for index in 1..3000 ->
                let length = random.Next(1, 40)

                let text =
                    Seq.init length (fun _ -> fragments[random.Next fragments.Length])
                    |> String.concat " "

                let extension = if index % 4 = 0 then ".fsi" else ".fs"
                $"Generated{index}{extension}", text
        ]

    let private repositorySources () =
        let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

        [
            "src"
            "tests"
        ]
        |> Seq.collect (fun folder ->
            Directory.EnumerateFiles(
                Path.Combine(root, folder),
                "*.fs*",
                SearchOption.AllDirectories
            )
        )
        |> Seq.filter (fun path ->
            (path.EndsWith(".fs", StringComparison.OrdinalIgnoreCase)
             || path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase))
            && not (
                path.Contains($"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}")
            )
            && not (
                path.Contains($"{Path.DirectorySeparatorChar}bin{Path.DirectorySeparatorChar}")
            )
        )
        |> Seq.sort
        |> Seq.toList

    [<Tests>]
    let tests =
        testList "Issue29.ParserRobustness" [
            testCase "the parser finishes without an exception on generated token sequences"
            <| fun _ ->
                for logicalPath, text in generated do
                    expectCompletes logicalPath text

            testCase "the parser finishes without an exception on every repository F# source"
            <| fun _ ->
                let sources = repositorySources ()

                Expect.isNonEmpty sources "The repository contains F# sources"

                for path in sources do
                    expectCompletes (Path.GetFileName path) (File.ReadAllText path)
        ]
