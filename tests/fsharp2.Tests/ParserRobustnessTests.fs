namespace fsharp2.Tests

open System
open System.IO
open System.Threading.Tasks
open Expecto
open FSharp2.Compiler

module ParserRobustnessTests =
    let private language mode =
        LanguageVersion.normalize (Some mode)
        |> Result.defaultWith failtest

    let private limit = TimeSpan.FromSeconds 10.0

    let private implicitModule =
        ImplicitModule.Accepted {
            ReferencesFingerprint = "namespaces"
            Names = Collections.Immutable.ImmutableHashSet.Create "System"
        }

    let private parse (logicalPath: string) document =
        if logicalPath.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
            Parser.parseSignatureFile document
            |> ignore
        else
            Parser.parseImplementationFile document
            |> ignore

            SyntaxRouting.tryProject implicitModule document
            |> ignore

    let private expectCompletesIn mode (logicalPath: string) (text: string) =
        let document =
            SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
            |> LexicalPipeline.prepare (language mode) Array.empty

        let work = Task.Run(fun () -> parse logicalPath document)

        let completed =
            try
                work.Wait limit
            with :? AggregateException as failure ->
                failtest
                    $"The parser threw {failure.InnerException.GetType().Name}: {failure.InnerException.Message}\nInput {logicalPath} ({mode}):\n{text}"

        Expect.isTrue
            completed
            $"The parser must finish within {limit.TotalSeconds} seconds for {logicalPath} ({mode}):\n{text}"

    let private expectCompletes logicalPath text =
        for mode in
            [
                "7.0"
                "10.0"
            ] do
            expectCompletesIn mode logicalPath text

    let private nested depth (opening: string) (closing: string) (body: string) =
        String.replicate depth opening
        + body
        + String.replicate depth closing

    let private deeplyNested = [
        "Parentheses.fs", $"""module Program{"\n"}let x = {nested 200 "(" ")" "1"}{"\n"}"""
        "Lists.fs", $"""module Program{"\n"}let x = {nested 200 "[ " " ]" "1"}{"\n"}"""
        "Records.fs", $"""module Program{"\n"}let x = {nested 200 "{ A = " " }" "1"}{"\n"}"""
        "Lambdas.fs", $"""module Program{"\n"}let x = {nested 200 "fun a -> " "" "a"}{"\n"}"""
        "Conditions.fs",
        $"""module Program{"\n"}let x = {nested 200 "if a then " " else 0" "1"}{"\n"}"""
        "DotLambdas.fs", $"""module Program{"\n"}let x = {nested 200 "f (_.A " ")" "1"}{"\n"}"""
        "Types.fsi", $"""module Program{"\n"}val x: {nested 200 "(" ")" "int"}{"\n"}"""
        "Functions.fsi", $"""module Program{"\n"}val x: {nested 200 "int -> " "" "int"}{"\n"}"""
        "Unclosed.fs", $"""module Program{"\n"}let x = {String.replicate 200 "("}1{"\n"}"""
        "Modules.fs",
        "module Program\n"
        + String.concat "" [
            for level in 0..60 ->
                String.replicate (level * 4) " "
                + $"module M{level} =\n"
        ]
        + String.replicate (61 * 4) " "
        + "let x = 1\n"
    ]

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

            testCase "the parser finishes without an exception on deeply nested input"
            <| fun _ ->
                for logicalPath, text in deeplyNested do
                    expectCompletes logicalPath text

            testCase "the parser finishes without an exception on every repository F# source"
            <| fun _ ->
                let sources = repositorySources ()

                Expect.isNonEmpty sources "The repository contains F# sources"

                for path in sources do
                    expectCompletes (Path.GetFileName path) (File.ReadAllText path)
        ]
