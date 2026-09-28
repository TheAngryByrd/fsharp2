namespace fsharp2.Tests

open System
open System.Collections.Immutable
open System.Security.Cryptography
open System.Text
open Expecto
open FSharp2.Compiler

module SyntaxProjectionTests =
    let private language =
        LanguageVersion.normalize (Some "10.0")
        |> Result.defaultWith failtest

    let private byteFingerprint (values: byte array) =
        values
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let private fingerprint (text: string) =
        text
        |> Encoding.UTF8.GetBytes
        |> byteFingerprint

    let private withoutChecksum (modules: ParsedModule list) =
        modules
        |> List.map (fun parsedModule -> {
            parsedModule with
                SourceChecksum = ImmutableArray.Empty
        })

    let private document logicalPath text =
        SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
        |> LexicalPipeline.prepare language Array.empty

    let private project text =
        match SyntaxRouting.tryProject (document "Program.fs" text) with
        | Some modules -> SyntaxProjectionResult.Projected modules
        | None ->
            SyntaxProjectionResult.ProjectionUnsupported {
                Start = { Offset = 0; Line = 1; Column = 1 }
                End = { Offset = 0; Line = 1; Column = 1 }
            }

    let private prototype text =
        Frontend.parse [] {
            Path = "Program.fs"
            Text = text
            ContentFingerprint = "content"
        }

    let private bodies = [
        "42"
        "0"
        "2147483647"
        "\"text\""
        "\"\""
        "true"
        "false"
        "()"
        "value"
        "argv"
        "1 + 2"
        "f x"
        "\"a\\\\b\""
        "2147483648"
        "0x10"
        "1L"
        "(1)"
        "[ 1 ]"
        "Values.answer"
    ]

    let private bindings = [
        fun body -> $"let answer = {body}"
        fun body -> $"let run () = {body}"
        fun body -> $"let echo value = {body}"
        fun body -> $"let echo (value: int) = {body}"
        fun body -> $"let private hidden = {body}"
        fun body -> $"[<EntryPoint>]\nlet main argv = {body}"
    ]

    let private layouts = [
        fun (lines: string list) ->
            String.Join("\n", lines)
            + "\n"
        fun (lines: string list) -> String.Join("\r\n", lines)
        fun (lines: string list) ->
            "// leading comment\n"
            + String.Join("\n\n", lines)
            + "\n"
    ]

    let private lexicalCorpus = [
        "module M\n(* c *)\nlet x = 1\n"
        "module M\nlet x = 1 (* c *)\n"
        "module M\nlet ``x`` = 1\n"
        "module ``M``\nlet x = 1\n"
        "module M\nlet f ``p`` = ``p``\n"
        "module M\nlet assembly = 1\n"
        "module M\nlet f assembly = 1\n"
        "module assembly\nlet x = 1\n"
        "module M\nlet x = isNull\n"
        "module M\nlet x = isNull\nlet y = 1\n"
        "module M\nlet x = ignore\n"
        "module M\nlet s = \"a\nb\"\n"
        "module M\nlet s = \"a\r\nb\"\n"
        "module M\rlet x = 1\r"
        "module M // c\rlet x = 1"
        "module M\nlet x = 'a'\n"
        "module M\nlet x = 0_1\n"
        "module M\nlet x = 007\n"
        "module M\n\tlet x = 1\n"
        "module M\nlet x = 1   \n"
        "module M\nlet x = \"// not a comment\"\n"
        "module M\nlet x = \"(* not a comment *)\"\n"
        "module M\nlet x' = 1\n"
        "module M\nlet x = y'\n"
        "module M\nlet λ = 1\n"
        "module M\n[<EntryPoint;>]\nlet main argv = 0\n"
        "module M\n[<EntryPoint; >]\nlet main argv = 0\n"
        "module M\nlet f x = 1\n[<EntryPoint;>]\nlet main argv = f\n"
        "module M\n[< EntryPoint >]\nlet main argv = 0\n"
    ]

    let private corpus = [
        yield! lexicalCorpus

        for header in
            [
                "module Program"
                "module Values"
                "module Sample.Values"
                "namespace Sample"
            ] do
            for layout in layouts do
                for binding in bindings do
                    for body in bodies do
                        yield
                            layout [
                                header
                                binding body
                            ]

                        yield
                            layout [
                                header
                                binding body
                                "let after = 1"
                            ]

                        yield
                            layout [
                                header
                                "let before = 1"
                                binding body
                            ]
    ]

    let private references () =
        let reference name (path: string) =
            let image = IO.File.ReadAllBytes path

            TargetReferenceSnapshot.Create(
                StableIdentity.create $"reference:{name}",
                $"{name}.dll",
                image,
                byteFingerprint image
            )

        ImmutableArray.Create(
            reference "System.Runtime" (Reflection.Assembly.Load("System.Runtime").Location),
            reference
                "FSharp.Core"
                typeof<Microsoft.FSharp.Core.EntryPointAttribute>.Assembly.Location
        )
        |> ReferenceTypeIndex.Create
        |> Result.defaultWith failtest

    let private compileWithService (text: string) =
        let service = CompilerService()

        let source =
            SourceSnapshot.Create(
                StableIdentity.create "source:program",
                "Program.fs",
                text,
                fingerprint text
            )

        let result = service.Compile("Program", language, [], references (), [ source ])
        result, service.Statistics.SyntaxProjections

    [<Tests>]
    let tests =
        testList "Issue29.SyntaxProjection" [
            testCase "the compiler service parses a projected module with the syntax parser"
            <| fun _ ->
                let result, projections =
                    compileWithService
                        "module Program\nlet answer = 42\n[<EntryPoint>]\nlet main argv = 0\n"

                Expect.isOk result "The projected module compiles"
                Expect.equal projections 1 "The syntax parser produced the parsed module"

            testCase "the compiler service keeps the prototype parser for other modules"
            <| fun _ ->
                for text in
                    [
                        "namespace Sample\nmodule Values =\n    let answer = 42\n"
                        "module Program\nlet answer = 1 + 2\n"
                        "#nowarn \"25\"\nmodule Program\nlet answer = 42\n"
                        "module Program\nlet broken = )\n"
                    ] do
                    let _, projections = compileWithService text

                    Expect.equal projections 0 $"The prototype parser handles this source:\n{text}"

            testCase "every projected module equals the prototype parser result"
            <| fun _ ->
                let mutable projected = 0

                for text in corpus do
                    match project text with
                    | SyntaxProjectionResult.ProjectionUnsupported _ -> ()
                    | SyntaxProjectionResult.Projected modules ->
                        projected <-
                            projected
                            + 1

                        match prototype text with
                        | Error diagnostic ->
                            failtest
                                $"The projection accepted a source that the prototype parser rejects with {diagnostic.Code}: {diagnostic.Message}\n{text}"
                        | Ok expected ->
                            Expect.equal
                                (withoutChecksum modules)
                                (withoutChecksum expected)
                                $"The projection must equal the prototype parser result for:\n{text}"

                Expect.isGreaterThan projected 0 "The corpus contains projected sources"

            testCase
                "every name that the prototype parser matches in a pattern or a string comparison is excluded from projection"
            <| fun _ ->
                let frontend =
                    IO.File.ReadAllText(
                        IO.Path.Combine(
                            __SOURCE_DIRECTORY__,
                            "..",
                            "..",
                            "src",
                            "fsc2.Prototype",
                            "Frontend.fs"
                        )
                    )

                let names pattern =
                    Text.RegularExpressions.Regex.Matches(frontend, pattern)
                    |> Seq.map (fun found -> found.Groups[1].Value)
                    |> Set.ofSeq

                let matched =
                    Set.union
                        (names "\| Identifier \"([^\"]+)\"")
                        (names "[A-Za-z]Name (?:=|<>) \"([A-Za-z_][A-Za-z0-9_]*)\"")

                Expect.isEmpty
                    (Set.difference matched (Set.add "EntryPoint" Frontend.specialIdentifiers))
                    "Each name that Frontend.parse matches must stay on the prototype path, except EntryPoint, which the projection checks as an exact attribute list"

            testCase "the syntax parser runs only on sources with a projectable token shape"
            <| fun _ ->
                for text in
                    [
                        "namespace Sample\nmodule Values =\n    let answer = 42\n"
                        "module Program\nopen System\nlet answer = 42\n"
                        "module Program\ntype Point = { X: int }\n"
                        "module Program\nlet answer = 1 + 2\n"
                        "module Program\nlet answer = f x\n"
                        "module Program\nlet rec answer = 42\n"
                        "module Program\nlet answer = match x with | A -> 1\n"
                        "module Program\n[<Literal>]\nlet answer = 42\n"
                        "module Program\nlet answer: int = 42\n"
                        "module Program\n"
                        "let answer = 42\n"
                    ] do
                    Expect.isFalse
                        (SyntaxRouting.isEligible (document "Program.fs" text))
                        $"The syntax parser must not run on this source:\n{text}"

                Expect.isFalse
                    (SyntaxRouting.isEligible (
                        document "Program.fsi" "module Program\nlet answer = 42\n"
                    ))
                    "The syntax parser must not run on a signature file"

            testCase "the token shape check accepts every source that the projection accepts"
            <| fun _ ->
                let mutable projectable = 0

                for text in corpus do
                    let source = document "Program.fs" text
                    let syntax = Parser.parseImplementationFile source

                    if
                        syntax.Diagnostics.IsEmpty
                        && source.Directives.IsEmpty
                        && source.Diagnostics.IsEmpty
                        && Frontend.isTokenizedLikeLexicalDocument source
                    then
                        match
                            SyntaxProjection.project "content" ImmutableArray.Empty syntax.File
                        with
                        | SyntaxProjectionResult.Projected _ ->
                            projectable <-
                                projectable
                                + 1

                            Expect.isTrue
                                (SyntaxRouting.hasProjectableTokenShape source)
                                $"The token shape check must accept a projectable source:\n{text}"
                        | SyntaxProjectionResult.ProjectionUnsupported _ -> ()

                Expect.isGreaterThan projectable 0 "The corpus contains projectable sources"

            testCase "each supported construct is projected"
            <| fun _ ->
                for text in
                    [
                        "module Program\nlet answer = 42\n"
                        "module Program\nlet run () = \"text\"\n"
                        "module Program\nlet echo value = value\n"
                        "module Program\nlet flag = true\n"
                        "module Program\nlet unit = ()\n"
                        "module Program\nlet before = 1\n[<EntryPoint>]\nlet main argv = 0\n"
                    ] do
                    match project text with
                    | SyntaxProjectionResult.Projected _ -> ()
                    | SyntaxProjectionResult.ProjectionUnsupported range ->
                        failtest
                            $"Expected a projection, but the projection stopped at line {range.Start.Line}, column {range.Start.Column}:\n{text}"

            testCase "sources with syntax diagnostics are never projected"
            <| fun _ ->
                for text in
                    [
                        "module Program\nlet broken = )\nlet answer = 42\n"
                        "module Program\nlet answer = 42\n)\nlet after = 1\n"
                    ] do
                    match project text with
                    | SyntaxProjectionResult.ProjectionUnsupported _ -> ()
                    | SyntaxProjectionResult.Projected _ ->
                        failtest
                            $"A source with a syntax error must stay on the prototype parser:\n{text}"
        ]
