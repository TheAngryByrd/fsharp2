namespace fsharp2.Tests

open System
open Expecto
open FSharp2.Compiler

module ParserAdjacencyTests =
    let private parse mode text =
        let language =
            LanguageVersion.normalize (Some mode)
            |> Result.defaultWith failtest

        SourceSnapshot.Create(StableIdentity.create "Program.fs", "Program.fs", text, "content")
        |> LexicalPipeline.prepare language Array.empty
        |> Parser.parseImplementationFile

    let rec private shape expression =
        match expression with
        | SyntaxExpression.Constant(SyntaxConstant.Numeric text, _) -> text
        | SyntaxExpression.Constant(SyntaxConstant.Unit, _) -> "()"
        | SyntaxExpression.Constant(other, _) -> string other
        | SyntaxExpression.Identifier name -> name.Text
        | SyntaxExpression.Parenthesized(inner, _) -> $"({shape inner})"
        | SyntaxExpression.List(items, _) ->
            items
            |> Seq.map shape
            |> String.concat "; "
            |> sprintf "[%s]"
        | SyntaxExpression.Application(func, argument, _) -> $"app({shape func}, {shape argument})"
        | SyntaxExpression.BracketApplication(target, argument, _) ->
            $"bracket({shape target}, {shape argument})"
        | SyntaxExpression.DotLambda(body, _) -> $"dot({shape body})"
        | other -> failtest $"Unexpected expression {other}"

    let private body mode text =
        let result = parse mode $"module Program\nlet f = {text}\n"

        Expect.isEmpty
            result.Diagnostics
            $"The Compatibility Oracle reports no parse diagnostic for '{text}' at {mode}"

        match Seq.exactlyOne (Seq.exactlyOne result.File.Contents).Declarations with
        | ImplementationDeclaration.Let(_, bindings, _) -> (Seq.exactlyOne bindings).Body
        | other -> failtest $"Expected a let declaration, found {other}"

    let private dotLambdaCases = [
        "_.ToString()", "dot(app(ToString, ()))"
        "_.Item(0)", "dot(app(Item, (0)))"
        "_.Head[0]", "dot(bracket(Head, 0))"
        "g _.A[0] x", "app(app(g, dot(bracket(A, 0))), x)"
        "g _.A 1", "app(app(g, dot(A)), 1)"
    ]

    let private unprovenDotLambdaCases = [
        "_.Head[0][1]"
        "_.M()[0]"
        "_.M(1)(2)"
        "_.Items[0](2)"
        "_.ToString ()"
        "_.Head [0]"
        "g _.M()(1)"
    ]

    let private bracketCases = [
        "arr[0]", "bracket(arr, 0)"
        "f arr[0]", "app(f, bracket(arr, 0))"
        "arr[0][1]", "bracket(bracket(arr, 0), 1)"
        "f(1)[0]", "bracket(app(f, (1)), 0)"
        "arr [0]", "app(arr, [0])"
        "arr[ 0 ]", "bracket(arr, 0)"
        "g arr[0][1]", "app(app(g, bracket(arr, 0)), [1])"
        "g arr[0](1)", "app(app(g, bracket(arr, 0)), (1))"
        "g (arr)[0]", "app(app(g, (arr)), [0])"
        "g [1; 2][0]", "app(app(g, [1; 2]), [0])"
        "g 1(2)", "app(app(g, 1), (2))"
        "f (x)(y)", "app(app(f, (x)), (y))"
        "f(1)(2) 3", "app(app(app(f, (1)), (2)), 3)"
    ]

    let private successiveArguments startColumn endColumn =
        $"Program.fs(2,{startColumn},2,{endColumn}): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"

    let private successiveArgumentCases = [
        "g f(1)", [ successiveArguments 11 15 ]
        "g f(1)(2)", [ successiveArguments 11 15 ]
        "g f(1)[0]", [ successiveArguments 11 15 ]
        "g f(1) 2", [ successiveArguments 11 15 ]
        "g f(1) (2)", [ successiveArguments 11 15 ]
        "g f(1) h(2)",
        [
            successiveArguments 11 15
            successiveArguments 16 20
        ]
        "g A.f(1)", [ successiveArguments 11 17 ]
        "g f()", [ successiveArguments 11 14 ]
        "g f (1)(2)", []
        "g (f)(1)", []
        "g f(1) )",
        [
            successiveArguments 11 15
            "Program.fs(2,16,2,17): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "g f(1) h(2) ]",
        [
            successiveArguments 11 15
            successiveArguments 16 20
            "Program.fs(2,21,2,22): error FS0010: Unexpected symbol ']' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        ") g f(1)", [ "Program.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding" ]
    ]

    let private oracleLines (result: ImplementationFileParseResult) =
        result.Diagnostics
        |> Seq.map (fun diagnostic ->
            let range = diagnostic.Range

            $"Program.fs({range.Start.Line},{range.Start.Column},{range.End.Line},{range.End.Column}): {SyntaxDiagnosticText.severity diagnostic} {diagnostic.Code}: {diagnostic.Message}"
        )
        |> Seq.toList

    [<Tests>]
    let tests =
        testList "Issue29.ParserAdjacency" [
            testList "an adjacent argument belongs to the dot lambda body" [
                for text, expected in dotLambdaCases do
                    for mode in
                        [
                            "8.0"
                            "10.0"
                        ] ->
                        testCase $"{text} {mode}"
                        <| fun _ ->
                            Expect.equal
                                (shape (body mode text))
                                expected
                                "The Oracle type-checks one adjacent argument inside the lambda and a later spaced argument as a separate argument"
            ]

            testList "a dot lambda body with more than one adjacent step stays explicit" [
                for text in unprovenDotLambdaCases ->
                    testCase text
                    <| fun _ ->
                        let result = parse "10.0" $"module Program\nlet f = {text}\n"

                        Expect.exists
                            result.Diagnostics
                            (fun diagnostic -> diagnostic.Code = "FSC2P1001")
                            "The Oracle type checker reports FS3584 here, so the tree is not proven"

                        Expect.all
                            result.Diagnostics
                            (fun diagnostic -> diagnostic.Code = "FSC2P1001")
                            "The Oracle reports no parse diagnostic here"
            ]

            testList "an adjacent bracket is a high-precedence application at every version" [
                for text, expected in bracketCases do
                    for mode in
                        [
                            "5.0"
                            "6.0"
                            "7.0"
                            "10.0"
                        ] ->
                        testCase $"{text} {mode}"
                        <| fun _ ->
                            Expect.equal
                                (shape (body mode text))
                                expected
                                "At 5.0 and 6.0 the Oracle type checker reads an adjacent bracket after a name as one argument, and a second bracket as a separate argument"
            ]

            testList "an argument with an adjacent parenthesized argument reports FS0597" [
                for text, expected in successiveArgumentCases do
                    for mode in
                        [
                            "5.0"
                            "10.0"
                        ] ->
                        testCase $"{text} {mode}"
                        <| fun _ ->
                            Expect.sequenceEqual
                                (oracleLines (parse mode $"module Program\nlet x = {text}\n"))
                                expected
                                "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an adjacent bracket after a dot lambda before F# 8.0 reports only FS3350" [
                for text, startColumn in
                    [
                        "_.Items[0]", 9
                        "g _.Items[0]", 11
                    ] ->
                    testCase text
                    <| fun _ ->
                        let result = parse "5.0" $"module Program\nlet f = {text}\n"

                        Expect.sequenceEqual
                            (oracleLines result)
                            [
                                $"Program.fs(2,{startColumn},2,{startColumn
                                                                + 2}): error FS3350: Feature 'underscore dot shorthand for accessor only function' is not available in F# 5.0. Please use language version 8.0 or greater."
                            ]
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testCase "an adjacent argument after an unsupported index target stays explicit"
            <| fun _ ->
                let result = parse "10.0" "module Program\nlet f = arr[0..1]\n"

                Expect.exists
                    result.Diagnostics
                    (fun diagnostic -> diagnostic.Code = "FSC2P1001")
                    "A slice is not modeled, so the parser reports an explicit diagnostic"
        ]
