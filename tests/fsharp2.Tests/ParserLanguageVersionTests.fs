namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module ParserLanguageVersionTests =
    let private parse mode logicalPath text =
        let language =
            LanguageVersion.normalize (Some mode)
            |> Result.defaultWith failtest

        SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
        |> LexicalPipeline.prepare language Array.empty
        |> Parser.parseImplementationFile

    let private oracleLines logicalPath (result: ImplementationFileParseResult) =
        result.Diagnostics
        |> Seq.map (fun diagnostic ->
            let range = diagnostic.Range

            $"{logicalPath}({range.Start.Line},{range.Start.Column},{range.End.Line},{range.End.Column}): error {diagnostic.Code}: {diagnostic.Message}"
        )
        |> Seq.toList

    let private unavailable logicalPath line startColumn endColumn version =
        $"{logicalPath}({line},{startColumn},{line},{endColumn}): error FS3350: Feature 'underscore dot shorthand for accessor only function' is not available in F# {version}. Please use language version 8.0 or greater."

    let private dotLambdaCases = [
        "DotLambda.fs", "module Program\nlet lengths = List.map _.Length [ \"a\" ]\n", [ 2, 24, 26 ]
        "Standalone.fs", "module Program\nlet f = _.Length\n", [ 2, 9, 11 ]
        "Chain.fs", "module Program\nlet f = _.A.B\n", [ 2, 9, 11 ]
        "SpaceBefore.fs", "module Program\nlet f = _ .Length\n", [ 2, 9, 12 ]
        "SpaceAfter.fs", "module Program\nlet f = _. Length\n", [ 2, 9, 11 ]
        "Parenthesized.fs", "module Program\nlet f = (_.Length)\n", [ 2, 10, 12 ]
        "Infix.fs", "module Program\nlet f = _.Length + 1\n", [ 2, 9, 11 ]
        "TwoArgs.fs",
        "module Program\nlet f = g _.A _.B\n",
        [
            2, 11, 13
            2, 15, 17
        ]
        "TwoInList.fs",
        "module Program\nlet f = [ _.A; _.B ]\n",
        [
            2, 11, 13
            2, 16, 18
        ]
    ]

    let rec private dotLambdaMembers expression =
        match expression with
        | SyntaxExpression.DotLambda(members, _) -> [ members.Text ]
        | SyntaxExpression.Application(func, argument, _) ->
            dotLambdaMembers func
            @ dotLambdaMembers argument
        | SyntaxExpression.Infix(_, left, right, _) ->
            dotLambdaMembers left
            @ dotLambdaMembers right
        | SyntaxExpression.Parenthesized(inner, _) -> dotLambdaMembers inner
        | SyntaxExpression.List(items, _) ->
            items
            |> Seq.collect dotLambdaMembers
            |> Seq.toList
        | _ -> []

    let private bindingBody (result: ImplementationFileParseResult) =
        match Seq.last (Seq.exactlyOne result.File.Contents).Declarations with
        | ImplementationDeclaration.Let(_, bindings, _) -> (Seq.exactlyOne bindings).Body
        | other -> failtest $"Expected a let declaration, found {other}"

    [<Tests>]
    let tests =
        testList "Issue29.ParserLanguageVersion" [
            testList "the underscore dot shorthand reports FS3350 before F# 8.0" [
                for logicalPath, text, ranges in dotLambdaCases do
                    for version in
                        [
                            "4.6"
                            "5.0"
                            "6.0"
                            "7.0"
                        ] ->
                        testCase $"{logicalPath} {version}"
                        <| fun _ ->
                            Expect.sequenceEqual
                                (oracleLines logicalPath (parse version logicalPath text))
                                [
                                    for line, startColumn, endColumn in ranges ->
                                        unavailable logicalPath line startColumn endColumn version
                                ]
                                "The diagnostics must match the Compatibility Oracle"
            ]

            testList "the underscore dot shorthand parses from F# 8.0" [
                for logicalPath, text, ranges in dotLambdaCases do
                    for version in
                        [
                            "8.0"
                            "9.0"
                            "10.0"
                            "default"
                            "preview"
                        ] ->
                        testCase $"{logicalPath} {version}"
                        <| fun _ ->
                            let result = parse version logicalPath text

                            Expect.isEmpty
                                (oracleLines logicalPath result)
                                "The Compatibility Oracle reports no diagnostics"

                            Expect.equal
                                (dotLambdaMembers (bindingBody result)).Length
                                ranges.Length
                                "Each shorthand is a dot lambda node"
            ]

            testList "feature diagnostics keep their order with recovery diagnostics" [
                for logicalPath, text, expected in
                    [
                        "GateThenBindingEnd.fs",
                        "module Program\nlet f = _.A )\nlet g = 1\n",
                        [
                            unavailable "GateThenBindingEnd.fs" 2 9 11 "7.0"
                            "GateThenBindingEnd.fs(2,13,2,14): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
                        ]

                        "GateAroundBindingEnd.fs",
                        "module Program\nlet f = g _.A ] _.B\n",
                        [
                            unavailable "GateAroundBindingEnd.fs" 2 11 13 "7.0"
                            "GateAroundBindingEnd.fs(2,15,2,16): error FS0010: Unexpected symbol ']' in binding. Expected incomplete structured construct at or before this point or other token."
                        ]

                        "GateInClauses.fs",
                        "module Program\nlet f x =\n    match x with\n    | A -> _.X\n    | B -> _.Y\n",
                        [
                            unavailable "GateInClauses.fs" 4 12 14 "7.0"
                            unavailable "GateInClauses.fs" 5 12 14 "7.0"
                        ]

                        "GateAfterFileRecovery.fs",
                        "module Program\nlet a = 1\n)\nlet f = _.A\n",
                        [
                            "GateAfterFileRecovery.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
                        ]
                    ] ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines logicalPath (parse "7.0" logicalPath text))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testCase "the dot lambda keeps its member path and range"
            <| fun _ ->
                let result = parse "10.0" "Chain.fs" "module Program\nlet f = _.A.B\n"

                match bindingBody result with
                | SyntaxExpression.DotLambda(members, range) ->
                    Expect.equal members.Text "A.B" "Member path"

                    Expect.equal
                        (range.Start.Line, range.Start.Column, range.End.Line, range.End.Column)
                        (2, 9, 2, 14)
                        "Dot lambda range"
                | other -> failtest $"Expected a dot lambda, found {other}"

            testCase "a bare underscore expression reports an explicit unsupported diagnostic"
            <| fun _ ->
                for text in
                    [
                        "module Program\nlet f = _\n"
                        "module Program\nlet f = _\nlet g = 1\n"
                        "module Program\nlet f = _ + 1\n"
                        "module Program\nlet f = _.\n"
                    ] do
                    let result = parse "10.0" "Underscore.fs" text

                    Expect.exists
                        result.Diagnostics
                        (fun diagnostic -> diagnostic.Code = "FSC2P1001")
                        $"The Oracle reports FS0010 here, which the parser does not model:\n{text}"

                    Expect.all
                        result.Diagnostics
                        (fun diagnostic -> diagnostic.Code = "FSC2P1001")
                        $"The parser must not invent an FS diagnostic:\n{text}"
        ]
