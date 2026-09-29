namespace fsharp2.Tests

open System
open Expecto
open FSharp2.Compiler

module ParserGrammarTests =
    let private parseAt mode logicalPath text =
        let language =
            LanguageVersion.normalize (Some mode)
            |> Result.defaultWith failtest

        SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
        |> LexicalPipeline.prepare language Array.empty
        |> Parser.parseImplementationFile

    let private parse = parseAt "10.0"

    let private position (range: SourceRange) =
        range.Start.Line, range.Start.Column, range.End.Line, range.End.Column

    let private oracleLines logicalPath (result: ImplementationFileParseResult) =
        result.Diagnostics
        |> Seq.map (fun diagnostic ->
            $"{logicalPath}({diagnostic.Range.Start.Line},{diagnostic.Range.Start.Column}): {SyntaxDiagnosticText.severity diagnostic} {diagnostic.Code}: {diagnostic.Message}"
        )
        |> Seq.toList

    let rec private typeShape syntaxType =
        match syntaxType with
        | SyntaxType.LongIdentifier name -> name.Text
        | SyntaxType.GlobalLongIdentifier(_, None, _) -> "global"
        | SyntaxType.GlobalLongIdentifier(_, Some name, _) -> $"global.{name.Text}"
        | SyntaxType.Variable variable -> variable.Text
        | SyntaxType.Application(typeConstructor, arguments, true, _) ->
            $"""{arguments
                 |> Seq.map typeShape
                 |> String.concat ", "} {typeShape typeConstructor}"""
        | SyntaxType.Application(typeConstructor, arguments, false, _) ->
            $"""{typeShape typeConstructor}<{arguments
                                             |> Seq.map typeShape
                                             |> String.concat ", "}>"""
        | SyntaxType.Function(argument, result, _) ->
            $"({typeShape argument} -> {typeShape result})"
        | SyntaxType.Tuple(elements, _) ->
            $"""({elements
                  |> Seq.map typeShape
                  |> String.concat " * "})"""
        | SyntaxType.Parenthesized(inner, _) -> typeShape inner
        | SyntaxType.SignatureParameter(name, parameterType, _) ->
            $"{name.Text}: {typeShape parameterType}"
        | SyntaxType.Missing _ -> "<missing>"

    let rec private patternShape pattern =
        match pattern with
        | SyntaxPattern.Named name -> name.Text
        | SyntaxPattern.Wildcard _ -> "_"
        | SyntaxPattern.Constant(SyntaxConstant.Numeric text, _) -> text
        | SyntaxPattern.Constant(SyntaxConstant.Unit, _) -> "()"
        | SyntaxPattern.Constant(other, _) -> string other
        | SyntaxPattern.Parenthesized(inner, _) -> $"({patternShape inner})"
        | SyntaxPattern.Tuple(items, _) ->
            items
            |> Seq.map patternShape
            |> String.concat ", "
        | SyntaxPattern.UnionCase(name, Some argument, _) -> $"{name.Text} {patternShape argument}"
        | SyntaxPattern.UnionCase(name, None, _) -> name.Text
        | SyntaxPattern.List(items, _) ->
            $"""[{items
                  |> Seq.map patternShape
                  |> String.concat "; "}]"""
        | SyntaxPattern.Typed(inner, patternType, _) ->
            $"{patternShape inner}: {typeShape patternType}"
        | SyntaxPattern.Missing _ -> "<missing>"

    let rec private expressionShape expression =
        match expression with
        | SyntaxExpression.Constant(SyntaxConstant.Numeric text, _)
        | SyntaxExpression.Constant(SyntaxConstant.String text, _) -> text
        | SyntaxExpression.Constant(SyntaxConstant.Unit, _) -> "()"
        | SyntaxExpression.Constant(other, _) -> string other
        | SyntaxExpression.Identifier name -> name.Text
        | SyntaxExpression.Parenthesized(inner, _) -> $"({expressionShape inner})"
        | SyntaxExpression.Tuple(items, _) ->
            items
            |> Seq.map expressionShape
            |> String.concat ", "
        | SyntaxExpression.Application(func, argument, _) ->
            $"[{expressionShape func} {expressionShape argument}]"
        | SyntaxExpression.Infix(operator, left, right, _) ->
            $"{{{expressionShape left} {operator.Text} {expressionShape right}}}"
        | SyntaxExpression.If(condition, thenBranch, elseBranch, _) ->
            let elseText =
                elseBranch
                |> Option.map (fun branch -> $" else {expressionShape branch}")
                |> Option.defaultValue ""

            $"if {expressionShape condition} then {expressionShape thenBranch}{elseText}"
        | SyntaxExpression.Match(input, clauses, _) ->
            let clauseText (clause: SyntaxMatchClause) =
                let guard =
                    clause.Guard
                    |> Option.map (fun guard -> $" when {expressionShape guard}")
                    |> Option.defaultValue ""

                $"| {patternShape clause.Pattern}{guard} -> {expressionShape clause.Result}"

            $"""match {expressionShape input} with {clauses
                                                    |> Seq.map clauseText
                                                    |> String.concat " "}"""
        | SyntaxExpression.Lambda(patterns, body, _) ->
            $"""fun {patterns
                     |> Seq.map patternShape
                     |> String.concat " "} -> {expressionShape body}"""
        | SyntaxExpression.List(items, _) ->
            $"""[{items
                  |> Seq.map expressionShape
                  |> String.concat "; "}]"""
        | SyntaxExpression.Record(fields, _) ->
            let fieldText (field: SyntaxRecordFieldValue) =
                $"{field.Name.Text} = {expressionShape field.Value}"

            $"""{{{fields
                   |> Seq.map fieldText
                   |> String.concat "; "}}}"""
        | SyntaxExpression.DotLambda(body, _) -> $"_.{expressionShape body}"
        | SyntaxExpression.BracketApplication(target, index, _) ->
            $"{expressionShape target}[{expressionShape index}]"
        | SyntaxExpression.Missing _ -> "<missing>"

    let private memberShape (value: SyntaxMember) =
        let prefix =
            match value.Kind with
            | SyntaxMemberKind.Instance self -> $"{self.Text}."
            | SyntaxMemberKind.Static -> "static "

        let parameters =
            value.Parameters
            |> Seq.map patternShape
            |> String.concat " "

        $"member {prefix}{value.Name.Text} {parameters} = {expressionShape value.Body}"

    let private typeDefinitionShape (definition: SyntaxTypeDefinition) =
        let constructor =
            definition.PrimaryConstructor
            |> Option.map patternShape
            |> Option.defaultValue ""

        let representation =
            match definition.Representation with
            | SyntaxTypeRepresentation.Record fields ->
                fields
                |> Seq.map (fun field ->
                    let mutability = if field.IsMutable then "mutable " else ""
                    $"{mutability}{field.Name.Text}: {typeShape field.Type}"
                )
                |> String.concat "; "
                |> sprintf "{ %s }"
            | SyntaxTypeRepresentation.Union cases ->
                cases
                |> Seq.map (fun case ->
                    let fields =
                        case.Fields
                        |> Seq.map (fun field ->
                            match field.Name with
                            | Some name -> $"{name.Text}: {typeShape field.Type}"
                            | None -> typeShape field.Type
                        )
                        |> String.concat " * "

                    if case.Fields.IsEmpty then
                        $"| {case.Name.Text}"
                    else
                        $"| {case.Name.Text} of {fields}"
                )
                |> String.concat " "
            | SyntaxTypeRepresentation.Abbreviation abbreviated -> typeShape abbreviated
            | SyntaxTypeRepresentation.Class members ->
                members
                |> Seq.map memberShape
                |> String.concat "; "
            | SyntaxTypeRepresentation.Missing _ -> "<missing>"

        $"type {definition.Name.Text}{constructor} = {representation}"

    let rec private declarationShape declaration =
        match declaration with
        | ImplementationDeclaration.Type group ->
            Seq.append [ group.First ] group.Rest
            |> Seq.map typeDefinitionShape
            |> String.concat " and "
        | ImplementationDeclaration.Let(_, _, bindings, _) ->
            let binding = Seq.exactlyOne bindings

            let head =
                Seq.append [ binding.Head ] binding.Parameters
                |> Seq.map patternShape
                |> String.concat " "

            $"let {head} = {expressionShape binding.Body}"
        | ImplementationDeclaration.Expression(attributes, body, _, _) when attributes.IsEmpty ->
            $"expr {expressionShape body}"
        | ImplementationDeclaration.Expression(attributes, body, _, _) ->
            $"expr [{attributes.Length} attribute lists] {expressionShape body}"
        | ImplementationDeclaration.NestedModule nested ->
            let inner =
                nested.Declarations
                |> Seq.map declarationShape
                |> String.concat "; "

            $"module {nested.Name.Text} = [{inner}]"
        | ImplementationDeclaration.Skipped _ -> "skipped"
        | other -> string other

    let private shapes logicalPath text =
        let result = parse logicalPath text

        Expect.equal
            (oracleLines logicalPath result)
            []
            "The Compatibility Oracle reports no parse diagnostics"

        let root = Seq.exactlyOne result.File.Contents

        root.Declarations
        |> Seq.map declarationShape
        |> Seq.toList,
        root.Declarations
        |> Seq.map (fun declaration -> position declaration.Range)
        |> Seq.toList

    let private symbol text = $"symbol '{text}'"
    let private keyword text = $"keyword '{text}'"

    let private closingTokens = [
        ")", symbol ")"
        "]", symbol "]"
        "}", symbol "}"
        "|]", symbol "|]"
        ">]", symbol ">]"
        "end", keyword "end"
        "done", keyword "done"
        "of", keyword "of"
        "elif", keyword "elif"
    ]

    let private recoveryPoints = [
        "ClauseArrow",
        "module Program\nlet f x =\n    match x with\n    | A {0} 1\n",
        (4, 9),
        "in pattern matching. Expected '->' or other token.",
        []

        "ClauseResult",
        "module Program\nlet f x =\n    match x with\n    | A -> {0}\n",
        (4, 12),
        "in pattern matching",
        []

        "LambdaArrow",
        "module Program\nlet f = fun x {0} x\n",
        (2, 15),
        "in lambda expression. Expected '->' or other token.",
        []

        "MatchWith",
        "module Program\nlet f x = match x {0} A -> 1\n",
        (2, 19),
        "in expression. Expected 'with' or other token.",
        []

        "RecordValue", "module Program\nlet r = {{ X = {0} }}\n", (2, 15), "in expression", [ "}" ]

        "FieldType",
        "module Program\ntype P = {{ X: {0} }}\n",
        (2, 15),
        "in field declaration",
        [ "}" ]

        "FieldColon",
        "module Program\ntype P = {{ X {0} int }}\n",
        (2, 14),
        "in field declaration. Expected ':' or other token.",
        [ "}" ]

        "CaseType", "module Program\ntype U =\n    | A of {0}\n", (3, 12), "in union case", []

        "CaseName", "module Program\ntype U =\n    | {0}\n", (3, 7), "in union case", [ "of" ]

        "TypeEquals",
        "module Program\ntype T {0} int\n",
        (2, 8),
        "in type definition. Expected '=' or other token.",
        [
            ")"
            "}"
            "end"
        ]

        "FirstCaseType", "module Program\ntype U = A of {0}\n", (2, 15), "in type definition", []

        "LambdaStart", "module Program\nlet f = fun {0} -> 1\n", (2, 13), "in lambda expression", []

        "NamespaceDefinitionStart",
        "namespace A\nlet a = 1\n{0}\n",
        (3, 1),
        "in implementation file. Expected incomplete structured construct at or before this point or other token.",
        []

        "AnonymousDefinitionStart", "let a = 1\n{0}\n", (2, 1), "in implementation file", []

        "TypeDefinitionStart",
        "module Program\ntype T {0} int\n",
        (2, 8),
        "in definition. Expected incomplete structured construct at or before this point or other token.",
        [
            "]"
            "|]"
            ">]"
            "done"
            "of"
            "elif"
        ]
    ]

    let private recoveryCases = [
        for point, template, (line, column), suffix, excluded in recoveryPoints do
            for token, description in closingTokens do
                if not (List.contains token excluded) then
                    let logicalPath = $"{point}.fs"

                    logicalPath,
                    String.Format(template, token),
                    $"{logicalPath}({line},{column}): error FS0010: Unexpected {description} {suffix}"
    ]

    let private unsupportedCases = [
        "CaseNameOf.fs", "module Program\ntype U =\n    | of\n"
        "RecordValueBrace.fs", "module Program\nlet r = { X = } }\n"
        "FieldTypeBrace.fs", "module Program\ntype P = { X: } }\n"
        "FieldColonBrace.fs", "module Program\ntype P = { X } int }\n"
        "IfWithoutThen.fs", "module Program\nlet f x = if x 1 else 2\nlet first = 1\n"
        "ListElement.fs", "module Program\nlet xs = [ 1; ) ]\nlet first = 1\n"
        "RecordWithoutEquals.fs", "module Program\nlet r = { X ) 1 }\nlet first = 1\n"
        "MemberBody.fs",
        "module Program\ntype C() =\n    member this.X = )\n    member this.Y = 1\nlet first = 1\n"
        "TypeName.fs", "module Program\ntype ) = int\nlet first = 1\n"
    ]

    let private explicitAfterOracleCases = [
        "NestedFirstThenNested.fs",
        "module M\nmodule N =\n    )\n    let b = )\nlet c = )\n",
        [
            "NestedFirstThenNested.fs(3,5): error FS0010: Unexpected symbol ')' in definition"
            "NestedFirstThenNested.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "LambdaBeforeClauseBar.fs",
        "module P\nlet f x = match x with | A -> fun y -> y | B -> id\n",
        [
            "LambdaBeforeClauseBar.fs(2,42): error FS0010: Unexpected symbol '|' in lambda expression. Expected incomplete structured construct at or before this point or other token."
            "LambdaBeforeClauseBar.fs(3,1): error FS0010: Incomplete structured construct at or before this point in binding. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private nestedFirstCases = [
        "NestedFirstOnly.fs",
        "module M\nmodule N =\n    )\n",
        [ "NestedFirstOnly.fs(3,5): error FS0010: Unexpected symbol ')' in definition" ]

        "NestedFirstKeyword.fs",
        "module M\nmodule N =\n    end\n",
        [ "NestedFirstKeyword.fs(3,5): error FS0010: Unexpected keyword 'end' in definition" ]

        "NestedFirstEquals.fs",
        "module M\nmodule N =\n    =\n",
        [ "NestedFirstEquals.fs(3,5): error FS0010: Unexpected symbol '=' in definition" ]

        "NestedFirstThenRoot.fs",
        "module M\nmodule N =\n    )\nlet c = )\n",
        [
            "NestedFirstThenRoot.fs(3,5): error FS0010: Unexpected symbol ')' in definition"
            "NestedFirstThenRoot.fs(4,9): error FS0010: Unexpected symbol ')' in binding"
        ]

        "NestedFirstThenRootLater.fs",
        "module M\nmodule N =\n    ]\nlet c = 1\nlet d = )\n",
        [
            "NestedFirstThenRootLater.fs(3,5): error FS0010: Unexpected symbol ']' in definition"
            "NestedFirstThenRootLater.fs(5,9): error FS0010: Unexpected symbol ')' in binding"
        ]

        "NestedFirstInNamespace.fs",
        "namespace X\nmodule N =\n    )\nlet c = )\n",
        [
            "NestedFirstInNamespace.fs(3,5): error FS0010: Unexpected symbol ')' in definition"
            "NestedFirstInNamespace.fs(4,9): error FS0010: Unexpected symbol ')' in binding"
        ]

        "NestedFirstThenModule.fs",
        "namespace X\nmodule N =\n    )\nmodule K =\n    let d = )\n",
        [
            "NestedFirstThenModule.fs(3,5): error FS0010: Unexpected symbol ')' in definition"
            "NestedFirstThenModule.fs(5,13): error FS0010: Unexpected symbol ')' in binding"
        ]
    ]

    let private laterDeclarationCases = [
        "NestedRecoveryThenDeclarations.fs",
        "namespace A\nmodule M =\n    let a = 1\n    )\n    let b = )\nlet c = )\n",
        [
            "NestedRecoveryThenDeclarations.fs(4,5): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "NestedRecoveryThenDeclarations.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "NestedRecoveryAtEnd.fs",
        "namespace A\nmodule M =\n    let a = 1\n    )\n",
        [
            "NestedRecoveryAtEnd.fs(4,5): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "NestedRecoveryAtEnd.fs(5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "UnionCaseAfterBar.fs",
        "module P\ntype U = A | B of )\n",
        [ "UnionCaseAfterBar.fs(2,19): error FS0010: Unexpected symbol ')' in union case" ]

        "BlockFirstCase.fs",
        "module P\ntype U =\n    A of )\n",
        [ "BlockFirstCase.fs(3,10): error FS0010: Unexpected symbol ')' in type definition" ]

        "LambdaWithoutPatterns.fs",
        "module P\nlet f = fun -> 1\n",
        [
            "LambdaWithoutPatterns.fs(2,13): error FS0010: Unexpected symbol '->' in lambda expression"
        ]

        "SecondNamespace.fs",
        "module P\nlet a = 1\n)\nnamespace B\nlet b = )\n",
        [
            "SecondNamespace.fs(3,1): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "NamespaceCascade.fs",
        "namespace A\nlet a = 1\n)\nlet b = )\nmodule M =\n    let c = )\n",
        [
            "NamespaceCascade.fs(3,1): error FS0010: Unexpected symbol ')' in implementation file. Expected incomplete structured construct at or before this point or other token."
        ]

        "AnonymousCascade.fs",
        "let a = 1\n)\nlet b = )\ntype T = { X: ) }\n",
        [ "AnonymousCascade.fs(2,1): error FS0010: Unexpected symbol ')' in implementation file" ]

        "ClauseArrowLater.fs",
        "module Program\nlet f x =\n    match x with\n    | A ) 1\n    | B -> 2\nlet later = )\n",
        [
            "ClauseArrowLater.fs(4,9): error FS0010: Unexpected symbol ')' in pattern matching. Expected '->' or other token."
            "ClauseArrowLater.fs(6,13): error FS0010: Unexpected symbol ')' in binding"
        ]

        "LambdaArrowLater.fs",
        "module Program\nlet f = fun x ) x\nlet later = )\n",
        [
            "LambdaArrowLater.fs(2,15): error FS0010: Unexpected symbol ')' in lambda expression. Expected '->' or other token."
            "LambdaArrowLater.fs(3,13): error FS0010: Unexpected symbol ')' in binding"
        ]

        "RecordValueLater.fs",
        "module Program\nlet r = { X = ); Y = 1 }\nlet later = )\n",
        [
            "RecordValueLater.fs(2,15): error FS0010: Unexpected symbol ')' in expression"
            "RecordValueLater.fs(3,13): error FS0010: Unexpected symbol ')' in binding"
        ]

        "FieldTypeLater.fs",
        "module Program\ntype P = { X: ); Y: int }\nlet later = )\n",
        [
            "FieldTypeLater.fs(2,15): error FS0010: Unexpected symbol ')' in field declaration"
            "FieldTypeLater.fs(3,13): error FS0010: Unexpected symbol ')' in binding"
        ]

        "CaseTypeLater.fs",
        "module Program\ntype U =\n    | A of int * )\n    | B\nlet later = )\n",
        [
            "CaseTypeLater.fs(3,18): error FS0010: Unexpected symbol ')' in union case"
            "CaseTypeLater.fs(5,13): error FS0010: Unexpected symbol ')' in binding"
        ]

        "TypeDefinitionStartLater.fs",
        "module Program\ntype T end int\nlet later = )\n",
        [
            "TypeDefinitionStartLater.fs(2,8): error FS0010: Unexpected keyword 'end' in definition. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private lessThanPrelude =
        "module Lt\nlet a = 1\nlet b = 2\nlet f (x: int) = x\n"

    let private adjacentComparisonCases = [
        "a<b", "{a < b}"
        "a< b", "{a < b}"
        "a<<<1", "{a <<< 1}"
        "a<=b", "{a <= b}"
        "a<>b", "{a <> b}"
        "a<b && b>a", "{{a < b} && {b > a}}"
        "(a)<b", "{(a) < b}"
        "1<b", "{1 < b}"
        "a<b = true", "{{a < b} = Boolean true}"
        "f a<b", "{[f a] < b}"
        "f<|1", "{f <| 1}"
        "a<b + 1", "{a < {b + 1}}"
        "a<1", "{a < 1}"
        "a<1 && b>a", "{{a < 1} && {b > a}}"
        "a<b || b>a", "{{a < b} || {b > a}}"
        "if a<b then b>a else false", "if {a < b} then {b > a} else Boolean false"
        "a<(b)", "{a < (b)}"
        "(a<b)", "({a < b})"
        "match a<b with | true -> b>a | _ -> false",
        "match {a < b} with | Boolean true -> {b > a} | _ -> Boolean false"
        "[ a<b ], b>a", "[{a < b}], {b > a}"
        "{ A = a<b }, b>a", "{A = {a < b}}, {b > a}"
        "if true then a<b else b>a", "if Boolean true then {a < b} else {b > a}"
        "a<b [ b>a ]", "{a < [b [{b > a}]]}"
    ]

    let private adjacentTypeArgumentCases = [
        "TypeApp.fs", "let c = id<int> 1\n", []
        "TypeClose.fs", "let c = a<b>a\n", []
        "TypeComma.fs", "let c = (a<b, b>a)\n", []
        "TypeLines.fs",
        "let c = a<b\nb>a\n",
        [
            "TypeLines.fs(6,3): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "TypeLines.fs(5,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "TypeLinesComma.fs", "let c = (a<b,\n         b>a)\n", []
        "TypeMinus.fs",
        "let c = a<b - 1>a\n",
        [
            "TypeMinus.fs(5,13): error FS0010: Unexpected symbol '-' in type arguments. Expected ',' or other token."
            "TypeMinus.fs(5,13): error FS1241: Expected type argument or static argument"
        ]
        "TypeStar.fs", "let c = a<b * b>a\n", []
        "TypeSemicolon.fs",
        "let c = [a<b; b>a]\n",
        [
            "TypeSemicolon.fs(5,13): error FS0010: Unexpected symbol ';' in type arguments. Expected ',' or other token."
            "TypeSemicolon.fs(5,13): error FS1241: Expected type argument or static argument"
        ]
        "TypeNumber.fs", "let c = a<1>a\n", []
        "TypeDot.fs", "let c = a<b.x>a\n", []
        "TypeArrow.fs",
        "let c = match a with | v when v<b -> b>a | _ -> false\n",
        [
            "TypeArrow.fs(5,42): error FS0010: Unexpected symbol '|' in pattern matching. Expected '->' or other token."
        ]
        "TypeString.fs", "let c = a<b, \"x\", b>a\n", []
    ]

    [<Tests>]
    let tests =
        testList "Issue29.ParserGrammar" [
            testList "an adjacent less-than without closing type arguments is a comparison" [
                for text, expected in adjacentComparisonCases ->
                    testCase text
                    <| fun _ ->
                        let declarations, _ = shapes "Lt.fs" $"{lessThanPrelude}let c = {text}\n"

                        Expect.equal
                            (List.last declarations)
                            $"let c = {expected}"
                            "The Oracle type checker accepts this binding as a comparison"
            ]

            testCase "an offside bar ends a same-line abbreviation or union"
            <| fun _ ->
                let firstShape text =
                    let result = parse "Offside.fs" text

                    (Seq.exactlyOne result.File.Contents).Declarations
                    |> Seq.head
                    |> declarationShape

                Expect.equal
                    (firstShape "module P\ntype U = string\n| B\n")
                    "type U = string"
                    "An identifier before an offside '|' is an abbreviation"

                Expect.equal
                    (firstShape "module P\ntype U = A of int\n| B\n")
                    "type U = | A of int"
                    "The union ends before an offside '|'"

            testCase "a let keyword on the next line ends the type argument scan"
            <| fun _ ->
                let declarations, _ =
                    shapes "LtLet.fs" $"{lessThanPrelude}let c = a<b\nlet d = b>a\n"

                Expect.sequenceEqual
                    (List.skip 3 declarations)
                    [
                        "let c = {a < b}"
                        "let d = {b > a}"
                    ]
                    "The Oracle type checker accepts both bindings as comparisons"

            testList "the adjacent less-than rule is the same in F# 4.6" [
                testCase "a comparison"
                <| fun _ ->
                    let result = parseAt "4.6" "Lt46.fs" $"{lessThanPrelude}let c = a<b && b>a\n"

                    Expect.isEmpty
                        result.Diagnostics
                        "The Oracle type checker accepts this binding at 4.6"

                testCase "closed type arguments"
                <| fun _ ->
                    let result = parseAt "4.6" "Lt46.fs" $"{lessThanPrelude}let c = a<b>a\n"

                    SyntaxDiagnosticText.expectExplicitlyUnsupported
                        []
                        result.Diagnostics
                        (oracleLines "Lt46.fs" result)
            ]

            testList "an adjacent less-than with closing type arguments stays explicit" [
                for logicalPath, text, oracle in adjacentTypeArgumentCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse
                                logicalPath
                                (lessThanPrelude
                                 + text)

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testCase "top-level expressions are declarations with their structure and ranges"
            <| fun _ ->
                let declarations, ranges =
                    shapes
                        "TopLevel.fs"
                        "module TopLevel
1 + 2
printfn \"a\"
let b = 1
f (
    1)
(1, 2)
[1; 2]
if true then 1 else 2
fun x -> x
match 1 with
| _ -> ()
[<A>]
1
\"a\"
module Nested =
    f 1
"

                Expect.sequenceEqual
                    declarations
                    [
                        "expr {1 + 2}"
                        "expr [printfn \"a\"]"
                        "let b = 1"
                        "expr [f (1)]"
                        "expr (1, 2)"
                        "expr [1; 2]"
                        "expr if Boolean true then 1 else 2"
                        "expr fun x -> x"
                        "expr match 1 with | _ -> ()"
                        "expr [1 attribute lists] 1"
                        "expr \"a\""
                        "module Nested = [expr [f 1]]"
                    ]
                    "Top-level expression declarations"

                Expect.sequenceEqual
                    ranges
                    [
                        2, 1, 2, 6
                        3, 1, 3, 12
                        4, 1, 4, 10
                        5, 1, 6, 7
                        7, 1, 7, 7
                        8, 1, 8, 7
                        9, 1, 9, 22
                        10, 1, 10, 11
                        11, 1, 12, 10
                        13, 1, 14, 2
                        15, 1, 15, 4
                        16, 1, 17, 8
                    ]
                    "Top-level expression declaration ranges"

            testCase "a top-level expression continued on an indented line stays explicit"
            <| fun _ ->
                let result =
                    parse
                        "Continued.fs"
                        "module Continued
x
    1
"

                SyntaxDiagnosticText.expectExplicitlyUnsupported
                    []
                    result.Diagnostics
                    (oracleLines "Continued.fs" result)

            testCase
                "record, union, class, and abbreviation type definitions keep their structure and ranges"
            <| fun _ ->
                let declarations, ranges =
                    shapes
                        "Types.fs"
                        "module Shapes

type Point = { X: int; mutable Y: int }

type Shape =
    | Circle of radius: float
    | Square of float
    | Empty

type Counter(start: int) =
    member this.Start = start
    member _.Add(x: int, y: int) = x + y + start
    static member Zero = Counter(0)

type Alias = int list
"

                Expect.sequenceEqual
                    declarations
                    [
                        "type Point = { X: int; mutable Y: int }"
                        "type Shape = | Circle of radius: float | Square of float | Empty"
                        "type Counter(start: int) = member this.Start  = start; member _.Add (x: int, y: int) = {{x + y} + start}; member static Zero  = [Counter (0)]"
                        "type Alias = int list"
                    ]
                    "Type definitions"

                Expect.sequenceEqual
                    ranges
                    [
                        3, 1, 3, 40
                        5, 1, 8, 12
                        10, 1, 13, 36
                        15, 1, 15, 22
                    ]
                    "Type definition ranges"

            testCase
                "match, if, lambda, list, and record expressions keep their structure and ranges"
            <| fun _ ->
                let declarations, ranges =
                    shapes
                        "Expressions.fs"
                        "module Expressions

let classify value =
    match value with
    | Some 0 -> \"zero\"
    | Some n when n > 0 -> \"positive\"
    | Some _ -> \"negative\"
    | None -> \"none\"

let pick flag = if flag then 1 else 2

let choose flag =
    if flag then
        [ 1; 2; 3 ]
    else
        []

let increment = fun x -> x + 1

let apply = List.map (fun (x: int) y -> x + y) [ 1 ]

let origin = { X = 0; Y = 0 }

let items = [ origin.X; 1 ]
"

                Expect.sequenceEqual
                    declarations
                    [
                        "let classify value = match value with | Some 0 -> \"zero\" | Some n when {n > 0} -> \"positive\" | Some _ -> \"negative\" | None -> \"none\""
                        "let pick flag = if flag then 1 else 2"
                        "let choose flag = if flag then [1; 2; 3] else []"
                        "let increment = fun x -> {x + 1}"
                        "let apply = [[List.map (fun (x: int) y -> {x + y})] [1]]"
                        "let origin = {X = 0; Y = 0}"
                        "let items = [origin.X; 1]"
                    ]
                    "Expressions"

                Expect.sequenceEqual
                    ranges
                    [
                        3, 1, 8, 21
                        10, 1, 10, 38
                        12, 1, 16, 11
                        18, 1, 18, 31
                        20, 1, 20, 53
                        22, 1, 22, 30
                        24, 1, 24, 28
                    ]
                    "Declaration ranges"

            testList "each closing token reports the Oracle message at the new recovery points" [
                for logicalPath, text, oracle in recoveryCases ->
                    testCase text
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines logicalPath (parse logicalPath text))
                            [ oracle ]
                            "The diagnostic must match the Compatibility Oracle"
            ]

            testList "later declarations keep the diagnostics that the Oracle reports" [
                for logicalPath, text, oracle in
                    laterDeclarationCases
                    @ nestedFirstCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines logicalPath (parse logicalPath text))
                            oracle
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "recovery with more Oracle diagnostics than the parser models stays explicit" [
                for logicalPath, text, oracle in explicitAfterOracleCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.exists
                            result.Diagnostics
                            (fun diagnostic -> diagnostic.Code = "FSC2P1001")
                            "The parser must report an explicit FSC2P1001 diagnostic"

                        for line in oracleLines logicalPath result do
                            if not (line.Contains ": error FSC2P1001: ") then
                                Expect.contains
                                    oracle
                                    line
                                    "Each FS diagnostic must be a diagnostic that the Compatibility Oracle reports"
            ]

            testCase "a lambda clause result ends at the next aligned clause"
            <| fun _ ->
                let declarations, _ =
                    shapes
                        "LambdaClauses.fs"
                        "module P\nlet f x =\n    match x with\n    | A -> fun y -> y\n    | B -> id\n"

                Expect.sequenceEqual
                    declarations
                    [ "let f x = match x with | A -> fun y -> y | B -> id" ]
                    "Clauses"

            testList "unsupported recovery reports one explicit diagnostic per group" [
                for logicalPath, text in unsupportedCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.isNonEmpty result.Diagnostics "The parser reports a diagnostic"

                        Expect.equal
                            result.Diagnostics[0].Code
                            "FSC2P1001"
                            "The Oracle reports more diagnostics than the parser models, so the first diagnostic is explicit"
            ]
        ]
