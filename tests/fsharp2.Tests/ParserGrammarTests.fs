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
        | SyntaxType.NestedName nested ->
            let arguments =
                nested.Enclosing.Arguments
                |> Seq.map typeShape
                |> String.concat ", "

            $"{typeShape nested.Enclosing.TypeName.Type}<{arguments}>.{nested.Name.Text}"
        | SyntaxType.Array(element, suffix, _) ->
            $"{typeShape element}[{System.String(',', suffix.Commas.Length)}]"
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

    let private bindingHead (binding: SyntaxBinding) =
        let mutability = if binding.IsMutable then "mutable " else ""

        Seq.append [ binding.Head ] binding.Parameters
        |> Seq.map patternShape
        |> String.concat " "
        |> sprintf "%s%s" mutability

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
        | SyntaxExpression.Sequential(first, second, _) ->
            $"seq[{expressionShape first}; {expressionShape second}]"
        | SyntaxExpression.LetOrUse(keyword, isRecursive, binding, body, _) ->
            let keywordText =
                match keyword with
                | SyntaxLetKeyword.Let -> "let"
                | SyntaxLetKeyword.Use -> "use"

            let recursive = if isRecursive then " rec" else ""

            $"{keywordText}{recursive} {bindingHead binding} = {expressionShape binding.Body} in {expressionShape body}"
        | SyntaxExpression.LongIdentifierSet(name, value, _) ->
            $"{{{name.Text} <- {expressionShape value}}}"
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
            $"let {bindingHead binding} = {expressionShape binding.Body}"
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
        | ImplementationDeclaration.Do(_, body, _) -> $"do {expressionShape body}"
        | ImplementationDeclaration.Skipped _ -> "skipped"
        | other -> string other

    let rec private sequentialRanges expression =
        match expression with
        | SyntaxExpression.Sequential(first, second, range) ->
            position range
            :: sequentialRanges first
            @ sequentialRanges second
        | SyntaxExpression.If(_, thenBranch, elseBranch, _) ->
            sequentialRanges thenBranch
            @ (elseBranch
               |> Option.map sequentialRanges
               |> Option.defaultValue [])
        | SyntaxExpression.Match(_, clauses, _) ->
            clauses
            |> Seq.collect (fun clause -> sequentialRanges clause.Result)
            |> Seq.toList
        | SyntaxExpression.Lambda(_, body, _) -> sequentialRanges body
        | _ -> []

    let rec private blockRanges expression =
        let text name range =
            let startLine, startColumn, endLine, endColumn = position range
            $"{name}({startLine},{startColumn}--{endLine},{endColumn})"

        match expression with
        | SyntaxExpression.Sequential(first, second, range) ->
            text "seq" range
            :: blockRanges first
            @ blockRanges second
        | SyntaxExpression.LetOrUse(_, _, binding, body, range) ->
            text "let" range
            :: blockRanges binding.Body
            @ blockRanges body
        | SyntaxExpression.LongIdentifierSet(_, value, range) ->
            text "set" range
            :: blockRanges value
        | SyntaxExpression.Infix(_, left, right, _) ->
            blockRanges left
            @ blockRanges right
        | SyntaxExpression.Tuple(items, _) ->
            items
            |> Seq.collect blockRanges
            |> Seq.toList
        | SyntaxExpression.Application(func, argument, _) ->
            blockRanges func
            @ blockRanges argument
        | SyntaxExpression.Parenthesized(inner, _) -> blockRanges inner
        | SyntaxExpression.If(_, thenBranch, elseBranch, _) ->
            blockRanges thenBranch
            @ (elseBranch
               |> Option.map blockRanges
               |> Option.defaultValue [])
        | SyntaxExpression.Match(_, clauses, _) ->
            clauses
            |> Seq.collect (fun clause -> blockRanges clause.Result)
            |> Seq.toList
        | SyntaxExpression.Lambda(_, body, _) -> blockRanges body
        | _ -> []

    let private declarationBodies (declarations: ImplementationDeclaration seq) =
        declarations
        |> Seq.collect (fun declaration ->
            match declaration with
            | ImplementationDeclaration.Let(_, _, bindings, _) ->
                bindings
                |> Seq.map _.Body
            | ImplementationDeclaration.Do(_, body, _) -> Seq.singleton body
            | ImplementationDeclaration.Expression(_, body, _, _) -> Seq.singleton body
            | ImplementationDeclaration.Type group ->
                Seq.append [ group.First ] group.Rest
                |> Seq.collect (fun definition ->
                    match definition.Representation with
                    | SyntaxTypeRepresentation.Class members ->
                        members
                        |> Seq.map _.Body
                    | _ -> Seq.empty
                )
            | _ -> Seq.empty
        )
        |> Seq.toList

    let rec private localBindings expression =
        match expression with
        | SyntaxExpression.LetOrUse(_, _, binding, body, _) ->
            binding
            :: localBindings binding.Body
            @ localBindings body
        | SyntaxExpression.Sequential(first, second, _) ->
            localBindings first
            @ localBindings second
        | _ -> []

    let rec private bindings (declarations: ImplementationDeclaration seq) =
        declarations
        |> Seq.toList
        |> List.collect (fun declaration ->
            match declaration with
            | ImplementationDeclaration.Let(_, _, bindings, _) ->
                bindings
                |> Seq.toList
                |> List.collect (fun binding ->
                    binding
                    :: localBindings binding.Body
                )
            | ImplementationDeclaration.NestedModule nested -> bindings nested.Declarations
            | _ -> []
        )

    let private bindingText (binding: SyntaxBinding) =
        let startLine, startColumn, _, _ =
            binding.Accessibility
            |> Option.map _.Range
            |> Option.defaultValue binding.Head.Range
            |> position

        let _, _, endLine, endColumn =
            Seq.append [ binding.Head ] binding.Parameters
            |> Seq.last
            |> _.Range
            |> position

        $"{bindingHead binding} = {expressionShape binding.Body} ({startLine},{startColumn}--{endLine},{endColumn})"

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

    // The Compatibility Oracle reports no parse diagnostic for each text. The ranges are the FCS 43.10.101 Sequential ranges, with 1-based columns.
    let private sequentialCases = [
        "SequentialTwo.fs",
        "module A\nlet f () =\n    ignore 1\n    2\n",
        [ "let f () = seq[[ignore 1]; 2]" ],
        [ 3, 5, 4, 6 ]
        "SequentialThree.fs",
        "module A\nlet f () =\n    ignore 1\n    ignore 2\n    3\n",
        [ "let f () = seq[[ignore 1]; seq[[ignore 2]; 3]]" ],
        [
            3, 5, 5, 6
            4, 5, 5, 6
        ]
        "SequentialEntryPoint.fs",
        "module A\n[<EntryPoint>]\nlet main argv =\n    printfn \"x\"\n    0\n",
        [ "let main argv = seq[[printfn \"x\"]; 0]" ],
        [ 4, 5, 5, 6 ]
        "SequentialDo.fs",
        "module A\ndo\n    ignore 1\n    ignore 2\n",
        [ "do seq[[ignore 1]; [ignore 2]]" ],
        [ 3, 5, 4, 13 ]
        "SequentialThen.fs",
        "module A\nlet f c =\n    if c then\n        ignore 1\n        2\n    else\n        3\n",
        [ "let f c = if c then seq[[ignore 1]; 2] else 3" ],
        [ 4, 9, 5, 10 ]
        "SequentialElse.fs",
        "module A\nlet f c =\n    if c then 1\n    else\n        ignore 1\n        2\n",
        [ "let f c = if c then 1 else seq[[ignore 1]; 2]" ],
        [ 5, 9, 6, 10 ]
        "SequentialClause.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        ignore 1\n        2\n    | _ -> 3\n",
        [ "let f x = match x with | 1 -> seq[[ignore 1]; 2] | _ -> 3" ],
        [ 5, 9, 6, 10 ]
        "SequentialLambda.fs",
        "module A\nlet f =\n    fun () ->\n        ignore 1\n        2\n",
        [ "let f = fun () -> seq[[ignore 1]; 2]" ],
        [ 4, 9, 5, 10 ]
        "SequentialBeforeLet.fs",
        "module A\nlet f () =\n    ignore 1\n    2\nlet g = 3\n",
        [
            "let f () = seq[[ignore 1]; 2]"
            "let g = 3"
        ],
        [ 3, 5, 4, 6 ]
        "SequentialComment.fs",
        "module A\nlet f () =\n    ignore 1\n\n    // c\n    2\n",
        [ "let f () = seq[[ignore 1]; 2]" ],
        [ 3, 5, 6, 6 ]
        "SequentialAfterIf.fs",
        "module A\nlet f c =\n    if c then ignore 1\n    2\n",
        [ "let f c = seq[if c then [ignore 1]; 2]" ],
        [ 3, 5, 4, 6 ]
        "SequentialAfterMatch.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> ignore 1\n    2\n",
        [ "let f x = seq[match x with | _ -> [ignore 1]; 2]" ],
        [ 3, 5, 5, 6 ]
        "SequentialMatchAfter.fs",
        "module A\nlet f () =\n    ignore 1\n    match 1 with\n    | _ -> 2\n",
        [ "let f () = seq[[ignore 1]; match 1 with | _ -> 2]" ],
        [ 3, 5, 5, 13 ]
        "SequentialIfAfter.fs",
        "module A\nlet f () =\n    ignore 1\n    if true then 2 else 3\n",
        [ "let f () = seq[[ignore 1]; if Boolean true then 2 else 3]" ],
        [ 3, 5, 4, 26 ]
        "SequentialLambdaAfter.fs",
        "module A\nlet f () =\n    ignore 1\n    fun x -> x\n",
        [ "let f () = seq[[ignore 1]; fun x -> x]" ],
        [ 3, 5, 4, 15 ]
        "SequentialAfterIfBlock.fs",
        "module A\nlet f c =\n    if c then\n        ignore 1\n    else\n        ignore 2\n    3\n",
        [ "let f c = seq[if c then [ignore 1] else [ignore 2]; 3]" ],
        [ 3, 5, 7, 6 ]
        "SequentialAfterThenBlock.fs",
        "module A\nlet f c =\n    if c then\n        ignore 1\n    3\n",
        [ "let f c = seq[if c then [ignore 1]; 3]" ],
        [ 3, 5, 5, 6 ]
        "SequentialAfterNestedBlocks.fs",
        "module A\nlet f c =\n    if c then\n        if c then\n            ignore 1\n    3\n",
        [ "let f c = seq[if c then if c then [ignore 1]; 3]" ],
        [ 3, 5, 6, 6 ]
        "SequentialAfterMatchBlock.fs",
        "module A\nlet f v =\n    match v with\n    | _ ->\n        ignore 1\n    2\n",
        [ "let f v = seq[match v with | _ -> [ignore 1]; 2]" ],
        [ 3, 5, 6, 6 ]
        "SequentialMember.fs",
        "module A\ntype T() =\n    member _.M() =\n        ignore 1\n        2\n",
        [ "type T() = member _.M () = seq[[ignore 1]; 2]" ],
        [ 4, 9, 5, 10 ]
    ]

    let private sequentialExplicitCases = [
        "SequentialClose.fs",
        "module A\nlet f () =\n    ignore 1\n    )\n",
        [
            "SequentialClose.fs(4,5): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "SequentialCloseAfter.fs",
        "module A\nlet f () =\n    ignore 1\n    2 )\n",
        [
            "SequentialCloseAfter.fs(4,7): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "SequentialInfix.fs", "module A\nlet f () =\n    1\n    + 2\n", []
    ]

    let private blockOffsideExplicitCases = [
        "BlockOffsideBinding.fs",
        "module A\nlet f =\n    g\n  1\n",
        [
            "BlockOffsideBinding.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BlockOffsideBinding.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ],
        "BlockOffsideBinding.fs(4,3)"
        "BlockOffsideBeforeRoot.fs",
        "module A\nlet f =\n    g\n  1\nlet h = 2\n",
        [
            "BlockOffsideBeforeRoot.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BlockOffsideBeforeRoot.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BlockOffsideBeforeRoot.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ],
        "BlockOffsideBeforeRoot.fs(4,3)"
        "BlockOffsideNestedBinding.fs",
        "module A\nmodule M =\n    let f =\n        g\n      1\n",
        [
            "BlockOffsideNestedBinding.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BlockOffsideNestedBinding.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ],
        "BlockOffsideNestedBinding.fs(5,7)"
        "BlockOffsideDo.fs",
        "module A\ndo\n    ignore\n  1\n",
        [
            "BlockOffsideDo.fs(4,3): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ],
        "BlockOffsideDo.fs(4,3)"
        "BlockOffsideNestedDo.fs",
        "module A\nmodule M =\n    do\n        ignore\n      1\n",
        [
            "BlockOffsideNestedDo.fs(5,7): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ],
        "BlockOffsideNestedDo.fs(5,7)"
    ]

    // The Compatibility Oracle reports no parse diagnostic for each text. The ranges are the FCS 43.10.101 LetOrUse and Sequential ranges, with 1-based columns.
    let private localLetCases = [
        "LocalLet.fs",
        "module A\nlet f () =\n    let x = 1\n    x\n",
        [ "let f () = let x = 1 in x" ],
        [ "let(3,5--4,6)" ]
        "LocalLetAfter.fs",
        "module A\nlet f () =\n    ignore 1\n    let x = 2\n    x\n",
        [ "let f () = seq[[ignore 1]; let x = 2 in x]" ],
        [
            "seq(3,5--5,6)"
            "let(4,5--5,6)"
        ]
        "LocalLetTwo.fs",
        "module A\nlet f () =\n    let x = 1\n    let y = 2\n    x\n",
        [ "let f () = let x = 1 in let y = 2 in x" ],
        [
            "let(3,5--5,6)"
            "let(4,5--5,6)"
        ]
        "LocalLetBlock.fs",
        "module A\nlet f () =\n    let x =\n        1\n    x\n",
        [ "let f () = let x = 1 in x" ],
        [ "let(3,5--5,6)" ]
        "LocalLetBlockSequential.fs",
        "module A\nlet f () =\n    let x =\n        ignore 1\n        2\n    x\n",
        [ "let f () = let x = seq[[ignore 1]; 2] in x" ],
        [
            "let(3,5--6,6)"
            "seq(4,9--5,10)"
        ]
        "LocalFunction.fs",
        "module A\nlet f () =\n    let g y = y\n    g 1\n",
        [ "let f () = let g y = y in [g 1]" ],
        [ "let(3,5--4,8)" ]
        "LocalUse.fs",
        "module A\nlet f () =\n    use x = 1\n    x\n",
        [ "let f () = use x = 1 in x" ],
        [ "let(3,5--4,6)" ]
        "LocalRec.fs",
        "module A\nlet f () =\n    let rec g x = g x\n    g 1\n",
        [ "let f () = let rec g x = [g x] in [g 1]" ],
        [ "let(3,5--4,8)" ]
        "LocalLetSequentialBody.fs",
        "module A\nlet f () =\n    let x = 1\n    ignore x\n    x\n",
        [ "let f () = let x = 1 in seq[[ignore x]; x]" ],
        [
            "let(3,5--5,6)"
            "seq(4,5--5,6)"
        ]
        "LocalLetEntryPoint.fs",
        "module A\n[<EntryPoint>]\nlet main argv =\n    let name = \"x\"\n    printfn \"%s\" name\n    0\n",
        [ "let main argv = let name = \"x\" in seq[[[printfn \"%s\"] name]; 0]" ],
        [
            "let(4,5--6,6)"
            "seq(5,5--6,6)"
        ]
        "LocalLetDo.fs",
        "module A\ndo\n    let x = 1\n    ignore x\n",
        [ "do let x = 1 in [ignore x]" ],
        [ "let(3,5--4,13)" ]
        "LocalLetThen.fs",
        "module A\nlet f c =\n    if c then\n        let x = 1\n        x\n    else\n        2\n",
        [ "let f c = if c then let x = 1 in x else 2" ],
        [ "let(4,9--5,10)" ]
        "LocalLetClause.fs",
        "module A\nlet f v =\n    match v with\n    | _ ->\n        let x = 1\n        x\n",
        [ "let f v = match v with | _ -> let x = 1 in x" ],
        [ "let(5,9--6,10)" ]
        "LocalLetLambda.fs",
        "module A\nlet f =\n    fun () ->\n        let x = 1\n        x\n",
        [ "let f = fun () -> let x = 1 in x" ],
        [ "let(4,9--5,10)" ]
        "LocalLetMember.fs",
        "module A\ntype T() =\n    member _.M() =\n        let x = 1\n        x\n",
        [ "type T() = member _.M () = let x = 1 in x" ],
        [ "let(4,9--5,10)" ]
        "LocalLetTuple.fs",
        "module A\nlet f () =\n    let a, b = 1, 2\n    a\n",
        [ "let f () = let a, b = 1, 2 in a" ],
        [ "let(3,5--4,6)" ]
        "LocalLetNested.fs",
        "module A\nlet f () =\n    let g () =\n        let y = 1\n        y\n    g ()\n",
        [ "let f () = let g () = let y = 1 in y in [g ()]" ],
        [
            "let(3,5--6,9)"
            "let(4,9--5,10)"
        ]
        "LocalLetBeforeRoot.fs",
        "module A\nlet f () =\n    let x = 1\n    x\nlet g = 2\n",
        [
            "let f () = let x = 1 in x"
            "let g = 2"
        ],
        [ "let(3,5--4,6)" ]
    ]

    // The ranges are the FCS 43.10.101 ranges. FCS gives the missing body an empty range after the binding.
    let private unfinishedLocalLetCases = [
        "UnfinishedAfter.fs",
        "module A\nlet f () =\n    ignore 1\n    let x = 2\n",
        [
            "UnfinishedAfter.fs(4,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = seq[[ignore 1]; let x = 2 in <missing>]" ],
        [
            "seq(3,5--4,14)"
            "let(4,5--4,14)"
        ]
        "UnfinishedOnly.fs",
        "module A\nlet f () =\n    let x = 2\n",
        [
            "UnfinishedOnly.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = let x = 2 in <missing>" ],
        [ "let(3,5--3,14)" ]
        "UnfinishedUse.fs",
        "module A\nlet f () =\n    use x = 2\n",
        [
            "UnfinishedUse.fs(3,5): error FS0588: The block following this 'use' is unfinished. Every code block is an expression and must have a result. 'use' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = use x = 2 in <missing>" ],
        [ "let(3,5--3,14)" ]
        "UnfinishedRec.fs",
        "module A\nlet f () =\n    let rec g x = g x\n",
        [
            "UnfinishedRec.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = let rec g x = [g x] in <missing>" ],
        [ "let(3,5--3,22)" ]
        "UnfinishedFunction.fs",
        "module A\nlet f () =\n    let g y = y\n",
        [
            "UnfinishedFunction.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = let g y = y in <missing>" ],
        [ "let(3,5--3,16)" ]
        "UnfinishedBlock.fs",
        "module A\nlet f () =\n    let x =\n        1\n",
        [
            "UnfinishedBlock.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = let x = 1 in <missing>" ],
        [ "let(3,5--4,10)" ]
        "UnfinishedInner.fs",
        "module A\nlet f () =\n    let x =\n        let y = 1\n    x\n",
        [
            "UnfinishedInner.fs(4,9): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f () = let x = let y = 1 in <missing> in x" ],
        [
            "let(3,5--5,6)"
            "let(4,9--4,18)"
        ]
        "UnfinishedThen.fs",
        "module A\nlet f c =\n    if c then\n        let x = 1\n    else\n        2\n",
        [
            "UnfinishedThen.fs(4,9): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "let f c = if c then let x = 1 in <missing> else 2" ],
        [ "let(4,9--4,18)" ]
        "UnfinishedDo.fs",
        "module A\ndo\n    let x = 1\n",
        [
            "UnfinishedDo.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [ "do let x = 1 in <missing>" ],
        [ "let(3,5--3,14)" ]
        "UnfinishedBeforeRoot.fs",
        "module A\nlet f () =\n    ignore 1\n    let x = 2\nlet g = 3\n",
        [
            "UnfinishedBeforeRoot.fs(4,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ],
        [
            "let f () = seq[[ignore 1]; let x = 2 in <missing>]"
            "let g = 3"
        ],
        [
            "seq(3,5--4,14)"
            "let(4,5--4,14)"
        ]
        "UnfinishedBeforeRootError.fs",
        "module A\nlet f () =\n    ignore 1\n    let x = 2\nlet g = )\n",
        [
            "UnfinishedBeforeRootError.fs(4,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
            "UnfinishedBeforeRootError.fs(5,9): error FS0010: Unexpected symbol ')' in binding"
        ],
        [
            "let f () = seq[[ignore 1]; let x = 2 in <missing>]"
            "let g = <missing>"
        ],
        [
            "seq(3,5--4,14)"
            "let(4,5--4,14)"
        ]
    ]

    let private localLetExplicitCases = [
        "LocalLetClose.fs",
        "module A\nlet f () =\n    let x = 1\n    )\n",
        [
            "LocalLetClose.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
            "LocalLetClose.fs(4,5): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "LocalLetIn.fs", "module A\nlet f () =\n    let x = 1 in x\n", []
        "LocalLetAnd.fs",
        "module A\nlet f () =\n    let a = 1\n    and b = 2\n    a\n",
        [
            "LocalLetAnd.fs(3,5): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]
        "LocalLetNoEquals.fs",
        "module A\nlet f () =\n    let x 1\n    x\n",
        [
            "LocalLetNoEquals.fs(4,5): error FS0010: Incomplete structured construct at or before this point in binding. Expected '=' or other token."
        ]
    ]

    // Compatibility Oracle reports no parse diagnostic. Ranges are the FCS 43.10.101 binding head pattern ranges, 1-based columns.
    let private mutableBindingCases = [
        "MutableRoot.fs", "module A\nlet mutable x = 1\n", [ "mutable x = 1 (2,13--2,14)" ]
        "MutablePrivate.fs",
        "module A\nlet mutable private x = 1\n",
        [ "mutable x = 1 (2,13--2,22)" ]
        "MutableRec.fs", "module A\nlet rec mutable x = 1\n", [ "mutable x = 1 (2,17--2,18)" ]
        "MutableAnd.fs",
        "module A\nlet rec f () = x\nand mutable x = 2\n",
        [
            "f () = x (2,9--2,13)"
            "mutable x = 2 (3,13--3,14)"
        ]
        "MutableParameters.fs",
        "module A\nlet mutable f x = x\n",
        [ "mutable f x = x (2,13--2,16)" ]
        "MutableTuple.fs",
        "module A\nlet mutable a, b = 1, 2\n",
        [ "mutable a, b = 1, 2 (2,13--2,17)" ]
        "MutableParenthesized.fs",
        "module A\nlet mutable (x) = 1\n",
        [ "mutable (x) = 1 (2,13--2,16)" ]
        "MutableWildcard.fs", "module A\nlet mutable _ = 1\n", [ "mutable _ = 1 (2,13--2,14)" ]
        "MutableBlock.fs", "module A\nlet mutable x =\n    1\n", [ "mutable x = 1 (2,13--2,14)" ]
        "MutableAttribute.fs",
        "module A\n[<DefaultValue>]\nlet mutable x = 1\n",
        [ "mutable x = 1 (3,13--3,14)" ]
        "MutableTwo.fs",
        "module A\nlet mutable x = 1\nlet mutable y = x\n",
        [
            "mutable x = 1 (2,13--2,14)"
            "mutable y = x (3,13--3,14)"
        ]
        "MutableNested.fs",
        "module A\nmodule M =\n    let mutable x = 1\n",
        [ "mutable x = 1 (3,17--3,18)" ]
        "LocalMutable.fs",
        "module A\nlet f () =\n    let mutable x = 1\n    x\n",
        [
            "f () = let mutable x = 1 in x (2,5--2,9)"
            "mutable x = 1 (3,17--3,18)"
        ]
        "LocalMutableRec.fs",
        "module A\nlet f () =\n    let rec mutable x = 1\n    x\n",
        [
            "f () = let rec mutable x = 1 in x (2,5--2,9)"
            "mutable x = 1 (3,21--3,22)"
        ]
        "LocalMutableEntryPoint.fs",
        "module A\n[<EntryPoint>]\nlet main _ =\n    let mutable count = 0\n    count\n",
        [
            "main _ = let mutable count = 0 in count (3,5--3,11)"
            "mutable count = 0 (4,17--4,22)"
        ]
    ]

    let private mutableBindingErrorCases = [
        "MutableTwice.fs",
        "module A\nlet mutable mutable x = 1\n",
        [ "MutableTwice.fs(2,13): error FS0010: Unexpected keyword 'mutable' in binding" ]
        "MutableAfterAccessibility.fs",
        "module A\nlet private mutable x = 1\n",
        [
            "MutableAfterAccessibility.fs(2,13): error FS0010: Unexpected keyword 'mutable' in binding"
        ]
        "MutableAfterAccessibilityBeforeRoot.fs",
        "module A\nlet mutable x = 1\nlet private mutable y = 2\nlet z = 3\n",
        [
            "MutableAfterAccessibilityBeforeRoot.fs(3,13): error FS0010: Unexpected keyword 'mutable' in binding"
        ]
        "MutableNoName.fs",
        "module A\nlet mutable = 1\n",
        [ "MutableNoName.fs(2,13): error FS0010: Unexpected symbol '=' in binding" ]
        "MutableValue.fs",
        "module A\nlet mutable x = )\n",
        [ "MutableValue.fs(2,17): error FS0010: Unexpected symbol ')' in binding" ]
        "MutableInBody.fs",
        "module A\nlet x = mutable\n",
        [ "MutableInBody.fs(2,9): error FS0010: Unexpected keyword 'mutable' in binding" ]
        "MutableInBodyBlock.fs",
        "module A\nlet x =\n    mutable\n",
        [ "MutableInBodyBlock.fs(3,5): error FS0010: Unexpected keyword 'mutable' in binding" ]
        "LocalMutableLast.fs",
        "module A\nlet f () =\n    let mutable x = 1\n",
        [
            "LocalMutableLast.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]
    ]

    // Compatibility Oracle reports no parse diagnostic. Ranges are FCS 43.10.101 LongIdentSet, LetOrUse, and Sequential ranges, 1-based columns.
    let private assignmentCases = [
        "AssignLocal.fs",
        "module A\nlet f () =\n    let mutable x = 0\n    x <- x + 1\n    x\n",
        [ "let f () = let mutable x = 0 in seq[{x <- {x + 1}}; x]" ],
        [
            "let(3,5--5,6)"
            "seq(4,5--5,6)"
            "set(4,5--4,15)"
        ]
        "AssignLong.fs",
        "module A\nlet f () =\n    a.b <- 1\n",
        [ "let f () = {a.b <- 1}" ],
        [ "set(3,5--3,13)" ]
        "AssignTuple.fs",
        "module A\nlet f () =\n    x <- 1, 2\n",
        [ "let f () = {x <- 1, 2}" ],
        [ "set(3,5--3,14)" ]
        "AssignApplication.fs",
        "module A\nlet f () =\n    x <- g 1\n",
        [ "let f () = {x <- [g 1]}" ],
        [ "set(3,5--3,13)" ]
        "AssignChain.fs",
        "module A\nlet f () =\n    x <- y <- 1\n",
        [ "let f () = {x <- {y <- 1}}" ],
        [
            "set(3,5--3,16)"
            "set(3,10--3,16)"
        ]
        "AssignIf.fs",
        "module A\nlet f () =\n    if c then x <- 1 else x <- 2\n",
        [ "let f () = if c then {x <- 1} else {x <- 2}" ],
        [
            "set(3,15--3,21)"
            "set(3,27--3,33)"
        ]
        "AssignLambda.fs",
        "module A\nlet f () =\n    ignore (fun () -> x <- 1)\n",
        [ "let f () = [ignore (fun () -> {x <- 1})]" ],
        [ "set(3,23--3,29)" ]
        "AssignParenthesized.fs",
        "module A\nlet f () =\n    ignore (x <- 1)\n",
        [ "let f () = [ignore ({x <- 1})]" ],
        [ "set(3,13--3,19)" ]
        "AssignBlock.fs",
        "module A\nlet f () =\n    x <-\n        1\n",
        [ "let f () = {x <- 1}" ],
        [ "set(3,5--4,10)" ]
        "AssignBlockSequential.fs",
        "module A\nlet f () =\n    x <-\n        ignore 1\n        2\n",
        [ "let f () = {x <- seq[[ignore 1]; 2]}" ],
        [
            "set(3,5--5,10)"
            "seq(4,9--5,10)"
        ]
        "AssignClause.fs",
        "module A\nlet f () =\n    match c with\n    | _ -> x <- 1\n",
        [ "let f () = match c with | _ -> {x <- 1}" ],
        [ "set(4,12--4,18)" ]
        "AssignInfix.fs",
        "module A\nlet f () =\n    x <- 1 + 2\n",
        [ "let f () = {x <- {1 + 2}}" ],
        [ "set(3,5--3,15)" ]
        "AssignOr.fs",
        "module A\nlet f () =\n    x <- a || b\n",
        [ "let f () = {x <- {a || b}}" ],
        [ "set(3,5--3,16)" ]
        "AssignEquals.fs",
        "module A\nlet f () =\n    x <- a = b\n",
        [ "let f () = {x <- {a = b}}" ],
        [ "set(3,5--3,15)" ]
        "AssignPipe.fs",
        "module A\nlet f () =\n    x <- 1 |> id\n",
        [ "let f () = {x <- {1 |> id}}" ],
        [ "set(3,5--3,17)" ]
        "AssignAfterInfix.fs",
        "module A\nlet f () =\n    a + b <- 1\n",
        [ "let f () = {a + {b <- 1}}" ],
        [ "set(3,9--3,15)" ]
        "AssignAfterComma.fs",
        "module A\nlet f () =\n    a, b <- 1\n",
        [ "let f () = a, {b <- 1}" ],
        [ "set(3,8--3,14)" ]
        "AssignIfValue.fs",
        "module A\nlet f () =\n    x <- if c then 1 else 2\n",
        [ "let f () = {x <- if c then 1 else 2}" ],
        [ "set(3,5--3,28)" ]
        "AssignMatchValue.fs",
        "module A\nlet f () =\n    x <- match c with _ -> 1\n",
        [ "let f () = {x <- match c with | _ -> 1}" ],
        [ "set(3,5--3,29)" ]
        "AssignLambdaValue.fs",
        "module A\nlet f () =\n    x <- fun y -> y\n",
        [ "let f () = {x <- fun y -> y}" ],
        [ "set(3,5--3,20)" ]
        "AssignSequential.fs",
        "module A\nlet f () =\n    x <- 1\n    x\n",
        [ "let f () = seq[{x <- 1}; x]" ],
        [
            "seq(3,5--4,6)"
            "set(3,5--3,11)"
        ]
        "AssignThenBlock.fs",
        "module A\nlet f () =\n    if c then\n        x <- 1\n    x\n",
        [ "let f () = seq[if c then {x <- 1}; x]" ],
        [
            "seq(3,5--5,6)"
            "set(4,9--4,15)"
        ]
        "AssignBindingLine.fs",
        "module A\nlet f () = x <- 1\n",
        [ "let f () = {x <- 1}" ],
        [ "set(2,12--2,18)" ]
        "AssignDo.fs", "module A\ndo x <- 1\n", [ "do {x <- 1}" ], [ "set(2,4--2,10)" ]
        "AssignModuleExpression.fs", "module A\nx <- 1\n", [ "expr {x <- 1}" ], [ "set(2,1--2,7)" ]
    ]

    let private assignmentExplicitCases = [
        "AssignToApplication.fs", "module A\nlet f () =\n    g x <- 1\n", []
        "AssignToParenthesized.fs", "module A\nlet f () =\n    (x) <- 1\n", []
        "AssignToIndex.fs", "module A\nlet f () =\n    a[0] <- 1\n", []
        "AssignToConstant.fs", "module A\nlet f () =\n    1 <- 2\n", []
        "AssignToDotLambda.fs", "module A\nlet f () =\n    _.x <- 1\n", []
        "AssignClose.fs",
        "module A\nlet f () =\n    x <- )\n",
        [ "AssignClose.fs(3,10): error FS0010: Unexpected symbol ')' in expression" ]
        "AssignMissing.fs",
        "module A\nlet f () =\n    x <-\n",
        [
            "AssignMissing.fs(4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "AssignMissing.fs(4,1): error FS3524: Expecting expression"
        ]
        "AssignMissingBeforeRoot.fs",
        "module A\nlet f () =\n    x <-\nlet g = 1\n",
        [
            "AssignMissingBeforeRoot.fs(4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "AssignMissingBeforeRoot.fs(4,1): error FS3524: Expecting expression"
        ]
        "AssignValueNextLine.fs", "module A\nlet f () =\n    x <- \n    1\n", []
        "AssignOperatorNextLine.fs", "module A\nlet f () =\n    x\n        <- 1\n", []
        "AssignOperatorSameColumn.fs",
        "module A\nlet f () =\n    x\n    <- 1\n",
        [
            "AssignOperatorSameColumn.fs(4,5): error FS0010: Unexpected symbol '<-' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private mutableBindingExplicitCases = [
        "MutableTyped.fs", "module A\nlet mutable x: int = 1\n", []
        "MutableNextLine.fs", "module A\nlet mutable\n    x = 1\n", []
        "LocalMutableTwice.fs",
        "module A\nlet f () =\n    let mutable mutable x = 1\n    x\n",
        [ "LocalMutableTwice.fs(3,17): error FS0010: Unexpected keyword 'mutable' in binding" ]
        "LocalMutableAccessibility.fs",
        "module A\nlet f () =\n    let mutable private x = 1\n    x\n",
        []
        "LocalMutableNextLine.fs",
        "module A\nlet f () =\n    let mutable\n        x = 1\n    x\n",
        []
        "MutableInPattern.fs",
        "module A\nlet (mutable x) = 1\n",
        [
            "MutableInPattern.fs(2,6): error FS0010: Unexpected keyword 'mutable' in pattern. Expected ')' or other token."
            "MutableInPattern.fs(2,5): error FS0583: Unmatched '('"
        ]
    ]

    let private delimitedLineCases = [
        "ParenLines.fs",
        "module A\nlet f () =\n    (\n        ignore 1\n        2\n    )\n",
        [ "let f () = (seq[[ignore 1]; 2])" ],
        [ "seq(4,9--5,10)" ]
        "ListLines.fs",
        "module A\nlet xs =\n    [\n        1\n        2\n    ]\n",
        [ "let xs = [1; 2]" ],
        []
        "RecordLines.fs",
        "module A\nlet r =\n    { A = 1\n      B = 2 }\n",
        [ "let r = {A = 1; B = 2}" ],
        []
        "ParenInfixLines.fs",
        "module A\nlet f a =\n    (a\n     |> id\n     |> id)\n",
        [ "let f a = ({{a |> id} |> id})" ],
        []
        "ParenCloseLine.fs",
        "module A\nlet f () =\n    g (\n        1\n    )\n",
        [ "let f () = [g (1)]" ],
        []
        "ListArgumentCloseLine.fs",
        "module A\nlet f () =\n    g [\n        1\n    ]\n",
        [ "let f () = [g [1]]" ],
        []
        "ParenTupleUndent.fs",
        "module A\nlet f () =\n    g (1,\n  2)\n",
        [ "let f () = [g (1, 2)]" ],
        []
        "LambdaBodyLines.fs",
        "module A\nlet f xs =\n    xs |> List.iter (fun x ->\n        ignore x\n        ignore 2)\n",
        [ "let f xs = {xs |> [List.iter (fun x -> seq[[ignore x]; [ignore 2]])]}" ],
        [ "seq(4,9--5,17)" ]
        "LambdaBodySameLineAligned.fs",
        "module A\nlet f xs =\n    xs |> List.iter (fun x -> ignore x\n                              ignore 2)\n",
        [ "let f xs = {xs |> [List.iter (fun x -> seq[[ignore x]; [ignore 2]])]}" ],
        [ "seq(3,31--4,39)" ]
        "ConditionalThenInfix.fs",
        "module A\nlet f c =\n    (if c then 1 else 2\n     |> id)\n",
        [ "let f c = ({if c then 1 else 2 |> id})" ],
        []
        "ParenLocalBinding.fs",
        "module A\nlet f () =\n    (let x = 1\n     x)\n",
        [ "let f () = (let x = 1 in x)" ],
        [ "let(3,6--4,7)" ]
        "ListApplicationLines.fs",
        "module A\nlet xs =\n    [ ignore 1\n      2 ]\n",
        [ "let xs = [[ignore 1]; 2]" ],
        []
        "ClauseResultLines.fs",
        "module A\nlet f x =\n    (match x with\n     | 1 ->\n         ignore 1\n         2\n     | _ -> 3)\n",
        [ "let f x = (match x with | 1 -> seq[[ignore 1]; 2] | _ -> 3)" ],
        [ "seq(5,10--6,11)" ]
        "ParenArgumentIndented.fs",
        "module A\nlet f () =\n    (g 1\n        2)\n",
        [ "let f () = ([[g 1] 2])" ],
        []
        "RecordValueLines.fs",
        "module A\nlet r =\n    { A =\n        ignore 1\n        2 }\n",
        [ "let r = {A = seq[[ignore 1]; 2]}" ],
        []
        "ParenTupleInfix.fs",
        "module A\nlet f a b =\n    (a, b\n     |> id)\n",
        [ "let f a b = (a, {b |> id})" ],
        []
        "ThenBranchLines.fs",
        "module A\nlet f c =\n    (if c then\n        ignore 1\n        2\n     else 3)\n",
        [ "let f c = (if c then seq[[ignore 1]; 2] else 3)" ],
        [ "seq(4,9--5,10)" ]
        "ParenInfixUndent.fs",
        "module A\nlet f a =\n    (a\n   |> id)\n",
        [ "let f a = ({a |> id})" ],
        []
        "LambdaBodyArgument.fs",
        "module A\nlet f () =\n    (fun x -> g x\n                1)\n",
        [ "let f () = (fun x -> [[g x] 1])" ],
        []
        "ParenFirstLineItems.fs",
        "module A\nlet f () =\n    (ignore 1\n     2)\n",
        [ "let f () = (seq[[ignore 1]; 2])" ],
        [ "seq(3,6--4,7)" ]
        "ListSeparatorAndLine.fs",
        "module A\nlet xs =\n    [ 1; 2\n      3 ]\n",
        [ "let xs = [1; 2; 3]" ],
        []
        "ParenInfixPrecedence.fs",
        "module A\nlet f a b c =\n    (a + b\n     * c)\n",
        [ "let f a b c = ({a + {b * c}})" ],
        []
        "ParenAssignmentInfix.fs",
        "module A\nlet f () =\n    (x <- 1\n     |> id)\n",
        [ "let f () = ({x <- {1 |> id}})" ],
        [ "set(3,6--4,11)" ]
        "LambdaThenInfix.fs",
        "module A\nlet f () =\n    (fun x -> x\n     |> id)\n",
        [ "let f () = ({fun x -> x |> id})" ],
        []
        "ListArgumentIndented.fs",
        "module A\nlet xs =\n    [ g 1\n        2 ]\n",
        [ "let xs = [[[g 1] 2]]" ],
        []
        "LambdaBodyArgumentLine.fs",
        "module A\nlet f xs =\n    xs |> List.iter (fun x ->\n        g x\n          1)\n",
        [ "let f xs = {xs |> [List.iter (fun x -> [[g x] 1])]}" ],
        []
        "LambdaBodyThreeLines.fs",
        "module A\nlet f xs =\n    List.iter (fun x ->\n        ignore x\n        ignore 2\n        ignore 3) xs\n",
        [ "let f xs = [[List.iter (fun x -> seq[[ignore x]; seq[[ignore 2]; [ignore 3]]])] xs]" ],
        [
            "seq(4,9--6,17)"
            "seq(5,9--6,17)"
        ]
        "ParenLocalBindingValueLine.fs",
        "module A\nlet f () =\n    (let x =\n        1\n     x)\n",
        [ "let f () = (let x = 1 in x)" ],
        [ "let(3,6--5,7)" ]
        "ParenAssignmentValueLine.fs",
        "module A\nlet f () =\n    (x <-\n        1)\n",
        [ "let f () = ({x <- 1})" ],
        [ "set(3,6--4,10)" ]
        "ParenThenElseAligned.fs",
        "module A\nlet f c =\n    (if c\n     then 1\n     else 2)\n",
        [ "let f c = (if c then 1 else 2)" ],
        []
        "LambdaArgumentThenItem.fs",
        "module A\nlet f xs =\n    (List.iter (fun x ->\n        ignore x) xs\n     ignore 2)\n",
        [ "let f xs = (seq[[[List.iter (fun x -> [ignore x])] xs]; [ignore 2]])" ],
        [ "seq(3,6--5,14)" ]
        "RecordValueLineThenField.fs",
        "module A\nlet r =\n    { A =\n        1\n      B = 2 }\n",
        [ "let r = {A = 1; B = 2}" ],
        []
        "ClauseResultSameLineAligned.fs",
        "module A\nlet f x =\n    (match x with\n     | 1 -> ignore 1\n            2\n     | _ -> 3)\n",
        [ "let f x = (match x with | 1 -> seq[[ignore 1]; 2] | _ -> 3)" ],
        [ "seq(4,13--5,14)" ]
        "ThenBranchSameLineAligned.fs",
        "module A\nlet f c =\n    (if c then ignore 1\n               2\n     else 3)\n",
        [ "let f c = (if c then seq[[ignore 1]; 2] else 3)" ],
        [ "seq(3,16--4,17)" ]
        "ParenLocalBindingValueAligned.fs",
        "module A\nlet f () =\n    (let x = ignore 1\n             2\n     x)\n",
        [ "let f () = (let x = seq[[ignore 1]; 2] in x)" ],
        [
            "let(3,6--5,7)"
            "seq(3,14--4,15)"
        ]
        "ParenInfixUndentTwo.fs",
        "module A\nlet f a =\n    (a\n   |> id)\n",
        [ "let f a = ({a |> id})" ],
        []
        "ParenInfixUndentThree.fs",
        "module A\nlet f a =\n    (a\n  |> id)\n",
        [ "let f a = ({a |> id})" ],
        []
        "ParenPlusUndentTwo.fs",
        "module A\nlet f a b =\n    (a\n   + b)\n",
        [ "let f a b = ({a + b})" ],
        []
        "LambdaBodyInfixAligned.fs",
        "module A\nlet f () =\n    (fun x -> x\n              |> id)\n",
        [ "let f () = (fun x -> {x |> id})" ],
        []
        "IndexLines.fs",
        "module A\nlet f (a: int list) =\n    a[\n        0\n    ]\n",
        [ "let f (a: int list) = a[0]" ],
        []
        "ParenLocalBindingBodyLines.fs",
        "module A\nlet f () =\n    (let x = 1\n     x\n     x)\n",
        [ "let f () = (let x = 1 in seq[x; x])" ],
        [
            "let(3,6--5,7)"
            "seq(4,6--5,7)"
        ]
        "RecordValueInfix.fs",
        "module A\nlet r =\n    { A = 1\n          + 2 }\n",
        [ "let r = {A = {1 + 2}}" ],
        []
        "ParenCommaLine.fs",
        "module A\nlet f a b =\n    (a\n     , b)\n",
        [ "let f a b = (a, b)" ],
        []
        "ParenMatchThenItem.fs",
        "module A\nlet f x =\n    (match x with\n     | 1 ->\n         ignore 1\n     2)\n",
        [ "let f x = (seq[match x with | 1 -> [ignore 1]; 2])" ],
        [ "seq(3,6--6,7)" ]
        "NestedIfOuterElse.fs",
        "module A\nlet f a b =\n    (if a then\n        if b then 1\n     else 2)\n",
        [ "let f a b = (if a then if b then 1 else 2)" ],
        []
        "NestedMatchOuterClause.fs",
        "module A\nlet f a b =\n    (match a with\n     | 1 ->\n         match b with\n         | 2 -> 3\n         | _ -> 4\n     | _ -> 5)\n",
        [ "let f a b = (match a with | 1 -> match b with | 2 -> 3 | _ -> 4 | _ -> 5)" ],
        []
        "ClauseLambdaThenClause.fs",
        "module A\nlet f x =\n    (match x with\n     | 1 -> fun y -> y\n     | _ -> id)\n",
        [ "let f x = (match x with | 1 -> fun y -> y | _ -> id)" ],
        []
        "ParenElifAligned.fs",
        "module A\nlet f a b =\n    (if a then 1\n     elif b then 2\n     else 3)\n",
        [ "let f a b = (if a then 1 else if b then 2 else 3)" ],
        []
        "ParenItemThenInfix.fs",
        "module A\nlet f () =\n    (ignore 1\n     2\n     |> id)\n",
        [ "let f () = (seq[[ignore 1]; {2 |> id}])" ],
        [ "seq(3,6--5,11)" ]
        "ListLambdaThenItem.fs",
        "module A\nlet xs =\n    [ fun x ->\n        x\n      id ]\n",
        [ "let xs = [fun x -> x; id]" ],
        []
        "NestedIfInnerElse.fs",
        "module A\nlet f a b =\n    (if a then\n        if b then 1\n        else 3\n     else 2)\n",
        [ "let f a b = (if a then if b then 1 else 3 else 2)" ],
        []
        "LambdaBodyLocalBinding.fs",
        "module A\nlet f xs =\n    List.iter (fun x ->\n        let y = x\n        ignore y) xs\n",
        [ "let f xs = [[List.iter (fun x -> let y = x in [ignore y])] xs]" ],
        [ "let(4,9--5,17)" ]
        "LambdaBodyLeftOfBlock.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)\n",
        [ "let f () = [g (fun x -> x)]" ],
        []
        "LambdaBodyAtBlockColumn.fs",
        "module A\nlet f () =\n    g (fun x ->\n    x)\n",
        [ "let f () = [g (fun x -> x)]" ],
        []
        "NestedLambdaBodies.fs",
        "module A\nlet f () =\n    g (fun x ->\n        h (fun y ->\n        y))\n",
        [ "let f () = [g (fun x -> [h (fun y -> y)])]" ],
        []
        "BlockLineOpensDelimiter.fs",
        "module A\nlet f xs =\n    ignore 1\n    List.iter (fun x ->\n        ignore x) xs\n",
        [ "let f xs = seq[[ignore 1]; [[List.iter (fun x -> [ignore x])] xs]]" ],
        [ "seq(3,5--5,21)" ]
        "BlockLineOpensDelimiterThenItem.fs",
        "module A\nlet f xs =\n    ignore 1\n    List.iter (fun x ->\n        ignore x) xs\n    ignore 2\n",
        [ "let f xs = seq[[ignore 1]; seq[[[List.iter (fun x -> [ignore x])] xs]; [ignore 2]]]" ],
        [
            "seq(3,5--6,13)"
            "seq(4,5--6,13)"
        ]
        "RootLineOpensDelimiter.fs",
        "module A\nlet xs = [\n    1\n    2\n]\nlet y = 1\n",
        [
            "let xs = [1; 2]"
            "let y = 1"
        ],
        []
        "NestedBlockLineOpensDelimiter.fs",
        "module A\nlet f c =\n    if c then\n        ignore (\n            1)\n    ignore 2\n",
        [ "let f c = seq[if c then [ignore (1)]; [ignore 2]]" ],
        [ "seq(3,5--6,13)" ]
    ]

    let private delimitedLineExplicitCases = [
        "ParenItemAtParen.fs", "module A\nlet f () =\n    (ignore 1\n    2)\n", []
        "ParenItemLeftOfContent.fs", "module A\nlet f () =\n    (   ignore 1\n      2)\n", []
        "ListItemLeftOfContent.fs", "module A\nlet xs =\n    [ 1\n     2 ]\n", []
        "RecordValueAligned.fs",
        "module A\nlet r =\n    { A = ignore 1\n          2 }\n",
        [
            "RecordValueAligned.fs(4,11): error FS0010: Unexpected integer literal in expression. Expected '}' or other token."
            "RecordValueAligned.fs(3,5): error FS0604: Unmatched '{'"
        ]
        "ParenLinesThenItem.fs", "module A\nlet f () =\n    g (\n        1\n    )\n    h ()\n", []
        "LambdaLinesThenItem.fs",
        "module A\nlet f xs =\n    xs |> List.iter (fun x ->\n        ignore x\n        ignore 2)\n    ignore 3\n",
        []
        "ListItemAtBracket.fs", "module A\nlet xs =\n    [ 1\n    2 ]\n", []
        "ParenItemsLeftOfContent.fs", "module A\nlet f () =\n    (ignore 1\n    2\n    3)\n", []
        "LambdaBodyOffsideArgument.fs",
        "module A\nlet f xs =\n    List.iter (fun x ->\n        ignore x\n      2) xs\n",
        [
            "LambdaBodyOffsideArgument.fs(5,7): error FS0010: Unexpected integer literal in expression"
        ]
        "NestedMatchAtClauseColumn.fs",
        "module A\nlet f a b =\n    (match a with\n     | 1 ->\n     match b with\n     | 2 -> 3\n     | _ -> 4)\n",
        []
        "ElseLeftOfParenContent.fs", "module A\nlet f a =\n    g (if a then 1\n  else 2)\n", []
        "ThenBranchLeftOfIf.fs",
        "module A\nlet f c =\n    (if c then\n  1\n     else 2)\n",
        [
            "ThenBranchLeftOfIf.fs(4,3): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:6). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "ThenBranchLeftOfIf.fs(4,3): error FS3524: Expecting expression"
            "ThenBranchLeftOfIf.fs(4,3): error FS0010: Unexpected integer literal in expression"
            "ThenBranchLeftOfIf.fs(5,12): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
            "ThenBranchLeftOfIf.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "ThenBranchLeftOfIf.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "ClauseResultLeftOfBar.fs",
        "module A\nlet f x =\n    (match x with\n     | _ ->\n   1)\n",
        [
            "ClauseResultLeftOfBar.fs(5,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "ClauseResultLeftOfBar.fs(5,4): error FS0010: Incomplete structured construct at or before this point in pattern matching"
        ]
        "LocalValueLeftOfLet.fs",
        "module A\nlet f () =\n    (let x =\n   1\n     x)\n",
        [
            "LocalValueLeftOfLet.fs(4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:6). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LocalValueLeftOfLet.fs(4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]
        "LocalLetLineOpensDelimiter.fs",
        "module A\nlet f () =\n    let a = g (fun x ->\n      x)\n    a\n",
        []
        "LambdaBodyAtLocalLetColumn.fs",
        "module A\nlet f () =\n    let a = g (fun x ->\n    x)\n    a\n",
        [
            "LambdaBodyAtLocalLetColumn.fs(4,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyAtLocalLetColumn.fs(3,16): error FS0611: Missing function body"
            "LambdaBodyAtLocalLetColumn.fs(4,5): error FS0010: Unexpected identifier in expression"
        ]
        "LocalLetLineOpensLambda.fs",
        "module A\nlet f xs =\n    let g = List.map (fun x ->\n        x)\n    g xs\n",
        []
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

            testList "a new line at the same column in a block is a sequential expression" [
                for logicalPath, text, expectedDeclarations, expectedRanges in sequentialCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with sequential expressions"

                        let root = Seq.exactlyOne (parse logicalPath text).File.Contents

                        let ranges =
                            declarationBodies root.Declarations
                            |> List.collect sequentialRanges

                        Expect.sequenceEqual
                            ranges
                            expectedRanges
                            "The sequential expression ranges"
            ]

            testList "a sequential expression that the parser does not model stays explicit" [
                for logicalPath, text, oracle in sequentialExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList
                "a line between the binding column and the block column stays explicit at the Oracle position"
                [
                    for logicalPath, text, oracle, position in blockOffsideExplicitCases ->
                        testCase logicalPath
                        <| fun _ ->
                            let result = parse logicalPath text
                            let lines = oracleLines logicalPath result

                            SyntaxDiagnosticText.expectExplicitlyUnsupported
                                oracle
                                result.Diagnostics
                                lines

                            Expect.sequenceEqual
                                (lines
                                 |> List.filter (fun line -> line.Contains ": error FSC2P1001: ")
                                 |> List.map (fun line ->
                                     line.Substring(0, line.IndexOf ": error ")
                                 ))
                                [ position ]
                                "The explicit diagnostic is at the first Compatibility Oracle diagnostic"
                ]

            testList "a let or use at the start of a block line binds the rest of the block" [
                for logicalPath, text, expectedDeclarations, expectedRanges in localLetCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with local bindings"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect blockRanges)
                            expectedRanges
                            "The local binding and sequential expression ranges"
            ]

            testList "a let or use at the end of a block reports FS0588 and parsing continues" [
                for logicalPath, text, oracle, expectedDeclarations, expectedRanges in
                    unfinishedLocalLetCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        let declarations =
                            (Seq.exactlyOne result.File.Contents).Declarations
                            |> Seq.map declarationShape
                            |> Seq.toList

                        Expect.sequenceEqual
                            (oracleLines logicalPath result)
                            oracle
                            "The Compatibility Oracle diagnostics"

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with an unfinished local binding"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect blockRanges)
                            expectedRanges
                            "The local binding and sequential expression ranges"
            ]

            testList "a local binding that the parser does not model stays explicit" [
                for logicalPath, text, oracle in localLetExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "a mutable keyword before the head pattern marks the binding mutable" [
                for logicalPath, text, expected in mutableBindingCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.isEmpty
                            (oracleLines logicalPath result)
                            "The Compatibility Oracle reports no diagnostic"

                        Expect.sequenceEqual
                            (bindings (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.map bindingText)
                            expected
                            "The bindings with their head pattern ranges"
            ]

            testList "a mutable keyword that a binding cannot take reports the Oracle diagnostic" [
                for logicalPath, text, oracle in mutableBindingErrorCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.sequenceEqual
                            (oracleLines logicalPath result)
                            oracle
                            "The Compatibility Oracle diagnostics"
            ]

            testList "an assignment sets the long identifier before it to the expression after it" [
                for logicalPath, text, expectedDeclarations, expectedRanges in assignmentCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with assignments"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect blockRanges)
                            expectedRanges
                            "The assignment, local binding, and sequential expression ranges"
            ]

            testList "an assignment that the parser does not model stays explicit" [
                for logicalPath, text, oracle in assignmentExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "a mutable binding that the parser does not model stays explicit" [
                for logicalPath, text, oracle in mutableBindingExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "lines inside delimiters build the Oracle tree" [
                for logicalPath, text, expectedDeclarations, expectedRanges in delimitedLineCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with lines inside delimiters"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect blockRanges)
                            expectedRanges
                            "The sequential expression ranges"
            ]

            testList "lines inside delimiters that the parser does not model stay explicit" [
                for logicalPath, text, oracle in delimitedLineExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
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
