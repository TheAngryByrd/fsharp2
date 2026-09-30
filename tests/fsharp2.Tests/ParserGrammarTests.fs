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

    let private lexicalAndParserLines logicalPath text =
        let language =
            LanguageVersion.normalize (Some "10.0")
            |> Result.defaultWith failtest

        let document =
            SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
            |> LexicalPipeline.prepare language Array.empty

        let line (start: SourcePosition) severity code message =
            start, $"{logicalPath}({start.Line},{start.Column}): {severity} {code}: {message}"

        let lexical =
            document.Diagnostics
            |> Seq.map (fun diagnostic ->
                let severity =
                    match diagnostic.Severity with
                    | DiagnosticSeverity.Warning -> "warning"
                    | _ -> "error"

                line diagnostic.Range.Start severity diagnostic.Code diagnostic.Message
            )

        let syntax =
            (Parser.parseImplementationFile document).Diagnostics
            |> Seq.map (fun diagnostic ->
                line
                    diagnostic.Range.Start
                    (SyntaxDiagnosticText.severity diagnostic)
                    diagnostic.Code
                    diagnostic.Message
            )

        Seq.append lexical syntax
        |> Seq.sortBy (fun (start, _) -> start.Line, start.Column)
        |> Seq.map snd
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

    let private prefixText (operator: SyntaxPrefixOperator) =
        match operator with
        | SyntaxPrefixOperator.Negate _ -> "~-"
        | SyntaxPrefixOperator.Plus _ -> "~+"
        | SyntaxPrefixOperator.Dereference _ -> "~!"

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
        | SyntaxExpression.Prefix(operator, operand, _) ->
            $"{prefixText operator}{expressionShape operand}"
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
        | SyntaxExpression.Upcast(target, targetType, _) ->
            $"{{{expressionShape target} :> {typeShape targetType}}}"
        | SyntaxExpression.Downcast(target, targetType, _) ->
            $"{{{expressionShape target} :?> {typeShape targetType}}}"
        | SyntaxExpression.TypeTest(target, targetType, _) ->
            $"{{{expressionShape target} :? {typeShape targetType}}}"
        | SyntaxExpression.DotGet(target, members, _) -> $"{expressionShape target}.{members.Text}"
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

    let rec private infixRanges expression =
        let text name range =
            let startLine, startColumn, endLine, endColumn = position range
            $"{name}({startLine},{startColumn}--{endLine},{endColumn})"

        let all expressions =
            expressions
            |> Seq.collect infixRanges
            |> Seq.toList

        match expression with
        | SyntaxExpression.Infix(operator, left, right, range) ->
            text operator.Text range
            :: all [
                left
                right
            ]
        | SyntaxExpression.Prefix(operator, operand, range) ->
            text (prefixText operator) range
            :: infixRanges operand
        | SyntaxExpression.Tuple(items, range) ->
            text "tuple" range
            :: all items
        | SyntaxExpression.LongIdentifierSet(_, value, range) ->
            text "set" range
            :: infixRanges value
        | SyntaxExpression.Sequential(first, second, range) ->
            text "seq" range
            :: all [
                first
                second
            ]
        | SyntaxExpression.LetOrUse(_, _, binding, body, range) ->
            text "let" range
            :: all [
                binding.Body
                body
            ]
        | SyntaxExpression.Application(func, argument, _) ->
            all [
                func
                argument
            ]
        | SyntaxExpression.Parenthesized(inner, _) -> infixRanges inner
        | SyntaxExpression.If(condition, thenBranch, elseBranch, _) ->
            all (
                [
                    condition
                    thenBranch
                ]
                @ Option.toList elseBranch
            )
        | SyntaxExpression.Match(input, clauses, _) ->
            infixRanges input
            @ all (
                clauses
                |> Seq.map _.Result
            )
        | SyntaxExpression.Lambda(_, body, _) -> infixRanges body
        | SyntaxExpression.List(items, _) -> all items
        | SyntaxExpression.Upcast(target, _, range) ->
            text ":>" range
            :: infixRanges target
        | SyntaxExpression.Downcast(target, _, range) ->
            text ":?>" range
            :: infixRanges target
        | SyntaxExpression.TypeTest(target, _, range) ->
            text ":?" range
            :: infixRanges target
        | SyntaxExpression.DotGet(target, _, range) ->
            text "dot" range
            :: infixRanges target
        | _ -> []

    let rec private applicationRanges expression =
        let all expressions =
            expressions
            |> Seq.collect applicationRanges
            |> Seq.toList

        match expression with
        | SyntaxExpression.Application(func, argument, range) ->
            let startLine, startColumn, endLine, endColumn = position range

            $"app({startLine},{startColumn}--{endLine},{endColumn})"
            :: all [
                func
                argument
            ]
        | SyntaxExpression.Infix(_, left, right, _) ->
            all [
                left
                right
            ]
        | SyntaxExpression.Prefix(_, operand, _) -> applicationRanges operand
        | SyntaxExpression.Tuple(items, _) -> all items
        | SyntaxExpression.LongIdentifierSet(_, value, _) -> applicationRanges value
        | SyntaxExpression.Sequential(first, second, _) ->
            all [
                first
                second
            ]
        | SyntaxExpression.LetOrUse(_, _, binding, body, _) ->
            all [
                binding.Body
                body
            ]
        | SyntaxExpression.Parenthesized(inner, _) -> applicationRanges inner
        | SyntaxExpression.If(condition, thenBranch, elseBranch, _) ->
            all (
                [
                    condition
                    thenBranch
                ]
                @ Option.toList elseBranch
            )
        | SyntaxExpression.Match(input, clauses, _) ->
            applicationRanges input
            @ all (
                clauses
                |> Seq.map _.Result
            )
        | SyntaxExpression.Lambda(_, body, _) -> applicationRanges body
        | SyntaxExpression.List(items, _) -> all items
        | SyntaxExpression.Upcast(target, _, _)
        | SyntaxExpression.Downcast(target, _, _)
        | SyntaxExpression.TypeTest(target, _, _)
        | SyntaxExpression.DotGet(target, _, _) -> applicationRanges target
        | _ -> []

    let rec private conditionalRanges expression =
        let all expressions =
            expressions
            |> Seq.collect conditionalRanges
            |> Seq.toList

        match expression with
        | SyntaxExpression.If(condition, thenBranch, elseBranch, range) ->
            let startLine, startColumn, endLine, endColumn = position range

            let elseMark = if elseBranch.IsSome then "E" else "-"

            $"if({startLine},{startColumn}--{endLine},{endColumn}){elseMark}"
            :: all (
                [
                    condition
                    thenBranch
                ]
                @ Option.toList elseBranch
            )
        | SyntaxExpression.Infix(_, left, right, _) ->
            all [
                left
                right
            ]
        | SyntaxExpression.Application(func, argument, _) ->
            all [
                func
                argument
            ]
        | SyntaxExpression.Sequential(first, second, _) ->
            all [
                first
                second
            ]
        | SyntaxExpression.LetOrUse(_, _, binding, body, _) ->
            all [
                binding.Body
                body
            ]
        | SyntaxExpression.Parenthesized(inner, _) -> conditionalRanges inner
        | SyntaxExpression.Match(input, clauses, _) ->
            conditionalRanges input
            @ all (
                clauses
                |> Seq.map _.Result
            )
        | SyntaxExpression.List(items, _) -> all items
        | _ -> []

    let rec private matchRanges expression =
        let all expressions =
            expressions
            |> Seq.collect matchRanges
            |> Seq.toList

        match expression with
        | SyntaxExpression.Match(input, clauses, range) ->
            let startLine, startColumn, endLine, endColumn = position range

            $"match({startLine},{startColumn}--{endLine},{endColumn}){clauses.Length}"
            :: matchRanges input
            @ all (
                clauses
                |> Seq.map _.Result
            )
        | SyntaxExpression.If(condition, thenBranch, elseBranch, _) ->
            all (
                [
                    condition
                    thenBranch
                ]
                @ Option.toList elseBranch
            )
        | SyntaxExpression.Infix(_, left, right, _) ->
            all [
                left
                right
            ]
        | SyntaxExpression.Application(func, argument, _) ->
            all [
                func
                argument
            ]
        | SyntaxExpression.Sequential(first, second, _) ->
            all [
                first
                second
            ]
        | SyntaxExpression.LetOrUse(_, _, binding, body, _) ->
            all [
                binding.Body
                body
            ]
        | SyntaxExpression.Parenthesized(inner, _) -> matchRanges inner
        | SyntaxExpression.Lambda(_, body, _) -> matchRanges body
        | SyntaxExpression.List(items, _) -> all items
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
    ]

    let private blockOffsideExplicitCases = [
        "BlockOffsideBeforeRoot.fs",
        "module A\nlet f =\n    g\n  1\nlet h = 2\n",
        [
            "BlockOffsideBeforeRoot.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BlockOffsideBeforeRoot.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BlockOffsideBeforeRoot.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ],
        "BlockOffsideBeforeRoot.fs(4,3)"
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
        "LambdaBodyAtLaterBlockLine.fs",
        "module A\nlet f () =\n    ignore 0\n    List.iter (fun x ->\n    ignore x) [ 1 ]\n",
        [ "let f () = seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] [1]]]" ],
        [ "seq(3,5--5,20)" ]
        "LambdaBodyAtModuleExpressionColumn.fs",
        "List.iter (fun x ->\nignore x) [ 1 ]\n",
        [ "expr [[List.iter (fun x -> [ignore x])] [1]]" ],
        []
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
        "AssignValueLines.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <-\n        1\n        2)\n",
        [
            "let mutable x = 0"
            "let f () = ({x <- seq[1; 2]})"
        ],
        [
            "set(4,6--6,10)"
            "seq(5,9--6,10)"
        ]
        "AssignValueLinesInList.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    [ x <-\n        1\n        2 ]\n",
        [
            "let mutable x = 0"
            "let f () = [{x <- seq[1; 2]}]"
        ],
        []
        "AssignValueLinesThenItem.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <-\n        1\n        2\n     ignore 3)\n",
        [
            "let mutable x = 0"
            "let f () = (seq[{x <- seq[1; 2]}; [ignore 3]])"
        ],
        [
            "seq(4,6--7,14)"
            "set(4,6--6,10)"
            "seq(5,9--6,10)"
        ]
        "AssignValueLocalLet.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <-\n        let y = 1\n        y)\n",
        [
            "let mutable x = 0"
            "let f () = ({x <- let y = 1 in y})"
        ],
        [
            "set(4,6--6,10)"
            "let(5,9--6,10)"
        ]
        "AssignValueLinePipe.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <-\n        1\n        |> id)\n",
        [
            "let mutable x = 0"
            "let f () = ({x <- {1 |> id}})"
        ],
        [ "set(4,6--6,14)" ]
        "AssignValueSameLineThenAligned.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <- 1\n          2)\n",
        [
            "let mutable x = 0"
            "let f () = ({x <- [1 2]})"
        ],
        [ "set(4,6--5,12)" ]
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
        "AssignValueAtTarget.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <-\n     1\n     2)\n",
        []
        "AssignValueLeftOfTarget.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <-\n    1)\n",
        []
    ]

    let private delimitedLineInventedCases = [
        "LambdaBodyNestedModuleLetAtLet.fs",
        "module A\nmodule M =\n    let f xs =\n        List.iter (fun x ->\n    ignore x) xs\n",
        [
            "LambdaBodyNestedModuleLetAtLet.fs(5,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyNestedModuleLetAtLet.fs(4,20): error FS0611: Missing function body"
            "LambdaBodyNestedModuleLetAtLet.fs(5,5): error FS0010: Unexpected identifier in expression"
        ],
        [
            "LambdaBodyNestedModuleLetAtLet.fs(5,13): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "LambdaBodyNestedModuleLetAtLet.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "LambdaBodyAtBindingColumn.fs",
        "module A\nlet f () =\n    g (fun x ->\nx)\n",
        [
            "LambdaBodyAtBindingColumn.fs(4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyAtBindingColumn.fs(3,8): error FS0611: Missing function body"
            "LambdaBodyAtBindingColumn.fs(4,1): error FS0010: Unexpected identifier in expression"
        ],
        [
            "LambdaBodyAtBindingColumn.fs(4,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private infixLineCases = [
        "InfixAtColumnPipe.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map id\n",
        [ "let f xs = {xs |> [List.map id]}" ],
        [ "|>(3,5--4,19)" ]
        "InfixAtColumnPlus.fs",
        "module A\nlet f () =\n    1\n    + 2\n",
        [ "let f () = {1 + 2}" ],
        [ "+(3,5--4,8)" ]
        "InfixAtColumnComma.fs",
        "module A\nlet f a b =\n    a\n    , b\n",
        [ "let f a b = a, b" ],
        [ "tuple(3,5--4,8)" ]
        "InfixAtColumnMinusSpaced.fs",
        "module A\nlet f a =\n    a\n    - 1\n",
        [ "let f a = {a - 1}" ],
        [ "-(3,5--4,8)" ]
        "InfixAtColumnAmpamp.fs",
        "module A\nlet f a b =\n    a\n    && b\n",
        [ "let f a b = {a && b}" ],
        [ "&&(3,5--4,9)" ]
        "InfixAtColumnCons.fs",
        "module A\nlet f a b =\n    a\n    :: b\n",
        [ "let f a b = {a :: b}" ],
        [ "::(3,5--4,9)" ]
        "InfixAtColumnAfterIf.fs",
        "module A\nlet f c =\n    if c then 1 else 2\n    + 3\n",
        [ "let f c = {if c then 1 else 2 + 3}" ],
        [ "+(3,5--4,8)" ]
        "InfixAtColumnTwoPipes.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map id\n    |> List.rev\n",
        [ "let f xs = {{xs |> [List.map id]} |> List.rev}" ],
        [
            "|>(3,5--5,16)"
            "|>(3,5--4,19)"
        ]
        "InfixAtColumnAfterLocalLet.fs",
        "module A\nlet f () =\n    let x = 1\n    x\n    + 1\n",
        [ "let f () = let x = 1 in {x + 1}" ],
        [
            "let(3,5--5,8)"
            "+(4,5--5,8)"
        ]
        "InfixAtColumnAfterItem.fs",
        "module A\nlet f a b =\n    ignore 1\n    a\n    + b\n",
        [ "let f a b = seq[[ignore 1]; {a + b}]" ],
        [
            "seq(3,5--5,8)"
            "+(4,5--5,8)"
        ]
        "InfixAtColumnThenItem.fs",
        "module A\nlet f a b =\n    a\n    + b\n    ignore 2\n",
        [ "let f a b = seq[{a + b}; [ignore 2]]" ],
        [
            "seq(3,5--5,13)"
            "+(3,5--4,8)"
        ]
        "InfixAtColumnStar.fs",
        "module A\nlet f a b =\n    a\n    * b\n",
        [ "let f a b = {a * b}" ],
        [ "*(3,5--4,8)" ]
        "InfixAtColumnGreaterEqual.fs",
        "module A\nlet f a b =\n    a\n    >= b\n",
        [ "let f a b = {a >= b}" ],
        [ ">=(3,5--4,9)" ]
        "InfixAtColumnBackpipe.fs",
        "module A\nlet f g b =\n    g\n    <| b\n",
        [ "let f g b = {g <| b}" ],
        [ "<|(3,5--4,9)" ]
        "InfixAtColumnAssignValue.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    x <- 1\n    + 2\n",
        [
            "let mutable x = 0"
            "let f () = {x <- {1 + 2}}"
        ],
        [
            "set(4,5--5,8)"
            "+(4,10--5,8)"
        ]
        "InfixAtColumnLambdaBody.fs",
        "module A\nlet f () =\n    List.map (fun x ->\n        x\n        + 1)\n",
        [ "let f () = [List.map (fun x -> {x + 1})]" ],
        [ "+(4,9--5,12)" ]
        "InfixAtColumnLocalValue.fs",
        "module A\nlet f a b =\n    let y =\n        a\n        + b\n    y\n",
        [ "let f a b = let y = {a + b} in y" ],
        [
            "let(3,5--6,6)"
            "+(4,9--5,12)"
        ]
        "InfixAtColumnBarbar.fs",
        "module A\nlet f a b =\n    a\n    || b\n",
        [ "let f a b = {a || b}" ],
        [ "||(3,5--4,9)" ]
        "InfixAtColumnAt.fs",
        "module A\nlet f a b =\n    a\n    @ b\n",
        [ "let f a b = {a @ b}" ],
        [ "@(3,5--4,8)" ]
        "InfixAtColumnCompose.fs",
        "module A\nlet f a b =\n    a\n    >> b\n",
        [ "let f a b = {a >> b}" ],
        [ ">>(3,5--4,9)" ]
        "InfixAtColumnAppThenPipe.fs",
        "module A\nlet f g x =\n    g x\n    |> ignore\n",
        [ "let f g x = {[g x] |> ignore}" ],
        [ "|>(3,5--4,14)" ]
        "InfixAtColumnAfterElseBlock.fs",
        "module A\nlet f c =\n    if c then\n        1\n    else\n        2\n    + 3\n",
        [ "let f c = {if c then 1 else 2 + 3}" ],
        [ "+(3,5--7,8)" ]
        "InfixAtColumnClauseResult.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> 1\n    + 2\n",
        [ "let f x = {match x with | _ -> 1 + 2}" ],
        [ "+(3,5--5,8)" ]
        "InfixAtColumnThenSameLine.fs",
        "module A\nlet f c =\n    if c then 1\n    |> ignore\n",
        [ "let f c = {if c then 1 |> ignore}" ],
        [ "|>(3,5--4,14)" ]
        "InfixAtColumnDiv.fs",
        "module A\nlet f a b =\n    a\n    / b\n",
        [ "let f a b = {a / b}" ],
        [ "/(3,5--4,8)" ]
        "InfixAtColumnPow.fs",
        "module A\nlet f a b =\n    a\n    ** b\n",
        [ "let f a b = {a ** b}" ],
        [ "**(3,5--4,9)" ]
        "InfixAtColumnNotequal.fs",
        "module A\nlet f a b =\n    a\n    <> b\n",
        [ "let f a b = {a <> b}" ],
        [ "<>(3,5--4,9)" ]
        "InfixUndentedPlusUndent2.fs",
        "module A\nlet f a b =\n      a\n    + b\n",
        [ "let f a b = {a + b}" ],
        [ "+(3,7--4,8)" ]
        "InfixUndentedPipeUndent3.fs",
        "module A\nlet f a c =\n      a\n   |> c\n",
        [ "let f a c = {a |> c}" ],
        [ "|>(3,7--4,8)" ]
        "InfixUndentedCommaUndent2.fs",
        "module A\nlet f a b =\n      a\n    , b\n",
        [ "let f a b = a, b" ],
        [ "tuple(3,7--4,8)" ]
        "InfixUndentedAmpampUndent3.fs",
        "module A\nlet f a b =\n      a\n   && b\n",
        [ "let f a b = {a && b}" ],
        [ "&&(3,7--4,8)" ]
        "InfixUndentedConsUndent3.fs",
        "module A\nlet f a b =\n      a\n   :: b\n",
        [ "let f a b = {a :: b}" ],
        [ "::(3,7--4,8)" ]
        "InfixUndentedLocalValue.fs",
        "module A\nlet f a b =\n    let y =\n          a\n        + b\n    y\n",
        [ "let f a b = let y = {a + b} in y" ],
        [
            "let(3,5--6,6)"
            "+(4,11--5,12)"
        ]
        "InfixUndentedBlockCol5PlusCol3.fs",
        "module A\nlet f a b =\n    a\n  + b\n",
        [ "let f a b = {a + b}" ],
        [ "+(3,5--4,6)" ]
        "InfixUndentedMinusUndent2.fs",
        "module A\nlet f a b =\n      a\n    - b\n",
        [ "let f a b = {a - b}" ],
        [ "-(3,7--4,8)" ]
        "InfixUndentedTwoPipesUndent.fs",
        "module A\nlet f a g h =\n      a\n    |> g\n    |> h\n",
        [ "let f a g h = {{a |> g} |> h}" ],
        [
            "|>(3,7--5,9)"
            "|>(3,7--4,9)"
        ]
        "InfixUndentedModuleLetCol1.fs",
        "module A\nlet x =\n  1\n+ 2\n",
        [ "let x = {1 + 2}" ],
        [ "+(3,3--4,4)" ]
        "InfixUndentedPipeUndent3NestedIf.fs",
        "module A\nlet f c a g =\n    if c then\n          a\n        |> g\n    else a\n",
        [ "let f c a g = if c then {a |> g} else a" ],
        [ "|>(4,11--5,13)" ]
        "InfixUndentedStarUndent2.fs",
        "module A\nlet f a b =\n      a\n    * b\n",
        [ "let f a b = {a * b}" ],
        [ "*(3,7--4,8)" ]
        "InfixUndentedPlusUndent1.fs",
        "module A\nlet f a b =\n      a\n     + b\n",
        [ "let f a b = {a + b}" ],
        [ "+(3,7--4,9)" ]
        "InfixUndentedSecondLineUndent.fs",
        "module A\nlet f a b c =\n      a\n      + b\n    + c\n",
        [ "let f a b c = {{a + b} + c}" ],
        [
            "+(3,7--5,8)"
            "+(3,7--4,10)"
        ]
        "InfixUndentedGreaterEqualUndent2.fs",
        "module A\nlet f a b =\n      a\n    >= b\n",
        [ "let f a b = {a >= b}" ],
        [ ">=(3,7--4,9)" ]
        "InfixLineParenIfElsePlus.fs",
        "module A\nlet f c =\n    (if c then 1 else 2\n     + 3)\n",
        [ "let f c = ({if c then 1 else 2 + 3})" ],
        [ "+(3,6--4,9)" ]
        "InfixLineParenIfPipe.fs",
        "module A\nlet f c =\n    (if c then 1\n     |> ignore)\n",
        [ "let f c = ({if c then 1 |> ignore})" ],
        [ "|>(3,6--4,15)" ]
        "InfixLineParenMatchPlus.fs",
        "module A\nlet f x =\n    (match x with\n     | _ -> 1\n     + 2)\n",
        [ "let f x = ({match x with | _ -> 1 + 2})" ],
        [ "+(3,6--5,9)" ]
        "InfixLineParenAssignPlus.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    (x <- 1\n     + 2)\n",
        [
            "let mutable x = 0"
            "let f () = ({x <- {1 + 2}})"
        ],
        [
            "set(4,6--5,9)"
            "+(4,11--5,9)"
        ]
        "InfixLineParenFunPlus.fs",
        "module A\nlet f () =\n    (fun x -> x\n     + 1)\n",
        [ "let f () = ({fun x -> x + 1})" ],
        [ "+(3,6--4,9)" ]
        "InfixLineParenElseMid.fs",
        "module A\nlet f c =\n    (if c then 1 else 2\n        + 3)\n",
        [ "let f c = ({if c then 1 else 2 + 3})" ],
        [ "+(3,6--4,12)" ]
        "InfixLineParenTuplePlus.fs",
        "module A\nlet f a b c =\n    (a, b\n     + c)\n",
        [ "let f a b c = (a, {b + c})" ],
        [
            "tuple(3,6--4,9)"
            "+(3,9--4,9)"
        ]
        "InfixLineParenTimesAfterPlus.fs",
        "module A\nlet f a b c =\n    (a + b\n     * c)\n",
        [ "let f a b c = ({a + {b * c}})" ],
        [
            "+(3,6--4,9)"
            "*(3,10--4,9)"
        ]
        "InfixLineFunPlusBlock.fs",
        "module A\nlet f () =\n    fun x -> x\n    + 1\n",
        [ "let f () = {fun x -> x + 1}" ],
        [ "+(3,5--4,8)" ]
        "InfixLineTimesAfterPlus.fs",
        "module A\nlet f a b c =\n    a + b\n    * c\n",
        [ "let f a b c = {a + {b * c}}" ],
        [
            "+(3,5--4,8)"
            "*(3,9--4,8)"
        ]
        "InfixLineTupleThenPlus.fs",
        "module A\nlet f a b c =\n    a, b\n    + c\n",
        [ "let f a b c = a, {b + c}" ],
        [
            "tuple(3,5--4,8)"
            "+(3,8--4,8)"
        ]
        "InfixLineConsChain.fs",
        "module A\nlet f a b c =\n    a :: b\n    :: c\n",
        [ "let f a b c = {a :: {b :: c}}" ],
        [
            "::(3,5--4,9)"
            "::(3,10--4,9)"
        ]
        "InfixLinePlusThenSameLinePipe.fs",
        "module A\nlet f a b =\n    a\n    + b |> id\n",
        [ "let f a b = {{a + b} |> id}" ],
        [
            "|>(3,5--4,14)"
            "+(3,5--4,8)"
        ]
        "InfixLineThenBlockPipe.fs",
        "module A\nlet f c =\n    if c then\n        1\n    else 2\n    |> ignore\n",
        [ "let f c = {if c then 1 else 2 |> ignore}" ],
        [ "|>(3,5--6,14)" ]
        "InfixLineParenLetBodyPlus.fs",
        "module A\nlet f () =\n    (let y = 1\n     y\n     + 2)\n",
        [ "let f () = (let y = 1 in {y + 2})" ],
        [
            "let(3,6--5,9)"
            "+(4,6--5,9)"
        ]
        "InfixLineAssignNextLinePlus.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    x <-\n        1\n    + 2\n",
        [
            "let mutable x = 0"
            "let f () = {{x <- 1} + 2}"
        ],
        [
            "+(4,5--6,8)"
            "set(4,5--5,10)"
        ]
        "InfixLinePipeLambdaArg.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n        x)\n",
        [ "let f xs = {xs |> [List.map (fun x -> x)]}" ],
        [ "|>(3,5--5,11)" ]
        "InfixLineSeqItemAfterPipeBlock.fs",
        "module A\nlet f xs =\n    xs\n    |> ignore\n    ignore 2\n",
        [ "let f xs = seq[{xs |> ignore}; [ignore 2]]" ],
        [
            "seq(3,5--5,13)"
            "|>(3,5--4,14)"
        ]
        "InfixLineParenEqualsUndent.fs",
        "module A\nlet f a b =\n    (  a\n     = b)\n",
        [ "let f a b = ({a = b})" ],
        [ "=(3,8--4,9)" ]
        "InfixLineSetComma.fs",
        "module A\nlet mutable x = (0, 0)\nlet f () =\n    x <- 1\n    , 2\n",
        [
            "let mutable x = (0, 0)"
            "let f () = {x <- 1, 2}"
        ],
        [
            "tuple(2,18--2,22)"
            "set(4,5--5,8)"
            "tuple(4,10--5,8)"
        ]
        "InfixLineCommaThenPlus.fs",
        "module A\nlet f a b c =\n    a + b\n    , c\n",
        [ "let f a b c = {a + b}, c" ],
        [
            "tuple(3,5--4,8)"
            "+(3,5--3,10)"
        ]
        "InfixLinePipeAfterMatchBlock.fs",
        "module A\nlet f x =\n    match x with\n    | _ ->\n        1\n    |> ignore\n",
        [ "let f x = {match x with | _ -> 1 |> ignore}" ],
        [ "|>(3,5--6,14)" ]
        "InfixLinePlusAfterLambdaBlock.fs",
        "module A\nlet f () =\n    fun x ->\n        x\n    + 1\n",
        [ "let f () = {fun x -> x + 1}" ],
        [ "+(3,5--5,8)" ]
        "InfixLineUndentAfterItem.fs",
        "module A\nlet f a b =\n      ignore 1\n      a\n    + b\n",
        [ "let f a b = seq[[ignore 1]; {a + b}]" ],
        [
            "seq(3,7--5,8)"
            "+(4,7--5,8)"
        ]
        "InfixLineDoBlock.fs",
        "module A\ndo\n    1\n    |> ignore\n",
        [ "do {1 |> ignore}" ],
        [ "|>(3,5--4,14)" ]
        "InfixLineParenLessUndent.fs",
        "module A\nlet f a b =\n    (  a\n     < b)\n",
        [ "let f a b = ({a < b})" ],
        [ "<(3,8--4,9)" ]
    ]

    let private infixLineExplicitCases = [
        "QuotationThenLine.fs", "module A\nlet f () =\n    g 1 <@ 2 @>\n        3\n", []
        "UntypedQuotationThenLine.fs", "module A\nlet f () =\n    g <@@ 2 @@>\n        3\n", []
        "UntypedQuotationArgumentThenLine.fs",
        "module A\nlet f () =\n    g 1 <@@ 2 @@>\n        3\n",
        []
        "QuotationThenArgument.fs", "module A\nlet f () =\n    g <@ 1 @> 2\n", []
        "QuotationCloseNextLine.fs", "module A\nlet f () =\n    g 1 <@ 2\n           @>\n", []
        "QuotationInParentheses.fs", "module A\nlet f () =\n    g (<@ 2 @>)\n        3\n", []
        "InfixAtColumnQuestionQuestion.fs",
        "module A
let f a b =
    a
    ?? b
",
        [
            "InfixAtColumnQuestionQuestion.fs(4,5): error FS0010: Unexpected symbol '??' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "InfixUndentedQuestionQuestion.fs",
        "module A
let f a b =
      a
    ?? b
",
        [
            "InfixUndentedQuestionQuestion.fs(4,5): error FS0010: Unexpected symbol '??' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "InfixLineParenQuestionQuestion.fs",
        "module A
let f a b =
    (a
     ?? b)
",
        [
            "InfixLineParenQuestionQuestion.fs(4,6): error FS0010: Unexpected symbol '??' in expression"
        ]
        "InfixAtColumnEquals.fs",
        "module A\nlet f a b =\n    a\n    = b\n",
        [
            "InfixAtColumnEquals.fs(4,5): error FS0010: Unexpected symbol '=' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "InfixAtColumnLess.fs",
        "module A\nlet f a b =\n    a\n    < b\n",
        [
            "InfixAtColumnLess.fs(4,5): error FS0010: Unexpected symbol '<' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "InfixAtColumnModuleExpr.fs", "module A\n1\n|> ignore\n", []
        "InfixAtColumnAtLetColumn.fs", "module A\nlet x =\n    1\n+ 2\n", []
        "InfixAtColumnGreater.fs",
        "module A\nlet f a b =\n    a\n    > b\n",
        [
            "InfixAtColumnGreater.fs(4,5): error FS0010: Unexpected symbol '>' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "InfixUndentedPlusUndent4.fs",
        "module A\nlet f a b =\n      a\n  + b\n",
        [
            "InfixUndentedPlusUndent4.fs(4,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "InfixUndentedPlusUndent4.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "InfixUndentedPipeUndent4.fs",
        "module A\nlet f a c =\n      a\n  |> c\n",
        [
            "InfixUndentedPipeUndent4.fs(4,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "InfixUndentedPipeUndent4.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "InfixUndentedEqualsUndent1.fs",
        "module A\nlet f a b =\n      a\n     = b\n",
        [
            "InfixUndentedEqualsUndent1.fs(4,6): error FS0010: Unexpected symbol '=' in binding. Expected incomplete structured construct at or before this point or other token."
            "InfixUndentedEqualsUndent1.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "InfixUndentedEqualsUndent1.fs(5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "InfixUndentedBlockCol5PlusCol2.fs",
        "module A\nlet f a b =\n    a\n + b\n",
        [
            "InfixUndentedBlockCol5PlusCol2.fs(4,2): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "InfixUndentedBlockCol5PlusCol2.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "InfixUndentedUndentThenItem.fs",
        "module A\nlet f a b =\n      a\n    + b\n      ignore 2\n",
        []
        "InfixUndentedUndentThenBlockItem.fs",
        "module A\nlet f a b =\n    ignore 1\n    a\n  + b\n    ignore 2\n",
        []
        "InfixUndentedLessUndent1.fs",
        "module A\nlet f a b =\n      a\n     < b\n",
        [
            "InfixUndentedLessUndent1.fs(4,6): error FS0010: Unexpected symbol '<' in binding. Expected incomplete structured construct at or before this point or other token."
            "InfixUndentedLessUndent1.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "InfixUndentedAmpampUndent4.fs",
        "module A\nlet f a b =\n      a\n  && b\n",
        [
            "InfixUndentedAmpampUndent4.fs(4,3): error FS0010: Unexpected symbol '&&' in binding. Expected incomplete structured construct at or before this point or other token."
            "InfixUndentedAmpampUndent4.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "InfixLineParenEqualsAt.fs",
        "module A\nlet f a b =\n    (a\n     = b)\n",
        [ "InfixLineParenEqualsAt.fs(4,6): error FS0010: Unexpected symbol '=' in expression" ]
        "InfixLineParenLessAt.fs",
        "module A\nlet f a b =\n    (a\n     < b)\n",
        [ "InfixLineParenLessAt.fs(4,6): error FS0010: Unexpected symbol '<' in expression" ]
        "InfixLineParenGreaterAt.fs",
        "module A\nlet f a b =\n    (a\n     > b)\n",
        [ "InfixLineParenGreaterAt.fs(4,6): error FS0010: Unexpected symbol '>' in expression" ]
        "InfixLineParenAmpAdjacent.fs", "module A\nlet f a b =\n    (a\n     &&b)\n", []
        "InfixLineListEqualsAt.fs",
        "module A\nlet f a b =\n    [ a\n      = b ]\n",
        [
            "InfixLineListEqualsAt.fs(4,7): error FS0010: Unexpected symbol '=' in expression. Expected ']' or other token."
            "InfixLineListEqualsAt.fs(3,5): error FS0598: Unmatched '['"
        ]
    ]

    let private continuationLineCases = [
        "ContinuationLineAfterTrue.fs",
        "module A\nlet f () =\n    g 1 true\n        2\n",
        [ "let f () = [[[g 1] Boolean true] 2]" ],
        [
            "app(3,5--4,10)"
            "app(3,5--3,13)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineAfterFalse.fs",
        "module A\nlet f () =\n    g false\n      2\n",
        [ "let f () = [[g Boolean false] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--3,12)"
        ]
        "ContinuationLineAfterTrueArgument.fs",
        "module A\nlet f () =\n    g true 1\n      2\n",
        [ "let f () = [[[g Boolean true] 1] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--3,13)"
            "app(3,5--3,11)"
        ]
        "ContinuationLinePipeAfterTrue.fs",
        "module A\nlet f () =\n    g true\n        |> ignore\n",
        [ "let f () = {[g Boolean true] |> ignore}" ],
        [
            "|>(3,5--4,18)"
            "app(3,5--3,11)"
        ]
        "ContinuationLineLocalAfterTrue.fs",
        "module A\nlet f () =\n    let y = g true\n              2\n    y\n",
        [ "let f () = let y = [[g Boolean true] 2] in y" ],
        [
            "let(3,5--5,6)"
            "app(3,13--4,16)"
            "app(3,13--3,19)"
        ]
        "ContinuationLineTrailingAndAfterTrue.fs",
        "module A\nlet f () =\n    true &&\n      false\n",
        [ "let f () = {Boolean true && Boolean false}" ],
        [ "&&(3,5--4,12)" ]
        "ContinuationLineModuleExpression.fs",
        "module Continued\nx\n    1\n",
        [ "expr [x 1]" ],
        [ "app(2,1--3,6)" ]
        "ContinuationLineAssignOperator.fs",
        "module A\nlet f () =\n    x\n        <- 1\n",
        [ "let f () = {x <- 1}" ],
        [ "set(3,5--4,13)" ]
        "ContinuationLineAfterUndentedInfix.fs",
        "module A\nlet f a b c =\n      a\n    + b\n        c\n",
        [ "let f a b c = {a + [b c]}" ],
        [
            "+(3,7--5,10)"
            "app(4,7--5,10)"
        ]
        "ContinuationDeclarationUnionBarRightOfFirstCase.fs",
        "module M\ntype U = A\n          | B\n",
        [ "type U = | A | B" ],
        []
        "ContinuationLineArgNext.fs",
        "module A\nlet f () =\n    g 1\n      2\n",
        [ "let f () = [[g 1] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineHeadThenArgs.fs",
        "module A\nlet f () =\n    ignore\n        1\n    2\n",
        [ "let f () = seq[[ignore 1]; 2]" ],
        [
            "seq(3,5--5,6)"
            "app(3,5--4,10)"
        ]
        "ContinuationLineTwoArgLines.fs",
        "module A\nlet f () =\n    g 1\n      2\n      3\n",
        [ "let f () = [[[g 1] 2] 3]" ],
        [
            "app(3,5--5,8)"
            "app(3,5--4,8)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgLinesStair.fs",
        "module A\nlet f () =\n    g 1\n      2\n        3\n",
        [ "let f () = [[[g 1] 2] 3]" ],
        [
            "app(3,5--5,10)"
            "app(3,5--4,8)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgThenItem.fs",
        "module A\nlet f () =\n    g 1\n      2\n    ignore 3\n",
        [ "let f () = seq[[[g 1] 2]; [ignore 3]]" ],
        [
            "seq(3,5--5,13)"
            "app(3,5--4,8)"
            "app(3,5--3,8)"
            "app(5,5--5,13)"
        ]
        "ContinuationLineArgBetween.fs",
        "module A\nlet f () =\n    g 1\n        2\n      3\n",
        [ "let f () = [[[g 1] 2] 3]" ],
        [
            "app(3,5--5,8)"
            "app(3,5--4,10)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgAtColPlus1.fs",
        "module A\nlet f () =\n    g 1\n     2\n",
        [ "let f () = [[g 1] 2]" ],
        [
            "app(3,5--4,7)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineModuleExpr.fs",
        "module A\nprintfn \"%d %d\"\n  1\n  2\n",
        [ "expr [[[printfn \"%d %d\"] 1] 2]" ],
        [
            "app(2,1--4,4)"
            "app(2,1--3,4)"
            "app(2,1--2,16)"
        ]
        "ContinuationLineSameLineBody.fs",
        "module A\nlet f () = g 1\n              2\n",
        [ "let f () = [[g 1] 2]" ],
        [
            "app(2,12--3,16)"
            "app(2,12--2,15)"
        ]
        "ContinuationLineLocalLetSameLine.fs",
        "module A\nlet f () =\n    let y = g 1\n              2\n    y\n",
        [ "let f () = let y = [[g 1] 2] in y" ],
        [
            "let(3,5--5,6)"
            "app(3,13--4,16)"
            "app(3,13--3,16)"
        ]
        "ContinuationLineInfixEnd.fs",
        "module A\nlet f a b =\n    a +\n      b\n",
        [ "let f a b = {a + b}" ],
        [ "+(3,5--4,8)" ]
        "ContinuationLinePipeEnd.fs",
        "module A\nlet f xs =\n    xs |>\n      List.rev\n",
        [ "let f xs = {xs |> List.rev}" ],
        [ "|>(3,5--4,15)" ]
        "ContinuationLineClauseResultArg.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> g 1\n             2\n",
        [ "let f x = match x with | _ -> [[g 1] 2]" ],
        [
            "app(4,12--5,15)"
            "app(4,12--4,15)"
        ]
        "ContinuationLineThenSameLineArg.fs",
        "module A\nlet f c =\n    if c then g 1\n                2\n    else 3\n",
        [ "let f c = if c then [[g 1] 2] else 3" ],
        [
            "app(3,15--4,18)"
            "app(3,15--3,18)"
        ]
        "ContinuationLineLambdaSameLine.fs",
        "module A\nlet f () =\n    fun x -> g x\n                 2\n",
        [ "let f () = fun x -> [[g x] 2]" ],
        [
            "app(3,14--4,19)"
            "app(3,14--3,17)"
        ]
        "ContinuationLineArgParen.fs",
        "module A\nlet f () =\n    g 1\n      (2)\n",
        [ "let f () = [[g 1] (2)]" ],
        [
            "app(3,5--4,10)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgList.fs",
        "module A\nlet f () =\n    g 1\n      [ 2 ]\n",
        [ "let f () = [[g 1] [2]]" ],
        [
            "app(3,5--4,12)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgLambda.fs",
        "module A\nlet f xs =\n    List.map\n      (fun x -> x)\n      xs\n",
        [ "let f xs = [[List.map (fun x -> x)] xs]" ],
        [
            "app(3,5--5,9)"
            "app(3,5--4,19)"
        ]
        "ContinuationLineInfixThenArg.fs",
        "module A\nlet f a b =\n    a + g\n          b\n",
        [ "let f a b = {a + [g b]}" ],
        [
            "+(3,5--4,12)"
            "app(3,9--4,12)"
        ]
        "ContinuationLineAssignArg.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    x <- g\n      1\n",
        [
            "let mutable x = 0"
            "let f () = {x <- [g 1]}"
        ],
        [
            "set(4,5--5,8)"
            "app(4,10--5,8)"
        ]
        "ContinuationLineAssignArgRight.fs",
        "module A\nlet mutable x = 0\nlet f () =\n    x <- g\n          1\n",
        [
            "let mutable x = 0"
            "let f () = {x <- [g 1]}"
        ],
        [
            "set(4,5--5,12)"
            "app(4,10--5,12)"
        ]
        "ContinuationLineDoArg.fs",
        "module A\ndo\n    g 1\n      2\n",
        [ "do [[g 1] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineModuleLetArg.fs",
        "module A\nlet x =\n    g 1\n      2\n",
        [ "let x = [[g 1] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineTupleLine.fs",
        "module A\nlet f a b =\n    a,\n      b\n",
        [ "let f a b = a, b" ],
        [ "tuple(3,5--4,8)" ]
        "ContinuationLineArgIf.fs",
        "module A\nlet f c =\n    g 1\n      (if c then 2 else 3)\n",
        [ "let f c = [[g 1] (if c then 2 else 3)]" ],
        [
            "app(3,5--4,27)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgIdentDot.fs",
        "module A\nlet f () =\n    g 1\n      x.Y\n",
        [ "let f () = [[g 1] x.Y]" ],
        [
            "app(3,5--4,10)"
            "app(3,5--3,8)"
        ]
        "ContinuationLineArgString.fs",
        "module A\nlet f () =\n    printfn \"%d\"\n      1\n",
        [ "let f () = [[printfn \"%d\"] 1]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--3,17)"
        ]
        "ContinuationDeclarationTypeAbbrev.fs",
        "module A\ntype T =\n    int\n      list\n",
        [ "type T = int list" ],
        []
        "ContinuationDeclarationRecordField.fs",
        "module A\ntype R =\n    { A: int\n      B: int }\n",
        [ "type R = { A: int; B: int }" ],
        []
        "ContinuationDeclarationMemberBodyArg.fs",
        "module A\ntype T() =\n    member _.M () = g 1\n                      2\n",
        [ "type T() = member _.M () = [[g 1] 2]" ],
        [
            "app(3,21--4,24)"
            "app(3,21--3,24)"
        ]
        "ContinuationDeclarationMemberNext.fs",
        "module A\ntype T() =\n    member _.M () =\n        g 1\n          2\n",
        [ "type T() = member _.M () = [[g 1] 2]" ],
        [
            "app(4,9--5,12)"
            "app(4,9--4,12)"
        ]
        "ContinuationDeclarationNestedModuleArg.fs",
        "module A\nmodule M =\n    let x =\n        g 1\n          2\n",
        [ "module M = [let x = [[g 1] 2]]" ],
        []
        "ContinuationDeclarationAttrThenLet.fs",
        "module A\n[<Literal>]\nlet x = 1\n",
        [ "let x = 1" ],
        []
        "ContinuationDeclarationUnionCaseNext.fs",
        "module A\ntype U =\n    | A\n        of int\n",
        [ "type U = | A of int" ],
        []
        "ContinuationDeclarationLetHeadNext.fs",
        "module A\nlet f\n      x = x\n",
        [ "let f x = x" ],
        []
    ]

    let private continuationLineExplicitCases = [
        "ContinuationLineBetweenAfterTrue.fs",
        "module A\nlet f () =\n    let y = g true\n          2\n    y\n",
        [
            "ContinuationLineBetweenAfterTrue.fs(4,11): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "ContinuationLineBetweenAfterTrue.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "ContinuationLineAfterNull.fs", "module A\nlet f () =\n    g null\n      2\n", []
        "ContinuationLineQuotationAfterTrue.fs",
        "module A\nlet f () =\n    g true <@ 1 @>\n        2\n",
        []
        "ContinuationLineTypeApplicationAfterTrue.fs",
        "module A\nlet f () =\n    g true id<int>\n        2\n",
        []
        "ContinuationLineParenOpensBlockLine.fs",
        "module A\nlet f () =\n    ignore (\n        1)\n    2\n",
        []
        "ContinuationLineSameLineBodyLeft.fs",
        "module A\nlet f () = g 1\n    2\n",
        [
            "ContinuationLineSameLineBodyLeft.fs(3,5): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "ContinuationLineSameLineBodyLeft.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "ContinuationLineSameLineBodyAt.fs", "module A\nlet f () = g 1\n           2\n", []
        "ContinuationLineLocalLetSameLineLeft.fs",
        "module A\nlet f () =\n    let y = g 1\n          2\n    y\n",
        [
            "ContinuationLineLocalLetSameLineLeft.fs(4,11): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "ContinuationLineLocalLetSameLineLeft.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "ContinuationLineLocalLetSameLineAt.fs",
        "module A\nlet f () =\n    let y = g 1\n            2\n    y\n",
        []
        "ContinuationLineClauseResultArgLeft.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> g 1\n           2\n",
        []
        "ContinuationLineLambdaSameLineLeft.fs",
        "module A\nlet f () =\n    fun x -> g x\n       2\n",
        [
            "ContinuationLineLambdaSameLineLeft.fs(4,8): error FS0010: Unexpected integer literal in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "ContinuationLineEqualityEnd.fs", "module A\nlet f a b =\n    a =\n      b\n", []
        "ContinuationLineArgLetKeyword.fs",
        "module A\nlet f () =\n    g 1\n      let y = 2 in y\n",
        [
            "ContinuationLineArgLetKeyword.fs(4,7): error FS0010: Unexpected keyword 'let' or 'use' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ContinuationDeclarationUnionOf.fs",
        "module A\ntype U =\n    | A of int *\n        int\n",
        []
        "ContinuationDeclarationMemberBodyArgLeft.fs",
        "module A\ntype T() =\n    member _.M () = g 1\n        2\n",
        [
            "ContinuationDeclarationMemberBodyArgLeft.fs(4,9): error FS0010: Unexpected integer literal in member definition"
        ]
        "ContinuationDeclarationOpenLine.fs", "module A\nopen System\n  .Text\n", []
        "ContinuationDeclarationLetEqualsNext.fs", "module A\nlet f x\n    = x\n", []
        "ContinuationDeclarationExprAfterDecl.fs",
        "module A\nlet x = 1\n  + 2\n",
        [
            "ContinuationDeclarationExprAfterDecl.fs(3,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "ContinuationDeclarationExprAfterDecl.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
    ]

    let private conditionalLineCases = [
        "ConditionalThenElseAligned.fs",
        "module A\nlet f c =\n    if c\n    then 1\n    else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--5,11)E" ]
        "ConditionalThenAlignedNoElse.fs",
        "module A\nlet f c =\n    if c\n    then ignore 1\n",
        [ "let f c = if c then [ignore 1]" ],
        [
            "app(4,10--4,18)"
            "if(3,5--4,18)-"
        ]
        "ConditionalThenAlignedBodyNext.fs",
        "module A\nlet f c =\n    if c\n    then\n        1\n    else\n        2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--7,10)E" ]
        "ConditionalThenAlignedElif.fs",
        "module A\nlet f a b =\n    if a\n    then 1\n    elif b\n    then 2\n    else 3\n",
        [ "let f a b = if a then 1 else if b then 2 else 3" ],
        [
            "if(3,5--7,11)E"
            "if(5,5--7,11)E"
        ]
        "ConditionalThenAlignedThenItem.fs",
        "module A\nlet f c =\n    if c\n    then ignore 1\n    ignore 2\n",
        [ "let f c = seq[if c then [ignore 1]; [ignore 2]]" ],
        [
            "seq(3,5--5,13)"
            "app(4,10--4,18)"
            "app(5,5--5,13)"
            "if(3,5--4,18)-"
        ]
        "ConditionalThenAlignedLocal.fs",
        "module A\nlet f c =\n    let y =\n        if c\n        then 1\n        else 2\n    y\n",
        [ "let f c = let y = if c then 1 else 2 in y" ],
        [
            "let(3,5--7,6)"
            "if(4,9--6,15)E"
        ]
        "ConditionalThenAlignedClause.fs",
        "module A\nlet f x c =\n    match x with\n    | _ ->\n        if c\n        then 1\n        else 2\n",
        [ "let f x c = match x with | _ -> if c then 1 else 2" ],
        [ "if(5,9--7,15)E" ]
        "ConditionalThenAlignedModuleLet.fs",
        "module A\nlet x =\n    if true\n    then 1\n    else 2\n",
        [ "let x = if Boolean true then 1 else 2" ],
        [ "if(3,5--5,11)E" ]
        "ConditionalLetValueIfAligned.fs",
        "module A\nlet f c =\n    let y = if c\n            then 1\n            else 2\n    y\n",
        [ "let f c = let y = if c then 1 else 2 in y" ],
        [
            "let(3,5--6,6)"
            "if(3,13--5,19)E"
        ]
        "ConditionalCondNext.fs",
        "module A\nlet f () =\n    if\n     true\n    then 1\n    else 2\n",
        [ "let f () = if Boolean true then 1 else 2" ],
        [ "if(3,5--6,11)E" ]
        "ConditionalCondNextDeep.fs",
        "module A\nlet f c =\n    if\n        c\n    then 1\n    else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--6,11)E" ]
        "ConditionalCondTwoLines.fs",
        "module A\nlet f a b =\n    if a\n       && b then 1\n    else 2\n",
        [ "let f a b = if {a && b} then 1 else 2" ],
        [
            "&&(3,8--4,12)"
            "if(3,5--5,11)E"
        ]
        "ConditionalCondTwoLinesLeft.fs",
        "module A\nlet f a b =\n    if a\n     && b then 1\n    else 2\n",
        [ "let f a b = if {a && b} then 1 else 2" ],
        [
            "&&(3,8--4,10)"
            "if(3,5--5,11)E"
        ]
        "ConditionalCondArgBetween.fs",
        "module A\nlet f () =\n    if g 1\n      2 then 1\n    else 2\n",
        [ "let f () = if [[g 1] 2] then 1 else 2" ],
        [
            "app(3,8--4,8)"
            "app(3,8--3,11)"
            "if(3,5--5,11)E"
        ]
        "ConditionalCondAfterAnd.fs",
        "module A\nlet f a b =\n    if a &&\n      b then 1\n    else 2\n",
        [ "let f a b = if {a && b} then 1 else 2" ],
        [
            "&&(3,8--4,8)"
            "if(3,5--5,11)E"
        ]
        "ConditionalThenElseIndented.fs",
        "module A\nlet f c =\n    if c\n      then 1\n      else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--5,13)E" ]
        "ConditionalElseIndented.fs",
        "module A\nlet f c =\n    if c then 1\n      else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--4,13)E" ]
        "ConditionalElseRightOfThen.fs",
        "module A\nlet f c =\n    if c then 1\n             else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--4,20)E" ]
        "ConditionalThenIndentedBodyNext.fs",
        "module A\nlet f c =\n    if c\n      then\n        1\n      else\n        2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--7,10)E" ]
        "ConditionalThenIndentedNoElse.fs",
        "module A\nlet f c =\n    if c\n      then ignore 1\n    ignore 2\n",
        [ "let f c = seq[if c then [ignore 1]; [ignore 2]]" ],
        [
            "seq(3,5--5,13)"
            "app(4,12--4,20)"
            "app(5,5--5,13)"
            "if(3,5--4,20)-"
        ]
        "ConditionalElifIndented.fs",
        "module A\nlet f a b =\n    if a then 1\n      elif b then 2\n      else 3\n",
        [ "let f a b = if a then 1 else if b then 2 else 3" ],
        [
            "if(3,5--5,13)E"
            "if(4,7--5,13)E"
        ]
        "ConditionalThenRightOfCond.fs",
        "module A\nlet f c =\n    if c\n         then 1\n         else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--5,16)E" ]
        "ConditionalBodyIfPlus2.fs",
        "module A\nlet f x =\n    if x > 0\n       then\n      x\n       else\n      0\n",
        [ "let f x = if {x > 0} then x else 0" ],
        [
            ">(3,8--3,13)"
            "if(3,5--7,8)E"
        ]
        "ConditionalElseBodyIfPlus1.fs",
        "module A\nlet f c =\n    if c then 1\n    else\n     2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--5,7)E" ]
        "ConditionalThenBodyIfPlus1Aligned.fs",
        "module A\nlet f c =\n    if c then\n     1\n    else 2\n",
        [ "let f c = if c then 1 else 2" ],
        [ "if(3,5--5,11)E" ]
        "ConditionalNestedElseOuter.fs",
        "module A\nlet f a b =\n    if a then if b then 1\n              else 2\n",
        [ "let f a b = if a then if b then 1 else 2" ],
        [
            "if(3,5--4,21)-"
            "if(3,15--4,21)E"
        ]
        "ConditionalNestedElseLeftInner.fs",
        "module A\nlet f a b =\n    if a then if b then ignore 1\n      else ignore 2\n",
        [ "let f a b = if a then if b then [ignore 1] else [ignore 2]" ],
        [
            "app(3,25--3,33)"
            "app(4,12--4,20)"
            "if(3,5--4,20)E"
            "if(3,15--3,33)-"
        ]
        "ConditionalNestedElseAtInner.fs",
        "module A\nlet f a b =\n    if a then if b then 1\n              else 2\n    else 3\n",
        [ "let f a b = if a then if b then 1 else 2 else 3" ],
        [
            "if(3,5--5,11)E"
            "if(3,15--4,21)E"
        ]
        "ConditionalNestedBlockElse.fs",
        "module A\nlet f a b =\n    if a then\n        if b then 1\n        else 2\n    else 3\n",
        [ "let f a b = if a then if b then 1 else 2 else 3" ],
        [
            "if(3,5--6,11)E"
            "if(4,9--5,15)E"
        ]
        "ConditionalNestedBlockElseOuter.fs",
        "module A\nlet f a b =\n    if a then\n        if b then ignore 1\n    else ignore 2\n",
        [ "let f a b = if a then if b then [ignore 1] else [ignore 2]" ],
        [
            "app(4,19--4,27)"
            "app(5,10--5,18)"
            "if(3,5--5,18)E"
            "if(4,9--4,27)-"
        ]
        "ConditionalNestedElseBetween.fs",
        "module A\nlet f a b =\n    if a then if b then ignore 1\n            else ignore 2\n",
        [ "let f a b = if a then if b then [ignore 1] else [ignore 2]" ],
        [
            "app(3,25--3,33)"
            "app(4,18--4,26)"
            "if(3,5--4,26)E"
            "if(3,15--3,33)-"
        ]
        "ConditionalParenThenAligned.fs",
        "module A\nlet f c =\n    (if c\n     then 1\n     else 2)\n",
        [ "let f c = (if c then 1 else 2)" ],
        [ "if(3,6--5,12)E" ]
        "ConditionalParenThenIndented.fs",
        "module A\nlet f c =\n    (if c\n       then 1\n       else 2)\n",
        [ "let f c = (if c then 1 else 2)" ],
        [ "if(3,6--5,14)E" ]
        "ConditionalParenCondNext.fs",
        "module A\nlet f c =\n    (if\n       c\n     then 1\n     else 2)\n",
        [ "let f c = (if c then 1 else 2)" ],
        [ "if(3,6--6,12)E" ]
        "ConditionalListIfAligned.fs",
        "module A\nlet f c =\n    [ if c\n      then 1\n      else 2 ]\n",
        [ "let f c = [if c then 1 else 2]" ],
        [ "if(3,7--5,13)E" ]
        "ConditionalCondLess.fs",
        "module A\nlet f a b =\n    if a < b\n    then 1\n    else 2\n",
        [ "let f a b = if {a < b} then 1 else 2" ],
        [
            "<(3,8--3,13)"
            "if(3,5--5,11)E"
        ]
        "ConditionalCondGreaterLine.fs",
        "module A\nlet f a b =\n    if a\n       > b then 1\n    else 2\n",
        [ "let f a b = if {a > b} then 1 else 2" ],
        [
            ">(3,8--4,11)"
            "if(3,5--5,11)E"
        ]
        "ConditionalThenPipeLine.fs",
        "module A\nlet f c =\n    if c\n    then 1\n    else 2\n    |> ignore\n",
        [ "let f c = {if c then 1 else 2 |> ignore}" ],
        [
            "|>(3,5--6,14)"
            "if(3,5--5,11)E"
        ]
        "ConditionalThenInfixLine.fs",
        "module A\nlet f c =\n    if c\n    then 1\n         + 2\n    else 3\n",
        [ "let f c = if c then {1 + 2} else 3" ],
        [
            "+(4,10--5,13)"
            "if(3,5--6,11)E"
        ]
        "ConditionalThenArgLine.fs",
        "module A\nlet f c =\n    if c\n    then g 1\n           2\n    else 3\n",
        [ "let f c = if c then [[g 1] 2] else 3" ],
        [
            "app(4,10--5,13)"
            "app(4,10--4,13)"
            "if(3,5--6,11)E"
        ]
        "ConditionalCondAppLine.fs",
        "module A\nlet f () =\n    if g 1\n         2 then 1\n    else 2\n",
        [ "let f () = if [[g 1] 2] then 1 else 2" ],
        [
            "app(3,8--4,11)"
            "app(3,8--3,11)"
            "if(3,5--5,11)E"
        ]
        "ConditionalThenTrueArg.fs",
        "module A\nlet f c =\n    if c\n    then g true\n           2\n    else 3\n",
        [ "let f c = if c then [[g Boolean true] 2] else 3" ],
        [
            "app(4,10--5,13)"
            "app(4,10--4,16)"
            "if(3,5--6,11)E"
        ]
        "ConditionalThenAlignedNested.fs",
        "module A\nlet f a b =\n    if a\n    then\n        if b\n        then 1\n        else 2\n    else 3\n",
        [ "let f a b = if a then if b then 1 else 2 else 3" ],
        [
            "if(3,5--8,11)E"
            "if(5,9--7,15)E"
        ]
        "ConditionalDoIfAligned.fs",
        "module A\ndo\n    if true\n    then ignore 1\n",
        [ "do if Boolean true then [ignore 1]" ],
        [
            "app(4,10--4,18)"
            "if(3,5--4,18)-"
        ]
    ]

    let private conditionalLineExplicitCases = [
        "ConditionalCondNextThenSame.fs",
        "module A\nlet f c =\n    if\n        c then 1\n    else 2\n",
        []
        "ConditionalCondNextAtIf.fs",
        "module A\nlet f c =\n    if\n    c\n    then 1\n    else 2\n",
        [
            "ConditionalCondNextAtIf.fs(3,8): error FS0010: Incomplete structured construct at or before this point in expression"
            "ConditionalCondNextAtIf.fs(3,5): error FS0589: Incomplete conditional. Expected 'if <expr> then <expr>' or 'if <expr> then <expr> else <expr>'."
            "ConditionalCondNextAtIf.fs(5,12): error FS0010: Incomplete structured construct at or before this point in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ConditionalLetValueThenLeft.fs",
        "module A\nlet f c =\n    let y = if c\n          then 1\n          else 2\n    y\n",
        [
            "ConditionalLetValueThenLeft.fs(4,11): error FS0010: Incomplete structured construct at or before this point in expression"
            "ConditionalLetValueThenLeft.fs(3,13): error FS0589: Incomplete conditional. Expected 'if <expr> then <expr>' or 'if <expr> then <expr> else <expr>'."
            "ConditionalLetValueThenLeft.fs(4,11): error FS0010: Unexpected keyword 'then' in binding. Expected incomplete structured construct at or before this point or other token."
            "ConditionalLetValueThenLeft.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "ConditionalLetValueThenLeft.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
            "ConditionalLetValueThenLeft.fs(5,11): error FS0010: Unexpected keyword 'else' in binding. Expected incomplete structured construct at or before this point or other token."
            "ConditionalLetValueThenLeft.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "ConditionalLetValueThenLeft.fs(6,5): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "ConditionalBodyAtIf.fs",
        "module A\nlet f x =\n    if x > 0\n       then\n    x\n       else\n    0\n",
        [
            "ConditionalBodyAtIf.fs(5,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "ConditionalBodyAtIf.fs(5,5): error FS3524: Expecting expression"
            "ConditionalBodyAtIf.fs(6,8): error FS0010: Unexpected keyword 'else' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ConditionalThenBodyAtIf.fs",
        "module A\nlet f c =\n    if c then\n    1\n    else 2\n",
        [
            "ConditionalThenBodyAtIf.fs(4,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "ConditionalThenBodyAtIf.fs(4,5): error FS3524: Expecting expression"
            "ConditionalThenBodyAtIf.fs(5,5): error FS0010: Unexpected keyword 'else' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ConditionalNestedSameLineElse.fs",
        "module A\nlet f a b =\n    if a then if b then 1 else 2 else 3\n",
        [
            "ConditionalNestedSameLineElse.fs(3,34): error FS0010: Unexpected keyword 'else' in expression. Expected incomplete structured construct at or before this point or other token."
            "ConditionalNestedSameLineElse.fs(4,1): error FS0010: Incomplete structured construct at or before this point in binding. Expected incomplete structured construct at or before this point or other token."
            "ConditionalNestedSameLineElse.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "ConditionalNestedSameLineElse.fs(4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "ConditionalThenQuote.fs",
        "module A\nlet f c =\n    if c\n    then <@ 1 @>\n    else <@ 2 @>\n",
        []
        "ConditionalCondTypeapp.fs",
        "module A\nlet f () =\n    if id<int> 1 = 1\n    then 1\n    else 2\n",
        []
        "ConditionalCondQuote.fs",
        "module A\nlet f () =\n    if g <@ 1 @>\n    then 1\n    else 2\n",
        []
        "ConditionalThenElseSemicolon.fs",
        "module A\nlet f c =\n    if c\n    then 1; 2\n    else 3\n",
        []
    ]

    let private matchLineCases = [
        "MatchNestedAtClauseColumnInParentheses.fs",
        "module A\nlet f a b =\n    (match a with\n     | 1 ->\n     match b with\n     | 2 -> 3\n     | _ -> 4)\n",
        [ "let f a b = (match a with | 1 -> match b with | 2 -> 3 | _ -> 4)" ],
        [
            "match(3,6--7,14)1"
            "match(5,6--7,14)2"
        ]
        "MatchWithLineWithClause.fs",
        "module A\nlet f x =\n    match x\n      with _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--4,18)1" ]
        "MatchWithAligned.fs",
        "module A\nlet f x =\n    match x\n    with\n    | _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--5,13)1" ]
        "MatchWithAlignedClauseSame.fs",
        "module A\nlet f x =\n    match x\n    with _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--4,16)1" ]
        "MatchWithAlignedTwoClauses.fs",
        "module A\nlet f x =\n    match x\n    with\n    | 1 -> 2\n    | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,13)2" ]
        "MatchWithIndented.fs",
        "module A\nlet f x =\n    match x\n      with\n      | _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--5,15)1" ]
        "MatchWithIndentedBarsLeft.fs",
        "module A\nlet f x =\n    match x\n      with\n    | _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--5,13)1" ]
        "MatchWithRightOfInput.fs",
        "module A\nlet f x =\n    match x\n          with\n          | _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--5,19)1" ]
        "MatchWithAlignedItemAfter.fs",
        "module A\nlet f x =\n    match x\n    with\n    | _ -> ignore 1\n    ignore 2\n",
        [ "let f x = seq[match x with | _ -> [ignore 1]; [ignore 2]]" ],
        [
            "seq(3,5--6,13)"
            "app(5,12--5,20)"
            "app(6,5--6,13)"
            "match(3,5--5,20)1"
        ]
        "MatchWithInLetValue.fs",
        "module A\nlet f x =\n    let y =\n        match x\n        with\n        | _ -> 1\n    y\n",
        [ "let f x = let y = match x with | _ -> 1 in y" ],
        [
            "let(3,5--7,6)"
            "match(4,9--6,17)1"
        ]
        "MatchBarsAligned.fs",
        "module A\nlet f x =\n    match x with\n    | 1 -> 2\n    | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--5,13)2" ]
        "MatchBarsIndented.fs",
        "module A\nlet f x =\n    match x with\n      | 1 -> 2\n      | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--5,15)2" ]
        "MatchBarBetween.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n      | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,15)2" ]
        "MatchLastBodyAtMatch.fs",
        "module A\nlet f x =\n    match x with\n    | 1 -> 2\n    | _ ->\n    3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,6)2" ]
        "MatchFirstBodyAtMatch.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n    2\n    | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,13)2" ]
        "MatchLastBodyTwoItems.fs",
        "module A\nlet f x =\n    match x with\n    | _ ->\n    ignore 1\n    2\n",
        [ "let f x = match x with | _ -> seq[[ignore 1]; 2]" ],
        [
            "seq(5,5--6,6)"
            "app(5,5--5,13)"
            "match(3,5--6,6)1"
        ]
        "MatchLastBodyAtBarIndented.fs",
        "module A\nlet f x =\n    match x with\n      | _ ->\n      3\n",
        [ "let f x = match x with | _ -> 3" ],
        [ "match(3,5--5,8)1" ]
        "MatchLastBodyLet.fs",
        "module A\nlet f x =\n    match x with\n    | _ ->\n    let y = 1\n    y\n",
        [ "let f x = match x with | _ -> let y = 1 in y" ],
        [
            "let(5,5--6,6)"
            "match(3,5--6,6)1"
        ]
        "MatchLastBodyInLetValue.fs",
        "module A\nlet f x =\n    let y =\n        match x with\n        | _ ->\n        3\n    y\n",
        [ "let f x = let y = match x with | _ -> 3 in y" ],
        [
            "let(3,5--7,6)"
            "match(4,9--6,10)1"
        ]
        "MatchLastBodyAtMatchMidLine.fs",
        "module A\nlet f x =\n    let y = match x with\n            | _ ->\n            3\n    y\n",
        [ "let f x = let y = match x with | _ -> 3 in y" ],
        [
            "let(3,5--6,6)"
            "match(3,13--5,14)1"
        ]
        "MatchLastBodyAfterGuard.fs",
        "module A\nlet f x =\n    match x with\n    | y when y > 0 ->\n    y\n",
        [ "let f x = match x with | y when {y > 0} -> y" ],
        [ "match(3,5--5,6)1" ]
        "MatchNestedSameLineOuterBar.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 -> match y with | 2 -> 3 | _ -> 4\n    | _ -> 5\n",
        [ "let f x y = match x with | 1 -> match y with | 2 -> 3 | _ -> 4 | _ -> 5" ],
        [
            "match(3,5--5,13)2"
            "match(4,12--4,42)2"
        ]
        "MatchNestedBlockOuterBar.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 ->\n        match y with\n        | 2 -> 3\n        | _ -> 4\n    | _ -> 5\n",
        [ "let f x y = match x with | 1 -> match y with | 2 -> 3 | _ -> 4 | _ -> 5" ],
        [
            "match(3,5--8,13)2"
            "match(5,9--7,17)2"
        ]
        "MatchNestedSameLineNextBar.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 -> match y with\n           | 2 -> 3\n    | _ -> 5\n",
        [ "let f x y = match x with | 1 -> match y with | 2 -> 3 | _ -> 5" ],
        [
            "match(3,5--6,13)2"
            "match(4,12--5,20)1"
        ]
        "MatchNestedInnerBarAtInner.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 -> match y with\n           | 2 -> 3\n           | _ -> 4\n    | _ -> 5\n",
        [ "let f x y = match x with | 1 -> match y with | 2 -> 3 | _ -> 4 | _ -> 5" ],
        [
            "match(3,5--7,13)2"
            "match(4,12--6,20)2"
        ]
        "MatchNestedBarBetween.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 -> match y with\n           | 2 -> 3\n         | _ -> 4\n",
        [ "let f x y = match x with | 1 -> match y with | 2 -> 3 | _ -> 4" ],
        [
            "match(3,5--6,18)2"
            "match(4,12--5,20)1"
        ]
        "MatchNestedLastBodyAtInner.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 ->\n        match y with\n        | _ ->\n        3\n    | _ -> 5\n",
        [ "let f x y = match x with | 1 -> match y with | _ -> 3 | _ -> 5" ],
        [
            "match(3,5--8,13)2"
            "match(5,9--7,10)1"
        ]
        "MatchNestedLastBodyAtOuter.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 -> 2\n    | _ ->\n    match y with\n    | _ -> 3\n",
        [ "let f x y = match x with | 1 -> 2 | _ -> match y with | _ -> 3" ],
        [
            "match(3,5--7,13)2"
            "match(6,5--7,13)1"
        ]
        "MatchNestedInParenOuterBar.fs",
        "module A\nlet f x y =\n    (match x with\n     | 1 -> match y with | 2 -> 3 | _ -> 4\n     | _ -> 5)\n",
        [ "let f x y = (match x with | 1 -> match y with | 2 -> 3 | _ -> 4 | _ -> 5)" ],
        [
            "match(3,6--5,14)2"
            "match(4,13--4,43)2"
        ]
        "MatchLambdaMatchOuterBar.fs",
        "module A\nlet f x =\n    match x with\n    | 1 -> fun y -> match y with | 2 -> 3 | _ -> 4\n    | _ -> fun _ -> 5\n",
        [
            "let f x = match x with | 1 -> fun y -> match y with | 2 -> 3 | _ -> 4 | _ -> fun _ -> 5"
        ],
        [
            "match(3,5--5,22)2"
            "match(4,21--4,51)2"
        ]
        "MatchIfInClauseOuterBar.fs",
        "module A\nlet f c x =\n    match x with\n    | 1 -> if c then 2 else 3\n    | _ -> 4\n",
        [ "let f c x = match x with | 1 -> if c then 2 else 3 | _ -> 4" ],
        [
            "if(4,12--4,30)E"
            "match(3,5--5,13)2"
        ]
        "MatchMatchInIfThen.fs",
        "module A\nlet f c x =\n    if c then\n        match x with\n        | _ -> 1\n    else 2\n",
        [ "let f c x = if c then match x with | _ -> 1 else 2" ],
        [
            "if(3,5--6,11)E"
            "match(4,9--5,17)1"
        ]
        "MatchMatchInIfSameLine.fs",
        "module A\nlet f c x =\n    if c then match x with | _ -> 1\n    else 2\n",
        [ "let f c x = if c then match x with | _ -> 1 else 2" ],
        [
            "if(3,5--4,11)E"
            "match(3,15--3,36)1"
        ]
        "MatchMatchInLambda.fs",
        "module A\nlet f xs =\n    List.map (fun x ->\n        match x with\n        | _ -> 1) xs\n",
        [ "let f xs = [[List.map (fun x -> match x with | _ -> 1)] xs]" ],
        [
            "app(3,5--5,21)"
            "app(3,5--5,18)"
            "match(4,9--5,17)1"
        ]
        "MatchMatchInParenWithAligned.fs",
        "module A\nlet f x =\n    (match x\n     with\n     | _ -> 1)\n",
        [ "let f x = (match x with | _ -> 1)" ],
        [ "match(3,6--5,14)1" ]
        "MatchMatchInParenLastBody.fs",
        "module A\nlet f x =\n    (match x with\n     | _ ->\n     3)\n",
        [ "let f x = (match x with | _ -> 3)" ],
        [ "match(3,6--5,7)1" ]
        "MatchMatchInList.fs",
        "module A\nlet f x =\n    [ match x with\n      | _ -> 1 ]\n",
        [ "let f x = [match x with | _ -> 1]" ],
        [ "match(3,7--4,15)1" ]
        "MatchMatchInParenBars.fs",
        "module A\nlet f x =\n    (match x with\n     | 1 -> 2\n     | _ -> 3)\n",
        [ "let f x = (match x with | 1 -> 2 | _ -> 3)" ],
        [ "match(3,6--5,14)2" ]
        "MatchIfInClauseElse.fs",
        "module A\nlet f c x =\n    match x with\n    | _ ->\n        if c then 1\n        else 2\n",
        [ "let f c x = match x with | _ -> if c then 1 else 2" ],
        [
            "if(5,9--6,15)E"
            "match(3,5--6,15)1"
        ]
        "MatchMatchAfterPipe.fs",
        "module A\nlet f x =\n    x\n    |> fun y ->\n        match y with\n        | _ -> 1\n",
        [ "let f x = {x |> fun y -> match y with | _ -> 1}" ],
        [
            "|>(3,5--6,17)"
            "match(5,9--6,17)1"
        ]
        "MatchClausePipeLine.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> 1\n    |> ignore\n",
        [ "let f x = {match x with | _ -> 1 |> ignore}" ],
        [
            "|>(3,5--5,14)"
            "match(3,5--4,13)1"
        ]
        "MatchClauseInfixLine.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> 1\n           + 2\n",
        [ "let f x = match x with | _ -> {1 + 2}" ],
        [
            "+(4,12--5,15)"
            "match(3,5--5,15)1"
        ]
        "MatchClauseArgLine.fs",
        "module A\nlet f x =\n    match x with\n    | _ -> g 1\n             2\n",
        [ "let f x = match x with | _ -> [[g 1] 2]" ],
        [
            "app(4,12--5,15)"
            "app(4,12--4,15)"
            "match(3,5--5,15)1"
        ]
        "MatchWithThenBarOnLine.fs",
        "module A\nlet f x =\n    match x\n    with | _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--4,18)1" ]
        "MatchMatchInputNextLine.fs",
        "module A\nlet f x =\n    match\n        x\n    with\n    | _ -> 1\n",
        [ "let f x = match x with | _ -> 1" ],
        [ "match(3,5--6,13)1" ]
        "MatchMatchInputTwoLines.fs",
        "module A\nlet f x y =\n    match x\n          y with\n    | _ -> 1\n",
        [ "let f x y = match [x y] with | _ -> 1" ],
        [
            "app(3,11--4,12)"
            "match(3,5--5,13)1"
        ]
        "MatchBarLeftOfMidMatch.fs",
        "module A\nlet f x =\n    let y = match x with\n            | 1 -> 2\n            | _ -> 3\n    y\n",
        [ "let f x = let y = match x with | 1 -> 2 | _ -> 3 in y" ],
        [
            "let(3,5--6,6)"
            "match(3,13--5,21)2"
        ]
        "MatchWithLeftOfMidMatch.fs",
        "module A\nlet f x =\n    let y = match x\n            with _ -> 1\n    y\n",
        [ "let f x = let y = match x with | _ -> 1 in y" ],
        [
            "let(3,5--5,6)"
            "match(3,13--4,24)1"
        ]
        "MatchNestedBlockBodyAtInnerThenOuter.fs",
        "module A\nlet f x y =\n    match x with\n    | 1 ->\n        match y with\n        | 2 -> 3\n        | _ ->\n        4\n    | _ -> 5\n",
        [ "let f x y = match x with | 1 -> match y with | 2 -> 3 | _ -> 4 | _ -> 5" ],
        [
            "match(3,5--9,13)2"
            "match(5,9--8,10)2"
        ]
        "MatchWithLineThenBlockBarBetween.fs",
        "module A\nlet f x =\n    match x\n      with\n        | 1 -> 2\n        | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,17)2" ]
    ]

    let private matchLineExplicitCases = [
        "MatchWithIndentedSameClause.fs",
        "module A\nlet f x =\n    match x\n      with _ -> 1\n         | _ -> 2\n",
        []
        "MatchWithLeftOfMatch.fs",
        "module A\nlet f x =\n    match x\n  with\n    | _ -> 1\n",
        [
            "MatchWithLeftOfMatch.fs(4,3): error FS0010: Incomplete structured construct at or before this point in expression. Expected 'with' or other token."
            "MatchWithLeftOfMatch.fs(4,3): error FS0010: Unexpected keyword 'with' in binding. Expected incomplete structured construct at or before this point or other token."
            "MatchWithLeftOfMatch.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "MatchWithLeftOfMatch.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "MatchBarLeftOfMatch.fs",
        "module A\nlet f x =\n    match x with\n      | 1 -> 2\n    | _ -> 3\n",
        [
            "MatchBarLeftOfMatch.fs(5,5): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "MatchBarLeftOfFirst.fs",
        "module A\nlet f x =\n    match x with\n      | 1 -> 2\n     | _ -> 3\n",
        [
            "MatchBarLeftOfFirst.fs(5,6): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
            "MatchBarLeftOfFirst.fs(5,6): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "MatchFirstNoBarThenBar.fs",
        "module A\nlet f x =\n    match x with\n    1 -> 2\n    | _ -> 3\n",
        []
        "MatchLastBodyLeft.fs",
        "module A\nlet f x =\n    match x with\n    | _ ->\n   3\n",
        [
            "MatchLastBodyLeft.fs(5,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "MatchLastBodyLeft.fs(5,4): error FS0010: Incomplete structured construct at or before this point in pattern matching"
            "MatchLastBodyLeft.fs(5,4): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "MatchLastBodyLeft.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "MatchLastBodyBetween.fs",
        "module A\nlet f x =\n    match x with\n      | _ ->\n     3\n",
        []
        "MatchMatchInElse.fs",
        "module A\nlet f c x =\n    if c then 1\n    else\n    match x with\n    | _ -> 2\n",
        []
        "MatchFunction.fs", "module A\nlet f =\n    function\n    | _ -> 1\n", []
        "MatchTryWith.fs", "module A\nlet f () =\n    try\n        1\n    with _ -> 2\n", []
        "MatchTryWithBars.fs",
        "module A\nlet f () =\n    try\n        1\n    with\n    | _ -> 2\n",
        []
        "MatchMatchInTry.fs",
        "module A\nlet f x =\n    try\n        match x with\n        | _ -> 1\n    with _ -> 2\n",
        []
        "MatchRecordWithLine.fs", "module A\nlet f r =\n    { r\n      with A = 1 }\n", []
        "MatchMatchBang.fs", "module A\nlet f x =\n    match x\n    with\n    | _ -> <@ 1 @>\n", []
        "MatchClauseTypeapp.fs",
        "module A\nlet f x =\n    match x\n    with\n    | _ -> id<int> 1\n",
        []
        "MatchSecondWith.fs",
        "module A\nlet f x =\n    match x\n    with\n    | _ -> 1\n    with _ -> 2\n",
        [
            "MatchSecondWith.fs(6,5): error FS0010: Unexpected keyword 'with' in binding. Expected incomplete structured construct at or before this point or other token."
            "MatchSecondWith.fs(7,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "MatchBarsLeftOfMidMatch.fs",
        "module A\nlet f x =\n    let y = match x with\n        | 1 -> 2\n        | _ -> 3\n    y\n",
        [
            "MatchBarsLeftOfMidMatch.fs(4,9): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:13). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "MatchBarsLeftOfMidMatch.fs(4,9): error FS0010: Incomplete structured construct at or before this point in expression"
            "MatchBarsLeftOfMidMatch.fs(4,9): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
            "MatchBarsLeftOfMidMatch.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "MatchBarsLeftOfMidMatch.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]
    ]

    let private clauseBarLayoutCases = [
        "ClauseBarG04.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n             | 0 -> b\n           | y -> y\n",
        []
        "ClauseBarG07.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 ->\n        match b with\n        | 0 -> b\n      | y -> y\n    | x -> x\n",
        []
        "ClauseBarD14.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | y -> x + y\n      | z -> z\n",
        []
        "ClauseBarLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n       | y -> y\n",
        [ "FS0058(7,8)" ]
        "ClauseBarLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n      | y -> y\n",
        []
        "ClauseBarLeft3.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n     | y -> y\n",
        []
        "ClauseBarLeft4AtOuter.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n    | y -> y\n",
        []
        "ClauseBarTopLeft1.fs",
        "module A\nlet f b =\n    match b with\n      | 0 -> 1\n     | y -> y\n",
        [ "FS0058(5,6)" ]
        "ClauseBarTopLeft2.fs",
        "module A\nlet f b =\n    match b with\n      | 0 -> 1\n    | y -> y\n",
        []
        "ClauseBarTopDeeperLeft2.fs",
        "module A\nlet f b =\n    match b with\n        | 0 -> 1\n      | y -> y\n",
        [ "FS0058(5,7)" ]
        "ClauseBarTopDeeperLeft1.fs",
        "module A\nlet f b =\n    match b with\n        | 0 -> 1\n       | y -> y\n",
        [ "FS0058(5,8)" ]
        "ClauseBarNobarLeft2.fs",
        "module A\nlet f b =\n    match b with\n      0 -> 1\n    | y -> y\n",
        []
        "ClauseBarNobarLeft2Deeper.fs",
        "module A\nlet f b =\n    match b with\n        0 -> 1\n      | y -> y\n",
        [ "FS0058(5,7)" ]
        "ClauseBarNobarLeft1.fs",
        "module A\nlet f b =\n    match b with\n        0 -> 1\n       | y -> y\n",
        [ "FS0058(5,8)" ]
        "ClauseBarInnerSameLineLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n           | 0 -> b\n         | y -> y\n",
        []
        "ClauseBarInnerSameLineLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n           | 0 -> b\n          | y -> y\n",
        [ "FS0058(6,11)" ]
        "ClauseBarInnerSameLineAtMatch.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n             | 0 -> b\n           | y -> y\n    | x -> x\n",
        []
        "ClauseBarG07OuterAfter.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 ->\n        match b with\n          | 0 -> b\n        | y -> y\n    | x -> x\n",
        []
        "ClauseBarTwoLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n      | 1 -> 2\n      | y -> y\n",
        []
        "ClauseBarBodyBlockBarPlus1.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n     | _ -> 3\n",
        []
        "ClauseBarBodyBlockBarPlus2.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n      | _ -> 3\n",
        []
        "ClauseBarBodyBlockBarPlus3.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n       | _ -> 3\n",
        []
        "ClauseBarInnerBodyDeeperLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 ->\n            1\n      | y -> y\n",
        []
        "ClauseBarInnerBodyDeeperLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 ->\n            1\n       | y -> y\n",
        [ "FS0058(8,8)" ]
        "ClauseBarTopBodyBlockBetween.fs",
        "module A\nlet f b =\n    match b with\n    | 0 ->\n        g 1\n      | y -> y\n",
        []
        "ClauseBarNobarInnerLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        0 -> 1\n      | y -> y\n",
        [ "FS0058(7,7)" ]
        "ClauseBarNobarInnerLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        0 -> 1\n       | y -> y\n",
        [ "FS0058(7,8)" ]
        "ClauseBarInnerBarsIndentedLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n          | 0 -> 1\n         | y -> y\n",
        [ "FS0058(7,10)" ]
        "ClauseBarInnerBarsIndentedLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n          | 0 -> 1\n        | y -> y\n",
        []
        "ClauseBarIfBodyBarBetween.fs",
        "module A\nlet f c x =\n    match x with\n    | 1 ->\n        if c then 2 else 3\n      | _ -> 4\n",
        []
        "ClauseBarThreeLevels.fs",
        "module A\nlet f a b c =\n    match a with\n    | x ->\n        match b with\n        | y ->\n            match c with\n            | z -> z\n          | w -> w\n    | v -> v\n",
        []
    ]

    let private clauseBarCases = [
        "ClauseBarG04.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n             | 0 -> b\n           | y -> y\n",
        [ "let f a b = match a with | 0 -> match b with | 0 -> b | y -> y" ],
        [
            "match(3,5--6,20)2"
            "match(4,12--5,22)1"
        ]
        "ClauseBarG07.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 ->\n        match b with\n        | 0 -> b\n      | y -> y\n    | x -> x\n",
        [ "let f a b = match a with | 0 -> match b with | 0 -> b | y -> y | x -> x" ],
        [
            "match(3,5--8,13)3"
            "match(5,9--6,17)1"
        ]
        "ClauseBarD14.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | y -> x + y\n      | z -> z\n",
        [ "let f a b = match a with | x -> match b with | y -> {x + y} | z -> z" ],
        [
            "+(6,16--6,21)"
            "match(3,5--7,15)2"
            "match(5,9--6,21)1"
        ]
        "ClauseBarLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n      | y -> y\n",
        [ "let f a b = match a with | x -> match b with | 0 -> 1 | y -> y" ],
        [
            "match(3,5--7,15)2"
            "match(5,9--6,17)1"
        ]
        "ClauseBarLeft3.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n     | y -> y\n",
        [ "let f a b = match a with | x -> match b with | 0 -> 1 | y -> y" ],
        [
            "match(3,5--7,14)2"
            "match(5,9--6,17)1"
        ]
        "ClauseBarLeft4AtOuter.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n    | y -> y\n",
        [ "let f a b = match a with | x -> match b with | 0 -> 1 | y -> y" ],
        [
            "match(3,5--7,13)2"
            "match(5,9--6,17)1"
        ]
        "ClauseBarInnerSameLineLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n           | 0 -> b\n         | y -> y\n",
        [ "let f a b = match a with | 0 -> match b with | 0 -> b | y -> y" ],
        [
            "match(3,5--6,18)2"
            "match(4,12--5,20)1"
        ]
        "ClauseBarInnerSameLineAtMatch.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n             | 0 -> b\n           | y -> y\n    | x -> x\n",
        [ "let f a b = match a with | 0 -> match b with | 0 -> b | y -> y | x -> x" ],
        [
            "match(3,5--7,13)3"
            "match(4,12--5,22)1"
        ]
        "ClauseBarTwoLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n      | 1 -> 2\n      | y -> y\n",
        [ "let f a b = match a with | x -> match b with | 0 -> 1 | 1 -> 2 | y -> y" ],
        [
            "match(3,5--8,15)3"
            "match(5,9--6,17)1"
        ]
        "ClauseBarBodyBlockBarPlus1.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n     | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,14)2" ]
        "ClauseBarBodyBlockBarPlus2.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n      | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,15)2" ]
        "ClauseBarBodyBlockBarPlus3.fs",
        "module A\nlet f x =\n    match x with\n    | 1 ->\n        2\n       | _ -> 3\n",
        [ "let f x = match x with | 1 -> 2 | _ -> 3" ],
        [ "match(3,5--6,16)2" ]
        "ClauseBarInnerBodyDeeperLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 ->\n            1\n      | y -> y\n",
        [ "let f a b = match a with | x -> match b with | 0 -> 1 | y -> y" ],
        [
            "match(3,5--8,15)2"
            "match(5,9--7,14)1"
        ]
        "ClauseBarTopBodyBlockBetween.fs",
        "module A\nlet f b =\n    match b with\n    | 0 ->\n        g 1\n      | y -> y\n",
        [ "let f b = match b with | 0 -> [g 1] | y -> y" ],
        [
            "app(5,9--5,12)"
            "match(3,5--6,15)2"
        ]
        "ClauseBarIfBodyBarBetween.fs",
        "module A\nlet f c x =\n    match x with\n    | 1 ->\n        if c then 2 else 3\n      | _ -> 4\n",
        [ "let f c x = match x with | 1 -> if c then 2 else 3 | _ -> 4" ],
        [
            "if(5,9--5,27)E"
            "match(3,5--6,15)2"
        ]
        "ClauseBarThreeLevels.fs",
        "module A\nlet f a b c =\n    match a with\n    | x ->\n        match b with\n        | y ->\n            match c with\n            | z -> z\n          | w -> w\n    | v -> v\n",
        [
            "let f a b c = match a with | x -> match b with | y -> match c with | z -> z | w -> w | v -> v"
        ],
        [
            "match(3,5--10,13)2"
            "match(5,9--9,19)2"
            "match(7,13--8,21)1"
        ]
    ]

    let private clauseBarExplicitCases = [
        "ClauseBarLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 -> 1\n       | y -> y\n",
        [
            "ClauseBarLeft1.fs(7,8): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
        ]
        "ClauseBarTopLeft1.fs",
        "module A\nlet f b =\n    match b with\n      | 0 -> 1\n     | y -> y\n",
        [
            "ClauseBarTopLeft1.fs(5,6): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
            "ClauseBarTopLeft1.fs(5,6): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ClauseBarTopLeft2.fs",
        "module A\nlet f b =\n    match b with\n      | 0 -> 1\n    | y -> y\n",
        [
            "ClauseBarTopLeft2.fs(5,5): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ClauseBarTopDeeperLeft2.fs",
        "module A\nlet f b =\n    match b with\n        | 0 -> 1\n      | y -> y\n",
        [
            "ClauseBarTopDeeperLeft2.fs(5,7): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ClauseBarTopDeeperLeft1.fs",
        "module A\nlet f b =\n    match b with\n        | 0 -> 1\n       | y -> y\n",
        [
            "ClauseBarTopDeeperLeft1.fs(5,8): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
            "ClauseBarTopDeeperLeft1.fs(5,8): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "ClauseBarNobarLeft2.fs",
        "module A\nlet f b =\n    match b with\n      0 -> 1\n    | y -> y\n",
        []
        "ClauseBarNobarLeft2Deeper.fs",
        "module A\nlet f b =\n    match b with\n        0 -> 1\n      | y -> y\n",
        []
        "ClauseBarNobarLeft1.fs",
        "module A\nlet f b =\n    match b with\n        0 -> 1\n       | y -> y\n",
        []
        "ClauseBarInnerSameLineLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 -> match b with\n           | 0 -> b\n          | y -> y\n",
        [
            "ClauseBarInnerSameLineLeft1.fs(6,11): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
        ]
        "ClauseBarG07OuterAfter.fs",
        "module A\nlet f a b =\n    match a with\n    | 0 ->\n        match b with\n          | 0 -> b\n        | y -> y\n    | x -> x\n",
        []
        "ClauseBarInnerBodyDeeperLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        | 0 ->\n            1\n       | y -> y\n",
        [
            "ClauseBarInnerBodyDeeperLeft1.fs(8,8): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
        ]
        "ClauseBarNobarInnerLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        0 -> 1\n      | y -> y\n",
        []
        "ClauseBarNobarInnerLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n        0 -> 1\n       | y -> y\n",
        []
        "ClauseBarInnerBarsIndentedLeft1.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n          | 0 -> 1\n         | y -> y\n",
        [
            "ClauseBarInnerBarsIndentedLeft1.fs(7,10): error FS0058: The '|' tokens separating rules of this pattern match are misaligned by one column. Consider realigning your code or using further indentation."
        ]
        "ClauseBarInnerBarsIndentedLeft2.fs",
        "module A\nlet f a b =\n    match a with\n    | x ->\n        match b with\n          | 0 -> 1\n        | y -> y\n",
        []
    ]

    let private lambdaBodyCases = [
        "LambdaBodyX08.fs",
        "module A\nlet f g =\n    g (fun x ->\n        x)\n",
        [ "let f g = [g (fun x -> x)]" ],
        [ "app(3,5--4,11)" ]
        "LambdaBodyX09.fs",
        "module A\nlet f xs =\n    xs |> List.map (fun x ->\n        x + 1)\n",
        [ "let f xs = {xs |> [List.map (fun x -> {x + 1})]}" ],
        [
            "|>(3,5--4,15)"
            "+(4,9--4,14)"
            "app(3,11--4,15)"
        ]
        "LambdaBodyX17.fs",
        "module A\nlet f g =\n    g (fun x ->\n  x)\n",
        [ "let f g = [g (fun x -> x)]" ],
        [ "app(3,5--4,5)" ]
        "LambdaBodyLaterLineAtBlock.fs",
        "module A\nlet f xs =\n    ignore 0\n    List.iter (fun x ->\n    ignore x) xs\n",
        [ "let f xs = seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] xs]]" ],
        [
            "seq(3,5--5,17)"
            "app(3,5--3,13)"
            "app(4,5--5,17)"
            "app(4,5--5,14)"
            "app(5,5--5,13)"
        ]
        "LambdaBodyLaterLineLeftOfBlock.fs",
        "module A\nlet f xs =\n    ignore 0\n    List.iter (fun x ->\n  ignore x) xs\n",
        [ "let f xs = seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] xs]]" ],
        [
            "seq(3,5--5,15)"
            "app(3,5--3,13)"
            "app(4,5--5,15)"
            "app(4,5--5,12)"
            "app(5,3--5,11)"
        ]
        "LambdaBodyLaterLineTwoBodyLines.fs",
        "module A\nlet f xs =\n    ignore 0\n    List.iter (fun x ->\n    ignore x\n    ignore 2) xs\n",
        [ "let f xs = seq[[ignore 0]; [[List.iter (fun x -> seq[[ignore x]; [ignore 2]])] xs]]" ],
        [
            "seq(3,5--6,17)"
            "seq(5,5--6,13)"
            "app(3,5--3,13)"
            "app(4,5--6,17)"
            "app(4,5--6,14)"
            "app(5,5--5,13)"
            "app(6,5--6,13)"
        ]
        "LambdaBodyLaterLineThenItem.fs",
        "module A\nlet f xs =\n    ignore 0\n    List.iter (fun x ->\n    ignore x) xs\n    ignore 3\n",
        [ "let f xs = seq[[ignore 0]; seq[[[List.iter (fun x -> [ignore x])] xs]; [ignore 3]]]" ],
        [
            "seq(3,5--6,13)"
            "seq(4,5--6,13)"
            "app(3,5--3,13)"
            "app(4,5--5,17)"
            "app(4,5--5,14)"
            "app(5,5--5,13)"
            "app(6,5--6,13)"
        ]
        "LambdaBodyLocalBodyLine.fs",
        "module A\nlet f xs =\n    let y = 1\n    List.iter (fun x ->\n    ignore (x + y)) xs\n",
        [ "let f xs = let y = 1 in [[List.iter (fun x -> [ignore ({x + y})])] xs]" ],
        [
            "let(3,5--5,23)"
            "+(5,13--5,18)"
            "app(4,5--5,23)"
            "app(4,5--5,20)"
            "app(5,5--5,19)"
        ]
        "LambdaBodyNestedLocalBodyLine.fs",
        "module A\nlet f xs =\n    let a = 1\n    let b = 2\n    List.iter (fun x ->\n    ignore (x + a + b)) xs\n",
        [
            "let f xs = let a = 1 in let b = 2 in [[List.iter (fun x -> [ignore ({{x + a} + b})])] xs]"
        ],
        [
            "let(3,5--6,27)"
            "let(4,5--6,27)"
            "+(6,13--6,22)"
            "+(6,13--6,18)"
            "app(5,5--6,27)"
            "app(5,5--6,24)"
            "app(6,5--6,23)"
        ]
        "LambdaBodyThenBlockLine.fs",
        "module A\nlet f c xs =\n    if c then\n        ignore 0\n        List.iter (fun x ->\n        ignore x) xs\n",
        [ "let f c xs = if c then seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] xs]]" ],
        [
            "seq(4,9--6,21)"
            "app(4,9--4,17)"
            "app(5,9--6,21)"
            "app(5,9--6,18)"
            "app(6,9--6,17)"
            "if(3,5--6,21)-"
        ]
        "LambdaBodyThenBlockLeft.fs",
        "module A\nlet f c xs =\n    if c then\n        List.iter (fun x ->\n      ignore x) xs\n",
        [ "let f c xs = if c then [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(4,9--5,19)"
            "app(4,9--5,16)"
            "app(5,7--5,15)"
            "if(3,5--5,19)-"
        ]
        "LambdaBodyClauseBlockLine.fs",
        "module A\nlet f v xs =\n    match v with\n    | _ ->\n        ignore 0\n        List.iter (fun x ->\n        ignore x) xs\n",
        [
            "let f v xs = match v with | _ -> seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] xs]]"
        ],
        [
            "seq(5,9--7,21)"
            "app(5,9--5,17)"
            "app(6,9--7,21)"
            "app(6,9--7,18)"
            "app(7,9--7,17)"
            "match(3,5--7,21)1"
        ]
        "LambdaBodyClauseBlockLeft.fs",
        "module A\nlet f v xs =\n    match v with\n    | _ ->\n        List.iter (fun x ->\n      ignore x) xs\n",
        [ "let f v xs = match v with | _ -> [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(5,9--6,19)"
            "app(5,9--6,16)"
            "app(6,7--6,15)"
            "match(3,5--6,19)1"
        ]
        "LambdaBodyClauseBlockAtBar.fs",
        "module A\nlet f v xs =\n    match v with\n    | _ ->\n        List.iter (fun x ->\n    ignore x) xs\n",
        [ "let f v xs = match v with | _ -> [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(5,9--6,17)"
            "app(5,9--6,14)"
            "app(6,5--6,13)"
            "match(3,5--6,17)1"
        ]
        "LambdaBodyElseBlockLeft.fs",
        "module A\nlet f c xs =\n    if c then ()\n    else\n        List.iter (fun x ->\n      ignore x) xs\n",
        [ "let f c xs = if c then () else [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(5,9--6,19)"
            "app(5,9--6,16)"
            "app(6,7--6,15)"
            "if(3,5--6,19)E"
        ]
        "LambdaBodyElseBlockAtIf.fs",
        "module A\nlet f c xs =\n    if c then ()\n    else\n        List.iter (fun x ->\n    ignore x) xs\n",
        [ "let f c xs = if c then () else [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(5,9--6,17)"
            "app(5,9--6,14)"
            "app(6,5--6,13)"
            "if(3,5--6,17)E"
        ]
        "LambdaBodyNestedLambdaAtBlock.fs",
        "module A\nlet f xs ys =\n    List.iter (fun x ->\n    List.iter (fun y ->\n    ignore (x + y)) ys) xs\n",
        [
            "let f xs ys = [[List.iter (fun x -> [[List.iter (fun y -> [ignore ({x + y})])] ys])] xs]"
        ],
        [
            "+(5,13--5,18)"
            "app(3,5--5,27)"
            "app(3,5--5,24)"
            "app(4,5--5,23)"
            "app(4,5--5,20)"
            "app(5,5--5,19)"
        ]
        "LambdaBodyNestedLambdaLeft.fs",
        "module A\nlet f xs ys =\n    List.iter (fun x ->\n      List.iter (fun y ->\n    ignore (x + y)) ys) xs\n",
        [
            "let f xs ys = [[List.iter (fun x -> [[List.iter (fun y -> [ignore ({x + y})])] ys])] xs]"
        ],
        [
            "+(5,13--5,18)"
            "app(3,5--5,27)"
            "app(3,5--5,24)"
            "app(4,7--5,23)"
            "app(4,7--5,20)"
            "app(5,5--5,19)"
        ]
        "LambdaBodyModuleExpression.fs",
        "module A\nList.iter (fun x ->\n ignore x) [ 1 ]\n",
        [ "expr [[List.iter (fun x -> [ignore x])] [1]]" ],
        [
            "app(2,1--3,17)"
            "app(2,1--3,11)"
            "app(3,2--3,10)"
        ]
        "LambdaBodyModuleDo.fs",
        "module A\ndo\n    List.iter (fun x ->\n    ignore x) [ 1 ]\n",
        [ "do [[List.iter (fun x -> [ignore x])] [1]]" ],
        [
            "app(3,5--4,20)"
            "app(3,5--4,14)"
            "app(4,5--4,13)"
        ]
        "LambdaBodyMemberBodyRightOfMember.fs",
        "module A\ntype T() =\n    member _.M xs =\n        List.iter (fun x ->\n     ignore x) xs\n",
        [ "type T() = member _.M xs = [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(4,9--5,18)"
            "app(4,9--5,15)"
            "app(5,6--5,14)"
        ]
        "LambdaBodyMemberBlockLine.fs",
        "module A\ntype T() =\n    member _.M xs =\n        ignore 0\n        List.iter (fun x ->\n        ignore x) xs\n",
        [ "type T() = member _.M xs = seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] xs]]" ],
        [
            "seq(4,9--6,21)"
            "app(4,9--4,17)"
            "app(5,9--6,21)"
            "app(5,9--6,18)"
            "app(6,9--6,17)"
        ]
        "LambdaBodyNestedModuleLet.fs",
        "module A\nmodule M =\n    let f xs =\n        ignore 0\n        List.iter (fun x ->\n        ignore x) xs\n",
        [ "module M = [let f xs = seq[[ignore 0]; [[List.iter (fun x -> [ignore x])] xs]]]" ],
        []
        "LambdaBodyFunSameLineBodyLeft.fs",
        "module A\nlet g = fun x ->\n    x + 1\n",
        [ "let g = fun x -> {x + 1}" ],
        [ "+(3,5--3,10)" ]
        "LambdaBodyFunThenLine.fs",
        "module A\nlet f xs =\n    List.iter (fun x ->\n    ignore x\n    ) xs\n",
        [ "let f xs = [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(3,5--5,9)"
            "app(3,5--5,6)"
            "app(4,5--4,13)"
        ]
        "LambdaBodyBodyInfixLine.fs",
        "module A\nlet f g =\n    ignore 0\n    g (fun x ->\n    x\n    + 1)\n",
        [ "let f g = seq[[ignore 0]; [g (fun x -> {x + 1})]]" ],
        [
            "seq(3,5--6,9)"
            "+(5,5--6,8)"
            "app(3,5--3,13)"
            "app(4,5--6,9)"
        ]
        "LambdaBodyBodyMatch.fs",
        "module A\nlet f g =\n    ignore 0\n    g (fun x ->\n    match x with\n    | y -> y)\n",
        [ "let f g = seq[[ignore 0]; [g (fun x -> match x with | y -> y)]]" ],
        [
            "seq(3,5--6,14)"
            "app(3,5--3,13)"
            "app(4,5--6,14)"
            "match(5,5--6,13)1"
        ]
        "LambdaBodyBodyIf.fs",
        "module A\nlet f g c =\n    ignore 0\n    g (fun x ->\n    if c then x else 0)\n",
        [ "let f g c = seq[[ignore 0]; [g (fun x -> if c then x else 0)]]" ],
        [
            "seq(3,5--5,24)"
            "app(3,5--3,13)"
            "app(4,5--5,24)"
        ]
        "LambdaBodyThenSameLineLeftOfThen.fs",
        "module A\nlet f c xs =\n    if c then List.iter (fun x ->\n      ignore x) xs\n",
        [ "let f c xs = if c then [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(3,15--4,19)"
            "app(3,15--4,16)"
            "app(4,7--4,15)"
            "if(3,5--4,19)-"
        ]
        "LambdaBodyNestedIfInnerLeft.fs",
        "module A\nlet f c d xs =\n    if c then\n        if d then\n            List.iter (fun x ->\n          ignore x) xs\n",
        [ "let f c d xs = if c then if d then [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(5,13--6,23)"
            "app(5,13--6,20)"
            "app(6,11--6,19)"
            "if(3,5--6,23)-"
            "if(4,9--6,23)-"
        ]
    ]

    let private lambdaBodyExplicitCases = [
        "LambdaBodyLaterLineAtLet.fs",
        "module A\nlet f xs =\n    ignore 0\n    List.iter (fun x ->\nignore x) xs\n",
        [
            "LambdaBodyLaterLineAtLet.fs(5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyLaterLineAtLet.fs(4,16): error FS0611: Missing function body"
            "LambdaBodyLaterLineAtLet.fs(5,1): error FS0010: Unexpected identifier in expression"
        ]
        "LambdaBodyLocalValueAtLet.fs",
        "module A\nlet f xs =\n    let g = List.iter (fun x ->\n    ignore x)\n    g xs\n",
        [
            "LambdaBodyLocalValueAtLet.fs(4,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyLocalValueAtLet.fs(3,24): error FS0611: Missing function body"
            "LambdaBodyLocalValueAtLet.fs(4,5): error FS0010: Unexpected identifier in expression"
        ]
        "LambdaBodyLocalValueRightOfLet.fs",
        "module A\nlet f xs =\n    let g = List.iter (fun x ->\n     ignore x)\n    g xs\n",
        []
        "LambdaBodyThenBlockAtIf.fs",
        "module A\nlet f c xs =\n    if c then\n        List.iter (fun x ->\n    ignore x) xs\n",
        [
            "LambdaBodyThenBlockAtIf.fs(5,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyThenBlockAtIf.fs(4,20): error FS0611: Missing function body"
            "LambdaBodyThenBlockAtIf.fs(5,5): error FS0010: Unexpected identifier in expression"
        ]
        "LambdaBodyClauseInLetValueLeftOfMatch.fs",
        "module A\nlet f v xs =\n    let r =\n        match v with\n        | _ ->\n            List.iter (fun x ->\n      ignore x) xs\n    r\n",
        [
            "LambdaBodyClauseInLetValueLeftOfMatch.fs(7,7): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:9). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyClauseInLetValueLeftOfMatch.fs(6,24): error FS0611: Missing function body"
            "LambdaBodyClauseInLetValueLeftOfMatch.fs(7,7): error FS0010: Unexpected identifier in expression"
        ]
        "LambdaBodyClauseInLetValueAtLet.fs",
        "module A\nlet f v xs =\n    let r =\n        match v with\n        | _ ->\n            List.iter (fun x ->\n    ignore x) xs\n    r\n",
        [
            "LambdaBodyClauseInLetValueAtLet.fs(7,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:9). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyClauseInLetValueAtLet.fs(6,24): error FS0611: Missing function body"
            "LambdaBodyClauseInLetValueAtLet.fs(7,5): error FS0010: Unexpected identifier in expression"
        ]
        "LambdaBodyMemberBodyAtMember.fs",
        "module A\ntype T() =\n    member _.M xs =\n        List.iter (fun x ->\n    ignore x) xs\n",
        [
            "LambdaBodyMemberBodyAtMember.fs(5,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyMemberBodyAtMember.fs(4,20): error FS0611: Missing function body"
            "LambdaBodyMemberBodyAtMember.fs(5,5): error FS0010: Unexpected identifier in expression"
        ]
        "LambdaBodyFunValueBodyLeft.fs", "module A\nlet f =\n    fun x ->\n    x + 1\n", []
        "LambdaBodyPipeBackFun.fs",
        "module A\nlet f xs =\n    xs |> List.iter <| fun x ->\n    ignore x\n",
        []
        "LambdaBodyBodyQuote.fs",
        "module A\nlet f g =\n    ignore 0\n    g (fun x ->\n    <@ x @>)\n",
        []
        "LambdaBodyBodyTypeapp.fs",
        "module A\nlet f g =\n    ignore 0\n    g (fun x ->\n    id<int> x)\n",
        []
        "LambdaBodyNestedIfAtInnerIf.fs",
        "module A\nlet f c d xs =\n    if c then\n        if d then\n            List.iter (fun x ->\n        ignore x) xs\n",
        [
            "LambdaBodyNestedIfAtInnerIf.fs(6,9): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:9). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "LambdaBodyNestedIfAtInnerIf.fs(5,24): error FS0611: Missing function body"
            "LambdaBodyNestedIfAtInnerIf.fs(6,9): error FS0010: Unexpected identifier in expression"
        ]
    ]

    let private undentedCloseCases = [
        "UndentedCloseG1IgnoreList.fs",
        "module A\nlet f () =\n    g 1 (fun x ->\n  ignore x) [ 1 ]\n",
        [ "let f () = [[[g 1] (fun x -> [ignore x])] [1]]" ],
        [
            "app(3,5--4,18)"
            "app(3,5--4,12)"
            "app(3,5--3,8)"
            "app(4,3--4,11)"
        ]
        "UndentedCloseIterIgnoreInt.fs",
        "module A\nlet f () =\n    List.iter (fun x ->\n  ignore x) 2\n",
        [ "let f () = [[List.iter (fun x -> [ignore x])] 2]" ],
        [
            "app(3,5--4,14)"
            "app(3,5--4,12)"
            "app(4,3--4,11)"
        ]
        "UndentedCloseIterIgnoreIdent.fs",
        "module A\nlet f xs =\n    List.iter (fun x ->\n  ignore x) xs\n",
        [ "let f xs = [[List.iter (fun x -> [ignore x])] xs]" ],
        [
            "app(3,5--4,15)"
            "app(3,5--4,12)"
            "app(4,3--4,11)"
        ]
        "UndentedCloseIterIgnoreIntLong.fs",
        "module A\nlet f () =\n    List.iterate (fun x ->\n  ignore x) 2\n",
        [ "let f () = [[List.iterate (fun x -> [ignore x])] 2]" ],
        [
            "app(3,5--4,14)"
            "app(3,5--4,12)"
            "app(4,3--4,11)"
        ]
        "UndentedCloseCloseAtBlockCol.fs",
        "module A\nlet f () =\n    g (fun x ->\n   x) 2\n",
        [ "let f () = [[g (fun x -> x)] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--4,6)"
        ]
        "UndentedCloseCloseLeftThenDecl.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)\nlet y = 1\n",
        [
            "let f () = [g (fun x -> x)]"
            "let y = 1"
        ],
        [ "app(3,5--4,5)" ]
        "UndentedCloseListCloseLeftArg.fs",
        "module A\nlet f () =\n    g [ 1;\n  2 ] 3\n",
        [ "let f () = [[g [1; 2]] 3]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--4,6)"
        ]
        "UndentedCloseCloseLeftBodyAppArg.fs",
        "module A\nlet f () =\n    g (fun x ->\n  ignore x) 2\n",
        [ "let f () = [[g (fun x -> [ignore x])] 2]" ],
        [
            "app(3,5--4,14)"
            "app(3,5--4,12)"
            "app(4,3--4,11)"
        ]
        "UndentedCloseCloseLeftLastInThen.fs",
        "module A\nlet f c =\n    if c then\n        g (fun x ->\n      x)\n    else 3\n",
        [ "let f c = if c then [g (fun x -> x)] else 3" ],
        [
            "app(4,9--5,9)"
            "if(3,5--6,11)E"
        ]
        "UndentedCloseRecordCloseLeftArg.fs",
        "module A\nlet f () =\n    g { A = 1;\n B = 2 } 3\n",
        [ "let f () = [[g {A = 1; B = 2}] 3]" ],
        [
            "app(3,5--4,11)"
            "app(3,5--4,9)"
        ]
        "UndentedCloseCondCloseLeftBodyApp.fs",
        "module A\nlet f () =\n    if g (fun x ->\n         ignore x) then 1 else 2\n",
        [ "let f () = if [g (fun x -> [ignore x])] then 1 else 2" ],
        [
            "app(3,8--4,19)"
            "app(4,10--4,18)"
            "if(3,5--4,33)E"
        ]
        "UndentedCloseCloseOnly.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)\n",
        [ "let f () = [g (fun x -> x)]" ],
        [ "app(3,5--4,5)" ]
        "UndentedCloseCloseThenList.fs",
        "module A\nlet f () =\n    List.iter (fun x ->\n  ignore x) [ 1 ]\n",
        [ "let f () = [[List.iter (fun x -> [ignore x])] [1]]" ],
        [
            "app(3,5--4,18)"
            "app(3,5--4,12)"
            "app(4,3--4,11)"
        ]
        "UndentedCloseCloseAtBlockThenArg.fs",
        "module A\nlet f () =\n    g 1 (fun x ->\n    x) 2\n",
        [ "let f () = [[[g 1] (fun x -> x)] 2]" ],
        [
            "app(3,5--4,9)"
            "app(3,5--4,7)"
            "app(3,5--3,8)"
        ]
        "UndentedCloseCloseRightOfBlockThenArg.fs",
        "module A\nlet f () =\n    g 1 (fun x ->\n     x) 2\n",
        [ "let f () = [[[g 1] (fun x -> x)] 2]" ],
        [
            "app(3,5--4,10)"
            "app(3,5--4,8)"
            "app(3,5--3,8)"
        ]
        "UndentedCloseNestedCloseThenArg.fs",
        "module A\nlet f () =\n    g (h (fun x ->\n  x)) 2\n",
        [ "let f () = [[g ([h (fun x -> x)])] 2]" ],
        [
            "app(3,5--4,8)"
            "app(3,5--4,6)"
            "app(3,8--4,5)"
        ]
        "UndentedCloseListCloseThenArg.fs",
        "module A\nlet f () =\n    g [ (fun x ->\n  x) ] 2\n",
        [ "let f () = [[g [(fun x -> x)]] 2]" ],
        [
            "app(3,5--4,9)"
            "app(3,5--4,7)"
        ]
        "UndentedCloseRecordCloseThenArg.fs",
        "module A\nlet f () =\n    g { A = (fun x ->\n  x) } 2\n",
        [ "let f () = [[g {A = (fun x -> x)}] 2]" ],
        [
            "app(3,5--4,9)"
            "app(3,5--4,7)"
        ]
        "UndentedCloseModuleExprCloseThenArg.fs",
        "module A\ng 1 (fun x ->\n x) 2\n",
        [ "expr [[[g 1] (fun x -> x)] 2]" ],
        [
            "app(2,1--3,6)"
            "app(2,1--3,4)"
            "app(2,1--2,4)"
        ]
    ]

    let private undentedCloseExplicitCases = [
        "UndentedCloseGXInt.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) 2\n",
        [
            "UndentedCloseGXInt.fs(4,6): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXInt.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseIterXIdent.fs",
        "module A\nlet f xs =\n    List.iter (fun x ->\n  x) xs\n",
        [
            "UndentedCloseIterXIdent.fs(4,6): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseIterXIdent.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseG1XList.fs",
        "module A\nlet f () =\n    g 1 (fun x ->\n  x) [ 1 ]\n",
        [
            "UndentedCloseG1XList.fs(4,6): error FS0010: Unexpected symbol '[' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseG1XList.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "UndentedCloseG1XList.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedCloseG1XIdent.fs",
        "module A\nlet f y =\n    g 1 (fun x ->\n  x) y\n",
        [
            "UndentedCloseG1XIdent.fs(4,6): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseG1XIdent.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXIdent.fs",
        "module A\nlet f y =\n    g (fun x ->\n  x) y\n",
        [
            "UndentedCloseGXIdent.fs(4,6): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXIdent.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXList.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) [ 1 ]\n",
        [
            "UndentedCloseGXList.fs(4,6): error FS0010: Unexpected symbol '[' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXList.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "UndentedCloseGXList.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedCloseGXParen.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) (2)\n",
        [
            "UndentedCloseGXParen.fs(4,6): error FS0010: Unexpected symbol '(' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXParen.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "UndentedCloseGXParen.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedCloseGXString.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) \"s\"\n",
        [
            "UndentedCloseGXString.fs(4,6): error FS0010: Unexpected string literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXString.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXIntFar.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)          2\n",
        [
            "UndentedCloseGXIntFar.fs(4,15): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXIntFar.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXIdentSameColAsParen.fs",
        "module A\nlet f y =\n    g (fun x ->\n  x)   y\n",
        [
            "UndentedCloseGXIdentSameColAsParen.fs(4,8): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXIdentSameColAsParen.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXIdentColParenPlus1.fs",
        "module A\nlet f y =\n    g (fun x ->\n  x)    y\n",
        [
            "UndentedCloseGXIdentColParenPlus1.fs(4,9): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXIdentColParenPlus1.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXIntColParenPlus1.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)    2\n",
        [
            "UndentedCloseGXIntColParenPlus1.fs(4,9): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXIntColParenPlus1.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseGXIntColParen.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)   2\n",
        [
            "UndentedCloseGXIntColParen.fs(4,8): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseGXIntColParen.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseLeftThenInfixLine.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)\n    |> ignore\n",
        [
            "UndentedCloseCloseLeftThenInfixLine.fs(5,5): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseLeftThenInfixLine.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseThenBlockCloseLeftArg.fs",
        "module A\nlet f c =\n    if c then\n        g (fun x ->\n      x) 2\n    else 3\n",
        [
            "UndentedCloseThenBlockCloseLeftArg.fs(5,10): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedCloseLocalValueCloseLeftThenBody.fs",
        "module A\nlet f () =\n    let y = g (fun x ->\n      x)\n    y\n",
        []
        "UndentedCloseCloseLeftThenMoreIndented.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x)\n        2\n",
        [
            "UndentedCloseCloseLeftThenMoreIndented.fs(5,9): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseLeftThenMoreIndented.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseLeftThenElse.fs",
        "module A\nlet f c =\n    if c then g (fun x ->\n  x)\n    else 3\n",
        [
            "UndentedCloseCloseLeftThenElse.fs(4,3): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "UndentedCloseCloseLeftThenElse.fs(3,18): error FS0611: Missing function body"
            "UndentedCloseCloseLeftThenElse.fs(4,3): error FS0010: Unexpected identifier in expression"
            "UndentedCloseCloseLeftThenElse.fs(5,5): error FS0010: Unexpected keyword 'else' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseLeftThenElse.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "UndentedCloseCloseLeftThenElse.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "UndentedCloseCloseLeftThenBar.fs",
        "module A\nlet f v =\n    match v with\n    | _ -> g (fun x ->\n  x)\n    | _ -> 3\n",
        [
            "UndentedCloseCloseLeftThenBar.fs(5,3): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "UndentedCloseCloseLeftThenBar.fs(4,15): error FS0611: Missing function body"
            "UndentedCloseCloseLeftThenBar.fs(5,3): error FS0010: Unexpected identifier in expression"
            "UndentedCloseCloseLeftThenBar.fs(6,5): error FS0010: Unexpected symbol '|' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseLeftThenBar.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseLeftMember.fs",
        "module A\ntype T() =\n    member _.M () =\n        g (fun x ->\n      x) 2\n",
        [
            "UndentedCloseCloseLeftMember.fs(5,10): error FS0010: Unexpected integer literal in member definition"
        ]
        "UndentedCloseListCloseLeftArg.fs",
        "module A\nlet f () =\n    g [ 1;\n 2 ] 3\n",
        [
            "UndentedCloseListCloseLeftArg.fs(4,6): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseListCloseLeftArg.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseListCloseLeftThenItem.fs",
        "module A\nlet f () =\n    ignore [ 1;\n 2 ]\n    ignore 3\n",
        [
            "UndentedCloseListCloseLeftThenItem.fs(5,5): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseListCloseLeftThenItem.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseApp2.fs",
        "module A\nlet f () =\n    g 1 (fun x ->\n  x) 2\n",
        [
            "UndentedCloseApp2.fs(4,6): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseApp2.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCond1.fs",
        "module A\nlet f () =\n    if g (fun x ->\n  x) then 1 else 2\n",
        [
            "UndentedCloseCond1.fs(4,3): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "UndentedCloseCond1.fs(3,11): error FS0611: Missing function body"
            "UndentedCloseCond1.fs(4,3): error FS0010: Unexpected identifier in expression"
            "UndentedCloseCond1.fs(4,13): error FS0010: Unexpected keyword 'else' in expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedCloseCloseThenPipe.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) |> ignore\n",
        [
            "UndentedCloseCloseThenPipe.fs(4,6): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseThenPipe.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseInnerCloseThenArgInOuter.fs",
        "module A\nlet f () =\n    g (h (fun x ->\n  x) 2)\n",
        []
        "UndentedCloseCloseThenComma.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x), 2\n",
        [
            "UndentedCloseCloseThenComma.fs(4,5): error FS0010: Unexpected symbol ',' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseThenComma.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseThenNextLineItem.fs",
        "module A\nlet f () =\n    ignore (fun x ->\n  x)\n    ignore 2\n",
        [
            "UndentedCloseCloseThenNextLineItem.fs(5,5): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseThenNextLineItem.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseLocalLetCloseThenArg.fs",
        "module A\nlet f () =\n    let y = g 1 (fun x ->\n  x) 2\n    y\n",
        [
            "UndentedCloseLocalLetCloseThenArg.fs(4,3): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "UndentedCloseLocalLetCloseThenArg.fs(3,18): error FS0611: Missing function body"
            "UndentedCloseLocalLetCloseThenArg.fs(4,3): error FS0010: Unexpected identifier in expression"
            "UndentedCloseLocalLetCloseThenArg.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
            "UndentedCloseLocalLetCloseThenArg.fs(4,6): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseLocalLetCloseThenArg.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseThenSemicolon.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x); 2\n",
        [
            "UndentedCloseCloseThenSemicolon.fs(4,5): error FS0010: Unexpected symbol ';' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseThenSemicolon.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseThenInfix.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) + 1\n",
        [
            "UndentedCloseCloseThenInfix.fs(4,6): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseThenInfix.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedCloseCloseThenOpenAgain.fs",
        "module A\nlet f () =\n    g (fun x ->\n  x) (fun y ->\n  y)\n",
        [
            "UndentedCloseCloseThenOpenAgain.fs(4,6): error FS0010: Unexpected symbol '(' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedCloseCloseThenOpenAgain.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "UndentedCloseCloseThenOpenAgain.fs(6,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private bindingOffsideCases = [
        "BlockOffsideBinding.fs",
        "module A\nlet f =\n    g\n  1\n",
        [
            "BlockOffsideBinding.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BlockOffsideBinding.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BlockOffsideNestedBinding.fs",
        "module A\nmodule M =\n    let f =\n        g\n      1\n",
        [
            "BlockOffsideNestedBinding.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BlockOffsideNestedBinding.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BlockOffsideDo.fs",
        "module A\ndo\n    ignore\n  1\n",
        [
            "BlockOffsideDo.fs(4,3): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BlockOffsideNestedDo.fs",
        "module A\nmodule M =\n    do\n        ignore\n      1\n",
        [
            "BlockOffsideNestedDo.fs(5,7): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "ContinuationLineLeftAfterTrue.fs",
        "module A\nlet f () =\n    g true\n   2\n",
        [
            "ContinuationLineLeftAfterTrue.fs(4,4): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "ContinuationLineLeftAfterTrue.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideX25.fs", "module A\nlet f () =\n    let x =\n      1\n    x\n", []
        "BindingOffsideRBind.fs",
        "module A\nlet f =\n    g\n  1\n",
        [
            "BindingOffsideRBind.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideRBind.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideRNbind.fs",
        "module A\nmodule M =\n    let f =\n        g\n      1\n",
        [
            "BindingOffsideRNbind.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideRNbind.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideRDo.fs",
        "module A\ndo\n    ignore\n  1\n",
        [
            "BindingOffsideRDo.fs(4,3): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BindingOffsideRNdo.fs",
        "module A\nmodule M =\n    do\n        ignore\n      1\n",
        [
            "BindingOffsideRNdo.fs(5,7): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BindingOffsideIdent.fs",
        "module A\nlet f =\n    g\n  x\n",
        [
            "BindingOffsideIdent.fs(4,3): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideIdent.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsidePlus.fs", "module A\nlet f =\n    g\n  + 1\n", []
        "BindingOffsidePipe.fs", "module A\nlet f =\n    g\n  |> ignore\n", []
        "BindingOffsideString.fs",
        "module A\nlet f =\n    g\n  \"s\"\n",
        [
            "BindingOffsideString.fs(4,3): error FS0010: Unexpected string literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideString.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideMinus.fs", "module A\nlet f =\n    g\n  - 1\n", []
        "BindingOffsideAmpamp.fs", "module A\nlet f =\n    g\n  && true\n", []
        "BindingOffsideFloat.fs",
        "module A\nlet f =\n    g\n  1.0\n",
        [
            "BindingOffsideFloat.fs(4,3): error FS0010: Unexpected floating point literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideFloat.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideTwoLinesAfter.fs",
        "module A\nlet f =\n    g\n  1\n  2\n",
        [
            "BindingOffsideTwoLinesAfter.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideTwoLinesAfter.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideBlockAfter.fs",
        "module A\nlet f =\n    g\n  1\n    2\n",
        [
            "BindingOffsideBlockAfter.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideBlockAfter.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideLetRec.fs",
        "module A\nlet rec f =\n    g\n  1\n",
        [
            "BindingOffsideLetRec.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideLetRec.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideLetParams.fs",
        "module A\nlet f x =\n    g x\n  1\n",
        [
            "BindingOffsideLetParams.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideLetParams.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideAtLetPlus1.fs",
        "module A\nlet f =\n    g\n 1\n",
        [
            "BindingOffsideAtLetPlus1.fs(4,2): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideAtLetPlus1.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideSequentialThenOffside.fs",
        "module A\nlet f =\n    ignore 0\n    g\n  1\n",
        [
            "BindingOffsideSequentialThenOffside.fs(5,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideSequentialThenOffside.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideAttributeLet.fs",
        "module A\n[<Literal>]\nlet f =\n    1\n  2\n",
        [
            "BindingOffsideAttributeLet.fs(5,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideAttributeLet.fs(3,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideNamespace.fs",
        "namespace N\nmodule M =\n    let f =\n        g\n      1\n",
        [
            "BindingOffsideNamespace.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideNamespace.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
    ]

    let private bindingOffsideExplicitCases = [
        "BindingOffsideRBindlater.fs",
        "module A\nlet f =\n    g\n  1\nlet h = 2\n",
        [
            "BindingOffsideRBindlater.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideRBindlater.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BindingOffsideRBindlater.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "BindingOffsideKeywordIf.fs",
        "module A\nlet f =\n    g\n  if true then 1 else 2\n",
        [
            "BindingOffsideKeywordIf.fs(4,3): error FS0010: Unexpected keyword 'if' in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideKeywordIf.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BindingOffsideKeywordIf.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BindingOffsideParen.fs",
        "module A\nlet f =\n    g\n  (1)\n",
        [
            "BindingOffsideParen.fs(4,3): error FS0010: Unexpected symbol '(' in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideParen.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BindingOffsideParen.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BindingOffsideLongIdent.fs",
        "module A\nlet f =\n    g\n  List.empty\n",
        [
            "BindingOffsideLongIdent.fs(4,3): error FS0010: Unexpected identifier in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideLongIdent.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BindingOffsideLongIdent.fs(5,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BindingOffsideDeclAfterTwo.fs",
        "module A\nlet f =\n    g\n  1\nlet h = 2\nlet i = 3\n",
        [
            "BindingOffsideDeclAfterTwo.fs(4,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideDeclAfterTwo.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BindingOffsideDeclAfterTwo.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "BindingOffsideDoThenDecl.fs",
        "module A\ndo\n    ignore\n  1\nlet h = 2\n",
        [
            "BindingOffsideDoThenDecl.fs(4,3): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideDoThenDecl.fs(6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
        "BindingOffsideNestedThenDecl.fs",
        "module A\nmodule M =\n    let f =\n        g\n      1\n    let h = 2\n",
        [
            "BindingOffsideNestedThenDecl.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideNestedThenDecl.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "BindingOffsideNestedThenDecl.fs(7,1): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "BindingOffsideNestedThenRoot.fs",
        "module A\nmodule M =\n    let f =\n        g\n      1\nlet h = 2\n",
        [
            "BindingOffsideNestedThenRoot.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideNestedThenRoot.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideLocalLet.fs",
        "module A\nlet f () =\n    let y = g 1\n          2\n    y\n",
        [
            "BindingOffsideLocalLet.fs(4,11): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideLocalLet.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideLocalLetBlock.fs",
        "module A\nlet f () =\n    let y =\n        g\n      1\n    y\n",
        [
            "BindingOffsideLocalLetBlock.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideLocalLetBlock.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "BindingOffsideMember.fs",
        "module A\ntype T() =\n    member _.M =\n        g\n      1\n",
        [
            "BindingOffsideMember.fs(5,7): error FS0010: Unexpected integer literal in member definition"
        ]
        "BindingOffsideLetValueSameLine.fs",
        "module A\nlet f = g\n  1\n",
        [
            "BindingOffsideLetValueSameLine.fs(3,3): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "BindingOffsideLetValueSameLine.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
    ]

    let private undentedItemCloseCases = [
        "UndentedItemCloseRootletCloseAtValue.fs",
        "module A\nlet y = g (fun x ->\n         x) 2\n",
        [ "let y = [[g (fun x -> x)] 2]" ],
        [
            "app(2,9--3,14)"
            "app(2,9--3,12)"
        ]
        "UndentedItemCloseRootletCloseRightOfValue.fs",
        "module A\nlet y = g (fun x ->\n          x) 2\n",
        [ "let y = [[g (fun x -> x)] 2]" ],
        [
            "app(2,9--3,15)"
            "app(2,9--3,13)"
        ]
        "UndentedItemCloseRootletCloseLeftNoToken.fs",
        "module A\nlet y = g (fun x ->\n x)\n",
        [ "let y = [g (fun x -> x)]" ],
        [ "app(2,9--3,4)" ]
        "UndentedItemCloseRootletBodyAppCloseRight.fs",
        "module A\nlet y = g (fun x ->\n ignore x) 2\n",
        [ "let y = [[g (fun x -> [ignore x])] 2]" ],
        [
            "app(2,9--3,13)"
            "app(2,9--3,11)"
            "app(3,2--3,10)"
        ]
        "UndentedItemCloseClauseResultCloseRight.fs",
        "module A\nlet f v =\n    match v with\n    | _ -> g (fun x ->\n       ignore x) 2\n",
        [ "let f v = match v with | _ -> [[g (fun x -> [ignore x])] 2]" ],
        [
            "app(4,12--5,19)"
            "app(4,12--5,17)"
            "app(5,8--5,16)"
            "match(3,5--5,19)1"
        ]
        "UndentedItemCloseIfCondRightOfIf.fs",
        "module A\nlet f () =\n    if g (fun x ->\n      x) then 1 else 2\n",
        [ "let f () = if [g (fun x -> x)] then 1 else 2" ],
        [
            "app(3,8--4,9)"
            "if(3,5--4,23)E"
        ]
        "UndentedItemCloseIfCondRightOfIfBodyApp.fs",
        "module A\nlet f () =\n    if g (fun x ->\n      ignore x) then 1 else 2\n",
        [ "let f () = if [g (fun x -> [ignore x])] then 1 else 2" ],
        [
            "app(3,8--4,16)"
            "app(4,7--4,15)"
            "if(3,5--4,30)E"
        ]
        "UndentedItemClosePipeLineCloseRight.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  ignore x)\n    |> ignore\n",
        [ "let f xs = {{xs |> [List.map (fun x -> [ignore x])]} |> ignore}" ],
        [
            "|>(3,5--6,14)"
            "|>(3,5--5,12)"
            "app(4,8--5,12)"
            "app(5,3--5,11)"
        ]
        "UndentedItemClosePipeLineCloseAt.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n    x)\n    |> ignore\n",
        [ "let f xs = {{xs |> [List.map (fun x -> x)]} |> ignore}" ],
        [
            "|>(3,5--6,14)"
            "|>(3,5--5,7)"
            "app(4,8--5,7)"
        ]
        "UndentedItemCloseAssignValue.fs",
        "module A\nlet mutable z = 0\nlet f () =\n    z <- g (fun x ->\n      x) 2\n",
        [
            "let mutable z = 0"
            "let f () = {z <- [[g (fun x -> x)] 2]}"
        ],
        [
            "set(4,5--5,11)"
            "app(4,10--5,11)"
            "app(4,10--5,9)"
        ]
        "UndentedItemCloseInfixLineCloseRight.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n        x)\n  |> ignore\n",
        [ "let f xs = {{xs |> [List.map (fun x -> x)]} |> ignore}" ],
        [
            "|>(3,5--6,12)"
            "|>(3,5--5,11)"
            "app(4,8--5,11)"
        ]
        "UndentedItemCloseRootletThenDecl.fs",
        "module A\nlet y = g (fun x ->\n x)\nlet z = 1\n",
        [
            "let y = [g (fun x -> x)]"
            "let z = 1"
        ],
        [ "app(2,9--3,4)" ]
    ]

    let private undentedItemCloseExplicitCases = [
        "UndentedItemClosePipe3.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n    |> ignore\n",
        [
            "UndentedItemClosePipe3.fs(6,5): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemClosePipe3.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseRootlet.fs",
        "module A\nlet y = g (fun x ->\n x) 2\n",
        [
            "UndentedItemCloseRootlet.fs(3,5): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseRootlet.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseLocalLetValue.fs",
        "module A\nlet f () =\n    let y = g (fun x ->\n     x) 2\n    y\n",
        [
            "UndentedItemCloseLocalLetValue.fs(4,9): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseLocalLetValue.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseLocalLetValueCloseRight.fs",
        "module A\nlet f () =\n    let y = g (fun x ->\n     ignore x) 2\n    y\n",
        []
        "UndentedItemCloseClauseResult.fs",
        "module A\nlet f v =\n    match v with\n    | _ -> g (fun x ->\n       x) 2\n",
        [
            "UndentedItemCloseClauseResult.fs(5,11): error FS0010: Unexpected integer literal in expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedItemCloseThenSameLine.fs",
        "module A\nlet f c =\n    if c then g (fun x ->\n       x) 2\n    else 3\n",
        [
            "UndentedItemCloseThenSameLine.fs(4,11): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedItemCloseElseSameLine.fs",
        "module A\nlet f c =\n    if c then 1 else g (fun x ->\n       x) 2\n",
        [
            "UndentedItemCloseElseSameLine.fs(4,11): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedItemCloseMemberValue.fs",
        "module A\ntype T() =\n    member _.M = g (fun x ->\n       x) 2\n",
        [
            "UndentedItemCloseMemberValue.fs(4,11): error FS0010: Unexpected integer literal in member definition"
        ]
        "UndentedItemCloseDoValue.fs",
        "module A\ndo g (fun x ->\n x) 2\n",
        [
            "UndentedItemCloseDoValue.fs(3,5): error FS0010: Unexpected integer literal in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "UndentedItemCloseInfixLine.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n  |> ignore\n",
        [
            "UndentedItemCloseInfixLine.fs(6,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLine.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineColumnTwo.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n |> ignore\n",
        [
            "UndentedItemCloseInfixLineColumnTwo.fs(6,2): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineColumnTwo.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineRightOfClose.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n   |> ignore\n",
        [
            "UndentedItemCloseInfixLineRightOfClose.fs(6,4): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineRightOfClose.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineRightOfItem.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n     |> ignore\n",
        [
            "UndentedItemCloseInfixLineRightOfItem.fs(6,6): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineRightOfItem.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineTwice.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n  |> ignore\n    |> ignore\n",
        [
            "UndentedItemCloseInfixLineTwice.fs(6,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineTwice.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLinePlus.fs",
        "module A\nlet f x =\n    x\n    + g (fun y ->\n  y)\n  + 1\n",
        [
            "UndentedItemCloseInfixLinePlus.fs(6,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLinePlus.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineComma.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n  , 1\n",
        [
            "UndentedItemCloseInfixLineComma.fs(6,3): error FS0010: Unexpected symbol ',' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineComma.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineCons.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n  :: ys\n",
        [
            "UndentedItemCloseInfixLineCons.fs(6,3): error FS0010: Unexpected symbol '::' in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineCons.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineList.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map [\n  1]\n  |> ignore\n",
        [
            "UndentedItemCloseInfixLineList.fs(6,3): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineList.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineRootlet.fs",
        "module A\nlet y = g (fun x ->\n  x)\n |> h\n",
        [
            "UndentedItemCloseInfixLineRootlet.fs(4,2): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineRootlet.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "UndentedItemCloseInfixLineLocalLet.fs",
        "module A\nlet f xs =\n    let y =\n        xs\n        |> List.map (fun x ->\n      x)\n      |> ignore\n    y\n",
        [
            "UndentedItemCloseInfixLineLocalLet.fs(7,7): error FS0010: Unexpected infix operator in binding. Expected incomplete structured construct at or before this point or other token."
            "UndentedItemCloseInfixLineLocalLet.fs(3,5): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "UndentedItemCloseInfixLineLocalLet.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]
        "UndentedItemCloseInfixLineMember.fs",
        "module A\ntype T() =\n    member _.M xs =\n        xs\n        |> List.map (fun x ->\n      x)\n      |> ignore\n",
        [
            "UndentedItemCloseInfixLineMember.fs(7,7): error FS0010: Unexpected infix operator in member definition"
        ]
    ]

    let private prefixSignCases = [
        "PrefixSignLiteralArgument.fs",
        "module A\nlet r = f -1\n",
        [ "let r = [f -1]" ],
        [ "app(2,9--2,13)" ]
        "PrefixSignSpacedSubtraction.fs",
        "module A\nlet r = f - 1\n",
        [ "let r = {f - 1}" ],
        [ "-(2,9--2,14)" ]
        "PrefixSignAdjacentSubtraction.fs",
        "module A\nlet r = f-1\n",
        [ "let r = {f - 1}" ],
        [ "-(2,9--2,12)" ]
        "PrefixSignPlusLiteralArgument.fs",
        "module A\nlet r = f +1\n",
        [ "let r = [f +1]" ],
        [ "app(2,9--2,13)" ]
        "PrefixSignIdentifierArgument.fs",
        "module A\nlet r = f -x\n",
        [ "let r = [f ~-x]" ],
        [
            "~-(2,11--2,13)"
            "app(2,9--2,13)"
        ]
        "PrefixSignPlusIdentifierArgument.fs",
        "module A\nlet r = f +x\n",
        [ "let r = [f ~+x]" ],
        [
            "~+(2,11--2,13)"
            "app(2,9--2,13)"
        ]
        "PrefixSignArgumentTakesOneAtom.fs",
        "module A\nlet r = f -g x\n",
        [ "let r = [[f ~-g] x]" ],
        [
            "~-(2,11--2,13)"
            "app(2,9--2,15)"
            "app(2,9--2,13)"
        ]
        "PrefixSignParenthesizedArgument.fs",
        "module A\nlet r = f -(x)\n",
        [ "let r = [f ~-(x)]" ],
        [
            "~-(2,11--2,15)"
            "app(2,9--2,15)"
        ]
        "PrefixSignUnsignedLiteralArgument.fs",
        "module A\nlet r = f -1u\n",
        [ "let r = [f ~-1u]" ],
        [
            "~-(2,11--2,14)"
            "app(2,9--2,14)"
        ]
        "PrefixSignFloatArgument.fs",
        "module A\nlet r = f -1.5\n",
        [ "let r = [f -1.5]" ],
        [ "app(2,9--2,15)" ]
        "PrefixSignHexArgument.fs",
        "module A\nlet r = f -0x10\n",
        [ "let r = [f -0x10]" ],
        [ "app(2,9--2,16)" ]
        "PrefixSignInt32MinimumArgument.fs",
        "module A\nlet r = f -2147483648\n",
        [ "let r = [f -2147483648]" ],
        [ "app(2,9--2,22)" ]
        "PrefixSignTwoArguments.fs",
        "module A\nlet r = f -1 -2\n",
        [ "let r = [[f -1] -2]" ],
        [
            "app(2,9--2,16)"
            "app(2,9--2,13)"
        ]
        "PrefixSignArgumentThenAdjacentMinus.fs",
        "module A\nlet r = f -1-2\n",
        [ "let r = {[f -1] - 2}" ],
        [
            "-(2,9--2,15)"
            "app(2,9--2,13)"
        ]
        "PrefixSignLiteralHead.fs", "module A\nlet r = -1\n", [ "let r = -1" ], []
        "PrefixSignPlusLiteralHead.fs", "module A\nlet r = +1\n", [ "let r = +1" ], []
        "PrefixSignIdentifierHead.fs",
        "module A\nlet r = -x\n",
        [ "let r = ~-x" ],
        [ "~-(2,9--2,11)" ]
        "PrefixSignSpacedHead.fs", "module A\nlet r = - x\n", [ "let r = ~-x" ], [ "~-(2,9--2,12)" ]
        "PrefixSignSpacedLiteralHead.fs",
        "module A\nlet r = - 1\n",
        [ "let r = ~-1" ],
        [ "~-(2,9--2,12)" ]
        "PrefixSignHeadApplication.fs",
        "module A\nlet r = -f x\n",
        [ "let r = ~-[f x]" ],
        [
            "~-(2,9--2,13)"
            "app(2,10--2,13)"
        ]
        "PrefixSignHeadTwice.fs",
        "module A\nlet r = - -x\n",
        [ "let r = ~-~-x" ],
        [
            "~-(2,9--2,13)"
            "~-(2,11--2,13)"
        ]
        "PrefixSignLiteralHeadApplication.fs",
        "module A\nlet r = -1 x\n",
        [ "let r = [-1 x]" ],
        [ "app(2,9--2,13)" ]
        "PrefixSignAfterParenthesized.fs",
        "module A\nlet r = (f)-1\n",
        [ "let r = {(f) - 1}" ],
        [ "-(2,9--2,14)" ]
        "PrefixSignAfterSpacedParenthesized.fs",
        "module A\nlet r = (f) -1\n",
        [ "let r = [(f) -1]" ],
        [ "app(2,9--2,15)" ]
        "PrefixSignAfterConstant.fs",
        "module A\nlet r = 1 -1\n",
        [ "let r = [1 -1]" ],
        [ "app(2,9--2,13)" ]
        "PrefixSignBeforeInfix.fs",
        "module A\nlet r = f -1 + 2\n",
        [ "let r = {[f -1] + 2}" ],
        [
            "+(2,9--2,17)"
            "app(2,9--2,13)"
        ]
        "PrefixSignHeadBeforeInfix.fs",
        "module A\nlet r = -x * 2\n",
        [ "let r = {~-x * 2}" ],
        [
            "*(2,9--2,15)"
            "~-(2,9--2,11)"
        ]
        "PrefixSignAfterInfix.fs",
        "module A\nlet r = a - -x\n",
        [ "let r = {a - ~-x}" ],
        [
            "-(2,9--2,15)"
            "~-(2,13--2,15)"
        ]
        "PrefixSignInParentheses.fs",
        "module A\nlet r = (f -1)\n",
        [ "let r = ([f -1])" ],
        [ "app(2,10--2,14)" ]
        "PrefixSignInList.fs",
        "module A\nlet r = [-1; -x]\n",
        [ "let r = [-1; ~-x]" ],
        [ "~-(2,14--2,16)" ]
        "PrefixSignInRecord.fs", "module A\nlet r = { X = -1 }\n", [ "let r = {X = -1}" ], []
        "PrefixSignInTuple.fs",
        "module A\nlet r = -1, -x\n",
        [ "let r = -1, ~-x" ],
        [
            "tuple(2,9--2,15)"
            "~-(2,13--2,15)"
        ]
        "PrefixSignConditionBranch.fs",
        "module A\nlet r c = if -c then -1 else 1\n",
        [ "let r c = if ~-c then -1 else 1" ],
        [ "~-(2,14--2,16)" ]
        "PrefixSignLambdaBody.fs",
        "module A\nlet r = fun x -> -x\n",
        [ "let r = fun x -> ~-x" ],
        [ "~-(2,18--2,20)" ]
        "PrefixSignNextLineArgument.fs",
        "module A\nlet r =\n    f\n        -1\n",
        [ "let r = [f -1]" ],
        [ "app(3,5--4,11)" ]
        "PrefixSignNextLineSubtraction.fs",
        "module A\nlet r =\n    f\n    - 1\n",
        [ "let r = {f - 1}" ],
        [ "-(3,5--4,8)" ]
        "PrefixSignNextLineItem.fs",
        "module A\nlet r =\n    f ()\n    -x\n",
        [ "let r = seq[[f ()]; ~-x]" ],
        [
            "seq(3,5--4,7)"
            "~-(4,5--4,7)"
            "app(3,5--3,9)"
        ]
        "PrefixSignNextLineItemInParentheses.fs",
        "module A\nlet r = (f\n         -1)\n",
        [ "let r = (seq[f; -1])" ],
        [ "seq(2,10--3,12)" ]
        "PrefixSignNextLineListItem.fs",
        "module A\nlet r = [\n    1\n    -1\n]\n",
        [ "let r = [1; -1]" ],
        []
        "PrefixSignEndOfLineSubtraction.fs",
        "module A\nlet r =\n    f -\n        1\n",
        [ "let r = {f - 1}" ],
        [ "-(3,5--4,10)" ]
        "PrefixSignLocalLetBody.fs",
        "module A\nlet r () =\n    let y = 1\n    - 1\n",
        [ "let r () = let y = 1 in ~-1" ],
        [
            "let(3,5--4,8)"
            "~-(4,5--4,8)"
        ]
        "PrefixSignAfterNestedBlock.fs",
        "module A\nlet r c =\n    if c then\n        1\n    else\n        2\n    -1\n",
        [ "let r c = seq[if c then 1 else 2; -1]" ],
        [ "seq(3,5--7,7)" ]
        "PrefixSignSubtractionAfterNestedBlock.fs",
        "module A\nlet r c =\n    if c then\n        1\n    else\n        2\n    - 1\n",
        [ "let r c = {if c then 1 else 2 - 1}" ],
        [ "-(3,5--7,8)" ]
        "PrefixSignModuleExpression.fs",
        "module A\nlet a = 1\n-1\n",
        [
            "let a = 1"
            "expr -1"
        ],
        []
        "InfixLineParenMinusAdjacentIdent.fs",
        "module A\nlet f a x =\n    (a\n     -x)\n",
        [ "let f a x = (seq[a; ~-x])" ],
        [
            "seq(3,6--4,8)"
            "~-(4,6--4,8)"
        ]
        "InfixAtColumnNestedBindingBlock.fs",
        "module A\nlet f a b =\n    let y =\n        a\n    + b\n    y\n",
        [ "let f a b = let y = a in seq[~+b; y]" ],
        [
            "let(3,5--6,6)"
            "seq(5,5--6,6)"
            "~+(5,5--5,8)"
        ]
        "InfixLineLocalLetSameLineValue.fs",
        "module A\nlet f a b =\n    let y = a\n    + b\n    y\n",
        [ "let f a b = let y = a in seq[~+b; y]" ],
        [
            "let(3,5--5,6)"
            "seq(4,5--5,6)"
            "~+(4,5--4,8)"
        ]
        "InfixLineMinusIdent.fs",
        "module A\nlet f a x =\n    a\n    -x\n",
        [ "let f a x = seq[a; ~-x]" ],
        [
            "seq(3,5--4,7)"
            "~-(4,5--4,7)"
        ]
        "InfixLinePlusAdjacentIdent.fs",
        "module A\nlet f a x =\n    a\n    +x\n",
        [ "let f a x = seq[a; ~+x]" ],
        [
            "seq(3,5--4,7)"
            "~+(4,5--4,7)"
        ]
        "InfixLineParenMinusAdjacentLit.fs",
        "module A\nlet f a =\n    (a\n     -1)\n",
        [ "let f a = (seq[a; -1])" ],
        [ "seq(3,6--4,8)" ]
        "InfixAtColumnMinusAdjacent.fs",
        "module A\nlet f a =\n    a\n    -1\n",
        [ "let f a = seq[a; -1]" ],
        [ "seq(3,5--4,7)" ]
        "ConditionalBodyIfPlus1.fs",
        "module A\nlet f x =\n    if x > 0\n       then\n     x\n       else\n     -x\n",
        [ "let f x = if {x > 0} then x else ~-x" ],
        [
            ">(3,8--3,13)"
            "~-(7,6--7,8)"
        ]
        "PrefixMinus.fs",
        "module Program\nlet y = f -1\n",
        [ "let y = [f -1]" ],
        [ "app(2,9--2,13)" ]
        "PrefixSignLineAtLocalBodyColumn.fs",
        "module A\nlet f a =\n    let g =\n        a\n    -1\n    g\n",
        [ "let f a = let g = a in seq[-1; g]" ],
        [
            "let(3,5--6,6)"
            "seq(5,5--6,6)"
        ]
        "SpacedSignLineAtLocalBodyColumn.fs",
        "module A\nlet f a =\n    let g =\n        a\n    + 1\n    g\n",
        [ "let f a = let g = a in seq[~+1; g]" ],
        [
            "let(3,5--6,6)"
            "seq(5,5--6,6)"
            "~+(5,5--5,8)"
        ]
        "SignedHexFloatLiteral.fs",
        "module A\nlet y = f -0x1LF\n",
        [ "let y = [f -0x1LF]" ],
        [ "app(2,9--2,17)" ]
        "SignedUnsignedLongLiteral.fs",
        "module A\nlet y = f -1uL\n",
        [ "let y = [f ~-1uL]" ],
        [
            "~-(2,11--2,15)"
            "app(2,9--2,15)"
        ]
        "SignedLeadingZeroLiteral.fs",
        "module A\nlet y = f -08\n",
        [ "let y = [f -08]" ],
        [ "app(2,9--2,14)" ]
        "SignedDecimalLiteral.fs",
        "module A\nlet y = f -1.5M\n",
        [ "let y = [f -1.5M]" ],
        [ "app(2,9--2,16)" ]
        "SignedBignumLiteral.fs",
        "module A\nlet y = f -1Z\n",
        [ "let y = [f -1Z]" ],
        [ "app(2,9--2,14)" ]
        "SignedDecimalExponentLiteral.fs",
        "module A\nlet y = f -1e5m\n",
        [ "let y = [f -1e5m]" ],
        [ "app(2,9--2,16)" ]
        "SignedHexWithLetterE.fs",
        "module A\nlet y = f -0x1e3\n",
        [ "let y = [f -0x1e3]" ],
        [ "app(2,9--2,17)" ]
        "SignedUnsignedByteInRange.fs",
        "module A\nlet y = f -255uy\n",
        [ "let y = [f ~-255uy]" ],
        [
            "~-(2,11--2,17)"
            "app(2,9--2,17)"
        ]
    ]

    let private prefixOperatorExplicitCases = [
        "PrefixAmpersandArgument.fs", "module A\nlet r = a &b\n", []
        "PrefixDoubleAmpersandArgument.fs", "module A\nlet r = a &&b\n", []
        "PrefixPercentArgument.fs", "module A\nlet r = a %b\n", []
        "PrefixMinusDotArgument.fs", "module A\nlet r = f -.1\n", []
        "PrefixPlusDotArgument.fs", "module A\nlet r = f +.1\n", []
        "PrefixSignConditionalOperand.fs",
        "module A\nlet r c = f -if c then 1 else 2\n",
        [
            "PrefixSignConditionalOperand.fs(2,14): error FS0010: Unexpected keyword 'if' in expression"
        ]
        "PrefixSignHeadConditionalOperand.fs",
        "module A\nlet r c = -if c then 1 else 2\n",
        [
            "PrefixSignHeadConditionalOperand.fs(2,12): error FS0010: Unexpected keyword 'if' in expression"
        ]
        "PrefixSignModuleSubtractionLine.fs", "module A\nlet a = 1\n- 1\n", []
        "PrefixSignUndentedInParentheses.fs", "module A\nlet r = (f\n        -1)\n", []
        "SpacedSignLineLeftOfLocalValue.fs",
        "module A\nlet f a =\n    let g =\n        a\n   + 1\n    g\n",
        []
        "SpacedSignLineLeftOfLocalValueAlone.fs",
        "module A\nlet f a =\n    let g =\n        a\n   + 1\n",
        []
        "SpacedSignLineLeftOfNestedLocalValue.fs",
        "module A\nlet f a =\n    let g =\n        let h = a\n       + 1\n        h\n    g\n",
        []
        "PrefixSignLineLeftOfLocalValue.fs",
        "module A\nlet f a =\n    let g =\n        a\n   -1\n    g\n",
        [
            "PrefixSignLineLeftOfLocalValue.fs(3,5): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
            "PrefixSignLineLeftOfLocalValue.fs(5,4): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
            "PrefixSignLineLeftOfLocalValue.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "SpacedSignLineLeftOfBlockThenItem.fs", "module A\nlet f a =\n    a\n   + 1\n    g\n", []
    ]

    let private prefixArgumentDiagnosticCases = [
        "PrefixSignHighPrecedenceArgument.fs",
        "module A\nlet r = f -g(x)\n",
        [
            "PrefixSignHighPrecedenceArgument.fs(2,11): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
        ]
        "DereferenceHighPrecedenceArgument.fs",
        "module A\nlet r = f !g(x)\n",
        [
            "DereferenceHighPrecedenceArgument.fs(2,11): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
        ]
    ]

    let private dotOperatorCases = [
        "DotOperatorBelowAddition.fs",
        "module A\nlet r = a .>> b + c\n",
        [ "let r = {a .>> {b + c}}" ],
        [
            ".>>(2,9--2,20)"
            "+(2,15--2,20)"
        ]
        "DotOperatorLeftAssociative.fs",
        "module A\nlet r = a .>> b .>> c\n",
        [ "let r = {{a .>> b} .>> c}" ],
        [
            ".>>(2,9--2,22)"
            ".>>(2,9--2,16)"
        ]
        "DotOperatorWithTrailingDot.fs",
        "module A\nlet r = a .>> b >>. c\n",
        [ "let r = {{a .>> b} >>. c}" ],
        [
            ">>.(2,9--2,22)"
            ".>>(2,9--2,16)"
        ]
        "DotMultiplyAboveAddition.fs",
        "module A\nlet r = a .* b + c\n",
        [ "let r = {{a .* b} + c}" ],
        [
            "+(2,9--2,19)"
            ".*(2,9--2,15)"
        ]
        "DotPlusBelowMultiply.fs",
        "module A\nlet r = a .+ b * c\n",
        [ "let r = {a .+ {b * c}}" ],
        [
            ".+(2,9--2,19)"
            "*(2,14--2,19)"
        ]
        "DotPowerRightAssociative.fs",
        "module A\nlet r = a .** b .** c\n",
        [ "let r = {a .** {b .** c}}" ],
        [
            ".**(2,9--2,22)"
            ".**(2,15--2,22)"
        ]
        "DotAtRightAssociative.fs",
        "module A\nlet r = a .@ b .@ c\n",
        [ "let r = {a .@ {b .@ c}}" ],
        [
            ".@(2,9--2,20)"
            ".@(2,14--2,20)"
        ]
        "DotBarBarIsComparison.fs",
        "module A\nlet r = a .|| b = c\n",
        [ "let r = {{a .|| b} = c}" ],
        [
            "=(2,9--2,20)"
            ".||(2,9--2,16)"
        ]
        "DotAmpAmpIsComparison.fs",
        "module A\nlet r = a .&& b = c\n",
        [ "let r = {{a .&& b} = c}" ],
        [
            "=(2,9--2,20)"
            ".&&(2,9--2,16)"
        ]
        "DotAmpIsComparison.fs",
        "module A\nlet r = a .& b = c\n",
        [ "let r = {{a .& b} = c}" ],
        [
            "=(2,9--2,19)"
            ".&(2,9--2,15)"
        ]
        "DotBangEquals.fs",
        "module A\nlet r = a .!= b + c\n",
        [ "let r = {a .!= {b + c}}" ],
        [
            ".!=(2,9--2,20)"
            "+(2,15--2,20)"
        ]
        "TwoDotsIgnored.fs",
        "module A\nlet r = a ..> b + c\n",
        [ "let r = {a ..> {b + c}}" ],
        [
            "..>(2,9--2,20)"
            "+(2,15--2,20)"
        ]
        "DotOperatorAdjacent.fs",
        "module A\nlet r = a.>>b\n",
        [ "let r = {a .>> b}" ],
        [ ".>>(2,9--2,14)" ]
        "DotMinusAdjacentIsInfix.fs",
        "module A\nlet r = a .-b\n",
        [ "let r = {a .- b}" ],
        [ ".-(2,9--2,14)" ]
        "DotOperatorAfterMemberAccess.fs",
        "module A\nlet r = a.b .>> c\n",
        [ "let r = {a.b .>> c}" ],
        [ ".>>(2,9--2,18)" ]
        "DotOperatorInTuple.fs",
        "module A\nlet r = a .>> b, c\n",
        [ "let r = {a .>> b}, c" ],
        [
            "tuple(2,9--2,19)"
            ".>>(2,9--2,16)"
        ]
        "DotOperatorNextLine.fs",
        "module A\nlet r =\n    a\n    .>> b\n",
        [ "let r = {a .>> b}" ],
        [ ".>>(3,5--4,10)" ]
        "DotOperatorNextLineUndented.fs",
        "module A\nlet r =\n    a\n  .>> b\n",
        [ "let r = {a .>> b}" ],
        [ ".>>(3,5--4,8)" ]
        "DotOperatorNextLineInParentheses.fs",
        "module A\nlet r = (a\n        .>> b)\n",
        [ "let r = ({a .>> b})" ],
        [ ".>>(2,10--3,14)" ]
        "DotOperatorEndOfLine.fs",
        "module A\nlet r =\n    a .>>\n        b\n",
        [ "let r = {a .>> b}" ],
        [ ".>>(3,5--4,10)" ]
        "DotOperatorAfterNestedBlock.fs",
        "module A\nlet r c =\n    if c then\n        a\n    else\n        b\n    .>> d\n",
        [ "let r c = if c then a else {b .>> d}" ],
        [ ".>>(6,9--7,10)" ]
    ]

    let private dotOperatorExplicitCases = [
        "DotDollarOperator.fs",
        "module A\nlet r = a .$ b + c\n",
        [
            "DotDollarOperator.fs(2,11): error FS0035: This construct is deprecated: '$' is not permitted as a character in operator names and is reserved for future use"
        ]
        "QuestionMarkOperator.fs", "module A\nlet r = a ?>> b + c\n", []
        "ThreeDots.fs",
        "module A\nlet r = a ... b\n",
        [
            "ThreeDots.fs(2,13): error FS0010: Unexpected symbol '.' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "DotArrowOperator.fs", "module A\nlet r = a .-> b\n", []
        "DotColonColon.fs",
        "module A\nlet r = a .:: b\n",
        [ "DotColonColon.fs(2,11): error FS0599: Missing qualification after '.'" ]
        "DotOperatorModuleLine.fs",
        "module A\nlet a = 1\n.>> b\n",
        [
            "DotOperatorModuleLine.fs(3,1): error FS0010: Unexpected infix operator in definition. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private assignmentOperatorCases = [
        "AssignTupleValue.fs",
        "module A\nlet r () = x := 1, 2\n",
        [ "let r () = {x := 1, 2}" ],
        [
            ":=(2,12--2,21)"
            "tuple(2,17--2,21)"
        ]
        "AssignBelowOr.fs",
        "module A\nlet r () = x := a || b\n",
        [ "let r () = {x := {a || b}}" ],
        [
            ":=(2,12--2,23)"
            "||(2,17--2,23)"
        ]
        "AssignRightAssociative.fs",
        "module A\nlet r () = x := y := 1\n",
        [ "let r () = {x := {y := 1}}" ],
        [
            ":=(2,12--2,23)"
            ":=(2,17--2,23)"
        ]
        "AssignTupleTarget.fs",
        "module A\nlet r () = a, x := 1\n",
        [ "let r () = {a, x := 1}" ],
        [
            ":=(2,12--2,21)"
            "tuple(2,12--2,16)"
        ]
        "AssignApplicationTarget.fs",
        "module A\nlet r () = f x := 1\n",
        [ "let r () = {[f x] := 1}" ],
        [
            ":=(2,12--2,20)"
            "app(2,12--2,15)"
        ]
        "AssignConditionalValue.fs",
        "module A\nlet r c = x := if c then 1 else 2\n",
        [ "let r c = {x := if c then 1 else 2}" ],
        [ ":=(2,11--2,34)" ]
        "AssignLambdaValue.fs",
        "module A\nlet r () = x := fun y -> y\n",
        [ "let r () = {x := fun y -> y}" ],
        [ ":=(2,12--2,27)" ]
        "AssignInSetValue.fs",
        "module A\nlet mutable z = 0\nlet r () = z <- y := 1\n",
        [
            "let mutable z = 0"
            "let r () = {z <- {y := 1}}"
        ],
        [
            "set(3,12--3,23)"
            ":=(3,17--3,23)"
        ]
        "AssignInParentheses.fs",
        "module A\nlet r () = (x := 1, 2)\n",
        [ "let r () = ({x := 1, 2})" ],
        [
            ":=(2,13--2,22)"
            "tuple(2,18--2,22)"
        ]
        "AssignAdjacent.fs",
        "module A\nlet r () = x:=1\n",
        [ "let r () = {x := 1}" ],
        [ ":=(2,12--2,16)" ]
        "AssignModuleExpression.fs", "module A\nx := 1\n", [ "expr {x := 1}" ], [ ":=(2,1--2,7)" ]
        "AssignSequentialItem.fs",
        "module A\nlet r () =\n    x := 1\n    y\n",
        [ "let r () = seq[{x := 1}; y]" ],
        [
            "seq(3,5--4,6)"
            ":=(3,5--3,11)"
        ]
        "AssignValueNextLine.fs",
        "module A\nlet r () =\n    x :=\n        1\n",
        [ "let r () = {x := 1}" ],
        [ ":=(3,5--4,10)" ]
        "AssignOperatorLine.fs",
        "module A\nlet r () =\n    x\n    := 1\n",
        [ "let r () = {x := 1}" ],
        [ ":=(3,5--4,9)" ]
        "AssignOperatorLineUndented.fs",
        "module A\nlet r () =\n    x\n  := 1\n",
        [ "let r () = {x := 1}" ],
        [ ":=(3,5--4,7)" ]
        "AssignCommaLine.fs",
        "module A\nlet r () =\n    x := 1\n    , 2\n",
        [ "let r () = {x := 1, 2}" ],
        [
            ":=(3,5--4,8)"
            "tuple(3,10--4,8)"
        ]
        "AssignInfixLine.fs",
        "module A\nlet r () =\n    x := 1\n    + 2\n",
        [ "let r () = {x := {1 + 2}}" ],
        [
            ":=(3,5--4,8)"
            "+(3,10--4,8)"
        ]
        "AssignOperatorLineTwice.fs",
        "module A\nlet r () =\n    x := y\n    := 1\n",
        [ "let r () = {x := {y := 1}}" ],
        [
            ":=(3,5--4,9)"
            ":=(3,10--4,9)"
        ]
        "AssignOperatorLineAfterSet.fs",
        "module A\nlet mutable z = 0\nlet r () =\n    z <- y\n    := 1\n",
        [
            "let mutable z = 0"
            "let r () = {z <- {y := 1}}"
        ],
        [
            "set(4,5--5,9)"
            ":=(4,10--5,9)"
        ]
        "AssignOperatorLineAfterSetBlock.fs",
        "module A\nlet mutable z = 0\nlet r () =\n    z <-\n        y\n    := 1\n",
        [
            "let mutable z = 0"
            "let r () = {{z <- y} := 1}"
        ],
        [
            ":=(4,5--6,9)"
            "set(4,5--5,10)"
        ]
        "AssignOperatorLineAfterLambda.fs",
        "module A\nlet r =\n    fun () -> x\n    := 1\n",
        [ "let r = {fun () -> x := 1}" ],
        [ ":=(3,5--4,9)" ]
        "InfixAtColumnColonEquals.fs",
        "module A\nlet f a b =\n    a\n    := b\n",
        [ "let f a b = {a := b}" ],
        [ ":=(3,5--4,9)" ]
    ]

    let private assignmentOperatorExplicitCases = [
        "AssignLineAfterUndentedClose.fs",
        "module A\nlet f xs =\n    xs\n    |> List.map (fun x ->\n  x)\n  := 1\n",
        [
            "AssignLineAfterUndentedClose.fs(6,3): error FS0010: Unexpected symbol ':=' in binding. Expected incomplete structured construct at or before this point or other token."
            "AssignLineAfterUndentedClose.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "AssignLineAtModuleColumn.fs",
        "module A\nlet r () =\n    x\n:= 1\n",
        [
            "AssignLineAtModuleColumn.fs(4,1): error FS0010: Unexpected symbol ':=' in definition. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private orKeywordCases = [
        "OrKeywordBelowAnd.fs",
        "module A\nlet r = a or b && c\n",
        [ "let r = {a or {b && c}}" ],
        [
            "or(2,9--2,20)"
            "&&(2,14--2,20)"
        ]
        "OrKeywordLeftAssociative.fs",
        "module A\nlet r = a or b or c\n",
        [ "let r = {{a or b} or c}" ],
        [
            "or(2,9--2,20)"
            "or(2,9--2,15)"
        ]
        "OrKeywordAfterBarBar.fs",
        "module A\nlet r = a || b or c\n",
        [ "let r = {{a || b} or c}" ],
        [
            "or(2,9--2,20)"
            "||(2,9--2,15)"
        ]
        "OrKeywordBeforeBarBar.fs",
        "module A\nlet r = a or b || c\n",
        [ "let r = {{a or b} || c}" ],
        [
            "||(2,9--2,20)"
            "or(2,9--2,15)"
        ]
        "OrKeywordBelowComparison.fs",
        "module A\nlet r = a or b = c\n",
        [ "let r = {a or {b = c}}" ],
        [
            "or(2,9--2,19)"
            "=(2,14--2,19)"
        ]
        "OrKeywordBelowAmpersand.fs",
        "module A\nlet r = a & b or c\n",
        [ "let r = {{a & b} or c}" ],
        [
            "or(2,9--2,19)"
            "&(2,9--2,14)"
        ]
        "OrKeywordApplications.fs",
        "module A\nlet r = f a or g b\n",
        [ "let r = {[f a] or [g b]}" ],
        [
            "or(2,9--2,19)"
            "app(2,9--2,12)"
            "app(2,16--2,19)"
        ]
        "OrKeywordInTuple.fs",
        "module A\nlet r = a, b or c\n",
        [ "let r = a, {b or c}" ],
        [
            "tuple(2,9--2,18)"
            "or(2,12--2,18)"
        ]
        "OrKeywordInAssignment.fs",
        "module A\nlet r () = x := a or b\n",
        [ "let r () = {x := {a or b}}" ],
        [
            ":=(2,12--2,23)"
            "or(2,17--2,23)"
        ]
        "OrKeywordInCondition.fs",
        "module A\nlet r = if a or b then 1 else 2\n",
        [ "let r = if {a or b} then 1 else 2" ],
        [ "or(2,12--2,18)" ]
        "OrKeywordAdjacentParentheses.fs",
        "module A\nlet r = (a)or(b)\n",
        [ "let r = {(a) or (b)}" ],
        [ "or(2,9--2,17)" ]
        "OrKeywordLine.fs",
        "module A\nlet r =\n    a\n    or b\n",
        [ "let r = {a or b}" ],
        [ "or(3,5--4,9)" ]
        "OrKeywordLineUndented.fs",
        "module A\nlet r =\n    a\n  or b\n",
        [ "let r = {a or b}" ],
        [ "or(3,5--4,7)" ]
        "OrKeywordEndOfLine.fs",
        "module A\nlet r =\n    a or\n        b\n",
        [ "let r = {a or b}" ],
        [ "or(3,5--4,10)" ]
        "OrKeywordLineInParentheses.fs",
        "module A\nlet r = (a\n        or b)\n",
        [ "let r = ({a or b})" ],
        [ "or(2,10--3,13)" ]
        "OrKeywordThenInfixLine.fs",
        "module A\nlet r =\n    a or b\n    |> f\n",
        [ "let r = {a or {b |> f}}" ],
        [
            "or(3,5--4,9)"
            "|>(3,10--4,9)"
        ]
        "OrKeywordAfterNestedBlock.fs",
        "module A\nlet r c =\n    if c then\n        a\n    else\n        b\n    or d\n",
        [ "let r c = {if c then a else b or d}" ],
        [ "or(3,5--7,9)" ]
        "BarBarLineUndentedTwoColumns.fs",
        "module A\nlet r =\n    a\n || b\n",
        [ "let r = {a || b}" ],
        [ "||(3,5--4,6)" ]
    ]

    let private orKeywordExplicitCases = [
        "OrKeywordLineUndentedBeyondLimit.fs",
        "module A\nlet r =\n    a\n or b\n",
        [
            "OrKeywordLineUndentedBeyondLimit.fs(4,2): error FS0010: Unexpected keyword 'or' in binding. Expected incomplete structured construct at or before this point or other token."
            "OrKeywordLineUndentedBeyondLimit.fs(2,1): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
        ]
        "OrKeywordModuleLine.fs",
        "module A\nlet a = 1\nor b\n",
        [
            "OrKeywordModuleLine.fs(3,1): error FS0010: Unexpected keyword 'or' in definition. Expected incomplete structured construct at or before this point or other token."
        ]
        "OrKeywordEndOfLineAtBlockColumn.fs", "module A\nlet r =\n    a or\n    b\n", []
    ]

    let private dereferenceCases = [
        "DereferenceArgument.fs",
        "module A\nlet r = f !x\n",
        [ "let r = [f ~!x]" ],
        [
            "~!(2,11--2,13)"
            "app(2,9--2,13)"
        ]
        "DereferenceSpacedArgument.fs",
        "module A\nlet r = f ! x\n",
        [ "let r = [f ~!x]" ],
        [
            "~!(2,11--2,14)"
            "app(2,9--2,14)"
        ]
        "DereferenceHead.fs", "module A\nlet r = !x\n", [ "let r = ~!x" ], [ "~!(2,9--2,11)" ]
        "DereferenceHeadTakesOneAtom.fs",
        "module A\nlet r = !f x\n",
        [ "let r = [~!f x]" ],
        [
            "~!(2,9--2,11)"
            "app(2,9--2,13)"
        ]
        "DereferenceHighPrecedenceApplication.fs",
        "module A\nlet r = !f(x)\n",
        [ "let r = ~![f (x)]" ],
        [
            "~!(2,9--2,14)"
            "app(2,10--2,14)"
        ]
        "DereferenceIndexer.fs",
        "module A\nlet r = !x[0]\n",
        [ "let r = ~!x[0]" ],
        [ "~!(2,9--2,14)" ]
        "DereferenceLongIdentifier.fs",
        "module A\nlet r = f !x.y\n",
        [ "let r = [f ~!x.y]" ],
        [
            "~!(2,11--2,15)"
            "app(2,9--2,15)"
        ]
        "DereferenceBeforeInfix.fs",
        "module A\nlet r = !x + 1\n",
        [ "let r = {~!x + 1}" ],
        [
            "+(2,9--2,15)"
            "~!(2,9--2,11)"
        ]
        "DereferenceTwoArguments.fs",
        "module A\nlet r = f !x y\n",
        [ "let r = [[f ~!x] y]" ],
        [
            "~!(2,11--2,13)"
            "app(2,9--2,15)"
            "app(2,9--2,13)"
        ]
        "DereferenceParenthesized.fs",
        "module A\nlet r = !(f x)\n",
        [ "let r = ~!([f x])" ],
        [
            "~!(2,9--2,15)"
            "app(2,11--2,14)"
        ]
        "DereferenceUnderSign.fs",
        "module A\nlet r = - !x\n",
        [ "let r = ~-~!x" ],
        [
            "~-(2,9--2,13)"
            "~!(2,11--2,13)"
        ]
        "DereferenceConstant.fs", "module A\nlet r = !1\n", [ "let r = ~!1" ], [ "~!(2,9--2,11)" ]
        "DereferenceAfterParentheses.fs",
        "module A\nlet r = (f)!x\n",
        [ "let r = [(f) ~!x]" ],
        [
            "~!(2,12--2,14)"
            "app(2,9--2,14)"
        ]
        "DereferenceAfterConstant.fs",
        "module A\nlet r = f 1!x\n",
        [ "let r = [[f 1] ~!x]" ],
        [
            "~!(2,12--2,14)"
            "app(2,9--2,14)"
            "app(2,9--2,12)"
        ]
        "DereferenceInList.fs",
        "module A\nlet r = [!x; !y]\n",
        [ "let r = [~!x; ~!y]" ],
        [
            "~!(2,10--2,12)"
            "~!(2,14--2,16)"
        ]
        "DereferenceInCondition.fs",
        "module A\nlet r = if !x then 1 else 2\n",
        [ "let r = if ~!x then 1 else 2" ],
        [ "~!(2,12--2,14)" ]
        "DereferenceModuleExpression.fs", "module A\n!x\n", [ "expr ~!x" ], [ "~!(2,1--2,3)" ]
        "DereferenceNextLineItem.fs",
        "module A\nlet r =\n    f ()\n    !x\n",
        [ "let r = seq[[f ()]; ~!x]" ],
        [
            "seq(3,5--4,7)"
            "~!(4,5--4,7)"
            "app(3,5--3,9)"
        ]
        "DereferenceNextLineArgument.fs",
        "module A\nlet r =\n    f\n        !x\n",
        [ "let r = [f ~!x]" ],
        [
            "~!(4,9--4,11)"
            "app(3,5--4,11)"
        ]
        "BangEqualsStaysInfix.fs",
        "module A\nlet r = a !=b\n",
        [ "let r = {a != b}" ],
        [ "!=(2,9--2,14)" ]
    ]

    let private reservedBangIdentifierCases = [
        "DereferenceAfterIdentifier.fs",
        "module A\nlet r = f!x\n",
        [ "let r = [f! x]" ],
        [
            "DereferenceAfterIdentifier.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "DereferenceOperandBeforeBangEquals.fs",
        "module A\nlet y = !x!=y\n",
        [ "let y = {~!x! = y}" ],
        [
            "DereferenceOperandBeforeBangEquals.fs(2,10): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBang.fs",
        "module A\nlet r = x!\n",
        [ "let r = x!" ],
        [
            "IdentifierBang.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangEquals.fs",
        "module A\nlet r = x!=y\n",
        [ "let r = {x! = y}" ],
        [
            "IdentifierBangEquals.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangEqualsArgument.fs",
        "module A\nlet r = f x!=y\n",
        [ "let r = {[f x!] = y}" ],
        [
            "IdentifierBangEqualsArgument.fs(2,11): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangEqualsInParentheses.fs",
        "module A\nlet r = (x!=y)\n",
        [ "let r = ({x! = y})" ],
        [
            "IdentifierBangEqualsInParentheses.fs(2,10): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangArgument.fs",
        "module A\nlet r = f x!\n",
        [ "let r = [f x!]" ],
        [
            "IdentifierBangArgument.fs(2,11): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangMemberAccess.fs",
        "module A\nlet r = x!.A\n",
        [ "let r = x!.A" ],
        [
            "IdentifierBangMemberAccess.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "QualifiedIdentifierBang.fs",
        "module A\nlet r = A.B!\n",
        [ "let r = A.B!" ],
        [
            "QualifiedIdentifierBang.fs(2,11): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangApplication.fs",
        "module A\nlet r = x!y\n",
        [ "let r = [x! y]" ],
        [
            "IdentifierBangApplication.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "PrimedIdentifierBang.fs",
        "module A\nlet r = x'!\n",
        [ "let r = x'!" ],
        [
            "PrimedIdentifierBang.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "UnderscoreBang.fs",
        "module A\nlet r = _!\n",
        [ "let r = _!" ],
        [
            "UnderscoreBang.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangInList.fs",
        "module A\nlet r = [x!]\n",
        [ "let r = [x!]" ],
        [
            "IdentifierBangInList.fs(2,10): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangBinding.fs",
        "module A\nlet x! = 1\n",
        [ "let x! = 1" ],
        [
            "IdentifierBangBinding.fs(2,5): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "IdentifierBangParameter.fs",
        "module A\nlet f x! = 1\n",
        [ "let f x! = 1" ],
        [
            "IdentifierBangParameter.fs(2,7): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
        "EscapedIdentifierBeforeDereference.fs",
        "module A\nlet r = ``x``!y\n",
        [ "let r = [``x`` ~!y]" ],
        []
        "IdentifierBangPattern.fs",
        "module A\nlet r x =\n    match x with\n    | x! -> 1\n",
        [ "let r x = match x with | x! -> 1" ],
        [
            "IdentifierBangPattern.fs(4,7): error FS1141: Identifiers followed by '!' are reserved for future use"
        ]
    ]

    let private dereferenceExplicitCases = [
        "DoBangKeyword.fs", "module A\ndo! x\n", []
        "MatchBangKeyword.fs", "module A\nlet r () =\n    match! x with\n    | _ -> 1\n", []
        "IfFollowedByBang.fs",
        "module A\nlet r = if!x then 1 else 2\n",
        [
            "IfFollowedByBang.fs(2,9): error FS1141: Identifiers followed by '!' are reserved for future use"
            "IfFollowedByBang.fs(2,21): error FS0010: Unexpected keyword 'else' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "DoubleBangOperator.fs", "module A\nlet r = !!x\n", []
        "BangPlusOperator.fs", "module A\nlet r = f !+x\n", []
        "DereferenceConditionalOperand.fs",
        "module A\nlet r c = !if c then x else y\n",
        [
            "DereferenceConditionalOperand.fs(2,12): error FS0010: Unexpected keyword 'if' in expression"
        ]
    ]

    let private bodyArgumentLineCases = [
        "LambdaBodyThenPrefixSignLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      -1\n",
        [
            "LambdaBodyThenPrefixSignLine.fs(5,7): error FS0010: Unexpected integer literal in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenPlusSignLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      +1\n",
        [
            "LambdaBodyThenPlusSignLine.fs(5,7): error FS0010: Unexpected integer literal in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenDereferenceLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      !x\n",
        [
            "LambdaBodyThenDereferenceLine.fs(5,7): error FS0010: Unexpected prefix operator in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenSignLineThenAtom.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      -1\n      b\n",
        [
            "LambdaBodyThenSignLineThenAtom.fs(5,7): error FS0010: Unexpected integer literal in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenSignIdentifierLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      -x\n",
        [
            "LambdaBodyThenSignIdentifierLine.fs(5,7): error FS0010: Unexpected prefix operator in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaApplicationBodyThenSignLine.fs",
        "module A\nlet y =\n    fun x ->\n        f a\n      -1\n",
        [
            "LambdaApplicationBodyThenSignLine.fs(5,7): error FS0010: Unexpected integer literal in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "MatchClauseResultThenSignLine.fs",
        "module A\nlet y =\n    match x with\n    | A ->\n        a\n      -1\n",
        [
            "MatchClauseResultThenSignLine.fs(6,7): error FS0010: Unexpected integer literal in expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "MatchClauseResultThenSignLineThenClause.fs",
        "module A\nlet y =\n    match x with\n    | A ->\n        a\n      -1\n    | B -> 2\n",
        [
            "MatchClauseResultThenSignLineThenClause.fs(6,7): error FS0010: Unexpected integer literal in expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "ThenBodyThenSignLineThenElse.fs",
        "module A\nlet y =\n    if c then\n        a\n      -1\n    else 2\n",
        [
            "ThenBodyThenSignLineThenElse.fs(5,7): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenIdentifierLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      f\n",
        [
            "LambdaBodyThenIdentifierLine.fs(5,7): error FS0010: Unexpected identifier in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenParenthesizedLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      (b)\n",
        [
            "LambdaBodyThenParenthesizedLine.fs(5,7): error FS0010: Unexpected symbol '(' in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
        "LambdaBodyThenConstantLine.fs",
        "module A\nlet y =\n    fun x ->\n        a\n      1\n",
        [
            "LambdaBodyThenConstantLine.fs(5,7): error FS0010: Unexpected integer literal in lambda expression. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private numericLiteralCases = [
        "SignedSingleBitsOutOfRange.fs",
        "module A\nlet r = f -0x123456789lf\n",
        [ "let r = [f -0x123456789lf]" ],
        [
            "SignedSingleBitsOutOfRange.fs(2,12): error FS1155: This number is outside the allowable range for 32-bit floats"
        ]
        "SignedSingleBitsOutOfRangeHead.fs",
        "module A\nlet r = -0x123456789lf\n",
        [ "let r = -0x123456789lf" ],
        [
            "SignedSingleBitsOutOfRangeHead.fs(2,10): error FS1155: This number is outside the allowable range for 32-bit floats"
        ]
        "SignedSingleBitsOutOfRangeInList.fs",
        "module A\nlet r = [ -0x100000000lf ]\n",
        [ "let r = [-0x100000000lf]" ],
        [
            "SignedSingleBitsOutOfRangeInList.fs(2,12): error FS1155: This number is outside the allowable range for 32-bit floats"
        ]
        "SignedDoubleBitsOutOfRange.fs",
        "module A\nlet r = f -0x1234567890123456789LF\n",
        [ "let r = [f -0x1234567890123456789LF]" ],
        [ "SignedDoubleBitsOutOfRange.fs(2,12): error FS1153: Invalid floating point number" ]
        "SignedDoubleBitsAboveLimit.fs",
        "module A\nlet r = f -0x10000000000000000LF\n",
        [ "let r = [f -0x10000000000000000LF]" ],
        [ "SignedDoubleBitsAboveLimit.fs(2,12): error FS1153: Invalid floating point number" ]
        "SignedSingleBitsLimit.fs",
        "module A\nlet r = f -0xFFFFFFFFlf\n",
        [ "let r = [f -0xFFFFFFFFlf]" ],
        []
        "SignedDoubleBitsLimit.fs",
        "module A\nlet r = f -0x7FF0000000000000LF\n",
        [ "let r = [f -0x7FF0000000000000LF]" ],
        []
        "SingleBitsOutOfRange.fs",
        "module A\nlet r = f 0x123456789lf\n",
        [ "let r = [f 0x123456789lf]" ],
        [
            "SingleBitsOutOfRange.fs(2,11): error FS1155: This number is outside the allowable range for 32-bit floats"
        ]
        "DoubleBitsOutOfRange.fs",
        "module A\nlet r = f 0x10000000000000000LF\n",
        [ "let r = [f 0x10000000000000000LF]" ],
        [ "DoubleBitsOutOfRange.fs(2,11): error FS1153: Invalid floating point number" ]
        "Int32AtLimit.fs",
        "module A\nlet r = f 2147483648\n",
        [ "let r = [f 2147483648]" ],
        [
            "Int32AtLimit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "Int32AtLimitHead.fs",
        "module A\nlet r = 2147483648\n",
        [ "let r = 2147483648" ],
        [
            "Int32AtLimitHead.fs(2,9): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SByteAtLimit.fs",
        "module A\nlet r = f 128y\n",
        [ "let r = [f 128y]" ],
        [
            "SByteAtLimit.fs(2,11): error FS1142: This number is outside the allowable range for 8-bit signed integers"
        ]
        "Int16AtLimit.fs",
        "module A\nlet r = f 32768s\n",
        [ "let r = [f 32768s]" ],
        [
            "Int16AtLimit.fs(2,11): error FS1145: This number is outside the allowable range for 16-bit signed integers"
        ]
        "Int64AtLimit.fs",
        "module A\nlet r = f 9223372036854775808L\n",
        [ "let r = [f 9223372036854775808L]" ],
        [
            "Int64AtLimit.fs(2,11): error FS1149: This number is outside the allowable range for 64-bit signed integers"
        ]
        "NativeIntAtLimit.fs",
        "module A\nlet r = f 9223372036854775808n\n",
        [ "let r = [f 9223372036854775808n]" ],
        [
            "NativeIntAtLimit.fs(2,11): error FS1151: This number is outside the allowable range for signed native integers"
        ]
        "Int32AboveLimit.fs",
        "module A\nlet r = f 21474836479\n",
        [ "let r = [f 21474836479]" ],
        [
            "Int32AboveLimit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "ByteAboveLimit.fs",
        "module A\nlet r = f 256uy\n",
        [ "let r = [f 256uy]" ],
        [
            "ByteAboveLimit.fs(2,11): error FS1144: This number is outside the allowable range for 8-bit unsigned integers"
        ]
        "UInt16AboveLimit.fs",
        "module A\nlet r = f 65536us\n",
        [ "let r = [f 65536us]" ],
        [
            "UInt16AboveLimit.fs(2,11): error FS1146: This number is outside the allowable range for 16-bit unsigned integers"
        ]
        "UInt32AboveLimit.fs",
        "module A\nlet r = f 4294967296u\n",
        [ "let r = [f 4294967296u]" ],
        [
            "UInt32AboveLimit.fs(2,11): error FS1148: This number is outside the allowable range for 32-bit unsigned integers"
        ]
        "UInt32LowerLAboveLimit.fs",
        "module A\nlet r = f 4294967296ul\n",
        [ "let r = [f 4294967296ul]" ],
        [
            "UInt32LowerLAboveLimit.fs(2,11): error FS1148: This number is outside the allowable range for 32-bit unsigned integers"
        ]
        "UInt64AboveLimit.fs",
        "module A\nlet r = f 18446744073709551616UL\n",
        [ "let r = [f 18446744073709551616UL]" ],
        [
            "UInt64AboveLimit.fs(2,11): error FS1150: This number is outside the allowable range for 64-bit unsigned integers"
        ]
        "UNativeIntAboveLimit.fs",
        "module A\nlet r = f 18446744073709551616un\n",
        [ "let r = [f 18446744073709551616un]" ],
        [
            "UNativeIntAboveLimit.fs(2,11): error FS1152: This number is outside the allowable range for unsigned native integers"
        ]
        "HexSByteAboveLimit.fs",
        "module A\nlet r = f 0x100y\n",
        [ "let r = [f 0x100y]" ],
        [
            "HexSByteAboveLimit.fs(2,11): error FS1143: This number is outside the allowable range for hexadecimal 8-bit signed integers"
        ]
        "OctalSByteAboveLimit.fs",
        "module A\nlet r = f 0o400y\n",
        [ "let r = [f 0o400y]" ],
        [
            "OctalSByteAboveLimit.fs(2,11): error FS1143: This number is outside the allowable range for hexadecimal 8-bit signed integers"
        ]
        "HexInt16AboveLimit.fs",
        "module A\nlet r = f 0x10000s\n",
        [ "let r = [f 0x10000s]" ],
        [
            "HexInt16AboveLimit.fs(2,11): error FS1145: This number is outside the allowable range for 16-bit signed integers"
        ]
        "HexInt32AboveLimit.fs",
        "module A\nlet r = f 0x100000000\n",
        [ "let r = [f 0x100000000]" ],
        [
            "HexInt32AboveLimit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "BinaryInt32AboveLimit.fs",
        "module A\nlet r = f 0b100000000000000000000000000000000\n",
        [ "let r = [f 0b100000000000000000000000000000000]" ],
        [
            "BinaryInt32AboveLimit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "HexInt64AboveLimit.fs",
        "module A\nlet r = f 0x10000000000000000L\n",
        [ "let r = [f 0x10000000000000000L]" ],
        [
            "HexInt64AboveLimit.fs(2,11): error FS1149: This number is outside the allowable range for 64-bit signed integers"
        ]
        "DecimalAboveLimit.fs",
        "module A\nlet r = f 79228162514264337593543950336M\n",
        [ "let r = [f 79228162514264337593543950336M]" ],
        [
            "DecimalAboveLimit.fs(2,11): error FS1154: This number is outside the allowable range for decimal literals"
        ]
        "DecimalExponentAboveLimit.fs",
        "module A\nlet r = f 1e29M\n",
        [ "let r = [f 1e29M]" ],
        [
            "DecimalExponentAboveLimit.fs(2,11): error FS1154: This number is outside the allowable range for decimal literals"
        ]
        "HexInt32AllBits.fs", "module A\nlet r = f 0xFFFFFFFF\n", [ "let r = [f 0xFFFFFFFF]" ], []
        "HexSByteAllBits.fs", "module A\nlet r = f 0xFFy\n", [ "let r = [f 0xFFy]" ], []
        "SignedHexSByteAllBits.fs", "module A\nlet r = -0xFFy\n", [ "let r = -0xFFy" ], []
        "SignedHexInt16AllBits.fs",
        "module A\nlet r = f -0xFF_FFs\n",
        [ "let r = [f -0xFF_FFs]" ],
        []
        "PrefixSignHexLimit.fs",
        "module A\nlet r = f -0x80000000\n",
        [ "let r = [f -0x80000000]" ],
        []
        "PrefixSignInt32Overflow.fs",
        "module A\nlet r = f -2147483649\n",
        [ "let r = [f -2147483649]" ],
        [
            "PrefixSignInt32Overflow.fs(2,12): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "PrefixPlusInt32Limit.fs",
        "module A\nlet r = f +2147483648\n",
        [ "let r = [f +2147483648]" ],
        [
            "PrefixPlusInt32Limit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "PrefixPlusSByteLimit.fs",
        "module A\nlet r = f +128y\n",
        [ "let r = [f +128y]" ],
        [
            "PrefixPlusSByteLimit.fs(2,11): error FS1142: This number is outside the allowable range for 8-bit signed integers"
        ]
        "PrefixSignDecimalOverflow.fs",
        "module A\nlet r = f -79228162514264337593543950336m\n",
        [ "let r = [f -79228162514264337593543950336m]" ],
        [
            "PrefixSignDecimalOverflow.fs(2,12): error FS1154: This number is outside the allowable range for decimal literals"
        ]
        "SignedHexInt64SignBit.fs",
        "module A\nlet r = f -0x8000000000000000L\n",
        [ "let r = [f -0x8000000000000000L]" ],
        []
        "SignedBinarySByteAllBits.fs",
        "module A\nlet r = f -0b11111111y\n",
        [ "let r = [f -0b11111111y]" ],
        []
        "SignedHexInt32AboveLimit.fs",
        "module A\nlet r = f -0x100000000\n",
        [ "let r = [f -0x100000000]" ],
        [
            "SignedHexInt32AboveLimit.fs(2,12): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SignedInt32Limit.fs",
        "module A\nlet r = f -2147483648\n",
        [ "let r = [f -2147483648]" ],
        []
        "SignedInt32LimitHead.fs", "module A\nlet r = -2147483648\n", [ "let r = -2147483648" ], []
        "SignedInt32LimitInParentheses.fs",
        "module A\nlet r = (-2147483648)\n",
        [ "let r = (-2147483648)" ],
        []
        "SignedInt32LimitAfterSubtraction.fs",
        "module A\nlet r = x - -2147483648\n",
        [ "let r = {x - -2147483648}" ],
        []
        "SubtractedInt32Limit.fs",
        "module A\nlet r = x-2147483648\n",
        [ "let r = {x - 2147483648}" ],
        [
            "SubtractedInt32Limit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SpacedSubtractedInt32Limit.fs",
        "module A\nlet r = x - 2147483648\n",
        [ "let r = {x - 2147483648}" ],
        [
            "SpacedSubtractedInt32Limit.fs(2,13): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SpacedSignInt32Limit.fs",
        "module A\nlet r = f - 2147483648\n",
        [ "let r = {f - 2147483648}" ],
        [
            "SpacedSignInt32Limit.fs(2,13): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SpacedSignInt32LimitHead.fs",
        "module A\nlet r = - 2147483648\n",
        [ "let r = ~-2147483648" ],
        [
            "SpacedSignInt32LimitHead.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SignedParenthesizedInt32Limit.fs",
        "module A\nlet r = -(2147483648)\n",
        [ "let r = ~-(2147483648)" ],
        [
            "SignedParenthesizedInt32Limit.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "SubtractedInt32LimitArgument.fs",
        "module A\nlet r = f x-2147483648\n",
        [ "let r = {[f x] - 2147483648}" ],
        [
            "SubtractedInt32LimitArgument.fs(2,13): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "DereferencedInt32Limit.fs",
        "module A\nlet r = f !2147483648\n",
        [ "let r = [f ~!2147483648]" ],
        [
            "DereferencedInt32Limit.fs(2,12): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "Int32LimitArguments.fs",
        "module A\nlet r = f 2147483648 2147483648\n",
        [ "let r = [[f 2147483648] 2147483648]" ],
        [
            "Int32LimitArguments.fs(2,11): error FS1147: This number is outside the allowable range for 32-bit signed integers"
            "Int32LimitArguments.fs(2,22): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "Int32LimitInfixOperand.fs",
        "module A\nlet r = 1 + 2147483648\n",
        [ "let r = {1 + 2147483648}" ],
        [
            "Int32LimitInfixOperand.fs(2,13): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "Int32LimitConditionalBranch.fs",
        "module A\nlet r c = if c then 2147483648 else 1\n",
        [ "let r c = if c then 2147483648 else 1" ],
        [
            "Int32LimitConditionalBranch.fs(2,21): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "Int32LimitTupleItem.fs",
        "module A\nlet r = 2147483648, 1\n",
        [ "let r = 2147483648, 1" ],
        [
            "Int32LimitTupleItem.fs(2,9): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "PlusInt32LimitHead.fs",
        "module A\nlet r = +2147483648\n",
        [ "let r = +2147483648" ],
        [
            "PlusInt32LimitHead.fs(2,9): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "PlusInt32AboveLimit.fs",
        "module A\nlet r = f +2147483649\n",
        [ "let r = [f +2147483649]" ],
        [
            "PlusInt32AboveLimit.fs(2,12): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
        "PlusSByteAboveLimit.fs",
        "module A\nlet r = f +129y\n",
        [ "let r = [f +129y]" ],
        [
            "PlusSByteAboveLimit.fs(2,12): error FS1142: This number is outside the allowable range for 8-bit signed integers"
        ]
        "SignedDecimalLimit.fs",
        "module A\nlet r = f -79228162514264337593543950335M\n",
        [ "let r = [f -79228162514264337593543950335M]" ],
        []
        "SignedNativeIntLimit.fs",
        "module A\nlet r = f -9223372036854775808n\n",
        [ "let r = [f -9223372036854775808n]" ],
        []
        "SignedUInt32LowerLAboveLimit.fs",
        "module A\nlet r = f -4294967296ul\n",
        [ "let r = [f ~-4294967296ul]" ],
        [
            "SignedUInt32LowerLAboveLimit.fs(2,12): error FS1148: This number is outside the allowable range for 32-bit unsigned integers"
        ]
        "InvalidLiteralSuffixUpperLF.fs",
        "module A\nlet r = f 1LF\n",
        [ "let r = [f 1LF]" ],
        [
            "InvalidLiteralSuffixUpperLF.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "InvalidFloatSuffixL.fs",
        "module A\nlet r = f 1.5L\n",
        [ "let r = [f 1.5L]" ],
        [
            "InvalidFloatSuffixL.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "InvalidHexBignumSuffix.fs",
        "module A\nlet r = f 0x1FI\n",
        [ "let r = [f 0x1FI]" ],
        [
            "InvalidHexBignumSuffix.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "InvalidTrailingUnderscore.fs",
        "module A\nlet r = f 1_\n",
        [ "let r = [f 1_]" ],
        [
            "InvalidTrailingUnderscore.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "InvalidBinaryDigitTwo.fs",
        "module A\nlet r = f 0b2\n",
        [ "let r = [f 0b2]" ],
        [
            "InvalidBinaryDigitTwo.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "InvalidFractionUnderscore.fs",
        "module A\nlet r = f 1._0\n",
        [ "let r = [f 1._0]" ],
        [
            "InvalidFractionUnderscore.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "InvalidExponentUnderscore.fs",
        "module A\nlet r = f 1e_5\n",
        [ "let r = [f 1e_5]" ],
        [
            "InvalidExponentUnderscore.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "Int32LimitPattern.fs",
        "module A\nlet r x =\n    match x with\n    | 2147483648 -> 1\n    | _ -> 2\n",
        [ "let r x = match x with | 2147483648 -> 1 | _ -> 2" ],
        [
            "Int32LimitPattern.fs(4,7): error FS1147: This number is outside the allowable range for 32-bit signed integers"
        ]
    ]

    let private numericLiteralDotCases = [
        "SignedHexWithFraction.fs",
        "module A\nlet y = f -0x1.0\n",
        [ "let y = [[f -0x1] 0]" ],
        [ "SignedHexWithFraction.fs(2,15): error FS0599: Missing qualification after '.'" ]
        "HexLiteralDotFraction.fs",
        "module A\nlet r = f 0x1.0\n",
        [ "let r = [[f 0x1] 0]" ],
        [ "HexLiteralDotFraction.fs(2,14): error FS0599: Missing qualification after '.'" ]
        "HexLiteralMemberAccess.fs", "module A\nlet r = f 0x1.A\n", [ "let r = [f 0x1.A]" ], []
        "UnsignedLiteralDotFraction.fs",
        "module A\nlet r = f 1u.0\n",
        [ "let r = [[f 1u] 0]" ],
        [ "UnsignedLiteralDotFraction.fs(2,13): error FS0599: Missing qualification after '.'" ]
        "FloatLiteralMemberAccess.fs", "module A\nlet r = f 1.5.A\n", [ "let r = [f 1.5.A]" ], []
        "SByteLiteralMemberAccess.fs", "module A\nlet r = f 1y.A\n", [ "let r = [f 1y.A]" ], []
        "ExponentLiteralDotFraction.fs",
        "module A\nlet r = f 1e5.0\n",
        [ "let r = [[f 1e5] 0]" ],
        [ "ExponentLiteralDotFraction.fs(2,14): error FS0599: Missing qualification after '.'" ]
        "TrailingUnderscoreDotFraction.fs",
        "module A\nlet r = f 1_.0\n",
        [ "let r = [[f 1_] 0]" ],
        [
            "TrailingUnderscoreDotFraction.fs(2,11): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
            "TrailingUnderscoreDotFraction.fs(2,13): error FS0599: Missing qualification after '.'"
        ]
    ]

    let private castAndMemberAccessCases = [
        "Upcast.fs", "module A\nlet r = x :> T\n", [ "let r = {x :> T}" ], [ ":>(2,9--2,15)" ]
        "UpcastApplication.fs",
        "module A\nlet r = f x :> T\n",
        [ "let r = {[f x] :> T}" ],
        [
            ":>(2,9--2,17)"
            "app(2,9--2,12)"
        ]
        "UpcastChain.fs",
        "module A\nlet r = x :> T :> U\n",
        [ "let r = {{x :> T} :> U}" ],
        [
            ":>(2,9--2,20)"
            ":>(2,9--2,15)"
        ]
        "UpcastAfterComparison.fs",
        "module A\nlet r = a = b :> T\n",
        [ "let r = {{a = b} :> T}" ],
        [
            ":>(2,9--2,19)"
            "=(2,9--2,14)"
        ]
        "UpcastBelowAnd.fs",
        "module A\nlet r = a && b :> T\n",
        [ "let r = {a && {b :> T}}" ],
        [
            "&&(2,9--2,20)"
            ":>(2,14--2,20)"
        ]
        "UpcastAfterAddition.fs",
        "module A\nlet r = a + b :> T\n",
        [ "let r = {{a + b} :> T}" ],
        [
            ":>(2,9--2,19)"
            "+(2,9--2,14)"
        ]
        "UpcastAfterCons.fs",
        "module A\nlet r = a :: x :> T\n",
        [ "let r = {{a :: x} :> T}" ],
        [
            ":>(2,9--2,20)"
            "::(2,9--2,15)"
        ]
        "UpcastAfterPipeLeft.fs",
        "module A\nlet r = f <| x :> T\n",
        [ "let r = {{f <| x} :> T}" ],
        [
            ":>(2,9--2,20)"
            "<|(2,9--2,15)"
        ]
        "UpcastAfterLessThan.fs",
        "module A\nlet r = x < z :> T\n",
        [ "let r = {{x < z} :> T}" ],
        [
            ":>(2,9--2,19)"
            "<(2,9--2,14)"
        ]
        "UpcastOfNegation.fs",
        "module A\nlet r = -x :> T\n",
        [ "let r = {~-x :> T}" ],
        [
            ":>(2,9--2,16)"
            "~-(2,9--2,11)"
        ]
        "UpcastTupleItem.fs",
        "module A\nlet r = x :> T, y\n",
        [ "let r = {x :> T}, y" ],
        [
            "tuple(2,9--2,18)"
            ":>(2,9--2,15)"
        ]
        "UpcastComparisonOperand.fs",
        "module A\nlet r = x :> T = y\n",
        [ "let r = {{x :> T} = y}" ],
        [
            "=(2,9--2,19)"
            ":>(2,9--2,15)"
        ]
        "UpcastAdditionOperand.fs",
        "module A\nlet r = x :> T + 1\n",
        [ "let r = {{x :> T} + 1}" ],
        [
            "+(2,9--2,19)"
            ":>(2,9--2,15)"
        ]
        "UpcastConsOperand.fs",
        "module A\nlet r = x :> T :: z\n",
        [ "let r = {{x :> T} :: z}" ],
        [
            "::(2,9--2,20)"
            ":>(2,9--2,15)"
        ]
        "UpcastAssignedByColonEquals.fs",
        "module A\nlet r = x := a :> T\n",
        [ "let r = {x := {a :> T}}" ],
        [
            ":=(2,9--2,20)"
            ":>(2,14--2,20)"
        ]
        "UpcastAssignedByArrow.fs",
        "module A\nlet r = z <- x :> T\n",
        [ "let r = {z <- {x :> T}}" ],
        [
            "set(2,9--2,20)"
            ":>(2,14--2,20)"
        ]
        "UpcastPostfixTypeApplication.fs",
        "module A\nlet r = x :> int list\n",
        [ "let r = {x :> int list}" ],
        [ ":>(2,9--2,22)" ]
        "UpcastParenthesizedFunctionType.fs",
        "module A\nlet r = x :> (int -> int)\n",
        [ "let r = {x :> (int -> int)}" ],
        [ ":>(2,9--2,26)" ]
        "UpcastTupleType.fs",
        "module A\nlet r = x :> int * int\n",
        [ "let r = {x :> (int * int)}" ],
        [ ":>(2,9--2,23)" ]
        "UpcastFunctionType.fs",
        "module A\nlet r = x :> T -> U\n",
        [ "let r = {x :> (T -> U)}" ],
        [ ":>(2,9--2,20)" ]
        "UpcastQualifiedType.fs",
        "module A\nlet r = x :> A.B\n",
        [ "let r = {x :> A.B}" ],
        [ ":>(2,9--2,17)" ]
        "UpcastTypeArguments.fs",
        "module A\nlet r = x :> T<int>\n",
        [ "let r = {x :> T<int>}" ],
        [ ":>(2,9--2,20)" ]
        "UpcastArrayType.fs",
        "module A\nlet r = x :> T[]\n",
        [ "let r = {x :> T[]}" ],
        [ ":>(2,9--2,17)" ]
        "UpcastWithoutSpaces.fs",
        "module A\nlet r = x:>T\n",
        [ "let r = {x :> T}" ],
        [ ":>(2,9--2,13)" ]
        "UpcastInParentheses.fs",
        "module A\nlet r = f (x :> T)\n",
        [ "let r = [f ({x :> T})]" ],
        [
            ":>(2,12--2,18)"
            "app(2,9--2,19)"
        ]
        "UpcastInList.fs",
        "module A\nlet r = [x :> T; y]\n",
        [ "let r = [{x :> T}; y]" ],
        [ ":>(2,10--2,16)" ]
        "UpcastInRecord.fs", "module A\nlet r = { A = x :> T }\n", [ "let r = {A = {x :> T}}" ], []
        "UpcastLambdaBody.fs",
        "module A\nlet r = fun x -> x :> T\n",
        [ "let r = fun x -> {x :> T}" ],
        [ ":>(2,18--2,24)" ]
        "UpcastClauseResult.fs",
        "module A\nlet r = match x with\n        | A -> x :> T\n        | B -> z\n",
        [ "let r = match x with | A -> {x :> T} | B -> z" ],
        [ ":>(3,16--3,22)" ]
        "Downcast.fs", "module A\nlet r = x :?> T\n", [ "let r = {x :?> T}" ], [ ":?>(2,9--2,16)" ]
        "DowncastChain.fs",
        "module A\nlet r = x :?> T :?> U\n",
        [ "let r = {{x :?> T} :?> U}" ],
        [
            ":?>(2,9--2,22)"
            ":?>(2,9--2,16)"
        ]
        "DowncastAfterUpcast.fs",
        "module A\nlet r = x :> T :?> U\n",
        [ "let r = {{x :> T} :?> U}" ],
        [
            ":?>(2,9--2,21)"
            ":>(2,9--2,15)"
        ]
        "TypeTest.fs", "module A\nlet r = x :? T\n", [ "let r = {x :? T}" ], [ ":?(2,9--2,15)" ]
        "TypeTestChain.fs",
        "module A\nlet r = x :? T :? U\n",
        [ "let r = {{x :? T} :? U}" ],
        [
            ":?(2,9--2,20)"
            ":?(2,9--2,15)"
        ]
        "TypeTestAboveComparison.fs",
        "module A\nlet r = a = b :? T\n",
        [ "let r = {a = {b :? T}}" ],
        [
            "=(2,9--2,19)"
            ":?(2,13--2,19)"
        ]
        "TypeTestBelowAddition.fs",
        "module A\nlet r = a + b :? T\n",
        [ "let r = {{a + b} :? T}" ],
        [
            ":?(2,9--2,19)"
            "+(2,9--2,14)"
        ]
        "TypeTestAboveCons.fs",
        "module A\nlet r = a :: x :? T\n",
        [ "let r = {a :: {x :? T}}" ],
        [
            "::(2,9--2,20)"
            ":?(2,14--2,20)"
        ]
        "TypeTestAfterPipeLeft.fs",
        "module A\nlet r = f <| x :? T\n",
        [ "let r = {f <| {x :? T}}" ],
        [
            "<|(2,9--2,20)"
            ":?(2,14--2,20)"
        ]
        "TypeTestAfterUpcast.fs",
        "module A\nlet r = x :> T :? U\n",
        [ "let r = {{x :> T} :? U}" ],
        [
            ":?(2,9--2,20)"
            ":>(2,9--2,15)"
        ]
        "TypeTestConditionThenDowncast.fs",
        "module A\nlet r = if x :? T then x :?> T else y\n",
        [ "let r = if {x :? T} then {x :?> T} else y" ],
        [
            ":?(2,12--2,18)"
            ":?>(2,24--2,31)"
        ]
        "TypeTestMatchInput.fs",
        "module A\nlet r = match x :? T with\n        | true -> 1\n        | _ -> 2\n",
        [ "let r = match {x :? T} with | Boolean true -> 1 | _ -> 2" ],
        [ ":?(2,15--2,21)" ]
        "MemberAccessOnParentheses.fs",
        "module A\nlet r = (x).A\n",
        [ "let r = (x).A" ],
        [ "dot(2,9--2,14)" ]
        "MemberAccessLongIdentifier.fs",
        "module A\nlet r = (x).A.B\n",
        [ "let r = (x).A.B" ],
        [ "dot(2,9--2,16)" ]
        "MemberAccessOnHighPrecedenceApplication.fs",
        "module A\nlet r = f(x).A\n",
        [ "let r = [f (x)].A" ],
        [
            "dot(2,9--2,15)"
            "app(2,9--2,13)"
        ]
        "MemberAccessInArgument.fs",
        "module A\nlet r = f (x).A\n",
        [ "let r = [f (x).A]" ],
        [
            "dot(2,11--2,16)"
            "app(2,9--2,16)"
        ]
        "MemberAccessOnString.fs",
        "module A\nlet r = \"s\".Length\n",
        [ "let r = \"s\".Length" ],
        [ "dot(2,9--2,19)" ]
        "MemberAccessOnList.fs",
        "module A\nlet r = [1].Head\n",
        [ "let r = [1].Head" ],
        [ "dot(2,9--2,17)" ]
        "MemberAccessOnFloat.fs",
        "module A\nlet r = 1.5.A\n",
        [ "let r = 1.5.A" ],
        [ "dot(2,9--2,14)" ]
        "MemberAccessOnHexLiteral.fs",
        "module A\nlet r = 0x1.A\n",
        [ "let r = 0x1.A" ],
        [ "dot(2,9--2,14)" ]
        "MemberAccessOnRecord.fs",
        "module A\nlet r = { A = 1 }.A\n",
        [ "let r = {A = 1}.A" ],
        [ "dot(2,9--2,20)" ]
        "MemberAccessOnBoolean.fs",
        "module A\nlet r = true.A\n",
        [ "let r = Boolean true.A" ],
        [ "dot(2,9--2,15)" ]
        "MemberAccessOnCharacter.fs",
        "module A\nlet r = 'c'.A\n",
        [ "let r = Character \"'c'\".A" ],
        [ "dot(2,9--2,14)" ]
        "MemberAccessEscapedName.fs",
        "module A\nlet r = (x).``A B``\n",
        [ "let r = (x).``A B``" ],
        [ "dot(2,9--2,20)" ]
        "MemberAccessApplied.fs",
        "module A\nlet r = (x).A(1)\n",
        [ "let r = [(x).A (1)]" ],
        [
            "dot(2,9--2,14)"
            "app(2,9--2,17)"
        ]
        "MemberAccessAppliedWithSpace.fs",
        "module A\nlet r = (x).A (1)\n",
        [ "let r = [(x).A (1)]" ],
        [
            "dot(2,9--2,14)"
            "app(2,9--2,18)"
        ]
        "MemberAccessWithArgument.fs",
        "module A\nlet r = (x).A x\n",
        [ "let r = [(x).A x]" ],
        [
            "dot(2,9--2,14)"
            "app(2,9--2,16)"
        ]
        "MemberAccessNegated.fs",
        "module A\nlet r = -(x).A\n",
        [ "let r = ~-(x).A" ],
        [
            "~-(2,9--2,15)"
            "dot(2,10--2,15)"
        ]
        "MemberAccessDereferenced.fs",
        "module A\nlet r = !(x).A\n",
        [ "let r = ~!(x).A" ],
        [
            "~!(2,9--2,15)"
            "dot(2,10--2,15)"
        ]
        "MemberAccessAdditionOperand.fs",
        "module A\nlet r = (x).A + 1\n",
        [ "let r = {(x).A + 1}" ],
        [
            "+(2,9--2,18)"
            "dot(2,9--2,14)"
        ]
        "MemberAccessArguments.fs",
        "module A\nlet r = f (x).A (y).B\n",
        [ "let r = [[f (x).A] (y).B]" ],
        [
            "dot(2,11--2,16)"
            "dot(2,17--2,22)"
            "app(2,9--2,22)"
            "app(2,9--2,16)"
        ]
        "MemberAccessAfterMethodCall.fs",
        "module A\nlet r = x.A(1).B\n",
        [ "let r = [x.A (1)].B" ],
        [
            "dot(2,9--2,17)"
            "app(2,9--2,15)"
        ]
        "MemberAccessChainOfMethodCalls.fs",
        "module A\nlet r = x.A(1).B(2).C\n",
        [ "let r = [[x.A (1)].B (2)].C" ],
        [
            "dot(2,9--2,22)"
            "dot(2,9--2,17)"
            "app(2,9--2,20)"
            "app(2,9--2,15)"
        ]
        "MemberAccessOnIndex.fs",
        "module A\nlet r = x[0].A\n",
        [ "let r = x[0].A" ],
        [ "dot(2,9--2,15)" ]
        "MemberAccessIndexed.fs",
        "module A\nlet r = (x).A[0].B\n",
        [ "let r = (x).A[0].B" ],
        [ "dot(2,9--2,19)" ]
        "MemberAccessUpcast.fs",
        "module A\nlet r = (x).A :> T\n",
        [ "let r = {(x).A :> T}" ],
        [
            ":>(2,9--2,19)"
            "dot(2,9--2,14)"
        ]
        "MemberAccessInArgumentLongIdentifier.fs",
        "module A\nlet r = f (x).A.B\n",
        [ "let r = [f (x).A.B]" ],
        [
            "dot(2,11--2,18)"
            "app(2,9--2,18)"
        ]
        "MemberAccessStringArgument.fs",
        "module A\nlet r = f \"s\".Length\n",
        [ "let r = [f \"s\".Length]" ],
        [
            "dot(2,11--2,21)"
            "app(2,9--2,21)"
        ]
    ]

    let private castAndMemberAccessExplicitCases = [
        "UpcastLineAtBlockColumn.fs", "module A\nlet r =\n    x\n    :> T\n", []
        "UpcastLineIndented.fs", "module A\nlet r =\n    x\n        :> T\n", []
        "UpcastTypeOnNextLine.fs", "module A\nlet r =\n    x :>\n        T\n", []
        "TypeTestLineAtBlockColumn.fs",
        "module A\nlet r =\n    x\n    :? T\n",
        [
            "TypeTestLineAtBlockColumn.fs(4,5): error FS0010: Unexpected symbol ':?' in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "UpcastConstantType.fs", "module A\nlet r = x :> 1\n", []
        "UpcastFlexibleType.fs", "module A\nlet r = x :> #seq<int>\n", []
        "UpcastTypeBeforeAppend.fs",
        "module A\nlet r = x :> T @ z\n",
        [
            "UpcastTypeBeforeAppend.fs(2,18): error FS0010: Unexpected identifier in expression. Expected integer literal, '(', '-' or other token."
        ]
        "UpcastTypeBeforeCaret.fs",
        "module A\nlet r = x :> T ^ z\n",
        [
            "UpcastTypeBeforeCaret.fs(2,18): error FS0010: Unexpected identifier in expression. Expected integer literal, '(', '-' or other token."
        ]
        "UpcastTypeBeforeAdjacentSign.fs",
        "module A\nlet r = x :> T -1\n",
        [
            "UpcastTypeBeforeAdjacentSign.fs(2,16): error FS0010: Unexpected integer literal in binding. Expected incomplete structured construct at or before this point or other token."
        ]
        "TypeTestGuardBeforeArrow.fs",
        "module A\nlet r =\n    match x with\n    | A when y :? T -> 1\n    | _ -> 2\n",
        [
            "TypeTestGuardBeforeArrow.fs(5,5): error FS0010: Incomplete structured construct at or before this point in pattern matching. Expected '->' or other token."
        ]
        "MemberAccessDotSpaceName.fs", "module A\nlet r = (x). A\n", []
        "MemberAccessSpaceDot.fs", "module A\nlet r = (x) .A\n", []
        "MemberAccessNextLine.fs", "module A\nlet r =\n    (x)\n        .A\n", []
        "MemberAccessAfterSecondApplication.fs", "module A\nlet r = f(x)(y).A\n", []
        "MemberAccessAfterParenthesizedApplication.fs", "module A\nlet r = (x)(y).A\n", []
        "MemberAccessArgumentApplied.fs",
        "module A\nlet r = f (x).A(1)\n",
        [
            "MemberAccessArgumentApplied.fs(2,11): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
        ]
        "MemberAccessAssignment.fs", "module A\nlet r = (x).A.B <- 1\n", []
        "MemberAccessTypeApplication.fs", "module A\nlet r = (x).A<int>\n", []
    ]

    let private memberAccessDiagnosticCases = [
        "MemberAccessOnIdentifierArgumentApplication.fs",
        "module A\nlet r = f x(1).A\n",
        [ "let r = [f [x (1)].A]" ],
        [
            "MemberAccessOnIdentifierArgumentApplication.fs(2,11): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
        ]
        "MemberAccessMissingName.fs",
        "module A\nlet r = (x).\n",
        [ "let r = (x)" ],
        [ "MemberAccessMissingName.fs(2,12): error FS0599: Missing qualification after '.'" ]
        "MemberAccessNumericName.fs",
        "module A\nlet r = (x).1\n",
        [ "let r = [(x) 1]" ],
        [ "MemberAccessNumericName.fs(2,12): error FS0599: Missing qualification after '.'" ]
    ]

    let private unreadableSignedLiteralCases = [
        "SignedHexBignumSuffix.fs",
        "module A\nlet y = f -0x1FI\n",
        [
            "SignedHexBignumSuffix.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixLs.fs",
        "module A\nlet y = f -1ls\n",
        [
            "SignedLiteralSuffixLs.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedHexDecimalSuffix.fs",
        "module A\nlet y = f -0x1M\n",
        [
            "SignedHexDecimalSuffix.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedBinaryBignumSuffix.fs",
        "module A\nlet y = f -0b1I\n",
        [
            "SignedBinaryBignumSuffix.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedOctalBignumSuffix.fs",
        "module A\nlet y = f -0o7N\n",
        [
            "SignedOctalBignumSuffix.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixLsAgain.fs",
        "module A\nlet y = f -1ls\n",
        [
            "SignedLiteralSuffixLsAgain.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedHexBignumHead.fs",
        "module A\nlet y = -0x1FI\n",
        [
            "SignedHexBignumHead.fs(2,10): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixXy.fs",
        "module A\nlet y = f -1xy\n",
        [
            "SignedLiteralSuffixXy.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedHexLowerDecimalSuffix.fs",
        "module A\nlet y = f -0x1Fm\n",
        [
            "SignedHexLowerDecimalSuffix.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedHexSuffixG.fs",
        "module A\nlet y = f -0x1G\n",
        [
            "SignedHexSuffixG.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedHexSuffixQInList.fs",
        "module A\nlet y = [ -0x1Q ]\n",
        [
            "SignedHexSuffixQInList.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
    ]

    let private invalidSignedLiteralCases = [
        "SignedBinaryDigitTwo.fs",
        "module A\nlet y = f -0b2\n",
        [
            "SignedBinaryDigitTwo.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedFloatSuffixI.fs",
        "module A\nlet y = f -1.0I\n",
        [
            "SignedFloatSuffixI.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedFloatSuffixL.fs",
        "module A\nlet y = f -1.5L\n",
        [
            "SignedFloatSuffixL.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedFloatSuffixLowerLF.fs",
        "module A\nlet y = f -1.0lf\n",
        [
            "SignedFloatSuffixLowerLF.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedFloatSuffixUpperLF.fs",
        "module A\nlet y = f -1.0LF\n",
        [
            "SignedFloatSuffixUpperLF.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedFloatSuffixY.fs",
        "module A\nlet y = f -1.0y\n",
        [
            "SignedFloatSuffixY.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedHexNoDigits.fs",
        "module A\nlet y = f -0x\n",
        [
            "SignedHexNoDigits.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralEmptyExponent.fs",
        "module A\nlet y = f -1e\n",
        [
            "SignedLiteralEmptyExponent.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixLu.fs",
        "module A\nlet y = f -1lu\n",
        [
            "SignedLiteralSuffixLu.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixUl.fs",
        "module A\nlet y = f -1Ul\n",
        [
            "SignedLiteralSuffixUl.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixUpperLF.fs",
        "module A\nlet y = f -1LF\n",
        [
            "SignedLiteralSuffixUpperLF.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralSuffixUpperLFHead.fs",
        "module A\nlet y = -1LF\n",
        [
            "SignedLiteralSuffixUpperLFHead.fs(2,10): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedLiteralTrailingUnderscore.fs",
        "module A\nlet y = f -1_\n",
        [
            "SignedLiteralTrailingUnderscore.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedOctalDigitNine.fs",
        "module A\nlet y = f -0o9\n",
        [
            "SignedOctalDigitNine.fs(2,12): error FS1156: This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
        ]
        "SignedUnsignedByteOutOfRange.fs",
        "module A\nlet y = f -1_000uy\n",
        [
            "SignedUnsignedByteOutOfRange.fs(2,12): error FS1144: This number is outside the allowable range for 8-bit unsigned integers"
        ]
        "SignedUnsignedHexByteOutOfRange.fs",
        "module A\nlet y = f -0x1e3uy\n",
        [
            "SignedUnsignedHexByteOutOfRange.fs(2,12): error FS1144: This number is outside the allowable range for 8-bit unsigned integers"
        ]
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

            testList
                "a lambda body offside inside a delimiter reports FSC2P1001 first and a known extra FS0010"
                [
                    for logicalPath, text, oracle, invented in delimitedLineInventedCases ->
                        testCase logicalPath
                        <| fun _ ->
                            let result = parse logicalPath text

                            Expect.equal
                                (Seq.head result.Diagnostics).Code
                                "FSC2P1001"
                                "The first diagnostic is the explicit unsupported diagnostic"

                            Expect.sequenceEqual
                                (oracleLines logicalPath result
                                 |> List.filter (fun line ->
                                     not (line.Contains ": error FSC2P1001: ")
                                     && not (List.contains line oracle)
                                 ))
                                invented
                                "The FS diagnostics that the Compatibility Oracle does not report"
                ]

            testList "an infix line continues the block item" [
                for logicalPath, text, expectedDeclarations, expectedRanges in infixLineCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with an infix line"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect infixRanges)
                            expectedRanges
                            "The infix, tuple, assignment, local binding, and sequential expression ranges"
            ]

            testList "an infix line that the parser does not model stays explicit" [
                for logicalPath, text, oracle in infixLineExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "an argument on a more indented line continues the application" [
                for logicalPath, text, expectedDeclarations, expectedRanges in continuationLineCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with a continuation line"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, tuple, assignment, local binding, sequential, and application ranges"
            ]

            testList "a more indented line that the parser does not model stays explicit" [
                for logicalPath, text, oracle in continuationLineExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "then, elif, and else lines belong to the innermost open if" [
                for logicalPath, text, expectedDeclarations, expectedRanges in conditionalLineCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with conditional lines"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                                 @ conditionalRanges body
                             ))
                            expectedRanges
                            "The infix, application, and conditional ranges, and the else of each conditional"
            ]

            testList "a conditional line that the parser does not model stays explicit" [
                for logicalPath, text, oracle in conditionalLineExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "with, clause bars, and clause results can align with match" [
                for logicalPath, text, expectedDeclarations, expectedRanges in matchLineCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with match lines"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                                 @ conditionalRanges body
                                 @ matchRanges body
                             ))
                            expectedRanges
                            "The infix, application, conditional, and match ranges, and the clause count of each match"
            ]

            testList "a match line that the parser does not model stays explicit" [
                for logicalPath, text, oracle in matchLineExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList
                "the layout reports FS0058 for a clause bar between two block columns only where the Oracle does"
                [
                    for logicalPath, text, expected in clauseBarLayoutCases ->
                        testCase logicalPath
                        <| fun _ ->
                            let language =
                                LanguageVersion.normalize (Some "10.0")
                                |> Result.defaultWith failtest

                            let document =
                                SourceSnapshot.Create(
                                    StableIdentity.create logicalPath,
                                    logicalPath,
                                    text,
                                    "content"
                                )
                                |> LexicalPipeline.prepare language Array.empty

                            Expect.sequenceEqual
                                (document.Diagnostics
                                 |> Seq.map (fun diagnostic ->
                                     $"{diagnostic.Code}({diagnostic.Range.Start.Line},{diagnostic.Range.Start.Column})"
                                 ))
                                expected
                                "The layout diagnostics"
                ]

            testList "a clause bar left of an inner match belongs to the outer match" [
                for logicalPath, text, expectedDeclarations, expectedRanges in clauseBarCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with clause bars"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                                 @ conditionalRanges body
                                 @ matchRanges body
                             ))
                            expectedRanges
                            "The match ranges and the clause count of each match"
            ]

            testList "a clause bar that the parser does not model stays explicit" [
                for logicalPath, text, oracle in clauseBarExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "a lambda body can undent to the enclosing let, member, if, else, or match" [
                for logicalPath, text, expectedDeclarations, expectedRanges in lambdaBodyCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with lambda bodies"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                                 @ conditionalRanges body
                                 @ matchRanges body
                             ))
                            expectedRanges
                            "The infix, application, conditional, and match ranges, and the clause count of each match"
            ]

            testList "a lambda body that the parser does not model stays explicit" [
                for logicalPath, text, oracle in lambdaBodyExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "a delimiter that closes left of the line that opened it ends the block item" [
                for logicalPath, text, expectedDeclarations, expectedRanges in undentedCloseCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with delimiters that close on a later line"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                                 @ conditionalRanges body
                                 @ matchRanges body
                             ))
                            expectedRanges
                            "The infix, application, conditional, and match ranges, and the clause count of each match"
            ]

            testList "a token after a delimiter that closes left of its line stays explicit" [
                for logicalPath, text, oracle in undentedCloseExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList
                "a line between a binding or do column and its block column reports the Oracle diagnostics"
                [
                    for logicalPath, text, oracle in bindingOffsideCases ->
                        testCase logicalPath
                        <| fun _ ->
                            let result = parse logicalPath text

                            Expect.sequenceEqual
                                (oracleLines logicalPath result)
                                oracle
                                "The diagnostics must match the Compatibility Oracle"
                ]

            testList
                "a line between a binding or do column and its block column that the parser does not model stays explicit"
                [
                    for logicalPath, text, oracle in bindingOffsideExplicitCases ->
                        testCase logicalPath
                        <| fun _ ->
                            let result = parse logicalPath text

                            SyntaxDiagnosticText.expectExplicitlyUnsupported
                                oracle
                                result.Diagnostics
                                (oracleLines logicalPath result)
                ]

            testList "a delimiter that closes left of its block item ends the item" [
                for logicalPath, text, expectedDeclarations, expectedRanges in
                    undentedItemCloseCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with delimiters that close on a later line"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                                 @ conditionalRanges body
                                 @ matchRanges body
                             ))
                            expectedRanges
                            "The infix, application, conditional, and match ranges, and the clause count of each match"
            ]

            testList "a leading dot does not change the precedence of an operator" [
                for logicalPath, text, expectedDeclarations, expectedRanges in dotOperatorCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with dot operators"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, prefix, and application ranges"
            ]

            testList "a dot operator the parser does not model stays explicit" [
                for logicalPath, text, oracle in dotOperatorExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "':=' binds looser than ',' and tighter than '<-'" [
                for logicalPath, text, expectedDeclarations, expectedRanges in
                    assignmentOperatorCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with ':='"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, prefix, and application ranges"
            ]

            testList "a ':=' line the parser does not model stays explicit" [
                for logicalPath, text, oracle in assignmentOperatorExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "the 'or' keyword is an infix operator at the '||' level" [
                for logicalPath, text, expectedDeclarations, expectedRanges in orKeywordCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with 'or'"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, prefix, and application ranges"
            ]

            testList "an 'or' line the parser does not model stays explicit" [
                for logicalPath, text, oracle in orKeywordExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "'!' is a prefix operator on one atomic expression" [
                for logicalPath, text, expectedDeclarations, expectedRanges in dereferenceCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with '!'"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, prefix, and application ranges"
            ]

            testList
                "an identifier followed by '!' is one reserved token, and an escaped identifier is not (Oracle FS1141)"
                [
                    for logicalPath, text, expectedDeclarations, oracle in
                        reservedBangIdentifierCases ->
                        testCase logicalPath
                        <| fun _ ->
                            Expect.sequenceEqual
                                (lexicalAndParserLines logicalPath text)
                                oracle
                                "The lexer and parser diagnostics match the Compatibility Oracle"

                            Expect.sequenceEqual
                                ((Seq.exactlyOne (parse logicalPath text).File.Contents)
                                    .Declarations
                                 |> Seq.map declarationShape)
                                expectedDeclarations
                                "The declarations with a reserved identifier"
                ]

            testList "a '!' form the parser does not model stays explicit" [
                for logicalPath, text, oracle in dereferenceExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "an argument line left of a lambda, match, or conditional body stays explicit" [
                for logicalPath, text, oracle in bodyArgumentLineCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "a signed literal with an unreadable suffix reports the Oracle diagnostics" [
                for logicalPath, text, oracle in unreadableSignedLiteralCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (lexicalAndParserLines logicalPath text)
                            oracle
                            "The lexer and parser diagnostics match the Compatibility Oracle"
            ]

            testList
                "a signed literal that is not a valid numeric literal reports the Oracle diagnostics"
                [
                    for logicalPath, text, oracle in invalidSignedLiteralCases ->
                        testCase logicalPath
                        <| fun _ ->
                            Expect.sequenceEqual
                                (lexicalAndParserLines logicalPath text)
                                oracle
                                "The lexer and parser diagnostics match the Compatibility Oracle"
                ]

            testList "a numeric literal reports the Oracle form and range diagnostics" [
                for logicalPath, text, expectedDeclarations, oracle in numericLiteralCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (lexicalAndParserLines logicalPath text)
                            oracle
                            "The lexer and parser diagnostics match the Compatibility Oracle"

                        Expect.sequenceEqual
                            ((Seq.exactlyOne (parse logicalPath text).File.Contents).Declarations
                             |> Seq.map declarationShape)
                            expectedDeclarations
                            "The declarations with a numeric literal"
            ]

            testList "a '.' after a numeric literal reads member access or reports FS0599" [
                for logicalPath, text, expectedDeclarations, oracle in numericLiteralDotCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (lexicalAndParserLines logicalPath text)
                            oracle
                            "The lexer and parser diagnostics match the Compatibility Oracle"

                        Expect.sequenceEqual
                            ((Seq.exactlyOne (parse logicalPath text).File.Contents).Declarations
                             |> Seq.map declarationShape)
                            expectedDeclarations
                            "The declarations with a '.' after a numeric literal"
            ]

            testList "':>', ':?>', ':?', and member access read the FCS tree" [
                for logicalPath, text, expectedDeclarations, expectedRanges in
                    castAndMemberAccessCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            (lexicalAndParserLines logicalPath text)
                            []
                            "The Compatibility Oracle reports no diagnostics"

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with casts, type tests, and member access"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, cast, member access, and application ranges"
            ]

            testList "a cast or member access form the parser does not model stays explicit" [
                for logicalPath, text, oracle in castAndMemberAccessExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (lexicalAndParserLines logicalPath text)
            ]

            testList "member access reports the Oracle diagnostics" [
                for logicalPath, text, expectedDeclarations, oracle in memberAccessDiagnosticCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (lexicalAndParserLines logicalPath text)
                            oracle
                            "The lexer and parser diagnostics match the Compatibility Oracle"

                        Expect.sequenceEqual
                            ((Seq.exactlyOne (parse logicalPath text).File.Contents).Declarations
                             |> Seq.map declarationShape)
                            expectedDeclarations
                            "The declarations with member access"
            ]

            testList "an adjacent sign is a prefix operator and a spaced sign is subtraction" [
                for logicalPath, text, expectedDeclarations, expectedRanges in prefixSignCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text
                        let declarations, _ = shapes logicalPath text

                        Expect.sequenceEqual
                            declarations
                            expectedDeclarations
                            "The declarations with prefix and infix signs"

                        Expect.sequenceEqual
                            (declarationBodies (Seq.exactlyOne result.File.Contents).Declarations
                             |> List.collect (fun body ->
                                 infixRanges body
                                 @ applicationRanges body
                             ))
                            expectedRanges
                            "The infix, prefix, and application ranges"
            ]

            testList "a prefix operator the parser does not model stays explicit" [
                for logicalPath, text, oracle in prefixOperatorExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testList "a prefix operator argument reports the Oracle diagnostics" [
                for logicalPath, text, oracle in prefixArgumentDiagnosticCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.sequenceEqual
                            (oracleLines logicalPath result)
                            oracle
                            "The diagnostics match the Compatibility Oracle"
            ]

            testList "a token after a delimiter that closes left of its block item stays explicit" [
                for logicalPath, text, oracle in undentedItemCloseExplicitCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result)
            ]

            testCase
                "an else or a clause bar left of the inner construct inside a delimiter belongs to the outer construct"
            <| fun _ ->
                let parenthesized logicalPath text =
                    let result = parse logicalPath text

                    Expect.isEmpty
                        (oracleLines logicalPath result)
                        "The Compatibility Oracle reports no diagnostic"

                    match declarationBodies (Seq.exactlyOne result.File.Contents).Declarations with
                    | [ SyntaxExpression.Parenthesized(inner, _) ] -> inner
                    | other -> failtestf "Expected one parenthesized binding body, but got %A" other

                match
                    parenthesized
                        "NestedIfOuterElse.fs"
                        "module A\nlet f a b =\n    (if a then\n        if b then 1\n     else 2)\n"
                with
                | SyntaxExpression.If(_, SyntaxExpression.If(_, _, None, _), Some _, _) -> ()
                | other ->
                    failtestf "The else must belong to the outer if: %s" (expressionShape other)

                match
                    parenthesized
                        "NestedMatchOuterClause.fs"
                        "module A\nlet f a b =\n    (match a with\n     | 1 ->\n         match b with\n         | 2 -> 3\n         | _ -> 4\n     | _ -> 5)\n"
                with
                | SyntaxExpression.Match(_, outerClauses, _) when outerClauses.Length = 2 ->
                    match outerClauses[0].Result with
                    | SyntaxExpression.Match(_, innerClauses, _) ->
                        Expect.equal innerClauses.Length 2 "The inner match keeps its two clauses"
                    | other ->
                        failtestf
                            "The first clause result must be the inner match: %s"
                            (expressionShape other)
                | other ->
                    failtestf
                        "The last clause must belong to the outer match: %s"
                        (expressionShape other)

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
