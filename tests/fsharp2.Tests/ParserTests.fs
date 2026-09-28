namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module ParserTests =
    let private prepare logicalPath text =
        let language =
            LanguageVersion.normalize (Some "10.0")
            |> Result.defaultWith failtest

        SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
        |> LexicalPipeline.prepare language Array.empty

    let private parse logicalPath text =
        prepare logicalPath text
        |> Parser.parseImplementationFile

    let private position (range: SourceRange) =
        range.Start.Line, range.Start.Column, range.End.Line, range.End.Column

    let private oracleLines logicalPath (result: ImplementationFileParseResult) =
        result.Diagnostics
        |> Seq.map (fun diagnostic ->
            $"{logicalPath}({diagnostic.Range.Start.Line},{diagnostic.Range.Start.Column}): error {diagnostic.Code}: {diagnostic.Message}"
        )
        |> Seq.toList

    let rec private declarationShape declaration =
        match declaration with
        | ImplementationDeclaration.Open(name, _) -> $"open {name.Text}"
        | ImplementationDeclaration.Let(_, bindings, _) ->
            bindings
            |> Seq.map (fun binding ->
                let head =
                    match binding.Head with
                    | SyntaxPattern.Named identifier -> identifier.Text
                    | other -> string other

                $"let {head}/{binding.Parameters.Length}"
            )
            |> String.concat " and "
        | ImplementationDeclaration.NestedModule(name, declarations, _) ->
            let inner =
                declarations
                |> Seq.map declarationShape
                |> String.concat "; "

            $"module {name.Text} = [{inner}]"
        | ImplementationDeclaration.Skipped _ -> "skipped"

    [<Tests>]
    let tests =
        testList "Issue29.Parser" [
            testCase "namespaces retain opens, nested modules, and ordered let declarations"
            <| fun _ ->
                let text =
                    "namespace Sample.Core

open System
open System.Collections.Generic

module Values =
    let answer = 42
    let add left right = left + right * 2
    let pair = (1, \"two\")

module Later =
    let result = Values.add 1 (Values.add 2 3)
"

                let result = parse "Containers.fs" text

                Expect.equal
                    (oracleLines "Containers.fs" result)
                    []
                    "The Compatibility Oracle reports no parse diagnostics for this file"

                let contents = Expect.wantSome (Seq.tryExactlyOne result.File.Contents) "One root"

                Expect.equal contents.Kind ModuleOrNamespaceKind.Namespace "Namespace root"

                Expect.equal
                    (contents.Name
                     |> Option.map _.Text)
                    (Some "Sample.Core")
                    "Namespace name"

                Expect.sequenceEqual
                    (contents.Declarations
                     |> Seq.map declarationShape)
                    [
                        "open System"
                        "open System.Collections.Generic"
                        "module Values = [let answer/0; let add/2; let pair/0]"
                        "module Later = [let result/0]"
                    ]
                    "Ordered declarations"

                Expect.equal (position contents.Range) (1, 1, 12, 47) "Root range"

                Expect.sequenceEqual
                    (contents.Declarations
                     |> Seq.map (fun declaration -> position declaration.Range))
                    [
                        3, 1, 3, 12
                        4, 1, 4, 32
                        6, 1, 9, 26
                        11, 1, 12, 47
                    ]
                    "Declaration ranges"

            testCase "expressions follow application, infix precedence, tuple, and constant rules"
            <| fun _ ->
                let result =
                    parse "Expressions.fs" "module Program\nlet value = f x + g (1, y) * 2 - 3\n"

                Expect.equal (oracleLines "Expressions.fs" result) [] "No parse diagnostics"

                let contents = Seq.exactlyOne result.File.Contents

                Expect.equal contents.Kind ModuleOrNamespaceKind.NamedModule "Named module root"

                let binding =
                    match Seq.exactlyOne contents.Declarations with
                    | ImplementationDeclaration.Let(false, bindings, _) -> Seq.exactlyOne bindings
                    | other -> failtest $"Expected one let declaration, found {other}"

                let rec shape expression =
                    match expression with
                    | SyntaxExpression.Constant(constant, _) ->
                        match constant with
                        | SyntaxConstant.Numeric text -> text
                        | other -> string other
                    | SyntaxExpression.Identifier name -> name.Text
                    | SyntaxExpression.Parenthesized(inner, _) -> $"({shape inner})"
                    | SyntaxExpression.Tuple(items, _) ->
                        items
                        |> Seq.map shape
                        |> String.concat ", "
                    | SyntaxExpression.Application(func, argument, _) ->
                        $"[{shape func} {shape argument}]"
                    | SyntaxExpression.Infix(operator, left, right, _) ->
                        $"{{{shape left} {operator.Text} {shape right}}}"
                    | SyntaxExpression.Missing _ -> "<missing>"

                Expect.equal
                    (shape binding.Body)
                    "{{[f x] + {[g (1, y)] * 2}} - 3}"
                    "Precedence and associativity"

                Expect.equal (position binding.Body.Range) (2, 13, 2, 35) "Body range"
                Expect.equal (position binding.Range) (2, 5, 2, 35) "Binding range"
        ]
