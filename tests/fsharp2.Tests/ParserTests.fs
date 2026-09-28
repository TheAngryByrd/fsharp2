namespace fsharp2.Tests

open System.Collections.Immutable
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

    let private oracleLines logicalPath (diagnostics: ImmutableArray<SyntaxDiagnostic>) =
        diagnostics
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
                    | SyntaxPattern.Missing _ -> "<missing>"
                    | other -> string other

                let body =
                    match binding.Body with
                    | SyntaxExpression.Missing _ -> " = <missing>"
                    | _ -> ""

                let skipped = if binding.Skipped.IsSome then " (skipped)" else ""

                $"let {head}/{binding.Parameters.Length}{body}{skipped}"
            )
            |> String.concat " and "
        | ImplementationDeclaration.NestedModule(name, declarations, _) ->
            let inner =
                declarations
                |> Seq.map declarationShape
                |> String.concat "; "

            $"module {name.Text} = [{inner}]"
        | ImplementationDeclaration.Skipped _ -> "skipped"

    let private parseSignature logicalPath text =
        prepare logicalPath text
        |> Parser.parseSignatureFile

    let rec private typeShape syntaxType =
        match syntaxType with
        | SyntaxType.LongIdentifier name -> name.Text
        | SyntaxType.Variable variable -> variable.Text
        | SyntaxType.Application(typeConstructor, arguments, true, _) ->
            let arguments =
                arguments
                |> Seq.map typeShape
                |> String.concat ", "

            $"{arguments} {typeShape typeConstructor}"
        | SyntaxType.Application(typeConstructor, arguments, false, _) ->
            let arguments =
                arguments
                |> Seq.map typeShape
                |> String.concat ", "

            $"{typeShape typeConstructor}<{arguments}>"
        | SyntaxType.Function(argument, result, _) ->
            $"({typeShape argument} -> {typeShape result})"
        | SyntaxType.Tuple(elements, _) ->
            let elements =
                elements
                |> Seq.map typeShape
                |> String.concat " * "

            $"({elements})"
        | SyntaxType.Parenthesized(inner, _) -> typeShape inner
        | SyntaxType.SignatureParameter(name, parameterType, _) ->
            $"{name.Text}: {typeShape parameterType}"
        | SyntaxType.Missing _ -> "<missing>"

    let rec private signatureShape declaration =
        match declaration with
        | SignatureDeclaration.Open(name, _) -> $"open {name.Text}"
        | SignatureDeclaration.Val value ->
            let name =
                value.Name
                |> Option.map _.Text
                |> Option.defaultValue "<missing>"

            let skipped = if value.Skipped.IsSome then " (skipped)" else ""

            $"val {name}: {typeShape value.Type}{skipped}"
        | SignatureDeclaration.NestedModule(name, declarations, _) ->
            let inner =
                declarations
                |> Seq.map signatureShape
                |> String.concat "; "

            $"module {name.Text} = [{inner}]"
        | SignatureDeclaration.Skipped _ -> "skipped"

    let private signatureRecoveryCases = [
        "TypeStart.fsi",
        "module Program\n\nval broken: )\nval first: int\n",
        [ "TypeStart.fsi(3,13): error FS0010: Unexpected symbol ')' in value signature" ],
        [
            "val broken: <missing> (skipped)"
            "val first: int"
        ]

        "TypeAfterArrow.fsi",
        "module Program\nval broken: int -> )\nval tuple: int * ]\nval first: int\n",
        [ "TypeAfterArrow.fsi(2,20): error FS0010: Unexpected symbol ')' in value signature" ],
        [
            "val broken: (int -> <missing>) (skipped)"
            "val tuple: (int * <missing>) (skipped)"
            "val first: int"
        ]

        "Name.fsi",
        "module Program\nval ): int\nval end: int\nval first: int\n",
        [
            "Name.fsi(2,5): error FS0010: Unexpected symbol ')' in value signature. Expected identifier, '(', '(*)' or other token."
        ],
        [
            "val <missing>: <missing> (skipped)"
            "val <missing>: <missing> (skipped)"
            "val first: int"
        ]

        "Colon.fsi",
        "module Program\nval broken int\nval other )\nval first: int\n",
        [
            "Colon.fsi(2,12): error FS0010: Unexpected identifier in value signature. Expected ':' or other token."
        ],
        [
            "val broken: <missing> (skipped)"
            "val other: <missing> (skipped)"
            "val first: int"
        ]

        "Trailing.fsi",
        "module Program\nval broken: int list )\nval other: int end\nval first: int\n",
        [
            "Trailing.fsi(2,22): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "val broken: int list (skipped)"
            "val other: int (skipped)"
            "val first: int"
        ]

        "DeclarationStart.fsi",
        "module Program\nval f: int\n]\nval first: int\n",
        [
            "DeclarationStart.fsi(3,1): error FS0010: Unexpected symbol ']'. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "val f: int"
            "skipped"
            "val first: int"
        ]

        "Incomplete.fsi",
        "module Program\nval broken:\nval arrow: int ->   \nval first: int\n",
        [
            "Incomplete.fsi(2,13): error FS0010: Incomplete structured construct at or before this point in value signature"
            "Incomplete.fsi(3,19): error FS0010: Incomplete structured construct at or before this point in value signature"
        ],
        [
            "val broken: <missing>"
            "val arrow: (int -> <missing>)"
            "val first: int"
        ]

        "OpenIncomplete.fsi",
        "module Program\nopen   \nval first: int\n",
        [
            "OpenIncomplete.fsi(2,6): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ],
        [ "val first: int" ]

        "NestedModule.fsi",
        "namespace Sample\nmodule Inner =\n    val broken: }\n    val first: int\nmodule Later =\n    val second: int\n",
        [ "NestedModule.fsi(3,17): error FS0010: Unexpected symbol '}' in value signature" ],
        [
            "module Inner = [val broken: <missing> (skipped); val first: int]"
            "module Later = [val second: int]"
        ]

        "SiblingModules.fsi",
        "namespace Sample\nmodule First =\n    val a: )\n    val b: )\n    val c: int\nmodule Second =\n    val d: ]\n",
        [
            "SiblingModules.fsi(3,12): error FS0010: Unexpected symbol ')' in value signature"
            "SiblingModules.fsi(4,12): error FS0010: Unexpected symbol ')' in value signature"
            "SiblingModules.fsi(7,12): error FS0010: Unexpected symbol ']' in value signature"
        ],
        [
            "module First = [val a: <missing> (skipped); val b: <missing> (skipped); val c: int]"
            "module Second = [val d: <missing> (skipped)]"
        ]

        "NestedTrailing.fsi",
        "namespace N\nmodule M =\n    val a: int )\n    val c: )\n",
        [
            "NestedTrailing.fsi(3,16): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "NestedTrailing.fsi(4,12): error FS0010: Unexpected symbol ')' in value signature"
        ],
        [ "module M = [val a: int (skipped); val c: <missing> (skipped)]" ]

        "NestedDeclarationStart.fsi",
        "namespace N\nmodule M =\n    val a: int\n    )\n    val c: int )\n",
        [
            "NestedDeclarationStart.fsi(4,5): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "NestedDeclarationStart.fsi(5,16): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
        ],
        [ "module M = [val a: int; skipped; val c: int (skipped)]" ]
    ]

    let private bindingRecoveryCases = [
        "OpenIncomplete.fs",
        "module Program\nopen\nlet first = 1\n",
        [
            "OpenIncomplete.fs(2,6): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ],
        [ "let first/0" ]

        "DefinitionDiscards.fs",
        "module Program\nlet a = 1\n)\nlet b = 1\nlet c = )\n",
        [
            "DefinitionDiscards.fs(3,1): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "let a/0"
            "skipped"
            "let b/0"
            "let c/0 = <missing> (skipped)"
        ]

        "BodyStart.fs",
        "module Program\n\nlet broken = )\n\nlet first = 1\n\nlet second = first\n",
        [ "BodyStart.fs(3,14): error FS0010: Unexpected symbol ')' in binding" ],
        [
            "let broken/0 = <missing> (skipped)"
            "let first/0"
            "let second/0"
        ]

        "Consecutive.fs",
        "module Program\nlet broken = )\nlet other = ]\nlet first = 1\n",
        [
            "Consecutive.fs(2,14): error FS0010: Unexpected symbol ')' in binding"
            "Consecutive.fs(3,13): error FS0010: Unexpected symbol ']' in binding"
        ],
        [
            "let broken/0 = <missing> (skipped)"
            "let other/0 = <missing> (skipped)"
            "let first/0"
        ]

        "BlockBody.fs",
        "module Program\nlet f =\n    )\nlet first = 1\n",
        [ "BlockBody.fs(3,5): error FS0010: Unexpected symbol ')' in binding" ],
        [
            "let f/0 = <missing> (skipped)"
            "let first/0"
        ]

        "HeadStart.fs",
        "module Program\nlet ) = 1\nlet first = 1\n",
        [ "HeadStart.fs(2,5): error FS0010: Unexpected symbol ')' in binding" ],
        [
            "let <missing>/0 = <missing> (skipped)"
            "let first/0"
        ]

        "Keywords.fs",
        "module Program\nlet broken = end\nlet guarded = private\nlet first = 1\n",
        [
            "Keywords.fs(2,14): error FS0010: Unexpected keyword 'end' in binding"
            "Keywords.fs(3,15): error FS0010: Unexpected keyword 'private' in binding"
        ],
        [
            "let broken/0 = <missing> (skipped)"
            "let guarded/0 = <missing> (skipped)"
            "let first/0"
        ]

        "MissingEquals.fs",
        "module Program\nlet f x ) = 1\nlet first = 1\n",
        [
            "MissingEquals.fs(2,9): error FS0010: Unexpected symbol ')' in binding. Expected '=' or other token."
        ],
        [
            "let f/1 = <missing> (skipped)"
            "let first/0"
        ]

        "Trailing.fs",
        "module Program\nlet f x = g x ]\nlet first = 1\n",
        [
            "Trailing.fs(2,15): error FS0010: Unexpected symbol ']' in binding. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "let f/1 (skipped)"
            "let first/0"
        ]

        "MixedRules.fs",
        "module Program\nlet f x ] = 1\nlet g = 2 }\nlet first = 1\n",
        [
            "MixedRules.fs(2,9): error FS0010: Unexpected symbol ']' in binding. Expected '=' or other token."
            "MixedRules.fs(3,11): error FS0010: Unexpected symbol '}' in binding. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "let f/1 = <missing> (skipped)"
            "let g/0 (skipped)"
            "let first/0"
        ]

        "DeclarationStart.fs",
        "module Program\nlet f x = 1\n)\nlet first = 1\n",
        [
            "DeclarationStart.fs(3,1): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "let f/1"
            "skipped"
            "let first/0"
        ]

        "NestedModule.fs",
        "namespace Sample\n\nmodule Inner =\n    let broken = }\n    let first = 1\n\nmodule Later =\n    let second = 2\n",
        [ "NestedModule.fs(4,18): error FS0010: Unexpected symbol '}' in binding" ],
        [
            "module Inner = [let broken/0 = <missing> (skipped); let first/0]"
            "module Later = [let second/0]"
        ]

        "AccessibilityHead.fs",
        "module Program\nlet private = 1\nlet first = 1\n",
        [ "AccessibilityHead.fs(2,13): error FS0010: Unexpected symbol '=' in binding" ],
        [
            "let <missing>/0 = <missing> (skipped)"
            "let first/0"
        ]

        "RecursiveGroup.fs",
        "module Program\nlet rec f x = )\nand g = 1\nlet first = 1\n",
        [ "RecursiveGroup.fs(2,15): error FS0010: Unexpected symbol ')' in binding" ],
        [
            "let f/1 = <missing> (skipped) and let g/0"
            "let first/0"
        ]
    ]

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
                    (oracleLines "Containers.fs" result.Diagnostics)
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

                Expect.equal
                    (oracleLines "Expressions.fs" result.Diagnostics)
                    []
                    "No parse diagnostics"

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

            testCase "let bindings retain accessibility with its exact range"
            <| fun _ ->
                let result =
                    parse
                        "Access.fs"
                        "module Program\nlet private seed = 40\nlet internal f x = x\nlet public answer = f seed\n"

                Expect.equal (oracleLines "Access.fs" result.Diagnostics) [] "No parse diagnostics"

                Expect.sequenceEqual
                    ((Seq.exactlyOne result.File.Contents).Declarations
                     |> Seq.collect (fun declaration ->
                         match declaration with
                         | ImplementationDeclaration.Let(_, bindings, _) -> bindings
                         | other -> failtest $"Expected let declarations, found {other}"
                     )
                     |> Seq.map (fun binding ->
                         binding.Accessibility
                         |> Option.map (fun access -> access.Kind, position access.Range),
                         declarationShape (
                             ImplementationDeclaration.Let(
                                 false,
                                 ImmutableArray.Create binding,
                                 binding.Range
                             )
                         )
                     ))
                    [
                        Some(SyntaxAccessibility.Private, (2, 5, 2, 12)), "let seed/0"
                        Some(SyntaxAccessibility.Internal, (3, 5, 3, 13)), "let f/1"
                        Some(SyntaxAccessibility.Public, (4, 5, 4, 11)), "let answer/0"
                    ]
                    "Accessibility"

            testList "binding and definition recovery keeps later declarations" [
                for logicalPath, text, oracle, declarations in bindingRecoveryCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.sequenceEqual
                            (oracleLines logicalPath result.Diagnostics)
                            oracle
                            "The diagnostics must match the Compatibility Oracle"

                        Expect.sequenceEqual
                            (result.File.Contents
                             |> Seq.collect _.Declarations
                             |> Seq.map declarationShape)
                            declarations
                            "Recovery must keep each later declaration"
            ]

            testCase "signature files retain opens, nested modules, value types, and accessibility"
            <| fun _ ->
                let text =
                    "namespace Sample.Core

open System

module Values =
    val answer: int
    val add: left: int -> right: int -> int
    val pair: int * string
    val map: ('a -> 'b) -> 'a list -> list<'b>
    val private nested: Map<string, list<int>> -> unit
"

                let result = parseSignature "Signatures.fsi" text

                Expect.equal
                    (oracleLines "Signatures.fsi" result.Diagnostics)
                    []
                    "The Compatibility Oracle reports no parse diagnostics for this file"

                let contents = Seq.exactlyOne result.File.Contents

                Expect.equal contents.Kind ModuleOrNamespaceKind.Namespace "Namespace root"

                Expect.sequenceEqual
                    (contents.Declarations
                     |> Seq.map signatureShape)
                    [
                        "open System"
                        "module Values = [val answer: int; val add: (left: int -> (right: int -> int)); val pair: (int * string); val map: (('a -> 'b) -> ('a list -> list<'b>)); val nested: (Map<string, list<int>> -> unit)]"
                    ]
                    "Ordered signature declarations"

                let values =
                    match contents.Declarations[1] with
                    | SignatureDeclaration.NestedModule(_, declarations, range) ->
                        Expect.equal (position range) (5, 1, 10, 55) "Nested module range"

                        declarations
                        |> Seq.map (fun declaration ->
                            match declaration with
                            | SignatureDeclaration.Val value -> value
                            | other -> failtest $"Expected a value signature, found {other}"
                        )
                        |> Seq.toArray
                    | other -> failtest $"Expected a nested module, found {other}"

                Expect.sequenceEqual
                    (values
                     |> Seq.map (fun value -> position value.Range, position value.Type.Range))
                    [
                        (6, 5, 6, 20), (6, 17, 6, 20)
                        (7, 5, 7, 44), (7, 14, 7, 44)
                        (8, 5, 8, 27), (8, 15, 8, 27)
                        (9, 5, 9, 47), (9, 14, 9, 47)
                        (10, 5, 10, 55), (10, 25, 10, 55)
                    ]
                    "Value and type ranges"

                Expect.equal
                    (values[4].Accessibility
                     |> Option.map _.Kind)
                    (Some SyntaxAccessibility.Private)
                    "Private value"

            testList "signature recovery keeps later declarations" [
                for logicalPath, text, oracle, declarations in signatureRecoveryCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parseSignature logicalPath text

                        Expect.sequenceEqual
                            (oracleLines logicalPath result.Diagnostics)
                            oracle
                            "The diagnostics must match the Compatibility Oracle"

                        Expect.sequenceEqual
                            (result.File.Contents
                             |> Seq.collect _.Declarations
                             |> Seq.map signatureShape)
                            declarations
                            "Recovery must keep each later declaration"
            ]

            testCase "recovery nodes keep the exact missing and skipped ranges"
            <| fun _ ->
                let result = parse "Ranges.fs" "module Program\nlet broken = |]\nlet first = 1\n"

                let binding =
                    match Seq.head (Seq.exactlyOne result.File.Contents).Declarations with
                    | ImplementationDeclaration.Let(_, bindings, _) -> Seq.exactlyOne bindings
                    | other -> failtest $"Expected a let declaration, found {other}"

                Expect.equal (position binding.Body.Range) (2, 14, 2, 14) "Missing body range"

                let skipped = Expect.wantSome binding.Skipped "Skipped tokens"

                Expect.equal (position skipped.Range) (2, 14, 2, 16) "Skipped range"

                Expect.sequenceEqual
                    (skipped.Tokens
                     |> Seq.map _.Text)
                    [
                        "|"
                        "]"
                    ]
                    "Skipped tokens"
        ]
