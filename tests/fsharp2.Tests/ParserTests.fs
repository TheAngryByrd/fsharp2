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

    let private rootShapes shape (root: ModuleOrNamespaceSyntax<'Declaration>) =
        Seq.append
            (root.Declarations
             |> Seq.map shape)
            (root.DiscardedByRecovery
             |> Seq.map (fun declaration -> $"discarded: {shape declaration}"))

    let private rootShape kind =
        match kind with
        | ModuleOrNamespaceKind.AnonymousModule -> "anonymous module"
        | ModuleOrNamespaceKind.NamedModule name -> $"module {name.Text}"
        | ModuleOrNamespaceKind.Namespace(Some name) -> $"namespace {name.Text}"
        | ModuleOrNamespaceKind.Namespace None -> "namespace <missing>"

    let private position (range: SourceRange) =
        range.Start.Line, range.Start.Column, range.End.Line, range.End.Column

    let private oracleLines logicalPath (diagnostics: ImmutableArray<SyntaxDiagnostic>) =
        diagnostics
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
        | SyntaxType.Array(element, rank, _) ->
            $"{typeShape element}[{System.String(',', rank - 1)}]"
        | SyntaxType.SignatureParameter(name, parameterType, _) ->
            $"{name.Text}: {typeShape parameterType}"
        | SyntaxType.Missing _ -> "<missing>"

    let private openTargetShape target =
        match target with
        | SyntaxOpenTarget.ModuleOrNamespace name -> name.Text
        | SyntaxOpenTarget.GlobalModuleOrNamespace(_, None) -> "global"
        | SyntaxOpenTarget.GlobalModuleOrNamespace(_, Some name) -> $"global.{name.Text}"
        | SyntaxOpenTarget.Type syntaxType -> $"type {typeShape syntaxType}"

    let rec private declarationShape declaration =
        match declaration with
        | ImplementationDeclaration.Open(target, _) -> $"open {openTargetShape target}"
        | ImplementationDeclaration.Let(_, _, bindings, _) ->
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
        | ImplementationDeclaration.NestedModule nested ->
            let inner =
                Seq.append
                    (nested.Declarations
                     |> Seq.map declarationShape)
                    (nested.DiscardedByRecovery
                     |> Seq.map (fun declaration -> $"discarded: {declarationShape declaration}"))
                |> String.concat "; "

            $"module {nested.Name.Text} = [{inner}]"
        | ImplementationDeclaration.Type group ->
            Seq.append [ group.First ] group.Rest
            |> Seq.map (fun definition -> $"type {definition.Name.Text}")
            |> String.concat " and "
        | ImplementationDeclaration.Do(attributes, _, _) ->
            $"do [{attributes.Length} attribute lists]"
        | ImplementationDeclaration.Expression(attributes, _, _, _) ->
            $"expr [{attributes.Length} attribute lists]"
        | ImplementationDeclaration.Skipped _ -> "skipped"

    let private trailingDotOpenCase logicalPath (declaration: string) expectedShape =
        testCase logicalPath
        <| fun _ ->
            let result =
                parse
                    logicalPath
                    $"module Program
{declaration}
"

            Expect.sequenceEqual
                (oracleLines logicalPath result.Diagnostics)
                [
                    $"{logicalPath}(2,12): error FS3117: Unexpected end of type. Expected a name after this point."
                ]
                "Compatibility Oracle diagnostics"

            Expect.sequenceEqual
                (result.Diagnostics
                 |> Seq.map (fun diagnostic -> position diagnostic.Range))
                [ 2, 12, 2, 13 ]
                "Diagnostic range"

            let declarations = (Seq.exactlyOne result.File.Contents).Declarations

            Expect.sequenceEqual
                (declarations
                 |> Seq.map declarationShape)
                [ expectedShape ]
                "Open target"

            Expect.sequenceEqual
                (declarations
                 |> Seq.map (fun declaration -> position declaration.Range))
                [ 2, 1, 2, 13 ]
                "Open declaration range"

    let private parseSignature logicalPath text =
        prepare logicalPath text
        |> Parser.parseSignatureFile

    let rec private signatureShape declaration =
        match declaration with
        | SignatureDeclaration.Open(target, _) -> $"open {openTargetShape target}"
        | SignatureDeclaration.Val value ->
            let name =
                value.Name
                |> Option.map _.Text
                |> Option.defaultValue "<missing>"

            let skipped = if value.Skipped.IsSome then " (skipped)" else ""

            $"val {name}: {typeShape value.Type}{skipped}"
        | SignatureDeclaration.NestedModule nested ->
            let inner =
                Seq.append
                    (nested.Declarations
                     |> Seq.map signatureShape)
                    (nested.DiscardedByRecovery
                     |> Seq.map (fun declaration -> $"discarded: {signatureShape declaration}"))
                |> String.concat "; "

            $"module {nested.Name.Text} = [{inner}]"
        | SignatureDeclaration.Skipped _ -> "skipped"

    let private signatureRecoveryCases = [
        "EndWithoutNewline.fsi",
        "module Program\nval broken:",
        [
            "EndWithoutNewline.fsi(2,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ],
        [ "val broken: <missing>" ]

        "EndAfterNewline.fsi",
        "module Program\nval broken:\n",
        [
            "EndAfterNewline.fsi(3,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ],
        [ "val broken: <missing>" ]

        "EndAfterArrow.fsi",
        "module Program\nval broken: int ->",
        [
            "EndAfterArrow.fsi(2,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ],
        [ "val broken: (int -> <missing>)" ]

        "RecoveryContinuesAcrossOpen.fsi",
        "module Program\nval a: )\nopen System\nval c: )\n",
        [
            "RecoveryContinuesAcrossOpen.fsi(2,8): error FS0010: Unexpected symbol ')' in value signature"
        ],
        [
            "val a: <missing> (skipped)"
            "discarded: open System"
            "discarded: val c: <missing> (skipped)"
        ]

        "TypeStart.fsi",
        "module Program\n\nval broken: )\nval first: int\n",
        [ "TypeStart.fsi(3,13): error FS0010: Unexpected symbol ')' in value signature" ],
        [
            "val broken: <missing> (skipped)"
            "discarded: val first: int"
        ]

        "TypeAfterArrow.fsi",
        "module Program\nval broken: int -> )\nval tuple: int * ]\nval first: int\n",
        [ "TypeAfterArrow.fsi(2,20): error FS0010: Unexpected symbol ')' in value signature" ],
        [
            "val broken: (int -> <missing>) (skipped)"
            "discarded: val tuple: (int * <missing>) (skipped)"
            "discarded: val first: int"
        ]

        "Name.fsi",
        "module Program\nval ): int\nval end: int\nval first: int\n",
        [
            "Name.fsi(2,5): error FS0010: Unexpected symbol ')' in value signature. Expected identifier, '(', '(*)' or other token."
        ],
        [
            "val <missing>: <missing> (skipped)"
            "discarded: val <missing>: <missing> (skipped)"
            "discarded: val first: int"
        ]

        "Colon.fsi",
        "module Program\nval broken int\nval other )\nval first: int\n",
        [
            "Colon.fsi(2,12): error FS0010: Unexpected identifier in value signature. Expected ':' or other token."
        ],
        [
            "val broken: <missing> (skipped)"
            "discarded: val other: <missing> (skipped)"
            "discarded: val first: int"
        ]

        "Trailing.fsi",
        "module Program\nval broken: int list )\nval other: int end\nval first: int\n",
        [
            "Trailing.fsi(2,22): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "val broken: int list (skipped)"
            "discarded: val other: int (skipped)"
            "discarded: val first: int"
        ]

        "DeclarationStart.fsi",
        "module Program\nval f: int\n]\nval first: int\n",
        [
            "DeclarationStart.fsi(3,1): error FS0010: Unexpected symbol ']'. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "val f: int"
            "skipped"
            "discarded: val first: int"
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

        "NestedFirstSignature.fsi",
        "module M\nmodule N =\n    )\nval b: int\n",
        [ "NestedFirstSignature.fsi(3,5): error FS0010: Unexpected symbol ')' in signature file" ],
        [
            "module N = [skipped]"
            "val b: int"
        ]

        "NestedFirstSignatureThenValues.fsi",
        "module M\nmodule N =\n    )\n    val b: )\nval c: )\n",
        [
            "NestedFirstSignatureThenValues.fsi(3,5): error FS0010: Unexpected symbol ')' in signature file"
            "NestedFirstSignatureThenValues.fsi(4,12): error FS0010: Unexpected symbol ')' in value signature"
            "NestedFirstSignatureThenValues.fsi(5,8): error FS0010: Unexpected symbol ')' in value signature"
        ],
        [
            "module N = [skipped; val b: <missing> (skipped)]"
            "val c: <missing> (skipped)"
        ]

        "AnonymousSignature.fsi",
        "val a: int\n)\n",
        [ "AnonymousSignature.fsi(2,1): error FS0010: Unexpected symbol ')' in signature file" ],
        [
            "val a: int"
            "skipped"
        ]

        "NamespaceSignature.fsi",
        "namespace A\nval a: int\n)\n",
        [
            "NamespaceSignature.fsi(3,1): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ],
        [
            "val a: int"
            "skipped"
        ]

        "IndentedRoot.fsi",
        "namespace N\n    val a: )\n    val b: )\n    val c: int\n",
        [ "IndentedRoot.fsi(2,12): error FS0010: Unexpected symbol ')' in value signature" ],
        [
            "val a: <missing> (skipped)"
            "discarded: val b: <missing> (skipped)"
            "discarded: val c: int"
        ]

        "NestedDeclarationStart.fsi",
        "namespace N\nmodule M =\n    val a: int\n    )\n    val c: int )\n",
        [
            "NestedDeclarationStart.fsi(4,5): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "NestedDeclarationStart.fsi(5,16): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
        ],
        [ "module M = [val a: int; skipped; val c: int (skipped)]" ]
    ]

    let private tokenClassCases =
        let symbol text = $"symbol '{text}'"
        let keyword text = $"keyword '{text}'"

        let closing = [
            symbol "|]"
            symbol ">]"
            keyword "end"
            keyword "done"
            keyword "of"
            keyword "elif"
        ]

        let tokenText (description: string) = description.Split('\'')[1]

        [
            "BindingBody",
            "module Program\n\nlet broken = {0}\n\nlet first = 1\n",
            (3, 14),
            "in binding",
            closing
            @ [
                symbol ")"
                symbol "]"
                symbol "}"
                symbol "="
                symbol ";"
                symbol ":"
                symbol "|"
                symbol "."
                keyword "finally"
                keyword "to"
                keyword "when"
                keyword "as"
                keyword "private"
            ]

            "BindingHead",
            "module Program\nlet {0} = 1\nlet first = 1\n",
            (2, 5),
            "in binding",
            closing
            @ [
                symbol "="
                symbol ";"
                symbol ":"
                symbol "."
                symbol "|"
                keyword "as"
                keyword "to"
                keyword "when"
                keyword "finally"
            ]

            "BindingEquals",
            "module Program\nlet f x {0} = 1\nlet first = 1\n",
            (2, 9),
            "in binding. Expected '=' or other token.",
            closing

            "BindingEnd",
            "module Program\nlet f x = g x {0}\nlet first = 1\n",
            (2, 15),
            "in binding. Expected incomplete structured construct at or before this point or other token.",
            closing

            "DefinitionStart",
            "module Program\nlet f x = 1\n{0}\nlet first = 1\n",
            (3, 1),
            "in definition. Expected incomplete structured construct at or before this point or other token.",
            closing
            @ [
                symbol "="
                symbol ":"
                symbol "."
            ]
        ]
        |> List.collect (fun (point, template, (line, column), suffix, descriptions) ->
            descriptions
            |> List.map (fun description ->
                let logicalPath = $"{point}.fs"
                let text = System.String.Format(template, tokenText description)

                logicalPath,
                text,
                $"{logicalPath}({line},{column}): error FS0010: Unexpected {description} {suffix}"
            )
        )

    let private unsupportedCases = [
        "TypeApplication.fs", "module Program\nlet y = f<int> x\n", []
        "PrefixMinus.fs", "module Program\nlet y = f -1\n", []
        "ModuleThenNamespace.fs",
        "module Program\nlet x = 1\nnamespace N\nlet y = 2\n",
        [
            "ModuleThenNamespace.fs(3,1): error FS0010: Unexpected keyword 'namespace' in definition. Expected incomplete structured construct at or before this point or other token."
            "ModuleThenNamespace.fs(1,1): error FS0530: Only '#' compiler directives may occur prior to the first 'namespace' declaration"
        ]
        "ParenthesisClosedByBracket.fs",
        "module Program\nlet f = (]\nlet y = 2\n",
        [
            "ParenthesisClosedByBracket.fs(2,10): error FS0010: Unexpected symbol ']' in binding"
            "ParenthesisClosedByBracket.fs(2,9): error FS0583: Unmatched '('"
        ]
    ]

    let private unsupportedAfterOracleCases = [
        "ParenthesizedDot.fs",
        "module M\nlet x = (A.",
        [
            "ParenthesizedDot.fs(2,11): error FS0599: Missing qualification after '.'"
            "ParenthesizedDot.fs(2,9): error FS0583: Unmatched '('"
        ]
    ]

    let private definitionRecovery = "module Program\nlet a = 1\n)\n"

    let private definitionRecoveryOracle logicalPath =
        $"{logicalPath}(3,1): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."

    let private implementationCascadeCases = [
        "LetRecAnd.fs", "let rec f x = g x\nand g y = )\n", 1
        "AttributedLet.fs", "[<Literal>]\nlet f = )\n", 1
        "NestedModule.fs", "module Inner =\n    let f = )\n", 1
        "Open.fs", "open System\nlet f = )\n", 2
        "BlockError.fs", "let f x =\n  )\n", 1
        "Type.fs", "type T = int\nlet f = )\n", 2
    ]

    let private implementationCascadeUnsupportedCases = [
        "LetBlockBody.fs", "let f =\n    let inner = 1\n    )\nlet z = 1\n"
        "Do.fs", "do )\n"
    ]

    let private valueRecovery = "module Program\nval a: )\n"

    let private valueRecoveryOracle logicalPath =
        $"{logicalPath}(2,8): error FS0010: Unexpected symbol ')' in value signature"

    let private signatureCascadeCases = [
        "AttributedVal.fsi", "[<Literal>]\nval b: )\n", 1
        "PrivateVal.fsi", "val private b: int -> )\n", 1
        "IncompleteOpen.fsi", "open System\nopen\nval c: int\n", 2
        "IncompleteVal.fsi", "val b:\nval c: int\n", 2
    ]

    let private unsupportedSignatureCases = [
        "NestedModuleSig.fsi",
        valueRecovery
        + "module Inner =\n    val b: )\n",
        [
            valueRecoveryOracle "NestedModuleSig.fsi"
            "NestedModuleSig.fsi(5,1): error FS0010: Incomplete structured construct at or before this point in signature file"
        ]

        "ValueThenLet.fsi",
        "module Program\nval a: )\nlet b = 1\n",
        [
            "ValueThenLet.fsi(2,8): error FS0010: Unexpected symbol ')' in value signature"
            "ValueThenLet.fsi(4,1): error FS0010: Incomplete structured construct at or before this point in signature file"
        ]
    ]

    let private bindingRecoveryCases = [
        "TypeThenLet.fs",
        "module Program\ntype T = int\nlet c = )\n",
        [ "TypeThenLet.fs(3,9): error FS0010: Unexpected symbol ')' in binding" ],
        [
            "type T"
            "let c/0 = <missing> (skipped)"
        ]

        "TrailingDotOpen.fs",
        "module Program\nopen System.\nlet y = 2\n",
        [
            "TrailingDotOpen.fs(2,14): error FS0010: Incomplete structured construct at or before this point in open declaration"
        ],
        [
            "open System"
            "let y/0"
        ]

        "TrailingDotExpression.fs",
        "module Program\nlet x = a.\nlet y = 1\n",
        [ "TrailingDotExpression.fs(2,10): error FS0599: Missing qualification after '.'" ],
        [
            "let x/0"
            "let y/0"
        ]

        "OpenAtEndWithoutNewline.fs",
        "module Program\nopen",
        [
            "OpenAtEndWithoutNewline.fs(2,1): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ],
        []

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
            "discarded: let b/0"
            "discarded: let c/0 = <missing> (skipped)"
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
            "discarded: let first/0"
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

                Expect.equal (rootShape contents.Kind) "namespace Sample.Core" "Namespace root"

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

                Expect.equal (rootShape contents.Kind) "module Program" "Named module root"

                let binding =
                    match Seq.exactlyOne contents.Declarations with
                    | ImplementationDeclaration.Let(_, false, bindings, _) ->
                        Seq.exactlyOne bindings
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
                    | other ->
                        failtest $"Expected an application or infix expression, found {other}"

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
                         | ImplementationDeclaration.Let(_, _, bindings, _) -> bindings
                         | other -> failtest $"Expected let declarations, found {other}"
                     )
                     |> Seq.map (fun binding ->
                         binding.Accessibility
                         |> Option.map (fun access -> access.Kind, position access.Range),
                         declarationShape (
                             ImplementationDeclaration.Let(
                                 SyntaxLetKeyword.Let,
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
                             |> Seq.collect (rootShapes declarationShape))
                            declarations
                            "Recovery must keep each later declaration and mark each declaration that the Oracle discards"
            ]

            testCase
                "assembly attribute lists on do declarations parse in SDK-generated input shapes"
            <| fun _ ->
                let attributeText (attribute: SyntaxAttribute) =
                    let target =
                        attribute.Target
                        |> Option.map (fun target -> $"{target.Text}: ")
                        |> Option.defaultValue ""

                    let argument =
                        match attribute.Argument with
                        | Some(SyntaxExpression.Parenthesized(SyntaxExpression.Tuple(items, _), _)) ->
                            $"({items.Length} arguments)"
                        | Some(SyntaxExpression.Parenthesized _) -> "(1 argument)"
                        | Some other -> string other
                        | None -> ""

                    $"{target}{attribute.Name.Text}{argument}"

                let doAttributes (declaration: ImplementationDeclaration) =
                    match declaration with
                    | ImplementationDeclaration.Do(attributes, body, _) ->
                        Expect.equal
                            body
                            (SyntaxExpression.Constant(SyntaxConstant.Unit, body.Range))
                            "The generated do body is unit"

                        attributes
                        |> Seq.collect _.Attributes
                        |> Seq.map attributeText
                        |> Seq.toList
                    | other -> failtest $"Expected a do declaration, found {other}"

                let targetFramework =
                    parse
                        ".NETCoreApp,Version=v10.0.AssemblyAttributes.fs"
                        "namespace Microsoft.BuildSettings\r\n                [<System.Runtime.Versioning.TargetFrameworkAttribute(\".NETCoreApp,Version=v10.0\", FrameworkDisplayName=\".NET 10.0\")>]\r\n                do ()\r\n"

                Expect.isEmpty
                    targetFramework.Diagnostics
                    "The Compatibility Oracle reports no parse diagnostics"

                let root = Seq.exactlyOne targetFramework.File.Contents

                Expect.equal
                    (rootShape root.Kind)
                    "namespace Microsoft.BuildSettings"
                    "Generated namespace"

                Expect.equal
                    (doAttributes (Seq.exactlyOne root.Declarations))
                    [ "System.Runtime.Versioning.TargetFrameworkAttribute(2 arguments)" ]
                    "Target framework attribute"

                let assemblyInfo =
                    parse
                        "SdkGenerated.AssemblyInfo.fs"
                        "// <auto-generated>\r\n//     Generated by the FSharp WriteCodeFragment class.\r\n// </auto-generated>\r\nnamespace FSharp\r\n\r\nopen System\r\nopen System.Reflection\r\n\r\n\r\n[<assembly: System.Reflection.AssemblyCompanyAttribute(\"SdkGenerated\")>]\r\n[<assembly: System.Reflection.AssemblyConfigurationAttribute(\"Release\")>]\r\n[<assembly: System.Reflection.AssemblyMetadataAttribute(\"RepositoryUrl\", \"https://example.invalid/sdk\")>]\r\n[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo(\"SdkGenerated.Tests\")>]\r\ndo()\r\n\r\n"

                Expect.isEmpty
                    assemblyInfo.Diagnostics
                    "The Compatibility Oracle reports no parse diagnostics"

                let root = Seq.exactlyOne assemblyInfo.File.Contents

                Expect.sequenceEqual
                    (root.Declarations
                     |> Seq.map declarationShape)
                    [
                        "open System"
                        "open System.Reflection"
                        "do [4 attribute lists]"
                    ]
                    "Generated declarations"

                Expect.equal
                    (doAttributes root.Declarations[2])
                    [
                        "assembly: System.Reflection.AssemblyCompanyAttribute(1 argument)"
                        "assembly: System.Reflection.AssemblyConfigurationAttribute(1 argument)"
                        "assembly: System.Reflection.AssemblyMetadataAttribute(2 arguments)"
                        "assembly: System.Runtime.CompilerServices.InternalsVisibleTo(1 argument)"
                    ]
                    "Assembly attributes"

            testCase "attribute lists attach to let bindings and do declarations with exact ranges"
            <| fun _ ->
                let result =
                    parse
                        "Attributes.fs"
                        "module Program\n[<EntryPoint>]\nlet main argv = 0\n[<Literal>]\nlet name = \"x\"\n[<assembly: A; B(1)>]\ndo ()\n"

                Expect.equal
                    (oracleLines "Attributes.fs" result.Diagnostics)
                    []
                    "No parse diagnostics"

                let declarations = (Seq.exactlyOne result.File.Contents).Declarations

                let bindingAttributes index =
                    match declarations[index] with
                    | ImplementationDeclaration.Let(_, _, bindings, range) ->
                        let binding = Seq.head bindings

                        position range,
                        binding.Attributes
                        |> Seq.collect _.Attributes
                        |> Seq.map (fun attribute -> attribute.Name.Text, position attribute.Range)
                        |> Seq.toList
                    | other -> failtest $"Expected a let declaration, found {other}"

                Expect.equal
                    (bindingAttributes 0)
                    ((2, 1, 3, 18), [ "EntryPoint", (2, 3, 2, 13) ])
                    "Entry point attribute"

                Expect.equal
                    (bindingAttributes 1)
                    ((4, 1, 5, 15), [ "Literal", (4, 3, 4, 10) ])
                    "Literal attribute"

                match declarations[2] with
                | ImplementationDeclaration.Do(attributes, _, range) ->
                    Expect.equal (position range) (6, 1, 7, 6) "Do range"

                    let list = Seq.exactlyOne attributes

                    Expect.equal (position list.Range) (6, 1, 6, 22) "Attribute list range"

                    Expect.sequenceEqual
                        (list.Attributes
                         |> Seq.map (fun attribute ->
                             attribute.Target
                             |> Option.map _.Text,
                             attribute.Name.Text,
                             position attribute.Range
                         ))
                        [
                            Some "assembly", "A", (6, 3, 6, 14)
                            None, "B", (6, 16, 6, 20)
                        ]
                        "Attributes in one list"
                | other -> failtest $"Expected a do declaration, found {other}"

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

                Expect.equal (rootShape contents.Kind) "namespace Sample.Core" "Namespace root"

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
                    | SignatureDeclaration.NestedModule nested ->
                        Expect.equal (position nested.Range) (5, 1, 10, 55) "Nested module range"

                        nested.Declarations
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
                             |> Seq.collect (rootShapes signatureShape))
                            declarations
                            "Recovery must keep each later declaration and mark each declaration that the Oracle discards"
            ]

            testList "each token class reports the Oracle message at its recovery point" [
                for logicalPath, text, oracle in tokenClassCases ->
                    testCase text
                    <| fun _ ->
                        let result = parse logicalPath text

                        Expect.sequenceEqual
                            (oracleLines logicalPath result.Diagnostics)
                            [ oracle ]
                            "The diagnostic must match the Compatibility Oracle"
            ]

            testList "unsupported syntax reports explicit diagnostics and no invented FS diagnostic" [
                for logicalPath, text, oracle in
                    unsupportedCases
                    @ unsupportedAfterOracleCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parse logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result.Diagnostics)

                for logicalPath, later in implementationCascadeUnsupportedCases ->
                    testCase $"after recovery: {logicalPath}"
                    <| fun _ ->
                        let result =
                            parse
                                logicalPath
                                (definitionRecovery
                                 + later)

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            [ definitionRecoveryOracle logicalPath ]
                            result.Diagnostics
                            (oracleLines logicalPath result.Diagnostics)

                for logicalPath, text, oracle in unsupportedSignatureCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result = parseSignature logicalPath text

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            result.Diagnostics
                            (oracleLines logicalPath result.Diagnostics)
            ]

            testList "the Oracle discards every later declaration after a file-level recovery" [
                for logicalPath, later, discarded in implementationCascadeCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse
                                logicalPath
                                (definitionRecovery
                                 + later)

                        let root = Seq.exactlyOne result.File.Contents

                        Expect.equal
                            (oracleLines logicalPath result.Diagnostics)
                            [ definitionRecoveryOracle logicalPath ]
                            "The diagnostics must match the Compatibility Oracle"

                        Expect.sequenceEqual
                            (root.Declarations
                             |> Seq.map declarationShape)
                            [
                                "let a/0"
                                "skipped"
                            ]
                            "Only the declarations before the recovery stay"

                        Expect.equal
                            root.DiscardedByRecovery.Length
                            discarded
                            "Each later declaration is marked as discarded"

                for logicalPath, later, discarded in signatureCascadeCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parseSignature
                                logicalPath
                                (valueRecovery
                                 + later)

                        let root = Seq.exactlyOne result.File.Contents

                        Expect.equal
                            (oracleLines logicalPath result.Diagnostics)
                            [ valueRecoveryOracle logicalPath ]
                            "The diagnostics must match the Compatibility Oracle"

                        Expect.sequenceEqual
                            (root.Declarations
                             |> Seq.map signatureShape)
                            [ "val a: <missing> (skipped)" ]
                            "Only the declarations before the recovery stay"

                        Expect.equal
                            root.DiscardedByRecovery.Length
                            discarded
                            "Each later declaration is marked as discarded"
            ]

            testCase "nested modules keep the declarations that the Oracle discards"
            <| fun _ ->
                let logicalPath = "NestedThenLet.fs"

                let result =
                    parse
                        logicalPath
                        "namespace A\nmodule M =\n    let a = 1\n    )\n    let b = 1\n"

                Expect.equal
                    (oracleLines logicalPath result.Diagnostics
                     |> List.head)
                    "NestedThenLet.fs(4,5): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
                    "The first diagnostic matches the Compatibility Oracle"

                Expect.sequenceEqual
                    (result.File.Contents
                     |> Seq.collect (rootShapes declarationShape))
                    [ "module M = [let a/0; skipped; discarded: let b/0]" ]
                    "The nested module keeps its discarded declaration apart"

            testCase "recovery nodes keep the exact missing and skipped ranges"
            <| fun _ ->
                let result = parse "Ranges.fs" "module Program\nlet broken = |]\nlet first = 1\n"

                let binding =
                    match Seq.head (Seq.exactlyOne result.File.Contents).Declarations with
                    | ImplementationDeclaration.Let(_, _, bindings, _) -> Seq.exactlyOne bindings
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

            testCase "an array type keeps its element type and rank"
            <| fun _ ->
                let result =
                    parseSignature
                        "Arrays.fsi"
                        "module Program
val a: int[]
val b: int[,]
val c: int[][]
val d: List<int>[]
val e: int list[]
val f: int[] list
val g: int [ , , ]
val h: (int * string)[] -> global.System.String[]
"

                Expect.isEmpty
                    result.Diagnostics
                    "The Compatibility Oracle reports no parse diagnostics"

                Expect.sequenceEqual
                    (result.File.Contents
                     |> Seq.collect (rootShapes signatureShape))
                    [
                        "val a: int[]"
                        "val b: int[,]"
                        "val c: int[][]"
                        "val d: List<int>[]"
                        "val e: int list[]"
                        "val f: int[] list"
                        "val g: int[,,]"
                        "val h: ((int * string)[] -> global.System.String[])"
                    ]
                    "Each array type keeps its element type and rank"

            testCase "an open declaration keeps its target kind"
            <| fun _ ->
                let result =
                    parse
                        "Opens.fs"
                        "module Program
open System.IO
open global
open global.System
open type System.Math
open type System.Collections.Generic.List<int>
open type int list
open type global.System.Math
"

                Expect.isEmpty
                    result.Diagnostics
                    "The Compatibility Oracle reports no parse diagnostics"

                Expect.sequenceEqual
                    (result.File.Contents
                     |> Seq.collect (rootShapes declarationShape))
                    [
                        "open System.IO"
                        "open global"
                        "open global.System"
                        "open type System.Math"
                        "open type System.Collections.Generic.List<int>"
                        "open type int list"
                        "open type global.System.Math"
                    ]
                    "Each open declaration keeps its target"

            testList "an open declaration with a trailing dot ends after the dot" [
                trailingDotOpenCase "OpenDot.fs" "open System." "open System"
                trailingDotOpenCase "GlobalDot.fs" "open global." "open global"
            ]

            testCase "a type group keeps each definition in order"
            <| fun _ ->
                let result =
                    parse
                        "Group.fs"
                        "module Program
type T = int
and U = string
and V = bool
let x = 1
"

                Expect.isEmpty result.Diagnostics "A valid type group has no diagnostics"

                Expect.sequenceEqual
                    (result.File.Contents
                     |> Seq.collect (rootShapes declarationShape))
                    [
                        "type T and type U and type V"
                        "let x/0"
                    ]
                    "The group keeps each definition, and the next declaration follows it"

            testCase "a type group keeps every skipped token"
            <| fun _ ->
                let result =
                    parse
                        "Group.fs"
                        "module Program
type T = int )
and U = string
and V = bool
"

                match Seq.exactlyOne (Seq.exactlyOne result.File.Contents).Declarations with
                | ImplementationDeclaration.Type group ->
                    let skipped = Expect.wantSome group.First.Skipped "Skipped tokens"

                    Expect.equal (position skipped.Range) (2, 14, 4, 13) "Skipped range"

                    Expect.sequenceEqual
                        (skipped.Tokens
                         |> Seq.map _.Text)
                        [
                            ")"
                            "and"
                            "U"
                            "="
                            "string"
                            "and"
                            "V"
                            "="
                            "bool"
                        ]
                        "Skipped tokens"
                | other -> failtest $"Expected a type declaration, found {other}"

            testCase "an expression declaration keeps the tokens skipped after its recovery"
            <| fun _ ->
                let result = parse "Skipped.fs" "module Program\nf x ) g\n"

                match Seq.exactlyOne (Seq.exactlyOne result.File.Contents).Declarations with
                | ImplementationDeclaration.Expression(_, _, skipped, range) ->
                    Expect.equal
                        (position range)
                        (2, 1, 2, 4)
                        "The declaration range ends at the expression"

                    let skipped = Expect.wantSome skipped "Skipped tokens"

                    Expect.equal (position skipped.Range) (2, 5, 2, 8) "Skipped range"

                    Expect.sequenceEqual
                        (skipped.Tokens
                         |> Seq.map _.Text)
                        [
                            ")"
                            "g"
                        ]
                        "Skipped tokens"
                | other -> failtest $"Expected an expression declaration, found {other}"
        ]
