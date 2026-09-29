namespace fsharp2.Tests

open System.Collections.Immutable
open Expecto
open FSharp2.Compiler

module ParserCompilationTests =
    let private parseAt mode target (files: (string * string) list) =
        let language =
            LanguageVersion.normalize (Some mode)
            |> Result.defaultWith failtest

        files
        |> List.map (fun (logicalPath, text) ->
            let kind =
                SyntaxSourceKind.parse logicalPath
                |> Result.defaultWith (fun error ->
                    failtest $"'{logicalPath}' has no source kind: {error}"
                )

            {
                Kind = kind
                Document =
                    SourceSnapshot.Create(
                        StableIdentity.create logicalPath,
                        logicalPath,
                        text,
                        "content"
                    )
                    |> LexicalPipeline.prepare language Array.empty
            }
        )
        |> ImmutableArray.CreateRange
        |> Parser.parseCompilation target

    let private parse = parseAt "10.0"

    let private oracleLines (result: SyntaxCompilationResult) =
        result.Diagnostics
        |> Seq.map (fun fileDiagnostic ->
            let diagnostic = fileDiagnostic.Diagnostic
            let range = diagnostic.Range

            $"{fileDiagnostic.LogicalPath}({range.Start.Line},{range.Start.Column},{range.End.Line},{range.End.Column}): {SyntaxDiagnosticText.severity diagnostic} {diagnostic.Code}: {diagnostic.Message}"
        )
        |> Seq.toList

    let private missingDeclaration logicalPath startLine startColumn endLine endColumn =
        $"{logicalPath}({startLine},{startColumn},{endLine},{endColumn}): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."

    let private moduleEqualsDeclaration logicalPath startLine startColumn endLine endColumn =
        $"{logicalPath}({startLine},{startColumn},{endLine},{endColumn}): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration. When using a module declaration at the start of a file the '=' sign is not allowed. If this is a top-level module, consider removing the = to resolve this error."

    let private implicitModule
        logicalPath
        moduleName
        fileName
        startLine
        startColumn
        endLine
        endColumn
        =
        $"{logicalPath}({startLine},{startColumn},{endLine},{endColumn}): warning FS0221: The declarations in this file will be placed in an implicit module '{moduleName}' based on the file name '{fileName}'. However this is not a valid F# identifier, so the contents will not be accessible from other files. Consider renaming the file or adding a 'module' or 'namespace' declaration at the top of the file."

    let private anonymous = "Anonymous.fs", "let a = 1\n"
    let private named = "Named.fs", "module B\nlet b = 1\n"
    let private last = "Last.fs", "module Z\nlet z = 1\n"
    let private namedError = "NamedError.fs", "module D\nlet d = )\n"
    let private anonymousError = "AnonymousError.fs", "let c = )\n"

    let private cases = [
        "an anonymous first file",
        SyntaxCompilationTarget.Executable,
        [
            anonymous
            named
        ],
        [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]

        "an anonymous last file",
        SyntaxCompilationTarget.Executable,
        [
            named
            anonymous
        ],
        []

        "an anonymous last file in a library",
        SyntaxCompilationTarget.Library,
        [
            named
            anonymous
        ],
        [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]

        "one anonymous file in a library",
        SyntaxCompilationTarget.Library,
        [ anonymous ],
        [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]

        "one anonymous file in an executable", SyntaxCompilationTarget.Executable, [ anonymous ], []

        "parse diagnostics keep file order",
        SyntaxCompilationTarget.Executable,
        [
            namedError
            anonymousError
        ],
        [
            "NamedError.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding"
            "AnonymousError.fs(1,9,1,10): error FS0010: Unexpected symbol ')' in binding"
        ]

        "FS0222 follows the parse diagnostics of its file",
        SyntaxCompilationTarget.Executable,
        [
            anonymousError
            namedError
        ],
        [
            "AnonymousError.fs(1,9,1,10): error FS0010: Unexpected symbol ')' in binding"
            missingDeclaration "AnonymousError.fs" 1 1 2 1
            "NamedError.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding"
        ]

        "FS0222 follows a file-level recovery",
        SyntaxCompilationTarget.Executable,
        [
            "Discard.fs", "let a = 1\n)\nlet b = )\n"
            last
        ],
        [
            "Discard.fs(2,1,2,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "Discard.fs" 1 1 2 1
        ]

        "the range ends at the end of input without a line break",
        SyntaxCompilationTarget.Executable,
        [
            "NoNewline.fs", "let a = 1"
            last
        ],
        [ missingDeclaration "NoNewline.fs" 1 1 1 10 ]

        "the range starts at the first token",
        SyntaxCompilationTarget.Executable,
        [
            "Padded.fs", "// comment\n\nlet a = 1\n\n\n"
            last
        ],
        [ missingDeclaration "Padded.fs" 3 1 4 1 ]

        "the range ends at the next line after a carriage return and line feed",
        SyntaxCompilationTarget.Executable,
        [
            "Crlf.fs", "let a = 1\r\nlet b = 2\r\n"
            last
        ],
        [ missingDeclaration "Crlf.fs" 1 1 2 1 ]

        "the range ends at the next line for a multi-line declaration",
        SyntaxCompilationTarget.Executable,
        [
            "MultiLine.fs", "let a =\n    1\nlet b = 2\n"
            last
        ],
        [ missingDeclaration "MultiLine.fs" 1 1 2 1 ]

        "an empty file has an empty range at the end of input",
        SyntaxCompilationTarget.Executable,
        [
            "Empty.fs", ""
            last
        ],
        [ missingDeclaration "Empty.fs" 1 1 1 1 ]

        "a comment-only file has an empty range at the end of input",
        SyntaxCompilationTarget.Executable,
        [
            "CommentOnly.fs", "// only a comment\n"
            last
        ],
        [ missingDeclaration "CommentOnly.fs" 2 1 2 1 ]

        "the range starts at an indented first token",
        SyntaxCompilationTarget.Executable,
        [
            "Indented.fs", "   let a = 1\n"
            last
        ],
        [ missingDeclaration "Indented.fs" 1 4 2 1 ]

        "the range starts after a leading directive",
        SyntaxCompilationTarget.Executable,
        [
            "Directive.fs", "#nowarn \"40\"\nlet a = 1\n"
            last
        ],
        [ missingDeclaration "Directive.fs" 2 1 3 1 ]

        "the range starts after a multi-line block comment",
        SyntaxCompilationTarget.Executable,
        [
            "BlockComment.fs", "(* a\n b *) let a = 1\n"
            last
        ],
        [ missingDeclaration "BlockComment.fs" 2 7 3 1 ]

        "a single-line binding range ends at the end of input after trailing space",
        SyntaxCompilationTarget.Executable,
        [
            "Trailing.fs", "\nlet a = 1   "
            last
        ],
        [ missingDeclaration "Trailing.fs" 2 1 2 13 ]

        "a single-line binding range ends at the end of input after a comment",
        SyntaxCompilationTarget.Executable,
        [
            "LineComment.fs", "let a = 1 // c"
            last
        ],
        [ missingDeclaration "LineComment.fs" 1 1 1 15 ]

        "a single-line open range ends at its last token",
        SyntaxCompilationTarget.Executable,
        [
            "Open.fs", "open System\n\n"
            last
        ],
        [ missingDeclaration "Open.fs" 1 1 1 12 ]

        "an open range ignores a discarded token on the next line",
        SyntaxCompilationTarget.Executable,
        [
            "OpenDiscard.fs", "open System\n)"
            last
        ],
        [
            "OpenDiscard.fs(2,1,2,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "OpenDiscard.fs" 1 1 1 12
        ]

        "a single-line type definition range spans to the next line",
        SyntaxCompilationTarget.Executable,
        [
            "Type.fs", "type T = int\n"
            last
        ],
        [ missingDeclaration "Type.fs" 1 1 2 1 ]

        "a whitespace-only file has an empty range at the end of input",
        SyntaxCompilationTarget.Executable,
        [
            "Whitespace.fs", "   "
            last
        ],
        [ missingDeclaration "Whitespace.fs" 1 4 1 4 ]

        "a signature open range ends at the end of input",
        SyntaxCompilationTarget.Executable,
        [
            "OpenSig.fsi", "open System\n"
            last
        ],
        [ missingDeclaration "OpenSig.fsi" 1 1 2 1 ]

        "a signature range ends at the end of input without a line break",
        SyntaxCompilationTarget.Executable,
        [
            "ValSig.fsi", "val a: int // c"
            last
        ],
        [ missingDeclaration "ValSig.fsi" 1 1 1 16 ]

        "a multi-line signature range is not cut at the next line",
        SyntaxCompilationTarget.Executable,
        [
            "NestedSig.fsi", "module M =\n    val a: int\n"
            last
        ],
        [ moduleEqualsDeclaration "NestedSig.fsi" 1 1 3 1 ]

        "a signature range ends at a discarded token",
        SyntaxCompilationTarget.Executable,
        [
            "OpenDiscardSig.fsi",
            "open System
)"
            last
        ],
        [
            "OpenDiscardSig.fsi(2,1,2,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "OpenDiscardSig.fsi" 1 1 2 2
        ]

        "a signature range ends at a discarded token before the end of input",
        SyntaxCompilationTarget.Executable,
        [
            "ValDiscardSig.fsi",
            "val a: int

)
"
            last
        ],
        [
            "ValDiscardSig.fsi(3,1,3,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "ValDiscardSig.fsi" 1 1 3 2
        ]

        "an anonymous script file before the last file",
        SyntaxCompilationTarget.Executable,
        [
            "Script.fsx", "let a = 1\n"
            last
        ],
        []

        "an anonymous script file with an upper-case extension",
        SyntaxCompilationTarget.Executable,
        [
            "Script.FSX", "let a = 1\n"
            last
        ],
        []

        "an anonymous fsscript file in a library",
        SyntaxCompilationTarget.Library,
        [ "Script.FsScript", "let a = 1\n" ],
        []

        "anonymous script files keep their parse diagnostics",
        SyntaxCompilationTarget.Executable,
        [
            "Broken.fsx", "let a = )\n"
            last
        ],
        [ "Broken.fsx(1,9,1,10): error FS0010: Unexpected symbol ')' in binding" ]

        "an anonymous implementation file with an upper-case extension",
        SyntaxCompilationTarget.Executable,
        [
            "Upper.FS", "let a = 1\n"
            last
        ],
        [ missingDeclaration "Upper.FS" 1 1 2 1 ]

        "an anonymous signature file with a mixed-case extension",
        SyntaxCompilationTarget.Executable,
        [
            "Mixed.FsI", "val a: int\n"
            last
        ],
        [ missingDeclaration "Mixed.FsI" 1 1 2 1 ]

        "an implicit module name with a hyphen before the last file",
        SyntaxCompilationTarget.Executable,
        [
            "my-file.fs", "let a = 1\n"
            last
        ],
        [
            missingDeclaration "my-file.fs" 1 1 2 1
            implicitModule "my-file.fs" "My-file" "my-file.fs" 1 1 2 1
        ]

        "an implicit module name with a hyphen in the last file",
        SyntaxCompilationTarget.Executable,
        [
            last
            "my-file.fs", "let a = 1\n"
        ],
        [ implicitModule "my-file.fs" "My-file" "my-file.fs" 1 1 2 1 ]

        "an implicit module name with a hyphen in a library",
        SyntaxCompilationTarget.Library,
        [ "my-file.fs", "let a = 1\n" ],
        [
            missingDeclaration "my-file.fs" 1 1 2 1
            implicitModule "my-file.fs" "My-file" "my-file.fs" 1 1 2 1
        ]

        "the implicit module message uses the file name without its folder",
        SyntaxCompilationTarget.Executable,
        [
            last
            "/src/my-file.fs", "let a = 1\n"
        ],
        [ implicitModule "/src/my-file.fs" "My-file" "my-file.fs" 1 1 2 1 ]

        "an implicit module name with a space",
        SyntaxCompilationTarget.Executable,
        [
            last
            "a b.fs", "let a = 1\n"
        ],
        [ implicitModule "a b.fs" "A b" "a b.fs" 1 1 2 1 ]

        "an implicit module name with a dot",
        SyntaxCompilationTarget.Executable,
        [
            last
            "a.b.fs", "let a = 1\n"
        ],
        [ implicitModule "a.b.fs" "A.b" "a.b.fs" 1 1 2 1 ]

        "an implicit module name with an apostrophe",
        SyntaxCompilationTarget.Executable,
        [
            last
            "x'.fs", "let a = 1\n"
        ],
        [ implicitModule "x'.fs" "X'" "x'.fs" 1 1 2 1 ]

        "an implicit module name from an upper-case extension",
        SyntaxCompilationTarget.Executable,
        [
            last
            "q-x.FS", "let a = 1\n"
        ],
        [ implicitModule "q-x.FS" "Q-x" "q-x.FS" 1 1 2 1 ]

        "implicit module names with letters, digits, and underscores",
        SyntaxCompilationTarget.Executable,
        [
            "1a.fs", "let a = 1\n"
            "_x.fs", "let a = 1\n"
            "type.fs", "let a = 1\n"
            "\u00e9.fs", "let a = 1\n"
            "Ab1.fs", "let a = 1\n"
            last
        ],
        [
            missingDeclaration "1a.fs" 1 1 2 1
            missingDeclaration "_x.fs" 1 1 2 1
            missingDeclaration "type.fs" 1 1 2 1
            missingDeclaration "\u00e9.fs" 1 1 2 1
            missingDeclaration "Ab1.fs" 1 1 2 1
        ]

        yield!
            [
                "a٣b"
                "aʰb"
                "aאb"
                "ǅb"
                "αb"
            ]
            |> List.map (fun stem ->
                $"a valid implicit module name with the first UTF-16 code units {int stem[0]:X4} {int stem[1]:X4}",
                SyntaxCompilationTarget.Executable,
                [
                    last
                    $"{stem}.fs",
                    "let a = 1
"
                ],
                []
            )

        yield!
            [
                "a‿b", "A‿b"
                "a﹏b", "A﹏b"
                "éx", "Éx"
                "aःb", "Aःb"
                "a­b", "A­b"
                "a‌b", "A‌b"
                "aⅠb", "AⅠb"
                "́ab", "́ab"
                "‿ab", "‿ab"
                "Ⅰab", "Ⅰab"
                "a#b", "A#b"
                "a$b", "A$b"
                "a`b", "A`b"
                "a\U0001D400b", "A\U0001D400b"
                "a²b", "A²b"
                "\U0001D400ab", "\U0001D400ab"
                "\U0001D41A-b", "\U0001D41A-b"
                "\U00010428-b", "\U00010428-b"
                "ǆa-b", "Ǆa-b"
                "Ǆa-b", "Ǆa-b"
                "ßa-b", "ßa-b"
                "ĳa-b", "Ĳa-b"
                "ﬁa-b", "ﬁa-b"
                "i-b", "I-b"
                "ı-b", "ı-b"
                "ς-b", "Σ-b"
                "æ-b", "Æ-b"
                "ᾀ-b", "ᾈ-b"
            ]
            |> List.map (fun (stem, moduleName) ->
                let fileName = $"{stem}.fs"

                let codeUnits =
                    stem
                    |> Seq.map (fun character -> $"{int character:X4}")
                    |> String.concat " "

                $"an implicit module name with the UTF-16 code units {codeUnits}",
                SyntaxCompilationTarget.Executable,
                [
                    last
                    fileName, "let a = 1\n"
                ],
                [ implicitModule fileName moduleName fileName 1 1 2 1 ]
            )

        "implicit module names of a signature and its implementation",
        SyntaxCompilationTarget.Executable,
        [
            "a.b.fsi", "val a: int\n"
            "a.b.fs", "let a = 1\n"
        ],
        [
            missingDeclaration "a.b.fsi" 1 1 2 1
            implicitModule "a.b.fsi" "A.b" "a.b.fsi" 1 1 2 1
            implicitModule "a.b.fs" "A.b" "a.b.fs" 1 1 2 1
        ]

        "a script file has no implicit module warning",
        SyntaxCompilationTarget.Executable,
        [
            "a.b.fsx", "let a = 1\n"
            last
        ],
        []

        "a file without declarations has no implicit module warning",
        SyntaxCompilationTarget.Executable,
        [
            "e-mpty.fs", ""
            "c-o.fs", "// c\n"
            last
        ],
        [
            missingDeclaration "e-mpty.fs" 1 1 1 1
            missingDeclaration "c-o.fs" 2 1 2 1
        ]

        "an implicit module warning follows the parse diagnostics and FS0222",
        SyntaxCompilationTarget.Executable,
        [
            "e-r.fs", "let a = )\n"
            last
        ],
        [
            "e-r.fs(1,9,1,10): error FS0010: Unexpected symbol ')' in binding"
            missingDeclaration "e-r.fs" 1 1 2 1
            implicitModule "e-r.fs" "E-r" "e-r.fs" 1 1 2 1
        ]

        "an implicit module warning after a file-level recovery",
        SyntaxCompilationTarget.Executable,
        [
            "k-d.fs", "let a = 1\n)\n"
            last
        ],
        [
            "k-d.fs(2,1,2,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "k-d.fs" 1 1 2 1
            implicitModule "k-d.fs" "K-d" "k-d.fs" 1 1 2 1
        ]

        "an implicit module warning for an open declaration",
        SyntaxCompilationTarget.Executable,
        [
            "o-p.fs", "open System\n"
            last
        ],
        [
            missingDeclaration "o-p.fs" 1 1 1 12
            implicitModule "o-p.fs" "O-p" "o-p.fs" 1 1 1 12
        ]

        "an implicit module warning for a nested module",
        SyntaxCompilationTarget.Executable,
        [
            "n-e.fs", "module M =\n    let a = 1\n"
            last
        ],
        [
            moduleEqualsDeclaration "n-e.fs" 1 1 2 1
            implicitModule "n-e.fs" "N-e" "n-e.fs" 1 1 2 1
        ]

        "an implicit module warning for a signature open declaration",
        SyntaxCompilationTarget.Executable,
        [
            "s-o.fsi", "open System\n"
            last
        ],
        [
            missingDeclaration "s-o.fsi" 1 1 2 1
            implicitModule "s-o.fsi" "S-o" "s-o.fsi" 1 1 2 1
        ]

        "a file with only discarded tokens has the range of the first token",
        SyntaxCompilationTarget.Executable,
        [
            "d-x.fs", ")\n"
            last
        ],
        [
            "d-x.fs(1,1,1,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "d-x.fs" 1 1 1 2
        ]

        "a file with only discarded tokens on two lines",
        SyntaxCompilationTarget.Executable,
        [
            "D1.fs", ")\n)\n"
            last
        ],
        [
            "D1.fs(1,1,1,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "D1.fs" 1 1 1 2
        ]

        "a file with an indented discarded token",
        SyntaxCompilationTarget.Executable,
        [
            "D2.fs", "\n  )  "
            last
        ],
        [
            "D2.fs(2,3,2,4): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "D2.fs" 2 3 2 4
        ]

        "a declaration after a discarded first token is discarded",
        SyntaxCompilationTarget.Executable,
        [
            "D3.fs", ") let a = 1\n"
            last
        ],
        [
            "D3.fs(1,1,1,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "D3.fs" 1 1 1 2
        ]

        "a discarded first token without a line break",
        SyntaxCompilationTarget.Executable,
        [
            "D7.fs", ")"
            last
        ],
        [
            "D7.fs(1,1,1,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "D7.fs" 1 1 1 2
        ]

        "a signature file with only discarded tokens has the range of the first token",
        SyntaxCompilationTarget.Executable,
        [
            "s-d.fsi", ")\n"
            last
        ],
        [
            "s-d.fsi(1,1,1,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "s-d.fsi" 1 1 1 2
        ]

        "a signature declaration after a discarded first token is discarded",
        SyntaxCompilationTarget.Executable,
        [
            "s-k.fsi", ") val a: int\n"
            last
        ],
        [
            "s-k.fsi(1,1,1,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "s-k.fsi" 1 1 1 2
        ]

        "a named signature module discards every token after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "A1.fsi", "module M\n)\n)\n"
            last
        ],
        [
            "A1.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a named signature module discards a value after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "A2.fsi", "module M\n)\nval a: int\n"
            last
        ],
        [
            "A2.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a named signature module discards later stray tokens after a value",
        SyntaxCompilationTarget.Executable,
        [
            "A3.fsi", "module M\nval a: int\n)\nval b: int\n)\n"
            last
        ],
        [
            "A3.fsi(3,1,3,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a named signature module reports one stray token on a line",
        SyntaxCompilationTarget.Executable,
        [
            "A4.fsi", "module M\n) )\nval a: int\n"
            last
        ],
        [
            "A4.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a named signature module discards an open declaration after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "A5.fsi", "module M\n)\nopen System\n)\n"
            last
        ],
        [
            "A5.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a signature namespace discards every token after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "A8.fsi", "namespace N\n)\n)\n"
            last
        ],
        [
            "A8.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a stray token after a value discards the next line",
        SyntaxCompilationTarget.Executable,
        [
            "A9.fsi", "module M\nval a: int )\n)\n"
            last
        ],
        [
            "A9.fsi(2,12,2,13): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a named signature module discards a let declaration after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "A10.fsi", "module M\n)\nlet a = 1\n"
            last
        ],
        [
            "A10.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a named signature module discards a type declaration after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "A11.fsi", "module M\n)\ntype T = int\n)\n"
            last
        ],
        [
            "A11.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a later signature namespace is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "B1.fsi", "namespace N\n)\nnamespace M\nval a: )\n"
            last
        ],
        [
            "B1.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a nested signature module is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "B2.fsi", "module M\n)\nmodule Inner =\n    val b: )\n"
            last
        ],
        [
            "B2.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an attributed value is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "B3.fsi", "module M\n)\n[<A>]\nval b: int\n"
            last
        ],
        [
            "B3.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a conditional value is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "B4.fsi", "module M\n)\n#if X\nval b: )\n#endif\n"
            last
        ],
        [
            "B4.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a multi-line type is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "B5.fsi", "module M\n)\ntype T =\n    | A\n    | B\n"
            last
        ],
        [
            "B5.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an exception declaration is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "B7.fsi", "module M\n)\nexception E\n"
            last
        ],
        [
            "B7.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an incomplete value is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "C1.fsi", "module M\n)\nval y:\n"
            last
        ],
        [
            "C1.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an attributed open is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "E1.fsi", "module M\n)\n[<A>]\nopen System\n"
            last
        ],
        [
            "E1.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an attributed nested module is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "E2.fsi", "module M\n)\n[<A>]\nmodule N =\n    val b: int\n"
            last
        ],
        [
            "E2.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an attributed open is discarded after a stray first token",
        SyntaxCompilationTarget.Executable,
        [
            "E3.fsi", ")\n[<A>] open System\n"
            last
        ],
        [
            "E3.fsi(1,1,1,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "E3.fsi" 1 1 1 2
        ]

        "an attributed open is discarded after a definition recovery",
        SyntaxCompilationTarget.Executable,
        [
            "E4.fs", "module M\nlet a = 1\n)\n[<A>]\nopen System\n"
            last
        ],
        [
            "E4.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "an attributed nested module is discarded after a definition recovery",
        SyntaxCompilationTarget.Executable,
        [
            "E5.fs", "module M\nlet a = 1\n)\n[<A>]\nmodule N =\n    let b = 1\n"
            last
        ],
        [
            "E5.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "a do declaration is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "C3.fsi", "module M\n)\ndo )\n"
            last
        ],
        [
            "C3.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "a let block is discarded after a stray token",
        SyntaxCompilationTarget.Executable,
        [
            "C5.fsi", "module M\n)\nlet f =\n    let inner = 1\n    )\nlet z = 1\n"
            last
        ],
        [
            "C5.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "an anonymous signature file discards every token after a stray first token",
        SyntaxCompilationTarget.Executable,
        [
            "A6.fsi", ")\n)\n"
            last
        ],
        [
            "A6.fsi(1,1,1,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "A6.fsi" 1 1 1 2
        ]

        "an anonymous signature file discards a value after a stray first token",
        SyntaxCompilationTarget.Executable,
        [
            "A7.fsi", ")\nval a: int\n)\n"
            last
        ],
        [
            "A7.fsi(1,1,1,2): error FS0010: Unexpected symbol ')' in signature file"
            missingDeclaration "A7.fsi" 1 1 1 2
        ]

        "an expression before a let declaration",
        SyntaxCompilationTarget.Executable,
        [
            "A1.fs",
            "1 + 2
let b = 1
"
            last
        ],
        [ missingDeclaration "A1.fs" 1 1 2 1 ]

        "a multi-line expression",
        SyntaxCompilationTarget.Executable,
        [
            "A2.fs",
            "f (
    1)
"
            last
        ],
        [ missingDeclaration "A2.fs" 1 1 2 1 ]

        "a multi-line expression before a let declaration",
        SyntaxCompilationTarget.Executable,
        [
            "A3.fs",
            "f (
    1)
let b = 1
"
            last
        ],
        [ missingDeclaration "A3.fs" 1 1 2 1 ]

        "a last expression on the next line",
        SyntaxCompilationTarget.Executable,
        [
            "A4.fs",
            "let a = 1
f 2
"
            last
        ],
        [ missingDeclaration "A4.fs" 1 1 2 1 ]

        "a last expression before blank lines",
        SyntaxCompilationTarget.Executable,
        [
            "A5.fs",
            "let a = 1
f 2

"
            last
        ],
        [ missingDeclaration "A5.fs" 1 1 2 1 ]

        "a last multi-line expression on the next line",
        SyntaxCompilationTarget.Executable,
        [
            "A6.fs",
            "let a = 1
f (
  2)
"
            last
        ],
        [ missingDeclaration "A6.fs" 1 1 2 1 ]

        "a single-line expression range ends at its last token",
        SyntaxCompilationTarget.Executable,
        [
            "A7.fs",
            "1
"
            last
        ],
        [ missingDeclaration "A7.fs" 1 1 1 2 ]

        "a single-line expression range ignores trailing space",
        SyntaxCompilationTarget.Executable,
        [
            "S15.fs",
            "1 + 2   
"
            last
        ],
        [ missingDeclaration "S15.fs" 1 1 1 6 ]

        "a single-line expression range without a line break",
        SyntaxCompilationTarget.Executable,
        [
            "S5.fs", "f ()"
            last
        ],
        [ missingDeclaration "S5.fs" 1 1 1 5 ]

        "a module header is not followed by an application on the next line",
        SyntaxCompilationTarget.Executable,
        [
            "Y12.fs",
            "module M
f x = 1
"
            last
        ],
        []

        "a module header is not followed by a name on the next line",
        SyntaxCompilationTarget.Executable,
        [
            "Y21.fs",
            "module M
x = 1
"
            last
        ],
        []

        "a dotted module header is not followed by a dotted name on the next line",
        SyntaxCompilationTarget.Executable,
        [
            "Y22.fs",
            "module A.B
x.y = 1
"
            last
        ],
        []

        "a nested module after a comment in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "CommentMod.fs",
            "// c
module M =
    let a = 1
"
            last
        ],
        [ moduleEqualsDeclaration "CommentMod.fs" 2 1 3 1 ]

        "a nested module before a let declaration in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "ModThenLet.fs",
            "module M =
    let a = 1
let b = 2
"
            last
        ],
        [ moduleEqualsDeclaration "ModThenLet.fs" 1 1 2 1 ]

        "a nested module after a let declaration in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "LetThenMod.fs",
            "let b = 2
module M =
    let a = 1
"
            last
        ],
        [ missingDeclaration "LetThenMod.fs" 1 1 2 1 ]

        "a nested module after an open declaration in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "OpenThenMod.fs",
            "open System
module M =
    let a = 1
"
            last
        ],
        [ missingDeclaration "OpenThenMod.fs" 1 1 2 1 ]

        "a nested module after a stray first token in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "sk_mod.fs",
            ")
module M =
    let a = 1
"
            last
        ],
        [
            "sk_mod.fs(1,1,1,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "sk_mod.fs" 1 1 1 2
        ]

        "a nested module after a stray first line in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "sk_mod2.fs",
            ") x
module M =
    let a = 1
"
            last
        ],
        [
            "sk_mod2.fs(1,1,1,2): error FS0010: Unexpected symbol ')' in implementation file"
            missingDeclaration "sk_mod2.fs" 1 1 1 2
        ]

        "a nested module in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "NestedOnly.fs", "module M =\n    let a = 1\n"
            last
        ],
        [ moduleEqualsDeclaration "NestedOnly.fs" 1 1 2 1 ]

        "a namespace root",
        SyntaxCompilationTarget.Executable,
        [
            "Namespace.fs", "namespace N\nlet a = 1\n"
            last
        ],
        []

        "anonymous signature and implementation files",
        SyntaxCompilationTarget.Executable,
        [
            "AnonymousSig.fsi", "val a: int\n"
            "AnonymousSig.fs", "let a = 1\n"
            last
        ],
        [
            missingDeclaration "AnonymousSig.fsi" 1 1 2 1
            missingDeclaration "AnonymousSig.fs" 1 1 2 1
        ]

        "an anonymous implementation file after its signature file",
        SyntaxCompilationTarget.Executable,
        [
            "AnonymousSig.fsi", "val a: int\n"
            "AnonymousSig.fs", "let a = 1\n"
        ],
        [ missingDeclaration "AnonymousSig.fsi" 1 1 2 1 ]

        "an anonymous last signature file",
        SyntaxCompilationTarget.Executable,
        [
            "First.fs", "module Q\nlet q = 1\n"
            "LastSig.fsi", "val a: int\n"
        ],
        []
    ]

    let private expressionDeclarationCases = [
        "Y2.fs",
        "module M\nf x )\n",
        [
            "Y2.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y3.fs",
        "module M\nmatch 1 with\n| _ )\n",
        [
            "Y3.fs(3,5,3,6): error FS0010: Unexpected symbol ')' in pattern matching. Expected '->' or other token."
        ]

        "Y4.fs",
        "module M\nfun x )\n",
        [
            "Y4.fs(2,7,2,8): error FS0010: Unexpected symbol ')' in lambda expression. Expected '->' or other token."
        ]

        "Y5.fs",
        "module M\n{ A = ) }\n",
        [ "Y5.fs(2,7,2,8): error FS0010: Unexpected symbol ')' in expression" ]

        "Y7.fs",
        "module M\nf x )\nlet b = 1\n",
        [
            "Y7.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y9.fs",
        "module M\nmatch 1 )\n",
        [
            "Y9.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in expression. Expected 'with' or other token."
        ]

        "Y12.fs", "module M\nf x = 1\n", []

        "Y13.fs",
        "module M\nmodule N =\n    f x )\n    let b = 1\n",
        [
            "Y13.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "Y13.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y14.fs",
        "f x )\n",
        [
            "Y14.fs(1,5,1,6): error FS0010: Unexpected symbol ')' in implementation file"
            "Y14.fs(1,1,1,4): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "Y15.fs",
        "module M\nf x\n)\n",
        [
            "Y15.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y16.fs",
        "namespace N\nf x )\n",
        [
            "Y16.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in implementation file. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y17.fs",
        "namespace N\nf x )\nlet b = 1\n",
        [
            "Y17.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in implementation file. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y18.fs",
        "module M\nmodule N =\n    let a = 1\n    f x )\n",
        [
            "Y18.fs(4,9,4,10): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "Y18.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y19.fs",
        "module M\nf x )\nf y )\n",
        [
            "Y19.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y20.fs",
        "f x )\nlet b = 1\n",
        [
            "Y20.fs(1,5,1,6): error FS0010: Unexpected symbol ')' in implementation file"
            "Y20.fs(1,1,1,4): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "Y21.fs", "module M\nx = 1\n", []

        "Y22.fs", "module A.B\nx.y = 1\n", []

        "Y23.fs",
        "module M\nf ]\n",
        [
            "Y23.fs(2,3,2,4): error FS0010: Unexpected symbol ']' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y24.fs",
        "module M\n1 )\n",
        [
            "Y24.fs(2,3,2,4): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y25.fs",
        "module M\nmodule N =\n    let a = 1\n    f x )\n    let b = 2\n",
        [
            "Y25.fs(4,9,4,10): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "Y25.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y26.fs",
        "module M\n(1) )\n",
        [
            "Y26.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "Y27.fs",
        "module M\nf x end\n",
        [
            "Y27.fs(2,5,2,8): error FS0010: Unexpected keyword 'end' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "N1.fs",
        "module M\nmodule N =\n    1 )\n",
        [
            "N1.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "N1.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "N2.fs",
        "module M\nmodule N =\n    let a = 1\n    1 )\n",
        [
            "N2.fs(4,7,4,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "N2.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "N3.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n",
        [
            "N3.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "N3.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "N4.fs",
        "module M\nmodule N =\n    f x ]\nlet c = 3\n",
        [
            "N4.fs(3,9,3,10): error FS0010: Unexpected symbol ']' in definition. Expected incomplete structured construct at or before this point or other token."
            "N4.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "N5.fs",
        "module M\nmodule N =\n    1 end\n",
        [
            "N5.fs(3,7,3,10): error FS0010: Unexpected keyword 'end' in definition. Expected incomplete structured construct at or before this point or other token."
            "N5.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "N6.fs",
        "namespace Q\nmodule N =\n    1 )\n",
        [
            "N6.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "N6.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "N7.fs",
        "module M\nmodule N =\n    1 )\nmodule O =\n    let d = 4\n",
        [
            "N7.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "N7.fs(4,1,4,7): error FS0010: Unexpected keyword 'module' in implementation file"
        ]

        "N8.fs",
        "module M\nmodule N =\n    let a = 1\n    1 )\n    let b = 2\n",
        [
            "N8.fs(4,7,4,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "N8.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]
    ]

    let private unmodeledExpressionDeclarationCases = [
        "Y1.fs",
        "module M\n1 +\n",
        [
            "Y1.fs(3,1,3,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "Y1.fs(2,3,2,4): error FS3156: Unexpected token '+' or incomplete expression"
        ]

        "Y6.fs",
        "module M\n(1, )\n",
        [ "Y6.fs(2,3,2,4): error FS3100: Expected an expression after this point" ]

        "Y8.fs",
        "module M\nif true then )\n",
        [
            "Y8.fs(2,14,2,15): error FS0010: Unexpected symbol ')' in expression"
            "Y8.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y10.fs",
        "module M\n[1; )\n",
        [
            "Y10.fs(2,5,2,6): error FS0010: Unexpected symbol ')' in expression. Expected ']' or other token."
            "Y10.fs(2,1,2,2): error FS0598: Unmatched '['"
        ]

        "Y11.fs",
        "module M\nf (\n",
        [
            "Y11.fs(3,1,3,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (1:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "Y11.fs(2,3,2,4): error FS0583: Unmatched '('"
        ]

        "Y28.fs",
        "module M\nf x in\n",
        [
            "Y28.fs(2,5,2,7): error FS0010: Unexpected keyword 'in' in definition. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private nestedRecoveryEndCases = [
        "m2_let.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let a = 1\n        let b = 2\nlet c = 3\n",
        [
            "m2_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "m2_let.fs(6,9,6,12): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "m2_none.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let a = 1\n        let b = 2\n",
        [
            "m2_none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "m2_none.fs(6,9,6,12): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "m1_expr.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let a = 1\n        f 2\nlet c = 3\n",
        [
            "m1_expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "m1_expr.fs(6,9,6,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "m1_nested.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let a = 1\n    let b = 2\nlet c = 3\n",
        [
            "m1_nested.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "m1_nested.fs(6,5,6,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "mexpr_let.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        f 1\n        let b = 2\nlet c = 3\n",
        [
            "mexpr_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "mexpr_let.fs(7,1,7,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "m3_let.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let a = 1\n        let b = 2\n        let e = 3\nlet c = 3\n",
        [
            "m3_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "m3_let.fs(6,9,6,12): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "e_none.fs",
        "module M\nmodule N =\n    1 )\n",
        [
            "e_none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_none.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "e_let.fs",
        "module M\nmodule N =\n    1 )\nlet c = 3\n",
        [
            "e_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_let.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "e_letrec.fs",
        "module M\nmodule N =\n    1 )\nlet rec c = 3\n",
        [
            "e_letrec.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_letrec.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "e_use.fs",
        "module M\nmodule N =\n    1 )\nuse c = 3\n",
        [
            "e_use.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_use.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "e_module.fs",
        "module M\nmodule N =\n    1 )\nmodule O =\n    let d = 4\n",
        [
            "e_module.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_module.fs(4,1,4,7): error FS0010: Unexpected keyword 'module' in implementation file"
        ]

        "e_type.fs",
        "module M\nmodule N =\n    1 )\ntype T = int\n",
        [
            "e_type.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_type.fs(4,1,4,5): error FS0010: Unexpected keyword 'type' in implementation file"
        ]

        "e_open.fs",
        "module M\nmodule N =\n    1 )\nopen System\n",
        [
            "e_open.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_open.fs(4,1,4,5): error FS0010: Unexpected keyword 'open' in implementation file"
        ]

        "e_do.fs",
        "module M\nmodule N =\n    1 )\ndo ()\n",
        [
            "e_do.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_do.fs(4,1,4,3): error FS0010: Unexpected keyword 'do' in implementation file"
        ]

        "e_expr.fs",
        "module M\nmodule N =\n    1 )\nf 1\n",
        [
            "e_expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_expr.fs(4,1,4,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "e_attr.fs",
        "module M\nmodule N =\n    1 )\n[<A>]\nlet c = 3\n",
        [
            "e_attr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_attr.fs(4,1,4,3): error FS0010: Unexpected symbol '[<' in implementation file"
        ]

        "e_exception.fs",
        "module M\nmodule N =\n    1 )\nexception E\n",
        [
            "e_exception.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_exception.fs(4,1,4,10): error FS0010: Unexpected keyword 'exception' in implementation file"
        ]

        "e_ident.fs",
        "module M\nmodule N =\n    1 )\nx\n",
        [
            "e_ident.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_ident.fs(4,1,4,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "e_paren.fs",
        "module M\nmodule N =\n    1 )\n)\n",
        [
            "e_paren.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_paren.fs(4,1,4,2): error FS0010: Unexpected symbol ')' in implementation file"
        ]

        "e_blank.fs",
        "module M\nmodule N =\n    1 )\n\n\n",
        [
            "e_blank.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_blank.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "e_comment.fs",
        "module M\nmodule N =\n    1 )\n// c\n",
        [
            "e_comment.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_comment.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "e_indented.fs",
        "module M\nmodule N =\n    1 )\n    let e = 5\n",
        [
            "e_indented.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_indented.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "e_twolets.fs",
        "module M\nmodule N =\n    1 )\nlet c = 3\nlet d = 4\n",
        [
            "e_twolets.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "e_twolets.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "s_none.fs",
        "module M\nmodule N =\n    let a = 1\n    )\n",
        [
            "s_none.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "s_let.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nlet c = 3\n",
        [
            "s_let.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_let.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "s_letrec.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nlet rec c = 3\n",
        [
            "s_letrec.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_letrec.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "s_use.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nuse c = 3\n",
        [
            "s_use.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_use.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "s_module.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nmodule O =\n    let d = 4\n",
        [
            "s_module.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_module.fs(5,1,5,7): error FS0010: Unexpected keyword 'module' in implementation file"
        ]

        "s_type.fs",
        "module M\nmodule N =\n    let a = 1\n    )\ntype T = int\n",
        [
            "s_type.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_type.fs(5,1,5,5): error FS0010: Unexpected keyword 'type' in implementation file"
        ]

        "s_open.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nopen System\n",
        [
            "s_open.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_open.fs(5,1,5,5): error FS0010: Unexpected keyword 'open' in implementation file"
        ]

        "s_do.fs",
        "module M\nmodule N =\n    let a = 1\n    )\ndo ()\n",
        [
            "s_do.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_do.fs(5,1,5,3): error FS0010: Unexpected keyword 'do' in implementation file"
        ]

        "s_expr.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nf 1\n",
        [
            "s_expr.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_expr.fs(5,1,5,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "s_attr.fs",
        "module M\nmodule N =\n    let a = 1\n    )\n[<A>]\nlet c = 3\n",
        [
            "s_attr.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_attr.fs(5,1,5,3): error FS0010: Unexpected symbol '[<' in implementation file"
        ]

        "s_exception.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nexception E\n",
        [
            "s_exception.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_exception.fs(5,1,5,10): error FS0010: Unexpected keyword 'exception' in implementation file"
        ]

        "s_ident.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nx\n",
        [
            "s_ident.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_ident.fs(5,1,5,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "s_paren.fs",
        "module M\nmodule N =\n    let a = 1\n    )\n)\n",
        [
            "s_paren.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_paren.fs(5,1,5,2): error FS0010: Unexpected symbol ')' in implementation file"
        ]

        "s_blank.fs",
        "module M\nmodule N =\n    let a = 1\n    )\n\n\n",
        [
            "s_blank.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_blank.fs(7,1,7,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "s_comment.fs",
        "module M\nmodule N =\n    let a = 1\n    )\n// c\n",
        [
            "s_comment.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_comment.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "s_indented.fs",
        "module M\nmodule N =\n    let a = 1\n    )\n    let e = 5\n",
        [
            "s_indented.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_indented.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "s_twolets.fs",
        "module M\nmodule N =\n    let a = 1\n    )\nlet c = 3\nlet d = 4\n",
        [
            "s_twolets.fs(4,5,4,6): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "s_twolets.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "ns_let.fs",
        "namespace Q\nmodule N =\n    1 )\nlet c = 3\n",
        [
            "ns_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "ns_let.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "ns_ns.fs",
        "namespace Q\nmodule N =\n    1 )\nnamespace R\nlet c = 3\n",
        [
            "ns_ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "ns_ns.fs(4,1,4,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "ns_none.fs",
        "namespace Q\nmodule N =\n    1 )\n",
        [
            "ns_none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "ns_none.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "deep.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    let b = 2\nlet c = 3\n",
        [
            "deep.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "deep.fs(6,1,6,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "deep_none.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n",
        [
            "deep_none.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "deep_none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "eof_nonl.fs",
        "module M\nmodule N =\n    1 )",
        [
            "eof_nonl.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "eof_nonl.fs(3,1,3,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "f_num.fs",
        "module M\nmodule N =\n    1 )\n1\n",
        [
            "f_num.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_num.fs(4,1,4,2): error FS0010: Unexpected integer literal in implementation file"
        ]

        "f_str.fs",
        "module M\nmodule N =\n    1 )\n\"a\"\n",
        [
            "f_str.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_str.fs(4,1,4,4): error FS0010: Unexpected string literal in implementation file"
        ]

        "f_lparen.fs",
        "module M\nmodule N =\n    1 )\n(1)\n",
        [
            "f_lparen.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_lparen.fs(4,1,4,2): error FS0010: Unexpected symbol '(' in implementation file"
        ]

        "f_lbrack.fs",
        "module M\nmodule N =\n    1 )\n[1]\n",
        [
            "f_lbrack.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_lbrack.fs(4,1,4,2): error FS0010: Unexpected symbol '[' in implementation file"
        ]

        "f_if.fs",
        "module M\nmodule N =\n    1 )\nif true then ()\n",
        [
            "f_if.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_if.fs(4,1,4,3): error FS0010: Unexpected keyword 'if' in implementation file"
        ]

        "f_match.fs",
        "module M\nmodule N =\n    1 )\nmatch 1 with _ -> ()\n",
        [
            "f_match.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_match.fs(4,1,4,6): error FS0010: Unexpected keyword 'match' in implementation file"
        ]

        "f_fun.fs",
        "module M\nmodule N =\n    1 )\nfun x -> x\n",
        [
            "f_fun.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_fun.fs(4,1,4,4): error FS0010: Unexpected keyword 'fun' in implementation file"
        ]

        "f_under.fs",
        "module M\nmodule N =\n    1 )\n_\n",
        [
            "f_under.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_under.fs(4,1,4,2): error FS0010: Unexpected symbol '_' in implementation file"
        ]

        "f_private.fs",
        "module M\nmodule N =\n    1 )\nprivate x\n",
        [
            "f_private.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_private.fs(4,1,4,8): error FS0010: Unexpected keyword 'private' in implementation file"
        ]

        "f_inline.fs",
        "module M\nmodule N =\n    1 )\ninline x\n",
        [
            "f_inline.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_inline.fs(4,1,4,7): error FS0010: Unexpected keyword 'inline' in implementation file"
        ]

        "f_rbrack.fs",
        "module M\nmodule N =\n    1 )\n]\n",
        [
            "f_rbrack.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_rbrack.fs(4,1,4,2): error FS0010: Unexpected symbol ']' in implementation file"
        ]

        "f_rbrace.fs",
        "module M\nmodule N =\n    1 )\n}\n",
        [
            "f_rbrace.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_rbrace.fs(4,1,4,2): error FS0010: Unexpected symbol '}' in implementation file"
        ]

        "f_end.fs",
        "module M\nmodule N =\n    1 )\nend\n",
        [
            "f_end.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_end.fs(4,1,4,4): error FS0010: Unexpected keyword 'end' in implementation file"
        ]

        "f_eq.fs",
        "module M\nmodule N =\n    1 )\n= 1\n",
        [
            "f_eq.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_eq.fs(4,1,4,2): error FS0010: Unexpected symbol '=' in implementation file"
        ]

        "f_bar.fs",
        "module M\nmodule N =\n    1 )\n| A\n",
        [
            "f_bar.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_bar.fs(4,1,4,2): error FS0010: Unexpected symbol '|' in implementation file"
        ]

        "f_hash.fs",
        "module M\nmodule N =\n    1 )\n#if X\nlet c = 3\n#endif\n",
        [
            "f_hash.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_hash.fs(7,1,7,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "f_inner_error.fs",
        "module M\nmodule N =\n    1 )\n    let b = )\nlet c = 3\n",
        [
            "f_inner_error.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_inner_error.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "f_inner_error_end.fs",
        "module M\nmodule N =\n    1 )\n    let b = )\n",
        [
            "f_inner_error_end.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_inner_error_end.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "f_root_error.fs",
        "module M\nmodule N =\n    1 )\nlet c = )\n",
        [
            "f_root_error.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_root_error.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "f_val.fs",
        "module M\nmodule N =\n    1 )\nval c: int\n",
        [
            "f_val.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_val.fs(4,1,4,4): error FS0010: Unexpected keyword 'val' in implementation file"
        ]

        "f_typeabbr.fs",
        "module M\nmodule N =\n    1 )\ntype T = int\nlet c = 3\n",
        [
            "f_typeabbr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_typeabbr.fs(4,1,4,5): error FS0010: Unexpected keyword 'type' in implementation file"
        ]

        "f_nested_ok.fs",
        "module M\nmodule N =\n    1 )\nmodule O =\n    let d = 4\n",
        [
            "f_nested_ok.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_nested_ok.fs(4,1,4,7): error FS0010: Unexpected keyword 'module' in implementation file"
        ]

        "crlf_none.fs",
        "module M\r\nmodule N =\r\n    1 )\r\n",
        [
            "crlf_none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "crlf_none.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "trail_space.fs",
        "module M\nmodule N =\n    1 )\n   ",
        [
            "trail_space.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "trail_space.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "trail_comment.fs",
        "module M\nmodule N =\n    1 )\n// c",
        [
            "trail_comment.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "trail_comment.fs(4,1,4,5): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "r_let.fs",
        "module M\nmodule N =\n    let a = 1\n    type R = { X: ) }\nlet c = 3\n",
        [ "r_let.fs(4,19,4,20): error FS0010: Unexpected symbol ')' in field declaration" ]

        "clean_let__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\nlet c = 3\n",
        [
            "clean_let__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_let__let.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_let__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n",
        [
            "clean_let__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_let__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_let__expr.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\nf 1\n",
        [
            "clean_let__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_let__expr.fs(5,1,5,2): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_let__ns.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\nnamespace Q\n",
        [
            "clean_let__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_let__ns.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_expr__let.fs",
        "module M\nmodule N =\n    1 )\n    f 2\nlet c = 3\n",
        [
            "clean_expr__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_expr__let.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "clean_expr__none.fs",
        "module M\nmodule N =\n    1 )\n    f 2\n",
        [
            "clean_expr__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_expr__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_expr__expr.fs",
        "module M\nmodule N =\n    1 )\n    f 2\nf 1\n",
        [
            "clean_expr__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_expr__expr.fs(5,1,5,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "clean_open__let.fs",
        "module M\nmodule N =\n    1 )\n    open System\nlet c = 3\n",
        [
            "clean_open__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_open__let.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "clean_open__none.fs",
        "module M\nmodule N =\n    1 )\n    open System\n",
        [
            "clean_open__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_open__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_open__expr.fs",
        "module M\nmodule N =\n    1 )\n    open System\nf 1\n",
        [
            "clean_open__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_open__expr.fs(5,1,5,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "clean_mod__let.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let d = 4\nlet c = 3\n",
        [
            "clean_mod__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_mod__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_mod__none.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let d = 4\n",
        [
            "clean_mod__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_mod__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_mod__expr.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let d = 4\nf 1\n",
        [
            "clean_mod__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_mod__expr.fs(6,1,6,2): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_mod__ns.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let d = 4\nnamespace Q\n",
        [
            "clean_mod__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_mod__ns.fs(6,1,6,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "two_lets__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    let e = 3\nlet c = 3\n",
        [
            "two_lets__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "two_lets__let.fs(5,5,5,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "two_lets__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    let e = 3\n",
        [
            "two_lets__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "two_lets__none.fs(5,5,5,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "two_lets__expr.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    let e = 3\nf 1\n",
        [
            "two_lets__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "two_lets__expr.fs(5,5,5,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "two_lets__ns.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    let e = 3\nnamespace Q\n",
        [
            "two_lets__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "two_lets__ns.fs(5,5,5,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "stray__let.fs",
        "module M\nmodule N =\n    1 )\n    )\nlet c = 3\n",
        [
            "stray__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "stray__let.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "stray__none.fs",
        "module M\nmodule N =\n    1 )\n    )\n",
        [
            "stray__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "stray__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "stray__expr.fs",
        "module M\nmodule N =\n    1 )\n    )\nf 1\n",
        [
            "stray__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "stray__expr.fs(5,1,5,2): error FS0010: Unexpected identifier in implementation file"
        ]

        "g_app__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = g f(1)\nlet c = 3\n",
        [
            "g_app__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "g_app__let.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "g_app__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = g f(1)\n",
        [
            "g_app__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "g_app__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "g_app__expr.fs",
        "module M\nmodule N =\n    1 )\n    let b = g f(1)\nf 1\n",
        [
            "g_app__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "g_app__expr.fs(5,1,5,2): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "g_app__ns.fs",
        "module M\nmodule N =\n    1 )\n    let b = g f(1)\nnamespace Q\n",
        [
            "g_app__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "g_app__ns.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "dot__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = _.A\nlet c = 3\n",
        [
            "dot__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "dot__let.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "dot__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = _.A\n",
        [
            "dot__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "dot__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "dot__expr.fs",
        "module M\nmodule N =\n    1 )\n    let b = _.A\nf 1\n",
        [
            "dot__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "dot__expr.fs(5,1,5,2): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "dot__ns.fs",
        "module M\nmodule N =\n    1 )\n    let b = _.A\nnamespace Q\n",
        [
            "dot__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "dot__ns.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "do__let.fs",
        "module M\nmodule N =\n    1 )\n    do ()\nlet c = 3\n",
        [
            "do__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "do__let.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "do__none.fs",
        "module M\nmodule N =\n    1 )\n    do ()\n",
        [
            "do__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "do__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "attr_let__let.fs",
        "module M\nmodule N =\n    1 )\n    [<A>]\n    let b = 2\nlet c = 3\n",
        [
            "attr_let__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "attr_let__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "attr_let__none.fs",
        "module M\nmodule N =\n    1 )\n    [<A>]\n    let b = 2\n",
        [
            "attr_let__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "attr_let__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_rec__let.fs",
        "module M\nmodule N =\n    1 )\n    let rec b = 2\nlet c = 3\n",
        [
            "let_rec__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_rec__let.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_rec__none.fs",
        "module M\nmodule N =\n    1 )\n    let rec b = 2\n",
        [
            "let_rec__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_rec__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "expr_let__let.fs",
        "module M\nmodule N =\n    1 )\n    f 2\n    let b = 2\nlet c = 3\n",
        [
            "expr_let__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "expr_let__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "expr_let__none.fs",
        "module M\nmodule N =\n    1 )\n    f 2\n    let b = 2\n",
        [
            "expr_let__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "expr_let__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_expr__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    f 2\nlet c = 3\n",
        [
            "let_expr__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_expr__let.fs(5,5,5,6): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_expr__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    f 2\n",
        [
            "let_expr__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_expr__none.fs(5,5,5,6): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "open_let__let.fs",
        "module M\nmodule N =\n    1 )\n    open System\n    let b = 2\nlet c = 3\n",
        [
            "open_let__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "open_let__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "open_let__none.fs",
        "module M\nmodule N =\n    1 )\n    open System\n    let b = 2\n",
        [
            "open_let__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "open_let__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_open__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    open System\nlet c = 3\n",
        [
            "let_open__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_open__let.fs(5,5,5,9): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_open__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    open System\n",
        [
            "let_open__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_open__none.fs(5,5,5,9): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_multi__let.fs",
        "module M\nmodule N =\n    1 )\n    let b =\n        2\nlet c = 3\n",
        [
            "let_multi__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_multi__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_multi__none.fs",
        "module M\nmodule N =\n    1 )\n    let b =\n        2\n",
        [
            "let_multi__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_multi__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_stray__let.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    )\nlet c = 3\n",
        [
            "let_stray__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_stray__let.fs(5,5,5,6): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "let_stray__none.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    )\n",
        [
            "let_stray__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "let_stray__none.fs(5,5,5,6): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "stray_let__let.fs",
        "module M\nmodule N =\n    1 )\n    )\n    let b = 2\nlet c = 3\n",
        [
            "stray_let__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "stray_let__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "stray_let__none.fs",
        "module M\nmodule N =\n    1 )\n    )\n    let b = 2\n",
        [
            "stray_let__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "stray_let__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "mod_two__let.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let d = 4\n        let e = 5\nlet c = 3\n",
        [
            "mod_two__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "mod_two__let.fs(6,9,6,12): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "mod_two__none.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        let d = 4\n        let e = 5\n",
        [
            "mod_two__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "mod_two__none.fs(6,9,6,12): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "ns_expr.fs",
        "namespace Q\nmodule N =\n    1 )\n    f 2\nnamespace R\n",
        [
            "ns_expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "ns_expr.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "ns_stray.fs",
        "namespace Q\nmodule N =\n    1 )\n    )\nnamespace R\n",
        [
            "ns_stray.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "ns_stray.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_type.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    type T = int\n",
        [
            "n_type.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_type.fs(5,5,5,9): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_module.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    module O =\n        let d = 4\n",
        [
            "n_module.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_module.fs(5,5,5,11): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_do.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    do ()\n",
        [
            "n_do.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_do.fs(5,5,5,7): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_attr.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    [<A>]\n    let e = 3\n",
        [
            "n_attr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_attr.fs(5,5,5,7): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_exception.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    exception E\n",
        [
            "n_exception.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_exception.fs(5,5,5,14): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_num.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    1\n",
        [
            "n_num.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_num.fs(5,5,5,6): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_str.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    \"a\"\n",
        [
            "n_str.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_str.fs(5,5,5,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_lbrack.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n    [1]\n",
        [
            "n_lbrack.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_lbrack.fs(5,5,5,6): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_ident_root.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\nx\n",
        [
            "n_ident_root.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_ident_root.fs(5,1,5,2): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_attr_root.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\n[<A>]\nlet c = 3\n",
        [
            "n_attr_root.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_attr_root.fs(5,1,5,3): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_type_root.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\ntype T = int\n",
        [
            "n_type_root.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_type_root.fs(5,1,5,5): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_module_root.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\nmodule O =\n    let d = 4\n",
        [
            "n_module_root.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_module_root.fs(5,1,5,7): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "nsfile_let.fs",
        "namespace Q\nmodule N =\n    1 )\n    let b = 2\nnamespace R\n",
        [
            "nsfile_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "nsfile_let.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "nsfile_let_root.fs",
        "namespace Q\nmodule N =\n    1 )\n    let b = 2\nlet c = 3\n",
        [
            "nsfile_let_root.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "nsfile_let_root.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "do_nested_let.fs",
        "module M\nmodule N =\n    1 )\n    do ()\n    let e = 3\n",
        [
            "do_nested_let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "do_nested_let.fs(5,5,5,8): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "o_let_let.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    let b = 2\n    let e = 3\nlet c = 3\n",
        [
            "o_let_let.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_let_let.fs(7,1,7,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "o_let_none.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    let b = 2\n",
        [
            "o_let_none.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_let_none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "o_let_expr.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    let b = 2\n    f 1\n",
        [
            "o_let_expr.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_let_expr.fs(7,1,7,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "o_do_root.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    do ()\nlet c = 3\n",
        [
            "o_do_root.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_do_root.fs(6,1,6,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "o_type_root.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    type T = int\nlet c = 3\n",
        [
            "o_type_root.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_type_root.fs(6,1,6,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]
    ]

    let private unmodeledNestedRecoveryEndCases = [
        "f_letbang.fs",
        "module M\nmodule N =\n    1 )\nlet! c = 3\n",
        [
            "f_letbang.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "f_letbang.fs(4,1,4,5): error FS0010: Unexpected binder keyword in implementation file"
        ]

        "anon_let.fs",
        "module N =\n    1 )\nlet c = 3\n",
        [
            "anon_let.fs(2,7,2,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "anon_let.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration. When using a module declaration at the start of a file the '=' sign is not allowed. If this is a top-level module, consider removing the = to resolve this error."
        ]

        "anon_none.fs",
        "module N =\n    1 )\n",
        [
            "anon_none.fs(2,7,2,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "anon_none.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration. When using a module declaration at the start of a file the '=' sign is not allowed. If this is a top-level module, consider removing the = to resolve this error."
        ]

        "t_let.fs",
        "module M\nmodule N =\n    let a = 1\n    type T = )\nlet c = 3\n",
        [
            "t_let.fs(4,14,4,15): error FS0010: Unexpected symbol ')' in type definition"
            "t_let.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "t_none.fs",
        "module M\nmodule N =\n    let a = 1\n    type T = )\n",
        [
            "t_none.fs(4,14,4,15): error FS0010: Unexpected symbol ')' in type definition"
            "t_none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "t_first.fs",
        "module M\nmodule N =\n    type T = )\nlet c = 3\n",
        [
            "t_first.fs(3,14,3,15): error FS0010: Unexpected symbol ')' in type definition"
            "t_first.fs(4,1,4,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "clean_expr__ns.fs",
        "module M\nmodule N =\n    1 )\n    f 2\nnamespace Q\n",
        [
            "clean_expr__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_expr__ns.fs(1,1,3,6): error FS0530: Only '#' compiler directives may occur prior to the first 'namespace' declaration"
            "clean_expr__ns.fs(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "clean_open__ns.fs",
        "module M\nmodule N =\n    1 )\n    open System\nnamespace Q\n",
        [
            "clean_open__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_open__ns.fs(1,1,3,6): error FS0530: Only '#' compiler directives may occur prior to the first 'namespace' declaration"
            "clean_open__ns.fs(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "clean_type__let.fs",
        "module M\nmodule N =\n    1 )\n    type T = int\nlet c = 3\n",
        [
            "clean_type__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_type__let.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_type__none.fs",
        "module M\nmodule N =\n    1 )\n    type T = int\n",
        [
            "clean_type__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_type__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_type__expr.fs",
        "module M\nmodule N =\n    1 )\n    type T = int\nf 1\n",
        [
            "clean_type__expr.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_type__expr.fs(5,1,5,2): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "clean_type__ns.fs",
        "module M\nmodule N =\n    1 )\n    type T = int\nnamespace Q\n",
        [
            "clean_type__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "clean_type__ns.fs(5,1,5,10): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "stray__ns.fs",
        "module M\nmodule N =\n    1 )\n    )\nnamespace Q\n",
        [
            "stray__ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "stray__ns.fs(1,1,3,6): error FS0530: Only '#' compiler directives may occur prior to the first 'namespace' declaration"
            "stray__ns.fs(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "exception__let.fs",
        "module M\nmodule N =\n    1 )\n    exception E\nlet c = 3\n",
        [
            "exception__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "exception__let.fs(5,1,5,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]

        "exception__none.fs",
        "module M\nmodule N =\n    1 )\n    exception E\n",
        [
            "exception__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "exception__none.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "type_type__let.fs",
        "module M\nmodule N =\n    1 )\n    type T = int\n    type U = int\nlet c = 3\n",
        [
            "type_type__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "type_type__let.fs(5,5,5,9): error FS0010: Unexpected keyword 'type' in implementation file"
        ]

        "type_type__none.fs",
        "module M\nmodule N =\n    1 )\n    type T = int\n    type U = int\n",
        [
            "type_type__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "type_type__none.fs(5,5,5,9): error FS0010: Unexpected keyword 'type' in implementation file"
        ]

        "mod_let__let.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        f 1\nlet c = 3\n",
        [
            "mod_let__let.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "mod_let__let.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "mod_let__none.fs",
        "module M\nmodule N =\n    1 )\n    module O =\n        f 1\n",
        [
            "mod_let__none.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "mod_let__none.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "n_letbang_root.fs",
        "module M\nmodule N =\n    1 )\n    let b = 2\nlet! c = 3\n",
        [
            "n_letbang_root.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "n_letbang_root.fs(5,1,5,5): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "modfile_ns.fs",
        "module M\nmodule N =\n    1 )\nnamespace R\n",
        [
            "modfile_ns.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "modfile_ns.fs(1,1,3,6): error FS0530: Only '#' compiler directives may occur prior to the first 'namespace' declaration"
            "modfile_ns.fs(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "letrec_and.fs",
        "module M\nmodule N =\n    1 )\n    let rec b = 2\n    and c = 3\nlet d = 4\n",
        [
            "letrec_and.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "letrec_and.fs(5,5,5,8): error FS0010: Unexpected keyword 'and' in implementation file"
        ]

        "o_inner_let.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n        let x = 1\n    let b = 2\nlet c = 3\n",
        [
            "o_inner_let.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_inner_let.fs(6,5,6,8): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "o_inner_let_only.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n        let x = 1\nlet c = 3\n",
        [
            "o_inner_let_only.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_inner_let_only.fs(6,1,6,4): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "o_mod.fs",
        "module M\nmodule N =\n    module O =\n        1 )\n    module P =\n        let d = 4\nlet c = 3\n",
        [
            "o_mod.fs(4,11,4,12): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "o_mod.fs(7,1,7,4): error FS0010: Unexpected keyword 'let' or 'use' in implementation file"
        ]
    ]

    let private attributedModuleCases = [
        "am_ok.fs", "module M\n[<AutoOpen>]\nmodule N =\n    let a = 1\n", []

        "am_two_lists.fs",
        "module M\n[<AutoOpen>]\n[<RequireQualifiedAccess>]\nmodule N =\n    let a = 1\n",
        []

        "am_args.fs",
        "module M\n[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]\nmodule N =\n    let a = 1\n",
        []

        "am_nested.fs",
        "module M\nmodule N =\n    [<AutoOpen>]\n    module O =\n        let a = 1\n",
        []

        "am_ns.fs", "namespace Q\n[<AutoOpen>]\nmodule N =\n    let a = 1\n", []

        "am_error.fs",
        "module M\n[<AutoOpen>]\nmodule N =\n    let a = )\nlet b = 2\n",
        [ "am_error.fs(4,13,4,14): error FS0010: Unexpected symbol ')' in binding" ]

        "am_anon.fs",
        "[<AutoOpen>]\nmodule N =\n    let a = 1\n",
        [
            "am_anon.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration. When using a module declaration at the start of a file the '=' sign is not allowed. If this is a top-level module, consider removing the = to resolve this error."
        ]

        "am_sig.fsi", "module M\n[<AutoOpen>]\nmodule N =\n    val a: int\n", []

        "am_access.fs", "module M\n[<AutoOpen>]\nmodule private N =\n    let a = 1\n", []

        "ac_internal.fs", "module M\nmodule internal N =\n    let a = 1\n", []

        "ac_public.fs", "module M\nmodule public N =\n    let a = 1\n", []

        "ac_error.fs",
        "module M\nmodule private N =\n    let a = )\nlet b = 2\n",
        [ "ac_error.fs(3,13,3,14): error FS0010: Unexpected symbol ')' in binding" ]

        "ac_sig.fsi", "module M\nmodule private N =\n    val a: int\n", []

        "ac_anon.fs",
        "module private N =\n    let a = 1\n",
        [
            "ac_anon.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration. When using a module declaration at the start of a file the '=' sign is not allowed. If this is a top-level module, consider removing the = to resolve this error."
        ]
    ]

    let private unmodeledAttributedModuleCases = [
        "am_same_line.fs",
        "module M\n[<AutoOpen>] module N =\n    let a = 1\n",
        [
            "am_same_line.fs(3,5,3,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:14). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "am_same_line.fs(3,5,3,8): error FS0010: Incomplete structured construct at or before this point in definition"
        ]

        "ao_open.fs",
        "module M\n[<A>]\nopen System\n",
        [ "ao_open.fs(3,1,3,5): error FS0010: Unexpected keyword 'open' in definition" ]

        "ao_open_line.fs",
        "module M\n[<A>] open System\n",
        [ "ao_open_line.fs(2,7,2,11): error FS0010: Unexpected keyword 'open' in definition" ]

        "ac_twice.fs",
        "module M\nmodule private private N =\n    let a = 1\n",
        [
            "ac_twice.fs(2,16,2,23): error FS0010: Unexpected keyword 'private' in definition. Expected identifier, 'global' or other token."
            "ac_twice.fs(2,1,2,23): error FS0534: A module abbreviation must be a simple name, not a path"
        ]
    ]

    let private letKeywordCases = [
        "u_simple.fs",
        "module M\nuse c = 3\n",
        [
            "u_simple.fs(2,1,2,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_multi.fs",
        "module M\nuse c =\n    3\nlet d = 4\n",
        [
            "u_multi.fs(2,1,3,6): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_two.fs",
        "module M\nuse c = 3\nuse d = 4\n",
        [
            "u_two.fs(2,1,2,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
            "u_two.fs(3,1,3,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_rec.fs",
        "module M\nuse rec c = 3\n",
        [
            "u_rec.fs(2,1,2,14): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_attr.fs",
        "module M\n[<A>]\nuse c = 3\n",
        [
            "u_attr.fs(3,1,3,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_nested.fs",
        "module M\nmodule N =\n    use c = 3\n",
        [
            "u_nested.fs(3,5,3,14): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_ns.fs",
        "namespace Q\nuse c = 3\n",
        [
            "u_ns.fs(2,1,2,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_anon.fs",
        "use c = 3\n",
        [
            "u_anon.fs(1,1,1,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
            "u_anon.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "u_err.fs",
        "module M\nuse c = )\n",
        [
            "u_err.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding"
            "u_err.fs(2,1,2,8): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_func.fs",
        "module M\nuse f x = x\n",
        [
            "u_func.fs(2,1,2,12): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_and.fs",
        "module M\nuse c = 3\nand d = 4\n",
        [
            "u_and.fs(2,1,3,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
            "u_and.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "u_tuple.fs",
        "module M\nuse (c, d) = (3, 4)\n",
        [
            "u_tuple.fs(2,1,2,20): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_access.fs",
        "module M\nuse private c = 3\n",
        [
            "u_access.fs(2,1,2,18): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "u_after_err.fs",
        "module M\nlet a = )\nuse c = 3\n",
        [
            "u_after_err.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding"
            "u_after_err.fs(3,1,3,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "d_file.fs",
        "module M\n)\nuse c = 3\n",
        [
            "d_file.fs(2,1,2,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "d_nested.fs",
        "module M\nmodule N =\n    1 )\n    use c = 3\nlet d = 4\n",
        [
            "d_nested.fs(3,7,3,8): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "d_nested.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "u_comment.fs",
        "module M\nuse c = 3 // x\n",
        [
            "u_comment.fs(2,1,2,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "r_trail.fs",
        "module M\nuse c = 1 )\n",
        [
            "r_trail.fs(2,11,2,12): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
            "r_trail.fs(2,1,2,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "r_block.fs",
        "module M\nuse c =\n    )\n",
        [
            "r_block.fs(3,5,3,6): error FS0010: Unexpected symbol ')' in binding"
            "r_block.fs(2,1,2,8): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "r_eq.fs",
        "module M\nuse c x )\n",
        [
            "r_eq.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding. Expected '=' or other token."
            "r_eq.fs(2,1,2,8): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "l_and2.fs",
        "module M\nlet a = 1\nand b = 2\n",
        [
            "l_and2.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_rec.fs", "module M\nlet rec a = 1\nand b = 2\n", []

        "l_three.fs",
        "module M\nlet a = 1\nand b = 2\nand c = 3\n",
        [
            "l_three.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_err_first.fs",
        "module M\nlet a = )\nand b = 2\n",
        [ "l_err_first.fs(2,9,2,10): error FS0010: Unexpected symbol ')' in binding" ]

        "l_err_second.fs",
        "module M\nlet a = 1\nand b = )\n",
        [
            "l_err_second.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in binding"
            "l_err_second.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_attr.fs",
        "module M\n[<A>]\nlet a = 1\nand b = 2\n",
        [
            "l_attr.fs(3,1,3,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_then.fs",
        "module M\nlet a = 1\nand b = 2\nlet c = 3\n",
        [
            "l_then.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_twice.fs",
        "module M\nlet a = 1\nand b = 2\nlet c = 3\nand d = 4\n",
        [
            "l_twice.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
            "l_twice.fs(4,1,4,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_anon.fs",
        "let a = 1\nand b = 2\n",
        [
            "l_anon.fs(1,1,1,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
            "l_anon.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "l_ns.fs",
        "namespace Q\nlet a = 1\nand b = 2\n",
        [
            "l_ns.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_multi.fs",
        "module M\nlet a =\n    1\nand b =\n    2\n",
        [
            "l_multi.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_func.fs",
        "module M\nlet f x = x\nand g y = y\n",
        [
            "l_func.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "u_and2.fs",
        "module M\nuse a = 1\nand b = 2\n",
        [
            "u_and2.fs(2,1,3,10): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
            "u_and2.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_access.fs",
        "module M\nlet private a = 1\nand b = 2\n",
        [
            "l_access.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "l_nested.fs",
        "module N\nmodule O =\n    let a = 1\n    and b = 2\n",
        [
            "l_nested.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "u_sig.fsi",
        "module M\nuse c: int\n",
        [
            "u_sig.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]
    ]

    let private unmodeledLetKeywordCases = [
        "u_local.fs", "module M\nlet f () =\n    use c = 3\n    c\n", []

        "d_first.fs",
        "module M\nmodule N =\n    )\n    use c = 3\nlet d = 4\n",
        [
            "d_first.fs(3,5,3,6): error FS0010: Unexpected symbol ')' in definition"
            "d_first.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "l_and.fs",
        "module M\nlet a = 1 and b = 2\n",
        [
            "l_and.fs(2,11,2,14): error FS0010: Unexpected keyword 'and' in binding. Expected incomplete structured construct at or before this point or other token."
            "l_and.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding. Expected incomplete structured construct at or before this point or other token."
            "l_and.fs(2,1,2,4): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "l_and.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
            "l_and.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "r_inner.fs",
        "module M\nuse c = f (\n    1 ]\n",
        [
            "r_inner.fs(3,7,3,8): error FS0010: Unexpected symbol ']' in expression"
            "r_inner.fs(2,1,3,6): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

    ]

    let private nestedAndCases = [
        "n_deep.fs",
        "module N\nmodule O =\n    module P =\n        let a = 1\n        and b = 2\n",
        [
            "n_deep.fs(4,9,4,12): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_err.fs",
        "module N\nmodule O =\n    let a = 1\n    and b = )\n",
        [
            "n_err.fs(4,13,4,14): error FS0010: Unexpected symbol ')' in binding"
            "n_err.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_multi.fs",
        "module N\nmodule O =\n    let a =\n        1\n    and b =\n        2\n",
        [
            "n_multi.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_ns.fs",
        "namespace Q\nmodule O =\n    let a = 1\n    and b = 2\n",
        [
            "n_ns.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_rec.fs", "module N\nmodule O =\n    let rec a = 1\n    and b = 2\n", []

        "n_root_after.fs",
        "module N\nmodule O =\n    let a = 1\n    and b = 2\nlet c = 3\n",
        [
            "n_root_after.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_then.fs",
        "module N\nmodule O =\n    let a = 1\n    and b = 2\n    let c = 3\n",
        [
            "n_then.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_three.fs",
        "module N\nmodule O =\n    let a = 1\n    and b = 2\n    and c = 3\n",
        [
            "n_three.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_two.fs",
        "module N\nmodule O =\n    let a = 1\n    and b = 2\n",
        [
            "n_two.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]

        "n_use.fs",
        "module N\nmodule O =\n    use a = 1\n    and b = 2\n",
        [
            "n_use.fs(3,5,4,14): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
            "n_use.fs(3,5,3,8): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]
    ]

    let private unmodeledNestedAndCases = [
        "n_indented_and.fs",
        "module N\nlet a = 1\n    and b = 2\n",
        [
            "n_indented_and.fs(2,1,2,4): error FS0576: The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"
        ]
    ]

    let private signatureRecoveryCases = [
        "mod_let.fsi",
        "module M\nlet c: int\n",
        [
            "mod_let.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_use.fsi",
        "module M\nuse c: int\n",
        [
            "mod_use.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_letrec.fsi",
        "module M\nlet rec c: int\n",
        [
            "mod_letrec.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_letbang.fsi",
        "module M\nlet! c = 1\n",
        [
            "mod_letbang.fsi(2,1,2,5): error FS0010: Unexpected binder keyword. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_do.fsi",
        "module M\ndo ()\n",
        [
            "mod_do.fsi(2,1,2,3): error FS0010: Unexpected keyword 'do'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_ident.fsi",
        "module M\nc: int\n",
        [
            "mod_ident.fsi(2,1,2,2): error FS0010: Unexpected identifier. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_num.fsi",
        "module M\n1\n",
        [
            "mod_num.fsi(2,1,2,2): error FS0010: Unexpected integer literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_str.fsi",
        "module M\n\"a\"\n",
        [
            "mod_str.fsi(2,1,2,4): error FS0010: Unexpected string literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_lparen.fsi",
        "module M\n(c)\n",
        [
            "mod_lparen.fsi(2,1,2,2): error FS0010: Unexpected symbol '('. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_lbrack.fsi",
        "module M\n[1]\n",
        [
            "mod_lbrack.fsi(2,1,2,2): error FS0010: Unexpected symbol '['. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_if.fsi",
        "module M\nif true then ()\n",
        [
            "mod_if.fsi(2,1,2,3): error FS0010: Unexpected keyword 'if'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_match.fsi",
        "module M\nmatch 1 with _ -> ()\n",
        [
            "mod_match.fsi(2,1,2,6): error FS0010: Unexpected keyword 'match'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_fun.fsi",
        "module M\nfun x -> x\n",
        [
            "mod_fun.fsi(2,1,2,4): error FS0010: Unexpected keyword 'fun'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_bar.fsi",
        "module M\n| A\n",
        [
            "mod_bar.fsi(2,1,2,2): error FS0010: Unexpected symbol '|'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_under.fsi",
        "module M\n_\n",
        [
            "mod_under.fsi(2,1,2,2): error FS0010: Unexpected symbol '_'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_inline.fsi",
        "module M\ninline c: int\n",
        [
            "mod_inline.fsi(2,1,2,7): error FS0010: Unexpected keyword 'inline'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_and.fsi",
        "module M\nand c: int\n",
        [
            "mod_and.fsi(2,1,2,4): error FS0010: Unexpected keyword 'and'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_rbrace.fsi",
        "module M\n}\n",
        [
            "mod_rbrace.fsi(2,1,2,2): error FS0010: Unexpected symbol '}'. Expected incomplete structured construct at or before this point or other token."
        ]

        "mod_end.fsi",
        "module M\nend\n",
        [
            "mod_end.fsi(2,1,2,4): error FS0010: Unexpected keyword 'end'. Expected incomplete structured construct at or before this point or other token."
        ]

        "nested_let.fsi",
        "module M\nmodule N =\n    let c: int\n",
        [
            "nested_let.fsi(3,5,3,8): error FS0010: Unexpected keyword 'let' or 'use' in signature file"
            "nested_let.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_use.fsi",
        "module M\nmodule N =\n    use c: int\n",
        [
            "nested_use.fsi(3,5,3,8): error FS0010: Unexpected keyword 'let' or 'use' in signature file"
            "nested_use.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_letrec.fsi",
        "module M\nmodule N =\n    let rec c: int\n",
        [
            "nested_letrec.fsi(3,5,3,8): error FS0010: Unexpected keyword 'let' or 'use' in signature file"
            "nested_letrec.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_letbang.fsi",
        "module M\nmodule N =\n    let! c = 1\n",
        [
            "nested_letbang.fsi(3,5,3,9): error FS0010: Unexpected binder keyword in signature file"
            "nested_letbang.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_do.fsi",
        "module M\nmodule N =\n    do ()\n",
        [
            "nested_do.fsi(3,5,3,7): error FS0010: Unexpected keyword 'do' in signature file"
            "nested_do.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_num.fsi",
        "module M\nmodule N =\n    1\n",
        [
            "nested_num.fsi(3,5,3,6): error FS0010: Unexpected integer literal in signature file"
            "nested_num.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_str.fsi",
        "module M\nmodule N =\n    \"a\"\n",
        [
            "nested_str.fsi(3,5,3,8): error FS0010: Unexpected string literal in signature file"
            "nested_str.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_lparen.fsi",
        "module M\nmodule N =\n    (c)\n",
        [
            "nested_lparen.fsi(3,5,3,6): error FS0010: Unexpected symbol '(' in signature file"
            "nested_lparen.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_lbrack.fsi",
        "module M\nmodule N =\n    [1]\n",
        [
            "nested_lbrack.fsi(3,5,3,6): error FS0010: Unexpected symbol '[' in signature file"
            "nested_lbrack.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_if.fsi",
        "module M\nmodule N =\n    if true then ()\n",
        [
            "nested_if.fsi(3,5,3,7): error FS0010: Unexpected keyword 'if' in signature file"
            "nested_if.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_match.fsi",
        "module M\nmodule N =\n    match 1 with _ -> ()\n",
        [
            "nested_match.fsi(3,5,3,10): error FS0010: Unexpected keyword 'match' in signature file"
            "nested_match.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_fun.fsi",
        "module M\nmodule N =\n    fun x -> x\n",
        [
            "nested_fun.fsi(3,5,3,8): error FS0010: Unexpected keyword 'fun' in signature file"
            "nested_fun.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_eq.fsi",
        "module M\nmodule N =\n    = 1\n",
        [
            "nested_eq.fsi(3,5,3,6): error FS0010: Unexpected symbol '=' in signature file"
            "nested_eq.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_bar.fsi",
        "module M\nmodule N =\n    | A\n",
        [
            "nested_bar.fsi(3,5,3,6): error FS0010: Unexpected symbol '|' in signature file"
            "nested_bar.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_under.fsi",
        "module M\nmodule N =\n    _\n",
        [
            "nested_under.fsi(3,5,3,6): error FS0010: Unexpected symbol '_' in signature file"
            "nested_under.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_inline.fsi",
        "module M\nmodule N =\n    inline c: int\n",
        [
            "nested_inline.fsi(3,5,3,11): error FS0010: Unexpected keyword 'inline' in signature file"
            "nested_inline.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_and.fsi",
        "module M\nmodule N =\n    and c: int\n",
        [
            "nested_and.fsi(3,5,3,8): error FS0010: Unexpected keyword 'and' in signature file"
            "nested_and.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_rbrace.fsi",
        "module M\nmodule N =\n    }\n",
        [
            "nested_rbrace.fsi(3,5,3,6): error FS0010: Unexpected symbol '}' in signature file"
            "nested_rbrace.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_end.fsi",
        "module M\nmodule N =\n    end\n",
        [
            "nested_end.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "nested_end.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "after_let.fsi",
        "module M\nval a: int\nlet c: int\n",
        [
            "after_let.fsi(3,1,3,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_use.fsi",
        "module M\nval a: int\nuse c: int\n",
        [
            "after_use.fsi(3,1,3,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_letrec.fsi",
        "module M\nval a: int\nlet rec c: int\n",
        [
            "after_letrec.fsi(3,1,3,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_letbang.fsi",
        "module M\nval a: int\nlet! c = 1\n",
        [
            "after_letbang.fsi(3,1,3,5): error FS0010: Unexpected binder keyword. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_do.fsi",
        "module M\nval a: int\ndo ()\n",
        [
            "after_do.fsi(3,1,3,3): error FS0010: Unexpected keyword 'do'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_ident.fsi",
        "module M\nval a: int\nc: int\n",
        [
            "after_ident.fsi(3,1,3,2): error FS0010: Unexpected identifier. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_num.fsi",
        "module M\nval a: int\n1\n",
        [
            "after_num.fsi(3,1,3,2): error FS0010: Unexpected integer literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_str.fsi",
        "module M\nval a: int\n\"a\"\n",
        [
            "after_str.fsi(3,1,3,4): error FS0010: Unexpected string literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_lparen.fsi",
        "module M\nval a: int\n(c)\n",
        [
            "after_lparen.fsi(3,1,3,2): error FS0010: Unexpected symbol '('. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_lbrack.fsi",
        "module M\nval a: int\n[1]\n",
        [
            "after_lbrack.fsi(3,1,3,2): error FS0010: Unexpected symbol '['. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_if.fsi",
        "module M\nval a: int\nif true then ()\n",
        [
            "after_if.fsi(3,1,3,3): error FS0010: Unexpected keyword 'if'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_match.fsi",
        "module M\nval a: int\nmatch 1 with _ -> ()\n",
        [
            "after_match.fsi(3,1,3,6): error FS0010: Unexpected keyword 'match'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_fun.fsi",
        "module M\nval a: int\nfun x -> x\n",
        [
            "after_fun.fsi(3,1,3,4): error FS0010: Unexpected keyword 'fun'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_eq.fsi",
        "module M\nval a: int\n= 1\n",
        [
            "after_eq.fsi(3,1,3,2): error FS0010: Unexpected symbol '='. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_bar.fsi",
        "module M\nval a: int\n| A\n",
        [
            "after_bar.fsi(3,1,3,2): error FS0010: Unexpected symbol '|'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_under.fsi",
        "module M\nval a: int\n_\n",
        [
            "after_under.fsi(3,1,3,2): error FS0010: Unexpected symbol '_'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_inline.fsi",
        "module M\nval a: int\ninline c: int\n",
        [
            "after_inline.fsi(3,1,3,7): error FS0010: Unexpected keyword 'inline'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_and.fsi",
        "module M\nval a: int\nand c: int\n",
        [
            "after_and.fsi(3,1,3,4): error FS0010: Unexpected keyword 'and'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_rbrace.fsi",
        "module M\nval a: int\n}\n",
        [
            "after_rbrace.fsi(3,1,3,2): error FS0010: Unexpected symbol '}'. Expected incomplete structured construct at or before this point or other token."
        ]

        "after_end.fsi",
        "module M\nval a: int\nend\n",
        [
            "after_end.fsi(3,1,3,4): error FS0010: Unexpected keyword 'end'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_let.fsi",
        "namespace Q\nlet c: int\n",
        [
            "ns_let.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_use.fsi",
        "namespace Q\nuse c: int\n",
        [
            "ns_use.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_letrec.fsi",
        "namespace Q\nlet rec c: int\n",
        [
            "ns_letrec.fsi(2,1,2,4): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_letbang.fsi",
        "namespace Q\nlet! c = 1\n",
        [
            "ns_letbang.fsi(2,1,2,5): error FS0010: Unexpected binder keyword. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_do.fsi",
        "namespace Q\ndo ()\n",
        [
            "ns_do.fsi(2,1,2,3): error FS0010: Unexpected keyword 'do'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_ident.fsi",
        "namespace Q\nc: int\n",
        [
            "ns_ident.fsi(2,1,2,2): error FS0010: Unexpected identifier. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_num.fsi",
        "namespace Q\n1\n",
        [
            "ns_num.fsi(2,1,2,2): error FS0010: Unexpected integer literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_str.fsi",
        "namespace Q\n\"a\"\n",
        [
            "ns_str.fsi(2,1,2,4): error FS0010: Unexpected string literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_lparen.fsi",
        "namespace Q\n(c)\n",
        [
            "ns_lparen.fsi(2,1,2,2): error FS0010: Unexpected symbol '('. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_lbrack.fsi",
        "namespace Q\n[1]\n",
        [
            "ns_lbrack.fsi(2,1,2,2): error FS0010: Unexpected symbol '['. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_if.fsi",
        "namespace Q\nif true then ()\n",
        [
            "ns_if.fsi(2,1,2,3): error FS0010: Unexpected keyword 'if'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_match.fsi",
        "namespace Q\nmatch 1 with _ -> ()\n",
        [
            "ns_match.fsi(2,1,2,6): error FS0010: Unexpected keyword 'match'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_fun.fsi",
        "namespace Q\nfun x -> x\n",
        [
            "ns_fun.fsi(2,1,2,4): error FS0010: Unexpected keyword 'fun'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_eq.fsi",
        "namespace Q\n= 1\n",
        [
            "ns_eq.fsi(2,1,2,2): error FS0010: Unexpected symbol '='. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_bar.fsi",
        "namespace Q\n| A\n",
        [
            "ns_bar.fsi(2,1,2,2): error FS0010: Unexpected symbol '|'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_under.fsi",
        "namespace Q\n_\n",
        [
            "ns_under.fsi(2,1,2,2): error FS0010: Unexpected symbol '_'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_inline.fsi",
        "namespace Q\ninline c: int\n",
        [
            "ns_inline.fsi(2,1,2,7): error FS0010: Unexpected keyword 'inline'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_and.fsi",
        "namespace Q\nand c: int\n",
        [
            "ns_and.fsi(2,1,2,4): error FS0010: Unexpected keyword 'and'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_rbrace.fsi",
        "namespace Q\n}\n",
        [
            "ns_rbrace.fsi(2,1,2,2): error FS0010: Unexpected symbol '}'. Expected incomplete structured construct at or before this point or other token."
        ]

        "ns_end.fsi",
        "namespace Q\nend\n",
        [
            "ns_end.fsi(2,1,2,4): error FS0010: Unexpected keyword 'end'. Expected incomplete structured construct at or before this point or other token."
        ]

        "first_end.fsi",
        "module M\nmodule N =\n    end\n",
        [
            "first_end.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_end.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_end_val.fsi",
        "module M\nmodule N =\n    end\nval b: int\n",
        [ "first_end_val.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file" ]

        "first_end_nested_val.fsi",
        "module M\nmodule N =\n    end\n    val c: int\nval b: int\n",
        [
            "first_end_nested_val.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
        ]

        "first_end_blank.fsi",
        "module M\nmodule N =\n    end\n\n\n",
        [
            "first_end_blank.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_end_blank.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_end_nonl.fsi",
        "module M\nmodule N =\n    end",
        [
            "first_end_nonl.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_end_nonl.fsi(3,8,3,8): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_end_module.fsi",
        "module M\nmodule N =\n    end\nmodule O =\n    val c: int\n",
        [
            "first_end_module.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
        ]

        "first_end_ns.fsi",
        "namespace Q\nmodule N =\n    end\n",
        [
            "first_end_ns.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_end_ns.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_end_ns2.fsi",
        "namespace Q\nmodule N =\n    end\nnamespace R\nval c: int\n",
        [ "first_end_ns2.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file" ]

        "second_end.fsi",
        "module M\nmodule N =\n    val a: int\n    end\n",
        [
            "second_end.fsi(4,5,4,8): error FS0010: Unexpected keyword 'end' in signature file. Expected incomplete structured construct at or before this point or other token."
            "second_end.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "second_end_val.fsi",
        "module M\nmodule N =\n    val a: int\n    end\nval b: int\n",
        [
            "second_end_val.fsi(4,5,4,8): error FS0010: Unexpected keyword 'end' in signature file. Expected incomplete structured construct at or before this point or other token."
        ]

        "first_paren.fsi",
        "module M\nmodule N =\n    )\n",
        [
            "first_paren.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "first_paren.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_val_err.fsi",
        "module M\nmodule N =\n    val a: )\n",
        [
            "first_val_err.fsi(3,12,3,13): error FS0010: Unexpected symbol ')' in value signature"
            "first_val_err.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "deep_first_end.fsi",
        "module M\nmodule N =\n    module O =\n        end\n",
        [
            "deep_first_end.fsi(4,9,4,12): error FS0010: Unexpected keyword 'end' in signature file"
            "deep_first_end.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_end_lib.fsi",
        "module M\nmodule N =\n    end\n",
        [
            "first_end_lib.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_end_lib.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "same_level_val.fsi",
        "module M\nmodule N =\n    end\n    val c: int\n",
        [ "same_level_val.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file" ]

        "same_level_two.fsi",
        "module M\nmodule N =\n    val a: int\n    end\n    val c: int\n",
        [
            "same_level_two.fsi(4,5,4,8): error FS0010: Unexpected keyword 'end' in signature file. Expected incomplete structured construct at or before this point or other token."
        ]

        "deep_then_outer.fsi",
        "module M\nmodule N =\n    module O =\n        end\n    val c: int\n",
        [
            "deep_then_outer.fsi(4,9,4,12): error FS0010: Unexpected keyword 'end' in signature file"
        ]

        "err_then_open.fsi",
        "module M\nmodule N =\n    end\nopen System\n",
        [ "err_then_open.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file" ]

        "err_then_comment.fsi",
        "module M\nmodule N =\n    end\n// c\n",
        [
            "err_then_comment.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "err_then_comment.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "clean_then_err.fsi",
        "module M\nmodule O =\n    val d: int\nmodule N =\n    end\n",
        [
            "clean_then_err.fsi(5,5,5,8): error FS0010: Unexpected keyword 'end' in signature file"
            "clean_then_err.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_err.fsi",
        "module M\nval a: int\n)\n",
        [
            "root_err.fsi(3,1,3,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
        ]

        "val_err_root.fsi",
        "module M\nval a: )\n",
        [ "val_err_root.fsi(2,8,2,9): error FS0010: Unexpected symbol ')' in value signature" ]

        "nested_val_err_then_val.fsi",
        "module M\nmodule N =\n    val a: )\n    val c: int\n",
        [
            "nested_val_err_then_val.fsi(3,12,3,13): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "two_errors.fsi",
        "module M\nmodule N =\n    end\n    )\n",
        [
            "two_errors.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "two_errors.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first-end.fsi",
        "module M\nmodule N =\n    end\n",
        [
            "first-end.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first-end.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "later_let.fsi",
        "module M\nmodule N =\n    val a: int\n    let c: int\n",
        [
            "later_let.fsi(4,5,4,8): error FS0010: Unexpected keyword 'let' or 'use' in signature file. Expected incomplete structured construct at or before this point or other token."
            "later_let.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "later_do.fsi",
        "module M\nmodule N =\n    val a: int\n    do ()\n",
        [
            "later_do.fsi(4,5,4,7): error FS0010: Unexpected keyword 'do' in signature file. Expected incomplete structured construct at or before this point or other token."
            "later_do.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "later_num.fsi",
        "module M\nmodule N =\n    val a: int\n    1\n",
        [
            "later_num.fsi(4,5,4,6): error FS0010: Unexpected integer literal in signature file. Expected incomplete structured construct at or before this point or other token."
            "later_num.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "later_letbang.fsi",
        "module M\nmodule N =\n    val a: int\n    let! c = 1\n",
        [
            "later_letbang.fsi(4,5,4,9): error FS0010: Unexpected binder keyword in signature file. Expected incomplete structured construct at or before this point or other token."
            "later_letbang.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "later_paren_val.fsi",
        "module M\nmodule N =\n    val a: int\n    )\n    val c: int\n",
        [
            "later_paren_val.fsi(4,5,4,6): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
        ]

        "first_paren_paren_val.fsi",
        "module M\nmodule N =\n    )\n    )\n    val c: int\n",
        [
            "first_paren_paren_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
        ]

        "first_then_nested_err.fsi",
        "module M\nmodule N =\n    end\n    val c: int\n    )\n",
        [
            "first_then_nested_err.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_then_nested_err.fsi(5,5,5,6): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "first_then_nested_err.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "first_then_val_then_root.fsi",
        "module M\nmodule N =\n    end\n    val c: )\nval b: int\n",
        [
            "first_then_val_then_root.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "first_then_val_then_root.fsi(4,12,4,13): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "fixture.fsi",
        "namespace SyntaxSignatures\n\nmodule Values =\n    val broken: )\n    val first: int\n    val other int\n\nmodule Later =\n    val second: int -> ]\n",
        [
            "fixture.fsi(4,17,4,18): error FS0010: Unexpected symbol ')' in value signature"
            "fixture.fsi(6,15,6,18): error FS0010: Unexpected identifier in value signature. Expected ':' or other token."
            "fixture.fsi(9,24,9,25): error FS0010: Unexpected symbol ']' in value signature"
        ]

        "ns_val_paren.fsi",
        "namespace Q\nmodule N =\n    val a: )\n",
        [
            "ns_val_paren.fsi(3,12,3,13): error FS0010: Unexpected symbol ')' in value signature"
            "ns_val_paren.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "ns_val_arrow.fsi",
        "namespace Q\nmodule N =\n    val a: int -> ]\n",
        [ "ns_val_arrow.fsi(3,19,3,20): error FS0010: Unexpected symbol ']' in value signature" ]

        "mod_val_arrow.fsi",
        "module M\nmodule N =\n    val a: int -> ]\n",
        [ "mod_val_arrow.fsi(3,19,3,20): error FS0010: Unexpected symbol ']' in value signature" ]

        "mod_val_bracket.fsi",
        "module M\nmodule N =\n    val a: ]\n",
        [
            "mod_val_bracket.fsi(3,12,3,13): error FS0010: Unexpected symbol ']' in value signature"
            "mod_val_bracket.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "mod_val_colon.fsi",
        "module M\nmodule N =\n    val a int\n",
        [
            "mod_val_colon.fsi(3,11,3,14): error FS0010: Unexpected identifier in value signature. Expected ':' or other token."
            "mod_val_colon.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "mod_val_name.fsi",
        "module M\nmodule N =\n    val )\n",
        [
            "mod_val_name.fsi(3,9,3,10): error FS0010: Unexpected symbol ')' in value signature. Expected identifier, '(', '(*)' or other token."
            "mod_val_name.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "sig222d-ns_end.fsi",
        "namespace Q\nmodule N =\n    end\n",
        [
            "sig222d-ns_end.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
            "sig222d-ns_end.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "mod_second_val_paren.fsi",
        "module M\nmodule N =\n    val b: int\n    val a: )\n",
        [
            "mod_second_val_paren.fsi(4,12,4,13): error FS0010: Unexpected symbol ')' in value signature"
            "mod_second_val_paren.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "n_trail_paren.fsi",
        "module M\nmodule N =\n    val a: int )\n",
        [
            "n_trail_paren.fsi(3,16,3,17): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "n_trail_paren.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "n_trail_paren_val.fsi",
        "module M\nmodule N =\n    val a: int )\n    val b: int\n",
        [
            "n_trail_paren_val.fsi(3,16,3,17): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
        ]

        "n_trail_num.fsi",
        "module M\nmodule N =\n    val a: int 1\n",
        [
            "n_trail_num.fsi(3,16,3,17): error FS0010: Unexpected integer literal in signature file. Expected incomplete structured construct at or before this point or other token."
            "n_trail_num.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "r_trail_num.fsi",
        "module M\nval a: int 1\n",
        [
            "r_trail_num.fsi(2,12,2,13): error FS0010: Unexpected integer literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "r_trail_str.fsi",
        "module M\nval a: int \"s\"\n",
        [
            "r_trail_str.fsi(2,12,2,15): error FS0010: Unexpected string literal. Expected incomplete structured construct at or before this point or other token."
        ]

        "r_trail_let.fsi",
        "module M\nval a: int let\n",
        [
            "r_trail_let.fsi(2,12,2,15): error FS0010: Unexpected keyword 'let' or 'use'. Expected incomplete structured construct at or before this point or other token."
        ]

        "n_arrow_then_paren.fsi",
        "module M\nmodule N =\n    val a: int -> ]\n    )\n",
        [
            "n_arrow_then_paren.fsi(3,19,3,20): error FS0010: Unexpected symbol ']' in value signature"
        ]

        "n_star.fsi",
        "module M\nmodule N =\n    val a: int * ]\n",
        [ "n_star.fsi(3,18,3,19): error FS0010: Unexpected symbol ']' in value signature" ]

        "n_list.fsi",
        "module M\nmodule N =\n    val a: int list ]\n",
        [
            "n_list.fsi(3,21,3,22): error FS0010: Unexpected symbol ']' in signature file. Expected incomplete structured construct at or before this point or other token."
            "n_list.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]
    ]

    let private unmodeledSignatureRecoveryCases = [
        "mod_eq.fsi",
        "module M\n= 1\n",
        [
            "mod_eq.fsi(2,3,2,4): error FS0010: Unexpected integer literal in signature file"
            "mod_eq.fsi(3,1,3,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "mod_private.fsi",
        "module M\nprivate c: int\n",
        [ "mod_private.fsi(2,9,2,10): error FS0010: Unexpected identifier" ]

        "nested_ident.fsi",
        "module M\nmodule N =\n    c: int\n",
        [
            "nested_ident.fsi(3,6,3,7): error FS0010: Unexpected symbol ':' in signature file. Expected incomplete structured construct at or before this point, '.' or other token."
            "nested_ident.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_private.fsi",
        "module M\nmodule N =\n    private c: int\n",
        [
            "nested_private.fsi(3,13,3,14): error FS0010: Unexpected identifier in signature file"
            "nested_private.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "after_private.fsi",
        "module M\nval a: int\nprivate c: int\n",
        [
            "after_private.fsi(3,9,3,10): error FS0010: Unexpected identifier in signature file"
            "after_private.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "ns_private.fsi",
        "namespace Q\nprivate c: int\n",
        [ "ns_private.fsi(2,9,2,10): error FS0010: Unexpected identifier" ]

        "deep_then_root.fsi",
        "module M\nmodule N =\n    module O =\n        end\nval c: int\n",
        [
            "deep_then_root.fsi(4,9,4,12): error FS0010: Unexpected keyword 'end' in signature file"
            "deep_then_root.fsi(6,1,6,1): error FS0010: Unexpected end of input in signature file. Expected incomplete structured construct at or before this point or other token."
            "deep_then_root.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "r_trail_eq.fsi", "module M\nval a: int = 1\n", []
    ]

    let private headerLossCases = [
        "nested_arrow_then_root_arrow.fsi",
        "module M\nmodule N =\n    val a: int -> ]\nval c: int -> )\n",
        [
            "nested_arrow_then_root_arrow.fsi(3,19,3,20): error FS0010: Unexpected symbol ']' in value signature"
            "nested_arrow_then_root_arrow.fsi(4,15,4,16): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "no_lost_root_arrow.fsi",
        "module M\nval c: int -> )\n",
        [
            "no_lost_root_arrow.fsi(2,15,2,16): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "root_arrow_eof.fsi",
        "module M\nmodule N =\n    )\nval c: int ->\n",
        [
            "root_arrow_eof.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_arrow_eof.fsi(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "root_arrow_err_then_err.fsi",
        "module M\nmodule N =\n    )\nval c: int -> )\nval d: )\n",
        [
            "root_arrow_err_then_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_arrow_err_then_err.fsi(4,15,4,16): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "root_arrow_err_then_val.fsi",
        "module M\nmodule N =\n    )\nval c: int -> )\nval d: int\n",
        [
            "root_arrow_err_then_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_arrow_err_then_val.fsi(4,15,4,16): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "root_arrow_err.fsi",
        "module M\nmodule N =\n    )\nval c: int -> )\n",
        [
            "root_arrow_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_arrow_err.fsi(4,15,4,16): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "root_star_err.fsi",
        "module M\nmodule N =\n    )\nval c: int * ]\n",
        [
            "root_star_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_star_err.fsi(4,14,4,15): error FS0010: Unexpected symbol ']' in value signature"
        ]

        "attr_mod.fsi",
        "module M\nmodule N =\n    )\n[<A>]\nmodule O =\n    val b: )\n",
        [
            "attr_mod.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "attr_mod.fsi(6,12,6,13): error FS0010: Unexpected symbol ')' in value signature"
            "attr_mod.fsi(7,1,7,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "attr_val_line.fsi",
        "module M\nmodule N =\n    )\n    [<A>] val b: )\n",
        [
            "attr_val_line.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "attr_val_line.fsi(4,18,4,19): error FS0010: Unexpected symbol ')' in value signature"
            "attr_val_line.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "attr_val_ok.fsi",
        "module M\nmodule N =\n    )\n    [<A>]\n    val b: int\n",
        [ "attr_val_ok.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file" ]

        "attr_val_root.fsi",
        "module M\nmodule N =\n    )\n[<A>]\nval b: )\n",
        [
            "attr_val_root.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "attr_val_root.fsi(5,8,5,9): error FS0010: Unexpected symbol ')' in value signature"
            "attr_val_root.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "attr_val.fsi",
        "module M\nmodule N =\n    )\n    [<A>]\n    val b: )\n",
        [
            "attr_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "attr_val.fsi(5,12,5,13): error FS0010: Unexpected symbol ')' in value signature"
            "attr_val.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "ns_after_ns_mod.fsi",
        "namespace P\nmodule N =\n    )\nnamespace Q\nmodule O =\n    val x: )\n",
        [
            "ns_after_ns_mod.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "ns_after_ns_mod.fsi(6,12,6,13): error FS0010: Unexpected symbol ')' in value signature"
            "ns_after_ns_mod.fsi(7,1,7,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "ns_after_ns.fsi",
        "namespace P\nmodule N =\n    )\nnamespace Q\nval x: )\n",
        [
            "ns_after_ns.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "ns_after_ns.fsi(5,8,5,9): error FS0010: Unexpected symbol ')' in value signature"
            "ns_after_ns.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "inside_type_then_root_err.fsi",
        "module M\nmodule N =\n    val a: int -> ]\nval c: )\n",
        [
            "inside_type_then_root_err.fsi(3,19,3,20): error FS0010: Unexpected symbol ']' in value signature"
            "inside_type_then_root_err.fsi(4,8,4,9): error FS0010: Unexpected symbol ')' in value signature"
            "inside_type_then_root_err.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_ok_then_root_err.fsi",
        "module M\nmodule N =\n    )\nmodule O =\n    val d: int\nval c: )\n",
        [
            "nested_ok_then_root_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "nested_ok_then_root_err.fsi(6,8,6,9): error FS0010: Unexpected symbol ')' in value signature"
            "nested_ok_then_root_err.fsi(7,1,7,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "no_lost_root_err.fsi",
        "module M\nval c: )\n",
        [ "no_lost_root_err.fsi(2,8,2,9): error FS0010: Unexpected symbol ')' in value signature" ]

        "root_arrow.fsi",
        "module M\nmodule N =\n    )\nval b: int -> ]\n",
        [
            "root_arrow.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_arrow.fsi(4,15,4,16): error FS0010: Unexpected symbol ']' in value signature"
        ]

        "root_ok_then_err.fsi",
        "module M\nmodule N =\n    )\nval b: int\nval c: )\n",
        [
            "root_ok_then_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_ok_then_err.fsi(5,8,5,9): error FS0010: Unexpected symbol ')' in value signature"
            "root_ok_then_err.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_ok_then_stray.fsi",
        "module M\nmodule N =\n    )\nval b: int\n)\n",
        [
            "root_ok_then_stray.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_ok_then_stray.fsi(5,1,5,2): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "root_ok_then_stray.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_open_then_err.fsi",
        "module M\nmodule N =\n    )\nopen System\nval c: )\n",
        [
            "root_open_then_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_open_then_err.fsi(5,8,5,9): error FS0010: Unexpected symbol ')' in value signature"
            "root_open_then_err.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_trail.fsi",
        "module M\nmodule N =\n    )\nval b: int )\n",
        [
            "root_trail.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_trail.fsi(4,12,4,13): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "root_trail.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_val_colon.fsi",
        "module M\nmodule N =\n    )\nval b int\n",
        [
            "root_val_colon.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_val_colon.fsi(4,7,4,10): error FS0010: Unexpected identifier in value signature. Expected ':' or other token."
            "root_val_colon.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_val_err_then_err.fsi",
        "module M\nmodule N =\n    )\nval b: )\nval c: )\n",
        [
            "root_val_err_then_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_val_err_then_err.fsi(4,8,4,9): error FS0010: Unexpected symbol ')' in value signature"
            "root_val_err_then_err.fsi(5,8,5,9): error FS0010: Unexpected symbol ')' in value signature"
            "root_val_err_then_err.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_val_err_then_val.fsi",
        "module M\nmodule N =\n    )\nval b: )\nval c: int\n",
        [
            "root_val_err_then_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_val_err_then_val.fsi(4,8,4,9): error FS0010: Unexpected symbol ')' in value signature"
        ]

        "root_val_err.fsi",
        "module M\nmodule N =\n    )\nval b: )\n",
        [
            "root_val_err.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_val_err.fsi(4,8,4,9): error FS0010: Unexpected symbol ')' in value signature"
            "root_val_err.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_name_first.fsi",
        "module M\nmodule N =\n    val )\n",
        [
            "nested_name_first.fsi(3,9,3,10): error FS0010: Unexpected symbol ')' in value signature. Expected identifier, '(', '(*)' or other token."
            "nested_name_first.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "ns_lost_then_stray.fsi",
        "namespace P\nmodule N =\n    )\nnamespace Q\nval b: int\n)\n",
        [
            "ns_lost_then_stray.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "ns_lost_then_stray.fsi(6,1,6,2): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "ns_lost_then_stray.fsi(7,1,7,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_inside_type_then_val.fsi",
        "module M\nmodule N =\n    )\nval b: int -> ]\nval c: int\n",
        [
            "root_inside_type_then_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_inside_type_then_val.fsi(4,15,4,16): error FS0010: Unexpected symbol ']' in value signature"
        ]

        "root_inside_type.fsi",
        "module M\nmodule N =\n    )\nval b: int -> ]\n",
        [
            "root_inside_type.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_inside_type.fsi(4,15,4,16): error FS0010: Unexpected symbol ']' in value signature"
        ]

        "root_stray_then_val.fsi",
        "module M\nmodule N =\n    )\nval b: int\n)\nval c: )\n",
        [
            "root_stray_then_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_stray_then_val.fsi(5,1,5,2): error FS0010: Unexpected symbol ')' in signature file. Expected incomplete structured construct at or before this point or other token."
            "root_stray_then_val.fsi(6,8,6,9): error FS0010: Unexpected symbol ')' in value signature"
            "root_stray_then_val.fsi(7,1,7,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]
    ]

    let private unmodeledHeaderLossCases = [
        "ns_after_mod_clean.fsi",
        "module M\nmodule N =\n    )\nnamespace Q\nval x: int\n",
        [ "ns_after_mod_clean.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file" ]

        "ns_after_mod_ok.fsi",
        "module M\nmodule N =\n    val a: int\nnamespace Q\nval x: int\n",
        [
            "ns_after_mod_ok.fsi(4,1,4,10): error FS0010: Unexpected keyword 'namespace'. Expected incomplete structured construct at or before this point or other token."
            "ns_after_mod_ok.fsi(1,1,3,15): error FS0530: Only '#' compiler directives may occur prior to the first 'namespace' declaration"
            "ns_after_mod_ok.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "ns_after_mod.fsi",
        "module M\nmodule N =\n    )\nnamespace Q\nval x: )\n",
        [
            "ns_after_mod.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "ns_after_mod.fsi(5,8,5,9): error FS0010: Unexpected symbol ')' in value signature"
            "ns_after_mod.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_val_name.fsi",
        "module M\nmodule N =\n    )\nval )\n",
        [
            "root_val_name.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_val_name.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_name_then_val.fsi",
        "module M\nmodule N =\n    )\n    val )\n    val c: )\n",
        [
            "nested_name_then_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "nested_name_then_val.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "nested_name.fsi",
        "module M\nmodule N =\n    )\n    val )\n",
        [
            "nested_name.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "nested_name.fsi(5,1,5,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "root_name_then_val.fsi",
        "module M\nmodule N =\n    )\nval )\nval c: )\n",
        [
            "root_name_then_val.fsi(3,5,3,6): error FS0010: Unexpected symbol ')' in signature file"
            "root_name_then_val.fsi(6,1,6,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]
    ]

    let private incompleteConstructCases = [
        "b_comment_next.fs",
        "module M\nlet a = // c\nlet b = 1\n",
        [
            "b_comment_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_comment_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_nested_next.fs",
        "module M\nmodule N =\n    let a =\n    let b = 1\n",
        [
            "b_nested_next.fs(4,5,4,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_next.fs(4,5,4,8): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_nested_root.fs",
        "module M\nmodule N =\n    let a =\nlet b = 1\n",
        [
            "b_nested_root.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_root_eof.fs",
        "module M\nlet a =\n",
        [
            "b_root_eof.fs(3,1,3,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_root_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_root_next.fs",
        "module M\nlet a =\nlet b = 1\n",
        [
            "b_root_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_root_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "v_colon_eof.fsi",
        "module M\nval a:\n",
        [
            "v_colon_eof.fsi(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_colon_nested_root.fsi",
        "module M\nmodule N =\n    val a:\nval b: int\n",
        [
            "v_colon_nested_root.fsi(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_colon_next.fsi",
        "module M\nval a:\nval b: int\n",
        [
            "v_colon_next.fsi(2,8,3,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_nested_blank_root.fsi",
        "module M\nmodule N =\n    val a: int ->\n\nval b: int\n",
        [
            "v_nested_blank_root.fsi(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_nested_eof.fsi",
        "module M\nmodule N =\n    val a: int ->\n",
        [
            "v_nested_eof.fsi(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_nested_next.fsi",
        "module M\nmodule N =\n    val a: int ->\n    val b: int\n",
        [
            "v_nested_next.fsi(3,19,4,5): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_nested_root.fsi",
        "module M\nmodule N =\n    val a: int ->\nval b: int\n",
        [
            "v_nested_root.fsi(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_root_blank.fsi",
        "module M\nval a: int ->\n\n\nval b: int\n",
        [
            "v_root_blank.fsi(2,15,5,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_root_eof_nonl.fsi",
        "module M\nval a: int ->",
        [
            "v_root_eof_nonl.fsi(2,1,2,14): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_root_eof.fsi",
        "module M\nval a: int ->\n",
        [
            "v_root_eof.fsi(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "v_root_next.fsi",
        "module M\nval a: int ->\nval b: int\n",
        [
            "v_root_next.fsi(2,15,3,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "o_nested_eof.fs",
        "module M\nmodule N =\n    open\n",
        [
            "o_nested_eof.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "o_nested_next.fs",
        "module M\nmodule N =\n    open\n    let b = 1\n",
        [
            "o_nested_next.fs(3,10,4,5): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "o_nested_root.fs",
        "module M\nmodule N =\n    open\nlet b = 1\n",
        [
            "o_nested_root.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "o_root_blank.fs",
        "module M\nopen\n\nlet b = 1\n",
        [
            "o_root_blank.fs(2,6,4,1): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "o_root_eof.fs",
        "module M\nopen\n",
        [
            "o_root_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "o_root_next.fs",
        "module M\nopen\nlet b = 1\n",
        [
            "o_root_next.fs(2,6,3,1): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "od_nested_next.fs",
        "module M\nmodule N =\n    open System.\n    let b = 1\n",
        [
            "od_nested_next.fs(3,18,4,5): error FS0010: Incomplete structured construct at or before this point in open declaration"
        ]

        "od_root_next.fs",
        "module M\nopen System.\nlet b = 1\n",
        [
            "od_root_next.fs(2,14,3,1): error FS0010: Incomplete structured construct at or before this point in open declaration"
        ]

        "os_nested_root.fsi",
        "module M\nmodule N =\n    open\nval b: int\n",
        [
            "os_nested_root.fsi(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "os_root_next.fsi",
        "module M\nopen\nval b: int\n",
        [
            "os_root_next.fsi(2,6,3,1): error FS0010: Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
        ]

        "n_arrow_end.fsi",
        "module M\nmodule N =\n    val a: int ->\n",
        [
            "n_arrow_end.fsi(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in value signature"
        ]

        "b_and.fs",
        "module M\nlet rec a = 1\nand b =\nlet c = 1\n",
        [
            "b_and.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_rec.fs",
        "module M\nlet rec a =\nand b = 1\n",
        [
            "b_and_rec.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_rec.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_nonrec.fs",
        "module M\nlet a =\nand b = 1\n",
        [
            "b_and_nonrec.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_nonrec.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_blank.fs",
        "module M\nlet rec a =\n\nand b = 1\nlet c = 2\n",
        [
            "b_and_blank.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_blank.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_func.fs",
        "module M\nlet rec f x =\nand g y = 1\n",
        [
            "b_and_func.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_func.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_attr.fs",
        "module M\n[<A>]\nlet rec a =\nand b = 1\n",
        [
            "b_and_attr.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_attr.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_private.fs",
        "module M\nlet rec private a =\nand b = 1\n",
        [
            "b_and_private.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_private.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_use.fs",
        "module M\nuse a =\nand b = 1\n",
        [
            "b_and_use.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_use.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_and_use.fs(2,1,2,8): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "b_and_nested.fs",
        "namespace A\nmodule N =\n    let rec a =\n    and b = 1\n",
        [
            "b_and_nested.fs(4,5,4,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_nested.fs(4,5,4,8): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_body_next.fs",
        "module M\nlet rec a =\nand b =\n    1\n",
        [
            "b_and_body_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_body_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_then_let.fs",
        "module M\nlet rec a =\nand b = 1\nlet c =\n",
        [
            "b_and_then_let.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_then_let.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_and_then_let.fs(5,1,5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_then_let.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_and_mid.fs",
        "module M\nlet rec a = 1\nand b =\nand c = 1\n",
        [
            "b_and_mid.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_and_mid.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "g_nobody_eof.fs",
        "module M\nlet rec a =\nand b =\n",
        [
            "g_nobody_eof.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_eof.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nobody_eof.fs(4,1,4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nobody_long.fs",
        "module M\nlet rec a =\nand b x y z w =\n",
        [
            "g_nobody_long.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_long.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nobody_long.fs(4,1,4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nobody_next.fs",
        "module M\nlet rec a =\nand b =\nlet c = 1\n",
        [
            "g_nobody_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nobody_next.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nobody_type.fs",
        "module M\nlet rec a =\nand b =\ntype T = int\n",
        [
            "g_nobody_type.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_type.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nobody_type.fs(4,1,4,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nobody_three.fs",
        "module M\nlet rec a =\nand b =\nand c = 1\n",
        [
            "g_nobody_three.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_three.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nobody_three.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nobody_three_last.fs",
        "module M\nlet rec a =\nand b = 1\nand c =\n",
        [
            "g_nobody_three_last.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_three_last.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nobody_three_last.fs(5,1,5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nobody_err.fs",
        "module M\nlet rec a =\nand b = )\n",
        [
            "g_nobody_err.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nobody_err.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "g_ok_nobody_nobody.fs",
        "module M\nlet rec a = 1\nand b =\nand c =\n",
        [
            "g_ok_nobody_nobody.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_ok_nobody_nobody.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_ok_nobody_nobody.fs(5,1,5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_err_nobody.fs",
        "module M\nlet rec a = )\nand b =\n",
        [
            "g_err_nobody.fs(2,13,2,14): error FS0010: Unexpected symbol ')' in binding"
            "g_err_nobody.fs(4,1,4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_err_err.fs",
        "module M\nlet rec a = )\nand b = )\n",
        [ "g_err_err.fs(2,13,2,14): error FS0010: Unexpected symbol ')' in binding" ]

        "g_err_ok_let.fs",
        "module M\nlet rec a = )\nand b = 1\nlet c =\n",
        [
            "g_err_ok_let.fs(2,13,2,14): error FS0010: Unexpected symbol ')' in binding"
            "g_err_ok_let.fs(5,1,5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_err_ok_let.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "g_nonrec_then_let_err.fs",
        "module M\nlet a =\nand b = )\nlet c = )\n",
        [
            "g_nonrec_then_let_err.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nonrec_then_let_err.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nonrec_then_let_err.fs(4,9,4,10): error FS0010: Unexpected symbol ')' in binding"
        ]

        "g_nested_nobody.fs",
        "namespace A\nmodule N =\n    let rec a =\n    and b =\n    let c = 1\n",
        [
            "g_nested_nobody.fs(4,5,4,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nested_nobody.fs(4,5,4,8): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nested_nobody.fs(5,5,5,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "g_nested_err_root.fs",
        "namespace A\nmodule N =\n    let rec a =\n    and b = )\nlet c = )\n",
        [
            "g_nested_err_root.fs(4,5,4,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "g_nested_err_root.fs(4,5,4,8): error FS0010: Incomplete structured construct at or before this point in binding"
            "g_nested_err_root.fs(5,9,5,10): error FS0010: Unexpected symbol ')' in binding"
        ]

        "b_anon.fs",
        "let a =\nlet b = 1\n",
        [
            "b_anon.fs(2,1,2,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (1:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_anon.fs(2,1,2,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_anon.fs(1,1,2,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
        ]

        "b_attr.fs",
        "module M\n[<A>]\nlet a =\nlet b = 1\n",
        [
            "b_attr.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_attr.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_blank_next.fs",
        "module M\nlet a =\n\nlet b = 1\n",
        [
            "b_blank_next.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_blank_next.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_comment.fs",
        "module M\nlet a = // c\nlet b = 1\n",
        [
            "b_comment.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_comment.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_eof_nonl.fs",
        "module M\nlet a =",
        [
            "b_eof_nonl.fs(2,1,2,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof_nonl.fs(2,1,2,8): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_eof.fs",
        "module M\nlet a =\n",
        [
            "b_eof.fs(3,1,3,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_func.fs",
        "module M\nlet f x =\nlet b = 1\n",
        [
            "b_func.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_func.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_nested_eof.fs",
        "module M\nmodule N =\n    let a =\n",
        [
            "b_nested_eof.fs(4,1,4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_eof.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "nobody-b_nested_next.fs",
        "module M\nmodule N =\n    let a =\n    let b = 1\n",
        [
            "nobody-b_nested_next.fs(4,5,4,8): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "nobody-b_nested_next.fs(4,5,4,8): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "nobody-b_nested_root.fs",
        "module M\nmodule N =\n    let a =\nlet b = 1\n",
        [
            "nobody-b_nested_root.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "nobody-b_nested_root.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_next.fs",
        "module M\nlet a =\nlet b = 1\n",
        [
            "b_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_ns.fs",
        "namespace Q\nmodule N =\n    let a =\n",
        [
            "b_ns.fs(4,1,4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_ns.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_private.fs",
        "module M\nlet private a =\nlet b = 1\n",
        [
            "b_private.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_private.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_rec.fs",
        "module M\nlet rec a =\nlet b = 1\n",
        [
            "b_rec.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_rec.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_then_err.fs",
        "module M\nlet a =\nlet b = )\n",
        [
            "b_then_err.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_then_err.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in binding"
        ]

        "b_then_expr.fs",
        "module M\nlet a =\nf 1\n",
        [
            "b_then_expr.fs(3,1,3,2): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_expr.fs(3,1,3,2): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_then_module.fs",
        "module M\nlet a =\nmodule N =\n    let b = 1\n",
        [
            "b_then_module.fs(3,1,3,7): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_module.fs(3,1,3,7): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_then_open.fs",
        "module M\nlet a =\nopen System\n",
        [
            "b_then_open.fs(3,1,3,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_open.fs(3,1,3,5): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_then_type.fs",
        "module M\nlet a =\ntype T = int\n",
        [
            "b_then_type.fs(3,1,3,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_type.fs(3,1,3,5): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_two.fs",
        "module M\nlet a =\nlet b =\n",
        [
            "b_two.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_two.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_two.fs(4,1,4,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_two.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "b_use.fs",
        "module M\nuse a =\nlet b = 1\n",
        [
            "b_use.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_use.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_use.fs(2,1,2,8): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
        ]

        "iar_next.fs",
        "module M\nlet x = 1\n)\nlet y =\nlet z = 1\n",
        [
            "iar_next.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "iar_next.fs(5,1,5,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]

        "iar_nested.fs",
        "module M\nmodule N =\n    let x = )\nlet y =\n",
        [
            "iar_nested.fs(3,13,3,14): error FS0010: Unexpected symbol ')' in binding"
            "iar_nested.fs(5,1,5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "iar_nested.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "iar_nested_inner.fs",
        "module M\nmodule N =\n    let x = )\n    let y =\nlet z = 1\n",
        [
            "iar_nested_inner.fs(3,13,3,14): error FS0010: Unexpected symbol ')' in binding"
            "iar_nested_inner.fs(5,1,5,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "iar_nested_inner.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "iar_nested_next.fs",
        "module M\nmodule N =\n    let x = )\nlet y =\nlet z = 1\n",
        [
            "iar_nested_next.fs(3,13,3,14): error FS0010: Unexpected symbol ')' in binding"
            "iar_nested_next.fs(5,1,5,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "iar_nested_next.fs(5,1,5,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "iar_ok.fs",
        "module M\nlet x = 1\n)\nlet y = 1\n",
        [
            "iar_ok.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
        ]

        "IncompleteAfterRecovery.fs",
        "module M\nlet x = 1\n)\nlet y =\n",
        [
            "IncompleteAfterRecovery.fs(3,1,3,2): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "IncompleteAfterRecovery.fs(5,1,5,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (4:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
        ]
    ]

    let private unmodeledIncompleteConstructCases = [
        "b_and_same_line.fs",
        "module M\nlet rec a = and b = 1\n",
        [
            "b_and_same_line.fs(2,13,2,16): error FS0010: Unexpected keyword 'and' in binding"
            "b_and_same_line.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding. Expected incomplete structured construct at or before this point or other token."
            "b_and_same_line.fs(2,1,2,4): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "b_and_same_line.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "b_and_indented.fs",
        "module M\nlet rec a =\n  and b = 1\n",
        [
            "b_and_indented.fs(3,3,3,6): error FS0010: Unexpected keyword 'and' in binding"
            "b_and_indented.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in binding. Expected incomplete structured construct at or before this point or other token."
            "b_and_indented.fs(2,1,2,4): error FS3118: Incomplete value or function definition. If this is in an expression, the body of the expression must be indented to the same column as the 'let' keyword."
            "b_and_indented.fs(4,1,4,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "od_nested_root.fs",
        "module M\nmodule N =\n    open System.\nlet b = 1\n",
        [
            "od_nested_root.fs(3,16,3,17): error FS3117: Unexpected end of type. Expected a name after this point."
        ]

        "od_root_eof_nonl.fs",
        "module M\nopen System.",
        [
            "od_root_eof_nonl.fs(2,12,2,13): error FS3117: Unexpected end of type. Expected a name after this point."
        ]

        "od_root_eof.fs",
        "module M\nopen System.\n",
        [
            "od_root_eof.fs(2,12,2,13): error FS3117: Unexpected end of type. Expected a name after this point."
        ]

        "b_do.fs",
        "module M\ndo\nlet b = 1\n",
        [
            "b_do.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_do.fs(3,1,3,4): error FS3524: Expecting expression"
        ]
    ]

    let private strictIndentationCases = [
        "8.0",
        "b_next@8.0",
        "b_next.fs",
        "module M\nlet a =\nlet b = 1\n",
        [
            "b_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "8.0",
        "b_eof@8.0",
        "b_eof.fs",
        "module M\nlet a =\n",
        [
            "b_eof.fs(3,1,3,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "8.0",
        "b_nested_root@8.0",
        "b_nested_root.fs",
        "module M\nmodule N =\n    let a =\nlet b = 1\n",
        [
            "b_nested_root.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "8.0",
        "b_then_err@8.0",
        "b_then_err.fs",
        "module M\nlet a =\nlet b = )\n",
        [
            "b_then_err.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_then_err.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in binding"
        ]

        "10.0",
        "b_next@10.0",
        "b_next.fs",
        "module M\nlet a =\nlet b = 1\n",
        [
            "b_next.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "10.0",
        "b_eof@10.0",
        "b_eof.fs",
        "module M\nlet a =\n",
        [
            "b_eof.fs(3,1,3,1): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "10.0",
        "b_nested_root@10.0",
        "b_nested_root.fs",
        "module M\nmodule N =\n    let a =\nlet b = 1\n",
        [
            "b_nested_root.fs(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "10.0",
        "b_then_err@10.0",
        "b_then_err.fs",
        "module M\nlet a =\nlet b = )\n",
        [
            "b_then_err.fs(3,1,3,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,1,3,4): error FS0010: Incomplete structured construct at or before this point in binding"
            "b_then_err.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in binding"
        ]
    ]

    let private unmodeledStrictIndentationCases = [
        "5.0",
        "b_next@5.0",
        "b_next.fs",
        "module M\nlet a =\nlet b = 1\n",
        [
            "b_next.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]

        "5.0",
        "b_eof@5.0",
        "b_eof.fs",
        "module M\nlet a =\n",
        [
            "b_eof.fs(3,1,3,1): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "5.0",
        "b_nested_root@5.0",
        "b_nested_root.fs",
        "module M\nmodule N =\n    let a =\nlet b = 1\n",
        [
            "b_nested_root.fs(4,1,4,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]

        "5.0",
        "b_then_err@5.0",
        "b_then_err.fs",
        "module M\nlet a =\nlet b = )\n",
        [
            "b_then_err.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in binding"
            "b_then_err.fs(3,1,3,4): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]

        "7.0",
        "b_next@7.0",
        "b_next.fs",
        "module M\nlet a =\nlet b = 1\n",
        [
            "b_next.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_next.fs(3,1,3,4): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]

        "7.0",
        "b_eof@7.0",
        "b_eof.fs",
        "module M\nlet a =\n",
        [
            "b_eof.fs(3,1,3,1): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_eof.fs(3,1,3,1): error FS0010: Incomplete structured construct at or before this point in binding"
        ]

        "7.0",
        "b_nested_root@7.0",
        "b_nested_root.fs",
        "module M\nmodule N =\n    let a =\nlet b = 1\n",
        [
            "b_nested_root.fs(4,1,4,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:5). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_nested_root.fs(4,1,4,4): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]

        "7.0",
        "b_then_err@7.0",
        "b_then_err.fs",
        "module M\nlet a =\nlet b = )\n",
        [
            "b_then_err.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,1,3,4): warning FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (2:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            "b_then_err.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in binding"
            "b_then_err.fs(3,1,3,4): error FS0588: The block following this 'let' is unfinished. Every code block is an expression and must have a result. 'let' cannot be the final code element in a block. Consider giving this block an explicit result."
        ]
    ]

    [<Tests>]
    let tests =
        testList "Issue29.ParserCompilation" [
            testList "multi-file diagnostics match the Compatibility Oracle" [
                for name, target, files, expected in cases ->
                    testCase name
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (parse target files))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "each fsc target name selects the Oracle FS0222 rule" [
                for name, expected in
                    [
                        "exe", []
                        "Exe", []
                        "winexe", []
                        "WinExe", []
                        "library", [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]
                        "Library", [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]
                        "module", [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]
                        "MODULE", [ missingDeclaration "Anonymous.fs" 1 1 2 1 ]
                    ] ->
                    testCase name
                    <| fun _ ->
                        let target =
                            SyntaxCompilationTarget.tryParse name
                            |> Option.defaultWith (fun () ->
                                failtest $"The Oracle accepts --target:{name}"
                            )

                        Expect.sequenceEqual
                            (oracleLines (
                                parse target [
                                    named
                                    anonymous
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "a target name that the Oracle rejects has no target" [
                for name in
                    [
                        ""
                        " exe"
                        "appcontainerexe"
                        "winmdobj"
                    ] ->
                    testCase $"'{name}'"
                    <| fun _ ->
                        Expect.isNone
                            (SyntaxCompilationTarget.tryParse name)
                            $"The Oracle reports FS0224 or FS1048 for --target:{name}"
            ]

            testList "the file extension selects the source kind" [
                for logicalPath, expected in
                    [
                        "/src/A.fs", Ok SyntaxSourceKind.Implementation
                        "A.FS", Ok SyntaxSourceKind.Implementation
                        "A.fsx", Ok SyntaxSourceKind.Script
                        "A.FSX", Ok SyntaxSourceKind.Script
                        "A.fsscript", Ok SyntaxSourceKind.Script
                        "A.FsScript", Ok SyntaxSourceKind.Script
                        "A.fsi", Ok SyntaxSourceKind.Signature
                        "A.FsI", Ok SyntaxSourceKind.Signature
                        "A.fsx.fs", Ok SyntaxSourceKind.Implementation
                        "A.ml", Error SyntaxSourceKindError.RequiresMLCompatibility
                        "A.MLI", Error SyntaxSourceKindError.RequiresMLCompatibility
                        "A.txt", Error SyntaxSourceKindError.Unrecognized
                        "A.fs.txt", Error SyntaxSourceKindError.Unrecognized
                        "NoExtension", Error SyntaxSourceKindError.Unrecognized
                    ] ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.equal
                            (SyntaxSourceKind.parse logicalPath)
                            expected
                            "The Oracle accepts .fs, .fsi, .fsx, and .fsscript, needs ML compatibility for .ml and .mli, and reports FS0226 for other extensions"
            ]

            testList "an unmodeled shape after a signature recovery stays explicit" [
                for logicalPath, text, oracle in
                    [
                        "C4.fsi",
                        "module M\n)\nmodule N =\nval b: int\n",
                        [
                            "C4.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
                            "C4.fsi(4,1,4,4): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (3:1). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
                        ]
                        "B6.fsi",
                        "module M\nval a: )\nmodule Inner =\n    val b: )\n",
                        [
                            "B6.fsi(2,8,2,9): error FS0010: Unexpected symbol ')' in value signature"
                            "B6.fsi(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in signature file"
                        ]
                        "C2.fsi",
                        "module M\n)\nval y: int\n    )\n",
                        [
                            "C2.fsi(2,1,2,2): error FS0010: Unexpected symbol ')'. Expected incomplete structured construct at or before this point or other token."
                        ]
                    ] ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList "a top-level expression declaration reports the Oracle diagnostics" [
                for logicalPath, text, expected in expressionDeclarationCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled error in a top-level expression declaration stays explicit" [
                for logicalPath, text, oracle in unmodeledExpressionDeclarationCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testCase
                "an attributed nested module first in an anonymous root reports the module-equals text"
            <| fun _ ->
                Expect.sequenceEqual
                    (oracleLines (
                        parse SyntaxCompilationTarget.Executable [
                            "AttrMod.fs",
                            "[<AutoOpen>]
module M =
    let a = 1
"
                            last
                        ]
                    ))
                    [ moduleEqualsDeclaration "AttrMod.fs" 1 1 2 1 ]
                    "The diagnostics must match the Compatibility Oracle"

            testList
                "the next root declaration after a nested module recovery reports the Oracle diagnostic"
                [
                    for logicalPath, text, expected in nestedRecoveryEndCases ->
                        testCase logicalPath
                        <| fun _ ->
                            Expect.sequenceEqual
                                (oracleLines (
                                    parse SyntaxCompilationTarget.Executable [
                                        logicalPath, text
                                        last
                                    ]
                                ))
                                expected
                                "The diagnostics must match the Compatibility Oracle"
                ]

            testList "an unmodeled end of a nested module recovery stays explicit" [
                for logicalPath, text, oracle in unmodeledNestedRecoveryEndCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList "an attributed or accessible nested module reports the Oracle diagnostics" [
                for logicalPath, text, expected in attributedModuleCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled attributed or accessible declaration stays explicit" [
                for logicalPath, text, oracle in unmodeledAttributedModuleCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testCase "a nested module keeps its attributes, accessibility, and range"
            <| fun _ ->
                let result =
                    parse SyntaxCompilationTarget.Executable [
                        "Attributed.fs",
                        "module M
[<AutoOpen>]
[<RequireQualifiedAccess>]
module private N =
    let a = 1
"
                    ]

                match result.Files[0] with
                | SyntaxFile.Implementation file ->
                    match Seq.exactlyOne (Seq.exactlyOne file.Contents).Declarations with
                    | ImplementationDeclaration.NestedModule nested ->
                        Expect.equal nested.Attributes.Length 2 "Both attribute lists"

                        Expect.equal
                            (nested.Accessibility
                             |> Option.map _.Kind)
                            (Some SyntaxAccessibility.Private)
                            "Accessibility"

                        Expect.equal
                            (nested.Range.Start.Line, nested.Range.Start.Column)
                            (2, 1)
                            "The range starts at the first attribute list"
                    | other -> failtest $"Expected a nested module, found {other}"
                | other -> failtest $"Expected an implementation file, found {other}"

            testList "use and non-recursive and report the Oracle diagnostics" [
                for logicalPath, text, expected in letKeywordCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled use or and shape stays explicit" [
                for logicalPath, text, oracle in unmodeledLetKeywordCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList "an and group in a nested module reports the Oracle diagnostics" [
                for logicalPath, text, expected in nestedAndCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled and group stays explicit" [
                for logicalPath, text, oracle in unmodeledNestedAndCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList "a signature recovery reports the Oracle diagnostics" [
                for logicalPath, text, expected in signatureRecoveryCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled signature recovery stays explicit" [
                for logicalPath, text, oracle in unmodeledSignatureRecoveryCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList
                "a nested signature recovery at the end of input follows the FS0222 target rule"
                [
                    for name, target, files, expected in
                        [
                            "the last file of an executable",
                            SyntaxCompilationTarget.Executable,
                            [
                                last
                                "first_end.fsi", "module M\nmodule N =\n    end\n"
                            ],
                            [
                                "first_end.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
                            ]
                            "a library",
                            SyntaxCompilationTarget.Library,
                            [ "first_end_lib.fsi", "module M\nmodule N =\n    end\n" ],
                            [
                                "first_end_lib.fsi(3,5,3,8): error FS0010: Unexpected keyword 'end' in signature file"
                                "first_end_lib.fsi(4,1,4,1): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."
                            ]
                            "a value type error after an arrow in a library",
                            SyntaxCompilationTarget.Library,
                            [
                                "fixture.fsi",
                                "namespace SyntaxSignatures\n\nmodule Values =\n    val broken: )\n    val first: int\n    val other int\n\nmodule Later =\n    val second: int -> ]\n"
                            ],
                            [
                                "fixture.fsi(4,17,4,18): error FS0010: Unexpected symbol ')' in value signature"
                                "fixture.fsi(6,15,6,18): error FS0010: Unexpected identifier in value signature. Expected ':' or other token."
                                "fixture.fsi(9,24,9,25): error FS0010: Unexpected symbol ']' in value signature"
                            ]
                        ] ->
                        testCase name
                        <| fun _ ->
                            Expect.sequenceEqual
                                (oracleLines (parse target files))
                                expected
                                "The diagnostics must match the Compatibility Oracle"
                ]

            testList "a lost signature module header reports the Oracle diagnostics" [
                for logicalPath, text, expected in headerLossCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled lost signature module header stays explicit" [
                for logicalPath, text, oracle in unmodeledHeaderLossCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList "an incomplete construct reports the Oracle range" [
                for logicalPath, text, expected in incompleteConstructCases ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parse SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "an unmodeled incomplete construct stays explicit" [
                for logicalPath, text, oracle in unmodeledIncompleteConstructCases ->
                    testCase logicalPath
                    <| fun _ ->
                        let result =
                            parse SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testList "a binding without a body reports the strict indentation diagnostics" [
                for mode, name, logicalPath, text, expected in strictIndentationCases ->
                    testCase name
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parseAt mode SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testList "a binding without a body before F# 8.0 stays explicit" [
                for mode, name, logicalPath, text, oracle in unmodeledStrictIndentationCases ->
                    testCase name
                    <| fun _ ->
                        let result =
                            parseAt mode SyntaxCompilationTarget.Executable [
                                logicalPath, text
                                last
                            ]

                        SyntaxDiagnosticText.expectExplicitlyUnsupported
                            oracle
                            (result.Diagnostics
                             |> Seq.map _.Diagnostic)
                            (oracleLines result)
            ]

            testCase "a module-level use keeps its keyword in the tree"
            <| fun _ ->
                let result =
                    parse SyntaxCompilationTarget.Executable [
                        "UseTree.fs", "module M\nuse c = 3\nlet d = 4\n"
                    ]

                match result.Files[0] with
                | SyntaxFile.Implementation file ->
                    Expect.sequenceEqual
                        ((Seq.exactlyOne file.Contents).Declarations
                         |> Seq.map (fun declaration ->
                             match declaration with
                             | ImplementationDeclaration.Let(keyword, _, _, _) -> keyword
                             | other -> failtest $"Expected a let declaration, found {other}"
                         ))
                        [
                            SyntaxLetKeyword.Use
                            SyntaxLetKeyword.Let
                        ]
                        "Keywords"
                | other -> failtest $"Expected an implementation file, found {other}"

            testList "a non-recovery error keeps the full FS0524 range" [
                for mode, logicalPath, text, expected in
                    [
                        "7.0",
                        "u_dot.fs",
                        "module M\nuse f = _.A\n",
                        [
                            "u_dot.fs(2,9,2,11): error FS3350: Feature 'underscore dot shorthand for accessor only function' is not available in F# 7.0. Please use language version 8.0 or greater."
                            "u_dot.fs(2,1,2,12): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
                        ]
                        "10.0",
                        "u_succ.fs",
                        "module M\nuse x = g f(1)\n",
                        [
                            "u_succ.fs(2,11,2,15): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
                            "u_succ.fs(2,1,2,15): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
                        ]
                        "10.0",
                        "u_succ_err.fs",
                        "module M\nuse x = g f(1) )\n",
                        [
                            "u_succ_err.fs(2,11,2,15): error FS0597: Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
                            "u_succ_err.fs(2,16,2,17): error FS0010: Unexpected symbol ')' in binding. Expected incomplete structured construct at or before this point or other token."
                            "u_succ_err.fs(2,1,2,15): warning FS0524: 'use' bindings are not permitted in modules and are treated as 'let' bindings"
                        ]
                    ] ->
                    testCase logicalPath
                    <| fun _ ->
                        Expect.sequenceEqual
                            (oracleLines (
                                parseAt mode SyntaxCompilationTarget.Executable [
                                    logicalPath, text
                                    last
                                ]
                            ))
                            expected
                            "The diagnostics must match the Compatibility Oracle"
            ]

            testCase "files keep their order and their kind"
            <| fun _ ->
                let result =
                    parse SyntaxCompilationTarget.Executable [
                        "B.fsi", "module B\nval b: int\n"
                        "B.fs", "module B\nlet b = 1\n"
                        "A.fs", "module A\nlet a = 2\n"
                    ]

                Expect.sequenceEqual
                    (result.Files
                     |> Seq.map (fun file ->
                         match file with
                         | SyntaxFile.Signature signature -> $"signature {signature.LogicalPath}"
                         | SyntaxFile.Implementation implementation ->
                             $"implementation {implementation.LogicalPath}"
                     ))
                    [
                        "signature B.fsi"
                        "implementation B.fs"
                        "implementation A.fs"
                    ]
                    "Files keep the request order and their signature or implementation kind"
        ]
