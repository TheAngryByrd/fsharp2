namespace fsharp2.Tests

open System.Collections.Immutable
open Expecto
open FSharp2.Compiler

module ParserCompilationTests =
    let private language =
        LanguageVersion.normalize (Some "10.0")
        |> Result.defaultWith failtest

    let private parse target (files: (string * string) list) =
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
        [ missingDeclaration "NestedSig.fsi" 1 1 3 1 ]

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
            missingDeclaration "n-e.fs" 1 1 2 1
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

        "a nested module in an anonymous root",
        SyntaxCompilationTarget.Executable,
        [
            "NestedOnly.fs", "module M =\n    let a = 1\n"
            last
        ],
        [ missingDeclaration "NestedOnly.fs" 1 1 2 1 ]

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
    ]

    let private unmodeledExpressionDeclarationCases = [
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

        "Y13.fs",
        "module M\nmodule N =\n    f x )\n    let b = 1\n",
        [
            "Y13.fs(3,9,3,10): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "Y13.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y18.fs",
        "module M\nmodule N =\n    let a = 1\n    f x )\n",
        [
            "Y18.fs(4,9,4,10): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "Y18.fs(5,1,5,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y25.fs",
        "module M\nmodule N =\n    let a = 1\n    f x )\n    let b = 2\n",
        [
            "Y25.fs(4,9,4,10): error FS0010: Unexpected symbol ')' in definition. Expected incomplete structured construct at or before this point or other token."
            "Y25.fs(6,1,6,1): error FS0010: Incomplete structured construct at or before this point in implementation file"
        ]

        "Y28.fs",
        "module M\nf x in\n",
        [
            "Y28.fs(2,5,2,7): error FS0010: Unexpected keyword 'in' in definition. Expected incomplete structured construct at or before this point or other token."
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
