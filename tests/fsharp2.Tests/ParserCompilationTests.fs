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
