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
            SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
            |> LexicalPipeline.prepare language Array.empty
        )
        |> ImmutableArray.CreateRange
        |> Parser.parseCompilation target

    let private oracleLines (result: SyntaxCompilationResult) =
        result.Diagnostics
        |> Seq.map (fun fileDiagnostic ->
            let diagnostic = fileDiagnostic.Diagnostic
            let range = diagnostic.Range

            $"{fileDiagnostic.LogicalPath}({range.Start.Line},{range.Start.Column},{range.End.Line},{range.End.Column}): error {diagnostic.Code}: {diagnostic.Message}"
        )
        |> Seq.toList

    let private missingDeclaration logicalPath startLine startColumn endLine endColumn =
        $"{logicalPath}({startLine},{startColumn},{endLine},{endColumn}): error FS0222: Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."

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
