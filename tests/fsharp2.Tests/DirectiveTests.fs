namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module DirectiveTests =
    let private prepare mode defines text =
        let language =
            LanguageVersion.normalize (Some mode)
            |> Result.defaultWith failtest

        let source = SourceText.fromString text
        Directives.analyze language (Set.ofList defines) source

    [<Tests>]
    let tests =
        testList "Issue28.Directives" [
            testCase "selects elif and preserves every source offset"
            <| fun _ ->
                let text = "#if A\nlet a = 1\n#elif B\nlet b = 2\n#else\nlet c = 3\n#endif"
                let result = prepare "10.0" [ "B" ] text

                Expect.equal result.CompatibilityText.Length text.Length "Compatibility text length"

                Expect.equal
                    (result.CompatibilityText
                     |> Seq.filter ((=) '\n')
                     |> Seq.length)
                    6
                    "Newline count"

                Expect.stringContains result.CompatibilityText "let b = 2" "Elif branch is active"
                Expect.isFalse (result.CompatibilityText.Contains "let a = 1") "If branch is blank"

                Expect.isFalse
                    (result.CompatibilityText.Contains "let c = 3")
                    "Else branch is blank"

                Expect.isEmpty result.Diagnostics "Balanced directives are valid"

            testCase "evaluates Boolean conditional expressions"
            <| fun _ ->
                let text =
                    "#if A && !(B || C)
let selected = 1
#else
let other = 2
#endif"

                let result = prepare "10.0" [ "A" ] text

                Expect.stringContains
                    result.CompatibilityText
                    "let selected = 1"
                    "The expression selects the if branch"

                Expect.isFalse
                    (result.CompatibilityText.Contains "let other = 2")
                    "The else branch is blank"

                Expect.isEmpty result.Diagnostics "A complete expression is valid"

            testCase "accepts tab separators and trailing line comments on conditional directives"
            <| fun _ ->
                let text =
                    "#if	A // enabled
let selected = 1
#else  // other
let other = 2
#endif   // done"

                let result = prepare "10.0" [ "A" ] text

                Expect.stringContains
                    result.CompatibilityText
                    "let selected = 1"
                    "The tab-separated condition is evaluated"

                Expect.isFalse
                    (result.CompatibilityText.Contains "let other = 2")
                    "The commented else directive closes the active branch"

                Expect.isEmpty result.Diagnostics "Commented directives are balanced"

            testCase "reports unmatched conditional directives with Oracle messages"
            <| fun _ ->
                let diagnosticsFor text =
                    (prepare "10.0" [] text).Diagnostics
                    |> Seq.map (fun diagnostic ->
                        diagnostic.Code,
                        diagnostic.Message,
                        diagnostic.Range.Start.Line,
                        diagnostic.Range.Start.Column
                    )
                    |> Seq.toList

                Expect.equal
                    (diagnosticsFor "#endif\nlet x = 1")
                    [
                        "FS0010",
                        "#endif has no matching #if in definition. Expected incomplete structured construct at or before this point or other token.",
                        1,
                        1
                    ]
                    "An unmatched #endif"

                Expect.equal
                    (diagnosticsFor "#elif A\nlet x = 1")
                    [
                        "FS0010",
                        "Unexpected keyword 'elif' in directive. Expected identifier or other token.",
                        1,
                        2
                    ]
                    "An unmatched #elif"

                Expect.equal
                    (diagnosticsFor "#if A\nlet x = 1")
                    [ "FS0513", "End of file in #if section begun at or after here", 1, 1 ]
                    "An unterminated #if"

            testCase "ignores unclosed literals inside inactive branches"
            <| fun _ ->
                let text =
                    "#if NEVER
let broken = \"
#else
let selected = 1
#endif"

                let result = prepare "10.0" [] text

                Expect.stringContains
                    result.CompatibilityText
                    "let selected = 1"
                    "The else branch stays active"

                Expect.isFalse
                    (result.CompatibilityText.Contains "#endif")
                    "The closing directive is still recognized"

                Expect.isEmpty result.Diagnostics "Inactive text reports no diagnostics"

            testCase "accepts every Oracle line directive form"
            <| fun _ ->
                let mappedLine (text: string) =
                    let result = prepare "10.0" [] text

                    let mapped =
                        SourceMap.positionAt result.SourceMap (text.IndexOf("let value"))
                        |> SourceMap.mapPosition result.SourceMap

                    Expect.isEmpty result.Diagnostics $"The line directive in '{text}' is valid"
                    mapped.LogicalPath, mapped.Line

                let lines (values: string list) = String.concat "\n" values

                Expect.equal
                    (mappedLine (
                        lines [
                            "#line 10"
                            "let value = 1"
                        ]
                    ))
                    (None, 10)
                    "A line number without a file"

                Expect.equal
                    (mappedLine (
                        lines [
                            "#line 1 \"a.fs\""
                            "#line 20"
                            "let value = 1"
                        ]
                    ))
                    (Some "a.fs", 20)
                    "A line number without a file keeps the current file"

                Expect.equal
                    (mappedLine (
                        lines [
                            "# 10 \"f.fs\""
                            "let value = 1"
                        ]
                    ))
                    (Some "f.fs", 10)
                    "The short form"

                Expect.equal
                    (mappedLine (
                        lines [
                            "#line abc"
                            "let value = 1"
                        ]
                    ))
                    (None, 2)
                    "An ignored malformed directive"

            testCase "parses warning directive arguments"
            <| fun _ ->
                let lines (values: string list) = String.concat "\n" values

                let diagnosticsFor text =
                    (prepare "10.0" [] text).Diagnostics
                    |> Seq.map (fun diagnostic ->
                        Expect.equal
                            diagnostic.Severity
                            (if diagnostic.Code = "FS0203" then
                                 LexicalSeverity.Warning
                             else
                                 LexicalSeverity.Error)
                            $"{diagnostic.Code} severity"

                        diagnostic.Code, diagnostic.Range.Start.Column
                    )
                    |> Seq.toList

                Expect.equal
                    (diagnosticsFor (
                        lines [
                            "#nowarn"
                            "let x = 1"
                        ]
                    ))
                    [ "FS3875", 1 ]
                    "A directive without arguments"

                Expect.equal
                    (diagnosticsFor (
                        lines [
                            "#nowarn x"
                            "let x = 1"
                        ]
                    ))
                    [ "FS0203", 9 ]
                    "An unquoted invalid argument is an Oracle warning"

                Expect.equal
                    (diagnosticsFor (
                        lines [
                            "#nowarn \"abc\""
                            "let x = 1"
                        ]
                    ))
                    [ "FS0203", 9 ]
                    "A quoted invalid argument is an Oracle warning"

                Expect.sequenceEqual
                    ((prepare
                        "10.0"
                        []
                        (lines [
                            "#nowarn \"40\" FS0049 25"
                            "let x = 1"
                        ]))
                         .WarningDirectives
                     |> Seq.map _.Code)
                    [
                        "40"
                        "FS0049"
                        "25"
                    ]
                    "Every argument becomes a warning directive"

            testCase "keeps later directives after a string that closes in inactive text"
            <| fun _ ->
                let text =
                    String.concat "@" [
                        "#if NEVER"
                        "let s = \""
                        "#endif"
                        "let t = \"a\""
                        "#if OTHER"
                        "let u = 1"
                        "#endif"
                    ]

                let result = prepare "10.0" [] (text.Replace('@', '\n'))

                Expect.stringContains
                    result.CompatibilityText
                    "let t = \"a\""
                    "The active string stays"

                Expect.isFalse
                    (result.CompatibilityText.Contains "let u = 1")
                    "The later inactive branch is blanked"

                Expect.isEmpty result.Diagnostics "The directives stay balanced"

            testCase "reports a duplicate else at the Oracle position"
            <| fun _ ->
                let text =
                    String.concat "\n" [
                        "#if A"
                        "#else"
                        "#else"
                        "#endif"
                    ]

                Expect.equal
                    ((prepare "10.0" [] text).Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code, diagnostic.Message, diagnostic.Range.Start.Line
                     )
                     |> Seq.toList)
                    [
                        "FS0010",
                        "#endif required for #else in definition. Expected incomplete structured construct at or before this point or other token.",
                        3
                    ]
                    "The second else reports the Oracle message"

            testCase "keeps directives after an interpolated verbatim string"
            <| fun _ ->
                let text =
                    String.concat "\n" [
                        "let path = @$\"C:\\\""
                        "#if NEVER"
                        "let hidden = 1"
                        "#endif"
                    ]

                let result = prepare "10.0" [] text

                Expect.isFalse
                    (result.CompatibilityText.Contains "let hidden = 1")
                    "The directive after the string is recognized"

                Expect.isEmpty result.Diagnostics "The directives stay balanced"

            testCase "reports malformed conditional expressions at the Oracle position"
            <| fun _ ->
                let diagnosticFor expression =
                    let result =
                        prepare
                            "10.0"
                            []
                            $"#if {expression}
let value = 1
#endif"

                    result.Diagnostics
                    |> Seq.map (fun diagnostic ->
                        diagnostic.Code, diagnostic.Range.Start.Line, diagnostic.Range.Start.Column
                    )
                    |> Seq.toList

                Expect.equal
                    (diagnosticFor "A &&")
                    [ "FS3184", 1, 9 ]
                    "A trailing operator is incomplete"

                Expect.equal
                    (diagnosticFor "A B")
                    [ "FS3184", 1, 7 ]
                    "Adjacent identifiers are incomplete"

                Expect.equal (diagnosticFor "(A") [ "FS3185", 1, 7 ] "An open group requires ')'"

                Expect.equal
                    (diagnosticFor "A $")
                    [ "FS3182", 1, 8 ]
                    "An unknown character is unexpected"

            testCase "records line light and indent directives"
            <| fun _ ->
                let result =
                    prepare
                        "10.0"
                        []
                        "#line 40 \"mapped.fs\"\n#light \"off\"\n#indent \"on\"\nlet value = 1"

                let mapped =
                    SourceMap.positionAt result.SourceMap 60
                    |> SourceMap.mapPosition result.SourceMap

                Expect.equal (mapped.LogicalPath, mapped.Line) (Some "mapped.fs", 42) "Line mapping"

                Expect.sequenceEqual
                    (result.Directives
                     |> Seq.map _.Kind)
                    [
                        DirectiveKind.Line
                        DirectiveKind.Light
                        DirectiveKind.Indent
                    ]
                    "Directive order"

            testCase "ignores directive text inside comments strings and inactive branches"
            <| fun _ ->
                let protectedText =
                    "(*\n#if HIDDEN\n*)\nlet text = \"\"\"\n#line 99 \"wrong.fs\"\n\"\"\"\nlet value = 1"

                let protectedResult = prepare "10.0" [] protectedText

                Expect.isEmpty
                    protectedResult.Directives
                    "Comment and string content does not create directives"

                Expect.isEmpty
                    protectedResult.Diagnostics
                    "Comment and string content does not create directive diagnostics"

                let inactiveText = "#if MISSING\n#line 90 \"wrong.fs\"\n#endif\nlet value = 1"

                let inactiveResult = prepare "10.0" [] inactiveText

                let mapped =
                    SourceMap.positionAt
                        inactiveResult.SourceMap
                        (inactiveText.IndexOf("let value"))
                    |> SourceMap.mapPosition inactiveResult.SourceMap

                Expect.equal
                    mapped.LogicalPath
                    None
                    "An inactive line directive does not change the path"

                Expect.equal mapped.Line 4 "An inactive line directive does not change the line"

            testCase "gates scoped warnon and records path-neutral warning actions"
            <| fun _ ->
                let text = "#nowarn \"25\"\nlet first = 1\n#warnon \"25\"\nlet second = 2"
                let mode9 = prepare "9.0" [] text
                let mode10 = prepare "10.0" [] text

                Expect.sequenceEqual
                    (mode10.WarningDirectives
                     |> Seq.map (fun item -> item.Action, item.Code))
                    [
                        LocalWarningDirectiveAction.Disable, "25"
                        LocalWarningDirectiveAction.Enable, "25"
                    ]
                    "Warning actions"

                Expect.isNone
                    mode10.WarningDirectives[0].LogicalPath
                    "Cached warning directives are path-neutral"

                Expect.contains
                    (mode9.Diagnostics
                     |> Seq.map _.Code)
                    "FS3350"
                    "9.0 reports the gated directive"

                Expect.isFalse
                    (mode10.Diagnostics
                     |> Seq.exists (fun item -> item.Code = "FS3350"))
                    "10.0 enables warnon"

            testCase "recovers ordered unmatched and EOF directive faults"
            <| fun _ ->
                let result = prepare "10.0" [] "#else\n#endif\n#if A\nlet value = 1"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map _.Code)
                    [
                        "FS0010"
                        "FS0010"
                        "FS0513"
                    ]
                    "All directive faults are retained"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map _.Order)
                    [
                        0L
                        1L
                        2L
                    ]
                    "Diagnostic order"
        ]
