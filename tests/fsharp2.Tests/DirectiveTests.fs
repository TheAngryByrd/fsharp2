namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module DirectiveTests =
    let private prepare mode defines text =
        let language =
            LanguageVersion.normalize (Some mode)
            |> Result.defaultWith failtest

        let source = SourceText.fromString text
        let lexed = Lexer.tokenize language source
        Directives.analyze language (Set.ofList defines) source lexed

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
                        "FS0010"
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
