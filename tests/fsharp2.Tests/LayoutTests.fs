namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module LayoutTests =
    let private prepare text =
        let language =
            LanguageVersion.normalize (Some "10.0")
            |> Result.defaultWith failtest

        let source = SourceText.fromString text
        let directives = Directives.analyze language Set.empty source

        let active =
            Lexer.tokenize language {
                source with
                    Text = directives.CompatibilityText
            }

        Layout.apply
            source
            {
                directives with
                    Diagnostics = active.Diagnostics.AddRange(directives.Diagnostics)
            }
            active

    [<Tests>]
    let tests =
        testList "Issue28.Layout" [
            testCase "emits block separator and dedent events with exact ranges"
            <| fun _ ->
                let result = prepare "let outer =\n    let inner = 1\n    inner\nlet after = 2"

                Expect.sequenceEqual
                    (result.Tokens
                     |> Seq.choose (fun token ->
                         if token.Kind = LayoutTokenKind.SourceToken then
                             None
                         else
                             Some(token.Kind, token.Range.Start.Line, token.Range.Start.Column)
                     ))
                    [
                        LayoutTokenKind.BeginBlock, 2, 5
                        LayoutTokenKind.Separator, 3, 5
                        LayoutTokenKind.EndBlock, 4, 1
                    ]
                    "Offside events"

            testCase "treats tabs consistently and ignores blank and comment-only lines"
            <| fun _ ->
                let result =
                    prepare "let outer =\n\tlet first = 1\n\n    // comment\n\tlet second = 2"

                let structural =
                    result.Tokens
                    |> Seq.filter (fun token ->
                        token.Kind
                        <> LayoutTokenKind.SourceToken
                    )
                    |> Seq.toArray

                Expect.equal structural.Length 3 "Blank and comment lines do not create events"
                Expect.equal structural[0].Kind LayoutTokenKind.BeginBlock "Tab starts a block"

                Expect.equal
                    structural[1].Kind
                    LayoutTokenKind.Separator
                    "Equal indentation separates declarations"

                Expect.equal structural[2].Kind LayoutTokenKind.EndBlock "EOF closes the block"

            testCase "suppresses layout inside balanced delimiters"
            <| fun _ ->
                let result = prepare "let values = [\n    1\n    2\n]"

                Expect.isFalse
                    (result.Tokens
                     |> Seq.exists (fun token -> token.Kind = LayoutTokenKind.BeginBlock))
                    "Delimited indentation is not an offside block"

            testCase "ignores delimiters inside literals and comments"
            <| fun _ ->
                let result =
                    prepare
                        "let outer =\n    let text = \"(\"\n    (* [ *)\n    let value = 1\nlet after = 2"

                Expect.sequenceEqual
                    (result.Tokens
                     |> Seq.choose (fun token ->
                         if token.Kind = LayoutTokenKind.SourceToken then
                             None
                         else
                             Some(token.Kind, token.Range.Start.Line)
                     ))
                    [
                        LayoutTokenKind.BeginBlock, 2
                        LayoutTokenKind.Separator, 4
                        LayoutTokenKind.EndBlock, 5
                    ]
                    "Literal and comment delimiters do not change layout depth"

            testCase "applies light syntax directives to offside events"
            <| fun _ ->
                let result =
                    prepare "#light \"off\"\nlet first =\n    1\n#light \"on\"\nlet second =\n    2"

                Expect.sequenceEqual
                    (result.Tokens
                     |> Seq.choose (fun token ->
                         if token.Kind = LayoutTokenKind.SourceToken then
                             None
                         else
                             Some(token.Kind, token.Range.Start.Line)
                     ))
                    [
                        LayoutTokenKind.BeginBlock, 6
                        LayoutTokenKind.EndBlock, 6
                    ]
                    "Offside events apply only while light syntax is enabled"

            testCase "ignores inactive conditional text"
            <| fun _ ->
                let result =
                    prepare
                        "let value =
#if NEVER
        $
    ignored
#endif
    1"

                Expect.isEmpty
                    result.Diagnostics
                    "Inactive text reports no lexical or layout diagnostics"

                Expect.isFalse
                    (result.Tokens
                     |> Seq.exists (fun token ->
                         token.Token
                         |> Option.exists (fun source -> source.Text = "ignored")
                     ))
                    "Inactive tokens do not reach the layout stream"

                Expect.sequenceEqual
                    (result.Tokens
                     |> Seq.choose (fun token ->
                         if token.Kind = LayoutTokenKind.SourceToken then
                             None
                         else
                             Some(token.Kind, token.Range.Start.Line)
                     ))
                    [
                        LayoutTokenKind.BeginBlock, 6
                        LayoutTokenKind.EndBlock, 6
                    ]
                    "Only active lines create offside events"

            testCase "reports bad dedent and still closes blocks at EOF"
            <| fun _ ->
                let result = prepare "let outer =\n    let inner =\n        1\n  let bad = 2"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map _.Code)
                    [ "FS0058" ]
                    "The layout pass reports one offside diagnostic without parser context"

                Expect.equal
                    (result.Tokens
                     |> Seq.filter (fun token -> token.Kind = LayoutTokenKind.EndBlock)
                     |> Seq.length)
                    2
                    "Both open blocks close"

            testCase "reports an invalid character without inventing offside recovery"
            <| fun _ ->
                let result =
                    prepare
                        "module Program
let value =�("

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code,
                         diagnostic.Message,
                         diagnostic.Range.Start.Line,
                         diagnostic.Range.Start.Column
                     ))
                    [ "FS0010", "Unexpected character '�'.", 2, 12 ]
                    "The layout pass adds no parser recovery diagnostics"
        ]
