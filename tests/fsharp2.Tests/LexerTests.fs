namespace fsharp2.Tests

open System
open Expecto
open FSharp2.Compiler

module LexerTests =
    let private tokenize text =
        let language =
            LanguageVersion.normalize (Some "10.0")
            |> Result.defaultWith failtest

        Lexer.tokenize language (SourceText.fromString text)

    let private projection result =
        result.Tokens
        |> Seq.map (fun token ->
            token.Kind,
            token.Text,
            token.Range.Start.Line,
            token.Range.Start.Column,
            token.Range.End.Column
        )
        |> Seq.toArray

    [<Tests>]
    let tests =
        testList "Issue28.Lexer" [
            testCase "recognizes keywords Unicode and escaped identifiers with exact ranges"
            <| fun _ ->
                let result = tokenize "let café = ``odd name``\nmodule Ω"

                Expect.sequenceEqual
                    (projection result)
                    [|
                        LexicalTokenKind.Keyword, "let", 1, 1, 4
                        LexicalTokenKind.Identifier, "café", 1, 5, 9
                        LexicalTokenKind.Operator, "=", 1, 10, 11
                        LexicalTokenKind.EscapedIdentifier, "``odd name``", 1, 12, 24
                        LexicalTokenKind.Keyword, "module", 2, 1, 7
                        LexicalTokenKind.Identifier, "Ω", 2, 8, 9
                        LexicalTokenKind.EndOfFile, "", 2, 9, 9
                    |]
                    "Token projection"

                Expect.isEmpty result.Diagnostics "Valid identifiers do not report diagnostics"

            testCase "recognizes numeric character byte and string literal forms"
            <| fun _ ->
                let result =
                    tokenize
                        "0x2AUL 0b1010 3.14M 'λ' '\\n' 'a'B \"x\\n\" @\"raw\" \"\"\"triple\"\"\" \"bytes\"B"

                Expect.sequenceEqual
                    (result.Tokens
                     |> Seq.map (fun token -> token.Kind, token.Text)
                     |> Seq.toArray)
                    [|
                        LexicalTokenKind.NumericLiteral, "0x2AUL"
                        LexicalTokenKind.NumericLiteral, "0b1010"
                        LexicalTokenKind.NumericLiteral, "3.14M"
                        LexicalTokenKind.CharacterLiteral, "'λ'"
                        LexicalTokenKind.CharacterLiteral, "'\\n'"
                        LexicalTokenKind.ByteCharacterLiteral, "'a'B"
                        LexicalTokenKind.StringLiteral, "\"x\\n\""
                        LexicalTokenKind.StringLiteral, "@\"raw\""
                        LexicalTokenKind.StringLiteral, "\"\"\"triple\"\"\""
                        LexicalTokenKind.ByteStringLiteral, "\"bytes\"B"
                        LexicalTokenKind.EndOfFile, ""
                    |]
                    "Literal projection"

            testCase "uses longest operator match and preserves comment trivia"
            <| fun _ ->
                let result = tokenize "value<|||other // tail\n(* outer (* nested *) end *) next"

                Expect.contains
                    (result.Tokens
                     |> Seq.map _.Text)
                    "<|||"
                    "The complete symbolic operator is one token"

                Expect.equal result.Trivia.Length 5 "Whitespace, newline, and comments are retained"

                Expect.equal
                    result.Trivia[1].Kind
                    LexicalTriviaKind.LineComment
                    "Line comment trivia"

                Expect.equal
                    result.Trivia[3].Kind
                    LexicalTriviaKind.BlockComment
                    "Nested block comment trivia"

                Expect.stringContains result.Trivia[3].Text "nested" "Nested comment text"

            testCase "recovers after separated invalid source characters"
            <| fun _ ->
                let result = tokenize "let a = 1\u0000\nlet b = \uFFFD\nlet c = 3"

                Expect.sequenceEqual
                    (result.Diagnostics
                     |> Seq.map (fun diagnostic ->
                         diagnostic.Code,
                         diagnostic.Range.Start.Line,
                         diagnostic.Range.Start.Column
                     )
                     |> Seq.toArray)
                    [|
                        "FS0010", 1, 10
                        "FS0010", 2, 9
                    |]
                    "Separated faults remain ordered"

                Expect.contains
                    (result.Tokens
                     |> Seq.map _.Text)
                    "c"
                    "Lexing continues after both faults"

            testCase "reports unterminated escaped identifiers strings and block comments"
            <| fun _ ->
                let escaped = tokenize "let ``name = 1"
                let text = tokenize "let value = \"text"
                let comment = tokenize "let value = 1 (* comment"

                Expect.equal escaped.Diagnostics[0].Code "FS1230" "Escaped identifier diagnostic"
                Expect.equal text.Diagnostics[0].Code "FS0514" "String diagnostic"
                Expect.equal comment.Diagnostics[0].Code "FS0010" "Block comment diagnostic"
        ]
