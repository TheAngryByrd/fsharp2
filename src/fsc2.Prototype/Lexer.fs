namespace FSharp2.Compiler

open System
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type internal LexicalTokenKind =
    | Keyword
    | Identifier
    | EscapedIdentifier
    | Operator
    | NumericLiteral
    | CharacterLiteral
    | ByteCharacterLiteral
    | StringLiteral
    | ByteStringLiteral
    | Delimiter
    | Invalid
    | EndOfFile

[<RequireQualifiedAccess>]
type internal LexicalTriviaKind =
    | Whitespace
    | Newline
    | LineComment
    | BlockComment

type internal LexicalToken = {
    Kind: LexicalTokenKind
    Text: string
    Range: SourceRange
}

type internal LexicalTrivia = {
    Kind: LexicalTriviaKind
    Text: string
    Range: SourceRange
}

type internal SourceLexicalDiagnostic = {
    Code: string
    Message: string
    Range: SourceRange
    Order: int64
}

type internal LexerResult = {
    Tokens: ImmutableArray<LexicalToken>
    Trivia: ImmutableArray<LexicalTrivia>
    Diagnostics: ImmutableArray<SourceLexicalDiagnostic>
}

module internal Lexer =

    let private numericLiteralPattern =
        Text.RegularExpressions.Regex(
            "^(?:(?:0[xX][0-9a-fA-F][0-9a-fA-F_]*|0[oO][0-7][0-7_]*|0[bB][01][01_]*)(?:y|uy|s|us|l|u|ul|uL|UL|L|n|un|lf|LF)?"
            + "|[0-9][0-9_]*(?:y|uy|s|us|l|u|ul|uL|UL|L|n|un|I|Q|R|Z|N|G|M|m)?"
            + "|[0-9][0-9_]*(?:\\.[0-9_]*)?(?:[eE][+-]?[0-9][0-9_]*)?(?:f|F|m|M)?)$",
            Text.RegularExpressions.RegexOptions.CultureInvariant
        )

    let private isValidNumericLiteral (literal: string) = numericLiteralPattern.IsMatch literal

    let private keywords =
        set [
            "and"
            "as"
            "assert"
            "base"
            "begin"
            "class"
            "default"
            "delegate"
            "do"
            "done"
            "downcast"
            "downto"
            "elif"
            "else"
            "end"
            "exception"
            "extern"
            "false"
            "finally"
            "fixed"
            "for"
            "fun"
            "function"
            "global"
            "if"
            "in"
            "inherit"
            "inline"
            "interface"
            "internal"
            "lazy"
            "let"
            "match"
            "member"
            "module"
            "mutable"
            "namespace"
            "new"
            "not"
            "null"
            "of"
            "open"
            "or"
            "override"
            "private"
            "public"
            "rec"
            "return"
            "static"
            "struct"
            "then"
            "to"
            "true"
            "try"
            "type"
            "upcast"
            "use"
            "val"
            "void"
            "when"
            "while"
            "with"
            "yield"
        ]

    let private isIdentifierStart character =
        character = '_'
        || Char.IsLetter character

    let private isIdentifierContinue character =
        isIdentifierStart character
        || Char.IsDigit character
        || character = '\''

    let private isOperatorCharacter (character: char) =
        "!%&*+-./<=>?@^|~:#".IndexOf(character)
        >= 0

    let tokenize (_language: LanguageVersionIdentity) (source: DecodedSource) =
        let text = source.Text
        let tokens = ResizeArray<LexicalToken>()
        let trivia = ResizeArray<LexicalTrivia>()
        let diagnostics = ResizeArray<SourceLexicalDiagnostic>()
        let mutable offset = 0
        let mutable diagnosticOrder = 0L

        let range startOffset endOffset = {
            Start = SourceMap.positionAt source.Map startOffset
            End = SourceMap.positionAt source.Map endOffset
        }

        let slice startOffset endOffset =
            text.Substring(
                startOffset,
                endOffset
                - startOffset
            )

        let addToken kind startOffset endOffset =
            tokens.Add {
                Kind = kind
                Text = slice startOffset endOffset
                Range = range startOffset endOffset
            }

        let addTrivia kind startOffset endOffset =
            trivia.Add {
                Kind = kind
                Text = slice startOffset endOffset
                Range = range startOffset endOffset
            }

        let addDiagnostic code message startOffset endOffset =
            diagnostics.Add {
                Code = code
                Message = message
                Range = range startOffset endOffset
                Order = diagnosticOrder
            }

            diagnosticOrder <-
                diagnosticOrder
                + 1L

        let unexpectedCharacterMessage current startOffset endOffset =
            let position = SourceMap.positionAt source.Map startOffset

            let lineStart =
                source.Map.LineStarts[position.Line
                                      - 1]

            let prefix =
                text
                    .Substring(
                        lineStart,
                        startOffset
                        - lineStart
                    )
                    .TrimStart()

            if prefix.StartsWith("let ", StringComparison.Ordinal) then
                if
                    endOffset = text.Length
                    || text[endOffset] = '\r'
                    || text[endOffset] = '\n'
                then
                    $"Unexpected character '{current}' in binding. Expected incomplete structured construct at or before this point or other token."
                else
                    $"Unexpected character '{current}' in binding"
            else
                $"Unexpected character '{current}'."

        let scanQuoted startOffset delimiterLength verbatim =
            let mutable cursor =
                startOffset
                + delimiterLength

            let mutable closed = false

            while cursor < text.Length
                  && not closed do
                if
                    delimiterLength = 3
                    && cursor + 2 < text.Length
                    && text[cursor] = '"'
                    && text[cursor + 1] = '"'
                    && text[cursor + 2] = '"'
                then
                    cursor <- cursor + 3
                    closed <- true
                elif
                    delimiterLength = 1
                    && text[cursor] = '"'
                then
                    if
                        verbatim
                        && cursor + 1 < text.Length
                        && text[cursor + 1] = '"'
                    then
                        cursor <- cursor + 2
                    else
                        cursor <- cursor + 1
                        closed <- true
                elif
                    not verbatim
                    && text[cursor] = '\\'
                    && cursor + 1 < text.Length
                then
                    cursor <- cursor + 2
                else
                    cursor <- cursor + 1

            cursor, closed

        while offset < text.Length do
            let startOffset = offset
            let current = text[offset]

            if
                current = ' '
                || current = '\t'
            then
                offset <- offset + 1

                while offset < text.Length
                      && (text[offset] = ' '
                          || text[offset] = '\t') do
                    offset <- offset + 1

                addTrivia LexicalTriviaKind.Whitespace startOffset offset
            elif
                current = '\r'
                || current = '\n'
            then
                offset <-
                    if
                        current = '\r'
                        && offset + 1 < text.Length
                        && text[offset + 1] = '\n'
                    then
                        offset + 2
                    else
                        offset + 1

                addTrivia LexicalTriviaKind.Newline startOffset offset
            elif
                current = '/'
                && offset + 1 < text.Length
                && text[offset + 1] = '/'
            then
                offset <- offset + 2

                while offset < text.Length
                      && text[offset]
                         <> '\r'
                      && text[offset]
                         <> '\n' do
                    offset <- offset + 1

                addTrivia LexicalTriviaKind.LineComment startOffset offset
            elif
                current = '('
                && offset + 1 < text.Length
                && text[offset + 1] = '*'
            then
                offset <- offset + 2
                let mutable depth = 1

                while offset < text.Length
                      && depth > 0 do
                    if
                        offset + 1 < text.Length
                        && text[offset] = '('
                        && text[offset + 1] = '*'
                    then
                        depth <- depth + 1
                        offset <- offset + 2
                    elif
                        offset + 1 < text.Length
                        && text[offset] = '*'
                        && text[offset + 1] = ')'
                    then
                        depth <- depth - 1
                        offset <- offset + 2
                    else
                        offset <- offset + 1

                addTrivia LexicalTriviaKind.BlockComment startOffset offset

                if depth > 0 then
                    addDiagnostic
                        "FS0010"
                        "Incomplete structured construct at or before this point in expression."
                        startOffset
                        offset
            elif
                current = '`'
                && offset + 1 < text.Length
                && text[offset + 1] = '`'
            then
                offset <- offset + 2
                let mutable closed = false

                while offset < text.Length
                      && not closed do
                    if
                        offset + 1 < text.Length
                        && text[offset] = '`'
                        && text[offset + 1] = '`'
                    then
                        offset <- offset + 2
                        closed <- true
                    else
                        offset <- offset + 1

                addToken
                    (if closed then
                         LexicalTokenKind.EscapedIdentifier
                     else
                         LexicalTokenKind.Invalid)
                    startOffset
                    offset

                if not closed then
                    addDiagnostic
                        "FS1230"
                        "Inner quote is not permitted in an identifier."
                        startOffset
                        offset
            elif
                current = '@'
                && offset + 1 < text.Length
                && text[offset + 1] = '"'
            then
                let endOffset, closed =
                    scanQuoted
                        (startOffset
                         + 1)
                        1
                        true

                offset <- endOffset
                addToken LexicalTokenKind.StringLiteral startOffset offset

                if not closed then
                    addDiagnostic
                        "FS0514"
                        "End of file in string begun at or before here."
                        startOffset
                        offset
            elif current = '"' then
                let delimiterLength =
                    if
                        offset + 2 < text.Length
                        && text[offset + 1] = '"'
                        && text[offset + 2] = '"'
                    then
                        3
                    else
                        1

                let endOffset, closed = scanQuoted startOffset delimiterLength false
                offset <- endOffset

                let kind =
                    if
                        closed
                        && offset < text.Length
                        && text[offset] = 'B'
                    then
                        offset <- offset + 1
                        LexicalTokenKind.ByteStringLiteral
                    else
                        LexicalTokenKind.StringLiteral

                addToken kind startOffset offset

                if not closed then
                    addDiagnostic
                        "FS0514"
                        "End of file in string begun at or before here."
                        startOffset
                        offset
            elif current = '\'' then
                let isHexDigit character = Uri.IsHexDigit character

                let digitsMatch start count predicate =
                    start
                    + count
                    <= text.Length
                    && Seq.forall predicate (text.AsSpan(start, count).ToArray())

                let bodyLength =
                    let bodyStart =
                        startOffset
                        + 1

                    if
                        bodyStart
                        >= text.Length
                    then
                        None
                    elif
                        text[bodyStart]
                        <> '\\'
                    then
                        if
                            text[bodyStart] = '\n'
                            || text[bodyStart] = '\r'
                        then
                            None
                        else
                            Some 1
                    elif
                        bodyStart
                        + 1
                        >= text.Length
                    then
                        None
                    else
                        match
                            text[bodyStart
                                 + 1]
                        with
                        | 'u' when
                            digitsMatch
                                (bodyStart
                                 + 2)
                                4
                                isHexDigit
                            ->
                            Some 6
                        | 'U' when
                            digitsMatch
                                (bodyStart
                                 + 2)
                                8
                                isHexDigit
                            ->
                            Some 10
                        | 'x' when
                            digitsMatch
                                (bodyStart
                                 + 2)
                                2
                                isHexDigit
                            ->
                            Some 4
                        | digit when
                            Char.IsDigit digit
                            && digitsMatch
                                (bodyStart
                                 + 1)
                                3
                                Char.IsDigit
                            ->
                            Some 4
                        | _ -> Some 2

                let closingOffset =
                    bodyLength
                    |> Option.map (fun length ->
                        startOffset
                        + 1
                        + length
                    )
                    |> Option.filter (fun closing ->
                        closing < text.Length
                        && text[closing] = '\''
                    )

                match closingOffset with
                | Some closing ->
                    offset <-
                        closing
                        + 1

                    let kind =
                        if
                            offset < text.Length
                            && text[offset] = 'B'
                        then
                            offset <- offset + 1
                            LexicalTokenKind.ByteCharacterLiteral
                        else
                            LexicalTokenKind.CharacterLiteral

                    addToken kind startOffset offset
                | None when
                    startOffset
                    + 1 < text.Length
                    && (isIdentifierStart
                            text[startOffset
                                 + 1]
                        || text[startOffset
                                + 1] = '`')
                    ->
                    offset <-
                        startOffset
                        + 1

                    addToken LexicalTokenKind.Delimiter startOffset offset
                | None ->
                    offset <-
                        min
                            text.Length
                            (startOffset
                             + 2)

                    addToken LexicalTokenKind.CharacterLiteral startOffset offset

                    addDiagnostic
                        "FS0514"
                        "End of file in character literal begun at or before here."
                        startOffset
                        offset
            elif Char.IsDigit current then
                offset <- offset + 1

                let isRadixLiteral =
                    current = '0'
                    && offset < text.Length
                    && "xXoObB".IndexOf(text[offset])
                       >= 0

                while offset < text.Length
                      && (Char.IsLetterOrDigit text[offset]
                          || text[offset] = '_'
                          || (text[offset] = '.'
                              && not (
                                  offset + 1 < text.Length
                                  && text[offset + 1] = '.'
                              ))
                          || ((text[offset] = '+'
                               || text[offset] = '-')
                              && not isRadixLiteral
                              && (text[offset - 1] = 'e'
                                  || text[offset - 1] = 'E')
                              && offset + 1 < text.Length
                              && Char.IsDigit text[offset + 1])) do
                    offset <- offset + 1

                addToken LexicalTokenKind.NumericLiteral startOffset offset

                if
                    not (
                        isValidNumericLiteral (
                            text.Substring(
                                startOffset,
                                offset
                                - startOffset
                            )
                        )
                    )
                then
                    addDiagnostic
                        "FS1156"
                        "This is not a valid numeric literal. Valid numeric literals include 1, 0x1, 0o1, 0b1, 1l (int/int32), 1u (uint/uint32), 1L (int64), 1UL (uint64), 1s (int16), 1us (uint16), 1y (int8/sbyte), 1uy (uint8/byte), 1.0 (float/double), 1.0f (float32/single), 1.0m (decimal), 1I (bigint)."
                        startOffset
                        offset
            elif isIdentifierStart current then
                offset <- offset + 1

                while offset < text.Length
                      && isIdentifierContinue text[offset] do
                    offset <- offset + 1

                let value = slice startOffset offset

                addToken
                    (if keywords.Contains value then
                         LexicalTokenKind.Keyword
                     else
                         LexicalTokenKind.Identifier)
                    startOffset
                    offset
            elif
                current = '\u0000'
                || current = '\uFFFD'
            then
                offset <- offset + 1
                addToken LexicalTokenKind.Invalid startOffset offset

                addDiagnostic
                    "FS0010"
                    (unexpectedCharacterMessage current startOffset offset)
                    startOffset
                    offset
            elif isOperatorCharacter current then
                offset <- offset + 1

                while offset < text.Length
                      && isOperatorCharacter text[offset] do
                    offset <- offset + 1

                addToken LexicalTokenKind.Operator startOffset offset
            elif
                "()[]{},;".IndexOf(current)
                >= 0
            then
                offset <- offset + 1
                addToken LexicalTokenKind.Delimiter startOffset offset
            else
                offset <- offset + 1
                addToken LexicalTokenKind.Invalid startOffset offset

                addDiagnostic
                    "FS0010"
                    (unexpectedCharacterMessage current startOffset offset)
                    startOffset
                    offset

        let eof = SourceMap.positionAt source.Map text.Length

        tokens.Add {
            Kind = LexicalTokenKind.EndOfFile
            Text = String.Empty
            Range = { Start = eof; End = eof }
        }

        {
            Tokens = ImmutableArray.CreateRange tokens
            Trivia = ImmutableArray.CreateRange trivia
            Diagnostics = ImmutableArray.CreateRange diagnostics
        }
