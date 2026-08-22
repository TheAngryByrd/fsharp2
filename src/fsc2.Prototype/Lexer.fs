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
        "!%&*+-./<=>?@^|~:#".IndexOf(character) >= 0

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
            text.Substring(startOffset, endOffset - startOffset)

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

            diagnosticOrder <- diagnosticOrder + 1L

        let scanQuoted startOffset delimiterLength verbatim =
            let mutable cursor = startOffset + delimiterLength
            let mutable closed = false

            while cursor < text.Length && not closed do
                if delimiterLength = 3 && cursor + 2 < text.Length && text[cursor] = '"' && text[cursor + 1] = '"' && text[cursor + 2] = '"' then
                    cursor <- cursor + 3
                    closed <- true
                elif delimiterLength = 1 && text[cursor] = '"' then
                    if verbatim && cursor + 1 < text.Length && text[cursor + 1] = '"' then
                        cursor <- cursor + 2
                    else
                        cursor <- cursor + 1
                        closed <- true
                elif not verbatim && text[cursor] = '\\' && cursor + 1 < text.Length then
                    cursor <- cursor + 2
                else
                    cursor <- cursor + 1

            cursor, closed

        while offset < text.Length do
            let startOffset = offset
            let current = text[offset]

            if current = ' ' || current = '\t' then
                offset <- offset + 1

                while offset < text.Length && (text[offset] = ' ' || text[offset] = '\t') do
                    offset <- offset + 1

                addTrivia LexicalTriviaKind.Whitespace startOffset offset
            elif current = '\r' || current = '\n' then
                offset <-
                    if current = '\r' && offset + 1 < text.Length && text[offset + 1] = '\n' then
                        offset + 2
                    else
                        offset + 1

                addTrivia LexicalTriviaKind.Newline startOffset offset
            elif current = '/' && offset + 1 < text.Length && text[offset + 1] = '/' then
                offset <- offset + 2

                while offset < text.Length && text[offset] <> '\r' && text[offset] <> '\n' do
                    offset <- offset + 1

                addTrivia LexicalTriviaKind.LineComment startOffset offset
            elif current = '(' && offset + 1 < text.Length && text[offset + 1] = '*' then
                offset <- offset + 2
                let mutable depth = 1

                while offset < text.Length && depth > 0 do
                    if offset + 1 < text.Length && text[offset] = '(' && text[offset + 1] = '*' then
                        depth <- depth + 1
                        offset <- offset + 2
                    elif offset + 1 < text.Length && text[offset] = '*' && text[offset + 1] = ')' then
                        depth <- depth - 1
                        offset <- offset + 2
                    else
                        offset <- offset + 1

                addTrivia LexicalTriviaKind.BlockComment startOffset offset

                if depth > 0 then
                    addDiagnostic "FS0010" "Incomplete structured construct at or before this point in expression." startOffset offset
            elif current = '`' && offset + 1 < text.Length && text[offset + 1] = '`' then
                offset <- offset + 2
                let mutable closed = false

                while offset < text.Length && not closed do
                    if offset + 1 < text.Length && text[offset] = '`' && text[offset + 1] = '`' then
                        offset <- offset + 2
                        closed <- true
                    else
                        offset <- offset + 1

                addToken (if closed then LexicalTokenKind.EscapedIdentifier else LexicalTokenKind.Invalid) startOffset offset

                if not closed then
                    addDiagnostic "FS1230" "Inner quote is not permitted in an identifier." startOffset offset
            elif current = '@' && offset + 1 < text.Length && text[offset + 1] = '"' then
                let endOffset, closed = scanQuoted (startOffset + 1) 1 true
                offset <- endOffset
                addToken LexicalTokenKind.StringLiteral startOffset offset

                if not closed then
                    addDiagnostic "FS0514" "End of file in string begun at or before here." startOffset offset
            elif current = '"' then
                let delimiterLength =
                    if offset + 2 < text.Length && text[offset + 1] = '"' && text[offset + 2] = '"' then 3 else 1

                let endOffset, closed = scanQuoted startOffset delimiterLength false
                offset <- endOffset

                let kind =
                    if closed && offset < text.Length && text[offset] = 'B' then
                        offset <- offset + 1
                        LexicalTokenKind.ByteStringLiteral
                    else
                        LexicalTokenKind.StringLiteral

                addToken kind startOffset offset

                if not closed then
                    addDiagnostic "FS0514" "End of file in string begun at or before here." startOffset offset
            elif current = '\'' then
                offset <- offset + 1

                if offset < text.Length && text[offset] = '\\' then
                    offset <- min text.Length (offset + 2)
                elif offset < text.Length then
                    offset <- offset + 1

                let closed = offset < text.Length && text[offset] = '\''

                if closed then
                    offset <- offset + 1

                let kind =
                    if closed && offset < text.Length && text[offset] = 'B' then
                        offset <- offset + 1
                        LexicalTokenKind.ByteCharacterLiteral
                    else
                        LexicalTokenKind.CharacterLiteral

                addToken kind startOffset offset

                if not closed then
                    addDiagnostic "FS0514" "End of file in character literal begun at or before here." startOffset offset
            elif Char.IsDigit current then
                offset <- offset + 1

                while offset < text.Length && (Char.IsLetterOrDigit text[offset] || text[offset] = '_' || text[offset] = '.') do
                    offset <- offset + 1

                addToken LexicalTokenKind.NumericLiteral startOffset offset
            elif isIdentifierStart current then
                offset <- offset + 1

                while offset < text.Length && isIdentifierContinue text[offset] do
                    offset <- offset + 1

                let value = slice startOffset offset
                addToken (if keywords.Contains value then LexicalTokenKind.Keyword else LexicalTokenKind.Identifier) startOffset offset
            elif current = '\u0000' || current = '\uFFFD' then
                offset <- offset + 1
                addToken LexicalTokenKind.Invalid startOffset offset
                addDiagnostic "FS0010" $"Unexpected character '{current}'." startOffset offset
            elif isOperatorCharacter current then
                offset <- offset + 1

                while offset < text.Length && isOperatorCharacter text[offset] do
                    offset <- offset + 1

                addToken LexicalTokenKind.Operator startOffset offset
            elif "()[]{},;".IndexOf(current) >= 0 then
                offset <- offset + 1
                addToken LexicalTokenKind.Delimiter startOffset offset
            else
                offset <- offset + 1
                addToken LexicalTokenKind.Invalid startOffset offset
                addDiagnostic "FS0010" $"Unexpected character '{current}'." startOffset offset

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
