namespace FSharp2.Compiler

open System
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type internal LayoutTokenKind =
    | SourceToken
    | BeginBlock
    | Separator
    | EndBlock

type internal LayoutToken = {
    Kind: LayoutTokenKind
    Token: LexicalToken option
    Range: SourceRange
}

type internal LayoutResult = {
    Tokens: ImmutableArray<LayoutToken>
    Diagnostics: ImmutableArray<SourceLexicalDiagnostic>
}

module internal Layout =
    let apply (source: DecodedSource) (directives: DirectiveResult) (lexed: LexerResult) =
        let tokens = ResizeArray<LayoutToken>()
        let diagnostics = ResizeArray<SourceLexicalDiagnostic>(directives.Diagnostics)
        let mutable indents = [ 0 ]
        let mutable openers: SourcePosition list = []
        let mutable previousSignificantStart: SourcePosition option = None
        let mutable delimiterDepth = 0
        let mutable order = int64 diagnostics.Count

        let isActive (token: LexicalToken) =
            let offset = token.Range.Start.Offset

            offset
            >= source.Text.Length
            || directives.CompatibilityText[offset] = source.Text[offset]

        let activeTokens =
            lexed.Tokens
            |> Seq.filter isActive
            |> Seq.toArray

        let event kind line column offset =
            let position = {
                Offset = offset
                Line = line
                Column = column
            }

            tokens.Add {
                Kind = kind
                Token = None
                Range = { Start = position; End = position }
            }

        for lineIndex = 0 to source.Map.LineStarts.Length
                             - 1 do
            let startOffset = source.Map.LineStarts[lineIndex]

            let endOffset =
                if
                    lineIndex
                    + 1 < source.Map.LineStarts.Length
                then
                    source.Map.LineStarts[lineIndex
                                          + 1]
                else
                    source.Text.Length

            let mutable contentEnd = endOffset

            while contentEnd > startOffset
                  && (directives.CompatibilityText[contentEnd
                                                   - 1] = '\r'
                      || directives.CompatibilityText[contentEnd
                                                      - 1] = '\n') do
                contentEnd <-
                    contentEnd
                    - 1

            let lineText =
                directives.CompatibilityText.Substring(
                    startOffset,
                    contentEnd
                    - startOffset
                )

            let mutable charIndex = 0
            let mutable indentation = 0

            while charIndex < lineText.Length
                  && (lineText[charIndex] = ' '
                      || lineText[charIndex] = '\t') do
                indentation <-
                    indentation
                    + (if lineText[charIndex] = '\t' then 4 else 1)

                charIndex <-
                    charIndex
                    + 1

            let trimmed = lineText.Substring(charIndex)
            let depthBefore = delimiterDepth

            let line =
                lineIndex
                + 1

            let lineTokens =
                activeTokens
                |> Seq.filter (fun token ->
                    token.Kind
                    <> LexicalTokenKind.EndOfFile
                    && token.Range.Start.Offset
                       >= startOffset
                    && token.Range.Start.Offset < contentEnd
                )
                |> Seq.toArray

            for token in lineTokens do
                if token.Kind = LexicalTokenKind.Delimiter then
                    if
                        token.Text.Length > 0
                        && "([{".IndexOf(token.Text[0])
                           >= 0
                    then
                        delimiterDepth <-
                            delimiterDepth
                            + 1
                    elif
                        token.Text.Length > 0
                        && ")]}"
                            .IndexOf(
                                token.Text[token.Text.Length
                                           - 1]
                            )
                           >= 0
                    then
                        delimiterDepth <-
                            max
                                0
                                (delimiterDepth
                                 - 1)

            let isDirectiveLine =
                directives.Directives
                |> Seq.exists (fun directive -> directive.Range.Start.Line = line)

            let lightSyntaxEnabled =
                directives.Directives
                |> Seq.filter (fun directive ->
                    directive.Kind = DirectiveKind.Light
                    && directive.Range.Start.Line < line
                )
                |> Seq.tryLast
                |> Option.forall (fun directive ->
                    not (directive.Text.Contains("\"off\"", StringComparison.Ordinal))
                )


            if
                lineTokens.Length > 0
                && not isDirectiveLine
                && lightSyntaxEnabled
                && depthBefore = 0
                && delimiterDepth = 0
            then
                let column =
                    charIndex
                    + 1

                if indentation > indents.Head then
                    indents <-
                        indentation
                        :: indents

                    openers <-
                        previousSignificantStart
                        |> Option.defaultValue {
                            Offset =
                                startOffset
                                + charIndex
                            Line = line
                            Column = column
                        }
                        |> fun position ->
                            position
                            :: openers

                    event
                        LayoutTokenKind.BeginBlock
                        line
                        column
                        (startOffset
                         + charIndex)
                elif
                    indentation = indents.Head
                    && indentation > 0
                then
                    event
                        LayoutTokenKind.Separator
                        line
                        column
                        (startOffset
                         + charIndex)
                else
                    let closedOpeners = ResizeArray<SourcePosition>()

                    while indents.Head > indentation do
                        indents <- indents.Tail

                        match openers with
                        | opener :: tail ->
                            closedOpeners.Add opener
                            openers <- tail
                        | [] -> ()

                        event
                            LayoutTokenKind.EndBlock
                            line
                            column
                            (startOffset
                             + charIndex)

                    if
                        indentation
                        <> indents.Head
                    then
                        let position = {
                            Offset =
                                startOffset
                                + charIndex
                            Line = line
                            Column = column
                        }

                        diagnostics.Add {
                            Code = "FS0058"
                            Message = "Unexpected syntax or possible incorrect indentation."
                            Range = { Start = position; End = position }
                            Order = order
                            Severity = LexicalSeverity.Error
                        }

                        order <- order + 1L

                previousSignificantStart <-
                    Some {
                        Offset =
                            startOffset
                            + charIndex
                        Line = line
                        Column = column
                    }

        let eof = SourceMap.positionAt source.Map source.Text.Length

        while indents.Head > 0 do
            indents <- indents.Tail

            tokens.Add {
                Kind = LayoutTokenKind.EndBlock
                Token = None
                Range = { Start = eof; End = eof }
            }

        for token in activeTokens do
            tokens.Add {
                Kind = LayoutTokenKind.SourceToken
                Token = Some token
                Range = token.Range
            }

        {
            Tokens = ImmutableArray.CreateRange tokens
            Diagnostics = ImmutableArray.CreateRange diagnostics
        }
