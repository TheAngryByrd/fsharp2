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
    let isPrefixOperator (token: LexicalToken) (next: LexicalToken option) =
        token.Kind = LexicalTokenKind.Operator
        && (token.Text.StartsWith('+')
            || token.Text.StartsWith('-')
            || token.Text.StartsWith('%')
            || token.Text = "&"
            || token.Text = "&&")
        && next
           |> Option.exists (fun next -> next.Range.Start.Offset = token.Range.End.Offset)

    // The FCS lexical filter does not treat '=', '<', '>', or a prefix operator as an infix token for layout.
    let isInfixToken (token: LexicalToken) (next: LexicalToken option) =
        match token.Kind with
        | LexicalTokenKind.Delimiter -> token.Text = ","
        | LexicalTokenKind.Keyword -> token.Text = "or"
        | LexicalTokenKind.Operator when not (isPrefixOperator token next) ->
            match token.Text with
            | "="
            | "<"
            | ">"
            | "|"
            | "->"
            | "<-"
            | "<@"
            | "<@@"
            | "@>"
            | "@@>" -> false
            | "!="
            | "::"
            | ":="
            | ":>"
            | ":?>" -> true
            | text when text.StartsWith('.') ->
                let rest = text.TrimStart '.'

                rest.Length > 0
                && not (rest.Contains '$')
                && ("@^<>=|&+-*/%".IndexOf(rest[0])
                    >= 0
                    || rest.StartsWith("!=", StringComparison.Ordinal))
            | text ->
                "@^<>=|&+-*/%$".IndexOf(text[0])
                >= 0
        | _ -> false

    // FCS counts the 'or' keyword as one character when an infix token undents.
    let infixLength (token: LexicalToken) =
        if token.Kind = LexicalTokenKind.Keyword then
            1
        else
            token.Text.Length

    let private opensBlockAfter (token: LexicalToken) =
        token.Kind = LexicalTokenKind.Keyword
        && token.Text
           <> "true"
        && token.Text
           <> "false"
        && token.Text
           <> "null"
        && token.Text
           <> "or"
        || token.Kind = LexicalTokenKind.Operator
           && (token.Text = "="
               || token.Text = "->"
               || token.Text = "<-"
               || token.Text = "|"
               || token.Text = ":")

    type private BlockLine = { StartsWithBar: bool; HasArrow: bool }

    [<RequireQualifiedAccess>]
    type private ItemColumn =
        | Opened of int
        | Conditional of int
        | Match of int

    let private isKeywordText text (token: LexicalToken) =
        token.Kind = LexicalTokenKind.Keyword
        && token.Text = text

    let private startsConditional (token: LexicalToken) =
        isKeywordText "if" token
        || isKeywordText "elif" token

    let private continuesConditional (token: LexicalToken) =
        isKeywordText "then" token
        || isKeywordText "elif" token
        || isKeywordText "else" token

    let private continuesConstruct (token: LexicalToken) =
        continuesConditional token
        || isKeywordText "with" token

    let rec private withoutLastMatch items =
        match items with
        | [] -> []
        | ItemColumn.Match _ :: rest -> rest
        | item :: rest ->
            item
            :: withoutLastMatch rest

    let rec private innermostMatch column items =
        match items with
        | [] -> None
        | ItemColumn.Match matchColumn :: _ when
            matchColumn
            <= column
            ->
            Some matchColumn
        | _ :: rest -> innermostMatch column rest

    let private clears startsInfix column item =
        match item with
        | ItemColumn.Opened blockColumn ->
            column > blockColumn
            || startsInfix
               && column = blockColumn
        | ItemColumn.Conditional ifColumn -> column > ifColumn
        | ItemColumn.Match matchColumn -> column > matchColumn

    // FCS closes the contexts after an 'if' at 'then', 'elif', or 'else', and after a 'match' at 'with'. An 'if' or a 'match' stays open until a token is left of it.
    let rec private openConstruct (keyword: LexicalToken) column items =
        match items with
        | [] -> None
        | ItemColumn.Conditional ifColumn :: _ when
            continuesConditional keyword
            && ifColumn
               <= column
            ->
            Some items
        | ItemColumn.Match matchColumn :: rest when
            isKeywordText "with" keyword
            && matchColumn
               <= column
            ->
            Some rest
        | _ :: rest -> openConstruct keyword column rest

    let apply (source: DecodedSource) (directives: DirectiveResult) (lexed: LexerResult) =
        let tokens = ResizeArray<LayoutToken>()
        let diagnostics = ResizeArray<SourceLexicalDiagnostic>(directives.Diagnostics)
        let mutable indents = [ 0 ]

        let mutable blockLines = [
            {
                StartsWithBar = false
                HasArrow = false
            }
        ]

        let mutable openers: SourcePosition list = []
        let mutable previousSignificantStart: SourcePosition option = None
        let mutable previousLastToken: LexicalToken option = None
        let mutable itemBlockColumns: ItemColumn list = []
        let mutable savedItemColumns: ItemColumn list list = []
        let mutable firstClauseLimit: int option = None
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

            let mutable lineBlockColumns = []
            let mutable lineFirstClauseLimit = None
            let mutable lineWithMatch = None
            let mutable lineHasArrow = false
            let mutable lastPopped = None

            for index = 0 to lineTokens.Length
                             - 1 do
                let token = lineTokens[index]

                if
                    delimiterDepth = 0
                    && token.Kind = LexicalTokenKind.Operator
                    && token.Text = "->"
                then
                    lineHasArrow <- true

                if
                    delimiterDepth = 0
                    && isKeywordText "with" token
                then
                    if index + 1 = lineTokens.Length then
                        lineFirstClauseLimit <- innermostMatch Int32.MaxValue lineBlockColumns

                    lineBlockColumns <- withoutLastMatch lineBlockColumns

                if
                    delimiterDepth = 0
                    && startsConditional token
                then
                    lineBlockColumns <-
                        ItemColumn.Conditional token.Range.Start.Column
                        :: lineBlockColumns
                elif
                    delimiterDepth = 0
                    && isKeywordText "match" token
                then
                    lineBlockColumns <-
                        ItemColumn.Match token.Range.Start.Column
                        :: lineBlockColumns
                elif
                    delimiterDepth = 0
                    && index + 1 < lineTokens.Length
                    && opensBlockAfter token
                    && token.Text
                       <> "<-"
                then
                    lineBlockColumns <-
                        ItemColumn.Opened lineTokens[index + 1].Range.Start.Column
                        :: lineBlockColumns

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

                let startsInfix = isInfixToken lineTokens[0] (Array.tryItem 1 lineTokens)

                let startsBar =
                    lineTokens[0].Kind = LexicalTokenKind.Operator
                    && lineTokens[0].Text = "|"

                let lineBlockLine = {
                    StartsWithBar = startsBar
                    HasArrow = lineHasArrow
                }

                let continuesIf = continuesConstruct lineTokens[0]

                if continuesIf then
                    while (openConstruct lineTokens[0] column itemBlockColumns).IsNone
                          && (
                              match savedItemColumns with
                              | saved :: _ -> (openConstruct lineTokens[0] column saved).IsSome
                              | [] -> false
                          ) do
                        lastPopped <- Some(indents.Head, blockLines.Head)
                        indents <- indents.Tail
                        blockLines <- blockLines.Tail

                        match openers with
                        | _ :: tail -> openers <- tail
                        | [] -> ()

                        match savedItemColumns with
                        | saved :: tail ->
                            itemBlockColumns <- saved
                            savedItemColumns <- tail
                        | [] -> itemBlockColumns <- []

                        event
                            LayoutTokenKind.EndBlock
                            line
                            column
                            (startOffset
                             + charIndex)

                let conditional =
                    if continuesIf then
                        (lineWithMatch <- innermostMatch column itemBlockColumns
                         openConstruct lineTokens[0] column itemBlockColumns)
                    else
                        None

                // FCS starts an offside context only after a token that opens a block, at the next token on that line.
                let continuesItem =
                    previousLastToken
                    |> Option.exists (fun token -> not (opensBlockAfter token))
                    && itemBlockColumns
                       |> List.forall (clears startsInfix column)

                match conditional with
                | Some items when indentation > indents.Head ->
                    itemBlockColumns <-
                        lineBlockColumns
                        @ items
                | _ when
                    indentation > indents.Head
                    && continuesItem
                    ->
                    itemBlockColumns <-
                        lineBlockColumns
                        @ itemBlockColumns
                | _ when indentation > indents.Head ->
                    savedItemColumns <-
                        itemBlockColumns
                        :: savedItemColumns

                    itemBlockColumns <- lineBlockColumns

                    indents <-
                        indentation
                        :: indents

                    blockLines <-
                        lineBlockLine
                        :: blockLines

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
                | _ when
                    indentation = indents.Head
                    && indentation > 0
                    ->
                    itemBlockColumns <-
                        match conditional with
                        | Some items ->
                            lineBlockColumns
                            @ items
                        | None when startsInfix ->
                            lineBlockColumns
                            @ itemBlockColumns
                        | None -> lineBlockColumns

                    blockLines <-
                        lineBlockLine
                        :: blockLines.Tail

                    event
                        LayoutTokenKind.Separator
                        line
                        column
                        (startOffset
                         + charIndex)
                | _ ->
                    // 15.1.9: an infix token can be left of a block column by its length plus one.
                    let continuesBlock column =
                        startsInfix
                        && indentation
                           + infixLength lineTokens[0]
                           + 1
                           >= column

                    while indents.Head > indentation
                          && not (continuesBlock indents.Head) do
                        lastPopped <- Some(indents.Head, blockLines.Head)
                        indents <- indents.Tail
                        blockLines <- blockLines.Tail

                        match openers with
                        | _ :: tail -> openers <- tail
                        | [] -> ()

                        match savedItemColumns with
                        | saved :: tail ->
                            itemBlockColumns <- saved
                            savedItemColumns <- tail
                        | [] -> itemBlockColumns <- []

                        event
                            LayoutTokenKind.EndBlock
                            line
                            column
                            (startOffset
                             + charIndex)

                    let conditional =
                        if continuesIf then
                            (lineWithMatch <- innermostMatch column itemBlockColumns
                             openConstruct lineTokens[0] column itemBlockColumns)
                        else
                            None

                    itemBlockColumns <-
                        match conditional with
                        | Some items ->
                            lineBlockColumns
                            @ items
                        | None when startsInfix ->
                            lineBlockColumns
                            @ itemBlockColumns
                        | None -> lineBlockColumns

                    if
                        conditional.IsSome
                        && indentation > indents.Head
                    then
                        ()
                    elif indents.Head > indentation then
                        event
                            LayoutTokenKind.Separator
                            line
                            column
                            (startOffset
                             + charIndex)
                    elif
                        indentation
                        <> indents.Head
                        && not (
                            startsBar
                            && blockLines.Head.StartsWithBar
                            && match lastPopped with
                               | Some(poppedIndent, popped) ->
                                   if popped.StartsWithBar then
                                       indentation
                                       <> poppedIndent
                                          - 1
                                   else
                                       not popped.HasArrow
                               | None -> false
                        )
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
                            Severity = DiagnosticSeverity.Error
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

            if
                lineTokens.Length > 0
                && not isDirectiveLine
            then
                let start = lineTokens[0].Range.Start

                // The Oracle reports FS0058 for a first clause on the line after 'with' when the clause is left of 'match'.
                match firstClauseLimit with
                | Some limit when
                    depthBefore = 0
                    && start.Column < limit
                    && not (
                        diagnostics
                        |> Seq.exists (fun diagnostic -> diagnostic.Range.Start = start)
                    )
                    ->
                    diagnostics.Add {
                        Code = "FS0058"
                        Message = "Unexpected syntax or possible incorrect indentation."
                        Range = { Start = start; End = start }
                        Order = order
                        Severity = DiagnosticSeverity.Error
                    }

                    order <- order + 1L
                | _ -> ()

                firstClauseLimit <-
                    if
                        lineTokens.Length = 1
                        && isKeywordText "with" lineTokens[0]
                    then
                        lineWithMatch
                    else
                        lineFirstClauseLimit

                if
                    depthBefore > 0
                    || delimiterDepth > 0
                then
                    itemBlockColumns <-
                        lineBlockColumns
                        @ itemBlockColumns

                previousLastToken <- Array.tryLast lineTokens

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
