namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Collections.Immutable

module internal Parser =
    type private Cursor(tokens: ImmutableArray<LayoutToken>) =
        let mutable index = 0
        let mutable remainder: LayoutToken option = None
        let mutable lastEnd = tokens[0].Range.Start

        member _.Current =
            remainder
            |> Option.defaultValue tokens[index]

        member _.LastEnd = lastEnd

        member this.Peek offset =
            if offset = 0 then
                this.Current
            else
                tokens[min
                           (index
                            + offset)
                           (tokens.Length
                            - 1)]

        member this.Advance() =
            let token = this.Current

            if token.Kind = LayoutTokenKind.SourceToken then
                lastEnd <- token.Range.End

            remainder <- None

            if index + 1 < tokens.Length then
                index <- index + 1

            token

        member this.AdvanceFirstCharacter() =
            let token = this.Current

            match token.Token with
            | Some source when source.Text.Length > 1 ->
                let split = {
                    source.Range.Start with
                        Offset =
                            source.Range.Start.Offset
                            + 1
                        Column =
                            source.Range.Start.Column
                            + 1
                }

                let first = {
                    source with
                        Text = source.Text.Substring(0, 1)
                        Range = { source.Range with End = split }
                }

                let rest = {
                    source with
                        Text = source.Text.Substring 1
                        Range = { source.Range with Start = split }
                }

                lastEnd <- split

                remainder <-
                    Some {
                        token with
                            Token = Some rest
                            Range = rest.Range
                    }

                {
                    token with
                        Token = Some first
                        Range = first.Range
                }
            | _ -> this.Advance()

    type private ParserState = {
        Cursor: Cursor
        Diagnostics: ResizeArray<SyntaxDiagnostic>
        ReportedStarts: HashSet<int>
    }

    let private sourceKind (token: LayoutToken) =
        token.Token
        |> Option.map _.Kind

    let private tokenText (token: LayoutToken) =
        token.Token
        |> Option.map _.Text
        |> Option.defaultValue String.Empty

    let private isKind kind (token: LayoutToken) = sourceKind token = Some kind

    let private isText kind text (token: LayoutToken) =
        isKind kind token
        && tokenText token = text

    let private isKeyword = isText LexicalTokenKind.Keyword
    let private isOperator = isText LexicalTokenKind.Operator
    let private isDelimiter = isText LexicalTokenKind.Delimiter
    let private isEndOfFile = isKind LexicalTokenKind.EndOfFile

    let private isIdentifier (token: LayoutToken) =
        isKind LexicalTokenKind.Identifier token
        || isKind LexicalTokenKind.EscapedIdentifier token

    let private span (start: SourceRange) (finish: SourceRange) = {
        Start = start.Start
        End = finish.End
    }

    let private emptyAt position = { Start = position; End = position }

    let private report state code message (range: SourceRange) =
        state.ReportedStarts.Add range.Start.Offset
        |> ignore

        state.Diagnostics.Add {
            Code = code
            Message = message
            Range = range
        }

    let private describe (token: LayoutToken) =
        match token.Kind, token.Token with
        | LayoutTokenKind.SourceToken, Some source ->
            match source.Kind with
            | LexicalTokenKind.Keyword -> $"keyword '{source.Text}'"
            | LexicalTokenKind.EndOfFile -> "end of input"
            | LexicalTokenKind.Identifier
            | LexicalTokenKind.EscapedIdentifier -> $"identifier '{source.Text}'"
            | _ -> $"symbol '{source.Text}'"
        | LayoutTokenKind.BeginBlock, _ -> "an indented block"
        | LayoutTokenKind.Separator, _ -> "a new line at the same indentation"
        | LayoutTokenKind.EndBlock, _
        | LayoutTokenKind.SourceToken, None -> "the end of an indented block"

    let private unsupportedCode = "FSC2P1001"

    let private reportedAt state (token: LayoutToken) =
        state.ReportedStarts.Contains token.Range.Start.Offset

    let private reportUnsupported state (token: LayoutToken) context =
        report
            state
            unsupportedCode
            $"The syntax parser does not support {describe token} in {context}."
            token.Range

    [<RequireQualifiedAccess>]
    type private RecoveryPoint =
        | BindingStart
        | BindingEquals
        | BindingEnd
        | DefinitionStart
        | ValueName
        | ValueColon
        | ValueType
        | SignatureFile
        | NestedSignature

    let private closingKeywords =
        set [
            "done"
            "elif"
            "end"
            "of"
        ]

    let private bindingStartKeywords =
        set [
            "as"
            "finally"
            "internal"
            "private"
            "public"
            "to"
            "when"
        ]

    let private closesArrayOrAttribute (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1

        (isOperator "|" token
         || isOperator ">" token)
        && isDelimiter "]" next
        && next.Range.Start.Offset = token.Range.End.Offset

    let private unexpectedToken point (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1
        let text = tokenText token

        let symbol value range = Some($"symbol '{value}'", range)
        let keyword () = Some($"keyword '{text}'", token.Range)

        if closesArrayOrAttribute cursor then
            symbol $"{text}]" (span token.Range next.Range)
        elif
            isDelimiter ")" token
            || isDelimiter "]" token
            || isDelimiter "}" token
        then
            symbol text token.Range
        elif
            isKind LexicalTokenKind.Keyword token
            && closingKeywords.Contains text
        then
            keyword ()
        else
            match point with
            | RecoveryPoint.BindingStart when
                isOperator "=" token
                || isOperator ":" token
                || isOperator "." token
                || isOperator "|" token
                || isDelimiter ";" token
                ->
                symbol text token.Range
            | RecoveryPoint.BindingStart when
                isKind LexicalTokenKind.Keyword token
                && bindingStartKeywords.Contains text
                ->
                keyword ()
            | RecoveryPoint.DefinitionStart when
                isOperator "=" token
                || isOperator ":" token
                || isOperator "." token
                ->
                symbol text token.Range
            | RecoveryPoint.ValueColon when isIdentifier token -> Some("identifier", token.Range)
            | _ -> None

    let private reportUnexpected state point context =
        match unexpectedToken point state.Cursor with
        | Some(description, range) ->
            let message =
                match point with
                | RecoveryPoint.BindingStart -> $"Unexpected {description} in binding"
                | RecoveryPoint.BindingEquals ->
                    $"Unexpected {description} in binding. Expected '=' or other token."
                | RecoveryPoint.BindingEnd ->
                    $"Unexpected {description} in binding. Expected incomplete structured construct at or before this point or other token."
                | RecoveryPoint.DefinitionStart ->
                    $"Unexpected {description} in definition. Expected incomplete structured construct at or before this point or other token."
                | RecoveryPoint.ValueName ->
                    $"Unexpected {description} in value signature. Expected identifier, '(', '(*)' or other token."
                | RecoveryPoint.ValueColon ->
                    $"Unexpected {description} in value signature. Expected ':' or other token."
                | RecoveryPoint.ValueType -> $"Unexpected {description} in value signature"
                | RecoveryPoint.SignatureFile ->
                    $"Unexpected {description}. Expected incomplete structured construct at or before this point or other token."
                | RecoveryPoint.NestedSignature ->
                    $"Unexpected {description} in signature file. Expected incomplete structured construct at or before this point or other token."

            report state "FS0010" message range
            true
        | None ->
            reportUnsupported state state.Cursor.Current context
            false

    let private isOffside (context: SourcePosition) (token: LayoutToken) =
        token.Kind = LayoutTokenKind.SourceToken
        && token.Range.Start.Line > context.Line
        && token.Range.Start.Column
           <= context.Column

    let private skipUntil state (context: SourcePosition) =
        let cursor = state.Cursor
        let start = cursor.Current.Range
        let skipped = ImmutableArray.CreateBuilder<LexicalToken>()
        let mutable depth = 0
        let mutable stop = false

        while not stop do
            let token = cursor.Current

            match token.Kind with
            | _ when isEndOfFile token -> stop <- true
            | LayoutTokenKind.BeginBlock ->
                depth <- depth + 1

                cursor.Advance()
                |> ignore
            | LayoutTokenKind.EndBlock when depth > 0 ->
                depth <- depth - 1

                cursor.Advance()
                |> ignore
            | LayoutTokenKind.EndBlock
            | LayoutTokenKind.Separator when depth = 0 -> stop <- true
            | LayoutTokenKind.SourceToken when
                depth = 0
                && isOffside context token
                ->
                stop <- true
            | _ ->
                token.Token
                |> Option.iter skipped.Add

                cursor.Advance()
                |> ignore

        if skipped.Count = 0 then
            None
        else
            Some {
                Tokens = skipped.ToImmutable()
                Range = span start (emptyAt cursor.LastEnd)
            }

    let private identifier (token: LayoutToken) = {
        Text = tokenText token
        Range = token.Range
    }

    let private afterLastToken (cursor: Cursor) =
        let position =
            if isEndOfFile cursor.Current then
                let eof = cursor.Current.Range.Start
                // The Compatibility Oracle reports an incomplete construct at end of input at column 1 of the last line.
                {
                    eof with
                        Offset =
                            eof.Offset
                            - (eof.Column
                               - 1)
                        Column = 1
                }
            else
                // The Compatibility Oracle reports an incomplete construct one column after the last token.
                {
                    cursor.LastEnd with
                        Offset =
                            cursor.LastEnd.Offset
                            + 1
                        Column =
                            cursor.LastEnd.Column
                            + 1
                }

        emptyAt position

    let private reportIncomplete state context =
        report
            state
            "FS0010"
            $"Incomplete structured construct at or before this point in {context}"
            (afterLastToken state.Cursor)

    let private endsLine context (token: LayoutToken) =
        token.Kind = LayoutTokenKind.Separator
        || token.Kind = LayoutTokenKind.EndBlock
        || isEndOfFile token
        || isOffside context token

    let private longIdentifierWith state (onTrailingDot: LayoutToken -> unit) =
        let cursor = state.Cursor
        let parts = ImmutableArray.CreateBuilder<SyntaxIdentifier>()
        let mutable stop = false

        parts.Add(identifier (cursor.Advance()))

        while not stop
              && isOperator "." cursor.Current do
            let dot = cursor.Advance()

            if isIdentifier cursor.Current then
                parts.Add(identifier (cursor.Advance()))
            else
                onTrailingDot dot
                stop <- true

        let parts = parts.ToImmutable()

        {
            Parts = parts
            Range =
                span
                    parts[0].Range
                    parts[parts.Length
                          - 1]
                        .Range
        }

    let private longIdentifier state =
        longIdentifierWith
            state
            (fun _ -> reportUnsupported state state.Cursor.Current "a long identifier")

    let private constant (token: LayoutToken) =
        match sourceKind token with
        | Some LexicalTokenKind.NumericLiteral -> Some(SyntaxConstant.Numeric(tokenText token))
        | Some LexicalTokenKind.StringLiteral -> Some(SyntaxConstant.String(tokenText token))
        | Some LexicalTokenKind.CharacterLiteral -> Some(SyntaxConstant.Character(tokenText token))
        | Some LexicalTokenKind.Keyword when tokenText token = "true" ->
            Some(SyntaxConstant.Boolean true)
        | Some LexicalTokenKind.Keyword when tokenText token = "false" ->
            Some(SyntaxConstant.Boolean false)
        | _ -> None

    let private infixPrecedence (token: LayoutToken) =
        if not (isKind LexicalTokenKind.Operator token) then
            None
        else
            let text = tokenText token

            match text with
            | "->"
            | "<-"
            | ":="
            | ":>"
            | ":?"
            | ":?>"
            | "|"
            | "."
            | ".."
            | ":" -> None
            | "||" -> Some(1, false)
            | "&"
            | "&&" -> Some(2, false)
            | "::" -> Some(6, true)
            | "!=" -> Some(4, false)
            | _ when text.StartsWith("**", StringComparison.Ordinal) -> Some(9, true)
            | _ ->
                match text[0] with
                | '<'
                | '>'
                | '='
                | '|'
                | '&'
                | '$' -> Some(4, false)
                | '^'
                | '@' -> Some(5, true)
                | '+'
                | '-' -> Some(7, false)
                | '*'
                | '/'
                | '%' -> Some(8, false)
                | _ -> None

    let private canStartAtom (token: LayoutToken) =
        isIdentifier token
        || (constant token).IsSome
        || isDelimiter "(" token

    let rec private parseExpression state context =
        let cursor = state.Cursor
        let first = parseInfix state context 0

        if
            isDelimiter "," cursor.Current
            && not (isOffside context cursor.Current)
        then
            let items = ImmutableArray.CreateBuilder<SyntaxExpression>()
            items.Add first

            while isDelimiter "," cursor.Current
                  && not (isOffside context cursor.Current) do
                cursor.Advance()
                |> ignore

                items.Add(parseInfix state context 0)

            let items = items.ToImmutable()

            SyntaxExpression.Tuple(
                items,
                span
                    first.Range
                    items[items.Length
                          - 1]
                        .Range
            )
        else
            first

    and private parseInfix state context minimum =
        let cursor = state.Cursor
        let mutable left = parseApplication state context
        let mutable stop = false

        while not stop do
            match infixPrecedence cursor.Current with
            | Some(precedence, rightAssociative) when
                precedence
                >= minimum
                && not (isOffside context cursor.Current)
                && not (closesArrayOrAttribute cursor)
                ->
                let token = cursor.Current
                let next = cursor.Peek 1
                let text = tokenText token
                let followsWithoutSpace = token.Range.Start.Offset = cursor.LastEnd.Offset
                let precedesWithoutSpace = next.Range.Start.Offset = token.Range.End.Offset

                if
                    followsWithoutSpace
                    && text.StartsWith("<", StringComparison.Ordinal)
                then
                    reportUnsupported state token "a type application in an expression"
                elif
                    not followsWithoutSpace
                    && precedesWithoutSpace
                    && (text = "-"
                        || text = "+")
                then
                    reportUnsupported state token "a prefix operator application"

                let operator = identifier (cursor.Advance())

                let right =
                    parseInfix
                        state
                        context
                        (if rightAssociative then
                             precedence
                         else
                             precedence
                             + 1)

                left <- SyntaxExpression.Infix(operator, left, right, span left.Range right.Range)
            | _ -> stop <- true

        left

    and private parseApplication state context =
        let cursor = state.Cursor
        let mutable result = parseAtom state context

        while canStartAtom cursor.Current
              && not (isOffside context cursor.Current) do
            let argument = parseAtom state context

            result <-
                SyntaxExpression.Application(result, argument, span result.Range argument.Range)

        result

    and private parseAtom state context =
        let cursor = state.Cursor
        let token = cursor.Current

        match constant token with
        | Some value ->
            cursor.Advance()
            |> ignore

            SyntaxExpression.Constant(value, token.Range)
        | None when isIdentifier token ->
            longIdentifierWith
                state
                (fun dot ->
                    if endsLine context cursor.Current then
                        report state "FS0599" "Missing qualification after '.'" dot.Range
                    else
                        reportUnsupported state cursor.Current "a long identifier"
                )
            |> SyntaxExpression.Identifier
        | None when isDelimiter "(" token ->
            cursor.Advance()
            |> ignore

            if isDelimiter ")" cursor.Current then
                let close = cursor.Advance()
                SyntaxExpression.Constant(SyntaxConstant.Unit, span token.Range close.Range)
            else
                let inner = parseExpression state context

                if isDelimiter ")" cursor.Current then
                    let close = cursor.Advance()
                    SyntaxExpression.Parenthesized(inner, span token.Range close.Range)
                else
                    if not (reportedAt state cursor.Current) then
                        reportUnsupported state cursor.Current "a parenthesized expression"

                    SyntaxExpression.Parenthesized(inner, span token.Range (emptyAt cursor.LastEnd))
        | None ->
            reportUnsupported state token "an expression"

            SyntaxExpression.Missing {
                Expected = "expression"
                Range = emptyAt token.Range.Start
            }

    let private canStartPattern (token: LayoutToken) =
        isIdentifier token
        || (constant token).IsSome
        || isDelimiter "(" token

    let rec private parsePattern state context =
        let cursor = state.Cursor
        let first = parseAtomicPattern state context

        if isDelimiter "," cursor.Current then
            let items = ImmutableArray.CreateBuilder<SyntaxPattern>()
            items.Add first

            while isDelimiter "," cursor.Current do
                cursor.Advance()
                |> ignore

                items.Add(parseAtomicPattern state context)

            let items = items.ToImmutable()

            SyntaxPattern.Tuple(
                items,
                span
                    first.Range
                    items[items.Length
                          - 1]
                        .Range
            )
        else
            first

    and private parseAtomicPattern state context =
        let cursor = state.Cursor
        let token = cursor.Current

        match constant token with
        | Some value ->
            cursor.Advance()
            |> ignore

            SyntaxPattern.Constant(value, token.Range)
        | None when
            isKind LexicalTokenKind.Identifier token
            && tokenText token = "_"
            ->
            cursor.Advance()
            |> ignore

            SyntaxPattern.Wildcard token.Range
        | None when isIdentifier token -> SyntaxPattern.Named(identifier (cursor.Advance()))
        | None when isDelimiter "(" token ->
            cursor.Advance()
            |> ignore

            if isDelimiter ")" cursor.Current then
                let close = cursor.Advance()
                SyntaxPattern.Constant(SyntaxConstant.Unit, span token.Range close.Range)
            else
                let inner = parsePattern state context

                if isDelimiter ")" cursor.Current then
                    let close = cursor.Advance()
                    SyntaxPattern.Parenthesized(inner, span token.Range close.Range)
                else
                    if not (reportedAt state cursor.Current) then
                        reportUnsupported state cursor.Current "a parenthesized pattern"

                    SyntaxPattern.Parenthesized(inner, span token.Range (emptyAt cursor.LastEnd))
        | None ->
            reportUnsupported state token "a pattern"

            SyntaxPattern.Missing {
                Expected = "pattern"
                Range = emptyAt token.Range.Start
            }

    let private missingExpression (token: LayoutToken) =
        SyntaxExpression.Missing {
            Expected = "expression"
            Range = emptyAt token.Range.Start
        }

    let private missingPattern (token: LayoutToken) =
        SyntaxPattern.Missing {
            Expected = "pattern"
            Range = emptyAt token.Range.Start
        }

    let private endsBinding context (token: LayoutToken) =
        token.Kind = LayoutTokenKind.Separator
        || token.Kind = LayoutTokenKind.EndBlock
        || isEndOfFile token
        || isKeyword "and" token
        || isOffside context token

    let private parseAccessibility (cursor: Cursor) =
        let token = cursor.Current

        let kind =
            if isKeyword "public" token then
                Some SyntaxAccessibility.Public
            elif isKeyword "internal" token then
                Some SyntaxAccessibility.Internal
            elif isKeyword "private" token then
                Some SyntaxAccessibility.Private
            else
                None

        kind
        |> Option.map (fun kind ->
            cursor.Advance()
            |> ignore

            { Kind = kind; Range = token.Range }
        )

    let private isAttributeListStart (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1

        isDelimiter "[" token
        && isOperator "<" next
        && next.Range.Start.Offset = token.Range.End.Offset

    let private isAttributeListEnd (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1

        isOperator ">" token
        && isDelimiter "]" next
        && next.Range.Start.Offset = token.Range.End.Offset

    let private parseAttributeList state =
        let cursor = state.Cursor
        let openBracket = cursor.Advance()

        cursor.Advance()
        |> ignore

        let context = openBracket.Range.Start
        let attributes = ImmutableArray.CreateBuilder<SyntaxAttribute>()
        let mutable stop = false

        while not stop do
            let start = cursor.Current

            let target =
                if
                    (isIdentifier start
                     || isKind LexicalTokenKind.Keyword start)
                    && isOperator ":" (cursor.Peek 1)
                then
                    let target = identifier (cursor.Advance())

                    cursor.Advance()
                    |> ignore

                    Some target
                else
                    None

            if isIdentifier cursor.Current then
                let name = longIdentifier state

                let argument =
                    if
                        canStartAtom cursor.Current
                        && not (isOffside context cursor.Current)
                    then
                        Some(parseAtom state context)
                    else
                        None

                attributes.Add {
                    Target = target
                    Name = name
                    Argument = argument
                    Range = span start.Range (emptyAt cursor.LastEnd)
                }

                if isDelimiter ";" cursor.Current then
                    cursor.Advance()
                    |> ignore
                else
                    stop <- true
            else
                stop <- true

        if isAttributeListEnd cursor then
            cursor.Advance()
            |> ignore

            cursor.Advance()
            |> ignore
        else
            reportUnsupported state cursor.Current "an attribute list"

            skipUntil state context
            |> ignore

        {
            Attributes = attributes.ToImmutable()
            Range = span openBracket.Range (emptyAt cursor.LastEnd)
        }

    let private parseAttributeLists state =
        let cursor = state.Cursor
        let lists = ImmutableArray.CreateBuilder<SyntaxAttributeList>()

        while isAttributeListStart cursor do
            lists.Add(parseAttributeList state)

            while cursor.Current.Kind = LayoutTokenKind.Separator do
                cursor.Advance()
                |> ignore

        lists.ToImmutable()

    let private declarationStart
        (attributes: ImmutableArray<SyntaxAttributeList>)
        (keyword: LayoutToken)
        =
        if attributes.IsEmpty then
            keyword.Range
        else
            attributes[0].Range

    let private parseBinding state context attributes =
        let cursor = state.Cursor
        let reported = state.Diagnostics.Count
        let mutable recovered = false
        let mutable skipped = None

        let recover point =
            if state.Diagnostics.Count = reported then
                match point with
                | Some point ->
                    reportUnexpected state point "a binding"
                    |> ignore
                | None -> reportUnsupported state cursor.Current "a binding"

            recovered <- true
            skipped <- skipUntil state context

        let parseBody () =
            if canStartAtom cursor.Current then
                parseExpression state context
            else
                let missing = missingExpression cursor.Current
                recover (Some RecoveryPoint.BindingStart)
                missing

        let accessibility = parseAccessibility cursor

        let head =
            if canStartPattern cursor.Current then
                parsePattern state context
            else
                let missing = missingPattern cursor.Current
                recover (Some RecoveryPoint.BindingStart)
                missing

        let parameters = ImmutableArray.CreateBuilder<SyntaxPattern>()

        while not recovered
              && canStartPattern cursor.Current
              && not (isOffside context cursor.Current) do
            parameters.Add(parseAtomicPattern state context)

        let body =
            if recovered then
                missingExpression cursor.Current
            elif not (isOperator "=" cursor.Current) then
                let missing = missingExpression cursor.Current
                recover (Some RecoveryPoint.BindingEquals)
                missing
            else
                cursor.Advance()
                |> ignore

                if cursor.Current.Kind = LayoutTokenKind.BeginBlock then
                    cursor.Advance()
                    |> ignore

                    let body = parseBody ()

                    if
                        not recovered
                        && cursor.Current.Kind
                           <> LayoutTokenKind.EndBlock
                    then
                        recover None

                    if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                        cursor.Advance()
                        |> ignore

                    body
                elif endsBinding context cursor.Current then
                    let missing = missingExpression cursor.Current
                    recover None
                    missing
                else
                    parseBody ()

        if
            not recovered
            && not (endsBinding context cursor.Current)
        then
            recover (Some RecoveryPoint.BindingEnd)

        {
            Attributes = attributes
            Accessibility = accessibility
            Head = head
            Parameters = parameters.ToImmutable()
            Body = body
            Skipped = skipped
            Range =
                span
                    (accessibility
                     |> Option.map _.Range
                     |> Option.defaultValue head.Range)
                    (emptyAt cursor.LastEnd)
        }

    let private parseLet state attributes =
        let cursor = state.Cursor
        let letToken = cursor.Advance()
        let context = letToken.Range.Start

        let isRecursive =
            if isKeyword "rec" cursor.Current then
                cursor.Advance()
                |> ignore

                true
            else
                false

        let bindings = ImmutableArray.CreateBuilder<SyntaxBinding>()
        bindings.Add(parseBinding state context attributes)

        while isKeyword "and" cursor.Current do
            cursor.Advance()
            |> ignore

            bindings.Add(parseBinding state context ImmutableArray.Empty)

        ImplementationDeclaration.Let(
            isRecursive,
            bindings.ToImmutable(),
            span (declarationStart attributes letToken) (emptyAt cursor.LastEnd)
        )

    let private parseDo state attributes =
        let cursor = state.Cursor
        let doToken = cursor.Advance()
        let context = doToken.Range.Start
        let reported = state.Diagnostics.Count

        let body =
            if cursor.Current.Kind = LayoutTokenKind.BeginBlock then
                cursor.Advance()
                |> ignore

                let body =
                    if canStartAtom cursor.Current then
                        parseExpression state context
                    else
                        reportUnsupported state cursor.Current "a do declaration"
                        missingExpression cursor.Current

                if
                    cursor.Current.Kind
                    <> LayoutTokenKind.EndBlock
                then
                    if state.Diagnostics.Count = reported then
                        reportUnsupported state cursor.Current "a do declaration"

                    skipUntil state context
                    |> ignore

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                body
            elif
                canStartAtom cursor.Current
                && not (isOffside context cursor.Current)
            then
                parseExpression state context
            else
                reportUnsupported state cursor.Current "a do declaration"
                missingExpression cursor.Current

        ImplementationDeclaration.Do(
            attributes,
            body,
            span (declarationStart attributes doToken) (emptyAt cursor.LastEnd)
        )

    [<RequireQualifiedAccess>]
    type private TypeGap =
        | UnexpectedToken
        | EndAfterColonOrArrow
        | EndAfterStar

    let private missingType (token: LayoutToken) =
        SyntaxType.Missing {
            Expected = "type"
            Range = emptyAt token.Range.Start
        }

    let private isTypeVariable (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1

        isDelimiter "'" token
        && isIdentifier next
        && next.Range.Start.Offset = token.Range.End.Offset

    let private canStartType (cursor: Cursor) =
        isIdentifier cursor.Current
        || isDelimiter "(" cursor.Current
        || isTypeVariable cursor

    let private closesTypeArguments (token: LayoutToken) =
        isKind LexicalTokenKind.Operator token
        && tokenText token
           |> Seq.forall ((=) '>')

    let rec private parseType state context (onGap: TypeGap -> unit option) =
        let cursor = state.Cursor
        let argument = parseTupleType state context onGap

        if
            isOperator "->" cursor.Current
            && not (isOffside context cursor.Current)
        then
            cursor.Advance()
            |> ignore

            let result =
                parseTypeOperand
                    state
                    context
                    onGap
                    TypeGap.EndAfterColonOrArrow
                    (fun () -> parseType state context onGap)

            SyntaxType.Function(argument, result, span argument.Range result.Range)
        else
            argument

    and private parseTypeOperand state context onGap endGap parse =
        let cursor = state.Cursor
        let token = cursor.Current

        if
            canStartType cursor
            && not (isOffside context token)
        then
            parse ()
        else
            let missing = missingType token

            let gap =
                if endsLine context token then
                    endGap
                else
                    TypeGap.UnexpectedToken

            if (onGap gap).IsNone then
                reportUnsupported state token "a type"

            missing

    and private parseTupleType state context onGap =
        let cursor = state.Cursor
        let first = parseParameterType state context onGap

        if
            isOperator "*" cursor.Current
            && not (isOffside context cursor.Current)
        then
            let elements = ImmutableArray.CreateBuilder<SyntaxType>()
            elements.Add first

            while isOperator "*" cursor.Current
                  && not (isOffside context cursor.Current) do
                cursor.Advance()
                |> ignore

                elements.Add(
                    parseTypeOperand
                        state
                        context
                        onGap
                        TypeGap.EndAfterStar
                        (fun () -> parseParameterType state context onGap)
                )

            let elements = elements.ToImmutable()

            SyntaxType.Tuple(
                elements,
                span
                    first.Range
                    elements[elements.Length
                             - 1]
                        .Range
            )
        else
            first

    and private parseParameterType state context onGap =
        let cursor = state.Cursor

        if
            isIdentifier cursor.Current
            && isOperator ":" (cursor.Peek 1)
        then
            let name = identifier (cursor.Advance())

            cursor.Advance()
            |> ignore

            let parameterType =
                parseTypeOperand
                    state
                    context
                    onGap
                    TypeGap.EndAfterColonOrArrow
                    (fun () -> parseApplicationType state context)

            SyntaxType.SignatureParameter(name, parameterType, span name.Range parameterType.Range)
        else
            parseApplicationType state context

    and private parseApplicationType state context =
        let cursor = state.Cursor
        let mutable result = parseAtomicType state context

        while isIdentifier cursor.Current
              && not (isOffside context cursor.Current) do
            let typeConstructor = SyntaxType.LongIdentifier(longIdentifier state)

            result <-
                SyntaxType.Application(
                    typeConstructor,
                    ImmutableArray.Create result,
                    true,
                    span result.Range typeConstructor.Range
                )

        result

    and private parseAtomicType state context =
        let cursor = state.Cursor
        let token = cursor.Current

        let nested () =
            parseTypeOperand
                state
                context
                (fun _ -> None)
                TypeGap.UnexpectedToken
                (fun () -> parseType state context (fun _ -> None))

        if isTypeVariable cursor then
            cursor.Advance()
            |> ignore

            let name = cursor.Advance()

            SyntaxType.Variable {
                Text =
                    "'"
                    + tokenText name
                Range = span token.Range name.Range
            }
        elif isDelimiter "(" token then
            cursor.Advance()
            |> ignore

            let inner = nested ()

            if isDelimiter ")" cursor.Current then
                let close = cursor.Advance()
                SyntaxType.Parenthesized(inner, span token.Range close.Range)
            else
                if not (reportedAt state cursor.Current) then
                    reportUnsupported state cursor.Current "a parenthesized type"

                SyntaxType.Parenthesized(inner, span token.Range (emptyAt cursor.LastEnd))
        else
            let name = SyntaxType.LongIdentifier(longIdentifier state)

            if isOperator "<" cursor.Current then
                cursor.Advance()
                |> ignore

                let arguments = ImmutableArray.CreateBuilder<SyntaxType>()
                arguments.Add(nested ())

                while isDelimiter "," cursor.Current do
                    cursor.Advance()
                    |> ignore

                    arguments.Add(nested ())

                if closesTypeArguments cursor.Current then
                    let close = cursor.AdvanceFirstCharacter()

                    SyntaxType.Application(
                        name,
                        arguments.ToImmutable(),
                        false,
                        span name.Range close.Range
                    )
                else
                    if not (reportedAt state cursor.Current) then
                        reportUnsupported state cursor.Current "type arguments"

                    SyntaxType.Application(
                        name,
                        arguments.ToImmutable(),
                        false,
                        span name.Range (emptyAt cursor.LastEnd)
                    )
            else
                name

    [<RequireQualifiedAccess>]
    type private ListRecovery =
        | Continues
        | Discards

    let private parseVal state nested attributes =
        let cursor = state.Cursor
        let valToken = cursor.Advance()
        let context = valToken.Range.Start
        let reported = state.Diagnostics.Count
        let mutable recovered = false
        let mutable skipped = None

        let recover point =
            if state.Diagnostics.Count = reported then
                reportUnexpected state point "a value signature"
                |> ignore

            recovered <- true
            skipped <- skipUntil state context

        let onGap gap =
            match gap with
            | TypeGap.UnexpectedToken -> Some(recover RecoveryPoint.ValueType)
            | TypeGap.EndAfterColonOrArrow -> Some(reportIncomplete state "value signature")
            | TypeGap.EndAfterStar -> None

        let accessibility = parseAccessibility cursor

        let name =
            if isIdentifier cursor.Current then
                Some(identifier (cursor.Advance()))
            else
                recover RecoveryPoint.ValueName
                None

        let valueType =
            if recovered then
                missingType cursor.Current
            elif isOperator ":" cursor.Current then
                cursor.Advance()
                |> ignore

                parseTypeOperand
                    state
                    context
                    onGap
                    TypeGap.EndAfterColonOrArrow
                    (fun () -> parseType state context onGap)
            else
                let missing = missingType cursor.Current
                recover RecoveryPoint.ValueColon
                missing

        if
            not recovered
            && not (endsLine context cursor.Current)
        then
            recover (
                if nested then
                    RecoveryPoint.NestedSignature
                else
                    RecoveryPoint.SignatureFile
            )

        let value =
            SignatureDeclaration.Val {
                Attributes = attributes
                Accessibility = accessibility
                Name = name
                Type = valueType
                Skipped = skipped
                Range = span (declarationStart attributes valToken) (emptyAt cursor.LastEnd)
            }

        Some value,
        (if recovered then
             ListRecovery.Discards
         else
             ListRecovery.Continues)

    let private parseOpen state =
        let cursor = state.Cursor
        let openToken = cursor.Advance()
        let context = openToken.Range.Start

        if
            isIdentifier cursor.Current
            && not (isOffside context cursor.Current)
        then
            let name =
                longIdentifierWith
                    state
                    (fun _ ->
                        if
                            cursor.Current.Kind = LayoutTokenKind.Separator
                            || (cursor.Current.Kind = LayoutTokenKind.SourceToken
                                && not (isEndOfFile cursor.Current)
                                && isOffside context cursor.Current)
                        then
                            reportIncomplete state "open declaration"
                        else
                            reportUnsupported state cursor.Current "an open declaration"
                    )

            Some(name, span openToken.Range name.Range)
        elif endsLine context cursor.Current then
            report
                state
                "FS0010"
                "Incomplete structured construct at or before this point in open declaration. Expected identifier, 'global', 'type' or other token."
                (afterLastToken cursor)

            None
        else
            reportUnsupported state cursor.Current "an open declaration"
            None

    type private DeclarationRules<'Declaration> = {
        Parse:
            ParserState
                -> bool
                -> ImmutableArray<SyntaxAttributeList>
                -> LayoutToken
                -> ('Declaration option * ListRecovery) option
        ContinuesRecovery: LayoutToken -> bool
        StartPoint: bool -> RecoveryPoint
        Open: LongIdentifier * SourceRange -> 'Declaration
        NestedModule:
            SyntaxIdentifier -> ImmutableArray<'Declaration> -> SourceRange -> 'Declaration
        Skipped: SkippedSyntax -> 'Declaration
    }

    let private isDeclarationListEnd (token: LayoutToken) =
        token.Kind = LayoutTokenKind.EndBlock
        || isEndOfFile token
        || isKeyword "namespace" token

    let rec private parseDeclarations
        state
        (rules: DeclarationRules<'Declaration>)
        nested
        : ImmutableArray<'Declaration> * ImmutableArray<'Declaration> =
        let cursor = state.Cursor
        let declarations = ImmutableArray.CreateBuilder<'Declaration>()
        let discarded = ImmutableArray.CreateBuilder<'Declaration>()
        let mutable suppressFrom = None
        let mutable interrupted = false
        let mutable stop = false

        while not stop do
            let token = cursor.Current

            if token.Kind = LayoutTokenKind.Separator then
                cursor.Advance()
                |> ignore
            elif isDeclarationListEnd token then
                stop <- true
            else
                let target = if suppressFrom.IsSome then discarded else declarations

                let reported = state.Diagnostics.Count
                let attributes = parseAttributeLists state
                let token = cursor.Current

                match suppressFrom with
                | Some _ when
                    not interrupted
                    && not (rules.ContinuesRecovery token)
                    ->
                    reportUnsupported state token "a declaration after syntax recovery"
                    interrupted <- true
                    suppressFrom <- Some state.Diagnostics.Count
                | _ -> ()

                let parsed, recovery =
                    if
                        not attributes.IsEmpty
                        && (isKeyword "open" token
                            || isKeyword "module" token)
                    then
                        reportUnsupported state token "an attributed declaration"
                        None, ListRecovery.Continues
                    elif isKeyword "open" token then
                        parseOpen state
                        |> Option.map rules.Open,
                        ListRecovery.Continues
                    elif isKeyword "module" token then
                        parseNestedModule state rules, ListRecovery.Continues
                    else
                        match rules.Parse state nested attributes token with
                        | Some result -> result
                        | None ->
                            if reportedAt state token then
                                None, ListRecovery.Continues
                            elif
                                reportUnexpected
                                    state
                                    (rules.StartPoint nested)
                                    "a module or namespace declaration"
                            then
                                None, ListRecovery.Discards
                            else
                                None, ListRecovery.Continues

                parsed
                |> Option.iter target.Add

                let next = cursor.Current

                if
                    not (isDeclarationListEnd next)
                    && next.Kind
                       <> LayoutTokenKind.Separator
                    && not (isOffside token.Range.Start next)
                then
                    if
                        state.Diagnostics.Count = reported
                        && not (reportedAt state next)
                    then
                        reportUnsupported state next "a module or namespace declaration"

                    skipUntil state token.Range.Start
                    |> Option.iter (
                        rules.Skipped
                        >> target.Add
                    )

                match suppressFrom with
                | Some first ->
                    let kept =
                        state.Diagnostics
                        |> Seq.skip first
                        |> Seq.filter (fun diagnostic -> diagnostic.Code = unsupportedCode)
                        |> Seq.toArray

                    state.Diagnostics.RemoveRange(
                        first,
                        state.Diagnostics.Count
                        - first
                    )

                    state.Diagnostics.AddRange kept
                    suppressFrom <- Some state.Diagnostics.Count
                | None when
                    recovery = ListRecovery.Discards
                    && not nested
                    ->
                    // The Compatibility Oracle discards later declarations that continue this recovery and reports no diagnostic for them.
                    suppressFrom <- Some state.Diagnostics.Count
                | None -> ()

        declarations.ToImmutable(), discarded.ToImmutable()

    and private parseNestedModule
        state
        (rules: DeclarationRules<'Declaration>)
        : 'Declaration option =
        let cursor = state.Cursor
        let moduleToken = cursor.Advance()

        if not (isIdentifier cursor.Current) then
            reportUnsupported state cursor.Current "a module declaration"
            None
        else
            let name = identifier (cursor.Advance())

            if
                isOperator "=" cursor.Current
                && (cursor.Peek 1).Kind = LayoutTokenKind.BeginBlock
            then
                cursor.Advance()
                |> ignore

                cursor.Advance()
                |> ignore

                let declarations, _ = parseDeclarations state rules true
                let range = span moduleToken.Range (emptyAt cursor.LastEnd)

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                Some(rules.NestedModule name declarations range)
            else
                reportUnsupported state cursor.Current "a module declaration"
                None

    let private parseRoots state rules =
        let cursor = state.Cursor
        let roots = ImmutableArray.CreateBuilder<ModuleOrNamespaceSyntax<_>>()

        let rootDeclarations () =
            if cursor.Current.Kind = LayoutTokenKind.BeginBlock then
                cursor.Advance()
                |> ignore

                let declarations = parseDeclarations state rules false

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                declarations
            else
                parseDeclarations state rules false

        let root kind (start: SourceRange) (declarations, discarded) = {
            Kind = kind
            Declarations = declarations
            DiscardedByRecovery = discarded
            Range = span start (emptyAt cursor.LastEnd)
        }

        if isKeyword "namespace" cursor.Current then
            while isKeyword "namespace" cursor.Current do
                let namespaceToken = cursor.Advance()

                let name =
                    if isIdentifier cursor.Current then
                        Some(longIdentifier state)
                    else
                        reportUnsupported state cursor.Current "a namespace declaration"
                        None

                let declarations = rootDeclarations ()

                roots.Add(
                    root (ModuleOrNamespaceKind.Namespace name) namespaceToken.Range declarations
                )
        else
            let first = cursor.Current

            let header =
                let mutable offset = 1

                while isIdentifier (cursor.Peek offset)
                      || isOperator "." (cursor.Peek offset) do
                    offset <- offset + 1

                if
                    isKeyword "module" first
                    && isIdentifier (cursor.Peek 1)
                    && not (isOperator "=" (cursor.Peek offset))
                then
                    let moduleToken = cursor.Advance()
                    Some(moduleToken, longIdentifier state)
                else
                    None

            match header with
            | Some(moduleToken, name) ->
                let declarations = rootDeclarations ()

                roots.Add(
                    root (ModuleOrNamespaceKind.NamedModule name) moduleToken.Range declarations
                )
            | None ->
                let declarations = rootDeclarations ()
                roots.Add(root ModuleOrNamespaceKind.AnonymousModule first.Range declarations)

        if not (isEndOfFile cursor.Current) then
            reportUnsupported state cursor.Current "a module or namespace declaration"

            while not (isEndOfFile cursor.Current) do
                cursor.Advance()
                |> ignore

        roots.ToImmutable()

    let private implementationRules = {
        Parse =
            fun state _ attributes token ->
                if isKeyword "let" token then
                    Some(Some(parseLet state attributes), ListRecovery.Continues)
                elif isKeyword "do" token then
                    Some(Some(parseDo state attributes), ListRecovery.Continues)
                else
                    None
        ContinuesRecovery = fun _ -> true
        StartPoint = fun _ -> RecoveryPoint.DefinitionStart
        Open = ImplementationDeclaration.Open
        NestedModule =
            fun name declarations range ->
                ImplementationDeclaration.NestedModule(name, declarations, range)
        Skipped = ImplementationDeclaration.Skipped
    }

    let private signatureRules = {
        Parse =
            fun state nested attributes token ->
                if isKeyword "val" token then
                    Some(parseVal state nested attributes)
                else
                    None
        ContinuesRecovery =
            fun token ->
                isKeyword "val" token
                || isKeyword "open" token
        StartPoint =
            fun nested ->
                if nested then
                    RecoveryPoint.NestedSignature
                else
                    RecoveryPoint.SignatureFile
        Open = SignatureDeclaration.Open
        NestedModule =
            fun name declarations range ->
                SignatureDeclaration.NestedModule(name, declarations, range)
        Skipped = SignatureDeclaration.Skipped
    }

    let private start (document: LexicalDocument) = {
        Cursor = Cursor(document.LayoutTokens)
        Diagnostics = ResizeArray()
        ReportedStarts = HashSet()
    }

    let parseImplementationFile (document: LexicalDocument) : ImplementationFileParseResult =
        let state = start document
        let contents = parseRoots state implementationRules

        {
            File = {
                StableId = document.StableId
                LogicalPath = document.LogicalPath
                Contents = contents
            }
            Diagnostics = ImmutableArray.CreateRange state.Diagnostics
        }

    let parseSignatureFile (document: LexicalDocument) : SignatureFileParseResult =
        let state = start document
        let contents = parseRoots state signatureRules

        {
            File = {
                StableId = document.StableId
                LogicalPath = document.LogicalPath
                Contents = contents
            }
            Diagnostics = ImmutableArray.CreateRange state.Diagnostics
        }
