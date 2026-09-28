namespace FSharp2.Compiler

open System
open System.Collections.Immutable

module internal Parser =
    type private Cursor(tokens: ImmutableArray<LayoutToken>) =
        let mutable index = 0
        let mutable lastEnd = tokens[0].Range.Start

        member _.Current = tokens[index]

        member _.LastEnd = lastEnd

        member _.Peek offset =
            tokens[min
                       (index
                        + offset)
                       (tokens.Length
                        - 1)]

        member _.Advance() =
            let token = tokens[index]

            if token.Kind = LayoutTokenKind.SourceToken then
                lastEnd <- token.Range.End

            if index + 1 < tokens.Length then
                index <- index + 1

            token

    type private ParserState = {
        Cursor: Cursor
        Diagnostics: ResizeArray<SyntaxDiagnostic>
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

    let private span (start: SourceRange) (finish: SourceRange) = {
        Start = start.Start
        End = finish.End
    }

    let private emptyAt position = { Start = position; End = position }

    let private report state code message range =
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

    let private reportUnsupported state (token: LayoutToken) context =
        report
            state
            "FSC2P1001"
            $"The syntax parser does not support {describe token} in {context}."
            token.Range

    [<RequireQualifiedAccess>]
    type private RecoveryPoint =
        | BindingStart
        | BindingEquals
        | BindingEnd
        | DefinitionStart

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

    let private unexpectedToken point (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1
        let text = tokenText token

        let closesArrayOrAttribute =
            (isOperator "|" token
             || isOperator ">" token)
            && isDelimiter "]" next
            && next.Range.Start.Offset = token.Range.End.Offset

        let symbol value range = Some($"symbol '{value}'", range)
        let keyword () = Some($"keyword '{text}'", token.Range)

        if closesArrayOrAttribute then
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

            report state "FS0010" message range
        | None -> reportUnsupported state state.Cursor.Current context

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

    let private isIdentifier (token: LayoutToken) =
        isKind LexicalTokenKind.Identifier token
        || isKind LexicalTokenKind.EscapedIdentifier token

    let private longIdentifier (cursor: Cursor) =
        let parts = ImmutableArray.CreateBuilder<SyntaxIdentifier>()

        parts.Add(identifier (cursor.Advance()))

        while isOperator "." cursor.Current do
            cursor.Advance()
            |> ignore

            if isIdentifier cursor.Current then
                parts.Add(identifier (cursor.Advance()))

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
                ->
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
        | None when isIdentifier token -> SyntaxExpression.Identifier(longIdentifier cursor)
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
                    reportUnsupported state cursor.Current "a parenthesized expression"

                    SyntaxExpression.Parenthesized(inner, span token.Range (emptyAt cursor.LastEnd))
        | None ->
            reportUnsupported state token "an expression"

            SyntaxExpression.Missing {
                Expected = "expression"
                Range = emptyAt token.Range.Start
            }

    let private canStartPattern (token: LayoutToken) =
        isKind LexicalTokenKind.Identifier token
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
        | None when isKind LexicalTokenKind.Identifier token ->
            SyntaxPattern.Named(identifier (cursor.Advance()))
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

    let private parseBinding state context =
        let cursor = state.Cursor
        let reported = state.Diagnostics.Count
        let mutable recovered = false
        let mutable skipped = None

        let recover point =
            if state.Diagnostics.Count = reported then
                match point with
                | Some point -> reportUnexpected state point "a binding"
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

        let accessibility =
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

    let private parseLet state =
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
        bindings.Add(parseBinding state context)

        while isKeyword "and" cursor.Current do
            cursor.Advance()
            |> ignore

            bindings.Add(parseBinding state context)

        ImplementationDeclaration.Let(
            isRecursive,
            bindings.ToImmutable(),
            span letToken.Range (emptyAt cursor.LastEnd)
        )

    let private parseOpen state =
        let cursor = state.Cursor
        let openToken = cursor.Advance()

        if isIdentifier cursor.Current then
            let name = longIdentifier cursor
            Some(ImplementationDeclaration.Open(name, span openToken.Range name.Range))
        else
            reportUnsupported state cursor.Current "an open declaration"
            None

    let private isDeclarationListEnd (token: LayoutToken) =
        token.Kind = LayoutTokenKind.EndBlock
        || isEndOfFile token
        || isKeyword "namespace" token

    let rec private parseDeclarations state =
        let cursor = state.Cursor
        let declarations = ImmutableArray.CreateBuilder<ImplementationDeclaration>()
        let mutable stop = false

        while not stop do
            let token = cursor.Current

            if token.Kind = LayoutTokenKind.Separator then
                cursor.Advance()
                |> ignore
            elif isDeclarationListEnd token then
                stop <- true
            else
                let reported = state.Diagnostics.Count

                let parsed =
                    if isKeyword "open" token then
                        parseOpen state
                    elif isKeyword "let" token then
                        Some(parseLet state)
                    elif isKeyword "module" token then
                        parseNestedModule state
                    else
                        reportUnexpected
                            state
                            RecoveryPoint.DefinitionStart
                            "a module or namespace declaration"

                        None

                parsed
                |> Option.iter declarations.Add

                let next = cursor.Current

                if
                    not (isDeclarationListEnd next)
                    && next.Kind
                       <> LayoutTokenKind.Separator
                    && not (isOffside token.Range.Start next)
                then
                    if state.Diagnostics.Count = reported then
                        reportUnsupported state next "a module or namespace declaration"

                    skipUntil state token.Range.Start
                    |> Option.iter (
                        ImplementationDeclaration.Skipped
                        >> declarations.Add
                    )

        declarations.ToImmutable()

    and private parseNestedModule state =
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

                let declarations = parseDeclarations state
                let range = span moduleToken.Range (emptyAt cursor.LastEnd)

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                Some(ImplementationDeclaration.NestedModule(name, declarations, range))
            else
                reportUnsupported state cursor.Current "a module declaration"
                None

    let private parseRoots state =
        let cursor = state.Cursor
        let roots = ImmutableArray.CreateBuilder<ModuleOrNamespaceSyntax>()

        let root kind name (start: SourceRange) declarations = {
            Kind = kind
            Name = name
            Declarations = declarations
            Range = span start (emptyAt cursor.LastEnd)
        }

        if isKeyword "namespace" cursor.Current then
            while isKeyword "namespace" cursor.Current do
                let namespaceToken = cursor.Advance()

                let name =
                    if isIdentifier cursor.Current then
                        Some(longIdentifier cursor)
                    else
                        reportUnsupported state cursor.Current "a namespace declaration"
                        None

                let declarations = parseDeclarations state

                roots.Add(
                    root ModuleOrNamespaceKind.Namespace name namespaceToken.Range declarations
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
                    Some(moduleToken, longIdentifier cursor)
                else
                    None

            match header with
            | Some(moduleToken, name) ->
                let declarations = parseDeclarations state

                roots.Add(
                    root
                        ModuleOrNamespaceKind.NamedModule
                        (Some name)
                        moduleToken.Range
                        declarations
                )
            | None ->
                let declarations = parseDeclarations state

                roots.Add(root ModuleOrNamespaceKind.AnonymousModule None first.Range declarations)

        roots.ToImmutable()

    let parseImplementationFile (document: LexicalDocument) =
        let state = {
            Cursor = Cursor(document.LayoutTokens)
            Diagnostics = ResizeArray()
        }

        let contents = parseRoots state

        {
            File = {
                StableId = document.StableId
                LogicalPath = document.LogicalPath
                Contents = contents
            }
            Diagnostics = ImmutableArray.CreateRange state.Diagnostics
        }
