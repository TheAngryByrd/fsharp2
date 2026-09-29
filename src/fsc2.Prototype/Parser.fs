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

        member _.EndBefore offset =
            let mutable position =
                min
                    index
                    (tokens.Length
                     - 1)

            let mutable found = None

            while found.IsNone
                  && position
                     >= 0 do
                let token = tokens[position]

                if
                    token.Kind = LayoutTokenKind.SourceToken
                    && token.Range.End.Offset
                       <= offset
                then
                    found <- Some token.Range.End

                position <-
                    position
                    - 1

            found

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

    [<RequireQualifiedAccess>]
    type private DeclarationAfterRecovery =
        | Discarded of rootReportStart: int option
        | Unmodeled of reportsAtEnd: bool
        | DiscardedIfValueOrOpen
        | ReportedAtRoot of recoveryDepth: int
        | IncompleteAtNext
        | SkippedInSignatureModule of recoveryDepth: int * reportsAtEnd: bool

    [<RequireQualifiedAccess>]
    type private Recovery =
        | Parsing
        | Suppressing of suppressFrom: int * next: DeclarationAfterRecovery
        | Interrupted of suppressFrom: int

    [<RequireQualifiedAccess>]
    type private DiscardedDeclaration =
        | KeepsRootReport
        | MakesNextIncomplete
        | Unmodeled

    // The Compatibility Oracle loses the signature module header for the rest of the file after a nested recovery.
    [<RequireQualifiedAccess>]
    type private SignatureHeader =
        | Kept
        | Lost
        | LostAtEnd of SourceRange

    type private ParserState = {
        Cursor: Cursor
        Diagnostics: ResizeArray<SyntaxDiagnostic>
        ReportedStarts: HashSet<int>
        Language: LanguageVersionIdentity
        mutable Recovery: Recovery
        mutable InAnonymousRoot: bool
        mutable Depth: int
        mutable SignatureHeader: SignatureHeader
    }

    [<RequireQualifiedAccess>]
    type private DeclarationList =
        | ModuleRoot
        | NamespaceRoot
        | AnonymousRoot
        | NestedModule

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
            Severity = DiagnosticSeverity.Error
            Code = code
            Message = message
            Range = range
        }

    let private reportWarning state code message (range: SourceRange) =
        state.Diagnostics.Add {
            Severity = DiagnosticSeverity.Warning
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

    let private featureGateCode = "FS3350"

    let private successiveArgumentsCode = "FS0597"

    let private useInModuleCode = "FS0524"

    let private letAndCode = "FS0576"

    let private offsideCode = "FS0058"

    let private isRecoveryCode code =
        code
        <> featureGateCode
        && code
           <> successiveArgumentsCode
        && code
           <> useInModuleCode
        && code
           <> letAndCode
        && code
           <> offsideCode

    let private reportedAt state (token: LayoutToken) =
        state.ReportedStarts.Contains token.Range.Start.Offset

    let private reportedSince state count =
        seq {
            count .. state.Diagnostics.Count
                     - 1
        }
        |> Seq.exists (fun index ->
            state.Diagnostics[index].Code
            |> isRecoveryCode
        )

    let private keepFirstDiagnosticSince state count =
        let firstRecovery =
            seq {
                count .. state.Diagnostics.Count
                         - 1
            }
            |> Seq.tryFind (fun index ->
                state.Diagnostics[index].Code
                |> isRecoveryCode
            )

        match firstRecovery with
        | Some first when state.Diagnostics.Count > first + 1 ->
            let later =
                state.Diagnostics
                |> Seq.skip (first + 1)
                |> Seq.filter (fun diagnostic ->
                    diagnostic.Code = unsupportedCode
                    || diagnostic.Code = offsideCode
                )
                |> Seq.toArray

            state.Diagnostics.RemoveRange(
                first + 1,
                state.Diagnostics.Count
                - first
                - 1
            )

            state.Diagnostics.AddRange later
        | _ -> ()

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
        | ClauseArrow
        | ClauseResult
        | LambdaArrow
        | MatchWith
        | RecordFieldValue
        | FieldType
        | FieldColon
        | UnionCaseField
        | UnionCaseName
        | TypeEquals
        | FirstUnionCaseField
        | LambdaStart
        | NamespaceFile
        | AnonymousFile
        | AnonymousSignature
        | NestedFirstDefinition
        | NestedFirstSignature

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

    let private signatureStartKeywords =
        HashSet [
            "do"
            "if"
            "match"
            "fun"
            "inline"
            "and"
        ]

    let private unexpectedToken point (cursor: Cursor) =
        let token = cursor.Current

        let signatureStart =
            match point with
            | RecoveryPoint.SignatureFile
            | RecoveryPoint.NestedSignature
            | RecoveryPoint.NestedFirstSignature -> true
            | _ -> false

        let next = cursor.Peek 1
        let text = tokenText token

        let symbol value range = Some($"symbol '{value}'", range)
        let keyword () = Some($"keyword '{text}'", token.Range)

        let excluded =
            match point with
            | RecoveryPoint.RecordFieldValue
            | RecoveryPoint.FieldType
            | RecoveryPoint.FieldColon -> isDelimiter "}" token
            | RecoveryPoint.UnionCaseName -> isKeyword "of" token
            | RecoveryPoint.TypeEquals ->
                isDelimiter ")" token
                || isDelimiter "}" token
                || isKeyword "end" token
            | _ -> false

        if excluded then
            None
        elif closesArrayOrAttribute cursor then
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
            | RecoveryPoint.LambdaStart when isOperator "->" token -> symbol text token.Range
            | RecoveryPoint.NestedFirstDefinition when isOperator "=" token ->
                symbol text token.Range
            | _ when
                signatureStart
                && (isKeyword "let" token
                    || isKeyword "use" token)
                && isOperator "!" next
                && next.Range.Start.Offset = token.Range.End.Offset
                ->
                Some("binder keyword", span token.Range next.Range)
            | _ when
                signatureStart
                && (isKeyword "let" token
                    || isKeyword "use" token)
                ->
                Some("keyword 'let' or 'use'", token.Range)
            | _ when
                signatureStart
                && isKind LexicalTokenKind.Keyword token
                && signatureStartKeywords.Contains text
                ->
                keyword ()
            | _ when
                signatureStart
                && text = "_"
                ->
                symbol text token.Range
            | RecoveryPoint.SignatureFile when isKind LexicalTokenKind.Identifier token ->
                Some("identifier", token.Range)
            | _ when
                signatureStart
                && isKind LexicalTokenKind.NumericLiteral token
                && Seq.forall Char.IsAsciiDigit text
                ->
                Some("integer literal", token.Range)
            | _ when
                signatureStart
                && isKind LexicalTokenKind.StringLiteral token
                && text.StartsWith("\"", StringComparison.Ordinal)
                && not (text.StartsWith("\"\"\"", StringComparison.Ordinal))
                ->
                Some("string literal", token.Range)
            | _ when
                signatureStart
                && (isDelimiter "(" token
                    || isDelimiter "[" token
                    || isOperator "=" token
                    || isOperator "|" token)
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
                | RecoveryPoint.ValueName ->
                    $"Unexpected {description} in value signature. Expected identifier, '(', '(*)' or other token."
                | RecoveryPoint.ValueColon ->
                    $"Unexpected {description} in value signature. Expected ':' or other token."
                | RecoveryPoint.ValueType -> $"Unexpected {description} in value signature"
                | RecoveryPoint.SignatureFile ->
                    $"Unexpected {description}. Expected incomplete structured construct at or before this point or other token."
                | RecoveryPoint.NestedSignature ->
                    $"Unexpected {description} in signature file. Expected incomplete structured construct at or before this point or other token."
                | RecoveryPoint.ClauseArrow ->
                    $"Unexpected {description} in pattern matching. Expected '->' or other token."
                | RecoveryPoint.ClauseResult -> $"Unexpected {description} in pattern matching"
                | RecoveryPoint.LambdaArrow ->
                    $"Unexpected {description} in lambda expression. Expected '->' or other token."
                | RecoveryPoint.MatchWith ->
                    $"Unexpected {description} in expression. Expected 'with' or other token."
                | RecoveryPoint.RecordFieldValue -> $"Unexpected {description} in expression"
                | RecoveryPoint.FieldType -> $"Unexpected {description} in field declaration"
                | RecoveryPoint.FieldColon ->
                    $"Unexpected {description} in field declaration. Expected ':' or other token."
                | RecoveryPoint.UnionCaseField
                | RecoveryPoint.UnionCaseName -> $"Unexpected {description} in union case"
                | RecoveryPoint.TypeEquals ->
                    $"Unexpected {description} in type definition. Expected '=' or other token."
                | RecoveryPoint.FirstUnionCaseField ->
                    $"Unexpected {description} in type definition"
                | RecoveryPoint.LambdaStart -> $"Unexpected {description} in lambda expression"
                | RecoveryPoint.NamespaceFile ->
                    $"Unexpected {description} in implementation file. Expected incomplete structured construct at or before this point or other token."
                | RecoveryPoint.AnonymousFile -> $"Unexpected {description} in implementation file"
                | RecoveryPoint.AnonymousSignature -> $"Unexpected {description} in signature file"
                | RecoveryPoint.NestedFirstDefinition -> $"Unexpected {description} in definition"
                | RecoveryPoint.NestedFirstSignature ->
                    $"Unexpected {description} in signature file"

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
        let rec nextSourceToken offset closesBlock =
            let token = cursor.Peek offset

            match token.Kind with
            | LayoutTokenKind.SourceToken -> token, closesBlock
            | LayoutTokenKind.EndBlock -> nextSourceToken (offset + 1) true
            | LayoutTokenKind.BeginBlock
            | LayoutTokenKind.Separator -> nextSourceToken (offset + 1) closesBlock

        let next, closesBlock = nextSourceToken 0 false

        if isEndOfFile next then
            let eof = next.Range.Start
            // The Compatibility Oracle reports an incomplete construct at end of input from column 1 of the last line.
            {
                Start = {
                    eof with
                        Offset =
                            eof.Offset
                            - (eof.Column
                               - 1)
                        Column = 1
                }
                End = eof
            }
        elif closesBlock then
            next.Range
        else
            // The Compatibility Oracle starts an incomplete construct one column after the last token.
            {
                Start = {
                    cursor.LastEnd with
                        Offset =
                            cursor.LastEnd.Offset
                            + 1
                        Column =
                            cursor.LastEnd.Column
                            + 1
                }
                End = next.Range.Start
            }

    let private nextSourceToken (cursor: Cursor) =
        let rec next offset =
            let token = cursor.Peek offset

            match token.Kind with
            | LayoutTokenKind.SourceToken -> token
            | LayoutTokenKind.EndBlock
            | LayoutTokenKind.BeginBlock
            | LayoutTokenKind.Separator -> next (offset + 1)

        next 0

    let private nextTokenOrEndRange (cursor: Cursor) =
        let token = nextSourceToken cursor

        if isEndOfFile token then
            let eof = token.Range.Start

            {
                Start = {
                    eof with
                        Offset =
                            eof.Offset
                            - (eof.Column
                               - 1)
                        Column = 1
                }
                End = eof
            }
        else
            token.Range

    let private reportOffside state (context: SourcePosition) range =
        report
            state
            offsideCode
            $"Unexpected syntax or possible incorrect indentation: this token is offside of context started at position ({context.Line}:{context.Column}). Try indenting this further.\nTo continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7."
            range

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

            if
                isIdentifier cursor.Current
                && tokenText cursor.Current
                   <> "_"
            then
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
              && not (isOffside context cursor.Current)
              && not (isOperator ":" (cursor.Peek 1)) do
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

    let private canStartAtom (token: LayoutToken) =
        isIdentifier token
        || (constant token).IsSome
        || isDelimiter "(" token
        || isDelimiter "[" token
        || isDelimiter "{" token

    let private canStartExpression (token: LayoutToken) =
        canStartAtom token
        || isKeyword "if" token
        || isKeyword "match" token
        || isKeyword "fun" token

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

    let private canStartPattern (token: LayoutToken) =
        isIdentifier token
        || (constant token).IsSome
        || isDelimiter "(" token

    let private continuesOnNewLine (context: SourcePosition) (token: LayoutToken) =
        isOffside context token
        && token.Range.Start.Column = context.Column

    let private skipBlock state (context: SourcePosition) =
        let cursor = state.Cursor

        while cursor.Current.Kind
              <> LayoutTokenKind.EndBlock
              && not (isEndOfFile cursor.Current) do
            let before = cursor.Current

            if before.Kind = LayoutTokenKind.Separator then
                cursor.Advance()
                |> ignore
            else
                skipUntil state context
                |> ignore

                if
                    cursor.Current.Kind = before.Kind
                    && cursor.Current.Range = before.Range
                then
                    cursor.Advance()
                    |> ignore

    let rec private parsePatternWith allowArguments state context =
        let cursor = state.Cursor
        let first = parseElementPattern allowArguments false state context

        if isDelimiter "," cursor.Current then
            let items = ImmutableArray.CreateBuilder<SyntaxPattern>()
            items.Add first

            while isDelimiter "," cursor.Current do
                cursor.Advance()
                |> ignore

                items.Add(parseElementPattern allowArguments false state context)

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

    and private parseElementPattern allowArguments allowType state context =
        let cursor = state.Cursor
        let token = cursor.Current

        let pattern =
            if
                allowArguments
                && isIdentifier token
                && tokenText token
                   <> "_"
            then
                let name = longIdentifier state
                let next = cursor.Current

                let argument =
                    if
                        (canStartPattern next
                         || isDelimiter "[" next)
                        && not (isOffside context next)
                    then
                        Some(parseAtomicPattern state context)
                    else
                        None

                match argument with
                | None when name.Parts.Length = 1 -> SyntaxPattern.Named name.Parts[0]
                | None -> SyntaxPattern.UnionCase(name, None, name.Range)
                | Some argument ->
                    SyntaxPattern.UnionCase(name, Some argument, span name.Range argument.Range)
            else
                parseAtomicPattern state context

        if
            allowType
            && isOperator ":" cursor.Current
        then
            cursor.Advance()
            |> ignore

            let patternType =
                parseTypeOperand
                    state
                    context
                    (fun _ -> None)
                    TypeGap.UnexpectedToken
                    (fun () -> parseType state context (fun _ -> None))

            SyntaxPattern.Typed(pattern, patternType, span pattern.Range patternType.Range)
        else
            pattern

    and private parseAtomicPattern state context =
        let cursor = state.Cursor
        let token = cursor.Current

        match constant token with
        | Some value ->
            cursor.Advance()
            |> ignore

            SyntaxPattern.Constant(value, token.Range)
        | None when
            isIdentifier token
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
                let first = parseElementPattern true true state context

                let inner =
                    if isDelimiter "," cursor.Current then
                        let items = ImmutableArray.CreateBuilder<SyntaxPattern>()
                        items.Add first

                        while isDelimiter "," cursor.Current do
                            cursor.Advance()
                            |> ignore

                            items.Add(parseElementPattern true true state context)

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

                if isDelimiter ")" cursor.Current then
                    let close = cursor.Advance()
                    SyntaxPattern.Parenthesized(inner, span token.Range close.Range)
                else
                    if not (reportedAt state cursor.Current) then
                        reportUnsupported state cursor.Current "a parenthesized pattern"

                    SyntaxPattern.Parenthesized(inner, span token.Range (emptyAt cursor.LastEnd))
        | None when isDelimiter "[" token ->
            cursor.Advance()
            |> ignore

            let items = ImmutableArray.CreateBuilder<SyntaxPattern>()
            let mutable closed = isDelimiter "]" cursor.Current

            while not closed
                  && (canStartPattern cursor.Current
                      || isDelimiter "[" cursor.Current) do
                items.Add(parsePatternWith true state context)

                if isDelimiter ";" cursor.Current then
                    cursor.Advance()
                    |> ignore

                closed <- isDelimiter "]" cursor.Current

            if closed then
                let close = cursor.Advance()
                SyntaxPattern.List(items.ToImmutable(), span token.Range close.Range)
            else
                if not (reportedAt state cursor.Current) then
                    reportUnsupported state cursor.Current "a list pattern"

                SyntaxPattern.List(items.ToImmutable(), span token.Range (emptyAt cursor.LastEnd))
        | None ->
            reportUnsupported state token "a pattern"
            missingPattern token

    let private parsePattern state context = parsePatternWith false state context

    let private comparisonOperatorsAfterOperand =
        HashSet [
            "<<<"
            "<="
            "<>"
            "<|"
        ]

    let private typeArgumentScanStops =
        HashSet [
            "+"
            "="
            "||"
            "&&"
            "|>"
            ":"
            "["
            "]"
            "}"
        ]

    let private typeArgumentScanStopKeywords =
        HashSet [
            "then"
            "let"
            "with"
            "in"
            "do"
            "else"
        ]

    let private typeArgumentScanContinues =
        HashSet [
            ","
            "*"
            "-"
            ";"
            "."
            "->"
        ]

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

    and private adjacentLessThanIsOperator (cursor: Cursor) =
        let text = tokenText cursor.Current

        if
            text
            <> "<"
        then
            comparisonOperatorsAfterOperand.Contains text
        else
            // The Compatibility Oracle reads an adjacent '<' as type arguments when a scan across lines finds the closing '>'.
            let rec scan offset parentheses =
                let token = cursor.Peek offset
                let text = tokenText token

                match token.Kind with
                | LayoutTokenKind.BeginBlock
                | LayoutTokenKind.Separator
                | LayoutTokenKind.EndBlock -> scan (offset + 1) parentheses
                | LayoutTokenKind.SourceToken ->
                    if isEndOfFile token then
                        true
                    elif isDelimiter ")" token then
                        parentheses = 0
                        || scan
                            (offset + 1)
                            (parentheses
                             - 1)
                    elif isDelimiter "(" token then
                        scan
                            (offset + 1)
                            (parentheses
                             + 1)
                    elif
                        isKind LexicalTokenKind.Keyword token
                        && typeArgumentScanStopKeywords.Contains text
                    then
                        true
                    elif
                        (isKind LexicalTokenKind.Operator token
                         || isKind LexicalTokenKind.Delimiter token)
                        && typeArgumentScanStops.Contains text
                    then
                        true
                    elif
                        isIdentifier token
                        || isKind LexicalTokenKind.NumericLiteral token
                        || isKind LexicalTokenKind.StringLiteral token
                        || ((isKind LexicalTokenKind.Operator token
                             || isKind LexicalTokenKind.Delimiter token)
                            && typeArgumentScanContinues.Contains text)
                    then
                        scan (offset + 1) parentheses
                    else
                        false

            scan 1 0

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
                    && not (adjacentLessThanIsOperator cursor)
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

        let result =
            match parseAtom state context with
            | SyntaxExpression.DotLambda _ as dotLambda ->
                if
                    canStartAtom cursor.Current
                    && not (isOffside context cursor.Current)
                    && not (reportedAt state cursor.Current)
                then
                    reportUnsupported
                        state
                        cursor.Current
                        "an argument after an underscore dot shorthand"

                dotLambda
            | head -> parsePostfix state context Int32.MaxValue head

        let mutable result = result

        while canStartAtom cursor.Current
              && not (isOffside context cursor.Current) do
            let argument = parseArgument state context

            result <-
                SyntaxExpression.Application(result, argument, span result.Range argument.Range)

        result

    and private parseArgument state context =
        match parseAtom state context with
        | SyntaxExpression.Identifier _ as name ->
            let argument = parsePostfix state context 1 name

            match argument with
            | SyntaxExpression.Application(_, _, range) ->
                report
                    state
                    successiveArgumentsCode
                    "Successive arguments should be separated by spaces or tupled, and arguments involving function or method applications should be parenthesized"
                    range
            | _ -> ()

            argument
        | argument -> argument

    and private isAdjacentStep state (token: LayoutToken) =
        token.Kind = LayoutTokenKind.SourceToken
        && token.Range.Start.Offset = state.Cursor.LastEnd.Offset
        && (isDelimiter "(" token
            || isDelimiter "[" token)

    and private parsePostfix state context maximumSteps target =
        let cursor = state.Cursor
        let mutable result = target
        let mutable steps = 0
        let mutable more = true

        while more
              && steps < maximumSteps
              && isAdjacentStep state cursor.Current do
            steps <- steps + 1

            if isDelimiter "(" cursor.Current then
                let argument = parseAtom state context

                result <-
                    SyntaxExpression.Application(result, argument, span result.Range argument.Range)
            else
                let openToken = cursor.Advance()

                let index =
                    if canStartExpression cursor.Current then
                        parseExpression state openToken.Range.Start
                    else
                        missingExpression cursor.Current

                if isDelimiter "]" cursor.Current then
                    let close = cursor.Advance()

                    result <-
                        SyntaxExpression.BracketApplication(
                            result,
                            index,
                            span result.Range close.Range
                        )
                else
                    if not (reportedAt state cursor.Current) then
                        reportUnsupported state cursor.Current "an index expression"

                    result <-
                        SyntaxExpression.BracketApplication(
                            result,
                            index,
                            span result.Range (emptyAt cursor.LastEnd)
                        )

                    more <- false

        result

    and private parseAtom state context =
        let cursor = state.Cursor
        let token = cursor.Current

        match constant token with
        | Some value ->
            cursor.Advance()
            |> ignore

            SyntaxExpression.Constant(value, token.Range)
        | None when
            isIdentifier token
            && tokenText token = "_"
            ->
            parseDotLambda state context
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
        | None when isKeyword "if" token -> parseIf state context
        | None when isKeyword "match" token -> parseMatch state context
        | None when isKeyword "fun" token -> parseLambda state context
        | None when isDelimiter "[" token -> parseList state
        | None when isDelimiter "{" token -> parseRecord state
        | None ->
            reportUnsupported state token "an expression"
            missingExpression token

    and private parseDotLambda state context =
        let cursor = state.Cursor
        let underscore = cursor.Advance()

        if not (isOperator "." cursor.Current) then
            reportUnsupported state underscore "an underscore expression"
            missingExpression underscore
        else
            let dot = cursor.Advance()

            if
                not (isIdentifier cursor.Current)
                || tokenText cursor.Current = "_"
            then
                reportUnsupported state cursor.Current "an underscore dot shorthand"
                missingExpression cursor.Current
            else
                let members = longIdentifier state

                if
                    not (
                        LanguageFeature.isAvailable
                            state.Language
                            LanguageFeature.UnderscoreDotShorthand
                    )
                then
                    report
                        state
                        featureGateCode
                        (LanguageFeature.unavailableDiagnostic
                            state.Language
                            LanguageFeature.UnderscoreDotShorthand)
                        (span underscore.Range dot.Range)

                let body = parsePostfix state context 1 (SyntaxExpression.Identifier members)

                if
                    isAdjacentStep state cursor.Current
                    && not (reportedAt state cursor.Current)
                then
                    reportUnsupported
                        state
                        cursor.Current
                        "a second adjacent argument in an underscore dot shorthand"

                SyntaxExpression.DotLambda(body, span underscore.Range body.Range)

    and private parseBranchStart state context point =
        let cursor = state.Cursor

        if
            canStartExpression cursor.Current
            && not (isOffside context cursor.Current)
        then
            parseExpression state context
        else
            let missing = missingExpression cursor.Current

            match point with
            | Some point ->
                reportUnexpected state point "an expression"
                |> ignore
            | None -> reportUnsupported state cursor.Current "an expression"

            missing

    and private parseBranch state context point =
        let cursor = state.Cursor

        if cursor.Current.Kind = LayoutTokenKind.BeginBlock then
            let block = cursor.Advance()
            let blockContext = block.Range.Start
            let reported = state.Diagnostics.Count
            let body = parseBranchStart state blockContext point

            if
                cursor.Current.Kind
                <> LayoutTokenKind.EndBlock
            then
                if not (reportedSince state reported) then
                    reportUnsupported state cursor.Current "a sequential expression"

                skipBlock state blockContext

            if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                cursor.Advance()
                |> ignore

            body
        else
            parseBranchStart state context point

    and private parseIf state context =
        let cursor = state.Cursor
        let ifToken = cursor.Advance()
        let condition = parseExpression state context

        if not (isKeyword "then" cursor.Current) then
            if not (reportedAt state cursor.Current) then
                reportUnsupported state cursor.Current "a conditional expression"

            SyntaxExpression.If(
                condition,
                missingExpression cursor.Current,
                None,
                span ifToken.Range (emptyAt cursor.LastEnd)
            )
        else
            cursor.Advance()
            |> ignore

            let thenBranch = parseBranch state context None

            if
                cursor.Current.Kind = LayoutTokenKind.Separator
                && (isKeyword "else" (cursor.Peek 1)
                    || isKeyword "elif" (cursor.Peek 1))
                && (cursor.Peek 1).Range.Start.Column = ifToken.Range.Start.Column
            then
                cursor.Advance()
                |> ignore

            let elseBranch =
                if isKeyword "else" cursor.Current then
                    cursor.Advance()
                    |> ignore

                    Some(parseBranch state context None)
                elif isKeyword "elif" cursor.Current then
                    Some(parseIf state context)
                else
                    None

            SyntaxExpression.If(
                condition,
                thenBranch,
                elseBranch,
                span ifToken.Range (emptyAt cursor.LastEnd)
            )

    and private parseMatch state context =
        let cursor = state.Cursor
        let matchToken = cursor.Advance()
        let input = parseExpression state context
        let clauses = ImmutableArray.CreateBuilder<SyntaxMatchClause>()

        if not (isKeyword "with" cursor.Current) then
            reportUnexpected state RecoveryPoint.MatchWith "a match expression"
            |> ignore
        else
            cursor.Advance()
            |> ignore

            let startsClause () =
                (cursor.Current.Kind = LayoutTokenKind.BeginBlock
                 || cursor.Current.Kind = LayoutTokenKind.Separator)
                && isOperator "|" (cursor.Peek 1)

            let closesBlock = cursor.Current.Kind = LayoutTokenKind.BeginBlock

            if startsClause () then
                cursor.Advance()
                |> ignore

            let mutable more = true

            while more do
                let bar =
                    if isOperator "|" cursor.Current then
                        Some(cursor.Advance())
                    else
                        None

                let start = cursor.Current

                let clauseContext =
                    bar
                    |> Option.map _.Range.Start
                    |> Option.defaultValue start.Range.Start

                if
                    not (
                        canStartPattern start
                        || isDelimiter "[" start
                    )
                then
                    if not (reportedAt state start) then
                        reportUnsupported state start "a match clause"

                    more <- false
                else
                    let pattern = parsePatternWith true state clauseContext

                    let guard =
                        if isKeyword "when" cursor.Current then
                            cursor.Advance()
                            |> ignore

                            Some(parseExpression state clauseContext)
                        else
                            None

                    if not (isOperator "->" cursor.Current) then
                        reportUnexpected state RecoveryPoint.ClauseArrow "a match clause"
                        |> ignore

                        more <- false
                    else
                        cursor.Advance()
                        |> ignore

                        let reported = state.Diagnostics.Count

                        let result =
                            parseBranch state clauseContext (Some RecoveryPoint.ClauseResult)

                        clauses.Add {
                            Pattern = pattern
                            Guard = guard
                            Result = result
                            Range =
                                span
                                    (bar
                                     |> Option.map _.Range
                                     |> Option.defaultValue start.Range)
                                    (emptyAt cursor.LastEnd)
                        }

                        if reportedSince state reported then
                            more <- false
                        elif isOperator "|" cursor.Current then
                            ()
                        elif
                            cursor.Current.Kind = LayoutTokenKind.Separator
                            && isOperator "|" (cursor.Peek 1)
                        then
                            cursor.Advance()
                            |> ignore
                        else
                            more <- false

            if
                closesBlock
                && cursor.Current.Kind = LayoutTokenKind.EndBlock
            then
                cursor.Advance()
                |> ignore

        SyntaxExpression.Match(
            input,
            clauses.ToImmutable(),
            span matchToken.Range (emptyAt cursor.LastEnd)
        )

    and private parseLambda state context =
        let cursor = state.Cursor
        let funToken = cursor.Advance()
        let patterns = ImmutableArray.CreateBuilder<SyntaxPattern>()

        while canStartPattern cursor.Current
              && not (isOffside context cursor.Current) do
            patterns.Add(parseAtomicPattern state context)

        let body =
            if
                patterns.Count > 0
                && isOperator "->" cursor.Current
            then
                cursor.Advance()
                |> ignore

                parseBranch state context None
            else
                let missing = missingExpression cursor.Current

                if not (reportedAt state cursor.Current) then
                    let point =
                        if patterns.Count = 0 then
                            RecoveryPoint.LambdaStart
                        else
                            RecoveryPoint.LambdaArrow

                    reportUnexpected state point "a lambda expression"
                    |> ignore

                missing

        if
            isOperator "|" cursor.Current
            && not (closesArrayOrAttribute cursor)
            && not (reportedAt state cursor.Current)
        then
            reportUnsupported state cursor.Current "a lambda expression body"

        SyntaxExpression.Lambda(
            patterns.ToImmutable(),
            body,
            span funToken.Range (emptyAt cursor.LastEnd)
        )

    and private parseList state =
        let cursor = state.Cursor
        let openToken = cursor.Advance()
        let items = ImmutableArray.CreateBuilder<SyntaxExpression>()
        let elementContext = cursor.Current.Range.Start
        let mutable closed = isDelimiter "]" cursor.Current
        let mutable failed = false

        while not closed
              && not failed do
            if canStartExpression cursor.Current then
                items.Add(parseExpression state elementContext)

                if isDelimiter ";" cursor.Current then
                    cursor.Advance()
                    |> ignore

                closed <- isDelimiter "]" cursor.Current

                if
                    not closed
                    && not (canStartExpression cursor.Current)
                then
                    failed <- true
            else
                failed <- true

        if closed then
            let close = cursor.Advance()
            SyntaxExpression.List(items.ToImmutable(), span openToken.Range close.Range)
        else
            if not (reportedAt state cursor.Current) then
                reportUnsupported state cursor.Current "a list expression"

            SyntaxExpression.List(
                items.ToImmutable(),
                span openToken.Range (emptyAt cursor.LastEnd)
            )

    and private parseRecord state =
        let cursor = state.Cursor
        let openToken = cursor.Advance()
        let fields = ImmutableArray.CreateBuilder<SyntaxRecordFieldValue>()
        let mutable closed = false
        let mutable failed = false

        while not closed
              && not failed do
            let start = cursor.Current
            let fieldContext = start.Range.Start

            if not (isIdentifier start) then
                failed <- true
            else
                let name = longIdentifier state

                if not (isOperator "=" cursor.Current) then
                    failed <- true
                else
                    cursor.Advance()
                    |> ignore

                    if not (canStartExpression cursor.Current) then
                        reportUnexpected state RecoveryPoint.RecordFieldValue "a record field"
                        |> ignore

                        failed <- true
                    else
                        let value = parseExpression state fieldContext

                        fields.Add {
                            Name = name
                            Value = value
                            Range = span start.Range value.Range
                        }

                        let separated = isDelimiter ";" cursor.Current

                        if separated then
                            cursor.Advance()
                            |> ignore

                        if isDelimiter "}" cursor.Current then
                            closed <- true
                        elif
                            not separated
                            && not (continuesOnNewLine fieldContext cursor.Current)
                        then
                            failed <- true

        if closed then
            let close = cursor.Advance()
            SyntaxExpression.Record(fields.ToImmutable(), span openToken.Range close.Range)
        else
            if not (reportedAt state cursor.Current) then
                reportUnsupported state cursor.Current "a record expression"

            SyntaxExpression.Record(
                fields.ToImmutable(),
                span openToken.Range (emptyAt cursor.LastEnd)
            )

    let private endsBinding context (token: LayoutToken) =
        token.Kind = LayoutTokenKind.Separator
        || token.Kind = LayoutTokenKind.EndBlock
        || isEndOfFile token
        || isKeyword "and" token
        || isOffside context token

    let private endsWithoutBody context (cursor: Cursor) =
        let next = nextSourceToken cursor

        endsBinding context cursor.Current
        && (not (isKeyword "and" next)
            || (next.Range.Start.Line > cursor.LastEnd.Line
                && next.Range.Start.Column = context.Column))

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

    let private isSuppressing state =
        match state.Recovery with
        | Recovery.Parsing -> false
        | Recovery.Suppressing _
        | Recovery.Interrupted _ -> true

    let private pendingDeclaration state =
        match state.Recovery with
        | Recovery.Suppressing(_, next) -> Some next
        | Recovery.Parsing
        | Recovery.Interrupted _ -> None

    let private discardsSilently state =
        match pendingDeclaration state with
        | Some(DeclarationAfterRecovery.Discarded _)
        | Some(DeclarationAfterRecovery.SkippedInSignatureModule _) -> true
        | _ -> false

    let private parseBinding state context attributes =
        let cursor = state.Cursor
        let reported = state.Diagnostics.Count
        let mutable recovered = false
        let mutable skipped = None

        let recover point =
            if not (reportedSince state reported) then
                match point with
                | Some point ->
                    reportUnexpected state point "a binding"
                    |> ignore
                | None -> reportUnsupported state cursor.Current "a binding"

            recovered <- true
            skipped <- skipUntil state context

        let parseBody () =
            if canStartExpression cursor.Current then
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
                        cursor.Current.Kind
                        <> LayoutTokenKind.EndBlock
                    then
                        if not (reportedSince state reported) then
                            reportUnsupported state cursor.Current "a binding"

                        recovered <- true
                        skipBlock state context

                    if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                        cursor.Advance()
                        |> ignore

                    body
                elif
                    endsWithoutBody context cursor
                    && LanguageBehavior.isActive state.Language LanguageBehavior.StrictIndentation
                then
                    let missing = missingExpression cursor.Current
                    let range = nextTokenOrEndRange cursor

                    if
                        isSuppressing state
                        && not (discardsSilently state)
                    then
                        reportUnsupported
                            state
                            cursor.Current
                            "a binding without a body after syntax recovery"

                    reportOffside state context range

                    report
                        state
                        "FS0010"
                        "Incomplete structured construct at or before this point in binding"
                        range

                    missing
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

        keepFirstDiagnosticSince state reported

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

    let private useInModuleMessage =
        "'use' bindings are not permitted in modules and are treated as 'let' bindings"

    let private letAndMessage =
        "The declaration form 'let ... and ...' for non-recursive bindings is not used in F# code. Consider using a sequence of 'let' bindings"

    let private parseLet state attributes =
        let cursor = state.Cursor
        let letToken = cursor.Advance()
        let context = letToken.Range.Start

        let keyword =
            if isKeyword "use" letToken then
                SyntaxLetKeyword.Use
            else
                SyntaxLetKeyword.Let

        let isRecursive =
            if isKeyword "rec" cursor.Current then
                cursor.Advance()
                |> ignore

                true
            else
                false

        let bindings = ImmutableArray.CreateBuilder<SyntaxBinding>()
        let reported = state.Diagnostics.Count
        bindings.Add(parseBinding state context attributes)
        let firstBindingRecovered = reportedSince state reported

        let continuesWithAnd () =
            isKeyword "and" cursor.Current
            || (cursor.Current.Kind = LayoutTokenKind.Separator
                && isKeyword "and" (cursor.Peek 1))

        while continuesWithAnd () do
            if cursor.Current.Kind = LayoutTokenKind.Separator then
                cursor.Advance()
                |> ignore

            let previousEnd = cursor.LastEnd
            let andToken = cursor.Advance()

            if
                not isRecursive
                && andToken.Range.Start.Line = previousEnd.Line
            then
                reportUnsupported state andToken "a non-recursive 'and' on the same line"

            bindings.Add(parseBinding state context ImmutableArray.Empty)

        keepFirstDiagnosticSince state reported

        if keyword = SyntaxLetKeyword.Use then
            let finish =
                state.Diagnostics
                |> Seq.skip reported
                |> Seq.tryFind (fun diagnostic -> isRecoveryCode diagnostic.Code)
                |> Option.bind (fun diagnostic -> cursor.EndBefore diagnostic.Range.Start.Offset)
                |> Option.defaultValue cursor.LastEnd

            reportWarning
                state
                useInModuleCode
                useInModuleMessage
                (span letToken.Range (emptyAt finish))

        if
            not isRecursive
            && bindings.Count > 1
            && not firstBindingRecovered
        then
            report state letAndCode letAndMessage letToken.Range

        ImplementationDeclaration.Let(
            keyword,
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
                    if canStartExpression cursor.Current then
                        parseExpression state context
                    else
                        reportUnsupported state cursor.Current "a do declaration"
                        missingExpression cursor.Current

                if
                    cursor.Current.Kind
                    <> LayoutTokenKind.EndBlock
                then
                    if not (reportedSince state reported) then
                        reportUnsupported state cursor.Current "a do declaration"

                    skipUntil state context
                    |> ignore

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                body
            elif
                canStartExpression cursor.Current
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
    type private ListRecovery =
        | Continues
        | Discards
        | DiscardsAfterDeclaration
        | DiscardsInValue
        | DiscardsInsideValueType
        | Unmodeled

    let private parseExpressionDeclaration state point attributes =
        let cursor = state.Cursor
        let start = cursor.Current
        let context = start.Range.Start
        let reported = state.Diagnostics.Count
        let body = parseExpression state context
        let range = span (declarationStart attributes start) (emptyAt cursor.LastEnd)

        let skipped, recovery =
            if
                reportedSince state reported
                || endsLine context cursor.Current
            then
                None, ListRecovery.Continues
            elif reportUnexpected state point "an expression declaration" then
                skipUntil state context, ListRecovery.DiscardsAfterDeclaration
            else
                None, ListRecovery.Continues

        ImplementationDeclaration.Expression(attributes, body, skipped, range), recovery

    let private parseVal state nested attributes =
        let cursor = state.Cursor
        let valToken = cursor.Advance()
        let context = valToken.Range.Start
        let reported = state.Diagnostics.Count
        let mutable recovered = None
        let mutable typeStart = -1
        let mutable recoveredInsideType = false
        let mutable skipped = None

        let recover point =
            if not (reportedSince state reported) then
                reportUnexpected state point "a value signature"
                |> ignore

            recovered <- Some point

            recoveredInsideType <-
                point = RecoveryPoint.ValueType
                && cursor.Current.Range.Start.Offset
                   <> typeStart

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
            if recovered.IsSome then
                missingType cursor.Current
            elif isOperator ":" cursor.Current then
                cursor.Advance()
                |> ignore

                typeStart <- cursor.Current.Range.Start.Offset

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
            recovered.IsNone
            && isOperator "=" cursor.Current
            && not (endsLine context cursor.Current)
        then
            reportUnsupported state cursor.Current "a literal value signature"
            recovered <- Some RecoveryPoint.ValueType
            skipped <- skipUntil state context
        elif
            recovered.IsNone
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

        let recovery =
            match recovered with
            | None -> ListRecovery.Continues
            | Some RecoveryPoint.SignatureFile
            | Some RecoveryPoint.NestedSignature -> ListRecovery.Discards
            | Some _ when recoveredInsideType -> ListRecovery.DiscardsInsideValueType
            | Some _ -> ListRecovery.DiscardsInValue

        Some value, recovery

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

    let private missingRepresentation (token: LayoutToken) =
        SyntaxTypeRepresentation.Missing {
            Expected = "type representation"
            Range = emptyAt token.Range.Start
        }

    let private parseRecordFields state =
        let cursor = state.Cursor

        cursor.Advance()
        |> ignore

        let fields = ImmutableArray.CreateBuilder<SyntaxRecordField>()
        let mutable closed = false
        let mutable failed = false

        while not closed
              && not failed do
            let start = cursor.Current
            let fieldContext = start.Range.Start

            let isMutable =
                if isKeyword "mutable" start then
                    cursor.Advance()
                    |> ignore

                    true
                else
                    false

            if not (isIdentifier cursor.Current) then
                failed <- true
            else
                let name = identifier (cursor.Advance())

                let hasColon = isOperator ":" cursor.Current

                if hasColon then
                    cursor.Advance()
                    |> ignore

                if not hasColon then
                    reportUnexpected state RecoveryPoint.FieldColon "a record field"
                    |> ignore

                    failed <- true
                elif not (canStartType cursor) then
                    reportUnexpected state RecoveryPoint.FieldType "a record field"
                    |> ignore

                    failed <- true
                else
                    let fieldType = parseType state fieldContext (fun _ -> None)

                    fields.Add {
                        IsMutable = isMutable
                        Name = name
                        Type = fieldType
                        Range = span start.Range fieldType.Range
                    }

                    let separated = isDelimiter ";" cursor.Current

                    if separated then
                        cursor.Advance()
                        |> ignore

                    if isDelimiter "}" cursor.Current then
                        closed <- true
                    elif
                        not separated
                        && not (continuesOnNewLine fieldContext cursor.Current)
                    then
                        failed <- true

        if closed then
            cursor.Advance()
            |> ignore
        elif not (reportedAt state cursor.Current) then
            reportUnsupported state cursor.Current "a record type"

        SyntaxTypeRepresentation.Record(fields.ToImmutable())

    let private parseUnionCases state =
        let cursor = state.Cursor
        let cases = ImmutableArray.CreateBuilder<SyntaxUnionCase>()
        let mutable more = true

        while more do
            let start = cursor.Current

            let hasBar = isOperator "|" start

            if hasBar then
                cursor.Advance()
                |> ignore

            let fieldPoint =
                if
                    hasBar
                    || cases.Count > 0
                then
                    RecoveryPoint.UnionCaseField
                else
                    RecoveryPoint.FirstUnionCaseField

            let context = start.Range.Start

            if not (isIdentifier cursor.Current) then
                reportUnexpected state RecoveryPoint.UnionCaseName "a union case"
                |> ignore

                more <- false
            else
                let name = identifier (cursor.Advance())
                let fields = ImmutableArray.CreateBuilder<SyntaxUnionField>()
                let mutable failed = false

                if isKeyword "of" cursor.Current then
                    cursor.Advance()
                    |> ignore

                    let mutable moreFields = true

                    while moreFields do
                        let fieldStart = cursor.Current

                        let fieldName =
                            if
                                isIdentifier fieldStart
                                && isOperator ":" (cursor.Peek 1)
                            then
                                let fieldName = identifier (cursor.Advance())

                                cursor.Advance()
                                |> ignore

                                Some fieldName
                            else
                                None

                        if not (canStartType cursor) then
                            reportUnexpected state fieldPoint "a union case"
                            |> ignore

                            failed <- true
                            moreFields <- false
                        else
                            let fieldType = parseApplicationType state context

                            fields.Add {
                                Name = fieldName
                                Type = fieldType
                                Range = span fieldStart.Range fieldType.Range
                            }

                            if isOperator "*" cursor.Current then
                                cursor.Advance()
                                |> ignore
                            else
                                moreFields <- false

                cases.Add {
                    Name = name
                    Fields = fields.ToImmutable()
                    Range = span start.Range (emptyAt cursor.LastEnd)
                }

                if failed then
                    more <- false
                elif isOperator "|" cursor.Current then
                    ()
                elif
                    cursor.Current.Kind = LayoutTokenKind.Separator
                    && isOperator "|" (cursor.Peek 1)
                then
                    cursor.Advance()
                    |> ignore
                else
                    more <- false

        SyntaxTypeRepresentation.Union(cases.ToImmutable())

    let private parseMembers state =
        let cursor = state.Cursor
        let members = ImmutableArray.CreateBuilder<SyntaxMember>()
        let mutable more = true

        while more do
            let start = cursor.Current
            let context = start.Range.Start
            let attributes = parseAttributeLists state

            let isStatic =
                if isKeyword "static" cursor.Current then
                    cursor.Advance()
                    |> ignore

                    true
                else
                    false

            if not (isKeyword "member" cursor.Current) then
                if not (reportedAt state cursor.Current) then
                    reportUnsupported state cursor.Current "a class member"

                more <- false
            else
                cursor.Advance()
                |> ignore

                let signature =
                    if isStatic then
                        if isIdentifier cursor.Current then
                            Some(SyntaxMemberKind.Static, identifier (cursor.Advance()))
                        else
                            None
                    elif
                        isIdentifier cursor.Current
                        && isOperator "." (cursor.Peek 1)
                        && isIdentifier (cursor.Peek 2)
                    then
                        let self = identifier (cursor.Advance())

                        cursor.Advance()
                        |> ignore

                        Some(SyntaxMemberKind.Instance self, identifier (cursor.Advance()))
                    else
                        None

                match signature with
                | Some(kind, name) ->
                    let parameters = ImmutableArray.CreateBuilder<SyntaxPattern>()

                    while canStartPattern cursor.Current
                          && not (isOffside context cursor.Current) do
                        parameters.Add(parseAtomicPattern state context)

                    if not (isOperator "=" cursor.Current) then
                        if not (reportedAt state cursor.Current) then
                            reportUnsupported state cursor.Current "a class member"

                        more <- false
                    else
                        cursor.Advance()
                        |> ignore

                        let reported = state.Diagnostics.Count
                        let body = parseBranch state context None

                        members.Add {
                            Attributes = attributes
                            Kind = kind
                            Name = name
                            Parameters = parameters.ToImmutable()
                            Body = body
                            Range = span start.Range (emptyAt cursor.LastEnd)
                        }

                        if reportedSince state reported then
                            more <- false
                        elif cursor.Current.Kind = LayoutTokenKind.Separator then
                            cursor.Advance()
                            |> ignore
                        else
                            more <- false
                | None ->
                    if not (reportedAt state cursor.Current) then
                        reportUnsupported state cursor.Current "a class member"

                    more <- false

        SyntaxTypeRepresentation.Class(members.ToImmutable())

    let private startsUnion (cursor: Cursor) =
        isOperator "|" cursor.Current
        || (isIdentifier cursor.Current
            && (isKeyword "of" (cursor.Peek 1)
                || isOperator "|" (cursor.Peek 1)))

    let private startsMember (token: LayoutToken) =
        isKeyword "member" token
        || isKeyword "static" token

    let private parseRepresentation state context =
        let cursor = state.Cursor

        if isDelimiter "{" cursor.Current then
            parseRecordFields state
        elif startsUnion cursor then
            parseUnionCases state
        elif startsMember cursor.Current then
            parseMembers state
        elif canStartType cursor then
            SyntaxTypeRepresentation.Abbreviation(parseType state context (fun _ -> None))
        else
            let missing = missingRepresentation cursor.Current

            if not (reportedAt state cursor.Current) then
                reportUnsupported state cursor.Current "a type definition"

            missing

    let private parseTypeDefinition state attributes =
        let cursor = state.Cursor
        let typeToken = cursor.Advance()
        let context = typeToken.Range.Start
        let reported = state.Diagnostics.Count
        let accessibility = parseAccessibility cursor

        if not (isIdentifier cursor.Current) then
            reportUnsupported state cursor.Current "a type name"
            None, ListRecovery.Continues
        else
            let name = identifier (cursor.Advance())

            let primaryConstructor =
                if isDelimiter "(" cursor.Current then
                    Some(parseAtomicPattern state context)
                else
                    None

            let representation, recovery =
                if isOperator "=" cursor.Current then
                    cursor.Advance()
                    |> ignore

                    if cursor.Current.Kind = LayoutTokenKind.BeginBlock then
                        let block = cursor.Advance()
                        let blockReported = state.Diagnostics.Count
                        let representation = parseRepresentation state block.Range.Start

                        if
                            cursor.Current.Kind
                            <> LayoutTokenKind.EndBlock
                        then
                            if not (reportedSince state blockReported) then
                                reportUnsupported state cursor.Current "a type definition"

                            skipBlock state block.Range.Start

                        if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                            cursor.Advance()
                            |> ignore

                        representation, ListRecovery.Continues
                    else
                        parseRepresentation state context, ListRecovery.Continues
                // The Compatibility Oracle ends the definition at ')', '}', or 'end' after a type name, but reports '=' as expected for other closing tokens.
                elif
                    isDelimiter ")" cursor.Current
                    || isDelimiter "}" cursor.Current
                    || isKeyword "end" cursor.Current
                then
                    let missing = missingRepresentation cursor.Current

                    reportUnexpected state RecoveryPoint.DefinitionStart "a type definition"
                    |> ignore

                    missing, ListRecovery.Discards
                else
                    let missing = missingRepresentation cursor.Current

                    if reportUnexpected state RecoveryPoint.TypeEquals "a type definition" then
                        missing, ListRecovery.Unmodeled
                    else
                        missing, ListRecovery.Continues

            let skipped =
                if endsLine context cursor.Current then
                    None
                else
                    if not (reportedSince state reported) then
                        reportUnsupported state cursor.Current "a type definition"

                    skipUntil state context

            keepFirstDiagnosticSince state reported

            let definition = {
                Attributes = attributes
                Accessibility = accessibility
                Name = name
                PrimaryConstructor = primaryConstructor
                Representation = representation
                Skipped = skipped
                Range = span (declarationStart attributes typeToken) (emptyAt cursor.LastEnd)
            }

            Some(ImplementationDeclaration.Type definition), recovery

    type private DeclarationRules<'Declaration> = {
        Parse:
            ParserState
                -> DeclarationList
                -> ImmutableArray<SyntaxAttributeList>
                -> LayoutToken
                -> ('Declaration option * ListRecovery) option
        NestedRecoveryDiscards: bool
        SkipsUnrecognizedAfterRecovery: bool
        StartPoint: DeclarationList -> RecoveryPoint
        Discarded: 'Declaration -> DiscardedDeclaration
        FirstNestedPoint: RecoveryPoint
        Open: LongIdentifier * SourceRange -> 'Declaration
        NestedModule: SyntaxNestedModule<'Declaration> -> 'Declaration
        Skipped: SkippedSyntax -> 'Declaration
    }

    let private isDeclarationListEnd (token: LayoutToken) =
        token.Kind = LayoutTokenKind.EndBlock
        || isEndOfFile token
        || isKeyword "namespace" token

    let private reportAfterRecovery state (token: LayoutToken) =
        reportUnsupported state token "a declaration after syntax recovery"
        state.Recovery <- Recovery.Interrupted state.Diagnostics.Count

    let private offsetAfterAttributeLists (cursor: Cursor) =
        let rec after offset =
            let token = cursor.Peek offset
            let next = cursor.Peek(offset + 1)

            if
                isDelimiter "[" token
                && isOperator "<" next
                && next.Range.Start.Offset = token.Range.End.Offset
            then
                let rec close offset =
                    let token = cursor.Peek offset
                    let next = cursor.Peek(offset + 1)

                    if isEndOfFile token then
                        offset
                    elif
                        isOperator ">" token
                        && isDelimiter "]" next
                        && next.Range.Start.Offset = token.Range.End.Offset
                    then
                        offset + 2
                    else
                        close (offset + 1)

                let rec skipSeparators offset =
                    if (cursor.Peek offset).Kind = LayoutTokenKind.Separator then
                        skipSeparators (offset + 1)
                    else
                        offset

                after (skipSeparators (close (offset + 2)))
            else
                offset

        after 0

    let private resumesSignatureModule (token: LayoutToken) =
        isKeyword "val" token
        || isKeyword "open" token
        || isKeyword "module" token

    let private suppress state next =
        state.Recovery <- Recovery.Suppressing(state.Diagnostics.Count, next)

    let private continueSuppression state next =
        match state.Recovery with
        | Recovery.Suppressing(suppressFrom, _) ->
            state.Recovery <- Recovery.Suppressing(suppressFrom, next)
        | Recovery.Parsing
        | Recovery.Interrupted _ -> ()

    let private rootKeywords =
        HashSet [
            "module"
            "type"
            "open"
            "do"
            "exception"
            "if"
            "match"
            "fun"
            "private"
            "inline"
            "end"
            "val"
        ]

    let private rootSymbols =
        HashSet [
            "("
            ")"
            "["
            "]"
            "}"
            "="
            "|"
        ]

    let private rootDeclarationToken (cursor: Cursor) =
        let token = cursor.Current
        let next = cursor.Peek 1
        let text = tokenText token

        let adjacent = next.Range.Start.Offset = token.Range.End.Offset

        if isAttributeListStart cursor then
            Some("symbol '[<'", span token.Range next.Range)
        elif
            (isKeyword "let" token
             || isKeyword "use" token)
            && isOperator "!" next
            && adjacent
        then
            Some("binder keyword", span token.Range next.Range)
        elif
            isKeyword "let" token
            || isKeyword "use" token
        then
            Some("keyword 'let' or 'use'", token.Range)
        elif
            isKind LexicalTokenKind.Keyword token
            && rootKeywords.Contains text
        then
            Some($"keyword '{text}'", token.Range)
        elif text = "_" then
            Some("symbol '_'", token.Range)
        elif isKind LexicalTokenKind.Identifier token then
            Some("identifier", token.Range)
        elif
            isKind LexicalTokenKind.NumericLiteral token
            && Seq.forall Char.IsAsciiDigit text
        then
            Some("integer literal", token.Range)
        elif
            isKind LexicalTokenKind.StringLiteral token
            && text.StartsWith("\"", StringComparison.Ordinal)
            && not (text.StartsWith("\"\"\"", StringComparison.Ordinal))
        then
            Some("string literal", token.Range)
        elif
            (isKind LexicalTokenKind.Delimiter token
             || isKind LexicalTokenKind.Operator token)
            && rootSymbols.Contains text
        then
            Some($"symbol '{text}'", token.Range)
        else
            None

    let private reportIncompleteAtRoot state (range: SourceRange) =
        report
            state
            "FS0010"
            "Incomplete structured construct at or before this point in implementation file"
            range

        suppress state (DeclarationAfterRecovery.Discarded(Some range.Start.Offset))

    let private reportAtRoot state =
        match rootDeclarationToken state.Cursor with
        | Some(description, range) ->
            report state "FS0010" $"Unexpected {description} in implementation file" range
            suppress state (DeclarationAfterRecovery.Discarded(Some range.Start.Offset))
        | None -> reportAfterRecovery state state.Cursor.Current

    let private reportIncompleteAtNext state =
        match rootDeclarationToken state.Cursor with
        | Some(_, range) -> reportIncompleteAtRoot state range
        | None -> reportAfterRecovery state state.Cursor.Current

    let private reportPendingAtEnd state =
        match pendingDeclaration state with
        | Some(DeclarationAfterRecovery.SkippedInSignatureModule(_, reportsAtEnd)) when
            isEndOfFile state.Cursor.Current
            ->
            if reportsAtEnd then
                state.SignatureHeader <-
                    SignatureHeader.LostAtEnd(emptyAt state.Cursor.Current.Range.Start)
        | Some(DeclarationAfterRecovery.ReportedAtRoot _)
        | Some DeclarationAfterRecovery.IncompleteAtNext ->
            let token = state.Cursor.Current

            if isEndOfFile token then
                let eof = token.Range.Start

                let lineStart = {
                    eof with
                        Offset =
                            eof.Offset
                            - (eof.Column
                               - 1)
                        Column = 1
                }

                reportIncompleteAtRoot state { Start = lineStart; End = eof }
            else
                reportIncompleteAtRoot state token.Range
        | _ -> ()


    let private suppressAfterNestedRecovery state =
        if state.InAnonymousRoot then
            suppress state (DeclarationAfterRecovery.Unmodeled true)
        else
            suppress state (DeclarationAfterRecovery.ReportedAtRoot state.Depth)

    let private removeSuppressedDiagnostics state =
        let rootReportStart =
            match pendingDeclaration state with
            | Some(DeclarationAfterRecovery.Discarded rootReportStart) -> rootReportStart
            | _ -> None

        let removeFrom suppressFrom =
            let kept =
                state.Diagnostics
                |> Seq.skip suppressFrom
                |> Seq.filter (fun diagnostic ->
                    (diagnostic.Code = unsupportedCode
                     || (diagnostic.Code = offsideCode
                         && discardsSilently state))
                    && rootReportStart
                       <> Some diagnostic.Range.Start.Offset
                )
                |> Seq.toArray

            state.Diagnostics.RemoveRange(
                suppressFrom,
                state.Diagnostics.Count
                - suppressFrom
            )

            state.Diagnostics.AddRange kept

        match state.Recovery with
        | Recovery.Suppressing(suppressFrom, next) ->
            removeFrom suppressFrom
            state.Recovery <- Recovery.Suppressing(state.Diagnostics.Count, next)
        | Recovery.Interrupted suppressFrom ->
            removeFrom suppressFrom
            state.Recovery <- Recovery.Interrupted state.Diagnostics.Count
        | Recovery.Parsing -> ()

    let rec private parseDeclarations
        state
        (rules: DeclarationRules<'Declaration>)
        (list: DeclarationList)
        : ImmutableArray<'Declaration> * ImmutableArray<'Declaration> =
        let cursor = state.Cursor
        let declarations = ImmutableArray.CreateBuilder<'Declaration>()
        let discarded = ImmutableArray.CreateBuilder<'Declaration>()
        let mutable scopedSuppression = false
        let mutable stop = false

        while not stop do
            let token = cursor.Current

            if token.Kind = LayoutTokenKind.Separator then
                cursor.Advance()
                |> ignore
            elif isDeclarationListEnd token then
                stop <- true
            else
                match pendingDeclaration state with
                | Some(DeclarationAfterRecovery.SkippedInSignatureModule(recoveryDepth, _)) when
                    resumesSignatureModule (cursor.Peek(offsetAfterAttributeLists cursor))
                    ->
                    let resumeOffset = offsetAfterAttributeLists cursor

                    if
                        state.Depth
                        + 1 < recoveryDepth
                        || (isKeyword "val" (cursor.Peek resumeOffset)
                            && not (
                                isIdentifier (
                                    cursor.Peek(
                                        resumeOffset
                                        + 1
                                    )
                                )
                            ))
                    then
                        // The Compatibility Oracle does not resume at a value without a name, and stays silent for later values.
                        reportAfterRecovery state token
                    else
                        state.Recovery <- Recovery.Parsing
                | _ -> ()

                let target = if isSuppressing state then discarded else declarations

                let firstInNestedModule =
                    list = DeclarationList.NestedModule
                    && declarations.Count = 0
                    && discarded.Count = 0

                let reported = state.Diagnostics.Count

                match pendingDeclaration state with
                | Some(DeclarationAfterRecovery.ReportedAtRoot _) when
                    list
                    <> DeclarationList.NestedModule
                    ->
                    reportAtRoot state
                | Some DeclarationAfterRecovery.IncompleteAtNext -> reportIncompleteAtNext state
                | _ -> ()

                let discardedBeforeRootReport =
                    match pendingDeclaration state with
                    | Some(DeclarationAfterRecovery.ReportedAtRoot _) -> true
                    | _ -> false

                let attributes = parseAttributeLists state
                let token = cursor.Current

                let interrupts =
                    match pendingDeclaration state with
                    | None
                    | Some(DeclarationAfterRecovery.Discarded _)
                    | Some(DeclarationAfterRecovery.ReportedAtRoot _)
                    | Some DeclarationAfterRecovery.IncompleteAtNext
                    | Some(DeclarationAfterRecovery.SkippedInSignatureModule _) -> false
                    | Some(DeclarationAfterRecovery.Unmodeled _) -> true
                    | Some DeclarationAfterRecovery.DiscardedIfValueOrOpen ->
                        not (
                            isKeyword "val" token
                            || isKeyword "open" token
                        )

                if interrupts then
                    reportAfterRecovery state token

                let parsed, recovery =
                    if
                        not attributes.IsEmpty
                        && (isKeyword "open" token
                            || (isKeyword "module" token
                                && attributes[attributes.Length
                                              - 1]
                                    .Range.End.Line = token.Range.Start.Line))
                    then
                        if discardsSilently state then
                            skipUntil state token.Range.Start
                            |> Option.iter (
                                rules.Skipped
                                >> target.Add
                            )
                        else
                            reportUnsupported state token "an attributed declaration"

                        None, ListRecovery.Continues
                    elif isKeyword "open" token then
                        parseOpen state
                        |> Option.map rules.Open,
                        ListRecovery.Continues
                    elif isKeyword "module" token then
                        parseNestedModule state rules attributes, ListRecovery.Continues
                    else
                        match rules.Parse state list attributes token with
                        | Some result -> result
                        | None when
                            rules.SkipsUnrecognizedAfterRecovery
                            && discardsSilently state
                            ->
                            // The Compatibility Oracle discards any declaration after a signature file recovery and reports no diagnostic for it.
                            skipUntil state token.Range.Start
                            |> Option.iter (
                                rules.Skipped
                                >> target.Add
                            )

                            None, ListRecovery.Continues
                        | None ->
                            let point =
                                if firstInNestedModule then
                                    rules.FirstNestedPoint
                                else
                                    match rules.StartPoint list with
                                    | RecoveryPoint.SignatureFile when
                                        (state.SignatureHeader
                                         <> SignatureHeader.Kept)
                                        ->
                                        RecoveryPoint.NestedSignature
                                    | point -> point

                            if reportedAt state token then
                                None, ListRecovery.Continues
                            elif
                                reportUnexpected state point "a module or namespace declaration"
                            then
                                None, ListRecovery.Discards
                            else
                                None, ListRecovery.Continues

                parsed
                |> Option.iter target.Add

                if discardedBeforeRootReport then
                    match pendingDeclaration state with
                    | Some(DeclarationAfterRecovery.ReportedAtRoot recoveryDepth) ->
                        let discardedKind =
                            if state.Depth < recoveryDepth then
                                // The Compatibility Oracle keeps the root diagnostic for declarations of an outer module.
                                Some DiscardedDeclaration.KeepsRootReport
                            elif recoveryDepth = 1 then
                                parsed
                                |> Option.map rules.Discarded
                            else
                                Some DiscardedDeclaration.Unmodeled

                        match discardedKind with
                        | None
                        | Some DiscardedDeclaration.KeepsRootReport -> ()
                        | Some DiscardedDeclaration.MakesNextIncomplete ->
                            continueSuppression state DeclarationAfterRecovery.IncompleteAtNext
                        | Some DiscardedDeclaration.Unmodeled ->
                            continueSuppression state (DeclarationAfterRecovery.Unmodeled true)
                    | _ -> ()

                let next = cursor.Current

                if
                    not (isDeclarationListEnd next)
                    && next.Kind
                       <> LayoutTokenKind.Separator
                    && not (isOffside token.Range.Start next)
                then
                    if
                        not (reportedSince state reported)
                        && not (reportedAt state next)
                    then
                        reportUnsupported state next "a module or namespace declaration"

                    skipUntil state token.Range.Start
                    |> Option.iter (
                        rules.Skipped
                        >> target.Add
                    )

                if isSuppressing state then
                    removeSuppressedDiagnostics state
                else
                    match recovery, list with
                    | ListRecovery.Discards, DeclarationList.NestedModule when
                        rules.NestedRecoveryDiscards
                        && firstInNestedModule
                        ->
                        // The Compatibility Oracle discards later declarations of this module only, and reports one more diagnostic if any exist.
                        suppress state (DeclarationAfterRecovery.Unmodeled false)
                        scopedSuppression <- true
                    | ListRecovery.Discards, DeclarationList.NestedModule when
                        rules.NestedRecoveryDiscards
                        ->
                        // The Compatibility Oracle discards the rest of the file after this recovery and reports one more diagnostic at its end.
                        suppressAfterNestedRecovery state
                    | ListRecovery.DiscardsAfterDeclaration, DeclarationList.NestedModule ->
                        // The Compatibility Oracle discards the rest of the file, also after the first declaration of the module, and reports one more diagnostic.
                        suppressAfterNestedRecovery state
                    | ListRecovery.Discards, DeclarationList.NestedModule
                    | ListRecovery.DiscardsInValue, DeclarationList.NestedModule ->
                        // The Compatibility Oracle skips the next tokens silently until a value, open, module, or namespace.
                        state.SignatureHeader <- SignatureHeader.Lost

                        suppress
                            state
                            (DeclarationAfterRecovery.SkippedInSignatureModule(state.Depth, true))
                    | ListRecovery.DiscardsInsideValueType, DeclarationList.NestedModule ->
                        state.SignatureHeader <- SignatureHeader.Lost

                        suppress
                            state
                            (DeclarationAfterRecovery.SkippedInSignatureModule(state.Depth, false))
                    | ListRecovery.Discards, _ when
                        (state.SignatureHeader
                         <> SignatureHeader.Kept)
                        ->
                        suppress
                            state
                            (DeclarationAfterRecovery.SkippedInSignatureModule(
                                state.Depth
                                + 1,
                                true
                            ))
                    | ListRecovery.Discards, _
                    | ListRecovery.DiscardsAfterDeclaration, _ ->
                        // The Compatibility Oracle discards the rest of the file after this recovery and reports no diagnostic for it.
                        suppress state (DeclarationAfterRecovery.Discarded None)
                    | ListRecovery.DiscardsInValue, _ when
                        (state.SignatureHeader
                         <> SignatureHeader.Kept)
                        ->
                        // The Compatibility Oracle keeps the lost module header, so this recovery also reports FS0222 at the end.
                        suppress
                            state
                            (DeclarationAfterRecovery.SkippedInSignatureModule(
                                state.Depth
                                + 1,
                                true
                            ))
                    | ListRecovery.DiscardsInValue, _
                    | ListRecovery.DiscardsInsideValueType, _ ->
                        // The Compatibility Oracle discards later values and opens, and reports FS0010 at the end for other declarations.
                        suppress state DeclarationAfterRecovery.DiscardedIfValueOrOpen
                    | ListRecovery.Unmodeled, _ ->
                        suppress state (DeclarationAfterRecovery.Unmodeled false)
                    | ListRecovery.Continues, _ -> ()

        match state.Recovery with
        | Recovery.Suppressing _ when scopedSuppression -> state.Recovery <- Recovery.Parsing
        | _ -> ()

        declarations.ToImmutable(), discarded.ToImmutable()

    and private parseNestedModule
        state
        (rules: DeclarationRules<'Declaration>)
        attributes
        : 'Declaration option =
        let cursor = state.Cursor
        let moduleToken = cursor.Advance()
        let accessibility = parseAccessibility cursor

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

                state.Depth <-
                    state.Depth
                    + 1

                let declarations, discarded =
                    parseDeclarations state rules DeclarationList.NestedModule

                state.Depth <-
                    state.Depth
                    - 1

                let range = span (declarationStart attributes moduleToken) (emptyAt cursor.LastEnd)

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                Some(
                    rules.NestedModule {
                        Attributes = attributes
                        Accessibility = accessibility
                        Name = name
                        Declarations = declarations
                        DiscardedByRecovery = discarded
                        Range = range
                    }
                )
            else
                reportUnsupported state cursor.Current "a module declaration"
                None

    let private parseRoots state rules =
        let cursor = state.Cursor
        let roots = ImmutableArray.CreateBuilder<ModuleOrNamespaceSyntax<_>>()

        let rootDeclarations list =
            if cursor.Current.Kind = LayoutTokenKind.BeginBlock then
                cursor.Advance()
                |> ignore

                let declarations = parseDeclarations state rules list

                if cursor.Current.Kind = LayoutTokenKind.EndBlock then
                    cursor.Advance()
                    |> ignore

                declarations
            else
                parseDeclarations state rules list

        let root kind (start: SourceRange) (declarations, discarded) = {
            Kind = kind
            Declarations = declarations
            DiscardedByRecovery = discarded
            Range = span start (emptyAt cursor.LastEnd)
        }

        if isKeyword "namespace" cursor.Current then
            while isKeyword "namespace" cursor.Current do
                reportPendingAtEnd state
                let namespaceToken = cursor.Advance()

                let name =
                    if isIdentifier cursor.Current then
                        Some(longIdentifier state)
                    else
                        reportUnsupported state cursor.Current "a namespace declaration"
                        None

                let declarations = rootDeclarations DeclarationList.NamespaceRoot

                roots.Add(
                    root (ModuleOrNamespaceKind.Namespace name) namespaceToken.Range declarations
                )
        else
            let first = cursor.Current

            let header =
                let mutable offset = 2

                while isOperator "." (cursor.Peek offset)
                      && isIdentifier (cursor.Peek(offset + 1)) do
                    offset <- offset + 2

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
                let declarations = rootDeclarations DeclarationList.ModuleRoot

                roots.Add(
                    root (ModuleOrNamespaceKind.NamedModule name) moduleToken.Range declarations
                )
            | None ->
                state.InAnonymousRoot <- true
                let declarations = rootDeclarations DeclarationList.AnonymousRoot
                roots.Add(root ModuleOrNamespaceKind.AnonymousModule first.Range declarations)

        if not (isEndOfFile cursor.Current) then
            match state.Recovery with
            | Recovery.Parsing ->
                reportUnsupported state cursor.Current "a module or namespace declaration"
            | Recovery.Suppressing(_, DeclarationAfterRecovery.ReportedAtRoot _) ->
                // The Compatibility Oracle reports FS0530 for a namespace after a named module.
                reportAfterRecovery state cursor.Current
            | Recovery.Suppressing(_, DeclarationAfterRecovery.SkippedInSignatureModule _) ->
                reportAfterRecovery state cursor.Current
            | Recovery.Suppressing _
            | Recovery.Interrupted _ -> reportPendingAtEnd state

            while not (isEndOfFile cursor.Current) do
                cursor.Advance()
                |> ignore

        reportPendingAtEnd state

        match pendingDeclaration state with
        | Some(DeclarationAfterRecovery.Unmodeled true) -> reportAfterRecovery state cursor.Current
        | _ -> ()

        roots.ToImmutable()

    let private implementationStartPoint list =
        match list with
        | DeclarationList.NamespaceRoot -> RecoveryPoint.NamespaceFile
        | DeclarationList.AnonymousRoot -> RecoveryPoint.AnonymousFile
        | DeclarationList.ModuleRoot
        | DeclarationList.NestedModule -> RecoveryPoint.DefinitionStart

    let private implementationRules = {
        Parse =
            fun state list attributes token ->
                if
                    isKeyword "let" token
                    || isKeyword "use" token
                then
                    Some(Some(parseLet state attributes), ListRecovery.Continues)
                elif isKeyword "do" token then
                    Some(Some(parseDo state attributes), ListRecovery.Continues)
                elif isKeyword "type" token then
                    Some(parseTypeDefinition state attributes)
                elif canStartExpression token then
                    let declaration, recovery =
                        parseExpressionDeclaration state (implementationStartPoint list) attributes

                    Some(Some declaration, recovery)
                else
                    None
        NestedRecoveryDiscards = true
        SkipsUnrecognizedAfterRecovery = false
        FirstNestedPoint = RecoveryPoint.NestedFirstDefinition
        StartPoint = implementationStartPoint
        Discarded =
            fun declaration ->
                match declaration with
                | ImplementationDeclaration.Expression _
                | ImplementationDeclaration.Open _
                | ImplementationDeclaration.Skipped _ -> DiscardedDeclaration.KeepsRootReport
                | ImplementationDeclaration.Let(_, _, bindings, _) when bindings.Length = 1 ->
                    DiscardedDeclaration.MakesNextIncomplete
                | ImplementationDeclaration.Do _ -> DiscardedDeclaration.MakesNextIncomplete
                | ImplementationDeclaration.Let _
                | ImplementationDeclaration.Type _
                | ImplementationDeclaration.NestedModule _ -> DiscardedDeclaration.Unmodeled
        Open = ImplementationDeclaration.Open
        NestedModule = ImplementationDeclaration.NestedModule
        Skipped = ImplementationDeclaration.Skipped
    }

    let private signatureRules = {
        Parse =
            fun state list attributes token ->
                if isKeyword "val" token then
                    Some(
                        parseVal
                            state
                            (list = DeclarationList.NestedModule
                             || (state.SignatureHeader
                                 <> SignatureHeader.Kept))
                            attributes
                    )
                else
                    None
        NestedRecoveryDiscards = false
        SkipsUnrecognizedAfterRecovery = true
        FirstNestedPoint = RecoveryPoint.NestedFirstSignature
        Discarded = fun _ -> DiscardedDeclaration.Unmodeled
        StartPoint =
            fun list ->
                match list with
                | DeclarationList.NestedModule -> RecoveryPoint.NestedSignature
                | DeclarationList.AnonymousRoot -> RecoveryPoint.AnonymousSignature
                | DeclarationList.ModuleRoot
                | DeclarationList.NamespaceRoot -> RecoveryPoint.SignatureFile
        Open = SignatureDeclaration.Open
        NestedModule = SignatureDeclaration.NestedModule
        Skipped = SignatureDeclaration.Skipped
    }

    let private start (document: LexicalDocument) = {
        Cursor = Cursor(document.LayoutTokens)
        Diagnostics = ResizeArray()
        ReportedStarts = HashSet()
        Language = document.LanguageVersion
        Recovery = Recovery.Parsing
        InAnonymousRoot = false
        Depth = 0
        SignatureHeader = SignatureHeader.Kept
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
            UnresumedRecoveryAtEnd =
                match state.SignatureHeader with
                | SignatureHeader.LostAtEnd range -> Some range
                | SignatureHeader.Kept
                | SignatureHeader.Lost -> None
        }

    let private missingDeclarationMessage =
        "Files in libraries or multiple-file applications must begin with a namespace or module declaration, e.g. 'namespace SomeNamespace.SubNamespace' or 'module SomeNamespace.SomeModule'. Only the last source file of an application may omit such a declaration."

    let private moduleEqualsDeclarationMessage =
        "Files in libraries or multiple-file applications must begin with a namespace or module declaration. When using a module declaration at the start of a file the '=' sign is not allowed. If this is a top-level module, consider removing the = to resolve this error."

    let private implicitModuleMessage moduleName fileName =
        $"The declarations in this file will be placed in an implicit module '{moduleName}' based on the file name '{fileName}'. However this is not a valid F# identifier, so the contents will not be accessible from other files. Consider renaming the file or adding a 'module' or 'namespace' declaration at the top of the file."

    type private AnonymousRoot = {
        Range: SourceRange
        HasDeclarations: bool
        StartsWithNestedModule: bool
    }

    let private nextToken (document: LexicalDocument) offset =
        document.LayoutTokens
        |> Seq.find (fun token ->
            token.Kind = LayoutTokenKind.SourceToken
            && token.Range.Start.Offset
               >= offset
        )

    let private nextTokenStart document offset = (nextToken document offset).Range.Start

    let private anonymousRoot (contents: ImmutableArray<ModuleOrNamespaceSyntax<_>>) =
        contents
        |> Seq.tryFind (fun root ->
            match root.Kind with
            | ModuleOrNamespaceKind.AnonymousModule -> true
            | ModuleOrNamespaceKind.NamedModule _
            | ModuleOrNamespaceKind.Namespace _ -> false
        )

    let private anonymousImplementation
        (document: LexicalDocument)
        (root: ModuleOrNamespaceSyntax<ImplementationDeclaration>)
        =
        let first = nextToken document 0
        let start = first.Range.Start

        let kept =
            root.Declarations
            |> Seq.filter (fun declaration ->
                match declaration with
                | ImplementationDeclaration.Skipped _ -> false
                | _ -> true
            )
            |> Seq.toList

        let range =
            match List.tryLast kept with
            | None when isEndOfFile first -> emptyAt start
            | None -> first.Range
            | Some last ->
                let finish =
                    match last with
                    | ImplementationDeclaration.Open(_, range)
                    | ImplementationDeclaration.Expression(_, _, _, range) -> range.End
                    | _ -> nextTokenStart document last.Range.End.Offset

                if finish.Line > start.Line then
                    {
                        Start = start
                        End =
                            SourceMap.positionAt
                                document.SourceMap
                                document.SourceMap.LineStarts[start.Line]
                    }
                else
                    { Start = start; End = finish }

        {
            Range = range
            HasDeclarations = not kept.IsEmpty
            StartsWithNestedModule =
                match Seq.tryHead root.Declarations with
                | Some(ImplementationDeclaration.NestedModule _) -> true
                | _ -> false
        }

    let private anonymousSignature
        (document: LexicalDocument)
        (root: ModuleOrNamespaceSyntax<SignatureDeclaration>)
        =
        let first = nextToken document 0
        let start = first.Range.Start

        let hasDeclarations =
            root.Declarations
            |> Seq.exists (fun declaration ->
                match declaration with
                | SignatureDeclaration.Skipped _ -> false
                | _ -> true
            )

        let range =
            match Seq.tryLast root.Declarations with
            | _ when
                not hasDeclarations
                && isEndOfFile first
                ->
                emptyAt start
            | _ when not hasDeclarations -> first.Range
            | None -> emptyAt start
            | Some(SignatureDeclaration.Skipped skipped) -> {
                Start = start
                End = skipped.Range.End
              }
            | Some last -> {
                Start = start
                End = nextTokenStart document last.Range.End.Offset
              }

        {
            Range = range
            HasDeclarations = hasDeclarations
            StartsWithNestedModule =
                match Seq.tryHead root.Declarations with
                | Some(SignatureDeclaration.NestedModule _) -> true
                | _ -> false
        }

    let private invalidImplicitModuleName (logicalPath: string) =
        let fileName = IO.Path.GetFileName logicalPath
        let stem = IO.Path.GetFileNameWithoutExtension fileName

        let moduleName =
            if stem.Length = 0 then
                stem
            else
                string (Char.ToUpperInvariant stem[0])
                + stem.Substring 1

        if
            moduleName
            |> Seq.forall (fun character ->
                Char.IsLetterOrDigit character
                || character = '_'
            )
        then
            None
        else
            Some(moduleName, fileName)

    let parseCompilation (target: SyntaxCompilationTarget) (sources: ImmutableArray<SyntaxSource>) =
        let files = ImmutableArray.CreateBuilder<SyntaxFile>()
        let diagnostics = ImmutableArray.CreateBuilder<SyntaxFileDiagnostic>()

        sources
        |> Seq.iteri (fun index source ->
            let document = source.Document

            let file, fileDiagnostics, anonymous, unresumedAtEnd =
                if source.Kind = SyntaxSourceKind.Signature then
                    let result = parseSignatureFile document

                    SyntaxFile.Signature result.File,
                    result.Diagnostics,
                    anonymousRoot result.File.Contents
                    |> Option.map (anonymousSignature document),
                    result.UnresumedRecoveryAtEnd
                else
                    let result = parseImplementationFile document

                    SyntaxFile.Implementation result.File,
                    result.Diagnostics,
                    anonymousRoot result.File.Contents
                    |> Option.map (anonymousImplementation document),
                    None

            let add diagnostic =
                diagnostics.Add {
                    LogicalPath = document.LogicalPath
                    Diagnostic = diagnostic
                }

            files.Add file
            Seq.iter add fileDiagnostics

            let unmodeled =
                fileDiagnostics
                |> Seq.exists (fun diagnostic -> diagnostic.Code = unsupportedCode)

            let requiresDeclaration =
                match target with
                | SyntaxCompilationTarget.Library -> true
                | SyntaxCompilationTarget.Executable ->
                    index < sources.Length
                            - 1

            match source.Kind, anonymous, unresumedAtEnd with
            | SyntaxSourceKind.Script, _, _ -> ()
            | _, None, Some range when
                requiresDeclaration
                && not unmodeled
                ->
                // The Compatibility Oracle loses the module header when a nested signature recovery reaches the end of input.
                add {
                    Severity = DiagnosticSeverity.Error
                    Code = "FS0222"
                    Message = missingDeclarationMessage
                    Range = range
                }
            | _, None, _ -> ()
            | _, Some _, _ when unmodeled -> ()
            | _, Some anonymous, _ ->

                if requiresDeclaration then
                    add {
                        Severity = DiagnosticSeverity.Error
                        Code = "FS0222"
                        Message =
                            if anonymous.StartsWithNestedModule then
                                moduleEqualsDeclarationMessage
                            else
                                missingDeclarationMessage
                        Range = anonymous.Range
                    }

                match invalidImplicitModuleName document.LogicalPath with
                | Some(moduleName, fileName) when anonymous.HasDeclarations ->
                    add {
                        Severity = DiagnosticSeverity.Warning
                        Code = "FS0221"
                        Message = implicitModuleMessage moduleName fileName
                        Range = anonymous.Range
                    }
                | _ -> ()
        )

        {
            Files = files.ToImmutable()
            Diagnostics = diagnostics.ToImmutable()
        }
