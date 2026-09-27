namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Security.Cryptography
open System.Text

module internal Frontend =
    [<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
    type private TokenKind =
        | ModuleKeyword
        | NamespaceKeyword
        | OpenKeyword
        | AssemblyKeyword
        | InternalKeyword
        | TypeKeyword
        | StaticKeyword
        | InlineKeyword
        | InheritKeyword
        | NewKeyword
        | OverrideKeyword
        | TryKeyword
        | MatchKeyword
        | AsKeyword
        | WithKeyword
        | WhenKeyword
        | AndKeyword
        | MemberKeyword
        | NullKeyword
        | LetKeyword
        | DoKeyword
        | ValKeyword
        | MutableKeyword
        | FunKeyword
        | IfKeyword
        | ThenKeyword
        | ElseKeyword
        | NotKeyword
        | Identifier of string
        | TypeParameter of string
        | Integer of int
        | StringLiteralToken of string
        | AttributeStart
        | AttributeEnd
        | LeftParenthesis
        | RightParenthesis
        | LeftBrace
        | RightBrace
        | LeftBracket
        | RightBracket
        | Colon
        | TypeTest
        | Subtype
        | Comma
        | Semicolon
        | Dot
        | LessThan
        | GreaterThan
        | Arrow
        | LeftArrow
        | PipeLeft
        | PipeRight
        | Ampersand
        | Bang
        | Hash
        | Star
        | Equals
        | Bar
        | EndOfFile

        override _.ToString() = "TokenKind"

    type private Token = {
        Kind: TokenKind
        Range: SourceRange
    } with

        override _.ToString() = "Token"

    type private ConditionalFrame = {
        ParentIsActive: bool
        ConditionIsTrue: bool
        HasElse: bool
        IfRange: SourceRange
    } with

        override _.ToString() = "ConditionalFrame"

    type private ParseResultBuilder() =
        member _.Bind(result, continuation) = Result.bind continuation result
        member _.Return(value) = Ok value
        member _.ReturnFrom(result) = result

    let private parseResult = ParseResultBuilder()

    let private diagnostic code path range message = {
        Code = code
        Message = message
        Path = Some path
        Range = Some range
    }

    let private prototypeDiagnostic path range message =
        diagnostic "FSC2P1001" path range message

    let private preprocessConditionals (defines: Set<string>) (source: SourceInput) =
        let text = source.Text
        let output = StringBuilder(text)
        let mutable frames: ConditionalFrame list = []
        let mutable isActive = true
        let mutable lineStart = 0
        let mutable line = 1
        let mutable error: CompilerDiagnostic option = None

        let range startOffset startColumn endOffset endColumn = {
            Start = {
                Offset = startOffset
                Line = line
                Column = startColumn
            }
            End = {
                Offset = endOffset
                Line = line
                Column = endColumn
            }
        }

        let blank startOffset endOffset =
            for offset in
                startOffset .. endOffset
                               - 1 do
                if
                    output.[offset]
                    <> '\r'
                    && output.[offset]
                       <> '\n'
                then
                    output.[offset] <- ' '

        let validDefineName (value: string) =
            value.Length > 0
            && (Char.IsLetter(value.[0])
                || value.[0] = '_')
            && (value
                |> Seq.forall (fun character ->
                    Char.IsLetterOrDigit(character)
                    || character = '_'
                ))

        while lineStart < text.Length
              && error.IsNone do
            let newline = text.IndexOf('\n', lineStart)

            let lineEnd = if newline < 0 then text.Length else newline

            let contentEnd =
                if
                    lineEnd > lineStart
                    && text.[lineEnd
                             - 1] = '\r'
                then
                    lineEnd
                    - 1
                else
                    lineEnd

            let lineText =
                text.Substring(
                    lineStart,
                    contentEnd
                    - lineStart
                )

            let trimmed = lineText.TrimStart()

            let leading =
                lineText.Length
                - trimmed.Length

            let directiveOffset =
                lineStart
                + leading

            let directiveColumn =
                leading
                + 1

            let directiveRange =
                range
                    directiveOffset
                    directiveColumn
                    contentEnd
                    (contentEnd
                     - lineStart
                     + 1)

            if
                trimmed.StartsWith("#if", StringComparison.Ordinal)
                && (trimmed.Length = 3
                    || Char.IsWhiteSpace(trimmed.[3]))
            then
                blank lineStart contentEnd
                let define = trimmed.Substring(3).Trim()

                if not (validDefineName define) then
                    error <-
                        Some(
                            prototypeDiagnostic
                                source.Path
                                directiveRange
                                "expected a conditional-compilation symbol after '#if'"
                        )
                else
                    let conditionIsTrue = defines.Contains(define)

                    frames <-
                        {
                            ParentIsActive = isActive
                            ConditionIsTrue = conditionIsTrue
                            HasElse = false
                            IfRange = directiveRange
                        }
                        :: frames

                    isActive <-
                        isActive
                        && conditionIsTrue
            elif trimmed = "#else" then
                blank lineStart contentEnd

                match frames with
                | [] ->
                    error <-
                        Some(
                            prototypeDiagnostic
                                source.Path
                                directiveRange
                                "'#else' has no matching '#if'"
                        )
                | frame :: tail when frame.HasElse ->
                    error <-
                        Some(
                            prototypeDiagnostic
                                source.Path
                                directiveRange
                                "a conditional-compilation block can contain only one '#else'"
                        )
                | frame :: tail ->
                    frames <-
                        { frame with HasElse = true }
                        :: tail

                    isActive <-
                        frame.ParentIsActive
                        && not frame.ConditionIsTrue
            elif trimmed = "#endif" then
                blank lineStart contentEnd

                match frames with
                | [] ->
                    error <-
                        Some(
                            prototypeDiagnostic
                                source.Path
                                directiveRange
                                "'#endif' has no matching '#if'"
                        )
                | frame :: tail ->
                    frames <- tail
                    isActive <- frame.ParentIsActive
            elif not isActive then
                blank lineStart contentEnd

            if newline < 0 then
                lineStart <- text.Length
            else
                lineStart <-
                    newline
                    + 1

                line <- line + 1

        match error, frames with
        | Some diagnostic, _ -> Error diagnostic
        | None, frame :: _ ->
            Error(
                prototypeDiagnostic
                    source.Path
                    frame.IfRange
                    "conditional-compilation block is missing '#endif'"
            )
        | None, [] -> Ok(output.ToString())

    let private tokenize (source: SourceInput) =
        let tokens = ResizeArray<Token>()
        let text = source.Text
        let mutable offset = 0
        let mutable line = 1
        let mutable column = 1
        let mutable tokenizationError: CompilerDiagnostic option = None

        let position () = {
            Offset = offset
            Line = line
            Column = column
        }

        let advance () =
            if text.[offset] = '\n' then
                line <- line + 1
                column <- 1
            else
                column <- column + 1

            offset <- offset + 1

        let add kind start =
            tokens.Add {
                Kind = kind
                Range = { Start = start; End = position () }
            }

        while offset < text.Length
              && tokenizationError.IsNone do
            let current = text.[offset]

            if Char.IsWhiteSpace(current) then
                advance ()
            elif
                current = '/'
                && offset + 1 < text.Length
                && text.[offset + 1] = '/'
            then
                while offset < text.Length
                      && text.[offset]
                         <> '\n' do
                    advance ()
            elif
                current = '\''
                && offset + 1 < text.Length
                && (Char.IsLetter(text.[offset + 1])
                    || text.[offset + 1] = '_')
            then
                let start = position ()
                advance ()
                let first = offset

                while offset < text.Length
                      && (Char.IsLetterOrDigit(text.[offset])
                          || text.[offset] = '_'
                          || text.[offset] = '\'') do
                    advance ()

                add
                    (TypeParameter(
                        text.Substring(
                            first,
                            offset
                            - first
                        )
                    ))
                    start
            elif
                Char.IsLetter(current)
                || current = '_'
            then
                let start = position ()
                let first = offset

                while offset < text.Length
                      && (Char.IsLetterOrDigit(text.[offset])
                          || text.[offset] = '_'
                          || text.[offset] = '\'') do
                    advance ()

                let value =
                    text.Substring(
                        first,
                        offset
                        - first
                    )

                match value with
                | "module" -> add ModuleKeyword start
                | "namespace" -> add NamespaceKeyword start
                | "open" -> add OpenKeyword start
                | "assembly" -> add AssemblyKeyword start
                | "internal" -> add InternalKeyword start
                | "type" -> add TypeKeyword start
                | "static" -> add StaticKeyword start
                | "inline" -> add InlineKeyword start
                | "inherit" -> add InheritKeyword start
                | "new" -> add NewKeyword start
                | "override" -> add OverrideKeyword start
                | "try" -> add TryKeyword start
                | "match" -> add MatchKeyword start
                | "as" -> add AsKeyword start
                | "with" -> add WithKeyword start
                | "when" -> add WhenKeyword start
                | "and" -> add AndKeyword start
                | "member" -> add MemberKeyword start
                | "null" -> add NullKeyword start
                | "let" -> add LetKeyword start
                | "do" -> add DoKeyword start
                | "val" -> add ValKeyword start
                | "mutable" -> add MutableKeyword start
                | "fun" -> add FunKeyword start
                | "if" -> add IfKeyword start
                | "then" -> add ThenKeyword start
                | "else" -> add ElseKeyword start
                | "not" -> add NotKeyword start
                | _ -> add (Identifier value) start
            elif Char.IsDigit(current) then
                let start = position ()
                let first = offset

                while offset < text.Length
                      && Char.IsDigit(text.[offset]) do
                    advance ()

                let value =
                    Int32.Parse(
                        text.Substring(
                            first,
                            offset
                            - first
                        )
                    )

                add (Integer value) start
            elif current = '"' then
                let start = position ()
                advance ()
                let first = offset

                while offset < text.Length
                      && text.[offset]
                         <> '"'
                      && text.[offset]
                         <> '\n'
                      && text.[offset]
                         <> '\r' do
                    advance ()

                if
                    offset < text.Length
                    && text.[offset] = '"'
                then
                    let value =
                        text.Substring(
                            first,
                            offset
                            - first
                        )

                    advance ()
                    add (StringLiteralToken value) start
                else
                    tokenizationError <-
                        Some(
                            prototypeDiagnostic
                                source.Path
                                { Start = start; End = position () }
                                "unterminated string literal"
                        )
            else
                let start = position ()

                if
                    current = '['
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '<'
                then
                    advance ()
                    advance ()
                    add AttributeStart start
                elif
                    current = '>'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = ']'
                then
                    advance ()
                    advance ()
                    add AttributeEnd start
                elif
                    current = ':'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '?'
                then
                    advance ()
                    advance ()
                    add TypeTest start
                elif
                    current = ':'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '>'
                then
                    advance ()
                    advance ()
                    add Subtype start
                elif
                    current = '-'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '>'
                then
                    advance ()
                    advance ()
                    add Arrow start
                elif
                    current = '<'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '|'
                then
                    advance ()
                    advance ()
                    add PipeLeft start
                elif
                    current = '<'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '-'
                then
                    advance ()
                    advance ()
                    add LeftArrow start
                elif
                    current = '|'
                    && offset + 1 < text.Length
                    && text.[offset + 1] = '>'
                then
                    advance ()
                    advance ()
                    add PipeRight start
                else
                    advance ()

                    match current with
                    | '(' -> add LeftParenthesis start
                    | ')' -> add RightParenthesis start
                    | '{' -> add LeftBrace start
                    | '}' -> add RightBrace start
                    | '[' -> add LeftBracket start
                    | ']' -> add RightBracket start
                    | ':' -> add Colon start
                    | ',' -> add Comma start
                    | ';' -> add Semicolon start
                    | '.' -> add Dot start
                    | '<' -> add LessThan start
                    | '>' -> add GreaterThan start
                    | '&' -> add Ampersand start
                    | '!' -> add Bang start
                    | '#' -> add Hash start
                    | '*' -> add Star start
                    | '=' -> add Equals start
                    | '|' -> add Bar start
                    | _ ->
                        tokenizationError <-
                            Some(
                                prototypeDiagnostic
                                    source.Path
                                    { Start = start; End = position () }
                                    "unsupported token"
                            )

        match tokenizationError with
        | Some error -> Error error
        | None ->
            let finalPosition = position ()

            tokens.Add {
                Kind = EndOfFile
                Range = {
                    Start = finalPosition
                    End = finalPosition
                }
            }

            Ok(List.ofSeq tokens)

    let parse (defines: string list) (source: SourceInput) =
        let sourceChecksum =
            source.Text
            |> Encoding.UTF8.GetBytes
            |> SHA256.HashData

        let defines =
            defines
            |> Set.ofList

        let preprocessingResult = preprocessConditionals defines source

        let fingerprintContent =
            match preprocessingResult with
            | Ok preprocessedText -> preprocessedText
            | Error _ -> source.Text

        let contentFingerprint =
            fingerprintContent
            |> Encoding.UTF8.GetBytes
            |> SHA256.HashData
            |> Convert.ToHexString
            |> fun value -> value.ToLowerInvariant()

        let tokenizationResult =
            match preprocessingResult with
            | Error error -> Error error
            | Ok preprocessedText -> tokenize { source with Text = preprocessedText }

        match tokenizationResult with
        | Error error -> Error error
        | Ok tokens ->
            let input = List.toArray tokens
            let mutable index = 0

            let current () = input.[index]

            let consume () =
                let token = current ()
                index <- index + 1
                token

            let expected token message =
                if (current ()).Kind = token then
                    Ok(consume ())
                else
                    Error(prototypeDiagnostic source.Path (current ()).Range message)

            let identifier message =
                let token = consume ()

                match token.Kind with
                | Identifier value -> Ok(value, token)
                | _ -> Error(prototypeDiagnostic source.Path token.Range message)

            let qualifiedIdentifier message =
                let rec remaining parts =
                    match (current ()).Kind with
                    | Dot ->
                        consume ()
                        |> ignore

                        match identifier message with
                        | Error error -> Error error
                        | Ok(value, _) ->
                            remaining (
                                value
                                :: parts
                            )
                    | _ ->
                        parts
                        |> List.rev
                        |> String.concat "."
                        |> Ok

                match identifier message with
                | Error error -> Error error
                | Ok(value, _) -> remaining [ value ]

            let qualifiedTypeName (value: string) =
                let separator = value.LastIndexOf('.')

                if separator < 0 then
                    {
                        Namespace = String.Empty
                        Name = value
                    }
                else
                    {
                        Namespace = value.Substring(0, separator)
                        Name =
                            value.Substring(
                                separator
                                + 1
                            )
                    }

            let typeParameter message =
                let token = consume ()

                match token.Kind with
                | TypeParameter value -> Ok(value, token)
                | _ -> Error(prototypeDiagnostic source.Path token.Range message)

            let rec parseTypeExpression () =
                parseResult {
                    let! firstAtom = parseTypeAtom ()

                    let rec parsePostfixTypeApplications appliedType =
                        match (current ()).Kind with
                        | Identifier "ValueOption"
                        | Identifier "option"
                        | Identifier "voption"
                        | Identifier "list"
                        | Identifier "array"
                        | Identifier "seq" ->
                            let typeConstructor = consume ()

                            let constructorName =
                                match typeConstructor.Kind with
                                | Identifier name -> name
                                | _ -> invalidOp "the postfix type constructor changed kind"

                            let constructor =
                                ParsedNamedType(
                                    {
                                        Namespace = String.Empty
                                        Name = constructorName
                                    },
                                    typeConstructor.Range
                                )

                            parsePostfixTypeApplications (
                                ParsedGenericTypeApplication(
                                    constructor,
                                    [ appliedType ],
                                    {
                                        Start = appliedType.Range.Start
                                        End = typeConstructor.Range.End
                                    }
                                )
                            )
                        | _ -> appliedType

                    let first = parsePostfixTypeApplications firstAtom

                    let rec parseTupleElements elements =
                        match (current ()).Kind with
                        | Star ->
                            consume ()
                            |> ignore

                            parseTypeAtom ()
                            |> Result.bind (fun element ->
                                parseTupleElements (
                                    element
                                    :: elements
                                )
                            )
                        | _ ->
                            match List.rev elements with
                            | [ element ] -> Ok element
                            | orderedElements ->
                                Ok(
                                    ParsedTupleType(
                                        orderedElements,
                                        {
                                            Start = first.Range.Start
                                            End = (List.last orderedElements).Range.End
                                        }
                                    )
                                )

                    let! left = parseTupleElements [ first ]

                    return!
                        match (current ()).Kind with
                        | Arrow ->
                            consume ()
                            |> ignore

                            parseTypeExpression ()
                            |> Result.map (fun right ->
                                ParsedFunctionType(
                                    left,
                                    right,
                                    {
                                        Start = left.Range.Start
                                        End = right.Range.End
                                    }
                                )
                            )
                        | _ -> Ok left
                }

            and parseTypeAtom () =
                parseResult {
                    let! atom =
                        match (current ()).Kind with
                        | TypeParameter value ->
                            let token = consume ()
                            Ok(ParsedTypeParameter(value, token.Range))
                        | Identifier "_" ->
                            let token = consume ()
                            Ok(ParsedWildcardType token.Range)
                        | LeftParenthesis ->
                            parseResult {
                                let! _ = expected LeftParenthesis "expected '('"
                                let! groupedType = parseTypeExpression ()
                                let! _ = expected RightParenthesis "expected ')' after a type"
                                return groupedType
                            }
                        | Hash ->
                            let hashToken = consume ()

                            parseTypeAtom ()
                            |> Result.map (fun superType ->
                                ParsedFlexibleType(
                                    superType,
                                    {
                                        Start = hashToken.Range.Start
                                        End = superType.Range.End
                                    }
                                )
                            )
                        | Identifier _ ->
                            let start = (current ()).Range.Start

                            qualifiedIdentifier "expected a type"
                            |> Result.map (fun name ->
                                ParsedNamedType(
                                    qualifiedTypeName name,
                                    {
                                        Start = start
                                        End = input.[index - 1].Range.End
                                    }
                                )
                            )
                        | _ ->
                            Error(
                                prototypeDiagnostic source.Path (current ()).Range "expected a type"
                            )

                    return!
                        match (current ()).Kind with
                        | LessThan ->
                            consume ()
                            |> ignore

                            parseTypeArguments []
                            |> Result.bind (fun arguments ->
                                expected GreaterThan "expected '>'"
                                |> Result.map (fun closeToken ->
                                    ParsedGenericTypeApplication(
                                        atom,
                                        arguments,
                                        {
                                            Start = atom.Range.Start
                                            End = closeToken.Range.End
                                        }
                                    )
                                )
                            )
                        | _ -> Ok atom
                }

            and parseTypeArguments arguments =
                parseResult {
                    let! argument = parseTypeExpression ()

                    let arguments =
                        argument
                        :: arguments

                    return!
                        match (current ()).Kind with
                        | Comma ->
                            consume ()
                            |> ignore

                            parseTypeArguments arguments
                        | GreaterThan -> Ok(List.rev arguments)
                        | _ ->
                            Error(
                                prototypeDiagnostic
                                    source.Path
                                    (current ()).Range
                                    "expected ',' or '>' after a type argument"
                            )
                }

            let rec parseExpression () =
                let expressionToken = current ()

                let sequenceExpressions expressions =
                    let expressions = List.rev expressions

                    match expressions with
                    | [ expression, range ] -> expression, range
                    | _ ->
                        let _, firstRange = List.head expressions
                        let _, lastRange = List.last expressions

                        SequentialValueExpression expressions,
                        {
                            Start = firstRange.Start
                            End = lastRange.End
                        }

                let parseComputationBindingName () =
                    parseResult {
                        let wrapped =
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                consume ()
                                |> ignore

                                true
                            | _ -> false

                        let! bindingName, bindingToken =
                            identifier "expected a computation binding name"

                        let! _bindingType =
                            match (current ()).Kind with
                            | Colon ->
                                consume ()
                                |> ignore

                                parseTypeExpression ()
                                |> Result.map Some
                            | _ -> Ok None

                        let! _ =
                            if wrapped then
                                expected RightParenthesis "expected ')' after a computation binding"
                                |> Result.map ignore
                            else
                                Ok()

                        return bindingName, bindingToken
                    }

                let hasComputationBindingBeforeRightBrace startIndex =
                    let rec loop tokenIndex =
                        if
                            tokenIndex
                            + 1
                            >= input.Length
                        then
                            false
                        else
                            match input.[tokenIndex].Kind with
                            | RightBrace
                            | EndOfFile -> false
                            | LetKeyword when
                                input
                                    .[tokenIndex
                                      + 1].Kind = Bang
                                ->
                                true
                            | _ ->
                                loop (
                                    tokenIndex
                                    + 1
                                )

                    loop startIndex

                let hasOrdinaryBindingBeforeRightBrace startIndex =
                    let rec loop tokenIndex =
                        if
                            tokenIndex
                            + 1
                            >= input.Length
                        then
                            false
                        else
                            match input.[tokenIndex].Kind with
                            | RightBrace
                            | EndOfFile -> false
                            | LetKeyword when
                                input
                                    .[tokenIndex
                                      + 1].Kind
                                <> Bang
                                ->
                                true
                            | Identifier "use" -> true
                            | _ ->
                                loop (
                                    tokenIndex
                                    + 1
                                )

                    loop startIndex

                let hasForBeforeRightBrace startIndex =
                    let rec loop tokenIndex =
                        if
                            tokenIndex
                            >= input.Length
                        then
                            false
                        else
                            match input.[tokenIndex].Kind with
                            | RightBrace
                            | EndOfFile -> false
                            | Identifier "for" -> true
                            | _ ->
                                loop (
                                    tokenIndex
                                    + 1
                                )

                    loop startIndex

                let parseCallArguments () =
                    let rec loop arguments =
                        parseResult {
                            let! argument =
                                match (current ()).Kind with
                                | Identifier argumentName when
                                    index + 1 < input.Length
                                    && input.[index + 1].Kind = Equals
                                    ->
                                    parseResult {
                                        let nameToken = consume ()
                                        let! _ = expected Equals "expected '='"
                                        let! value, valueRange = parseExpression ()

                                        return
                                            NamedCallArgument(
                                                argumentName,
                                                value,
                                                {
                                                    Start = nameToken.Range.Start
                                                    End = valueRange.End
                                                }
                                            )
                                    }
                                | Ampersand ->
                                    consume ()
                                    |> ignore

                                    parseResult {
                                        let! rootName, _ =
                                            identifier "expected an address-of expression"

                                        let rec parseMemberPath members =
                                            match (current ()).Kind with
                                            | Dot ->
                                                parseResult {
                                                    let! _ = expected Dot "expected '.'"

                                                    let! memberName, _ =
                                                        identifier
                                                            "expected a member name after '.'"

                                                    return!
                                                        parseMemberPath (
                                                            memberName
                                                            :: members
                                                        )
                                                }
                                            | _ -> Ok(List.rev members)

                                        let! memberPath = parseMemberPath []
                                        return AddressOfExpression(rootName, memberPath)
                                    }
                                | _ ->
                                    parseResult {
                                        let! firstExpression, _ = parseExpression ()

                                        let rec parseApplications expression =
                                            match (current ()).Kind with
                                            | Comma
                                            | RightParenthesis -> Ok expression
                                            | _ ->
                                                parseExpression ()
                                                |> Result.bind (fun (argument, _) ->
                                                    parseApplications (
                                                        FunctionApplication(expression, argument)
                                                    )
                                                )

                                        return! parseApplications firstExpression
                                    }

                            let arguments =
                                argument
                                :: arguments

                            return!
                                match (current ()).Kind with
                                | Comma ->
                                    consume ()
                                    |> ignore

                                    loop arguments
                                | RightParenthesis -> Ok(List.rev arguments)
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected ',' or ')' after a trait-call argument"
                                    )
                        }

                    match (current ()).Kind with
                    | RightParenthesis -> Ok []
                    | _ -> loop []

                let parseMemberPath rootName =
                    let rec loop members =
                        parseResult {
                            let! _ = expected Dot "expected '.'"
                            let! memberName, _ = identifier "expected a member name"

                            let members =
                                memberName
                                :: members

                            return!
                                match (current ()).Kind with
                                | Dot when
                                    index + 1 < input.Length
                                    && input.[index + 1].Kind = LeftBracket
                                    ->
                                    Ok(List.rev members)
                                | Dot -> loop members
                                | _ -> Ok(List.rev members)
                        }

                    loop []
                    |> Result.map (fun members -> rootName, members)

                let rec parsePostfixMemberCalls expression expressionRange =
                    match (current ()).Kind with
                    | Dot when
                        index + 1 < input.Length
                        && input.[index + 1].Kind = LeftBracket
                        ->
                        parseResult {
                            let! _ = expected Dot "expected '.'"
                            let! _ = expected LeftBracket "expected '[' after '.'"
                            let! argument, _ = parseExpression ()
                            let! closeToken = expected RightBracket "expected ']'"

                            let range = {
                                Start = expressionRange.Start
                                End = closeToken.Range.End
                            }

                            return!
                                parsePostfixMemberCalls
                                    (ExpressionMemberCall(expression, "get_Item", [ argument ]))
                                    range
                        }
                    | Dot ->
                        parseResult {
                            let! _ = expected Dot "expected '.'"
                            let! memberName, _ = identifier "expected a member name"

                            let! _ = expected LeftParenthesis "expected '(' after a member name"

                            let! arguments = parseCallArguments ()

                            let! closeToken =
                                expected RightParenthesis "expected ')' after member arguments"

                            let range = {
                                Start = expressionRange.Start
                                End = closeToken.Range.End
                            }

                            return!
                                parsePostfixMemberCalls
                                    (ExpressionMemberCall(expression, memberName, arguments))
                                    range
                        }
                    | PipeLeft ->
                        consume ()
                        |> ignore

                        parseExpression ()
                        |> Result.bind (fun (argument, argumentRange) ->
                            parsePostfixMemberCalls (FunctionApplication(expression, argument)) {
                                Start = expressionRange.Start
                                End = argumentRange.End
                            }
                        )
                    | PipeRight ->
                        let pipeToken = consume ()

                        match (current ()).Kind with
                        | Identifier "ignore" ->
                            let ignoreToken = consume ()

                            let range = {
                                Start = expressionRange.Start
                                End = ignoreToken.Range.End
                            }

                            parsePostfixMemberCalls
                                (SequentialValueExpression [
                                    expression, expressionRange
                                    UnitLiteral, ignoreToken.Range
                                ])
                                range
                        | Identifier _ when
                            index + 1 < input.Length
                            && input.[index + 1].Kind = LessThan
                            ->
                            parseResult {
                                let! constructedType = parseTypeExpression ()

                                let range = {
                                    Start = expressionRange.Start
                                    End = constructedType.Range.End
                                }

                                return!
                                    parsePostfixMemberCalls
                                        (TypeConstruction(
                                            constructedType,
                                            [ expression ],
                                            expressionRange
                                        ))
                                        range
                            }
                        | Identifier receiverName when
                            index + 1 < input.Length
                            && input.[index + 1].Kind = Dot
                            ->
                            let receiverToken = consume ()

                            parseResult {
                                let! _, memberPath = parseMemberPath receiverName
                                let memberToken = input.[index - 1]

                                let! explicitArguments, targetEnd =
                                    match (current ()).Kind with
                                    | LeftParenthesis ->
                                        parseResult {
                                            let! _ = expected LeftParenthesis "expected '('"
                                            let! arguments = parseCallArguments ()

                                            let! closeToken =
                                                expected RightParenthesis "expected ')'"

                                            return arguments, closeToken.Range.End
                                        }
                                    | _ -> Ok([], memberToken.Range.End)

                                let targetRange = {
                                    Start = receiverToken.Range.Start
                                    End = targetEnd
                                }

                                let pipelineName =
                                    "$pipeline@"
                                    + pipeToken.Range.Start.Line.ToString(
                                        Globalization.CultureInfo.InvariantCulture
                                    )
                                    + "_"
                                    + pipeToken.Range.Start.Column.ToString(
                                        Globalization.CultureInfo.InvariantCulture
                                    )

                                let calledExpression =
                                    match memberPath with
                                    | [ memberName ] ->
                                        MemberCall(
                                            receiverName,
                                            memberName,
                                            explicitArguments
                                            @ [ ValueReference pipelineName ]
                                        )
                                    | _ ->
                                        let memberName = List.last memberPath

                                        let receiver =
                                            memberPath
                                            |> List.take (
                                                memberPath.Length
                                                - 1
                                            )
                                            |> List.fold
                                                (fun receiverExpression name ->
                                                    ExpressionMemberAccess(
                                                        receiverExpression,
                                                        name
                                                    )
                                                )
                                                (ValueReference receiverName)

                                        ExpressionMemberCall(
                                            receiver,
                                            memberName,
                                            explicitArguments
                                            @ [ ValueReference pipelineName ]
                                        )

                                let pipedExpression =
                                    LetExpression(
                                        pipelineName,
                                        false,
                                        false,
                                        expression,
                                        calledExpression,
                                        expressionRange,
                                        targetRange
                                    )

                                return!
                                    parsePostfixMemberCalls pipedExpression {
                                        Start = expressionRange.Start
                                        End = targetEnd
                                    }
                            }
                        | Identifier functionName ->
                            let functionToken = consume ()

                            parsePostfixMemberCalls
                                (FunctionApplication(ValueReference functionName, expression))
                                {
                                    Start = expressionRange.Start
                                    End = functionToken.Range.End
                                }
                        | _ ->
                            Error(
                                prototypeDiagnostic
                                    source.Path
                                    (current ()).Range
                                    "expected a function after '|>'"
                            )
                    | _ -> Ok(expression, expressionRange)

                match expressionToken.Kind with
                | TryKeyword ->
                    parseResult {
                        let sequenceExpression expressions =
                            let expressions = List.rev expressions

                            match expressions with
                            | [ expression, range ] -> expression, range
                            | _ ->
                                let _, firstRange = List.head expressions
                                let _, lastRange = List.last expressions

                                SequentialValueExpression expressions,
                                {
                                    Start = firstRange.Start
                                    End = lastRange.End
                                }

                        let! tryToken = expected TryKeyword "expected 'try'"
                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | WithKeyword -> Ok(sequenceExpression expressions)
                            | RightParenthesis
                            | RightBrace
                            | EndOfFile
                            | MemberKeyword
                            | StaticKeyword
                            | AttributeStart
                            | TypeKeyword
                            | AndKeyword
                            | ElseKeyword -> Ok(sequenceExpression expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpression expressions)

                        let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]
                        let! withToken = expected WithKeyword "expected 'with' after a try body"

                        let parseHandlerBody () =
                            parseResult {
                                let handlerIndent = (current ()).Range.Start.Column
                                let! firstHandler, firstHandlerRange = parseExpression ()

                                let rec parseHandler expressions =
                                    let token = current ()

                                    match token.Kind with
                                    | RightParenthesis
                                    | RightBrace
                                    | EndOfFile
                                    | MemberKeyword
                                    | StaticKeyword
                                    | AttributeStart
                                    | TypeKeyword
                                    | AndKeyword
                                    | ElseKeyword
                                    | Bar -> Ok(sequenceExpression expressions)
                                    | _ when
                                        token.Range.Start.Column
                                        >= handlerIndent
                                        ->
                                        parseExpression ()
                                        |> Result.bind (fun expression ->
                                            parseHandler (
                                                expression
                                                :: expressions
                                            )
                                        )
                                    | _ -> Ok(sequenceExpression expressions)

                                return! parseHandler [ firstHandler, firstHandlerRange ]
                            }

                        let! bindingName, handler, handlerRange =
                            match (current ()).Kind with
                            | Bar ->
                                let clauseIndent = (current ()).Range.Start.Column

                                let parseGuard () =
                                    parseResult {
                                        let! left, leftRange = parseExpression ()

                                        return!
                                            match (current ()).Kind with
                                            | Equals ->
                                                parseResult {
                                                    consume ()
                                                    |> ignore

                                                    let! right, rightRange = parseExpression ()

                                                    let range = {
                                                        Start = leftRange.Start
                                                        End = rightRange.End
                                                    }

                                                    return
                                                        EqualityExpression(left, right, range),
                                                        range
                                                }
                                            | _ -> Ok(left, leftRange)
                                    }

                                let parseClause () =
                                    parseResult {
                                        let! _ =
                                            expected Bar "expected '|' before an exception clause"

                                        let! pattern =
                                            match (current ()).Kind with
                                            | TypeTest ->
                                                parseResult {
                                                    let typeTestToken = consume ()
                                                    let! targetType = parseTypeExpression ()

                                                    let! _ =
                                                        expected
                                                            AsKeyword
                                                            "expected 'as' in an exception type-test pattern"

                                                    let! bindingName, bindingToken =
                                                        identifier
                                                            "expected an exception type-test binding name"

                                                    return
                                                        ParsedTypeTestPattern(
                                                            targetType,
                                                            bindingName,
                                                            {
                                                                Start = typeTestToken.Range.Start
                                                                End = bindingToken.Range.End
                                                            }
                                                        )
                                                }
                                            | Identifier name ->
                                                let token = consume ()
                                                Ok(ParsedNamedPattern(name, token.Range))
                                            | _ ->
                                                Error(
                                                    prototypeDiagnostic
                                                        source.Path
                                                        (current ()).Range
                                                        "expected a supported exception pattern"
                                                )

                                        let! guard =
                                            match (current ()).Kind with
                                            | WhenKeyword ->
                                                consume ()
                                                |> ignore

                                                parseGuard ()
                                                |> Result.map Some
                                            | _ -> Ok None

                                        let! _ =
                                            expected
                                                Arrow
                                                "expected '->' after an exception pattern"

                                        let! handler, handlerRange = parseHandlerBody ()
                                        return pattern, guard, handler, handlerRange
                                    }

                                let rec parseClauses clauses =
                                    parseClause ()
                                    |> Result.bind (fun clause ->
                                        let clauses =
                                            clause
                                            :: clauses

                                        match (current ()).Kind with
                                        | Bar when (current ()).Range.Start.Column = clauseIndent ->
                                            parseClauses clauses
                                        | _ -> Ok(List.rev clauses)
                                    )

                                let rec buildDispatch catchBindingName clauses =
                                    match clauses with
                                    | [ (ParsedNamedPattern(_, _), None, handler, handlerRange) ] ->
                                        Ok(handler, handlerRange)
                                    | ((ParsedTypeTestPattern _ as pattern),
                                       guard,
                                       matchedHandler,
                                       matchedRange) :: remaining ->
                                        buildDispatch catchBindingName remaining
                                        |> Result.map (fun (fallbackHandler, fallbackRange) ->
                                            let range = {
                                                Start = pattern.Range.Start
                                                End = fallbackRange.End
                                            }

                                            MatchExpression(
                                                ValueReference catchBindingName,
                                                [
                                                    pattern, guard, matchedHandler, matchedRange
                                                    ParsedNamedPattern("_", fallbackRange),
                                                    None,
                                                    fallbackHandler,
                                                    fallbackRange
                                                ],
                                                withToken.Range,
                                                range
                                            ),
                                            range
                                        )
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                withToken.Range
                                                "exception clauses require type tests followed by a final named fallback"
                                        )

                                parseClauses []
                                |> Result.bind (fun clauses ->
                                    match List.tryLast clauses with
                                    | Some(ParsedNamedPattern(name, _), None, _, _) ->
                                        let catchBindingName =
                                            if name = "_" then "$fsharp2CaughtException" else name

                                        buildDispatch catchBindingName clauses
                                        |> Result.map (fun (handler, handlerRange) ->
                                            catchBindingName, handler, handlerRange
                                        )
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                withToken.Range
                                                "expected a final named exception clause"
                                        )
                                )
                            | _ ->
                                parseResult {
                                    let! bindingName, _ =
                                        identifier "expected an exception binding after 'with'"

                                    let! _ =
                                        expected Arrow "expected '->' after an exception binding"

                                    let! handler, handlerRange = parseHandlerBody ()
                                    return bindingName, handler, handlerRange
                                }

                        let range = {
                            Start = tryToken.Range.Start
                            End = handlerRange.End
                        }

                        return
                            TryWithExpression(
                                body,
                                bindingName,
                                handler,
                                tryToken.Range,
                                withToken.Range,
                                bodyRange,
                                handlerRange,
                                range
                            ),
                            range
                    }
                | MatchKeyword ->
                    parseResult {
                        let sequenceExpression expressions =
                            let expressions = List.rev expressions

                            match expressions with
                            | [ expression, range ] -> expression, range
                            | _ ->
                                let _, firstRange = List.head expressions
                                let _, lastRange = List.last expressions

                                SequentialValueExpression expressions,
                                {
                                    Start = firstRange.Start
                                    End = lastRange.End
                                }

                        let! matchToken = expected MatchKeyword "expected 'match'"
                        let! inputExpression, _ = parseExpression ()

                        let! withToken = expected WithKeyword "expected 'with' after a match input"

                        let matchHeaderRange = {
                            Start = matchToken.Range.Start
                            End = withToken.Range.End
                        }

                        let clauseIndent = (current ()).Range.Start.Column

                        let rec parseMatchPattern () =
                            match (current ()).Kind with
                            | TypeTest ->
                                parseResult {
                                    let typeTestToken = consume ()
                                    let! targetType = parseTypeExpression ()

                                    let! _ =
                                        expected AsKeyword "expected 'as' in a type-test pattern"

                                    let! bindingName, bindingToken =
                                        identifier "expected a type-test binding name"

                                    return
                                        ParsedTypeTestPattern(
                                            targetType,
                                            bindingName,
                                            {
                                                Start = typeTestToken.Range.Start
                                                End = bindingToken.Range.End
                                            }
                                        )
                                }
                            | NullKeyword ->
                                let token = consume ()
                                Ok(ParsedNullPattern token.Range)
                            | LeftParenthesis ->
                                parseResult {
                                    let openToken = consume ()

                                    match (current ()).Kind with
                                    | RightParenthesis ->
                                        let closeToken = consume ()

                                        return
                                            ParsedUnitPattern {
                                                Start = openToken.Range.Start
                                                End = closeToken.Range.End
                                            }
                                    | _ ->
                                        let! first = parseMatchPattern ()

                                        let rec parseElements elements =
                                            match (current ()).Kind with
                                            | Comma ->
                                                consume ()
                                                |> ignore

                                                parseMatchPattern ()
                                                |> Result.bind (fun element ->
                                                    parseElements (
                                                        element
                                                        :: elements
                                                    )
                                                )
                                            | RightParenthesis -> Ok(List.rev elements)
                                            | _ ->
                                                Error(
                                                    prototypeDiagnostic
                                                        source.Path
                                                        (current ()).Range
                                                        "expected ',' or ')' after a match pattern"
                                                )

                                        let! elements = parseElements [ first ]
                                        let! closeToken = expected RightParenthesis "expected ')'"

                                        match elements with
                                        | [ pattern ] -> return pattern
                                        | _ ->
                                            return
                                                ParsedTuplePattern(
                                                    elements,
                                                    {
                                                        Start = openToken.Range.Start
                                                        End = closeToken.Range.End
                                                    }
                                                )
                                }
                            | Identifier name ->
                                let token = consume ()

                                let isUnionCase =
                                    not (String.IsNullOrEmpty(name))
                                    && Char.IsUpper(name.[0])

                                match isUnionCase, (current ()).Kind with
                                | true, (Identifier _ | LeftParenthesis) ->
                                    parseMatchPattern ()
                                    |> Result.map (fun argument ->
                                        ParsedUnionCasePattern(
                                            name,
                                            argument,
                                            {
                                                Start = token.Range.Start
                                                End = argument.Range.End
                                            }
                                        )
                                    )
                                | _ -> Ok(ParsedNamedPattern(name, token.Range))
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected a supported match pattern"
                                )

                        let parseClause () =
                            parseResult {
                                let! barToken = expected Bar "expected '|' before a match clause"
                                let! pattern = parseMatchPattern ()

                                let! _ = expected Arrow "expected '->' after a match pattern"
                                let bodyIndent = (current ()).Range.Start.Column
                                let! firstBody, firstBodyRange = parseExpression ()

                                let rec parseBody expressions =
                                    let token = current ()

                                    match token.Kind with
                                    | Bar when
                                        token.Range.Start.Column
                                        <= clauseIndent
                                        ->
                                        Ok(sequenceExpression expressions)
                                    | RightParenthesis
                                    | RightBrace
                                    | EndOfFile
                                    | MemberKeyword
                                    | StaticKeyword
                                    | AttributeStart
                                    | TypeKeyword
                                    | AndKeyword
                                    | ElseKeyword -> Ok(sequenceExpression expressions)
                                    | _ when
                                        token.Range.Start.Column
                                        >= bodyIndent
                                        ->
                                        parseExpression ()
                                        |> Result.bind (fun expression ->
                                            parseBody (
                                                expression
                                                :: expressions
                                            )
                                        )
                                    | _ -> Ok(sequenceExpression expressions)

                                let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]

                                return pattern, body, bodyRange, barToken.Range
                            }

                        let rec parseClauses clauses =
                            parseClause ()
                            |> Result.bind (fun (pattern, body, bodyRange, barRange) ->
                                let clauses =
                                    (pattern, None, body, bodyRange)
                                    :: clauses

                                match (current ()).Kind with
                                | Bar when (current ()).Range.Start.Column = clauseIndent ->
                                    parseClauses clauses
                                | _ -> Ok(List.rev clauses, barRange)
                            )

                        let! clauses, _ = parseClauses []
                        let _, _, _, finalBodyRange = List.last clauses

                        let range = {
                            Start = matchToken.Range.Start
                            End = finalBodyRange.End
                        }

                        return
                            MatchExpression(inputExpression, clauses, matchHeaderRange, range),
                            range
                    }
                | LeftBrace ->
                    parseResult {
                        let! openToken = expected LeftBrace "expected '{'"
                        let! _ = expected NewKeyword "expected 'new' in an object expression"
                        let! baseType = parseTypeExpression ()

                        let! _ =
                            expected LeftParenthesis "expected '(' after an object-expression type"

                        let! constructorArguments = parseCallArguments ()

                        let! _ =
                            expected
                                RightParenthesis
                                "expected ')' after object-expression constructor arguments"

                        let! _ = expected WithKeyword "expected 'with' in an object expression"

                        let parseMember () =
                            parseResult {
                                let memberStartToken = current ()

                                let! isOverride =
                                    match memberStartToken.Kind with
                                    | OverrideKeyword ->
                                        consume ()
                                        |> ignore

                                        Ok true
                                    | MemberKeyword ->
                                        consume ()
                                        |> ignore

                                        Ok false
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                memberStartToken.Range
                                                "expected 'override' or 'member' in an object expression"
                                        )

                                let! receiverName, _ =
                                    identifier "expected an object-expression member receiver"

                                let! _ =
                                    expected Dot "expected '.' after an object-expression receiver"

                                let! memberName, _ =
                                    identifier "expected an object-expression member name"

                                let! _ =
                                    expected
                                        LeftParenthesis
                                        "expected '(' after an object-expression member"

                                let rec parseMemberParameters parameters =
                                    match (current ()).Kind with
                                    | RightParenthesis -> Ok(List.rev parameters)
                                    | Identifier _ ->
                                        parseResult {
                                            let! parameterName, _ =
                                                identifier "expected an object-expression parameter"

                                            return!
                                                match (current ()).Kind with
                                                | Comma ->
                                                    consume ()
                                                    |> ignore

                                                    parseMemberParameters (
                                                        parameterName
                                                        :: parameters
                                                    )
                                                | RightParenthesis ->
                                                    Ok(
                                                        List.rev (
                                                            parameterName
                                                            :: parameters
                                                        )
                                                    )
                                                | _ ->
                                                    Error(
                                                        prototypeDiagnostic
                                                            source.Path
                                                            (current ()).Range
                                                            "expected ',' or ')' after an object-expression parameter"
                                                    )
                                        }
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected an object-expression parameter"
                                        )

                                let! memberParameters = parseMemberParameters []
                                let! _ = expected RightParenthesis "expected ')'"

                                let! _ =
                                    expected
                                        Equals
                                        "expected '=' before an object-expression member body"

                                let! memberBody, memberBodyRange = parseExpression ()

                                return {
                                    IsOverride = isOverride
                                    ReceiverName = receiverName
                                    Name = memberName
                                    ParameterNames = memberParameters
                                    Body = memberBody
                                    BodyRange = memberBodyRange
                                    Range = {
                                        Start = memberStartToken.Range.Start
                                        End = memberBodyRange.End
                                    }
                                }
                            }

                        let rec parseMembers members =
                            parseMember ()
                            |> Result.bind (fun memberDeclaration ->
                                let members =
                                    memberDeclaration
                                    :: members

                                match (current ()).Kind with
                                | OverrideKeyword
                                | MemberKeyword -> parseMembers members
                                | RightBrace -> Ok(List.rev members)
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected another member or '}' after an object-expression member"
                                    )
                            )

                        let! members = parseMembers []

                        let! closeToken =
                            expected RightBrace "expected '}' after an object expression"

                        let range = {
                            Start = openToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return
                            ObjectExpression(baseType, constructorArguments, members, range), range
                    }
                | LeftParenthesis when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = RightParenthesis
                    ->
                    let openToken = consume ()
                    let closeToken = consume ()

                    Ok(
                        UnitLiteral,
                        {
                            Start = openToken.Range.Start
                            End = closeToken.Range.End
                        }
                    )
                | LeftParenthesis ->
                    parseResult {
                        let! openToken = expected LeftParenthesis "expected '('"
                        let! firstExpression, firstRange = parseExpression ()

                        let rec parseApplications expression expressionRange =
                            parseResult {
                                return!
                                    match (current ()).Kind with
                                    | Comma ->
                                        let rec parseTuple elements =
                                            parseResult {
                                                let! _ = expected Comma "expected ','"
                                                let! element, _ = parseExpression ()

                                                let elements =
                                                    element
                                                    :: elements

                                                return!
                                                    match (current ()).Kind with
                                                    | Comma -> parseTuple elements
                                                    | RightParenthesis ->
                                                        let closeToken = consume ()

                                                        let range = {
                                                            Start = openToken.Range.Start
                                                            End = closeToken.Range.End
                                                        }

                                                        let tuple =
                                                            TupleExpression(
                                                                List.rev elements,
                                                                range
                                                            )

                                                        match (current ()).Kind with
                                                        | Dot
                                                        | PipeLeft
                                                        | PipeRight ->
                                                            parsePostfixMemberCalls tuple range
                                                        | _ -> Ok(tuple, range)
                                                    | _ ->
                                                        Error(
                                                            prototypeDiagnostic
                                                                source.Path
                                                                (current ()).Range
                                                                "expected ',' or ')' after a tuple element"
                                                        )
                                            }

                                        parseTuple [ expression ]
                                    | RightParenthesis ->
                                        let closeToken = consume ()

                                        match (current ()).Kind with
                                        | Dot ->
                                            parsePostfixMemberCalls expression {
                                                Start = expressionToken.Range.Start
                                                End = closeToken.Range.End
                                            }
                                        | PipeLeft
                                        | PipeRight ->
                                            parsePostfixMemberCalls expression expressionRange
                                        | _ -> Ok(expression, expressionRange)
                                    | Subtype ->
                                        parseResult {
                                            let! _ = expected Subtype "expected ':>'"
                                            let! targetType = parseTypeExpression ()

                                            let! _ =
                                                expected
                                                    RightParenthesis
                                                    "expected ')' after an upcast"

                                            return
                                                ExplicitUpcastExpression(expression, targetType),
                                                {
                                                    Start = expressionRange.Start
                                                    End = targetType.Range.End
                                                }
                                        }
                                    | EndOfFile ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ')' after an expression"
                                        )
                                    | _ ->
                                        parseExpression ()
                                        |> Result.bind (fun (argument, argumentRange) ->
                                            parseApplications
                                                (FunctionApplication(expression, argument))
                                                {
                                                    Start = expressionRange.Start
                                                    End = argumentRange.End
                                                }
                                        )
                            }

                        return! parseApplications firstExpression firstRange
                    }
                | NotKeyword ->
                    parseResult {
                        let! notToken = expected NotKeyword "expected 'not'"
                        let! expression, expressionRange = parseExpression ()

                        let range = {
                            Start = notToken.Range.Start
                            End = expressionRange.End
                        }

                        return BooleanNegationExpression(expression, range), range
                    }
                | LetKeyword when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = Bang
                    ->
                    parseResult {
                        let! letToken = expected LetKeyword "expected 'let'"
                        let! _ = expected Bang "expected '!' after 'let'"

                        let! bindingName, bindingToken = parseComputationBindingName ()

                        let! _ = expected Equals "expected '=' after a computation binding"
                        let! inputExpression, inputRange = parseExpression ()

                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | RightBrace
                            | EndOfFile
                            | ElseKeyword -> Ok(sequenceExpressions expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpressions expressions)

                        let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]

                        let bindingRange = {
                            Start = letToken.Range.Start
                            End = inputRange.End
                        }

                        return
                            ComputationBindingExpression(
                                bindingName,
                                inputExpression,
                                body,
                                bindingRange,
                                bodyRange
                            ),
                            {
                                Start = letToken.Range.Start
                                End = bodyRange.End
                            }
                    }
                | DoKeyword when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = Bang
                    ->
                    parseResult {
                        let doToken = consume ()
                        let! _ = expected Bang "expected '!' after 'do'"
                        let! inputExpression, inputRange = parseExpression ()

                        let range = {
                            Start = doToken.Range.Start
                            End = inputRange.End
                        }

                        return ComputationDoExpression(inputExpression, range), range
                    }
                | IfKeyword ->
                    parseResult {
                        let sequenceExpression expressions =
                            let expressions = List.rev expressions

                            match expressions with
                            | [ expression, range ] -> expression, range
                            | _ ->
                                let _, firstRange = List.head expressions
                                let _, lastRange = List.last expressions

                                SequentialValueExpression expressions,
                                {
                                    Start = firstRange.Start
                                    End = lastRange.End
                                }

                        let! ifToken = expected IfKeyword "expected 'if'"
                        let! firstCondition, firstConditionRange = parseExpression ()

                        let rec parseConjunction condition conditionRange =
                            match (current ()).Kind with
                            | Ampersand when
                                index + 1 < input.Length
                                && input.[index + 1].Kind = Ampersand
                                ->
                                let firstAmpersand = consume ()
                                let secondAmpersand = consume ()

                                parseExpression ()
                                |> Result.bind (fun (right, rightRange) ->
                                    let operatorRange = {
                                        Start = firstAmpersand.Range.Start
                                        End = secondAmpersand.Range.End
                                    }

                                    let syntheticFalseRange = {
                                        Start = operatorRange.End
                                        End = operatorRange.End
                                    }

                                    let range = {
                                        Start = conditionRange.Start
                                        End = rightRange.End
                                    }

                                    parseConjunction
                                        (ConditionalExpression(
                                            condition,
                                            right,
                                            BooleanLiteral false,
                                            conditionRange,
                                            rightRange,
                                            syntheticFalseRange
                                        ))
                                        range
                                )
                            | _ -> Ok(condition, conditionRange)

                        let! condition, _ = parseConjunction firstCondition firstConditionRange

                        let! thenToken =
                            expected ThenKeyword "expected 'then' after an if condition"

                        let ifTrueIndent = (current ()).Range.Start.Column
                        let! firstIfTrue, firstIfTrueRange = parseExpression ()

                        let rec parseIfTrue expressions =
                            match (current ()).Kind with
                            | ElseKeyword -> Ok(sequenceExpression expressions)
                            | _ when
                                (current ()).Range.Start.Column
                                >= ifTrueIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseIfTrue (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpression expressions)

                        let! ifTrue, ifTrueRange = parseIfTrue [ firstIfTrue, firstIfTrueRange ]

                        let! ifFalse, ifFalseRange =
                            match (current ()).Kind with
                            | ElseKeyword ->
                                parseResult {
                                    let! _ =
                                        expected ElseKeyword "expected 'else' after an if branch"

                                    let ifFalseIndent = (current ()).Range.Start.Column
                                    let! firstIfFalse, firstIfFalseRange = parseExpression ()

                                    let rec parseIfFalse expressions =
                                        let token = current ()

                                        match token.Kind with
                                        | RightParenthesis
                                        | EndOfFile
                                        | MemberKeyword
                                        | StaticKeyword
                                        | AttributeStart
                                        | TypeKeyword
                                        | AndKeyword
                                        | ElseKeyword -> Ok(sequenceExpression expressions)
                                        | _ when
                                            token.Range.Start.Column
                                            >= ifFalseIndent
                                            ->
                                            parseExpression ()
                                            |> Result.bind (fun expression ->
                                                parseIfFalse (
                                                    expression
                                                    :: expressions
                                                )
                                            )
                                        | _ -> Ok(sequenceExpression expressions)

                                    return! parseIfFalse [ firstIfFalse, firstIfFalseRange ]
                                }
                            | _ ->
                                let range = {
                                    Start = ifTrueRange.End
                                    End = ifTrueRange.End
                                }

                                Ok(UnitLiteral, range)

                        return
                            ConditionalExpression(
                                condition,
                                ifTrue,
                                ifFalse,
                                {
                                    Start = ifToken.Range.Start
                                    End = thenToken.Range.End
                                },
                                ifTrueRange,
                                ifFalseRange
                            ),
                            {
                                Start = ifToken.Range.Start
                                End = ifFalseRange.End
                            }
                    }
                | LetKeyword ->
                    parseResult {
                        let sequenceExpression expressions =
                            let expressions = List.rev expressions

                            match expressions with
                            | [ expression, range ] -> expression, range
                            | _ ->
                                let _, firstRange = List.head expressions
                                let _, lastRange = List.last expressions

                                SequentialValueExpression expressions,
                                {
                                    Start = firstRange.Start
                                    End = lastRange.End
                                }

                        let! letToken = expected LetKeyword "expected 'let'"

                        let isInline =
                            match (current ()).Kind with
                            | InlineKeyword ->
                                consume ()
                                |> ignore

                                true
                            | _ -> false

                        let isMutable =
                            match (current ()).Kind with
                            | MutableKeyword ->
                                consume ()
                                |> ignore

                                true
                            | _ -> false

                        let! bindingName, _ = identifier "expected a local binding name"

                        let parseLocalFunctionParameter () =
                            parseResult {
                                let wrappedStart =
                                    match (current ()).Kind with
                                    | LeftParenthesis ->
                                        (consume ()).Range.Start
                                        |> Some
                                    | _ -> None

                                let! parameterName, parameterToken =
                                    identifier "expected a local function parameter"

                                let! parameterType =
                                    match (current ()).Kind with
                                    | Colon ->
                                        consume ()
                                        |> ignore

                                        parseTypeExpression ()
                                        |> Result.map Some
                                    | _ -> Ok None

                                let parameterEnd =
                                    parameterType
                                    |> Option.map (fun parameterType -> parameterType.Range.End)
                                    |> Option.defaultValue parameterToken.Range.End

                                let! parameterEnd =
                                    match wrappedStart with
                                    | Some _ ->
                                        expected
                                            RightParenthesis
                                            "expected ')' after a wrapped local function parameter"
                                        |> Result.map (fun token -> token.Range.End)
                                    | None -> Ok parameterEnd

                                return
                                    parameterName,
                                    parameterType,
                                    {
                                        Start =
                                            wrappedStart
                                            |> Option.defaultValue parameterToken.Range.Start
                                        End = parameterEnd
                                    }
                            }

                        let parseParenthesizedLocalParameters () =
                            parseResult {
                                let! _ =
                                    expected
                                        LeftParenthesis
                                        "expected '(' before local function parameters"

                                let rec parseParameters parameters =
                                    parseResult {
                                        let! parameter = parseLocalFunctionParameter ()

                                        let parameters =
                                            parameter
                                            :: parameters

                                        return!
                                            match (current ()).Kind with
                                            | Comma ->
                                                consume ()
                                                |> ignore

                                                parseParameters parameters
                                            | RightParenthesis -> Ok(List.rev parameters)
                                            | _ ->
                                                Error(
                                                    prototypeDiagnostic
                                                        source.Path
                                                        (current ()).Range
                                                        "expected ',' or ')' after a local function parameter"
                                                )
                                    }

                                let! parameters = parseParameters []
                                let! _ = expected RightParenthesis "expected ')'"
                                return parameters
                            }

                        let rec parseLocalFunctionParameters parameters =
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                parseParenthesizedLocalParameters ()
                                |> Result.bind (fun parameterGroup ->
                                    parseLocalFunctionParameters (
                                        (List.rev parameterGroup)
                                        @ parameters
                                    )
                                )
                            | Identifier _ ->
                                parseLocalFunctionParameter ()
                                |> Result.bind (fun parameter ->
                                    parseLocalFunctionParameters (
                                        parameter
                                        :: parameters
                                    )
                                )
                            | Equals -> Ok(List.rev parameters)
                            | _ when List.isEmpty parameters -> Ok []
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected another local function parameter or '='"
                                )

                        let! localFunctionParameters = parseLocalFunctionParameters []

                        let! _bindingType =
                            match (current ()).Kind with
                            | Colon ->
                                consume ()
                                |> ignore

                                parseTypeExpression ()
                                |> Result.map Some
                            | _ -> Ok None

                        let! _ = expected Equals "expected '=' after a local binding name"
                        let! parsedValue, valueRange = parseExpression ()

                        let value =
                            (localFunctionParameters, parsedValue)
                            ||> List.foldBack (fun
                                                   (parameterName, parameterType, parameterRange)
                                                   body ->
                                LambdaExpression(
                                    parameterName,
                                    parameterType,
                                    body,
                                    {
                                        Start = parameterRange.Start
                                        End = valueRange.End
                                    }
                                )
                            )

                        let shouldExpandLocalFunction =
                            isInline
                            || not (List.isEmpty localFunctionParameters)

                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | RightParenthesis
                            | EndOfFile
                            | MemberKeyword
                            | StaticKeyword
                            | AttributeStart
                            | TypeKeyword
                            | AndKeyword
                            | ElseKeyword -> Ok(sequenceExpression expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpression expressions)

                        let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]

                        let bindingRange = {
                            Start = letToken.Range.Start
                            End = valueRange.End
                        }

                        return
                            LetExpression(
                                bindingName,
                                isMutable,
                                shouldExpandLocalFunction,
                                value,
                                body,
                                bindingRange,
                                bodyRange
                            ),
                            {
                                Start = letToken.Range.Start
                                End = bodyRange.End
                            }
                    }
                | Integer value ->
                    consume ()
                    |> ignore

                    Ok(IntegerLiteral value, expressionToken.Range)
                | StringLiteralToken value ->
                    consume ()
                    |> ignore

                    Ok(StringLiteral value, expressionToken.Range)
                | NullKeyword ->
                    consume ()
                    |> ignore

                    Ok(NullLiteral, expressionToken.Range)
                | Identifier "true" ->
                    consume ()
                    |> ignore

                    Ok(BooleanLiteral true, expressionToken.Range)
                | Identifier "false" ->
                    consume ()
                    |> ignore

                    Ok(BooleanLiteral false, expressionToken.Range)
                | FunKeyword ->
                    parseResult {
                        let! funToken = expected FunKeyword "expected 'fun'"

                        let parseParameter () =
                            match (current ()).Kind with
                            | LeftParenthesis when
                                index + 1 < input.Length
                                && input.[index + 1].Kind = RightParenthesis
                                ->
                                consume ()
                                |> ignore

                                consume ()
                                |> ignore

                                Ok(
                                    None
                                    : (string *
                                    ParsedTypeExpression option *
                                    (string * SourceRange) list option *
                                    SourceRange) option
                                )
                            | LeftParenthesis ->
                                parseResult {
                                    let! openToken = expected LeftParenthesis "expected '('"

                                    let! parameterName, parameterToken =
                                        identifier "expected a lambda parameter"

                                    return!
                                        match (current ()).Kind with
                                        | Colon ->
                                            parseResult {
                                                consume ()
                                                |> ignore

                                                let! parameterType = parseTypeExpression ()

                                                let! closeToken =
                                                    expected RightParenthesis "expected ')'"

                                                return
                                                    Some(
                                                        parameterName,
                                                        Some parameterType,
                                                        None,
                                                        {
                                                            Start = openToken.Range.Start
                                                            End = closeToken.Range.End
                                                        }
                                                    )
                                            }
                                        | Comma ->
                                            let rec parseTupleNames names =
                                                parseResult {
                                                    let! _ = expected Comma "expected ','"

                                                    let! name, nameToken =
                                                        identifier
                                                            "expected a tuple-pattern lambda parameter"

                                                    let names =
                                                        (name, nameToken.Range)
                                                        :: names

                                                    return!
                                                        match (current ()).Kind with
                                                        | Comma -> parseTupleNames names
                                                        | RightParenthesis ->
                                                            parseResult {
                                                                let! closeToken =
                                                                    expected
                                                                        RightParenthesis
                                                                        "expected ')'"

                                                                return
                                                                    List.rev names, closeToken.Range
                                                            }
                                                        | _ ->
                                                            Error(
                                                                prototypeDiagnostic
                                                                    source.Path
                                                                    (current ()).Range
                                                                    "expected ',' or ')' in a tuple-pattern lambda parameter"
                                                            )
                                                }

                                            parseTupleNames [ parameterName, parameterToken.Range ]
                                            |> Result.map (fun (names, closeRange) ->
                                                let syntheticName =
                                                    "$tuplePattern"
                                                    + funToken.Range.Start.Offset.ToString(
                                                        Globalization.CultureInfo.InvariantCulture
                                                    )

                                                Some(
                                                    syntheticName,
                                                    None,
                                                    Some names,
                                                    {
                                                        Start = openToken.Range.Start
                                                        End = closeRange.End
                                                    }
                                                )
                                            )
                                        | RightParenthesis ->
                                            parseResult {
                                                let! closeToken =
                                                    expected RightParenthesis "expected ')'"

                                                return
                                                    Some(
                                                        parameterName,
                                                        None,
                                                        None,
                                                        {
                                                            Start = openToken.Range.Start
                                                            End = closeToken.Range.End
                                                        }
                                                    )
                                            }
                                        | _ ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "expected ':', ',' or ')' in a lambda parameter"
                                            )
                                }
                            | Identifier _ ->
                                identifier "expected a lambda parameter"
                                |> Result.map (fun (name, token) ->
                                    Some(name, None, None, token.Range)
                                )
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected a lambda parameter"
                                )

                        let rec parseParameters parameters =
                            parseResult {
                                let! parameter = parseParameter ()

                                let parameters =
                                    parameter
                                    :: parameters

                                return!
                                    match (current ()).Kind with
                                    | Identifier _
                                    | LeftParenthesis -> parseParameters parameters
                                    | _ -> Ok(List.rev parameters)
                            }

                        let! parameters = parseParameters []

                        let! _ = expected Arrow "expected '->'"

                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let finishBody expressions firstRange lastRange =
                            let expressions = List.rev expressions

                            let body =
                                match expressions with
                                | [ expression ] -> expression
                                | _ -> SequentialExpression expressions

                            Ok(
                                body,
                                {
                                    Start = firstRange.Start
                                    End = lastRange.End
                                }
                            )

                        let rec parseBody expressions firstRange lastRange =
                            let token = current ()

                            match token.Kind with
                            | RightParenthesis
                            | EndOfFile
                            | MemberKeyword
                            | StaticKeyword
                            | AttributeStart
                            | TypeKeyword
                            | AndKeyword
                            | ElseKeyword -> finishBody expressions firstRange lastRange
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun (expression, expressionRange) ->
                                    parseBody
                                        (expression
                                         :: expressions)
                                        firstRange
                                        expressionRange
                                )
                            | _ -> finishBody expressions firstRange lastRange

                        let! body, bodyRange = parseBody [ firstBody ] firstBodyRange firstBodyRange

                        let lambdaRange = {
                            Start = funToken.Range.Start
                            End = bodyRange.End
                        }

                        let lambda =
                            (parameters, body)
                            ||> List.foldBack (fun parameter body ->
                                match parameter with
                                | Some(parameterName, parameterType, None, _) ->
                                    LambdaExpression(
                                        parameterName,
                                        parameterType,
                                        body,
                                        lambdaRange
                                    )
                                | Some(parameterName, None, Some tupleNames, patternRange) ->
                                    let tupleBody =
                                        tupleNames
                                        |> List.mapi (fun index (name, nameRange) ->
                                            name,
                                            nameRange,
                                            "Item"
                                            + (index + 1)
                                                .ToString(
                                                    Globalization.CultureInfo.InvariantCulture
                                                )
                                        )
                                        |> List.foldBack (fun (name, nameRange, itemName) body ->
                                            LetExpression(
                                                name,
                                                false,
                                                false,
                                                ExpressionMemberAccess(
                                                    ValueReference parameterName,
                                                    itemName
                                                ),
                                                body,
                                                nameRange,
                                                bodyRange
                                            )
                                        )
                                        <| body

                                    LambdaExpression(
                                        parameterName,
                                        None,
                                        tupleBody,
                                        {
                                            Start = patternRange.Start
                                            End = lambdaRange.End
                                        }
                                    )
                                | Some(_, Some _, Some _, _) ->
                                    invalidOp
                                        "a tuple-pattern lambda parameter cannot have one annotation"
                                | None -> UnitLambdaExpression(body, lambdaRange)
                            )

                        return lambda, lambdaRange
                    }
                | Identifier "while" ->
                    parseResult {
                        let whileToken = consume ()
                        let! condition, conditionRange = parseExpression ()
                        let! _ = expected DoKeyword "expected 'do' after a while condition"

                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | RightBrace
                            | EndOfFile
                            | ElseKeyword -> Ok(sequenceExpressions expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpressions expressions)

                        let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]

                        let range = {
                            Start = whileToken.Range.Start
                            End = bodyRange.End
                        }

                        return
                            WhileExpression(condition, body, conditionRange, bodyRange, range),
                            range
                    }
                | Identifier "for" ->
                    parseResult {
                        let forToken = consume ()
                        let! bindingName, bindingToken = identifier "expected a for-loop binding"
                        let! inName, inToken = identifier "expected 'in' after a for-loop binding"

                        let! _ =
                            if inName = "in" then
                                Ok()
                            else
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        inToken.Range
                                        "expected 'in' after a for-loop binding"
                                )

                        let! sequence, sequenceRange = parseExpression ()
                        let! _ = expected DoKeyword "expected 'do' after a for-loop sequence"

                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | RightBrace
                            | EndOfFile
                            | ElseKeyword -> Ok(sequenceExpressions expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpressions expressions)

                        let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]

                        let range = {
                            Start = forToken.Range.Start
                            End = bodyRange.End
                        }

                        return
                            ForExpression(
                                bindingName,
                                sequence,
                                body,
                                bindingToken.Range,
                                sequenceRange,
                                bodyRange,
                                range
                            ),
                            range
                    }
                | Identifier builderName when
                    index + 3 < input.Length
                    && input.[index + 1].Kind = LeftBrace
                    && input.[index + 2].Kind = LetKeyword
                    && input.[index + 3].Kind
                       <> Bang
                    && hasForBeforeRightBrace (index + 4)
                    ->
                    parseResult {
                        let builderToken = consume ()
                        let! _ = expected LeftBrace "expected '{' after a computation builder"
                        let! letToken = expected LetKeyword "expected a local computation binding"

                        let isMutable =
                            match (current ()).Kind with
                            | MutableKeyword ->
                                consume ()
                                |> ignore

                                true
                            | _ -> false

                        let! bindingName, _ = identifier "expected a local computation binding name"

                        let! _ = expected Equals "expected '=' after a local computation binding"

                        let! bindingValue, bindingValueRange = parseExpression ()
                        let! forExpression, forRange = parseExpression ()

                        let! returnName, returnToken =
                            identifier "expected 'return' after the for expression"

                        let! _ =
                            if returnName = "return" then
                                Ok()
                            else
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        returnToken.Range
                                        "expected 'return' after the for expression"
                                )

                        let returnKind =
                            if (current ()).Kind = Bang then
                                consume ()
                                |> ignore

                                ComputationReturnFrom
                            else
                                ComputationReturn

                        let! returnExpression, returnExpressionRange = parseExpression ()
                        let! closeToken = expected RightBrace "expected '}' after the computation"

                        let computationRange = {
                            Start = builderToken.Range.Start
                            End = closeToken.Range.End
                        }

                        let returnRange = {
                            Start = returnToken.Range.Start
                            End = returnExpressionRange.End
                        }

                        let returnExpression =
                            BindReturnFromComputation(
                                builderName,
                                [],
                                returnKind,
                                returnExpression,
                                returnRange
                            )

                        let bodyRange = {
                            Start = forRange.Start
                            End = returnRange.End
                        }

                        let body =
                            SequentialValueExpression [
                                forExpression, forRange
                                returnExpression, returnRange
                            ]

                        let bindingRange = {
                            Start = letToken.Range.Start
                            End = bindingValueRange.End
                        }

                        return
                            ComputationExpression(
                                builderName,
                                LetExpression(
                                    bindingName,
                                    isMutable,
                                    false,
                                    bindingValue,
                                    body,
                                    bindingRange,
                                    bodyRange
                                ),
                                computationRange
                            ),
                            computationRange
                    }
                | Identifier builderName when
                    index + 3 < input.Length
                    && input.[index + 1].Kind = LeftBrace
                    && input.[index + 2].Kind = LetKeyword
                    && input.[index + 3].Kind = Bang
                    && hasOrdinaryBindingBeforeRightBrace (index + 4)
                    ->
                    parseResult {
                        let builderToken = consume ()
                        let! _ = expected LeftBrace "expected '{' after a computation builder"

                        let parseComputationBinding () =
                            parseResult {
                                let! letToken = expected LetKeyword "expected 'let!'"
                                let! _ = expected Bang "expected '!' after 'let'"
                                let! bindingName, _ = parseComputationBindingName ()
                                let! _ = expected Equals "expected '=' after a computation binding"
                                let! inputExpression, inputRange = parseExpression ()

                                return
                                    bindingName,
                                    inputExpression,
                                    {
                                        Start = letToken.Range.Start
                                        End = inputRange.End
                                    }
                            }

                        let rec parseLeadingBindings bindings =
                            parseComputationBinding ()
                            |> Result.bind (fun binding ->
                                let bindings =
                                    binding
                                    :: bindings

                                match (current ()).Kind with
                                | LetKeyword when
                                    index + 1 < input.Length
                                    && input.[index + 1].Kind = Bang
                                    ->
                                    parseLeadingBindings bindings
                                | LetKeyword
                                | Identifier "use" -> Ok(List.rev bindings)
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected an ordinary local binding after the computation binding"
                                    )
                            )

                        let parseOrdinaryBinding () =
                            parseResult {
                                let! bindingToken, isUse =
                                    match (current ()).Kind with
                                    | LetKeyword -> Ok(consume (), false)
                                    | Identifier "use" -> Ok(consume (), true)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'let' or 'use'"
                                        )

                                let! bindingName, _ =
                                    identifier "expected a local computation binding name"

                                let! _ =
                                    expected Equals "expected '=' after a local computation binding"

                                let! value, valueRange = parseExpression ()

                                return
                                    bindingName,
                                    value,
                                    isUse,
                                    {
                                        Start = bindingToken.Range.Start
                                        End = valueRange.End
                                    }
                            }

                        let rec parseOrdinaryBindings bindings =
                            parseOrdinaryBinding ()
                            |> Result.bind (fun binding ->
                                let bindings =
                                    binding
                                    :: bindings

                                match (current ()).Kind with
                                | LetKeyword when
                                    index + 1 < input.Length
                                    && input.[index + 1].Kind
                                       <> Bang
                                    ->
                                    parseOrdinaryBindings bindings
                                | Identifier "use" -> parseOrdinaryBindings bindings
                                | LetKeyword -> Ok(List.rev bindings)
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected 'let!' after the ordinary computation binding"
                                    )
                            )

                        let rec parseTrailingBindings bindings =
                            parseComputationBinding ()
                            |> Result.bind (fun binding ->
                                let bindings =
                                    binding
                                    :: bindings

                                match (current ()).Kind with
                                | LetKeyword -> parseTrailingBindings bindings
                                | Identifier "return" -> Ok(List.rev bindings)
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected 'let!' or 'return' after the computation binding"
                                    )
                            )

                        let! leadingBindings = parseLeadingBindings []
                        let! ordinaryBindings = parseOrdinaryBindings []
                        let! trailingBindings = parseTrailingBindings []

                        let! returnName, returnToken =
                            identifier "expected 'return' after the computation binding"

                        let! _ =
                            if returnName = "return" then
                                Ok returnToken
                            else
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        returnToken.Range
                                        "expected 'return' after the computation binding"
                                )

                        let returnKind =
                            if (current ()).Kind = Bang then
                                consume ()
                                |> ignore

                                ComputationReturnFrom
                            else
                                ComputationReturn

                        let! firstExpression, firstRange = parseExpression ()

                        let! returnExpression, _ =
                            match (current ()).Kind with
                            | Comma ->
                                parseResult {
                                    consume ()
                                    |> ignore

                                    let! secondExpression, secondRange = parseExpression ()

                                    let tupleRange = {
                                        Start = firstRange.Start
                                        End = secondRange.End
                                    }

                                    return
                                        TupleExpression(
                                            [
                                                firstExpression
                                                secondExpression
                                            ],
                                            tupleRange
                                        ),
                                        tupleRange
                                }
                            | _ -> Ok(firstExpression, firstRange)

                        let! closeToken = expected RightBrace "expected '}' after the computation"

                        let computationRange = {
                            Start = builderToken.Range.Start
                            End = closeToken.Range.End
                        }

                        let bindReturn =
                            BindReturnFromComputation(
                                builderName,
                                trailingBindings
                                |> List.map (fun (name, input, _) -> name, input),
                                returnKind,
                                returnExpression,
                                computationRange
                            )

                        let bodyAfterOrdinaryBindings, bodyAfterOrdinaryRange =
                            ((bindReturn, computationRange),
                             (ordinaryBindings
                              |> List.rev))
                            ||> List.fold (fun (body, bodyRange) (name, value, _, bindingRange) ->
                                LetExpression(
                                    name,
                                    false,
                                    false,
                                    value,
                                    body,
                                    bindingRange,
                                    bodyRange
                                ),
                                {
                                    Start = bindingRange.Start
                                    End = bodyRange.End
                                }
                            )

                        let body, _ =
                            ((bodyAfterOrdinaryBindings, bodyAfterOrdinaryRange),
                             (leadingBindings
                              |> List.rev))
                            ||> List.fold (fun (body, bodyRange) (name, input, bindingRange) ->
                                ComputationBindingExpression(
                                    name,
                                    input,
                                    body,
                                    bindingRange,
                                    bodyRange
                                ),
                                {
                                    Start = bindingRange.Start
                                    End = bodyRange.End
                                }
                            )

                        return!
                            parsePostfixMemberCalls
                                (ComputationExpression(builderName, body, computationRange))
                                computationRange
                    }
                | Identifier builderName when
                    index + 3 < input.Length
                    && input.[index + 1].Kind = LeftBrace
                    && input.[index + 2].Kind = LetKeyword
                    && (
                        match input.[index + 3].Kind with
                        | Identifier _ -> true
                        | _ -> false
                    )
                    && hasComputationBindingBeforeRightBrace (index + 4)
                    && not (hasForBeforeRightBrace (index + 4))
                    ->
                    parseResult {
                        let builderToken = consume ()
                        let! _ = expected LeftBrace "expected '{' after a computation builder"

                        let rec parseOrdinaryBindings bindings =
                            parseResult {
                                let letToken = consume ()

                                let! bindingName, _ =
                                    identifier "expected a local computation binding name"

                                let! _ =
                                    expected Equals "expected '=' after a local computation binding"

                                let! value, valueRange = parseExpression ()

                                let binding =
                                    bindingName,
                                    value,
                                    {
                                        Start = letToken.Range.Start
                                        End = valueRange.End
                                    }

                                match (current ()).Kind with
                                | LetKeyword when
                                    index + 1 < input.Length
                                    && input.[index + 1].Kind
                                       <> Bang
                                    ->
                                    return!
                                        parseOrdinaryBindings (
                                            binding
                                            :: bindings
                                        )
                                | LetKeyword ->
                                    return
                                        List.rev (
                                            binding
                                            :: bindings
                                        )
                                | _ ->
                                    return!
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'let!' after the local computation binding"
                                        )
                            }

                        let! ordinaryBindings = parseOrdinaryBindings []

                        let rec parseBindings bindings =
                            parseResult {
                                let! _ = expected LetKeyword "expected 'let!' in the computation"
                                let! _ = expected Bang "expected '!' after 'let'"

                                let! bindingName, _ = parseComputationBindingName ()

                                let! _ = expected Equals "expected '=' after a computation binding"

                                let! inputExpression, _ = parseExpression ()

                                let bindings =
                                    (bindingName, inputExpression)
                                    :: bindings

                                match (current ()).Kind with
                                | LetKeyword -> return! parseBindings bindings
                                | Identifier "return" -> return List.rev bindings
                                | _ ->
                                    return!
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'let!' or 'return' after the computation binding"
                                        )
                            }

                        let! bindings = parseBindings []

                        let! returnName, returnToken =
                            identifier "expected 'return!' after the computation binding"

                        let! _ =
                            if returnName = "return" then
                                Ok returnToken
                            else
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        returnToken.Range
                                        "expected 'return!' after the computation binding"
                                )

                        let returnKind =
                            if (current ()).Kind = Bang then
                                consume ()
                                |> ignore

                                ComputationReturnFrom
                            else
                                ComputationReturn

                        let! firstExpression, firstRange = parseExpression ()

                        let! returnExpression, _ =
                            match (current ()).Kind with
                            | Comma ->
                                parseResult {
                                    consume ()
                                    |> ignore

                                    let! secondExpression, secondRange = parseExpression ()

                                    let tupleRange = {
                                        Start = firstRange.Start
                                        End = secondRange.End
                                    }

                                    return
                                        TupleExpression(
                                            [
                                                firstExpression
                                                secondExpression
                                            ],
                                            tupleRange
                                        ),
                                        tupleRange
                                }
                            | _ -> Ok(firstExpression, firstRange)

                        let! closeToken = expected RightBrace "expected '}' after the computation"

                        let computationRange = {
                            Start = builderToken.Range.Start
                            End = closeToken.Range.End
                        }

                        let bindReturn =
                            BindReturnFromComputation(
                                builderName,
                                bindings,
                                returnKind,
                                returnExpression,
                                computationRange
                            )

                        let body, _ =
                            (ordinaryBindings, (bindReturn, computationRange))
                            ||> List.foldBack (fun
                                                   (bindingName, value, bindingRange)
                                                   (body, bodyRange) ->
                                let range = {
                                    Start = bindingRange.Start
                                    End = bodyRange.End
                                }

                                LetExpression(
                                    bindingName,
                                    false,
                                    false,
                                    value,
                                    body,
                                    bindingRange,
                                    bodyRange
                                ),
                                range
                            )

                        return!
                            parsePostfixMemberCalls
                                (ComputationExpression(builderName, body, computationRange))
                                computationRange
                    }
                | Identifier builderName when
                    index + 3 < input.Length
                    && input.[index + 1].Kind = LeftBrace
                    && input.[index + 2].Kind = LetKeyword
                    && input.[index + 3].Kind
                       <> Bang
                    ->
                    parseResult {
                        let builderToken = consume ()
                        let! _ = expected LeftBrace "expected '{' after a computation builder"
                        let! body, _ = parseExpression ()
                        let! closeToken = expected RightBrace "expected '}' after the computation"

                        let range = {
                            Start = builderToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return ComputationExpression(builderName, body, range), range
                    }
                | Identifier builderName when
                    index + 2 < input.Length
                    && input.[index + 1].Kind = LeftBrace
                    && input.[index + 2].Kind = DoKeyword
                    ->
                    parseResult {
                        let builderToken = consume ()
                        let! _ = expected LeftBrace "expected '{' after a computation builder"

                        let hasFinallyClause startIndex =
                            let rec loop tokenIndex =
                                if
                                    tokenIndex
                                    >= input.Length
                                then
                                    false
                                else
                                    match input.[tokenIndex].Kind with
                                    | Identifier "finally" -> true
                                    | WithKeyword
                                    | RightBrace
                                    | EndOfFile -> false
                                    | _ ->
                                        loop (
                                            tokenIndex
                                            + 1
                                        )

                            loop startIndex

                        let parseReturn () =
                            parseResult {
                                let returnToken = consume ()

                                let returnKind =
                                    if (current ()).Kind = Bang then
                                        consume ()
                                        |> ignore

                                        ComputationReturnFrom
                                    else
                                        ComputationReturn

                                let! expression, expressionRange = parseExpression ()

                                let range = {
                                    Start = returnToken.Range.Start
                                    End = expressionRange.End
                                }

                                return
                                    BindReturnFromComputation(
                                        builderName,
                                        [],
                                        returnKind,
                                        expression,
                                        range
                                    ),
                                    range
                            }

                        let rec parseComputationElement () =
                            match (current ()).Kind with
                            | Identifier "return" -> parseReturn ()
                            | TryKeyword when hasFinallyClause index -> parseTryFinally ()
                            | _ -> parseExpression ()

                        and parseTryFinally () =
                            parseResult {
                                let tryToken = consume ()
                                let bodyIndent = (current ()).Range.Start.Column
                                let! firstBody, firstBodyRange = parseComputationElement ()

                                let rec parseBody expressions =
                                    let token = current ()

                                    match token.Kind with
                                    | Identifier "finally" -> Ok(sequenceExpressions expressions)
                                    | RightBrace
                                    | EndOfFile -> Ok(sequenceExpressions expressions)
                                    | _ when
                                        token.Range.Start.Column
                                        >= bodyIndent
                                        ->
                                        parseComputationElement ()
                                        |> Result.bind (fun expression ->
                                            parseBody (
                                                expression
                                                :: expressions
                                            )
                                        )
                                    | _ -> Ok(sequenceExpressions expressions)

                                let! body, bodyRange = parseBody [ firstBody, firstBodyRange ]

                                let! finallyToken =
                                    match (current ()).Kind with
                                    | Identifier "finally" -> Ok(consume ())
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'finally' after a try body"
                                        )

                                let compensationIndent = (current ()).Range.Start.Column
                                let! firstCompensation, firstCompensationRange = parseExpression ()

                                let rec parseCompensation expressions =
                                    let token = current ()

                                    match token.Kind with
                                    | RightBrace
                                    | EndOfFile -> Ok(sequenceExpressions expressions)
                                    | _ when
                                        token.Range.Start.Column
                                        >= compensationIndent
                                        ->
                                        parseExpression ()
                                        |> Result.bind (fun expression ->
                                            parseCompensation (
                                                expression
                                                :: expressions
                                            )
                                        )
                                    | _ -> Ok(sequenceExpressions expressions)

                                let! compensation, compensationRange =
                                    parseCompensation [ firstCompensation, firstCompensationRange ]

                                let range = {
                                    Start = tryToken.Range.Start
                                    End = compensationRange.End
                                }

                                return
                                    TryFinallyExpression(
                                        body,
                                        compensation,
                                        tryToken.Range,
                                        finallyToken.Range,
                                        bodyRange,
                                        compensationRange,
                                        range
                                    ),
                                    range
                            }

                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseComputationElement ()

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | RightBrace
                            | EndOfFile -> Ok(sequenceExpressions expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseComputationElement ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(sequenceExpressions expressions)

                        let! body, _ = parseBody [ firstBody, firstBodyRange ]
                        let! closeToken = expected RightBrace "expected '}' after the computation"

                        let range = {
                            Start = builderToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return ComputationExpression(builderName, body, range), range
                    }
                | Identifier builderName when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LeftBrace
                    ->
                    parseResult {
                        let builderToken = consume ()
                        let! _ = expected LeftBrace "expected '{' after a computation builder"

                        let rec parseBindings bindings =
                            parseResult {
                                let! _ = expected LetKeyword "expected 'let!' in the computation"
                                let! _ = expected Bang "expected '!' after 'let'"

                                let! bindingName, _ = parseComputationBindingName ()

                                let! _ = expected Equals "expected '=' after a computation binding"

                                let! inputExpression, _ = parseExpression ()

                                let bindings =
                                    (bindingName, inputExpression)
                                    :: bindings

                                match (current ()).Kind with
                                | LetKeyword -> return! parseBindings bindings
                                | Identifier "return" -> return List.rev bindings
                                | _ ->
                                    return!
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'let!' or 'return' after the computation binding"
                                        )
                            }

                        let! bindings =
                            match (current ()).Kind with
                            | Identifier "return" -> Ok []
                            | _ -> parseBindings []

                        let! returnName, returnToken =
                            identifier "expected 'return!' after the computation binding"

                        let! _ =
                            if returnName = "return" then
                                Ok returnToken
                            else
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        returnToken.Range
                                        "expected 'return!' after the computation binding"
                                )

                        let returnKind =
                            if (current ()).Kind = Bang then
                                consume ()
                                |> ignore

                                ComputationReturnFrom
                            else
                                ComputationReturn

                        let! firstExpression, firstRange = parseExpression ()

                        let! returnExpression, _ =
                            match (current ()).Kind with
                            | Comma ->
                                parseResult {
                                    consume ()
                                    |> ignore

                                    let! secondExpression, secondRange = parseExpression ()

                                    let tupleRange = {
                                        Start = firstRange.Start
                                        End = secondRange.End
                                    }

                                    return
                                        TupleExpression(
                                            [
                                                firstExpression
                                                secondExpression
                                            ],
                                            tupleRange
                                        ),
                                        tupleRange
                                }
                            | _ -> Ok(firstExpression, firstRange)

                        let! closeToken = expected RightBrace "expected '}' after the computation"

                        return
                            BindReturnFromComputation(
                                builderName,
                                bindings,
                                returnKind,
                                returnExpression,
                                {
                                    Start = builderToken.Range.Start
                                    End = closeToken.Range.End
                                }
                            ),
                            {
                                Start = builderToken.Range.Start
                                End = closeToken.Range.End
                            }
                    }
                | Identifier "struct" when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LeftParenthesis
                    ->
                    parseResult {
                        let structToken = consume ()
                        let! _ = expected LeftParenthesis "expected '(' after 'struct'"
                        let! elements = parseCallArguments ()
                        let! closeToken = expected RightParenthesis "expected ')'"

                        let range = {
                            Start = structToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return!
                            if elements.Length < 2 then
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        range
                                        "a struct tuple requires at least two elements"
                                )
                            else
                                Ok(StructTupleExpression(elements, range), range)
                    }
                | Identifier "isNull" ->
                    parseResult {
                        let isNullToken = consume ()
                        let! argument, argumentRange = parseExpression ()

                        return
                            MemberCall(
                                "Object",
                                "__fsharp2_isNull",
                                [
                                    argument
                                    NullLiteral
                                ]
                            ),
                            {
                                Start = isNullToken.Range.Start
                                End = argumentRange.End
                            }
                    }
                | Identifier typeName when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LeftParenthesis
                    && not (String.IsNullOrEmpty typeName)
                    && Char.IsUpper typeName.[0]
                    ->
                    parseResult {
                        let typeToken = consume ()
                        let! openToken = expected LeftParenthesis "expected '('"
                        let! arguments = parseCallArguments ()
                        let! closeToken = expected RightParenthesis "expected ')'"

                        let constructedType =
                            ParsedNamedType(
                                {
                                    Namespace = String.Empty
                                    Name = typeName
                                },
                                typeToken.Range
                            )

                        let range = {
                            Start = typeToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return!
                            parsePostfixMemberCalls
                                (TypeConstruction(
                                    constructedType,
                                    arguments,
                                    {
                                        Start = openToken.Range.Start
                                        End = closeToken.Range.End
                                    }
                                ))
                                range
                    }
                | Identifier functionName when
                    index + 2 < input.Length
                    && input.[index + 1].Kind = LeftParenthesis
                    && input.[index + 2].Kind = RightParenthesis
                    ->
                    parseResult {
                        let startToken = consume ()
                        let! _ = expected LeftParenthesis "expected '('"
                        let! closeToken = expected RightParenthesis "expected ')'"

                        let range = {
                            Start = startToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return!
                            match (current ()).Kind with
                            | Subtype ->
                                parseResult {
                                    let! _ = expected Subtype "expected ':>'"
                                    let! targetType = parseTypeExpression ()

                                    return
                                        ExplicitUpcastExpression(
                                            UnitApplication functionName,
                                            targetType
                                        ),
                                        {
                                            Start = range.Start
                                            End = targetType.Range.End
                                        }
                                }
                            | _ -> parsePostfixMemberCalls (UnitApplication functionName) range
                    }
                | Identifier typeName when
                    index + 1 < input.Length
                    && (
                        match input.[index + 1].Kind with
                        | Identifier _ -> true
                        | _ -> false
                    )
                    && not (String.IsNullOrEmpty typeName)
                    && Char.IsUpper typeName.[0]
                    ->
                    parseResult {
                        let typeToken = consume ()
                        let! argument, argumentRange = parseExpression ()

                        let constructedType =
                            ParsedNamedType(
                                {
                                    Namespace = String.Empty
                                    Name = typeName
                                },
                                typeToken.Range
                            )

                        return
                            TypeConstruction(constructedType, [ argument ], argumentRange),
                            {
                                Start = typeToken.Range.Start
                                End = argumentRange.End
                            }
                    }
                | Identifier _ when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LessThan
                    ->
                    parseResult {
                        let! constructedType = parseTypeExpression ()

                        return!
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                parseResult {
                                    let! openToken = expected LeftParenthesis "expected '('"
                                    let! arguments = parseCallArguments ()
                                    let! closeToken = expected RightParenthesis "expected ')'"

                                    let argumentRange = {
                                        Start = openToken.Range.Start
                                        End = closeToken.Range.End
                                    }

                                    return
                                        TypeConstruction(constructedType, arguments, argumentRange),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = closeToken.Range.End
                                        }
                                }
                            | Dot ->
                                parseResult {
                                    let! _ = expected Dot "expected '.'"

                                    let! memberName, _ = identifier "expected a static member name"

                                    let! _ =
                                        expected
                                            LeftParenthesis
                                            "expected '(' after a static member name"

                                    let! arguments = parseCallArguments ()
                                    let! closeToken = expected RightParenthesis "expected ')'"

                                    return
                                        StaticTypeMemberCall(constructedType, memberName, arguments),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = closeToken.Range.End
                                        }
                                }
                            | Identifier _ ->
                                parseResult {
                                    let! argument, argumentRange = parseExpression ()

                                    return
                                        TypeConstruction(
                                            constructedType,
                                            [ argument ],
                                            argumentRange
                                        ),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = argumentRange.End
                                        }
                                }
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected a constructor argument, '(', or '.' after a constructed generic type"
                                )
                    }
                | NewKeyword ->
                    parseResult {
                        let newToken = consume ()
                        let! constructedType = parseTypeExpression ()
                        let! openToken = expected LeftParenthesis "expected '('"
                        let! arguments = parseCallArguments ()
                        let! closeToken = expected RightParenthesis "expected ')'"

                        return
                            TypeConstruction(
                                constructedType,
                                arguments,
                                {
                                    Start = openToken.Range.Start
                                    End = closeToken.Range.End
                                }
                            ),
                            {
                                Start = newToken.Range.Start
                                End = closeToken.Range.End
                            }
                    }
                | Identifier name when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LeftArrow
                    ->
                    parseResult {
                        let startToken = consume ()
                        let! _ = expected LeftArrow "expected '<-'"
                        let! value, valueRange = parseExpression ()

                        return
                            LocalAssignment(name, value),
                            {
                                Start = startToken.Range.Start
                                End = valueRange.End
                            }
                    }
                | Identifier receiverName when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = Dot
                    ->
                    consume ()
                    |> ignore

                    parseResult {
                        let! _, memberPath = parseMemberPath receiverName

                        return!
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                parseResult {
                                    let! _ = expected LeftParenthesis "expected '('"
                                    let! arguments = parseCallArguments ()
                                    let! closeToken = expected RightParenthesis "expected ')'"

                                    let calledExpression =
                                        match memberPath with
                                        | [ memberName ] ->
                                            MemberCall(receiverName, memberName, arguments)
                                        | _ ->
                                            let memberName = List.last memberPath

                                            let receiver =
                                                memberPath
                                                |> List.take (
                                                    memberPath.Length
                                                    - 1
                                                )
                                                |> List.fold
                                                    (fun expression name ->
                                                        ExpressionMemberAccess(expression, name)
                                                    )
                                                    (ValueReference receiverName)

                                            ExpressionMemberCall(receiver, memberName, arguments)

                                    return!
                                        parsePostfixMemberCalls calledExpression {
                                            Start = expressionToken.Range.Start
                                            End = closeToken.Range.End
                                        }
                                }
                            | LessThan when memberPath.Length = 1 ->
                                parseResult {
                                    let! _ = expected LessThan "expected '<'"
                                    let! typeArguments = parseTypeArguments []
                                    let! _ = expected GreaterThan "expected '>'"
                                    let! _ = expected LeftParenthesis "expected '('"
                                    let! arguments = parseCallArguments ()
                                    let! closeToken = expected RightParenthesis "expected ')'"

                                    return
                                        GenericMemberCall(
                                            receiverName,
                                            memberPath.Head,
                                            typeArguments,
                                            arguments
                                        ),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = closeToken.Range.End
                                        }
                                }
                            | LeftArrow ->
                                parseResult {
                                    let! _ = expected LeftArrow "expected '<-'"
                                    let! value, valueRange = parseExpression ()

                                    let valueEnd =
                                        match value, input.[index - 1].Kind with
                                        | ExplicitUpcastExpression _, RightParenthesis ->
                                            input.[index - 1].Range.End
                                        | _ -> valueRange.End

                                    return
                                        MemberAssignment(receiverName, memberPath, value),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = valueEnd
                                        }
                                }
                            | Identifier _ when
                                memberPath.Length = 1
                                && (current ()).Range.Start.Line = input.[index - 1].Range.End.Line
                                ->
                                parseResult {
                                    let! argument, argumentRange = parseExpression ()

                                    return
                                        MemberCall(receiverName, memberPath.Head, [ argument ]),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = argumentRange.End
                                        }
                                }
                            | _ when memberPath.Length = 1 ->
                                let memberToken = input.[index - 1]

                                let range = {
                                    Start = expressionToken.Range.Start
                                    End = memberToken.Range.End
                                }

                                parsePostfixMemberCalls
                                    (BoundInstanceMember(receiverName, memberPath.Head))
                                    range
                            | _ ->
                                let memberToken = input.[index - 1]

                                let expression =
                                    memberPath
                                    |> List.fold
                                        (fun expression name ->
                                            ExpressionMemberAccess(expression, name)
                                        )
                                        (ValueReference receiverName)

                                parsePostfixMemberCalls expression {
                                    Start = expressionToken.Range.Start
                                    End = memberToken.Range.End
                                }
                    }
                | Identifier functionName when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LeftParenthesis
                    && (input.[index + 1].Range.Start.Line = expressionToken.Range.End.Line
                        || input.[index + 1].Range.Start.Column > expressionToken.Range.Start.Column)
                    && not (String.IsNullOrEmpty functionName)
                    && not (Char.IsUpper functionName.[0])
                    ->
                    parseResult {
                        consume ()
                        |> ignore

                        let! argument, argumentRange = parseExpression ()

                        let rec parseFollowingApplications expression expressionRange =
                            let token = current ()

                            if
                                token.Kind = LeftParenthesis
                                && (token.Range.Start.Line = expressionRange.End.Line
                                    || token.Range.Start.Column > expressionToken.Range.Start.Column)
                            then
                                parseExpression ()
                                |> Result.bind (fun (nextArgument, nextArgumentRange) ->
                                    parseFollowingApplications
                                        (FunctionApplication(expression, nextArgument))
                                        {
                                            Start = expressionRange.Start
                                            End = nextArgumentRange.End
                                        }
                                )
                            else
                                Ok(expression, expressionRange)

                        return!
                            parseFollowingApplications
                                (FunctionApplication(ValueReference functionName, argument))
                                {
                                    Start = expressionToken.Range.Start
                                    End = argumentRange.End
                                }
                    }
                | Identifier functionName when
                    index + 1 < input.Length
                    && (
                        match input.[index + 1].Kind with
                        | Identifier _ -> true
                        | _ -> false
                    )
                    && input.[index + 1].Range.Start.Line = expressionToken.Range.End.Line
                    ->
                    parseResult {
                        consume ()
                        |> ignore

                        let! argument, argumentRange = parseExpression ()

                        return
                            FunctionApplication(ValueReference functionName, argument),
                            {
                                Start = expressionToken.Range.Start
                                End = argumentRange.End
                            }
                    }
                | Identifier value ->
                    consume ()
                    |> ignore

                    match (current ()).Kind with
                    | Subtype ->
                        parseResult {
                            let! _ = expected Subtype "expected ':>'"
                            let! targetType = parseTypeExpression ()

                            return
                                ExplicitUpcastExpression(ValueReference value, targetType),
                                {
                                    Start = expressionToken.Range.Start
                                    End = targetType.Range.End
                                }
                        }
                    | _ -> parsePostfixMemberCalls (ValueReference value) expressionToken.Range
                | RightParenthesis ->
                    consume ()
                    |> ignore

                    Error(
                        diagnostic
                            "FS0010"
                            source.Path
                            expressionToken.Range
                            "Unexpected symbol ')' in binding"
                    )
                | _ ->
                    consume ()
                    |> ignore

                    Error(
                        prototypeDiagnostic
                            source.Path
                            expressionToken.Range
                            "expected an expression"
                    )

            let finishDeclaration
                moduleName
                (declarationToken: Token)
                declarationName
                isUnitFunction
                declaredType
                =
                match expected Equals "expected '='" with
                | Error error -> Error error
                | Ok _ ->
                    match parseExpression () with
                    | Error error -> Error error
                    | Ok(body, bodyRange) ->
                        match expected EndOfFile "expected end of file" with
                        | Error error -> Error error
                        | Ok _ ->
                            Ok {
                                StableId =
                                    "module:"
                                    + moduleName
                                ContainerKind = ModuleSource
                                Namespace = String.Empty
                                Name = moduleName
                                IsPublic = true
                                OpenedNamespaces = []
                                SourceChecksum =
                                    sourceChecksum
                                    |> ImmutableArray.CreateRange<byte>
                                ContentFingerprint = contentFingerprint
                                Attributes = []
                                AssemblyAttributes = []
                                Declarations = [
                                    ParsedMethod {
                                        Name = declarationName
                                        IsUnitFunction = isUnitFunction
                                        DeclaredType = declaredType
                                        Body = body
                                        BodyRange = bodyRange
                                        Range = {
                                            Start = declarationToken.Range.Start
                                            End = bodyRange.End
                                        }
                                    }
                                ]
                            }

            let parseModule () =
                match identifier "expected a module name" with
                | Ok(moduleName, _) ->
                    match expected LetKeyword "expected 'let'" with
                    | Error error -> Error error
                    | Ok _ ->
                        let declarationToken = consume ()

                        match declarationToken.Kind with
                        | Identifier declarationName ->
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                consume ()
                                |> ignore

                                match expected RightParenthesis "expected ')'" with
                                | Error error -> Error error
                                | Ok _ ->
                                    finishDeclaration
                                        moduleName
                                        declarationToken
                                        declarationName
                                        true
                                        None
                            | Colon ->
                                consume ()
                                |> ignore

                                let typeToken = consume ()

                                match typeToken.Kind with
                                | Identifier "int" ->
                                    finishDeclaration
                                        moduleName
                                        declarationToken
                                        declarationName
                                        false
                                        (Some ParsedInt32)
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            typeToken.Range
                                            "expected the type 'int'"
                                    )
                            | Equals ->
                                finishDeclaration
                                    moduleName
                                    declarationToken
                                    declarationName
                                    false
                                    None
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected '(', ':', or '='"
                                )
                        | _ ->
                            Error(
                                prototypeDiagnostic
                                    source.Path
                                    declarationToken.Range
                                    "expected a declaration name"
                            )
                | Error error -> Error error

            let parseNamespaceFile () =
                let stringArgument message =
                    let token = consume ()

                    match token.Kind with
                    | StringLiteralToken value -> Ok value
                    | _ -> Error(prototypeDiagnostic source.Path token.Range message)

                let rec parseOpenNamespaces namespaces =
                    match (current ()).Kind with
                    | OpenKeyword ->
                        consume ()
                        |> ignore

                        parseResult {
                            let! namespaceName = qualifiedIdentifier "expected an opened namespace"

                            return!
                                parseOpenNamespaces (
                                    namespaceName
                                    :: namespaces
                                )
                        }
                    | _ -> Ok(List.rev namespaces)

                let parseDeclarationAttribute () =
                    parseResult {
                        let start = (current ()).Range.Start

                        let! attributeTypeName =
                            qualifiedIdentifier "expected an attribute type name"

                        let parseArgument () =
                            let token = consume ()

                            match token.Kind with
                            | Identifier "false" -> Ok(ParsedBooleanAttributeArgument false)
                            | Identifier "true" -> Ok(ParsedBooleanAttributeArgument true)
                            | StringLiteralToken value -> Ok(ParsedStringAttributeArgument value)
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        token.Range
                                        "expected a boolean or string attribute argument"
                                )

                        let rec parseArguments arguments =
                            parseResult {
                                let! argument = parseArgument ()

                                let arguments =
                                    argument
                                    :: arguments

                                return!
                                    match (current ()).Kind with
                                    | Comma ->
                                        consume ()
                                        |> ignore

                                        parseArguments arguments
                                    | RightParenthesis -> Ok(List.rev arguments)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ',' or ')' after an attribute argument"
                                        )
                            }

                        let! constructorArguments =
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                consume ()
                                |> ignore

                                parseResult {
                                    let! arguments =
                                        match (current ()).Kind with
                                        | RightParenthesis -> Ok []
                                        | _ -> parseArguments []

                                    let! _ = expected RightParenthesis "expected ')'"
                                    return arguments
                                }
                            | _ -> Ok []

                        return {
                            AttributeType = qualifiedTypeName attributeTypeName
                            ConstructorArguments = constructorArguments
                            Range = {
                                Start = start
                                End = input.[index - 1].Range.End
                            }
                        }
                    }

                let parseDeclarationAttributes () =
                    parseResult {
                        let! _ = expected AttributeStart "expected '[<'"

                        let rec parseAttributes attributes =
                            parseResult {
                                let! attribute = parseDeclarationAttribute ()

                                let attributes =
                                    attribute
                                    :: attributes

                                return!
                                    match (current ()).Kind with
                                    | Semicolon ->
                                        consume ()
                                        |> ignore

                                        parseAttributes attributes
                                    | AttributeEnd -> Ok(List.rev attributes)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ';' or '>]' after an attribute"
                                        )
                            }

                        let! attributes = parseAttributes []
                        let! _ = expected AttributeEnd "expected '>]'"
                        return attributes
                    }

                let rec parseAttributeArguments constructorArguments namedArguments =
                    match (current ()).Kind with
                    | RightParenthesis -> Ok(List.rev constructorArguments, List.rev namedArguments)
                    | Comma ->
                        consume ()
                        |> ignore

                        match (current ()).Kind with
                        | Identifier name when
                            index + 1 < input.Length
                            && input.[index + 1].Kind = Equals
                            ->
                            consume ()
                            |> ignore

                            consume ()
                            |> ignore

                            parseResult {
                                let! value =
                                    stringArgument "expected a string-valued named argument"

                                let argument: ParsedNamedStringArgument = {
                                    Name = name
                                    Value = value
                                }

                                return!
                                    parseAttributeArguments
                                        constructorArguments
                                        (argument
                                         :: namedArguments)
                            }
                        | StringLiteralToken _ when List.isEmpty namedArguments ->
                            parseResult {
                                let! value = stringArgument "expected a string constructor argument"

                                return!
                                    parseAttributeArguments
                                        (value
                                         :: constructorArguments)
                                        namedArguments
                            }
                        | _ ->
                            Error(
                                prototypeDiagnostic
                                    source.Path
                                    (current ()).Range
                                    "expected a string constructor or named argument"
                            )
                    | _ ->
                        Error(
                            prototypeDiagnostic source.Path (current ()).Range "expected ',' or ')'"
                        )

                let resolveAttributeType openNamespaces attributeTypeName =
                    let attributeType = qualifiedTypeName attributeTypeName

                    if String.IsNullOrEmpty(attributeType.Namespace) then
                        match
                            openNamespaces
                            |> List.rev
                            |> List.tryHead
                        with
                        | Some openedNamespace -> {
                            Namespace = openedNamespace
                            Name = attributeType.Name
                          }
                        | None -> attributeType
                    else
                        attributeType

                let parseAssemblyAttribute openNamespaces =
                    parseResult {
                        let! attributeStart = expected AttributeStart "expected '[<'"

                        let! _ =
                            match (current ()).Kind with
                            | AssemblyKeyword ->
                                consume ()
                                |> ignore

                                expected Colon "expected ':'"
                                |> Result.map ignore
                            | _ -> Ok()

                        let! attributeTypeName =
                            qualifiedIdentifier "expected an attribute type name"

                        let! _ = expected LeftParenthesis "expected '('"
                        let! firstArgument = stringArgument "expected a string constructor argument"

                        let! constructorArguments, namedArguments =
                            parseAttributeArguments [ firstArgument ] []

                        let! _ = expected RightParenthesis "expected ')'"
                        let! attributeEnd = expected AttributeEnd "expected '>]'"

                        return {
                            AttributeType = resolveAttributeType openNamespaces attributeTypeName
                            ConstructorArguments = constructorArguments
                            NamedArguments = namedArguments
                            Range = {
                                Start = attributeStart.Range.Start
                                End = attributeEnd.Range.End
                            }
                        }
                    }

                let rec parseRemainingAssemblyAttributes openNamespaces attributes =
                    match (current ()).Kind with
                    | AttributeStart ->
                        parseResult {
                            let! attribute = parseAssemblyAttribute openNamespaces

                            return!
                                parseRemainingAssemblyAttributes
                                    openNamespaces
                                    (attribute
                                     :: attributes)
                        }
                    | DoKeyword -> Ok(List.rev attributes)
                    | _ ->
                        Error(
                            prototypeDiagnostic
                                source.Path
                                (current ()).Range
                                "expected an assembly attribute or 'do'"
                        )

                let parseLiteralDeclaration () =
                    parseResult {
                        let! _ = expected LetKeyword "expected 'let'"
                        let! _ = expected AttributeStart "expected '[<'"
                        let! literalName, literalToken = identifier "expected 'Literal'"

                        let! _ =
                            if literalName = "Literal" then
                                Ok()
                            else
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        literalToken.Range
                                        "expected 'Literal'"
                                )

                        let! _ = expected AttributeEnd "expected '>]'"

                        let! declarationName, declarationToken =
                            identifier "expected a declaration name"

                        let! _ = expected Equals "expected '='"
                        let valueToken = consume ()

                        let! value =
                            match valueToken.Kind with
                            | StringLiteralToken value -> Ok value
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        valueToken.Range
                                        "expected a string literal"
                                )

                        return
                            ParsedLiteralField {
                                Name = declarationName
                                Value = value
                                ValueRange = valueToken.Range
                                Range = {
                                    Start = declarationToken.Range.Start
                                    End = valueToken.Range.End
                                }
                            }
                    }

                let rec parseLiteralDeclarations declarations =
                    match (current ()).Kind with
                    | LetKeyword ->
                        parseResult {
                            let! declaration = parseLiteralDeclaration ()

                            return!
                                parseLiteralDeclarations (
                                    declaration
                                    :: declarations
                                )
                        }
                    | EndOfFile -> Ok(List.rev declarations)
                    | _ ->
                        Error(
                            prototypeDiagnostic
                                source.Path
                                (current ()).Range
                                "expected a literal declaration or end of file"
                        )

                let parseParameter () =
                    parseResult {
                        let! attributes =
                            match (current ()).Kind with
                            | AttributeStart -> parseDeclarationAttributes ()
                            | _ -> Ok []

                        let wrappedParameterStart =
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                let token = consume ()
                                Some token.Range.Start
                            | _ -> None

                        let! parameterName, parameterToken = identifier "expected a parameter name"

                        let! parameterType =
                            match (current ()).Kind with
                            | Colon ->
                                consume ()
                                |> ignore

                                parseTypeExpression ()
                            | _ -> Ok(ParsedWildcardType parameterToken.Range)

                        let! parameterEnd =
                            match wrappedParameterStart with
                            | Some _ ->
                                expected RightParenthesis "expected ')' after a wrapped parameter"
                                |> Result.map (fun token -> token.Range.End)
                            | None -> Ok parameterType.Range.End

                        return {
                            Attributes = attributes
                            Name = parameterName
                            Type = parameterType
                            Range = {
                                Start =
                                    wrappedParameterStart
                                    |> Option.defaultValue parameterToken.Range.Start
                                End = parameterEnd
                            }
                        }
                    }

                let rec parseParameters parameters =
                    match (current ()).Kind with
                    | RightParenthesis -> Ok(List.rev parameters)
                    | _ ->
                        parseParameter ()
                        |> Result.bind (fun parameter ->
                            let parameters =
                                parameter
                                :: parameters

                            match (current ()).Kind with
                            | Comma ->
                                consume ()
                                |> ignore

                                parseParameters parameters
                            | RightParenthesis -> Ok(List.rev parameters)
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected ',' or ')' after a method parameter"
                                )
                        )

                let parseMethodBody () =
                    parseResult {
                        let bodyIndent = (current ()).Range.Start.Column
                        let! firstBody, firstBodyRange = parseExpression ()

                        let finish expressions =
                            let expressions = List.rev expressions

                            match expressions with
                            | [ expression, range ] -> expression, range
                            | _ ->
                                let _, firstRange = List.head expressions
                                let _, lastRange = List.last expressions

                                SequentialValueExpression expressions,
                                {
                                    Start = firstRange.Start
                                    End = lastRange.End
                                }

                        let rec parseBody expressions =
                            let token = current ()

                            match token.Kind with
                            | RightParenthesis
                            | RightBrace
                            | EndOfFile
                            | MemberKeyword
                            | StaticKeyword
                            | AttributeStart
                            | TypeKeyword
                            | AndKeyword
                            | ModuleKeyword
                            | NamespaceKeyword
                            | ElseKeyword -> Ok(finish expressions)
                            | _ when
                                token.Range.Start.Column
                                >= bodyIndent
                                ->
                                parseExpression ()
                                |> Result.bind (fun expression ->
                                    parseBody (
                                        expression
                                        :: expressions
                                    )
                                )
                            | _ -> Ok(finish expressions)

                        return! parseBody [ firstBody, firstBodyRange ]
                    }

                let parseInstanceMethod attributes =
                    parseResult {
                        let! memberToken = expected MemberKeyword "expected 'member'"
                        let! _ = expected InlineKeyword "expected 'inline'"

                        let isPublic =
                            match (current ()).Kind with
                            | InternalKeyword ->
                                consume ()
                                |> ignore

                                false
                            | _ -> true

                        let! receiverName, _ = identifier "expected an instance member receiver"

                        let! _ = expected Dot "expected '.'"
                        let! methodName, _ = identifier "expected an instance member name"

                        let parseDirectConstraint () =
                            parseResult {
                                let! constrainedParameter, parameterToken =
                                    typeParameter "expected a constrained type parameter"

                                return!
                                    match (current ()).Kind with
                                    | Subtype ->
                                        consume ()
                                        |> ignore

                                        parseTypeExpression ()
                                        |> Result.map (fun superType ->
                                            ParsedSubtypeConstraint(
                                                constrainedParameter,
                                                superType,
                                                {
                                                    Start = parameterToken.Range.Start
                                                    End = superType.Range.End
                                                }
                                            )
                                        )
                                    | Colon ->
                                        parseResult {
                                            let! _ = expected Colon "expected ':'"
                                            let! _ = expected LeftParenthesis "expected '('"
                                            let! _ = expected MemberKeyword "expected 'member'"
                                            let! memberName, _ = identifier "expected a member name"
                                            let! _ = expected Colon "expected ':'"
                                            let! memberType = parseTypeExpression ()

                                            let! closeToken =
                                                expected RightParenthesis "expected ')'"

                                            return
                                                ParsedMemberConstraint(
                                                    constrainedParameter,
                                                    memberName,
                                                    memberType,
                                                    {
                                                        Start = parameterToken.Range.Start
                                                        End = closeToken.Range.End
                                                    }
                                                )
                                        }
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ':>' or a member constraint"
                                        )
                            }

                        let rec parseMethodConstraints constraints =
                            parseResult {
                                let! constraint' =
                                    match (current ()).Kind with
                                    | TypeParameter _ ->
                                        parseDirectConstraint ()
                                        |> Result.map ParsedDirectConstraint
                                    | _ ->
                                        parseTypeExpression ()
                                        |> Result.map ParsedAbbreviationConstraint

                                let constraints =
                                    constraint'
                                    :: constraints

                                return!
                                    match (current ()).Kind with
                                    | AndKeyword ->
                                        consume ()
                                        |> ignore

                                        parseMethodConstraints constraints
                                    | GreaterThan -> Ok(List.rev constraints)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'and' or '>' after a method constraint"
                                        )
                            }

                        let rec parseMethodTypeParameters parameters =
                            parseResult {
                                let! parameter, _ =
                                    typeParameter "expected an instance-member type parameter"

                                let parameters =
                                    parameter
                                    :: parameters

                                return!
                                    match (current ()).Kind with
                                    | Comma ->
                                        consume ()
                                        |> ignore

                                        parseMethodTypeParameters parameters
                                    | WhenKeyword ->
                                        consume ()
                                        |> ignore

                                        parseMethodConstraints []
                                        |> Result.map (fun constraints ->
                                            List.rev parameters, constraints
                                        )
                                    | GreaterThan -> Ok(List.rev parameters, [])
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ',', 'when', or '>' after an instance-member type parameter"
                                        )
                            }

                        let! methodTypeParameters, methodConstraints =
                            match (current ()).Kind with
                            | LessThan ->
                                consume ()
                                |> ignore

                                parseResult {
                                    let! parameters, constraints = parseMethodTypeParameters []

                                    let! _ = expected GreaterThan "expected '>'"
                                    return parameters, constraints
                                }
                            | _ -> Ok([], [])

                        let! parameters =
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                parseResult {
                                    let! _ = expected LeftParenthesis "expected '('"
                                    let! parameters = parseParameters []
                                    let! _ = expected RightParenthesis "expected ')'"
                                    return parameters
                                }
                            | Identifier _ ->
                                let rec parseCurriedParameters parameters =
                                    match (current ()).Kind with
                                    | Identifier _ ->
                                        parseParameter ()
                                        |> Result.bind (fun parameter ->
                                            parseCurriedParameters (
                                                parameter
                                                :: parameters
                                            )
                                        )
                                    | Equals
                                    | Colon -> Ok(List.rev parameters)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected an instance-member parameter or '='"
                                        )

                                parseCurriedParameters []
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected an instance-member parameter"
                                )

                        let! returnType =
                            match (current ()).Kind with
                            | Colon ->
                                consume ()
                                |> ignore

                                parseTypeExpression ()
                                |> Result.map Some
                            | _ -> Ok None

                        let! _ = expected Equals "expected '='"
                        let! body, bodyRange = parseMethodBody ()

                        return {
                            Attributes = attributes
                            IsPublic = isPublic
                            ReceiverName = receiverName
                            Name = methodName
                            TypeParameters = methodTypeParameters
                            Constraints = methodConstraints
                            Parameters = parameters
                            ReturnType = returnType
                            Body = body
                            BodyRange = bodyRange
                            Range = {
                                Start = memberToken.Range.Start
                                End = bodyRange.End
                            }
                        }
                    }

                let parseTypeDeclaration attributes =
                    parseResult {
                        let declarationToken = consume ()

                        let! typeToken =
                            match declarationToken.Kind with
                            | TypeKeyword
                            | AndKeyword -> Ok declarationToken
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        declarationToken.Range
                                        "expected 'type' or 'and'"
                                )

                        let isPublic =
                            match (current ()).Kind with
                            | InternalKeyword ->
                                consume ()
                                |> ignore

                                false
                            | _ -> true

                        let declarationNameToken = current ()

                        let! declaredTypeName =
                            qualifiedIdentifier "expected a type-abbreviation name"
                            |> Result.map qualifiedTypeName

                        let declarationName = declaredTypeName.Name

                        let parseConstraint () =
                            parseResult {
                                let! constrainedParameter, parameterToken =
                                    typeParameter "expected a constrained type parameter"

                                return!
                                    match (current ()).Kind with
                                    | Subtype ->
                                        consume ()
                                        |> ignore

                                        parseTypeExpression ()
                                        |> Result.map (fun superType ->
                                            ParsedSubtypeConstraint(
                                                constrainedParameter,
                                                superType,
                                                {
                                                    Start = parameterToken.Range.Start
                                                    End = superType.Range.End
                                                }
                                            )
                                        )
                                    | Colon ->
                                        parseResult {
                                            let! _ = expected Colon "expected ':'"
                                            let! _ = expected LeftParenthesis "expected '('"
                                            let! _ = expected MemberKeyword "expected 'member'"

                                            let! memberName, _ = identifier "expected a member name"

                                            let! _ = expected Colon "expected ':'"
                                            let! memberType = parseTypeExpression ()

                                            let! closeToken =
                                                expected RightParenthesis "expected ')'"

                                            return
                                                ParsedMemberConstraint(
                                                    constrainedParameter,
                                                    memberName,
                                                    memberType,
                                                    {
                                                        Start = parameterToken.Range.Start
                                                        End = closeToken.Range.End
                                                    }
                                                )
                                        }
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ':>' or a member constraint"
                                        )
                            }

                        let rec parseConstraints constraints =
                            parseResult {
                                let! constraint' = parseConstraint ()

                                let constraints =
                                    constraint'
                                    :: constraints

                                return!
                                    match (current ()).Kind with
                                    | AndKeyword ->
                                        consume ()
                                        |> ignore

                                        parseConstraints constraints
                                    | GreaterThan -> Ok(List.rev constraints)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'and' or '>' after a type constraint"
                                        )
                            }

                        let rec parseTypeParameters parameters =
                            parseResult {
                                let! parameter, _ = typeParameter "expected a type parameter"

                                let parameters =
                                    parameter
                                    :: parameters

                                return!
                                    match (current ()).Kind with
                                    | Comma ->
                                        consume ()
                                        |> ignore

                                        parseTypeParameters parameters
                                    | WhenKeyword ->
                                        consume ()
                                        |> ignore

                                        parseConstraints []
                                        |> Result.map (fun constraints ->
                                            List.rev parameters, constraints
                                        )
                                    | GreaterThan -> Ok(List.rev parameters, [])
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ',', 'when', or '>' after a type parameter"
                                        )
                            }

                        let! typeParameters, constraints =
                            match (current ()).Kind with
                            | LessThan ->
                                consume ()
                                |> ignore

                                parseResult {
                                    let! parameters, constraints = parseTypeParameters []
                                    let! _ = expected GreaterThan "expected '>'"
                                    return parameters, constraints
                                }
                            | _ -> Ok([], [])

                        let rec parseMethodConstraints constraints =
                            parseResult {
                                let! constraint' =
                                    match (current ()).Kind with
                                    | TypeParameter _ ->
                                        parseConstraint ()
                                        |> Result.map ParsedDirectConstraint
                                    | _ ->
                                        parseTypeExpression ()
                                        |> Result.map ParsedAbbreviationConstraint

                                let constraints =
                                    constraint'
                                    :: constraints

                                return!
                                    match (current ()).Kind with
                                    | AndKeyword ->
                                        consume ()
                                        |> ignore

                                        parseMethodConstraints constraints
                                    | GreaterThan -> Ok(List.rev constraints)
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected 'and' or '>' after a method constraint"
                                        )
                            }

                        let rec parseMethodTypeParameters parameters =
                            parseResult {
                                let! parameter, _ = typeParameter "expected a method type parameter"

                                let parameters =
                                    parameter
                                    :: parameters

                                return!
                                    match (current ()).Kind with
                                    | Comma ->
                                        consume ()
                                        |> ignore

                                        parseMethodTypeParameters parameters
                                    | WhenKeyword ->
                                        consume ()
                                        |> ignore

                                        parseMethodConstraints []
                                        |> Result.map (fun methodConstraints ->
                                            List.rev parameters, methodConstraints
                                        )
                                    | GreaterThan -> Ok(List.rev parameters, [])
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ',', 'when', or '>' after a method type parameter"
                                        )
                            }

                        let parseStaticMethod attributes allowDeclaredReturnType =
                            parseResult {
                                let! staticToken = expected StaticKeyword "expected 'static'"
                                let! _ = expected MemberKeyword "expected 'member'"

                                let isInline =
                                    match (current ()).Kind with
                                    | InlineKeyword ->
                                        consume ()
                                        |> ignore

                                        true
                                    | _ -> false

                                let isPublic =
                                    match (current ()).Kind with
                                    | InternalKeyword ->
                                        consume ()
                                        |> ignore

                                        false
                                    | _ -> true

                                let! methodName, _ = identifier "expected a static member name"

                                let! methodTypeParameters, methodConstraints =
                                    match (current ()).Kind with
                                    | LessThan ->
                                        consume ()
                                        |> ignore

                                        parseResult {
                                            let! parameters, constraints =
                                                parseMethodTypeParameters []

                                            let! _ = expected GreaterThan "expected '>'"
                                            return parameters, constraints
                                        }
                                    | _ -> Ok([], [])

                                let! parameters, argumentCounts =
                                    match (current ()).Kind with
                                    | LeftParenthesis ->
                                        let rec parseParameterGroups groups =
                                            parseResult {
                                                let! _ = expected LeftParenthesis "expected '('"
                                                let! group = parseParameters []
                                                let! _ = expected RightParenthesis "expected ')'"

                                                let groups =
                                                    group
                                                    :: groups

                                                return!
                                                    match (current ()).Kind with
                                                    | LeftParenthesis -> parseParameterGroups groups
                                                    | _ ->
                                                        let groups = List.rev groups

                                                        Ok(
                                                            groups
                                                            |> List.collect id,
                                                            groups
                                                            |> List.map List.length
                                                        )
                                            }

                                        parseParameterGroups []
                                    | Identifier _ ->
                                        let rec parseCurriedParameters parameters =
                                            match (current ()).Kind with
                                            | Identifier _ ->
                                                parseParameter ()
                                                |> Result.bind (fun parameter ->
                                                    parseCurriedParameters (
                                                        parameter
                                                        :: parameters
                                                    )
                                                )
                                            | Equals
                                            | Colon ->
                                                let parameters = List.rev parameters

                                                Ok(parameters, List.replicate parameters.Length 1)
                                            | _ ->
                                                Error(
                                                    prototypeDiagnostic
                                                        source.Path
                                                        (current ()).Range
                                                        "expected a curried parameter or '='"
                                                )

                                        parseCurriedParameters []
                                    | _ ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected a static member parameter"
                                        )

                                let! returnType =
                                    match allowDeclaredReturnType, (current ()).Kind with
                                    | true, Colon ->
                                        consume ()
                                        |> ignore

                                        parseTypeExpression ()
                                        |> Result.map Some
                                    | _ -> Ok None

                                let! _ = expected Equals "expected '='"
                                let! body, bodyRange = parseMethodBody ()

                                return {
                                    Attributes = attributes
                                    IsInline = isInline
                                    IsPublic = isPublic
                                    Name = methodName
                                    TypeParameters = methodTypeParameters
                                    Constraints = methodConstraints
                                    ArgumentCounts = argumentCounts
                                    Parameters = parameters
                                    ReturnType = returnType
                                    Body = body
                                    BodyRange = bodyRange
                                    Range = {
                                        Start = staticToken.Range.Start
                                        End = bodyRange.End
                                    }
                                }
                            }

                        let rec parseStaticMethods methods =
                            match (current ()).Kind with
                            | AttributeStart when
                                (current ()).Range.Start.Column > typeToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! attributes = parseDeclarationAttributes ()

                                    let! methodDeclaration =
                                        match (current ()).Kind with
                                        | StaticKeyword -> parseStaticMethod attributes true
                                        | _ ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "expected 'static' after member attributes"
                                            )

                                    return!
                                        parseStaticMethods (
                                            methodDeclaration
                                            :: methods
                                        )
                                }
                            | StaticKeyword ->
                                parseResult {
                                    let! methodDeclaration = parseStaticMethod [] true

                                    return!
                                        parseStaticMethods (
                                            methodDeclaration
                                            :: methods
                                        )
                                }
                            | AttributeStart
                            | LetKeyword
                            | ModuleKeyword
                            | TypeKeyword
                            | AndKeyword
                            | NamespaceKeyword
                            | EndOfFile -> Ok(List.rev methods)
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected a static member, type declaration, or end of file"
                                )

                        let rec parseObjectMethods methods =
                            match (current ()).Kind with
                            | AttributeStart when
                                (current ()).Range.Start.Column > typeToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! attributes = parseDeclarationAttributes ()

                                    let! methodDeclaration =
                                        match (current ()).Kind with
                                        | MemberKeyword ->
                                            parseInstanceMethod attributes
                                            |> Result.map ParsedInstanceObjectMethod
                                        | StaticKeyword ->
                                            parseStaticMethod attributes true
                                            |> Result.map ParsedStaticObjectMethod
                                        | _ ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "expected 'member' or 'static' after member attributes"
                                            )

                                    return!
                                        parseObjectMethods (
                                            methodDeclaration
                                            :: methods
                                        )
                                }
                            | MemberKeyword ->
                                parseResult {
                                    let! methodDeclaration = parseInstanceMethod []

                                    return!
                                        parseObjectMethods (
                                            ParsedInstanceObjectMethod methodDeclaration
                                            :: methods
                                        )
                                }
                            | StaticKeyword ->
                                parseResult {
                                    let! methodDeclaration = parseStaticMethod [] true

                                    return!
                                        parseObjectMethods (
                                            ParsedStaticObjectMethod methodDeclaration
                                            :: methods
                                        )
                                }
                            | AttributeStart
                            | LetKeyword
                            | ModuleKeyword
                            | TypeKeyword
                            | AndKeyword
                            | NamespaceKeyword
                            | EndOfFile -> Ok(List.rev methods)
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected an object member, type declaration, or end of file"
                                )

                        let parseField fieldAttributes =
                            parseResult {
                                let! fieldToken = expected ValKeyword "expected 'val'"

                                let isMutable =
                                    match (current ()).Kind with
                                    | MutableKeyword ->
                                        consume ()
                                        |> ignore

                                        true
                                    | _ -> false

                                let! fieldName, _ = identifier "expected a field name"
                                let! _ = expected Colon "expected ':'"
                                let! fieldType = parseTypeExpression ()

                                return {
                                    Name = fieldName
                                    IsMutable = isMutable
                                    Type = fieldType
                                    Attributes = fieldAttributes
                                    Range = {
                                        Start = fieldToken.Range.Start
                                        End = fieldType.Range.End
                                    }
                                }
                            }

                        let rec parseFields fields =
                            match (current ()).Kind with
                            | AttributeStart ->
                                parseResult {
                                    let! fieldAttributes = parseDeclarationAttributes ()
                                    let! field = parseField fieldAttributes

                                    return!
                                        parseFields (
                                            field
                                            :: fields
                                        )
                                }
                            | _ -> Ok(List.rev fields)

                        let isCurrentModuleAugmentation = (current ()).Kind = WithKeyword

                        let! isObjectType =
                            match (current ()).Kind with
                            | WithKeyword ->
                                consume ()
                                |> ignore

                                Ok false
                            | LeftParenthesis ->
                                parseResult {
                                    let! _ = expected LeftParenthesis "expected '('"
                                    let! _ = expected RightParenthesis "expected ')'"
                                    return true
                                }
                            | _ -> Ok false

                        let! _ =
                            if isCurrentModuleAugmentation then
                                Ok typeToken
                            else
                                expected Equals "expected '='"

                        let! baseType =
                            if isObjectType then
                                match (current ()).Kind with
                                | InheritKeyword ->
                                    consume ()
                                    |> ignore

                                    parseResult {
                                        let! parsedBaseType = parseTypeExpression ()
                                        let! _ = expected LeftParenthesis "expected '('"
                                        let! _ = expected RightParenthesis "expected ')'"
                                        return Some parsedBaseType
                                    }
                                | _ -> Ok None
                            else
                                Ok None

                        return!
                            if isCurrentModuleAugmentation then
                                if
                                    not (List.isEmpty attributes)
                                    || not (List.isEmpty typeParameters)
                                    || not (List.isEmpty constraints)
                                then
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            typeToken.Range
                                            "attributed or generic type augmentations are not yet supported"
                                    )
                                else
                                    parseObjectMethods []
                                    |> Result.bind (fun methods ->
                                        match List.tryLast methods with
                                        | Some lastMethod ->
                                            Ok(
                                                ParsedObjectType {
                                                    Container =
                                                        ParsedCurrentModuleAugmentation
                                                            declaredTypeName
                                                    Name = declarationName
                                                    BaseType = None
                                                    Methods = methods
                                                    ConstructorRange = declarationNameToken.Range
                                                    Range = {
                                                        Start = typeToken.Range.Start
                                                        End = lastMethod.Range.End
                                                    }
                                                }
                                            )
                                        | None ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "a type augmentation must declare a member"
                                            )
                                    )
                            elif not (String.IsNullOrEmpty(declaredTypeName.Namespace)) then
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        declarationNameToken.Range
                                        "qualified type names are supported only for type augmentations"
                                )
                            else
                                match isObjectType, (current ()).Kind with
                                | true, _ when
                                    not (List.isEmpty attributes)
                                    || not (List.isEmpty typeParameters)
                                    || not (List.isEmpty constraints)
                                    ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            typeToken.Range
                                            "attributed or generic object types are not yet supported"
                                    )
                                | true, MemberKeyword
                                | true, StaticKeyword
                                | true, AttributeStart ->
                                    parseObjectMethods []
                                    |> Result.bind (fun methods ->
                                        match List.tryLast methods with
                                        | Some lastMethod ->
                                            Ok(
                                                ParsedObjectType {
                                                    Container = OrdinaryObjectType
                                                    Name = declarationName
                                                    BaseType = baseType
                                                    Methods = methods
                                                    ConstructorRange = declarationNameToken.Range
                                                    Range = {
                                                        Start = typeToken.Range.Start
                                                        End = lastMethod.Range.End
                                                    }
                                                }
                                            )
                                        | None ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "an object type must declare an instance member"
                                            )
                                    )
                                | true, _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "an object type must declare an instance member"
                                    )
                                | false, AttributeStart when not (List.isEmpty attributes) ->
                                    parseFields []
                                    |> Result.bind (fun fields ->
                                        match List.tryLast fields with
                                        | Some lastField ->
                                            parseObjectMethods []
                                            |> Result.map (fun methods ->
                                                let declarationEnd =
                                                    methods
                                                    |> List.tryLast
                                                    |> Option.map _.Range.End
                                                    |> Option.defaultValue lastField.Range.End

                                                ParsedStructType {
                                                    Name = declarationName
                                                    TypeParameters = typeParameters
                                                    Attributes = attributes
                                                    Fields = fields
                                                    Methods = methods
                                                    Range = {
                                                        Start = typeToken.Range.Start
                                                        End = declarationEnd
                                                    }
                                                }
                                            )
                                        | None ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "an attributed struct type must declare a field"
                                            )
                                    )
                                | false, StaticKeyword when
                                    List.isEmpty typeParameters
                                    && List.isEmpty constraints
                                    ->
                                    parseStaticMethods []
                                    |> Result.bind (fun methods ->
                                        match List.tryLast methods with
                                        | Some lastMethod ->
                                            Ok(
                                                ParsedStaticType {
                                                    IsPublic = isPublic
                                                    Name = declarationName
                                                    Methods = methods
                                                    Range = {
                                                        Start = typeToken.Range.Start
                                                        End = lastMethod.Range.End
                                                    }
                                                }
                                            )
                                        | None ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "a static type must declare a member"
                                            )
                                    )
                                | false, _ ->
                                    parseResult {
                                        let targetStart = (current ()).Range.Start
                                        let! targetType = parseTypeExpression ()

                                        let! allowsNull, targetEnd =
                                            match (current ()).Kind with
                                            | Bar ->
                                                consume ()
                                                |> ignore

                                                match
                                                    expected NullKeyword "expected 'null' after '|'"
                                                with
                                                | Error error -> Error error
                                                | Ok nullToken -> Ok(true, nullToken.Range.End)
                                            | _ -> Ok(false, input.[index - 1].Range.End)

                                        return
                                            ParsedTypeAbbreviation {
                                                Name = declarationName
                                                TypeParameters = typeParameters
                                                Constraints = constraints
                                                Target = {
                                                    Type = targetType
                                                    AllowsNull = allowsNull
                                                    Range = { Start = targetStart; End = targetEnd }
                                                }
                                                Range = {
                                                    Start = typeToken.Range.Start
                                                    End = targetEnd
                                                }
                                            }
                                    }
                    }

                let parseExtensionModule attributes =
                    parseResult {
                        let! moduleToken = expected ModuleKeyword "expected 'module'"
                        let! moduleName, _ = identifier "expected a module name"
                        let! _ = expected Equals "expected '='"
                        let! declaration = parseTypeDeclaration []

                        return!
                            match declaration with
                            | ParsedObjectType({
                                                   Container = ParsedCurrentModuleAugmentation targetTypeName
                                               } as objectType) ->
                                Ok(
                                    ParsedObjectType {
                                        objectType with
                                            Container =
                                                ParsedExtensionModule(
                                                    moduleName,
                                                    attributes,
                                                    targetTypeName
                                                )
                                            Range = {
                                                Start = moduleToken.Range.Start
                                                End = objectType.Range.End
                                            }
                                    }
                                )
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        moduleToken.Range
                                        "an extension module must begin with a type augmentation"
                                )
                    }

                let rec parseNestedModule parentPath attributes =
                    parseResult {
                        let! moduleToken = expected ModuleKeyword "expected 'module'"
                        let! moduleName, moduleNameToken = identifier "expected a module name"
                        let! _ = expected Equals "expected '='"

                        let modulePath =
                            parentPath
                            @ [ moduleName ]

                        let! openedNamespaces = parseOpenNamespaces []

                        let parseModuleBinding () =
                            parseResult {
                                let! letToken = expected LetKeyword "expected 'let'"

                                let rec parseBindingModifiers isInline isPublic =
                                    match (current ()).Kind with
                                    | InlineKeyword ->
                                        consume ()
                                        |> ignore

                                        parseBindingModifiers true isPublic
                                    | InternalKeyword ->
                                        consume ()
                                        |> ignore

                                        parseBindingModifiers isInline false
                                    | _ -> isInline, isPublic

                                let isInline, isPublic = parseBindingModifiers false true

                                let! bindingName, _ = identifier "expected a module binding name"

                                let! typeParameters =
                                    match (current ()).Kind with
                                    | LessThan ->
                                        consume ()
                                        |> ignore

                                        let rec parseTypeParameters parameters =
                                            parseResult {
                                                let! parameter, _ =
                                                    typeParameter "expected a module type parameter"

                                                let parameters =
                                                    parameter
                                                    :: parameters

                                                return!
                                                    match (current ()).Kind with
                                                    | Comma ->
                                                        consume ()
                                                        |> ignore

                                                        parseTypeParameters parameters
                                                    | GreaterThan -> Ok(List.rev parameters)
                                                    | _ ->
                                                        Error(
                                                            prototypeDiagnostic
                                                                source.Path
                                                                (current ()).Range
                                                                "expected ',' or '>' after a module type parameter"
                                                        )
                                            }

                                        parseResult {
                                            let! parameters = parseTypeParameters []
                                            let! _ = expected GreaterThan "expected '>'"
                                            return parameters
                                        }
                                    | _ -> Ok []

                                return!
                                    match (current ()).Kind with
                                    | LeftParenthesis ->
                                        parseResult {
                                            let rec parseParameterGroups groups =
                                                parseResult {
                                                    let! _ = expected LeftParenthesis "expected '('"
                                                    let! group = parseParameters []

                                                    let! _ =
                                                        expected RightParenthesis "expected ')'"

                                                    let groups =
                                                        group
                                                        :: groups

                                                    return!
                                                        match (current ()).Kind with
                                                        | LeftParenthesis ->
                                                            parseParameterGroups groups
                                                        | _ ->
                                                            let groups = List.rev groups

                                                            Ok(
                                                                groups
                                                                |> List.collect id,
                                                                groups
                                                                |> List.map List.length
                                                            )
                                                }

                                            let! parameters, argumentCounts =
                                                parseParameterGroups []

                                            let! returnType =
                                                match (current ()).Kind with
                                                | Colon ->
                                                    consume ()
                                                    |> ignore

                                                    parseTypeExpression ()
                                                    |> Result.map Some
                                                | _ -> Ok None

                                            let! _ = expected Equals "expected '='"
                                            let! body, bodyRange = parseExpression ()

                                            return
                                                Choice2Of2 {
                                                    Attributes = []
                                                    IsInline = isInline
                                                    IsPublic = isPublic
                                                    Name = bindingName
                                                    TypeParameters = typeParameters
                                                    Constraints = []
                                                    ArgumentCounts = argumentCounts
                                                    Parameters = parameters
                                                    ReturnType = returnType
                                                    Body = body
                                                    BodyRange = bodyRange
                                                    Range = {
                                                        Start = letToken.Range.Start
                                                        End = bodyRange.End
                                                    }
                                                }
                                        }
                                    | _ when isInline ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "an inline module binding must declare a parameter list"
                                        )
                                    | _ ->
                                        parseResult {
                                            let! _ = expected Equals "expected '='"
                                            let! body, bodyRange = parseExpression ()

                                            return
                                                Choice1Of2 {
                                                    Name = bindingName
                                                    Body = body
                                                    BodyRange = bodyRange
                                                    Range = {
                                                        Start = letToken.Range.Start
                                                        End = bodyRange.End
                                                    }
                                                }
                                        }
                            }

                        let rec parseModuleBindings
                            openedNamespaces
                            values
                            methods
                            extensions
                            (modules: ParsedNestedModuleDeclaration list)
                            lastRange
                            =
                            match (current ()).Kind with
                            | LetKeyword when
                                (current ()).Range.Start.Column > moduleToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! binding = parseModuleBinding ()

                                    return!
                                        match binding with
                                        | Choice1Of2 value ->
                                            parseModuleBindings
                                                openedNamespaces
                                                (value
                                                 :: values)
                                                methods
                                                extensions
                                                modules
                                                (Some value.Range)
                                        | Choice2Of2 methodDeclaration ->
                                            parseModuleBindings
                                                openedNamespaces
                                                values
                                                (methodDeclaration
                                                 :: methods)
                                                extensions
                                                modules
                                                (Some methodDeclaration.Range)
                                }
                            | OpenKeyword when
                                (current ()).Range.Start.Column > moduleToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! additionalNamespaces = parseOpenNamespaces []

                                    return!
                                        parseModuleBindings
                                            (openedNamespaces
                                             @ additionalNamespaces)
                                            values
                                            methods
                                            extensions
                                            modules
                                            lastRange
                                }
                            | TypeKeyword when
                                (current ()).Range.Start.Column > moduleToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! extensionDeclaration = parseTypeDeclaration []

                                    let! extension =
                                        match extensionDeclaration with
                                        | ParsedObjectType({
                                                               Container = ParsedCurrentModuleAugmentation targetTypeName
                                                           } as objectType) ->
                                            Ok {
                                                objectType with
                                                    Container =
                                                        ParsedExtensionModule(
                                                            moduleName,
                                                            attributes,
                                                            targetTypeName
                                                        )
                                            }
                                        | _ ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "a nested module type declaration must be a type augmentation"
                                            )

                                    return!
                                        parseModuleBindings
                                            openedNamespaces
                                            values
                                            methods
                                            (extension
                                             :: extensions)
                                            modules
                                            (Some extension.Range)
                                }
                            | AttributeStart when
                                (current ()).Range.Start.Column > moduleToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! nestedAttributes = parseDeclarationAttributes ()

                                    let! (nestedModule: ParsedNestedModuleDeclaration) =
                                        match (current ()).Kind with
                                        | ModuleKeyword ->
                                            parseNestedModule modulePath nestedAttributes
                                        | _ ->
                                            Error(
                                                prototypeDiagnostic
                                                    source.Path
                                                    (current ()).Range
                                                    "expected a nested module after the declaration attributes"
                                            )

                                    return!
                                        parseModuleBindings
                                            openedNamespaces
                                            values
                                            methods
                                            extensions
                                            (nestedModule
                                             :: modules)
                                            (Some nestedModule.Range)
                                }
                            | ModuleKeyword when
                                (current ()).Range.Start.Column > moduleToken.Range.Start.Column
                                ->
                                parseResult {
                                    let! (nestedModule: ParsedNestedModuleDeclaration) =
                                        parseNestedModule modulePath []

                                    return!
                                        parseModuleBindings
                                            openedNamespaces
                                            values
                                            methods
                                            extensions
                                            (nestedModule
                                             :: modules)
                                            (Some nestedModule.Range)
                                }
                            | EndOfFile ->
                                Ok(
                                    openedNamespaces,
                                    List.rev values,
                                    List.rev methods,
                                    List.rev extensions,
                                    List.rev modules,
                                    lastRange
                                )
                            | _ when
                                (current ()).Range.Start.Column
                                <= moduleToken.Range.Start.Column
                                ->
                                Ok(
                                    openedNamespaces,
                                    List.rev values,
                                    List.rev methods,
                                    List.rev extensions,
                                    List.rev modules,
                                    lastRange
                                )
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected a nested-module binding declaration"
                                )

                        let! openedNamespaces, values, methods, extensions, modules, lastRange =
                            parseModuleBindings openedNamespaces [] [] [] [] None

                        return!
                            match lastRange with
                            | Some lastRange ->
                                Ok(
                                    {
                                        Name = moduleName
                                        ModulePath = modulePath
                                        Attributes = attributes
                                        OpenedNamespaces = openedNamespaces
                                        Values = values
                                        Methods = methods
                                        Extensions = extensions
                                        Modules = modules
                                        Range = {
                                            Start = moduleToken.Range.Start
                                            End = lastRange.End
                                        }
                                    }
                                    : ParsedNestedModuleDeclaration
                                )
                            | None ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        moduleNameToken.Range
                                        "a nested module must declare a value, function, or module"
                                )
                    }

                let moduleBodyStartsTypeExtension () =
                    let rec afterHeader tokenIndex =
                        if
                            tokenIndex
                            >= input.Length
                        then
                            None
                        elif input.[tokenIndex].Kind = Equals then
                            Some(
                                tokenIndex
                                + 1
                            )
                        else
                            afterHeader (
                                tokenIndex
                                + 1
                            )

                    match afterHeader index with
                    | Some bodyIndex when
                        bodyIndex < input.Length
                        && input.[bodyIndex].Kind = TypeKeyword
                        ->
                        let rec findWith tokenIndex =
                            if
                                tokenIndex
                                >= input.Length
                            then
                                false
                            else
                                match input.[tokenIndex].Kind with
                                | WithKeyword -> true
                                | Equals
                                | LeftParenthesis
                                | LetKeyword
                                | MemberKeyword
                                | StaticKeyword
                                | ModuleKeyword
                                | NamespaceKeyword
                                | EndOfFile -> false
                                | _ ->
                                    findWith (
                                        tokenIndex
                                        + 1
                                    )

                        findWith (
                            bodyIndex
                            + 1
                        )
                    | _ -> false

                let parseNestedOrExtensionModule attributes =
                    if moduleBodyStartsTypeExtension () then
                        parseExtensionModule attributes
                    else
                        parseNestedModule [] attributes
                        |> Result.map ParsedNestedModule

                let canStartTypeDeclaration declarations token =
                    match token with
                    | TypeKeyword -> true
                    | AndKeyword ->
                        match declarations with
                        | declaration :: _ -> ParsedDeclaration.isTypeDeclaration declaration
                        | _ -> false
                    | _ -> false

                let rec parseTypeAbbreviations declarations =
                    match (current ()).Kind with
                    | token when canStartTypeDeclaration declarations token ->
                        parseResult {
                            let! declaration = parseTypeDeclaration []

                            return!
                                parseTypeAbbreviations (
                                    declaration
                                    :: declarations
                                )
                        }
                    | ModuleKeyword ->
                        parseResult {
                            let! declaration = parseNestedOrExtensionModule []

                            return!
                                parseTypeAbbreviations (
                                    declaration
                                    :: declarations
                                )
                        }
                    | AttributeStart ->
                        parseResult {
                            let! attributes = parseDeclarationAttributes ()

                            let! declaration =
                                match (current ()).Kind with
                                | token when canStartTypeDeclaration declarations token ->
                                    parseTypeDeclaration attributes
                                | ModuleKeyword -> parseNestedOrExtensionModule attributes
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected a type or module declaration after attributes"
                                    )

                            return!
                                parseTypeAbbreviations (
                                    declaration
                                    :: declarations
                                )
                        }
                    | NamespaceKeyword
                    | EndOfFile -> Ok(List.rev declarations)
                    | _ ->
                        Error(
                            prototypeDiagnostic
                                source.Path
                                (current ()).Range
                                "expected a namespace type or module declaration"
                        )

                let rec parseModuleDeclarations
                    attributeOpenNamespaces
                    assemblyAttributes
                    declarations
                    =
                    match (current ()).Kind with
                    | LetKeyword ->
                        parseResult {
                            let! declaration = parseLiteralDeclaration ()

                            return!
                                parseModuleDeclarations
                                    attributeOpenNamespaces
                                    assemblyAttributes
                                    (declaration
                                     :: declarations)
                        }
                    | token when canStartTypeDeclaration declarations token ->
                        parseResult {
                            let! declaration = parseTypeDeclaration []

                            return!
                                parseModuleDeclarations
                                    attributeOpenNamespaces
                                    assemblyAttributes
                                    (declaration
                                     :: declarations)
                        }
                    | ModuleKeyword ->
                        parseResult {
                            let! declaration = parseNestedOrExtensionModule []

                            return!
                                parseModuleDeclarations
                                    attributeOpenNamespaces
                                    assemblyAttributes
                                    (declaration
                                     :: declarations)
                        }
                    | AttributeStart when
                        index + 1 < input.Length
                        && input.[index + 1].Kind = AssemblyKeyword
                        ->
                        parseResult {
                            let! firstAttribute = parseAssemblyAttribute attributeOpenNamespaces

                            let! parsedAssemblyAttributes =
                                parseRemainingAssemblyAttributes attributeOpenNamespaces [
                                    firstAttribute
                                ]

                            let! _ = expected DoKeyword "expected 'do'"
                            let! _ = expected LeftParenthesis "expected '('"
                            let! _ = expected RightParenthesis "expected ')'"

                            return!
                                parseModuleDeclarations
                                    attributeOpenNamespaces
                                    (assemblyAttributes
                                     @ parsedAssemblyAttributes)
                                    declarations
                        }
                    | AttributeStart ->
                        parseResult {
                            let! attributes = parseDeclarationAttributes ()

                            let! declaration =
                                match (current ()).Kind with
                                | token when canStartTypeDeclaration declarations token ->
                                    parseTypeDeclaration attributes
                                | ModuleKeyword -> parseNestedOrExtensionModule attributes
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected a type or module declaration after attributes"
                                    )

                            return!
                                parseModuleDeclarations
                                    attributeOpenNamespaces
                                    assemblyAttributes
                                    (declaration
                                     :: declarations)
                        }
                    | NamespaceKeyword
                    | EndOfFile -> Ok(assemblyAttributes, List.rev declarations)
                    | _ ->
                        Error(
                            prototypeDiagnostic
                                source.Path
                                (current ()).Range
                                "expected a module declaration or end of file"
                        )

                let namespaceFile namespaceName openNamespaces assemblyAttributes declarations = {
                    StableId =
                        "namespace:"
                        + namespaceName
                    ContainerKind = NamespaceSource
                    Namespace = namespaceName
                    Name = namespaceName
                    IsPublic = false
                    OpenedNamespaces = openNamespaces
                    SourceChecksum =
                        sourceChecksum
                        |> ImmutableArray.CreateRange<byte>
                    ContentFingerprint = contentFingerprint
                    Attributes = []
                    AssemblyAttributes = assemblyAttributes
                    Declarations = declarations
                }

                let moduleBodyStartsOrdinaryValue () =
                    let rec afterHeader tokenIndex =
                        if
                            tokenIndex
                            >= input.Length
                        then
                            input.Length
                        elif input.[tokenIndex].Kind = Equals then
                            tokenIndex
                            + 1
                        else
                            afterHeader (
                                tokenIndex
                                + 1
                            )

                    let rec afterLine line tokenIndex =
                        if
                            tokenIndex < input.Length
                            && input.[tokenIndex].Range.Start.Line = line
                        then
                            afterLine
                                line
                                (tokenIndex
                                 + 1)
                        else
                            tokenIndex

                    let rec afterOpenDeclarations tokenIndex =
                        if
                            tokenIndex < input.Length
                            && input.[tokenIndex].Kind = OpenKeyword
                        then
                            afterLine
                                input.[tokenIndex].Range.Start.Line
                                (tokenIndex
                                 + 1)
                            |> afterOpenDeclarations
                        else
                            tokenIndex

                    let bodyIndex =
                        afterHeader index
                        |> afterOpenDeclarations

                    bodyIndex < input.Length
                    && input.[bodyIndex].Kind = LetKeyword
                    && (bodyIndex
                        + 1
                        >= input.Length
                        || input
                            .[bodyIndex
                              + 1].Kind
                           <> AttributeStart)

                let finishNamespaceFile
                    namespaceName
                    openNamespaces
                    assemblyAttributes
                    moduleAttributes
                    =
                    let sourceChecksum =
                        sourceChecksum
                        |> ImmutableArray.CreateRange<byte>

                    match (current ()).Kind with
                    | NamespaceKeyword
                    | EndOfFile ->
                        Ok {
                            StableId =
                                "namespace:"
                                + namespaceName
                            ContainerKind = NamespaceSource
                            Namespace = namespaceName
                            Name = namespaceName
                            IsPublic = false
                            OpenedNamespaces = openNamespaces
                            SourceChecksum = sourceChecksum
                            ContentFingerprint = contentFingerprint
                            Attributes = []
                            AssemblyAttributes = assemblyAttributes
                            Declarations = []
                        }
                    | ModuleKeyword when moduleBodyStartsOrdinaryValue () ->
                        parseNestedModule [] moduleAttributes
                        |> Result.map (fun moduleDeclaration ->
                            namespaceFile namespaceName openNamespaces assemblyAttributes [
                                ParsedNestedModule moduleDeclaration
                            ]
                        )
                    | ModuleKeyword ->
                        consume ()
                        |> ignore

                        parseResult {
                            let! isPublic =
                                match (current ()).Kind with
                                | InternalKeyword ->
                                    consume ()
                                    |> ignore

                                    Ok false
                                | _ -> Ok true

                            let! moduleName, _ = identifier "expected a module name"
                            let! _ = expected Equals "expected '='"
                            let! moduleOpenNamespaces = parseOpenNamespaces []

                            let! moduleAssemblyAttributes, declarations =
                                parseModuleDeclarations
                                    (openNamespaces
                                     @ moduleOpenNamespaces) [] []

                            return {
                                StableId =
                                    "module:"
                                    + namespaceName
                                    + "."
                                    + moduleName
                                ContainerKind = ModuleSource
                                Namespace = namespaceName
                                Name = moduleName
                                IsPublic = isPublic
                                OpenedNamespaces =
                                    openNamespaces
                                    @ moduleOpenNamespaces
                                SourceChecksum = sourceChecksum
                                ContentFingerprint = contentFingerprint
                                Attributes = moduleAttributes
                                AssemblyAttributes =
                                    assemblyAttributes
                                    @ moduleAssemblyAttributes
                                Declarations = declarations
                            }
                        }
                    | _ ->
                        Error(
                            prototypeDiagnostic
                                source.Path
                                (current ()).Range
                                "expected a module or end of file"
                        )

                let rec attributeSequenceIsFollowedByDo attributeStartIndex =
                    let rec afterAttribute tokenIndex =
                        if
                            tokenIndex
                            >= input.Length
                        then
                            tokenIndex
                        elif input.[tokenIndex].Kind = AttributeEnd then
                            tokenIndex
                            + 1
                        else
                            afterAttribute (
                                tokenIndex
                                + 1
                            )

                    let nextIndex = afterAttribute attributeStartIndex

                    if
                        nextIndex
                        >= input.Length
                    then
                        false
                    elif input.[nextIndex].Kind = AttributeStart then
                        attributeSequenceIsFollowedByDo nextIndex
                    else
                        input.[nextIndex].Kind = DoKeyword

                parseResult {
                    let! namespaceName = qualifiedIdentifier "expected a namespace name"
                    let! openNamespaces = parseOpenNamespaces []

                    return!
                        match (current ()).Kind with
                        | AttributeStart when
                            (index + 1 < input.Length
                             && input.[index + 1].Kind = AssemblyKeyword)
                            || attributeSequenceIsFollowedByDo index
                            ->
                            parseResult {
                                let! firstAttribute = parseAssemblyAttribute openNamespaces

                                let! assemblyAttributes =
                                    parseRemainingAssemblyAttributes openNamespaces [
                                        firstAttribute
                                    ]

                                let! _ = expected DoKeyword "expected 'do'"
                                let! _ = expected LeftParenthesis "expected '('"
                                let! _ = expected RightParenthesis "expected ')'"

                                return!
                                    finishNamespaceFile
                                        namespaceName
                                        openNamespaces
                                        assemblyAttributes
                                        []
                            }
                        | AttributeStart ->
                            parseResult {
                                let! moduleAttributes = parseDeclarationAttributes ()

                                return!
                                    finishNamespaceFile
                                        namespaceName
                                        openNamespaces
                                        []
                                        moduleAttributes
                            }
                        | TypeKeyword ->
                            parseResult {
                                let! declarations = parseTypeAbbreviations []

                                return namespaceFile namespaceName openNamespaces [] declarations
                            }
                        | ModuleKeyword -> finishNamespaceFile namespaceName openNamespaces [] []
                        | NamespaceKeyword
                        | EndOfFile -> Ok(namespaceFile namespaceName openNamespaces [] [])
                        | _ ->
                            Error(
                                prototypeDiagnostic
                                    source.Path
                                    (current ()).Range
                                    "expected an assembly attribute, type abbreviation, or end of file"
                            )
                }

            let parseContainer () =
                match (current ()).Kind with
                | ModuleKeyword ->
                    consume ()
                    |> ignore

                    parseModule ()
                | NamespaceKeyword ->
                    consume ()
                    |> ignore

                    parseNamespaceFile ()
                | _ ->
                    Error(
                        prototypeDiagnostic
                            source.Path
                            (current ()).Range
                            "expected 'module' or 'namespace'"
                    )

            let rec parseContainers containers =
                if
                    index
                    >= input.Length
                then
                    Ok(List.rev containers)
                else
                    match (current ()).Kind with
                    | EndOfFile -> Ok(List.rev containers)
                    | _ ->
                        parseResult {
                            let! parsed = parseContainer ()

                            return!
                                parseContainers (
                                    parsed
                                    :: containers
                                )
                        }

            parseContainers []
