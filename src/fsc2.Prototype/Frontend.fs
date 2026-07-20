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
        | WhenKeyword
        | AndKeyword
        | MemberKeyword
        | NullKeyword
        | LetKeyword
        | DoKeyword
        | ValKeyword
        | MutableKeyword
        | FunKeyword
        | Identifier of string
        | TypeParameter of string
        | Integer of int
        | StringLiteralToken of string
        | AttributeStart
        | AttributeEnd
        | LeftParenthesis
        | RightParenthesis
        | Colon
        | Subtype
        | Comma
        | Semicolon
        | Dot
        | LessThan
        | GreaterThan
        | Arrow
        | LeftArrow
        | Ampersand
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
                | "when" -> add WhenKeyword start
                | "and" -> add AndKeyword start
                | "member" -> add MemberKeyword start
                | "null" -> add NullKeyword start
                | "let" -> add LetKeyword start
                | "do" -> add DoKeyword start
                | "val" -> add ValKeyword start
                | "mutable" -> add MutableKeyword start
                | "fun" -> add FunKeyword start
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
                    && text.[offset + 1] = '-'
                then
                    advance ()
                    advance ()
                    add LeftArrow start
                else
                    advance ()

                    match current with
                    | '(' -> add LeftParenthesis start
                    | ')' -> add RightParenthesis start
                    | ':' -> add Colon start
                    | ',' -> add Comma start
                    | ';' -> add Semicolon start
                    | '.' -> add Dot start
                    | '<' -> add LessThan start
                    | '>' -> add GreaterThan start
                    | '&' -> add Ampersand start
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
                    let! first = parseTypeAtom ()

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

                let parseCallArguments () =
                    let rec loop arguments =
                        parseResult {
                            let! argument =
                                match (current ()).Kind with
                                | Ampersand ->
                                    consume ()
                                    |> ignore

                                    identifier "expected a trait-call argument"
                                    |> Result.map (
                                        fst
                                        >> AddressOfExpression
                                    )
                                | _ ->
                                    parseExpression ()
                                    |> Result.map fst

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
                                | Dot -> loop members
                                | _ -> Ok(List.rev members)
                        }

                    loop []
                    |> Result.map (fun members -> rootName, members)

                match expressionToken.Kind with
                | LeftParenthesis ->
                    parseResult {
                        let! _ = expected LeftParenthesis "expected '('"
                        let! firstExpression, firstRange = parseExpression ()

                        let rec parseApplications expression expressionRange =
                            parseResult {
                                return!
                                    match (current ()).Kind with
                                    | RightParenthesis ->
                                        consume ()
                                        |> ignore

                                        Ok(expression, expressionRange)
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
                | LetKeyword ->
                    parseResult {
                        let! letToken = expected LetKeyword "expected 'let'"
                        let! bindingName, _ = identifier "expected a local binding name"
                        let! _ = expected Equals "expected '=' after a local binding name"
                        let! value, valueRange = parseExpression ()
                        let! body, bodyRange = parseExpression ()

                        let bindingRange = {
                            Start = letToken.Range.Start
                            End = valueRange.End
                        }

                        return
                            LetExpression(bindingName, value, body, bindingRange, bodyRange),
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
                        let! parameterName, _ = identifier "expected a lambda parameter"
                        let! _ = expected Arrow "expected '->'"

                        let rec parseBody expressions firstRange =
                            parseResult {
                                let! expression, expressionRange = parseExpression ()

                                let expressions =
                                    expression
                                    :: expressions

                                let firstRange =
                                    firstRange
                                    |> Option.defaultValue expressionRange

                                return!
                                    match (current ()).Kind with
                                    | RightParenthesis ->
                                        let expressions = List.rev expressions

                                        let body =
                                            match expressions with
                                            | [ expression ] -> expression
                                            | _ -> SequentialExpression expressions

                                        Ok(
                                            body,
                                            {
                                                Start = firstRange.Start
                                                End = expressionRange.End
                                            }
                                        )
                                    | EndOfFile ->
                                        Error(
                                            prototypeDiagnostic
                                                source.Path
                                                (current ()).Range
                                                "expected ')' after a lambda body"
                                        )
                                    | _ -> parseBody expressions (Some firstRange)
                            }

                        let! body, bodyRange = parseBody [] None

                        return
                            LambdaExpression(parameterName, body),
                            {
                                Start = funToken.Range.Start
                                End = bodyRange.End
                            }
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

                        return
                            UnitApplication functionName,
                            {
                                Start = startToken.Range.Start
                                End = closeToken.Range.End
                            }
                    }
                | Identifier _ when
                    index + 1 < input.Length
                    && input.[index + 1].Kind = LessThan
                    ->
                    parseResult {
                        let! constructedType = parseTypeExpression ()
                        let! openToken = expected LeftParenthesis "expected '('"
                        let! argument, _ = parseExpression ()
                        let! closeToken = expected RightParenthesis "expected ')'"

                        let argumentRange = {
                            Start = openToken.Range.Start
                            End = closeToken.Range.End
                        }

                        return
                            TypeConstruction(constructedType, argument, argumentRange),
                            {
                                Start = expressionToken.Range.Start
                                End = closeToken.Range.End
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
                            | LeftParenthesis when memberPath.Length = 1 ->
                                parseResult {
                                    let! _ = expected LeftParenthesis "expected '('"
                                    let! arguments = parseCallArguments ()
                                    let! closeToken = expected RightParenthesis "expected ')'"

                                    return
                                        MemberCall(receiverName, memberPath.Head, arguments),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = closeToken.Range.End
                                        }
                                }
                            | LeftArrow ->
                                parseResult {
                                    let! _ = expected LeftArrow "expected '<-'"
                                    let! value, valueRange = parseExpression ()

                                    return
                                        MemberAssignment(receiverName, memberPath, value),
                                        {
                                            Start = expressionToken.Range.Start
                                            End = valueRange.End
                                        }
                                }
                            | Identifier _ when memberPath.Length = 1 ->
                                parseResult {
                                    let! argument, argumentRange = parseExpression ()

                                    return
                                        MemberCall(receiverName, memberPath.Head, [ argument ]),
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
                                        "expected a member call or assignment"
                                )
                    }
                | Identifier value ->
                    consume ()
                    |> ignore

                    Ok(ValueReference value, expressionToken.Range)
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

                        let! declarationName, declarationNameToken =
                            identifier "expected a type-abbreviation name"

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

                        let parseParameter () =
                            parseResult {
                                let! attributes =
                                    match (current ()).Kind with
                                    | AttributeStart -> parseDeclarationAttributes ()
                                    | _ -> Ok []

                                let! parameterName, parameterToken =
                                    identifier "expected a parameter name"

                                let! _ = expected Colon "expected ':'"
                                let! parameterType = parseTypeExpression ()

                                return {
                                    Attributes = attributes
                                    Name = parameterName
                                    Type = parameterType
                                    Range = {
                                        Start = parameterToken.Range.Start
                                        End = parameterType.Range.End
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

                        let parseStaticMethod attributes allowDeclaredReturnType =
                            parseResult {
                                let! staticToken = expected StaticKeyword "expected 'static'"
                                let! _ = expected MemberKeyword "expected 'member'"
                                let! _ = expected InlineKeyword "expected 'inline'"

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

                                let! _ = expected LeftParenthesis "expected '('"
                                let! parameters = parseParameters []
                                let! _ = expected RightParenthesis "expected ')'"

                                let! returnType =
                                    match allowDeclaredReturnType, (current ()).Kind with
                                    | true, Colon ->
                                        consume ()
                                        |> ignore

                                        parseTypeExpression ()
                                        |> Result.map Some
                                    | _ -> Ok None

                                let! _ = expected Equals "expected '='"
                                let! body, bodyRange = parseExpression ()

                                return {
                                    Attributes = attributes
                                    Name = methodName
                                    TypeParameters = methodTypeParameters
                                    Constraints = methodConstraints
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
                            | StaticKeyword ->
                                parseResult {
                                    let! methodDeclaration = parseStaticMethod [] false

                                    return!
                                        parseStaticMethods (
                                            methodDeclaration
                                            :: methods
                                        )
                                }
                            | TypeKeyword
                            | EndOfFile -> Ok(List.rev methods)
                            | _ ->
                                Error(
                                    prototypeDiagnostic
                                        source.Path
                                        (current ()).Range
                                        "expected a static member, type declaration, or end of file"
                                )

                        let parseInstanceMethod attributes =
                            parseResult {
                                let! memberToken = expected MemberKeyword "expected 'member'"
                                let! _ = expected InlineKeyword "expected 'inline'"

                                let! receiverName, _ =
                                    identifier "expected an instance member receiver"

                                let! _ = expected Dot "expected '.'"
                                let! methodName, _ = identifier "expected an instance member name"
                                let! _ = expected LeftParenthesis "expected '('"
                                let! parameters = parseParameters []
                                let! _ = expected RightParenthesis "expected ')'"

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

                                return {
                                    Attributes = attributes
                                    ReceiverName = receiverName
                                    Name = methodName
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

                        let rec parseObjectMethods methods =
                            match (current ()).Kind with
                            | AttributeStart ->
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
                            | TypeKeyword
                            | AndKeyword
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

                        let! isObjectType =
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                parseResult {
                                    let! _ = expected LeftParenthesis "expected '('"
                                    let! _ = expected RightParenthesis "expected ')'"
                                    return true
                                }
                            | _ -> Ok false

                        let! _ = expected Equals "expected '='"

                        return!
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
                                                Name = declarationName
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
                                        Ok(
                                            ParsedStructType {
                                                Name = declarationName
                                                TypeParameters = typeParameters
                                                Attributes = attributes
                                                Fields = fields
                                                Range = {
                                                    Start = typeToken.Range.Start
                                                    End = lastField.Range.End
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
                    | EndOfFile -> Ok(List.rev declarations)
                    | _ ->
                        Error(
                            prototypeDiagnostic
                                source.Path
                                (current ()).Range
                                "expected a type declaration or end of file"
                        )

                let rec parseModuleDeclarations declarations =
                    match (current ()).Kind with
                    | LetKeyword ->
                        parseResult {
                            let! declaration = parseLiteralDeclaration ()

                            return!
                                parseModuleDeclarations (
                                    declaration
                                    :: declarations
                                )
                        }
                    | token when canStartTypeDeclaration declarations token ->
                        parseResult {
                            let! declaration = parseTypeDeclaration []

                            return!
                                parseModuleDeclarations (
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
                                | _ ->
                                    Error(
                                        prototypeDiagnostic
                                            source.Path
                                            (current ()).Range
                                            "expected a type declaration after attributes"
                                    )

                            return!
                                parseModuleDeclarations (
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
                            let! declarations = parseModuleDeclarations []

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
                                AssemblyAttributes = assemblyAttributes
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
                        | EndOfFile -> Ok(namespaceFile namespaceName openNamespaces [] [])
                        | _ ->
                            Error(
                                prototypeDiagnostic
                                    source.Path
                                    (current ()).Range
                                    "expected an assembly attribute, type abbreviation, or end of file"
                            )
                }

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
