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
        | NullKeyword
        | LetKeyword
        | DoKeyword
        | Identifier of string
        | Integer of int
        | StringLiteralToken of string
        | AttributeStart
        | AttributeEnd
        | LeftParenthesis
        | RightParenthesis
        | Colon
        | Comma
        | Dot
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
            for offset in startOffset .. endOffset - 1 do
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

            let lineEnd =
                if newline < 0 then
                    text.Length
                else
                    newline

            let contentEnd =
                if
                    lineEnd > lineStart
                    && text.[lineEnd - 1] = '\r'
                then
                    lineEnd - 1
                else
                    lineEnd

            let lineText = text.Substring(lineStart, contentEnd - lineStart)
            let trimmed = lineText.TrimStart()
            let leading = lineText.Length - trimmed.Length
            let directiveOffset = lineStart + leading
            let directiveColumn = leading + 1

            let directiveRange =
                range
                    directiveOffset
                    directiveColumn
                    contentEnd
                    (contentEnd - lineStart + 1)

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

                    isActive <- isActive && conditionIsTrue
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
                    frames <- { frame with HasElse = true } :: tail
                    isActive <- frame.ParentIsActive && not frame.ConditionIsTrue
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
                lineStart <- newline + 1
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
                | "null" -> add NullKeyword start
                | "let" -> add LetKeyword start
                | "do" -> add DoKeyword start
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
                else
                    advance ()

                    match current with
                    | '(' -> add LeftParenthesis start
                    | ')' -> add RightParenthesis start
                    | ':' -> add Colon start
                    | ',' -> add Comma start
                    | '.' -> add Dot start
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

        let defines = defines |> Set.ofList

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
            | Ok preprocessedText ->
                tokenize { source with Text = preprocessedText }

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

            let parseExpression () =
                let expressionToken = consume ()

                match expressionToken.Kind with
                | Integer value -> Ok(IntegerLiteral value, expressionToken.Range)
                | StringLiteralToken value -> Ok(StringLiteral value, expressionToken.Range)
                | RightParenthesis ->
                    Error(
                        diagnostic
                            "FS0010"
                            source.Path
                            expressionToken.Range
                            "Unexpected symbol ')' in binding"
                    )
                | _ ->
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
                                Namespace = String.Empty
                                Name = moduleName
                                IsPublic = true
                                OpenedNamespaces = []
                                SourceChecksum =
                                    sourceChecksum
                                    |> ImmutableArray.CreateRange<byte>
                                ContentFingerprint = contentFingerprint
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

                let parseTypeAbbreviation () =
                    parseResult {
                        let! typeToken = expected TypeKeyword "expected 'type'"

                        let! declarationName, _ =
                            identifier "expected a type-abbreviation name"

                        let! _ = expected Equals "expected '='"
                        let targetStart = (current ()).Range.Start

                        let! targetName =
                            qualifiedIdentifier "expected an abbreviated type"

                        let targetName = qualifiedTypeName targetName

                        let! allowsNull, targetEnd =
                            match (current ()).Kind with
                            | Bar ->
                                consume ()
                                |> ignore

                                match expected NullKeyword "expected 'null' after '|'" with
                                | Error error -> Error error
                                | Ok nullToken -> Ok(true, nullToken.Range.End)
                            | _ -> Ok(false, input.[index - 1].Range.End)

                        return
                            ParsedTypeAbbreviation {
                                Name = declarationName
                                Target = {
                                    TypeName = targetName
                                    AllowsNull = allowsNull
                                    Range = {
                                        Start = targetStart
                                        End = targetEnd
                                    }
                                }
                                Range = {
                                    Start = typeToken.Range.Start
                                    End = targetEnd
                                }
                            }
                    }

                let rec parseTypeAbbreviations declarations =
                    match (current ()).Kind with
                    | TypeKeyword ->
                        parseResult {
                            let! declaration = parseTypeAbbreviation ()

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
                                "expected a type abbreviation or end of file"
                        )

                let namespaceFile namespaceName openNamespaces assemblyAttributes declarations = {
                    StableId =
                        "namespace:"
                        + namespaceName
                    Namespace = namespaceName
                    Name = namespaceName
                    IsPublic = false
                    OpenedNamespaces = openNamespaces
                    SourceChecksum =
                        sourceChecksum
                        |> ImmutableArray.CreateRange<byte>
                    ContentFingerprint = contentFingerprint
                    AssemblyAttributes = assemblyAttributes
                    Declarations = declarations
                }

                let finishNamespaceFile namespaceName openNamespaces assemblyAttributes =
                    let sourceChecksum =
                        sourceChecksum
                        |> ImmutableArray.CreateRange<byte>

                    match (current ()).Kind with
                    | EndOfFile ->
                        Ok {
                            StableId =
                                "namespace:"
                                + namespaceName
                            Namespace = namespaceName
                            Name = namespaceName
                            IsPublic = false
                            OpenedNamespaces = openNamespaces
                            SourceChecksum = sourceChecksum
                            ContentFingerprint = contentFingerprint
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
                            let! declarations = parseLiteralDeclarations []

                            return {
                                StableId =
                                    "module:"
                                    + namespaceName
                                    + "."
                                    + moduleName
                                Namespace = namespaceName
                                Name = moduleName
                                IsPublic = isPublic
                                OpenedNamespaces = openNamespaces
                                SourceChecksum = sourceChecksum
                                ContentFingerprint = contentFingerprint
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

                parseResult {
                    let! namespaceName = qualifiedIdentifier "expected a namespace name"
                    let! openNamespaces = parseOpenNamespaces []

                    return!
                        match (current ()).Kind with
                        | AttributeStart ->
                            parseResult {
                                let! firstAttribute = parseAssemblyAttribute openNamespaces

                                let! assemblyAttributes =
                                    parseRemainingAssemblyAttributes
                                        openNamespaces
                                        [ firstAttribute ]

                                let! _ = expected DoKeyword "expected 'do'"
                                let! _ = expected LeftParenthesis "expected '('"
                                let! _ = expected RightParenthesis "expected ')'"

                                return!
                                    finishNamespaceFile
                                        namespaceName
                                        openNamespaces
                                        assemblyAttributes
                            }
                        | TypeKeyword ->
                            parseResult {
                                let! declarations = parseTypeAbbreviations []

                                return
                                    namespaceFile
                                        namespaceName
                                        openNamespaces
                                        []
                                        declarations
                            }
                        | EndOfFile ->
                            Ok(namespaceFile namespaceName openNamespaces [] [])
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
