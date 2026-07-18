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
        | LetKeyword
        | Identifier of string
        | Integer of int
        | StringLiteralToken of string
        | LeftParenthesis
        | RightParenthesis
        | Colon
        | Equals
        | EndOfFile

        override _.ToString() = "TokenKind"

    type private Token =
        { Kind: TokenKind
          Range: SourceRange }

        override _.ToString() = "Token"

    let private diagnostic code path range message = {
        Code = code
        Message = message
        Path = Some path
        Range = Some range
    }

    let private prototypeDiagnostic path range message =
        diagnostic "FSC2P1001" path range message

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
                | "let" -> add LetKeyword start
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
                      && text.[offset] <> '"'
                      && text.[offset] <> '\n'
                      && text.[offset] <> '\r' do
                    advance ()

                if offset < text.Length
                   && text.[offset] = '"' then
                    let value = text.Substring(first, offset - first)
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
                advance ()

                match current with
                | '(' -> add LeftParenthesis start
                | ')' -> add RightParenthesis start
                | ':' -> add Colon start
                | '=' -> add Equals start
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

    let parse (source: SourceInput) =
        let sourceChecksum =
            source.Text
            |> Encoding.UTF8.GetBytes
            |> SHA256.HashData

        let contentFingerprint =
            sourceChecksum
            |> Convert.ToHexString
            |> fun value -> value.ToLowerInvariant()

        match tokenize source with
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
                    consume ()
                    |> ignore

                    Ok()
                else
                    Error(prototypeDiagnostic source.Path (current ()).Range message)

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
                | Ok() ->
                    match parseExpression () with
                    | Error error -> Error error
                    | Ok(body, bodyRange) ->
                        match expected EndOfFile "expected end of file" with
                        | Error error -> Error error
                        | Ok() ->
                            Ok {
                                Name = moduleName
                                SourceChecksum =
                                    sourceChecksum
                                    |> ImmutableArray.CreateRange<byte>
                                ContentFingerprint = contentFingerprint
                                Declarations = [
                                    {
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

            match expected ModuleKeyword "expected 'module'" with
            | Error error -> Error error
            | Ok() ->
                match (consume ()).Kind with
                | Identifier moduleName ->
                    match expected LetKeyword "expected 'let'" with
                    | Error error -> Error error
                    | Ok() ->
                        let declarationToken = consume ()

                        match declarationToken.Kind with
                        | Identifier declarationName ->
                            match (current ()).Kind with
                            | LeftParenthesis ->
                                consume ()
                                |> ignore

                                match expected RightParenthesis "expected ')'" with
                                | Error error -> Error error
                                | Ok() ->
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
                | _ ->
                    Error(
                        prototypeDiagnostic
                            source.Path
                            (current ()).Range
                            "expected a module name"
                    )
