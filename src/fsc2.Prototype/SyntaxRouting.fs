namespace FSharp2.Compiler

open System
open System.Security.Cryptography
open System.Text

module internal SyntaxRouting =
    let private isToken kind text (token: LexicalToken) =
        token.Kind = kind
        && token.Text = text

    let private isIdentifier (token: LexicalToken) =
        token.Kind = LexicalTokenKind.Identifier

    let private isAtom (token: LexicalToken) =
        match token.Kind with
        | LexicalTokenKind.NumericLiteral
        | LexicalTokenKind.StringLiteral
        | LexicalTokenKind.Identifier -> true
        | LexicalTokenKind.Keyword ->
            token.Text = "true"
            || token.Text = "false"
        | _ -> false

    let private entryPointAttribute = [
        isToken LexicalTokenKind.Delimiter "["
        isToken LexicalTokenKind.Operator "<"
        isToken LexicalTokenKind.Identifier "EntryPoint"
        isToken LexicalTokenKind.Operator ">"
        isToken LexicalTokenKind.Delimiter "]"
    ]

    let private unit = [
        isToken LexicalTokenKind.Delimiter "("
        isToken LexicalTokenKind.Delimiter ")"
    ]

    let rec private startsWith (checks: (LexicalToken -> bool) list) (tokens: LexicalToken list) =
        match checks, tokens with
        | [], _ -> Some tokens
        | check :: checks, token :: tokens when check token -> startsWith checks tokens
        | _ -> None

    let private skip checks tokens =
        startsWith checks tokens
        |> Option.defaultValue tokens

    let private declaration tokens =
        match skip entryPointAttribute tokens with
        | letToken :: name :: rest when
            isToken LexicalTokenKind.Keyword "let" letToken
            && isIdentifier name
            ->
            let rest =
                match startsWith unit rest with
                | Some rest -> rest
                | None ->
                    match rest with
                    | parameter :: rest when isIdentifier parameter -> rest
                    | _ -> rest

            match rest with
            | equals :: rest when isToken LexicalTokenKind.Operator "=" equals ->
                match startsWith unit rest with
                | Some rest -> Some rest
                | None ->
                    match rest with
                    | atom :: rest when isAtom atom -> Some rest
                    | _ -> None
            | _ -> None
        | _ -> None

    let hasProjectableTokenShape (document: LexicalDocument) =
        let tokens =
            document.Tokens
            |> Seq.filter (fun token ->
                token.Kind
                <> LexicalTokenKind.EndOfFile
            )
            |> List.ofSeq

        let rec declarations tokens =
            match tokens with
            | [] -> true
            | _ ->
                match declaration tokens with
                | Some rest -> declarations rest
                | None -> false

        match tokens with
        | moduleToken :: name :: (_ :: _ as rest) when
            isToken LexicalTokenKind.Keyword "module" moduleToken
            && isIdentifier name
            ->
            declarations rest
        | _ -> false

    let isEligible (document: LexicalDocument) =
        document.LogicalPath.EndsWith(".fs", StringComparison.OrdinalIgnoreCase)
        && document.Directives.IsEmpty
        && document.Diagnostics.IsEmpty
        && Frontend.isTokenizedLikeLexicalDocument document
        && hasProjectableTokenShape document

    let tryProject (document: LexicalDocument) =
        if isEligible document then
            let syntax = Parser.parseImplementationFile document

            if syntax.Diagnostics.IsEmpty then
                let contentFingerprint =
                    document.CompatibilityText
                    |> Encoding.UTF8.GetBytes
                    |> SHA256.HashData
                    |> Convert.ToHexString
                    |> _.ToLowerInvariant()

                match
                    SyntaxProjection.project contentFingerprint document.SourceChecksum syntax.File
                with
                | SyntaxProjectionResult.Projected parsed -> Some parsed
                | SyntaxProjectionResult.ProjectionUnsupported _ -> None
            else
                None
        else
            None
