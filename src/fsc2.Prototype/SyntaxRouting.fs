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

    let rec private qualifiedName tokens =
        match tokens with
        | name :: dot :: (next :: _ as rest) when
            isIdentifier name
            && isToken LexicalTokenKind.Operator "." dot
            && isIdentifier next
            ->
            qualifiedName rest
        | name :: rest when isIdentifier name -> Some rest
        | _ -> None

    let rec private openDeclarations tokens =
        match tokens with
        | openToken :: rest when isToken LexicalTokenKind.Keyword "open" openToken ->
            match qualifiedName rest with
            | Some rest -> openDeclarations rest
            | None -> []
        | rest -> rest

    let private startsModuleOrNamespace token =
        isToken LexicalTokenKind.Keyword "module" token
        || isToken LexicalTokenKind.Keyword "namespace" token

    let hasProjectableTokenShape implicitModule (document: LexicalDocument) =
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

        match tokens, implicitModule with
        | moduleToken :: name :: (_ :: _ as rest), _ when
            isToken LexicalTokenKind.Keyword "module" moduleToken
            && isIdentifier name
            ->
            declarations rest
        | first :: _, ImplicitModule.Accepted _ when not (startsModuleOrNamespace first) ->
            match openDeclarations tokens with
            | _ :: _ as rest -> declarations rest
            | [] -> false
        | _ -> false

    let isEligible implicitModule (document: LexicalDocument) =
        document.LogicalPath.EndsWith(".fs", StringComparison.OrdinalIgnoreCase)
        && document.Directives.IsEmpty
        && document.Diagnostics.IsEmpty
        && Frontend.isTokenizedLikeLexicalDocument document
        && hasProjectableTokenShape implicitModule document

    let isImplicitModuleCandidate implicitModule (document: LexicalDocument) =
        match implicitModule with
        | ImplicitModule.Rejected -> false
        | ImplicitModule.Accepted _ ->
            not document.Tokens.IsEmpty
            && not (startsModuleOrNamespace document.Tokens[0])

    let tryProject implicitModule (document: LexicalDocument) =
        if isEligible implicitModule document then
            let syntax = Parser.parseImplementationFile document

            if syntax.Diagnostics.IsEmpty then
                let contentFingerprint =
                    document.CompatibilityText
                    |> Encoding.UTF8.GetBytes
                    |> SHA256.HashData
                    |> Convert.ToHexString
                    |> _.ToLowerInvariant()

                match
                    SyntaxProjection.project
                        implicitModule
                        contentFingerprint
                        document.SourceChecksum
                        syntax.File
                with
                | SyntaxProjectionResult.Projected parsed -> Some parsed
                | SyntaxProjectionResult.ProjectionUnsupported _ -> None
            else
                None
        else
            None
