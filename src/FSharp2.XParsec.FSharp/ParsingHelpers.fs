namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState

[<AutoOpen>]
module Parsing =
    let tokenIndex (reader: Reader<PositionedToken, ParseState, _>) = reader.Index * 1<token>

    /// Creates a PositionedToken with the IsVirtual flag set, for synthesized tokens
    /// (virtual delimiters, recovery placeholders, etc.).
    let mkVirtualPT (tok: Token) (startIndex: int) =
        PositionedToken.Create(Token.ofUInt16 (uint16 tok ||| TokenRepresentation.IsVirtual), startIndex)

    /// Scans forward past #if branch content, respecting nested #if/#endif pairs.
    /// Stops (after consuming the stop token) at either:
    ///   * matching #endif at depth 0 — always stops here,
    ///   * matching #else at depth 0 — only when `stopOnElse` is true.
    /// Returns true if the stop was a matching #else. Gracefully stops on EOF.
    let private skipConditionalBranch (stopOnElse: bool) (reader: Reader<PositionedToken, ParseState, _>) =
        let mutable depth = 0
        let mutable foundElse = false
        let mutable stop = false

        while not stop do
            match reader.Peek() with
            | ValueNone -> stop <- true // EOF — unclosed #if, stop gracefully
            | ValueSome t ->
                match t.Token with
                | Token.IfDirective ->
                    // Nested #if: increase depth to track nesting
                    depth <- depth + 1
                    reader.Skip()
                | Token.EndIfDirective when depth = 0 ->
                    // Matching #endif found: consume it and stop
                    reader.Skip()
                    stop <- true
                | Token.EndIfDirective ->
                    // #endif for a nested #if: decrease depth
                    depth <- depth - 1
                    reader.Skip()
                | Token.ElseDirective when stopOnElse && depth = 0 ->
                    // Matching #else found: consume it and stop; else-branch is now active
                    reader.Skip()
                    foundElse <- true
                    stop <- true
                | _ -> reader.Skip()

        foundElse

    /// Scans forward past tokens in an inactive branch until reaching #else or #endif at depth 0.
    /// Nested #if/#endif pairs are depth-tracked and correctly skipped.
    /// The stop token (#else or #endif) is consumed before returning.
    /// Returns true if stopped at #else (an else-branch follows), false if stopped at #endif.
    /// Directives inside block comments (with the InComment flag) are correctly ignored.
    let skipInactiveBranch reader = skipConditionalBranch true reader

    /// Scans forward past an active else-branch until the matching #endif at depth 0 is consumed.
    /// Must be called after the #else token itself has already been consumed.
    let skipElseBranch reader =
        skipConditionalBranch false reader |> ignore

    /// Processes a #if directive: parses the condition expression, evaluates it against
    /// the current defined symbols, and either continues into the active branch or skips
    /// to the matching #else/#endif.
    /// Must be called with the reader positioned at the #if token (not yet consumed).
    let processIfDirective
        (nextSyntaxToken: Parser<_, _, _, _>)
        (ifToken: SyntaxToken)
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        let state = reader.State
        let lexed = state.Lexed
        let currentLine = findLineNumber state (tokenIndex reader)
        let nextLine = currentLine + 1<_>

        let nextLineTokenIndex =
            if nextLine < lexed.LineStarts.LengthM then
                lexed.LineStarts[nextLine]
            else
                lexed.Tokens.LengthM - 1<_> // EOF, will be handled gracefully by nextSyntaxToken

        // Create a slice of just the #if directive line, carrying the absolute start index
        // so the IfExpr parser can compute absolute token indices for symbol name extraction.
        let sliceLen = (nextLineTokenIndex * 1< / token>) - reader.Index
        let sliceReader = reader.Slice(0, sliceLen, { AbsoluteStart = reader.Index })

        // Advance the main reader past the #if line before branching
        reader.Index <- nextLineTokenIndex * 1< / token>

        match IfExpr.parseSlice sliceReader with
        | Ok ifExpr ->
            if IfExpr.evaluateStateful ifExpr reader.State then
                // Condition is true: the then-branch is active.
                // #else and #endif encountered later will be handled by nextSyntaxToken.
                nextSyntaxToken reader
            else if
                // Condition is false: skip over the inactive then-branch.
                skipInactiveBranch reader
            then
                // Stopped at #else: the else-branch is now active
                nextSyntaxToken reader
            else
                // Stopped at #endif: entire block skipped
                nextSyntaxToken reader
        | Error e ->
            // Invalid #if expression: record a diagnostic and treat the whole block as inactive
            let formatToken (token: PositionedToken) (sb: Text.StringBuilder) =
                sb.Append(string token.TokenWithoutCommentFlags)

            let formatTokens (tokens: PositionedToken seq) (sb: Text.StringBuilder) =
                sb.Append(String.Join(" ", tokens |> Seq.map (fun token -> string token.TokenWithoutCommentFlags)))

            let formatted =
                Text.StringBuilder()
                |> ErrorFormatting.formatParseError formatToken formatTokens e
                |> string

            let detail =
                formatted.Split('\n')
                |> Array.map (fun line -> line.TrimStart(' ', '└', '─', '│', '├').TrimEnd())
                |> Array.filter (fun line -> line <> "")
                |> String.concat ": "

            let msg = "Invalid #if expression: " + detail

            reader.State <- addDiagnosticAt (DiagnosticCode.Other msg) ifToken reader.State

            skipInactiveBranch reader |> ignore
            nextSyntaxToken reader

    let private computeInactiveTokens (state: ParseState) =
        let lexed = state.Lexed
        let tokens = lexed.Tokens
        let inactive = Array.zeroCreate<bool> tokens.Length
        let branches = Stack<struct (bool * bool)>()
        let mutable active = true
        let mutable i = 0<token>

        let evaluate (ifIndex: int<token>) =
            let nextLine = findLineNumber state ifIndex + 1<_>

            let nextLineTokenIndex =
                if nextLine < lexed.LineStarts.LengthM then
                    lexed.LineStarts[nextLine]
                else
                    tokens.LengthM - 1<_>

            let reader = Reader(tokens.AsReadableArray(), state, int ifIndex)
            let slice = reader.Slice(0, int (nextLineTokenIndex - ifIndex), { AbsoluteStart = reader.Index })

            match IfExpr.parseSlice slice with
            | Ok ifExpr -> IfExpr.evaluateStateful ifExpr state
            | Error _ -> false

        let skipDirectiveLine () =
            while i < tokens.LengthM
                  && tokens[i].TokenWithoutCommentFlags <> Token.Newline
                  && tokens[i].TokenWithoutCommentFlags <> Token.EOF do
                inactive[int i] <- true
                i <- i + 1<token>

        while i < tokens.LengthM do
            match tokens[i].Token with
            | Token.IfDirective ->
                let condition = active && evaluate i
                branches.Push(struct (active, condition))
                active <- condition
                skipDirectiveLine ()
            | Token.ElseDirective when branches.Count > 0 ->
                let struct (parentActive, condition) = branches.Peek()
                active <- parentActive && not condition
                skipDirectiveLine ()
            | Token.EndIfDirective when branches.Count > 0 ->
                let struct (parentActive, _) = branches.Pop()
                active <- parentActive
                skipDirectiveLine ()
            | Token.EOF -> i <- i + 1<token>
            | _ ->
                inactive[int i] <- not active
                i <- i + 1<token>

        inactive

    /// Whether the parser skips the token at `index` at a conditional directive: the directive lines and the inactive branches.
    let isInactiveToken (state: ParseState) (index: int<token>) =
        let cache = state.InactiveTokens

        if isNull cache.Value then
            cache.Value <- computeInactiveTokens state

        cache.Value[int index]

    /// A token that a scan of the token array must skip: trivia, or a token that the parser skips at a conditional directive.
    let isSkippedByScan (state: ParseState) (index: int<token>) =
        ParseState.isTriviaToken state state.Lexed.Tokens[index]
        || isInactiveToken state index

    let processWarnDirective
        (nextSyntaxToken: Parser<_, _, _, _>)
        (isSuppress: bool)
        (_directiveToken: PositionedToken)
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        let state = reader.State
        let lexed = state.Lexed
        let currentLine = findLineNumber state (tokenIndex reader)
        let nextLine = currentLine + 1<_>

        let nextLineTokenIndex =
            if nextLine < lexed.LineStarts.LengthM then
                lexed.LineStarts[nextLine]
            else
                lexed.Tokens.LengthM - 1<_>

        // Skip the directive token itself
        reader.Skip()

        let mutable warnDirectives = state.WarnDirectives

        // Scan remaining tokens on this line for warning codes
        while reader.Index < int nextLineTokenIndex do
            match reader.Peek() with
            | ValueNone -> reader.Index <- int nextLineTokenIndex
            | ValueSome token when isTriviaToken state token -> reader.Skip()
            | ValueSome token ->
                let tIdx = tokenIndex reader
                let text = lexed.GetTokenString(tIdx)
                reader.Skip()

                match token.Token with
                | Token.NumInt32 ->
                    match System.Int32.TryParse(text) with
                    | true, n ->
                        warnDirectives <-
                            {
                                Line = currentLine
                                WarningNumber = n
                                Suppress = isSuppress
                            }
                            :: warnDirectives
                    | _ -> ()
                | Token.StringOpen ->
                    // String is now fragmented: StringOpen, StringFragment*, StringClose
                    // Look for a single StringFragment containing the warning number
                    if reader.Index < int nextLineTokenIndex then
                        match reader.Peek() with
                        | ValueSome fragToken when fragToken.Token = Token.StringFragment ->
                            let fragIdx = tokenIndex reader
                            let fragText = lexed.GetTokenString(fragIdx)
                            reader.Skip()

                            match System.Int32.TryParse(fragText) with
                            | true, n ->
                                warnDirectives <-
                                    {
                                        Line = currentLine
                                        WarningNumber = n
                                        Suppress = isSuppress
                                    }
                                    :: warnDirectives
                            | _ -> ()
                        | _ -> ()
                | _ -> ()

        // Ensure we're past the directive line
        reader.Index <- int nextLineTokenIndex

        reader.State <-
            { reader.State with
                WarnDirectives = warnDirectives
            }

        nextSyntaxToken reader

    /// Returns the text length of the token at the given index in the lexed token array.
    let private getTokenLength (state: ParseState) (index: int<token>) =
        let tokens = state.Lexed.Tokens

        if index + 1<token> < tokens.LengthM then
            let t0 = tokens[index]
            let t1 = tokens[index + 1<token>]
            t1.StartIndex - t0.StartIndex
        else
            0

    /// Returns true if the token can appear as an infix operator in expressions.
    /// Used for the SeqBlock infix undentation exception (F# spec 15.1.9).
    let private isInfixToken (token: Token) =
        match token with
        | Token.OpAddition
        | Token.OpSubtraction
        | Token.OpMultiply
        | Token.OpDivision
        | Token.OpModulus
        | Token.OpExponentiation
        | Token.OpPipeRight
        | Token.OpPipeRight2
        | Token.OpPipeRight3
        | Token.OpPipeLeft
        | Token.OpPipeLeft2
        | Token.OpPipeLeft3
        | Token.OpComposeLeft
        | Token.OpComposeRight
        | Token.OpBooleanAnd
        | Token.OpBooleanOr
        | Token.OpBitwiseAnd
        | Token.OpBitwiseOr
        | Token.OpExclusiveOr
        | Token.OpLeftShift
        | Token.OpRightShift
        | Token.OpLessThan
        | Token.OpGreaterThan
        | Token.OpLessThanOrEqual
        | Token.OpGreaterThanOrEqual
        | Token.OpEquality
        | Token.OpInequality
        | Token.OpAppend
        | Token.OpCons
        | Token.OpArrowRight
        | Token.OpColonEquals
        | Token.OpBar
        | Token.OpBarBar
        | Token.OpAmp
        | Token.OpAmpAmp
        | Token.OpConcatenate
        | Token.OpComma
        | Token.OpSemicolon
        | Token.OpDot -> true
        | _ ->
            // Custom operators are TokenKind.Operator but not in the keyword list above
            TokenInfo.isOperator token && not (TokenInfo.canBePrefix token)

    /// Checks whether a context in the stack permits the given token at the given column.
    /// `ctx` is the context to check, `tokenCol` is the column of the token.
    let private contextPermitsToken (token: Token) (tokenCol: int) (ctx: Offside) =
        ctx.Indent <= tokenCol
        && (
            match ctx.Context, token with
            // 15.1.9: then/elif/else may align with if
            | OffsideContext.If, (Token.KWThen | Token.KWElif | Token.KWElse) -> true
            // 15.1.9: with/finally/| may align with try
            | OffsideContext.Try, (Token.KWWith | Token.KWFinally | Token.OpBar) -> true
            // 15.1.9: done may align with for
            | OffsideContext.For, Token.KWDone -> true
            // 15.1.9: done may align with do
            | OffsideContext.Do, Token.KWDone -> true
            // 15.1.9: and may align with let
            | OffsideContext.Let, Token.KWAnd -> true
            // 15.1.9: }, end, and, | may align with type
            | OffsideContext.Type, (Token.KWRBrace | Token.KWEnd | Token.KWAnd | Token.OpBar) -> true
            // 15.1.9: end may align with interface (WithAugment)
            | OffsideContext.WithAugment, Token.KWEnd -> true
            // 15.1.9: with/| may align with match
            | OffsideContext.Match, (Token.KWWith | Token.OpBar) -> true
            // 15.1.9: | may align with function
            | OffsideContext.Function, Token.OpBar -> true
            // 15.1.9: done may align with while
            | OffsideContext.While, Token.KWDone -> true
            | _ -> false
        )

    /// Returns true if the token is a closing delimiter that matches the given paren-like context.
    /// Closing delimiters are never offside from their matching opening context (F# spec 15.1.8).
    let private isMatchingClose (token: Token) (ctx: OffsideContext) =
        match ctx, token with
        | OffsideContext.Paren, Token.KWRParen -> true
        | OffsideContext.Bracket, Token.KWRBracket -> true
        | OffsideContext.BracketBar, Token.KWRArrayBracket -> true
        | OffsideContext.BraceBar, Token.KWRBraceBar -> true
        | OffsideContext.Brace, Token.KWRBrace -> true
        | OffsideContext.Begin, Token.KWEnd -> true
        | OffsideContext.Quote, (Token.OpQuotationTypedRight | Token.OpQuotationUntypedRight) -> true
        | _ -> false

    /// Like List.exists contextPermitsToken, but for OpBar tokens, stops at
    /// MatchClauses boundaries to prevent inner matches from consuming outer bars.
    let rec private contextPermitsTokenBounded (token: Token) (tokenCol: int) (stack: Offside list) =
        match stack with
        | [] -> false
        | ctx :: rest ->
            if contextPermitsToken token tokenCol ctx then
                true
            elif token = Token.OpBar && ctx.Context = OffsideContext.MatchClauses then
                false
            else
                contextPermitsTokenBounded token tokenCol rest

    /// Paren-like contexts per F# spec §15.1.10.4 — delimiters and begin/end.
    /// These sit on the context stack as markers (Indent = 0), not as offside lines.
    let private isParenLike (ctx: OffsideContext) =
        match ctx with
        | OffsideContext.Paren
        | OffsideContext.Bracket
        | OffsideContext.BracketBar
        | OffsideContext.BraceBar
        | OffsideContext.Brace
        | OffsideContext.Begin -> true
        | _ -> false

    // Walks the stack looking for any matching closing-delimiter context.
    // Hoisted out of isPermittedUndentation so no per-call closure is allocated.
    let rec private anyMatchingClose (token: Token) (stack: Offside list) =
        match stack with
        | [] -> false
        | ctx :: rest ->
            if isMatchingClose token ctx.Context then
                true
            else
                anyMatchingClose token rest

    // Fun/Function-body undentation: walk past SeqBlock+Paren-like+Fun/Function frames
    // to find the true enclosing offside line (F# spec §15.1.10.1).
    let rec private findFunBodyEnclosingIndent (tokenCol: int) (stack: Offside list) =
        match stack with
        | [] -> true // No other context to violate
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.BraceBar
            | OffsideContext.Brace
            | OffsideContext.Begin
            | OffsideContext.Fun
            | OffsideContext.Function
            | OffsideContext.MatchClauses -> findFunBodyEnclosingIndent tokenCol deeper
            | _ -> tokenCol >= ctx.Indent

    // MatchClauses-body undentation: same skip-past-containers rule, but also skips
    // Match and MatchClauses frames (F# spec §15.1.10.1 extended).
    let rec private findMatchBodyEnclosingIndent (tokenCol: int) (stack: Offside list) =
        match stack with
        | [] -> true
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.BraceBar
            | OffsideContext.Brace
            | OffsideContext.Begin
            | OffsideContext.Fun
            | OffsideContext.Function
            | OffsideContext.Match
            | OffsideContext.MatchClauses -> findMatchBodyEnclosingIndent tokenCol deeper
            | _ -> tokenCol >= ctx.Indent

    /// Scans back from the token at `index` to the token that started `clauses`. Returns whether a
    /// delimiter that opens after that token is still open, whether the previous syntax token is `->`,
    /// and the column of the previous rule body when the `|` before this rule does not end its block.
    let private scanMatchClause (state: ParseState) (clauses: Offside) (index: int<token>) =
        let tokens = state.Lexed.Tokens
        let mutable i = index - 1<token>
        let mutable depth = 0
        let mutable insideDelimiter = false
        let mutable stop = false
        let mutable previous = ValueNone
        let mutable bar = ValueNone
        let mutable bodyStart = ValueNone
        let mutable previousBodyColumn = ValueNone

        while not stop
              && i >= 0<token>
              && tokens[i].StartIndex >= clauses.Token.StartIndex do
            let token = tokens[i]

            if not (isSkippedByScan state i) then
                let kind = token.TokenWithoutCommentFlags

                if previous.IsNone then
                    previous <- ValueSome kind

                match kind with
                | Token.KWRParen
                | Token.KWRBracket
                | Token.KWRArrayBracket
                | Token.KWRBrace
                | Token.KWRBraceBar
                | Token.KWRHashParen
                | Token.KWEnd
                | Token.OpQuotationTypedRight
                | Token.OpQuotationUntypedRight -> depth <- depth + 1
                | Token.KWLParen
                | Token.KWLBracket
                | Token.KWLArrayBracket
                | Token.KWLBrace
                | Token.KWLBraceBar
                | Token.KWLHashParen
                | Token.KWBegin
                | Token.OpQuotationTypedLeft
                | Token.OpQuotationUntypedLeft ->
                    if depth > 0 then
                        depth <- depth - 1
                    else
                        insideDelimiter <- bar.IsNone
                        stop <- true
                | Token.OpBar when depth = 0 && bar.IsNone -> bar <- ValueSome i
                | Token.OpArrowRight when depth = 0 && bar.IsSome ->
                    match bar, bodyStart with
                    | ValueSome barIndex, ValueSome startIndex ->
                        let bodyColumn = ParseState.getIndent state startIndex

                        if ParseState.getIndent state barIndex >= bodyColumn then
                            previousBodyColumn <- ValueSome bodyColumn
                    | _ -> ()

                    stop <- true
                | _ -> ()

                if bar.IsSome then
                    bodyStart <- ValueSome i

            i <- i - 1<token>

        struct (insideDelimiter, previous = ValueSome Token.OpArrowRight, previousBodyColumn)

    let private columnOfToken (state: ParseState) (token: PositionedToken) (before: int<token>) =
        let tokens = state.Lexed.Tokens
        let mutable i = before - 1<token>

        while i > 0<token>
              && tokens[i].StartIndex > token.StartIndex do
            i <- i - 1<token>

        ParseState.getIndent state i

    /// The leftmost column of the first token of a rule body: the `match` column or an enclosing
    /// `(` or `begin` before it, or the `try` column (FCS LexFilter `undentationLimit` with RelaxWhitespace2).
    let private matchRuleBodyLimit (state: ParseState) (index: int<token>) (enclosing: Offside list) =
        match enclosing with
        | ({ Context = OffsideContext.Match } as matchCtx) :: { Context = OffsideContext.SeqBlock } :: ({
                                                                                                         Context = OffsideContext.Paren | OffsideContext.Begin
                                                                                                     } as paren) :: _ ->
            ValueSome(min matchCtx.Indent (columnOfToken state paren.Token index))
        | ({ Context = OffsideContext.Match } as matchCtx) :: _ -> ValueSome matchCtx.Indent
        | ({ Context = OffsideContext.Try } as tryCtx) :: _ -> ValueSome tryCtx.Indent
        | _ -> ValueNone

    // Used by the Match/Function/Try aligned-token rule (15.1.10 extended).
    // Walks past Match/MatchClauses/Function/Try/SeqBlock frames looking for an
    // enclosing paren-like frame.
    let rec private hasEnclosingParenAroundMatch (stack: Offside list) =
        match stack with
        | [] -> false
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Match
            | OffsideContext.MatchClauses
            | OffsideContext.Function
            | OffsideContext.Try -> hasEnclosingParenAroundMatch deeper
            | c when isParenLike c -> true
            | _ -> false

    /// Walk the context stack skipping SeqBlock+Paren pairs to find the enclosing
    /// expression's offside line for collection/CE undentation (F# spec 15.1.10.4). As in FCS, match
    /// clauses give no limit, and the `match` or `try` below them does.
    let rec private checkCollectionUndent (tokenCol: int) (stack: Offside list) : bool =
        match stack with
        | [] -> true // Walked past all paren-like/SeqBlock contexts; no enclosing offside line to violate
        | ctx :: deeper ->
            match (ctx: Offside).Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Fun
            | OffsideContext.Function
            | OffsideContext.MatchClauses -> checkCollectionUndent tokenCol deeper
            | c when isParenLike c -> checkCollectionUndent tokenCol deeper
            | _ ->
                // Found the enclosing non-paren context; check if token is within its indent
                tokenCol >= ctx.Indent

    /// F# spec §15.1.10.4 (collection/CE undentation) and its SeqBlockParen extension:
    /// a token may undent from the current context when it still lies within the enclosing
    /// expression's offside line. Returns the trace-rule name on success.
    /// Expects `stack` to be the full context list; inspects the innermost frame:
    ///   * `SeqBlock :: paren-like :: deeper` — SeqBlock pushed on top of a paren-like
    ///     context by `withContext` after `pEnclosed`. Rule: "15.1.10.4 SeqBlockParen".
    ///   * `paren-like :: rest` — the innermost frame is itself the paren-like container. Rule:
    ///     "15.1.10.4 Collection".
    let private tryCollectionUndent (tokenCol: int) (stack: Offside list) : string voption =
        match stack with
        | { Context = OffsideContext.SeqBlock } :: { Context = ctx } :: deeper when isParenLike ctx ->
            if checkCollectionUndent tokenCol deeper then
                ValueSome "15.1.10.4 SeqBlockParen"
            else
                ValueNone
        | { Context = ctx } :: rest when isParenLike ctx ->
            if checkCollectionUndent tokenCol rest then
                ValueSome "15.1.10.4 Collection"
            else
                ValueNone
        | _ -> ValueNone

    /// Determines whether a token at column `tokenCol` is permitted despite being
    /// strictly left of the innermost context's offside line.
    /// Implements F# spec sections 15.1.8 (Balancing), 15.1.9 (Exceptions to Offside Rules)
    /// and 15.1.10 (Permitted Undentations).
    let private isPermittedUndentation
        (token: Token)
        (tokenCol: int)
        (context: Offside list)
        (state: ParseState)
        (readerIndex: int64)
        =
        match context with
        | [] -> ValueNone
        | current :: enclosing ->

            // Closing delimiters are never offside from their matching paren-like context (15.1.8)
            if isMatchingClose token current.Context then
                ValueSome "15.1.8 MatchingClose"
            elif anyMatchingClose token enclosing then
                ValueSome "15.1.8 MatchingClose"

            // --- 15.1.9: Exceptions to Offside Rules ---

            // SeqBlock infix: an infix token may be offside by (tokenSize + 1)
            elif current.Context = OffsideContext.SeqBlock && isInfixToken token then
                let tokenLength = getTokenLength state (int readerIndex * 1<token>)

                if tokenCol >= current.Indent - (tokenLength + 1) then
                    ValueSome "15.1.9 InfixUndent"
                else if
                    // Still check deeper contexts, but don't let bars pass through MatchClauses
                    contextPermitsTokenBounded token tokenCol enclosing
                then
                    ValueSome "15.1.9 ContextPermits"
                else
                    // 15.1.10.4 fallback: infix undentation exceeded, but if the SeqBlock
                    // sits directly inside a paren-like context, delegate to the collection
                    // undentation rule.
                    tryCollectionUndent tokenCol context

            // Check if the token is permitted at the current context or any enclosing one
            elif contextPermitsToken token tokenCol current then
                ValueSome "15.1.9 ContextPermits"

            elif contextPermitsTokenBounded token tokenCol enclosing then
                ValueSome "15.1.9 ContextPermits"

            // --- 15.1.10: Permitted Undentations ---

            // 15.1.10.1: Fun/Function body undentation
            // The body may undent from fun/function but not past other offside lines.
            // "Constructs enclosed in brackets may be undented" — so we skip past
            // SeqBlock+Paren pairs to find the true enclosing offside line.
            elif
                current.Context = OffsideContext.Fun
                || current.Context = OffsideContext.Function
            then
                if findFunBodyEnclosingIndent tokenCol enclosing then
                    ValueSome "15.1.10.1 FunBody"
                else
                    ValueNone

            // MatchClauses: FCS permits no clause token left of the clause column, except the first
            // token of a rule body. A `|` on the line of the previous rule body leaves that body block
            // open in FCS, and the block column is then the limit. A delimiter opened in the clause,
            // the end of input, and a token that ends the `match` or `try` keep the enclosing limit. Without a leading `|`, FCS permits one
            // column left of the clause column (LexFilter `CtxtMatchClauses (leadingBar, offsidePos)`).
            elif current.Context = OffsideContext.MatchClauses && token <> Token.OpBar then
                let index = int readerIndex * 1<token>
                let struct (insideDelimiter, startsBody, previousBodyColumn) =
                    scanMatchClause state current index

                if
                    current.Token.Token <> Token.OpBar
                    && not startsBody
                    && tokenCol + 1 >= current.Indent
                then
                    ValueSome "15.1.9 MatchClausesWithoutBar"
                elif
                    insideDelimiter
                    || token = Token.EOF
                    || (not startsBody
                        && (match enclosing with
                            | owner :: _ -> tokenCol <= owner.Indent
                            | [] -> true))
                then
                    if findMatchBodyEnclosingIndent tokenCol enclosing then
                        ValueSome "15.1.10.1 MatchBody"
                    else
                        ValueNone
                elif startsBody then
                    match matchRuleBodyLimit state index enclosing, previousBodyColumn with
                    | ValueSome limit, ValueSome bodyColumn when tokenCol >= limit && tokenCol > bodyColumn ->
                        ValueSome "15.1.10.1 MatchRuleBody"
                    | ValueSome limit, ValueNone when tokenCol >= limit -> ValueSome "15.1.10.1 MatchRuleBody"
                    | _ -> ValueNone
                else
                    match previousBodyColumn with
                    | ValueSome bodyColumn when tokenCol >= bodyColumn -> ValueSome "15.1.10.1 MatchRuleAfterBody"
                    | _ -> ValueNone

            // 15.1.10.2 (if/then/else + paren/begin undentation) and 15.1.10.3 (module/class
            // body undentation inside begin/end) are intentionally omitted. Both spec rules
            // exist because F#'s Lexical Filtering step retrofits offside onto a token stream
            // after lexing, requiring special cases for paren-like frames. XParsec.FSharp is
            // offside-aware by construction: Paren and Begin are pushed (by pEnclosed and
            // withContextAt) with Indent=0 as pure stack markers, so they can never be the
            // current context in an offside check (tokenCol < 0 is impossible). Content inside
            // `(...)` or `begin...end` is bounded by the SeqBlock inside pInner, which is
            // handled by the SeqBlockParen arm of tryCollectionUndent below.

            // 15.1.10.4: Collection/CE undentation for Bracket, BracketBar, Brace contexts
            elif
                current.Context = OffsideContext.Bracket
                || current.Context = OffsideContext.BracketBar
                || current.Context = OffsideContext.BraceBar
                || current.Context = OffsideContext.Brace
            then
                tryCollectionUndent tokenCol context

            // 15.1.10.4 extended: SeqBlock inside paren-like context
            // withContext pushes a SeqBlock on top of the Paren context from pEnclosed.
            // When content undents past that SeqBlock but is still within the enclosing
            // expression's offside line, delegate to the collection undentation rule.
            elif current.Context = OffsideContext.SeqBlock then
                tryCollectionUndent tokenCol context

            // 15.1.10 extended: Match/Function/Try aligned tokens (with/|/finally)
            // may undent when the expression is enclosed in brackets, to the
            // enclosing expression's offside line. Mirrors SeqBlockParen above.
            elif
                (current.Context = OffsideContext.Match
                 || current.Context = OffsideContext.Function
                 || current.Context = OffsideContext.Try)
                && (token = Token.OpBar
                    || (token = Token.KWWith
                        && (current.Context = OffsideContext.Match || current.Context = OffsideContext.Try))
                    || (token = Token.KWFinally && current.Context = OffsideContext.Try))
            then
                if
                    hasEnclosingParenAroundMatch enclosing
                    && checkCollectionUndent tokenCol enclosing
                then
                    ValueSome "15.1.10 MatchParen"
                else
                    ValueNone

            else
                ValueNone

    let private errOffside: ErrorType<PositionedToken, ParseState> = Message "Offside"

    /// True when a syntax token comes before `token` on its line.
    let followsTokenOnSameLine (state: ParseState) (token: SyntaxToken) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular index ->
            let tokens = state.Lexed.Tokens
            let mutable i = index - 1<token>
            let mutable result = ValueNone

            while result.IsNone do
                if i < 0<token> then
                    result <- ValueSome false
                else
                    let previous = tokens[i]

                    if previous.TokenWithoutCommentFlags = Token.Newline then
                        result <- ValueSome false
                    elif isTriviaToken state previous then
                        i <- i - 1<token>
                    else
                        result <- ValueSome true

            result.Value

    let rec private nextSyntaxTokenImpl isPeek (reader: Reader<PositionedToken, ParseState, _>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token ->
            let state = reader.State

            match token.Token with
            | Token.IfDirective ->
                // processIfDirective expects the reader to be positioned AT the #if token,
                // and moves it off the directive line before it can raise a diagnostic — so
                // the token's index is captured here, while the reader still points at it.
                processIfDirective (nextSyntaxTokenImpl isPeek) (syntaxToken token reader.Index) reader
            | Token.ElseDirective ->
                // We are in an active then-branch that has reached its #else.
                // Skip the else-branch contents up to and including the matching #endif.
                reader.Skip() // consume #else
                skipElseBranch reader
                nextSyntaxTokenImpl isPeek reader
            | Token.EndIfDirective ->
                // End of a conditional block whose then-branch was active (no #else encountered).
                reader.Skip() // consume #endif
                nextSyntaxTokenImpl isPeek reader
            | Token.NoWarnDirective -> processWarnDirective (nextSyntaxTokenImpl isPeek) true token reader
            | Token.WarnOnDirective -> processWarnDirective (nextSyntaxTokenImpl isPeek) false token reader
            | _ when isTriviaToken state token ->
                reader.Skip()
                nextSyntaxTokenImpl isPeek reader
            | _ ->
                // SplitRAttrBracket: when the measure parser consumed the `>` half of `>]`,
                // it sets this flag so the remaining `]` half is presented as KWRBracket.
                // SplitPowerMinus: when the measure parser consumed the `^` half of a fused
                // `^-N` operator, it sets this flag so the remaining `-` half is presented
                // as OpSubtraction at StartIndex+1 (the numeric `N` follows as its own token).
                let token =
                    if state.SplitRAttrBracket && token.Token = Token.KWRAttrBracket then
                        ParseState.ifTrace state (fun t -> t.SplitRAttrBracketConsumed(token.StartIndex))
                        PositionedToken.Create(Token.KWRBracket, token.StartIndex + 1)
                    elif state.SplitPowerMinus then
                        let span = state.Lexed.GetTokenSpan(reader.Index * 1<token>)

                        if span.Length >= 2 && span.[0] = '^' && span.[1] = '-' then
                            ParseState.ifTrace state (fun t -> t.SplitPowerMinusConsumed(token.StartIndex))
                            PositionedToken.Create(Token.OpSubtraction, token.StartIndex + 1)
                        else
                            token
                    elif state.CharsConsumedAfterTypeParams > 0 then
                        // After type-parameter close consumed `n` leading chars of a fused
                        // operator token (e.g. `>` from `>:`, `>` from `>.`), present what remains
                        // as the appropriate non-operator token so syntactic parsers (`:`, `.`,
                        // `;`, `)`) work after a generic instantiation.
                        // `reprocessedOperatorAfterTypeParams` handles the operator case
                        // (`>>`, `>=`, etc.) before this code path is reached.
                        let span = state.Lexed.GetTokenSpan(reader.Index * 1<token>)
                        let charsConsumed = state.CharsConsumedAfterTypeParams

                        if span.Length > charsConsumed then
                            let nextChar = span.[charsConsumed]
                            // `.` is excluded because `reprocessedOperatorAfterTypeParams` handles
                            // `Foo<T>.Bar` (member access) by reclassifying the operator span.
                            let nextTok =
                                match nextChar with
                                | ':' -> ValueSome Token.OpColon
                                | ';' -> ValueSome Token.OpSemicolon
                                | ')' -> ValueSome Token.KWRParen
                                | ']' -> ValueSome Token.KWRBracket
                                | '}' -> ValueSome Token.KWRBrace
                                | ',' -> ValueSome Token.OpComma
                                | _ -> ValueNone

                            match nextTok with
                            | ValueSome t -> PositionedToken.Create(t, token.StartIndex + charsConsumed)
                            | ValueNone -> token
                        else
                            token
                    else
                        token

                // Compute the column once: the offside check and the trace callbacks
                // below both need it. `getIndent` is a top-3 self-CPU frame, so
                // eliminating the second call per token is worth the unconditional
                // compute in Verbose or empty-context cases (both rare).
                let tokenCol = ParseState.getIndent state (reader.Index * 1<token>)

                // Offside check (Light syntax only): fail if the token is strictly left of the
                // innermost context's offside line, unless a permitted undentation applies.
                let isOffside =
                    state.IndentationMode = Syntax.Light
                    && (
                        match state.Context with
                        | {
                              Indent = contextIndent
                              Context = ctx
                          } :: _ as context ->
                            if tokenCol < contextIndent then
                                match isPermittedUndentation token.Token tokenCol context state reader.Index with
                                | ValueSome rule ->
                                    ParseState.ifTrace
                                        state
                                        (fun t -> t.PermittedUndentation(token, tokenCol, contextIndent, rule))

                                    false
                                | ValueNone ->
                                    ParseState.ifTrace
                                        state
                                        (fun t -> t.OffsideFail(token, tokenCol, contextIndent, ctx))

                                    true
                            else
                                ParseState.ifTrace state (fun t -> t.OffsideOk(token, tokenCol, contextIndent, ctx))
                                false
                        | [] -> false
                    )

                if isOffside then
                    fail errOffside reader
                else
                    let t = syntaxToken token reader.Index

                    if isPeek then
                        ParseState.ifTrace state (fun tc -> tc.TokenPeeked(token, int reader.Index, tokenCol))
                        preturn t reader
                    else
                        ParseState.ifTrace state (fun tc -> tc.TokenConsumed(token, int reader.Index, tokenCol))

                        // Clear split flags + CharsConsumedAfterTypeParams in a single record copy
                        // when any are set. Original code issued two separate updates which
                        // allocated twice when both flags were true; one copy produces the same
                        // final state. `CharsConsumedAfterTypeParams` is reset here because the
                        // fused `>:`/`>.`/etc. token's remaining chars have now been consumed as a
                        // synthesised colon/dot/etc.
                        if
                            state.SplitRAttrBracket
                            || state.SplitPowerMinus
                            || state.CharsConsumedAfterTypeParams > 0
                        then
                            reader.State <-
                                { state with
                                    SplitRAttrBracket = false
                                    SplitPowerMinus = false
                                    CharsConsumedAfterTypeParams = 0
                                }

                        reader.Skip()
                        preturn t reader

    let nextSyntaxToken (reader: Reader<PositionedToken, ParseState, _>) = nextSyntaxTokenImpl false reader

    /// Advanced the reader to the next non-trivia token and returns it without consuming it.
    /// Allows parser to avoid re-skipping trivia tokens when it needs to look ahead at the next token to decide what to parse.
    let peekNextSyntaxToken (reader: Reader<PositionedToken, ParseState, _>) = nextSyntaxTokenImpl true reader

    /// The FCS limit at a module element: the column of the element, plus one for `let` and `and` (LexFilter
    /// CtxtLetDecl), and for `do` (CtxtDo) in a module body. The element is the last line start at `elementColumn`.
    let private moduleElementLimit (state: ParseState) (inModuleBody: bool) (elementColumn: int) (index: int<token>) =
        let tokens = state.Lexed.Tokens
        let mutable i = index
        let mutable found = ValueNone

        while found.IsNone && i >= 0<token> do
            let token = tokens[i]

            if
                not (isSkippedByScan state i)
                && ParseState.getIndent state i = elementColumn
                && not (followsTokenOnSameLine state (syntaxToken token (int i)))
            then
                found <- ValueSome token.TokenWithoutCommentFlags

            i <- i - 1<token>

        match found with
        | ValueSome(Token.KWLet | Token.KWAnd) -> ValueSome(elementColumn + 1)
        | ValueSome Token.KWDo when inModuleBody -> ValueSome(elementColumn + 1)
        | ValueSome _ -> ValueSome elementColumn
        | ValueNone -> ValueNone

    /// Whether the file starts with a named module header: `module` with no `=` before the end of its line.
    let private hasModuleHeader (state: ParseState) =
        let tokens = state.Lexed.Tokens
        let mutable i = 0<token>
        let mutable attributeDepth = 0

        while i < tokens.LengthM
              && (isSkippedByScan state i
                  || attributeDepth > 0
                  || tokens[i].TokenWithoutCommentFlags = Token.KWLAttrBracket) do
            match tokens[i].TokenWithoutCommentFlags with
            | Token.KWLAttrBracket -> attributeDepth <- attributeDepth + 1
            | Token.KWRAttrBracket -> attributeDepth <- attributeDepth - 1
            | _ -> ()

            i <- i + 1<token>

        if i < tokens.LengthM && tokens[i].TokenWithoutCommentFlags = Token.KWModule then
            while i < tokens.LengthM
                  && tokens[i].TokenWithoutCommentFlags <> Token.Newline
                  && tokens[i].TokenWithoutCommentFlags <> Token.OpEquality do
                i <- i + 1<token>

            i >= tokens.LengthM || tokens[i].TokenWithoutCommentFlags <> Token.OpEquality
        else
            false

    /// The column of the first element of the nested module whose keyword is `moduleToken`. The vendored
    /// parser has no frame for the element block of a nested module.
    let private nestedModuleElementColumn (state: ParseState) (moduleToken: PositionedToken) (before: int<token>) =
        let tokens = state.Lexed.Tokens
        let mutable i = before

        while i > 0<token>
              && tokens[i].StartIndex > moduleToken.StartIndex do
            i <- i - 1<token>

        while i < before
              && tokens[i].TokenWithoutCommentFlags <> Token.OpEquality do
            i <- i + 1<token>

        i <- i + 1<token>

        while i < before
              && (isSkippedByScan state i
                  || tokens[i].TokenWithoutCommentFlags = Token.KWBegin) do
            i <- i + 1<token>

        if i <= before then
            ValueSome(ParseState.getIndent state i)
        else
            ValueNone

    /// The FCS LexFilter `undentationLimit` for the block after an opening delimiter at `index`, from
    /// the frames below it. A frame that this function does not model gives no limit.
    let rec private delimitedBlockLimit (state: ParseState) (index: int<token>) (stack: Offside list) =
        match stack with
        | [] -> ValueNone
        | { Context = OffsideContext.SeqBlock; Indent = elementColumn } :: ([]
                                                                         | { Context = OffsideContext.Namespace } :: _) ->
            moduleElementLimit state (hasModuleHeader state) elementColumn index
        | { Context = OffsideContext.SeqBlock } :: { Context = OffsideContext.Module; Token = moduleToken } :: _ ->
            nestedModuleElementColumn state moduleToken index
            |> ValueOption.bind (fun elementColumn -> moduleElementLimit state true elementColumn index)
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.Brace
            | OffsideContext.BraceBar
            | OffsideContext.Begin
            | OffsideContext.Fun
            | OffsideContext.Function
            | OffsideContext.Else
            | OffsideContext.Vanilla -> delimitedBlockLimit state index deeper
            | OffsideContext.MatchClauses ->
                match deeper with
                | ({ Context = OffsideContext.Try | OffsideContext.Match } as owner) :: _ -> ValueSome owner.Indent
                | _ -> ValueSome(columnOfToken state ctx.Token index)
            | OffsideContext.Then
            | OffsideContext.If
            | OffsideContext.Do
            | OffsideContext.Try
            | OffsideContext.Match -> ValueSome ctx.Indent
            | OffsideContext.Let -> ValueSome(ctx.Indent + 1)
            | _ -> ValueNone

    let private previousSyntaxColumn (state: ParseState) (token: PositionedToken) =
        let tokens = state.Lexed.Tokens
        let mutable i = tokens.LengthM - 1<token>

        while i > 0<token>
              && tokens[i].StartIndex >= token.StartIndex do
            i <- i - 1<token>

        while i > 0<token>
              && isSkippedByScan state i do
            i <- i - 1<token>

        ParseState.getIndent state i

    /// The frames that remain after a closing delimiter at `column`: FCS ends a loop body block and
    /// its CtxtDo at a token left of the body and not right of the `do`.
    let rec private framesAfterClose (state: ParseState) (column: int) (stack: Offside list) =
        match stack with
        | ({ Context = OffsideContext.SeqBlock } as body) :: { Context = OffsideContext.Do } :: rest when
            column < body.Indent
            && column <= previousSyntaxColumn state body.Token
            ->
            framesAfterClose state column rest
        | _ -> stack

    let private isCloser (token: Token) =
        match token with
        | Token.KWRParen
        | Token.KWRBracket
        | Token.KWRArrayBracket
        | Token.KWRBrace
        | Token.KWRBraceBar
        | Token.KWEnd -> true
        | _ -> false

    /// When only closing delimiters come before the token at `index` on its line, the first of them.
    let private firstCloserOfLine (state: ParseState) (index: int<token>) =
        let tokens = state.Lexed.Tokens
        let mutable i = index - 1<token>
        let mutable first = index
        let mutable result = ValueNone

        while result.IsNone do
            if i < 0<token> then
                result <- ValueSome(ValueSome first)
            else
                let token = tokens[i]

                if token.TokenWithoutCommentFlags = Token.Newline then
                    result <- ValueSome(ValueSome first)
                elif isSkippedByScan state i then
                    i <- i - 1<token>
                elif isCloser token.TokenWithoutCommentFlags then
                    first <- i
                    i <- i - 1<token>
                else
                    result <- ValueSome ValueNone

        result.Value

    /// The Compatibility Oracle reports FS0010 at a token that follows, on the same line, closing
    /// delimiters where the first starts its line left of the enclosing block. The last closer of the run
    /// checks the token, with the frames that remain after it. A block directly inside
    /// a delimiter has no such limit. A token that aligns with an enclosing construct, such as `else` or `with`,
    /// has the column of that construct as its limit.
    let reportTokenAfterUndentedClose (closeTok: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        let state = reader.State

        match closeTok.Index with
        | TokenIndex.Regular closeIndex ->
          match firstCloserOfLine state closeIndex with
          | ValueSome firstIndex ->
            let column = ParseState.getIndent state firstIndex
            let tokens = state.Lexed.Tokens
            let mutable i = closeIndex + 1<token>
            let mutable next = ValueNone

            while next.IsNone && i < tokens.LengthM do
                let token = tokens[i]

                match token.TokenWithoutCommentFlags with
                | Token.Newline
                | Token.EOF -> i <- tokens.LengthM
                | Token.KWRParen
                | Token.KWRBracket
                | Token.KWRArrayBracket
                | Token.KWRBrace
                | Token.KWRBraceBar
                | Token.KWEnd -> i <- tokens.LengthM
                | _ when isSkippedByScan state i -> i <- i + 1<token>
                | _ -> next <- ValueSome(token, i)

            // FCS LexFilter gives no limit for the block after `fun`, `function`, `->` of a rule, `then`,
            // `else`, or `do`, and uses the frames below it.
            let opensBlock (ctx: OffsideContext) =
                match ctx with
                | OffsideContext.Fun
                | OffsideContext.Function
                | OffsideContext.MatchClauses
                | OffsideContext.Then
                | OffsideContext.Else
                | OffsideContext.Do -> true
                | _ -> false

            // A `fun` that starts the block below it keeps its body block as the limit, unless the closer is
            // also left of that block, which FCS then ends too.
            let startsBlockBelow (rest: Offside list) =
                match rest with
                | ({ Context = OffsideContext.Fun } as lambda) :: { Context = OffsideContext.SeqBlock
                                                                    Token = start
                                                                    Indent = startColumn } :: _ ->
                    start.StartIndex = lambda.Token.StartIndex
                    && column >= startColumn
                | _ -> false

            let rec limitingBlock (stack: Offside list) =
                match stack with
                | { Context = OffsideContext.SeqBlock } :: ({ Context = ctx } :: _ as rest) when
                    opensBlock ctx && not (startsBlockBelow rest)
                    ->
                    rest
                    |> List.skipWhile (fun frame ->
                        frame.Context <> OffsideContext.SeqBlock
                        && not (isParenLike frame.Context)
                    )
                    |> limitingBlock
                | _ -> stack

            match next, limitingBlock (framesAfterClose state column state.Context) with
            | ValueSome(token, index),
              { Context = OffsideContext.SeqBlock; Indent = blockColumn } :: enclosing ->
                let kind = token.TokenWithoutCommentFlags

                let limit =
                    let pairs (frame: Offside) =
                        match frame.Context, kind with
                        | OffsideContext.Then, (Token.KWElse | Token.KWElif) -> ValueSome(frame.Indent - 1)
                        | _ when contextPermitsToken kind System.Int32.MaxValue frame -> ValueSome frame.Indent
                        | _ -> ValueNone

                    match enclosing |> List.tryPick (pairs >> ValueOption.toOption) with
                    | Some ownerColumn -> ValueSome ownerColumn
                    | None ->
                        match enclosing with
                        | { Context = ctx } :: _ when isParenLike ctx -> ValueNone
                        | _ -> ValueSome blockColumn

                match limit with
                | ValueSome limit when column < limit ->
                    reader.State <-
                        ParseState.addDiagnosticAt
                            DiagnosticCode.TokenAfterUndentedClose
                            (syntaxToken token (int index))
                            state
                | _ -> ()
            | _ -> ()
          | ValueNone -> ()
        | TokenIndex.Virtual -> ()

        preturn () reader

    /// For an `else` left of its `if`: the Compatibility Oracle reports FS0010 when the `else` is left of the
    /// block that holds the `if`, and FS0058 when that block is directly inside a delimiter and the `else`
    /// is left of the enclosing offside line, where LexFilter pushes CtxtElse.
    let reportUndentedElse (elseTok: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        match elseTok.Index with
        | TokenIndex.Regular elseIndex ->
            let state = reader.State
            let column = ParseState.getIndent state elseIndex

            let undented =
                match state.Context with
                | { Context = OffsideContext.SeqBlock; Indent = blockColumn } :: { Context = ctx } :: _ when
                    not (isParenLike ctx)
                    ->
                    column < blockColumn
                | _ ->
                    match delimitedBlockLimit state (elseIndex - 1<token>) state.Context with
                    | ValueSome limit -> column < limit
                    | ValueNone -> false

            if undented then
                reader.State <- ParseState.addDiagnosticAt DiagnosticCode.UndentedBlockStart elseTok state
        | TokenIndex.Virtual -> ()

    /// The Compatibility Oracle reports FS0058 when the first token after an opening delimiter is left
    /// of the enclosing offside line, where LexFilter pushes CtxtSeqBlock.
    let reportUndentedBlockStart (reader: Reader<PositionedToken, ParseState, _>) =
        let openIndex = int reader.Index * 1<token> - 1<token>

        match peekNextSyntaxToken reader with
        | Ok first when openIndex >= 0<token> ->
            match first.Index with
            | TokenIndex.Regular firstIndex ->
                let state = reader.State

                match delimitedBlockLimit state openIndex state.Context with
                | ValueSome limit when ParseState.getIndent state firstIndex < limit ->
                    reader.State <- ParseState.addDiagnosticAt DiagnosticCode.UndentedBlockStart first state
                | _ -> ()
            | TokenIndex.Virtual -> ()
        | _ -> ()

        preturn () reader

    /// FCS ends CtxtFun at a lambda pattern or `->` that is not right of the `fun` column, and the Compatibility
    /// Oracle reports FS0010 there. A delimiter in a pattern has its own offside line.
    let reportUndentedLambdaHead (funTok: SyntaxToken) (arrow: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        match funTok.Index, arrow.Index with
        | TokenIndex.Regular funIndex, TokenIndex.Regular arrowIndex ->
            let state = reader.State
            let tokens = state.Lexed.Tokens
            let limit = ParseState.getIndent state funIndex + 1
            let mutable i = funIndex + 1<token>
            let mutable depth = 0
            let mutable reported = false

            while not reported && i <= arrowIndex do
                if not (isSkippedByScan state i) then
                    match tokens[i].TokenWithoutCommentFlags with
                    | Token.KWRParen
                    | Token.KWRBracket
                    | Token.KWRArrayBracket
                    | Token.KWRBrace
                    | Token.KWRBraceBar -> depth <- depth - 1
                    | _ -> ()

                    if depth <= 0 && ParseState.getIndent state i < limit then
                        reader.State <-
                            ParseState.addDiagnosticAt DiagnosticCode.UndentedLambdaHead (syntaxToken tokens[i] (int i)) state

                        reported <- true

                    match tokens[i].TokenWithoutCommentFlags with
                    | Token.KWLParen
                    | Token.KWLBracket
                    | Token.KWLArrayBracket
                    | Token.KWLBrace
                    | Token.KWLBraceBar -> depth <- depth + 1
                    | _ -> ()

                i <- i + 1<token>
        | _ -> ()

        preturn () reader

    /// The rightmost construct of `expr` whose last block continues to the right in FCS: `fun`, `function`,
    /// `match`, `try`, `if`, `while`, or `for`. A delimiter closes such a construct.
    let rec private rightmostOpenConstruct (expr: Expr<SyntaxToken>) =
        match expr with
        | Expr.Fun _
        | Expr.Function _
        | Expr.Match _
        | Expr.TryWith _
        | Expr.TryFinally _
        | Expr.IfThenElse _
        | Expr.While _
        | Expr.ForTo _
        | Expr.ForIn _ -> ValueSome expr
        | Expr.Sequential(items, _)
        | Expr.Tuple(items, _) when items.Length > 0 -> rightmostOpenConstruct items[items.Length - 1]
        | Expr.App(_, args) when args.Length > 0 -> rightmostOpenConstruct args[args.Length - 1]
        | Expr.InfixApp(_, _, right)
        | Expr.PrefixApp(_, right)
        | Expr.LetOrUse(body = ValueSome right) -> rightmostOpenConstruct right
        | _ -> ValueNone

    let private lastRuleBody (Rules(rules = rules)) =
        if rules.Length > 0 then
            match rules[rules.Length - 1] with
            | Rule.Rule(expr = body) -> ValueSome body
            | _ -> ValueNone
        else
            ValueNone

    /// Whether the open constructs at the right end of `expr` hold a `try` construct last. FCS gives a later
    /// `with` or `finally` on the same line to that inner `try`.
    let rec endsInTry (expr: Expr<SyntaxToken>) =
        let last =
            match rightmostOpenConstruct expr with
            | ValueSome(Expr.TryWith _ | Expr.TryFinally _) -> ValueNone
            | ValueSome(Expr.IfThenElse(elseBranch = ValueSome(ElseBranch(expr = body)))) -> ValueSome body
            | ValueSome(Expr.IfThenElse(thenExpr = body; elifBranches = elifs)) ->
                if elifs.Length = 0 then
                    ValueSome body
                else
                    match elifs[elifs.Length - 1] with
                    | ElifBranch.Elif(expr = body)
                    | ElifBranch.ElseIf(expr = body) -> ValueSome body
            | ValueSome(Expr.Fun(expr = body))
            | ValueSome(Expr.While(body = body))
            | ValueSome(Expr.ForTo(body = body))
            | ValueSome(Expr.ForIn(body = body)) -> ValueSome body
            | ValueSome(Expr.Match(rules = rules))
            | ValueSome(Expr.Function(rules = rules)) -> lastRuleBody rules
            | _ -> ValueNone

        match rightmostOpenConstruct expr, last with
        | ValueSome(Expr.TryWith _ | Expr.TryFinally _), _ -> true
        | _, ValueSome body -> endsInTry body
        | _ -> false

    /// The Compatibility Oracle reports FS0010 at a `do` or rule `->` on the line where the expression
    /// before it ends in an open construct, because FCS gives the token to that construct.
    let reportKeywordAfterOpenConstruct
        (header: Expr<SyntaxToken>)
        (keyword: SyntaxToken)
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        if
            (rightmostOpenConstruct header).IsSome
            && followsTokenOnSameLine reader.State keyword
        then
            reader.State <- ParseState.addDiagnosticAt DiagnosticCode.KeywordAfterOpenConstruct keyword reader.State

        preturn () reader

    /// Emits a trace message. Use with `do!` inside a `parser { }` CE for debugging.
    let trace (msg: string) (reader: Reader<PositionedToken, ParseState, _>) =
        ParseState.ifTrace reader.State (fun tc -> tc.Message msg)
        preturn () reader

    /// Consumes the given token, which must have been previously returned by `peekNextSyntaxToken`, and returns it.
    let consumePeeked (token: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        match token.Index with
        | TokenIndex.Virtual -> invalidOp "Cannot consume a virtual token"
        | TokenIndex.Regular tokenIdx ->
            assert (reader.Index = tokenIdx * 1< / token>) // Ensure the reader is still at the expected position
            reader.Index <- (tokenIdx + 1<token>) * 1< / token>
            let col = ParseState.getIndent reader.State tokenIdx
            ParseState.ifTrace reader.State (fun tc -> tc.TokenConsumed(token.PositionedToken, int tokenIdx, col))
            preturn token reader

    /// Peeks the next non-trivia token, asserts it matches the expected token,
    /// consumes it, and returns the token together with its column indent.
    /// Used by keyword expression parsers to capture the keyword's indent for offside context.
    let assertKeywordToken (expected: Token) (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextSyntaxToken reader with
        | Ok t when t.Token = expected ->
            (consumePeeked t
             |>> fun kwTok ->
                 let indent =
                     match kwTok.Index with
                     | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                     | TokenIndex.Virtual -> invalidOp ("Virtual tokens should not be used for '" + string expected + "' keyword")

                 struct (kwTok, indent))
                reader
        | Ok t -> fail (Message ("Expected '" + string expected + "' keyword")) reader
        | Error e -> Error e

    let assertKeywordTokens (expected1: Token) (expected2: Token) (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextSyntaxToken reader with
        | Ok t when t.Token = expected1 || t.Token = expected2 ->
            (consumePeeked t
             |>> fun kwTok ->
                 let indent =
                     match kwTok.Index with
                     | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                     | TokenIndex.Virtual ->
                         invalidOp ("Virtual tokens should not be used for '" + string expected1 + "'|'" + string expected2 + "' keyword")

                 struct (kwTok, indent))
                reader
        | Ok _ -> fail (Message ("Expected '" + string expected1 + "' or '" + string expected2 + "' keyword")) reader
        | Error e -> Error e

    /// The token a refusal at the reader's current position is reported at. `peeked` is what
    /// `peekNextSyntaxToken` returned, passed in rather than re-peeked so the failure is
    /// not traced twice: on `Ok`, that token; on a failure, the RAW token the reader sits
    /// on, which still points to a real place even though the parser would not accept it
    /// there; and `nowhere` past the end of input, where the input offers no token.
    let diagnosticToken
        (peeked: Result<SyntaxToken, ParseError<PositionedToken, ParseState>>)
        (reader: Reader<PositionedToken, ParseState, _>)
        : SyntaxToken =
        match peeked with
        | Ok tok -> tok
        | Error _ ->
            match reader.Peek() with
            | ValueSome tok -> syntaxToken tok reader.Index
            | ValueNone -> SyntaxToken.nowhere

    /// Core of the "match-token-or-synthesise-virtual" pattern. If the next
    /// non-trivia token matches `t`, consume and return it. Otherwise produce a
    /// virtual token of kind `t` without consuming, optionally emitting an error
    /// diagnostic built by `mkDiag` from the token at the failure site.
    let private nextSyntaxTokenVirtualCore
        (mkDiag: SyntaxToken -> DiagnosticCode voption)
        t
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        match peekNextSyntaxToken reader with
        | Ok token when token.Token = t ->
            // Real token matches: consume it and return it.
            consumePeeked token reader
        | result ->
            // Real token doesn't match (Ok with different token) or offside failure (Error):
            // optionally emit a diagnostic and produce a virtual substitute without consuming.
            let diagToken = diagnosticToken result reader

            match mkDiag diagToken with
            | ValueSome code -> reader.State <- ParseState.addDiagnosticAt code diagToken reader.State
            | ValueNone -> ()

            // The substitute stands where `diagToken` does; past end of input that is
            // offset 0, which is what `nowhere` carries.
            let pt = mkVirtualPT t diagToken.StartIndex

            ParseState.ifTrace reader.State (fun tc -> tc.VirtualToken(pt.Token, pt.StartIndex))

            preturn
                {
                    PositionedToken = pt
                    Index = TokenIndex.Virtual
                }
                reader

    let nextSyntaxTokenVirtualIfNot t reader =
        nextSyntaxTokenVirtualCore (fun _ -> ValueNone) t reader

    /// Primary API — takes a pre-built err. Callers are expected to build the `ErrorType.Message`
    /// statically at their module level so it allocates exactly once, not per failure.
    let inline nextSyntaxTokenSatisfiesL
        ([<InlineIfLambda>] predicate: SyntaxToken -> bool)
        (err: ErrorType<PositionedToken, ParseState>)
        =
        fun reader ->
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok token ->
                if predicate token then
                    consumePeeked token reader
                else
                    fail err reader

    let inline nextSyntaxTokenIsL (t: Token) (err: ErrorType<PositionedToken, ParseState>) =
        nextSyntaxTokenSatisfiesL (fun synTok -> synTok.Token = t) err

    /// Convenience wrapper. Builds `Message msg` and delegates. Use this ONLY when the caller
    /// binding is itself at module level (so the Message is allocated once per binding, not
    /// per runtime invocation); inline uses inside a `parser { }` CE will re-allocate the
    /// Message on every outer call.
    let inline nextSyntaxTokenSatisfiesLMsg ([<InlineIfLambda>] predicate: SyntaxToken -> bool) (msg: string) =
        nextSyntaxTokenSatisfiesL predicate (Message msg)

    let inline nextSyntaxTokenIsLMsg (t: Token) (msg: string) = nextSyntaxTokenIsL t (Message msg)

    let isPlainStringOpen (tok: Token) =
        match tok with
        | Token.StringOpen
        | Token.VerbatimStringOpen
        | Token.String3Open -> true
        | _ -> false

    let isPlainStringClose (tok: Token) =
        match tok with
        | Token.StringClose
        | Token.ByteArrayClose
        | Token.VerbatimStringClose
        | Token.VerbatimByteArrayClose
        | Token.String3Close
        | Token.UnterminatedStringLiteral
        | Token.UnterminatedVerbatimStringLiteral
        | Token.UnterminatedString3Literal -> true
        | _ -> false

    let isPlainStringFragment (tok: Token) =
        match tok with
        | Token.StringFragment
        | Token.EscapeSequence
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote
        | Token.FormatPlaceholder
        // A plain (non-interpolated) string body is NOT a printf format: a `%` that
        // does not start a valid placeholder is just literal text. The lexer scans
        // `%` in every string flavor, so this is the only place to keep those
        // chars verbatim — mirrors `pString`'s `isStringInvalidText` handling. It
        // matters for IL-intrinsic instruction strings (the sole consumer of this
        // helper), e.g. a JS-template `(# "$0 % $1" #)`: the string is opaque to
        // the front end, and only a printf-family consumer would care about format
        // validity.
        | Token.InvalidFormatPlaceholder
        | Token.InvalidFormatPercents -> true
        | _ -> false

    let plainStringKindOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringOpen -> StringKind.String t
        | Token.VerbatimStringOpen -> StringKind.VerbatimString t
        | Token.String3Open -> StringKind.String3 t
        | _ -> invalidOp ("Not a plain string open token: " + string t.Token)

    let plainStringPartOfToken (t: SyntaxToken) =
        match t.Token with
        // Literal content collapses to `Text`: escape sequences, the verbatim
        // doubled-quote, and the escaped `%%` are all string content that every
        // consumer (the freeze stitchers, printf typing) treats identically to
        // plain text. Only the *format markers* below stay distinct, because they
        // mark a string as a printf format / non-constant literal. This matches
        // `pString` (the string-expression parser), which delegates plain strings
        // to this one body parser.
        | Token.StringFragment
        | Token.EscapeSequence
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote -> StringPart.Text t
        | Token.FormatPlaceholder -> StringPart.FormatSpecifier t
        // A malformed `%` (or run of `%`) in a plain / IL-intrinsic string is not a
        // valid placeholder, so it is literal text — carried verbatim, NOT an error.
        // The lexer scans `%` in every string flavor, so this is the one place to
        // keep it; only a printf-family consumer would interpret format validity.
        | Token.InvalidFormatPlaceholder
        | Token.InvalidFormatPercents -> StringPart.InvalidText t
        | _ -> invalidOp ("Not a string fragment token: " + string t.Token)

    /// Parses a plain (non-interpolated) string literal into StringKind * StringPart list * closing token.
    let parsePlainStringLiteral msg (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextSyntaxToken reader with
        | Error e -> Error e
        | Ok token when isPlainStringOpen token.Token ->
            match consumePeeked token reader with
            | Error e -> Error e
            | Ok opening ->
                let kind = plainStringKindOfToken opening
                let parts = ResizeArray()
                let mutable closing = Unchecked.defaultof<SyntaxToken>
                let mutable finished = false
                let mutable error = ValueNone

                while not finished do
                    match peekNextSyntaxToken reader with
                    | Error e ->
                        error <- ValueSome e
                        finished <- true
                    | Ok t when isPlainStringFragment t.Token ->
                        match consumePeeked t reader with
                        | Error e ->
                            error <- ValueSome e
                            finished <- true
                        | Ok frag -> parts.Add(plainStringPartOfToken frag)
                    | Ok t when isPlainStringClose t.Token ->
                        match consumePeeked t reader with
                        | Error e ->
                            error <- ValueSome e
                            finished <- true
                        | Ok close ->
                            closing <- close
                            finished <- true
                    | Ok _ -> finished <- true

                match error with
                | ValueSome e -> Error e
                | ValueNone -> preturn (kind, parts.ToImmutableArray(), closing) reader
        | _ -> fail (Message msg) reader

    /// Matches Token.Identifier, Token.BacktickedIdentifier, or Token.UnterminatedBacktickedIdentifier.
    /// Emits a diagnostic for unterminated backticked identifiers.
    /// Primary API — takes a pre-built err; callers should build Message statically at module level.
    let nextSyntaxIdentifierL (err: ErrorType<PositionedToken, ParseState>) =
        fun (reader: Reader<PositionedToken, ParseState, _>) ->
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok token ->
                match token.Token with
                | Token.Identifier
                | Token.BacktickedIdentifier -> consumePeeked token reader
                | Token.UnterminatedBacktickedIdentifier ->
                    reader.State <-
                        ParseState.addDiagnosticAt
                            (DiagnosticCode.Other "Unterminated backticked identifier")
                            token
                            reader.State

                    consumePeeked token reader
                | _ -> fail err reader

    let inline nextSyntaxIdentifierLMsg (msg: string) = nextSyntaxIdentifierL (Message msg)

    let dispatchNextSyntaxTokenFallback (routes: (Token * Parser<_, _, _, _>) list) pFallback =
        // Note: Routes are typically <20 items, so linear search is fine. Likely to be 5 or less in practice.
        // So an array is likely more efficient than a dictionary.
        let items = routes |> List.map fst |> Array.ofList
        let parsers = routes |> List.map snd |> Array.ofList

        parser {
            let! next = peekNextSyntaxToken

            match Array.tryFindIndexV (fun t -> next.Token = t) items with
            | ValueSome i -> return! parsers[i]
            | ValueNone -> return! pFallback
        }

    let dispatchNextSyntaxTokenL (routes: (Token * Parser<_, _, _, _>) list) fallbackMsg =
        dispatchNextSyntaxTokenFallback routes (fail (Message fallbackMsg))

    /// Checks that the raw token immediately before the current reader position is not trivia.
    /// Used by adjacency-based parsers (high-precedence application, type application, measures)
    /// to confirm the upcoming token is truly adjacent to the preceding expression token,
    /// not just adjacent because `peekNextSyntaxToken` consumed intervening trivia.
    let isPrevTokenSyntax (reader: Reader<PositionedToken, ParseState, _>) =
        let idx = reader.Index
        idx > 0 && not (ParseState.isTriviaToken reader.State reader.Input[idx - 1])

    let currentIndent (reader: Reader<PositionedToken, ParseState, 'a>) =
        let state = reader.State
        let index = int reader.Index * 1<token>
        let indent = ParseState.getIndent state index
        preturn indent reader

    /// Returns the column (0-based) of the next non-trivia token without consuming it.
    /// Returns -1 at EOF.
    let peekSyntaxIndent: FSParser<int> =
        lookAhead (fun r ->
            match nextSyntaxToken r with
            | Error _ -> preturn -1 r
            | Ok token ->
                match token.Index with
                | TokenIndex.Virtual -> preturn 0 r
                | TokenIndex.Regular tokenIdx ->
                    let indent = ParseState.getIndent r.State tokenIdx
                    preturn indent r
        )

    /// Synthesises a VirtualSep (virtual `;`) token at the current reader position
    /// without consuming any input.
    let makeVirtualSep (reader: Reader<PositionedToken, ParseState, _>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token ->
            ParseState.ifTrace reader.State (fun tc -> tc.VirtualToken(Token.VirtualSep, token.StartIndex))

            preturn
                {
                    PositionedToken = PositionedToken.Create(Token.VirtualSep, token.StartIndex)
                    Index = TokenIndex.Virtual
                }
                reader

    /// Like nextSyntaxTokenVirtualIfNot but emits a UnclosedDelimiter diagnostic
    /// when the token must be synthesised.
    let nextSyntaxTokenVirtualWithDiagnostic (openTok: SyntaxToken voption) t reader =
        let mkDiag _ =
            let code =
                match openTok with
                | ValueSome o -> DiagnosticCode.UnclosedDelimiter(o.Token, Site.ofToken o, t)
                | ValueNone -> DiagnosticCode.Other(DiagnosticCode.expecting t)

            ValueSome code

        nextSyntaxTokenVirtualCore mkDiag t reader

    /// Wraps a parser so that on failure, it emits a diagnostic and returns a virtual token
    /// of the given type. Used for committed-keyword recovery where the parser must not fail
    /// after a keyword has been consumed.
    let recoverWithVirtualToken
        (expectedToken: Token)
        (diagMsg: string)
        (p: Parser<SyntaxToken, PositionedToken, ParseState, _>)
        : Parser<SyntaxToken, PositionedToken, ParseState, _> =
        fun reader ->
            match p reader with
            | Ok result -> Ok result
            | Error _ ->
                match peekNextSyntaxToken reader with
                | Error e -> Error e
                | Ok token ->
                    reader.State <- ParseState.addDiagnosticAt (DiagnosticCode.Other diagMsg) token reader.State

                    let pt = mkVirtualPT expectedToken token.StartIndex

                    ParseState.ifTrace reader.State (fun tc -> tc.VirtualToken(pt.Token, pt.StartIndex))

                    preturn
                        {
                            PositionedToken = pt
                            Index = TokenIndex.Virtual
                        }
                        reader

    /// Wraps a LongIdent parser so that on failure, it emits a diagnostic and returns a
    /// single-element virtual identifier. Used after committed keywords like `open`, `namespace`, `module`.
    let recoverLongIdent
        (diagMsg: string)
        (p: Parser<LongIdent<SyntaxToken>, PositionedToken, ParseState, _>)
        : Parser<LongIdent<SyntaxToken>, PositionedToken, ParseState, _> =
        fun reader ->
            match p reader with
            | Ok result -> Ok result
            | Error _ ->
                match peekNextSyntaxToken reader with
                | Error e -> Error e
                | Ok token ->
                    reader.State <- ParseState.addDiagnosticAt (DiagnosticCode.Other diagMsg) token reader.State

                    let pt = mkVirtualPT Token.Identifier token.StartIndex

                    ParseState.ifTrace reader.State (fun tc -> tc.VirtualToken(pt.Token, pt.StartIndex))

                    let virtualIdent: SyntaxToken =
                        {
                            PositionedToken = pt
                            Index = TokenIndex.Virtual
                        }

                    preturn
                        ({
                            Idents = ImmutableArray.Create(virtualIdent)
                            Dots = ImmutableArray.Empty
                        }
                        : LongIdent<SyntaxToken>)
                        reader

    module StoppingTokens =
        let afterType (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpComma
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.KWRBraceBar
            | Token.OpEquality
            | Token.KWWith
            | Token.KWIn
            | Token.OpArrowRight
            | Token.OpBar
            | Token.OpSemicolon
            | Token.EOF -> true
            | _ -> false

        let afterPattern (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpArrowRight
            | Token.KWWhen
            | Token.OpBar
            | Token.OpEquality
            | Token.KWIn
            | Token.EOF -> true
            | _ -> false

        let afterExpr (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpSemicolon
            | Token.KWIn
            | Token.OpBar
            | Token.KWWith
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.KWRBraceBar
            | Token.KWThen
            | Token.KWElse
            | Token.KWElif
            | Token.KWEnd
            | Token.KWDone
            | Token.KWDo
            | Token.InterpolatedExpressionClose
            | Token.EOF -> true
            | _ -> false

        let afterParen (tok: SyntaxToken) =
            match tok.Token with
            | Token.KWWith
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.KWRBraceBar
            | Token.KWEnd
            | Token.KWRArrayBracket
            | Token.OpQuotationTypedRight
            | Token.OpQuotationUntypedRight
            | Token.EOF -> true
            | _ -> false

        let afterRule (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpBar
            | Token.KWWith
            | Token.KWEnd
            | Token.EOF -> true
            | _ -> false

        /// Inside `member x.P with get … and set …`: a rejected member resumes at the next
        /// `and`, or at whatever starts the next member of the enclosing type.
        let afterGetSetBinding (tok: SyntaxToken) =
            match tok.Token with
            | Token.KWAnd
            | Token.KWMember
            | Token.KWAbstract
            | Token.KWStatic
            | Token.KWOverride
            | Token.KWDefault
            | Token.KWVal
            | Token.KWNew
            | Token.KWInterface
            | Token.KWEnd
            | Token.KWType
            | Token.EOF -> true
            | _ -> false

        let afterTypeDefn (tok: SyntaxToken) =
            match tok.Token with
            | Token.KWAnd
            | Token.KWType
            | Token.KWEnd
            | Token.EOF -> true
            | _ -> false

        let afterModuleElem (tok: SyntaxToken) =
            match tok.Token with
            // Tokens that can start a new module element
            | Token.KWLet
            | Token.KWDo
            | Token.KWOpen
            | Token.KWType
            | Token.KWModule
            | Token.KWNamespace
            | Token.KWException
            | Token.KWHash
            | Token.KWLAttrBracket // [< starts attributes which precede module elements
            // Closing/structural tokens that should not be consumed
            | Token.KWEnd
            | Token.KWWith
            | Token.KWIn
            | Token.EOF -> true
            | _ -> false

    /// THE placeholder shape `recoverWith` hands back: the node's own "missing" case when
    /// the skip swallowed nothing, and its token-carrying case when it swallowed something,
    /// so the skipped tokens survive into the tree instead of vanishing from it.
    let missingOrSkipped (missing: 'Parsed) (skipsTokens: ImArr<SyntaxToken> -> 'Parsed) (toks: ImArr<SyntaxToken>) =
        match toks.Length with
        | 0 -> missing
        | _ -> skipsTokens toks

    /// On failure: emits a diagnostic with the given code and the underlying ParseError,
    /// skips tokens until `stopping` returns true,
    /// then succeeds with `placeholder skippedTokens`.
    let recoverWith
        (stopping: SyntaxToken -> bool)
        (code: DiagnosticCode)
        (placeholder: ImArr<SyntaxToken> -> 'Parsed)
        (p: Parser<'Parsed, PositionedToken, ParseState, _>)
        : Parser<'Parsed, PositionedToken, ParseState, _> =
        fun reader ->
            match peekNextSyntaxToken reader with
            | Error e -> Error e
            | Ok startTok ->
                let pos = reader.Position

                match p reader with
                | Ok result -> Ok result
                | Error err ->
                    reader.Position <- pos // backtrack to the start of the failed parse, so we can skip the same tokens it would have seen
                    let skipped = ResizeArray<SyntaxToken>()
                    let mutable keepGoing = true

                    while keepGoing do
                        match peekNextSyntaxToken reader with
                        | Error _ -> keepGoing <- false
                        | Ok tok ->
                            if stopping tok then
                                keepGoing <- false
                            else
                                match consumePeeked tok reader with
                                | Ok t -> skipped.Add(t)
                                | Error _ -> keepGoing <- false

                    reader.State <- ParseState.addDiagnosticWithError code startTok err reader.State

                    Ok(placeholder (skipped.ToImmutableArray()))

    /// Wraps a parser with an offside context. Peeks the first token the inner parser will see
    /// to establish the offside column, pushes an Offside entry onto ParseState.Context, runs
    /// the inner parser, then pops the context on success or restores the full saved state on
    /// failure (keeping the operation safe for backtracking).
    let withContext (ctx: OffsideContext) innerParser (reader: Reader<PositionedToken, ParseState, _>) =
        let savedState = reader.State

        match peekNextSyntaxToken reader with
        | Error e -> Error e
        | Ok peekTok ->
            let indent =
                match peekTok.Index with
                | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                | TokenIndex.Virtual -> 0

            let entry =
                {
                    Context = ctx
                    Indent = indent
                    Token = peekTok.PositionedToken
                }

            reader.State <- ParseState.pushOffside entry reader.State

            match innerParser reader with
            | Ok result ->
                reader.State <- ParseState.popOffside entry reader.State
                Ok result
            | Error _ as e ->
                reader.State <- savedState
                e

    /// Like `withContext`, but uses an explicit indent and token for the offside context
    /// instead of peeking the next token. Used when the context's offside line should be
    /// at an already-known position (e.g. the `let` keyword's column per F# spec 15.1.7).
    let withContextAt
        (ctx: OffsideContext)
        (indent: int)
        (token: PositionedToken)
        innerParser
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        let savedState = reader.State

        let entry =
            {
                Context = ctx
                Indent = indent
                Token = token
            }

        reader.State <- ParseState.pushOffside entry reader.State

        match innerParser reader with
        | Ok result ->
            reader.State <- ParseState.popOffside entry reader.State
            Ok result
        | Error _ as e ->
            reader.State <- savedState
            e

    /// Like `withContextAt`, but reads the offside column off an already-parsed token rather
    /// than being told it. A virtual token has no column of its own, so it throws.
    let withContextAtToken
        (ctx: OffsideContext)
        (anchor: SyntaxToken)
        innerParser
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        let indent =
            match anchor.Index with
            | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
            | TokenIndex.Virtual -> failwith ("Attempted to set indent context with a virtual token " + string anchor.PositionedToken)

        withContextAt ctx indent anchor.PositionedToken innerParser reader

    /// Record field separator: accepts a real ';' or emits a virtual separator when the next
    /// token is at the same indent as the enclosing SeqBlock context (spec §15.1.5: $sep insertion).
    let pRecordFieldSep: FSParser<SyntaxToken> =
        let failSep = fail (Message "Expected ';' or newline at the same indent")

        parser {
            match! peekNextSyntaxToken with
            | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
            | t when t.Token = Token.KWRBrace || t.Token = Token.KWRBraceBar -> return! failSep
            | t ->
                let! indent = currentIndent
                let! state = getUserState

                let atContextIndent =
                    match state.Context with
                    | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                    | [] -> false

                if atContextIndent then
                    return virtualToken (PositionedToken.Create(Token.OpSemicolon, t.StartIndex))
                else
                    return! failSep
        }

    /// Fails if the next syntax token is 't'. Saves and restores reader position fully.
    let notFollowedBySyntaxToken t (reader: Reader<PositionedToken, ParseState, _>) =
        let pos = reader.Position

        match peekNextSyntaxToken reader with
        | Ok tok when tok.Token = t ->
            reader.Position <- pos
            fail (Message ("Named module cannot be followed by '" + string t + "'")) reader
        | _ ->
            reader.Position <- pos
            preturn () reader

    let inline choiceL p msg =
        // Shadow choiceL to give the full error message in debug builds,
        // but avoid the overhead of constructing the full error message
        // in release builds where it shouldn't be needed.
#if DEBUG
        choice p
#else
        choiceL p msg
#endif

    let private errExpectedGtCloseTypeApp: ErrorType<PositionedToken, ParseState> =
        Message "Expected '>' to close type application"

    let pCloseTypeParams: FSParser<SyntaxToken> =
        // 15.3 Lexical Analysis of Type Applications.
        //
        // Closes a type application with a `>` that may be fused inside a larger
        // operator like `>>`, `>>>`, `>=`, `>>=`, `>=.`, etc. When fused, we emit
        // a virtual `>` at the current char offset and advance CharsConsumedAfterTypeParams
        // so reprocessedOperatorAfterTypeParams can re-lex the remaining chars.
        //
        // The well-known `>`-starting operator tokens (OpGreaterThan, OpComposeRight,
        // OpRightShift, OpGreaterThanOrEqual) each dispatch on their enum value. Other
        // `>`-starting custom ops (`>>=`, `>=.`, `>..`, `>->`, `>?>`, `>|`, ...) share
        // the generic ComparisonAndBitwise slot and fall back to the span check.
        parser {
            let! state = getUserState
            let charsConsumed = state.CharsConsumedAfterTypeParams

            let emitVirtualGt (t: SyntaxToken) =
                parser {
                    let rAngle =
                        virtualToken (PositionedToken.Create(Token.OpGreaterThan, t.StartIndex + charsConsumed))

                    do!
                        updateUserState (fun s ->
                            { s with
                                CharsConsumedAfterTypeParams = charsConsumed + 1
                            }
                        )

                    return rAngle
                }

            let consumeAndReset (t: SyntaxToken) =
                parser {
                    let! rAngle = consumePeeked t

                    do!
                        updateUserState (fun s ->
                            { s with
                                CharsConsumedAfterTypeParams = 0
                            }
                        )

                    return rAngle
                }

            // Bind `t.Token` once and dispatch on the enum directly; F# emits a jumptable
            // for plain enum patterns. The prior `match t with | t when t.Token = ...`
            // form re-loaded `t.Token` per `when` clause and compiled to a sequential
            // chain of conditional branches.
            let! t = peekNextSyntaxToken
            let token = t.Token

            match token with
            | Token.OpGreaterThan when charsConsumed = 0 ->
                // Bare `>` — consume directly.
                return! consumePeeked t

            | Token.OpComposeRight ->
                // `>>` — two `>` chars.
                if charsConsumed < 1 then return! emitVirtualGt t
                elif charsConsumed = 1 then return! consumeAndReset t
                else return! fail errExpectedGtCloseTypeApp

            | Token.OpRightShift ->
                // `>>>` — three `>` chars.
                if charsConsumed < 2 then return! emitVirtualGt t
                elif charsConsumed = 2 then return! consumeAndReset t
                else return! fail errExpectedGtCloseTypeApp

            | Token.OpGreaterThanOrEqual when charsConsumed = 0 ->
                // `>=`: first char is `>`, the rest is `=`.
                return! emitVirtualGt t

            | _ when TokenInfo.isOperator token ->
                // Generic `>`-starting custom ops (`>>=`, `>=.`, `>..`, etc.) share the
                // OpGeneric slot, so we still need to look at the span text.
                let opString = tokenString t state

                if opString.Length > charsConsumed && opString.[charsConsumed] = '>' then
                    if opString.Length = charsConsumed + 1 then
                        return! consumeAndReset t
                    else
                        return! emitVirtualGt t
                else
                    return! fail errExpectedGtCloseTypeApp

            | _ -> return! fail errExpectedGtCloseTypeApp
        }

    let private errExpectedOperatorAfterTypeParams: ErrorType<PositionedToken, ParseState> =
        Message "Expected operator after type parameters"

    let private errNoOperatorToReprocess: ErrorType<PositionedToken, ParseState> =
        Message "No operator to reprocess after type parameters"

    let reprocessedOperatorAfterTypeParams: FSParser<SyntaxToken> =
        parser {
            let! state = getUserState
            let charsConsumed = state.CharsConsumedAfterTypeParams

            if charsConsumed > 0 then
                match! peekNextSyntaxToken with
                // Any `>`-starting operator — well-known (OpGreaterThan, OpComposeRight,
                // OpRightShift, OpGreaterThanOrEqual) or generic `>`-starting custom op —
                // is a candidate for reprocessing. The residual span is reclassified
                // directly via `Lexing.classifyOpSpan` instead of the full lex pipeline,
                // which would allocate a Substring + a Lexed (Tokens/LineStarts arrays)
                // per call.
                | t when TokenInfo.isOperator t.Token ->
                    let! op = consumePeeked t

                    do!
                        updateUserState (fun s ->
                            { s with
                                CharsConsumedAfterTypeParams = 0
                            }
                        )

                    match t.Index with
                    | TokenIndex.Regular iT ->
                        let opSpan = state.Lexed.GetTokenSpan(iT)
                        let residual = opSpan.Slice(charsConsumed)
                        let residualTok = Lexing.classifyOpSpan residual

                        return
                            { op with
                                PositionedToken = PositionedToken.Create(residualTok, t.StartIndex + charsConsumed)
                            }
                    | TokenIndex.Virtual -> return! fail errExpectedOperatorAfterTypeParams
                | _ -> return! fail errExpectedOperatorAfterTypeParams
            else
                return! fail errNoOperatorToReprocess

        }


    let private errPEnclosedInnerFailed: ErrorType<PositionedToken, ParseState> =
        Message "pEnclosed inner parser failed"

    let pEnclosed
        completeEmpty
        completeEnclosed
        missing
        skipsTokens
        (pLeft: Parser<_, _, _, _>)
        (expectedRightTok: Token)
        (parenKindConstructor: SyntaxToken -> ParenKind<SyntaxToken>)
        (offsideCtx: OffsideContext)
        (diagCode: DiagnosticCode)
        (pInner: Parser<_, _, _, _>)
        : Parser<_, PositionedToken, ParseState, _> =

        fun reader ->
            match pLeft reader with
            | Error e -> Error e
            | Ok l ->

                // Push the paren-like offside context immediately after consuming the left
                // delimiter. This must happen before any inner peek/parse so that the
                // collection-undentation rule (15.1.10.4) can see the context on the stack
                // when the inner content is at a lower indentation than the outer SeqBlock.
                let savedState = reader.State

                let entry: Offside =
                    {
                        Context = offsideCtx
                        Indent = 0 // Paren-like contexts use indent 0; undentation rules inspect them as stack markers
                        Token = l.PositionedToken
                    }

                reader.State <- ParseState.pushOffside entry reader.State

                let inline popAndReturn result =
                    reader.State <- ParseState.popOffside entry reader.State
                    result

                match peekNextSyntaxToken reader with
                | Error e ->
                    reader.State <- savedState
                    Error e
                | Ok t when t.Token = expectedRightTok ->
                    // Fast path: Empty block
                    match consumePeeked t reader with
                    | Ok r -> popAndReturn (Ok(completeEmpty (parenKindConstructor l) r))
                    | Error e ->
                        reader.State <- savedState
                        Error e
                | _ ->
                    // Normal path with recovery
                    let innerParser =
                        recoverWith
                            StoppingTokens.afterParen
                            diagCode
                            (fun toks ->
                                if toks.IsEmpty then
                                    let endTok =
                                        virtualToken (PositionedToken.Create(expectedRightTok, l.StartIndex + 1))

                                    completeEnclosed (parenKindConstructor l) missing endTok
                                else
                                    let endTok =
                                        let t = toks[toks.Length - 1]
                                        virtualToken (PositionedToken.Create(expectedRightTok, t.StartIndex))

                                    completeEnclosed (parenKindConstructor l) (skipsTokens toks) endTok
                            )
                            (parser {
                                let! e = pInner
                                let! r = nextSyntaxTokenVirtualWithDiagnostic (ValueSome l) expectedRightTok
                                return completeEnclosed (parenKindConstructor l) e r
                            })

                    match innerParser reader with
                    | Ok result -> popAndReturn (Ok result)
                    | Error _ ->
                        reader.State <- savedState
                        fail errPEnclosedInnerFailed reader
