namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type internal DirectiveKind =
    | Line
    | Light
    | Indent
    | Conditional
    | Warning

type internal LexicalDirective = {
    Kind: DirectiveKind
    Text: string
    Range: SourceRange
}

type internal PathNeutralWarningDirective = {
    Order: int64
    Action: LocalWarningDirectiveAction
    Code: string
    LogicalPath: string option
    Range: SourceRange
}

type internal DirectiveResult = {
    CompatibilityText: string
    SourceMap: SourceMap
    Directives: ImmutableArray<LexicalDirective>
    WarningDirectives: ImmutableArray<PathNeutralWarningDirective>
    Diagnostics: ImmutableArray<SourceLexicalDiagnostic>
}

module internal Directives =
    type private ConditionalFrame = {
        ParentActive: bool
        AnyTaken: bool
        CurrentActive: bool
        HasElse: bool
        Range: SourceRange
    }

    type private ConditionToken =
        | ConditionIdentifier of string
        | ConditionNot
        | ConditionAnd
        | ConditionOr
        | ConditionOpen
        | ConditionClose
        | ConditionEnd

    type internal ConditionError = {
        Code: string
        Message: string
        Index: int
    }

    let private tokenizeCondition (text: string) =
        let tokens = ResizeArray<ConditionToken * int>()
        let mutable index = 0
        let mutable error = None

        while error.IsNone
              && index < text.Length do
            let current = text[index]

            if Char.IsWhiteSpace current then
                index <- index + 1
            elif
                Char.IsLetter current
                || current = '_'
            then
                let start = index

                while index < text.Length
                      && (Char.IsLetterOrDigit text[index]
                          || text[index] = '_') do
                    index <- index + 1

                tokens.Add(
                    ConditionIdentifier(
                        text.Substring(
                            start,
                            index
                            - start
                        )
                    ),
                    start
                )
            elif current = '!' then
                tokens.Add(ConditionNot, index)
                index <- index + 1
            elif current = '(' then
                tokens.Add(ConditionOpen, index)
                index <- index + 1
            elif current = ')' then
                tokens.Add(ConditionClose, index)
                index <- index + 1
            elif
                current = '&'
                && index + 1 < text.Length
                && text[index + 1] = '&'
            then
                tokens.Add(ConditionAnd, index)
                index <- index + 2
            elif
                current = '|'
                && index + 1 < text.Length
                && text[index + 1] = '|'
            then
                tokens.Add(ConditionOr, index)
                index <- index + 2
            else
                error <-
                    Some {
                        Code = "FS3182"
                        Message = $"Unexpected character '{current}' in preprocessor expression"
                        Index = index + 1
                    }

        match error with
        | Some error -> Error error
        | None ->
            tokens.Add(ConditionEnd, text.Length)
            Ok(tokens.ToArray())

    let internal evaluateCondition (defines: Set<string>) (text: string) =
        let incomplete index = {
            Code = "FS3184"
            Message = "Incomplete preprocessor expression"
            Index = index
        }

        tokenizeCondition text
        |> Result.bind (fun tokens ->
            let mutable position = 0
            let current () = tokens[position]

            let rec parseOr () =
                parseAnd ()
                |> Result.bind (fun left ->
                    match fst (current ()) with
                    | ConditionOr ->
                        position <-
                            position
                            + 1

                        parseOr ()
                        |> Result.map (fun right ->
                            left
                            || right
                        )
                    | _ -> Ok left
                )

            and parseAnd () =
                parseUnary ()
                |> Result.bind (fun left ->
                    match fst (current ()) with
                    | ConditionAnd ->
                        position <-
                            position
                            + 1

                        parseAnd ()
                        |> Result.map (fun right ->
                            left
                            && right
                        )
                    | _ -> Ok left
                )

            and parseUnary () =
                match current () with
                | ConditionNot, _ ->
                    position <-
                        position
                        + 1

                    parseUnary ()
                    |> Result.map not
                | ConditionIdentifier name, _ ->
                    position <-
                        position
                        + 1

                    Ok(defines.Contains name)
                | ConditionOpen, _ ->
                    position <-
                        position
                        + 1

                    parseOr ()
                    |> Result.bind (fun value ->
                        match current () with
                        | ConditionClose, _ ->
                            position <-
                                position
                                + 1

                            Ok value
                        | _, index ->
                            Error {
                                Code = "FS3185"
                                Message = "Missing token ')' in preprocessor expression"
                                Index = index
                            }
                    )
                | _, index -> Error(incomplete index)

            parseOr ()
            |> Result.bind (fun value ->
                match current () with
                | ConditionEnd, _ -> Ok value
                | _, index -> Error(incomplete index)
            )
        )

    let analyze language (defines: Set<string>) (source: DecodedSource) (lexed: LexerResult) =
        let output = source.Text.ToCharArray()
        let directives = ResizeArray<LexicalDirective>()
        let warnings = ResizeArray<PathNeutralWarningDirective>()
        let diagnostics = ResizeArray<SourceLexicalDiagnostic>(lexed.Diagnostics)
        let mutable sourceMap = source.Map
        let mutable frames: ConditionalFrame list = []
        let mutable active = true
        let mutable warningOrder = 0L
        let mutable diagnosticOrder = int64 diagnostics.Count

        let addDiagnostic code message range =
            diagnostics.Add {
                Code = code
                Message = message
                Range = range
                Order = diagnosticOrder
            }

            diagnosticOrder <-
                diagnosticOrder
                + 1L

        let blank startOffset endOffset =
            for index = startOffset to endOffset
                                       - 1 do
                if
                    output[index]
                    <> '\r'
                    && output[index]
                       <> '\n'
                then
                    output[index] <- ' '

        let quotedValue (value: string) =
            let first = value.IndexOf '"'
            let last = value.LastIndexOf '"'

            if
                first >= 0
                && last > first
            then
                Some(
                    value.Substring(
                        first + 1,
                        last
                        - first
                        - 1
                    )
                )
            else
                None

        let protectedSpans = [|
            yield!
                lexed.Trivia
                |> Seq.filter (fun trivia -> trivia.Kind = LexicalTriviaKind.BlockComment)
                |> Seq.map _.Range
            yield!
                lexed.Tokens
                |> Seq.filter (fun token ->
                    token.Kind = LexicalTokenKind.StringLiteral
                    || token.Kind = LexicalTokenKind.ByteStringLiteral
                )
                |> Seq.map _.Range
        |]

        let inactiveLines = HashSet<int>()

        let isProtected offset =
            protectedSpans
            |> Array.exists (fun range ->
                range.Start.Offset < offset
                && offset < range.End.Offset
                && not (inactiveLines.Contains range.Start.Line)
            )

        for lineIndex = 0 to source.Map.LineStarts.Length
                             - 1 do
            let startOffset = source.Map.LineStarts[lineIndex]

            if not active then
                inactiveLines.Add(
                    lineIndex
                    + 1
                )
                |> ignore

            let endOffset =
                if
                    lineIndex
                    + 1 < source.Map.LineStarts.Length
                then
                    source.Map.LineStarts[lineIndex
                                          + 1]
                else
                    source.Text.Length

            let mutable contentEnd = endOffset

            while contentEnd > startOffset
                  && (source.Text[contentEnd
                                  - 1] = '\r'
                      || source.Text[contentEnd
                                     - 1] = '\n') do
                contentEnd <-
                    contentEnd
                    - 1

            let lineText =
                source.Text.Substring(
                    startOffset,
                    contentEnd
                    - startOffset
                )

            let trimmed =
                let text = lineText.TrimStart()

                let start =
                    startOffset
                    + lineText.Length
                    - text.Length

                if isProtected start then String.Empty else text

            let directiveStart =
                startOffset
                + lineText.Length
                - trimmed.Length

            let directiveRange = {
                Start = SourceMap.positionAt source.Map directiveStart
                End = SourceMap.positionAt source.Map contentEnd
            }

            let withoutLineComment (text: string) =
                match text.IndexOf("//", StringComparison.Ordinal) with
                | -1 -> text
                | index -> text.Substring(0, index)

            let isDirective (name: string) requiresArgument =
                trimmed.StartsWith(name, StringComparison.Ordinal)
                && (let rest = trimmed.Substring(name.Length)

                    if requiresArgument then
                        rest.Length > 0
                        && Char.IsWhiteSpace rest[0]
                    else
                        String.IsNullOrWhiteSpace(withoutLineComment rest))

            let conditionAt prefixLength =
                match
                    evaluateCondition defines (withoutLineComment (trimmed.Substring(prefixLength)))
                with
                | Ok value -> value
                | Error error ->
                    let position =
                        SourceMap.positionAt
                            source.Map
                            (directiveStart
                             + prefixLength
                             + error.Index)

                    addDiagnostic error.Code error.Message { Start = position; End = position }
                    false

            let add kind =
                diagnostics.RemoveAll(fun diagnostic ->
                    diagnostic.Order < int64 lexed.Diagnostics.Length
                    && diagnostic.Range.Start.Offset
                       >= startOffset
                    && diagnostic.Range.Start.Offset < contentEnd
                )
                |> ignore

                directives.Add {
                    Kind = kind
                    Text = trimmed
                    Range = directiveRange
                }

            if isDirective "#if" true then
                blank startOffset contentEnd
                add DirectiveKind.Conditional
                let condition = conditionAt 4

                frames <-
                    {
                        ParentActive = active
                        AnyTaken = condition
                        CurrentActive =
                            active
                            && condition
                        HasElse = false
                        Range = directiveRange
                    }
                    :: frames

                active <-
                    active
                    && condition
            elif isDirective "#elif" true then
                blank startOffset contentEnd
                add DirectiveKind.Conditional

                match frames with
                | [] ->
                    let keywordStart =
                        SourceMap.positionAt
                            source.Map
                            (directiveStart
                             + 1)

                    addDiagnostic
                        "FS0010"
                        "Unexpected keyword 'elif' in directive. Expected identifier or other token."
                        {
                            Start = keywordStart
                            End = keywordStart
                        }
                | frame :: tail ->
                    let condition =
                        let value = conditionAt 6

                        not frame.AnyTaken
                        && value

                    let next = {
                        frame with
                            AnyTaken =
                                frame.AnyTaken
                                || condition
                            CurrentActive =
                                frame.ParentActive
                                && condition
                    }

                    frames <-
                        next
                        :: tail

                    active <- next.CurrentActive
            elif isDirective "#else" false then
                blank startOffset contentEnd
                add DirectiveKind.Conditional

                match frames with
                | [] ->
                    addDiagnostic
                        "FS0010"
                        "#else has no matching #if in definition. Expected incomplete structured construct at or before this point or other token."
                        directiveRange
                | frame :: _ when frame.HasElse ->
                    addDiagnostic "FS0010" "Duplicate '#else'." directiveRange
                | frame :: tail ->
                    let next = {
                        frame with
                            HasElse = true
                            CurrentActive =
                                frame.ParentActive
                                && not frame.AnyTaken
                            AnyTaken = true
                    }

                    frames <-
                        next
                        :: tail

                    active <- next.CurrentActive
            elif isDirective "#endif" false then
                blank startOffset contentEnd
                add DirectiveKind.Conditional

                match frames with
                | [] ->
                    addDiagnostic
                        "FS0010"
                        "#endif has no matching #if in definition. Expected incomplete structured construct at or before this point or other token."
                        directiveRange
                | frame :: tail ->
                    frames <- tail
                    active <- frame.ParentActive
            elif not active then
                blank startOffset contentEnd
            elif
                isDirective "#line" true
                || (trimmed.Length > 2
                    && trimmed[0] = '#'
                    && Char.IsWhiteSpace trimmed[1]
                    && Char.IsDigit(trimmed.TrimStart('#').TrimStart()[0]))
            then
                blank startOffset contentEnd
                add DirectiveKind.Line

                let arguments =
                    (withoutLineComment (
                        if trimmed.StartsWith("#line", StringComparison.Ordinal) then
                            trimmed.Substring(5)
                        else
                            trimmed.Substring(1)
                    ))
                        .Trim()
                        .Split(' ', 2, StringSplitOptions.RemoveEmptyEntries)

                match arguments with
                | [| number |] ->
                    match Int32.TryParse number with
                    | true, logicalLine when logicalLine > 0 ->
                        sourceMap <-
                            SourceMap.addLineMapping
                                sourceMap
                                (lineIndex
                                 + 2)
                                logicalLine
                                None
                    | _ -> ()
                | [| number; rest |] ->
                    match Int32.TryParse number, quotedValue rest with
                    | (true, logicalLine), Some path when
                        logicalLine > 0
                        && not (String.IsNullOrWhiteSpace path)
                        ->
                        sourceMap <-
                            SourceMap.addLineMapping
                                sourceMap
                                (lineIndex
                                 + 2)
                                logicalLine
                                (Some path)
                    | _ -> ()
                | _ -> ()
            elif trimmed.StartsWith("#light", StringComparison.Ordinal) then
                blank startOffset contentEnd
                add DirectiveKind.Light
            elif trimmed.StartsWith("#indent", StringComparison.Ordinal) then
                blank startOffset contentEnd
                add DirectiveKind.Indent
            elif
                trimmed.StartsWith("#nowarn", StringComparison.Ordinal)
                || trimmed.StartsWith("#warnon", StringComparison.Ordinal)
            then
                blank startOffset contentEnd
                add DirectiveKind.Warning

                let nameLength = "#nowarn".Length

                let argumentText = withoutLineComment (trimmed.Substring(nameLength))

                let arguments =
                    Text.RegularExpressions.Regex.Matches(argumentText, "\"[^\"]*\"|[^\\s\"]+")
                    |> Seq.map (fun argument ->
                        argument.Value.Trim('"'),
                        directiveStart
                        + nameLength
                        + argument.Index
                    )
                    |> Seq.toList

                let isWarningNumber (value: string) =
                    let digits =
                        if value.StartsWith("FS", StringComparison.OrdinalIgnoreCase) then
                            value.Substring(2)
                        else
                            value

                    digits.Length > 0
                    && Seq.forall Char.IsDigit digits

                if List.isEmpty arguments then
                    let position = SourceMap.positionAt source.Map directiveStart

                    addDiagnostic
                        "FS3875"
                        "Warn directives must have warning number(s) as argument(s)"
                        { Start = position; End = position }

                for code, argumentOffset in arguments do
                    if not (isWarningNumber code) then
                        let position = SourceMap.positionAt source.Map argumentOffset

                        addDiagnostic "FS0203" $"Invalid warning number '{code}'" {
                            Start = position
                            End = position
                        }
                    else
                        let action =
                            if trimmed.StartsWith("#warnon", StringComparison.Ordinal) then
                                LocalWarningDirectiveAction.Enable
                            else
                                LocalWarningDirectiveAction.Disable

                        warnings.Add {
                            Order = warningOrder
                            Action = action
                            Code = code
                            LogicalPath = None
                            Range = directiveRange
                        }

                        warningOrder <-
                            warningOrder
                            + 1L

                        if
                            action = LocalWarningDirectiveAction.Enable
                            && not language.SupportsScopedWarningDirectives
                        then
                            addDiagnostic
                                "FS3350"
                                $"Feature 'Support for scoped enabling / disabling of warnings by #warn and #nowarn directives, also inside modules' is not available in F# {language.CanonicalMode}. Please use language version 10.0 or greater."
                                directiveRange
            elif not active then
                blank startOffset contentEnd

        for frame in
            frames
            |> List.rev do
            addDiagnostic "FS0513" "End of file in #if section begun at or after here" frame.Range

        let warningScopes =
            warnings
            |> Seq.mapi (fun index warning ->
                let scopeEnd =
                    warnings
                    |> Seq.skip (index + 1)
                    |> Seq.tryFind (fun candidate -> candidate.Code = warning.Code)
                    |> Option.map _.Range.Start
                    |> Option.defaultValue (SourceMap.positionAt source.Map source.Text.Length)

                {
                    warning with
                        Range = {
                            Start = warning.Range.End
                            End = scopeEnd
                        }
                }
            )
            |> ImmutableArray.CreateRange

        diagnostics.RemoveAll(fun diagnostic ->
            let offset = diagnostic.Range.Start.Offset

            diagnostic.Order < int64 lexed.Diagnostics.Length
            && offset < output.Length
            && output[offset]
               <> source.Text[offset]
        )
        |> ignore

        {
            CompatibilityText = String output
            SourceMap = sourceMap
            Directives = ImmutableArray.CreateRange directives
            WarningDirectives = warningScopes
            Diagnostics = ImmutableArray.CreateRange diagnostics
        }
