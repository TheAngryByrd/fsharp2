namespace FSharp2.Compiler

open System
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type internal DirectiveKind =
    | Line | Light | Indent | Conditional | Warning

type internal LexicalDirective = { Kind: DirectiveKind; Text: string; Range: SourceRange }

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
            diagnostics.Add { Code = code; Message = message; Range = range; Order = diagnosticOrder }
            diagnosticOrder <- diagnosticOrder + 1L

        let blank startOffset endOffset =
            for index = startOffset to endOffset - 1 do
                if output[index] <> '\r' && output[index] <> '\n' then output[index] <- ' '

        let quotedValue (value: string) =
            let first = value.IndexOf '"'
            let last = value.LastIndexOf '"'
            if first >= 0 && last > first then Some(value.Substring(first + 1, last - first - 1)) else None

        for lineIndex = 0 to source.Map.LineStarts.Length - 1 do
            let startOffset = source.Map.LineStarts[lineIndex]
            let endOffset = if lineIndex + 1 < source.Map.LineStarts.Length then source.Map.LineStarts[lineIndex + 1] else source.Text.Length
            let mutable contentEnd = endOffset
            while contentEnd > startOffset && (source.Text[contentEnd - 1] = '\r' || source.Text[contentEnd - 1] = '\n') do contentEnd <- contentEnd - 1
            let lineText = source.Text.Substring(startOffset, contentEnd - startOffset)
            let trimmed = lineText.TrimStart()
            let directiveStart = startOffset + lineText.Length - trimmed.Length
            let directiveRange = { Start = SourceMap.positionAt source.Map directiveStart; End = SourceMap.positionAt source.Map contentEnd }
            let add kind = directives.Add { Kind = kind; Text = trimmed; Range = directiveRange }

            if trimmed.StartsWith("#if ", StringComparison.Ordinal) then
                blank startOffset contentEnd; add DirectiveKind.Conditional
                let condition = defines.Contains(trimmed.Substring(4).Trim())
                frames <- { ParentActive = active; AnyTaken = condition; CurrentActive = active && condition; HasElse = false; Range = directiveRange } :: frames
                active <- active && condition
            elif trimmed.StartsWith("#elif ", StringComparison.Ordinal) then
                blank startOffset contentEnd; add DirectiveKind.Conditional
                match frames with
                | [] -> addDiagnostic "FS0010" "Unexpected '#elif'." directiveRange
                | frame :: tail ->
                    let condition = not frame.AnyTaken && defines.Contains(trimmed.Substring(6).Trim())
                    let next = { frame with AnyTaken = frame.AnyTaken || condition; CurrentActive = frame.ParentActive && condition }
                    frames <- next :: tail; active <- next.CurrentActive
            elif trimmed = "#else" then
                blank startOffset contentEnd; add DirectiveKind.Conditional
                match frames with
                | [] -> addDiagnostic "FS0010" "Unexpected '#else'." directiveRange
                | frame :: _ when frame.HasElse -> addDiagnostic "FS0010" "Duplicate '#else'." directiveRange
                | frame :: tail ->
                    let next = { frame with HasElse = true; CurrentActive = frame.ParentActive && not frame.AnyTaken; AnyTaken = true }
                    frames <- next :: tail; active <- next.CurrentActive
            elif trimmed = "#endif" then
                blank startOffset contentEnd; add DirectiveKind.Conditional
                match frames with
                | [] -> addDiagnostic "FS0010" "Unexpected '#endif'." directiveRange
                | frame :: tail -> frames <- tail; active <- frame.ParentActive
            elif trimmed.StartsWith("#line ", StringComparison.Ordinal) then
                blank startOffset contentEnd; add DirectiveKind.Line
                let parts = trimmed.Substring(6).Trim().Split(' ', 2, StringSplitOptions.RemoveEmptyEntries)
                match parts with
                | [| number; rest |] ->
                    match Int32.TryParse number, quotedValue rest with
                    | (true, logicalLine), Some path -> sourceMap <- SourceMap.addLineMapping sourceMap (lineIndex + 2) logicalLine (Some path)
                    | _ -> addDiagnostic "FS0010" "Invalid '#line' directive." directiveRange
                | _ -> addDiagnostic "FS0010" "Invalid '#line' directive." directiveRange
            elif trimmed.StartsWith("#light", StringComparison.Ordinal) then blank startOffset contentEnd; add DirectiveKind.Light
            elif trimmed.StartsWith("#indent", StringComparison.Ordinal) then blank startOffset contentEnd; add DirectiveKind.Indent
            elif trimmed.StartsWith("#nowarn", StringComparison.Ordinal) || trimmed.StartsWith("#warnon", StringComparison.Ordinal) then
                blank startOffset contentEnd; add DirectiveKind.Warning
                match quotedValue trimmed with
                | None -> addDiagnostic "FS0010" "Invalid warning directive." directiveRange
                | Some code ->
                    let action = if trimmed.StartsWith("#warnon", StringComparison.Ordinal) then LocalWarningDirectiveAction.Enable else LocalWarningDirectiveAction.Disable
                    warnings.Add { Order = warningOrder; Action = action; Code = code; LogicalPath = None; Range = directiveRange }
                    warningOrder <- warningOrder + 1L
                    if action = LocalWarningDirectiveAction.Enable && not language.SupportsScopedWarningDirectives then addDiagnostic "FS3350" "Feature 'warning directives' is not available in this language version." directiveRange
            elif not active then blank startOffset contentEnd

        for frame in frames |> List.rev do addDiagnostic "FS0010" "Incomplete conditional directive." frame.Range

        { CompatibilityText = String output; SourceMap = sourceMap; Directives = ImmutableArray.CreateRange directives; WarningDirectives = ImmutableArray.CreateRange warnings; Diagnostics = ImmutableArray.CreateRange diagnostics }
