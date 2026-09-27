namespace FSharp2.Compiler

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Security.Cryptography
open System.Text

type internal LexicalCore = {
    LanguageCacheIdentity: string
    Defines: ImmutableArray<string>
    SourceMap: SourceMap
    Tokens: ImmutableArray<LexicalToken>
    Trivia: ImmutableArray<LexicalTrivia>
    Directives: ImmutableArray<LexicalDirective>
    WarningDirectives: ImmutableArray<PathNeutralWarningDirective>
    LayoutTokens: ImmutableArray<LayoutToken>
    Diagnostics: ImmutableArray<SourceLexicalDiagnostic>
    SourceChecksum: ImmutableArray<byte>
    LexicalFingerprint: string
    CompatibilityText: string
}

type internal LexicalDocument = {
    StableId: StableIdentity
    LogicalPath: string
    ContentFingerprint: string
    LanguageVersion: LanguageVersionIdentity
    Core: LexicalCore
    WarningDirectives: ImmutableArray<LocalWarningDirective>
} with

    member this.Defines = this.Core.Defines
    member this.SourceMap = this.Core.SourceMap
    member this.Tokens = this.Core.Tokens
    member this.Trivia = this.Core.Trivia
    member this.Directives = this.Core.Directives
    member this.LayoutTokens = this.Core.LayoutTokens
    member this.Diagnostics = this.Core.Diagnostics
    member this.SourceChecksum = this.Core.SourceChecksum
    member this.LexicalFingerprint = this.Core.LexicalFingerprint
    member this.CompatibilityText = this.Core.CompatibilityText

type internal LexicalPreparationResult = {
    Document: LexicalDocument
    Decision: string
}

module internal LexicalPipeline =
    let private normalizeDefines (defines: string seq) =
        if obj.ReferenceEquals(defines, null) then
            nullArg "defines"

        let values =
            defines
            |> Seq.toArray

        values
        |> Array.iter (fun value ->
            if String.IsNullOrWhiteSpace value then
                invalidArg "defines" "defines must contain text."
        )

        values
        |> Array.distinct
        |> Array.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))
        |> ImmutableArray.CreateRange

    let private fingerprint languageCacheIdentity (defines: ImmutableArray<string>) text =
        use hash = IncrementalHash.CreateHash HashAlgorithmName.SHA256

        let append (value: string) =
            let bytes: byte array = Encoding.UTF8.GetBytes value
            let length: byte array = Array.zeroCreate sizeof<int>
            BinaryPrimitives.WriteInt32LittleEndian(length, bytes.Length)
            hash.AppendData length
            hash.AppendData bytes

        append "fsharp2-lexical-core-v1"
        append languageCacheIdentity

        for define in defines do
            append define

        append text

        hash.GetHashAndReset()
        |> Convert.ToHexString

    let private orderedLayoutTokens (tokens: ImmutableArray<LayoutToken>) =
        tokens
        |> Seq.mapi (fun index token -> index, token)
        |> Seq.sortBy (fun (index, token) ->
            token.Range.Start.Offset,
            (if token.Kind = LayoutTokenKind.SourceToken then 1 else 0),
            index
        )
        |> Seq.map snd
        |> ImmutableArray.CreateRange

    let private orderedDiagnostics (diagnostics: ImmutableArray<SourceLexicalDiagnostic>) =
        let effectiveOffset (diagnostic: SourceLexicalDiagnostic) =
            if diagnostic.Code = "FS0058" then
                diagnostics
                |> Seq.filter (fun (candidate: SourceLexicalDiagnostic) ->
                    candidate.Order < diagnostic.Order
                    && candidate.Range.Start.Line = diagnostic.Range.Start.Line
                    && candidate.Range.Start.Offset
                       >= diagnostic.Range.Start.Offset
                    && candidate.Range.Start.Offset < diagnostic.Range.End.Offset
                )
                |> Seq.map _.Range.End.Offset
                |> Seq.fold max diagnostic.Range.Start.Offset
            else
                diagnostic.Range.Start.Offset

        let hasStructuredRecovery =
            diagnostics
            |> Seq.exists (fun diagnostic -> diagnostic.Code = "FS3118")

        diagnostics
        |> Seq.sortBy (fun diagnostic ->
            if hasStructuredRecovery then
                0, diagnostic.Order
            else
                effectiveOffset diagnostic, diagnostic.Order
        )
        |> Seq.mapi (fun index diagnostic -> { diagnostic with Order = int64 index })
        |> ImmutableArray.CreateRange

    let prepareCore language defines source =
        let normalizedDefines = normalizeDefines defines
        let lexed = Lexer.tokenize language source

        let directives =
            Directives.analyze language (Set.ofSeq normalizedDefines) source lexed

        let layout = Layout.apply source directives lexed

        let isActive offset =
            offset
            >= source.Text.Length
            || directives.CompatibilityText[offset] = source.Text[offset]

        {
            LanguageCacheIdentity = language.CacheIdentity
            Defines = normalizedDefines
            SourceMap = directives.SourceMap
            Tokens =
                lexed.Tokens
                |> Seq.filter (fun token -> isActive token.Range.Start.Offset)
                |> ImmutableArray.CreateRange
            Trivia =
                lexed.Trivia
                |> Seq.filter (fun trivia -> isActive trivia.Range.Start.Offset)
                |> ImmutableArray.CreateRange
            Directives = directives.Directives
            WarningDirectives = directives.WarningDirectives
            LayoutTokens = orderedLayoutTokens layout.Tokens
            Diagnostics = orderedDiagnostics layout.Diagnostics
            SourceChecksum =
                source.Text
                |> Encoding.UTF8.GetBytes
                |> SHA256.HashData
                |> ImmutableArray.CreateRange
            LexicalFingerprint = fingerprint language.CacheIdentity normalizedDefines source.Text
            CompatibilityText = directives.CompatibilityText
        }

    let bind language (snapshot: SourceSnapshot) core =
        if
            not (
                String.Equals(
                    language.CacheIdentity,
                    core.LanguageCacheIdentity,
                    StringComparison.Ordinal
                )
            )
        then
            invalidArg "language" "language must match the lexical core cache identity."

        let warningDirectives =
            core.WarningDirectives
            |> Seq.map (fun directive ->
                LocalWarningDirective.Create(
                    directive.Order,
                    directive.Action,
                    directive.Code,
                    snapshot.LogicalPath,
                    Some directive.Range
                )
            )
            |> ImmutableArray.CreateRange

        let boundCore =
            let value =
                if snapshot.ContentFingerprint.StartsWith("sha256:", StringComparison.Ordinal) then
                    snapshot.ContentFingerprint.Substring(7)
                else
                    snapshot.ContentFingerprint

            if
                value.Length = 64
                && value
                   |> Seq.forall Uri.IsHexDigit
            then
                {
                    core with
                        SourceChecksum =
                            value
                            |> Convert.FromHexString
                            |> ImmutableArray.CreateRange
                }
            else
                core

        {
            StableId = snapshot.StableId
            LogicalPath = snapshot.LogicalPath
            ContentFingerprint = snapshot.ContentFingerprint
            LanguageVersion = language
            Core = boundCore
            WarningDirectives = warningDirectives
        }

    let prepare language defines (snapshot: SourceSnapshot) =
        snapshot.Text
        |> SourceText.fromString
        |> prepareCore language defines
        |> bind language snapshot
