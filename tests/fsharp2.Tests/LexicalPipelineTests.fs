namespace fsharp2.Tests

open System
open System.Collections.Generic
open Expecto
open FSharp2.Compiler

module LexicalPipelineTests =
    let private language mode =
        LanguageVersion.normalize (Some mode)
        |> Result.defaultWith failtest

    let private snapshot stableId logicalPath text fingerprint =
        SourceSnapshot.Create(StableIdentity.create stableId, logicalPath, text, fingerprint)

    [<Tests>]
    let tests =
        testList "Issue28.LexicalPipeline" [
            testCase "prepares the complete immutable source handoff"
            <| fun _ ->
                let text =
                    "#if ACTIVE\r\n#line 20 \"mapped.fs\"\r\n#nowarn \"25\"\r\nlet outer =\r\n    let value = 1\r\n    value\r\n#endif"

                let defines = [|
                    "ZETA"
                    "ACTIVE"
                    "ACTIVE"
                |]

                let document =
                    snapshot "source-a" "src/First.fs" text "content-a"
                    |> LexicalPipeline.prepare (language "latest") defines

                defines[0] <- "CHANGED"

                Expect.equal
                    document.StableId
                    (StableIdentity.create "source-a")
                    "Stable source identity"

                Expect.equal document.LogicalPath "src/First.fs" "Logical source path"
                Expect.equal document.ContentFingerprint "content-a" "Content fingerprint"

                Expect.equal
                    document.LanguageVersion.CacheIdentity
                    "10.0"
                    "Normalized language identity"

                Expect.sequenceEqual
                    document.Defines
                    [
                        "ACTIVE"
                        "ZETA"
                    ]
                    "Normalized defines"

                Expect.equal document.SourceMap.LineStarts.Length 7 "Physical line map"
                Expect.equal document.SourceMap.Newlines.Length 7 "Newline map"
                Expect.isGreaterThan document.Tokens.Length 1 "Lexical tokens"
                Expect.isGreaterThan document.Trivia.Length 1 "Lexical trivia"
                Expect.equal document.Directives.Length 4 "Source directives"
                Expect.isGreaterThan document.LayoutTokens.Length 1 "Layout tokens"
                Expect.isEmpty document.Diagnostics "Valid source diagnostics"
                Expect.equal document.WarningDirectives.Length 1 "Bound warning directives"

                Expect.equal
                    document.WarningDirectives[0].LogicalPath
                    "src/First.fs"
                    "Bound warning path"

                Expect.isNone document.Core.WarningDirectives[0].LogicalPath "Cached warning path"

                Expect.equal
                    document.CompatibilityText.Length
                    text.Length
                    "Compatibility text length"

                Expect.equal
                    (document.CompatibilityText
                     |> Seq.filter ((=) '\n')
                     |> Seq.length)
                    6
                    "Compatibility newlines"

                Expect.equal document.LexicalFingerprint.Length 64 "SHA-256 lexical fingerprint"

                let mutableTokens = document.Tokens :> IList<LexicalToken>

                Expect.throwsT<NotSupportedException>
                    (fun () -> mutableTokens[0] <- document.Tokens[0])
                    "Token collection rejects mutation"

            testCase "keeps cached cores path-neutral and rebinds request identity"
            <| fun _ ->
                let text = "#nowarn \"25\"\nlet value = 1"
                let firstSnapshot = snapshot "source-a" "root-a/Program.fs" text "fingerprint-a"
                let secondSnapshot = snapshot "source-b" "root-b/Renamed.fs" text "fingerprint-b"
                let first = LexicalPipeline.prepare (language "10.0") [||] firstSnapshot
                let second = LexicalPipeline.prepare (language "default") [||] secondSnapshot
                let rebound = LexicalPipeline.bind (language "default") secondSnapshot first.Core

                Expect.equal
                    first.LexicalFingerprint
                    second.LexicalFingerprint
                    "Path-neutral fingerprint"

                Expect.sequenceEqual
                    (first.Tokens
                     |> Seq.map (fun token -> token.Kind, token.Text, token.Range))
                    (second.Tokens
                     |> Seq.map (fun token -> token.Kind, token.Text, token.Range))
                    "Path-neutral token projection"

                Expect.isNone
                    first.Core.WarningDirectives[0].LogicalPath
                    "First cached warning path"

                Expect.isNone
                    second.Core.WarningDirectives[0].LogicalPath
                    "Second cached warning path"

                Expect.isTrue
                    (obj.ReferenceEquals(first.Core, rebound.Core))
                    "Rebinding reuses the core"

                Expect.equal rebound.StableId secondSnapshot.StableId "Rebound stable identity"
                Expect.equal rebound.LogicalPath secondSnapshot.LogicalPath "Rebound logical path"

                Expect.equal
                    rebound.ContentFingerprint
                    secondSnapshot.ContentFingerprint
                    "Rebound content identity"

                Expect.equal
                    rebound.WarningDirectives[0].LogicalPath
                    secondSnapshot.LogicalPath
                    "Rebound warning path"

            testCase "orders layout and source tokens by UTF-16 offset"
            <| fun _ ->
                let document =
                    snapshot
                        "source-a"
                        "Program.fs"
                        "let outer =\n    let inner = 1\n    inner"
                        "content-a"
                    |> LexicalPipeline.prepare (language "10.0") [||]

                let offsets =
                    document.LayoutTokens
                    |> Seq.map _.Range.Start.Offset
                    |> Seq.toArray

                Expect.sequenceEqual
                    offsets
                    (offsets
                     |> Array.sort)
                    "Layout handoff source order"

                let beginIndex =
                    document.LayoutTokens
                    |> Seq.findIndex (fun token -> token.Kind = LayoutTokenKind.BeginBlock)

                Expect.equal
                    document.LayoutTokens[beginIndex
                                          + 1]
                        .Token.Value.Text
                    "let"
                    "Block event precedes its first token"

            testCase "merges recoverable diagnostics in source order"
            <| fun _ ->
                let document =
                    snapshot
                        "source-a"
                        "Program.fs"
                        "#else\nlet outer =\n    let inner = 1\n  $"
                        "content-a"
                    |> LexicalPipeline.prepare (language "10.0") [||]

                Expect.sequenceEqual
                    (document.Diagnostics
                     |> Seq.map _.Order)
                    [
                        0L
                        1L
                        2L
                    ]
                    "Diagnostic order values"

                Expect.sequenceEqual
                    (document.Diagnostics
                     |> Seq.map (fun diagnostic -> diagnostic.Range.Start.Line))
                    [
                        1
                        4
                        4
                    ]
                    "Diagnostic source order"

                Expect.equal document.Diagnostics[2].Code "FS0058" "Layout diagnostic remains last"
        ]
