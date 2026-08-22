namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module SourceReuseTests =
    let private language mode =
        LanguageVersion.normalize (Some mode)
        |> Result.defaultWith failtest

    let private snapshot stableId path text fingerprint =
        SourceSnapshot.Create(StableIdentity.create stableId, path, text, fingerprint)

    let private prepare
        (service: CompilerService)
        mode
        (defines: string array)
        (source: SourceSnapshot)
        =
        service.PrepareSource(language mode, defines, source)

    [<Tests>]
    let tests =
        testList "Issue28.SourceReuse" [
            testCase "reuses a path-neutral lexical core and rebinds source identity"
            <| fun _ ->
                let service = CompilerService()
                let text = "#nowarn \"25\"\nmodule Reuse\nlet value = 42"
                let first = snapshot "source-a" "root-a/Reuse.fs" text "content-a"
                let second = snapshot "source-b" "root-b/Renamed.fs" text "content-b"
                let cold = prepare service "10.0" [||] first
                let warm = prepare service "10.0" [||] first
                let relocated = prepare service "default" [||] second

                Expect.equal cold.Decision "miss" "Cold source preparation"
                Expect.equal warm.Decision "hit" "Identical source preparation"
                Expect.equal relocated.Decision "hit" "Physical-root relocation"

                Expect.isTrue
                    (obj.ReferenceEquals(cold.Document.Core, warm.Document.Core))
                    "Warm core reuse"

                Expect.isTrue
                    (obj.ReferenceEquals(cold.Document.Core, relocated.Document.Core))
                    "Relocated core reuse"

                Expect.equal relocated.Document.StableId second.StableId "Relocated stable identity"

                Expect.equal
                    relocated.Document.LogicalPath
                    second.LogicalPath
                    "Relocated logical path"

                Expect.equal
                    relocated.Document.ContentFingerprint
                    second.ContentFingerprint
                    "Relocated content identity"

                Expect.equal
                    relocated.Document.WarningDirectives[0].LogicalPath
                    second.LogicalPath
                    "Relocated warning path"

            testCase "invalidates lexical cores for content define and language changes"
            <| fun _ ->
                let service = CompilerService()

                let conditional =
                    "#if FEATURE\nmodule Selected\nlet value = 1\n#else\nmodule Selected\nlet value = 2\n#endif"

                let baseline = snapshot "source-a" "Selected.fs" conditional "content-a"

                let edited =
                    snapshot
                        "source-a"
                        "Selected.fs"
                        (conditional
                         + "\n")
                        "content-b"

                let cold = prepare service "10.0" [||] baseline
                let contentChange = prepare service "10.0" [||] edited
                let defineChange = prepare service "10.0" [| "FEATURE" |] baseline

                let normalizedDefineReplay =
                    prepare
                        service
                        "10.0"
                        [|
                            "FEATURE"
                            "FEATURE"
                        |]
                        baseline

                let languageChange = prepare service "9.0" [| "FEATURE" |] baseline
                let languageAliasReplay = prepare service "latest" [| "FEATURE" |] baseline

                Expect.equal cold.Decision "miss" "Cold core"
                Expect.equal contentChange.Decision "miss" "Content edit"
                Expect.equal defineChange.Decision "miss" "Define change"
                Expect.equal normalizedDefineReplay.Decision "hit" "Normalized define replay"
                Expect.equal languageChange.Decision "miss" "Language change"
                Expect.equal languageAliasReplay.Decision "hit" "Language alias replay"

                Expect.stringContains
                    cold.Document.CompatibilityText
                    "let value = 2"
                    "Inactive define branch"

                Expect.stringContains
                    defineChange.Document.CompatibilityText
                    "let value = 1"
                    "Active define branch"

                Expect.notEqual
                    cold.Document.LexicalFingerprint
                    contentChange.Document.LexicalFingerprint
                    "Content fingerprint"

                Expect.notEqual
                    cold.Document.LexicalFingerprint
                    defineChange.Document.LexicalFingerprint
                    "Define fingerprint"

                Expect.notEqual
                    defineChange.Document.LexicalFingerprint
                    languageChange.Document.LexicalFingerprint
                    "Language fingerprint"

            testCase "preserves diagnostics across cold and relocated reuse"
            <| fun _ ->
                let service = CompilerService()
                let text = "#else\nmodule Invalid\n$"

                let cold =
                    prepare
                        service
                        "10.0"
                        [||]
                        (snapshot "source-a" "first/Invalid.fs" text "content-a")

                let relocated =
                    prepare
                        service
                        "10.0"
                        [||]
                        (snapshot "source-b" "second/Invalid.fs" text "content-b")

                let projection result =
                    result.Document.Diagnostics
                    |> Seq.map (fun diagnostic ->
                        diagnostic.Code, diagnostic.Message, diagnostic.Range, diagnostic.Order
                    )
                    |> Seq.toArray

                Expect.equal cold.Decision "miss" "Cold malformed source"
                Expect.equal relocated.Decision "hit" "Relocated malformed source"

                Expect.sequenceEqual
                    (projection cold)
                    (projection relocated)
                    "Diagnostic projection"

                Expect.isGreaterThan cold.Document.Diagnostics.Length 1 "Recoverable diagnostics"
        ]
