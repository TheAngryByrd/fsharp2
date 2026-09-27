namespace fsharp2.Tests

open Expecto
open FSharp2.Compiler

module LanguageVersionTests =
    [<Tests>]
    let tests =
        testList "Issue28.LanguageVersion" [
            testCase "selects all pinned SDK modes from one table"
            <| fun _ ->
                let expected = [|
                    "preview"
                    "default"
                    "latest"
                    "latestmajor"
                    "4.6"
                    "4.7"
                    "5.0"
                    "6.0"
                    "7.0"
                    "8.0"
                    "9.0"
                    "10.0"
                |]

                Expect.sequenceEqual
                    LanguageVersion.supportedModes
                    expected
                    "Mode order matches the pinned Oracle"

                for mode in expected do
                    match LanguageVersion.normalize (Some mode) with
                    | Error message -> failtestf "Mode %s was rejected: %s" mode message
                    | Ok identity ->
                        Expect.equal identity.RequestedMode mode "Requested mode is preserved"

            testCase "normalizes released aliases and keeps preview distinct"
            <| fun _ ->
                let normalize mode =
                    LanguageVersion.normalize (Some mode)
                    |> Result.defaultWith failtest

                let defaultMode = normalize "default"
                let latest = normalize "latest"
                let latestMajor = normalize "latestmajor"
                let released = normalize "10.0"
                let preview = normalize "preview"

                Expect.equal defaultMode.CanonicalMode "10.0" "Default selects the released mode"

                Expect.equal
                    latest.CacheIdentity
                    released.CacheIdentity
                    "Latest shares released feature identity"

                Expect.equal
                    latestMajor.CacheIdentity
                    released.CacheIdentity
                    "Latest major shares released feature identity"

                Expect.notEqual
                    preview.CacheIdentity
                    released.CacheIdentity
                    "Preview remains a distinct cache identity"

            testCase "gates scoped warnon at 10.0 and rejects unknown values"
            <| fun _ ->
                let mode9 =
                    LanguageVersion.normalize (Some "9.0")
                    |> Result.defaultWith failtest

                let mode10 =
                    LanguageVersion.normalize (Some "10.0")
                    |> Result.defaultWith failtest

                Expect.isFalse
                    mode9.SupportsScopedWarningDirectives
                    "9.0 reports the feature diagnostic"

                Expect.isTrue mode10.SupportsScopedWarningDirectives "10.0 enables scoped warnon"

                match LanguageVersion.normalize (Some "11.0") with
                | Ok _ -> failtest "Unknown mode was accepted"
                | Error message ->
                    Expect.stringContains message "11.0" "The unsupported input is structured"
        ]
