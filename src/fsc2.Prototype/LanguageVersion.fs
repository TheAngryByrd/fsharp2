namespace FSharp2.Compiler

open System
open System.Collections.Immutable

type internal LanguageVersionIdentity = {
    RequestedMode: string
    CanonicalMode: string
    CacheIdentity: string
    FeatureLevel: int
    IsPreview: bool
    SupportsScopedWarningDirectives: bool
}

module internal LanguageVersion =
    type private Entry = {
        Mode: string
        CanonicalMode: string
        CacheIdentity: string
        FeatureLevel: int
        IsPreview: bool
    }

    let private entries = [|
        { Mode = "preview"; CanonicalMode = "preview"; CacheIdentity = "preview"; FeatureLevel = 101; IsPreview = true }
        { Mode = "default"; CanonicalMode = "10.0"; CacheIdentity = "10.0"; FeatureLevel = 100; IsPreview = false }
        { Mode = "latest"; CanonicalMode = "10.0"; CacheIdentity = "10.0"; FeatureLevel = 100; IsPreview = false }
        { Mode = "latestmajor"; CanonicalMode = "10.0"; CacheIdentity = "10.0"; FeatureLevel = 100; IsPreview = false }
        { Mode = "4.6"; CanonicalMode = "4.6"; CacheIdentity = "4.6"; FeatureLevel = 46; IsPreview = false }
        { Mode = "4.7"; CanonicalMode = "4.7"; CacheIdentity = "4.7"; FeatureLevel = 47; IsPreview = false }
        { Mode = "5.0"; CanonicalMode = "5.0"; CacheIdentity = "5.0"; FeatureLevel = 50; IsPreview = false }
        { Mode = "6.0"; CanonicalMode = "6.0"; CacheIdentity = "6.0"; FeatureLevel = 60; IsPreview = false }
        { Mode = "7.0"; CanonicalMode = "7.0"; CacheIdentity = "7.0"; FeatureLevel = 70; IsPreview = false }
        { Mode = "8.0"; CanonicalMode = "8.0"; CacheIdentity = "8.0"; FeatureLevel = 80; IsPreview = false }
        { Mode = "9.0"; CanonicalMode = "9.0"; CacheIdentity = "9.0"; FeatureLevel = 90; IsPreview = false }
        { Mode = "10.0"; CanonicalMode = "10.0"; CacheIdentity = "10.0"; FeatureLevel = 100; IsPreview = false }
    |]

    let supportedModes =
        entries
        |> Seq.map _.Mode
        |> ImmutableArray.CreateRange

    let normalize requested =
        let mode =
            requested
            |> Option.defaultValue "default"
            |> _.Trim()
            |> _.ToLowerInvariant()

        entries
        |> Array.tryFind (fun entry -> String.Equals(entry.Mode, mode, StringComparison.Ordinal))
        |> function
            | None -> Error $"Language version '{mode}' is not supported."
            | Some entry ->
                Ok {
                    RequestedMode = entry.Mode
                    CanonicalMode = entry.CanonicalMode
                    CacheIdentity = entry.CacheIdentity
                    FeatureLevel = entry.FeatureLevel
                    IsPreview = entry.IsPreview
                    SupportsScopedWarningDirectives = entry.FeatureLevel >= 100
                }
