namespace FSharp2.Compiler

open System
open System.Collections.Immutable

module private ContractValidation =
    let value name (value: string) =
        if isNull value then
            nullArg name

        value

    let text name (value: string) =
        if String.IsNullOrWhiteSpace(value) then
            invalidArg name $"{name} must contain text."

        value

    let array name (values: 'T array) =
        if isNull values then
            nullArg name

        ImmutableArray.CreateRange values

    let references name (values: 'T array) =
        let copied = array name values

        copied
        |> Seq.iter (fun value ->
            if obj.ReferenceEquals(value, null) then
                nullArg name
        )

        copied

    let reference name value =
        if obj.ReferenceEquals(value, null) then
            nullArg name

        value

    let immutableArray name (values: ImmutableArray<'T>) =
        if values.IsDefault then
            invalidArg name $"{name} must be initialized."

        values

    let stringArray name (values: string array) =
        let copied = array name values

        copied
        |> Seq.iter (
            text name
            >> ignore
        )

        copied

[<RequireQualifiedAccess>]
module CompilerContract =
    [<Literal>]
    let Version = 2

[<Struct; StructuralEquality; StructuralComparison>]
type StableIdentity =
    private
    | StableIdentity of string

    member this.Value =
        let (StableIdentity value) = this
        value

    static member create(value: string) =
        StableIdentity(ContractValidation.text "value" value)

    override this.ToString() = this.Value

type CompilationAssemblyIdentity = {
    StableId: StableIdentity
    Name: string
} with

    static member Create(stableId, name) =
        let validatedName = ContractValidation.text "name" name

        if
            validatedName.IndexOfAny(
                [|
                    '/'
                    '\\'
                |]
            )
            >= 0
        then
            invalidArg "name" "name must not contain a path separator."

        {
            StableId = stableId
            Name = validatedName
        }

    override _.ToString() = "CompilationAssemblyIdentity"

[<Struct>]
type SourcePosition = {
    Offset: int
    Line: int
    Column: int
} with

    override _.ToString() = "SourcePosition"

[<Struct>]
type SourceRange = {
    Start: SourcePosition
    End: SourcePosition
} with

    override _.ToString() = "SourceRange"

type SourceSnapshot = {
    StableId: StableIdentity
    LogicalPath: string
    Text: string
    ContentFingerprint: string
} with

    static member Create(stableId, logicalPath, text, contentFingerprint) = {
        StableId = stableId
        LogicalPath = ContractValidation.text "logicalPath" logicalPath
        Text = ContractValidation.value "text" text
        ContentFingerprint = ContractValidation.text "contentFingerprint" contentFingerprint
    }

    override _.ToString() = "SourceSnapshot"

type TargetReferenceSnapshot = {
    StableId: StableIdentity
    LogicalPath: string
    PeImage: ImmutableArray<byte>
    ContentFingerprint: string
} with

    static member Create(stableId, logicalPath, peImage, contentFingerprint) = {
        StableId = stableId
        LogicalPath = ContractValidation.text "logicalPath" logicalPath
        PeImage = ContractValidation.array "peImage" peImage
        ContentFingerprint = ContractValidation.text "contentFingerprint" contentFingerprint
    }

    override _.ToString() = "TargetReferenceSnapshot"

[<RequireQualifiedAccess>]
type OptimizationMode =
    | Disabled
    | Enabled

[<RequireQualifiedAccess>]
type DebugFormat =
    | None
    | Portable

[<RequireQualifiedAccess>]
type SigningMode =
    | Unsigned
    | DelaySign
    | PublicSign
    | FullSign

[<RequireQualifiedAccess>]
type ResourceVisibility =
    | Public
    | Private

type SemanticOptions = {
    Defines: ImmutableArray<string>
    LanguageVersion: string option
    Optimization: OptimizationMode
    CheckNulls: bool
    NoFramework: bool
    TargetProfile: string option
} with

    static member Create
        (defines, languageVersion, optimization, checkNulls, noFramework, targetProfile)
        =
        let optimization = ContractValidation.reference "optimization" optimization

        languageVersion
        |> Option.iter (
            ContractValidation.text "languageVersion"
            >> ignore
        )

        targetProfile
        |> Option.iter (
            ContractValidation.text "targetProfile"
            >> ignore
        )

        {
            Defines = ContractValidation.stringArray "defines" defines
            LanguageVersion = languageVersion
            Optimization = optimization
            CheckNulls = checkNulls
            NoFramework = noFramework
            TargetProfile = targetProfile
        }

    override _.ToString() = "SemanticOptions"

[<RequireQualifiedAccess>]
type LocalWarningDirectiveAction =
    | Enable
    | Disable

type LocalWarningDirective = {
    Order: int64
    Action: LocalWarningDirectiveAction
    Code: string
    LogicalPath: string
    Range: SourceRange option
} with

    static member Create(order, action, code, logicalPath, range) =
        if order < 0L then
            invalidArg "order" "order must be non-negative."

        {
            Order = order
            Action = ContractValidation.reference "action" action
            Code = ContractValidation.text "code" code
            LogicalPath = ContractValidation.text "logicalPath" logicalPath
            Range = range
        }

    override _.ToString() = "LocalWarningDirective"

[<RequireQualifiedAccess>]
type DiagnosticStyle =
    | Default
    | VisualStudio
    | Gcc
    | Emacs
    | Rich
    | Flat

[<RequireQualifiedAccess>]
type ConsoleColorMode =
    | Automatic
    | Enabled
    | Disabled

type DiagnosticOptions = {
    WarningLevel: int option
    DisabledWarnings: ImmutableArray<string>
    EnabledWarnings: ImmutableArray<string>
    TreatWarningsAsErrors: bool
    WarningsAsErrors: ImmutableArray<string>
    WarningsNotAsErrors: ImmutableArray<string>
    MaximumErrors: int option
    AbortOnError: bool
    PreferredUICulture: string option
    LocalWarningDirectives: ImmutableArray<LocalWarningDirective>
    FullPaths: bool
    FlatErrors: bool
    Utf8Output: bool
    DiagnosticStyle: DiagnosticStyle
    ConsoleColorMode: ConsoleColorMode
    LCID: int option
    PreferredUILanguage: string option
    TestParserErrorRecovery: bool
    StandardOutputRedirected: bool
    StandardErrorRedirected: bool
} with

    static member Create(warningLevel, disabledWarnings, treatWarningsAsErrors, warningsAsErrors) =
        DiagnosticOptions.Create(
            warningLevel,
            disabledWarnings,
            [||],
            treatWarningsAsErrors,
            warningsAsErrors,
            [||],
            None,
            false,
            None,
            [||],
            false,
            false,
            false,
            DiagnosticStyle.Default,
            ConsoleColorMode.Automatic,
            None,
            None,
            false,
            false,
            false
        )

    static member Create
        (
            warningLevel,
            disabledWarnings,
            enabledWarnings,
            treatWarningsAsErrors,
            warningsAsErrors,
            warningsNotAsErrors,
            maximumErrors,
            abortOnError,
            preferredUICulture,
            localWarningDirectives,
            fullPaths,
            flatErrors,
            utf8Output,
            diagnosticStyle,
            consoleColorMode,
            lcid,
            preferredUILanguage,
            testParserErrorRecovery,
            standardOutputRedirected,
            standardErrorRedirected
        ) =
        maximumErrors
        |> Option.iter (fun value ->
            if value < 0 then
                invalidArg "maximumErrors" "maximumErrors must be non-negative."
        )

        lcid
        |> Option.iter (fun value ->
            if value < 0 then
                invalidArg "lcid" "lcid must be non-negative."
        )

        preferredUICulture
        |> Option.iter (
            ContractValidation.text "preferredUICulture"
            >> ignore
        )

        preferredUILanguage
        |> Option.iter (
            ContractValidation.text "preferredUILanguage"
            >> ignore
        )

        {
            WarningLevel = warningLevel
            DisabledWarnings = ContractValidation.stringArray "disabledWarnings" disabledWarnings
            EnabledWarnings = ContractValidation.stringArray "enabledWarnings" enabledWarnings
            TreatWarningsAsErrors = treatWarningsAsErrors
            WarningsAsErrors = ContractValidation.stringArray "warningsAsErrors" warningsAsErrors
            WarningsNotAsErrors =
                ContractValidation.stringArray "warningsNotAsErrors" warningsNotAsErrors
            MaximumErrors = maximumErrors
            AbortOnError = abortOnError
            PreferredUICulture = preferredUICulture
            LocalWarningDirectives =
                ContractValidation.references "localWarningDirectives" localWarningDirectives
            FullPaths = fullPaths
            FlatErrors = flatErrors
            Utf8Output = utf8Output
            DiagnosticStyle = ContractValidation.reference "diagnosticStyle" diagnosticStyle
            ConsoleColorMode = ContractValidation.reference "consoleColorMode" consoleColorMode
            LCID = lcid
            PreferredUILanguage = preferredUILanguage
            TestParserErrorRecovery = testParserErrorRecovery
            StandardOutputRedirected = standardOutputRedirected
            StandardErrorRedirected = standardErrorRedirected
        }

    override _.ToString() = "DiagnosticOptions"

[<RequireQualifiedAccess>]
type CompilationTarget =
    | Library
    | Executable

type EmissionOptions = {
    Target: CompilationTarget
    Deterministic: bool
    HighEntropyVirtualAddress: bool
    DebugFormat: DebugFormat
    EmbeddedSourceIdentities: ImmutableArray<StableIdentity>
    DebugDocumentPaths: ImmutableArray<string>
    SourceLinkJson: ImmutableArray<byte>
} with

    static member Create
        (
            deterministic,
            highEntropyVirtualAddress,
            debugFormat,
            embeddedSourceIdentities,
            debugDocumentPaths,
            sourceLinkJson
        ) =
        EmissionOptions.Create(
            CompilationTarget.Library,
            deterministic,
            highEntropyVirtualAddress,
            debugFormat,
            embeddedSourceIdentities,
            debugDocumentPaths,
            sourceLinkJson
        )

    static member Create
        (
            target,
            deterministic,
            highEntropyVirtualAddress,
            debugFormat,
            embeddedSourceIdentities,
            debugDocumentPaths,
            sourceLinkJson
        ) =
        let target = ContractValidation.reference "target" target
        let debugFormat = ContractValidation.reference "debugFormat" debugFormat

        {
            Target = target
            Deterministic = deterministic
            HighEntropyVirtualAddress = highEntropyVirtualAddress
            DebugFormat = debugFormat
            EmbeddedSourceIdentities =
                ContractValidation.array "embeddedSourceIdentities" embeddedSourceIdentities
            DebugDocumentPaths =
                ContractValidation.stringArray "debugDocumentPaths" debugDocumentPaths
            SourceLinkJson = ContractValidation.array "sourceLinkJson" sourceLinkJson
        }

    override _.ToString() = "EmissionOptions"

type SigningOptions = {
    Mode: SigningMode
    Key: ImmutableArray<byte>
} with

    static member Create(mode, key) =
        let mode = ContractValidation.reference "mode" mode

        {
            Mode = mode
            Key = ContractValidation.array "key" key
        }

    override _.ToString() = "SigningOptions"

type ManagedResourceSnapshot = {
    StableId: StableIdentity
    LogicalName: string
    Visibility: ResourceVisibility
    Content: ImmutableArray<byte>
    ContentFingerprint: string
} with

    static member Create(stableId, logicalName, visibility, content, contentFingerprint) =
        let visibility = ContractValidation.reference "visibility" visibility

        {
            StableId = stableId
            LogicalName = ContractValidation.text "logicalName" logicalName
            Visibility = visibility
            Content = ContractValidation.array "content" content
            ContentFingerprint = ContractValidation.text "contentFingerprint" contentFingerprint
        }

    override _.ToString() = "ManagedResourceSnapshot"

type NativeResourceSnapshot = {
    StableId: StableIdentity
    Content: ImmutableArray<byte>
    ContentFingerprint: string
} with

    static member Create(stableId, content, contentFingerprint) = {
        StableId = stableId
        Content = ContractValidation.array "content" content
        ContentFingerprint = ContractValidation.text "contentFingerprint" contentFingerprint
    }

    override _.ToString() = "NativeResourceSnapshot"

type ResourceInputs = {
    Managed: ImmutableArray<ManagedResourceSnapshot>
    Native: ImmutableArray<NativeResourceSnapshot>
} with

    static member Create(managed, native) = {
        Managed = ContractValidation.references "managed" managed
        Native = ContractValidation.references "native" native
    }

    override _.ToString() = "ResourceInputs"

[<RequireQualifiedAccess>]
type RequestedArtifact =
    | ImplementationAssembly
    | PortablePdb
    | ReferenceAssembly
    | Documentation
    | Custom of StableIdentity

[<RequireQualifiedAccess>]
type CompilationPhase =
    | Source
    | Syntax
    | ResolvedSymbols
    | TypedDeclarations
    | LoweredCode
    | OptimizedCode
    | SymbolicEmission
    | FinalLinking

[<RequireQualifiedAccess>]
type PhaseStatus =
    | NotStarted
    | Completed
    | Failed
    | Skipped
    | Cancelled
    | Unsupported

[<RequireQualifiedAccess>]
type DiagnosticSeverity =
    | Hidden
    | Information
    | Warning
    | Error

[<RequireQualifiedAccess>]
type DiagnosticStream =
    | StandardOutput
    | StandardError

[<RequireQualifiedAccess>]
type DiagnosticStage =
    | CommandLine
    | Compilation of CompilationPhase
    | Publication
    | Host

[<RequireQualifiedAccess>]
type DiagnosticDisposition =
    | Emitted
    | Suppressed

[<RequireQualifiedAccess>]
type DiagnosticSuppression =
    | WarningLevel
    | GlobalNowarn
    | LocalNowarn
    | OffByDefault
    | LanguageFeature
    | MaximumErrors
    | AbortBoundary

type PhaseResult = {
    Phase: CompilationPhase
    Status: PhaseStatus
    InputFingerprint: string option
    OutputFingerprint: string option
    TraceValues: ImmutableArray<string>
} with

    override _.ToString() = "PhaseResult"

type DiagnosticRelatedInformation = {
    Message: string
    LogicalPath: string option
    Range: SourceRange option
} with

    static member Create(message, logicalPath, range) =
        logicalPath
        |> Option.iter (
            ContractValidation.text "logicalPath"
            >> ignore
        )

        {
            Message = ContractValidation.value "message" message
            LogicalPath = logicalPath
            Range = range
        }

    override _.ToString() = "DiagnosticRelatedInformation"

type CompilationDiagnostic = {
    Occurrence: int64
    Code: string
    NumericCode: int
    Subcategory: string option
    Stage: DiagnosticStage
    OriginalSeverity: DiagnosticSeverity
    EffectiveSeverity: DiagnosticSeverity
    Disposition: DiagnosticDisposition
    Suppression: DiagnosticSuppression option
    Message: string
    LogicalPath: string option
    Range: SourceRange option
    RelatedInformation: ImmutableArray<DiagnosticRelatedInformation>
    Suggestions: ImmutableArray<string>
    Stream: DiagnosticStream option
} with

    static member Create
        (
            occurrence,
            code,
            numericCode,
            subcategory,
            stage,
            originalSeverity,
            effectiveSeverity,
            disposition,
            suppression,
            message,
            logicalPath,
            range,
            relatedInformation,
            suggestions,
            stream
        ) =
        if occurrence < 0L then
            invalidArg "occurrence" "occurrence must be non-negative."

        if numericCode < 0 then
            invalidArg "numericCode" "numericCode must be non-negative."

        subcategory
        |> Option.iter (
            ContractValidation.text "subcategory"
            >> ignore
        )

        logicalPath
        |> Option.iter (
            ContractValidation.text "logicalPath"
            >> ignore
        )

        let stage = ContractValidation.reference "stage" stage

        let originalSeverity =
            ContractValidation.reference "originalSeverity" originalSeverity

        let effectiveSeverity =
            ContractValidation.reference "effectiveSeverity" effectiveSeverity

        let disposition = ContractValidation.reference "disposition" disposition
        let suppression: DiagnosticSuppression option = suppression
        let stream: DiagnosticStream option = stream

        match disposition with
        | DiagnosticDisposition.Emitted ->
            if suppression.IsSome then
                invalidArg "suppression" "An emitted diagnostic cannot have a suppression reason."

            if stream.IsNone then
                invalidArg "stream" "An emitted diagnostic must select an output stream."
        | DiagnosticDisposition.Suppressed ->
            if
                effectiveSeverity
                <> DiagnosticSeverity.Hidden
            then
                invalidArg
                    "effectiveSeverity"
                    "A suppressed diagnostic must have Hidden effective severity."

            if suppression.IsNone then
                invalidArg "suppression" "A suppressed diagnostic must have a suppression reason."

            if stream.IsSome then
                invalidArg "stream" "A suppressed diagnostic cannot select an output stream."

        {
            Occurrence = occurrence
            Code = ContractValidation.text "code" code
            NumericCode = numericCode
            Subcategory = subcategory
            Stage = stage
            OriginalSeverity = originalSeverity
            EffectiveSeverity = effectiveSeverity
            Disposition = disposition
            Suppression = suppression
            Message = ContractValidation.value "message" message
            LogicalPath = logicalPath
            Range = range
            RelatedInformation =
                ContractValidation.references "relatedInformation" relatedInformation
            Suggestions = ContractValidation.stringArray "suggestions" suggestions
            Stream = stream
        }

    override _.ToString() = "CompilationDiagnostic"

type UnsupportedEnvelopeFailure = {
    Code: string
    Message: string
    StoppingPhase: CompilationPhase
    UnsupportedValueIdentity: string
} with

    override _.ToString() = "UnsupportedEnvelopeFailure"

type CompilationCancellation = {
    RequestIdentity: StableIdentity
    ObservedPhase: CompilationPhase
} with

    override _.ToString() = "CompilationCancellation"

type CompilationArtifact = {
    Kind: RequestedArtifact
    StableId: StableIdentity
    Fingerprint: string
    Bytes: ImmutableArray<byte>
} with

    override _.ToString() = "CompilationArtifact"

[<RequireQualifiedAccess>]
type CompilationOutcome =
    | Succeeded
    | Failed
    | Unsupported of UnsupportedEnvelopeFailure
    | Cancelled of CompilationCancellation

type CompilationResult = {
    Outcome: CompilationOutcome
    Diagnostics: ImmutableArray<CompilationDiagnostic>
    Artifacts: ImmutableArray<CompilationArtifact>
    Fingerprints: ImmutableArray<string>
    PhaseResults: ImmutableArray<PhaseResult>
    Traces: ImmutableArray<string>
} with

    override _.ToString() = "CompilationResult"

type CompilationRequest = {
    ContractVersion: int
    RequestIdentity: StableIdentity
    AssemblyIdentity: CompilationAssemblyIdentity
    Sources: ImmutableArray<SourceSnapshot>
    TargetReferences: ImmutableArray<TargetReferenceSnapshot>
    SemanticOptions: SemanticOptions
    DiagnosticOptions: DiagnosticOptions
    EmissionOptions: EmissionOptions
    SigningOptions: SigningOptions
    Resources: ResourceInputs
    RequestedArtifacts: ImmutableArray<RequestedArtifact>
} with

    static member Create
        (
            contractVersion: int,
            requestIdentity: StableIdentity,
            assemblyIdentity: CompilationAssemblyIdentity,
            sources: SourceSnapshot array,
            targetReferences: TargetReferenceSnapshot array,
            semanticOptions: SemanticOptions,
            diagnosticOptions: DiagnosticOptions,
            emissionOptions: EmissionOptions,
            signingOptions: SigningOptions,
            resources: ResourceInputs,
            requestedArtifacts: RequestedArtifact array
        ) =
        if
            contractVersion
            <> CompilerContract.Version
        then
            invalidArg
                "contractVersion"
                $"Unsupported compiler contract version '{contractVersion}'."

        let assemblyIdentity =
            ContractValidation.reference "assemblyIdentity" assemblyIdentity

        let semanticOptions = ContractValidation.reference "semanticOptions" semanticOptions

        let diagnosticOptions =
            ContractValidation.reference "diagnosticOptions" diagnosticOptions

        let emissionOptions = ContractValidation.reference "emissionOptions" emissionOptions
        let signingOptions = ContractValidation.reference "signingOptions" signingOptions
        let resources = ContractValidation.reference "resources" resources
        let copiedSources = ContractValidation.references "sources" sources

        let copiedTargetReferences =
            ContractValidation.references "targetReferences" targetReferences

        let copiedRequestedArtifacts =
            ContractValidation.references "requestedArtifacts" requestedArtifacts

        let validateIdentity name (identity: StableIdentity) =
            ContractValidation.text name identity.Value
            |> ignore

        validateIdentity "requestIdentity" requestIdentity
        validateIdentity "assemblyIdentity.StableId" assemblyIdentity.StableId

        let assemblyName =
            ContractValidation.text "assemblyIdentity.Name" assemblyIdentity.Name

        if
            assemblyName.IndexOfAny(
                [|
                    '/'
                    '\\'
                |]
            )
            >= 0
        then
            invalidArg
                "assemblyIdentity.Name"
                "assemblyIdentity.Name must not contain a path separator."

        for source in copiedSources do
            validateIdentity "sources.StableId" source.StableId

            ContractValidation.text "sources.LogicalPath" source.LogicalPath
            |> ignore

            ContractValidation.value "sources.Text" source.Text
            |> ignore

            ContractValidation.text "sources.ContentFingerprint" source.ContentFingerprint
            |> ignore

        for reference in copiedTargetReferences do
            validateIdentity "targetReferences.StableId" reference.StableId

            ContractValidation.text "targetReferences.LogicalPath" reference.LogicalPath
            |> ignore

            ContractValidation.immutableArray "targetReferences.PeImage" reference.PeImage
            |> ignore

            ContractValidation.text
                "targetReferences.ContentFingerprint"
                reference.ContentFingerprint
            |> ignore

        ContractValidation.immutableArray "semanticOptions.Defines" semanticOptions.Defines
        |> Seq.iter (
            ContractValidation.text "semanticOptions.Defines"
            >> ignore
        )

        semanticOptions.LanguageVersion
        |> Option.iter (
            ContractValidation.text "semanticOptions.LanguageVersion"
            >> ignore
        )

        semanticOptions.TargetProfile
        |> Option.iter (
            ContractValidation.text "semanticOptions.TargetProfile"
            >> ignore
        )

        ContractValidation.reference "semanticOptions.Optimization" semanticOptions.Optimization
        |> ignore

        ContractValidation.immutableArray
            "diagnosticOptions.DisabledWarnings"
            diagnosticOptions.DisabledWarnings
        |> Seq.iter (
            ContractValidation.text "diagnosticOptions.DisabledWarnings"
            >> ignore
        )

        ContractValidation.immutableArray
            "diagnosticOptions.EnabledWarnings"
            diagnosticOptions.EnabledWarnings
        |> Seq.iter (
            ContractValidation.text "diagnosticOptions.EnabledWarnings"
            >> ignore
        )

        ContractValidation.immutableArray
            "diagnosticOptions.WarningsAsErrors"
            diagnosticOptions.WarningsAsErrors
        |> Seq.iter (
            ContractValidation.text "diagnosticOptions.WarningsAsErrors"
            >> ignore
        )

        ContractValidation.immutableArray
            "diagnosticOptions.WarningsNotAsErrors"
            diagnosticOptions.WarningsNotAsErrors
        |> Seq.iter (
            ContractValidation.text "diagnosticOptions.WarningsNotAsErrors"
            >> ignore
        )

        ContractValidation.immutableArray
            "diagnosticOptions.LocalWarningDirectives"
            diagnosticOptions.LocalWarningDirectives
        |> Seq.iter (fun directive ->
            ContractValidation.reference "diagnosticOptions.LocalWarningDirectives" directive
            |> ignore

            if directive.Order < 0L then
                invalidArg
                    "diagnosticOptions.LocalWarningDirectives"
                    "A local warning directive order must be non-negative."

            ContractValidation.reference
                "diagnosticOptions.LocalWarningDirectives.Action"
                directive.Action
            |> ignore

            ContractValidation.text "diagnosticOptions.LocalWarningDirectives.Code" directive.Code
            |> ignore

            ContractValidation.text
                "diagnosticOptions.LocalWarningDirectives.LogicalPath"
                directive.LogicalPath
            |> ignore
        )

        diagnosticOptions.MaximumErrors
        |> Option.iter (fun value ->
            if value < 0 then
                invalidArg
                    "diagnosticOptions.MaximumErrors"
                    "diagnosticOptions.MaximumErrors must be non-negative."
        )

        diagnosticOptions.PreferredUICulture
        |> Option.iter (
            ContractValidation.text "diagnosticOptions.PreferredUICulture"
            >> ignore
        )

        diagnosticOptions.LCID
        |> Option.iter (fun value ->
            if value < 0 then
                invalidArg "diagnosticOptions.LCID" "diagnosticOptions.LCID must be non-negative."
        )

        diagnosticOptions.PreferredUILanguage
        |> Option.iter (
            ContractValidation.text "diagnosticOptions.PreferredUILanguage"
            >> ignore
        )

        ContractValidation.reference
            "diagnosticOptions.DiagnosticStyle"
            diagnosticOptions.DiagnosticStyle
        |> ignore

        ContractValidation.reference
            "diagnosticOptions.ConsoleColorMode"
            diagnosticOptions.ConsoleColorMode
        |> ignore

        ContractValidation.immutableArray
            "emissionOptions.EmbeddedSourceIdentities"
            emissionOptions.EmbeddedSourceIdentities
        |> Seq.iter (validateIdentity "emissionOptions.EmbeddedSourceIdentities")

        ContractValidation.immutableArray
            "emissionOptions.DebugDocumentPaths"
            emissionOptions.DebugDocumentPaths
        |> Seq.iter (
            ContractValidation.text "emissionOptions.DebugDocumentPaths"
            >> ignore
        )

        ContractValidation.immutableArray
            "emissionOptions.SourceLinkJson"
            emissionOptions.SourceLinkJson
        |> ignore

        ContractValidation.reference "emissionOptions.DebugFormat" emissionOptions.DebugFormat
        |> ignore

        ContractValidation.immutableArray "signingOptions.Key" signingOptions.Key
        |> ignore

        ContractValidation.reference "signingOptions.Mode" signingOptions.Mode
        |> ignore

        ContractValidation.immutableArray "resources.Managed" resources.Managed
        |> ignore

        ContractValidation.immutableArray "resources.Native" resources.Native
        |> ignore

        for resource in resources.Managed do
            ContractValidation.reference "resources.Managed" resource
            |> ignore

            validateIdentity "resources.Managed.StableId" resource.StableId

            ContractValidation.text "resources.Managed.LogicalName" resource.LogicalName
            |> ignore

            ContractValidation.reference "resources.Managed.Visibility" resource.Visibility
            |> ignore

            ContractValidation.immutableArray "resources.Managed.Content" resource.Content
            |> ignore

            ContractValidation.text
                "resources.Managed.ContentFingerprint"
                resource.ContentFingerprint
            |> ignore

        for resource in resources.Native do
            ContractValidation.reference "resources.Native" resource
            |> ignore

            validateIdentity "resources.Native.StableId" resource.StableId

            ContractValidation.immutableArray "resources.Native.Content" resource.Content
            |> ignore

            ContractValidation.text
                "resources.Native.ContentFingerprint"
                resource.ContentFingerprint
            |> ignore

        for artifact in copiedRequestedArtifacts do
            match artifact with
            | RequestedArtifact.Custom stableId ->
                validateIdentity "requestedArtifacts.Custom" stableId
            | _ -> ()

        let identities =
            seq {
                yield requestIdentity
                yield assemblyIdentity.StableId

                yield!
                    copiedSources
                    |> Seq.map (fun source -> source.StableId)

                yield!
                    copiedTargetReferences
                    |> Seq.map (fun reference -> reference.StableId)

                yield!
                    resources.Managed
                    |> Seq.map (fun resource -> resource.StableId)

                yield!
                    resources.Native
                    |> Seq.map (fun resource -> resource.StableId)

                for artifact in copiedRequestedArtifacts do
                    match artifact with
                    | RequestedArtifact.Custom stableId -> yield stableId
                    | _ -> ()
            }

        let duplicateIdentity =
            identities
            |> Seq.groupBy _.Value
            |> Seq.tryPick (fun (value, matches) ->
                if Seq.length matches > 1 then Some value else None
            )

        match duplicateIdentity with
        | Some value -> invalidArg "request" $"Duplicate stable identity '{value}'."
        | None -> {
            ContractVersion = contractVersion
            RequestIdentity = requestIdentity
            AssemblyIdentity = assemblyIdentity
            Sources = copiedSources
            TargetReferences = copiedTargetReferences
            SemanticOptions = semanticOptions
            DiagnosticOptions = diagnosticOptions
            EmissionOptions = emissionOptions
            SigningOptions = signingOptions
            Resources = resources
            RequestedArtifacts = copiedRequestedArtifacts
          }

    override _.ToString() = "CompilationRequest"
