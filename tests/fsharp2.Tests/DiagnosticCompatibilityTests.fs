namespace fsharp2.Tests

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open Expecto
open FSharp2.Compiler

module DiagnosticCompatibilityTests =
    let private pinnedSourceCommit = "b611d184a141d1ad1994ff59c01fecc10f787a89"

    let private repositoryRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    let private sourceRepository =
        match Environment.GetEnvironmentVariable("FSHARP2_ORACLE_SOURCE_ROOT") with
        | value when not (String.IsNullOrWhiteSpace(value)) -> Path.GetFullPath(value)
        | _ -> Path.GetFullPath(Path.Combine(repositoryRoot, "..", "fsharp"))

    let private runGit arguments =
        let startInfo = ProcessStartInfo("git")
        startInfo.WorkingDirectory <- sourceRepository
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true
        startInfo.UseShellExecute <- false

        for argument in arguments do
            startInfo.ArgumentList.Add(argument)

        use gitProcess = new Process(StartInfo = startInfo)

        if not (gitProcess.Start()) then
            failwith "Git did not start."

        let output = gitProcess.StandardOutput.ReadToEndAsync()
        let error = gitProcess.StandardError.ReadToEndAsync()
        gitProcess.WaitForExit()

        if
            gitProcess.ExitCode
            <> 0
        then
            failwith $"Git failed with exit code {gitProcess.ExitCode}: {error.Result.Trim()}"

        output.Result

    let private addCandidate (candidates: Dictionary<string, HashSet<string>>) number source =
        if
            number
            >= 0
            && number
               <= 9999
        then
            let familyId = sprintf "FS%04d" number

            match candidates.TryGetValue(familyId) with
            | true, sources ->
                sources.Add(source)
                |> ignore
            | false, _ ->
                candidates.Add(familyId, HashSet<string>([ source ], StringComparer.Ordinal))

    let private capturePath =
        Path.Combine(
            repositoryRoot,
            "tools",
            "FSharp2.DiagnosticInventory",
            "inputs",
            "candidate-sources.json"
        )

    let private canReadPinnedSource () =
        if not (Directory.Exists(sourceRepository)) then
            false
        else
            try
                String.Equals(
                    (runGit [
                        "cat-file"
                        "-t"
                        pinnedSourceCommit
                    ])
                        .Trim(),
                    "commit",
                    StringComparison.Ordinal
                )
            with _ ->
                false

    let private discoverCapturedCandidates () =
        use document = JsonDocument.Parse(File.ReadAllBytes(capturePath))
        let root = document.RootElement

        Expect.equal
            (root.GetProperty("sourceCommit").GetString())
            pinnedSourceCommit
            "The captured discovery inputs must use the approved source commit."

        let candidates = Dictionary<string, HashSet<string>>(StringComparer.Ordinal)

        for entry in root.GetProperty("entries").EnumerateArray() do
            let familyId = entry.GetProperty("familyId").GetString()
            let sourceKind = entry.GetProperty("sourceKind").GetString()

            match familyId, sourceKind with
            | null, _
            | _, null -> failtest "A captured discovery entry must have a familyId and sourceKind."
            | family, source ->
                let number = Int32.Parse(family.AsSpan(2))
                addCandidate candidates number source

        candidates

    let private discoverLiveCandidates () =
        let candidates = Dictionary<string, HashSet<string>>(StringComparer.Ordinal)

        let fsComp =
            runGit [
                "show"
                $"{pinnedSourceCommit}:src/Compiler/FSComp.txt"
            ]

        for matched in Regex.Matches(fsComp, @"(?m)^(\d{1,4}),([^,]+),") do
            let number = Int32.Parse(matched.Groups[1].Value)
            addCandidate candidates number "compiler-mapping"
            addCandidate candidates number "resource-inventory"

        let compilerDiagnostics =
            runGit [
                "show"
                $"{pinnedSourceCommit}:src/Compiler/Driver/CompilerDiagnostics.fs"
            ]

        let mappingStart =
            compilerDiagnostics.IndexOf("member exn.DiagnosticNumber =", StringComparison.Ordinal)

        let mappingEnd =
            compilerDiagnostics.IndexOf("member x.Number =", mappingStart, StringComparison.Ordinal)

        Expect.isGreaterThanOrEqual mappingStart 0 "The pinned diagnostic mapping must exist."

        Expect.isGreaterThan
            mappingEnd
            mappingStart
            "The pinned diagnostic mapping must have an end."

        let diagnosticMappings =
            compilerDiagnostics.Substring(
                mappingStart,
                mappingEnd
                - mappingStart
            )

        for matched in Regex.Matches(diagnosticMappings, @"->\s*(\d{1,4})\s*(?:\r?\n|$)") do
            addCandidate candidates (Int32.Parse(matched.Groups[1].Value)) "compiler-mapping"

        for matched in Regex.Matches(diagnosticMappings, @"//\s*(\d{1,4}) cannot be reused") do
            addCandidate candidates (Int32.Parse(matched.Groups[1].Value)) "compiler-mapping"

        for number in 94..100 do
            addCandidate candidates number "compiler-mapping"

        let upstreamTests =
            runGit [
                "grep"
                "-I"
                "-n"
                "-E"
                @"FS[0-9]{4}([^0-9]|$)"
                pinnedSourceCommit
                "--"
                "tests"
            ]

        let warningOptionTests =
            runGit [
                "grep"
                "-I"
                "-n"
                "-E"
                @"FS[0-9]{4}([^0-9]|$)|#(no)?warn|--(no)?warn|with(Warning|Error)Code|\b(Warning|Error|Information)[[:space:]]+[0-9]{1,4}"
                pinnedSourceCommit
                "--"
                "tests/FSharp.Compiler.ComponentTests/CompilerOptions/fsc/warn"
                "tests/FSharp.Compiler.ComponentTests/CompilerOptions/fsc/warnon"
                "tests/FSharp.Compiler.ComponentTests/CompilerDirectives/Nowarn.fs"
                "tests/fsharp/Compiler/Warnings"
            ]

        let diagnosticCode = Regex(@"FS(\d{4})(?!\d)", RegexOptions.CultureInvariant)

        let warningNumber =
            Regex(
                """(?:#(?:nowarn|warnon)|--(?:nowarn|warnon|warnaserror-?)\s*:|with(?:Warning|Error)Code|\b(?:Warning|Error|Information))\s*"?(?:FS)?(\d{1,4})""",
                RegexOptions.CultureInvariant
            )

        for line in
            upstreamTests.Split(
                [|
                    '\r'
                    '\n'
                |],
                StringSplitOptions.RemoveEmptyEntries
            ) do
            for matched in diagnosticCode.Matches(line) do
                addCandidate candidates (Int32.Parse(matched.Groups[1].Value)) "upstream-baseline"

        for line in
            warningOptionTests.Split(
                [|
                    '\r'
                    '\n'
                |],
                StringSplitOptions.RemoveEmptyEntries
            ) do
            for matched in diagnosticCode.Matches(line) do
                addCandidate candidates (Int32.Parse(matched.Groups[1].Value)) "upstream-baseline"

            for matched in warningNumber.Matches(line) do
                addCandidate candidates (Int32.Parse(matched.Groups[1].Value)) "upstream-baseline"

        let locksRoot = Path.Combine(repositoryRoot, "tests", "conformance", "locks")

        for path in
            Directory.EnumerateFiles(locksRoot, "*.oracle-lock.json", SearchOption.AllDirectories) do
            use document = JsonDocument.Parse(File.ReadAllBytes(path))

            if
                document.RootElement.TryGetProperty("diagnostics")
                |> fst
            then
                for diagnostic in document.RootElement.GetProperty("diagnostics").EnumerateArray() do
                    let code = diagnostic.GetProperty("code").GetString()

                    if
                        not (String.IsNullOrEmpty(code))
                        && Regex.IsMatch(code, @"^FS\d{4}$")
                    then
                        addCandidate candidates (Int32.Parse(code.AsSpan(2))) "dynamic-oracle"

        candidates

    let private discoverCandidates () =
        if canReadPinnedSource () then
            discoverLiveCandidates ()
        elif File.Exists(capturePath) then
            discoverCapturedCandidates ()
        else
            failtest
                $"The pinned source repository '{sourceRepository}' and capture '{capturePath}' are both unavailable."

    let private inventoryFamilies () =
        let inventoryPath =
            Path.Combine(
                repositoryRoot,
                "tests",
                "conformance",
                "inventories",
                "diagnostics",
                "fs-diagnostic-families.inventory.json"
            )

        if not (File.Exists(inventoryPath)) then
            Array.empty
        else
            use document = JsonDocument.Parse(File.ReadAllBytes(inventoryPath))

            document.RootElement.GetProperty("diagnosticFamilies").EnumerateArray()
            |> Seq.map (fun family -> family.GetProperty("familyId").GetString())
            |> Seq.choose Option.ofObj
            |> Seq.toArray

    [<Tests>]
    let tests =
        testList "Diagnostic Compatibility" [
            testCase "phase facts contain no rendered messages"
            <| fun _ ->
                let compilerAssembly = typeof<Compiler>.Assembly

                let visibility =
                    Reflection.BindingFlags.Public
                    ||| Reflection.BindingFlags.NonPublic

                let instanceMembers =
                    visibility
                    ||| Reflection.BindingFlags.Instance

                let staticMembers =
                    visibility
                    ||| Reflection.BindingFlags.Static

                let requiredType name =
                    let runtimeType = compilerAssembly.GetType($"FSharp2.Compiler.{name}", false)
                    Expect.isNotNull runtimeType $"The compiler must define {name}."
                    runtimeType

                let propertyNames (runtimeType: Type) =
                    runtimeType.GetProperties(instanceMembers)
                    |> Array.map _.Name
                    |> Set.ofArray

                let requiredMethod name (runtimeType: Type) =
                    let methodInfo = runtimeType.GetMethod(name, staticMembers)
                    Expect.isNotNull methodInfo $"{runtimeType.Name} must define {name}."
                    methodInfo

                let typedArray (elementType: Type) (values: obj array) =
                    let result = Array.CreateInstance(elementType, values.Length)

                    values
                    |> Array.iteri (fun index value -> result.SetValue(value, index))

                    result

                let propertyValue name (value: obj) =
                    value.GetType().GetProperty(name, instanceMembers).GetValue(value)

                let immutableItem name (value: obj) =
                    let items = propertyValue name value
                    items.GetType().GetProperty("Item").GetValue(items, [| box 0 |])

                let factProperties =
                    requiredType "DiagnosticFact"
                    |> propertyNames

                for propertyName in
                    [
                        "Key"
                        "Stage"
                        "OriginalSeverity"
                        "WarningLevel"
                        "OffByDefault"
                        "LogicalPath"
                        "Range"
                        "Arguments"
                        "RelatedFacts"
                        "SuggestionFacts"
                        "RecoveryGroup"
                        "ParentOccurrence"
                        "PhaseLocalOrder"
                    ] do
                    Expect.isTrue
                        (factProperties.Contains(propertyName))
                        $"DiagnosticFact must expose {propertyName}."

                Expect.isFalse
                    (factProperties.Contains("Message"))
                    "A phase fact must not contain a rendered message."

                for typeName in
                    [
                        "DiagnosticRelatedFact"
                        "DiagnosticSuggestionFact"
                    ] do
                    let names =
                        requiredType typeName
                        |> propertyNames

                    Expect.isFalse
                        (names.Contains("Message"))
                        $"{typeName} must not contain a rendered message."

                let argumentType = requiredType "DiagnosticArgument"
                let keyType = requiredType "DiagnosticKey"
                let relatedType = requiredType "DiagnosticRelatedFact"
                let suggestionType = requiredType "DiagnosticSuggestionFact"
                let factType = requiredType "DiagnosticFact"

                let stringArgumentCase =
                    Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(argumentType, visibility)
                    |> Array.find (fun case -> case.Name = "String")

                let argument value =
                    Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(
                        stringArgumentCase,
                        [| box value |],
                        visibility
                    )

                let originalArgument = argument "original"
                let changedArgument = argument "changed"

                let key =
                    (requiredMethod "create" keyType).Invoke(null, [| box "test.diagnostic" |])

                let relatedArguments = typedArray argumentType [| originalArgument |]

                let relatedFact =
                    (requiredMethod "Create" relatedType)
                        .Invoke(
                            null,
                            [|
                                key
                                relatedArguments
                                null
                                null
                                box 0L
                            |]
                        )

                let suggestionArguments = typedArray argumentType [| originalArgument |]

                let suggestionFact =
                    (requiredMethod "Create" suggestionType)
                        .Invoke(
                            null,
                            [|
                                key
                                suggestionArguments
                                box 0L
                            |]
                        )

                let factArguments = typedArray argumentType [| originalArgument |]
                let relatedFacts = typedArray relatedType [| relatedFact |]
                let suggestionFacts = typedArray suggestionType [| suggestionFact |]

                let fact =
                    (requiredMethod "Create" factType)
                        .Invoke(
                            null,
                            [|
                                key
                                box (DiagnosticStage.Compilation CompilationPhase.Syntax)
                                box DiagnosticSeverity.Error
                                null
                                box false
                                null
                                null
                                factArguments
                                relatedFacts
                                suggestionFacts
                                null
                                null
                                box 0L
                            |]
                        )

                relatedArguments.SetValue(changedArgument, 0)
                suggestionArguments.SetValue(changedArgument, 0)
                factArguments.SetValue(changedArgument, 0)
                relatedFacts.SetValue(null, 0)
                suggestionFacts.SetValue(null, 0)

                let assertOriginalArgument owner value =
                    let case, fields =
                        Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(
                            value,
                            argumentType,
                            visibility
                        )

                    Expect.equal case.Name "String" $"{owner} must preserve the argument type."

                    Expect.equal
                        (fields[0] :?> string)
                        "original"
                        $"{owner} must copy its arguments."

                assertOriginalArgument
                    "DiagnosticRelatedFact"
                    (immutableItem "Arguments" relatedFact)

                assertOriginalArgument
                    "DiagnosticSuggestionFact"
                    (immutableItem "Arguments" suggestionFact)

                assertOriginalArgument "DiagnosticFact" (immutableItem "Arguments" fact)

                let copiedRelatedFact = immutableItem "RelatedFacts" fact
                let copiedSuggestionFact = immutableItem "SuggestionFacts" fact
                Expect.isNotNull copiedRelatedFact "DiagnosticFact must copy related facts."
                Expect.isNotNull copiedSuggestionFact "DiagnosticFact must copy suggestion facts."

                assertOriginalArgument
                    "DiagnosticFact related facts"
                    (immutableItem "Arguments" copiedRelatedFact)

                assertOriginalArgument
                    "DiagnosticFact suggestion facts"
                    (immutableItem "Arguments" copiedSuggestionFact)

            testCase "warning policy matches pinned precedence"
            <| fun _ ->
                let compilerAssembly = typeof<Compiler>.Assembly

                let visibility =
                    Reflection.BindingFlags.Public
                    ||| Reflection.BindingFlags.NonPublic

                let staticMembers =
                    visibility
                    ||| Reflection.BindingFlags.Static

                let instanceMembers =
                    visibility
                    ||| Reflection.BindingFlags.Instance

                let requiredType name =
                    let runtimeType = compilerAssembly.GetType($"FSharp2.Compiler.{name}", false)
                    Expect.isNotNull runtimeType $"The compiler must define {name}."
                    runtimeType

                let requiredMethod name (runtimeType: Type) =
                    let methodInfo = runtimeType.GetMethod(name, staticMembers)
                    Expect.isNotNull methodInfo $"{runtimeType.Name} must define {name}."
                    methodInfo

                let options
                    warningLevel
                    disabledWarnings
                    enabledWarnings
                    treatWarningsAsErrors
                    warningsAsErrors
                    warningsNotAsErrors
                    maximumErrors
                    abortOnError
                    localWarningDirectives
                    =
                    DiagnosticOptions.Create(
                        warningLevel,
                        disabledWarnings,
                        enabledWarnings,
                        treatWarningsAsErrors,
                        warningsAsErrors,
                        warningsNotAsErrors,
                        maximumErrors,
                        abortOnError,
                        None,
                        localWarningDirectives,
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

                let defaults = options (Some 3) [||] [||] false [||] [||] None false [||]

                let diagnostic occurrence code numericCode severity =
                    CompilationDiagnostic.Create(
                        occurrence,
                        code,
                        numericCode,
                        None,
                        DiagnosticStage.Compilation CompilationPhase.Syntax,
                        severity,
                        severity,
                        DiagnosticDisposition.Emitted,
                        None,
                        "policy diagnostic",
                        Some "Program.fs",
                        None,
                        [||],
                        [||],
                        Some DiagnosticStream.StandardError
                    )

                let directive order action code =
                    LocalWarningDirective.Create(order, action, code, "Program.fs", None)

                let policyType = requiredType "DiagnosticPolicy"
                let evaluateMethod = requiredMethod "evaluate" policyType
                let inputMethod = requiredMethod "input" policyType
                let applyMethod = requiredMethod "apply" policyType
                let outcomeMethod = requiredMethod "outcome" policyType

                let apply
                    diagnosticOptions
                    (inputs: (int option * bool * bool * CompilationDiagnostic) array)
                    =
                    let runtimeInputs = Array.CreateInstance(inputMethod.ReturnType, inputs.Length)

                    inputs
                    |> Array.iteri (fun
                                        index
                                        (warningLevel, offByDefault, featureEnabled, diagnostic) ->
                        runtimeInputs.SetValue(
                            inputMethod.Invoke(
                                null,
                                [|
                                    box warningLevel
                                    box offByDefault
                                    box featureEnabled
                                    box diagnostic
                                |]
                            ),
                            index
                        )
                    )

                    applyMethod.Invoke(
                        null,
                        [|
                            box diagnosticOptions
                            box runtimeInputs
                        |]
                    )
                    |> unbox<System.Collections.Immutable.ImmutableArray<CompilationDiagnostic>>

                let applyOne diagnosticOptions warningLevel offByDefault featureEnabled diagnostic =
                    apply diagnosticOptions [|
                        Some warningLevel, offByDefault, featureEnabled, diagnostic
                    |]
                    |> Seq.exactlyOne

                let outcome compilationOutcome (inputs: seq<CompilationDiagnostic>) =
                    outcomeMethod.Invoke(
                        null,
                        [|
                            box compilationOutcome
                            box inputs
                        |]
                    )
                    :?> CompilationOutcome

                let assertResult
                    name
                    expectedSeverity
                    expectedDisposition
                    expectedSuppression
                    (actual: CompilationDiagnostic)
                    =
                    Expect.equal actual.EffectiveSeverity expectedSeverity $"{name}: severity"
                    Expect.equal actual.Disposition expectedDisposition $"{name}: disposition"
                    Expect.equal actual.Suppression expectedSuppression $"{name}: suppression"

                    let expectedStream =
                        match expectedDisposition, expectedSeverity with
                        | DiagnosticDisposition.Suppressed, _ -> None
                        | DiagnosticDisposition.Emitted, DiagnosticSeverity.Information ->
                            Some DiagnosticStream.StandardOutput
                        | DiagnosticDisposition.Emitted, _ -> Some DiagnosticStream.StandardError

                    Expect.equal actual.Stream expectedStream $"{name}: stream"

                let localWarnon = [| directive 1L LocalWarningDirectiveAction.Enable "FS0057" |]

                let localNowarn = [| directive 1L LocalWarningDirectiveAction.Disable "57" |]

                let cases = [
                    "original error",
                    options (Some 0) [| "57" |] [||] true [| "FS0057" |] [||] None false localNowarn,
                    DiagnosticSeverity.Error,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Error,
                    DiagnosticDisposition.Emitted,
                    None
                    "global promotion",
                    options (Some 3) [||] [||] true [||] [||] None false [||],
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Error,
                    DiagnosticDisposition.Emitted,
                    None
                    "per-code promotion ignores command-line nowarn",
                    options (Some 3) [| "FS0057" |] [||] false [| "57" |] [||] None false [||],
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Error,
                    DiagnosticDisposition.Emitted,
                    None
                    "per-code demotion vetoes global promotion",
                    options (Some 3) [||] [||] true [||] [| "FS0057" |] None false [||],
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Warning,
                    DiagnosticDisposition.Emitted,
                    None
                    "information per-code promotion",
                    options (Some 3) [||] [||] false [| "FS0057" |] [||] None false [||],
                    DiagnosticSeverity.Information,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Error,
                    DiagnosticDisposition.Emitted,
                    None
                    "enabled warning",
                    defaults,
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Warning,
                    DiagnosticDisposition.Emitted,
                    None
                    "local warnon overrides command-line nowarn and warning level",
                    options (Some 0) [| "57" |] [||] false [||] [||] None false localWarnon,
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Warning,
                    DiagnosticDisposition.Emitted,
                    None
                    "command-line warnon promotes information",
                    options (Some 0) [||] [| "FS0057" |] false [||] [||] None false [||],
                    DiagnosticSeverity.Information,
                    2,
                    true,
                    true,
                    DiagnosticSeverity.Warning,
                    DiagnosticDisposition.Emitted,
                    None
                    "enabled information",
                    defaults,
                    DiagnosticSeverity.Information,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Information,
                    DiagnosticDisposition.Emitted,
                    None
                    "global promotion respects command-line nowarn",
                    options (Some 3) [| "FS0057" |] [||] true [||] [||] None false [||],
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Hidden,
                    DiagnosticDisposition.Suppressed,
                    Some DiagnosticSuppression.GlobalNowarn
                    "local nowarn blocks per-code promotion",
                    options (Some 3) [||] [||] false [| "FS0057" |] [||] None false localNowarn,
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Hidden,
                    DiagnosticDisposition.Suppressed,
                    Some DiagnosticSuppression.LocalNowarn
                    "off-by-default warning",
                    defaults,
                    DiagnosticSeverity.Warning,
                    2,
                    true,
                    true,
                    DiagnosticSeverity.Hidden,
                    DiagnosticDisposition.Suppressed,
                    Some DiagnosticSuppression.OffByDefault
                    "feature-gated warning",
                    defaults,
                    DiagnosticSeverity.Warning,
                    2,
                    false,
                    false,
                    DiagnosticSeverity.Hidden,
                    DiagnosticDisposition.Suppressed,
                    Some DiagnosticSuppression.LanguageFeature
                    "original hidden occurrence",
                    defaults,
                    DiagnosticSeverity.Hidden,
                    2,
                    false,
                    true,
                    DiagnosticSeverity.Hidden,
                    DiagnosticDisposition.Suppressed,
                    Some DiagnosticSuppression.OffByDefault
                ]

                for name,
                    diagnosticOptions,
                    originalSeverity,
                    diagnosticWarningLevel,
                    offByDefault,
                    languageFeatureEnabled,
                    expectedSeverity,
                    expectedDisposition,
                    expectedSuppression in cases do
                    diagnostic 0L "FS0057" 57 originalSeverity
                    |> applyOne
                        diagnosticOptions
                        diagnosticWarningLevel
                        offByDefault
                        languageFeatureEnabled
                    |> assertResult name expectedSeverity expectedDisposition expectedSuppression

                for warningLevel in 0..5 do
                    let expectedSeverity, expectedDisposition, expectedSuppression =
                        if
                            warningLevel
                            >= 2
                        then
                            DiagnosticSeverity.Warning, DiagnosticDisposition.Emitted, None
                        else
                            DiagnosticSeverity.Hidden,
                            DiagnosticDisposition.Suppressed,
                            Some DiagnosticSuppression.WarningLevel

                    diagnostic 0L "57" 57 DiagnosticSeverity.Warning
                    |> applyOne
                        (options (Some warningLevel) [||] [||] false [||] [||] None false [||])
                        2
                        false
                        true
                    |> assertResult
                        $"warning level {warningLevel}"
                        expectedSeverity
                        expectedDisposition
                        expectedSuppression

                diagnostic 0L "FS1182" 1182 DiagnosticSeverity.Warning
                |> applyOne
                    (options (Some 0) [||] [| "1182" |] false [||] [||] None false [||])
                    2
                    true
                    true
                |> assertResult
                    "warnon enables an off-by-default warning"
                    DiagnosticSeverity.Warning
                    DiagnosticDisposition.Emitted
                    None

                let error = diagnostic 0L "FS0001" 1 DiagnosticSeverity.Error
                let promoted = diagnostic 1L "57" 57 DiagnosticSeverity.Warning
                let trailing = diagnostic 2L "FS0058" 58 DiagnosticSeverity.Warning

                let maximumResults =
                    apply (options (Some 3) [||] [||] false [| "FS0057" |] [||] (Some 1) false [||]) [|
                        None, false, true, error
                        Some 2, false, true, promoted
                        Some 2, false, true, trailing
                    |]

                Expect.equal maximumResults.Length 3 "Maximum errors must retain every occurrence."

                maximumResults[1]
                |> assertResult
                    "maximum-error boundary"
                    DiagnosticSeverity.Hidden
                    DiagnosticDisposition.Suppressed
                    (Some DiagnosticSuppression.MaximumErrors)

                let abortResults =
                    apply (options (Some 3) [||] [||] false [||] [||] None true [||]) [|
                        None, false, true, error
                        Some 2, false, true, trailing
                    |]

                Expect.equal abortResults.Length 2 "Abort-on-error must retain every occurrence."

                abortResults[1]
                |> assertResult
                    "abort boundary"
                    DiagnosticSeverity.Hidden
                    DiagnosticDisposition.Suppressed
                    (Some DiagnosticSuppression.AbortBoundary)

                let promotedError =
                    diagnostic 0L "FS0057" 57 DiagnosticSeverity.Warning
                    |> applyOne
                        (options (Some 3) [||] [||] false [| "57" |] [||] None false [||])
                        2
                        false
                        true

                Expect.equal
                    (outcome CompilationOutcome.Succeeded [ promotedError ])
                    CompilationOutcome.Failed
                    "An emitted effective error must fail compilation."

                let suppressedWarning =
                    diagnostic 0L "FS0057" 57 DiagnosticSeverity.Warning
                    |> applyOne
                        (options (Some 3) [| "57" |] [||] false [||] [||] None false [||])
                        2
                        false
                        true

                Expect.equal
                    (outcome CompilationOutcome.Failed [ suppressedWarning ])
                    CompilationOutcome.Succeeded
                    "A suppressed occurrence must not fail compilation."

                let commandLineType = requiredType "CommandLine"
                let parseMethod = requiredMethod "parse" commandLineType
                let outputPath = Path.Combine(Path.GetTempPath(), "wave3.dll")

                let parsed =
                    parseMethod.Invoke(
                        null,
                        [|
                            box [|
                                "--target:library"
                                $"-o:{outputPath}"
                                "--deterministic+"
                                "--debug:portable"
                                "--warn:5"
                                "--nowarn:FS0057"
                                "--nowarn:1182"
                                "--warnon:1182"
                                "--warnon:FS3180"
                                "--warnaserror+"
                                "--warnaserror:FS0057"
                                "--warnaserror:1182"
                                "--warnaserror-:57"
                                "--warnaserror-:FS1182"
                                "--maxerrors:7"
                                "--max-errors:9"
                                "--abortonerror"
                                "Program.fs"
                            |]
                        |]
                    )

                let resultCase, resultFields =
                    Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(
                        parsed,
                        parsed.GetType(),
                        visibility
                    )

                let parseDetail =
                    if resultCase.Name = "Error" then
                        resultFields[0] :?> string
                    else
                        String.Empty

                Expect.equal
                    resultCase.Name
                    "Ok"
                    $"Every Wave 3 warning option must parse. {parseDetail}"

                let invocation = resultFields[0]

                let invocationValue name =
                    invocation.GetType().GetProperty(name, instanceMembers).GetValue(invocation)

                Expect.sequenceEqual
                    (invocationValue "DisabledWarnings"
                     |> unbox<string list>)
                    [
                        "FS0057"
                        "1182"
                    ]
                    "Command-line nowarn must preserve code order."

                Expect.sequenceEqual
                    (invocationValue "EnabledWarnings"
                     |> unbox<string list>)
                    [
                        "1182"
                        "FS3180"
                    ]
                    "Command-line warnon must preserve code order."

                Expect.sequenceEqual
                    (invocationValue "WarningsAsErrors"
                     |> unbox<string list>)
                    [
                        "FS0057"
                        "1182"
                    ]
                    "Per-code promotion must preserve code order."

                Expect.sequenceEqual
                    (invocationValue "WarningsNotAsErrors"
                     |> unbox<string list>)
                    [
                        "57"
                        "FS1182"
                    ]
                    "Per-code demotion must preserve code order."

                Expect.equal
                    (invocationValue "MaximumErrors"
                     |> unbox<int option>)
                    (Some 9)
                    "The last maximum-error spelling must reach the invocation."

                Expect.isTrue
                    (invocationValue "AbortOnError"
                     |> unbox<bool>)
                    "Abort-on-error must reach the invocation."

                let adapterSuppressed =
                    diagnostic 0L "FS0058" 58 DiagnosticSeverity.Warning
                    |> applyOne
                        (options (Some 3) [| "58" |] [||] false [||] [||] None false [||])
                        2
                        false
                        true

                let adapterPromoted =
                    diagnostic 1L "FS0057" 57 DiagnosticSeverity.Warning
                    |> applyOne
                        (options (Some 3) [||] [||] false [| "57" |] [||] None false [||])
                        2
                        false
                        true

                let adapterResult: CompilationResult = {
                    Outcome = CompilationOutcome.Succeeded
                    Diagnostics =
                        System.Collections.Immutable.ImmutableArray.CreateRange [|
                            adapterSuppressed
                            adapterPromoted
                        |]
                    Artifacts = System.Collections.Immutable.ImmutableArray.Empty
                    Fingerprints = System.Collections.Immutable.ImmutableArray.Empty
                    PhaseResults = System.Collections.Immutable.ImmutableArray.Empty
                    Traces = System.Collections.Immutable.ImmutableArray.Empty
                }

                let pipelineType = requiredType "CompilationPipeline"
                let completeInvocationMethod = requiredMethod "completeInvocation" pipelineType

                let adapterResponse =
                    completeInvocationMethod.Invoke(
                        null,
                        [|
                            box (Stopwatch.GetTimestamp())
                            invocation
                            box adapterResult
                        |]
                    )

                let adapterError =
                    adapterResponse
                        .GetType()
                        .GetProperty("Error", instanceMembers)
                        .GetValue(adapterResponse)
                    :?> string

                Expect.stringContains
                    adapterError
                    "FS0057"
                    "The adapter must report the emitted effective error."

                Expect.isFalse
                    (adapterError.Contains("FS0058", StringComparison.Ordinal))
                    "The adapter must not report a suppressed occurrence as the failure."

            testCase "inventory discovers every pinned candidate"
            <| fun _ ->
                let expected = discoverCandidates ()
                let rows = inventoryFamilies ()

                let duplicates =
                    rows
                    |> Array.countBy id
                    |> Array.choose (fun (familyId, count) ->
                        if count = 1 then
                            None
                        else
                            Some $"{familyId} has {count} rows."
                    )

                let actual = HashSet<string>(rows, StringComparer.Ordinal)

                let missing =
                    expected.Keys
                    |> Seq.filter (
                        actual.Contains
                        >> not
                    )
                    |> Seq.sort
                    |> Seq.map (fun familyId ->
                        let sources =
                            expected[familyId]
                            |> Seq.sort
                            |> String.concat ", "

                        $"{familyId} <- {sources}"
                    )
                    |> Seq.toArray

                let extra =
                    actual
                    |> Seq.filter (
                        expected.ContainsKey
                        >> not
                    )
                    |> Seq.sort
                    |> Seq.map (fun familyId -> $"{familyId} is not in a pinned discovery source.")
                    |> Seq.toArray

                let failures =
                    Array.concat [
                        duplicates
                        missing
                        |> Array.map (fun value -> $"Missing {value}")
                        extra
                    ]

                if failures.Length > 0 then
                    failtest (String.concat Environment.NewLine failures)
        ]
