namespace fsharp2.Tests

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Resources
open System.Runtime.Loader
open System.Text
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

    let private diagnosticOptions
        preferredUICulture
        style
        flatErrors
        colorMode
        lcid
        preferredUILanguage
        standardOutputRedirected
        standardErrorRedirected
        =
        DiagnosticOptions.Create(
            Some 5,
            [||],
            [||],
            false,
            [||],
            [||],
            None,
            false,
            preferredUICulture,
            [||],
            false,
            flatErrors,
            true,
            style,
            colorMode,
            lcid,
            preferredUILanguage,
            false,
            standardOutputRedirected,
            standardErrorRedirected
        )

    let private diagnostic code numericCode subcategory severity message path range stream =
        CompilationDiagnostic.Create(
            0L,
            code,
            numericCode,
            subcategory,
            DiagnosticStage.Compilation CompilationPhase.Syntax,
            severity,
            severity,
            DiagnosticDisposition.Emitted,
            None,
            message,
            Some path,
            Some range,
            [||],
            [||],
            Some stream
        )

    let private renderedDiagnostic (rendered: obj) =
        let instanceMembers =
            Reflection.BindingFlags.Public
            ||| Reflection.BindingFlags.NonPublic
            ||| Reflection.BindingFlags.Instance

        let property name =
            rendered.GetType().GetProperty(name, instanceMembers).GetValue(rendered)

        property "Stream" :?> DiagnosticStream option, property "Bytes" :?> byte array

    let private renderingMethod name failureMessage =
        let renderingType =
            typeof<Compiler>.Assembly.GetType("FSharp2.Compiler.DiagnosticRendering", false)

        Expect.isNotNull renderingType "The compiler must define the diagnostic rendering boundary."

        let methodInfo =
            renderingType.GetMethod(
                name,
                Reflection.BindingFlags.Public
                ||| Reflection.BindingFlags.NonPublic
                ||| Reflection.BindingFlags.Static
            )

        Expect.isNotNull methodInfo failureMessage
        methodInfo

    let private renderDiagnostic options diagnostic sourceLine =
        let renderMethod =
            renderingMethod "render" "The diagnostic rendering boundary must define render."

        let rendered =
            renderMethod.Invoke(
                null,
                [|
                    box options
                    box diagnostic
                    box sourceLine
                |]
            )

        renderedDiagnostic rendered

    let private renderDiagnosticWithResources resources options diagnostic sourceLine =
        let renderMethod =
            renderingMethod
                "renderWithResources"
                "The diagnostic rendering boundary must accept an isolated resource manager."

        renderMethod.Invoke(
            null,
            [|
                box resources
                box options
                box diagnostic
                box sourceLine
            |]
        )
        |> renderedDiagnostic

    let private renderFormattedDiagnostic
        options
        error
        (sourceTextForPath: string -> string option)
        =
        let renderMethod =
            renderingMethod
                "renderFormattedError"
                "The diagnostic rendering boundary must render adapter errors."

        renderMethod.Invoke(
            null,
            [|
                box options
                box error
                box sourceTextForPath
            |]
        )
        |> renderedDiagnostic

    let private commandLineResult methodName arguments =
        let commandLineType =
            typeof<Compiler>.Assembly.GetType("FSharp2.Compiler.CommandLine", false)

        Expect.isNotNull commandLineType "The compiler must define the command-line boundary."

        let methodInfo =
            commandLineType.GetMethod(
                methodName,
                Reflection.BindingFlags.Public
                ||| Reflection.BindingFlags.NonPublic
                ||| Reflection.BindingFlags.Static
            )

        Expect.isNotNull methodInfo $"The command-line boundary must define {methodName}."

        let result = methodInfo.Invoke(null, [| box arguments |])

        Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(
            result,
            result.GetType(),
            Reflection.BindingFlags.Public
            ||| Reflection.BindingFlags.NonPublic
        )

    let private configuration =
        let releaseSegment =
            $"{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}"

        if
            AppContext.BaseDirectory.Contains(releaseSegment, StringComparison.OrdinalIgnoreCase)
        then
            "Release"
        else
            "Debug"

    let private compilerStartInfo arguments =
        let startInfo = ProcessStartInfo()
        let nativeExecutable = Environment.GetEnvironmentVariable("FSC2_EXECUTABLE")

        if String.IsNullOrWhiteSpace(nativeExecutable) then
            startInfo.FileName <- "dotnet"

            for argument in
                [
                    "run"
                    "--no-build"
                    "--configuration"
                    configuration
                    "--project"
                    $"{repositoryRoot}/src/fsc2.Prototype/fsc2.Prototype.fsproj"
                    "--"
                ] do
                startInfo.ArgumentList.Add(argument)
        else
            startInfo.FileName <- Path.GetFullPath(nativeExecutable)

        for argument in arguments do
            startInfo.ArgumentList.Add(argument)

        startInfo

    let private invokeCompilerBytes workingDirectory arguments =
        let startInfo = compilerStartInfo arguments
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use child = new Process(StartInfo = startInfo)
        use standardOutput = new MemoryStream()
        use standardError = new MemoryStream()

        child.Start()
        |> ignore

        let copyOutput = child.StandardOutput.BaseStream.CopyToAsync(standardOutput)
        let copyError = child.StandardError.BaseStream.CopyToAsync(standardError)

        if not (child.WaitForExit(30_000)) then
            child.Kill(true)
            failtest "fsc2 did not exit within 30 seconds"

        Threading.Tasks.Task.WaitAll [|
            copyOutput
            copyError
        |]

        child.ExitCode, standardOutput.ToArray(), standardError.ToArray()

    let private startCompilerService workingDirectory pipeName =
        let startInfo =
            compilerStartInfo [
                "--fsharp2-serve:"
                + pipeName
            ]

        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        let child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let ready = child.StandardOutput.ReadLineAsync()

        if not (ready.Wait(10_000)) then
            child.Kill(true)
            child.Dispose()
            failtest "the compiler service did not become ready within 10 seconds"

        Expect.equal
            ready.Result
            ("ready="
             + pipeName)
            "The compiler service must identify its endpoint."

        child

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
                            box defaults
                            box adapterResult
                        |]
                    )

                let adapterDiagnostics =
                    adapterResponse
                        .GetType()
                        .GetProperty("Diagnostics", instanceMembers)
                        .GetValue(adapterResponse)
                    |> unbox<ImmutableArray<CompilationDiagnostic>>

                Expect.isTrue
                    (adapterDiagnostics
                     |> Seq.exists (fun diagnostic ->
                         diagnostic.Code = "FS0057"
                         && diagnostic.EffectiveSeverity = DiagnosticSeverity.Error
                         && diagnostic.Disposition = DiagnosticDisposition.Emitted
                     ))
                    "The adapter must report the emitted effective error."

                Expect.isTrue
                    (adapterDiagnostics
                     |> Seq.exists (fun diagnostic ->
                         diagnostic.Code = "FS0058"
                         && diagnostic.Disposition = DiagnosticDisposition.Suppressed
                     ))
                    "The adapter must preserve the suppressed occurrence as structured data."

            testCase "rendering bytes match every pinned style"
            <| fun _ ->
                let parseRange = {
                    Start = { Offset = 0; Line = 3; Column = 17 }
                    End = { Offset = 1; Line = 3; Column = 18 }
                }

                let parseDiagnostic =
                    diagnostic
                        "FS0010"
                        10
                        (Some "parse")
                        DiagnosticSeverity.Error
                        "Unexpected symbol ')' in binding"
                        "Program.fs"
                        parseRange
                        DiagnosticStream.StandardError

                let plainDefault =
                    "\nProgram.fs(3,17): error FS0010: Unexpected symbol ')' in binding\r\n"

                let cases = [
                    DiagnosticStyle.Default, plainDefault
                    DiagnosticStyle.VisualStudio,
                    "\nProgram.fs(3,17,3,18): parse error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.Gcc,
                    "\nProgram.fs:3:17: error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.Emacs,
                    "\nFile \"Program.fs\", line 3, characters 16-17: error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.Rich,
                    "\nerror FS0010: Unexpected symbol ')' in binding\n"
                    + "  --> Program.fs (3,17)\n"
                    + "  3 | let answer () = )\n"
                    + System.String(' ', 22)
                    + "^\r\n"
                ]

                for style, expectedText in cases do
                    let stream, bytes =
                        renderDiagnostic
                            (diagnosticOptions
                                None
                                style
                                false
                                ConsoleColorMode.Disabled
                                None
                                None
                                true
                                true)
                            parseDiagnostic
                            (Some "let answer () = )")

                    Expect.equal stream (Some DiagnosticStream.StandardError) $"{style}: stream"

                    Expect.sequenceEqual
                        bytes
                        (Encoding.UTF8.GetBytes(expectedText))
                        $"{style}: exact UTF-8 bytes"

                let windowsPathDiagnostic =
                    diagnostic
                        "FS0010"
                        10
                        (Some "parse")
                        DiagnosticSeverity.Error
                        "Unexpected symbol ')' in binding"
                        "C:\\repo\\Program.fs"
                        parseRange
                        DiagnosticStream.StandardError

                let _, windowsPathBytes =
                    renderDiagnostic
                        (diagnosticOptions
                            None
                            DiagnosticStyle.Emacs
                            false
                            ConsoleColorMode.Disabled
                            None
                            None
                            true
                            true)
                        windowsPathDiagnostic
                        None

                Expect.sequenceEqual
                    windowsPathBytes
                    (Encoding.UTF8.GetBytes(
                        "\nFile \"C:/repo/Program.fs\", line 3, characters 16-17: error FS0010: Unexpected symbol ')' in binding\r\n"
                    ))
                    "Emacs output must normalize Windows path separators."

                let logicalPath = "folder/Program.fs"

                let logicalPathDiagnostic =
                    diagnostic
                        "FS0010"
                        10
                        (Some "parse")
                        DiagnosticSeverity.Error
                        "Unexpected symbol ')' in binding"
                        logicalPath
                        parseRange
                        DiagnosticStream.StandardError

                let operatingSystemPath = logicalPath.Replace('/', Path.DirectorySeparatorChar)

                let pathCases = [
                    DiagnosticStyle.Default,
                    $"\n{operatingSystemPath}(3,17): error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.VisualStudio,
                    "\nfolder\\Program.fs(3,17,3,18): parse error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.Gcc,
                    $"\n{operatingSystemPath}:3:17: error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.Emacs,
                    "\nFile \"folder/Program.fs\", line 3, characters 16-17: error FS0010: Unexpected symbol ')' in binding\r\n"
                    DiagnosticStyle.Rich,
                    "\nerror FS0010: Unexpected symbol ')' in binding\n"
                    + $"  --> {operatingSystemPath} (3,17)\r\n"
                ]

                for style, expectedText in pathCases do
                    let _, bytes =
                        renderDiagnostic
                            (diagnosticOptions
                                None
                                style
                                false
                                ConsoleColorMode.Disabled
                                None
                                None
                                true
                                true)
                            logicalPathDiagnostic
                            None

                    Expect.sequenceEqual
                        bytes
                        (Encoding.UTF8.GetBytes(expectedText))
                        $"{style}: path separators"

                let multiColumnRange = {
                    Start = { Offset = 0; Line = 5; Column = 9 }
                    End = { Offset = 5; Line = 5; Column = 14 }
                }

                let multiColumnDiagnostic =
                    diagnostic
                        "FS0001"
                        1
                        (Some "typecheck")
                        DiagnosticSeverity.Error
                        "Type mismatch"
                        "Program.fs"
                        multiColumnRange
                        DiagnosticStream.StandardError

                let _, multiColumnBytes =
                    renderDiagnostic
                        (diagnosticOptions
                            None
                            DiagnosticStyle.Rich
                            false
                            ConsoleColorMode.Disabled
                            None
                            None
                            true
                            true)
                        multiColumnDiagnostic
                        (Some "let x = mismatch")

                Expect.sequenceEqual
                    multiColumnBytes
                    (Encoding.UTF8.GetBytes(
                        "\nerror FS0001: Type mismatch\n"
                        + "  --> Program.fs (5,9)\n"
                        + "  5 | let x = mismatch\n"
                        + System.String(' ', 14)
                        + "^^^^^\r\n"
                    ))
                    "Rich output must mark every covered column."

                let typeRange = {
                    Start = { Offset = 0; Line = 5; Column = 26 }
                    End = { Offset = 1; Line = 5; Column = 37 }
                }

                let typeDiagnostic =
                    diagnostic
                        "FS0001"
                        1
                        (Some "typecheck")
                        DiagnosticSeverity.Error
                        "This expression was expected to have type\n    'int'    \nbut here has type\n    'string'"
                        "Program.fs"
                        typeRange
                        DiagnosticStream.StandardError

                let flatStream, flatBytes =
                    renderDiagnostic
                        (diagnosticOptions
                            None
                            DiagnosticStyle.Flat
                            true
                            ConsoleColorMode.Disabled
                            None
                            None
                            true
                            true)
                        typeDiagnostic
                        None

                Expect.equal flatStream (Some DiagnosticStream.StandardError) "Flat: stream"

                Expect.sequenceEqual
                    flatBytes
                    (Encoding.UTF8.GetBytes(
                        "\nProgram.fs(5,26): error FS0001: This expression was expected to have type\u001d    'int'    \u001dbut here has type\u001d    'string'\r\n"
                    ))
                    "Flat: embedded newlines must become ASCII 29."

                let coloredText =
                    "\nProgram.fs(3,17): \u001b[31merror FS0010\u001b[0m: Unexpected symbol ')' in binding\r\n"

                for name, colorMode, redirected, expectedText in
                    [
                        "explicit color", ConsoleColorMode.Enabled, true, coloredText
                        "automatic terminal color", ConsoleColorMode.Automatic, false, coloredText
                        "automatic redirected output",
                        ConsoleColorMode.Automatic,
                        true,
                        plainDefault
                        "disabled color", ConsoleColorMode.Disabled, false, plainDefault
                    ] do
                    let _, bytes =
                        renderDiagnostic
                            (diagnosticOptions
                                None
                                DiagnosticStyle.Default
                                false
                                colorMode
                                None
                                None
                                redirected
                                redirected)
                            parseDiagnostic
                            None

                    Expect.sequenceEqual bytes (Encoding.UTF8.GetBytes(expectedText)) name

                let standardOutputDiagnostic =
                    diagnostic
                        "FS0010"
                        10
                        (Some "parse")
                        DiagnosticSeverity.Error
                        "Unexpected symbol ')' in binding"
                        "Program.fs"
                        parseRange
                        DiagnosticStream.StandardOutput

                for name, standardOutputRedirected, standardErrorRedirected, expectedText in
                    [
                        "stdout terminal color", false, true, coloredText
                        "stdout redirected color", true, false, plainDefault
                    ] do
                    let stream, bytes =
                        renderDiagnostic
                            (diagnosticOptions
                                None
                                DiagnosticStyle.Default
                                false
                                ConsoleColorMode.Automatic
                                None
                                None
                                standardOutputRedirected
                                standardErrorRedirected)
                            standardOutputDiagnostic
                            None

                    Expect.equal stream (Some DiagnosticStream.StandardOutput) $"{name}: stream"
                    Expect.sequenceEqual bytes (Encoding.UTF8.GetBytes(expectedText)) name

                let richOptions =
                    diagnosticOptions
                        None
                        DiagnosticStyle.Rich
                        false
                        ConsoleColorMode.Disabled
                        None
                        None
                        true
                        true

                let multiSourceStream, multiSourceBytes =
                    renderFormattedDiagnostic
                        richOptions
                        "\nSecond.fs(3,17): error FS0010: Unexpected symbol ')' in binding"
                        (fun path ->
                            if path = "Second.fs" then
                                Some "module Second\n\nlet answer () = )"
                            else
                                Some "module First\n\nlet wrongSource = 1"
                        )

                Expect.equal
                    multiSourceStream
                    (Some DiagnosticStream.StandardError)
                    "Rich multi-source: stream"

                Expect.sequenceEqual
                    multiSourceBytes
                    (Encoding.UTF8.GetBytes(
                        "\nerror FS0010: Unexpected symbol ')' in binding\n"
                        + "  --> Second.fs (3,17)\n"
                        + "  3 | let answer () = )\n"
                        + System.String(' ', 22)
                        + "^\r\n"
                    ))
                    "Rich multi-source output must use the diagnostic source text."

                let optionCase, optionFields =
                    commandLineResult "diagnosticOptions" [| "--gnu-style-errors" |]

                Expect.equal optionCase.Name "Ok" "The GNU style option must select output options."

                let emacsOptions = optionFields[0] :?> DiagnosticOptions

                Expect.equal
                    emacsOptions.DiagnosticStyle
                    DiagnosticStyle.Emacs
                    "The GNU style option must select Emacs rendering."

                let colorCase, colorFields =
                    commandLineResult "diagnosticOptions" [| "--consolecolors" |]

                Expect.equal colorCase.Name "Ok" "The bare console-color option must parse."

                let colorOptions = colorFields[0] :?> DiagnosticOptions

                Expect.equal
                    colorOptions.ConsoleColorMode
                    ConsoleColorMode.Enabled
                    "The bare console-color option must enable color."

                let emacsOutputPath = Path.Combine(Path.GetTempPath(), "wave4-emacs.dll")

                let parseCase, _ =
                    commandLineResult "parse" [|
                        "--target:library"
                        "--deterministic+"
                        "--debug:portable"
                        "--gnu-style-errors"
                        "--consolecolors"
                        $"--out:{emacsOutputPath}"
                        "Program.fs"
                    |]

                Expect.equal parseCase.Name "Ok" "The main parser must accept GNU style output."

            testCase "culture matrix matches satellites and fallback"
            <| fun _ ->
                let range = {
                    Start = { Offset = 0; Line = 3; Column = 17 }
                    End = { Offset = 1; Line = 3; Column = 18 }
                }

                let parseDiagnostic =
                    diagnostic
                        "FS0010"
                        10
                        (Some "parse")
                        DiagnosticSeverity.Error
                        "Unexpected symbol ')' in binding"
                        "Program.fs"
                        range
                        DiagnosticStream.StandardError

                let expected message =
                    Encoding.UTF8.GetBytes($"\nProgram.fs(3,17): error FS0010: {message}\r\n")

                let cultures = [
                    "neutral", None, None, None, "Unexpected symbol ')' in binding"
                    "French satellite",
                    Some "fr-FR",
                    None,
                    None,
                    "symbole ')' inattendu dans la liaison"
                    "French parent fallback",
                    Some "fr-CA",
                    None,
                    None,
                    "symbole ')' inattendu dans la liaison"
                    "German satellite",
                    Some "de-DE",
                    None,
                    None,
                    "Unerwartete(s/r) Symbol \")\". in Bindung"
                    "missing satellite",
                    Some "zz-ZZ",
                    None,
                    None,
                    "Unexpected symbol ')' in binding"
                    "invalid culture",
                    Some "not a culture",
                    None,
                    None,
                    "Unexpected symbol ')' in binding"
                    "LCID no-op", None, Some 1036, None, "Unexpected symbol ')' in binding"
                    "preferred UI language over LCID",
                    None,
                    Some 1031,
                    Some "fr-FR",
                    "symbole ')' inattendu dans la liaison"
                    "explicit culture precedence",
                    Some "de-DE",
                    Some 1036,
                    Some "fr-FR",
                    "Unerwartete(s/r) Symbol \")\". in Bindung"
                ]

                for name, culture, lcid, preferredLanguage, expectedMessage in cultures do
                    let stream, bytes =
                        renderDiagnostic
                            (diagnosticOptions
                                culture
                                DiagnosticStyle.Default
                                false
                                ConsoleColorMode.Disabled
                                lcid
                                preferredLanguage
                                true
                                true)
                            parseDiagnostic
                            None

                    Expect.equal stream (Some DiagnosticStream.StandardError) $"{name}: stream"

                    Expect.sequenceEqual
                        bytes
                        (expected expectedMessage)
                        $"{name}: exact UTF-8 bytes"

                let compilerAssembly = typeof<Compiler>.Assembly

                let fixtureDirectory =
                    Path.Combine(Path.GetTempPath(), $"fsharp2-wave4-resources-{Guid.NewGuid():N}")

                let corruptSatelliteDirectory = Path.Combine(fixtureDirectory, "nl-NL")

                let corruptSatellitePath =
                    Path.Combine(
                        corruptSatelliteDirectory,
                        compilerAssembly.GetName().Name
                        + ".resources.dll"
                    )

                Directory.CreateDirectory(corruptSatelliteDirectory)
                |> ignore

                File.WriteAllBytes(
                    corruptSatellitePath,
                    [|
                        0x46uy
                        0x53uy
                        0x32uy
                    |]
                )

                let loadContext = new AssemblyLoadContext($"fsharp2-wave4-{Guid.NewGuid():N}", true)

                let resolveSatellite =
                    Func<AssemblyLoadContext, Reflection.AssemblyName, Reflection.Assembly>(fun
                                                                                                context
                                                                                                assemblyName ->
                        if
                            String.Equals(
                                assemblyName.Name,
                                compilerAssembly.GetName().Name
                                + ".resources",
                                StringComparison.Ordinal
                            )
                            && String.Equals(
                                assemblyName.CultureName,
                                "nl-NL",
                                StringComparison.OrdinalIgnoreCase
                            )
                        then
                            use stream = File.OpenRead(corruptSatellitePath)
                            context.LoadFromStream(stream)
                        else
                            null
                    )

                loadContext.add_Resolving resolveSatellite

                try
                    use assemblyStream =
                        new MemoryStream(File.ReadAllBytes(compilerAssembly.Location), false)

                    let isolatedAssembly = loadContext.LoadFromStream(assemblyStream)

                    let resources =
                        new ResourceManager(
                            "FSharp2.Compiler.Resources.Diagnostics",
                            isolatedAssembly
                        )

                    try
                        let stream, bytes =
                            renderDiagnosticWithResources
                                resources
                                (diagnosticOptions
                                    (Some "nl-NL")
                                    DiagnosticStyle.Default
                                    false
                                    ConsoleColorMode.Disabled
                                    None
                                    None
                                    true
                                    true)
                                parseDiagnostic
                                None

                        Expect.equal
                            stream
                            (Some DiagnosticStream.StandardError)
                            "corrupt satellite: stream"

                        Expect.sequenceEqual
                            bytes
                            (expected "Unexpected symbol ')' in binding")
                            "corrupt satellite: neutral UTF-8 fallback"
                    finally
                        resources.ReleaseAllResources()
                finally
                    loadContext.remove_Resolving resolveSatellite
                    loadContext.Unload()
                    File.Delete(corruptSatellitePath)
                    Directory.Delete(corruptSatelliteDirectory)
                    Directory.Delete(fixtureDirectory)

            testCase "direct and service results are structurally identical"
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

                let range = {
                    Start = { Offset = 12; Line = 3; Column = 13 }
                    End = { Offset = 13; Line = 3; Column = 14 }
                }

                let options =
                    DiagnosticOptions.Create(
                        Some 5,
                        [|
                            "FS0057"
                            "1182"
                        |],
                        [|
                            "1182"
                            "FS3180"
                        |],
                        true,
                        [|
                            "FS0057"
                            "1182"
                        |],
                        [|
                            "57"
                            "FS1182"
                        |],
                        Some 9,
                        true,
                        Some "fr-FR",
                        [|
                            LocalWarningDirective.Create(
                                4L,
                                LocalWarningDirectiveAction.Enable,
                                "FS0057",
                                "Program.fs",
                                Some range
                            )
                        |],
                        true,
                        false,
                        true,
                        DiagnosticStyle.Rich,
                        ConsoleColorMode.Disabled,
                        Some 1036,
                        Some "fr-CA",
                        true,
                        true,
                        true
                    )

                let suppressed =
                    CompilationDiagnostic.Create(
                        0L,
                        "FS0057",
                        57,
                        Some "typecheck",
                        DiagnosticStage.Compilation CompilationPhase.TypedDeclarations,
                        DiagnosticSeverity.Warning,
                        DiagnosticSeverity.Hidden,
                        DiagnosticDisposition.Suppressed,
                        Some DiagnosticSuppression.LocalNowarn,
                        "Unused value.",
                        Some "Program.fs",
                        Some range,
                        [||],
                        [||],
                        None
                    )

                let emitted =
                    CompilationDiagnostic.Create(
                        1L,
                        "FS0010",
                        10,
                        Some "parse",
                        DiagnosticStage.Compilation CompilationPhase.Syntax,
                        DiagnosticSeverity.Error,
                        DiagnosticSeverity.Error,
                        DiagnosticDisposition.Emitted,
                        None,
                        "Unexpected symbol ')' in binding",
                        Some "Program.fs",
                        Some range,
                        [|
                            DiagnosticRelatedInformation.Create(
                                "The binding starts here.",
                                Some "Program.fs",
                                Some range
                            )
                        |],
                        [|
                            "("
                            "value"
                        |],
                        Some DiagnosticStream.StandardError
                    )

                let result: CompilationResult = {
                    Outcome = CompilationOutcome.Failed
                    Diagnostics =
                        ImmutableArray.CreateRange [|
                            suppressed
                            emitted
                        |]
                    Artifacts = ImmutableArray.Empty
                    Fingerprints = ImmutableArray.Empty
                    PhaseResults = ImmutableArray.Empty
                    Traces = ImmutableArray.Empty
                }

                let outputPath = Path.Combine(Path.GetTempPath(), "wave5-structural.dll")

                let parsedCase, parsedFields =
                    commandLineResult "parse" [|
                        "--target:library"
                        $"--out:{outputPath}"
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
                        "--max-errors:9"
                        "--abortonerror"
                        "Program.fs"
                    |]

                Expect.equal parsedCase.Name "Ok" "The structural request must parse."
                let invocation = parsedFields[0]
                let pipelineType = requiredType "CompilationPipeline"
                let completeInvocation = requiredMethod "completeInvocation" pipelineType

                Expect.equal
                    (completeInvocation.GetParameters().Length)
                    4
                    "The adapter must carry structured diagnostic options into the result."

                let directResponse =
                    completeInvocation.Invoke(
                        null,
                        [|
                            box (Stopwatch.GetTimestamp())
                            invocation
                            box options
                            box result
                        |]
                    )

                Expect.isNull
                    (directResponse.GetType().GetProperty("Error", instanceMembers))
                    "The active service result must not expose a preformatted Error field."

                let serviceHostType = requiredType "ServiceHost"
                let writeInvocation = requiredMethod "writeInvocation" serviceHostType

                Expect.equal
                    (writeInvocation.GetParameters().Length)
                    4
                    "Protocol v10 requests must carry structured diagnostic options."

                let writeResponse = requiredMethod "writeResponse" serviceHostType
                let readResponse = requiredMethod "readResponse" serviceHostType

                use responseBytes = new MemoryStream()

                use responseWriter = new BinaryWriter(responseBytes, Encoding.UTF8, true)

                writeResponse.Invoke(
                    null,
                    [|
                        box responseWriter
                        directResponse
                    |]
                )
                |> ignore

                responseBytes.Position <- 0L

                use responseReader = new BinaryReader(responseBytes, Encoding.UTF8, true)
                let serviceResponse = readResponse.Invoke(null, [| box responseReader |])

                let responseValue name response =
                    response.GetType().GetProperty(name, instanceMembers).GetValue(response)

                let diagnosticOptionsProjection (value: DiagnosticOptions) =
                    value.WarningLevel,
                    (value.DisabledWarnings
                     |> Seq.toArray),
                    (value.EnabledWarnings
                     |> Seq.toArray),
                    value.TreatWarningsAsErrors,
                    (value.WarningsAsErrors
                     |> Seq.toArray),
                    (value.WarningsNotAsErrors
                     |> Seq.toArray),
                    value.MaximumErrors,
                    value.AbortOnError,
                    value.PreferredUICulture,
                    (value.LocalWarningDirectives
                     |> Seq.toArray),
                    value.FullPaths,
                    value.FlatErrors,
                    value.Utf8Output,
                    value.DiagnosticStyle,
                    value.ConsoleColorMode,
                    value.LCID,
                    value.PreferredUILanguage,
                    value.TestParserErrorRecovery,
                    value.StandardOutputRedirected,
                    value.StandardErrorRedirected

                let expectDiagnosticOptionsEqual message expected actual =
                    Expect.equal
                        (diagnosticOptionsProjection actual)
                        (diagnosticOptionsProjection expected)
                        message

                for propertyName in
                    [
                        "ExitCode"
                        "ServiceProcessId"
                        "QuerySchema"
                        "NodeKind"
                        "ContentFingerprint"
                        "PreviousContentFingerprint"
                        "InvalidationReason"
                        "ParseKey"
                        "CheckKey"
                        "LowerKey"
                        "DependencyCount"
                        "ParseDecision"
                        "CheckDecision"
                        "LowerDecision"
                        "ParseElapsedMicroseconds"
                        "CheckElapsedMicroseconds"
                        "LowerElapsedMicroseconds"
                        "LinkElapsedMicroseconds"
                        "PublishElapsedMicroseconds"
                        "CompileElapsedMicroseconds"
                        "ExportFingerprint"
                        "FragmentHash"
                        "Emitted"
                    ] do
                    Expect.equal
                        (responseValue propertyName serviceResponse)
                        (responseValue propertyName directResponse)
                        $"Protocol v10 must preserve {propertyName}."

                let transportedOptions =
                    responseValue "DiagnosticOptions" serviceResponse
                    |> unbox<DiagnosticOptions>

                expectDiagnosticOptionsEqual
                    "Protocol v10 must preserve every structured diagnostic option."
                    options
                    transportedOptions

                let diagnosticProjection (diagnostic: CompilationDiagnostic) =
                    diagnostic.Occurrence,
                    diagnostic.Code,
                    diagnostic.NumericCode,
                    diagnostic.Subcategory,
                    string diagnostic.Stage,
                    string diagnostic.OriginalSeverity,
                    string diagnostic.EffectiveSeverity,
                    string diagnostic.Disposition,
                    Option.map string diagnostic.Suppression,
                    diagnostic.Message,
                    diagnostic.LogicalPath,
                    diagnostic.Range,
                    (diagnostic.RelatedInformation
                     |> Seq.map (fun related -> related.Message, related.LogicalPath, related.Range)
                     |> Seq.toArray),
                    (diagnostic.Suggestions
                     |> Seq.toArray),
                    Option.map string diagnostic.Stream

                let directDiagnostics =
                    responseValue "Diagnostics" directResponse
                    |> unbox<ImmutableArray<CompilationDiagnostic>>

                let serviceDiagnostics =
                    responseValue "Diagnostics" serviceResponse
                    |> unbox<ImmutableArray<CompilationDiagnostic>>

                Expect.sequenceEqual
                    (directDiagnostics
                     |> Seq.map diagnosticProjection)
                    (serviceDiagnostics
                     |> Seq.map diagnosticProjection)
                    "Protocol v10 must preserve every diagnostic occurrence and field."

                let sourcePath =
                    Path.Combine(
                        repositoryRoot,
                        "tests",
                        "FSharp2.Prototype.Diagnostics",
                        "UnexpectedToken.fs"
                    )

                let sourceInputType = requiredType "SourceInput"

                let invocationSourcePath =
                    invocation
                        .GetType()
                        .GetProperty("SourcePaths", instanceMembers)
                        .GetValue(invocation)
                    |> unbox<string list>
                    |> List.exactlyOne

                let liveSource =
                    Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(
                        sourceInputType,
                        [|
                            box invocationSourcePath
                            box (File.ReadAllText(sourcePath))
                        |],
                        visibility
                    )

                let sourceValues = Array.CreateInstance(sourceInputType, 1)
                sourceValues.SetValue(liveSource, 0)

                let listModuleType =
                    typeof<list<int>>.Assembly
                        .GetType("Microsoft.FSharp.Collections.ListModule", true)

                let ofSeq =
                    listModuleType
                        .GetMethod("OfSeq", staticMembers)
                        .MakeGenericMethod([| sourceInputType |])

                let liveSources = ofSeq.Invoke(null, [| box sourceValues |])

                let createRequest = requiredMethod "createRequestWithDiagnosticOptions" pipelineType

                let liveRequest =
                    createRequest.Invoke(
                        null,
                        [|
                            invocation
                            box options
                            liveSources
                        |]
                    )
                    |> unbox<CompilationRequest>

                let liveResult = Compiler().Compile(liveRequest, Threading.CancellationToken.None)

                let liveDirectResponse =
                    completeInvocation.Invoke(
                        null,
                        [|
                            box (Stopwatch.GetTimestamp())
                            invocation
                            box options
                            box liveResult
                        |]
                    )

                let root =
                    Path.Combine(
                        Path.GetTempPath(),
                        "fsharp2-wave5-service",
                        Guid.NewGuid().ToString("N")
                    )

                Directory.CreateDirectory(root)
                |> ignore

                let pipeName =
                    "fsharp2-"
                    + Guid.NewGuid().ToString("N")

                use service = startCompilerService root pipeName

                let readProtocolFailure (version: int) =
                    use client =
                        new NamedPipeClientStream(
                            ".",
                            pipeName,
                            PipeDirection.InOut,
                            PipeOptions.None
                        )

                    client.Connect(10_000)

                    use writer = new BinaryWriter(client, Encoding.UTF8, true)
                    writer.Write(0x46533250)
                    writer.Write(version)
                    writer.Flush()

                    use reader = new BinaryReader(client, Encoding.UTF8, true)
                    readResponse.Invoke(null, [| box reader |])

                try
                    for version in
                        [
                            9
                            11
                        ] do
                        let rejected = readProtocolFailure version

                        Expect.equal
                            (responseValue "ExitCode" rejected :?> int)
                            1
                            $"Protocol version {version} must fail."

                        let rejectionDiagnostics =
                            responseValue "Diagnostics" rejected
                            |> unbox<ImmutableArray<CompilationDiagnostic>>

                        Expect.equal
                            rejectionDiagnostics.Length
                            1
                            $"Protocol version {version} diagnostic count"

                        Expect.equal
                            rejectionDiagnostics[0].Code
                            "FSC2P2002"
                            $"Protocol version {version} code"

                        Expect.equal
                            rejectionDiagnostics[0].Message
                            "unsupported compiler service protocol version"
                            $"Protocol version {version} message"

                    let compileRemote = requiredMethod "compileRemote" serviceHostType

                    let liveServiceResponse =
                        compileRemote.Invoke(
                            null,
                            [|
                                box pipeName
                                invocation
                                box options
                                liveSources
                            |]
                        )

                    let liveDirectOptions =
                        responseValue "DiagnosticOptions" liveDirectResponse
                        |> unbox<DiagnosticOptions>

                    let liveServiceOptions =
                        responseValue "DiagnosticOptions" liveServiceResponse
                        |> unbox<DiagnosticOptions>

                    expectDiagnosticOptionsEqual
                        "The retained service must preserve every structured diagnostic option."
                        liveDirectOptions
                        liveServiceOptions

                    let liveDirectDiagnostics =
                        responseValue "Diagnostics" liveDirectResponse
                        |> unbox<ImmutableArray<CompilationDiagnostic>>

                    let liveServiceDiagnostics =
                        responseValue "Diagnostics" liveServiceResponse
                        |> unbox<ImmutableArray<CompilationDiagnostic>>

                    Expect.sequenceEqual
                        (liveServiceDiagnostics
                         |> Seq.map diagnosticProjection)
                        (liveDirectDiagnostics
                         |> Seq.map diagnosticProjection)
                        "The retained service must preserve every structured diagnostic occurrence."

                    let compile name serverName =
                        let responsePath =
                            Path.Combine(
                                root,
                                name
                                + ".rsp"
                            )

                        let outputPath =
                            Path.Combine(
                                root,
                                name
                                + ".dll"
                            )

                        let pdbPath =
                            Path.Combine(
                                root,
                                name
                                + ".pdb"
                            )

                        let arguments = ResizeArray<string>()

                        serverName
                        |> Option.iter (fun value -> arguments.Add($"--fsharp2-server:{value}"))

                        for argument in
                            [
                                "--nologo"
                                "--target:library"
                                "--fullpaths"
                                "--richerrors"
                                "--utf8output"
                                "--consolecolors-"
                                "--preferreduilang:fr-FR"
                                "--warn:5"
                                "--nowarn:FS0057"
                                "--warnon:FS3180"
                                "--warnaserror+"
                                "--warnaserror:FS0057"
                                "--warnaserror-:FS1182"
                                "--max-errors:9"
                                "--abortonerror"
                                "--deterministic+"
                                "--debug:portable"
                                $"--out:{outputPath}"
                                $"--pdb:{pdbPath}"
                                sourcePath
                            ] do
                            arguments.Add(argument)

                        File.WriteAllLines(responsePath, arguments)

                        invokeCompilerBytes root [
                            "@"
                            + responsePath
                        ]

                    let directExit, directOutput, directError = compile "direct" None
                    let serviceExit, serviceOutput, serviceError = compile "service" (Some pipeName)

                    Expect.equal directExit 1 "The direct diagnostic compilation must fail."
                    Expect.equal serviceExit directExit "The retained-service exit must match."

                    Expect.sequenceEqual
                        serviceOutput
                        directOutput
                        "The retained-service stdout bytes must match."

                    Expect.sequenceEqual
                        serviceError
                        directError
                        "The retained-service stderr bytes must match."

                    Expect.isGreaterThan
                        serviceError.Length
                        0
                        "The diagnostic must write bytes to stderr."
                finally
                    if not service.HasExited then
                        service.Kill(true)

                        service.WaitForExit(10_000)
                        |> ignore

                    let rec deleteRoot attempts =
                        try
                            Directory.Delete(root, true)
                        with :? IOException when attempts > 0 ->
                            Threading.Thread.Sleep(50)

                            deleteRoot (
                                attempts
                                - 1
                            )

                    deleteRoot 100

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
