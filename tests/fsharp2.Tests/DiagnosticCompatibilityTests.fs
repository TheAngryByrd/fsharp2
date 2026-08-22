namespace fsharp2.Tests

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open Expecto

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
