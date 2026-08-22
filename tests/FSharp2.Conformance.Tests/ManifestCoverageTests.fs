namespace FSharp2.Conformance.Tests

open System
open System.Collections.Immutable
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open Expecto
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module ManifestCoverageTests =
    let private caseProperty
        (name: string)
        (conformanceCase: ConformanceCase)
        : JsonElement option =
        let mutable value = Unchecked.defaultof<JsonElement>

        if conformanceCase.Document.TryGetProperty(name, &value) then
            Some value
        else
            None

    let private removeString value (items: JsonArray) =
        items
        |> Seq.filter (fun item ->
            not (isNull item)
            && item.GetValue<string>() = value
        )
        |> Seq.toArray
        |> Array.iter (fun item ->
            items.Remove(item)
            |> ignore
        )

    let private removeObjectsWith (propertyName: string) (values: string seq) (items: JsonArray) =
        items
        |> Seq.filter (fun item ->
            if isNull item then
                false
            else
                let value = item.AsObject()[propertyName]

                not (isNull value)
                && (values
                    |> Seq.contains (value.GetValue<string>()))
        )
        |> Seq.toArray
        |> Array.iter (fun item ->
            items.Remove(item)
            |> ignore
        )

    let private repositoryWithCases
        (repository: ConformanceRepository)
        (mutation: JsonObject -> unit)
        =
        let cases =
            repository.Cases
            |> Seq.map (fun conformanceCase ->
                let document = JsonNode.Parse(conformanceCase.Document.GetRawText())

                if isNull document then
                    failtest $"Case '{conformanceCase.CaseId}' has an empty document"

                let caseObject = document.AsObject()
                mutation caseObject

                ConformanceCase(
                    conformanceCase.CaseId,
                    conformanceCase.FilePath,
                    conformanceCase.Tags,
                    jsonNodeToElement caseObject
                )
            )
            |> ImmutableArray.CreateRange

        ConformanceRepository(
            repository.Root,
            repository.Manifest,
            repository.ComparisonPolicy,
            repository.Toolchain,
            cases
        )

    let private validateCaseCoverage name mutation =
        withCopiedRoot
            name
            (fun root ->
                let repository = ManifestLoader.Load(root)

                repositoryWithCases repository mutation
                |> CoverageValidator.Validate
            )

    let private expectCoverageIssue code path description source (result: ValidationResult) =
        Expect.isFalse result.IsValid $"Required {description} '{path}' remained covered"

        let issue =
            result.Issues
            |> Seq.tryFind (fun issue ->
                issue.Code = code
                && issue.Path = path
            )

        Expect.isSome issue $"Missing {description} did not produce issue '{code}'"

        Expect.equal
            issue.Value.Message
            $"Required {description} '{path}' has no {source} coverage."
            $"Issue '{code}' preserves its structured coverage message"

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Schema coverage" [
            testCase "Conformance Schema covers every declared phase row"
            <| fun _ ->
                withCopiedRoot
                    "schema-phase-coverage"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        CoverageValidator.Validate(repository)
                        |> expectValid

                        let featureRows =
                            repository.Manifest.GetProperty("featureRows").EnumerateArray()

                        for featureRow in featureRows do
                            let featureRowId = featureRow.GetProperty("id").GetString()

                            for requiredPolarity in
                                featureRow.GetProperty("requiredPolarities").EnumerateArray() do
                                let polarity = requiredPolarity.GetString()

                                let incompleteCases =
                                    repository.Cases
                                    |> Seq.filter (fun (conformanceCase: ConformanceCase) ->
                                        match
                                            caseProperty "featureRow" conformanceCase,
                                            caseProperty "polarity" conformanceCase
                                        with
                                        | Some candidateRow, Some candidatePolarity ->
                                            candidateRow.GetString()
                                            <> featureRowId
                                            || candidatePolarity.GetString()
                                               <> polarity
                                        | _ -> true
                                    )
                                    |> ImmutableArray.CreateRange

                                let incomplete =
                                    ConformanceRepository(
                                        repository.Root,
                                        repository.Manifest,
                                        repository.ComparisonPolicy,
                                        repository.Toolchain,
                                        incompleteCases
                                    )

                                let result = CoverageValidator.Validate(incomplete)

                                Expect.isTrue
                                    (result.Issues
                                     |> Seq.exists (fun issue ->
                                         issue.Code = "coverage-polarity"
                                         && issue.Path = featureRowId
                                     ))
                                    $"Feature row '{featureRowId}' must require '{polarity}' coverage"

                            for requiredPhase in
                                featureRow.GetProperty("requiredPhases").EnumerateArray() do
                                let phase = requiredPhase.GetString()

                                let phaseIncomplete =
                                    repositoryWithCases
                                        repository
                                        (fun caseDocument ->
                                            let candidateRow = caseDocument["featureRow"]

                                            if
                                                not (isNull candidateRow)
                                                && candidateRow.GetValue<string>() = featureRowId
                                            then
                                                let phaseExpectations =
                                                    caseDocument["phaseExpectations"].AsObject()

                                                let phases = phaseExpectations["phases"].AsArray()

                                                phases
                                                |> Seq.filter (fun item ->
                                                    let itemObject = item.AsObject()
                                                    itemObject["phase"].GetValue<string>() = phase
                                                )
                                                |> Seq.toArray
                                                |> Array.iter (fun item ->
                                                    phases.Remove(item)
                                                    |> ignore
                                                )
                                        )

                                let phaseResult = CoverageValidator.Validate(phaseIncomplete)

                                Expect.isTrue
                                    (phaseResult.Issues
                                     |> Seq.exists (fun issue ->
                                         issue.Code = "coverage-phase"
                                         && issue.Path = featureRowId
                                     ))
                                    $"Feature row '{featureRowId}' must require '{phase}' proof"
                    )

            testCase "Conformance Schema enforces declared comparison policy"
            <| fun _ ->
                withCopiedRoot
                    "schema-comparison-policy"
                    (fun root ->
                        let policyPath = Path.Combine(root, "comparison-policy.v1.json")

                        mutateJson
                            policyPath
                            (fun policy ->
                                let policyObject = policy.AsObject()

                                let comparatorVersions =
                                    policyObject["comparatorVersions"].AsArray()

                                let diagnosticComparator =
                                    comparatorVersions
                                    |> Seq.map _.AsObject()
                                    |> Seq.find (fun comparator ->
                                        comparator["id"].GetValue<string>() = "diagnostic-exact"
                                    )

                                diagnosticComparator["class"] <- JsonValue.Create("normalized")
                            )

                        let policyHash =
                            policyPath
                            |> readJson
                            |> CanonicalJson.Canonicalize
                            |> Hashing.Sha256

                        mutateJson
                            (Path.Combine(root, "manifest.json"))
                            (fun manifest ->
                                let manifestObject = manifest.AsObject()

                                let comparisonPolicy =
                                    manifestObject["comparisonPolicy"].AsObject()

                                comparisonPolicy["sha256"] <- JsonValue.Create(policyHash)
                            )

                        ManifestLoader.Validate(root)
                        |> expectInvalidWith "comparison-policy-comparator-class"
                    )

            testCase "Conformance Schema keeps integration corpora outside primary feature names"
            <| fun _ ->
                withCopiedRoot
                    "schema-integration-boundary"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        CoverageValidator.Validate(repository)
                        |> expectValid

                        let featureRows =
                            repository.Manifest.GetProperty("featureRows").GetRawText()

                        let integrationCorpora =
                            repository.Manifest.GetProperty("integrationCorpora").GetRawText()

                        Expect.isFalse
                            (featureRows.Contains("IcedTasks", StringComparison.OrdinalIgnoreCase))
                            "Primary feature rows must stay corpus-neutral"

                        Expect.isTrue
                            (integrationCorpora.Contains(
                                "IcedTasks",
                                StringComparison.OrdinalIgnoreCase
                            ))
                            "The IcedTasks identity belongs to integrationCorpora"

                        let integrationCases =
                            repository.Cases
                            |> Seq.filter (fun (conformanceCase: ConformanceCase) ->
                                match caseProperty "kind" conformanceCase with
                                | Some value -> value.GetString() = "integration"
                                | None -> false
                            )
                            |> Seq.toArray

                        Expect.isNonEmpty
                            integrationCases
                            "At least one integration case must be declared"

                        for conformanceCase in integrationCases do
                            Expect.isTrue
                                (caseProperty "integrationCorpus" conformanceCase
                                 |> Option.isSome)
                                $"Integration case '{conformanceCase.CaseId}' needs integrationCorpus"

                            Expect.isFalse
                                (caseProperty "featureRow" conformanceCase
                                 |> Option.isSome)
                                $"Integration case '{conformanceCase.CaseId}' must not enter featureRows"
                    )

            testList "Conformance Coverage Contract" [
                testCase "Conformance Coverage duplicate case IDs return structured validation"
                <| fun _ ->
                    withCopiedRoot
                        "coverage-duplicate-case"
                        (fun root ->
                            let sourcePath = firstCasePath root

                            let duplicatePath =
                                Path.Combine(
                                    Path.GetDirectoryName(sourcePath),
                                    "duplicate.case.json"
                                )

                            File.Copy(sourcePath, duplicatePath)

                            let duplicateId =
                                readJson sourcePath
                                |> _.GetProperty("caseId").GetString()

                            let result = ManifestLoader.Validate(root)

                            Expect.isFalse result.IsValid "Duplicate case IDs must be invalid"

                            let issue =
                                result.Issues
                                |> Seq.tryFind (fun issue ->
                                    issue.Code = "case-duplicate"
                                    && issue.Path = Path.GetFullPath(root)
                                )

                            Expect.isSome issue "Duplicate case IDs return a structured issue"

                            Expect.equal
                                issue.Value.Message
                                $"Duplicate case id '{duplicateId}'."
                                "The duplicate issue identifies the repeated case ID"
                        )

                testCase "Conformance Coverage requires every language version"
                <| fun _ ->
                    validateCaseCoverage
                        "coverage-language-version"
                        (fun document ->
                            let envelope = document["envelope"].AsObject()

                            envelope["languageVersions"].AsArray()
                            |> removeString "10.0"
                        )
                    |> expectCoverageIssue
                        "coverage-language-version"
                        "10.0"
                        "language version"
                        "case"

                testCase "Conformance Coverage requires every option cell"
                <| fun _ ->
                    withCopiedRoot
                        "coverage-option"
                        (fun root ->
                            for inventoryPath in
                                Directory.EnumerateFiles(
                                    Path.Combine(root, "inventories"),
                                    "*.inventory.json",
                                    SearchOption.AllDirectories
                                ) do
                                mutateJson
                                    inventoryPath
                                    (fun inventory ->
                                        let inventoryObject = inventory.AsObject()

                                        inventoryObject["optionCells"].AsArray()
                                        |> removeString
                                            "explicit-ordered-references-fsharp-core-identity"
                                    )

                            ManifestLoader.Load(root)
                            |> CoverageValidator.Validate
                        )
                    |> expectCoverageIssue
                        "coverage-option"
                        "explicit-ordered-references-fsharp-core-identity"
                        "option cell"
                        "inventory"

                testCase "Conformance Coverage requires every target framework"
                <| fun _ ->
                    validateCaseCoverage
                        "coverage-target-framework"
                        (fun document ->
                            let envelope = document["envelope"].AsObject()

                            envelope["targetFrameworks"].AsArray()
                            |> removeString "net10.0"
                        )
                    |> expectCoverageIssue
                        "coverage-target-framework"
                        "net10.0"
                        "target framework"
                        "case"

                testCase "Conformance Coverage requires every configuration"
                <| fun _ ->
                    validateCaseCoverage
                        "coverage-configuration"
                        (fun document ->
                            let matrix = document["matrix"].AsObject()
                            let configurations = matrix["configurations"].AsObject()

                            configurations["required"].AsArray()
                            |> removeString "Release"
                        )
                    |> expectCoverageIssue "coverage-configuration" "Release" "configuration" "case"

                testCase "Conformance Coverage requires every artifact kind"
                <| fun _ ->
                    validateCaseCoverage
                        "coverage-artifact"
                        (fun document ->
                            document["expectedArtifacts"].AsArray()
                            |> removeObjectsWith "kind" [ "xml-documentation" ]
                        )
                    |> expectCoverageIssue
                        "coverage-artifact"
                        "xml-documentation"
                        "artifact kind"
                        "case"

                testCase "Conformance Coverage requires every probe kind"
                <| fun _ ->
                    let result =
                        validateCaseCoverage
                            "coverage-probe"
                            (fun document ->
                                document["probes"].AsArray()
                                |> removeObjectsWith "kind" [
                                    "downstream-fsharp"
                                    "downstream-csharp"
                                ]
                            )

                    result
                    |> expectCoverageIssue "coverage-probe" "downstream-fsharp" "probe kind" "case"

                    result
                    |> expectCoverageIssue "coverage-probe" "downstream-csharp" "probe kind" "case"
            ]
        ]
