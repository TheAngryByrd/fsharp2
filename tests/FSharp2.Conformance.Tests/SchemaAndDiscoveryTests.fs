namespace FSharp2.Conformance.Tests

open System
open System.Collections.Immutable
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open Expecto
open FSharp2.Compiler
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module SchemaAndDiscoveryTests =
    let private diagnosticCompatibility =
        """
{
  "request": {
    "warningLevel": 5,
    "disabledWarnings": ["FS0044"],
    "enabledWarnings": ["FS1182"],
    "treatWarningsAsErrors": true,
    "warningsAsErrors": ["FS0020"],
    "warningsNotAsErrors": ["FS0044"],
    "maximumErrors": 100,
    "abortOnError": false,
    "preferredUICulture": "fr-FR",
    "localWarningDirectives": [
      {
        "kind": "nowarn",
        "code": "FS0044",
        "logicalPath": "/src/Program.fs",
        "range": { "startLine": 1, "startColumn": 0, "endLine": 1, "endColumn": 14 },
        "scope": "file"
      },
      {
        "kind": "nowarn",
        "code": "FS0020",
        "logicalPath": "/src/Program.fs",
        "range": { "startLine": 3, "startColumn": 0, "endLine": 3, "endColumn": 13 },
        "scope": "line"
      }
    ]
  },
  "output": {
    "fullPaths": true,
    "flatErrors": false,
    "utf8Output": false,
    "diagnosticStyle": "visual-studio",
    "consoleColorMode": "disabled",
    "lcid": 1036,
    "preferredUILanguage": "fr-FR",
    "testParserErrorRecovery": true,
    "standardOutputRedirected": true,
    "standardErrorRedirected": true
  },
  "occurrences": [
    {
      "occurrence": 0,
      "code": "FS0020",
      "numericCode": 20,
      "subcategory": "typecheck",
      "stage": { "kind": "compilation", "phase": "typed-declarations" },
      "originalSeverity": "warning",
      "effectiveSeverity": "error",
      "disposition": "emitted",
      "suppression": null,
      "message": "The result of this expression has type 'int' and is implicitly ignored.",
      "logicalPath": "/src/Program.fs",
      "materializedPath": "src/Program.fs",
      "range": { "startLine": 3, "startColumn": 0, "endLine": 3, "endColumn": 6 },
      "relatedInformation": [
        {
          "message": "The value is declared here.",
          "logicalPath": "/src/Library.fs",
          "range": { "startLine": 1, "startColumn": 4, "endLine": 1, "endColumn": 10 }
        },
        {
          "message": "The value is consumed here.",
          "logicalPath": "/src/Program.fs",
          "range": { "startLine": 3, "startColumn": 0, "endLine": 3, "endColumn": 6 }
        }
      ],
      "suggestions": ["bind the result", "ignore the result"],
      "recoveryGroup": "binding-group-0",
      "parentOccurrence": null,
      "rawBytes": {
        "offset": 0,
        "length": 128,
        "sha256": "sha256:7777777777777777777777777777777777777777777777777777777777777777"
      },
      "stream": "stderr"
    },
    {
      "occurrence": 1,
      "code": "FS0044",
      "numericCode": 44,
      "subcategory": null,
      "stage": { "kind": "command-line" },
      "originalSeverity": "warning",
      "effectiveSeverity": "hidden",
      "disposition": "suppressed",
      "suppression": "global-nowarn",
      "message": "This construct is deprecated.",
      "logicalPath": null,
      "materializedPath": null,
      "range": null,
      "relatedInformation": [],
      "suggestions": [],
      "recoveryGroup": null,
      "parentOccurrence": null,
      "rawBytes": null,
      "stream": null
    }
  ]
}
"""

    let private diagnosticFamilies =
        """
[
  {
    "familyId": "FS0020",
    "numericCode": 20,
    "candidateStatus": "candidate",
    "sourceMappingEvidence": [
      { "source": "pinned-compiler-map", "sha256": "sha256:0000000000000000000000000000000000000000000000000000000000000000" }
    ],
    "neutralResourceEvidence": [
      { "source": "FSComp.SR.tcExpressionResultIgnored", "sha256": "sha256:1111111111111111111111111111111111111111111111111111111111111111" }
    ],
    "satelliteResourceEvidence": [
      { "source": "fr-FR/FSComp.resources.dll", "sha256": "sha256:2222222222222222222222222222222222222222222222222222222222222222" }
    ],
    "upstreamBaselineEvidence": [
      { "source": "fsharpqa/Source/Conformance/BasicGrammarElements", "sha256": "sha256:3333333333333333333333333333333333333333333333333333333333333333" }
    ],
    "dynamicNumberSource": {
      "source": "UserCompilerMessage",
      "sha256": "sha256:9999999999999999999999999999999999999999999999999999999999999999",
      "boundary": "closed",
      "values": [20]
    },
    "variantIds": ["FS0020.expression-result-ignored"],
    "reachabilityReview": {
      "disposition": "reachable",
      "evidenceHashes": ["sha256:4444444444444444444444444444444444444444444444444444444444444444"]
    },
    "blockingFeatureIssue": null,
    "variants": [
      {
        "variantId": "FS0020.expression-result-ignored",
        "productionKey": "FSComp.SR.tcExpressionResultIgnored",
        "messageIdentity": "expression-result-ignored:{type}",
        "placeholders": [
          { "name": "type", "type": "string" }
        ],
        "originalSeverities": ["warning"],
        "effectiveSeverityCases": [
          { "condition": "warn-as-error", "severity": "error" }
        ],
        "stages": [
          { "kind": "compilation", "phase": "typed-declarations" }
        ],
        "subcategories": ["typecheck"],
        "languageVersions": ["preview"],
        "warningLevel": 5,
        "offByDefault": false,
        "optionSensitiveBehavior": ["warnaserror"],
        "cultures": [
          { "culture": "en-US", "available": true, "fallbackCulture": null }
        ],
        "triggerCaseIds": ["language.bindings.value-function-positive"],
        "recoveryShape": {
          "recoveryGroup": "nullable",
          "parentOccurrence": "nullable"
        },
        "relatedInformationShape": "ordered",
        "oracleEvidenceHashes": ["sha256:5555555555555555555555555555555555555555555555555555555555555555"]
      }
    ]
  }
]
"""

    let private workingDiagnosticClosure =
        """
{
  "state": "working",
  "dispositions": [
    {
      "familyId": "FS0020",
      "variantId": "FS0020.expression-result-ignored",
      "workingDisposition": "untriggered",
      "finalDisposition": null,
      "blockingIssue": "#27",
      "evidenceHashes": []
    }
  ]
}
"""

    let private finalDiagnosticClosure =
        """
{
  "state": "final",
  "dispositions": [
    {
      "familyId": "FS0020",
      "variantId": "FS0020.expression-result-ignored",
      "workingDisposition": "reachable",
      "finalDisposition": "reachable",
      "blockingIssue": null,
      "evidenceHashes": ["sha256:6666666666666666666666666666666666666666666666666666666666666666"]
    }
  ]
}
"""

    let private diagnosticCasePath root =
        Path.Combine(
            root,
            "cases",
            "language",
            "bindings",
            "language.bindings.value-function-positive.case.json"
        )

    let private diagnosticInventoryPath root =
        Path.Combine(root, "inventories", "bindings", "bindings.inventory.json")

    let private diagnosticClosurePath root =
        Path.Combine(root, "expectations", "bindings", "net10.0.closure.json")

    let private addDiagnosticInventoryAndClosure root =
        mutateJson
            (diagnosticInventoryPath root)
            (fun node ->
                (node.AsObject())["diagnosticFamilies"] <- JsonNode.Parse(diagnosticFamilies)
            )

        mutateJson
            (diagnosticClosurePath root)
            (fun node ->
                (node.AsObject())["diagnosticClosure"] <- JsonNode.Parse(workingDiagnosticClosure)
            )

    let private contractKinds = [
        "cases", "*.case.json", "case-v1", "oracle-lock-v1"
        "locks", "*.oracle-lock.json", "oracle-lock-v1", "family-inventory-v1"
        "inventories", "*.inventory.json", "family-inventory-v1", "closure-expectation-v1"
        "expectations", "*.closure.json", "closure-expectation-v1", "case-v1"
    ]

    let private contractPaths root directory pattern =
        Directory.EnumerateFiles(
            Path.Combine(root, directory),
            pattern,
            SearchOption.AllDirectories
        )
        |> Seq.sort

    let private selectedContractPath root directory pattern =
        if String.Equals(directory, "cases", StringComparison.Ordinal) then
            diagnosticCasePath root
        else
            contractPaths root directory pattern
            |> Seq.head

    let private assertDocumentKinds root =
        for directory, pattern, expected, _ in contractKinds do
            let paths =
                contractPaths root directory pattern
                |> Seq.toArray

            Expect.isNonEmpty paths $"The {directory} contract set must not be empty"

            for path in paths do
                let document = readJson path
                let mutable property = Unchecked.defaultof<JsonElement>

                let actual =
                    if document.TryGetProperty("documentKind", &property) then
                        property.GetString()
                    else
                        null

                Expect.equal
                    actual
                    expected
                    $"Every checked-in {pattern} document must select documentKind '{expected}': {path}"

    let private addDiagnosticCompatibility root casePath =
        mutateJson
            casePath
            (fun node ->
                let caseDocument = node.AsObject()
                caseDocument["diagnosticCompatibility"] <- JsonNode.Parse(diagnosticCompatibility)

                let options = (caseDocument["options"]).AsObject()

                options["compilerArguments"] <-
                    JsonNode.Parse(
                        """["--maxerrors:100","--max-errors:100","--gccerrors","--gnu-style-errors"]"""
                    )
            )

        let lockPath =
            Directory.EnumerateFiles(root, "*.oracle-lock.json", SearchOption.AllDirectories)
            |> Seq.head

        mutateJson
            lockPath
            (fun node ->
                let lockDocument = node.AsObject()
                lockDocument["diagnosticCompatibility"] <- JsonNode.Parse(diagnosticCompatibility)
            )

    let private validateDiagnosticMutation name mutation =
        withCopiedRoot
            name
            (fun root ->
                let casePath = diagnosticCasePath root
                addDiagnosticCompatibility root casePath
                mutation root casePath
                ManifestLoader.Validate(root)
            )

    let private validateMutation name mutation =
        withCopiedRoot
            name
            (fun root ->
                let casePath = firstCasePath root
                mutation root casePath
                ManifestLoader.Validate(root)
            )

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Schema and discovery" [
            testCase "Conformance Schema enforces the complete diagnostic compatibility envelope"
            <| fun _ ->
                withCopiedRoot
                    "schema-diagnostic-compatibility"
                    (fun root ->
                        let casePath = diagnosticCasePath root
                        addDiagnosticCompatibility root casePath
                        addDiagnosticInventoryAndClosure root
                        let result = ManifestLoader.Validate(root)

                        let details =
                            result.Issues
                            |> Seq.map (fun issue -> $"{issue.Code} {issue.Path}: {issue.Message}")
                            |> String.concat Environment.NewLine

                        Expect.isTrue
                            result.IsValid
                            $"The complete Issue #27 diagnostic compatibility envelope must validate:{Environment.NewLine}{details}"

                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById repository "language.bindings.value-function-positive"
                            )

                        let compatibility =
                            materialized.ResolvedDocument.GetProperty("diagnosticCompatibility")

                        let familyInventory =
                            repository.Inventories
                            |> Seq.find (fun inventory ->
                                let mutable property = Unchecked.defaultof<JsonElement>
                                inventory.TryGetProperty("diagnosticFamilies", &property)
                            )

                        let family = familyInventory.GetProperty("diagnosticFamilies")[0]
                        let variant = family.GetProperty("variants")[0]

                        Expect.equal
                            (family.GetProperty("familyId").GetString())
                            "FS0020"
                            "Repository loading preserves the diagnostic family identity"

                        Expect.equal
                            (variant.GetProperty("variantId").GetString())
                            "FS0020.expression-result-ignored"
                            "Repository loading preserves the diagnostic variant identity"

                        Expect.sequenceEqual
                            (variant.GetProperty("oracleEvidenceHashes").EnumerateArray()
                             |> Seq.map (fun value -> value.GetString()))
                            [
                                "sha256:5555555555555555555555555555555555555555555555555555555555555555"
                            ]
                            "Repository loading preserves exact Oracle evidence hashes"

                        let dynamicSource = family.GetProperty("dynamicNumberSource")

                        Expect.equal
                            (dynamicSource.GetProperty("source").GetString())
                            "UserCompilerMessage"
                            "Repository loading preserves the dynamic-number source"

                        Expect.equal
                            (dynamicSource.GetProperty("sha256").GetString())
                            "sha256:9999999999999999999999999999999999999999999999999999999999999999"
                            "Repository loading preserves the dynamic-number source evidence hash"

                        Expect.equal
                            (dynamicSource.GetProperty("boundary").GetString())
                            "closed"
                            "Repository loading preserves the closed dynamic-number boundary"

                        Expect.sequenceEqual
                            (dynamicSource.GetProperty("values").EnumerateArray()
                             |> Seq.map (fun value -> value.GetInt32()))
                            [ 20 ]
                            "Repository loading preserves the complete proven dynamic-number domain"

                        let closure = readJson (diagnosticClosurePath root)

                        Expect.equal
                            (closure
                                .GetProperty("diagnosticClosure")
                                .GetProperty("state")
                                .GetString())
                            "working"
                            "Validation preserves the Wave 1 working closure state"

                        Expect.sequenceEqual
                            materialized.Options.CompilerArguments
                            [
                                "--maxerrors:100"
                                "--max-errors:100"
                                "--gccerrors"
                                "--gnu-style-errors"
                            ]
                            "Materialization preserves ordered option aliases"

                        let directives =
                            compatibility
                                .GetProperty("request")
                                .GetProperty("localWarningDirectives")
                                .EnumerateArray()
                            |> Seq.toArray

                        Expect.sequenceEqual
                            (directives
                             |> Seq.map (fun directive ->
                                 directive.GetProperty("kind").GetString(),
                                 directive.GetProperty("code").GetString(),
                                 directive.GetProperty("scope").GetString()
                             ))
                            [
                                "nowarn", "FS0044", "file"
                                "nowarn", "FS0020", "line"
                            ]
                            "Materialization preserves ordered local nowarn directives"

                        Expect.sequenceEqual
                            (directives
                             |> Seq.map (fun directive ->
                                 directive.GetProperty("logicalPath").GetString(),
                                 directive
                                     .GetProperty("range")
                                     .GetProperty("startLine")
                                     .GetInt32(),
                                 directive
                                     .GetProperty("range")
                                     .GetProperty("endColumn")
                                     .GetInt32()
                             ))
                            [
                                "/src/Program.fs", 1, 14
                                "/src/Program.fs", 3, 13
                            ]
                            "Materialization preserves directive paths and ranges"

                        Expect.sequenceEqual
                            (compatibility.GetProperty("occurrences").EnumerateArray()
                             |> Seq.map (fun occurrence ->
                                 occurrence.GetProperty("occurrence").GetInt64()
                             ))
                            [
                                0L
                                1L
                            ]
                            "Materialization preserves deterministic occurrence order"

                        let emitted = compatibility.GetProperty("occurrences")[0]

                        Expect.sequenceEqual
                            (emitted.GetProperty("relatedInformation").EnumerateArray()
                             |> Seq.map (fun related -> related.GetProperty("message").GetString()))
                            [
                                "The value is declared here."
                                "The value is consumed here."
                            ]
                            "Materialization preserves related-information order"

                        Expect.sequenceEqual
                            (emitted.GetProperty("suggestions").EnumerateArray()
                             |> Seq.map (fun suggestion -> suggestion.GetString()))
                            [
                                "bind the result"
                                "ignore the result"
                            ]
                            "Materialization preserves suggestion order"

                        Expect.equal
                            (emitted.GetProperty("recoveryGroup").GetString())
                            "binding-group-0"
                            "Materialization preserves a recovery-group identity"

                        Expect.equal
                            (emitted.GetProperty("parentOccurrence").ValueKind)
                            JsonValueKind.Null
                            "A root recovery occurrence has no parent occurrence"

                        Expect.equal
                            (emitted.GetProperty("materializedPath").GetString())
                            "src/Program.fs"
                            "Materialization preserves the root-relative diagnostic path"

                        let rawBytes = emitted.GetProperty("rawBytes")

                        Expect.equal
                            (rawBytes.GetProperty("offset").GetInt64())
                            0L
                            "Materialization preserves the occurrence byte offset"

                        Expect.equal
                            (rawBytes.GetProperty("length").GetInt64())
                            128L
                            "Materialization preserves the occurrence byte length"

                        Expect.equal
                            (rawBytes.GetProperty("sha256").GetString())
                            "sha256:7777777777777777777777777777777777777777777777777777777777777777"
                            "Materialization preserves the occurrence raw-byte hash"

                        let suppressed = compatibility.GetProperty("occurrences")[1]

                        Expect.equal
                            (suppressed.GetProperty("effectiveSeverity").GetString())
                            "hidden"
                            "Suppressed occurrences retain hidden effective severity"

                        Expect.equal
                            (suppressed.GetProperty("stream").ValueKind)
                            JsonValueKind.Null
                            "Suppressed occurrences retain no output stream"

                        Expect.equal
                            (suppressed.GetProperty("materializedPath").ValueKind)
                            JsonValueKind.Null
                            "A suppressed occurrence has no materialized output path"

                        Expect.equal
                            (suppressed.GetProperty("rawBytes").ValueKind)
                            JsonValueKind.Null
                            "A suppressed occurrence has no raw output bytes"

                        let sdk = SdkSelection.Resolve(root, sdkRoot, null)
                        let request = CompilerContractProbe.CreateRequest(materialized)
                        let compilation = Compiler().Compile(request, CancellationToken.None)

                        Expect.equal
                            compilation.Outcome
                            CompilationOutcome.Succeeded
                            "The diagnostic contract fixture compiles"

                        let coreEvidence = CoreEvidenceWriter.Create(compilation)

                        let verdict =
                            VerdictResult(
                                ConformanceVerdict.Pass,
                                ImmutableArray<string>.Empty,
                                ImmutableArray<string>.Empty,
                                true
                            )

                        let runResult =
                            RunResultWriter.Create(
                                "run-diagnostic-compatibility",
                                null,
                                repository,
                                materialized,
                                sdk,
                                testAssemblyPath,
                                compilation,
                                coreEvidence,
                                null,
                                null,
                                null,
                                ImmutableArray<ProbeEvidence>.Empty,
                                ImmutableArray<ComparisonResult>.Empty,
                                verdict,
                                ImmutableDictionary<string, ImmutableArray<byte>>.Empty
                            )

                        let runResultPath = Path.Combine(root, "run-result.json")

                        File.WriteAllText(
                            runResultPath,
                            runResult.GetRawText(),
                            UTF8Encoding(false)
                        )

                        let restored = readJson runResultPath

                        let runCompatibility = restored.GetProperty("diagnosticCompatibility")

                        let expectedNode = JsonNode.Parse(compatibility.GetRawText())
                        let actualNode = JsonNode.Parse(runCompatibility.GetRawText())

                        Expect.isTrue
                            (JsonNode.DeepEquals(expectedNode, actualNode))
                            "Run-result write and read preserve the complete diagnostic compatibility envelope"

                        Expect.sequenceEqual
                            (runCompatibility.GetProperty("occurrences").EnumerateArray()
                             |> Seq.map (fun occurrence -> occurrence.ValueKind))
                            [
                                JsonValueKind.Object
                                JsonValueKind.Object
                            ]
                            "Run-result diagnostics remain structured occurrences"
                    )

                let partial =
                    withCopiedRoot
                        "schema-diagnostic-partial"
                        (fun root ->
                            let casePath = diagnosticCasePath root

                            mutateJson
                                casePath
                                (fun node ->
                                    (node.AsObject())["diagnosticCompatibility"] <-
                                        JsonNode.Parse("""{"request":{}}""")
                                )

                            ManifestLoader.Validate(root)
                        )

                Expect.isFalse
                    partial.IsValid
                    "A diagnostic compatibility envelope must be all-or-nothing"

                let unknownDiagnostic =
                    validateDiagnosticMutation
                        "schema-diagnostic-unknown"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let compatibility =
                                        ((node.AsObject())["diagnosticCompatibility"]).AsObject()

                                    let request = (compatibility["request"]).AsObject()
                                    request["unexpectedField"] <- JsonValue.Create(true)
                                )
                        )

                Expect.isFalse
                    unknownDiagnostic.IsValid
                    "Unknown diagnostic compatibility fields must fail closed"

                let localWarnon =
                    validateDiagnosticMutation
                        "schema-diagnostic-local-warnon"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let compatibility =
                                        ((node.AsObject())["diagnosticCompatibility"]).AsObject()

                                    let request = (compatibility["request"]).AsObject()

                                    let directives = (request["localWarningDirectives"]).AsArray()

                                    (directives[0].AsObject())["kind"] <-
                                        JsonValue.Create("warnon")
                                )
                        )

                Expect.isFalse
                    localWarnon.IsValid
                    "Local warning directives cannot contain command-line warnon entries"

                let unorderedOccurrences =
                    validateDiagnosticMutation
                        "schema-diagnostic-order"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[0].AsObject())["occurrence"] <-
                                        JsonValue.Create(2L)
                                )
                        )

                Expect.isFalse
                    unorderedOccurrences.IsValid
                    "Diagnostic occurrence values must increase in array order"

                let streamedSuppression =
                    validateDiagnosticMutation
                        "schema-diagnostic-suppressed-stream"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[1].AsObject())["stream"] <-
                                        JsonValue.Create("stderr")
                                )
                        )

                Expect.isFalse
                    streamedSuppression.IsValid
                    "A suppressed occurrence cannot select an output stream"

                let missingRecoveryGroup =
                    validateDiagnosticMutation
                        "schema-diagnostic-recovery-group"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[0].AsObject()).Remove("recoveryGroup")
                                    |> ignore
                                )
                        )

                Expect.isFalse
                    missingRecoveryGroup.IsValid
                    "Every diagnostic occurrence requires a nullable recoveryGroup field"

                let missingParentOccurrence =
                    validateDiagnosticMutation
                        "schema-diagnostic-parent-occurrence"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[0].AsObject()).Remove("parentOccurrence")
                                    |> ignore
                                )
                        )

                Expect.isFalse
                    missingParentOccurrence.IsValid
                    "Every diagnostic occurrence requires a nullable parentOccurrence field"

                let missingMaterializedPath =
                    validateDiagnosticMutation
                        "schema-diagnostic-materialized-path"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[0].AsObject()).Remove("materializedPath")
                                    |> ignore
                                )
                        )

                Expect.isFalse
                    missingMaterializedPath.IsValid
                    "Every diagnostic occurrence requires a nullable materializedPath field"

                let rootedMaterializedPath =
                    validateDiagnosticMutation
                        "schema-diagnostic-rooted-materialized-path"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[0].AsObject())["materializedPath"] <-
                                        JsonValue.Create("C:\\work\\Program.fs")
                                )
                        )

                Expect.isFalse
                    rootedMaterializedPath.IsValid
                    "A materialized diagnostic path must be root-relative"

                let missingRawBytes =
                    validateDiagnosticMutation
                        "schema-diagnostic-raw-bytes"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[0].AsObject()).Remove("rawBytes")
                                    |> ignore
                                )
                        )

                Expect.isFalse
                    missingRawBytes.IsValid
                    "Every diagnostic occurrence requires a nullable rawBytes field"

                let suppressedRawBytes =
                    validateDiagnosticMutation
                        "schema-diagnostic-suppressed-raw-bytes"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    (occurrences[1].AsObject())["rawBytes"] <-
                                        JsonNode.Parse(
                                            """{"offset":0,"length":1,"sha256":"sha256:8888888888888888888888888888888888888888888888888888888888888888"}"""
                                        )
                                )
                        )

                Expect.isFalse
                    suppressedRawBytes.IsValid
                    "A suppressed occurrence cannot link raw output bytes"

                let emptyRawBytes =
                    validateDiagnosticMutation
                        "schema-diagnostic-empty-raw-bytes"
                        (fun _ casePath ->
                            mutateJson
                                casePath
                                (fun node ->
                                    let occurrences =
                                        (((node.AsObject())["diagnosticCompatibility"]).AsObject()["occurrences"])
                                            .AsArray()

                                    let rawBytes =
                                        ((occurrences[0].AsObject())["rawBytes"]).AsObject()

                                    rawBytes["length"] <- JsonValue.Create(0L)
                                )
                        )

                Expect.isFalse
                    emptyRawBytes.IsValid
                    "An emitted occurrence must link at least one raw byte"

                let validateDynamicBoundary name (boundary: string) =
                    withCopiedRoot
                        name
                        (fun root ->
                            addDiagnosticInventoryAndClosure root

                            mutateJson
                                (diagnosticInventoryPath root)
                                (fun node ->
                                    let families =
                                        ((node.AsObject())["diagnosticFamilies"]).AsArray()

                                    let family = families[0].AsObject()

                                    let dynamicSource = JsonObject()

                                    dynamicSource["source"] <-
                                        JsonValue.Create("UserCompilerMessage")

                                    dynamicSource["sha256"] <-
                                        JsonValue.Create(
                                            "sha256:9999999999999999999999999999999999999999999999999999999999999999"
                                        )

                                    dynamicSource["boundary"] <- JsonValue.Create(boundary)

                                    dynamicSource["boundaryDescription"] <-
                                        JsonValue.Create(
                                            "The dynamic diagnostic-number domain is not closed."
                                        )

                                    dynamicSource["blockingIssue"] <- JsonValue.Create("#27")
                                    family["dynamicNumberSource"] <- dynamicSource
                                    family["blockingFeatureIssue"] <- JsonValue.Create("#27")
                                )

                            ManifestLoader.Validate(root)
                        )

                validateDynamicBoundary "schema-diagnostic-open-domain" "open"
                |> expectValid

                validateDynamicBoundary "schema-diagnostic-external-domain" "external"
                |> expectValid

                withCopiedRoot
                    "schema-diagnostic-not-applicable-domain"
                    (fun root ->
                        addDiagnosticInventoryAndClosure root

                        mutateJson
                            (diagnosticInventoryPath root)
                            (fun node ->
                                let families = ((node.AsObject())["diagnosticFamilies"]).AsArray()

                                let family = families[0].AsObject()

                                family["dynamicNumberSource"] <-
                                    JsonNode.Parse("""{"boundary":"not-applicable"}""")

                                family["blockingFeatureIssue"] <- null
                            )

                        ManifestLoader.Validate(root)
                        |> expectValid
                    )

                let incompleteDynamicBoundary name (payload: string) =
                    withCopiedRoot
                        name
                        (fun root ->
                            addDiagnosticInventoryAndClosure root

                            mutateJson
                                (diagnosticInventoryPath root)
                                (fun node ->
                                    let families =
                                        ((node.AsObject())["diagnosticFamilies"]).AsArray()

                                    let family = families[0].AsObject()
                                    family["dynamicNumberSource"] <- JsonNode.Parse(payload)
                                    family["blockingFeatureIssue"] <- JsonValue.Create("#27")
                                )

                            ManifestLoader.Validate(root)
                        )

                incompleteDynamicBoundary
                    "schema-diagnostic-open-domain-description"
                    """{"source":"UserCompilerMessage","sha256":"sha256:9999999999999999999999999999999999999999999999999999999999999999","boundary":"open","blockingIssue":"#27"}"""
                |> fun result ->
                    Expect.isFalse
                        result.IsValid
                        "An open dynamic-number boundary requires its description"

                incompleteDynamicBoundary
                    "schema-diagnostic-external-domain-issue"
                    """{"source":"UserCompilerMessage","sha256":"sha256:9999999999999999999999999999999999999999999999999999999999999999","boundary":"external","boundaryDescription":"The provider controls the diagnostic number."}"""
                |> fun result ->
                    Expect.isFalse
                        result.IsValid
                        "An external dynamic-number boundary requires its blocking issue"

                let incompleteClosedDomain =
                    withCopiedRoot
                        "schema-diagnostic-incomplete-domain"
                        (fun root ->
                            addDiagnosticInventoryAndClosure root

                            mutateJson
                                (diagnosticInventoryPath root)
                                (fun node ->
                                    let families =
                                        ((node.AsObject())["diagnosticFamilies"]).AsArray()

                                    let family = families[0].AsObject()

                                    let dynamicSource = (family["dynamicNumberSource"]).AsObject()

                                    dynamicSource.Remove("values")
                                    |> ignore
                                )

                            ManifestLoader.Validate(root)
                        )

                Expect.isFalse
                    incompleteClosedDomain.IsValid
                    "A closed dynamic-number boundary requires its complete proven values"

                let missingDynamicEvidence =
                    withCopiedRoot
                        "schema-diagnostic-missing-dynamic-evidence"
                        (fun root ->
                            addDiagnosticInventoryAndClosure root

                            mutateJson
                                (diagnosticInventoryPath root)
                                (fun node ->
                                    let families =
                                        ((node.AsObject())["diagnosticFamilies"]).AsArray()

                                    let family = families[0].AsObject()

                                    let dynamicSource = (family["dynamicNumberSource"]).AsObject()

                                    dynamicSource.Remove("sha256")
                                    |> ignore
                                )

                            ManifestLoader.Validate(root)
                        )

                Expect.isFalse
                    missingDynamicEvidence.IsValid
                    "An applicable dynamic-number source requires its evidence hash"

                let unknownDynamicBoundary =
                    validateDynamicBoundary "schema-diagnostic-unknown-domain" "observed"

                Expect.isFalse
                    unknownDynamicBoundary.IsValid
                    "A dynamic-number boundary must be closed, open, or external"

                withCopiedRoot
                    "schema-diagnostic-final-closure"
                    (fun root ->
                        addDiagnosticInventoryAndClosure root

                        mutateJson
                            (diagnosticClosurePath root)
                            (fun node ->
                                (node.AsObject())["diagnosticClosure"] <-
                                    JsonNode.Parse(finalDiagnosticClosure)
                            )

                        ManifestLoader.Validate(root)
                        |> expectValid
                    )

                let openFinalClosure =
                    withCopiedRoot
                        "schema-diagnostic-open-final-closure"
                        (fun root ->
                            addDiagnosticInventoryAndClosure root

                            mutateJson
                                (diagnosticClosurePath root)
                                (fun node ->
                                    let closure =
                                        ((node.AsObject())["diagnosticClosure"]).AsObject()

                                    closure["state"] <- JsonValue.Create("final")
                                )

                            ManifestLoader.Validate(root)
                        )

                Expect.isFalse
                    openFinalClosure.IsValid
                    "A final diagnostic closure cannot retain an untriggered working disposition"

                let incompleteVariant =
                    withCopiedRoot
                        "schema-diagnostic-incomplete-variant"
                        (fun root ->
                            addDiagnosticInventoryAndClosure root

                            mutateJson
                                (diagnosticInventoryPath root)
                                (fun node ->
                                    let families =
                                        ((node.AsObject())["diagnosticFamilies"]).AsArray()

                                    let variants =
                                        ((families[0].AsObject())["variants"]).AsArray()

                                    (variants[0].AsObject()).Remove("oracleEvidenceHashes")
                                    |> ignore
                                )

                            ManifestLoader.Validate(root)
                        )

                Expect.isFalse
                    incompleteVariant.IsValid
                    "Every diagnostic variant requires exact Oracle evidence hashes"

                withCopiedRoot
                    "schema-document-kinds"
                    (fun root ->
                        assertDocumentKinds root

                        ManifestLoader.Validate(root)
                        |> expectValid
                    )

                for directory, pattern, expected, wrong in contractKinds do
                    withCopiedRoot
                        $"schema-document-kind-{directory}"
                        (fun root ->
                            let path = selectedContractPath root directory pattern
                            let original = JsonNode.Parse(File.ReadAllText(path))

                            if isNull original then
                                failtest $"JSON document is empty: {path}"

                            removeProperty path "documentKind"

                            Expect.isFalse
                                (ManifestLoader.Validate(root)).IsValid
                                $"A {pattern} document without documentKind '{expected}' must fail validation"

                            writeJson path (original.DeepClone())
                            replaceString path "documentKind" wrong

                            Expect.isFalse
                                (ManifestLoader.Validate(root)).IsValid
                                $"A {pattern} document cannot select documentKind '{wrong}'"

                            writeJson path (original.DeepClone())
                            replaceString path "documentKind" "future-contract-v9"

                            Expect.isFalse
                                (ManifestLoader.Validate(root)).IsValid
                                $"A {pattern} document cannot select an unknown documentKind"
                        )

                let missing =
                    validateMutation
                        "schema-missing"
                        (fun _ casePath -> removeProperty casePath "schemaVersion")

                Expect.isFalse missing.IsValid "A required case field was removed"
                Expect.isNonEmpty missing.Issues "Missing fields must produce validation evidence"

                let unknown =
                    validateMutation
                        "schema-unknown"
                        (fun _ casePath ->
                            addStringProperty casePath "unexpectedField" "unexpected"
                        )

                Expect.isFalse unknown.IsValid "A closed case object received an unknown field"
                Expect.isNonEmpty unknown.Issues "Unknown fields must produce validation evidence"

            testCase "Conformance Schema discovers only case files"
            <| fun _ ->
                withCopiedRoot
                    "schema-discovery"
                    (fun root ->
                        let before = CaseDiscovery.Discover(root)
                        let casesRoot = Path.Combine(root, "cases")

                        let schemaDecoy =
                            Path.Combine(root, "schemas", "v1", "decoy.case.schema.json")

                        let lockDecoy = Path.Combine(casesRoot, "decoy.oracle-lock.json")
                        let inventoryDecoy = Path.Combine(casesRoot, "decoy.inventory.json")
                        let closureDecoy = Path.Combine(casesRoot, "decoy.closure.json")
                        File.WriteAllText(lockDecoy, "{}", UTF8Encoding(false))
                        File.WriteAllText(inventoryDecoy, "{}", UTF8Encoding(false))
                        File.WriteAllText(closureDecoy, "{}", UTF8Encoding(false))
                        File.WriteAllText(schemaDecoy, "{}", UTF8Encoding(false))
                        let after = CaseDiscovery.Discover(root)

                        Expect.sequenceEqual
                            after
                            before
                            "Non-case contracts must not enter discovery"

                        for path in after do
                            let fullPath = Path.GetFullPath(path)

                            let fullCasesRoot =
                                Path.GetFullPath(casesRoot)
                                + string Path.DirectorySeparatorChar

                            Expect.isTrue
                                (fullPath.StartsWith(
                                    fullCasesRoot,
                                    StringComparison.OrdinalIgnoreCase
                                ))
                                $"Discovered path must stay inside cases: {path}"

                            Expect.isTrue
                                (path.EndsWith(".case.json", StringComparison.Ordinal))
                                $"Discovered path must use the case suffix: {path}"

                        let oracleLock =
                            Directory.EnumerateFiles(
                                root,
                                "*.oracle-lock.json",
                                SearchOption.AllDirectories
                            )
                            |> Seq.head

                        let renamedLock =
                            Path.Combine(
                                Path.GetDirectoryName(oracleLock),
                                "renamed-lock.case.json"
                            )

                        File.Move(oracleLock, renamedLock)
                        let renamedLockResult = ManifestLoader.Validate(root)

                        Expect.isFalse
                            renamedLockResult.IsValid
                            "A lock renamed with the case suffix is a contract error"

                        Expect.isNonEmpty
                            renamedLockResult.Issues
                            "A renamed lock produces validation evidence"
                    )

            testCase "Conformance Schema rejects stale case lock and Oracle identities"
            <| fun _ ->
                let staleCase =
                    validateMutation
                        "schema-stale-case"
                        (fun root casePath ->
                            let caseDocument = readJson casePath
                            let source = caseDocument.GetProperty("sources")[0]
                            let fixturePath = source.GetProperty("fixturePath").GetString()

                            let fixture =
                                Path.Combine(
                                    root,
                                    fixturePath.Replace('/', Path.DirectorySeparatorChar)
                                )

                            File.AppendAllText(fixture, " ", UTF8Encoding(false))
                        )

                Expect.isFalse
                    staleCase.IsValid
                    "A source byte changed without updating its case hash"

                Expect.isNonEmpty
                    staleCase.Issues
                    "A stale case hash must produce validation evidence"

                withCopiedRoot
                    "schema-stale-lock"
                    (fun root ->
                        let lockPath =
                            Directory.EnumerateFiles(
                                root,
                                "*.oracle-lock.json",
                                SearchOption.AllDirectories
                            )
                            |> Seq.tryHead
                            |> Option.defaultWith (fun () ->
                                failtest "The conformance root has no Oracle lock"
                            )

                        let zeroHash =
                            "sha256:"
                            + String.replicate 64 "0"

                        replaceString lockPath "caseHash" zeroHash
                        let staleLock = ManifestLoader.Validate(root)
                        Expect.isFalse staleLock.IsValid "The Oracle lock case hash is stale"

                        Expect.isNonEmpty
                            staleLock.Issues
                            "A stale lock must produce validation evidence"
                    )

                withCopiedRoot
                    "schema-stale-oracle-identity"
                    (fun root ->
                        let lockPath =
                            Directory.EnumerateFiles(
                                root,
                                "*.oracle-lock.json",
                                SearchOption.AllDirectories
                            )
                            |> Seq.tryHead
                            |> Option.defaultWith (fun () ->
                                failtest "The conformance root has no Oracle lock"
                            )

                        mutateJson
                            lockPath
                            (fun lockDocument ->
                                let lockObject = lockDocument.AsObject()

                                let oracleIdentity = lockObject["oracleIdentity"].AsObject()

                                oracleIdentity["sdkVersion"] <- JsonValue.Create("10.0.111")
                            )

                        ManifestLoader.Validate(root)
                        |> expectInvalidWith "lock-oracle-identity"
                    )

                withCopiedRoot
                    "schema-stale-oracle-option-hash"
                    (fun root ->
                        let lockPath =
                            Directory.EnumerateFiles(
                                root,
                                "*.oracle-lock.json",
                                SearchOption.AllDirectories
                            )
                            |> Seq.tryHead
                            |> Option.defaultWith (fun () ->
                                failtest "The conformance root has no Oracle lock"
                            )

                        mutateJson
                            lockPath
                            (fun lockDocument ->
                                let lockObject = lockDocument.AsObject()

                                let oracleIdentity = lockObject["oracleIdentity"].AsObject()

                                oracleIdentity["optionHash"] <-
                                    JsonValue.Create(
                                        "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                                    )
                            )

                        ManifestLoader.Validate(root)
                        |> expectInvalidWith "lock-oracle-identity"
                    )

            testCase "Conformance Schema rejects performance schedule and statistic fields"
            <| fun _ ->
                let performance =
                    withCopiedRoot
                        "schema-performance"
                        (fun root ->
                            let manifestPath = Path.Combine(root, "manifest.json")

                            mutateJson
                                manifestPath
                                (fun node ->
                                    match node with
                                    | :? JsonObject as value ->
                                        value["performanceSchedule"] <- JsonObject()
                                        value["statistics"] <- JsonArray()
                                    | _ -> failtest "The manifest root must be an object"
                                )

                            ManifestLoader.Validate(root)
                        )

                Expect.isFalse performance.IsValid "Performance data belongs to issue #24"

                Expect.isNonEmpty
                    performance.Issues
                    "Forbidden performance fields must produce evidence"
        ]
