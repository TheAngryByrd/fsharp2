namespace FSharp2.Conformance.Tests

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Xml.Linq
open Expecto
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module MaterializationTests =
    let private materialize root caseId =
        let repository = ManifestLoader.Load(root)
        let conformanceCase = caseById repository caseId
        CaseMaterializer.Materialize(repository, conformanceCase)

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Materialization" [
            physicalLaneTestCase "Conformance Materialization preserves source and signature order"
            <| fun _ ->
                withCopiedRoot
                    "materialization-source-order"
                    (fun root ->
                        let materialized = materialize root "language.signatures.contract-positive"

                        materialized.Sources
                        |> Seq.map (fun source -> source.StableId)
                        |> fun actual ->
                            Expect.sequenceEqual
                                actual
                                [
                                    "source.public-api-signature"
                                    "source.public-api-implementation"
                                ]
                                "The signature must precede its implementation"

                        materialized.Sources
                        |> Seq.map (fun source -> source.LogicalPath)
                        |> fun actual ->
                            Expect.sequenceEqual
                                actual
                                [
                                    "/src/PublicApi.fsi"
                                    "/src/PublicApi.fs"
                                ]
                                "Logical source order must be path-free and stable"

                        for source in materialized.Sources do
                            Expect.isNonEmpty
                                source.Bytes
                                $"Source '{source.StableId}' must retain immutable bytes"

                            Expect.isTrue
                                (source.Sha256.StartsWith("sha256:", StringComparison.Ordinal))
                                "Source hashes use the declared format"

                        let repository = ManifestLoader.Load(root)

                        let orderedCase =
                            caseById repository "language.files.ordered-modules-positive"

                        let reversedDocument =
                            JsonNode.Parse(orderedCase.Document.GetRawText()).AsObject()

                        let reversedSources = reversedDocument["sources"].AsArray()
                        let firstSource = reversedSources[0].AsObject()
                        let secondSource = reversedSources[1].AsObject()
                        let firstOrder = (firstSource["order"]).GetValue<int>()
                        let secondOrder = (secondSource["order"]).GetValue<int>()
                        firstSource["order"] <- JsonValue.Create(secondOrder)
                        secondSource["order"] <- JsonValue.Create(firstOrder)

                        let reversedCase =
                            ConformanceCase(
                                orderedCase.CaseId,
                                orderedCase.FilePath,
                                orderedCase.Tags,
                                jsonNodeToElement reversedDocument
                            )

                        let reversed = CaseMaterializer.Materialize(repository, reversedCase)
                        let runRoot = Directory.GetParent(root).FullName
                        let reversedRunId = "reversed-source-order"

                        let roots =
                            LaneRoots.Create(Path.Combine(runRoot, reversedRunId), reversedRunId)

                        let sdk = SdkSelection.Resolve(root, sdkRoot, null)

                        let plan =
                            CoreCompileRunner.CreatePlan(
                                reversed,
                                roots,
                                sdk,
                                fsharp2CompilerHostPath ()
                            )

                        let result =
                            CoreCompileRunner
                                .RunAsync(plan.Oracle, CancellationToken.None)
                                .GetAwaiter()
                                .GetResult()

                        try
                            Expect.notEqual
                                result.Process.ExitCode
                                0
                                "Reversed source order fails the positive case"

                            Expect.equal
                                (CoreCompileRunner.Classify(result.Process))
                                ConformanceVerdict.Fail
                                "Reversed source order has a fail verdict"
                        finally
                            stopRecordedProcesses result.Process.Processes
                    )

            testCase "Conformance Materialization captures ordered target references"
            <| fun _ ->
                withCopiedRoot
                    "materialization-reference-order"
                    (fun root ->
                        let materialized =
                            materialize root "language.files.ordered-modules-positive"

                        materialized.TargetReferences
                        |> Seq.map (fun reference -> reference.StableId)
                        |> fun actual ->
                            Expect.sequenceEqual
                                actual
                                [
                                    "reference.netstandard"
                                    "reference.system-runtime"
                                    "reference.fsharp-core"
                                ]
                                "Target-reference closure order must match the reviewed closure"

                        materialized.TargetReferences
                        |> Seq.map (fun reference -> reference.EvaluatedItemSource)
                        |> fun actual ->
                            Expect.sequenceEqual
                                actual
                                [
                                    "ReferencePath"
                                    "ReferencePath"
                                    "ReferencePath"
                                ]
                                "Each reference records its evaluated MSBuild item source"

                        for reference in materialized.TargetReferences do
                            Expect.isNonEmpty
                                reference.Bytes
                                $"Target reference '{reference.StableId}' must retain immutable PE bytes"

                            Expect.isTrue
                                (reference.Sha256.StartsWith("sha256:", StringComparison.Ordinal))
                                "Reference hashes use the declared format"

                        let closurePath =
                            Path.Combine(root, "expectations", "files", "net10.0.closure.json")

                        mutateJson
                            closurePath
                            (fun document ->
                                let documentObject = document.AsObject()
                                let references = (documentObject["references"]).AsArray()
                                let firstReference = references[0].AsObject()

                                firstReference["sha256"] <-
                                    JsonValue.Create(
                                        "sha256:"
                                        + String.replicate 64 "0"
                                    )
                            )

                        let changedReference =
                            try
                                materialize root "language.files.ordered-modules-positive"
                                |> ignore

                                None
                            with :? ConformanceContractException as error ->
                                Some error

                        Expect.isSome
                            changedReference
                            "A changed target-reference hash fails before compilation"

                        Expect.isTrue
                            (changedReference.Value.Issues
                             |> Seq.exists (fun issue -> issue.Code = "reference-hash"))
                            "A changed target-reference hash reports reference-hash"
                    )

            testCase "Conformance Materialization normalizes all option groups"
            <| fun _ ->
                withCopiedRoot
                    "materialization-options"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        let conformanceCase =
                            caseById repository "language.bindings.value-function-positive"

                        let options = conformanceCase.Document.GetProperty("options")
                        let normalized = OptionNormalizer.Normalize(options)

                        let materialized =
                            CaseMaterializer.Materialize(repository, conformanceCase)

                        Expect.equal
                            (normalized.Semantic.GetProperty("languageVersion").GetString())
                            "default"
                            "Semantic aliases normalize to one language-version value"

                        Expect.equal
                            (normalized.Semantic.GetProperty("optimization").GetString())
                            "disabled"
                            "Optimization mode is normalized"

                        Expect.equal
                            (normalized.Diagnostic.GetProperty("warningLevel").GetInt32())
                            5
                            "Diagnostic options retain the warning level"

                        Expect.isTrue
                            (normalized.Emission.GetProperty("deterministic").GetBoolean())
                            "Emission options retain deterministic output"

                        Expect.equal
                            (normalized.Signing.GetProperty("mode").GetString())
                            "none"
                            "Signing options retain the unsigned mode"

                        Expect.equal
                            (normalized.Resources.GetProperty("embedded").GetArrayLength())
                            0
                            "Resource options retain an empty embedded list"

                        Expect.sequenceEqual
                            normalized.CompilerArguments
                            [
                                "--debug:portable"
                                "--deterministic+"
                                "--fullpaths"
                                "--flaterrors"
                                "--consolecolors-"
                                "--optimize-"
                                "--tailcalls-"
                            ]
                            "Compiler arguments preserve their normalized semantic order"

                        let completeOptions =
                            NormalizedOptionGroups(
                                json
                                    """{"languageVersion":"preview","defines":["TRACE","CUSTOM"],"optimization":"enabled","tailCalls":"enabled","checkedArithmetic":true,"targetProfile":"netstandard"}""",
                                json
                                    """{"warningLevel":4,"noWarn":["FS0020"],"warnAsError":["FS1182"],"warnAsWarn":["FS0044"],"fullPaths":true,"flatErrors":true,"consoleColors":true,"culture":"fr-FR"}""",
                                json
                                    """{"debugFormat":"embedded","deterministic":false,"documentation":true,"referenceAssembly":true,"outputType":"library","pathMap":[{"name":"physical-root","value":"/_mapped/root"}]}""",
                                json
                                    """{"mode":"delay","keyFile":"keys/compiler.snk","publicSign":false}""",
                                json
                                    """{"embedded":["resources/embedded.resources"],"linked":["resources/linked.resources"],"native":["resources/native.res"]}""",
                                immutableArray [ "--custom-compiler-option:value" ]
                            )

                        let completeCase =
                            MaterializedCase(
                                conformanceCase.CaseId,
                                materialized.Sources,
                                materialized.TargetReferences,
                                completeOptions,
                                materialized.RequestedArtifacts,
                                materialized.ResolvedDocument
                            )

                        let project =
                            MsBuildProjectWriter.CreateProjectBytes(completeCase)
                            |> Encoding.UTF8.GetString
                            |> XDocument.Parse

                        let property name =
                            let values =
                                project.Descendants(XName.Get(name))
                                |> Seq.map (fun element -> element.Value)
                                |> Seq.toArray

                            Expect.hasLength
                                values
                                1
                                $"The generated project must contain one '{name}' property"

                            values[0]

                        for name, value in
                            [
                                "LangVersion", "preview"
                                "DefineConstants", "TRACE;CUSTOM"
                                "Optimize", "true"
                                "Tailcalls", "true"
                                "TargetProfile", "netstandard"
                                "WarningLevel", "4"
                                "NoWarn", "FS0020"
                                "WarningsAsErrors", "FS1182"
                                "WarningsNotAsErrors", "FS0044"
                                "PreferredUILang", "fr-FR"
                                "DebugType", "embedded"
                                "Deterministic", "false"
                                "GenerateDocumentationFile", "true"
                                "ProduceReferenceAssembly", "true"
                                "OutputType", "Library"
                                "SignAssembly", "true"
                                "DelaySign", "true"
                                "PublicSign", "false"
                                "AssemblyOriginatorKeyFile", "keys/compiler.snk"
                                "Win32Resource", "resources/native.res"
                            ] do
                            Expect.equal
                                (property name)
                                value
                                $"The generated project applies normalized option '{name}'"

                        Expect.equal
                            (property "PathMap")
                            ".=/_mapped/root"
                            "The generated project applies root-neutral path maps"

                        let otherFlags = property "OtherFlags"

                        for flag in
                            [
                                "--checked+"
                                "--fullpaths"
                                "--flaterrors"
                                "--consolecolors+"
                                "--linkresource:resources/linked.resources"
                                "--custom-compiler-option:value"
                            ] do
                            Expect.stringContains
                                otherFlags
                                flag
                                $"The generated project applies normalized compiler flag '{flag}'"

                        let embeddedResources =
                            project.Descendants(XName.Get("EmbeddedResource"))
                            |> Seq.map (fun element ->
                                element.Attribute(XName.Get("Include")).Value
                            )

                        Expect.contains
                            embeddedResources
                            "resources/embedded.resources"
                            "The generated project applies normalized embedded resources"
                    )
        ]
