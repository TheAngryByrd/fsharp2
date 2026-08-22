namespace FSharp2.Conformance.Tests

open System
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Text.Json
open System.Threading
open Expecto
open FSharp2.Compiler
open FSharp2.Conformance
open FSharp2.Conformance.Tests.TestSupport

module ComparatorTests =
    type private PortablePdbPoint = {
        RawOffset: int
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

    let private portablePdbFixtureWithImport points localName importNamespace =
        let metadata = MetadataBuilder()

        let document =
            metadata.AddDocument(
                metadata.GetOrAddDocumentName("/src/Program.fs"),
                Unchecked.defaultof<GuidHandle>,
                Unchecked.defaultof<BlobHandle>,
                metadata.GetOrAddGuid(Guid.Parse("ab4f38c9-b6e6-43ba-bdcc-1c8097b3d18c"))
            )

        for point in points do
            let sequencePoints = BlobBuilder()
            sequencePoints.WriteCompressedInteger(0)
            sequencePoints.WriteCompressedInteger(point.RawOffset)

            sequencePoints.WriteCompressedInteger(
                point.EndLine
                - point.StartLine
            )

            if point.EndLine = point.StartLine then
                sequencePoints.WriteCompressedInteger(
                    point.EndColumn
                    - point.StartColumn
                )
            else
                sequencePoints.WriteCompressedSignedInteger(
                    point.EndColumn
                    - point.StartColumn
                )

            sequencePoints.WriteCompressedInteger(point.StartLine)
            sequencePoints.WriteCompressedInteger(point.StartColumn)

            metadata.AddMethodDebugInformation(document, metadata.GetOrAddBlob(sequencePoints))
            |> ignore

        let importScope =
            match importNamespace with
            | Some targetNamespace ->
                let targetNamespaceHandle = metadata.GetOrAddBlobUTF8(targetNamespace)
                let imports = BlobBuilder()
                imports.WriteCompressedInteger(int ImportDefinitionKind.ImportNamespace)

                imports.WriteCompressedInteger(MetadataTokens.GetHeapOffset(targetNamespaceHandle))

                metadata.AddImportScope(
                    Unchecked.defaultof<ImportScopeHandle>,
                    metadata.GetOrAddBlob(imports)
                )
            | None -> Unchecked.defaultof<ImportScopeHandle>

        match localName with
        | Some name ->
            let variable =
                metadata.AddLocalVariable(
                    LocalVariableAttributes.None,
                    0,
                    metadata.GetOrAddString(name)
                )

            metadata.AddLocalScope(
                MetadataTokens.MethodDefinitionHandle(1),
                importScope,
                variable,
                Unchecked.defaultof<LocalConstantHandle>,
                0,
                10
            )
            |> ignore
        | None -> ()

        let rowCounts = Array.zeroCreate MetadataTokens.TableCount
        rowCounts[int TableIndex.MethodDef] <- List.length points

        let pdb =
            PortablePdbBuilder(
                metadata,
                ImmutableArray.CreateRange(rowCounts),
                Unchecked.defaultof<MethodDefinitionHandle>,
                null
            )

        let result = BlobBuilder()

        pdb.Serialize(result)
        |> ignore

        result.ToImmutableArray()

    let private portablePdbFixture points localName =
        portablePdbFixtureWithImport points localName None

    let private portablePdb rawOffset startLine startColumn endLine endColumn =
        portablePdbFixture
            [
                {
                    RawOffset = rawOffset
                    StartLine = startLine
                    StartColumn = startColumn
                    EndLine = endLine
                    EndColumn = endColumn
                }
            ]
            None

    type private SemanticPdbMethod = {
        Name: string
        Point: PortablePdbPoint
        ImportedTypeRow: int option
    }

    type private SemanticPdbTypeReference = {
        Namespace: string
        Name: string
        ParentRow: int option
    }

    let private semanticTypeReference namespaceName name parentRow = {
        Namespace = namespaceName
        Name = name
        ParentRow = parentRow
    }

    let private semanticPortablePdbFixture
        referencedAssembly
        (methods: SemanticPdbMethod list)
        (typeReferences: SemanticPdbTypeReference list)
        =
        let assemblyMetadata = MetadataBuilder()

        assemblyMetadata.AddModule(
            0,
            assemblyMetadata.GetOrAddString("SemanticPdbFixture.dll"),
            assemblyMetadata.GetOrAddGuid(Guid.Parse("7f09f8bf-04ce-462d-a3e7-80b82fe93f53")),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        assemblyMetadata.AddAssembly(
            assemblyMetadata.GetOrAddString("SemanticPdbFixture"),
            System.Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.Sha256
        )
        |> ignore

        let assemblyReference =
            assemblyMetadata.AddAssemblyReference(
                assemblyMetadata.GetOrAddString(referencedAssembly),
                System.Version(10, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                enum<AssemblyFlags> 0,
                Unchecked.defaultof<BlobHandle>
            )

        for typeReference in typeReferences do
            let resolutionScope =
                match typeReference.ParentRow with
                | Some row -> MetadataTokens.EntityHandle(TableIndex.TypeRef, row)
                | None ->
                    MetadataTokens.EntityHandle(
                        TableIndex.AssemblyRef,
                        MetadataTokens.GetRowNumber(assemblyReference)
                    )

            assemblyMetadata.AddTypeReference(
                resolutionScope,
                assemblyMetadata.GetOrAddString(typeReference.Namespace),
                assemblyMetadata.GetOrAddString(typeReference.Name)
            )
            |> ignore

        assemblyMetadata.AddTypeDefinition(
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            assemblyMetadata.GetOrAddString("<Module>"),
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle(1),
            MetadataTokens.MethodDefinitionHandle(1)
        )
        |> ignore

        assemblyMetadata.AddTypeDefinition(
            TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.BeforeFieldInit,
            assemblyMetadata.GetOrAddString("SemanticPdbFixture"),
            assemblyMetadata.GetOrAddString("Program"),
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle(1),
            MetadataTokens.MethodDefinitionHandle(1)
        )
        |> ignore

        for method in methods do
            let signature = BlobBuilder()

            BlobEncoder(signature)
                .MethodSignature()
                .Parameters(0, (fun returnType -> returnType.Void()), (fun _ -> ()))

            assemblyMetadata.AddMethodDefinition(
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                MethodImplAttributes.IL
                ||| MethodImplAttributes.Managed,
                assemblyMetadata.GetOrAddString(method.Name),
                assemblyMetadata.GetOrAddBlob(signature),
                0,
                MetadataTokens.ParameterHandle(1)
            )
            |> ignore

        let peBuilder =
            ManagedPEBuilder(
                PEHeaderBuilder.CreateLibraryHeader(),
                MetadataRootBuilder(assemblyMetadata),
                BlobBuilder(),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly,
                null
            )

        let assembly = BlobBuilder()

        peBuilder.Serialize(assembly)
        |> ignore

        let pdbMetadata = MetadataBuilder()

        let document =
            pdbMetadata.AddDocument(
                pdbMetadata.GetOrAddDocumentName("/src/Program.fs"),
                Unchecked.defaultof<GuidHandle>,
                Unchecked.defaultof<BlobHandle>,
                pdbMetadata.GetOrAddGuid(Guid.Parse("ab4f38c9-b6e6-43ba-bdcc-1c8097b3d18c"))
            )

        for method in methods do
            let point = method.Point
            let sequencePoints = BlobBuilder()
            sequencePoints.WriteCompressedInteger(0)
            sequencePoints.WriteCompressedInteger(point.RawOffset)

            sequencePoints.WriteCompressedInteger(
                point.EndLine
                - point.StartLine
            )

            if point.EndLine = point.StartLine then
                sequencePoints.WriteCompressedInteger(
                    point.EndColumn
                    - point.StartColumn
                )
            else
                sequencePoints.WriteCompressedSignedInteger(
                    point.EndColumn
                    - point.StartColumn
                )

            sequencePoints.WriteCompressedInteger(point.StartLine)
            sequencePoints.WriteCompressedInteger(point.StartColumn)

            pdbMetadata.AddMethodDebugInformation(
                document,
                pdbMetadata.GetOrAddBlob(sequencePoints)
            )
            |> ignore

        for index, method in
            methods
            |> List.indexed do
            let importScope =
                match method.ImportedTypeRow with
                | Some row ->
                    let imports = BlobBuilder()
                    imports.WriteCompressedInteger(int ImportDefinitionKind.ImportType)

                    imports.WriteCompressedInteger(
                        CodedIndex.TypeDefOrRefOrSpec(MetadataTokens.TypeReferenceHandle(row))
                    )

                    pdbMetadata.AddImportScope(
                        Unchecked.defaultof<ImportScopeHandle>,
                        pdbMetadata.GetOrAddBlob(imports)
                    )
                | None -> Unchecked.defaultof<ImportScopeHandle>

            pdbMetadata.AddLocalScope(
                MetadataTokens.MethodDefinitionHandle(index + 1),
                importScope,
                Unchecked.defaultof<LocalVariableHandle>,
                Unchecked.defaultof<LocalConstantHandle>,
                0,
                10
            )
            |> ignore

        let rowCounts = Array.zeroCreate MetadataTokens.TableCount
        rowCounts[int TableIndex.TypeRef] <- List.length typeReferences
        rowCounts[int TableIndex.MethodDef] <- List.length methods

        let pdbBuilder =
            PortablePdbBuilder(
                pdbMetadata,
                ImmutableArray.CreateRange(rowCounts),
                Unchecked.defaultof<MethodDefinitionHandle>,
                null
            )

        let pdb = BlobBuilder()

        pdbBuilder.Serialize(pdb)
        |> ignore

        assembly.ToImmutableArray(), pdb.ToImmutableArray()

    let private metadataAssemblyWithPublicMethod parameterType =
        let metadata = MetadataBuilder()
        let moduleName = metadata.GetOrAddString("ComparatorMemberFixture.dll")

        metadata.AddModule(
            0,
            moduleName,
            metadata.GetOrAddGuid(Guid.Parse("6864c84e-4a0f-45c0-9e9d-a57a53cf0210")),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore

        metadata.AddAssembly(
            metadata.GetOrAddString("ComparatorMemberFixture"),
            System.Version(1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.Sha256
        )
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString("<Module>"),
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle(1),
            MetadataTokens.MethodDefinitionHandle(1)
        )
        |> ignore

        metadata.AddTypeDefinition(
            TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.BeforeFieldInit,
            metadata.GetOrAddString("ComparatorFixture"),
            metadata.GetOrAddString("Api"),
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle(1),
            MetadataTokens.MethodDefinitionHandle(1)
        )
        |> ignore

        let signature = BlobBuilder()

        BlobEncoder(signature)
            .MethodSignature()
            .Parameters(
                1,
                (fun returnType -> returnType.Void()),
                (fun parameters ->
                    let parameter = parameters.AddParameter().Type(false)

                    match parameterType with
                    | "string" -> parameter.String()
                    | "int32" -> parameter.Int32()
                    | value ->
                        invalidArg (nameof parameterType) $"Unsupported parameter type: {value}"
                )
            )

        metadata.AddMethodDefinition(
            MethodAttributes.Public
            ||| MethodAttributes.Static
            ||| MethodAttributes.HideBySig,
            MethodImplAttributes.IL
            ||| MethodImplAttributes.Managed,
            metadata.GetOrAddString("Transform"),
            metadata.GetOrAddBlob(signature),
            0,
            MetadataTokens.ParameterHandle(1)
        )
        |> ignore

        let peBuilder =
            ManagedPEBuilder(
                PEHeaderBuilder.CreateLibraryHeader(),
                MetadataRootBuilder(metadata),
                BlobBuilder(),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly,
                null
            )

        let result = BlobBuilder()

        peBuilder.Serialize(result)
        |> ignore

        result.ToImmutableArray()

    let private withOracleImplementation name action =
        withCopiedRoot
            name
            (fun root ->
                let runRoot = Directory.GetParent(root).FullName
                let repository = ManifestLoader.Load(root)

                let materialized =
                    CaseMaterializer.Materialize(
                        repository,
                        caseById repository "language.bindings.value-function-positive"
                    )

                let sdk = SdkSelection.Resolve(root, sdkRoot, null)
                let roots = LaneRoots.Create(runRoot, name)

                let plan = CoreCompileRunner.CreatePlan(materialized, roots, sdk, testAssemblyPath)

                let result =
                    CoreCompileRunner
                        .RunAsync(plan.Oracle, CancellationToken.None)
                        .GetAwaiter()
                        .GetResult()

                try
                    Expect.equal result.Process.ExitCode 0 "The Oracle implementation compiles"

                    let assemblyName =
                        materialized.ResolvedDocument
                            .GetProperty("project")
                            .GetProperty("assemblyName")
                            .GetString()

                    let assemblyPath =
                        Path.Combine(
                            roots.Oracle.Output,
                            assemblyName
                            + ".dll"
                        )

                    Expect.isTrue
                        (File.Exists(assemblyPath))
                        "The Oracle lane publishes the implementation assembly"

                    action repository materialized sdk assemblyPath
                finally
                    stopRecordedProcesses result.Process.Processes
            )

    let private downstreamConfiguration (sdk: SdkSelectionResult) source =
        JsonSerializer.SerializeToElement(
            {|
                dotnetPath = sdk.DotnetPath
                environment = sdk.Environment
                targetFramework = "net10.0"
                source = source
                expectedExitCode = 0
            |}
        )

    let private expectDownstreamProbe kind language source =
        withOracleImplementation
            $"probe-{kind}"
            (fun _ _ sdk assemblyPath ->
                let evidence =
                    ProbeRunner
                        .VerifyAsync(
                            ProbeRequest(
                                kind,
                                assemblyPath,
                                downstreamConfiguration sdk source,
                                TimeSpan.FromSeconds(60.0)
                            ),
                            CancellationToken.None
                        )
                        .GetAwaiter()
                        .GetResult()

                Expect.isTrue evidence.Passed $"Probe '{kind}' failed: {evidence.Difference}"
                Expect.equal evidence.Kind kind "Probe evidence retains the requested kind"
                Expect.isNull evidence.Difference "A passing downstream probe has no difference"

                let observation = evidence.Observation

                Expect.equal
                    (observation.GetProperty("language").GetString())
                    language
                    "The observation records the consumer language"

                Expect.equal
                    (observation.GetProperty("targetFramework").GetString())
                    "net10.0"
                    "The observation records the target framework"

                Expect.equal
                    (observation.GetProperty("buildExitCode").GetInt32())
                    0
                    "The downstream project builds"

                Expect.equal
                    (observation.GetProperty("runExitCode").GetInt32())
                    0
                    "The downstream consumer accepts Program.answer()"

                Expect.isFalse
                    (observation.GetProperty("timedOut").GetBoolean())
                    "The downstream consumer completes before the timeout"

                Expect.equal
                    (observation.GetProperty("standardOutput").GetString())
                    ""
                    "The downstream consumer writes no standard output"

                Expect.equal
                    (observation.GetProperty("standardError").GetString())
                    ""
                    "The downstream consumer writes no standard error"
            )

    let private compareMutation
        (comparator: JsonElement * JsonElement * JsonElement -> ComparisonResult)
        expected
        changed
        rule
        =
        let identical = comparator (expected, expected, rule)
        let mutated = comparator (expected, changed, rule)
        Expect.isTrue identical.Passed "An unchanged observation must pass"
        Expect.isFalse mutated.Passed "The declared mutation must fail"
        Expect.isNotNull mutated.Difference "A failed comparison must retain a difference"

    [<Tests>]
    let tests =
        testSequenced
        <| testList "Conformance Comparators and Probes" [
            testCase "Conformance Comparators detect exact mutations"
            <| fun _ ->
                let expected =
                    json
                        """{"code":"FS0010","severity":"error","message":"Unexpected symbol","logicalPath":"/src/Program.fs"}"""

                let changed =
                    json
                        """{"code":"FS0001","severity":"error","message":"Unexpected symbol","logicalPath":"/src/Program.fs"}"""

                let rule = json """{"id":"exact-v1","class":"exact"}"""
                compareMutation DiagnosticComparator.Compare expected changed rule

                let expectedArtifact = json """{"kind":"xml-documentation","text":"<doc />"}"""

                let changedArtifact =
                    json """{"kind":"xml-documentation","text":"<doc><member /></doc>"}"""

                compareMutation ArtifactComparator.Compare expectedArtifact changedArtifact rule

            testCase "Conformance Comparators normalize only declared path and newline rules"
            <| fun _ ->
                let expected =
                    json
                        """{"path":"/_fsharp2_conformance/root/Program.fs","text":"first\nsecond\n"}"""

                let physical =
                    json """{"path":"C:\\lane\\work\\Program.fs","text":"first\r\nsecond\r\n"}"""

                let semanticChange =
                    json """{"path":"C:\\lane\\work\\Program.fs","text":"first\r\nchanged\r\n"}"""

                let rule =
                    json
                        """{"id":"path-newline-v1","class":"normalized","pathMap":{"C:\\lane\\work":"/_fsharp2_conformance/root"},"newline":"lf"}"""

                let normalized = ArtifactComparator.Compare(expected, physical, rule)
                let changed = ArtifactComparator.Compare(expected, semanticChange, rule)
                Expect.isTrue normalized.Passed "Declared path and newline differences normalize"
                Expect.isFalse changed.Passed "Normalization must not hide a semantic text change"

            testCase "Conformance Comparators ignore token order and detect semantic changes"
            <| fun _ ->
                let ordered =
                    metadataAssembly [
                        "Alpha"
                        "Beta"
                    ]

                let reordered =
                    metadataAssembly [
                        "Beta"
                        "Alpha"
                    ]

                let changed =
                    metadataAssembly [
                        "Alpha"
                        "Gamma"
                    ]

                Expect.notEqual
                    ordered
                    reordered
                    "The fixture must change physical metadata row order"

                let tokenOnly = ManagedMetadataComparator.Compare(ordered, reordered)
                let semantic = ManagedMetadataComparator.Compare(ordered, changed)

                Expect.isTrue
                    tokenOnly.Passed
                    $"Token and row order do not define compatibility. Difference: {tokenOnly.Difference}"

                Expect.isFalse semantic.Passed "A public metadata identity change is observable"

            testCase "Conformance Comparators detect public member signature changes"
            <| fun _ ->
                let expected = metadataAssemblyWithPublicMethod "string"
                let changed = metadataAssemblyWithPublicMethod "int32"
                let comparison = ManagedMetadataComparator.Compare(expected, changed)

                Expect.notEqual
                    expected
                    changed
                    "The fixture must change the public method signature"

                Expect.isFalse comparison.Passed "A public method signature change is observable"
                Expect.isNotNull comparison.Difference "Member drift retains a semantic difference"

            testCase "Portable PDB comparison ignores raw IL offsets and preserves source mappings"
            <| fun _ ->
                let expected = portablePdb 47 10 5 10 6
                let relocated = portablePdb 49 10 5 10 6
                let changedMapping = portablePdb 49 10 5 10 7
                let rule = json """{"id":"portable-pdb-v1","class":"canonical-semantic"}"""

                Expect.notEqual expected relocated "The fixture must change the Portable PDB bytes"

                Expect.isTrue
                    (PortablePdbComparator.Compare(expected, relocated, rule).Passed)
                    "Raw IL offsets do not define Portable PDB compatibility"

                Expect.isFalse
                    (PortablePdbComparator.Compare(expected, changedMapping, rule).Passed)
                    "The 10:5-10:6 source mapping remains required"

            testCase "Conformance Comparators resolve Portable PDB identities through PE metadata"
            <| fun _ ->
                let alphaPoint = {
                    RawOffset = 47
                    StartLine = 10
                    StartColumn = 5
                    EndLine = 10
                    EndColumn = 6
                }

                let betaPoint = {
                    RawOffset = 73
                    StartLine = 20
                    StartColumn = 3
                    EndLine = 20
                    EndColumn = 8
                }

                let expectedAssembly, expectedPdb =
                    semanticPortablePdbFixture "System.Runtime" [
                        {
                            Name = "Alpha"
                            Point = alphaPoint
                            ImportedTypeRow = Some 1
                        }
                        {
                            Name = "Beta"
                            Point = betaPoint
                            ImportedTypeRow = None
                        }
                    ] [
                        semanticTypeReference "System" "String" None
                        semanticTypeReference "System" "Int32" None
                    ]

                let reorderedAssembly, reorderedPdb =
                    semanticPortablePdbFixture "System.Runtime" [
                        {
                            Name = "Beta"
                            Point = betaPoint
                            ImportedTypeRow = None
                        }
                        {
                            Name = "Alpha"
                            Point = alphaPoint
                            ImportedTypeRow = Some 2
                        }
                    ] [
                        semanticTypeReference "System" "Int32" None
                        semanticTypeReference "System" "String" None
                    ]

                let _, changedPdb =
                    semanticPortablePdbFixture "System.Runtime" [
                        {
                            Name = "Beta"
                            Point = betaPoint
                            ImportedTypeRow = None
                        }
                        {
                            Name = "Alpha"
                            Point = alphaPoint
                            ImportedTypeRow = Some 1
                        }
                    ] [
                        semanticTypeReference "System" "Int32" None
                        semanticTypeReference "System" "String" None
                    ]

                let rule = json """{"id":"portable-pdb-v1","class":"canonical-semantic"}"""

                let comparison =
                    PortablePdbComparator.Compare(
                        expectedAssembly,
                        expectedPdb,
                        reorderedAssembly,
                        reorderedPdb,
                        rule
                    )

                Expect.notEqual
                    expectedAssembly
                    reorderedAssembly
                    "The fixture must reorder PE method and type-reference rows"

                Expect.isTrue
                    comparison.Passed
                    "Raw PE method and type-reference row changes are not Portable PDB semantics"

                Expect.isFalse
                    (PortablePdbComparator
                        .Compare(expectedAssembly, expectedPdb, reorderedAssembly, changedPdb, rule)
                        .Passed)
                    "Changing Alpha from System.String to System.Int32 remains observable"

                let topLevelAssembly, topLevelPdb =
                    semanticPortablePdbFixture "FSharp.Core" [
                        {
                            Name = "TaskBuilder"
                            Point = alphaPoint
                            ImportedTypeRow = Some 1
                        }
                        {
                            Name = "QueryRun"
                            Point = betaPoint
                            ImportedTypeRow = Some 2
                        }
                    ] [
                        semanticTypeReference
                            "Microsoft.FSharp.Control.TaskBuilderExtensions"
                            "LowPriority"
                            None
                        semanticTypeReference
                            "Microsoft.FSharp.Linq.QueryRunExtensions"
                            "LowPriority"
                            None
                    ]

                let nestedAssembly, nestedPdb =
                    semanticPortablePdbFixture "FSharp.Core" [
                        {
                            Name = "TaskBuilder"
                            Point = alphaPoint
                            ImportedTypeRow = Some 2
                        }
                        {
                            Name = "QueryRun"
                            Point = betaPoint
                            ImportedTypeRow = Some 4
                        }
                    ] [
                        semanticTypeReference
                            "Microsoft.FSharp.Control"
                            "TaskBuilderExtensions"
                            None
                        semanticTypeReference "" "LowPriority" (Some 1)
                        semanticTypeReference "Microsoft.FSharp.Linq" "QueryRunExtensions" None
                        semanticTypeReference "" "LowPriority" (Some 3)
                    ]

                let importedTypeComparison =
                    PortablePdbComparator.Compare(
                        topLevelAssembly,
                        topLevelPdb,
                        nestedAssembly,
                        nestedPdb,
                        rule
                    )

                Expect.isFalse
                    importedTypeComparison.Passed
                    "Top-level dotted and nested imported TypeRefs have different semantic identities"

                Expect.isNotNull
                    importedTypeComparison.Difference
                    "The imported TypeRef mismatch retains its canonical identities"

                Expect.stringContains
                    importedTypeComparison.Difference
                    "TaskBuilderExtensions.LowPriority"
                    "The difference retains the top-level TaskBuilderExtensions identity"

                Expect.stringContains
                    importedTypeComparison.Difference
                    "TaskBuilderExtensions+LowPriority"
                    "The difference retains the nested TaskBuilderExtensions identity"

                Expect.stringContains
                    importedTypeComparison.Difference
                    "QueryRunExtensions.LowPriority"
                    "The difference retains the top-level QueryRunExtensions identity"

                Expect.stringContains
                    importedTypeComparison.Difference
                    "QueryRunExtensions+LowPriority"
                    "The difference retains the nested QueryRunExtensions identity"

            testCase "Conformance Comparators preserve Portable PDB local metadata"
            <| fun _ ->
                let point = {
                    RawOffset = 47
                    StartLine = 10
                    StartColumn = 5
                    EndLine = 10
                    EndColumn = 6
                }

                let expected = portablePdbFixture [ point ] (Some "value")
                let changed = portablePdbFixture [ point ] (Some "changed")
                let rule = json """{"id":"portable-pdb-v1","class":"canonical-semantic"}"""
                let comparison = PortablePdbComparator.Compare(expected, changed, rule)

                Expect.notEqual
                    expected
                    changed
                    "The fixture must change the local variable metadata"

                Expect.isFalse comparison.Passed "A local variable identity change is observable"

                Expect.isNotNull
                    comparison.Difference
                    "Local metadata drift retains a semantic difference"

            testCase "Conformance Comparators accept Portable PDB imports without target assemblies"
            <| fun _ ->
                let point = {
                    RawOffset = 47
                    StartLine = 10
                    StartColumn = 5
                    EndLine = 10
                    EndColumn = 6
                }

                let portablePdb =
                    portablePdbFixtureWithImport [ point ] (Some "value") (Some "System")

                let rule = json """{"id":"portable-pdb-v1","class":"canonical-semantic"}"""
                let comparison = PortablePdbComparator.Compare(portablePdb, portablePdb, rule)

                Expect.isTrue
                    comparison.Passed
                    "A valid namespace import without a target assembly compares normally"

            testCase "Conformance Comparators preserve diagnostic stream identity"
            <| fun _ ->
                let diagnostic = bytes "Program.fs(1,1): error FS0001: Type mismatch\n"
                let empty = ImmutableArray<byte>.Empty
                let rule = json """{"id":"logical-root-v1","class":"normalized"}"""

                let comparison =
                    DiagnosticComparator.CompareStreams(
                        1,
                        diagnostic,
                        empty,
                        1,
                        empty,
                        diagnostic,
                        rule
                    )

                Expect.isFalse
                    comparison.Passed
                    "Moving a diagnostic between stdout and stderr is observable"

                Expect.isNotNull comparison.Difference "Stream identity drift retains a difference"

            testCase "Conformance Comparators detect behavioral changes"
            <| fun _ ->
                let expectedValue =
                    json
                        """{"returnValue":42,"resources":{"app.resources":"sha256:resource-a"},"debugger":{"observations":[{"document":"Program.fs","line":1}]}}"""

                let expected = BehaviorObservation(0, "42\n", "", expectedValue, null)

                let changedExitCode = BehaviorObservation(1, "42\n", "", expectedValue, null)
                let changedOutput = BehaviorObservation(0, "43\n", "", expectedValue, null)
                let changedError = BehaviorObservation(0, "42\n", "warning\n", expectedValue, null)

                let changedException =
                    BehaviorObservation(
                        0,
                        "42\n",
                        "",
                        expectedValue,
                        "System.InvalidOperationException"
                    )

                let changedReturnValue =
                    BehaviorObservation(
                        0,
                        "42\n",
                        "",
                        json
                            """{"returnValue":43,"resources":{"app.resources":"sha256:resource-a"},"debugger":{"observations":[{"document":"Program.fs","line":1}]}}""",
                        null
                    )

                let changedResources =
                    BehaviorObservation(
                        0,
                        "42\n",
                        "",
                        json
                            """{"returnValue":42,"resources":{"app.resources":"sha256:resource-b"},"debugger":{"observations":[{"document":"Program.fs","line":1}]}}""",
                        null
                    )

                let changedDebugger =
                    BehaviorObservation(
                        0,
                        "42\n",
                        "",
                        json
                            """{"returnValue":42,"resources":{"app.resources":"sha256:resource-a"},"debugger":{"observations":[{"document":"Program.fs","line":2}]}}""",
                        null
                    )

                let expectObservedChange expectedField actual =
                    let comparison = BehaviorComparator.Compare(expected, actual)
                    Expect.isFalse comparison.Passed $"A change to {expectedField} is observable"

                    Expect.stringContains
                        comparison.Difference
                        expectedField
                        $"The difference identifies {expectedField}"

                Expect.isTrue
                    (BehaviorComparator.Compare(expected, expected).Passed)
                    "An unchanged runtime observation passes"

                expectObservedChange "exit code" changedExitCode
                expectObservedChange "standard output" changedOutput
                expectObservedChange "standard error" changedError
                expectObservedChange "return value" changedReturnValue
                expectObservedChange "exception" changedException
                expectObservedChange "resources" changedResources
                expectObservedChange "debugger observations" changedDebugger

            physicalLaneTestCase "Conformance Comparators detect within-compiler byte changes"
            <| fun _ ->
                let first = bytes "deterministic-artifact"
                let second = bytes "deterministic-artifact"
                let changed = bytes "deterministic-artifacU"

                Expect.isTrue
                    (DeterminismComparator.Compare(first, second).Passed)
                    "A deterministic repeat is byte-identical"

                Expect.isFalse
                    (DeterminismComparator.Compare(first, changed).Passed)
                    "One repeat byte changed"

                withCopiedRoot
                    "repeat"
                    (fun root ->
                        removeCaseProbe
                            root
                            "language.bindings.value-function-positive"
                            "managed-load"

                        let runRoot = Directory.GetParent(root).FullName
                        let outputRoot = Path.Combine(runRoot, "runs")
                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById repository "language.bindings.value-function-positive"
                            )

                        let exitCode =
                            ConformanceRunner
                                .RunAsync(
                                    immutableDictionary [
                                        "root", root
                                        "case", materialized.CaseId
                                        "dotnet-root", sdkRoot
                                        "fsharp2-host", fsharp2CompilerHostPath ()
                                        "output-root", outputRoot
                                    ],
                                    CancellationToken.None
                                )
                                .GetAwaiter()
                                .GetResult()

                        GC.Collect()
                        GC.WaitForPendingFinalizers()
                        GC.Collect()

                        Expect.equal exitCode 0 "The positive public run passes"

                        let emittedRunRoots = Directory.GetDirectories(outputRoot)
                        Expect.hasLength emittedRunRoots 1 "The public run emits one run root"
                        let bundleRoot = Path.Combine(emittedRunRoots[0], "bundle")

                        use runResult =
                            JsonDocument.Parse(
                                File.ReadAllBytes(Path.Combine(bundleRoot, "run-result.json"))
                            )

                        use comparison =
                            JsonDocument.Parse(
                                File.ReadAllBytes(Path.Combine(bundleRoot, "comparison.json"))
                            )

                        let repeatComparisons =
                            comparison.RootElement.GetProperty("comparisons").EnumerateArray()
                            |> Seq.filter (fun item ->
                                String.Equals(
                                    item.GetProperty("comparatorId").GetString(),
                                    "artifact-repeat",
                                    StringComparison.Ordinal
                                )
                            )
                            |> Seq.toArray

                        Expect.hasLength
                            repeatComparisons
                            (materialized.RequestedArtifacts.Length
                             * 2)
                            "The public runner compares every requested artifact for both compiler repeats"

                        let packageLocks =
                            runResult.RootElement
                                .GetProperty("inputs")
                                .GetProperty("packageLocks")
                                .EnumerateArray()
                            |> Seq.toArray

                        Expect.hasLength
                            packageLocks
                            4
                            "The run result retains both package inputs for both lanes"

                        let expectedPackagePaths = [|
                            "lanes/oracle/work/packages.lock.json"
                            "lanes/oracle/obj/project.assets.json"
                            "lanes/fsharp2/work/packages.lock.json"
                            "lanes/fsharp2/obj/project.assets.json"
                        |]

                        let packagePaths =
                            packageLocks
                            |> Array.map (fun item ->
                                item.GetProperty("id").GetString().Replace('\\', '/')
                            )

                        Expect.sequenceEqual
                            packagePaths
                            expectedPackagePaths
                            "Package input facts have stable lane and file order"

                        for item in packageLocks do
                            Expect.stringStarts
                                (item.GetProperty("sha256").GetString())
                                "sha256:"
                                "Each package input has a SHA-256 hash"

                        let comparisonEvidence =
                            runResult.RootElement.GetProperty("comparisons").EnumerateArray()
                            |> Seq.toArray

                        let comparatorDefinitions =
                            repository.ComparisonPolicy
                                .GetProperty("comparatorVersions")
                                .EnumerateArray()
                            |> Seq.toArray

                        let normalizationDefinitions =
                            repository.ComparisonPolicy
                                .GetProperty("normalizations")
                                .EnumerateArray()
                            |> Seq.toArray

                        for item in comparisonEvidence do
                            let comparatorId = item.GetProperty("comparatorId").GetString()
                            let version = item.GetProperty("version").GetInt32()
                            let comparisonClass = item.GetProperty("class").GetString()
                            let rule = item.GetProperty("rule").GetString()

                            let comparatorIsDeclared =
                                comparatorDefinitions
                                |> Array.exists (fun definition ->
                                    String.Equals(
                                        definition.GetProperty("id").GetString(),
                                        comparatorId,
                                        StringComparison.Ordinal
                                    )
                                    && definition.GetProperty("version").GetInt32() = version
                                    && String.Equals(
                                        definition.GetProperty("class").GetString(),
                                        comparisonClass,
                                        StringComparison.Ordinal
                                    )
                                )

                            let ruleIsDeclared =
                                not (
                                    String.Equals(
                                        comparisonClass,
                                        "normalized",
                                        StringComparison.Ordinal
                                    )
                                )
                                || (normalizationDefinitions
                                    |> Array.exists (fun definition ->
                                        String.Equals(
                                            definition.GetProperty("id").GetString(),
                                            rule,
                                            StringComparison.Ordinal
                                        )
                                        && definition.GetProperty("version").GetInt32() = version
                                    ))

                            Expect.isTrue
                                (comparatorIsDeclared
                                 && ruleIsDeclared)
                                $"Emitted comparator '{comparatorId}' v{version} class '{comparisonClass}' rule '{rule}' resolves exactly in comparison-policy.v1.json"

                            let rawHashes =
                                item.GetProperty("rawHashes").EnumerateArray()
                                |> Seq.map (fun value -> value.GetString())
                                |> Seq.toArray

                            Expect.hasLength
                                rawHashes
                                2
                                "Comparison evidence retains expected and actual raw hashes"

                            for rawHash in rawHashes do
                                Expect.stringStarts
                                    rawHash
                                    "sha256:"
                                    "Each comparison raw hash uses the SHA-256 domain"

                            let canonicalValues =
                                item.GetProperty("canonicalValues").EnumerateArray()
                                |> Seq.map (fun value -> value.GetString())
                                |> Seq.toArray

                            Expect.hasLength
                                canonicalValues
                                2
                                "Comparison evidence retains expected and actual canonical values"

                            Expect.isFalse
                                (canonicalValues
                                 |> Array.exists (fun value ->
                                     String.Equals(value, "equal", StringComparison.Ordinal)
                                     || String.Equals(
                                         value,
                                         "different",
                                         StringComparison.Ordinal
                                     )
                                 ))
                                "Comparison evidence retains values instead of a verdict marker"

                        let restoredDependencyComparison =
                            comparisonEvidence
                            |> Array.find (fun item ->
                                item.GetProperty("canonicalValues").EnumerateArray()
                                |> Seq.exists (fun value ->
                                    let text = value.GetString()

                                    text.Contains("\"packageLock\"", StringComparison.Ordinal)
                                    && text.Contains(
                                        "\"projectAssets\"",
                                        StringComparison.Ordinal
                                    )
                                )
                            )

                        let restoredDependencyTuple = [|
                            restoredDependencyComparison.GetProperty("comparatorId").GetString()
                            string (restoredDependencyComparison.GetProperty("version").GetInt32())
                            restoredDependencyComparison.GetProperty("class").GetString()
                            restoredDependencyComparison.GetProperty("rule").GetString()
                        |]

                        Expect.sequenceEqual
                            restoredDependencyTuple
                            [|
                                "path-newline-normalized"
                                "1"
                                "normalized"
                                "logical-root-v1"
                            |]
                            "Restored dependency evidence uses the declared normalized comparator"

                        let artifactFile kind =
                            match kind with
                            | "implementation-assembly" -> "implementation.dll"
                            | "portable-pdb" -> "portable.pdb"
                            | "reference-assembly" -> "reference.dll"
                            | "xml-documentation" -> "documentation.xml"
                            | value -> failtest $"No repeat artifact mapping exists for '{value}'"

                        for lane in
                            [
                                "oracle"
                                "fsharp2"
                            ] do
                            for artifact in materialized.RequestedArtifacts do
                                let path =
                                    Path.Combine(
                                        bundleRoot,
                                        "artifacts",
                                        lane
                                        + "-repeat",
                                        artifactFile artifact
                                    )

                                Expect.isTrue
                                    (File.Exists(path))
                                    $"The bundle retains the {lane} repeat for requested artifact '{artifact}'"
                    )

            testCase "Conformance Probes verify IL load API metadata PDB and runtime behavior"
            <| fun _ ->
                let pdbPath = Path.ChangeExtension(testAssemblyPath, ".pdb")
                Expect.isTrue (File.Exists(pdbPath)) "The test assembly has a Portable PDB"

                let requests = [
                    ProbeRequest(
                        "ilverify",
                        testAssemblyPath,
                        json "{}",
                        TimeSpan.FromSeconds(30.0)
                    )
                    ProbeRequest(
                        "managed-load",
                        testAssemblyPath,
                        json "{}",
                        TimeSpan.FromSeconds(30.0)
                    )
                    ProbeRequest(
                        "public-api",
                        testAssemblyPath,
                        json "{}",
                        TimeSpan.FromSeconds(30.0)
                    )
                    ProbeRequest(
                        "metadata",
                        testAssemblyPath,
                        json "{}",
                        TimeSpan.FromSeconds(30.0)
                    )
                    ProbeRequest("portable-pdb", pdbPath, json "{}", TimeSpan.FromSeconds(30.0))
                    ProbeRequest(
                        "runtime",
                        testAssemblyPath,
                        json
                            $"""{{"dotnetPath":{System.Text.Json.JsonSerializer.Serialize(dotnetPath)},"arguments":["--conformance-probe"],"expectedExitCode":0,"expectedStdout":"probe-ok\\n","expectedStderr":""}}""",
                        TimeSpan.FromSeconds(30.0)
                    )
                ]

                for request in requests do
                    let evidence =
                        ProbeRunner
                            .VerifyAsync(request, CancellationToken.None)
                            .GetAwaiter()
                            .GetResult()

                    Expect.isTrue
                        evidence.Passed
                        $"Probe '{request.Kind}' failed: {evidence.Difference}"

                    Expect.equal
                        evidence.Kind
                        request.Kind
                        "Probe evidence retains the requested kind"
        ]

    [<Tests>]
    let downstreamConsumerTests =
        testSequenced
        <| testList "Conformance Downstream Consumer Probes" [
            testCase "ProbeRunner executes a downstream F# consumer"
            <| fun _ ->
                expectDownstreamProbe
                    "downstream-fsharp"
                    "fsharp"
                    """module Consumer

[<EntryPoint>]
let main _ =
    if Program.answer () = 42 then 0 else 1
"""

            testCase "ProbeRunner executes a downstream C# consumer"
            <| fun _ ->
                expectDownstreamProbe
                    "downstream-csharp"
                    "csharp"
                    """internal static class Consumer
{
    private static int Main() => global::Program.answer() == 42 ? 0 : 1;
}
"""

            testCase "RunResultWriter records downstream consumer observation hashes"
            <| fun _ ->
                withCopiedRoot
                    "downstream-consumer-hashes"
                    (fun root ->
                        let repository = ManifestLoader.Load(root)

                        let materialized =
                            CaseMaterializer.Materialize(
                                repository,
                                caseById repository "language.bindings.value-function-positive"
                            )

                        let sdk = SdkSelection.Resolve(root, sdkRoot, null)
                        let request = CompilerContractProbe.CreateRequest(materialized)
                        let compilation = Compiler().Compile(request, CancellationToken.None)

                        Expect.equal
                            compilation.Outcome
                            CompilationOutcome.Succeeded
                            "The real direct compilation succeeds"

                        let coreEvidence = CoreEvidenceWriter.Create(compilation)

                        let fsharpObservation =
                            json
                                """{"language":"fsharp","targetFramework":"net10.0","buildExitCode":0,"runExitCode":0,"timedOut":false,"standardOutput":"","standardError":""}"""

                        let csharpObservation =
                            json
                                """{"language":"csharp","targetFramework":"net10.0","buildExitCode":0,"runExitCode":0,"timedOut":false,"standardOutput":"","standardError":""}"""

                        let probes =
                            immutableArray [
                                ProbeEvidence("downstream-fsharp", true, fsharpObservation, null)
                                ProbeEvidence("downstream-csharp", true, csharpObservation, null)
                            ]

                        let verdict =
                            VerdictResult(
                                ConformanceVerdict.Pass,
                                ImmutableArray<string>.Empty,
                                ImmutableArray<string>.Empty,
                                true
                            )

                        let runResult =
                            RunResultWriter.Create(
                                "run-downstream-consumer-hashes",
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
                                probes,
                                ImmutableArray<ComparisonResult>.Empty,
                                verdict,
                                ImmutableDictionary<string, ImmutableArray<byte>>.Empty
                            )

                        let consumerHashes =
                            runResult
                                .GetProperty("observations")
                                .GetProperty("consumer")
                                .EnumerateArray()
                            |> Seq.map (fun entry ->
                                entry.GetProperty("id").GetString(),
                                entry.GetProperty("sha256").GetString()
                            )
                            |> Map.ofSeq

                        Expect.equal
                            consumerHashes.Count
                            probes.Length
                            "Every downstream observation has one consumer hash"

                        probes
                        |> Seq.iteri (fun index probe ->
                            let id = $"{probe.Kind}:{index}"

                            Expect.isTrue
                                (consumerHashes.ContainsKey(id))
                                $"Consumer hash '{id}' exists"

                            Expect.equal
                                consumerHashes[id]
                                (Hashing.Sha256(CanonicalJson.Canonicalize(probe.Observation)))
                                $"Consumer hash '{id}' covers its observation"
                        )
                    )
        ]
