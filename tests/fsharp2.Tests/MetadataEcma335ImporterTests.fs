namespace FSharp2.Tests

open System
open System.Reflection
open Expecto
open FSharp2.Compiler

module MetadataEcma335ImporterTests =
    let private import fixture =
        fixture
        |> MetadataReferenceFixtures.request
        |> Ecma335ReferenceImporter.import

    let private expectImported fixture =
        match import fixture with
        | Ok [ imported ] -> imported
        | Ok imported ->
            failtestf
                "Fixture '%s' produced %d imported snapshots instead of one."
                fixture.Identity
                imported.Length
        | Error errors -> failtestf "Fixture '%s' failed with %A." fixture.Identity errors

    let private expectSnapshotSemantics (snapshot: ReferenceEcmaSnapshot) =
        match snapshot.Semantics.Get(Threading.CancellationToken.None) with
        | Ok semantics -> semantics
        | Error error ->
            failtestf "Fixture '%s' semantic demand failed with %A." snapshot.StableId error

    let private expectTypeSemantics (definition: ReferenceTypeDefinition) =
        match definition.Semantics.Get(Threading.CancellationToken.None) with
        | Ok semantics -> semantics
        | Error error ->
            failtestf
                "Type '%s' semantic demand failed with %A."
                definition.Identity.MetadataName
                error

    let private expectImportedMany fixtures =
        match
            fixtures
            |> MetadataReferenceFixtures.requestMany
            |> Ecma335ReferenceImporter.import
        with
        | Ok snapshots -> snapshots
        | Error errors -> failtestf "Fixture universe failed with %A." errors

    let private expectSingleError expectedKind expectedIdentity request =
        match Ecma335ReferenceImporter.import request with
        | Error [ error ] ->
            Expect.equal error.Kind expectedKind "The import error kind must be exact."

            Expect.equal
                error.ReferenceStableId
                expectedIdentity
                "The import error stable identity must be exact."

            error
        | result -> failtestf "Fixture '%s' returned %A." expectedIdentity result

    let private expectForwarderResolution (exportedType: ReferenceExportedType) =
        match exportedType.Resolution.Get(Threading.CancellationToken.None) with
        | Ok resolution -> resolution
        | Error error -> failtestf "Forwarder demand failed with %A." error

    let private expectForwarderError
        expectedKind
        expectedIdentity
        (exportedType: ReferenceExportedType)
        =
        match exportedType.Resolution.Get(Threading.CancellationToken.None) with
        | Error error ->
            Expect.equal error.Kind expectedKind "The forwarder error kind must be exact."

            Expect.equal
                error.ReferenceStableId
                expectedIdentity
                "The forwarder error stable identity must be exact."

            let repeated = exportedType.Resolution.Get(Threading.CancellationToken.None)
            Expect.equal repeated (Error error) "The forwarder failure must be memoized."
        | Ok resolution -> failtestf "Forwarder demand returned %A." resolution

    [<Tests>]
    let tests =
        testList "ECMA-335 metadata reference importer" [
            testCase "issue30.ecma.identity imports assembly, module, and scope identity"
            <| fun _ ->
                let identityFixture = MetadataReferenceFixtures.identityAssembly ()
                let imported = expectImported identityFixture

                Expect.isSome imported.Assembly "The fixture must import an assembly identity."

                Expect.equal
                    imported.Module.Name
                    "Issue30.Identity.dll"
                    "The module name must be exact."

                Expect.hasLength
                    imported.AssemblyReferences
                    1
                    "The assembly reference must be present."

                Expect.hasLength imported.ModuleReferences 1 "The module reference must be present."
                Expect.hasLength imported.Files 1 "The file scope must be present."

                let assembly =
                    imported.Assembly
                    |> Option.get

                Expect.equal assembly.Name "Issue30.Identity" "The assembly name must be exact."

                Expect.equal
                    assembly.Version
                    {
                        Major = 2
                        Minor = 3
                        Build = 4
                        Revision = 5
                    }
                    "The complete assembly version must be preserved."

                Expect.equal
                    assembly.Culture
                    (Some "en-US")
                    "The assembly culture must be preserved."

                Expect.equal
                    assembly.Flags
                    (int AssemblyFlags.PublicKey)
                    "Assembly flags must be preserved."

                Expect.equal
                    assembly.HashAlgorithm
                    (uint32 AssemblyHashAlgorithm.Sha256)
                    "The assembly hash algorithm must be preserved."

                Expect.sequenceEqual
                    assembly.PublicKey
                    [
                        1uy
                        2uy
                        3uy
                        4uy
                    ]
                    "The public key must be exact."

                Expect.hasLength assembly.PublicKeyToken 8 "The public-key token must be derived."
                Expect.equal imported.Module.Generation 7 "The module generation must be exact."

                Expect.equal
                    imported.Module.ModuleVersionId
                    (Guid.Parse("11111111-2222-3333-4444-555555555555"))
                    "The MVID must be exact."

                Expect.equal
                    imported.Module.GenerationId
                    (Some(Guid.Parse("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")))
                    "The generation ID must be exact."

                Expect.equal
                    imported.Module.BaseGenerationId
                    (Some(Guid.Parse("01234567-89ab-cdef-0123-456789abcdef")))
                    "The base generation ID must be exact."

                let assemblyReference = imported.AssemblyReferences.Head

                Expect.equal
                    assemblyReference.Identity.Name
                    "Issue30.Dependency"
                    "AssemblyRef name must be exact."

                Expect.equal
                    assemblyReference.HashValue
                    [
                        18uy
                        19uy
                    ]
                    "AssemblyRef hash must be exact."

                Expect.equal
                    assemblyReference.Identity.Version
                    {
                        Major = 6
                        Minor = 7
                        Build = 8
                        Revision = 9
                    }
                    "AssemblyRef version must be exact."

                Expect.equal
                    assemblyReference.Identity.Culture
                    (Some "fr-FR")
                    "AssemblyRef culture must be exact."

                Expect.equal assemblyReference.Identity.Flags 0 "AssemblyRef flags must be exact."

                Expect.isEmpty
                    assemblyReference.Identity.PublicKey
                    "A token-only AssemblyRef must not synthesize a public key."

                Expect.sequenceEqual
                    assemblyReference.Identity.PublicKeyToken
                    [
                        10uy
                        11uy
                        12uy
                        13uy
                        14uy
                        15uy
                        16uy
                        17uy
                    ]
                    "AssemblyRef public-key token must be exact."

                Expect.equal
                    imported.ModuleReferences.Head.Name
                    "Issue30.Native"
                    "ModuleRef name must be exact."

                Expect.equal
                    imported.Files.Head.Name
                    "Issue30.Part.netmodule"
                    "File name must be exact."

                Expect.isTrue
                    imported.Files.Head.ContainsMetadata
                    "The file must retain its metadata flag."

                Expect.sequenceEqual
                    imported.Files.Head.HashValue
                    [
                        20uy
                        21uy
                        22uy
                    ]
                    "The file hash must be exact."

                Expect.equal
                    imported.ContentFingerprint
                    identityFixture.Snapshot.ContentFingerprint
                    "The supplied content fingerprint must be preserved."

                let samePathContent =
                    let fixture = MetadataReferenceFixtures.semanticAssembly ()

                    {
                        fixture with
                            Snapshot = {
                                fixture.Snapshot with
                                    LogicalPath = identityFixture.Snapshot.LogicalPath
                            }
                    }

                let changed = expectImported samePathContent

                Expect.equal
                    changed.LogicalPath
                    imported.LogicalPath
                    "The comparison fixture must use the same logical path."

                Expect.notEqual
                    changed.ContentFingerprint
                    imported.ContentFingerprint
                    "Same-path content changes must produce a different fingerprint."

                let netmodule =
                    MetadataReferenceFixtures.netmodule ()
                    |> expectImported

                Expect.isNone netmodule.Assembly "A netmodule must not synthesize an Assembly row."

            testCase "issue30.ecma.nesting-accessibility preserves access facts"
            <| fun _ ->
                let imported =
                    MetadataReferenceFixtures.semanticAssembly ()
                    |> expectImported

                Expect.isNonEmpty imported.TypeDefinitions "Type definitions must be imported."

                imported.TypeDefinitions
                |> List.exists (fun definition -> not definition.Identity.EnclosingTypes.IsEmpty)
                |> fun found ->
                    Expect.isTrue found "Nested type identities must preserve their enclosing path."

                let definition name =
                    imported.TypeDefinitions
                    |> List.find (fun item -> item.Identity.MetadataName = name)

                Expect.equal
                    (definition "IContract").Accessibility
                    ReferenceTypeAccessibility.Public
                    "Public top-level accessibility must be exact."

                Expect.equal
                    (definition "GenericType`1").Accessibility
                    ReferenceTypeAccessibility.NotPublic
                    "Internal top-level accessibility must be exact."

                [
                    "Nested0", ReferenceTypeAccessibility.NestedPublic
                    "Nested1", ReferenceTypeAccessibility.NestedPrivate
                    "Nested2", ReferenceTypeAccessibility.NestedFamily
                    "Nested3", ReferenceTypeAccessibility.NestedAssembly
                    "Nested4", ReferenceTypeAccessibility.NestedFamilyAndAssembly
                    "Nested5", ReferenceTypeAccessibility.NestedFamilyOrAssembly
                ]
                |> List.iter (fun (name, accessibility) ->
                    let nested = definition name

                    Expect.equal
                        nested.Accessibility
                        accessibility
                        $"Nested accessibility for '{name}' must be exact."

                    Expect.sequenceEqual
                        nested.Identity.EnclosingTypes
                        [ "Parent" ]
                        $"Nested path for '{name}' must be exact."

                    Expect.sequenceEqual
                        nested.EnclosingAccessibilities
                        [ ReferenceTypeAccessibility.NotPublic ]
                        $"Enclosing accessibility for '{name}' must be exact."
                )

            testCase "issue30.ecma.forwarders imports forwarder destinations atomically"
            <| fun _ ->
                let facade, target = MetadataReferenceFixtures.forwarderPair ()

                let imported =
                    match
                        MetadataReferenceFixtures.requestMany [
                            facade
                            target
                        ]
                        |> Ecma335ReferenceImporter.import
                    with
                    | Ok snapshots ->
                        snapshots
                        |> List.find (fun snapshot -> snapshot.StableId = facade.Identity)
                    | Error errors -> failtestf "Forwarder pair failed with %A." errors

                Expect.isNonEmpty imported.ExportedTypes "The facade must expose forwarders."

                imported.ExportedTypes
                |> List.exists (fun exportedType ->
                    match exportedType.Target with
                    | ReferenceForwarderTarget.Assembly _ -> true
                    | _ -> false
                )
                |> fun found ->
                    Expect.isTrue
                        found
                        "A top-level facade forwarder must preserve its assembly target."

                imported.ExportedTypes
                |> List.exists (fun exportedType ->
                    match exportedType.Target with
                    | ReferenceForwarderTarget.ExportedType _ -> true
                    | _ -> false
                )
                |> fun found ->
                    Expect.isTrue
                        found
                        "A nested facade forwarder must preserve its exported parent."

                let topLevel =
                    imported.ExportedTypes
                    |> List.find (fun item -> item.SourceIdentity.EnclosingTypes.IsEmpty)
                    |> expectForwarderResolution

                Expect.equal
                    topLevel.DestinationStableId
                    target.Identity
                    "The top-level destination stable identity must be exact."

                Expect.hasLength topLevel.Steps 1 "A direct forwarder must contain one step."

                let nested =
                    imported.ExportedTypes
                    |> List.find (fun item -> not item.SourceIdentity.EnclosingTypes.IsEmpty)
                    |> expectForwarderResolution

                Expect.equal
                    nested.DestinationStableId
                    target.Identity
                    "The nested destination stable identity must be exact."

                Expect.hasLength nested.Steps 2 "A nested forwarder must retain its parent step."

                let missing =
                    facade
                    |> expectImported
                    |> _.ExportedTypes.Head

                missing
                |> expectForwarderError
                    ReferenceImportErrorKind.MissingForwarderTarget
                    facade.Identity

                let first, middle, finalTarget = MetadataReferenceFixtures.forwarderMultiHop ()

                let multiHop =
                    expectImportedMany [
                        first
                        middle
                        finalTarget
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = first.Identity)

                let multiHopResolution =
                    multiHop.ExportedTypes
                    |> List.find (fun item -> item.SourceIdentity.EnclosingTypes.IsEmpty)
                    |> expectForwarderResolution

                Expect.equal
                    multiHopResolution.DestinationStableId
                    finalTarget.Identity
                    "A multi-hop forwarder must reach the final target."

                Expect.hasLength
                    multiHopResolution.Steps
                    2
                    "A multi-hop forwarder must retain every transition."

                let cycleA, cycleB = MetadataReferenceFixtures.forwarderCycle ()

                let cycle =
                    expectImportedMany [
                        cycleA
                        cycleB
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = cycleA.Identity)
                    |> _.ExportedTypes.Head

                cycle
                |> expectForwarderError ReferenceImportErrorKind.ForwarderCycle cycleA.Identity

                let duplicate = MetadataReferenceFixtures.duplicateForwarder ()

                duplicate
                |> MetadataReferenceFixtures.request
                |> expectSingleError ReferenceImportErrorKind.DuplicateIdentity duplicate.Identity
                |> ignore

                let incompatibleFacade, incompatibleTarget =
                    MetadataReferenceFixtures.incompatibleForwarder ()

                let incompatible =
                    expectImportedMany [
                        incompatibleFacade
                        incompatibleTarget
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = incompatibleFacade.Identity)
                    |> _.ExportedTypes.Head

                incompatible
                |> expectForwarderError
                    ReferenceImportErrorKind.IncompatibleForwarderTarget
                    incompatibleFacade.Identity

                let fileFacade, fileTarget, fileMismatch =
                    MetadataReferenceFixtures.fileForwarder ()

                let fileImported =
                    expectImportedMany [
                        fileFacade
                        fileTarget
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = fileFacade.Identity)

                let fileResolution =
                    fileImported.ExportedTypes.Head
                    |> expectForwarderResolution

                Expect.equal
                    fileResolution.DestinationStableId
                    fileTarget.Identity
                    "A matching file hash must resolve the netmodule target."

                let mismatchedFile =
                    expectImportedMany [
                        fileFacade
                        fileMismatch
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = fileFacade.Identity)
                    |> _.ExportedTypes.Head

                mismatchedFile
                |> expectForwarderError
                    ReferenceImportErrorKind.IncompatibleForwarderTarget
                    fileFacade.Identity

                let unsupportedFacade, unsupportedTarget, _ =
                    MetadataReferenceFixtures.unsupportedFileHashAlgorithmForwarder ()

                let unsupportedHashAlgorithm =
                    expectImportedMany [
                        unsupportedFacade
                        unsupportedTarget
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = unsupportedFacade.Identity)
                    |> _.ExportedTypes.Head

                unsupportedHashAlgorithm
                |> expectForwarderError
                    ReferenceImportErrorKind.IncompatibleForwarderTarget
                    unsupportedFacade.Identity

            testCase "issue30.ecma.members-signatures decodes member and signature tables"
            <| fun _ ->
                let imported =
                    MetadataReferenceFixtures.semanticAssembly ()
                    |> expectImported

                let snapshotSemantics = expectSnapshotSemantics imported

                imported.TypeDefinitions
                |> List.collect (
                    expectTypeSemantics
                    >> _.Methods
                )
                |> fun methods ->
                    Expect.isNonEmpty
                        methods
                        "Method definitions and their signatures must be imported."

                Expect.isNonEmpty
                    snapshotSemantics.MemberReferences
                    "MemberRef signatures must be imported."

                Expect.isNonEmpty
                    snapshotSemantics.TypeSpecifications
                    "TypeSpec signatures must be imported."

                Expect.isNonEmpty
                    snapshotSemantics.StandaloneSignatures
                    "StandAloneSig signatures must be imported."

                let genericDefinition =
                    imported.TypeDefinitions
                    |> List.find (fun item -> item.Identity.MetadataName = "GenericType`1")

                let generic = expectTypeSemantics genericDefinition

                Expect.hasLength
                    generic.GenericParameters
                    1
                    "The generic parameter must be imported."

                let parameter = generic.GenericParameters.Head
                Expect.equal parameter.Index 0 "The generic parameter index must be exact."
                Expect.equal parameter.Name "T" "The generic parameter name must be exact."

                Expect.equal
                    parameter.Variance
                    ReferenceGenericVariance.Covariant
                    "Generic variance must be exact."

                Expect.isTrue
                    parameter.SpecialConstraints.ReferenceType
                    "The reference-type constraint must be retained."

                Expect.isTrue
                    parameter.SpecialConstraints.DefaultConstructor
                    "The default-constructor constraint must be retained."

                Expect.hasLength parameter.TypeConstraints 1 "The type constraint must be retained."
                Expect.hasLength generic.Methods 5 "Every owned method must be imported."
                Expect.hasLength generic.Fields 1 "Every owned field must be imported."
                Expect.hasLength generic.Properties 1 "Every owned property must be imported."
                Expect.hasLength generic.Events 1 "Every owned event must be imported."

                let field = generic.Fields.Head
                Expect.equal field.Name "Number" "The field name must be exact."

                Expect.equal
                    field.FieldType
                    (ReferenceCliType.Primitive ReferencePrimitiveType.Int32)
                    "The field signature must be exact."

                Expect.equal
                    field.DefaultValue
                    (Some(ReferenceConstant.Int32 42))
                    "The field constant must be exact."

                let property = generic.Properties.Head

                Expect.equal
                    property.Identity.Kind
                    ReferenceMemberKind.Property
                    "Property kind must be exact."

                Expect.equal
                    property.Identity.PropertySignature
                    (Some property.Signature)
                    "Property identity must contain the complete signature."

                Expect.isNone
                    property.Identity.EventType
                    "Property identity must not contain an event type."

                Expect.equal
                    property.Signature.ReturnType
                    (ReferenceCliType.Primitive ReferencePrimitiveType.Int32)
                    "The property return type must be exact."

                Expect.equal
                    property.GetterAccessibility
                    (Some ReferenceMemberAccessibility.Public)
                    "Getter accessibility must be exact."

                Expect.equal
                    property.SetterAccessibility
                    (Some ReferenceMemberAccessibility.Private)
                    "Setter accessibility must be exact."

                Expect.equal
                    property.DefaultValue
                    (Some(ReferenceConstant.Int32 11))
                    "The property constant must be exact."

                let eventDefinition = generic.Events.Head

                Expect.equal
                    eventDefinition.Identity.Kind
                    ReferenceMemberKind.Event
                    "Event kind must be exact."

                Expect.equal
                    eventDefinition.Identity.EventType
                    (Some eventDefinition.EventType)
                    "Event identity must contain the complete event type."

                Expect.isNone
                    eventDefinition.Identity.PropertySignature
                    "Event identity must not contain a property signature."

                Expect.equal
                    eventDefinition.AddAccessibility
                    (Some ReferenceMemberAccessibility.FamilyOrAssembly)
                    "Event add accessibility must be exact."

                Expect.equal
                    eventDefinition.RemoveAccessibility
                    (Some ReferenceMemberAccessibility.FamilyAndAssembly)
                    "Event remove accessibility must be exact."

                Expect.hasLength
                    generic.MethodImplementations
                    1
                    "The MethodImpl row must be imported."

                Expect.hasLength generic.Interfaces 1 "The InterfaceImpl row must be imported."

                let exotic =
                    generic.Methods
                    |> List.find (fun methodDefinition -> methodDefinition.Name = "Invoke")

                Expect.equal
                    exotic.Signature.CallingConvention
                    ReferenceSignatureCallingConvention.VarArgs
                    "The varargs calling convention must be exact."

                Expect.equal
                    exotic.Signature.GenericParameterCount
                    1
                    "The method generic arity must be exact."

                Expect.hasLength
                    exotic.Signature.ParameterTypes
                    7
                    "Every required and optional parameter must be imported."

                Expect.hasLength
                    snapshotSemantics.MemberReferences
                    6
                    "Every MemberRef row and parent form must be imported."

                let memberParentKinds =
                    snapshotSemantics.MemberReferences
                    |> List.map _.Identity.Parent
                    |> List.choose id
                    |> List.map (
                        function
                        | ReferenceMemberParent.Type _ -> "type"
                        | ReferenceMemberParent.Module _ -> "module"
                        | ReferenceMemberParent.Method _ -> "method"
                        | ReferenceMemberParent.TypeSpecification _ -> "specification"
                    )
                    |> Set.ofList

                Expect.equal
                    memberParentKinds
                    (set [
                        "type"
                        "module"
                        "method"
                        "specification"
                    ])
                    "Every MemberRef parent kind must be retained."

                Expect.hasLength
                    snapshotSemantics.MethodSpecifications
                    1
                    "The MethodSpec row must be imported."

                Expect.sequenceEqual
                    snapshotSemantics.MethodSpecifications.Head.TypeArguments
                    [ ReferenceCliType.Primitive ReferencePrimitiveType.String ]
                    "MethodSpec arguments must be exact."

                snapshotSemantics.StandaloneSignatures
                |> List.exists (
                    function
                    | ReferenceStandaloneSignature.LocalVariables values -> values.Length = 3
                    | _ -> false
                )
                |> fun found ->
                    Expect.isTrue found "The complete local-variable signature must be imported."

                snapshotSemantics.StandaloneSignatures
                |> List.exists (
                    function
                    | ReferenceStandaloneSignature.Field fieldType ->
                        match fieldType with
                        | ReferenceCliType.GenericMethodParameter 0 -> true
                        | _ -> false
                    | _ -> false
                )
                |> fun found ->
                    Expect.isTrue found "The field-form standalone signature must be imported."

                let duplicateProperty = MetadataReferenceFixtures.duplicateProperty ()

                duplicateProperty
                |> MetadataReferenceFixtures.request
                |> expectSingleError
                    ReferenceImportErrorKind.DuplicateIdentity
                    duplicateProperty.Identity
                |> ignore

                let duplicateEvent = MetadataReferenceFixtures.duplicateEvent ()

                duplicateEvent
                |> MetadataReferenceFixtures.request
                |> expectSingleError
                    ReferenceImportErrorKind.DuplicateIdentity
                    duplicateEvent.Identity
                |> ignore

            testCase "issue30.ecma.attributes decodes values without instantiation"
            <| fun _ ->
                let imported =
                    MetadataReferenceFixtures.semanticAssembly ()
                    |> expectImported

                let semantics = expectSnapshotSemantics imported

                Expect.isNonEmpty
                    semantics.CustomAttributes
                    "Assembly custom attributes must be decoded."

                semantics.CustomAttributes
                |> List.forall (fun attribute -> not attribute.RawBlob.IsEmpty)
                |> fun found ->
                    Expect.isTrue found "Every decoded attribute must preserve its raw blob."

                semantics.CustomAttributes
                |> List.collect _.FixedArguments
                |> List.contains (
                    ReferenceAttributeValue.TypeName(Some "Issue30.Outer+Inner, Issue30.Semantics")
                )
                |> fun found -> Expect.isTrue found "The serialized System.Type name must be exact."

                let rich =
                    semantics.CustomAttributes
                    |> List.find (fun attribute -> attribute.FixedArguments.Length = 6)

                Expect.equal
                    rich.FixedArguments.[0]
                    (ReferenceAttributeValue.Primitive(ReferenceConstant.Boolean true))
                    "The Boolean fixed argument must be exact."

                Expect.equal
                    rich.FixedArguments.[1]
                    (ReferenceAttributeValue.String(Some "issue30-attribute-marker"))
                    "The string fixed argument must be exact."

                match rich.FixedArguments.[2] with
                | ReferenceAttributeValue.Enum(_, ReferenceConstant.UInt16 513us) -> ()
                | value -> failtestf "The local enum fixed argument was %A." value

                Expect.equal
                    rich.FixedArguments.[4]
                    (ReferenceAttributeValue.Boxed(
                        ReferenceAttributeValue.Primitive(ReferenceConstant.Int32 42)
                    ))
                    "The boxed fixed argument must be exact."

                Expect.equal
                    rich.FixedArguments.[5]
                    (ReferenceAttributeValue.Array(
                        ReferenceCliType.Primitive ReferencePrimitiveType.Int32,
                        Some [
                            ReferenceAttributeValue.Primitive(ReferenceConstant.Int32 7)
                            ReferenceAttributeValue.Primitive(ReferenceConstant.Int32 9)
                        ]
                    ))
                    "The array fixed argument must be exact."

                Expect.hasLength rich.NamedArguments 2 "Every named argument must be decoded."

                Expect.equal
                    rich.NamedArguments.Head
                    {
                        Name = "Number"
                        IsField = true
                        ArgumentType = ReferenceCliType.Primitive ReferencePrimitiveType.Int32
                        Value = ReferenceAttributeValue.Primitive(ReferenceConstant.Int32 17)
                    }
                    "The named field argument must be exact."

                Expect.equal
                    rich.NamedArguments.Tail.Head
                    {
                        Name = "Text"
                        IsField = false
                        ArgumentType = ReferenceCliType.Primitive ReferencePrimitiveType.String
                        Value = ReferenceAttributeValue.String(Some "named-value")
                    }
                    "The named property argument must be exact."

                Expect.equal
                    semantics.FriendAssemblies
                    [
                        {
                            Name = "Issue30.Friend"
                            PublicKey = [
                                1uy
                                2uy
                                3uy
                                4uy
                            ]
                        }
                    ]
                    "The friend assembly identity must be exact."

                let declaringAssembly =
                    imported.Assembly
                    |> Option.get

                let friendRequest = {
                    RequestingAssembly = {
                        declaringAssembly with
                            Name = "Issue30.Friend"
                            PublicKey = [
                                1uy
                                2uy
                                3uy
                                4uy
                            ]
                            PublicKeyToken = []
                    }
                    IsWithinDeclaringType = false
                    IsDerivedFromDeclaringType = false
                }

                Expect.isTrue
                    (ReferenceAccessibility.isMemberAccessible
                        declaringAssembly
                        semantics.FriendAssemblies
                        [ ReferenceTypeAccessibility.NotPublic ]
                        ReferenceMemberAccessibility.Assembly
                        friendRequest)
                    "An exact friend public key must grant assembly access."

                Expect.isFalse
                    (ReferenceAccessibility.isMemberAccessible
                        declaringAssembly
                        semantics.FriendAssemblies
                        [ ReferenceTypeAccessibility.NotPublic ]
                        ReferenceMemberAccessibility.Assembly
                        {
                            friendRequest with
                                RequestingAssembly = {
                                    friendRequest.RequestingAssembly with
                                        PublicKey = [ 9uy ]
                                }
                        })
                    "A different friend public key must not grant assembly access."

                let consumer, decoy, provider = MetadataReferenceFixtures.externalEnumUniverse ()

                let externalSemantics =
                    expectImportedMany [
                        consumer
                        decoy
                        provider
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = consumer.Identity)
                    |> expectSnapshotSemantics

                match externalSemantics.CustomAttributes.Head.FixedArguments.Head with
                | ReferenceAttributeValue.Enum(ReferenceCliType.Reference(identity, _),
                                               ReferenceConstant.UInt16 513us) ->
                    Expect.equal
                        identity.Namespace
                        "Issue30.External"
                        "The external enum namespace must be exact."

                    Expect.sequenceEqual
                        identity.EnclosingTypes
                        [ "Container" ]
                        "The external enum nested path must be exact."

                    match identity.ResolutionScope with
                    | Some(ReferenceResolutionScope.Type parent) ->
                        match parent.ResolutionScope with
                        | Some(ReferenceResolutionScope.Assembly assembly) ->
                            Expect.equal
                                assembly.Name
                                "Issue30.ExternalEnum.Provider"
                                "External enum lookup must use the assembly scope."
                        | scope -> failtestf "External enum parent scope was %A." scope
                    | scope -> failtestf "External enum nested scope was %A." scope
                | value -> failtestf "The external enum fixed argument was %A." value

            testCase "issue30.ecma.attributes rejects malformed value mutations"
            <| fun _ ->
                [
                    MetadataReferenceFixtures.AttributeCorruption.InvalidProlog
                    MetadataReferenceFixtures.AttributeCorruption.Truncated
                    MetadataReferenceFixtures.AttributeCorruption.InvalidBoxedTag
                    MetadataReferenceFixtures.AttributeCorruption.TruncatedEnum
                    MetadataReferenceFixtures.AttributeCorruption.TrailingData
                ]
                |> List.iter (fun corruption ->
                    let imported =
                        MetadataReferenceFixtures.invalidAttribute corruption
                        |> expectImported

                    Expect.isFalse
                        imported.Semantics.IsValueCreated
                        $"Mutation '{corruption}' must remain lazy before demand."

                    let first = imported.Semantics.Get(Threading.CancellationToken.None)
                    let second = imported.Semantics.Get(Threading.CancellationToken.None)

                    Expect.equal
                        second
                        first
                        $"Mutation '{corruption}' must memoize the same failure."

                    match first with
                    | Error error ->
                        Expect.equal
                            error.Kind
                            ReferenceImportErrorKind.InvalidCustomAttribute
                            $"Mutation '{corruption}' must return InvalidCustomAttribute."
                    | Ok _ -> failtestf "Mutation '%A' decoded successfully." corruption
                )

                [
                    "Issue30.Friend, PublicKey=not-hex"
                    "Issue30.Friend, PublicKey="
                    "Issue30.Friend, PublicKey=0102, PublicKey=0304"
                ]
                |> List.iter (fun identity ->
                    let imported =
                        MetadataReferenceFixtures.malformedFriendAssembly identity
                        |> expectImported

                    let first = imported.Semantics.Get(Threading.CancellationToken.None)
                    let second = imported.Semantics.Get(Threading.CancellationToken.None)

                    Expect.equal
                        second
                        first
                        $"Malformed friend identity '{identity}' must memoize its failure."

                    match first with
                    | Error error ->
                        Expect.equal
                            error.Kind
                            ReferenceImportErrorKind.InvalidCustomAttribute
                            $"Malformed friend identity '{identity}' must fail as an attribute."
                    | Ok semantics ->
                        failtestf
                            "Malformed friend identity '%s' granted %A."
                            identity
                            semantics.FriendAssemblies
                )

                let consumer, decoy, _ = MetadataReferenceFixtures.externalEnumUniverse ()

                let missingExternalEnum =
                    expectImportedMany [
                        consumer
                        decoy
                    ]
                    |> List.find (fun snapshot -> snapshot.StableId = consumer.Identity)

                match missingExternalEnum.Semantics.Get(Threading.CancellationToken.None) with
                | Error error ->
                    Expect.equal
                        error.Kind
                        ReferenceImportErrorKind.InvalidCustomAttribute
                        "An unavailable external enum definition must fail safely."
                | Ok semantics -> failtestf "Missing external enum decoded as %A." semantics

            testCase (
                "issue30.ecma.malformed returns ordered structured failures "
                + "without partial identity"
            )
            <| fun _ ->
                let unusedMalformedEnum = MetadataReferenceFixtures.unusedMalformedEnum ()

                match
                    unusedMalformedEnum
                    |> MetadataReferenceFixtures.request
                    |> Ecma335ReferenceImporter.import
                with
                | Ok snapshots ->
                    Expect.hasLength
                        snapshots
                        1
                        "Unused malformed enum signatures must remain lazy."
                | Error errors -> failtestf "Eager enum import failed as %A." errors

                let expectError expectedKind identity request =
                    match Ecma335ReferenceImporter.import request with
                    | Error [ error ] ->
                        Expect.equal
                            error.Kind
                            expectedKind
                            $"Fixture '{identity}' returned the wrong error kind."

                        Expect.equal
                            error.ReferenceStableId
                            identity
                            "The error stable identity must be exact."

                        Expect.isNotEmpty
                            error.LogicalPath
                            "The error logical path must be present."

                        Expect.isNotEmpty
                            error.Message
                            "The structured error message must be present."

                        Expect.isLessThanOrEqual
                            error.Message.Length
                            512
                            "The structured error message must remain bounded."
                    | result -> failtestf "Fixture '%s' returned %A." identity result

                let invalidPe = MetadataReferenceFixtures.malformedImage ()

                invalidPe
                |> MetadataReferenceFixtures.request
                |> expectError ReferenceImportErrorKind.InvalidPortableExecutable invalidPe.Identity

                let identity = MetadataReferenceFixtures.identityAssembly ()

                let invalidFingerprint = {
                    identity with
                        Snapshot = {
                            identity.Snapshot with
                                ContentFingerprint = "not-a-fingerprint"
                        }
                }

                invalidFingerprint
                |> MetadataReferenceFixtures.request
                |> expectError
                    ReferenceImportErrorKind.InvalidContentFingerprint
                    invalidFingerprint.Identity

                let mismatchedFingerprint = {
                    identity with
                        Snapshot = {
                            identity.Snapshot with
                                ContentFingerprint = String.replicate 64 "0"
                        }
                }

                mismatchedFingerprint
                |> MetadataReferenceFixtures.request
                |> expectError
                    ReferenceImportErrorKind.ContentFingerprintMismatch
                    mismatchedFingerprint.Identity

                identity
                |> MetadataReferenceFixtures.requestWithLimits {
                    ReferenceImportLimits.Production with
                        MaximumPeImageBytes = 1L
                }
                |> expectError ReferenceImportErrorKind.ImageTooLarge identity.Identity

                identity
                |> MetadataReferenceFixtures.requestWithLimits {
                    ReferenceImportLimits.Production with
                        MaximumMetadataBytes = 1L
                }
                |> expectError ReferenceImportErrorKind.MetadataTooLarge identity.Identity

                identity
                |> MetadataReferenceFixtures.requestWithLimits {
                    ReferenceImportLimits.Production with
                        MaximumRowsPerTable = 0
                }
                |> expectError ReferenceImportErrorKind.TableRowLimitExceeded identity.Identity

                let noCli = MetadataReferenceFixtures.nativeImage ()

                noCli
                |> MetadataReferenceFixtures.request
                |> expectError ReferenceImportErrorKind.MissingCliMetadata noCli.Identity

                let malformedMetadata = MetadataReferenceFixtures.corruptMetadataRoot ()

                malformedMetadata
                |> MetadataReferenceFixtures.request
                |> expectError
                    ReferenceImportErrorKind.MalformedCliMetadata
                    malformedMetadata.Identity

                let malformedStream = MetadataReferenceFixtures.corruptStreamDirectory ()

                malformedStream
                |> MetadataReferenceFixtures.request
                |> expectError
                    ReferenceImportErrorKind.MalformedCliMetadata
                    malformedStream.Identity

                let invalidHeap = MetadataReferenceFixtures.invalidHeapIndex ()

                invalidHeap
                |> MetadataReferenceFixtures.request
                |> expectError ReferenceImportErrorKind.MalformedCliMetadata invalidHeap.Identity

                let invalidCodedIndex = MetadataReferenceFixtures.invalidCodedIndex ()

                invalidCodedIndex
                |> MetadataReferenceFixtures.request
                |> expectError
                    ReferenceImportErrorKind.MalformedCliMetadata
                    invalidCodedIndex.Identity

                let invalidResource = MetadataReferenceFixtures.invalidResource 2

                invalidResource
                |> MetadataReferenceFixtures.request
                |> expectError ReferenceImportErrorKind.InvalidResource invalidResource.Identity

                identity
                |> MetadataReferenceFixtures.requestWithLimits {
                    ReferenceImportLimits.Production with
                        MaximumRowsAcrossTables = 0
                }
                |> expectError ReferenceImportErrorKind.TotalRowLimitExceeded identity.Identity

                let invalidUtf8 = MetadataReferenceFixtures.invalidUtf8String ()

                invalidUtf8
                |> MetadataReferenceFixtures.requestWithLimits {
                    ReferenceImportLimits.Production with
                        MaximumHeapValueBytes =
                            MetadataReferenceFixtures.invalidUtf8StringByteLength
                            - 1
                }
                |> expectError ReferenceImportErrorKind.HeapValueTooLarge invalidUtf8.Identity

                invalidUtf8
                |> MetadataReferenceFixtures.requestWithLimits {
                    ReferenceImportLimits.Production with
                        MaximumHeapValueBytes =
                            MetadataReferenceFixtures.invalidUtf8StringByteLength
                }
                |> expectError ReferenceImportErrorKind.HeapValueTooLarge invalidUtf8.Identity

                let expectDemandError
                    (expectedKind: ReferenceImportErrorKind)
                    (result: Result<'value, ReferenceImportError>)
                    =
                    match result with
                    | Error error ->
                        Expect.equal error.Kind expectedKind "The lazy error kind must be exact."
                    | Ok value -> failtestf "Lazy malformed metadata decoded as %A." value

                let deepSignature = MetadataReferenceFixtures.malformedSignature 8

                let deepImported =
                    deepSignature
                    |> MetadataReferenceFixtures.requestWithLimits {
                        ReferenceImportLimits.Production with
                            MaximumSignatureDepth = 4
                    }
                    |> Ecma335ReferenceImporter.import
                    |> function
                        | Ok [ snapshot ] -> snapshot
                        | result -> failtestf "Deep-signature fixture returned %A." result

                Expect.isFalse
                    deepImported.Semantics.IsValueCreated
                    "The signature-depth failure must remain lazy before demand."

                deepImported.Semantics.Get(Threading.CancellationToken.None)
                |> expectDemandError ReferenceImportErrorKind.SignatureDepthExceeded

                let invalidStandalone =
                    MetadataReferenceFixtures.invalidStandaloneSignature ()
                    |> expectImported

                invalidStandalone.Semantics.Get(Threading.CancellationToken.None)
                |> expectDemandError ReferenceImportErrorKind.InvalidSignature

                let constrained =
                    MetadataReferenceFixtures.semanticAssembly ()
                    |> MetadataReferenceFixtures.requestWithLimits {
                        ReferenceImportLimits.Production with
                            MaximumGenericConstraints = 0
                    }
                    |> Ecma335ReferenceImporter.import
                    |> function
                        | Ok [ snapshot ] -> snapshot
                        | result -> failtestf "Generic-constraint fixture returned %A." result

                constrained.TypeDefinitions
                |> List.find (fun definition -> definition.Identity.MetadataName = "GenericType`1")
                |> fun definition -> definition.Semantics.Get(Threading.CancellationToken.None)
                |> expectDemandError ReferenceImportErrorKind.GenericConstraintLimitExceeded

                MetadataReferenceFixtures.requestMany [
                    identity
                    identity
                ]
                |> expectError ReferenceImportErrorKind.DuplicateIdentity identity.Identity

                match
                    MetadataReferenceFixtures.requestMany [
                        identity
                        invalidPe
                    ]
                    |> Ecma335ReferenceImporter.import
                with
                | Error [ error ] ->
                    Expect.equal
                        error.ReferenceStableId
                        invalidPe.Identity
                        "Atomic import must report the failing snapshot."
                | result -> failtestf "Atomic malformed import returned %A." result

                use cancellation = new Threading.CancellationTokenSource()
                cancellation.Cancel()

                let cancelledRequest = {
                    MetadataReferenceFixtures.request identity with
                        CancellationToken = cancellation.Token
                }

                Expect.throwsT<OperationCanceledException>
                    (fun () ->
                        Ecma335ReferenceImporter.import cancelledRequest
                        |> ignore
                    )
                    "Cancellation must not be translated to a metadata error."
        ]
