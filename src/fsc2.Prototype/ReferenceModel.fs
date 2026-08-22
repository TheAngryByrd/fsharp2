namespace FSharp2.Compiler

open System
open System.Threading

[<Struct>]
type internal ReferenceVersion = {
    Major: int
    Minor: int
    Build: int
    Revision: int
}

type internal ReferenceAssemblyIdentity = {
    Name: string
    Version: ReferenceVersion
    Culture: string option
    Flags: int
    HashAlgorithm: uint32
    PublicKey: byte list
    PublicKeyToken: byte list
}

type internal ReferenceModuleIdentity = {
    Name: string
    Generation: int
    ModuleVersionId: Guid
    GenerationId: Guid option
    BaseGenerationId: Guid option
}

type internal ReferenceAssemblyReference = {
    Identity: ReferenceAssemblyIdentity
    HashValue: byte list
}

type internal ReferenceModuleReference = { Name: string }

type internal ReferenceFileIdentity = {
    Name: string
    ContainsMetadata: bool
    HashValue: byte list
}

[<RequireQualifiedAccess>]
type internal ReferenceImportErrorKind =
    | Unsupported
    | InvalidContentFingerprint
    | ContentFingerprintMismatch
    | ImageTooLarge
    | InvalidPortableExecutable
    | MissingCliMetadata
    | MalformedCliMetadata
    | MetadataTooLarge
    | TableRowLimitExceeded
    | TotalRowLimitExceeded
    | HeapValueTooLarge
    | SignatureDepthExceeded
    | SignatureNodeLimitExceeded
    | GenericConstraintLimitExceeded
    | InvalidSignature
    | InvalidCustomAttribute
    | InvalidResource
    | DuplicateIdentity
    | MissingForwarderTarget
    | AmbiguousForwarderTarget
    | InvalidForwarderParent
    | IncompatibleForwarderTarget
    | ForwarderCycle
    | ResolutionDepthExceeded

type internal ReferenceImportError = {
    Kind: ReferenceImportErrorKind
    ReferenceStableId: string
    LogicalPath: string
    MetadataLocation: string option
    DecoderOffset: int option
    RelatedIdentity: string option
    Message: string
}

[<Sealed>]
type internal ReferenceSemanticDemand<'value>
    (evaluate: CancellationToken -> Result<'value, ReferenceImportError>) =
    let gate = obj ()
    let mutable value: Result<'value, ReferenceImportError> option = None

    member _.IsValueCreated = lock gate (fun () -> value.IsSome)

    member _.Get(cancellationToken: CancellationToken) =
        cancellationToken.ThrowIfCancellationRequested()

        lock
            gate
            (fun () ->
                match value with
                | Some cached -> cached
                | None ->
                    let evaluated = evaluate cancellationToken
                    value <- Some evaluated
                    evaluated
            )

[<RequireQualifiedAccess>]
type internal ReferenceResolutionScope =
    | CurrentModule
    | Module of moduleName: string
    | Assembly of identity: ReferenceAssemblyIdentity
    | Type of enclosingType: ReferenceTypeIdentity

and internal ReferenceTypeIdentity = {
    ModuleName: string
    Namespace: string
    EnclosingTypes: string list
    MetadataName: string
    GenericArity: int
    ResolutionScope: ReferenceResolutionScope option
}

[<RequireQualifiedAccess>]
type internal ReferenceTypeAccessibility =
    | Public
    | NotPublic
    | NestedPublic
    | NestedPrivate
    | NestedFamily
    | NestedAssembly
    | NestedFamilyAndAssembly
    | NestedFamilyOrAssembly

[<RequireQualifiedAccess>]
type internal ReferenceMemberAccessibility =
    | PrivateScope
    | Private
    | FamilyAndAssembly
    | Assembly
    | Family
    | FamilyOrAssembly
    | Public

[<RequireQualifiedAccess>]
type internal ReferenceGenericVariance =
    | Invariant
    | Covariant
    | Contravariant

type internal ReferenceGenericSpecialConstraints = {
    ReferenceType: bool
    ValueType: bool
    DefaultConstructor: bool
    AllowByRefLike: bool
}

[<RequireQualifiedAccess>]
type internal ReferencePrimitiveType =
    | Boolean
    | Byte
    | SByte
    | Char
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | IntPtr
    | UIntPtr
    | Single
    | Double
    | String
    | Object
    | TypedReference
    | Void

type internal ReferenceArrayShape = {
    Rank: int
    Sizes: int list
    LowerBounds: int list
}

[<RequireQualifiedAccess>]
type internal ReferenceSignatureCallingConvention =
    | Default
    | CDecl
    | StdCall
    | ThisCall
    | FastCall
    | VarArgs
    | Unmanaged
    | NativeVarArgs
    | Unknown of value: byte

type internal ReferenceMethodSignature = {
    CallingConvention: ReferenceSignatureCallingConvention
    IsInstance: bool
    IsExplicitThis: bool
    GenericParameterCount: int
    RequiredParameterCount: int
    ReturnType: ReferenceCliType
    ParameterTypes: ReferenceCliType list
}

and [<RequireQualifiedAccess>] internal ReferenceCliType =
    | Primitive of ReferencePrimitiveType
    | Definition of identity: ReferenceTypeIdentity * isValueType: bool
    | Reference of identity: ReferenceTypeIdentity * isValueType: bool
    | Specification of ReferenceCliType
    | GenericTypeParameter of index: int
    | GenericMethodParameter of index: int
    | GenericInstantiation of genericType: ReferenceCliType * arguments: ReferenceCliType list
    | SzArray of elementType: ReferenceCliType
    | Array of elementType: ReferenceCliType * shape: ReferenceArrayShape
    | Pointer of elementType: ReferenceCliType
    | ByReference of elementType: ReferenceCliType
    | Modified of modifier: ReferenceCliType * unmodifiedType: ReferenceCliType * isRequired: bool
    | Pinned of elementType: ReferenceCliType
    | FunctionPointer of ReferenceMethodSignature

[<RequireQualifiedAccess>]
type internal ReferenceMemberKind =
    | Method
    | Field
    | Property
    | Event

type internal ReferenceMemberIdentity = {
    DeclaringType: ReferenceTypeIdentity option
    Parent: ReferenceMemberParent option
    Kind: ReferenceMemberKind
    MetadataName: string
    GenericArity: int
    MethodSignature: ReferenceMethodSignature option
    FieldType: ReferenceCliType option
    PropertySignature: ReferenceMethodSignature option
    EventType: ReferenceCliType option
}

and [<RequireQualifiedAccess>] internal ReferenceMemberParent =
    | Type of ReferenceTypeIdentity
    | Module of moduleName: string
    | Method of ReferenceMemberIdentity
    | TypeSpecification of ReferenceCliType

type internal ReferenceGenericParameter = {
    Index: int
    Name: string
    Variance: ReferenceGenericVariance
    SpecialConstraints: ReferenceGenericSpecialConstraints
    TypeConstraints: ReferenceCliType list
}

[<RequireQualifiedAccess>]
type internal ReferenceConstant =
    | Null
    | Boolean of bool
    | Char of char
    | SByte of sbyte
    | Byte of byte
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | Single of single
    | Double of double
    | String of string

[<RequireQualifiedAccess>]
type internal ReferenceAttributeValue =
    | Null
    | Primitive of ReferenceConstant
    | String of string option
    | TypeName of string option
    | Enum of enumType: ReferenceCliType * value: ReferenceConstant
    | Boxed of ReferenceAttributeValue
    | Array of elementType: ReferenceCliType * values: ReferenceAttributeValue list option

type internal ReferenceAttributeNamedArgument = {
    Name: string
    IsField: bool
    ArgumentType: ReferenceCliType
    Value: ReferenceAttributeValue
}

[<RequireQualifiedAccess>]
type internal ReferenceAttributeConstructor =
    | MethodDefinition of ReferenceMemberIdentity
    | MemberReference of ReferenceMemberIdentity

type internal ReferenceEcmaCustomAttribute = {
    Constructor: ReferenceAttributeConstructor
    FixedArguments: ReferenceAttributeValue list
    NamedArguments: ReferenceAttributeNamedArgument list
    RawBlob: byte list
}

type internal ReferenceParameter = {
    Sequence: int
    Name: string option
    Attributes: int
    DefaultValue: ReferenceConstant option
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceEcmaMethodDefinition = {
    DeclaringType: ReferenceTypeIdentity
    Name: string
    Attributes: int
    ImplementationAttributes: int
    Accessibility: ReferenceMemberAccessibility
    Signature: ReferenceMethodSignature
    Parameters: ReferenceParameter list
    GenericParameters: ReferenceGenericParameter list
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceEcmaFieldDefinition = {
    DeclaringType: ReferenceTypeIdentity
    Name: string
    Attributes: int
    Accessibility: ReferenceMemberAccessibility
    FieldType: ReferenceCliType
    DefaultValue: ReferenceConstant option
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferencePropertyDefinition = {
    Identity: ReferenceMemberIdentity
    DeclaringType: ReferenceTypeIdentity
    Name: string
    Attributes: int
    Signature: ReferenceMethodSignature
    GetterAccessibility: ReferenceMemberAccessibility option
    SetterAccessibility: ReferenceMemberAccessibility option
    OtherAccessibilities: ReferenceMemberAccessibility list
    DefaultValue: ReferenceConstant option
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceEventDefinition = {
    Identity: ReferenceMemberIdentity
    DeclaringType: ReferenceTypeIdentity
    Name: string
    Attributes: int
    EventType: ReferenceCliType
    AddAccessibility: ReferenceMemberAccessibility option
    RemoveAccessibility: ReferenceMemberAccessibility option
    RaiseAccessibility: ReferenceMemberAccessibility option
    OtherAccessibilities: ReferenceMemberAccessibility list
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceMethodImplementation = {
    DeclaringType: ReferenceTypeIdentity
    MethodBody: ReferenceMemberIdentity
    MethodDeclaration: ReferenceMemberIdentity
}

type internal ReferenceInterfaceImplementation = {
    DeclaringType: ReferenceTypeIdentity
    InterfaceType: ReferenceCliType
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceMemberReference = { Identity: ReferenceMemberIdentity }

type internal ReferenceMethodSpecification = {
    Method: ReferenceMemberIdentity
    TypeArguments: ReferenceCliType list
}

[<RequireQualifiedAccess>]
type internal ReferenceStandaloneSignature =
    | Method of ReferenceMethodSignature
    | Field of ReferenceCliType
    | LocalVariables of ReferenceCliType list

type internal ReferenceTypeDefinitionSemantics = {
    BaseType: ReferenceCliType option
    GenericParameters: ReferenceGenericParameter list
    Methods: ReferenceEcmaMethodDefinition list
    Fields: ReferenceEcmaFieldDefinition list
    Properties: ReferencePropertyDefinition list
    Events: ReferenceEventDefinition list
    MethodImplementations: ReferenceMethodImplementation list
    Interfaces: ReferenceInterfaceImplementation list
    CustomAttributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceTypeDefinition = {
    Identity: ReferenceTypeIdentity
    Attributes: int
    Accessibility: ReferenceTypeAccessibility
    EnclosingAccessibilities: ReferenceTypeAccessibility list
    Semantics: ReferenceSemanticDemand<ReferenceTypeDefinitionSemantics>
}

[<RequireQualifiedAccess>]
type internal ReferenceForwarderTarget =
    | Assembly of ReferenceAssemblyIdentity
    | File of ReferenceFileIdentity
    | ExportedType of parent: ReferenceTypeIdentity

type internal ReferenceExportedType = {
    SourceIdentity: ReferenceTypeIdentity
    Attributes: int
    Target: ReferenceForwarderTarget
    Resolution: ReferenceSemanticDemand<ReferenceForwarderResolution>
}

and internal ReferenceForwarderStep = {
    SourceAssembly: ReferenceAssemblyIdentity option
    SourceType: ReferenceTypeIdentity
    Target: ReferenceForwarderTarget
}

and internal ReferenceForwarderResolution = {
    Steps: ReferenceForwarderStep list
    DestinationStableId: string
    DestinationType: ReferenceTypeIdentity
}

type internal ReferenceManifestResource = {
    Name: string
    Attributes: int
    Implementation: ReferenceResourceImplementation
    EmbeddedContentLength: int option
    EmbeddedContentFingerprint: string option
}

and [<RequireQualifiedAccess>] internal ReferenceResourceImplementation =
    | Embedded
    | File of ReferenceFileIdentity
    | Assembly of ReferenceAssemblyIdentity
    | ExportedType of ReferenceTypeIdentity

type internal ReferenceFriendAssembly = { Name: string; PublicKey: byte list }

type internal ReferenceAccessRequest = {
    RequestingAssembly: ReferenceAssemblyIdentity
    IsWithinDeclaringType: bool
    IsDerivedFromDeclaringType: bool
}

module internal ReferenceAccessibility =
    let private sameAssembly (left: ReferenceAssemblyIdentity) (right: ReferenceAssemblyIdentity) =
        left.Name = right.Name
        && left.Version = right.Version
        && left.Culture = right.Culture
        && left.PublicKeyToken = right.PublicKeyToken

    let private hasAssemblyAccess
        (declaringAssembly: ReferenceAssemblyIdentity)
        (friends: ReferenceFriendAssembly list)
        (request: ReferenceAccessRequest)
        =
        sameAssembly declaringAssembly request.RequestingAssembly
        || friends
           |> List.exists (fun friend ->
               friend.Name = request.RequestingAssembly.Name
               && (friend.PublicKey.IsEmpty
                   || friend.PublicKey = request.RequestingAssembly.PublicKey)
           )

    let private typeAllows assemblyAccess request =
        function
        | ReferenceTypeAccessibility.Public
        | ReferenceTypeAccessibility.NestedPublic -> true
        | ReferenceTypeAccessibility.NotPublic
        | ReferenceTypeAccessibility.NestedAssembly -> assemblyAccess
        | ReferenceTypeAccessibility.NestedPrivate -> request.IsWithinDeclaringType
        | ReferenceTypeAccessibility.NestedFamily -> request.IsDerivedFromDeclaringType
        | ReferenceTypeAccessibility.NestedFamilyAndAssembly ->
            request.IsDerivedFromDeclaringType
            && assemblyAccess
        | ReferenceTypeAccessibility.NestedFamilyOrAssembly ->
            request.IsDerivedFromDeclaringType
            || assemblyAccess

    let isMemberAccessible
        (declaringAssembly: ReferenceAssemblyIdentity)
        (friends: ReferenceFriendAssembly list)
        (declaringTypes: ReferenceTypeAccessibility list)
        (memberAccessibility: ReferenceMemberAccessibility)
        (request: ReferenceAccessRequest)
        =
        let assemblyAccess = hasAssemblyAccess declaringAssembly friends request

        let memberAllows =
            match memberAccessibility with
            | ReferenceMemberAccessibility.Public -> true
            | ReferenceMemberAccessibility.PrivateScope
            | ReferenceMemberAccessibility.Private -> request.IsWithinDeclaringType
            | ReferenceMemberAccessibility.Assembly -> assemblyAccess
            | ReferenceMemberAccessibility.Family -> request.IsDerivedFromDeclaringType
            | ReferenceMemberAccessibility.FamilyAndAssembly ->
                request.IsDerivedFromDeclaringType
                && assemblyAccess
            | ReferenceMemberAccessibility.FamilyOrAssembly ->
                request.IsDerivedFromDeclaringType
                || assemblyAccess

        memberAllows
        && (declaringTypes
            |> List.forall (typeAllows assemblyAccess request))

type internal ReferenceEcmaSemantics = {
    TypeSpecifications: ReferenceCliType list
    MemberReferences: ReferenceMemberReference list
    MethodSpecifications: ReferenceMethodSpecification list
    StandaloneSignatures: ReferenceStandaloneSignature list
    CustomAttributes: ReferenceEcmaCustomAttribute list
    FriendAssemblies: ReferenceFriendAssembly list
}

type internal ReferenceEcmaSnapshot = {
    StableId: string
    LogicalPath: string
    ContentFingerprint: string
    Assembly: ReferenceAssemblyIdentity option
    Module: ReferenceModuleIdentity
    AssemblyReferences: ReferenceAssemblyReference list
    ModuleReferences: ReferenceModuleReference list
    Files: ReferenceFileIdentity list
    TypeDefinitions: ReferenceTypeDefinition list
    TypeReferences: ReferenceTypeIdentity list
    ExportedTypes: ReferenceExportedType list
    ManifestResources: ReferenceManifestResource list
    Semantics: ReferenceSemanticDemand<ReferenceEcmaSemantics>
}

type internal ReferenceContentFingerprint = { Value: string }
type internal ReferenceSemanticApiFingerprint = { Value: string }
type internal ReferenceUniverseFingerprint = { Value: string }

[<RequireQualifiedAccess>]
type internal ReferenceFSharpAccessibility =
    | Public
    | Internal
    | Private
    | Protected
    | ProtectedInternal
    | PrivateProtected

[<RequireQualifiedAccess>]
type internal ReferenceFSharpConstraint =
    | CoercesTo of ReferenceFSharpType
    | SupportsNull
    | NotSupportsNull
    | DefaultConstructor
    | ValueType
    | ReferenceType
    | Comparison
    | Equality
    | Unmanaged
    | Delegate of tupleType: ReferenceFSharpType * returnType: ReferenceFSharpType
    | Member of name: string * argumentType: ReferenceFSharpType * returnType: ReferenceFSharpType

and [<RequireQualifiedAccess>] internal ReferenceFSharpType =
    | Cli of ReferenceCliType
    | Variable of stableId: string
    | Function of domain: ReferenceFSharpType * range: ReferenceFSharpType
    | Tuple of isStruct: bool * elements: ReferenceFSharpType list
    | Application of entityStableId: string * arguments: ReferenceFSharpType list
    | Abbreviation of entityStableId: string * target: ReferenceFSharpType
    | AnonymousRecord of
        isStruct: bool *
        fieldNames: string list *
        fieldTypes: ReferenceFSharpType list

type internal ReferenceFSharpTypeParameter = {
    StableId: string
    Name: string
    Constraints: ReferenceFSharpConstraint list
}

type internal ReferenceFSharpValue = {
    StableId: string
    LogicalName: string
    CompiledName: string
    Accessibility: ReferenceFSharpAccessibility
    Type: ReferenceFSharpType
    CurriedArgumentGroups: ReferenceFSharpType list list
    Attributes: ReferenceEcmaCustomAttribute list
    InlineOptimizationPayload: byte list option
}

[<RequireQualifiedAccess>]
type internal ReferenceFSharpEntityKind =
    | Namespace
    | Module
    | Type
    | Record
    | Union
    | Abbreviation

type internal ReferenceFSharpEntity = {
    StableId: string
    LogicalName: string
    CompiledName: string
    Kind: ReferenceFSharpEntityKind
    Accessibility: ReferenceFSharpAccessibility
    TypeParameters: ReferenceFSharpTypeParameter list
    Values: ReferenceFSharpValue list
    NestedEntities: ReferenceFSharpEntity list
    Attributes: ReferenceEcmaCustomAttribute list
}

type internal ReferenceFSharpCompilationUnit = {
    Name: string
    QualifiedName: string
    Entities: ReferenceFSharpEntity list
    SignatureVersion: ReferenceVersion
    OptimizationPayload: byte list option
}

type internal ReferenceImportLimits = {
    MaximumPeImageBytes: int64
    MaximumMetadataBytes: int64
    MaximumHeapValueBytes: int
    MaximumRowsPerTable: int
    MaximumRowsAcrossTables: int
    MaximumInflatedFSharpStreamBytes: int
    MaximumDeflateExpansionRatio: int
    MaximumSignatureDepth: int
    MaximumSignatureNodes: int
    MaximumGenericConstraints: int
    MaximumFSharpInternEntries: int
    MaximumFSharpGraphNodes: int
    MaximumPendingFixups: int
    MaximumResolutionDepth: int
} with

    static member Production = {
        MaximumPeImageBytes =
            512L
            * 1024L
            * 1024L
        MaximumMetadataBytes =
            256L
            * 1024L
            * 1024L
        MaximumHeapValueBytes =
            128
            * 1024
            * 1024
        MaximumRowsPerTable = 1_000_000
        MaximumRowsAcrossTables = 4_000_000
        MaximumInflatedFSharpStreamBytes =
            256
            * 1024
            * 1024
        MaximumDeflateExpansionRatio = 2_048
        MaximumSignatureDepth = 256
        MaximumSignatureNodes = 1_000_000
        MaximumGenericConstraints = 65_535
        MaximumFSharpInternEntries = 1_000_000
        MaximumFSharpGraphNodes = 2_000_000
        MaximumPendingFixups = 2_000_000
        MaximumResolutionDepth = 1_024
    }

type internal ReferenceImportRequest = {
    Limits: ReferenceImportLimits
    CancellationToken: CancellationToken
    Snapshots: TargetReferenceSnapshot list
}
