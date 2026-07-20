namespace FSharp2.Compiler

open System
open System.Collections.Immutable

module internal CompilerSchema =
    [<Literal>]
    let Query = 6

/// PROTOTYPE model for issue #8. These types deliberately contain no SRM
/// handles, tokens, offsets, RVAs, or final artifact identities.
[<Struct>]
type internal SourcePosition = {
    Offset: int
    Line: int
    Column: int
} with

    override _.ToString() = "SourcePosition"

[<Struct>]
type internal SourceRange = {
    Start: SourcePosition
    End: SourcePosition
} with

    override _.ToString() = "SourceRange"

type internal CompilerDiagnostic = {
    Code: string
    Message: string
    Path: string option
    Range: SourceRange option
} with

    override _.ToString() = "CompilerDiagnostic"

type internal SourceInput = {
    Path: string
    Text: string
} with

    override _.ToString() = "SourceInput"

type internal ManagedResourceInput = {
    LogicalName: string
    IsPublic: bool
    Data: byte array
} with

    override _.ToString() = "ManagedResourceInput"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal StrongNameMode =
    | Unsigned
    | DelaySign
    | PublicSign
    | FullSign

    override this.ToString() =
        match this with
        | Unsigned -> "Unsigned"
        | DelaySign -> "DelaySign"
        | PublicSign -> "PublicSign"
        | FullSign -> "FullSign"

type internal CompilerInvocation = {
    AssemblyPath: string
    PdbPath: string
    ReferenceAssemblyPath: string option
    DocumentationPath: string option
    SourcePaths: string list
    EmbeddedSourcePaths: string list
    ReferencePaths: string list
    Defines: string list
    LanguageVersion: string option
    Optimize: bool
    CheckNulls: bool
    NoFramework: bool
    WarningLevel: int option
    DisabledWarnings: string list
    TreatWarningsAsErrors: bool
    WarningsAsErrors: string list
    HighEntropyVA: bool
    TargetProfile: string option
    NoCopyFSharpCore: bool
    SimpleResolution: bool
    TestFlags: string list
    Deterministic: bool
    PortablePdb: bool
    SourceLinkJson: byte array
    DebugDocumentPaths: string list
    ManagedResource: ManagedResourceInput option
    NativeResourceData: byte array
    StrongNameMode: StrongNameMode
    StrongNameKey: byte array
    FullPaths: bool
    FlatErrors: bool
    Utf8Output: bool
    ServerName: string option
    TracePath: string option
} with

    override _.ToString() = "CompilerInvocation"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedExpression =
    | IntegerLiteral of int
    | StringLiteral of string
    | TraitCall of
        receiverName: string *
        memberName: string *
        argumentNames: string list

    override _.ToString() = "ParsedExpression"

type internal ParsedType =
    | ParsedInt32

    override _.ToString() = "ParsedType"

type internal QualifiedTypeName = {
    Namespace: string
    Name: string
} with

    override _.ToString() = "QualifiedTypeName"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedTypeExpression =
    | ParsedNamedType of QualifiedTypeName * SourceRange
    | ParsedTypeParameter of string * SourceRange
    | ParsedGenericTypeApplication of
        genericType: ParsedTypeExpression *
        arguments: ParsedTypeExpression list *
        range: SourceRange
    | ParsedFunctionType of ParsedTypeExpression * ParsedTypeExpression * SourceRange

    member this.Range =
        match this with
        | ParsedNamedType(_, range)
        | ParsedTypeParameter(_, range)
        | ParsedGenericTypeApplication(_, _, range)
        | ParsedFunctionType(_, _, range) -> range

    override _.ToString() = "ParsedTypeExpression"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedTypeConstraint =
    | ParsedSubtypeConstraint of
        typeParameter: string *
        superType: ParsedTypeExpression *
        range: SourceRange
    | ParsedMemberConstraint of
        typeParameter: string *
        memberName: string *
        memberType: ParsedTypeExpression *
        range: SourceRange

    member this.Range =
        match this with
        | ParsedSubtypeConstraint(_, _, range)
        | ParsedMemberConstraint(_, _, _, range) -> range

    override _.ToString() = "ParsedTypeConstraint"

type internal ParsedMethodDeclaration = {
    Name: string
    IsUnitFunction: bool
    DeclaredType: ParsedType option
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedMethodDeclaration"

type internal ParsedLiteralFieldDeclaration = {
    Name: string
    Value: string
    ValueRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedLiteralFieldDeclaration"

type internal ParsedParameter = {
    Name: string
    Type: ParsedTypeExpression
    Range: SourceRange
} with

    override _.ToString() = "ParsedParameter"

type internal ParsedStaticMethodDeclaration = {
    Name: string
    TypeParameters: string list
    Constraints: ParsedTypeExpression list
    Parameters: ParsedParameter list
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedStaticMethodDeclaration"

type internal ParsedStaticTypeDeclaration = {
    Name: string
    Methods: ParsedStaticMethodDeclaration list
    Range: SourceRange
} with

    override _.ToString() = "ParsedStaticTypeDeclaration"

type internal ParsedTypeReference = {
    Type: ParsedTypeExpression
    AllowsNull: bool
    Range: SourceRange
} with

    override _.ToString() = "ParsedTypeReference"

type internal ParsedTypeAbbreviationDeclaration = {
    Name: string
    TypeParameters: string list
    Constraints: ParsedTypeConstraint list
    Target: ParsedTypeReference
    Range: SourceRange
} with

    override _.ToString() = "ParsedTypeAbbreviationDeclaration"

type internal ParsedDeclaration =
    | ParsedMethod of ParsedMethodDeclaration
    | ParsedLiteralField of ParsedLiteralFieldDeclaration
    | ParsedTypeAbbreviation of ParsedTypeAbbreviationDeclaration
    | ParsedStaticType of ParsedStaticTypeDeclaration

    override _.ToString() = "ParsedDeclaration"

type internal ParsedNamedStringArgument = {
    Name: string
    Value: string
} with

    override _.ToString() = "ParsedNamedStringArgument"

type internal AssemblyAttributeKind =
    | TargetFrameworkAttribute
    | AssemblyTitleAttribute
    | AssemblyProductAttribute
    | AssemblyVersionAttribute
    | AssemblyMetadataAttribute
    | AssemblyFileVersionAttribute
    | AssemblyInformationalVersionAttribute

    override _.ToString() = "AssemblyAttributeKind"

type internal ParsedAssemblyAttribute = {
    AttributeType: QualifiedTypeName
    ConstructorArguments: string list
    NamedArguments: ParsedNamedStringArgument list
    Range: SourceRange
} with

    override _.ToString() = "ParsedAssemblyAttribute"

type internal ParsedModule = {
    StableId: string
    Namespace: string
    Name: string
    IsPublic: bool
    OpenedNamespaces: string list
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    AssemblyAttributes: ParsedAssemblyAttribute list
    Declarations: ParsedDeclaration list
} with

    override _.ToString() = "ParsedModule"

type internal CliType =
    | CliInt32
    | CliBoolean
    | CliString
    | CliMethodTypeParameter of int

    override _.ToString() = "CliType"

type internal TypedExpression =
    | TypedIntegerLiteral of int
    | TypedTraitCall of
        receiverName: string *
        memberName: string *
        argumentNames: string list

    override _.ToString() = "TypedExpression"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedTypeExpression =
    | TypedNamedType of QualifiedTypeName
    | TypedTypeParameter of string
    | TypedGenericTypeApplication of
        genericType: TypedTypeExpression *
        arguments: TypedTypeExpression list
    | TypedFunctionType of TypedTypeExpression * TypedTypeExpression

    override _.ToString() = "TypedTypeExpression"

type internal TypedParameter = {
    Name: string
    Type: CliType
} with

    override _.ToString() = "TypedParameter"

type internal TypedMethodDeclaration = {
    StableId: string
    Name: string
    GenericParameters: string list
    Constraints: TypedTypeExpression list
    Parameters: TypedParameter list
    ReturnType: CliType
    Body: TypedExpression
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedMethodDeclaration"

type internal TypedLiteralFieldDeclaration = {
    StableId: string
    Name: string
    Value: string
    ExportFingerprint: string
} with

    override _.ToString() = "TypedLiteralFieldDeclaration"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedTypeConstraint =
    | TypedSubtypeConstraint of typeParameter: string * superType: TypedTypeExpression
    | TypedMemberConstraint of
        typeParameter: string *
        memberName: string *
        memberType: TypedTypeExpression

    override _.ToString() = "TypedTypeConstraint"

type internal TypedTypeAbbreviationDeclaration = {
    StableId: string
    Name: string
    TypeParameters: string list
    Constraints: TypedTypeConstraint list
    TargetType: TypedTypeExpression
    AllowsNull: bool
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedTypeAbbreviationDeclaration"

type internal TypedStaticTypeDeclaration = {
    StableId: string
    Name: string
    Methods: TypedMethodDeclaration list
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedStaticTypeDeclaration"

type internal TypedDeclaration =
    | TypedMethod of TypedMethodDeclaration
    | TypedLiteralField of TypedLiteralFieldDeclaration
    | TypedTypeAbbreviation of TypedTypeAbbreviationDeclaration
    | TypedStaticType of TypedStaticTypeDeclaration

    member this.StableId =
        match this with
        | TypedMethod declaration -> declaration.StableId
        | TypedLiteralField declaration -> declaration.StableId
        | TypedTypeAbbreviation declaration -> declaration.StableId
        | TypedStaticType declaration -> declaration.StableId

    member this.ExportFingerprint =
        match this with
        | TypedMethod declaration -> declaration.ExportFingerprint
        | TypedLiteralField declaration -> declaration.ExportFingerprint
        | TypedTypeAbbreviation declaration -> declaration.ExportFingerprint
        | TypedStaticType declaration -> declaration.ExportFingerprint

    override _.ToString() = "TypedDeclaration"

type internal TypedAssemblyAttribute = {
    StableId: string
    Kind: AssemblyAttributeKind
    AttributeType: QualifiedTypeName
    ConstructorArguments: string list
    NamedArguments: ParsedNamedStringArgument list
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedAssemblyAttribute"

type internal TypedModule = {
    StableId: string
    Namespace: string
    Name: string
    IsPublic: bool
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    AssemblyAttributes: TypedAssemblyAttribute list
    Declarations: TypedDeclaration list
    ExportFingerprint: string
} with

    override _.ToString() = "TypedModule"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicInstruction =
    | LoadInt32 of int
    | LoadString of string
    | NewObject of declaringType: QualifiedTypeName * parameterTypes: CliType list
    | Throw
    | Return

    override _.ToString() = "SymbolicInstruction"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicMethodKind =
    | ModuleFunction
    | StaticInlineMemberStub

    override _.ToString() = "SymbolicMethodKind"

type internal SymbolicMethodFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Kind: SymbolicMethodKind
    GenericParameters: string list
    Constraints: TypedTypeExpression list
    Parameters: TypedParameter list
    ReturnType: CliType
    Instructions: SymbolicInstruction list
    DependencyIds: string list
    ContentHash: string
    DocumentIndex: int
    DocumentChecksum: ImmutableArray<byte>
    Range: SourceRange
} with

    override _.ToString() = "SymbolicMethodFragment"

type internal SymbolicLiteralFieldFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Value: string
    ContentHash: string
} with

    override _.ToString() = "SymbolicLiteralFieldFragment"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicTypeKind =
    | ModuleContainer
    | StaticMemberContainer

    override _.ToString() = "SymbolicTypeKind"

type internal SymbolicTypeFragment = {
    SchemaVersion: int
    StableId: string
    Namespace: string
    Name: string
    IsPublic: bool
    Kind: SymbolicTypeKind
    LiteralFields: SymbolicLiteralFieldFragment list
    Methods: SymbolicMethodFragment list
} with

    override _.ToString() = "SymbolicTypeFragment"

type internal SymbolicDocumentFragment = {
    SchemaVersion: int
    StableId: string
    Checksum: ImmutableArray<byte>
} with

    override _.ToString() = "SymbolicDocumentFragment"

type internal SymbolicTypeAbbreviationFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    TypeParameters: string list
    Constraints: TypedTypeConstraint list
    TargetType: TypedTypeExpression
    AllowsNull: bool
    ContentHash: string
} with

    override _.ToString() = "SymbolicTypeAbbreviationFragment"

type internal SymbolicNamedStringArgument = {
    Name: string
    Value: string
} with

    override _.ToString() = "SymbolicNamedStringArgument"

type internal SymbolicAssemblyAttributeFragment = {
    SchemaVersion: int
    StableId: string
    Kind: AssemblyAttributeKind
    AttributeType: QualifiedTypeName
    ConstructorArguments: string list
    NamedArguments: SymbolicNamedStringArgument list
    ContentHash: string
} with

    override _.ToString() = "SymbolicAssemblyAttributeFragment"

type internal SymbolicModuleFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    TypeAbbreviations: SymbolicTypeAbbreviationFragment list
    Types: SymbolicTypeFragment list
} with

    override _.ToString() = "SymbolicModuleFragment"

type internal SymbolicAssembly = {
    SchemaVersion: int
    StableId: string
    AssemblyName: string
    AssemblyVersion: Version
    PublicFingerprint: string
    Documents: SymbolicDocumentFragment list
    AssemblyAttributes: SymbolicAssemblyAttributeFragment list
    Module: SymbolicModuleFragment
} with

    override _.ToString() = "SymbolicAssembly"

type internal CompilerQueryResult = {
    SymbolicAssembly: SymbolicAssembly
    QuerySchema: int
    NodeKind: string
    ContentFingerprint: string
    PreviousContentFingerprint: string
    InvalidationReason: string
    ParseKey: string
    CheckKey: string
    LowerKey: string
    DependencyCount: int
    ParseDecision: string
    CheckDecision: string
    LowerDecision: string
    ParseElapsedMicroseconds: int64
    CheckElapsedMicroseconds: int64
    LowerElapsedMicroseconds: int64
} with

    override _.ToString() = "CompilerQueryResult"

type internal ServiceCompilationResponse = {
    ExitCode: int
    Error: string
    ServiceProcessId: int
    QuerySchema: int
    NodeKind: string
    ContentFingerprint: string
    PreviousContentFingerprint: string
    InvalidationReason: string
    ParseKey: string
    CheckKey: string
    LowerKey: string
    DependencyCount: int
    ParseDecision: string
    CheckDecision: string
    LowerDecision: string
    ParseElapsedMicroseconds: int64
    CheckElapsedMicroseconds: int64
    LowerElapsedMicroseconds: int64
    LinkElapsedMicroseconds: int64
    PublishElapsedMicroseconds: int64
    CompileElapsedMicroseconds: int64
    ExportFingerprint: string
    FragmentHash: string
    Emitted: bool
} with

    override _.ToString() = "ServiceCompilationResponse"

type internal QueryStatistics = {
    ParseHits: int
    ParseMisses: int
    CheckHits: int
    CheckMisses: int
    LowerHits: int
    LowerMisses: int
} with

    override _.ToString() = "QueryStatistics"
