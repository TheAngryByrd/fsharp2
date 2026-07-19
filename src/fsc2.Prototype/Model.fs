namespace FSharp2.Compiler

open System
open System.Collections.Immutable

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

    override _.ToString() = "ParsedExpression"

type internal ParsedType =
    | ParsedInt32

    override _.ToString() = "ParsedType"

type internal ParsedDeclaration = {
    Name: string
    IsUnitFunction: bool
    DeclaredType: ParsedType option
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedDeclaration"

type internal ParsedNamedStringArgument = {
    Name: string
    Value: string
} with

    override _.ToString() = "ParsedNamedStringArgument"

type internal QualifiedTypeName = {
    Namespace: string
    Name: string
} with

    override _.ToString() = "QualifiedTypeName"

type internal ParsedAssemblyAttribute = {
    AttributeType: QualifiedTypeName
    ConstructorArgument: string
    NamedArguments: ParsedNamedStringArgument list
    Range: SourceRange
} with

    override _.ToString() = "ParsedAssemblyAttribute"

type internal ParsedModule = {
    Name: string
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    AssemblyAttributes: ParsedAssemblyAttribute list
    Declarations: ParsedDeclaration list
} with

    override _.ToString() = "ParsedModule"

type internal ValueType =
    | Int32

    override _.ToString() = "ValueType"

type internal TypedExpression =
    | TypedIntegerLiteral of int

    override _.ToString() = "TypedExpression"

type internal TypedDeclaration = {
    StableId: string
    Name: string
    ReturnType: ValueType
    Body: TypedExpression
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedDeclaration"

type internal TypedAssemblyAttribute = {
    StableId: string
    AttributeType: QualifiedTypeName
    ConstructorArgument: string
    NamedArguments: ParsedNamedStringArgument list
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedAssemblyAttribute"

type internal TypedModule = {
    Name: string
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
    | Return

    override _.ToString() = "SymbolicInstruction"

type internal SymbolicMethodFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    ReturnType: ValueType
    Instructions: SymbolicInstruction list
    DependencyIds: string list
    ContentHash: string
    DocumentIndex: int
    DocumentChecksum: ImmutableArray<byte>
    Range: SourceRange
} with

    override _.ToString() = "SymbolicMethodFragment"

type internal SymbolicTypeFragment = {
    SchemaVersion: int
    StableId: string
    Namespace: string
    Name: string
    Methods: SymbolicMethodFragment list
} with

    override _.ToString() = "SymbolicTypeFragment"

type internal SymbolicDocumentFragment = {
    SchemaVersion: int
    StableId: string
    Checksum: ImmutableArray<byte>
} with

    override _.ToString() = "SymbolicDocumentFragment"

type internal SymbolicNamedStringArgument = {
    Name: string
    Value: string
} with

    override _.ToString() = "SymbolicNamedStringArgument"

type internal SymbolicAssemblyAttributeFragment = {
    SchemaVersion: int
    StableId: string
    AttributeType: QualifiedTypeName
    ConstructorArgument: string
    NamedArguments: SymbolicNamedStringArgument list
    ContentHash: string
} with

    override _.ToString() = "SymbolicAssemblyAttributeFragment"

type internal SymbolicModuleFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Types: SymbolicTypeFragment list
} with

    override _.ToString() = "SymbolicModuleFragment"

type internal SymbolicAssembly = {
    SchemaVersion: int
    StableId: string
    AssemblyName: string
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
