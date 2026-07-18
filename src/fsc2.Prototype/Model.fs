namespace FSharp2.Compiler

open System
open System.Collections.Immutable

/// PROTOTYPE model for issue #8. These types deliberately contain no SRM
/// handles, tokens, offsets, RVAs, or final artifact identities.
[<Struct>]
type internal SourcePosition =
    { Offset: int
      Line: int
      Column: int }

    override _.ToString() = "SourcePosition"

[<Struct>]
type internal SourceRange = {
    Start: SourcePosition
    End: SourcePosition
}

with
    override _.ToString() = "SourceRange"

type internal CompilerDiagnostic = {
    Code: string
    Message: string
    Path: string option
    Range: SourceRange option
}

with
    override _.ToString() = "CompilerDiagnostic"

type internal SourceInput =
    { Path: string
      Text: string }

    override _.ToString() = "SourceInput"

type internal CompilerInvocation = {
    AssemblyPath: string
    PdbPath: string
    SourcePaths: string list
    Deterministic: bool
    PortablePdb: bool
    SourceLinkJson: byte array
    FullPaths: bool
    FlatErrors: bool
    Utf8Output: bool
    ServerName: string option
    TracePath: string option
}

with
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
}

with
    override _.ToString() = "ParsedDeclaration"

type internal ParsedModule = {
    Name: string
    Path: string
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    Declarations: ParsedDeclaration list
}

with
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
}

with
    override _.ToString() = "TypedDeclaration"

type internal TypedModule = {
    Name: string
    Path: string
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    Declarations: TypedDeclaration list
    ExportFingerprint: string
}

with
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
    DocumentPath: string
    DocumentChecksum: ImmutableArray<byte>
    Range: SourceRange
}

with
    override _.ToString() = "SymbolicMethodFragment"

type internal SymbolicAssembly = {
    SchemaVersion: int
    AssemblyName: string
    ModuleName: string
    PublicFingerprint: string
    Methods: SymbolicMethodFragment list
}

with
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
}

with
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
}

with
    override _.ToString() = "ServiceCompilationResponse"

type internal QueryStatistics = {
    ParseHits: int
    ParseMisses: int
    CheckHits: int
    CheckMisses: int
    LowerHits: int
    LowerMisses: int
}

with
    override _.ToString() = "QueryStatistics"
