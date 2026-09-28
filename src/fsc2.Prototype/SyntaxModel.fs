namespace FSharp2.Compiler

open System.Collections.Immutable

type internal SyntaxIdentifier = { Text: string; Range: SourceRange }

type internal LongIdentifier = {
    Parts: ImmutableArray<SyntaxIdentifier>
    Range: SourceRange
} with

    member this.Text =
        this.Parts
        |> Seq.map _.Text
        |> String.concat "."

type internal MissingSyntax = { Expected: string; Range: SourceRange }

type internal SkippedSyntax = {
    Tokens: ImmutableArray<LexicalToken>
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal SyntaxConstant =
    | Numeric of string
    | String of string
    | Character of string
    | Boolean of bool
    | Unit

[<RequireQualifiedAccess>]
type internal SyntaxPattern =
    | Named of SyntaxIdentifier
    | Wildcard of SourceRange
    | Constant of SyntaxConstant * SourceRange
    | Parenthesized of SyntaxPattern * SourceRange
    | Tuple of ImmutableArray<SyntaxPattern> * SourceRange
    | Missing of MissingSyntax

    member this.Range =
        match this with
        | Named identifier -> identifier.Range
        | Wildcard range
        | Constant(_, range)
        | Parenthesized(_, range)
        | Tuple(_, range) -> range
        | Missing missing -> missing.Range

[<RequireQualifiedAccess>]
type internal SyntaxExpression =
    | Constant of SyntaxConstant * SourceRange
    | Identifier of LongIdentifier
    | Parenthesized of SyntaxExpression * SourceRange
    | Tuple of ImmutableArray<SyntaxExpression> * SourceRange
    | Application of SyntaxExpression * SyntaxExpression * SourceRange
    | Infix of SyntaxIdentifier * SyntaxExpression * SyntaxExpression * SourceRange
    | Missing of MissingSyntax

    member this.Range =
        match this with
        | Identifier name -> name.Range
        | Constant(_, range)
        | Parenthesized(_, range)
        | Tuple(_, range)
        | Application(_, _, range)
        | Infix(_, _, _, range) -> range
        | Missing missing -> missing.Range

type internal SyntaxBinding = {
    Head: SyntaxPattern
    Parameters: ImmutableArray<SyntaxPattern>
    Body: SyntaxExpression
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal ImplementationDeclaration =
    | Open of LongIdentifier * SourceRange
    | Let of isRecursive: bool * ImmutableArray<SyntaxBinding> * SourceRange
    | NestedModule of SyntaxIdentifier * ImmutableArray<ImplementationDeclaration> * SourceRange
    | Skipped of SkippedSyntax

    member this.Range =
        match this with
        | Open(_, range)
        | Let(_, _, range)
        | NestedModule(_, _, range) -> range
        | Skipped skipped -> skipped.Range

[<RequireQualifiedAccess>]
type internal ModuleOrNamespaceKind =
    | AnonymousModule
    | NamedModule
    | Namespace

type internal ModuleOrNamespaceSyntax = {
    Kind: ModuleOrNamespaceKind
    Name: LongIdentifier option
    Declarations: ImmutableArray<ImplementationDeclaration>
    Range: SourceRange
}

type internal ImplementationFileSyntax = {
    StableId: StableIdentity
    LogicalPath: string
    Contents: ImmutableArray<ModuleOrNamespaceSyntax>
}

type internal SyntaxDiagnostic = {
    Code: string
    Message: string
    Range: SourceRange
}

type internal ImplementationFileParseResult = {
    File: ImplementationFileSyntax
    Diagnostics: ImmutableArray<SyntaxDiagnostic>
}
