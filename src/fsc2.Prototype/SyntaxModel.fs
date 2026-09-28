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
type internal SyntaxType =
    | LongIdentifier of LongIdentifier
    | Variable of SyntaxIdentifier
    | Application of SyntaxType * ImmutableArray<SyntaxType> * isPostfix: bool * SourceRange
    | Function of SyntaxType * SyntaxType * SourceRange
    | Tuple of ImmutableArray<SyntaxType> * SourceRange
    | Parenthesized of SyntaxType * SourceRange
    | SignatureParameter of SyntaxIdentifier * SyntaxType * SourceRange
    | Missing of MissingSyntax

    member this.Range =
        match this with
        | LongIdentifier name -> name.Range
        | Variable variable -> variable.Range
        | Application(_, _, _, range)
        | Function(_, _, range)
        | Tuple(_, range)
        | Parenthesized(_, range)
        | SignatureParameter(_, _, range) -> range
        | Missing missing -> missing.Range

[<RequireQualifiedAccess>]
type internal SyntaxPattern =
    | Named of SyntaxIdentifier
    | Wildcard of SourceRange
    | Constant of SyntaxConstant * SourceRange
    | Parenthesized of SyntaxPattern * SourceRange
    | Tuple of ImmutableArray<SyntaxPattern> * SourceRange
    | UnionCase of LongIdentifier * SyntaxPattern option * SourceRange
    | List of ImmutableArray<SyntaxPattern> * SourceRange
    | Typed of SyntaxPattern * SyntaxType * SourceRange
    | Missing of MissingSyntax

    member this.Range =
        match this with
        | Named identifier -> identifier.Range
        | Wildcard range
        | Constant(_, range)
        | Parenthesized(_, range)
        | Tuple(_, range)
        | UnionCase(_, _, range)
        | List(_, range)
        | Typed(_, _, range) -> range
        | Missing missing -> missing.Range

[<RequireQualifiedAccess>]
type internal SyntaxExpression =
    | Constant of SyntaxConstant * SourceRange
    | Identifier of LongIdentifier
    | Parenthesized of SyntaxExpression * SourceRange
    | Tuple of ImmutableArray<SyntaxExpression> * SourceRange
    | Application of SyntaxExpression * SyntaxExpression * SourceRange
    | Infix of SyntaxIdentifier * SyntaxExpression * SyntaxExpression * SourceRange
    | If of SyntaxExpression * SyntaxExpression * SyntaxExpression option * SourceRange
    | Match of SyntaxExpression * ImmutableArray<SyntaxMatchClause> * SourceRange
    | Lambda of ImmutableArray<SyntaxPattern> * SyntaxExpression * SourceRange
    | List of ImmutableArray<SyntaxExpression> * SourceRange
    | Record of ImmutableArray<SyntaxRecordFieldValue> * SourceRange
    | Missing of MissingSyntax

    member this.Range =
        match this with
        | Identifier name -> name.Range
        | Constant(_, range)
        | Parenthesized(_, range)
        | Tuple(_, range)
        | Application(_, _, range)
        | Infix(_, _, _, range)
        | If(_, _, _, range)
        | Match(_, _, range)
        | Lambda(_, _, range)
        | List(_, range)
        | Record(_, range) -> range
        | Missing missing -> missing.Range

and internal SyntaxMatchClause = {
    Pattern: SyntaxPattern
    Guard: SyntaxExpression option
    Result: SyntaxExpression
    Range: SourceRange
}

and internal SyntaxRecordFieldValue = {
    Name: LongIdentifier
    Value: SyntaxExpression
    Range: SourceRange
}

type internal SyntaxAttribute = {
    Target: SyntaxIdentifier option
    Name: LongIdentifier
    Argument: SyntaxExpression option
    Range: SourceRange
}

type internal SyntaxAttributeList = {
    Attributes: ImmutableArray<SyntaxAttribute>
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal SyntaxAccessibility =
    | Public
    | Internal
    | Private

type internal SyntaxAccess = {
    Kind: SyntaxAccessibility
    Range: SourceRange
}

type internal SyntaxBinding = {
    Attributes: ImmutableArray<SyntaxAttributeList>
    Accessibility: SyntaxAccess option
    Head: SyntaxPattern
    Parameters: ImmutableArray<SyntaxPattern>
    Body: SyntaxExpression
    Skipped: SkippedSyntax option
    Range: SourceRange
}

type internal SyntaxRecordField = {
    IsMutable: bool
    Name: SyntaxIdentifier
    Type: SyntaxType
    Range: SourceRange
}

type internal SyntaxUnionField = {
    Name: SyntaxIdentifier option
    Type: SyntaxType
    Range: SourceRange
}

type internal SyntaxUnionCase = {
    Name: SyntaxIdentifier
    Fields: ImmutableArray<SyntaxUnionField>
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal SyntaxMemberKind =
    | Instance of self: SyntaxIdentifier
    | Static

type internal SyntaxMember = {
    Attributes: ImmutableArray<SyntaxAttributeList>
    Kind: SyntaxMemberKind
    Name: SyntaxIdentifier
    Parameters: ImmutableArray<SyntaxPattern>
    Body: SyntaxExpression
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal SyntaxTypeRepresentation =
    | Record of ImmutableArray<SyntaxRecordField>
    | Union of ImmutableArray<SyntaxUnionCase>
    | Abbreviation of SyntaxType
    | Class of ImmutableArray<SyntaxMember>
    | Missing of MissingSyntax

type internal SyntaxTypeDefinition = {
    Attributes: ImmutableArray<SyntaxAttributeList>
    Accessibility: SyntaxAccess option
    Name: SyntaxIdentifier
    PrimaryConstructor: SyntaxPattern option
    Representation: SyntaxTypeRepresentation
    Skipped: SkippedSyntax option
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal ImplementationDeclaration =
    | Open of LongIdentifier * SourceRange
    | Let of isRecursive: bool * ImmutableArray<SyntaxBinding> * SourceRange
    | Do of ImmutableArray<SyntaxAttributeList> * SyntaxExpression * SourceRange
    | Type of SyntaxTypeDefinition
    | NestedModule of SyntaxIdentifier * ImmutableArray<ImplementationDeclaration> * SourceRange
    | Skipped of SkippedSyntax

    member this.Range =
        match this with
        | Open(_, range)
        | Let(_, _, range)
        | Do(_, _, range)
        | NestedModule(_, _, range) -> range
        | Type definition -> definition.Range
        | Skipped skipped -> skipped.Range

type internal SyntaxValueSignature = {
    Attributes: ImmutableArray<SyntaxAttributeList>
    Accessibility: SyntaxAccess option
    Name: SyntaxIdentifier option
    Type: SyntaxType
    Skipped: SkippedSyntax option
    Range: SourceRange
}

[<RequireQualifiedAccess>]
type internal SignatureDeclaration =
    | Open of LongIdentifier * SourceRange
    | Val of SyntaxValueSignature
    | NestedModule of SyntaxIdentifier * ImmutableArray<SignatureDeclaration> * SourceRange
    | Skipped of SkippedSyntax

    member this.Range =
        match this with
        | Open(_, range)
        | NestedModule(_, _, range) -> range
        | Val value -> value.Range
        | Skipped skipped -> skipped.Range

[<RequireQualifiedAccess>]
type internal ModuleOrNamespaceKind =
    | AnonymousModule
    | NamedModule of LongIdentifier
    | Namespace of LongIdentifier option

type internal ModuleOrNamespaceSyntax<'Declaration> = {
    Kind: ModuleOrNamespaceKind
    Declarations: ImmutableArray<'Declaration>
    DiscardedByRecovery: ImmutableArray<'Declaration>
    Range: SourceRange
}

type internal ImplementationFileSyntax = {
    StableId: StableIdentity
    LogicalPath: string
    Contents: ImmutableArray<ModuleOrNamespaceSyntax<ImplementationDeclaration>>
}

type internal SignatureFileSyntax = {
    StableId: StableIdentity
    LogicalPath: string
    Contents: ImmutableArray<ModuleOrNamespaceSyntax<SignatureDeclaration>>
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

type internal SignatureFileParseResult = {
    File: SignatureFileSyntax
    Diagnostics: ImmutableArray<SyntaxDiagnostic>
}
