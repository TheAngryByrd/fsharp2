namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Globalization

module internal CompilerSchema =
    [<Literal>]
    let Query = 73

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

    static member FromSigningMode(mode: SigningMode) =
        match mode with
        | SigningMode.Unsigned -> Unsigned
        | SigningMode.DelaySign -> DelaySign
        | SigningMode.PublicSign -> PublicSign
        | SigningMode.FullSign -> FullSign

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

type internal ParsedType =
    | ParsedInt32

    override _.ToString() = "ParsedType"

type internal QualifiedTypeName = {
    Namespace: string
    Name: string
} with

    override _.ToString() = "QualifiedTypeName"

[<Struct>]
type internal TypeNameArity = {
    Name: string
    GenericArity: int
} with

    override _.ToString() = "TypeNameArity"

type internal ResolvedTypeName = {
    TypeName: QualifiedTypeName
    DeclarationId: string
    AssemblyName: string
    IsValueType: bool
} with

    override _.ToString() = "ResolvedTypeName"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedAttributeArgument =
    | ParsedBooleanAttributeArgument of bool
    | ParsedStringAttributeArgument of string

    override _.ToString() = "ParsedAttributeArgument"

type internal ParsedAttribute = {
    AttributeType: QualifiedTypeName
    ConstructorArguments: ParsedAttributeArgument list
    Range: SourceRange
} with

    override _.ToString() = "ParsedAttribute"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedTypeExpression =
    | ParsedNamedType of QualifiedTypeName * SourceRange
    | ParsedTypeParameter of string * SourceRange
    | ParsedWildcardType of SourceRange
    | ParsedFlexibleType of superType: ParsedTypeExpression * range: SourceRange
    | ParsedGenericTypeApplication of
        genericType: ParsedTypeExpression *
        arguments: ParsedTypeExpression list *
        range: SourceRange
    | ParsedTupleType of elements: ParsedTypeExpression list * range: SourceRange
    | ParsedFunctionType of ParsedTypeExpression * ParsedTypeExpression * SourceRange

    member this.Range =
        match this with
        | ParsedNamedType(_, range)
        | ParsedTypeParameter(_, range)
        | ParsedWildcardType range
        | ParsedFlexibleType(_, range)
        | ParsedGenericTypeApplication(_, _, range)
        | ParsedTupleType(_, range)
        | ParsedFunctionType(_, _, range) -> range

    override _.ToString() = "ParsedTypeExpression"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedMatchPattern =
    | ParsedTypeTestPattern of
        targetType: ParsedTypeExpression *
        bindingName: string *
        range: SourceRange
    | ParsedNullPattern of range: SourceRange
    | ParsedNamedPattern of name: string * range: SourceRange
    | ParsedUnitPattern of range: SourceRange
    | ParsedTuplePattern of elements: ParsedMatchPattern list * range: SourceRange
    | ParsedUnionCasePattern of caseName: string * argument: ParsedMatchPattern * range: SourceRange

    member this.Range =
        match this with
        | ParsedTypeTestPattern(_, _, range)
        | ParsedNullPattern range
        | ParsedNamedPattern(_, range)
        | ParsedUnitPattern range
        | ParsedTuplePattern(_, range)
        | ParsedUnionCasePattern(_, _, range) -> range

    override _.ToString() = "ParsedMatchPattern"

type internal ComputationReturnKind =
    | ComputationReturn
    | ComputationReturnFrom

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedExpression =
    | IntegerLiteral of int
    | UnitLiteral
    | BooleanLiteral of bool
    | StringLiteral of string
    | NullLiteral
    | ValueReference of string
    | NamedCallArgument of name: string * value: ParsedExpression * range: SourceRange
    | AddressOfExpression of rootName: string * memberPath: string list
    | UnitApplication of functionName: string
    | MemberCall of receiverName: string * memberName: string * arguments: ParsedExpression list
    | StaticTypeMemberCall of
        receiverType: ParsedTypeExpression *
        memberName: string *
        arguments: ParsedExpression list
    | GenericMemberCall of
        receiverName: string *
        memberName: string *
        typeArguments: ParsedTypeExpression list *
        arguments: ParsedExpression list
    | BoundInstanceMember of receiverName: string * memberName: string
    | MemberAssignment of rootName: string * memberPath: string list * value: ParsedExpression
    | SequentialExpression of ParsedExpression list
    | FunctionApplication of
        functionExpression: ParsedExpression *
        argumentExpression: ParsedExpression
    | ExpressionMemberCall of
        receiver: ParsedExpression *
        memberName: string *
        arguments: ParsedExpression list
    | ExpressionMemberAccess of receiver: ParsedExpression * memberName: string
    | ConditionalExpression of
        condition: ParsedExpression *
        ifTrue: ParsedExpression *
        ifFalse: ParsedExpression *
        conditionRange: SourceRange *
        ifTrueRange: SourceRange *
        ifFalseRange: SourceRange
    | ExplicitUpcastExpression of expression: ParsedExpression * targetType: ParsedTypeExpression
    | SequentialValueExpression of (ParsedExpression * SourceRange) list
    | LocalAssignment of name: string * value: ParsedExpression
    | BooleanNegationExpression of expression: ParsedExpression * range: SourceRange
    | EqualityExpression of left: ParsedExpression * right: ParsedExpression * range: SourceRange
    | TryWithExpression of
        body: ParsedExpression *
        bindingName: string *
        handler: ParsedExpression *
        tryRange: SourceRange *
        withRange: SourceRange *
        bodyRange: SourceRange *
        handlerRange: SourceRange *
        range: SourceRange
    | TryFinallyExpression of
        body: ParsedExpression *
        compensation: ParsedExpression *
        tryRange: SourceRange *
        finallyRange: SourceRange *
        bodyRange: SourceRange *
        compensationRange: SourceRange *
        range: SourceRange
    | MatchExpression of
        input: ParsedExpression *
        clauses:
            (ParsedMatchPattern *
            (ParsedExpression * SourceRange) option *
            ParsedExpression *
            SourceRange) list *
        matchHeaderRange: SourceRange *
        range: SourceRange
    | LetExpression of
        bindingName: string *
        isMutable: bool *
        isInline: bool *
        value: ParsedExpression *
        body: ParsedExpression *
        bindingRange: SourceRange *
        bodyRange: SourceRange
    | ComputationExpression of builderName: string * body: ParsedExpression * range: SourceRange
    | ComputationBindingExpression of
        bindingName: string *
        input: ParsedExpression *
        body: ParsedExpression *
        bindingRange: SourceRange *
        bodyRange: SourceRange
    | ComputationDoExpression of input: ParsedExpression * range: SourceRange
    | WhileExpression of
        condition: ParsedExpression *
        body: ParsedExpression *
        conditionRange: SourceRange *
        bodyRange: SourceRange *
        range: SourceRange
    | ForExpression of
        bindingName: string *
        sequence: ParsedExpression *
        body: ParsedExpression *
        bindingRange: SourceRange *
        sequenceRange: SourceRange *
        bodyRange: SourceRange *
        range: SourceRange
    | LambdaExpression of
        parameterName: string *
        parameterType: ParsedTypeExpression option *
        body: ParsedExpression *
        range: SourceRange
    | UnitLambdaExpression of body: ParsedExpression * range: SourceRange
    | TupleExpression of elements: ParsedExpression list * range: SourceRange
    | StructTupleExpression of elements: ParsedExpression list * range: SourceRange
    | TypeConstruction of
        constructedType: ParsedTypeExpression *
        arguments: ParsedExpression list *
        argumentRange: SourceRange
    | BindReturnFromComputation of
        builderName: string *
        bindings: (string * ParsedExpression) list *
        returnKind: ComputationReturnKind *
        returnFrom: ParsedExpression *
        range: SourceRange
    | ObjectExpression of
        baseType: ParsedTypeExpression *
        constructorArguments: ParsedExpression list *
        members: ParsedObjectExpressionMember list *
        range: SourceRange

    override _.ToString() = "ParsedExpression"

and internal ParsedObjectExpressionMember = {
    IsOverride: bool
    ReceiverName: string
    Name: string
    ParameterNames: string list
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedObjectExpressionMember"

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

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedMethodConstraint =
    | ParsedAbbreviationConstraint of ParsedTypeExpression
    | ParsedDirectConstraint of ParsedTypeConstraint

    override _.ToString() = "ParsedMethodConstraint"

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

type internal ParsedModuleValueDeclaration = {
    Name: string
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedModuleValueDeclaration"

type internal ParsedParameter = {
    Attributes: ParsedAttribute list
    Name: string
    Type: ParsedTypeExpression
    Range: SourceRange
} with

    override _.ToString() = "ParsedParameter"

type internal ParsedStaticMethodDeclaration = {
    Attributes: ParsedAttribute list
    IsInline: bool
    IsPublic: bool
    Name: string
    TypeParameters: string list
    Constraints: ParsedMethodConstraint list
    ArgumentCounts: int list
    Parameters: ParsedParameter list
    ReturnType: ParsedTypeExpression option
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedStaticMethodDeclaration"

type internal ParsedStaticTypeDeclaration = {
    IsPublic: bool
    Name: string
    Methods: ParsedStaticMethodDeclaration list
    Range: SourceRange
} with

    override _.ToString() = "ParsedStaticTypeDeclaration"

type internal ParsedInstanceMethodDeclaration = {
    Attributes: ParsedAttribute list
    IsPublic: bool
    ReceiverName: string
    Name: string
    TypeParameters: string list
    Constraints: ParsedMethodConstraint list
    Parameters: ParsedParameter list
    ReturnType: ParsedTypeExpression option
    Body: ParsedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedInstanceMethodDeclaration"

type internal ParsedObjectMethodDeclaration =
    | ParsedInstanceObjectMethod of ParsedInstanceMethodDeclaration
    | ParsedStaticObjectMethod of ParsedStaticMethodDeclaration

    member this.Range =
        match this with
        | ParsedInstanceObjectMethod declaration -> declaration.Range
        | ParsedStaticObjectMethod declaration -> declaration.Range

    override _.ToString() = "ParsedObjectMethodDeclaration"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal ParsedObjectTypeContainer =
    | OrdinaryObjectType
    | ParsedCurrentModuleAugmentation of targetTypeName: QualifiedTypeName
    | ParsedExtensionModule of
        name: string *
        attributes: ParsedAttribute list *
        targetTypeName: QualifiedTypeName

    override _.ToString() = "ParsedObjectTypeContainer"

type internal ParsedObjectTypeDeclaration = {
    Container: ParsedObjectTypeContainer
    Name: string
    BaseType: ParsedTypeExpression option
    Methods: ParsedObjectMethodDeclaration list
    ConstructorRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "ParsedObjectTypeDeclaration"

type internal ParsedNestedModuleDeclaration = {
    Name: string
    ModulePath: string list
    Attributes: ParsedAttribute list
    OpenedNamespaces: string list
    Values: ParsedModuleValueDeclaration list
    Methods: ParsedStaticMethodDeclaration list
    Extensions: ParsedObjectTypeDeclaration list
    Modules: ParsedNestedModuleDeclaration list
    Range: SourceRange
} with

    override _.ToString() = "ParsedNestedModuleDeclaration"

type internal ParsedFieldDeclaration = {
    Name: string
    IsMutable: bool
    Type: ParsedTypeExpression
    Attributes: ParsedAttribute list
    Range: SourceRange
} with

    override _.ToString() = "ParsedFieldDeclaration"

type internal ParsedStructTypeDeclaration = {
    Name: string
    TypeParameters: string list
    Attributes: ParsedAttribute list
    Fields: ParsedFieldDeclaration list
    Methods: ParsedObjectMethodDeclaration list
    Range: SourceRange
} with

    override _.ToString() = "ParsedStructTypeDeclaration"

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
    | ParsedNestedModule of ParsedNestedModuleDeclaration
    | ParsedTypeAbbreviation of ParsedTypeAbbreviationDeclaration
    | ParsedStaticType of ParsedStaticTypeDeclaration
    | ParsedObjectType of ParsedObjectTypeDeclaration
    | ParsedStructType of ParsedStructTypeDeclaration

    override _.ToString() = "ParsedDeclaration"

module internal ParsedDeclaration =
    let tryTypeIdentity moduleStableId =
        function
        | ParsedTypeAbbreviation declaration ->
            Some(
                {
                    Name = declaration.Name
                    GenericArity = declaration.TypeParameters.Length
                },
                moduleStableId
                + "/type-abbreviation:"
                + declaration.Name
            )
        | ParsedStaticType declaration ->
            Some(
                {
                    Name = declaration.Name
                    GenericArity = 0
                },
                moduleStableId
                + "/type:"
                + declaration.Name
            )
        | ParsedObjectType declaration ->
            match declaration.Container with
            | OrdinaryObjectType ->
                Some(
                    {
                        Name = declaration.Name
                        GenericArity = 0
                    },
                    moduleStableId
                    + "/type:"
                    + declaration.Name
                )
            | ParsedCurrentModuleAugmentation _
            | ParsedExtensionModule _ -> None
        | ParsedStructType declaration ->
            Some(
                {
                    Name = declaration.Name
                    GenericArity = declaration.TypeParameters.Length
                },
                moduleStableId
                + "/type:"
                + declaration.Name
            )
        | ParsedMethod _
        | ParsedLiteralField _
        | ParsedNestedModule _ -> None

    let isTypeDeclaration declaration =
        tryTypeIdentity String.Empty declaration
        |> Option.isSome

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
    | AssemblyAutoOpenAttribute

    override _.ToString() = "AssemblyAttributeKind"

type internal ParsedAssemblyAttribute = {
    AttributeType: QualifiedTypeName
    ConstructorArguments: string list
    NamedArguments: ParsedNamedStringArgument list
    Range: SourceRange
} with

    override _.ToString() = "ParsedAssemblyAttribute"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SourceContainerKind =
    | NamespaceSource
    | ModuleSource

    override _.ToString() = "SourceContainerKind"

type internal ParsedModule = {
    StableId: string
    ContainerKind: SourceContainerKind
    Namespace: string
    Name: string
    IsPublic: bool
    OpenedNamespaces: string list
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    Attributes: ParsedAttribute list
    AssemblyAttributes: ParsedAssemblyAttribute list
    Declarations: ParsedDeclaration list
} with

    override _.ToString() = "ParsedModule"

type internal ResolvedModule = {
    SourcePath: string
    DocumentIndex: int
    Syntax: ParsedModule
    VisibleValues: Map<string, string>
    ContentFingerprint: string
} with

    override _.ToString() = "ResolvedModule"

type internal ResolvedCompilation = {
    Modules: ResolvedModule list
    ContentFingerprint: string
} with

    override _.ToString() = "ResolvedCompilation"

type internal CliTypeReference = {
    DeclarationId: string
    AssemblyName: string
    TypeName: QualifiedTypeName
    IsValueType: bool
} with

    override _.ToString() = "CliTypeReference"

type internal CliType =
    | CliInt32
    | CliBoolean
    | CliString
    | CliObject
    | CliNativeInt
    | CliVoid
    | CliTypeParameter of int
    | CliMethodTypeParameter of int
    | CliByRef of CliType
    | CliArray of elementType: CliType
    | CliNamedType of CliTypeReference
    | CliGenericType of genericType: CliTypeReference * arguments: CliType list

    override _.ToString() = "CliType"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicDeclaringType =
    | CoreDeclaringType of QualifiedTypeName
    | CliDeclaringType of CliType

    override _.ToString() = "SymbolicDeclaringType"

module internal StableIdentityFormatting =
    let qualifiedTypeName (typeName: QualifiedTypeName) =
        if String.IsNullOrEmpty(typeName.Namespace) then
            typeName.Name
        else
            typeName.Namespace
            + "."
            + typeName.Name

    let rec cliType =
        function
        | CliInt32 -> "int32"
        | CliBoolean -> "bool"
        | CliString -> "string"
        | CliObject -> "object"
        | CliNativeInt -> "native-int"
        | CliVoid -> "void"
        | CliTypeParameter index ->
            "type-parameter:"
            + index.ToString(CultureInfo.InvariantCulture)
        | CliMethodTypeParameter index ->
            "method-parameter:"
            + index.ToString(CultureInfo.InvariantCulture)
        | CliByRef elementType ->
            "byref:"
            + cliType elementType
        | CliArray elementType ->
            "array:"
            + cliType elementType
        | CliNamedType typeReference ->
            String.concat "|" [
                "named"
                typeReference.DeclarationId
                typeReference.AssemblyName
                qualifiedTypeName typeReference.TypeName
                if typeReference.IsValueType then "value" else "reference"
            ]
        | CliGenericType(typeReference, arguments) ->
            String.concat "|" [
                "generic"
                typeReference.DeclarationId
                typeReference.AssemblyName
                qualifiedTypeName typeReference.TypeName
                if typeReference.IsValueType then "value" else "reference"
                yield!
                    arguments
                    |> List.map cliType
            ]

    let symbolicDeclaringType =
        function
        | CoreDeclaringType typeName ->
            "core:"
            + qualifiedTypeName typeName
        | CliDeclaringType declaringCliType ->
            "cli:"
            + cliType declaringCliType

[<AutoOpen>]
module internal StableIdentityExtensions =
    type StableIdentity with
        static member internal qualifiedTypeName(typeName) =
            StableIdentityFormatting.qualifiedTypeName typeName

        static member internal cliType(cliType) =
            StableIdentityFormatting.cliType cliType

        static member internal symbolicDeclaringType(declaringType) =
            StableIdentityFormatting.symbolicDeclaringType declaringType

type internal SymbolicMethodReference = {
    DeclaringType: SymbolicDeclaringType
    Name: string
    GenericArity: int
    IsInstance: bool
    ParameterTypes: CliType list
    ReturnType: CliType
    TargetStableId: string option
} with

    member this.StableId =
        String.concat "|" [
            "method-reference"
            StableIdentity.symbolicDeclaringType this.DeclaringType
            this.Name
            "generic:"
            this.GenericArity.ToString(CultureInfo.InvariantCulture)
            if this.IsInstance then "instance" else "static"
            yield!
                this.ParameterTypes
                |> List.map StableIdentity.cliType
            "return"
            StableIdentity.cliType this.ReturnType
        ]

    member this.DependencyId =
        this.TargetStableId
        |> Option.defaultValue this.StableId

    override _.ToString() = "SymbolicMethodReference"

type internal TypedCallArgument =
    | TypedValueArgument of string
    | TypedAddressOfArgument of rootName: string * memberPath: string list

    override _.ToString() = "TypedCallArgument"

type internal TypedAddressSource =
    | TypedParameterAddress of parameterIndex: int * parameterType: CliType
    | TypedLocalAddress of localIndex: int * localType: CliType

    override _.ToString() = "TypedAddressSource"

type internal TypedFieldAddress = {
    DeclaringType: CliType
    Name: string
    FieldType: CliType
    TargetStableId: string option
} with

    override _.ToString() = "TypedFieldAddress"

type internal TypedResumableCodeBody =
    | TypedStoreCapturedResult of
        dataFieldName: string *
        resultFieldName: string *
        resultFieldStableId: string
    | TypedInvokeCapturedUnitFunction

    override _.ToString() = "TypedResumableCodeBody"

type internal TypedResumableCodeExpression = {
    DelegateType: CliType
    StateMachineType: CliType
    DataType: CliType
    CaptureParameterIndex: int
    CaptureName: string
    StateMachineParameterName: string
    Body: TypedResumableCodeBody
    SourceLine: int
    Range: SourceRange
} with

    override _.ToString() = "TypedResumableCodeExpression"

type internal TypedResumableTryFinallyExpression = {
    ResumableCodeModuleType: CliType
    DelegateType: CliType
    DataType: CliType
    ResultType: CliType
    ComputationParameterIndex: int
    ComputationName: string
    Compensation: TypedResumableCodeExpression
} with

    override _.ToString() = "TypedResumableTryFinallyExpression"

type internal TypedStaticMethodCallTarget = {
    DeclaringType: CliType
    StableId: string
    Name: string
    GenericArity: int
    ParameterTypes: CliType list
    ReturnType: CliType
} with

    override _.ToString() = "TypedStaticMethodCallTarget"

type internal TypedObjectConstructionTarget = {
    DeclaringType: CliType
    StableId: string
    ParameterTypes: CliType list
    ParamArrayElementType: CliType option
} with

    override _.ToString() = "TypedObjectConstructionTarget"

type internal TypedInstanceMethodCallTarget = {
    DeclaringType: CliType
    Name: string
    ParameterTypes: CliType list
    ReturnType: CliType
    ResultType: CliType
} with

    override _.ToString() = "TypedInstanceMethodCallTarget"

type internal TypedBoundInstanceMethodExpression = {
    FunctionType: CliType
    DelegateType: CliType
    ReceiverType: CliType
    TargetStableId: string
    Target: TypedInstanceMethodCallTarget
    DomainType: CliType
    RangeType: CliType
    SourceLine: int
    Range: SourceRange
} with

    override _.ToString() = "TypedBoundInstanceMethodExpression"

type internal TypedUnitLambdaExpression = {
    FunctionType: CliType
    DelegateType: CliType
    CaptureParameterIndex: int
    CaptureName: string
    CaptureType: CliType
    DomainType: CliType
    RangeType: CliType
    SourceLine: int
    Range: SourceRange
} with

    override _.ToString() = "TypedUnitLambdaExpression"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal KnownAttributeKind =
    | AutoOpenAttribute
    | RequireQualifiedAccessAttribute
    | StructAttribute
    | NoComparisonAttribute
    | NoEqualityAttribute
    | DefaultValueAttribute
    | InlineIfLambdaAttribute
    | NoEagerConstraintApplicationAttribute
    | CompilationMappingAttribute
    | CompilationArgumentCountsAttribute

    override _.ToString() = "KnownAttributeKind"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SourceConstructKind =
    | ObjectTypeConstruct
    | ModuleConstruct

    override _.ToString() = "SourceConstructKind"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedAttributeArgument =
    | TypedBooleanAttributeArgument of bool
    | TypedSourceConstructAttributeArgument of SourceConstructKind
    | TypedInt32ArrayAttributeArgument of int list

    override _.ToString() = "TypedAttributeArgument"

type internal TypedCustomAttribute = {
    StableId: string
    Kind: KnownAttributeKind
    ConstructorArguments: TypedAttributeArgument list
    ExportFingerprint: string
} with

    override _.ToString() = "TypedCustomAttribute"

type internal TypedParameter = {
    Name: string
    Type: CliType
    Attributes: TypedCustomAttribute list
} with

    override _.ToString() = "TypedParameter"

type internal TypedExpression =
    | TypedIntegerLiteral of int
    | TypedStringLiteral of string
    | TypedNullLiteral
    | TypedUnitLiteral
    | TypedReceiverReference
    | TypedParameterReference of int
    | TypedLocalReference of int
    | TypedLet of
        localIndex: int *
        name: string *
        isMutable: bool *
        localType: CliType *
        value: TypedExpression *
        body: TypedExpression *
        bindingRange: SourceRange *
        bodyRange: SourceRange
    | TypedLocalAssignment of localIndex: int * localType: CliType * value: TypedExpression
    | TypedAddressOf of source: TypedAddressSource * fields: TypedFieldAddress list
    | TypedInstanceFieldGet of receiver: TypedExpression * field: TypedFieldAddress
    | TypedStaticMethodCall of
        target: TypedStaticMethodCallTarget *
        genericArguments: CliType list *
        arguments: TypedExpression list
    | TypedObjectConstruction of
        target: TypedObjectConstructionTarget *
        arguments: TypedExpression list
    | TypedDefaultValue of valueType: CliType * localIndex: int
    | TypedFunctionApplication of
        functionType: CliType *
        domainType: CliType *
        rangeType: CliType *
        functionExpression: TypedExpression *
        argumentExpression: TypedExpression
    | TypedInstanceMethodCall of
        target: TypedInstanceMethodCallTarget *
        receiver: TypedExpression *
        arguments: TypedExpression list
    | TypedBoundInstanceMethod of TypedBoundInstanceMethodExpression
    | TypedUnitLambda of TypedUnitLambdaExpression
    | TypedFunctionLambda of TypedFunctionLambdaExpression
    | TypedDelegateLambda of TypedDelegateLambdaExpression
    | TypedValueTaskBind of TypedValueTaskBindExpression
    | TypedValueTaskApply of TypedValueTaskApplyExpression
    | TypedValueTaskZip of TypedValueTaskZipExpression
    | TypedColdTaskParallelZip of TypedColdTaskParallelZipExpression
    | TypedTaskTryFinally of TypedTaskTryFinallyExpression
    | TypedAsyncWhile of TypedAsyncWhileExpression
    | TypedCancellableTaskSequential of TypedCancellableTaskSequentialExpression
    | TypedValueTaskOfUnit of TypedValueTaskOfUnitExpression
    | TypedConditional of
        condition: TypedExpression *
        ifTrue: TypedExpression *
        ifFalse: TypedExpression *
        conditionRange: SourceRange *
        ifTrueRange: SourceRange *
        ifFalseRange: SourceRange
    | TypedUpcast of
        sourceType: CliType *
        targetType: CliType *
        targetResolvedType: ResolvedTypeName *
        expression: TypedExpression
    | TypedSequential of (TypedExpression * CliType * SourceRange) list
    | TypedBooleanNegation of expression: TypedExpression * range: SourceRange
    | TypedEquality of left: TypedExpression * right: TypedExpression * range: SourceRange
    | TypedTryWith of
        body: TypedExpression *
        handlerLocalIndex: int *
        handlerName: string *
        catchType: CliType *
        handler: TypedExpression *
        tryRange: SourceRange *
        withRange: SourceRange *
        bodyRange: SourceRange *
        handlerRange: SourceRange *
        range: SourceRange
    | TypedNullMatch of
        input: TypedExpression *
        inputType: CliType *
        localIndex: int *
        bindingName: string *
        ifNull: TypedExpression *
        ifNotNull: TypedExpression *
        matchHeaderRange: SourceRange *
        ifNullRange: SourceRange *
        ifNotNullRange: SourceRange *
        range: SourceRange
    | TypedTypeTestMatch of
        input: TypedExpression *
        targetType: CliType *
        localIndex: int *
        bindingName: string *
        guard: (TypedExpression * SourceRange) option *
        ifMatched: TypedExpression *
        ifNotMatched: TypedExpression *
        matchHeaderRange: SourceRange *
        ifMatchedRange: SourceRange *
        ifNotMatchedRange: SourceRange *
        range: SourceRange
    | TypedPatternMatch of TypedPatternMatchExpression
    | TypedResumableCode of TypedResumableCodeExpression
    | TypedResumableTryFinally of TypedResumableTryFinallyExpression
    | TypedObjectExpression of
        typeReference: CliTypeReference *
        baseType: CliType *
        constructorArguments: TypedExpression list *
        members: TypedObjectExpressionMember list *
        range: SourceRange
    | TypedTraitCall of
        receiverName: string *
        memberName: string *
        arguments: TypedCallArgument list

    override _.ToString() = "TypedExpression"

and internal TypedPatternMatchExpression = {
    Input: TypedExpression
    InputType: CliType
    InputLocalIndex: int
    Clauses: TypedPatternMatchClause list
    MatchHeaderRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "TypedPatternMatchExpression"

and internal TypedPatternMatchClause = {
    Operations: TypedPatternOperation list
    Body: TypedExpression
    BodyRange: SourceRange
} with

    override _.ToString() = "TypedPatternMatchClause"

and internal TypedPatternOperation =
    | TypedPatternTypeTest of
        input: TypedExpression *
        targetType: CliType *
        localIndex: int *
        localName: string
    | TypedPatternBinding of
        input: TypedExpression *
        inputType: CliType *
        localIndex: int *
        name: string

    override _.ToString() = "TypedPatternOperation"

and internal TypedFunctionLambdaExpression = {
    FunctionType: CliType
    ConverterType: CliType
    ClosureType: CliType
    ClosureName: string
    ParameterName: string
    ParameterType: CliType
    ReturnType: CliType
    Body: TypedExpression
    Captures: TypedFunctionLambdaCapture list
    SourceLine: int
    LambdaRange: SourceRange
    ConstructionRange: SourceRange
    EmitDefaultConstructionSequencePoint: bool
} with

    override _.ToString() = "TypedFunctionLambdaExpression"

and internal TypedFunctionLambdaCapture = {
    OuterParameterIndex: int
    Name: string
    Type: CliType
    Field: TypedFieldAddress
} with

    override _.ToString() = "TypedFunctionLambdaCapture"

and internal TypedDelegateLambdaExpression = {
    DelegateType: CliType
    ClosureType: CliType
    ClosureName: string
    Captures: TypedFunctionLambdaCapture list
    LambdaParameterNames: string list
    LambdaParameterTypes: CliType list
    LambdaReturnType: CliType
    LambdaBody: TypedExpression
    LambdaSourceLine: int
    LambdaRange: SourceRange
    ConstructionRange: SourceRange
} with

    override _.ToString() = "TypedDelegateLambdaExpression"

and internal TypedValueTaskBindExpression = {
    BuilderName: string
    ReturnKind: ComputationReturnKind
    BinderParameterIndex: int
    SourceParameterIndex: int
    InputType: CliType
    OutputType: CliType
    BinderType: CliType
    InputValueTaskType: CliType
    OutputValueTaskType: CliType
    TaskTypeReference: CliTypeReference
    TaskAwaiterTypeReference: CliTypeReference
    FuncTypeReference: CliTypeReference
    CancellationTokenType: CliType
    TaskContinuationOptionsType: CliType
    TaskSchedulerType: CliType
    TaskExtensionsTypeReference: CliTypeReference
    NonGenericTaskTypeReference: CliTypeReference
    OperationCanceledExceptionType: CliType
    ExceptionType: CliType
    Range: SourceRange
} with

    override _.ToString() = "TypedValueTaskBindExpression"

and internal TypedValueTaskApplyExpression = {
    BuilderName: string
    ApplicableParameterIndex: int
    InputParameterIndex: int
    InputType: CliType
    OutputType: CliType
    ApplierType: CliType
    ApplicableValueTaskType: CliType
    InputValueTaskType: CliType
    OutputValueTaskType: CliType
    TaskTypeReference: CliTypeReference
    TaskAwaiterTypeReference: CliTypeReference
    FuncTypeReference: CliTypeReference
    CancellationTokenType: CliType
    TaskContinuationOptionsType: CliType
    TaskSchedulerType: CliType
    TaskExtensionsTypeReference: CliTypeReference
    NonGenericTaskTypeReference: CliTypeReference
    OperationCanceledExceptionType: CliType
    ExceptionType: CliType
    Range: SourceRange
} with

    override _.ToString() = "TypedValueTaskApplyExpression"

and internal TypedValueTaskZipExpression = {
    BuilderName: string
    LeftParameterIndex: int
    RightParameterIndex: int
    LeftType: CliType
    RightType: CliType
    TupleType: CliType
    LeftValueTaskType: CliType
    RightValueTaskType: CliType
    OutputValueTaskType: CliType
    TupleTypeReference: CliTypeReference
    TaskTypeReference: CliTypeReference
    TaskAwaiterTypeReference: CliTypeReference
    FuncTypeReference: CliTypeReference
    CancellationTokenType: CliType
    TaskContinuationOptionsType: CliType
    TaskSchedulerType: CliType
    TaskExtensionsTypeReference: CliTypeReference
    NonGenericTaskTypeReference: CliTypeReference
    OperationCanceledExceptionType: CliType
    ExceptionType: CliType
    Range: SourceRange
} with

    override _.ToString() = "TypedValueTaskZipExpression"

and internal TypedColdTaskParallelZipExpression = {
    Function: TypedFunctionLambdaExpression
    Zip: TypedValueTaskZipExpression
} with

    override _.ToString() = "TypedColdTaskParallelZipExpression"

and internal TypedTaskTryFinallyExpression = {
    WaitParameterIndex: int
    WorkParameterIndex: int
    CompensationParameterIndex: int
    ResultType: CliType
    NonGenericTaskType: CliType
    OutputTaskType: CliType
    CompensationType: CliType
    TaskTypeReference: CliTypeReference
    NonGenericTaskTypeReference: CliTypeReference
    NonGenericTaskAwaiterTypeReference: CliTypeReference
    TaskAwaiterTypeReference: CliTypeReference
    FuncTypeReference: CliTypeReference
    CancellationTokenType: CliType
    TaskContinuationOptionsType: CliType
    TaskSchedulerType: CliType
    TaskExtensionsTypeReference: CliTypeReference
    OperationCanceledExceptionType: CliType
    ExceptionType: CliType
    Range: SourceRange
} with

    override _.ToString() = "TypedTaskTryFinallyExpression"

and internal TypedAsyncWhileExpression = {
    GuardParameterIndex: int
    ComputationParameterIndex: int
    AsyncTypeReference: CliTypeReference
    AsyncBuilderTypeReference: CliTypeReference
    FSharpFunctionTypeReference: CliTypeReference
    ConverterTypeReference: CliTypeReference
    ExtraTopLevelOperatorsTypeReference: CliTypeReference
    UnitType: CliType
    AsyncBooleanType: CliType
    AsyncUnitType: CliType
    Range: SourceRange
} with

    override _.ToString() = "TypedAsyncWhileExpression"

and internal TypedCancellableTaskSequentialExpression = {
    SequenceParameterIndex: int
    ElementType: CliType
    SequenceType: CliType
    InputFunctionType: CliType
    InputTaskType: CliType
    OutputArrayType: CliType
    OutputTaskType: CliType
    OutputFunctionType: CliType
    CancellationTokenType: CliType
    UnitType: CliType
    EnumerableTypeReference: CliTypeReference
    FSharpFunctionTypeReference: CliTypeReference
    TaskTypeReference: CliTypeReference
    AsyncTypeReference: CliTypeReference
    AsyncModuleTypeReference: CliTypeReference
    AsyncBuilderTypeReference: CliTypeReference
    ExtraTopLevelOperatorsTypeReference: CliTypeReference
    SeqModuleTypeReference: CliTypeReference
    EnumeratorTypeReference: CliTypeReference
    ListTypeReference: CliTypeReference
    OptionTypeReference: CliTypeReference
    ConverterTypeReference: CliTypeReference
    Range: SourceRange
} with

    override _.ToString() =
        "TypedCancellableTaskSequentialExpression"

and internal TypedValueTaskOfUnitExpression = {
    BuilderName: string
    SourceParameterIndex: int
    SourceValueTaskType: CliType
    UnitType: CliType
    OutputValueTaskType: CliType
    TaskTypeReference: CliTypeReference
    NonGenericTaskTypeReference: CliTypeReference
    NonGenericTaskAwaiterTypeReference: CliTypeReference
    FuncTypeReference: CliTypeReference
    CancellationTokenType: CliType
    TaskContinuationOptionsType: CliType
    TaskSchedulerType: CliType
    TaskExtensionsTypeReference: CliTypeReference
    OperationCanceledExceptionType: CliType
    ExceptionType: CliType
    Range: SourceRange
} with

    override _.ToString() = "TypedValueTaskOfUnitExpression"

and internal TypedObjectExpressionMember = {
    IsOverride: bool
    ReceiverName: string
    Name: string
    Parameters: TypedParameter list
    ReturnType: CliType
    Body: TypedExpression
    BodyRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "TypedObjectExpressionMember"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedTypeExpression =
    | TypedNamedType of ResolvedTypeName
    | TypedTypeParameter of string
    | TypedGenericTypeApplication of
        genericType: TypedTypeExpression *
        arguments: TypedTypeExpression list
    | TypedByRefType of TypedTypeExpression
    | TypedTupleType of TypedTypeExpression list
    | TypedFunctionType of TypedTypeExpression * TypedTypeExpression

    override _.ToString() = "TypedTypeExpression"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedTypeConstraint =
    | TypedSubtypeConstraint of typeParameter: string * superType: TypedTypeExpression
    | TypedMemberConstraint of
        typeParameter: string *
        memberName: string *
        memberType: TypedTypeExpression

    override _.ToString() = "TypedTypeConstraint"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedMethodConstraint =
    | TypedAbbreviationConstraint of TypedTypeExpression
    | TypedDirectConstraint of TypedTypeConstraint

    override _.ToString() = "TypedMethodConstraint"

type internal TypedMethodDeclaration = {
    StableId: string
    Name: string
    IsPublic: bool
    GenericParameters: string list
    Constraints: TypedMethodConstraint list
    Attributes: TypedCustomAttribute list
    Parameters: TypedParameter list
    ReturnType: CliType
    Body: TypedExpression
    EmitHiddenEntrySequencePoint: bool
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedMethodDeclaration"

type internal TypedObjectMethodDeclaration =
    | TypedInstanceObjectMethod of receiverName: string * declaration: TypedMethodDeclaration
    | TypedStaticObjectMethod of TypedMethodDeclaration

    member this.Method =
        match this with
        | TypedInstanceObjectMethod(_, declaration)
        | TypedStaticObjectMethod declaration -> declaration

    override _.ToString() = "TypedObjectMethodDeclaration"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal TypedObjectTypeContainer =
    | OrdinaryTypedObjectType
    | TypedCurrentModuleAugmentation of extendedType: CliType
    | TypedExtensionModule of
        name: string *
        attributes: TypedCustomAttribute list *
        extendedType: CliType

    override _.ToString() = "TypedObjectTypeContainer"

type internal TypedLiteralFieldDeclaration = {
    StableId: string
    Name: string
    Value: string
    ExportFingerprint: string
} with

    override _.ToString() = "TypedLiteralFieldDeclaration"

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

type internal TypedFieldDeclaration = {
    StableId: string
    Name: string
    IsMutable: bool
    Type: CliType
    Attributes: TypedCustomAttribute list
    ExportFingerprint: string
} with

    override _.ToString() = "TypedFieldDeclaration"

type internal TypedStructTypeDeclaration = {
    StableId: string
    Name: string
    GenericParameters: string list
    Attributes: TypedCustomAttribute list
    Fields: TypedFieldDeclaration list
    Methods: TypedObjectMethodDeclaration list
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedStructTypeDeclaration"

type internal TypedStaticTypeDeclaration = {
    StableId: string
    IsPublic: bool
    Name: string
    Methods: TypedMethodDeclaration list
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedStaticTypeDeclaration"

type internal TypedObjectTypeDeclaration = {
    StableId: string
    Container: TypedObjectTypeContainer
    Name: string
    BaseType: CliType option
    Methods: TypedObjectMethodDeclaration list
    ExportFingerprint: string
    ConstructorRange: SourceRange
    Range: SourceRange
} with

    override _.ToString() = "TypedObjectTypeDeclaration"

type internal TypedModuleValueInitializer =
    | TypedModuleValueConstruction of TypedObjectConstructionTarget
    | TypedModuleValueAlias of targetStableId: string

    override _.ToString() = "TypedModuleValueInitializer"

type internal TypedModuleValueDeclaration = {
    StableId: string
    Name: string
    Type: CliType
    Initializer: TypedModuleValueInitializer
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedModuleValueDeclaration"

type internal TypedNestedModuleDeclaration = {
    StableId: string
    Name: string
    CompiledName: string
    Attributes: TypedCustomAttribute list
    Values: TypedModuleValueDeclaration list
    Methods: TypedMethodDeclaration list
    Extensions: TypedObjectTypeDeclaration list
    Modules: TypedNestedModuleDeclaration list
    ExportFingerprint: string
    Range: SourceRange
} with

    override _.ToString() = "TypedNestedModuleDeclaration"

type internal TypedDeclaration =
    | TypedMethod of TypedMethodDeclaration
    | TypedLiteralField of TypedLiteralFieldDeclaration
    | TypedNestedModule of TypedNestedModuleDeclaration
    | TypedTypeAbbreviation of TypedTypeAbbreviationDeclaration
    | TypedStaticType of TypedStaticTypeDeclaration
    | TypedObjectType of TypedObjectTypeDeclaration
    | TypedStructType of TypedStructTypeDeclaration

    member this.StableId =
        match this with
        | TypedMethod declaration -> declaration.StableId
        | TypedLiteralField declaration -> declaration.StableId
        | TypedNestedModule declaration -> declaration.StableId
        | TypedTypeAbbreviation declaration -> declaration.StableId
        | TypedStaticType declaration -> declaration.StableId
        | TypedObjectType declaration -> declaration.StableId
        | TypedStructType declaration -> declaration.StableId

    member this.ExportFingerprint =
        match this with
        | TypedMethod declaration -> declaration.ExportFingerprint
        | TypedLiteralField declaration -> declaration.ExportFingerprint
        | TypedNestedModule declaration -> declaration.ExportFingerprint
        | TypedTypeAbbreviation declaration -> declaration.ExportFingerprint
        | TypedStaticType declaration -> declaration.ExportFingerprint
        | TypedObjectType declaration -> declaration.ExportFingerprint
        | TypedStructType declaration -> declaration.ExportFingerprint

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
    ContainerKind: SourceContainerKind
    Namespace: string
    Name: string
    IsPublic: bool
    DocumentIndex: int
    SourceChecksum: ImmutableArray<byte>
    ContentFingerprint: string
    Attributes: TypedCustomAttribute list
    AssemblyAttributes: TypedAssemblyAttribute list
    Declarations: TypedDeclaration list
    ExportFingerprint: string
} with

    override _.ToString() = "TypedModule"

type internal SymbolicFieldReference = {
    DeclaringType: SymbolicDeclaringType
    Name: string
    FieldType: CliType
    TargetStableId: string option
} with

    member this.StableId =
        String.concat "|" [
            "field-reference"
            StableIdentity.symbolicDeclaringType this.DeclaringType
            this.Name
            StableIdentity.cliType this.FieldType
        ]

    member this.DependencyId =
        this.TargetStableId
        |> Option.defaultValue this.StableId

    override _.ToString() = "SymbolicFieldReference"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicInstruction =
    | MarkSequencePoint of SourceRange
    | MarkHiddenSequencePoint
    | MarkLabel of int
    | BranchIfFalse of int
    | Branch of int
    | Leave of int
    | DefineCatchRegion of
        tryStart: int *
        tryEnd: int *
        handlerStart: int *
        handlerEnd: int *
        catchType: CliType
    | DefineFinallyRegion of tryStart: int * tryEnd: int * handlerStart: int * handlerEnd: int
    | Nop
    | Box of CliType
    | UnboxAny of CliType
    | CastClass of CliType
    | IsInstance of CliType
    | CompareEqual
    | LoadInt32 of int
    | LoadString of string
    | LoadNull
    | LoadArgument of int
    | LoadArgumentAddress of int
    | LoadLocal of int
    | LoadLocalAddress of int
    | StoreLocal of int
    | Duplicate
    | InitializeObject of CliType
    | NewArray of elementType: CliType
    | StoreArrayElementReference
    | LoadField of SymbolicFieldReference
    | LoadFieldAddress of SymbolicFieldReference
    | StoreField of SymbolicFieldReference
    | LoadStaticField of SymbolicFieldReference
    | StoreStaticField of SymbolicFieldReference
    | CallMethod of SymbolicMethodReference
    | CallVirtualMethod of SymbolicMethodReference
    | CallGenericMethod of methodReference: SymbolicMethodReference * genericArguments: CliType list
    | LoadFunctionPointer of SymbolicMethodReference
    | NewObject of SymbolicMethodReference
    | Pop
    | Throw
    | EndFinally
    | Return

    override _.ToString() = "SymbolicInstruction"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicMethodKind =
    | ModuleFunction
    | InternalModuleFunction
    | ModuleValueGetter
    | StaticConstructor
    | TypeExtensionMember
    | StaticTypeExtensionMember
    | StaticInlineMemberStub
    | InternalStaticInlineMemberStub
    | InstanceConstructor
    | InstanceInlineMember
    | InternalInstanceInlineMember
    | ClosureConstructor
    | ClosureInvoke
    | ObjectExpressionOverride

    override _.ToString() = "SymbolicMethodKind"

type internal SymbolicCustomAttributeFragment = {
    SchemaVersion: int
    StableId: string
    Kind: KnownAttributeKind
    ConstructorArguments: TypedAttributeArgument list
    ContentHash: string
} with

    override _.ToString() = "SymbolicCustomAttributeFragment"

type internal SymbolicParameterFragment = {
    Name: string
    Type: CliType
    Attributes: SymbolicCustomAttributeFragment list
} with

    override _.ToString() = "SymbolicParameterFragment"

type internal SymbolicLocalFragment = {
    Index: int
    Name: string
    Type: CliType
} with

    override _.ToString() = "SymbolicLocalFragment"

type internal SymbolicMethodFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Kind: SymbolicMethodKind
    GenericParameters: string list
    Constraints: TypedMethodConstraint list
    GenericParameterConstraints: (int * CliType) list
    Attributes: SymbolicCustomAttributeFragment list
    Parameters: SymbolicParameterFragment list
    Locals: SymbolicLocalFragment list
    ReturnType: CliType
    Instructions: SymbolicInstruction list
    EmitDefaultSequencePoint: bool
    MaxStack: int
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

type internal SymbolicInstanceFieldFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Type: CliType
    Attributes: SymbolicCustomAttributeFragment list
    ContentHash: string
} with

    override _.ToString() = "SymbolicInstanceFieldFragment"

type internal SymbolicStaticFieldFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Type: CliType
    ContentHash: string
} with

    override _.ToString() = "SymbolicStaticFieldFragment"

type internal SymbolicPropertyFragment = {
    SchemaVersion: int
    StableId: string
    Name: string
    Type: CliType
    GetterStableId: string
    ContentHash: string
} with

    override _.ToString() = "SymbolicPropertyFragment"

[<System.Diagnostics.DebuggerDisplay("{ToString()}")>]
type internal SymbolicTypeKind =
    | ModuleContainer
    | ExtensionModuleContainer
    | StaticMemberContainer
    | ObjectContainer of baseType: CliType
    | StructContainer
    | ClosureContainer
    | ObjectExpressionContainer

    override _.ToString() = "SymbolicTypeKind"

type internal SymbolicTypeFragment = {
    SchemaVersion: int
    StableId: string
    Namespace: string
    Name: string
    IsPublic: bool
    EnclosingTypeStableId: string option
    Kind: SymbolicTypeKind
    GenericParameters: string list
    Attributes: SymbolicCustomAttributeFragment list
    LiteralFields: SymbolicLiteralFieldFragment list
    InstanceFields: SymbolicInstanceFieldFragment list
    StaticFields: SymbolicStaticFieldFragment list
    Properties: SymbolicPropertyFragment list
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
    ResolvedCompilation: ResolvedCompilation
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
