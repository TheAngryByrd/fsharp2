namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text

module private Fingerprint =
    let text (value: string) =
        value
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> fun hash -> hash.ToLowerInvariant()

    let parts (values: string seq) =
        values
        |> Seq.map (fun value ->
            value.Length.ToString(CultureInfo.InvariantCulture)
            + ":"
            + value
        )
        |> String.concat String.Empty
        |> text

module private TypeIdentity =
    let qualifiedName = StableIdentity.qualifiedTypeName

    let rec expression =
        function
        | TypedNamedType resolvedType ->
            Fingerprint.parts [
                "named"
                qualifiedName resolvedType.TypeName
                resolvedType.DeclarationId
            ]
        | TypedTypeParameter name ->
            Fingerprint.parts [
                "parameter"
                name
            ]
        | TypedGenericTypeApplication(genericType, arguments) ->
            Fingerprint.parts [
                "generic"
                expression genericType

                yield!
                    arguments
                    |> List.map expression
            ]
        | TypedByRefType elementType ->
            Fingerprint.parts [
                "byref"
                expression elementType
            ]
        | TypedTupleType elements ->
            Fingerprint.parts [
                "tuple"

                yield!
                    elements
                    |> List.map expression
            ]
        | TypedFunctionType(domain, range) ->
            Fingerprint.parts [
                "function"
                expression domain
                expression range
            ]

    let constraintIdentity =
        function
        | TypedSubtypeConstraint(typeParameter, superType) ->
            Fingerprint.parts [
                "subtype"
                typeParameter
                expression superType
            ]
        | TypedMemberConstraint(typeParameter, memberName, memberType) ->
            Fingerprint.parts [
                "member"
                typeParameter
                memberName
                expression memberType
            ]

    let methodConstraintIdentity =
        function
        | TypedAbbreviationConstraint constraintType ->
            Fingerprint.parts [
                "abbreviation"
                expression constraintType
            ]
        | TypedDirectConstraint constraint' ->
            Fingerprint.parts [
                "direct"
                constraintIdentity constraint'
            ]

    let cliType = StableIdentity.cliType

    let callArgument =
        function
        | TypedValueArgument name ->
            Fingerprint.parts [
                "value"
                name
            ]
        | TypedAddressOfArgument(rootName, memberPath) ->
            Fingerprint.parts [
                "address-of"
                rootName
                yield! memberPath
            ]

    let resumableCodeBody =
        function
        | TypedStoreCapturedResult(dataFieldName, resultFieldName, resultFieldStableId) ->
            Fingerprint.parts [
                "store-captured-result"
                dataFieldName
                resultFieldName
                resultFieldStableId
            ]
        | TypedInvokeCapturedUnitFunction -> "invoke-captured-unit-function"

    let resumableCode (expression: TypedResumableCodeExpression) =
        Fingerprint.parts [
            "resumable-code"
            cliType expression.DelegateType
            cliType expression.StateMachineType
            cliType expression.DataType
            expression.CaptureParameterIndex.ToString(CultureInfo.InvariantCulture)
            expression.CaptureName
            expression.StateMachineParameterName
            resumableCodeBody expression.Body
        ]

    let rec inlineBody =
        function
        | TypedIntegerLiteral value ->
            Fingerprint.parts [
                "integer"
                value.ToString(CultureInfo.InvariantCulture)
            ]
        | TypedStringLiteral value ->
            Fingerprint.parts [
                "string"
                value
            ]
        | TypedUnitLiteral -> "unit"
        | TypedParameterReference index ->
            Fingerprint.parts [
                "parameter"
                index.ToString(CultureInfo.InvariantCulture)
            ]
        | TypedLocalReference index ->
            Fingerprint.parts [
                "local"
                index.ToString(CultureInfo.InvariantCulture)
            ]
        | TypedLet(localIndex, _, isMutable, localType, value, body, _, _) ->
            Fingerprint.parts [
                "let"
                localIndex.ToString(CultureInfo.InvariantCulture)
                if isMutable then "mutable" else "immutable"
                cliType localType
                inlineBody value
                inlineBody body
            ]
        | TypedLocalAssignment(localIndex, localType, value) ->
            Fingerprint.parts [
                "local-assignment"
                localIndex.ToString(CultureInfo.InvariantCulture)
                cliType localType
                inlineBody value
            ]
        | TypedAddressOf(source, fields) ->
            let sourceIdentity =
                match source with
                | TypedParameterAddress(index, parameterType) ->
                    Fingerprint.parts [
                        "parameter"
                        index.ToString(CultureInfo.InvariantCulture)
                        cliType parameterType
                    ]
                | TypedLocalAddress(index, localType) ->
                    Fingerprint.parts [
                        "local"
                        index.ToString(CultureInfo.InvariantCulture)
                        cliType localType
                    ]

            Fingerprint.parts [
                "address-of"
                sourceIdentity

                yield!
                    fields
                    |> List.collect (fun field -> [
                        cliType field.DeclaringType
                        field.Name
                        cliType field.FieldType
                        field.TargetStableId
                        |> Option.defaultValue String.Empty
                    ])
            ]
        | TypedStaticMethodCall(target, genericArguments, arguments) ->
            Fingerprint.parts [
                "static-call"
                target.StableId

                yield!
                    genericArguments
                    |> List.map cliType

                yield!
                    arguments
                    |> List.map inlineBody
            ]
        | TypedFunctionApplication(functionType,
                                   domainType,
                                   rangeType,
                                   functionExpression,
                                   argumentExpression) ->
            Fingerprint.parts [
                "function-application"
                cliType functionType
                cliType domainType
                cliType rangeType
                inlineBody functionExpression
                inlineBody argumentExpression
            ]
        | TypedInstanceMethodCall(target, receiver, arguments) ->
            Fingerprint.parts [
                "instance-call"
                cliType target.DeclaringType
                target.Name

                yield!
                    target.ParameterTypes
                    |> List.map cliType

                cliType target.ReturnType
                cliType target.ResultType
                inlineBody receiver

                yield!
                    arguments
                    |> List.map inlineBody
            ]
        | TypedBoundInstanceMethod expression ->
            Fingerprint.parts [
                "bound-instance-member"
                cliType expression.FunctionType
                cliType expression.DelegateType
                cliType expression.ReceiverType
                expression.TargetStableId
                expression.Target.Name
                cliType expression.DomainType
                cliType expression.RangeType
            ]
        | TypedUnitLambda expression ->
            Fingerprint.parts [
                "unit-lambda"
                cliType expression.FunctionType
                cliType expression.DelegateType
                expression.CaptureParameterIndex.ToString(CultureInfo.InvariantCulture)
                expression.CaptureName
                cliType expression.CaptureType
                cliType expression.DomainType
                cliType expression.RangeType
            ]
        | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
            Fingerprint.parts [
                "conditional"
                inlineBody condition
                inlineBody ifTrue
                inlineBody ifFalse
            ]
        | TypedUpcast(sourceType, targetType, _, expression) ->
            Fingerprint.parts [
                "upcast"
                cliType sourceType
                cliType targetType
                inlineBody expression
            ]
        | TypedSequential expressions ->
            Fingerprint.parts [
                "sequence"

                yield!
                    expressions
                    |> List.collect (fun (expression, expressionType, _) -> [
                        cliType expressionType
                        inlineBody expression
                    ])
            ]
        | TypedBooleanNegation(expression, _) ->
            Fingerprint.parts [
                "boolean-negation"
                inlineBody expression
            ]
        | TypedTypeTestMatch(input,
                             targetType,
                             localIndex,
                             bindingName,
                             ifMatched,
                             ifNotMatched,
                             _,
                             _,
                             _,
                             _) ->
            Fingerprint.parts [
                "type-test-match"
                inlineBody input
                cliType targetType
                localIndex.ToString(CultureInfo.InvariantCulture)
                bindingName
                inlineBody ifMatched
                inlineBody ifNotMatched
            ]
        | TypedResumableCode expression -> resumableCode expression
        | TypedResumableTryFinally expression ->
            Fingerprint.parts [
                "resumable-try-finally"
                cliType expression.ResumableCodeModuleType
                cliType expression.DelegateType
                cliType expression.DataType
                cliType expression.ResultType
                expression.ComputationParameterIndex.ToString(CultureInfo.InvariantCulture)
                expression.ComputationName
                resumableCode expression.Compensation
            ]
        | TypedObjectExpression(typeReference,
                                baseType,
                                constructorArguments,
                                isOverride,
                                receiverName,
                                memberName,
                                memberReturnType,
                                memberBody,
                                _) ->
            Fingerprint.parts [
                "object-expression"
                typeReference.DeclarationId
                cliType baseType
                if isOverride then "override" else "member"
                receiverName
                memberName
                cliType memberReturnType

                yield!
                    constructorArguments
                    |> List.map inlineBody

                inlineBody memberBody
            ]
        | TypedTraitCall(receiverName, memberName, arguments) ->
            Fingerprint.parts [
                "trait-call"
                receiverName
                memberName
                yield!
                    arguments
                    |> List.map callArgument
            ]

    let attributeKind =
        function
        | AutoOpenAttribute -> "auto-open"
        | StructAttribute -> "struct"
        | NoComparisonAttribute -> "no-comparison"
        | NoEqualityAttribute -> "no-equality"
        | DefaultValueAttribute -> "default-value"
        | InlineIfLambdaAttribute -> "inline-if-lambda"
        | NoEagerConstraintApplicationAttribute -> "no-eager-constraint-application"
        | CompilationMappingAttribute -> "compilation-mapping"

    let attributeArgument =
        function
        | TypedBooleanAttributeArgument value -> if value then "bool:true" else "bool:false"
        | TypedSourceConstructAttributeArgument ObjectTypeConstruct ->
            "source-construct:object-type"
        | TypedSourceConstructAttributeArgument ModuleConstruct -> "source-construct:module"

    let customAttribute (attribute: TypedCustomAttribute) =
        Fingerprint.parts [
            attributeKind attribute.Kind

            yield!
                attribute.ConstructorArguments
                |> List.map attributeArgument
        ]

    let parameter (parameter: TypedParameter) =
        Fingerprint.parts [
            parameter.Name
            cliType parameter.Type
            "attributes"

            yield!
                parameter.Attributes
                |> List.map customAttribute
        ]

type private ObjectMethodKind =
    | InstanceObjectMethod
    | StaticObjectMethod

    member this.ExportIdentity =
        match this with
        | InstanceObjectMethod -> "instance"
        | StaticObjectMethod -> "static"

    member this.AllowedAttributeKinds =
        match this with
        | InstanceObjectMethod -> [
            DefaultValueAttribute
            NoEagerConstraintApplicationAttribute
          ]
        | StaticObjectMethod -> [ NoEagerConstraintApplicationAttribute ]

type private ObjectMethodCompletion = {
    Kind: ObjectMethodKind
    Name: string
    IsPublic: bool
    GenericParameters: string list
    Constraints: TypedMethodConstraint list
    ParsedAttributes: ParsedAttribute list
    ParsedParameters: ParsedParameter list
    Parameters: TypedParameter list
    ReturnType: CliType
    Body: TypedExpression
    Range: SourceRange
}

type private CheckedSourceType = {
    Namespace: string
    Name: string
    StableId: string
    Methods: TypedMethodDeclaration list
}

type private CheckedSourceStruct = {
    Namespace: string
    Declaration: TypedStructTypeDeclaration
}

/// A deliberately small in-memory query owner. Query identities and cached
/// values are semantic/compiler state; final SRM state never enters these maps.
type internal CompilerService() =
    let querySchema = CompilerSchema.Query
    let parseCache = Dictionary<string, ParsedModule>(StringComparer.Ordinal)
    let checkCache = Dictionary<string, TypedModule>(StringComparer.Ordinal)
    let lowerCache = Dictionary<string, SymbolicAssembly>(StringComparer.Ordinal)
    let lastSuccessfulContent = Dictionary<string, string>(StringComparer.Ordinal)
    let mutable parseHits = 0
    let mutable parseMisses = 0
    let mutable checkHits = 0
    let mutable checkMisses = 0
    let mutable lowerHits = 0
    let mutable lowerMisses = 0

    let elapsedMicroseconds started =
        Stopwatch.GetElapsedTime(started).Ticks
        / 10L

    let parse (defines: string list) (source: SourceInput) =
        let normalizedDefines =
            defines
            |> List.distinct
            |> List.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))

        let key =
            Fingerprint.parts [
                querySchema.ToString(CultureInfo.InvariantCulture)
                "defines"
                normalizedDefines.Length.ToString(CultureInfo.InvariantCulture)
                yield! normalizedDefines
                Path.GetFileName(source.Path)
                source.Text
            ]

        match parseCache.TryGetValue(key) with
        | true, parsed ->
            parseHits <-
                parseHits
                + 1

            Ok(parsed, key)
        | false, _ ->
            parseMisses <-
                parseMisses
                + 1

            match Frontend.parse normalizedDefines source with
            | Error error -> Error error
            | Ok parsed ->
                parseCache.Add(key, parsed)
                Ok(parsed, key)

    let check (references: ReferenceTypeIndex) (sourcePath: string) (parsed: ParsedModule) =
        let key =
            Fingerprint.parts [
                querySchema.ToString(CultureInfo.InvariantCulture)
                references.Fingerprint
                parsed.StableId
                parsed.ContentFingerprint
            ]

        match checkCache.TryGetValue(key) with
        | true, typed ->
            checkHits <-
                checkHits
                + 1

            Ok(
                {
                    typed with
                        SourceChecksum = parsed.SourceChecksum
                },
                key
            )
        | false, _ ->
            checkMisses <-
                checkMisses
                + 1

            let diagnostic range message =
                Error {
                    Code = "FSC2P1001"
                    Message = message
                    Path = Some sourcePath
                    Range = Some range
                }

            let checkedSourceTypes = ResizeArray<CheckedSourceType>()
            let checkedSourceStructs = ResizeArray<CheckedSourceStruct>()

            let addCheckedDeclaration namespaceName =
                function
                | TypedStaticType declaration ->
                    checkedSourceTypes.Add {
                        Namespace = namespaceName
                        Name = declaration.Name
                        StableId = declaration.StableId
                        Methods = declaration.Methods
                    }
                | TypedObjectType({ Container = OrdinaryTypedObjectType } as declaration) ->
                    checkedSourceTypes.Add {
                        Namespace = namespaceName
                        Name = declaration.Name
                        StableId = declaration.StableId
                        Methods =
                            declaration.Methods
                            |> List.choose (
                                function
                                | TypedStaticObjectMethod methodDeclaration ->
                                    Some methodDeclaration
                                | TypedInstanceObjectMethod _ -> None
                            )
                    }
                | TypedObjectType _ -> ()
                | TypedStructType declaration ->
                    checkedSourceStructs.Add {
                        Namespace = namespaceName
                        Declaration = declaration
                    }
                | TypedMethod _
                | TypedLiteralField _
                | TypedTypeAbbreviation _ -> ()

            let typeAbbreviations =
                parsed.Declarations
                |> List.choose (
                    function
                    | ParsedTypeAbbreviation declaration -> Some(declaration.Name, declaration)
                    | ParsedMethod _
                    | ParsedLiteralField _
                    | ParsedStaticType _
                    | ParsedObjectType _
                    | ParsedStructType _ -> None
                )
                |> Map.ofList

            let localTypes = Dictionary<TypeNameArity, ResolvedTypeName>()

            for declaration in parsed.Declarations do
                match ParsedDeclaration.tryTypeIdentity parsed.StableId declaration with
                | Some(key, declarationId) ->
                    let isValueType =
                        match declaration with
                        | ParsedStructType _ -> true
                        | ParsedMethod _
                        | ParsedLiteralField _
                        | ParsedTypeAbbreviation _
                        | ParsedStaticType _
                        | ParsedObjectType _ -> false

                    let resolved: ResolvedTypeName = {
                        TypeName = {
                            Namespace = parsed.Namespace
                            Name = key.Name
                        }
                        DeclarationId = declarationId
                        AssemblyName = String.Empty
                        IsValueType = isValueType
                    }

                    localTypes.TryAdd(key, resolved)
                    |> ignore
                | None -> ()

            let resolveNamedType
                (genericArity: int)
                (typeName: QualifiedTypeName)
                (range: SourceRange)
                =
                let localKey = ReferenceTypeName.simpleKey typeName.Name genericArity

                match
                    String.IsNullOrEmpty(typeName.Namespace), localTypes.TryGetValue(localKey)
                with
                | true, (true, resolved) -> Ok(TypedNamedType resolved)
                | _ ->
                    match
                        references.Resolve(
                            parsed.Namespace,
                            parsed.OpenedNamespaces,
                            typeName,
                            genericArity
                        )
                    with
                    | Ok resolved -> Ok(TypedNamedType resolved)
                    | Error message -> diagnostic range message

            let rec resolveType (declaredParameters: HashSet<string>) =
                function
                | ParsedTypeParameter(name, range) ->
                    if declaredParameters.Contains(name) then
                        Ok(TypedTypeParameter name)
                    else
                        diagnostic range $"the type parameter '{name}' is not declared"
                | ParsedWildcardType range ->
                    diagnostic range "a wildcard type requires an expected type"
                | ParsedFlexibleType(_, range) ->
                    diagnostic range "a flexible type must be generalized by its member declaration"
                | ParsedNamedType(typeName, range) -> resolveNamedType 0 typeName range
                | ParsedGenericTypeApplication(ParsedNamedType(typeName, _), [ argument ], _) when
                    String.IsNullOrEmpty(typeName.Namespace)
                    && typeName.Name = "byref"
                    ->
                    resolveType declaredParameters argument
                    |> Result.map TypedByRefType
                | ParsedGenericTypeApplication(genericType, arguments, _) ->
                    let typedGenericType =
                        match genericType with
                        | ParsedNamedType(typeName, range) ->
                            resolveNamedType arguments.Length typeName range
                        | _ -> resolveType declaredParameters genericType

                    match typedGenericType with
                    | Error error -> Error error
                    | Ok typedGenericType ->
                        let rec resolveArguments resolved =
                            function
                            | [] -> Ok(List.rev resolved)
                            | argument :: remaining ->
                                match resolveType declaredParameters argument with
                                | Error error -> Error error
                                | Ok typedArgument ->
                                    resolveArguments
                                        (typedArgument
                                         :: resolved)
                                        remaining

                        resolveArguments [] arguments
                        |> Result.map (fun typedArguments ->
                            TypedGenericTypeApplication(typedGenericType, typedArguments)
                        )
                | ParsedTupleType(elements, _) ->
                    let rec resolveElements resolved =
                        function
                        | [] -> Ok(List.rev resolved)
                        | element :: remaining ->
                            match resolveType declaredParameters element with
                            | Error error -> Error error
                            | Ok typedElement ->
                                resolveElements
                                    (typedElement
                                     :: resolved)
                                    remaining

                    resolveElements [] elements
                    |> Result.map TypedTupleType
                | ParsedFunctionType(domain, range, _) ->
                    match resolveType declaredParameters domain with
                    | Error error -> Error error
                    | Ok typedDomain ->
                        resolveType declaredParameters range
                        |> Result.map (fun typedRange -> TypedFunctionType(typedDomain, typedRange))

            let rec substituteType (substitutions: Map<string, ParsedTypeExpression>) expression =
                match expression with
                | ParsedTypeParameter(name, _) ->
                    substitutions
                    |> Map.tryFind name
                    |> Option.defaultValue expression
                | ParsedFlexibleType(superType, range) ->
                    ParsedFlexibleType(substituteType substitutions superType, range)
                | ParsedWildcardType _
                | ParsedNamedType _ -> expression
                | ParsedGenericTypeApplication(genericType, arguments, range) ->
                    ParsedGenericTypeApplication(
                        substituteType substitutions genericType,
                        arguments
                        |> List.map (substituteType substitutions),
                        range
                    )
                | ParsedTupleType(elements, range) ->
                    ParsedTupleType(
                        elements
                        |> List.map (substituteType substitutions),
                        range
                    )
                | ParsedFunctionType(domain, range, sourceRange) ->
                    ParsedFunctionType(
                        substituteType substitutions domain,
                        substituteType substitutions range,
                        sourceRange
                    )

            let rec expandTypeAbbreviations expanding expression =
                let expand = expandTypeAbbreviations expanding

                match expression with
                | ParsedGenericTypeApplication(ParsedNamedType(typeName, _), arguments, _) when
                    String.IsNullOrEmpty(typeName.Namespace)
                    ->
                    let expandedArguments =
                        arguments
                        |> List.map expand

                    match
                        typeAbbreviations
                        |> Map.tryFind typeName.Name
                    with
                    | Some abbreviation when
                        abbreviation.TypeParameters.Length = expandedArguments.Length
                        && not (
                            expanding
                            |> Set.contains typeName.Name
                        )
                        ->
                        abbreviation.Target.Type
                        |> substituteType (
                            List.zip abbreviation.TypeParameters expandedArguments
                            |> Map.ofList
                        )
                        |> expandTypeAbbreviations (
                            expanding
                            |> Set.add typeName.Name
                        )
                    | _ ->
                        match expression with
                        | ParsedGenericTypeApplication(genericType, _, range) ->
                            ParsedGenericTypeApplication(
                                expand genericType,
                                expandedArguments,
                                range
                            )
                        | _ -> expression
                | ParsedNamedType(typeName, _) when String.IsNullOrEmpty(typeName.Namespace) ->
                    match
                        typeAbbreviations
                        |> Map.tryFind typeName.Name
                    with
                    | Some abbreviation when
                        List.isEmpty abbreviation.TypeParameters
                        && not (
                            expanding
                            |> Set.contains typeName.Name
                        )
                        ->
                        abbreviation.Target.Type
                        |> expandTypeAbbreviations (
                            expanding
                            |> Set.add typeName.Name
                        )
                    | _ -> expression
                | ParsedGenericTypeApplication(genericType, arguments, range) ->
                    ParsedGenericTypeApplication(
                        expand genericType,
                        arguments
                        |> List.map expand,
                        range
                    )
                | ParsedTupleType(elements, range) ->
                    ParsedTupleType(
                        elements
                        |> List.map expand,
                        range
                    )
                | ParsedFunctionType(domain, range, sourceRange) ->
                    ParsedFunctionType(expand domain, expand range, sourceRange)
                | ParsedFlexibleType(superType, range) ->
                    ParsedFlexibleType(expand superType, range)
                | ParsedTypeParameter _
                | ParsedWildcardType _
                | ParsedNamedType _ -> expression

            let rec collectResults completed remaining =
                match remaining with
                | [] -> Ok(List.rev completed)
                | result :: tail ->
                    match result with
                    | Error diagnostic -> Error diagnostic
                    | Ok value ->
                        collectResults
                            (value
                             :: completed)
                            tail

            let knownAttributeKind (attribute: ParsedAttribute) =
                let name = attribute.AttributeType.Name

                match name with
                | "AutoOpen"
                | "AutoOpenAttribute" -> Some AutoOpenAttribute
                | "Struct"
                | "StructAttribute" -> Some StructAttribute
                | "NoComparison"
                | "NoComparisonAttribute" -> Some NoComparisonAttribute
                | "NoEquality"
                | "NoEqualityAttribute" -> Some NoEqualityAttribute
                | "DefaultValue"
                | "DefaultValueAttribute" -> Some DefaultValueAttribute
                | "InlineIfLambda"
                | "InlineIfLambdaAttribute" -> Some InlineIfLambdaAttribute
                | "NoEagerConstraintApplication"
                | "NoEagerConstraintApplicationAttribute" ->
                    Some NoEagerConstraintApplicationAttribute
                | _ -> None

            let typeCustomAttribute
                ownerStableId
                allowedKinds
                (index: int)
                (attribute: ParsedAttribute)
                =
                let unsupported message = diagnostic attribute.Range message

                let create kind arguments =
                    let stableId =
                        ownerStableId
                        + "/attribute:"
                        + index.ToString(CultureInfo.InvariantCulture)

                    Ok {
                        StableId = stableId
                        Kind = kind
                        ConstructorArguments = arguments
                        ExportFingerprint =
                            Fingerprint.parts [
                                stableId
                                TypeIdentity.attributeKind kind
                                yield!
                                    arguments
                                    |> List.map TypeIdentity.attributeArgument
                            ]
                    }

                match knownAttributeKind attribute with
                | None -> unsupported "the declaration attribute is not yet supported"
                | Some kind when
                    not (
                        allowedKinds
                        |> List.contains kind
                    )
                    ->
                    unsupported "the attribute is not valid on this declaration"
                | Some DefaultValueAttribute ->
                    match attribute.ConstructorArguments with
                    | [] -> create DefaultValueAttribute []
                    | [ ParsedBooleanAttributeArgument value ] ->
                        create DefaultValueAttribute [ TypedBooleanAttributeArgument value ]
                    | _ ->
                        unsupported
                            "DefaultValueAttribute requires zero arguments or one boolean argument"
                | Some kind ->
                    if List.isEmpty attribute.ConstructorArguments then
                        create kind []
                    else
                        unsupported "the declaration attribute does not accept arguments"

            let typeCustomAttributes ownerStableId allowedKinds attributes =
                attributes
                |> List.mapi (typeCustomAttribute ownerStableId allowedKinds)
                |> collectResults []

            let isInlineIfLambdaParameterType =
                function
                | CliNamedType typeReference ->
                    references.IsFSharpDelegate(typeReference.DeclarationId)
                | CliGenericType(typeReference, arguments) ->
                    (arguments.Length = 2
                     && typeReference.AssemblyName = "FSharp.Core"
                     && typeReference.TypeName.Namespace = "Microsoft.FSharp.Core"
                     && typeReference.TypeName.Name = "FSharpFunc`2")
                    || references.IsFSharpDelegate(typeReference.DeclarationId)
                | _ -> false

            let attachTypedParameterAttributes
                methodStableId
                (parsedParameters: ParsedParameter list)
                (parameters: TypedParameter list)
                =
                if
                    parsedParameters.Length
                    <> parameters.Length
                then
                    invalidOp "parsed and typed parameter counts must match"

                List.zip parsedParameters parameters
                |> List.mapi (fun index (parsedParameter, parameter) ->
                    let parameterStableId =
                        methodStableId
                        + "/parameter:"
                        + index.ToString(CultureInfo.InvariantCulture)
                        + ":"
                        + parameter.Name

                    typeCustomAttributes
                        parameterStableId
                        [ InlineIfLambdaAttribute ]
                        parsedParameter.Attributes
                    |> Result.bind (fun attributes ->
                        if
                            attributes
                            |> List.exists (fun attribute ->
                                attribute.Kind = InlineIfLambdaAttribute
                            )
                            && not (isInlineIfLambdaParameterType parameter.Type)
                        then
                            Error {
                                Code = "FS3519"
                                Message =
                                    "The 'InlineIfLambda' attribute may only be used on parameters of inlined functions of methods whose type is a function or F# delegate type."
                                Path = Some sourcePath
                                Range = Some parsedParameter.Range
                            }
                        else
                            Ok {
                                parameter with
                                    Attributes = attributes
                            }
                    )
                )
                |> collectResults []

            let rec inferMethodTypeArgument
                (substitutions: CliType option array)
                templateType
                actualType
                =
                match templateType with
                | CliMethodTypeParameter index ->
                    if
                        index < 0
                        || index
                           >= substitutions.Length
                    then
                        false
                    else
                        match substitutions.[index] with
                        | None ->
                            substitutions.[index] <- Some actualType
                            true
                        | Some inferredType -> inferredType = actualType
                | CliGenericType(templateReference, templateArguments) ->
                    match actualType with
                    | CliGenericType(actualReference, actualArguments) when
                        templateReference = actualReference
                        && templateArguments.Length = actualArguments.Length
                        ->
                        (templateArguments, actualArguments)
                        ||> List.forall2 (inferMethodTypeArgument substitutions)
                    | _ -> false
                | CliByRef templateElementType ->
                    match actualType with
                    | CliByRef actualElementType ->
                        inferMethodTypeArgument substitutions templateElementType actualElementType
                    | _ -> false
                | _ -> templateType = actualType

            let rec substituteMethodTypeArguments (substitutions: CliType option array) =
                function
                | CliMethodTypeParameter index ->
                    substitutions.[index]
                    |> Option.defaultWith (fun () ->
                        invalidOp "a static-call method type parameter was not inferred"
                    )
                | CliGenericType(typeReference, arguments) ->
                    CliGenericType(
                        typeReference,
                        arguments
                        |> List.map (substituteMethodTypeArguments substitutions)
                    )
                | CliByRef elementType ->
                    CliByRef(substituteMethodTypeArguments substitutions elementType)
                | cliType -> cliType

            let tryInferStaticMethod
                (sourceType: CheckedSourceType)
                (methodDeclaration: TypedMethodDeclaration)
                (argumentTypes: CliType list)
                =
                if
                    methodDeclaration.Parameters.Length
                    <> argumentTypes.Length
                then
                    None
                else
                    let substitutions = Array.create methodDeclaration.GenericParameters.Length None

                    let parametersMatch =
                        (methodDeclaration.Parameters
                         |> List.map _.Type,
                         argumentTypes)
                        ||> List.forall2 (inferMethodTypeArgument substitutions)

                    if
                        parametersMatch
                        && (substitutions
                            |> Array.forall Option.isSome)
                    then
                        let genericArguments =
                            substitutions
                            |> Array.choose id
                            |> Array.toList

                        let returnType =
                            substituteMethodTypeArguments substitutions methodDeclaration.ReturnType

                        Some(
                            {
                                DeclaringType = {
                                    DeclarationId = sourceType.StableId
                                    AssemblyName = String.Empty
                                    TypeName = {
                                        Namespace = sourceType.Namespace
                                        Name = sourceType.Name
                                    }
                                    IsValueType = false
                                }
                                StableId = methodDeclaration.StableId
                                Name = methodDeclaration.Name
                                GenericArity = methodDeclaration.GenericParameters.Length
                                ParameterTypes =
                                    methodDeclaration.Parameters
                                    |> List.map _.Type
                                ReturnType = methodDeclaration.ReturnType
                            },
                            genericArguments,
                            returnType
                        )
                    else
                        None

            let resolveStaticMethod receiverName memberName argumentTypes range =
                let visibleNamespaces =
                    parsed.Namespace
                    :: parsed.OpenedNamespaces
                    |> Set.ofList

                let candidates =
                    checkedSourceTypes
                    |> Seq.filter (fun sourceType ->
                        sourceType.Name = receiverName
                        && visibleNamespaces.Contains(sourceType.Namespace)
                    )
                    |> Seq.collect (fun sourceType ->
                        sourceType.Methods
                        |> Seq.choose (fun methodDeclaration ->
                            if methodDeclaration.Name = memberName then
                                tryInferStaticMethod sourceType methodDeclaration argumentTypes
                            else
                                None
                        )
                    )
                    |> Seq.toList

                match candidates with
                | [ candidate ] -> Ok candidate
                | [] ->
                    diagnostic
                        range
                        $"no visible static member '{receiverName}.{memberName}' matches the argument types"
                | _ ->
                    diagnostic
                        range
                        $"the static member call '{receiverName}.{memberName}' is ambiguous"

            let typeDeclaration =
                function
                | ParsedMethod declaration ->
                    match declaration.DeclaredType, declaration.Body with
                    | Some ParsedInt32, StringLiteral _ ->
                        Error {
                            Code = "FS0001"
                            Message =
                                String.concat Environment.NewLine [
                                    "This expression was expected to have type"
                                    "    'int'    "
                                    "but here has type"
                                    "    'string'"
                                ]
                            Path = Some sourcePath
                            Range = Some declaration.BodyRange
                        }

                    | _, IntegerLiteral value ->
                        let stableId =
                            parsed.StableId
                            + "/method:"
                            + declaration.Name
                            + (if declaration.IsUnitFunction then
                                   ":unit->int32"
                               else
                                   ":int32")

                        let exportFingerprint = Fingerprint.text stableId

                        Ok(
                            TypedMethod {
                                StableId = stableId
                                Name = declaration.Name
                                IsPublic = true
                                GenericParameters = []
                                Constraints = []
                                Attributes = []
                                Parameters = []
                                ReturnType = CliInt32
                                Body = TypedIntegerLiteral value
                                ExportFingerprint = exportFingerprint
                                Range = declaration.Range
                            }
                        )
                    | None, StringLiteral _ ->
                        Error {
                            Code = "FSC2P1001"
                            Message =
                                "string-valued declarations are not yet supported by the prototype"
                            Path = Some sourcePath
                            Range = Some declaration.BodyRange
                        }
                    | _, MemberCall _ ->
                        diagnostic
                            declaration.BodyRange
                            "trait calls are supported only in static inline members"
                    | _, ValueReference _
                    | _, AddressOfExpression _
                    | _, UnitApplication _ ->
                        diagnostic
                            declaration.BodyRange
                            "value references are supported only in parameterized members"
                    | _, UnitLiteral
                    | _, BooleanLiteral _
                    | _, BoundInstanceMember _
                    | _, MemberAssignment _
                    | _, SequentialExpression _
                    | _, FunctionApplication _
                    | _, ExpressionMemberCall _
                    | _, ConditionalExpression _
                    | _, ExplicitUpcastExpression _
                    | _, SequentialValueExpression _
                    | _, LocalAssignment _
                    | _, BooleanNegationExpression _
                    | _, LetExpression _
                    | _, LambdaExpression _
                    | _, UnitLambdaExpression _
                    | _, TypeConstruction _
                    | _, ObjectExpression _
                    | _, MatchExpression _ ->
                        diagnostic
                            declaration.BodyRange
                            "this expression form is supported only in instance members"
                | ParsedLiteralField declaration ->
                    let stableId =
                        parsed.StableId
                        + "/literal-field:"
                        + declaration.Name

                    Ok(
                        TypedLiteralField {
                            StableId = stableId
                            Name = declaration.Name
                            Value = declaration.Value
                            ExportFingerprint =
                                Fingerprint.text (
                                    stableId
                                    + "="
                                    + declaration.Value
                                )
                        }
                    )
                | ParsedTypeAbbreviation declaration ->
                    let stableId =
                        parsed.StableId
                        + "/type-abbreviation:"
                        + declaration.Name

                    let declaredParameters =
                        HashSet<string>(declaration.TypeParameters, StringComparer.Ordinal)

                    let resolveConstraint =
                        function
                        | ParsedSubtypeConstraint(typeParameter, superType, range) ->
                            if not (declaredParameters.Contains(typeParameter)) then
                                diagnostic
                                    range
                                    $"the constrained type parameter '{typeParameter}' is not declared"
                            else
                                resolveType declaredParameters superType
                                |> Result.map (fun typedSuperType ->
                                    TypedSubtypeConstraint(typeParameter, typedSuperType)
                                )
                        | ParsedMemberConstraint(typeParameter, memberName, memberType, range) ->
                            if not (declaredParameters.Contains(typeParameter)) then
                                diagnostic
                                    range
                                    $"the constrained type parameter '{typeParameter}' is not declared"
                            else
                                resolveType declaredParameters memberType
                                |> Result.map (fun typedMemberType ->
                                    TypedMemberConstraint(
                                        typeParameter,
                                        memberName,
                                        typedMemberType
                                    )
                                )

                    let rec resolveConstraints resolved =
                        function
                        | [] -> Ok(List.rev resolved)
                        | constraint' :: remaining ->
                            match resolveConstraint constraint' with
                            | Error error -> Error error
                            | Ok typedConstraint ->
                                resolveConstraints
                                    (typedConstraint
                                     :: resolved)
                                    remaining

                    if
                        declaredParameters.Count
                        <> declaration.TypeParameters.Length
                    then
                        diagnostic declaration.Range "generic type parameters must be unique"
                    else
                        match resolveConstraints [] declaration.Constraints with
                        | Error error -> Error error
                        | Ok constraints ->
                            match resolveType declaredParameters declaration.Target.Type with
                            | Error error -> Error error
                            | Ok targetType ->
                                let targetIdentity =
                                    Fingerprint.parts [
                                        stableId
                                        "parameters"
                                        yield! declaration.TypeParameters
                                        "constraints"

                                        yield!
                                            constraints
                                            |> List.map TypeIdentity.constraintIdentity

                                        "target"
                                        TypeIdentity.expression targetType
                                        if declaration.Target.AllowsNull then "null" else "non-null"
                                    ]

                                Ok(
                                    TypedTypeAbbreviation {
                                        StableId = stableId
                                        Name = declaration.Name
                                        TypeParameters = declaration.TypeParameters
                                        Constraints = constraints
                                        TargetType = targetType
                                        AllowsNull = declaration.Target.AllowsNull
                                        ExportFingerprint = targetIdentity
                                        Range = declaration.Range
                                    }
                                )
                | ParsedStaticType declaration ->
                    let stableId =
                        parsed.StableId
                        + "/type:"
                        + declaration.Name

                    let typeMethod (methodDeclaration: ParsedStaticMethodDeclaration) =
                        let methodParameters =
                            HashSet<string>(
                                methodDeclaration.TypeParameters,
                                StringComparer.Ordinal
                            )

                        let methodParameterIndex =
                            methodDeclaration.TypeParameters
                            |> List.mapi (fun index name -> name, index)
                            |> Map.ofList

                        let rec toCliType range =
                            function
                            | TypedTypeParameter name ->
                                match
                                    methodParameterIndex
                                    |> Map.tryFind name
                                with
                                | Some index -> Ok(CliMethodTypeParameter index)
                                | None ->
                                    diagnostic
                                        range
                                        $"the method type parameter '{name}' is not declared"
                            | TypedNamedType resolvedType when
                                resolvedType.TypeName.Namespace = "System"
                                && resolvedType.TypeName.Name = "Boolean"
                                ->
                                Ok CliBoolean
                            | TypedNamedType resolvedType when
                                resolvedType.TypeName.Namespace = "Microsoft.FSharp.Core"
                                && resolvedType.TypeName.Name = "Unit"
                                ->
                                Ok CliVoid
                            | TypedByRefType elementType ->
                                toCliType range elementType
                                |> Result.bind (fun cliElementType ->
                                    match cliElementType with
                                    | CliVoid -> diagnostic range "a byref element cannot be void"
                                    | _ -> Ok(CliByRef cliElementType)
                                )
                            | typedType ->
                                diagnostic
                                    range
                                    $"the CLI type '{TypeIdentity.expression typedType}' is not yet supported"

                        let resolveMethodConstraint =
                            function
                            | ParsedAbbreviationConstraint constraintType ->
                                resolveType methodParameters constraintType
                                |> Result.map TypedAbbreviationConstraint
                            | ParsedDirectConstraint directConstraint ->
                                match directConstraint with
                                | ParsedSubtypeConstraint(typeParameter, superType, range) ->
                                    if not (methodParameters.Contains(typeParameter)) then
                                        diagnostic
                                            range
                                            $"the constrained type parameter '{typeParameter}' is not declared"
                                    else
                                        resolveType methodParameters superType
                                        |> Result.map (fun typedSuperType ->
                                            TypedDirectConstraint(
                                                TypedSubtypeConstraint(
                                                    typeParameter,
                                                    typedSuperType
                                                )
                                            )
                                        )
                                | ParsedMemberConstraint(typeParameter,
                                                         memberName,
                                                         memberType,
                                                         range) ->
                                    if not (methodParameters.Contains(typeParameter)) then
                                        diagnostic
                                            range
                                            $"the constrained type parameter '{typeParameter}' is not declared"
                                    else
                                        resolveType methodParameters memberType
                                        |> Result.map (fun typedMemberType ->
                                            TypedDirectConstraint(
                                                TypedMemberConstraint(
                                                    typeParameter,
                                                    memberName,
                                                    typedMemberType
                                                )
                                            )
                                        )

                        let rec resolveMethodConstraints resolved =
                            function
                            | [] -> Ok(List.rev resolved)
                            | constraint' :: remaining ->
                                match resolveMethodConstraint constraint' with
                                | Error error -> Error error
                                | Ok typedConstraint ->
                                    resolveMethodConstraints
                                        (typedConstraint
                                         :: resolved)
                                        remaining

                        let rec typeParameters
                            (typed: TypedParameter list)
                            (remainingParameters: ParsedParameter list)
                            =
                            match remainingParameters with
                            | [] -> Ok(List.rev typed)
                            | parameter :: remaining ->
                                match resolveType methodParameters parameter.Type with
                                | Error error -> Error error
                                | Ok typedType ->
                                    match toCliType parameter.Range typedType with
                                    | Error error -> Error error
                                    | Ok cliType ->
                                        typeParameters
                                            ({
                                                Name = parameter.Name
                                                Type = cliType
                                                Attributes = []
                                             }
                                             :: typed)
                                            remaining

                        let inferTraitReturnType memberName =
                            let matchingMemberType =
                                methodDeclaration.Constraints
                                |> List.tryPick (fun constraint' ->
                                    match constraint' with
                                    | ParsedAbbreviationConstraint(ParsedGenericTypeApplication(ParsedNamedType(aliasName,
                                                                                                                _),
                                                                                                arguments,
                                                                                                _)) ->
                                        match
                                            typeAbbreviations
                                            |> Map.tryFind aliasName.Name
                                        with
                                        | Some abbreviation when
                                            abbreviation.TypeParameters.Length = arguments.Length
                                            ->
                                            let substitutions =
                                                List.zip abbreviation.TypeParameters arguments
                                                |> Map.ofList

                                            abbreviation.Constraints
                                            |> List.tryPick (
                                                function
                                                | ParsedMemberConstraint(_,
                                                                         constrainedMemberName,
                                                                         memberType,
                                                                         _) when
                                                    constrainedMemberName = memberName
                                                    ->
                                                    Some(substituteType substitutions memberType)
                                                | _ -> None
                                            )
                                        | _ -> None
                                    | ParsedDirectConstraint(ParsedMemberConstraint(_,
                                                                                    constrainedMemberName,
                                                                                    memberType,
                                                                                    _)) when
                                        constrainedMemberName = memberName
                                        ->
                                        Some memberType
                                    | _ -> None
                                )

                            match matchingMemberType with
                            | Some(ParsedFunctionType(_, returnType, _)) ->
                                match resolveType methodParameters returnType with
                                | Error error -> Error error
                                | Ok typedReturnType ->
                                    toCliType methodDeclaration.BodyRange typedReturnType
                            | Some returnType ->
                                match resolveType methodParameters returnType with
                                | Error error -> Error error
                                | Ok typedReturnType ->
                                    toCliType methodDeclaration.BodyRange typedReturnType
                            | None ->
                                diagnostic
                                    methodDeclaration.BodyRange
                                    $"no method constraint supplies '{memberName}'"

                        if
                            methodParameters.Count
                            <> methodDeclaration.TypeParameters.Length
                        then
                            diagnostic
                                methodDeclaration.Range
                                "method type parameters must be unique"
                        else
                            match
                                resolveMethodConstraints [] methodDeclaration.Constraints,
                                typeParameters [] methodDeclaration.Parameters,
                                methodDeclaration.Body
                            with
                            | Error error, _, _
                            | _, Error error, _ -> Error error
                            | Ok constraints,
                              Ok parameters,
                              MemberCall(receiverName, memberName, arguments) ->
                                let argumentName =
                                    function
                                    | TypedValueArgument name
                                    | TypedAddressOfArgument(name, _) -> name

                                let typedArguments =
                                    arguments
                                    |> List.choose (
                                        function
                                        | ValueReference name -> Some(TypedValueArgument name)
                                        | AddressOfExpression(rootName, memberPath) ->
                                            Some(TypedAddressOfArgument(rootName, memberPath))
                                        | _ -> None
                                    )

                                if
                                    typedArguments.Length
                                    <> arguments.Length
                                then
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "a trait-call argument is not a method parameter"
                                elif
                                    parameters
                                    |> List.exists (fun parameter -> parameter.Name = receiverName)
                                    |> not
                                then
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        $"the receiver '{receiverName}' is not a method parameter"
                                elif
                                    typedArguments
                                    |> List.exists (fun argument ->
                                        let argumentName = argumentName argument

                                        parameters
                                        |> List.exists (fun parameter ->
                                            parameter.Name = argumentName
                                        )
                                        |> not
                                    )
                                then
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "a trait-call argument is not a method parameter"
                                else
                                    match inferTraitReturnType memberName with
                                    | Error error -> Error error
                                    | Ok returnType ->
                                        let methodStableId =
                                            stableId
                                            + "/method:"
                                            + methodDeclaration.Name

                                        match
                                            attachTypedParameterAttributes
                                                methodStableId
                                                methodDeclaration.Parameters
                                                parameters
                                        with
                                        | Error error -> Error error
                                        | Ok parameters ->
                                            let exportFingerprint =
                                                Fingerprint.parts [
                                                    methodStableId
                                                    "generic-parameters"
                                                    yield! methodDeclaration.TypeParameters
                                                    "constraints"

                                                    yield!
                                                        constraints
                                                        |> List.map
                                                            TypeIdentity.methodConstraintIdentity

                                                    "parameters"

                                                    yield!
                                                        parameters
                                                        |> List.map TypeIdentity.parameter

                                                    "return"
                                                    TypeIdentity.cliType returnType
                                                    "trait-call"
                                                    receiverName
                                                    memberName

                                                    yield!
                                                        typedArguments
                                                        |> List.map TypeIdentity.callArgument
                                                ]

                                            Ok {
                                                StableId = methodStableId
                                                Name = methodDeclaration.Name
                                                IsPublic = true
                                                GenericParameters = methodDeclaration.TypeParameters
                                                Constraints = constraints
                                                Attributes = []
                                                Parameters = parameters
                                                ReturnType = returnType
                                                Body =
                                                    TypedTraitCall(
                                                        receiverName,
                                                        memberName,
                                                        typedArguments
                                                    )
                                                ExportFingerprint = exportFingerprint
                                                Range = methodDeclaration.Range
                                            }
                            | Ok _, Ok _, IntegerLiteral _
                            | Ok _, Ok _, UnitLiteral
                            | Ok _, Ok _, BooleanLiteral _
                            | Ok _, Ok _, StringLiteral _
                            | Ok _, Ok _, ValueReference _
                            | Ok _, Ok _, AddressOfExpression _
                            | Ok _, Ok _, UnitApplication _
                            | Ok _, Ok _, BoundInstanceMember _
                            | Ok _, Ok _, MemberAssignment _
                            | Ok _, Ok _, SequentialExpression _
                            | Ok _, Ok _, FunctionApplication _
                            | Ok _, Ok _, ExpressionMemberCall _
                            | Ok _, Ok _, ConditionalExpression _
                            | Ok _, Ok _, ExplicitUpcastExpression _
                            | Ok _, Ok _, SequentialValueExpression _
                            | Ok _, Ok _, LocalAssignment _
                            | Ok _, Ok _, BooleanNegationExpression _
                            | Ok _, Ok _, LetExpression _
                            | Ok _, Ok _, LambdaExpression _
                            | Ok _, Ok _, UnitLambdaExpression _
                            | Ok _, Ok _, TypeConstruction _
                            | Ok _, Ok _, ObjectExpression _
                            | Ok _, Ok _, MatchExpression _ ->
                                diagnostic
                                    methodDeclaration.BodyRange
                                    "static inline members require a constrained trait call"

                    match
                        declaration.Methods
                        |> List.map typeMethod
                        |> collectResults []
                    with
                    | Error error -> Error error
                    | Ok methods ->
                        Ok(
                            TypedStaticType {
                                StableId = stableId
                                Name = declaration.Name
                                Methods = methods
                                ExportFingerprint =
                                    methods
                                    |> List.map _.ExportFingerprint
                                    |> Fingerprint.parts
                                Range = declaration.Range
                            }
                        )
                | ParsedObjectType declaration ->
                    let stableId =
                        match declaration.Container with
                        | OrdinaryObjectType ->
                            parsed.StableId
                            + "/type:"
                            + declaration.Name
                        | ParsedExtensionModule(moduleName, _) ->
                            parsed.StableId
                            + "/module:"
                            + moduleName
                            + "/extension:"
                            + declaration.Name

                    let rec collectTypeParameters collected =
                        function
                        | ParsedTypeParameter(name, _) ->
                            if
                                collected
                                |> List.contains name
                            then
                                collected
                            else
                                collected
                                @ [ name ]
                        | ParsedFlexibleType(superType, _) ->
                            collectTypeParameters collected superType
                        | ParsedWildcardType _
                        | ParsedNamedType _ -> collected
                        | ParsedGenericTypeApplication(genericType, arguments, _) ->
                            (collectTypeParameters collected genericType, arguments)
                            ||> List.fold collectTypeParameters
                        | ParsedTupleType(elements, _) ->
                            (collected, elements)
                            ||> List.fold collectTypeParameters
                        | ParsedFunctionType(domain, range, _) ->
                            domain
                            |> collectTypeParameters collected
                            |> fun parameters -> collectTypeParameters parameters range

                    let fsharpUnitType = {
                        DeclarationId = "reference:FSharp.Core/type:Microsoft.FSharp.Core.Unit`0"
                        AssemblyName = "FSharp.Core"
                        TypeName = {
                            Namespace = "Microsoft.FSharp.Core"
                            Name = "Unit"
                        }
                        IsValueType = false
                    }

                    let fsharpFunctionType = {
                        DeclarationId =
                            "reference:FSharp.Core/type:Microsoft.FSharp.Core.FSharpFunc`2"
                        AssemblyName = "FSharp.Core"
                        TypeName = {
                            Namespace = "Microsoft.FSharp.Core"
                            Name = "FSharpFunc`2"
                        }
                        IsValueType = false
                    }

                    let rec toCliType methodParameterIndex range =
                        function
                        | TypedTypeParameter name ->
                            match
                                methodParameterIndex
                                |> Map.tryFind name
                            with
                            | Some index -> Ok(CliMethodTypeParameter index)
                            | None ->
                                diagnostic
                                    range
                                    $"the inferred method type parameter '{name}' is not declared"
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "Int32"
                            ->
                            Ok CliInt32
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "Boolean"
                            ->
                            Ok CliBoolean
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "String"
                            ->
                            Ok CliString
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "Object"
                            ->
                            Ok CliObject
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "Microsoft.FSharp.Core"
                            && resolvedType.TypeName.Name = "Unit"
                            ->
                            Ok(CliNamedType fsharpUnitType)
                        | TypedGenericTypeApplication(TypedNamedType resolvedType, arguments) ->
                            arguments
                            |> List.map (toCliType methodParameterIndex range)
                            |> collectResults []
                            |> Result.map (fun argumentTypes ->
                                CliGenericType(
                                    {
                                        DeclarationId = resolvedType.DeclarationId
                                        AssemblyName = resolvedType.AssemblyName
                                        TypeName = {
                                            Namespace = resolvedType.TypeName.Namespace
                                            Name =
                                                resolvedType.TypeName.Name
                                                + "`"
                                                + arguments.Length.ToString(
                                                    CultureInfo.InvariantCulture
                                                )
                                        }
                                        IsValueType = resolvedType.IsValueType
                                    },
                                    argumentTypes
                                )
                            )
                        | TypedNamedType resolvedType ->
                            Ok(
                                CliNamedType {
                                    DeclarationId = resolvedType.DeclarationId
                                    AssemblyName = resolvedType.AssemblyName
                                    TypeName = resolvedType.TypeName
                                    IsValueType = resolvedType.IsValueType
                                }
                            )
                        | TypedFunctionType(domain, rangeType) ->
                            match
                                toCliType methodParameterIndex range domain,
                                toCliType methodParameterIndex range rangeType
                            with
                            | Error error, _
                            | _, Error error -> Error error
                            | Ok domainType, Ok rangeType ->
                                Ok(
                                    CliGenericType(
                                        fsharpFunctionType,
                                        [
                                            domainType
                                            rangeType
                                        ]
                                    )
                                )
                        | TypedByRefType elementType ->
                            toCliType methodParameterIndex range elementType
                            |> Result.bind (fun cliElementType ->
                                match cliElementType with
                                | CliVoid -> diagnostic range "a byref element cannot be void"
                                | _ -> Ok(CliByRef cliElementType)
                            )
                        | typedType ->
                            diagnostic
                                range
                                $"the instance-member CLI type '{TypeIdentity.expression typedType}' is not yet supported"

                    let toCliReturnType methodParameterIndex range typedType =
                        match typedType with
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "Microsoft.FSharp.Core"
                            && resolvedType.TypeName.Name = "Unit"
                            ->
                            Ok CliVoid
                        | _ -> toCliType methodParameterIndex range typedType

                    let collectMethodTypeParameters
                        explicitTypeParameters
                        (parameters: ParsedParameter list)
                        returnType
                        =
                        let fromParameters =
                            (explicitTypeParameters, parameters)
                            ||> List.fold (fun collected parameter ->
                                collectTypeParameters collected parameter.Type
                            )

                        match returnType with
                        | Some returnType -> collectTypeParameters fromParameters returnType
                        | None -> fromParameters

                    let typeObjectMethodParameters
                        methodParameterIndex
                        declaredMethodParameters
                        (parameters: ParsedParameter list)
                        =
                        parameters
                        |> List.map (fun parameter ->
                            parameter.Type
                            |> expandTypeAbbreviations Set.empty
                            |> resolveType declaredMethodParameters
                            |> Result.bind (toCliType methodParameterIndex parameter.Range)
                            |> Result.map (fun parameterType ->
                                ({
                                    Name = parameter.Name
                                    Type = parameterType
                                    Attributes = []
                                }
                                : TypedParameter)
                            )
                        )
                        |> collectResults []

                    let typeObjectMethodReturnType
                        methodParameterIndex
                        declaredMethodParameters
                        returnType
                        =
                        match returnType with
                        | None -> Ok None
                        | Some returnType ->
                            returnType
                            |> expandTypeAbbreviations Set.empty
                            |> resolveType declaredMethodParameters
                            |> Result.bind (toCliReturnType methodParameterIndex returnType.Range)
                            |> Result.map Some

                    let finishObjectMethod (completion: ObjectMethodCompletion) =
                        let parameterIdentity =
                            match completion.Parameters with
                            | [] -> "unit"
                            | _ ->
                                completion.Parameters
                                |> List.map (fun parameter -> TypeIdentity.cliType parameter.Type)
                                |> String.concat "*"

                        let methodStableId =
                            stableId
                            + "/method:"
                            + completion.Name
                            + ":"
                            + parameterIdentity
                            + "->"
                            + TypeIdentity.cliType completion.ReturnType

                        match
                            typeCustomAttributes
                                methodStableId
                                completion.Kind.AllowedAttributeKinds
                                completion.ParsedAttributes,
                            attachTypedParameterAttributes
                                methodStableId
                                completion.ParsedParameters
                                completion.Parameters
                        with
                        | Error error, _
                        | _, Error error -> Error error
                        | Ok attributes, Ok parameters ->
                            let exportFingerprint =
                                Fingerprint.parts [
                                    methodStableId
                                    completion.Kind.ExportIdentity
                                    if completion.IsPublic then "public" else "internal"
                                    "generic-parameters"
                                    yield! completion.GenericParameters
                                    "constraints"

                                    yield!
                                        completion.Constraints
                                        |> List.map TypeIdentity.methodConstraintIdentity

                                    "attributes"

                                    yield!
                                        attributes
                                        |> List.map TypeIdentity.customAttribute

                                    "parameters"

                                    yield!
                                        parameters
                                        |> List.map TypeIdentity.parameter

                                    "return"
                                    TypeIdentity.cliType completion.ReturnType
                                    "inline-body"
                                    TypeIdentity.inlineBody completion.Body
                                ]

                            Ok {
                                StableId = methodStableId
                                Name = completion.Name
                                IsPublic = completion.IsPublic
                                GenericParameters = completion.GenericParameters
                                Constraints = completion.Constraints
                                Attributes = attributes
                                Parameters = parameters
                                ReturnType = completion.ReturnType
                                Body = completion.Body
                                ExportFingerprint = exportFingerprint
                                Range = completion.Range
                            }

                    let typeStaticMethod (methodDeclaration: ParsedStaticMethodDeclaration) =
                        let explicitTypeParametersAreUnique =
                            (methodDeclaration.TypeParameters
                             |> Set.ofList
                             |> Set.count) = methodDeclaration.TypeParameters.Length

                        if not explicitTypeParametersAreUnique then
                            diagnostic
                                methodDeclaration.Range
                                "method type parameters must be unique"
                        elif not (List.isEmpty methodDeclaration.Constraints) then
                            diagnostic
                                methodDeclaration.Range
                                "constraints on static object members are not yet supported"
                        else
                            let methodTypeParameters =
                                collectMethodTypeParameters
                                    methodDeclaration.TypeParameters
                                    methodDeclaration.Parameters
                                    methodDeclaration.ReturnType

                            let declaredMethodParameters =
                                HashSet<string>(methodTypeParameters, StringComparer.Ordinal)

                            let methodParameterIndex =
                                methodTypeParameters
                                |> List.mapi (fun index name -> name, index)
                                |> Map.ofList

                            let typedParameters =
                                typeObjectMethodParameters
                                    methodParameterIndex
                                    declaredMethodParameters
                                    methodDeclaration.Parameters

                            let declaredReturnType =
                                typeObjectMethodReturnType
                                    methodParameterIndex
                                    declaredMethodParameters
                                    methodDeclaration.ReturnType

                            match typedParameters, declaredReturnType with
                            | Error error, _
                            | _, Error error -> Error error
                            | Ok parameters, Ok _ when
                                (parameters
                                 |> List.map _.Name
                                 |> Set.ofList
                                 |> Set.count)
                                <> parameters.Length
                                ->
                                diagnostic
                                    methodDeclaration.Range
                                    "static-member parameter names must be unique"
                            | Ok parameters, Ok declaredReturnType ->
                                let rec substituteTypeArguments (arguments: CliType list) =
                                    function
                                    | CliTypeParameter index when index < arguments.Length ->
                                        arguments.[index]
                                    | CliGenericType(typeReference, typeArguments) ->
                                        CliGenericType(
                                            typeReference,
                                            typeArguments
                                            |> List.map (substituteTypeArguments arguments)
                                        )
                                    | CliByRef elementType ->
                                        CliByRef(substituteTypeArguments arguments elementType)
                                    | cliType -> cliType

                                let rec resolveAddressFields
                                    (fields: TypedFieldAddress list)
                                    (ownerType: CliType)
                                    =
                                    function
                                    | [] -> Ok(List.rev fields, ownerType)
                                    | fieldName :: remaining ->
                                        let ownerType =
                                            match ownerType with
                                            | CliByRef elementType -> elementType
                                            | cliType -> cliType

                                        let ownerReference, typeArguments =
                                            (match ownerType with
                                             | CliNamedType typeReference -> Some(typeReference, [])
                                             | CliGenericType(typeReference, arguments) ->
                                                 Some(typeReference, arguments)
                                             | _ -> None)
                                            |> Option.defaultWith (fun () ->
                                                invalidOp
                                                    "an address-of field owner must be a named type"
                                            )

                                        match
                                            checkedSourceStructs
                                            |> Seq.tryFind (fun sourceStruct ->
                                                sourceStruct.Declaration.StableId = ownerReference.DeclarationId
                                            )
                                        with
                                        | None ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                $"the address-of field owner '{TypeIdentity.cliType ownerType}' is not a local struct"
                                        | Some sourceStruct ->
                                            match
                                                sourceStruct.Declaration.Fields
                                                |> List.tryFind (fun field ->
                                                    field.Name = fieldName
                                                )
                                            with
                                            | None ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    $"the struct '{sourceStruct.Declaration.Name}' has no field '{fieldName}'"
                                            | Some field when not field.IsMutable ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    $"the address-of field '{fieldName}' is not mutable"
                                            | Some field ->
                                                let fieldType =
                                                    field.Type
                                                    |> substituteTypeArguments typeArguments

                                                resolveAddressFields
                                                    (({
                                                        DeclaringType = ownerType
                                                        Name = field.Name
                                                        FieldType = field.Type
                                                        TargetStableId = Some field.StableId
                                                     }
                                                     : TypedFieldAddress)
                                                     :: fields)
                                                    fieldType
                                                    remaining

                                let rec typeStaticExpression localBindings nextLocalIndex =
                                    function
                                    | IntegerLiteral value ->
                                        Ok(TypedIntegerLiteral value, CliInt32, nextLocalIndex)
                                    | StringLiteral value ->
                                        Ok(TypedStringLiteral value, CliString, nextLocalIndex)
                                    | UnitLiteral -> Ok(TypedUnitLiteral, CliVoid, nextLocalIndex)
                                    | ValueReference name ->
                                        match
                                            localBindings
                                            |> Map.tryFind name
                                        with
                                        | Some(localIndex, localType, _) ->
                                            Ok(
                                                TypedLocalReference localIndex,
                                                localType,
                                                nextLocalIndex
                                            )
                                        | None ->
                                            match
                                                parameters
                                                |> List.tryFindIndex (fun parameter ->
                                                    parameter.Name = name
                                                )
                                            with
                                            | Some index ->
                                                Ok(
                                                    TypedParameterReference index,
                                                    parameters.[index].Type,
                                                    nextLocalIndex
                                                )
                                            | None ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    $"the value '{name}' is not a static-member parameter or local binding"
                                    | UnitLambdaExpression(ValueReference captureName) ->
                                        match declaredReturnType with
                                        | Some(CliGenericType(functionReference,
                                                              [ domainType; rangeType ]) as functionType) when
                                            functionReference.DeclarationId = fsharpFunctionType.DeclarationId
                                            ->
                                            let unitType = CliNamedType fsharpUnitType

                                            match
                                                parameters
                                                |> List.tryFindIndex (fun parameter ->
                                                    parameter.Name = captureName
                                                )
                                            with
                                            | None ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    $"the lambda capture '{captureName}' is not a static-member parameter"
                                            | Some captureParameterIndex when
                                                domainType
                                                <> unitType
                                                ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "a unit lambda needs a unit-domain F# function type"
                                            | Some captureParameterIndex when
                                                parameters.[captureParameterIndex].Type
                                                <> rangeType
                                                ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the lambda body does not match the F# function range"
                                            | Some captureParameterIndex ->
                                                let converterName = {
                                                    Namespace = "System"
                                                    Name = "Converter"
                                                }

                                                match
                                                    resolveNamedType
                                                        2
                                                        converterName
                                                        methodDeclaration.BodyRange
                                                with
                                                | Error error -> Error error
                                                | Ok(TypedNamedType resolvedConverter) ->
                                                    let converterReference = {
                                                        DeclarationId =
                                                            resolvedConverter.DeclarationId
                                                        AssemblyName =
                                                            resolvedConverter.AssemblyName
                                                        TypeName = {
                                                            Namespace =
                                                                resolvedConverter.TypeName.Namespace
                                                            Name =
                                                                resolvedConverter.TypeName.Name
                                                                + "`2"
                                                        }
                                                        IsValueType = false
                                                    }

                                                    Ok(
                                                        TypedUnitLambda {
                                                            FunctionType = functionType
                                                            DelegateType =
                                                                CliGenericType(
                                                                    converterReference,
                                                                    [
                                                                        domainType
                                                                        rangeType
                                                                    ]
                                                                )
                                                            CaptureParameterIndex =
                                                                captureParameterIndex
                                                            CaptureName = captureName
                                                            CaptureType = rangeType
                                                            DomainType = domainType
                                                            RangeType = rangeType
                                                            SourceLine =
                                                                methodDeclaration.BodyRange.Start.Line
                                                            Range = methodDeclaration.BodyRange
                                                        },
                                                        functionType,
                                                        nextLocalIndex
                                                    )
                                                | Ok _ ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        "System.Converter did not resolve to a named CLI type"
                                        | _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "a unit lambda needs an explicit F# function return type"
                                    | UnitLambdaExpression _ ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            "only a captured value is supported in a unit lambda"
                                    | AddressOfExpression(rootName, memberPath) ->
                                        let source =
                                            match
                                                localBindings
                                                |> Map.tryFind rootName
                                            with
                                            | Some(localIndex, localType, isMutable) ->
                                                if
                                                    List.isEmpty memberPath
                                                    && not isMutable
                                                then
                                                    None
                                                else
                                                    Some(
                                                        TypedLocalAddress(localIndex, localType),
                                                        localType
                                                    )
                                            | None ->
                                                parameters
                                                |> List.tryFindIndex (fun parameter ->
                                                    parameter.Name = rootName
                                                )
                                                |> Option.map (fun parameterIndex ->
                                                    let parameterType =
                                                        parameters.[parameterIndex].Type

                                                    TypedParameterAddress(
                                                        parameterIndex,
                                                        parameterType
                                                    ),
                                                    parameterType
                                                )

                                        match source with
                                        | None ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                $"the address-of root '{rootName}' is not a mutable local or parameter"
                                        | Some(typedSource, sourceType) ->
                                            match resolveAddressFields [] sourceType memberPath with
                                            | Error error -> Error error
                                            | Ok(fields, addressedType) ->
                                                let addressedType =
                                                    match addressedType with
                                                    | CliByRef _ as byrefType when
                                                        List.isEmpty fields
                                                        ->
                                                        byrefType
                                                    | cliType -> CliByRef cliType

                                                Ok(
                                                    TypedAddressOf(typedSource, fields),
                                                    addressedType,
                                                    nextLocalIndex
                                                )
                                    | MemberCall(receiverName, memberName, arguments) ->
                                        let rec typeArguments
                                            typedArguments
                                            argumentTypes
                                            argumentLocalIndex
                                            =
                                            function
                                            | [] ->
                                                Ok(
                                                    List.rev typedArguments,
                                                    List.rev argumentTypes,
                                                    argumentLocalIndex
                                                )
                                            | argument :: remaining ->
                                                match
                                                    typeStaticExpression
                                                        localBindings
                                                        argumentLocalIndex
                                                        argument
                                                with
                                                | Error error -> Error error
                                                | Ok(typedArgument,
                                                     argumentType,
                                                     nextArgumentLocalIndex) ->
                                                    typeArguments
                                                        (typedArgument
                                                         :: typedArguments)
                                                        (argumentType
                                                         :: argumentTypes)
                                                        nextArgumentLocalIndex
                                                        remaining

                                        match typeArguments [] [] nextLocalIndex arguments with
                                        | Error error -> Error error
                                        | Ok(typedArguments, argumentTypes, nextArgumentLocalIndex) ->
                                            match
                                                resolveStaticMethod
                                                    receiverName
                                                    memberName
                                                    argumentTypes
                                                    methodDeclaration.BodyRange
                                            with
                                            | Error error -> Error error
                                            | Ok(target, genericArguments, returnType) ->
                                                Ok(
                                                    TypedStaticMethodCall(
                                                        target,
                                                        genericArguments,
                                                        typedArguments
                                                    ),
                                                    returnType,
                                                    nextArgumentLocalIndex
                                                )
                                    | FunctionApplication(functionExpression, argumentExpression) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                functionExpression
                                        with
                                        | Error error -> Error error
                                        | Ok(typedFunction,
                                             (CliGenericType(functionType, [ domainType; rangeType ]) as cliFunctionType),
                                             nextFunctionLocalIndex) when
                                            functionType.DeclarationId = fsharpFunctionType.DeclarationId
                                            ->
                                            match
                                                typeStaticExpression
                                                    localBindings
                                                    nextFunctionLocalIndex
                                                    argumentExpression
                                            with
                                            | Error error -> Error error
                                            | Ok(typedArgument, argumentType, nextArgumentLocalIndex) when
                                                argumentType = domainType
                                                ->
                                                Ok(
                                                    TypedFunctionApplication(
                                                        cliFunctionType,
                                                        domainType,
                                                        rangeType,
                                                        typedFunction,
                                                        typedArgument
                                                    ),
                                                    rangeType,
                                                    nextArgumentLocalIndex
                                                )
                                            | Ok _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the function argument type does not match its domain"
                                        | Ok _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "this expression is not an F# function"
                                    | ExpressionMemberCall(receiver, memberName, arguments) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                receiver
                                        with
                                        | Error error -> Error error
                                        | Ok(typedReceiver,
                                             (CliGenericType(typeReference, typeArguments) as receiverType),
                                             nextReceiverLocalIndex) when
                                            memberName = "Invoke"
                                            && typeReference.TypeName.Namespace = "System"
                                            && typeReference.TypeName.Name = "Func`"
                                                                             + typeArguments
                                                                                 .Length
                                                                                 .ToString(
                                                                                     CultureInfo.InvariantCulture
                                                                                 )
                                            && not (List.isEmpty typeArguments)
                                            ->
                                            let expectedArgumentTypes =
                                                typeArguments
                                                |> List.take (
                                                    typeArguments.Length
                                                    - 1
                                                )

                                            let resultType = List.last typeArguments

                                            let rec typeArguments'
                                                typedArguments
                                                actualArgumentTypes
                                                argumentLocalIndex
                                                =
                                                function
                                                | [] ->
                                                    Ok(
                                                        List.rev typedArguments,
                                                        List.rev actualArgumentTypes,
                                                        argumentLocalIndex
                                                    )
                                                | argument :: remaining ->
                                                    match
                                                        typeStaticExpression
                                                            localBindings
                                                            argumentLocalIndex
                                                            argument
                                                    with
                                                    | Error error -> Error error
                                                    | Ok(typedArgument,
                                                         argumentType,
                                                         nextArgumentLocalIndex) ->
                                                        typeArguments'
                                                            (typedArgument
                                                             :: typedArguments)
                                                            (argumentType
                                                             :: actualArgumentTypes)
                                                            nextArgumentLocalIndex
                                                            remaining

                                            match
                                                typeArguments'
                                                    []
                                                    []
                                                    nextReceiverLocalIndex
                                                    arguments
                                            with
                                            | Error error -> Error error
                                            | Ok(typedArguments,
                                                 actualArgumentTypes,
                                                 nextArgumentLocalIndex) when
                                                actualArgumentTypes = expectedArgumentTypes
                                                ->
                                                Ok(
                                                    TypedInstanceMethodCall(
                                                        {
                                                            DeclaringType = receiverType
                                                            Name = memberName
                                                            ParameterTypes =
                                                                expectedArgumentTypes
                                                                |> List.mapi (fun index _ ->
                                                                    CliTypeParameter index
                                                                )
                                                            ReturnType =
                                                                CliTypeParameter(
                                                                    typeArguments.Length
                                                                    - 1
                                                                )
                                                            ResultType = resultType
                                                        },
                                                        typedReceiver,
                                                        typedArguments
                                                    ),
                                                    resultType,
                                                    nextArgumentLocalIndex
                                                )
                                            | Ok _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the instance-member arguments do not match System.Func.Invoke"
                                        | Ok _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "this expression member call is not yet supported"
                                    | ConditionalExpression(condition,
                                                            ifTrue,
                                                            ifFalse,
                                                            conditionRange,
                                                            ifTrueRange,
                                                            ifFalseRange) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                condition
                                        with
                                        | Error error -> Error error
                                        | Ok(typedCondition, CliBoolean, nextConditionLocalIndex) ->
                                            match
                                                typeStaticExpression
                                                    localBindings
                                                    nextConditionLocalIndex
                                                    ifTrue
                                            with
                                            | Error error -> Error error
                                            | Ok(typedIfTrue, ifTrueType, nextIfTrueLocalIndex) ->
                                                match
                                                    typeStaticExpression
                                                        localBindings
                                                        nextIfTrueLocalIndex
                                                        ifFalse
                                                with
                                                | Error error -> Error error
                                                | Ok(typedIfFalse,
                                                     ifFalseType,
                                                     nextIfFalseLocalIndex) when
                                                    ifTrueType = ifFalseType
                                                    ->
                                                    Ok(
                                                        TypedConditional(
                                                            typedCondition,
                                                            typedIfTrue,
                                                            typedIfFalse,
                                                            conditionRange,
                                                            ifTrueRange,
                                                            ifFalseRange
                                                        ),
                                                        ifTrueType,
                                                        nextIfFalseLocalIndex
                                                    )
                                                | Ok _ ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        "the if expression branches do not have the same type"
                                        | Ok _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "the if expression condition is not bool"
                                    | ExplicitUpcastExpression(expression, targetType) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                expression
                                        with
                                        | Error error -> Error error
                                        | Ok(typedExpression,
                                             (CliMethodTypeParameter _ as sourceType),
                                             nextExpressionLocalIndex) ->
                                            match
                                                targetType
                                                |> expandTypeAbbreviations Set.empty
                                                |> resolveType declaredMethodParameters
                                            with
                                            | Error error -> Error error
                                            | Ok(TypedNamedType targetResolvedType) ->
                                                match
                                                    toCliType
                                                        methodParameterIndex
                                                        targetType.Range
                                                        (TypedNamedType targetResolvedType)
                                                with
                                                | Error error -> Error error
                                                | Ok targetCliType ->
                                                    Ok(
                                                        TypedUpcast(
                                                            sourceType,
                                                            targetCliType,
                                                            targetResolvedType,
                                                            typedExpression
                                                        ),
                                                        targetCliType,
                                                        nextExpressionLocalIndex
                                                    )
                                            | Ok _ ->
                                                diagnostic
                                                    targetType.Range
                                                    "the upcast target must be a named type"
                                        | Ok _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "this explicit upcast source is not yet supported"
                                    | BooleanNegationExpression(expression, range) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                expression
                                        with
                                        | Error error -> Error error
                                        | Ok(typedExpression, CliBoolean, nextExpressionLocalIndex) ->
                                            Ok(
                                                TypedBooleanNegation(typedExpression, range),
                                                CliBoolean,
                                                nextExpressionLocalIndex
                                            )
                                        | Ok _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "the operand of 'not' is not bool"
                                    | LocalAssignment(name, value) ->
                                        match
                                            localBindings
                                            |> Map.tryFind name
                                        with
                                        | None ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                $"the mutable local '{name}' is not defined"
                                        | Some(_, _, false) ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                $"the local '{name}' is not mutable"
                                        | Some(localIndex, localType, true) ->
                                            match
                                                typeStaticExpression
                                                    localBindings
                                                    nextLocalIndex
                                                    value
                                            with
                                            | Error error -> Error error
                                            | Ok(typedValue, valueType, nextValueLocalIndex) when
                                                valueType = localType
                                                ->
                                                Ok(
                                                    TypedLocalAssignment(
                                                        localIndex,
                                                        localType,
                                                        typedValue
                                                    ),
                                                    CliVoid,
                                                    nextValueLocalIndex
                                                )
                                            | Ok _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the assigned value does not match the mutable local type"
                                    | SequentialValueExpression expressions ->
                                        let rec typeExpressions
                                            typedExpressions
                                            expressionLocalIndex
                                            =
                                            function
                                            | [] ->
                                                let typedExpressions = List.rev typedExpressions

                                                match List.tryLast typedExpressions with
                                                | Some(_, resultType, _) ->
                                                    Ok(
                                                        TypedSequential typedExpressions,
                                                        resultType,
                                                        expressionLocalIndex
                                                    )
                                                | None ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        "a sequential expression must not be empty"
                                            | (expression, expressionRange) :: remaining ->
                                                match
                                                    typeStaticExpression
                                                        localBindings
                                                        expressionLocalIndex
                                                        expression
                                                with
                                                | Error error -> Error error
                                                | Ok(typedExpression,
                                                     expressionType,
                                                     nextExpressionLocalIndex) ->
                                                    typeExpressions
                                                        ((typedExpression,
                                                          expressionType,
                                                          expressionRange)
                                                         :: typedExpressions)
                                                        nextExpressionLocalIndex
                                                        remaining

                                        typeExpressions [] nextLocalIndex expressions
                                    | LetExpression(bindingName,
                                                    isMutable,
                                                    value,
                                                    body,
                                                    bindingRange,
                                                    bodyRange) ->
                                        match
                                            typeStaticExpression localBindings nextLocalIndex value
                                        with
                                        | Error error -> Error error
                                        | Ok(typedValue, valueType, nextValueLocalIndex) ->
                                            let localIndex = nextValueLocalIndex

                                            match
                                                typeStaticExpression
                                                    (localBindings
                                                     |> Map.add
                                                         bindingName
                                                         (localIndex, valueType, isMutable))
                                                    (localIndex
                                                     + 1)
                                                    body
                                            with
                                            | Error error -> Error error
                                            | Ok(typedBody, bodyType, nextBodyLocalIndex) ->
                                                Ok(
                                                    TypedLet(
                                                        localIndex,
                                                        bindingName,
                                                        isMutable,
                                                        valueType,
                                                        typedValue,
                                                        typedBody,
                                                        bindingRange,
                                                        bodyRange
                                                    ),
                                                    bodyType,
                                                    nextBodyLocalIndex
                                                )
                                    | MatchExpression(inputExpression,
                                                      clauses,
                                                      matchHeaderRange,
                                                      range) ->
                                        match clauses with
                                        | [ (ParsedTypeTestPattern(targetType,
                                                                   bindingName,
                                                                   patternRange),
                                             ifMatched,
                                             ifMatchedRange)
                                            (ParsedNamedPattern("_", _),
                                             ifNotMatched,
                                             ifNotMatchedRange) ] ->
                                            match
                                                typeStaticExpression
                                                    localBindings
                                                    nextLocalIndex
                                                    inputExpression
                                            with
                                            | Error error -> Error error
                                            | Ok(typedInput, inputType, nextInputLocalIndex) ->
                                                let targetTypeResult =
                                                    targetType
                                                    |> expandTypeAbbreviations Set.empty
                                                    |> resolveType declaredMethodParameters
                                                    |> Result.bind (
                                                        toCliType methodParameterIndex patternRange
                                                    )

                                                match targetTypeResult with
                                                | Error error -> Error error
                                                | Ok targetType when
                                                    match inputType, targetType with
                                                    | (CliObject | CliString | CliNamedType _ | CliGenericType _),
                                                      (CliObject | CliString | CliNamedType _ | CliGenericType _) ->
                                                        true
                                                    | _ -> false
                                                    ->
                                                    let localIndex = nextInputLocalIndex

                                                    match
                                                        typeStaticExpression
                                                            (localBindings
                                                             |> Map.add
                                                                 bindingName
                                                                 (localIndex, targetType, false))
                                                            (localIndex
                                                             + 1)
                                                            ifMatched
                                                    with
                                                    | Error error -> Error error
                                                    | Ok(typedIfMatched,
                                                         ifMatchedType,
                                                         nextMatchedLocalIndex) ->
                                                        match
                                                            typeStaticExpression
                                                                localBindings
                                                                nextMatchedLocalIndex
                                                                ifNotMatched
                                                        with
                                                        | Error error -> Error error
                                                        | Ok(typedIfNotMatched,
                                                             ifNotMatchedType,
                                                             nextNotMatchedLocalIndex) when
                                                            ifMatchedType = ifNotMatchedType
                                                            ->
                                                            Ok(
                                                                TypedTypeTestMatch(
                                                                    typedInput,
                                                                    targetType,
                                                                    localIndex,
                                                                    bindingName,
                                                                    typedIfMatched,
                                                                    typedIfNotMatched,
                                                                    matchHeaderRange,
                                                                    ifMatchedRange,
                                                                    ifNotMatchedRange,
                                                                    range
                                                                ),
                                                                ifMatchedType,
                                                                nextNotMatchedLocalIndex
                                                            )
                                                        | Ok _ ->
                                                            diagnostic
                                                                range
                                                                "the type-test match arms must have the same type"
                                                | Ok _ ->
                                                    diagnostic
                                                        range
                                                        "the runtime type-test match requires reference types"
                                        | _ ->
                                            diagnostic
                                                range
                                                "only a type-test clause followed by a wildcard clause is currently supported"
                                    | ObjectExpression(baseType,
                                                       constructorArguments,
                                                       isOverride,
                                                       receiverName,
                                                       memberName,
                                                       memberBody,
                                                       range) ->
                                        if not (List.isEmpty constructorArguments) then
                                            diagnostic
                                                range
                                                "object-expression constructor arguments are not yet supported"
                                        elif not isOverride then
                                            diagnostic
                                                range
                                                "only object-expression overrides are currently supported"
                                        elif
                                            memberName
                                            <> "ToString"
                                        then
                                            diagnostic
                                                range
                                                "only the parameterless System.Object.ToString override is currently supported"
                                        else
                                            match
                                                baseType
                                                |> expandTypeAbbreviations Set.empty
                                                |> resolveType declaredMethodParameters
                                            with
                                            | Error error -> Error error
                                            | Ok(TypedNamedType resolvedBaseType) when
                                                resolvedBaseType.TypeName.Namespace = "System"
                                                && resolvedBaseType.TypeName.Name = "Object"
                                                ->
                                                match memberBody with
                                                | StringLiteral value ->
                                                    let objectTypeStableId =
                                                        stableId
                                                        + "/method:"
                                                        + methodDeclaration.Name
                                                        + "/object-expression:"
                                                        + range.Start.Offset.ToString(
                                                            CultureInfo.InvariantCulture
                                                        )

                                                    let objectTypeReference = {
                                                        DeclarationId = objectTypeStableId
                                                        AssemblyName = String.Empty
                                                        TypeName = {
                                                            Namespace = String.Empty
                                                            Name =
                                                                "objectExpression@"
                                                                + range.Start.Line.ToString(
                                                                    CultureInfo.InvariantCulture
                                                                )
                                                        }
                                                        IsValueType = false
                                                    }

                                                    let baseCliType = CliObject

                                                    Ok(
                                                        TypedObjectExpression(
                                                            objectTypeReference,
                                                            baseCliType,
                                                            [],
                                                            true,
                                                            receiverName,
                                                            memberName,
                                                            CliString,
                                                            TypedStringLiteral value,
                                                            range
                                                        ),
                                                        baseCliType,
                                                        nextLocalIndex
                                                    )
                                                | _ ->
                                                    diagnostic
                                                        range
                                                        "the System.Object.ToString object-expression override must return a string literal"
                                            | Ok _ ->
                                                diagnostic
                                                    range
                                                    "only a System.Object object expression is currently supported"
                                    | BooleanLiteral _
                                    | UnitApplication _
                                    | BoundInstanceMember _
                                    | MemberAssignment _
                                    | SequentialExpression _
                                    | LambdaExpression _
                                    | TypeConstruction _ ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            "this static-member expression is not yet supported"

                                let rec inferredSubtypeConstraints =
                                    function
                                    | TypedUpcast(CliMethodTypeParameter parameterIndex,
                                                  _,
                                                  targetResolvedType,
                                                  expression) ->
                                        TypedDirectConstraint(
                                            TypedSubtypeConstraint(
                                                methodTypeParameters.[parameterIndex],
                                                TypedNamedType targetResolvedType
                                            )
                                        )
                                        :: inferredSubtypeConstraints expression
                                    | TypedUpcast(_, _, _, expression) ->
                                        inferredSubtypeConstraints expression
                                    | TypedLet(_, _, _, _, value, body, _, _) ->
                                        inferredSubtypeConstraints value
                                        @ inferredSubtypeConstraints body
                                    | TypedLocalAssignment(_, _, value) ->
                                        inferredSubtypeConstraints value
                                    | TypedBooleanNegation(expression, _) ->
                                        inferredSubtypeConstraints expression
                                    | TypedStaticMethodCall(_, _, arguments) ->
                                        arguments
                                        |> List.collect inferredSubtypeConstraints
                                    | TypedFunctionApplication(_,
                                                               _,
                                                               _,
                                                               functionExpression,
                                                               argumentExpression) ->
                                        inferredSubtypeConstraints functionExpression
                                        @ inferredSubtypeConstraints argumentExpression
                                    | TypedInstanceMethodCall(_, receiver, arguments) ->
                                        inferredSubtypeConstraints receiver
                                        @ (arguments
                                           |> List.collect inferredSubtypeConstraints)
                                    | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                                        inferredSubtypeConstraints condition
                                        @ inferredSubtypeConstraints ifTrue
                                        @ inferredSubtypeConstraints ifFalse
                                    | TypedObjectExpression(_, _, arguments, _, _, _, _, body, _) ->
                                        (arguments
                                         |> List.collect inferredSubtypeConstraints)
                                        @ inferredSubtypeConstraints body
                                    | TypedTypeTestMatch(input,
                                                         _,
                                                         _,
                                                         _,
                                                         ifMatched,
                                                         ifNotMatched,
                                                         _,
                                                         _,
                                                         _,
                                                         _) ->
                                        inferredSubtypeConstraints input
                                        @ inferredSubtypeConstraints ifMatched
                                        @ inferredSubtypeConstraints ifNotMatched
                                    | TypedSequential expressions ->
                                        expressions
                                        |> List.collect (fun (expression, _, _) ->
                                            inferredSubtypeConstraints expression
                                        )
                                    | TypedIntegerLiteral _
                                    | TypedStringLiteral _
                                    | TypedUnitLiteral
                                    | TypedParameterReference _
                                    | TypedLocalReference _
                                    | TypedAddressOf _
                                    | TypedBoundInstanceMethod _
                                    | TypedUnitLambda _
                                    | TypedResumableCode _
                                    | TypedResumableTryFinally _
                                    | TypedTraitCall _ -> []

                                let typedBody =
                                    typeStaticExpression Map.empty 0 methodDeclaration.Body

                                match typedBody with
                                | Error error -> Error error
                                | Ok(body, inferredReturnType, _) ->
                                    match declaredReturnType with
                                    | Some returnType when
                                        returnType
                                        <> inferredReturnType
                                        ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            "the static-member body does not match its declared return type"
                                    | _ ->
                                        let returnType =
                                            declaredReturnType
                                            |> Option.defaultValue inferredReturnType

                                        finishObjectMethod {
                                            Kind = StaticObjectMethod
                                            Name = methodDeclaration.Name
                                            IsPublic = true
                                            GenericParameters = methodTypeParameters
                                            Constraints =
                                                body
                                                |> inferredSubtypeConstraints
                                                |> List.distinctBy
                                                    TypeIdentity.methodConstraintIdentity
                                            ParsedAttributes = methodDeclaration.Attributes
                                            ParsedParameters = methodDeclaration.Parameters
                                            Parameters = parameters
                                            ReturnType = returnType
                                            Body = body
                                            Range = methodDeclaration.BodyRange
                                        }

                    let typeMethod (methodDeclaration: ParsedInstanceMethodDeclaration) =
                        let usedTypeParameterNames = HashSet<string>(StringComparer.Ordinal)

                        methodDeclaration.TypeParameters
                        |> List.iter (fun name ->
                            usedTypeParameterNames.Add(name)
                            |> ignore
                        )

                        let collectUsedTypeParameterNames typeExpression =
                            collectTypeParameters [] typeExpression
                            |> List.iter (fun name ->
                                usedTypeParameterNames.Add(name)
                                |> ignore
                            )

                        methodDeclaration.Parameters
                        |> List.iter (fun parameter -> collectUsedTypeParameterNames parameter.Type)

                        methodDeclaration.ReturnType
                        |> Option.iter collectUsedTypeParameterNames

                        let flexibleConstraints = ResizeArray<_>()
                        let mutable flexibleNameIndex = 0

                        let rec nextFlexibleTypeParameterName () =
                            let index = flexibleNameIndex

                            flexibleNameIndex <-
                                flexibleNameIndex
                                + 1

                            let candidate =
                                if index < 26 then
                                    char (
                                        int 'a'
                                        + index
                                    )
                                    |> string
                                else
                                    "a"
                                    + index.ToString(CultureInfo.InvariantCulture)

                            if usedTypeParameterNames.Add(candidate) then
                                candidate
                            else
                                nextFlexibleTypeParameterName ()

                        let rec generalizeFlexibleTypes =
                            function
                            | ParsedFlexibleType(superType, range) ->
                                let parameterName = nextFlexibleTypeParameterName ()
                                flexibleConstraints.Add(parameterName, superType, range)
                                ParsedTypeParameter(parameterName, range)
                            | ParsedGenericTypeApplication(genericType, arguments, range) ->
                                ParsedGenericTypeApplication(
                                    generalizeFlexibleTypes genericType,
                                    arguments
                                    |> List.map generalizeFlexibleTypes,
                                    range
                                )
                            | ParsedTupleType(elements, range) ->
                                ParsedTupleType(
                                    elements
                                    |> List.map generalizeFlexibleTypes,
                                    range
                                )
                            | ParsedFunctionType(domain, range, sourceRange) ->
                                ParsedFunctionType(
                                    generalizeFlexibleTypes domain,
                                    generalizeFlexibleTypes range,
                                    sourceRange
                                )
                            | ParsedTypeParameter _
                            | ParsedWildcardType _
                            | ParsedNamedType _ as typeExpression -> typeExpression

                        let generalizedParameters =
                            methodDeclaration.Parameters
                            |> List.map (fun parameter -> {
                                parameter with
                                    Type = generalizeFlexibleTypes parameter.Type
                            })

                        let generalizedReturnType =
                            methodDeclaration.ReturnType
                            |> Option.map generalizeFlexibleTypes

                        let methodTypeParameters =
                            collectMethodTypeParameters
                                methodDeclaration.TypeParameters
                                generalizedParameters
                                generalizedReturnType

                        let declaredMethodParameters =
                            HashSet<string>(methodTypeParameters, StringComparer.Ordinal)

                        let methodParameterIndex =
                            methodTypeParameters
                            |> List.mapi (fun index name -> name, index)
                            |> Map.ofList

                        let resolveDirectMethodConstraint =
                            function
                            | ParsedSubtypeConstraint(typeParameter, superType, range) ->
                                if not (declaredMethodParameters.Contains(typeParameter)) then
                                    diagnostic
                                        range
                                        $"the constrained type parameter '{typeParameter}' is not declared"
                                else
                                    resolveType declaredMethodParameters superType
                                    |> Result.map (fun typedSuperType ->
                                        TypedDirectConstraint(
                                            TypedSubtypeConstraint(typeParameter, typedSuperType)
                                        )
                                    )
                            | ParsedMemberConstraint(typeParameter, memberName, memberType, range) ->
                                if not (declaredMethodParameters.Contains(typeParameter)) then
                                    diagnostic
                                        range
                                        $"the constrained type parameter '{typeParameter}' is not declared"
                                else
                                    resolveType declaredMethodParameters memberType
                                    |> Result.map (fun typedMemberType ->
                                        TypedDirectConstraint(
                                            TypedMemberConstraint(
                                                typeParameter,
                                                memberName,
                                                typedMemberType
                                            )
                                        )
                                    )

                        let expandedAbbreviationConstraints constraintType =
                            match constraintType with
                            | ParsedGenericTypeApplication(ParsedNamedType(aliasName, _),
                                                           arguments,
                                                           _) when
                                String.IsNullOrEmpty(aliasName.Namespace)
                                ->
                                match
                                    typeAbbreviations
                                    |> Map.tryFind aliasName.Name
                                with
                                | Some abbreviation when
                                    abbreviation.TypeParameters.Length = arguments.Length
                                    ->
                                    let substitutions =
                                        List.zip abbreviation.TypeParameters arguments
                                        |> Map.ofList

                                    let substituteConstraint =
                                        function
                                        | ParsedSubtypeConstraint(typeParameter, superType, range) ->
                                            match
                                                ParsedTypeParameter(typeParameter, range)
                                                |> substituteType substitutions
                                            with
                                            | ParsedTypeParameter(substitutedParameter, _) ->
                                                Some(
                                                    ParsedSubtypeConstraint(
                                                        substitutedParameter,
                                                        substituteType substitutions superType,
                                                        range
                                                    )
                                                )
                                            | _ -> None
                                        | ParsedMemberConstraint(typeParameter,
                                                                 memberName,
                                                                 memberType,
                                                                 range) ->
                                            match
                                                ParsedTypeParameter(typeParameter, range)
                                                |> substituteType substitutions
                                            with
                                            | ParsedTypeParameter(substitutedParameter, _) ->
                                                Some(
                                                    ParsedMemberConstraint(
                                                        substitutedParameter,
                                                        memberName,
                                                        substituteType substitutions memberType,
                                                        range
                                                    )
                                                )
                                            | _ -> None

                                    abbreviation.Constraints
                                    |> List.choose substituteConstraint
                                | _ -> []
                            | _ -> []

                        let resolveMethodConstraint =
                            function
                            | ParsedAbbreviationConstraint constraintType ->
                                match resolveType declaredMethodParameters constraintType with
                                | Error error -> Error error
                                | Ok typedConstraintType ->
                                    constraintType
                                    |> expandedAbbreviationConstraints
                                    |> List.map resolveDirectMethodConstraint
                                    |> collectResults []
                                    |> Result.map (fun expanded ->
                                        TypedAbbreviationConstraint typedConstraintType
                                        :: expanded
                                    )
                            | ParsedDirectConstraint directConstraint ->
                                resolveDirectMethodConstraint directConstraint
                                |> Result.map List.singleton

                        let typedMethodConstraints =
                            methodDeclaration.Constraints
                            |> List.map resolveMethodConstraint
                            |> collectResults []
                            |> Result.map List.concat

                        let typedParameters =
                            typeObjectMethodParameters
                                methodParameterIndex
                                declaredMethodParameters
                                generalizedParameters

                        let typedFlexibleConstraints =
                            flexibleConstraints
                            |> Seq.map (fun (parameterName, superType, range) ->
                                superType
                                |> expandTypeAbbreviations Set.empty
                                |> resolveType declaredMethodParameters
                                |> Result.map (fun typedSuperType ->
                                    TypedDirectConstraint(
                                        TypedSubtypeConstraint(parameterName, typedSuperType)
                                    )
                                )
                            )
                            |> Seq.toList
                            |> collectResults []

                        match typedParameters, typedFlexibleConstraints, typedMethodConstraints with
                        | Error error, _, _
                        | _, Error error, _
                        | _, _, Error error -> Error error
                        | Ok _, Ok _, Ok _ when
                            (methodDeclaration.TypeParameters
                             |> Set.ofList
                             |> Set.count)
                            <> methodDeclaration.TypeParameters.Length
                            ->
                            diagnostic
                                methodDeclaration.Range
                                "method type parameters must be unique"
                        | Ok parameters, Ok _, Ok _ when
                            (parameters
                             |> List.map _.Name
                             |> Set.ofList
                             |> Set.count)
                            <> parameters.Length
                            ->
                            diagnostic
                                methodDeclaration.Range
                                "instance-member parameter names must be unique"
                        | Ok parameters, Ok flexibleConstraints, Ok explicitConstraints ->
                            let constraints =
                                explicitConstraints
                                @ flexibleConstraints

                            let isResumableCodeReference (reference: CliTypeReference) =
                                reference.AssemblyName = "FSharp.Core"
                                && reference.TypeName.Namespace = "Microsoft.FSharp.Core.CompilerServices"
                                && reference.TypeName.Name = "ResumableCode`2"

                            let resolveConstructedCliType expectedType constructedType =
                                let expandedType =
                                    constructedType
                                    |> expandTypeAbbreviations Set.empty

                                let resolveNormally () =
                                    expandedType
                                    |> resolveType declaredMethodParameters
                                    |> Result.bind (
                                        toCliType methodParameterIndex methodDeclaration.BodyRange
                                    )

                                match expectedType, expandedType with
                                | Some(CliGenericType(expectedReference, expectedArguments) as expected),
                                  ParsedGenericTypeApplication(ParsedNamedType(typeName, typeRange),
                                                               arguments,
                                                               _) when
                                    expectedArguments.Length = arguments.Length
                                    ->
                                    match resolveNamedType arguments.Length typeName typeRange with
                                    | Error error -> Error error
                                    | Ok(TypedNamedType resolvedGeneric) when
                                        resolvedGeneric.DeclarationId = expectedReference.DeclarationId
                                        ->
                                        List.map2
                                            (fun expectedArgument argument ->
                                                match argument with
                                                | ParsedWildcardType _ -> Ok()
                                                | _ ->
                                                    argument
                                                    |> resolveType declaredMethodParameters
                                                    |> Result.bind (
                                                        toCliType
                                                            methodParameterIndex
                                                            methodDeclaration.BodyRange
                                                    )
                                                    |> Result.bind (fun actualArgument ->
                                                        if actualArgument = expectedArgument then
                                                            Ok()
                                                        else
                                                            diagnostic
                                                                argument.Range
                                                                "the constructed type argument does not match its expected type"
                                                    )
                                            )
                                            expectedArguments
                                            arguments
                                        |> collectResults []
                                        |> Result.map (fun _ -> expected)
                                    | Ok _ ->
                                        diagnostic
                                            constructedType.Range
                                            "the constructed type does not match its expected type"
                                | Some expected, _ ->
                                    resolveNormally ()
                                    |> Result.bind (fun actual ->
                                        if actual = expected then
                                            Ok actual
                                        else
                                            diagnostic
                                                constructedType.Range
                                                "the constructed type does not match its expected type"
                                    )
                                | None, _ -> resolveNormally ()

                            let createResumableExpression
                                delegateType
                                dataType
                                captureParameterIndex
                                captureName
                                lambdaParameter
                                lambdaParameterType
                                body
                                argumentRange
                                =
                                let stateMachineName = {
                                    Namespace = "Microsoft.FSharp.Core.CompilerServices"
                                    Name = "ResumableStateMachine"
                                }

                                match
                                    resolveNamedType 1 stateMachineName methodDeclaration.BodyRange
                                with
                                | Error error -> Error error
                                | Ok(TypedNamedType resolvedStateMachine) ->
                                    let stateMachineReference = {
                                        DeclarationId = resolvedStateMachine.DeclarationId
                                        AssemblyName = resolvedStateMachine.AssemblyName
                                        TypeName = {
                                            Namespace = resolvedStateMachine.TypeName.Namespace
                                            Name =
                                                resolvedStateMachine.TypeName.Name
                                                + "`1"
                                        }
                                        IsValueType = resolvedStateMachine.IsValueType
                                    }

                                    let stateMachineType =
                                        CliGenericType(stateMachineReference, [ dataType ])

                                    let validateLambdaParameterType () =
                                        match lambdaParameterType with
                                        | None -> Ok()
                                        | Some parameterType ->
                                            parameterType
                                            |> expandTypeAbbreviations Set.empty
                                            |> resolveType declaredMethodParameters
                                            |> Result.bind (
                                                toCliType methodParameterIndex parameterType.Range
                                            )
                                            |> Result.bind (fun actualType ->
                                                if actualType = CliByRef stateMachineType then
                                                    Ok()
                                                else
                                                    diagnostic
                                                        parameterType.Range
                                                        "the lambda parameter type does not match the resumable state machine"
                                            )

                                    validateLambdaParameterType ()
                                    |> Result.map (fun () -> {
                                        DelegateType = delegateType
                                        StateMachineType = stateMachineType
                                        DataType = dataType
                                        CaptureParameterIndex = captureParameterIndex
                                        CaptureName = captureName
                                        StateMachineParameterName = lambdaParameter
                                        Body = body
                                        SourceLine = argumentRange.Start.Line
                                        Range = argumentRange
                                    })
                                | Ok _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "the resumable state-machine type did not resolve to a named CLI type"

                            let typeResumableCode
                                expectedType
                                constructedType
                                lambdaParameter
                                lambdaParameterType
                                lambdaBody
                                argumentRange
                                =
                                let unsupported () =
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "only the IcedTasks resumable Return and TryFinally compensation lambda shapes are supported"

                                match resolveConstructedCliType expectedType constructedType with
                                | Error error -> Error error
                                | Ok(CliGenericType(delegateReference, [ dataType; resultType ]) as delegateType) when
                                    isResumableCodeReference delegateReference
                                    ->
                                    match lambdaBody with
                                    | SequentialExpression [ MemberAssignment(assignmentRoot,
                                                                              [ dataFieldName
                                                                                resultFieldName ],
                                                                              ValueReference captureName)
                                                             BooleanLiteral true ] when
                                        assignmentRoot = lambdaParameter
                                        ->
                                        match
                                            parameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = captureName
                                            )
                                        with
                                        | None ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                $"the captured value '{captureName}' is not an instance-member parameter"
                                        | Some captureParameterIndex when
                                            parameters.[captureParameterIndex].Type
                                            <> resultType
                                            ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "the resumable result type does not match the captured parameter"
                                        | Some captureParameterIndex ->
                                            let localDataShape =
                                                match dataType with
                                                | CliGenericType(dataReference, dataArguments) when
                                                    String.IsNullOrEmpty(dataReference.AssemblyName)
                                                    && dataReference.IsValueType
                                                    ->
                                                    parsed.Declarations
                                                    |> List.tryPick (fun declaration ->
                                                        match
                                                            declaration,
                                                            ParsedDeclaration.tryTypeIdentity
                                                                parsed.StableId
                                                                declaration
                                                        with
                                                        | ParsedStructType structDeclaration,
                                                          Some(_, declarationId) when
                                                            declarationId = dataReference.DeclarationId
                                                            ->
                                                            Some(
                                                                structDeclaration,
                                                                dataArguments,
                                                                dataReference.DeclarationId
                                                            )
                                                        | _ -> None
                                                    )
                                                | _ -> None

                                            match localDataShape with
                                            | None ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the resumable data type must be a local struct"
                                            | Some(dataDeclaration, dataArguments, dataDeclarationId) ->
                                                let resultFieldIndex =
                                                    dataDeclaration.Fields
                                                    |> List.tryFindIndex (fun field ->
                                                        field.Name = resultFieldName
                                                    )

                                                let matchingResultFieldIndex =
                                                    match resultFieldIndex with
                                                    | Some index when
                                                        dataDeclaration.Fields.[index].IsMutable
                                                        ->
                                                        let field = dataDeclaration.Fields.[index]

                                                        match field.Type with
                                                        | ParsedTypeParameter(name, _) ->
                                                            dataDeclaration.TypeParameters
                                                            |> List.tryFindIndex ((=) name)
                                                            |> Option.bind (fun typeIndex ->
                                                                if
                                                                    typeIndex < dataArguments.Length
                                                                    && dataArguments.[typeIndex] = resultType
                                                                then
                                                                    Some index
                                                                else
                                                                    None
                                                            )
                                                        | _ -> None
                                                    | _ -> None

                                                match matchingResultFieldIndex with
                                                | None ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        $"the resumable data field '{resultFieldName}' must be mutable and match the result type"
                                                | Some _ when
                                                    dataFieldName
                                                    <> "Data"
                                                    ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        "the resumable state-machine field must be 'Data'"
                                                | Some resultFieldIndex ->
                                                    let resultFieldStableId =
                                                        dataDeclarationId
                                                        + "/field:"
                                                        + resultFieldIndex.ToString(
                                                            CultureInfo.InvariantCulture
                                                        )
                                                        + ":"
                                                        + resultFieldName

                                                    createResumableExpression
                                                        delegateType
                                                        dataType
                                                        captureParameterIndex
                                                        captureName
                                                        lambdaParameter
                                                        lambdaParameterType
                                                        (TypedStoreCapturedResult(
                                                            dataFieldName,
                                                            resultFieldName,
                                                            resultFieldStableId
                                                        ))
                                                        argumentRange
                                                    |> Result.map (fun expression ->
                                                        TypedResumableCode expression, delegateType
                                                    )
                                    | SequentialExpression [ UnitApplication captureName
                                                             BooleanLiteral true ] ->
                                        let unitType = CliNamedType fsharpUnitType

                                        let expectedCaptureType =
                                            CliGenericType(
                                                fsharpFunctionType,
                                                [
                                                    unitType
                                                    unitType
                                                ]
                                            )

                                        match
                                            parameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = captureName
                                            )
                                        with
                                        | None ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                $"the captured function '{captureName}' is not an instance-member parameter"
                                        | Some captureParameterIndex when
                                            resultType
                                            <> unitType
                                            ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "the compensation resumable code must return unit"
                                        | Some captureParameterIndex when
                                            parameters.[captureParameterIndex].Type
                                            <> expectedCaptureType
                                            ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "the compensation capture must have type unit -> unit"
                                        | Some captureParameterIndex ->
                                            createResumableExpression
                                                delegateType
                                                dataType
                                                captureParameterIndex
                                                captureName
                                                lambdaParameter
                                                lambdaParameterType
                                                TypedInvokeCapturedUnitFunction
                                                argumentRange
                                            |> Result.map (fun expression ->
                                                TypedResumableCode expression, delegateType
                                            )
                                    | _ -> unsupported ()
                                | Ok _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "the constructed expression must produce ResumableCode<'Data, 'T>"

                            let typeResumableTryFinally
                                computationName
                                constructedType
                                lambdaParameter
                                lambdaParameterType
                                lambdaBody
                                argumentRange
                                =
                                match
                                    parameters
                                    |> List.tryFindIndex (fun parameter ->
                                        parameter.Name = computationName
                                    )
                                with
                                | None ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        $"the computation '{computationName}' is not an instance-member parameter"
                                | Some computationParameterIndex ->
                                    match parameters.[computationParameterIndex].Type with
                                    | CliGenericType(delegateReference, [ dataType; resultType ]) as delegateType when
                                        isResumableCodeReference delegateReference
                                        ->
                                        let compensationDelegateType =
                                            CliGenericType(
                                                delegateReference,
                                                [
                                                    dataType
                                                    CliNamedType fsharpUnitType
                                                ]
                                            )

                                        match
                                            typeResumableCode
                                                (Some compensationDelegateType)
                                                constructedType
                                                lambdaParameter
                                                lambdaParameterType
                                                lambdaBody
                                                argumentRange
                                        with
                                        | Error error -> Error error
                                        | Ok(TypedResumableCode compensation, actualType) when
                                            actualType = compensationDelegateType
                                            ->
                                            let moduleTypeName = {
                                                Namespace = "Microsoft.FSharp.Core.CompilerServices"
                                                Name = "ResumableCode"
                                            }

                                            match
                                                resolveNamedType
                                                    0
                                                    moduleTypeName
                                                    methodDeclaration.BodyRange
                                            with
                                            | Error error -> Error error
                                            | Ok(TypedNamedType resolvedModuleType) ->
                                                let moduleType =
                                                    CliNamedType {
                                                        DeclarationId =
                                                            resolvedModuleType.DeclarationId
                                                        AssemblyName =
                                                            resolvedModuleType.AssemblyName
                                                        TypeName = resolvedModuleType.TypeName
                                                        IsValueType = resolvedModuleType.IsValueType
                                                    }

                                                Ok(
                                                    TypedResumableTryFinally {
                                                        ResumableCodeModuleType = moduleType
                                                        DelegateType = delegateType
                                                        DataType = dataType
                                                        ResultType = resultType
                                                        ComputationParameterIndex =
                                                            computationParameterIndex
                                                        ComputationName = computationName
                                                        Compensation = compensation
                                                    },
                                                    delegateType
                                                )
                                            | Ok _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the ResumableCode module did not resolve to a named CLI type"
                                        | Ok _ ->
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "the compensation expression does not match ResumableCode<'Data, unit>"
                                    | _ ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            "the computation must have type ResumableCode<'Data, 'T>"

                            let typeBoundInstanceMember receiverName memberName =
                                if
                                    receiverName
                                    <> methodDeclaration.ReceiverName
                                then
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        $"the bound member receiver '{receiverName}' is not this instance"
                                else
                                    let targetMethod =
                                        declaration.Methods
                                        |> List.tryPick (
                                            function
                                            | ParsedInstanceObjectMethod target when
                                                target.Name = memberName
                                                && List.isEmpty target.Parameters
                                                ->
                                                Some target
                                            | ParsedInstanceObjectMethod _
                                            | ParsedStaticObjectMethod _ -> None
                                        )

                                    match targetMethod with
                                    | None ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            $"the instance has no unit member named '{memberName}'"
                                    | Some targetMethod ->
                                        let targetTypeParameters =
                                            collectMethodTypeParameters
                                                []
                                                targetMethod.Parameters
                                                targetMethod.ReturnType

                                        if not (List.isEmpty targetTypeParameters) then
                                            diagnostic
                                                methodDeclaration.BodyRange
                                                "generic bound instance members are not yet supported"
                                        else
                                            let targetReturnType =
                                                typeObjectMethodReturnType
                                                    Map.empty
                                                    (HashSet<string>(StringComparer.Ordinal))
                                                    targetMethod.ReturnType
                                                |> Result.bind (fun declaredTargetReturnType ->
                                                    match
                                                        declaredTargetReturnType, targetMethod.Body
                                                    with
                                                    | Some returnType, _ -> Ok returnType
                                                    | None, IntegerLiteral _ -> Ok CliInt32
                                                    | None, UnitLiteral -> Ok CliVoid
                                                    | None, _ ->
                                                        diagnostic
                                                            targetMethod.BodyRange
                                                            "the bound member needs an explicit return type"
                                                )

                                            let functionReturnType =
                                                typeObjectMethodReturnType
                                                    methodParameterIndex
                                                    declaredMethodParameters
                                                    generalizedReturnType

                                            match functionReturnType, targetReturnType with
                                            | Error error, _
                                            | _, Error error -> Error error
                                            | Ok None, _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "a bound member result needs an explicit function return type"
                                            | Ok(Some(CliGenericType(functionReference,
                                                                     [ domainType; rangeType ]) as functionType)),
                                              Ok targetReturnType when
                                                functionReference.DeclarationId = fsharpFunctionType.DeclarationId
                                                ->
                                                let unitType = CliNamedType fsharpUnitType

                                                if
                                                    domainType
                                                    <> unitType
                                                then
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        "a unit member can only bind to a unit-domain F# function"
                                                elif
                                                    rangeType
                                                    <> targetReturnType
                                                then
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        "the bound member return type does not match the F# function range"
                                                else
                                                    let converterName = {
                                                        Namespace = "System"
                                                        Name = "Converter"
                                                    }

                                                    match
                                                        resolveNamedType
                                                            2
                                                            converterName
                                                            methodDeclaration.BodyRange
                                                    with
                                                    | Error error -> Error error
                                                    | Ok(TypedNamedType resolvedConverter) ->
                                                        let converterReference = {
                                                            DeclarationId =
                                                                resolvedConverter.DeclarationId
                                                            AssemblyName =
                                                                resolvedConverter.AssemblyName
                                                            TypeName = {
                                                                Namespace =
                                                                    resolvedConverter.TypeName.Namespace
                                                                Name =
                                                                    resolvedConverter.TypeName.Name
                                                                    + "`2"
                                                            }
                                                            IsValueType = false
                                                        }

                                                        let receiverType =
                                                            CliNamedType {
                                                                DeclarationId = stableId
                                                                AssemblyName = String.Empty
                                                                TypeName = {
                                                                    Namespace = parsed.Namespace
                                                                    Name = declaration.Name
                                                                }
                                                                IsValueType = false
                                                            }

                                                        let targetStableId =
                                                            stableId
                                                            + "/method:"
                                                            + targetMethod.Name
                                                            + ":unit->"
                                                            + TypeIdentity.cliType targetReturnType

                                                        Ok(
                                                            TypedBoundInstanceMethod {
                                                                FunctionType = functionType
                                                                DelegateType =
                                                                    CliGenericType(
                                                                        converterReference,
                                                                        [
                                                                            domainType
                                                                            rangeType
                                                                        ]
                                                                    )
                                                                ReceiverType = receiverType
                                                                TargetStableId = targetStableId
                                                                Target = {
                                                                    DeclaringType = receiverType
                                                                    Name = targetMethod.Name
                                                                    ParameterTypes = []
                                                                    ReturnType = targetReturnType
                                                                    ResultType = targetReturnType
                                                                }
                                                                DomainType = domainType
                                                                RangeType = rangeType
                                                                SourceLine =
                                                                    methodDeclaration.BodyRange.Start.Line
                                                                Range = methodDeclaration.BodyRange
                                                            },
                                                            functionType
                                                        )
                                                    | Ok _ ->
                                                        diagnostic
                                                            methodDeclaration.BodyRange
                                                            "System.Converter did not resolve to a named CLI type"
                                            | Ok(Some _), Ok _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    "the bound member result must be an F# function"

                            let typedBody =
                                match methodDeclaration.Body with
                                | IntegerLiteral value -> Ok(TypedIntegerLiteral value, CliInt32)
                                | ValueReference name ->
                                    match
                                        parameters
                                        |> List.tryFindIndex (fun parameter ->
                                            parameter.Name = name
                                        )
                                    with
                                    | Some index ->
                                        Ok(TypedParameterReference index, parameters.[index].Type)
                                    | None ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            $"the value '{name}' is not an instance-member parameter"
                                | BoundInstanceMember(receiverName, memberName) ->
                                    typeBoundInstanceMember receiverName memberName
                                | StringLiteral _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "string-valued instance members are not yet supported"
                                | MemberCall("ResumableCode",
                                             "TryFinally",
                                             [ ValueReference computationName
                                               TypeConstruction(constructedType,
                                                                LambdaExpression(lambdaParameter,
                                                                                 lambdaParameterType,
                                                                                 lambdaBody),
                                                                argumentRange) ]) ->
                                    typeResumableTryFinally
                                        computationName
                                        constructedType
                                        lambdaParameter
                                        lambdaParameterType
                                        lambdaBody
                                        argumentRange
                                | MemberCall _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "trait calls are not yet supported in instance members"
                                | TypeConstruction(constructedType,
                                                   LambdaExpression(lambdaParameter,
                                                                    lambdaParameterType,
                                                                    lambdaBody),
                                                   argumentRange) ->
                                    typeResumableCode
                                        None
                                        constructedType
                                        lambdaParameter
                                        lambdaParameterType
                                        lambdaBody
                                        argumentRange
                                | UnitLiteral
                                | BooleanLiteral _
                                | AddressOfExpression _
                                | UnitApplication _
                                | MemberAssignment _
                                | SequentialExpression _
                                | FunctionApplication _
                                | ExpressionMemberCall _
                                | ConditionalExpression _
                                | ExplicitUpcastExpression _
                                | SequentialValueExpression _
                                | LocalAssignment _
                                | BooleanNegationExpression _
                                | LetExpression _
                                | LambdaExpression _
                                | UnitLambdaExpression _
                                | TypeConstruction _
                                | ObjectExpression _
                                | MatchExpression _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "this instance-member expression is not yet supported"

                            let declaredReturnType =
                                typeObjectMethodReturnType
                                    methodParameterIndex
                                    declaredMethodParameters
                                    generalizedReturnType

                            match typedBody, declaredReturnType with
                            | Error error, _
                            | _, Error error -> Error error
                            | Ok(body, inferredReturnType), Ok(Some returnType) when
                                returnType
                                <> inferredReturnType
                                ->
                                diagnostic
                                    methodDeclaration.BodyRange
                                    "the instance-member body does not match its declared return type"
                            | Ok(body, inferredReturnType), Ok declaredReturnType ->
                                let returnType =
                                    declaredReturnType
                                    |> Option.defaultValue inferredReturnType

                                let range =
                                    match body with
                                    | TypedResumableCode expression -> expression.Range
                                    | TypedIntegerLiteral _
                                    | TypedStringLiteral _
                                    | TypedUnitLiteral
                                    | TypedParameterReference _
                                    | TypedLocalReference _
                                    | TypedLet _
                                    | TypedAddressOf _
                                    | TypedStaticMethodCall _
                                    | TypedFunctionApplication _
                                    | TypedInstanceMethodCall _
                                    | TypedBoundInstanceMethod _
                                    | TypedUnitLambda _
                                    | TypedConditional _
                                    | TypedUpcast _
                                    | TypedSequential _
                                    | TypedLocalAssignment _
                                    | TypedBooleanNegation _
                                    | TypedResumableTryFinally _
                                    | TypedObjectExpression _
                                    | TypedTypeTestMatch _
                                    | TypedTraitCall _ -> methodDeclaration.BodyRange

                                finishObjectMethod {
                                    Kind = InstanceObjectMethod
                                    Name = methodDeclaration.Name
                                    IsPublic = methodDeclaration.IsPublic
                                    GenericParameters = methodTypeParameters
                                    Constraints = constraints
                                    ParsedAttributes = methodDeclaration.Attributes
                                    ParsedParameters = generalizedParameters
                                    Parameters = parameters
                                    ReturnType = returnType
                                    Body = body
                                    Range = range
                                }

                    let typeObjectMethod =
                        function
                        | ParsedInstanceObjectMethod methodDeclaration ->
                            typeMethod methodDeclaration
                            |> Result.map (fun typedMethod ->
                                TypedInstanceObjectMethod(
                                    methodDeclaration.ReceiverName,
                                    typedMethod
                                )
                            )
                        | ParsedStaticObjectMethod methodDeclaration ->
                            typeStaticMethod methodDeclaration
                            |> Result.map TypedStaticObjectMethod

                    match
                        declaration.Methods
                        |> List.map typeObjectMethod
                        |> collectResults []
                    with
                    | Error error -> Error error
                    | Ok methods ->
                        let typedContainer =
                            match declaration.Container with
                            | OrdinaryObjectType -> Ok OrdinaryTypedObjectType
                            | ParsedExtensionModule(moduleName, attributes) ->
                                let targetTypeName = {
                                    Namespace = String.Empty
                                    Name = declaration.Name
                                }

                                match
                                    resolveNamedType 0 targetTypeName declaration.ConstructorRange,
                                    typeCustomAttributes stableId [ AutoOpenAttribute ] attributes
                                with
                                | Error error, _
                                | _, Error error -> Error error
                                | Ok targetType, Ok typedAttributes ->
                                    targetType
                                    |> toCliType Map.empty declaration.ConstructorRange
                                    |> Result.map (fun cliTargetType ->
                                        TypedExtensionModule(
                                            moduleName,
                                            typedAttributes,
                                            cliTargetType
                                        )
                                    )

                        match typedContainer with
                        | Error error -> Error error
                        | Ok typedContainer ->
                            let exportFingerprint =
                                match typedContainer with
                                | OrdinaryTypedObjectType ->
                                    Fingerprint.parts [
                                        stableId
                                        "constructor:unit"

                                        yield!
                                            methods
                                            |> List.map (fun methodDeclaration ->
                                                methodDeclaration.Method.ExportFingerprint
                                            )
                                    ]
                                | TypedExtensionModule(moduleName, attributes, targetType) ->
                                    Fingerprint.parts [
                                        stableId
                                        "extension-module"
                                        moduleName
                                        TypeIdentity.cliType targetType
                                        "attributes"

                                        yield!
                                            attributes
                                            |> List.map TypeIdentity.customAttribute

                                        yield!
                                            methods
                                            |> List.map (fun methodDeclaration ->
                                                methodDeclaration.Method.ExportFingerprint
                                            )
                                    ]

                            Ok(
                                TypedObjectType {
                                    StableId = stableId
                                    Container = typedContainer
                                    Name = declaration.Name
                                    Methods = methods
                                    ExportFingerprint = exportFingerprint
                                    ConstructorRange = declaration.ConstructorRange
                                    Range = declaration.Range
                                }
                            )
                | ParsedStructType declaration ->
                    let stableId =
                        parsed.StableId
                        + "/type:"
                        + declaration.Name

                    let declaredParameters =
                        HashSet<string>(declaration.TypeParameters, StringComparer.Ordinal)

                    let parameterIndex =
                        declaration.TypeParameters
                        |> List.mapi (fun index name -> name, index)
                        |> Map.ofList

                    let toFieldCliType range =
                        function
                        | TypedTypeParameter name ->
                            match
                                parameterIndex
                                |> Map.tryFind name
                            with
                            | Some index -> Ok(CliTypeParameter index)
                            | None ->
                                diagnostic range $"the type parameter '{name}' is not declared"
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "Int32"
                            ->
                            Ok CliInt32
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "Boolean"
                            ->
                            Ok CliBoolean
                        | TypedNamedType resolvedType when
                            resolvedType.TypeName.Namespace = "System"
                            && resolvedType.TypeName.Name = "String"
                            ->
                            Ok CliString
                        | typedType ->
                            diagnostic
                                range
                                $"the field type '{TypeIdentity.expression typedType}' is not yet supported"

                    let typeField (index: int) (field: ParsedFieldDeclaration) =
                        let fieldStableId =
                            stableId
                            + "/field:"
                            + index.ToString(CultureInfo.InvariantCulture)
                            + ":"
                            + field.Name

                        match
                            resolveType declaredParameters field.Type,
                            typeCustomAttributes
                                fieldStableId
                                [ DefaultValueAttribute ]
                                field.Attributes
                        with
                        | Error error, _
                        | _, Error error -> Error error
                        | Ok typedType, Ok attributes ->
                            match toFieldCliType field.Range typedType with
                            | Error error -> Error error
                            | Ok cliType ->
                                Ok {
                                    StableId = fieldStableId
                                    Name = field.Name
                                    IsMutable = field.IsMutable
                                    Type = cliType
                                    Attributes = attributes
                                    ExportFingerprint =
                                        Fingerprint.parts [
                                            fieldStableId
                                            if field.IsMutable then "mutable" else "immutable"
                                            TypeIdentity.cliType cliType

                                            yield!
                                                attributes
                                                |> List.map TypeIdentity.customAttribute
                                        ]
                                }

                    if
                        declaredParameters.Count
                        <> declaration.TypeParameters.Length
                    then
                        diagnostic declaration.Range "generic type parameters must be unique"
                    else
                        match
                            typeCustomAttributes
                                stableId
                                [
                                    StructAttribute
                                    NoComparisonAttribute
                                    NoEqualityAttribute
                                ]
                                declaration.Attributes,
                            declaration.Fields
                            |> List.mapi typeField
                            |> collectResults []
                        with
                        | Error error, _
                        | _, Error error -> Error error
                        | Ok attributes, Ok fields when
                            attributes
                            |> List.exists (fun attribute -> attribute.Kind = StructAttribute)
                            |> not
                            ->
                            diagnostic
                                declaration.Range
                                "the explicit-field type requires StructAttribute"
                        | Ok attributes, Ok fields ->
                            let exportFingerprint =
                                Fingerprint.parts [
                                    stableId
                                    "generic-parameters"
                                    yield! declaration.TypeParameters
                                    "attributes"
                                    yield!
                                        attributes
                                        |> List.map TypeIdentity.customAttribute
                                    "fields"

                                    yield!
                                        fields
                                        |> List.map _.ExportFingerprint
                                ]

                            Ok(
                                TypedStructType {
                                    StableId = stableId
                                    Name = declaration.Name
                                    GenericParameters = declaration.TypeParameters
                                    Attributes = attributes
                                    Fields = fields
                                    ExportFingerprint = exportFingerprint
                                    Range = declaration.Range
                                }
                            )

            let typeAssemblyAttribute index (attribute: ParsedAssemblyAttribute) =
                let attributeTypeName =
                    if String.IsNullOrEmpty(attribute.AttributeType.Namespace) then
                        attribute.AttributeType.Name
                    else
                        attribute.AttributeType.Namespace
                        + "."
                        + attribute.AttributeType.Name

                let typedAttribute kind =
                    let stableId =
                        parsed.StableId
                        + "/assembly-attribute:"
                        + index.ToString()
                        + ":"
                        + attributeTypeName

                    let exportFingerprint =
                        Fingerprint.parts [
                            attributeTypeName

                            yield! attribute.ConstructorArguments

                            yield!
                                attribute.NamedArguments
                                |> List.collect (fun argument -> [
                                    argument.Name
                                    argument.Value
                                ])
                        ]
                        |> Fingerprint.text

                    Ok {
                        StableId = stableId
                        Kind = kind
                        AttributeType = attribute.AttributeType
                        ConstructorArguments = attribute.ConstructorArguments
                        NamedArguments = attribute.NamedArguments
                        ExportFingerprint = exportFingerprint
                        Range = attribute.Range
                    }

                let tryAttributeKind =
                    function
                    | "System.Runtime.Versioning.TargetFrameworkAttribute" ->
                        Some TargetFrameworkAttribute
                    | "System.Reflection.AssemblyTitleAttribute" -> Some AssemblyTitleAttribute
                    | "System.Reflection.AssemblyProductAttribute" -> Some AssemblyProductAttribute
                    | "System.Reflection.AssemblyVersionAttribute" -> Some AssemblyVersionAttribute
                    | "System.Reflection.AssemblyMetadataAttribute" ->
                        Some AssemblyMetadataAttribute
                    | "System.Reflection.AssemblyFileVersionAttribute" ->
                        Some AssemblyFileVersionAttribute
                    | "System.Reflection.AssemblyInformationalVersionAttribute" ->
                        Some AssemblyInformationalVersionAttribute
                    | _ -> None

                match
                    tryAttributeKind attributeTypeName,
                    attribute.ConstructorArguments,
                    attribute.NamedArguments
                with
                | Some TargetFrameworkAttribute, [ _ ], [ { Name = "FrameworkDisplayName" } ] ->
                    typedAttribute TargetFrameworkAttribute
                | Some AssemblyVersionAttribute, [ value ], [] ->
                    match Version.TryParse(value) with
                    | true, _ -> typedAttribute AssemblyVersionAttribute
                    | false, _ ->
                        Error {
                            Code = "FSC2P1001"
                            Message = "the assembly version is invalid"
                            Path = Some sourcePath
                            Range = Some attribute.Range
                        }
                | Some AssemblyTitleAttribute, [ _ ], [] -> typedAttribute AssemblyTitleAttribute
                | Some AssemblyProductAttribute, [ _ ], [] ->
                    typedAttribute AssemblyProductAttribute
                | Some AssemblyFileVersionAttribute, [ _ ], [] ->
                    typedAttribute AssemblyFileVersionAttribute
                | Some AssemblyInformationalVersionAttribute, [ _ ], [] ->
                    typedAttribute AssemblyInformationalVersionAttribute
                | Some AssemblyMetadataAttribute, [ _; _ ], [] ->
                    typedAttribute AssemblyMetadataAttribute
                | Some TargetFrameworkAttribute, _, _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message =
                            "the generated target-framework attribute has unsupported named arguments"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }
                | Some _, _, _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message = "the assembly attribute has unsupported arguments"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }
                | None, _, _ ->
                    Error {
                        Code = "FSC2P1001"
                        Message = "unsupported assembly attribute"
                        Path = Some sourcePath
                        Range = Some attribute.Range
                    }

            let typedAssemblyAttributes =
                parsed.AssemblyAttributes
                |> List.mapi typeAssemblyAttribute
                |> collectResults []

            let typedModuleAttributes =
                typeCustomAttributes parsed.StableId [ AutoOpenAttribute ] parsed.Attributes

            match typedModuleAttributes, typedAssemblyAttributes with
            | Error diagnostic, _
            | _, Error diagnostic -> Error diagnostic
            | Ok moduleAttributes, Ok assemblyAttributes ->
                let rec typeDeclarations completed =
                    function
                    | [] -> Ok(List.rev completed)
                    | declaration :: remaining ->
                        match typeDeclaration declaration with
                        | Error diagnostic -> Error diagnostic
                        | Ok typedDeclaration ->
                            addCheckedDeclaration parsed.Namespace typedDeclaration

                            typeDeclarations
                                (typedDeclaration
                                 :: completed)
                                remaining

                let typedDeclarations =
                    parsed.Declarations
                    |> typeDeclarations []

                match typedDeclarations with
                | Error diagnostic -> Error diagnostic
                | Ok declarations ->
                    let typed = {
                        StableId = parsed.StableId
                        ContainerKind = parsed.ContainerKind
                        Namespace = parsed.Namespace
                        Name = parsed.Name
                        IsPublic = parsed.IsPublic
                        SourceChecksum = parsed.SourceChecksum
                        ContentFingerprint = parsed.ContentFingerprint
                        Attributes = moduleAttributes
                        AssemblyAttributes = assemblyAttributes
                        Declarations = declarations
                        ExportFingerprint =
                            [
                                parsed.StableId

                                if parsed.IsPublic then "public" else "internal"

                                yield!
                                    moduleAttributes
                                    |> List.map _.ExportFingerprint

                                yield!
                                    assemblyAttributes
                                    |> List.map _.ExportFingerprint

                                yield!
                                    declarations
                                    |> List.map _.ExportFingerprint
                            ]
                            |> String.concat "|"
                            |> Fingerprint.text
                    }

                    checkCache.Add(key, typed)
                    Ok(typed, key)

    let lower (assemblyName: string) (typedModules: TypedModule list) =
        let methodImplementationHash (methodDeclaration: TypedMethodDeclaration) =
            Fingerprint.parts [
                methodDeclaration.StableId
                methodDeclaration.ExportFingerprint
                TypeIdentity.inlineBody methodDeclaration.Body
            ]

        let modulesWithContentHashes =
            typedModules
            |> List.map (fun typed ->
                let declarationsWithContentHashes =
                    typed.Declarations
                    |> List.map (fun declaration ->
                        let contentHash =
                            match declaration with
                            | TypedMethod methodDeclaration ->
                                methodImplementationHash methodDeclaration
                            | TypedLiteralField fieldDeclaration ->
                                Fingerprint.text (
                                    fieldDeclaration.StableId
                                    + "="
                                    + fieldDeclaration.Value
                                )
                            | TypedTypeAbbreviation typeDeclaration ->
                                Fingerprint.parts [
                                    typeDeclaration.StableId
                                    "parameters"
                                    yield! typeDeclaration.TypeParameters
                                    "constraints"

                                    yield!
                                        typeDeclaration.Constraints
                                        |> List.map TypeIdentity.constraintIdentity

                                    "target"
                                    TypeIdentity.expression typeDeclaration.TargetType

                                    if typeDeclaration.AllowsNull then "null" else "non-null"
                                ]
                                |> Fingerprint.text
                            | TypedStaticType typeDeclaration ->
                                Fingerprint.parts [
                                    typeDeclaration.StableId

                                    yield!
                                        typeDeclaration.Methods
                                        |> List.collect (fun methodDeclaration -> [
                                            methodDeclaration.StableId
                                            methodDeclaration.ExportFingerprint
                                        ])
                                ]
                                |> Fingerprint.text
                            | TypedObjectType typeDeclaration ->
                                Fingerprint.parts [
                                    typeDeclaration.StableId

                                    yield!
                                        typeDeclaration.Methods
                                        |> List.collect (fun objectMethodDeclaration ->
                                            let methodDeclaration =
                                                objectMethodDeclaration.Method

                                            [
                                                methodDeclaration.StableId
                                                methodDeclaration.ExportFingerprint
                                                methodImplementationHash methodDeclaration
                                            ]
                                        )
                                ]
                                |> Fingerprint.text
                            | TypedStructType typeDeclaration ->
                                Fingerprint.parts [
                                    typeDeclaration.StableId
                                    "generic-parameters"
                                    yield! typeDeclaration.GenericParameters
                                    "attributes"

                                    yield!
                                        typeDeclaration.Attributes
                                        |> List.map TypeIdentity.customAttribute

                                    "fields"

                                    yield!
                                        typeDeclaration.Fields
                                        |> List.map _.ExportFingerprint
                                ]
                                |> Fingerprint.text

                        declaration, contentHash
                    )

                typed, declarationsWithContentHashes
            )

        let assemblyAttributesWithContentHashes =
            typedModules
            |> List.collect (fun typed ->
                typed.AssemblyAttributes
                |> List.map (fun attribute -> attribute, attribute.ExportFingerprint)
            )

        let isAssemblyVersionAttribute (attribute: TypedAssemblyAttribute) =
            attribute.Kind = AssemblyVersionAttribute

        let assemblyVersion =
            assemblyAttributesWithContentHashes
            |> List.tryPick (fun (attribute, _) ->
                if isAssemblyVersionAttribute attribute then
                    attribute.ConstructorArguments
                    |> List.tryHead
                else
                    None
            )
            |> Option.map (fun value ->
                let parsed = Version.Parse(value)

                Version(parsed.Major, parsed.Minor, max 0 parsed.Build, max 0 parsed.Revision)
            )
            |> Option.defaultValue (Version(1, 0, 0, 0))

        let implementationFingerprint =
            modulesWithContentHashes
            |> List.collect (
                snd
                >> List.map snd
            )
            |> String.concat "|"
            |> Fingerprint.text

        let contentFingerprint =
            typedModules
            |> List.map _.ContentFingerprint
            |> String.concat "|"
            |> Fingerprint.text

        let debugFingerprint =
            typedModules
            |> List.map (fun typed ->
                typed.SourceChecksum
                |> Seq.toArray
                |> Convert.ToHexString
            )
            |> Fingerprint.parts

        let key =
            Fingerprint.parts [
                querySchema.ToString(CultureInfo.InvariantCulture)
                assemblyName
                implementationFingerprint
                contentFingerprint
                debugFingerprint
            ]

        match lowerCache.TryGetValue(key) with
        | true, symbolic ->
            lowerHits <-
                lowerHits
                + 1

            symbolic, key
        | false, _ ->
            lowerMisses <-
                lowerMisses
                + 1

            let assemblyStableId =
                "assembly:"
                + assemblyName

            let moduleName =
                assemblyName
                + ".dll"

            let moduleStableId =
                assemblyStableId
                + "/module:"
                + moduleName

            let documents =
                typedModules
                |> List.mapi (fun documentIndex typed -> {
                    SchemaVersion = querySchema
                    StableId =
                        moduleStableId
                        + "/document:"
                        + documentIndex.ToString()
                    Checksum = typed.SourceChecksum
                })

            let assemblyAttributes =
                assemblyAttributesWithContentHashes
                |> List.filter (
                    fst
                    >> isAssemblyVersionAttribute
                    >> not
                )
                |> List.map (fun (attribute, contentHash) -> {
                    SchemaVersion = querySchema
                    StableId = attribute.StableId
                    Kind = attribute.Kind
                    AttributeType = attribute.AttributeType
                    ConstructorArguments = attribute.ConstructorArguments
                    NamedArguments =
                        attribute.NamedArguments
                        |> List.map (fun argument -> {
                            Name = argument.Name
                            Value = argument.Value
                        })
                    ContentHash = contentHash
                })

            let typeAbbreviations =
                modulesWithContentHashes
                |> List.collect (fun (_, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, contentHash) ->
                        match declaration with
                        | TypedTypeAbbreviation typeDeclaration ->
                            Some {
                                SchemaVersion = querySchema
                                StableId = typeDeclaration.StableId
                                Name = typeDeclaration.Name
                                TypeParameters = typeDeclaration.TypeParameters
                                Constraints = typeDeclaration.Constraints
                                TargetType = typeDeclaration.TargetType
                                AllowsNull = typeDeclaration.AllowsNull
                                ContentHash = contentHash
                            }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedStaticType _
                        | TypedObjectType _
                        | TypedStructType _ -> None
                    )
                )

            let customAttributeFragment (attribute: TypedCustomAttribute) = {
                SchemaVersion = querySchema
                StableId = attribute.StableId
                Kind = attribute.Kind
                ConstructorArguments = attribute.ConstructorArguments
                ContentHash = attribute.ExportFingerprint
            }

            let parameterFragment (parameter: TypedParameter) : SymbolicParameterFragment = {
                Name = parameter.Name
                Type = parameter.Type
                Attributes =
                    parameter.Attributes
                    |> List.map customAttributeFragment
            }

            let compilationMappingAttribute ownerStableId sourceConstruct =
                let stableId =
                    ownerStableId
                    + "/attribute:compilation-mapping"

                let arguments = [ TypedSourceConstructAttributeArgument sourceConstruct ]

                {
                    SchemaVersion = querySchema
                    StableId = stableId
                    Kind = CompilationMappingAttribute
                    ConstructorArguments = arguments
                    ContentHash =
                        Fingerprint.parts [
                            stableId
                            TypeIdentity.attributeKind CompilationMappingAttribute
                            yield!
                                arguments
                                |> List.map TypeIdentity.attributeArgument
                        ]
                }

            let rec methodTypeParametersToTypeParameters =
                function
                | CliMethodTypeParameter index -> CliTypeParameter index
                | CliByRef elementType -> CliByRef(methodTypeParametersToTypeParameters elementType)
                | CliGenericType(typeReference, arguments) ->
                    CliGenericType(
                        typeReference,
                        arguments
                        |> List.map methodTypeParametersToTypeParameters
                    )
                | (CliInt32 | CliBoolean | CliString | CliObject | CliNativeInt | CliVoid | CliTypeParameter _ | CliNamedType _) as cliType ->
                    cliType

            let closureName
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedResumableCodeExpression)
                =
                methodDeclaration.Name
                + "@"
                + expression.SourceLine.ToString(CultureInfo.InvariantCulture)

            let closureStableId (methodDeclaration: TypedMethodDeclaration) =
                methodDeclaration.StableId
                + "/closure:resumable-code"

            let closureTypeReference
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedResumableCodeExpression)
                =
                let name = closureName methodDeclaration expression

                {
                    DeclarationId = closureStableId methodDeclaration
                    AssemblyName = String.Empty
                    TypeName = {
                        Namespace = String.Empty
                        Name =
                            if List.isEmpty methodDeclaration.GenericParameters then
                                name
                            else
                                name
                                + "`"
                                + methodDeclaration.GenericParameters.Length.ToString(
                                    CultureInfo.InvariantCulture
                                )
                    }
                    IsValueType = false
                }

            let instantiateClosure typeReference genericArguments =
                match genericArguments with
                | [] -> CliNamedType typeReference
                | _ -> CliGenericType(typeReference, genericArguments)

            let closureLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedResumableCodeExpression)
                =
                let stableId = closureStableId methodDeclaration
                let name = closureName methodDeclaration expression
                let typeReference = closureTypeReference methodDeclaration expression

                let methodArguments =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index _ -> CliMethodTypeParameter index)

                let definitionArguments =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index _ -> CliTypeParameter index)

                let methodType = instantiateClosure typeReference methodArguments
                let definitionType = instantiateClosure typeReference definitionArguments

                let captureType =
                    methodDeclaration.Parameters.[expression.CaptureParameterIndex].Type
                    |> methodTypeParametersToTypeParameters

                let dataType =
                    expression.DataType
                    |> methodTypeParametersToTypeParameters

                let stateMachineType =
                    expression.StateMachineType
                    |> methodTypeParametersToTypeParameters

                let captureFieldReference = {
                    DeclaringType = CliDeclaringType definitionType
                    Name = expression.CaptureName
                    FieldType = captureType
                    TargetStableId =
                        Some(
                            stableId
                            + "/field:"
                            + expression.CaptureName
                        )
                }

                let dataFieldReference, resultFieldReference =
                    match expression.Body with
                    | TypedStoreCapturedResult(dataFieldName, resultFieldName, resultFieldStableId) ->
                        Some {
                            DeclaringType = CliDeclaringType stateMachineType
                            Name = dataFieldName
                            FieldType = CliTypeParameter 0
                            TargetStableId = None
                        },
                        Some {
                            DeclaringType = CliDeclaringType dataType
                            Name = resultFieldName
                            FieldType = captureType
                            TargetStableId = Some resultFieldStableId
                        }
                    | TypedInvokeCapturedUnitFunction -> None, None

                {|
                    StableId = stableId
                    Name = name
                    CaptureFieldStableId =
                        stableId
                        + "/field:"
                        + expression.CaptureName
                    ConstructorStableId =
                        stableId
                        + "/constructor"
                    InvokeStableId =
                        stableId
                        + "/method:Invoke"
                    MethodType = methodType
                    CaptureType = captureType
                    StateMachineType = stateMachineType
                    CaptureFieldReference = captureFieldReference
                    DataFieldReference = dataFieldReference
                    ResultFieldReference = resultFieldReference
                |}

            let boundMemberClosureLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedBoundInstanceMethodExpression)
                =
                let stableId =
                    methodDeclaration.StableId
                    + "/closure:bound-instance-member"

                let name =
                    methodDeclaration.Name
                    + "@"
                    + expression.SourceLine.ToString(CultureInfo.InvariantCulture)

                let typeReference = {
                    DeclarationId = stableId
                    AssemblyName = String.Empty
                    TypeName = {
                        Namespace = String.Empty
                        Name =
                            if List.isEmpty methodDeclaration.GenericParameters then
                                name
                            else
                                name
                                + "`"
                                + methodDeclaration.GenericParameters.Length.ToString(
                                    CultureInfo.InvariantCulture
                                )
                    }
                    IsValueType = false
                }

                let methodArguments =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index _ -> CliMethodTypeParameter index)

                let definitionArguments =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index _ -> CliTypeParameter index)

                let methodType = instantiateClosure typeReference methodArguments
                let definitionType = instantiateClosure typeReference definitionArguments

                let captureType =
                    expression.ReceiverType
                    |> methodTypeParametersToTypeParameters

                let domainType =
                    expression.DomainType
                    |> methodTypeParametersToTypeParameters

                let rangeType =
                    expression.RangeType
                    |> methodTypeParametersToTypeParameters

                let captureFieldStableId =
                    stableId
                    + "/field:receiver"

                {|
                    StableId = stableId
                    Name = name
                    MethodType = methodType
                    DefinitionType = definitionType
                    CaptureType = captureType
                    DomainType = domainType
                    RangeType = rangeType
                    CaptureFieldStableId = captureFieldStableId
                    CaptureFieldReference = {
                        DeclaringType = CliDeclaringType definitionType
                        Name = "receiver"
                        FieldType = captureType
                        TargetStableId = Some captureFieldStableId
                    }
                    ConstructorStableId =
                        stableId
                        + "/constructor"
                    InvokeStableId =
                        stableId
                        + "/method:Invoke"
                |}

            let unitLambdaClosureLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedUnitLambdaExpression)
                =
                let stableId =
                    methodDeclaration.StableId
                    + "/closure:unit-lambda"

                let name =
                    methodDeclaration.Name
                    + "@"
                    + expression.SourceLine.ToString(CultureInfo.InvariantCulture)

                let typeReference = {
                    DeclarationId = stableId
                    AssemblyName = String.Empty
                    TypeName = {
                        Namespace = String.Empty
                        Name =
                            if List.isEmpty methodDeclaration.GenericParameters then
                                name
                            else
                                name
                                + "`"
                                + methodDeclaration.GenericParameters.Length.ToString(
                                    CultureInfo.InvariantCulture
                                )
                    }
                    IsValueType = false
                }

                let methodArguments =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index _ -> CliMethodTypeParameter index)

                let definitionArguments =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index _ -> CliTypeParameter index)

                let methodType = instantiateClosure typeReference methodArguments
                let definitionType = instantiateClosure typeReference definitionArguments

                let captureType =
                    expression.CaptureType
                    |> methodTypeParametersToTypeParameters

                let domainType =
                    expression.DomainType
                    |> methodTypeParametersToTypeParameters

                let rangeType =
                    expression.RangeType
                    |> methodTypeParametersToTypeParameters

                let captureFieldStableId =
                    stableId
                    + "/field:"
                    + expression.CaptureName

                {|
                    StableId = stableId
                    Name = name
                    MethodType = methodType
                    DefinitionType = definitionType
                    CaptureType = captureType
                    DomainType = domainType
                    RangeType = rangeType
                    CaptureFieldStableId = captureFieldStableId
                    CaptureFieldReference = {
                        DeclaringType = CliDeclaringType definitionType
                        Name = expression.CaptureName
                        FieldType = captureType
                        TargetStableId = Some captureFieldStableId
                    }
                    ConstructorStableId =
                        stableId
                        + "/constructor"
                    InvokeStableId =
                        stableId
                        + "/method:Invoke"
                |}

            let methodArgumentIndex kind parameterIndex =
                match kind with
                | InstanceConstructor
                | InstanceInlineMember
                | InternalInstanceInlineMember
                | ObjectExpressionOverride
                | TypeExtensionMember ->
                    parameterIndex
                    + 1
                | ModuleFunction
                | StaticInlineMemberStub
                | ClosureConstructor
                | ClosureInvoke -> parameterIndex

            let objectExpressionConstructorStableId (typeReference: CliTypeReference) =
                typeReference.DeclarationId
                + "/constructor:unit"

            let objectExpressionConstructorReference (typeReference: CliTypeReference) = {
                DeclaringType = CliDeclaringType(CliNamedType typeReference)
                Name = ".ctor"
                GenericArity = 0
                IsInstance = true
                ParameterTypes = []
                ReturnType = CliVoid
                TargetStableId = Some(objectExpressionConstructorStableId typeReference)
            }

            let resumableCodeConstructionInstructions
                kind
                (methodDeclaration: TypedMethodDeclaration)
                expression
                =
                let layout = closureLayout methodDeclaration expression

                let constructor = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ layout.CaptureType ]
                    ReturnType = CliVoid
                    TargetStableId = Some layout.ConstructorStableId
                }

                let invoke = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = "Invoke"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ CliByRef layout.StateMachineType ]
                    ReturnType = CliBoolean
                    TargetStableId = Some layout.InvokeStableId
                }

                let delegateConstructor = {
                    DeclaringType = CliDeclaringType expression.DelegateType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [
                        CliObject
                        CliNativeInt
                    ]
                    ReturnType = CliVoid
                    TargetStableId = None
                }

                [
                    LoadArgument(methodArgumentIndex kind expression.CaptureParameterIndex)
                    NewObject constructor
                    LoadFunctionPointer invoke
                    NewObject delegateConstructor
                ]

            let boundInstanceMethodConstructionInstructions
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedBoundInstanceMethodExpression)
                =
                let layout = boundMemberClosureLayout methodDeclaration expression

                let constructor = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ layout.CaptureType ]
                    ReturnType = CliVoid
                    TargetStableId = Some layout.ConstructorStableId
                }

                let invoke = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = "Invoke"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ layout.DomainType ]
                    ReturnType = layout.RangeType
                    TargetStableId = Some layout.InvokeStableId
                }

                let delegateConstructor = {
                    DeclaringType = CliDeclaringType expression.DelegateType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [
                        CliObject
                        CliNativeInt
                    ]
                    ReturnType = CliVoid
                    TargetStableId = None
                }

                let fromConverter =
                    match expression.FunctionType, expression.DelegateType with
                    | CliGenericType(functionReference, _), CliGenericType(delegateReference, _) -> {
                        DeclaringType = CliDeclaringType expression.FunctionType
                        Name = "FromConverter"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            CliGenericType(
                                delegateReference,
                                [
                                    CliTypeParameter 0
                                    CliTypeParameter 1
                                ]
                            )
                        ]
                        ReturnType =
                            CliGenericType(
                                functionReference,
                                [
                                    CliTypeParameter 0
                                    CliTypeParameter 1
                                ]
                            )
                        TargetStableId = None
                      }
                    | _ ->
                        invalidOp
                            "bound member conversion requires generic F# function and converter types"

                [
                    LoadArgument 0
                    NewObject constructor
                    LoadFunctionPointer invoke
                    NewObject delegateConstructor
                    CallMethod fromConverter
                ]

            let unitLambdaConstructionInstructions
                kind
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedUnitLambdaExpression)
                =
                let layout = unitLambdaClosureLayout methodDeclaration expression

                let constructor = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ layout.CaptureType ]
                    ReturnType = CliVoid
                    TargetStableId = Some layout.ConstructorStableId
                }

                let invoke = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = "Invoke"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ layout.DomainType ]
                    ReturnType = layout.RangeType
                    TargetStableId = Some layout.InvokeStableId
                }

                let delegateConstructor = {
                    DeclaringType = CliDeclaringType expression.DelegateType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [
                        CliObject
                        CliNativeInt
                    ]
                    ReturnType = CliVoid
                    TargetStableId = None
                }

                let fromConverter =
                    match expression.FunctionType, expression.DelegateType with
                    | CliGenericType(functionReference, _), CliGenericType(delegateReference, _) -> {
                        DeclaringType = CliDeclaringType expression.FunctionType
                        Name = "FromConverter"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            CliGenericType(
                                delegateReference,
                                [
                                    CliTypeParameter 0
                                    CliTypeParameter 1
                                ]
                            )
                        ]
                        ReturnType =
                            CliGenericType(
                                functionReference,
                                [
                                    CliTypeParameter 0
                                    CliTypeParameter 1
                                ]
                            )
                        TargetStableId = None
                      }
                    | _ ->
                        invalidOp
                            "unit lambda conversion requires generic F# function and converter types"

                [
                    LoadArgument(methodArgumentIndex kind expression.CaptureParameterIndex)
                    NewObject constructor
                    LoadFunctionPointer invoke
                    NewObject delegateConstructor
                    CallMethod fromConverter
                ]

            let rec valueExpressionInstructions freshLabel kind =
                function
                | TypedIntegerLiteral value -> [ LoadInt32 value ], []
                | TypedStringLiteral value -> [ LoadString value ], []
                | TypedUnitLiteral -> [], []
                | TypedParameterReference index ->
                    [ LoadArgument(methodArgumentIndex kind index) ], []
                | TypedLocalReference index -> [ LoadLocal index ], []
                | TypedLocalAssignment(localIndex, _, value) ->
                    let valueInstructions, valueLocals =
                        valueExpressionInstructions freshLabel kind value

                    valueInstructions
                    @ [ StoreLocal localIndex ],
                    valueLocals
                | TypedAddressOf(source, fields) ->
                    let sourceInstruction =
                        match source with
                        | TypedParameterAddress(index, CliByRef _) ->
                            LoadArgument(methodArgumentIndex kind index)
                        | TypedParameterAddress(index, _) ->
                            LoadArgumentAddress(methodArgumentIndex kind index)
                        | TypedLocalAddress(index, CliByRef _) -> LoadLocal index
                        | TypedLocalAddress(index, _) -> LoadLocalAddress index

                    sourceInstruction
                    :: (fields
                        |> List.map (fun field ->
                            LoadFieldAddress {
                                DeclaringType = CliDeclaringType field.DeclaringType
                                Name = field.Name
                                FieldType = field.FieldType
                                TargetStableId = field.TargetStableId
                            }
                        )),
                    []
                | TypedBooleanNegation(expression, range) ->
                    let expressionInstructions, expressionLocals =
                        valueExpressionInstructions freshLabel kind expression

                    [ MarkSequencePoint range ]
                    @ expressionInstructions
                    @ [
                        LoadInt32 0
                        CompareEqual
                        MarkHiddenSequencePoint
                    ],
                    expressionLocals
                | TypedLet(localIndex, name, _, localType, value, body, bindingRange, bodyRange) ->
                    let valueInstructions, valueLocals =
                        valueExpressionInstructions freshLabel kind value

                    let bodyInstructions, bodyLocals =
                        valueExpressionInstructions freshLabel kind body

                    let bodySequencePoint =
                        match body with
                        | TypedSequential _ -> []
                        | _ -> [ MarkSequencePoint bodyRange ]

                    [ MarkSequencePoint bindingRange ]
                    @ valueInstructions
                    @ [ StoreLocal localIndex ]
                    @ bodySequencePoint
                    @ bodyInstructions,
                    valueLocals
                    @ [
                        {
                            Index = localIndex
                            Name = name
                            Type = localType
                        }
                    ]
                    @ bodyLocals
                | TypedStaticMethodCall(target, genericArguments, arguments) ->
                    let loweredArguments =
                        arguments
                        |> List.map (valueExpressionInstructions freshLabel kind)

                    let methodReference = {
                        DeclaringType = CliDeclaringType(CliNamedType target.DeclaringType)
                        Name = target.Name
                        GenericArity = target.GenericArity
                        IsInstance = false
                        ParameterTypes = target.ParameterTypes
                        ReturnType = target.ReturnType
                        TargetStableId = Some target.StableId
                    }

                    let callInstruction =
                        if List.isEmpty genericArguments then
                            CallMethod methodReference
                        else
                            CallGenericMethod(methodReference, genericArguments)

                    (loweredArguments
                     |> List.collect fst)
                    @ [ callInstruction ],
                    (loweredArguments
                     |> List.collect snd)
                | TypedFunctionApplication(functionType,
                                           domainType,
                                           rangeType,
                                           functionExpression,
                                           argumentExpression) ->
                    let functionInstructions, functionLocals =
                        valueExpressionInstructions freshLabel kind functionExpression

                    let argumentInstructions, argumentLocals =
                        valueExpressionInstructions freshLabel kind argumentExpression

                    let invoke = {
                        DeclaringType = CliDeclaringType functionType
                        Name = "Invoke"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = [ CliTypeParameter 0 ]
                        ReturnType = CliTypeParameter 1
                        TargetStableId = None
                    }

                    functionInstructions
                    @ argumentInstructions
                    @ [ CallVirtualMethod invoke ],
                    functionLocals
                    @ argumentLocals
                | TypedInstanceMethodCall(target, receiver, arguments) ->
                    let receiverInstructions, receiverLocals =
                        valueExpressionInstructions freshLabel kind receiver

                    let loweredArguments =
                        arguments
                        |> List.map (valueExpressionInstructions freshLabel kind)

                    let methodReference = {
                        DeclaringType = CliDeclaringType target.DeclaringType
                        Name = target.Name
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = target.ParameterTypes
                        ReturnType = target.ReturnType
                        TargetStableId = None
                    }

                    receiverInstructions
                    @ (loweredArguments
                       |> List.collect fst)
                    @ [ CallVirtualMethod methodReference ],
                    receiverLocals
                    @ (loweredArguments
                       |> List.collect snd)
                | TypedUpcast(sourceType, _, _, expression) ->
                    let expressionInstructions, expressionLocals =
                        valueExpressionInstructions freshLabel kind expression

                    expressionInstructions
                    @ [ Box sourceType ],
                    expressionLocals
                | TypedSequential expressions ->
                    let expressionCount = expressions.Length

                    let loweredExpressions =
                        expressions
                        |> List.mapi (fun index (expression, expressionType, expressionRange) ->
                            let instructions, locals =
                                valueExpressionInstructions freshLabel kind expression

                            let sequencePoint =
                                match expression with
                                | TypedConditional _
                                | TypedBooleanNegation _
                                | TypedLet _
                                | TypedSequential _ -> []
                                | _ -> [ MarkSequencePoint expressionRange ]

                            sequencePoint
                            @ instructions
                            @ (if
                                   index < expressionCount
                                           - 1
                                   && expressionType
                                      <> CliVoid
                               then
                                   [ Pop ]
                               else
                                   []),
                            locals
                        )

                    loweredExpressions
                    |> List.collect fst,
                    loweredExpressions
                    |> List.collect snd
                | TypedConditional(condition,
                                   ifTrue,
                                   ifFalse,
                                   conditionRange,
                                   ifTrueRange,
                                   ifFalseRange) ->
                    let falseLabel = freshLabel ()
                    let endLabel = freshLabel ()

                    let conditionInstructions, conditionLocals =
                        valueExpressionInstructions freshLabel kind condition

                    let ifTrueInstructions, ifTrueLocals =
                        valueExpressionInstructions freshLabel kind ifTrue

                    let ifFalseInstructions, ifFalseLocals =
                        valueExpressionInstructions freshLabel kind ifFalse

                    let ifTrueSequencePoint =
                        match ifTrue with
                        | TypedSequential _
                        | TypedUnitLiteral -> []
                        | _ -> [ MarkSequencePoint ifTrueRange ]

                    let ifFalseSequencePoint =
                        match ifFalse with
                        | TypedSequential _
                        | TypedUnitLiteral -> []
                        | _ -> [ MarkSequencePoint ifFalseRange ]

                    let joinInstructions =
                        match ifFalse with
                        | TypedUnitLiteral -> [
                            MarkHiddenSequencePoint
                            Branch endLabel
                            MarkLabel falseLabel
                            MarkHiddenSequencePoint
                            Nop
                            MarkLabel endLabel
                          ]
                        | _ ->
                            [
                                Branch endLabel
                                MarkLabel falseLabel
                            ]
                            @ ifFalseSequencePoint
                            @ ifFalseInstructions
                            @ [ MarkLabel endLabel ]

                    [
                        MarkSequencePoint conditionRange
                        Nop
                        MarkHiddenSequencePoint
                    ]
                    @ conditionInstructions
                    @ [ BranchIfFalse falseLabel ]
                    @ ifTrueSequencePoint
                    @ ifTrueInstructions
                    @ joinInstructions,
                    conditionLocals
                    @ ifTrueLocals
                    @ ifFalseLocals
                | TypedTypeTestMatch(input,
                                     targetType,
                                     localIndex,
                                     bindingName,
                                     ifMatched,
                                     ifNotMatched,
                                     matchHeaderRange,
                                     ifMatchedRange,
                                     ifNotMatchedRange,
                                     _) ->
                    let fallbackLabel = freshLabel ()
                    let endLabel = freshLabel ()

                    let inputInstructions, inputLocals =
                        valueExpressionInstructions freshLabel kind input

                    let matchedInstructions, matchedLocals =
                        valueExpressionInstructions freshLabel kind ifMatched

                    let notMatchedInstructions, notMatchedLocals =
                        valueExpressionInstructions freshLabel kind ifNotMatched

                    [
                        MarkSequencePoint matchHeaderRange
                        Nop
                        MarkHiddenSequencePoint
                    ]
                    @ inputInstructions
                    @ [
                        IsInstance targetType
                        StoreLocal localIndex
                        LoadLocal localIndex
                        BranchIfFalse fallbackLabel
                        MarkHiddenSequencePoint
                        Nop
                        MarkSequencePoint ifMatchedRange
                    ]
                    @ matchedInstructions
                    @ [
                        Branch endLabel
                        MarkLabel fallbackLabel
                        MarkSequencePoint ifNotMatchedRange
                    ]
                    @ notMatchedInstructions
                    @ [ MarkLabel endLabel ],
                    inputLocals
                    @ [
                        {
                            Index = localIndex
                            Name = bindingName
                            Type = targetType
                        }
                    ]
                    @ matchedLocals
                    @ notMatchedLocals
                | TypedObjectExpression(typeReference, _, constructorArguments, _, _, _, _, _, _) ->
                    if not (List.isEmpty constructorArguments) then
                        invalidOp
                            "object-expression constructor arguments reached an unsupported lowering path"

                    [ NewObject(objectExpressionConstructorReference typeReference) ], []
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedResumableCode _
                | TypedResumableTryFinally _
                | TypedTraitCall _ -> invalidOp "this expression cannot be lowered as a local value"

            let methodInstructions kind (methodDeclaration: TypedMethodDeclaration) =
                let mutable nextLabel = 0

                let freshLabel () =
                    let label = nextLabel

                    nextLabel <-
                        nextLabel
                        + 1

                    label

                match methodDeclaration.Body with
                | TypedIntegerLiteral value ->
                    [
                        LoadInt32 value
                        Return
                    ],
                    []
                | TypedParameterReference index ->
                    [
                        LoadArgument(methodArgumentIndex kind index)
                        Return
                    ],
                    []
                | (TypedStringLiteral _ | TypedUnitLiteral | TypedLocalReference _ | TypedLet _ | TypedLocalAssignment _ | TypedAddressOf _ | TypedStaticMethodCall _ | TypedFunctionApplication _ | TypedInstanceMethodCall _ | TypedConditional _ | TypedUpcast _ | TypedSequential _ | TypedBooleanNegation _ | TypedTypeTestMatch _ | TypedObjectExpression _) as expression ->
                    let instructions, locals =
                        valueExpressionInstructions freshLabel kind expression

                    instructions
                    @ [ Return ],
                    locals
                | TypedResumableCode expression ->
                    resumableCodeConstructionInstructions kind methodDeclaration expression
                    @ [ Return ],
                    []
                | TypedBoundInstanceMethod expression ->
                    boundInstanceMethodConstructionInstructions methodDeclaration expression
                    @ [ Return ],
                    []
                | TypedUnitLambda expression ->
                    unitLambdaConstructionInstructions kind methodDeclaration expression
                    @ [ Return ],
                    []
                | TypedResumableTryFinally expression ->
                    match expression.DelegateType with
                    | CliGenericType(delegateReference, _) ->
                        let unitType =
                            CliNamedType {
                                DeclarationId =
                                    "reference:FSharp.Core/type:Microsoft.FSharp.Core.Unit`0"
                                AssemblyName = "FSharp.Core"
                                TypeName = {
                                    Namespace = "Microsoft.FSharp.Core"
                                    Name = "Unit"
                                }
                                IsValueType = false
                            }

                        let referencedDataType = CliMethodTypeParameter 0
                        let referencedResultType = CliMethodTypeParameter 1

                        let referencedDelegateType resultType =
                            CliGenericType(
                                delegateReference,
                                [
                                    referencedDataType
                                    resultType
                                ]
                            )

                        let tryFinally = {
                            DeclaringType = CliDeclaringType expression.ResumableCodeModuleType
                            Name = "TryFinally"
                            GenericArity = 2
                            IsInstance = false
                            ParameterTypes = [
                                referencedDelegateType referencedResultType
                                referencedDelegateType unitType
                            ]
                            ReturnType = referencedDelegateType referencedResultType
                            TargetStableId = None
                        }

                        [
                            LoadArgument(
                                methodArgumentIndex kind expression.ComputationParameterIndex
                            )
                        ]
                        @ resumableCodeConstructionInstructions
                            kind
                            methodDeclaration
                            expression.Compensation
                        @ [
                            CallGenericMethod(
                                tryFinally,
                                [
                                    expression.DataType
                                    expression.ResultType
                                ]
                            )
                            Return
                        ],
                        []
                    | _ -> invalidOp "the TryFinally delegate type must be generic"
                | TypedTraitCall(_, memberName, _) ->
                    [
                        LoadString(
                            "Dynamic invocation of "
                            + memberName
                            + " is not supported"
                        )
                        NewObject(
                            {
                                DeclaringType =
                                    CoreDeclaringType {
                                        Namespace = "System"
                                        Name = "NotSupportedException"
                                    }
                                Name = ".ctor"
                                GenericArity = 0
                                IsInstance = true
                                ParameterTypes = [ CliString ]
                                ReturnType = CliVoid
                                TargetStableId = None
                            }
                        )
                        Throw
                    ],
                    []

            let rec constraintTypeDependencyIds =
                function
                | TypedNamedType resolvedType -> [ resolvedType.DeclarationId ]
                | TypedTypeParameter _ -> []
                | TypedGenericTypeApplication(genericType, arguments) ->
                    constraintTypeDependencyIds genericType
                    @ (arguments
                       |> List.collect constraintTypeDependencyIds)
                | TypedByRefType elementType -> constraintTypeDependencyIds elementType
                | TypedTupleType elements ->
                    elements
                    |> List.collect constraintTypeDependencyIds
                | TypedFunctionType(domain, range) ->
                    constraintTypeDependencyIds domain
                    @ constraintTypeDependencyIds range

            let methodDependencies (methodDeclaration: TypedMethodDeclaration) =
                methodDeclaration.Constraints
                |> List.collect (
                    function
                    | TypedAbbreviationConstraint typeExpression ->
                        constraintTypeDependencyIds typeExpression
                    | TypedDirectConstraint(TypedSubtypeConstraint(_, superType)) ->
                        constraintTypeDependencyIds superType
                    | TypedDirectConstraint(TypedMemberConstraint _) -> []
                )

            let rec constraintCliType methodParameterIndex =
                function
                | TypedTypeParameter name ->
                    match
                        methodParameterIndex
                        |> Map.tryFind name
                    with
                    | Some index -> CliMethodTypeParameter index
                    | None ->
                        invalidOp (
                            "the constrained method type parameter '"
                            + name
                            + "' has no generic-parameter index"
                        )
                | TypedNamedType resolvedType ->
                    CliNamedType {
                        DeclarationId = resolvedType.DeclarationId
                        AssemblyName = resolvedType.AssemblyName
                        TypeName = resolvedType.TypeName
                        IsValueType = resolvedType.IsValueType
                    }
                | TypedGenericTypeApplication(TypedNamedType resolvedType, arguments) ->
                    CliGenericType(
                        {
                            DeclarationId = resolvedType.DeclarationId
                            AssemblyName = resolvedType.AssemblyName
                            TypeName = {
                                Namespace = resolvedType.TypeName.Namespace
                                Name =
                                    resolvedType.TypeName.Name
                                    + "`"
                                    + arguments.Length.ToString(CultureInfo.InvariantCulture)
                            }
                            IsValueType = resolvedType.IsValueType
                        },
                        arguments
                        |> List.map (constraintCliType methodParameterIndex)
                    )
                | TypedGenericTypeApplication _
                | TypedByRefType _
                | TypedTupleType _
                | TypedFunctionType _ ->
                    invalidOp "the CLR generic constraint type is not supported"

            let genericParameterConstraints (methodDeclaration: TypedMethodDeclaration) =
                let methodParameterIndex =
                    methodDeclaration.GenericParameters
                    |> List.mapi (fun index name -> name, index)
                    |> Map.ofList

                methodDeclaration.Constraints
                |> List.choose (
                    function
                    | TypedDirectConstraint(TypedSubtypeConstraint(parameterName, superType)) ->
                        methodParameterIndex
                        |> Map.tryFind parameterName
                        |> Option.map (fun index ->
                            index, constraintCliType methodParameterIndex superType
                        )
                    | TypedAbbreviationConstraint _
                    | TypedDirectConstraint(TypedMemberConstraint _) -> None
                )

            let rec cliTypeDependencyIds =
                function
                | CliNamedType reference -> [ reference.DeclarationId ]
                | CliGenericType(reference, arguments) ->
                    reference.DeclarationId
                    :: (arguments
                        |> List.collect cliTypeDependencyIds)
                | CliByRef elementType -> cliTypeDependencyIds elementType
                | CliInt32
                | CliBoolean
                | CliString
                | CliObject
                | CliNativeInt
                | CliVoid
                | CliTypeParameter _
                | CliMethodTypeParameter _ -> []

            let methodFragment
                kind
                documentIndex
                documentChecksum
                fragmentStableId
                contentHash
                (methodDeclaration: TypedMethodDeclaration)
                =
                let instructions, locals = methodInstructions kind methodDeclaration

                let instructionDependencies =
                    instructions
                    |> List.collect (
                        function
                        | LoadField fieldReference
                        | LoadFieldAddress fieldReference
                        | StoreField fieldReference -> [ fieldReference.DependencyId ]
                        | CallMethod methodReference
                        | CallVirtualMethod methodReference
                        | LoadFunctionPointer methodReference
                        | NewObject methodReference -> [ methodReference.DependencyId ]
                        | CallGenericMethod(methodReference, genericArguments) ->
                            methodReference.DependencyId
                            :: (genericArguments
                                |> List.collect cliTypeDependencyIds)
                        | Box cliType -> cliTypeDependencyIds cliType
                        | IsInstance cliType -> cliTypeDependencyIds cliType
                        | MarkHiddenSequencePoint
                        | MarkLabel _
                        | BranchIfFalse _
                        | Branch _
                        | Nop
                        | CompareEqual
                        | LoadInt32 _
                        | LoadString _
                        | LoadNull
                        | LoadArgument _
                        | LoadArgumentAddress _
                        | LoadLocal _
                        | LoadLocalAddress _
                        | StoreLocal _
                        | MarkSequencePoint _
                        | Pop
                        | Throw
                        | Return -> []
                    )

                {
                    SchemaVersion = querySchema
                    StableId = fragmentStableId
                    Name = methodDeclaration.Name
                    Kind = kind
                    GenericParameters = methodDeclaration.GenericParameters
                    Constraints = methodDeclaration.Constraints
                    GenericParameterConstraints = genericParameterConstraints methodDeclaration
                    Attributes =
                        methodDeclaration.Attributes
                        |> List.map customAttributeFragment
                    Parameters =
                        methodDeclaration.Parameters
                        |> List.map parameterFragment
                    Locals = locals
                    ReturnType = methodDeclaration.ReturnType
                    Instructions = instructions
                    EmitDefaultSequencePoint =
                        match methodDeclaration.Body with
                        | TypedUnitLambda _ -> false
                        | _ -> true
                    MaxStack =
                        match methodDeclaration.Body with
                        | TypedResumableCode _
                        | TypedBoundInstanceMethod _
                        | TypedUnitLambda _
                        | TypedResumableTryFinally _ -> 8
                        | TypedIntegerLiteral _
                        | TypedStringLiteral _
                        | TypedUnitLiteral
                        | TypedParameterReference _
                        | TypedLocalReference _
                        | TypedLet _
                        | TypedAddressOf _
                        | TypedObjectExpression _
                        | TypedTraitCall _ -> 1
                        | TypedStaticMethodCall _
                        | TypedFunctionApplication _
                        | TypedInstanceMethodCall _
                        | TypedConditional _
                        | TypedUpcast _
                        | TypedSequential _
                        | TypedLocalAssignment _
                        | TypedBooleanNegation _
                        | TypedTypeTestMatch _ -> 8
                    DependencyIds =
                        methodDependencies methodDeclaration
                        @ instructionDependencies
                        |> List.distinct
                    ContentHash = contentHash
                    DocumentIndex = documentIndex
                    DocumentChecksum = documentChecksum
                    Range = methodDeclaration.Range
                }

            let moduleTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let typeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let literalFields =
                        declarationsWithContentHashes
                        |> List.choose (fun (declaration, contentHash) ->
                            match declaration with
                            | TypedLiteralField fieldDeclaration ->
                                Some {
                                    SchemaVersion = querySchema
                                    StableId =
                                        typeStableId
                                        + "/field:"
                                        + fieldDeclaration.StableId
                                    Name = fieldDeclaration.Name
                                    Value = fieldDeclaration.Value
                                    ContentHash = contentHash
                                }
                            | TypedMethod _
                            | TypedTypeAbbreviation _
                            | TypedStaticType _
                            | TypedObjectType _
                            | TypedStructType _ -> None
                        )

                    let methods =
                        declarationsWithContentHashes
                        |> List.choose (fun (declaration, contentHash) ->
                            match declaration with
                            | TypedMethod methodDeclaration ->
                                methodFragment
                                    ModuleFunction
                                    documentIndex
                                    typed.SourceChecksum
                                    (typeStableId
                                     + "/method:"
                                     + methodDeclaration.StableId)
                                    contentHash
                                    methodDeclaration
                                |> Some
                            | TypedLiteralField _
                            | TypedTypeAbbreviation _
                            | TypedStaticType _
                            | TypedObjectType _
                            | TypedStructType _ -> None
                        )

                    let containsNestedType =
                        typed.ContainerKind = ModuleSource
                        && (declarationsWithContentHashes
                            |> List.exists (
                                fst
                                >> function
                                    | TypedObjectType _
                                    | TypedStructType _ -> true
                                    | _ -> false
                            ))

                    let customAttributes =
                        if
                            List.isEmpty typed.Attributes
                            && not containsNestedType
                        then
                            []
                        else
                            [
                                yield!
                                    typed.Attributes
                                    |> List.map customAttributeFragment
                                compilationMappingAttribute typeStableId ModuleConstruct
                            ]

                    if
                        List.isEmpty literalFields
                        && List.isEmpty methods
                        && not containsNestedType
                        && List.isEmpty customAttributes
                    then
                        None
                    else
                        Some {
                            SchemaVersion = querySchema
                            StableId = typeStableId
                            Namespace = typed.Namespace
                            Name = typed.Name
                            IsPublic = typed.IsPublic
                            EnclosingTypeStableId = None
                            Kind = ModuleContainer
                            GenericParameters = []
                            Attributes = customAttributes
                            LiteralFields = literalFields
                            InstanceFields = []
                            Methods = methods
                        }
                )
                |> List.choose id

            let staticTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, typeContentHash) ->
                        match declaration with
                        | TypedStaticType typeDeclaration ->
                            let methods =
                                typeDeclaration.Methods
                                |> List.map (fun methodDeclaration ->
                                    let contentHash =
                                        Fingerprint.parts [
                                            typeContentHash
                                            methodDeclaration.ExportFingerprint
                                        ]

                                    methodFragment
                                        StaticInlineMemberStub
                                        documentIndex
                                        typed.SourceChecksum
                                        methodDeclaration.StableId
                                        contentHash
                                        methodDeclaration
                                )

                            Some {
                                SchemaVersion = querySchema
                                StableId = typeDeclaration.StableId
                                Namespace = typed.Namespace
                                Name = typeDeclaration.Name
                                IsPublic = true
                                EnclosingTypeStableId = None
                                Kind = StaticMemberContainer
                                GenericParameters = []
                                Attributes = []
                                LiteralFields = []
                                InstanceFields = []
                                Methods = methods
                            }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedObjectType _
                        | TypedStructType _ -> None
                    )
                )
                |> List.collect id

            let rec objectExpressions =
                function
                | (TypedObjectExpression(_, _, constructorArguments, _, _, _, _, body, _) as expression) -> [
                    yield expression

                    for argument in constructorArguments do
                        yield! objectExpressions argument

                    yield! objectExpressions body
                  ]
                | TypedLet(_, _, _, _, value, body, _, _) ->
                    objectExpressions value
                    @ objectExpressions body
                | TypedLocalAssignment(_, _, value)
                | TypedBooleanNegation(value, _)
                | TypedUpcast(_, _, _, value) -> objectExpressions value
                | TypedStaticMethodCall(_, _, arguments) ->
                    arguments
                    |> List.collect objectExpressions
                | TypedFunctionApplication(_, _, _, functionExpression, argumentExpression) ->
                    objectExpressions functionExpression
                    @ objectExpressions argumentExpression
                | TypedInstanceMethodCall(_, receiver, arguments) ->
                    objectExpressions receiver
                    @ (arguments
                       |> List.collect objectExpressions)
                | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                    objectExpressions condition
                    @ objectExpressions ifTrue
                    @ objectExpressions ifFalse
                | TypedTypeTestMatch(input,
                                     _,
                                     _,
                                     _,
                                     ifMatched,
                                     ifNotMatched,
                                     _,
                                     _,
                                     _,
                                     _) ->
                    objectExpressions input
                    @ objectExpressions ifMatched
                    @ objectExpressions ifNotMatched
                | TypedSequential expressions ->
                    expressions
                    |> List.collect (fun (expression, _, _) -> objectExpressions expression)
                | TypedIntegerLiteral _
                | TypedStringLiteral _
                | TypedUnitLiteral
                | TypedParameterReference _
                | TypedLocalReference _
                | TypedAddressOf _
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedResumableCode _
                | TypedResumableTryFinally _
                | TypedTraitCall _ -> []

            let objectTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested = typed.ContainerKind = ModuleSource

                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, typeContentHash) ->
                        match declaration with
                        | TypedObjectType typeDeclaration ->
                            match typeDeclaration.Container with
                            | TypedExtensionModule(moduleName, attributes, extendedType) ->
                                let methods =
                                    typeDeclaration.Methods
                                    |> List.choose (fun objectMethodDeclaration ->
                                        match objectMethodDeclaration with
                                        | TypedInstanceObjectMethod(receiverName,
                                                                    methodDeclaration) ->
                                            let extensionMethod = {
                                                methodDeclaration with
                                                    Name =
                                                        typeDeclaration.Name
                                                        + "."
                                                        + methodDeclaration.Name
                                                    Parameters =
                                                        {
                                                            Name = receiverName
                                                            Type = extendedType
                                                            Attributes = []
                                                        }
                                                        :: methodDeclaration.Parameters
                                                    ExportFingerprint =
                                                        Fingerprint.parts [
                                                            methodDeclaration.ExportFingerprint
                                                            "type-extension"
                                                            TypeIdentity.cliType extendedType
                                                        ]
                                            }

                                            methodFragment
                                                TypeExtensionMember
                                                documentIndex
                                                typed.SourceChecksum
                                                methodDeclaration.StableId
                                                (Fingerprint.parts [
                                                    typeContentHash
                                                    extensionMethod.ExportFingerprint
                                                    methodImplementationHash methodDeclaration
                                                ])
                                                extensionMethod
                                            |> Some
                                        | TypedStaticObjectMethod _ -> None
                                    )

                                Some {
                                    SchemaVersion = querySchema
                                    StableId = typeDeclaration.StableId
                                    Namespace = String.Empty
                                    Name = moduleName
                                    IsPublic = true
                                    EnclosingTypeStableId = Some moduleTypeStableId
                                    Kind = ExtensionModuleContainer
                                    GenericParameters = []
                                    Attributes = [
                                        yield!
                                            attributes
                                            |> List.map customAttributeFragment

                                        compilationMappingAttribute
                                            typeDeclaration.StableId
                                            ModuleConstruct
                                    ]
                                    LiteralFields = []
                                    InstanceFields = []
                                    Methods = methods
                                }
                            | OrdinaryTypedObjectType ->
                                let constructorStableId =
                                    typeDeclaration.StableId
                                    + "/constructor:unit"

                                let objectConstructor = {
                                    DeclaringType =
                                        CoreDeclaringType {
                                            Namespace = "System"
                                            Name = "Object"
                                        }
                                    Name = ".ctor"
                                    GenericArity = 0
                                    IsInstance = true
                                    ParameterTypes = []
                                    ReturnType = CliVoid
                                    TargetStableId = None
                                }

                                let constructor = {
                                    SchemaVersion = querySchema
                                    StableId = constructorStableId
                                    Name = ".ctor"
                                    Kind = InstanceConstructor
                                    GenericParameters = []
                                    Constraints = []
                                    GenericParameterConstraints = []
                                    Attributes = []
                                    Parameters = []
                                    Locals = []
                                    ReturnType = CliVoid
                                    Instructions = [
                                        LoadArgument 0
                                        CallMethod objectConstructor
                                        Return
                                    ]
                                    EmitDefaultSequencePoint = true
                                    MaxStack = 1
                                    DependencyIds = [ objectConstructor.StableId ]
                                    ContentHash =
                                        Fingerprint.parts [
                                            constructorStableId
                                            objectConstructor.StableId
                                        ]
                                    DocumentIndex = documentIndex
                                    DocumentChecksum = typed.SourceChecksum
                                    Range = typeDeclaration.ConstructorRange
                                }

                                let methods =
                                    typeDeclaration.Methods
                                    |> List.map (fun objectMethodDeclaration ->
                                        let kind, methodDeclaration =
                                            match objectMethodDeclaration with
                                            | TypedInstanceObjectMethod(_, methodDeclaration) when
                                                not methodDeclaration.IsPublic
                                                ->
                                                InternalInstanceInlineMember, methodDeclaration
                                            | TypedInstanceObjectMethod(_, methodDeclaration) ->
                                                InstanceInlineMember, methodDeclaration
                                            | TypedStaticObjectMethod methodDeclaration ->
                                                StaticInlineMemberStub, methodDeclaration

                                        methodFragment
                                            kind
                                            documentIndex
                                            typed.SourceChecksum
                                            methodDeclaration.StableId
                                            (methodImplementationHash methodDeclaration)
                                            methodDeclaration
                                    )

                                Some {
                                    SchemaVersion = querySchema
                                    StableId = typeDeclaration.StableId
                                    Namespace = if isNested then String.Empty else typed.Namespace
                                    Name = typeDeclaration.Name
                                    IsPublic = true
                                    EnclosingTypeStableId =
                                        if isNested then Some moduleTypeStableId else None
                                    Kind = ObjectContainer
                                    GenericParameters = []
                                    Attributes = [
                                        compilationMappingAttribute
                                            typeDeclaration.StableId
                                            ObjectTypeConstruct
                                    ]
                                    LiteralFields = []
                                    InstanceFields = []
                                    Methods =
                                        constructor
                                        :: methods
                                }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> None
                    )
                )
                |> List.collect id

            let objectExpressionTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let ownedMethods =
                        declarationsWithContentHashes
                        |> List.collect (fun (declaration, _) ->
                            match declaration with
                            | TypedMethod methodDeclaration -> [
                                moduleTypeStableId, methodDeclaration
                              ]
                            | TypedStaticType typeDeclaration ->
                                typeDeclaration.Methods
                                |> List.map (fun methodDeclaration ->
                                    typeDeclaration.StableId, methodDeclaration
                                )
                            | TypedObjectType typeDeclaration ->
                                typeDeclaration.Methods
                                |> List.map (fun objectMethodDeclaration ->
                                    typeDeclaration.StableId, objectMethodDeclaration.Method
                                )
                            | TypedLiteralField _
                            | TypedTypeAbbreviation _
                            | TypedStructType _ -> []
                        )

                    ownedMethods
                    |> List.collect (fun (enclosingTypeStableId, ownerMethod) ->
                        ownerMethod.Body
                        |> objectExpressions
                        |> List.map (fun expression ->
                            match expression with
                            | TypedObjectExpression(typeReference,
                                                    baseType,
                                                    constructorArguments,
                                                    isOverride,
                                                    receiverName,
                                                    memberName,
                                                    memberReturnType,
                                                    memberBody,
                                                    range) ->
                                if not (List.isEmpty constructorArguments) then
                                    invalidOp
                                        "object-expression constructor arguments reached symbolic lowering"

                                if not isOverride then
                                    invalidOp "an object-expression member must be an override"

                                let constructorStableId =
                                    objectExpressionConstructorStableId typeReference

                                let baseDeclaringType =
                                    match baseType with
                                    | CliObject ->
                                        CoreDeclaringType {
                                            Namespace = "System"
                                            Name = "Object"
                                        }
                                    | _ -> CliDeclaringType baseType

                                let baseConstructor = {
                                    DeclaringType = baseDeclaringType
                                    Name = ".ctor"
                                    GenericArity = 0
                                    IsInstance = true
                                    ParameterTypes = []
                                    ReturnType = CliVoid
                                    TargetStableId = None
                                }

                                let constructor = {
                                    SchemaVersion = querySchema
                                    StableId = constructorStableId
                                    Name = ".ctor"
                                    Kind = InstanceConstructor
                                    GenericParameters = []
                                    Constraints = []
                                    GenericParameterConstraints = []
                                    Attributes = []
                                    Parameters = []
                                    Locals = []
                                    ReturnType = CliVoid
                                    Instructions = [
                                        LoadArgument 0
                                        CallMethod baseConstructor
                                        Return
                                    ]
                                    EmitDefaultSequencePoint = false
                                    MaxStack = 1
                                    DependencyIds = [ baseConstructor.DependencyId ]
                                    ContentHash =
                                        Fingerprint.parts [
                                            constructorStableId
                                            baseConstructor.StableId
                                        ]
                                    DocumentIndex = documentIndex
                                    DocumentChecksum = typed.SourceChecksum
                                    Range = range
                                }

                                let overrideStableId =
                                    typeReference.DeclarationId
                                    + "/method:"
                                    + memberName

                                let overrideDeclaration = {
                                    StableId = overrideStableId
                                    Name = memberName
                                    IsPublic = true
                                    GenericParameters = []
                                    Constraints = []
                                    Attributes = []
                                    Parameters = []
                                    ReturnType = memberReturnType
                                    Body = memberBody
                                    ExportFingerprint =
                                        Fingerprint.parts [
                                            overrideStableId
                                            "override"
                                            receiverName
                                            TypeIdentity.cliType memberReturnType
                                            TypeIdentity.inlineBody memberBody
                                        ]
                                    Range = range
                                }

                                let overrideMethod =
                                    methodFragment
                                        ObjectExpressionOverride
                                        documentIndex
                                        typed.SourceChecksum
                                        overrideStableId
                                        (methodImplementationHash overrideDeclaration)
                                        overrideDeclaration

                                {
                                    SchemaVersion = querySchema
                                    StableId = typeReference.DeclarationId
                                    Namespace = String.Empty
                                    Name = typeReference.TypeName.Name
                                    IsPublic = false
                                    EnclosingTypeStableId = Some enclosingTypeStableId
                                    Kind = ObjectExpressionContainer
                                    GenericParameters = []
                                    Attributes = []
                                    LiteralFields = []
                                    InstanceFields = []
                                    Methods = [
                                        constructor
                                        overrideMethod
                                    ]
                                }
                            | _ ->
                                invalidOp
                                    "object-expression collection returned a different expression"
                        )
                    )
                )
                |> List.collect id

            let closureTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested = typed.ContainerKind = ModuleSource

                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedObjectType typeDeclaration ->
                            typeDeclaration.Methods
                            |> List.choose (fun objectMethodDeclaration ->
                                let methodDeclaration = objectMethodDeclaration.Method

                                let closureExpression =
                                    match methodDeclaration.Body with
                                    | TypedResumableCode expression -> Some expression
                                    | TypedResumableTryFinally expression ->
                                        Some expression.Compensation
                                    | TypedIntegerLiteral _
                                    | TypedStringLiteral _
                                    | TypedUnitLiteral
                                    | TypedParameterReference _
                                    | TypedLocalReference _
                                    | TypedLet _
                                    | TypedAddressOf _
                                    | TypedStaticMethodCall _
                                    | TypedFunctionApplication _
                                    | TypedInstanceMethodCall _
                                    | TypedBoundInstanceMethod _
                                    | TypedUnitLambda _
                                    | TypedConditional _
                                    | TypedUpcast _
                                    | TypedSequential _
                                    | TypedLocalAssignment _
                                    | TypedBooleanNegation _
                                    | TypedTypeTestMatch _
                                    | TypedObjectExpression _
                                    | TypedTraitCall _ -> None

                                match closureExpression with
                                | Some expression ->
                                    let layout = closureLayout methodDeclaration expression

                                    let objectConstructor = {
                                        DeclaringType =
                                            CoreDeclaringType {
                                                Namespace = "System"
                                                Name = "Object"
                                            }
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let constructor = {
                                        SchemaVersion = querySchema
                                        StableId = layout.ConstructorStableId
                                        Name = ".ctor"
                                        Kind = ClosureConstructor
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = expression.CaptureName
                                                Type = layout.CaptureType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = []
                                        ReturnType = CliVoid
                                        Instructions = [
                                            LoadArgument 0
                                            LoadArgument 1
                                            StoreField layout.CaptureFieldReference
                                            LoadArgument 0
                                            CallMethod objectConstructor
                                            Return
                                        ]
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds = [
                                            layout.CaptureFieldReference.DependencyId
                                            objectConstructor.DependencyId
                                        ]
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.ConstructorStableId
                                                layout.CaptureFieldReference.StableId
                                                objectConstructor.StableId
                                            ]
                                        DocumentIndex = documentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = methodDeclaration.Range
                                    }

                                    let invokeInstructions, invokeDependencies, invokeIdentity =
                                        match
                                            expression.Body,
                                            layout.DataFieldReference,
                                            layout.ResultFieldReference
                                        with
                                        | TypedStoreCapturedResult _,
                                          Some dataFieldReference,
                                          Some resultFieldReference ->
                                            [
                                                LoadArgument 1
                                                LoadFieldAddress dataFieldReference
                                                LoadArgument 0
                                                LoadField layout.CaptureFieldReference
                                                StoreField resultFieldReference
                                                LoadInt32 1
                                                Return
                                            ],
                                            [
                                                dataFieldReference.DependencyId
                                                layout.CaptureFieldReference.DependencyId
                                                resultFieldReference.DependencyId
                                            ],
                                            [
                                                dataFieldReference.StableId
                                                layout.CaptureFieldReference.StableId
                                                resultFieldReference.StableId
                                            ]
                                        | TypedInvokeCapturedUnitFunction, None, None ->
                                            let invokeFunction = {
                                                DeclaringType =
                                                    CliDeclaringType layout.CaptureType
                                                Name = "Invoke"
                                                GenericArity = 0
                                                IsInstance = true
                                                ParameterTypes = [ CliTypeParameter 0 ]
                                                ReturnType = CliTypeParameter 1
                                                TargetStableId = None
                                            }

                                            [
                                                LoadArgument 0
                                                LoadField layout.CaptureFieldReference
                                                LoadNull
                                                CallVirtualMethod invokeFunction
                                                Pop
                                                LoadInt32 1
                                                Return
                                            ],
                                            [
                                                layout.CaptureFieldReference.DependencyId
                                                invokeFunction.DependencyId
                                            ],
                                            [
                                                layout.CaptureFieldReference.StableId
                                                invokeFunction.StableId
                                            ]
                                        | _ ->
                                            invalidOp
                                                "the resumable closure layout does not match its body"

                                    let invoke = {
                                        SchemaVersion = querySchema
                                        StableId = layout.InvokeStableId
                                        Name = "Invoke"
                                        Kind = ClosureInvoke
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = expression.StateMachineParameterName
                                                Type = CliByRef layout.StateMachineType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = []
                                        ReturnType = CliBoolean
                                        Instructions = invokeInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds = invokeDependencies
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.InvokeStableId
                                                methodImplementationHash methodDeclaration
                                                yield! invokeIdentity
                                            ]
                                        DocumentIndex = documentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = methodDeclaration.Range
                                    }

                                    Some {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace =
                                            if isNested then String.Empty else typed.Namespace
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            if isNested then Some moduleTypeStableId else None
                                        Kind = ClosureContainer
                                        GenericParameters = methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = [
                                            {
                                                SchemaVersion = querySchema
                                                StableId = layout.CaptureFieldStableId
                                                Name = expression.CaptureName
                                                Type = layout.CaptureType
                                                Attributes = []
                                                ContentHash =
                                                    Fingerprint.parts [
                                                        layout.CaptureFieldStableId
                                                        TypeIdentity.cliType layout.CaptureType
                                                    ]
                                            }
                                        ]
                                        Methods = [
                                            constructor
                                            invoke
                                        ]
                                    }
                                | None -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> []
                    )
                )
                |> List.collect id

            let boundMemberClosureTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested = typed.ContainerKind = ModuleSource

                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedObjectType typeDeclaration ->
                            typeDeclaration.Methods
                            |> List.choose (fun objectMethodDeclaration ->
                                let methodDeclaration = objectMethodDeclaration.Method

                                match methodDeclaration.Body with
                                | TypedBoundInstanceMethod expression ->
                                    let layout =
                                        boundMemberClosureLayout methodDeclaration expression

                                    let objectConstructor = {
                                        DeclaringType =
                                            CoreDeclaringType {
                                                Namespace = "System"
                                                Name = "Object"
                                            }
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let constructor = {
                                        SchemaVersion = querySchema
                                        StableId = layout.ConstructorStableId
                                        Name = ".ctor"
                                        Kind = ClosureConstructor
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "receiver"
                                                Type = layout.CaptureType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = []
                                        ReturnType = CliVoid
                                        Instructions = [
                                            LoadArgument 0
                                            LoadArgument 1
                                            StoreField layout.CaptureFieldReference
                                            LoadArgument 0
                                            CallMethod objectConstructor
                                            Return
                                        ]
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds = [
                                            layout.CaptureFieldReference.DependencyId
                                            objectConstructor.DependencyId
                                        ]
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.ConstructorStableId
                                                layout.CaptureFieldStableId
                                                objectConstructor.StableId
                                            ]
                                        DocumentIndex = documentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let targetMethod = {
                                        DeclaringType = CliDeclaringType layout.CaptureType
                                        Name = expression.Target.Name
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = layout.RangeType
                                        TargetStableId = Some expression.TargetStableId
                                    }

                                    let invoke = {
                                        SchemaVersion = querySchema
                                        StableId = layout.InvokeStableId
                                        Name = "Invoke"
                                        Kind = ClosureInvoke
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "unitVar"
                                                Type = layout.DomainType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = []
                                        ReturnType = layout.RangeType
                                        Instructions = [
                                            LoadArgument 0
                                            LoadField layout.CaptureFieldReference
                                            CallMethod targetMethod
                                            Return
                                        ]
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds = [
                                            layout.CaptureFieldReference.DependencyId
                                            targetMethod.DependencyId
                                        ]
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.InvokeStableId
                                                methodImplementationHash methodDeclaration
                                                expression.TargetStableId
                                            ]
                                        DocumentIndex = documentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    Some {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace =
                                            if isNested then String.Empty else typed.Namespace
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            if isNested then Some moduleTypeStableId else None
                                        Kind = ClosureContainer
                                        GenericParameters = methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = [
                                            {
                                                SchemaVersion = querySchema
                                                StableId = layout.CaptureFieldStableId
                                                Name = "receiver"
                                                Type = layout.CaptureType
                                                Attributes = []
                                                ContentHash =
                                                    Fingerprint.parts [
                                                        layout.CaptureFieldStableId
                                                        TypeIdentity.cliType layout.CaptureType
                                                    ]
                                            }
                                        ]
                                        Methods = [
                                            constructor
                                            invoke
                                        ]
                                    }
                                | TypedIntegerLiteral _
                                | TypedStringLiteral _
                                | TypedUnitLiteral
                                | TypedParameterReference _
                                | TypedLocalReference _
                                | TypedLet _
                                | TypedLocalAssignment _
                                | TypedAddressOf _
                                | TypedStaticMethodCall _
                                | TypedFunctionApplication _
                                | TypedInstanceMethodCall _
                                | TypedUnitLambda _
                                | TypedConditional _
                                | TypedUpcast _
                                | TypedSequential _
                                | TypedBooleanNegation _
                                | TypedTypeTestMatch _
                                | TypedResumableCode _
                                | TypedResumableTryFinally _
                                | TypedObjectExpression _
                                | TypedTraitCall _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> []
                    )
                )
                |> List.collect id

            let unitLambdaClosureTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested = typed.ContainerKind = ModuleSource

                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedObjectType typeDeclaration ->
                            typeDeclaration.Methods
                            |> List.choose (fun objectMethodDeclaration ->
                                let methodDeclaration = objectMethodDeclaration.Method

                                match methodDeclaration.Body with
                                | TypedUnitLambda expression ->
                                    let layout =
                                        unitLambdaClosureLayout methodDeclaration expression

                                    let objectConstructor = {
                                        DeclaringType =
                                            CoreDeclaringType {
                                                Namespace = "System"
                                                Name = "Object"
                                            }
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let constructor = {
                                        SchemaVersion = querySchema
                                        StableId = layout.ConstructorStableId
                                        Name = ".ctor"
                                        Kind = ClosureConstructor
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = expression.CaptureName
                                                Type = layout.CaptureType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = []
                                        ReturnType = CliVoid
                                        Instructions = [
                                            LoadArgument 0
                                            LoadArgument 1
                                            StoreField layout.CaptureFieldReference
                                            LoadArgument 0
                                            CallMethod objectConstructor
                                            Return
                                        ]
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds = [
                                            layout.CaptureFieldReference.DependencyId
                                            objectConstructor.DependencyId
                                        ]
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.ConstructorStableId
                                                layout.CaptureFieldStableId
                                                objectConstructor.StableId
                                            ]
                                        DocumentIndex = documentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let invoke = {
                                        SchemaVersion = querySchema
                                        StableId = layout.InvokeStableId
                                        Name = "Invoke"
                                        Kind = ClosureInvoke
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "unitVar"
                                                Type = layout.DomainType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = []
                                        ReturnType = layout.RangeType
                                        Instructions = [
                                            LoadArgument 0
                                            LoadField layout.CaptureFieldReference
                                            Return
                                        ]
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds = [
                                            layout.CaptureFieldReference.DependencyId
                                        ]
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.InvokeStableId
                                                methodImplementationHash methodDeclaration
                                                layout.CaptureFieldStableId
                                            ]
                                        DocumentIndex = documentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    Some {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace =
                                            if isNested then String.Empty else typed.Namespace
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            if isNested then Some moduleTypeStableId else None
                                        Kind = ClosureContainer
                                        GenericParameters = methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = [
                                            {
                                                SchemaVersion = querySchema
                                                StableId = layout.CaptureFieldStableId
                                                Name = expression.CaptureName
                                                Type = layout.CaptureType
                                                Attributes = []
                                                ContentHash =
                                                    Fingerprint.parts [
                                                        layout.CaptureFieldStableId
                                                        TypeIdentity.cliType layout.CaptureType
                                                    ]
                                            }
                                        ]
                                        Methods = [
                                            constructor
                                            invoke
                                        ]
                                    }
                                | TypedIntegerLiteral _
                                | TypedStringLiteral _
                                | TypedUnitLiteral
                                | TypedParameterReference _
                                | TypedLocalReference _
                                | TypedLet _
                                | TypedLocalAssignment _
                                | TypedAddressOf _
                                | TypedStaticMethodCall _
                                | TypedFunctionApplication _
                                | TypedInstanceMethodCall _
                                | TypedBoundInstanceMethod _
                                | TypedConditional _
                                | TypedUpcast _
                                | TypedSequential _
                                | TypedBooleanNegation _
                                | TypedTypeTestMatch _
                                | TypedResumableCode _
                                | TypedResumableTryFinally _
                                | TypedObjectExpression _
                                | TypedTraitCall _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> []
                    )
                )
                |> List.collect id

            let structTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    let enclosingTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, typeContentHash) ->
                        match declaration with
                        | TypedStructType typeDeclaration ->
                            let attributes = [
                                yield!
                                    typeDeclaration.Attributes
                                    |> List.map customAttributeFragment

                                compilationMappingAttribute
                                    typeDeclaration.StableId
                                    ObjectTypeConstruct
                            ]

                            let fields =
                                typeDeclaration.Fields
                                |> List.map (fun field -> {
                                    SchemaVersion = querySchema
                                    StableId = field.StableId
                                    Name = field.Name
                                    Type = field.Type
                                    Attributes =
                                        field.Attributes
                                        |> List.map customAttributeFragment
                                    ContentHash = field.ExportFingerprint
                                })

                            Some {
                                SchemaVersion = querySchema
                                StableId = typeDeclaration.StableId
                                Namespace = String.Empty
                                Name = typeDeclaration.Name
                                IsPublic = true
                                EnclosingTypeStableId = Some enclosingTypeStableId
                                Kind = StructContainer
                                GenericParameters = typeDeclaration.GenericParameters
                                Attributes = attributes
                                LiteralFields = []
                                InstanceFields = fields
                                Methods = []
                            }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _ -> None
                    )
                )

            let types =
                moduleTypes
                @ staticTypes
                @ objectTypes
                @ objectExpressionTypes
                @ structTypes
                @ closureTypes
                @ boundMemberClosureTypes
                @ unitLambdaClosureTypes

            let symbolic: SymbolicAssembly = {
                SchemaVersion = querySchema
                StableId = assemblyStableId
                AssemblyName = assemblyName
                AssemblyVersion = assemblyVersion
                PublicFingerprint =
                    typedModules
                    |> List.map _.ExportFingerprint
                    |> String.concat "|"
                    |> Fingerprint.text
                Documents = documents
                AssemblyAttributes = assemblyAttributes
                Module = {
                    SchemaVersion = querySchema
                    StableId = moduleStableId
                    Name = moduleName
                    TypeAbbreviations = typeAbbreviations
                    Types = types
                }
            }

            lowerCache.Add(key, symbolic)
            symbolic, key

    member _.Compile
        (
            assemblyName: string,
            defines: string list,
            references: ReferenceTypeIndex,
            sources: SourceInput list
        ) =
        let combine values =
            match values with
            | [ value ] -> value
            | _ ->
                values
                |> String.concat "|"
                |> Fingerprint.text

        let stateKey =
            assemblyName
            + "\n"
            + references.Fingerprint
            + "\n"
            + (sources
               |> List.map _.Path
               |> String.concat "\n")

        let previousContentFingerprint =
            match lastSuccessfulContent.TryGetValue(stateKey) with
            | true, fingerprint -> fingerprint
            | false, _ -> String.Empty

        let before = {
            ParseHits = parseHits
            ParseMisses = parseMisses
            CheckHits = checkHits
            CheckMisses = checkMisses
            LowerHits = lowerHits
            LowerMisses = lowerMisses
        }

        let decision hitsBefore hitsAfter missesBefore missesAfter =
            let hitCount =
                hitsAfter
                - hitsBefore

            let missCount =
                missesAfter
                - missesBefore

            if
                hitCount > 0
                && missCount > 0
            then
                "partial"
            elif hitCount > 0 then
                "hit"
            else
                "miss"

        let rec parseAll parsed keys remaining =
            match remaining with
            | [] -> Ok(List.rev parsed, List.rev keys)
            | source :: tail ->
                match parse defines source with
                | Error diagnostic -> Error diagnostic
                | Ok(parsedModule, key) ->
                    parseAll
                        ((source, parsedModule)
                         :: parsed)
                        (key
                         :: keys)
                        tail

        let rec checkAll typed keys remaining =
            match remaining with
            | [] -> Ok(List.rev typed, List.rev keys)
            | (source, parsedModule) :: tail ->
                match check references source.Path parsedModule with
                | Error diagnostic -> Error diagnostic
                | Ok(typedModule, key) ->
                    checkAll
                        (typedModule
                         :: typed)
                        (key
                         :: keys)
                        tail

        let parseStarted = Stopwatch.GetTimestamp()

        match parseAll [] [] sources with
        | Error error -> Error error
        | Ok(parsedModules, parseKeys) ->
            let parseElapsedMicroseconds = elapsedMicroseconds parseStarted
            let checkStarted = Stopwatch.GetTimestamp()

            match checkAll [] [] parsedModules with
            | Error diagnostic -> Error diagnostic
            | Ok(typedModules, checkKeys) ->
                let checkElapsedMicroseconds = elapsedMicroseconds checkStarted
                let lowerStarted = Stopwatch.GetTimestamp()
                let symbolic, lowerKey = lower assemblyName typedModules
                let lowerElapsedMicroseconds = elapsedMicroseconds lowerStarted

                let contentFingerprint =
                    references.Fingerprint
                    :: (parsedModules
                        |> List.map (fun (_, parsedModule) -> parsedModule.ContentFingerprint))
                    |> combine

                let invalidationReason =
                    if previousContentFingerprint.Length = 0 then
                        "no-prior-state"
                    elif previousContentFingerprint = contentFingerprint then
                        "unchanged"
                    else
                        "source-content-changed"

                lastSuccessfulContent.[stateKey] <- contentFingerprint

                Ok {
                    SymbolicAssembly = symbolic
                    QuerySchema = querySchema
                    NodeKind = if sources.Length = 1 then "source" else "project"
                    ContentFingerprint = contentFingerprint
                    PreviousContentFingerprint = previousContentFingerprint
                    InvalidationReason = invalidationReason
                    ParseKey = combine parseKeys
                    CheckKey = combine checkKeys
                    LowerKey = lowerKey
                    DependencyCount =
                        symbolic.Module.TypeAbbreviations.Length
                        + (symbolic.Module.Types
                           |> List.sumBy (fun typeFragment ->
                               typeFragment.Methods
                               |> List.sumBy (fun methodFragment ->
                                   methodFragment.DependencyIds.Length
                               )
                           ))
                    ParseDecision =
                        decision before.ParseHits parseHits before.ParseMisses parseMisses
                    CheckDecision =
                        decision before.CheckHits checkHits before.CheckMisses checkMisses
                    LowerDecision =
                        decision before.LowerHits lowerHits before.LowerMisses lowerMisses
                    ParseElapsedMicroseconds = parseElapsedMicroseconds
                    CheckElapsedMicroseconds = checkElapsedMicroseconds
                    LowerElapsedMicroseconds = lowerElapsedMicroseconds
                }

    member _.Statistics = {
        ParseHits = parseHits
        ParseMisses = parseMisses
        CheckHits = checkHits
        CheckMisses = checkMisses
        LowerHits = lowerHits
        LowerMisses = lowerMisses
    }
