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
        | TypedAddressOfArgument name ->
            Fingerprint.parts [
                "address-of"
                name
            ]

    let inlineBody =
        function
        | TypedIntegerLiteral value ->
            Fingerprint.parts [
                "integer"
                value.ToString(CultureInfo.InvariantCulture)
            ]
        | TypedParameterReference index ->
            Fingerprint.parts [
                "parameter"
                index.ToString(CultureInfo.InvariantCulture)
            ]
        | TypedTraitCall(receiverName, memberName, arguments) ->
            Fingerprint.parts [
                "trait-call"
                receiverName
                memberName
                yield! arguments |> List.map callArgument
            ]

    let attributeKind =
        function
        | AutoOpenAttribute -> "auto-open"
        | StructAttribute -> "struct"
        | NoComparisonAttribute -> "no-comparison"
        | NoEqualityAttribute -> "no-equality"
        | DefaultValueAttribute -> "default-value"
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

            let rec substituteType
                (substitutions: Map<string, ParsedTypeExpression>)
                expression
                =
                match expression with
                | ParsedTypeParameter(name, _) ->
                    substitutions
                    |> Map.tryFind name
                    |> Option.defaultValue expression
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
                    let expandedArguments = arguments |> List.map expand

                    match typeAbbreviations |> Map.tryFind typeName.Name with
                    | Some abbreviation when
                        abbreviation.TypeParameters.Length = expandedArguments.Length
                        && not (expanding |> Set.contains typeName.Name)
                        ->
                        abbreviation.Target.Type
                        |> substituteType (
                            List.zip abbreviation.TypeParameters expandedArguments
                            |> Map.ofList
                        )
                        |> expandTypeAbbreviations (expanding |> Set.add typeName.Name)
                    | _ ->
                        match expression with
                        | ParsedGenericTypeApplication(genericType, _, range) ->
                            ParsedGenericTypeApplication(
                                expand genericType,
                                expandedArguments,
                                range
                            )
                        | _ -> expression
                | ParsedNamedType(typeName, _) when
                    String.IsNullOrEmpty(typeName.Namespace)
                    ->
                    match typeAbbreviations |> Map.tryFind typeName.Name with
                    | Some abbreviation when
                        List.isEmpty abbreviation.TypeParameters
                        && not (expanding |> Set.contains typeName.Name)
                        ->
                        abbreviation.Target.Type
                        |> expandTypeAbbreviations (expanding |> Set.add typeName.Name)
                    | _ -> expression
                | ParsedGenericTypeApplication(genericType, arguments, range) ->
                    ParsedGenericTypeApplication(
                        expand genericType,
                        arguments |> List.map expand,
                        range
                    )
                | ParsedTupleType(elements, range) ->
                    ParsedTupleType(elements |> List.map expand, range)
                | ParsedFunctionType(domain, range, sourceRange) ->
                    ParsedFunctionType(expand domain, expand range, sourceRange)
                | ParsedTypeParameter _
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
                | _ -> None

            let typeCustomAttribute
                ownerStableId
                allowedKinds
                (index: int)
                (attribute: ParsedAttribute)
                =
                let unsupported message = diagnostic attribute.Range message

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
                    | [ ParsedBooleanAttributeArgument value ] ->
                        let stableId =
                            ownerStableId
                            + "/attribute:"
                            + index.ToString(CultureInfo.InvariantCulture)

                        let arguments = [ TypedBooleanAttributeArgument value ]

                        Ok {
                            StableId = stableId
                            Kind = DefaultValueAttribute
                            ConstructorArguments = arguments
                            ExportFingerprint =
                                Fingerprint.parts [
                                    stableId
                                    TypeIdentity.attributeKind DefaultValueAttribute
                                    yield!
                                        arguments
                                        |> List.map TypeIdentity.attributeArgument
                                ]
                        }
                    | _ -> unsupported "DefaultValueAttribute requires one boolean argument"
                | Some kind ->
                    if List.isEmpty attribute.ConstructorArguments then
                        let stableId =
                            ownerStableId
                            + "/attribute:"
                            + index.ToString(CultureInfo.InvariantCulture)

                        Ok {
                            StableId = stableId
                            Kind = kind
                            ConstructorArguments = []
                            ExportFingerprint =
                                Fingerprint.parts [
                                    stableId
                                    TypeIdentity.attributeKind kind
                                ]
                        }
                    else
                        unsupported "the declaration attribute does not accept arguments"

            let typeCustomAttributes ownerStableId allowedKinds attributes =
                attributes
                |> List.mapi (typeCustomAttribute ownerStableId allowedKinds)
                |> collectResults []

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
                                GenericParameters = []
                                Constraints = []
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
                    | _, TraitCall _ ->
                        diagnostic
                            declaration.BodyRange
                            "trait calls are supported only in static inline members"
                    | _, ValueReference _ ->
                        diagnostic
                            declaration.BodyRange
                            "value references are supported only in parameterized members"
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
                              TraitCall(receiverName, memberName, arguments) ->
                                let argumentName =
                                    function
                                    | ParsedValueArgument name
                                    | ParsedAddressOfArgument name -> name

                                let typedArguments =
                                    arguments
                                    |> List.map (
                                        function
                                        | ParsedValueArgument name -> TypedValueArgument name
                                        | ParsedAddressOfArgument name ->
                                            TypedAddressOfArgument name
                                    )

                                if
                                    parameters
                                    |> List.exists (fun parameter -> parameter.Name = receiverName)
                                    |> not
                                then
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        $"the receiver '{receiverName}' is not a method parameter"
                                elif
                                    arguments
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
                                                    |> List.collect (fun parameter -> [
                                                        parameter.Name
                                                        TypeIdentity.cliType parameter.Type
                                                    ])

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
                                            GenericParameters = methodDeclaration.TypeParameters
                                            Constraints = constraints
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
                            | Ok _, Ok _, StringLiteral _
                            | Ok _, Ok _, ValueReference _ ->
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
                        parsed.StableId
                        + "/type:"
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
                                        [ domainType; rangeType ]
                                    )
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

                    let typeMethod (methodDeclaration: ParsedInstanceMethodDeclaration) =
                        let methodTypeParameters =
                            let fromParameters =
                                ([], methodDeclaration.Parameters)
                                ||> List.fold (fun collected parameter ->
                                    collectTypeParameters collected parameter.Type
                                )

                            match methodDeclaration.ReturnType with
                            | Some returnType ->
                                collectTypeParameters fromParameters returnType
                            | None -> fromParameters

                        let declaredMethodParameters =
                            HashSet<string>(methodTypeParameters, StringComparer.Ordinal)

                        let methodParameterIndex =
                            methodTypeParameters
                            |> List.mapi (fun index name -> name, index)
                            |> Map.ofList

                        let typedParameters =
                            methodDeclaration.Parameters
                            |> List.map (fun parameter ->
                                parameter.Type
                                |> expandTypeAbbreviations Set.empty
                                |> resolveType declaredMethodParameters
                                |> Result.bind (toCliType methodParameterIndex parameter.Range)
                                |> Result.map (fun parameterType -> {
                                    Name = parameter.Name
                                    Type = parameterType
                                })
                            )
                            |> collectResults []

                        match typedParameters with
                        | Error error -> Error error
                        | Ok parameters when
                            (parameters
                             |> List.map _.Name
                             |> Set.ofList
                             |> Set.count)
                            <> parameters.Length
                            ->
                            diagnostic
                                methodDeclaration.Range
                                "instance-member parameter names must be unique"
                        | Ok parameters ->
                            let typedBody =
                                match methodDeclaration.Body with
                                | IntegerLiteral value ->
                                    Ok(TypedIntegerLiteral value, CliInt32)
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
                                | StringLiteral _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "string-valued instance members are not yet supported"
                                | TraitCall _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "trait calls are not yet supported in instance members"

                            let declaredReturnType =
                                match methodDeclaration.ReturnType with
                                | None -> Ok None
                                | Some returnType ->
                                    returnType
                                    |> expandTypeAbbreviations Set.empty
                                    |> resolveType declaredMethodParameters
                                    |> Result.bind (
                                        toCliReturnType methodParameterIndex returnType.Range
                                    )
                                    |> Result.map Some

                            match typedBody, declaredReturnType with
                            | Error error, _
                            | _, Error error -> Error error
                            | Ok(body, inferredReturnType), Ok(Some returnType) when
                                returnType <> inferredReturnType
                                ->
                                diagnostic
                                    methodDeclaration.BodyRange
                                    "the instance-member body does not match its declared return type"
                            | Ok(body, inferredReturnType), Ok declaredReturnType ->
                                let returnType =
                                    declaredReturnType
                                    |> Option.defaultValue inferredReturnType

                                let parameterIdentity =
                                    match parameters with
                                    | [] -> "unit"
                                    | _ ->
                                        parameters
                                        |> List.map (fun parameter ->
                                            TypeIdentity.cliType parameter.Type
                                        )
                                        |> String.concat "*"

                                let methodStableId =
                                    stableId
                                    + "/method:"
                                    + methodDeclaration.Name
                                    + ":"
                                    + parameterIdentity
                                    + "->"
                                    + TypeIdentity.cliType returnType

                                let exportFingerprint =
                                    Fingerprint.parts [
                                        methodStableId
                                        "instance"
                                        "generic-parameters"
                                        yield! methodTypeParameters
                                        "parameters"

                                        yield!
                                            parameters
                                            |> List.collect (fun parameter -> [
                                                parameter.Name
                                                TypeIdentity.cliType parameter.Type
                                            ])

                                        "return"
                                        TypeIdentity.cliType returnType
                                        "inline-body"
                                        TypeIdentity.inlineBody body
                                    ]

                                Ok {
                                    StableId = methodStableId
                                    Name = methodDeclaration.Name
                                    GenericParameters = methodTypeParameters
                                    Constraints = []
                                    Parameters = parameters
                                    ReturnType = returnType
                                    Body = body
                                    ExportFingerprint = exportFingerprint
                                    Range = methodDeclaration.BodyRange
                                }

                    match
                        declaration.Methods
                        |> List.map typeMethod
                        |> collectResults []
                    with
                    | Error error -> Error error
                    | Ok methods ->
                        Ok(
                            TypedObjectType {
                                StableId = stableId
                                Name = declaration.Name
                                Methods = methods
                                ExportFingerprint =
                                    Fingerprint.parts [
                                        stableId
                                        "constructor:unit"

                                        yield!
                                            methods
                                            |> List.map _.ExportFingerprint
                                    ]
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
                let typedDeclarations =
                    parsed.Declarations
                    |> List.map typeDeclaration
                    |> collectResults []

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
                                        |> List.collect (fun methodDeclaration -> [
                                            methodDeclaration.StableId
                                            methodDeclaration.ExportFingerprint
                                            methodImplementationHash methodDeclaration
                                        ])
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

            let methodInstructions kind (methodDeclaration: TypedMethodDeclaration) =
                match methodDeclaration.Body with
                | TypedIntegerLiteral value -> [
                    LoadInt32 value
                    Return
                  ]
                | TypedParameterReference index ->
                    let argumentIndex =
                        match kind with
                        | InstanceConstructor
                        | InstanceInlineMember ->
                            index
                            + 1
                        | ModuleFunction
                        | StaticInlineMemberStub -> index

                    [
                        LoadArgument argumentIndex
                        Return
                    ]
                | TypedTraitCall(_, memberName, _) -> [
                    LoadString(
                        "Dynamic invocation of "
                        + memberName
                        + " is not supported"
                    )
                    NewObject(
                        {
                            Namespace = "System"
                            Name = "NotSupportedException"
                        },
                        [ CliString ]
                    )
                    Throw
                  ]

            let methodDependencies (methodDeclaration: TypedMethodDeclaration) =
                methodDeclaration.Constraints
                |> List.choose (
                    function
                    | TypedAbbreviationConstraint(TypedGenericTypeApplication(TypedNamedType resolvedType,
                                                                              _)) ->
                        Some resolvedType.DeclarationId
                    | TypedAbbreviationConstraint _
                    | TypedDirectConstraint _ -> None
                )

            let methodFragment
                kind
                documentIndex
                documentChecksum
                fragmentStableId
                contentHash
                (methodDeclaration: TypedMethodDeclaration)
                =
                {
                    SchemaVersion = querySchema
                    StableId = fragmentStableId
                    Name = methodDeclaration.Name
                    Kind = kind
                    GenericParameters = methodDeclaration.GenericParameters
                    Constraints = methodDeclaration.Constraints
                    Parameters = methodDeclaration.Parameters
                    ReturnType = methodDeclaration.ReturnType
                    Instructions = methodInstructions kind methodDeclaration
                    DependencyIds = methodDependencies methodDeclaration
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

            let objectTypes =
                modulesWithContentHashes
                |> List.mapi (fun documentIndex (typed, declarationsWithContentHashes) ->
                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested =
                        typed.ContainerKind = ModuleSource

                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, _) ->
                        match declaration with
                        | TypedObjectType typeDeclaration ->
                            let constructorStableId =
                                typeDeclaration.StableId
                                + "/constructor:unit"

                            let objectConstructor = {
                                DeclaringType = {
                                    Namespace = "System"
                                    Name = "Object"
                                }
                                Name = ".ctor"
                                IsInstance = true
                                ParameterTypes = []
                                ReturnType = CliVoid
                            }

                            let constructor = {
                                SchemaVersion = querySchema
                                StableId = constructorStableId
                                Name = ".ctor"
                                Kind = InstanceConstructor
                                GenericParameters = []
                                Constraints = []
                                Parameters = []
                                ReturnType = CliVoid
                                Instructions = [
                                    LoadArgument 0
                                    CallMethod objectConstructor
                                    Return
                                ]
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
                                |> List.map (fun methodDeclaration ->
                                    methodFragment
                                        InstanceInlineMember
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
                                Methods = constructor :: methods
                            }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> None
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
                @ structTypes

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
