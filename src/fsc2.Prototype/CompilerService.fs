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
        | TypedNullLiteral -> "null"
        | TypedUnitLiteral -> "unit"
        | TypedReceiverReference -> "receiver"
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
        | TypedInstanceFieldGet(receiver, field) ->
            Fingerprint.parts [
                "instance-field-get"
                inlineBody receiver
                cliType field.DeclaringType
                field.Name
                cliType field.FieldType
                field.TargetStableId
                |> Option.defaultValue String.Empty
            ]
        | TypedStaticMethodCall(target, genericArguments, arguments) ->
            Fingerprint.parts [
                "static-call"
                target.StableId
                cliType target.DeclaringType

                yield!
                    genericArguments
                    |> List.map cliType

                yield!
                    arguments
                    |> List.map inlineBody
            ]
        | TypedObjectConstruction(target, arguments) ->
            Fingerprint.parts [
                "object-construction"
                cliType target.DeclaringType
                target.StableId

                yield!
                    target.ParameterTypes
                    |> List.map cliType

                yield!
                    arguments
                    |> List.map inlineBody
            ]
        | TypedDefaultValue(valueType, localIndex) ->
            Fingerprint.parts [
                "default-value"
                cliType valueType
                localIndex.ToString(CultureInfo.InvariantCulture)
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
        | TypedFunctionLambda expression ->
            Fingerprint.parts [
                "function-lambda"
                cliType expression.FunctionType
                cliType expression.ConverterType
                expression.ParameterName
                cliType expression.ParameterType
                cliType expression.ReturnType

                yield!
                    expression.Captures
                    |> List.map (fun capture ->
                        Fingerprint.parts [
                            capture.OuterParameterIndex.ToString(
                                CultureInfo.InvariantCulture
                            )
                            capture.Name
                            cliType capture.Type
                            capture.Field.Name
                        ]
                    )

                inlineBody expression.Body
            ]
        | TypedDelegateLambda expression ->
            Fingerprint.parts [
                "delegate-lambda"
                cliType expression.DelegateType

                yield!
                    expression.Captures
                    |> List.map (fun capture ->
                        Fingerprint.parts [
                            capture.OuterParameterIndex.ToString(
                                CultureInfo.InvariantCulture
                            )
                            capture.Name
                            cliType capture.Type
                            capture.Field.Name
                        ]
                    )

                yield!
                    (expression.LambdaParameterNames, expression.LambdaParameterTypes)
                    ||> List.map2 (fun name parameterType ->
                        Fingerprint.parts [
                            name
                            cliType parameterType
                        ]
                    )

                cliType expression.LambdaReturnType
                inlineBody expression.LambdaBody
            ]
        | TypedValueTaskBind expression ->
            Fingerprint.parts [
                "value-task-bind"
                expression.BuilderName
                (match expression.ReturnKind with
                 | ComputationReturn -> "return"
                 | ComputationReturnFrom -> "return-from")
                expression.BinderParameterIndex.ToString(CultureInfo.InvariantCulture)
                expression.SourceParameterIndex.ToString(CultureInfo.InvariantCulture)
                cliType expression.InputType
                cliType expression.OutputType
                cliType expression.BinderType
                cliType expression.InputValueTaskType
                cliType expression.OutputValueTaskType
            ]
        | TypedValueTaskApply expression ->
            Fingerprint.parts [
                "value-task-apply"
                expression.BuilderName
                expression.ApplicableParameterIndex.ToString(CultureInfo.InvariantCulture)
                expression.InputParameterIndex.ToString(CultureInfo.InvariantCulture)
                cliType expression.InputType
                cliType expression.OutputType
                cliType expression.ApplierType
                cliType expression.ApplicableValueTaskType
                cliType expression.InputValueTaskType
                cliType expression.OutputValueTaskType
            ]
        | TypedValueTaskZip expression ->
            Fingerprint.parts [
                "value-task-zip"
                expression.BuilderName
                expression.LeftParameterIndex.ToString(CultureInfo.InvariantCulture)
                expression.RightParameterIndex.ToString(CultureInfo.InvariantCulture)
                cliType expression.LeftType
                cliType expression.RightType
                cliType expression.TupleType
                cliType expression.LeftValueTaskType
                cliType expression.RightValueTaskType
                cliType expression.OutputValueTaskType
            ]
        | TypedValueTaskOfUnit expression ->
            Fingerprint.parts [
                "value-task-of-unit"
                expression.BuilderName
                expression.SourceParameterIndex.ToString(CultureInfo.InvariantCulture)
                cliType expression.SourceValueTaskType
                cliType expression.UnitType
                cliType expression.OutputValueTaskType
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
        | TypedEquality(left, right, _) ->
            Fingerprint.parts [
                "equality"
                inlineBody left
                inlineBody right
            ]
        | TypedTryWith(body,
                       handlerLocalIndex,
                       handlerName,
                       catchType,
                       handler,
                       _,
                       _,
                       _,
                       _,
                       _) ->
            Fingerprint.parts [
                "try-with"
                inlineBody body
                handlerLocalIndex.ToString(CultureInfo.InvariantCulture)
                handlerName
                cliType catchType
                inlineBody handler
            ]
        | TypedNullMatch(input,
                         inputType,
                         localIndex,
                         bindingName,
                         ifNull,
                         ifNotNull,
                         _,
                         _,
                         _,
                         _) ->
            Fingerprint.parts [
                "null-match"
                inlineBody input
                cliType inputType
                localIndex.ToString(CultureInfo.InvariantCulture)
                bindingName
                inlineBody ifNull
                inlineBody ifNotNull
            ]
        | TypedTypeTestMatch(input,
                             targetType,
                             localIndex,
                             bindingName,
                             guard,
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
                match guard with
                | Some(guardExpression, _) -> inlineBody guardExpression
                | None -> "no-guard"
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
                                members,
                                _) ->
            Fingerprint.parts [
                "object-expression"
                typeReference.DeclarationId
                cliType baseType

                yield! constructorArguments |> List.map inlineBody

                yield!
                    members
                    |> List.map (fun memberDeclaration ->
                        Fingerprint.parts [
                            if memberDeclaration.IsOverride then "override" else "member"
                            memberDeclaration.ReceiverName
                            memberDeclaration.Name
                            cliType memberDeclaration.ReturnType

                            yield!
                                memberDeclaration.Parameters
                                |> List.map (fun parameter ->
                                    Fingerprint.parts [
                                        parameter.Name
                                        cliType parameter.Type
                                        "attributes"
                                        yield!
                                            parameter.Attributes
                                            |> List.map _.ExportFingerprint
                                    ]
                                )

                            inlineBody memberDeclaration.Body
                        ]
                    )
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
        | RequireQualifiedAccessAttribute -> "require-qualified-access"
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
    IsInline: bool
    IsPublic: bool
    GenericParameters: string list
    Constraints: TypedMethodConstraint list
    ParsedAttributes: ParsedAttribute list
    ParsedParameters: ParsedParameter list
    Parameters: TypedParameter list
    ReturnType: CliType
    Body: TypedExpression
    EmitHiddenEntrySequencePoint: bool
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

module private InlineExpansion =
    let private mapChildren transform =
        function
        | MemberCall(receiverName, memberName, arguments) ->
            MemberCall(receiverName, memberName, arguments |> List.map transform)
        | StaticTypeMemberCall(receiverType, memberName, arguments) ->
            StaticTypeMemberCall(receiverType, memberName, arguments |> List.map transform)
        | GenericMemberCall(receiverName, memberName, typeArguments, arguments) ->
            GenericMemberCall(
                receiverName,
                memberName,
                typeArguments,
                arguments |> List.map transform
            )
        | MemberAssignment(rootName, memberPath, value) ->
            MemberAssignment(rootName, memberPath, transform value)
        | SequentialExpression expressions -> SequentialExpression(expressions |> List.map transform)
        | FunctionApplication(functionExpression, argumentExpression) ->
            FunctionApplication(transform functionExpression, transform argumentExpression)
        | ExpressionMemberCall(receiver, memberName, arguments) ->
            ExpressionMemberCall(
                transform receiver,
                memberName,
                arguments |> List.map transform
            )
        | ExpressionMemberAccess(receiver, memberName) ->
            ExpressionMemberAccess(transform receiver, memberName)
        | ConditionalExpression(condition,
                                ifTrue,
                                ifFalse,
                                conditionRange,
                                ifTrueRange,
                                ifFalseRange) ->
            ConditionalExpression(
                transform condition,
                transform ifTrue,
                transform ifFalse,
                conditionRange,
                ifTrueRange,
                ifFalseRange
            )
        | ExplicitUpcastExpression(expression, targetType) ->
            ExplicitUpcastExpression(transform expression, targetType)
        | SequentialValueExpression expressions ->
            SequentialValueExpression(
                expressions
                |> List.map (fun (expression, range) -> transform expression, range)
            )
        | LocalAssignment(name, value) -> LocalAssignment(name, transform value)
        | BooleanNegationExpression(expression, range) ->
            BooleanNegationExpression(transform expression, range)
        | EqualityExpression(left, right, range) ->
            EqualityExpression(transform left, transform right, range)
        | TryWithExpression(body,
                            bindingName,
                            handler,
                            tryRange,
                            withRange,
                            bodyRange,
                            handlerRange,
                            range) ->
            TryWithExpression(
                transform body,
                bindingName,
                transform handler,
                tryRange,
                withRange,
                bodyRange,
                handlerRange,
                range
            )
        | MatchExpression(input, clauses, matchHeaderRange, range) ->
            MatchExpression(
                transform input,
                clauses
                |> List.map (fun (pattern, guard, body, bodyRange) ->
                    let guard =
                        guard
                        |> Option.map (fun (expression, guardRange) ->
                            transform expression, guardRange
                        )

                    pattern, guard, transform body, bodyRange
                ),
                matchHeaderRange,
                range
            )
        | LetExpression(bindingName,
                        isMutable,
                        isInline,
                        value,
                        body,
                        bindingRange,
                        bodyRange) ->
            LetExpression(
                bindingName,
                isMutable,
                isInline,
                transform value,
                transform body,
                bindingRange,
                bodyRange
            )
        | LambdaExpression(parameterName, parameterType, body, range) ->
            LambdaExpression(parameterName, parameterType, transform body, range)
        | UnitLambdaExpression(body, range) -> UnitLambdaExpression(transform body, range)
        | TupleExpression(elements, range) ->
            TupleExpression(elements |> List.map transform, range)
        | StructTupleExpression(elements, range) ->
            StructTupleExpression(elements |> List.map transform, range)
        | TypeConstruction(constructedType, arguments, argumentRange) ->
            TypeConstruction(constructedType, arguments |> List.map transform, argumentRange)
        | BindReturnFromComputation(builderName, bindings, returnKind, returnFrom, range) ->
            BindReturnFromComputation(
                builderName,
                bindings
                |> List.map (fun (name, input) -> name, transform input),
                returnKind,
                transform returnFrom,
                range
            )
        | ObjectExpression(baseType, constructorArguments, members, range) ->
            ObjectExpression(
                baseType,
                constructorArguments |> List.map transform,
                members
                |> List.map (fun memberDeclaration -> {
                    memberDeclaration with
                        Body = transform memberDeclaration.Body
                }),
                range
            )
        | IntegerLiteral _
        | UnitLiteral
        | BooleanLiteral _
        | StringLiteral _
        | NullLiteral
        | ValueReference _
        | AddressOfExpression _
        | UnitApplication _
        | BoundInstanceMember _ as expression -> expression

    let rec private substitute replacements =
        function
        | ValueReference name as expression ->
            replacements
            |> Map.tryFind name
            |> Option.defaultValue expression
        | AddressOfExpression(rootName, memberPath) as expression ->
            match replacements |> Map.tryFind rootName with
            | Some(ValueReference replacement) ->
                AddressOfExpression(replacement, memberPath)
            | _ -> expression
        | UnitApplication functionName as expression ->
            match replacements |> Map.tryFind functionName with
            | Some functionExpression ->
                FunctionApplication(functionExpression, UnitLiteral)
            | None -> expression
        | MemberCall(receiverName, memberName, arguments) ->
            let arguments = arguments |> List.map (substitute replacements)

            match replacements |> Map.tryFind receiverName with
            | Some(ValueReference replacement) ->
                MemberCall(replacement, memberName, arguments)
            | Some receiver ->
                ExpressionMemberCall(substitute replacements receiver, memberName, arguments)
            | None -> MemberCall(receiverName, memberName, arguments)
        | GenericMemberCall(receiverName, memberName, typeArguments, arguments) ->
            let receiverName =
                match replacements |> Map.tryFind receiverName with
                | Some(ValueReference replacement) -> replacement
                | _ -> receiverName

            GenericMemberCall(
                receiverName,
                memberName,
                typeArguments,
                arguments |> List.map (substitute replacements)
            )
        | BoundInstanceMember(receiverName, memberName) as expression ->
            match replacements |> Map.tryFind receiverName with
            | Some receiver -> ExpressionMemberAccess(receiver, memberName)
            | None -> expression
        | MemberAssignment(rootName, memberPath, value) ->
            let rootName =
                match replacements |> Map.tryFind rootName with
                | Some(ValueReference replacement) -> replacement
                | _ -> rootName

            MemberAssignment(rootName, memberPath, substitute replacements value)
        | LocalAssignment(name, value) ->
            let name =
                match replacements |> Map.tryFind name with
                | Some(ValueReference replacement) -> replacement
                | _ -> name

            LocalAssignment(name, substitute replacements value)
        | LetExpression(bindingName,
                        isMutable,
                        isInline,
                        value,
                        body,
                        bindingRange,
                        bodyRange) ->
            LetExpression(
                bindingName,
                isMutable,
                isInline,
                substitute replacements value,
                substitute (replacements |> Map.remove bindingName) body,
                bindingRange,
                bodyRange
            )
        | LambdaExpression(parameterName, parameterType, body, range) ->
            LambdaExpression(
                parameterName,
                parameterType,
                substitute (replacements |> Map.remove parameterName) body,
                range
            )
        | TryWithExpression(body,
                            bindingName,
                            handler,
                            tryRange,
                            withRange,
                            bodyRange,
                            handlerRange,
                            range) ->
            TryWithExpression(
                substitute replacements body,
                bindingName,
                substitute (replacements |> Map.remove bindingName) handler,
                tryRange,
                withRange,
                bodyRange,
                handlerRange,
                range
            )
        | expression -> mapChildren (substitute replacements) expression

    let private lambdaParameters (expression: ParsedExpression) =
        let rec collect parameters =
            function
            | LambdaExpression(parameterName, _, body, _) ->
                collect (parameterName :: parameters) body
            | body -> List.rev parameters, body

        collect [] expression

    let private application (expression: ParsedExpression) : (string * ParsedExpression list) option =
        let rec collect arguments =
            function
            | FunctionApplication(functionExpression, argument) ->
                collect (argument :: arguments) functionExpression
            | ValueReference functionName -> Some(functionName, arguments)
            | _ -> None

        collect [] expression

    let rec private replaceCalls
        functionName
        (parameters: string list)
        functionBody
        expression
        =
        let replacement =
            application expression
            |> Option.bind (fun (calledName, arguments) ->
                let arguments =
                    arguments
                    |> List.collect (function
                        | TupleExpression(elements, _) -> elements
                        | argument -> [ argument ]
                    )

                if
                    calledName = functionName
                    && arguments.Length = parameters.Length
                then
                    List.zip parameters arguments
                    |> Map.ofList
                    |> fun replacements -> substitute replacements functionBody
                    |> Some
                else
                    None
            )

        match replacement with
        | Some expression -> expression
        | None ->
            match expression with
            | LetExpression(bindingName,
                            isMutable,
                            isInline,
                            value,
                            body,
                            bindingRange,
                            bodyRange) ->
                LetExpression(
                    bindingName,
                    isMutable,
                    isInline,
                    replaceCalls functionName parameters functionBody value,
                    (if bindingName = functionName then
                         body
                     else
                         replaceCalls functionName parameters functionBody body),
                    bindingRange,
                    bodyRange
                )
            | LambdaExpression(parameterName, parameterType, body, range) ->
                LambdaExpression(
                    parameterName,
                    parameterType,
                    (if parameterName = functionName then
                         body
                     else
                         replaceCalls functionName parameters functionBody body),
                    range
                )
            | expression ->
                mapChildren (replaceCalls functionName parameters functionBody) expression

    let expand expression =
        let mutable expandedLocalFunction = false

        let rec expandExpression =
            function
            | LetExpression(bindingName,
                            false,
                            true,
                            value,
                            body,
                            _,
                            _) ->
                expandedLocalFunction <- true

                let parameters, functionBody = lambdaParameters value

                body
                |> replaceCalls bindingName parameters functionBody
                |> expandExpression
            | expression -> mapChildren expandExpression expression

        expandExpression expression, expandedLocalFunction

    let sourceRange defaultRange =
        function
        | LetExpression(_, false, true, _, _, _, bodyRange) -> bodyRange
        | _ -> defaultRange

/// A deliberately small in-memory query owner. Query identities and cached
/// values are semantic/compiler state; final SRM state never enters these maps.
type internal CompilerService() =
    let querySchema = CompilerSchema.Query
    let parseCache = Dictionary<string, ParsedModule list>(StringComparer.Ordinal)
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

    let check
        (references: ReferenceTypeIndex)
        (sourcePath: string)
        (documentIndex: int)
        (parsed: ParsedModule)
        =
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
                        DocumentIndex = documentIndex
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
                | TypedNestedModule _
                | TypedTypeAbbreviation _ -> ()

            let typeAbbreviations =
                parsed.Declarations
                |> List.choose (
                    function
                    | ParsedTypeAbbreviation declaration -> Some(declaration.Name, declaration)
                    | ParsedMethod _
                    | ParsedLiteralField _
                    | ParsedNestedModule _
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
                        | ParsedNestedModule _
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
                | "RequireQualifiedAccess"
                | "RequireQualifiedAccessAttribute" -> Some RequireQualifiedAccessAttribute
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
                                DeclaringType =
                                    CliNamedType {
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

            let rec inferReferenceMethodTypeArgument
                (declaringTypeArguments: CliType list)
                (methodSubstitutions: CliType option array)
                templateType
                actualType
                =
                match templateType with
                | CliTypeParameter index ->
                    index >= 0
                    && index < declaringTypeArguments.Length
                    && declaringTypeArguments.[index] = actualType
                | CliMethodTypeParameter index ->
                    if index < 0 || index >= methodSubstitutions.Length then
                        false
                    else
                        match methodSubstitutions.[index] with
                        | None ->
                            methodSubstitutions.[index] <- Some actualType
                            true
                        | Some inferredType -> inferredType = actualType
                | CliGenericType(templateReference, templateArguments) ->
                    match actualType with
                    | CliGenericType(actualReference, actualArguments) when
                        templateReference = actualReference
                        && templateArguments.Length = actualArguments.Length
                        ->
                        (templateArguments, actualArguments)
                        ||> List.forall2 (
                            inferReferenceMethodTypeArgument
                                declaringTypeArguments
                                methodSubstitutions
                        )
                    | _ -> false
                | CliByRef templateElementType ->
                    match actualType with
                    | CliByRef actualElementType ->
                        inferReferenceMethodTypeArgument
                            declaringTypeArguments
                            methodSubstitutions
                            templateElementType
                            actualElementType
                    | _ -> false
                | _ -> templateType = actualType

            let rec substituteReferenceMethodTypeArguments
                (declaringTypeArguments: CliType list)
                (methodSubstitutions: CliType option array)
                =
                function
                | CliTypeParameter index ->
                    if index < 0 || index >= declaringTypeArguments.Length then
                        invalidOp "a static-call declaring-type parameter was not supplied"
                    else
                        declaringTypeArguments.[index]
                | CliMethodTypeParameter index ->
                    if index < 0 || index >= methodSubstitutions.Length then
                        invalidOp "a static-call method type parameter was out of range"
                    else
                        methodSubstitutions.[index]
                        |> Option.defaultWith (fun () ->
                            invalidOp "a static-call method type parameter was not inferred"
                        )
                | CliGenericType(typeReference, arguments) ->
                    CliGenericType(
                        typeReference,
                        arguments
                        |> List.map (
                            substituteReferenceMethodTypeArguments
                                declaringTypeArguments
                                methodSubstitutions
                        )
                    )
                | CliByRef elementType ->
                    CliByRef(
                        substituteReferenceMethodTypeArguments
                            declaringTypeArguments
                            methodSubstitutions
                            elementType
                    )
                | cliType -> cliType

            let tryInferReferenceStaticMethod
                declaringType
                declaringTypeArguments
                (methodDefinition: ReferenceMethodDefinition)
                (argumentTypes: CliType list)
                =
                let requiredParameterCount =
                    methodDefinition.ParameterTypes.Length
                    - methodDefinition.OptionalParameterCount

                if
                    argumentTypes.Length < requiredParameterCount
                    || argumentTypes.Length > methodDefinition.ParameterTypes.Length
                then
                    None
                else
                    let methodSubstitutions =
                        Array.create methodDefinition.GenericArity None

                    let providedParameterTypes =
                        methodDefinition.ParameterTypes
                        |> List.truncate argumentTypes.Length

                    let parametersMatch =
                        (providedParameterTypes, argumentTypes)
                        ||> List.forall2 (
                            inferReferenceMethodTypeArgument
                                declaringTypeArguments
                                methodSubstitutions
                        )

                    if
                        parametersMatch
                        && (methodSubstitutions
                            |> Array.forall Option.isSome)
                    then
                        let genericArguments =
                            methodSubstitutions
                            |> Array.choose id
                            |> Array.toList

                        let returnType =
                            substituteReferenceMethodTypeArguments
                                declaringTypeArguments
                                methodSubstitutions
                                methodDefinition.ReturnType

                        Some(
                            {
                                DeclaringType = declaringType
                                StableId = methodDefinition.StableId
                                Name = methodDefinition.Name
                                GenericArity = methodDefinition.GenericArity
                                ParameterTypes = methodDefinition.ParameterTypes
                                ReturnType = methodDefinition.ReturnType
                            },
                            genericArguments,
                            returnType
                        )
                    else
                        None

            let tryApplyExplicitMethodTypeArguments
                (genericArity: int)
                (parameterTypes: CliType list)
                (optionalParameterCount: int)
                returnType
                (genericArguments: CliType list)
                (argumentTypes: CliType list)
                =
                let requiredParameterCount =
                    parameterTypes.Length
                    - optionalParameterCount

                if
                    genericArity
                    <> genericArguments.Length
                    || argumentTypes.Length < requiredParameterCount
                    || argumentTypes.Length > parameterTypes.Length
                then
                    None
                else
                    let substitutions =
                        genericArguments
                        |> List.map Some
                        |> List.toArray

                    let parametersMatch =
                        (parameterTypes
                         |> List.truncate argumentTypes.Length,
                         argumentTypes)
                        ||> List.forall2 (inferMethodTypeArgument substitutions)

                    if parametersMatch then
                        Some(substituteMethodTypeArguments substitutions returnType)
                    else
                        None

            let tryApplyExplicitSourceStaticMethod
                (sourceType: CheckedSourceType)
                (methodDeclaration: TypedMethodDeclaration)
                genericArguments
                argumentTypes
                =
                tryApplyExplicitMethodTypeArguments
                    methodDeclaration.GenericParameters.Length
                    (methodDeclaration.Parameters |> List.map _.Type)
                    0
                    methodDeclaration.ReturnType
                    genericArguments
                    argumentTypes
                |> Option.map (fun returnType ->
                    {
                        DeclaringType =
                            CliNamedType {
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
                        ParameterTypes = methodDeclaration.Parameters |> List.map _.Type
                        ReturnType = methodDeclaration.ReturnType
                    },
                    genericArguments,
                    returnType
                )

            let tryApplyExplicitReferenceStaticMethod
                declaringType
                (methodDefinition: ReferenceMethodDefinition)
                genericArguments
                argumentTypes
                =
                tryApplyExplicitMethodTypeArguments
                    methodDefinition.GenericArity
                    methodDefinition.ParameterTypes
                    methodDefinition.OptionalParameterCount
                    methodDefinition.ReturnType
                    genericArguments
                    argumentTypes
                |> Option.map (fun returnType ->
                    {
                        DeclaringType = declaringType
                        StableId = methodDefinition.StableId
                        Name = methodDefinition.Name
                        GenericArity = methodDefinition.GenericArity
                        ParameterTypes = methodDefinition.ParameterTypes
                        ReturnType = methodDefinition.ReturnType
                    },
                    genericArguments,
                    returnType
                )

            let resolveStaticMethod receiverName memberName argumentTypes range =
                let visibleNamespaces =
                    parsed.Namespace
                    :: parsed.OpenedNamespaces
                    |> Set.ofList

                let visibleSourceTypes =
                    checkedSourceTypes
                    |> Seq.filter (fun sourceType ->
                        sourceType.Name = receiverName
                        && visibleNamespaces.Contains(sourceType.Namespace)
                    )

                let sourceCandidates =
                    visibleSourceTypes
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

                let candidates =
                    if
                        visibleSourceTypes
                        |> Seq.isEmpty
                    then
                        match
                            references.Resolve(
                                parsed.Namespace,
                                parsed.OpenedNamespaces,
                                {
                                    Namespace = String.Empty
                                    Name = receiverName
                                },
                                0
                            )
                        with
                        | Error _ -> []
                        | Ok resolvedType ->
                            let referenceCandidates =
                                references.Methods(
                                    resolvedType.DeclarationId,
                                    memberName,
                                    true
                                )
                                |> List.choose (fun methodDefinition ->
                                    tryInferReferenceStaticMethod
                                        (CliNamedType methodDefinition.DeclaringType)
                                        []
                                        methodDefinition
                                        argumentTypes
                                )

                            let intrinsicCandidates =
                                match
                                    resolvedType.TypeName.Namespace,
                                    resolvedType.TypeName.Name,
                                    memberName,
                                    argumentTypes
                                with
                                | "System", "Object", "ReferenceEquals", [ CliObject; CliObject ]
                                | "System", "Object", "__fsharp2_isNull", [ CliObject; CliObject ] ->
                                    [
                                        {
                                            DeclaringType =
                                                CliNamedType {
                                                    DeclarationId = resolvedType.DeclarationId
                                                    AssemblyName = resolvedType.AssemblyName
                                                    TypeName = resolvedType.TypeName
                                                    IsValueType = resolvedType.IsValueType
                                                }
                                            StableId =
                                                resolvedType.DeclarationId
                                                + "|method|ReferenceEquals|generic|0|object|object|return|bool"
                                                + (if memberName = "__fsharp2_isNull" then
                                                       "|intrinsic|isNull"
                                                   else
                                                       String.Empty)
                                            Name = "ReferenceEquals"
                                            GenericArity = 0
                                            ParameterTypes = [ CliObject; CliObject ]
                                            ReturnType = CliBoolean
                                        },
                                        [],
                                        CliBoolean
                                    ]
                                | _ -> []

                            intrinsicCandidates
                            @ referenceCandidates
                            |> List.distinctBy (fun (methodReference, genericArguments, returnType) ->
                                methodReference.StableId,
                                genericArguments,
                                returnType
                            )
                    else
                        sourceCandidates

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

            let resolveStaticMethodOnType declaringType memberName argumentTypes range =
                let typeOwner =
                    match declaringType with
                    | CliNamedType typeReference -> Some(typeReference, [])
                    | CliGenericType(typeReference, typeArguments) ->
                        Some(typeReference, typeArguments)
                    | _ -> None

                match typeOwner with
                | None ->
                    diagnostic
                        range
                        $"the static member receiver '{TypeIdentity.cliType declaringType}' is not a named type"
                | Some(typeReference, typeArguments) ->
                    let indexedMethods =
                        references.Methods(typeReference.DeclarationId, memberName, true)

                    let candidates =
                        indexedMethods
                        |> List.choose (fun methodDefinition ->
                            tryInferReferenceStaticMethod
                                declaringType
                                typeArguments
                                methodDefinition
                                argumentTypes
                        )

                    match candidates with
                    | [ candidate ] -> Ok candidate
                    | [] ->
                        diagnostic
                            range
                            $"no static member '{TypeIdentity.cliType declaringType}.{memberName}' matches the argument types"
                    | _ ->
                        diagnostic
                            range
                            $"the static member call '{TypeIdentity.cliType declaringType}.{memberName}' is ambiguous"

            let resolveExplicitStaticMethod
                receiverName
                memberName
                genericArguments
                argumentTypes
                range
                =
                let visibleNamespaces =
                    parsed.Namespace
                    :: parsed.OpenedNamespaces
                    |> Set.ofList

                let visibleSourceTypes =
                    checkedSourceTypes
                    |> Seq.filter (fun sourceType ->
                        sourceType.Name = receiverName
                        && visibleNamespaces.Contains(sourceType.Namespace)
                    )

                let sourceCandidates =
                    visibleSourceTypes
                    |> Seq.collect (fun sourceType ->
                        sourceType.Methods
                        |> Seq.choose (fun methodDeclaration ->
                            if methodDeclaration.Name = memberName then
                                tryApplyExplicitSourceStaticMethod
                                    sourceType
                                    methodDeclaration
                                    genericArguments
                                    argumentTypes
                            else
                                None
                        )
                    )
                    |> Seq.toList

                let candidates =
                    if
                        visibleSourceTypes
                        |> Seq.isEmpty
                    then
                        match
                            references.Resolve(
                                parsed.Namespace,
                                parsed.OpenedNamespaces,
                                {
                                    Namespace = String.Empty
                                    Name = receiverName
                                },
                                0
                            )
                        with
                        | Error _ -> []
                        | Ok resolvedType ->
                            references.Methods(
                                resolvedType.DeclarationId,
                                memberName,
                                true
                            )
                            |> List.choose (fun methodDefinition ->
                                tryApplyExplicitReferenceStaticMethod
                                    (CliNamedType methodDefinition.DeclaringType)
                                    methodDefinition
                                    genericArguments
                                    argumentTypes
                            )
                    else
                        sourceCandidates

                match candidates with
                | [ candidate ] -> Ok candidate
                | [] ->
                    diagnostic
                        range
                        $"no visible static member '{receiverName}.{memberName}' matches the explicit type and argument types"
                | _ ->
                    diagnostic
                        range
                        $"the static member call '{receiverName}.{memberName}' is ambiguous"

            let rec typeDeclaration =
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
                                EmitHiddenEntrySequencePoint = false
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
                    | _, NullLiteral
                    | _, GenericMemberCall _
                    | _, StaticTypeMemberCall _
                    | _, BoundInstanceMember _
                    | _, MemberAssignment _
                    | _, SequentialExpression _
                    | _, FunctionApplication _
                    | _, ExpressionMemberCall _
                    | _, ExpressionMemberAccess _
                    | _, ConditionalExpression _
                    | _, ExplicitUpcastExpression _
                    | _, SequentialValueExpression _
                    | _, LocalAssignment _
                    | _, BooleanNegationExpression _
                    | _, EqualityExpression _
                    | _, TryWithExpression _
                    | _, LetExpression _
                    | _, LambdaExpression _
                    | _, UnitLambdaExpression _
                    | _, TupleExpression _
                    | _, StructTupleExpression _
                    | _, TypeConstruction _
                    | _, BindReturnFromComputation _
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
                | ParsedNestedModule declaration ->
                    let stableId =
                        (parsed.StableId, declaration.ModulePath)
                        ||> List.fold (fun parentStableId moduleName ->
                            parentStableId
                            + "/nested-module:"
                            + moduleName
                        )

                    let compiledName =
                        let key = ReferenceTypeName.simpleKey declaration.Name 0

                        if localTypes.ContainsKey(key) then
                            declaration.Name
                            + "Module"
                        else
                            declaration.Name

                    let rec toClosedCliType range =
                        function
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
                        | TypedNamedType resolvedType ->
                            Ok(
                                CliNamedType {
                                    DeclarationId = resolvedType.DeclarationId
                                    AssemblyName = resolvedType.AssemblyName
                                    TypeName = resolvedType.TypeName
                                    IsValueType = resolvedType.IsValueType
                                }
                            )
                        | TypedGenericTypeApplication(TypedNamedType resolvedType, arguments) ->
                            arguments
                            |> List.map (toClosedCliType range)
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
                        | typedType ->
                            diagnostic
                                range
                                $"the nested-module value type '{TypeIdentity.expression typedType}' is not yet supported"

                    let typeConstruction constructedType range =
                        let resolvedType =
                            constructedType
                            |> expandTypeAbbreviations Set.empty
                            |> resolveType (HashSet<string>(StringComparer.Ordinal))
                            |> Result.bind (toClosedCliType range)

                        resolvedType
                        |> Result.bind (fun constructedCliType ->
                            let declaringType, declaringTypeArguments =
                                match constructedCliType with
                                | CliNamedType typeReference -> Some typeReference, []
                                | CliGenericType(typeReference, arguments) ->
                                    Some typeReference, arguments
                                | _ -> None, []

                            let localCandidate =
                                declaringType
                                |> Option.bind (fun typeReference ->
                                    if
                                        List.isEmpty declaringTypeArguments
                                        && (checkedSourceTypes
                                            |> Seq.exists (fun sourceType ->
                                                sourceType.StableId = typeReference.DeclarationId
                                            ))
                                    then
                                        Some {
                                            DeclaringType = constructedCliType
                                            StableId =
                                                typeReference.DeclarationId
                                                + "/constructor:unit"
                                            ParameterTypes = []
                                        }
                                    else
                                        None
                                )

                            let referenceCandidates =
                                declaringType
                                |> Option.map (fun typeReference ->
                                    references.Methods(typeReference.DeclarationId, ".ctor", false)
                                    |> List.choose (fun constructor ->
                                        if
                                            constructor.GenericArity = 0
                                            && List.isEmpty constructor.ParameterTypes
                                        then
                                            Some {
                                                DeclaringType = constructedCliType
                                                StableId = constructor.StableId
                                                ParameterTypes = []
                                            }
                                        else
                                            None
                                    )
                                )
                                |> Option.defaultValue []

                            match localCandidate, referenceCandidates with
                            | Some target, [] -> Ok(target, constructedCliType)
                            | None, [ target ] -> Ok(target, constructedCliType)
                            | None, [] ->
                                diagnostic
                                    range
                                    "no visible parameterless constructor matches the nested-module value"
                            | _ ->
                                diagnostic
                                    range
                                    "the nested-module value constructor is ambiguous"
                        )

                    let rec typeValues
                        (bindings: Map<string, TypedModuleValueDeclaration>)
                        (completed: TypedModuleValueDeclaration list)
                        (remainingValues: ParsedModuleValueDeclaration list)
                        =
                        match remainingValues with
                        | [] -> Ok(List.rev completed)
                        | value :: remaining when bindings |> Map.containsKey value.Name ->
                            diagnostic value.Range $"the module value '{value.Name}' is duplicated"
                        | value :: remaining ->
                            let valueStableId =
                                stableId
                                + "/value:"
                                + value.Name

                            let typedValue =
                                match value.Body with
                                | TypeConstruction(constructedType, [], argumentRange) ->
                                    typeConstruction constructedType argumentRange
                                    |> Result.map (fun (target, valueType) ->
                                        valueType,
                                        TypedModuleValueConstruction target
                                    )
                                | UnitApplication constructedTypeName ->
                                    typeConstruction
                                        (ParsedNamedType(
                                            {
                                                Namespace = String.Empty
                                                Name = constructedTypeName
                                            },
                                            value.BodyRange
                                        ))
                                        value.BodyRange
                                    |> Result.map (fun (target, valueType) ->
                                        valueType,
                                        TypedModuleValueConstruction target
                                    )
                                | ValueReference targetName ->
                                    match bindings |> Map.tryFind targetName with
                                    | Some target ->
                                        Ok(
                                            target.Type,
                                            TypedModuleValueAlias target.StableId
                                        )
                                    | None ->
                                        diagnostic
                                            value.BodyRange
                                            $"the module value '{targetName}' is not defined before this binding"
                                | _ ->
                                    diagnostic
                                        value.BodyRange
                                        "nested-module values currently require a parameterless construction or an earlier value alias"

                            match typedValue with
                            | Error error -> Error error
                            | Ok(valueType, initializer) ->
                                let typed: TypedModuleValueDeclaration = {
                                    StableId = valueStableId
                                    Name = value.Name
                                    Type = valueType
                                    Initializer = initializer
                                    ExportFingerprint =
                                        Fingerprint.parts [
                                            valueStableId
                                            "type"
                                            TypeIdentity.cliType valueType
                                        ]
                                    Range = value.Range
                                }

                                typeValues
                                    (bindings |> Map.add value.Name typed)
                                    (typed :: completed)
                                    remaining

                    let typeMethods () =
                        match declaration.Methods with
                        | [] -> Ok []
                        | methods ->
                            let syntheticObjectType =
                                ParsedObjectType {
                                    Container = OrdinaryObjectType
                                    Name =
                                        declaration.Name
                                        + "ModuleFunctions"
                                    BaseType = None
                                    Methods =
                                        methods
                                        |> List.map ParsedStaticObjectMethod
                                    ConstructorRange = declaration.Range
                                    Range = declaration.Range
                                }

                            typeDeclaration syntheticObjectType
                            |> Result.bind (fun typedDeclaration ->
                                match typedDeclaration with
                                | TypedObjectType typedObjectType ->
                                    typedObjectType.Methods
                                    |> List.map (fun methodDeclaration ->
                                        match methodDeclaration with
                                        | TypedStaticObjectMethod typedMethod -> Ok typedMethod
                                        | TypedInstanceObjectMethod _ ->
                                            diagnostic
                                                declaration.Range
                                                "a nested-module function was typed as an instance member"
                                    )
                                    |> collectResults []
                                | _ ->
                                    diagnostic
                                        declaration.Range
                                        "a nested-module function did not produce a method container"
                            )

                    let typeModules () =
                        declaration.Modules
                        |> List.map (fun nestedModule ->
                            typeDeclaration (ParsedNestedModule nestedModule)
                            |> Result.bind (fun typedDeclaration ->
                                match typedDeclaration with
                                | TypedNestedModule typedModule -> Ok typedModule
                                | _ ->
                                    diagnostic
                                        nestedModule.Range
                                        "a recursive nested module did not produce a module declaration"
                            )
                        )
                        |> collectResults []

                    match
                        typeCustomAttributes
                            stableId
                            [
                                AutoOpenAttribute
                                RequireQualifiedAccessAttribute
                            ]
                            declaration.Attributes,
                        typeValues Map.empty [] declaration.Values,
                        typeMethods (),
                        typeModules ()
                    with
                    | Error error, _, _, _
                    | _, Error error, _, _
                    | _, _, Error error, _
                    | _, _, _, Error error -> Error error
                    | Ok attributes, Ok values, Ok methods, Ok modules ->
                        Ok(
                            TypedNestedModule {
                                StableId = stableId
                                Name = declaration.Name
                                CompiledName = compiledName
                                Attributes = attributes
                                Values = values
                                Methods = methods
                                Modules = modules
                                ExportFingerprint =
                                    Fingerprint.parts [
                                        stableId
                                        "compiled-name"
                                        compiledName
                                        "attributes"

                                        yield!
                                            attributes
                                            |> List.map _.ExportFingerprint

                                        "values"

                                        yield!
                                            values
                                            |> List.map _.ExportFingerprint

                                        "methods"

                                        yield!
                                            methods
                                            |> List.map _.ExportFingerprint

                                        "modules"

                                        yield!
                                            modules
                                            |> List.map _.ExportFingerprint
                                    ]
                                Range = declaration.Range
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

                    let typeConstrainedMethod
                        (methodDeclaration: ParsedStaticMethodDeclaration)
                        =
                        let explicitTypeParametersAreUnique =
                            (methodDeclaration.TypeParameters
                             |> Set.ofList
                             |> Set.count) = methodDeclaration.TypeParameters.Length

                        let usedTypeParameterNames =
                            HashSet<string>(
                                methodDeclaration.TypeParameters,
                                StringComparer.Ordinal
                            )

                        let mutable inferredTypeParameterIndex = 0

                        let rec nextInferredTypeParameterName () =
                            let index = inferredTypeParameterIndex

                            inferredTypeParameterIndex <-
                                inferredTypeParameterIndex
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
                                nextInferredTypeParameterName ()

                        let generalizedParameters, inferredTypeParameters =
                            (([], []), methodDeclaration.Parameters)
                            ||> List.fold (fun (parameters, inferred) parameter ->
                                match parameter.Type with
                                | ParsedWildcardType range ->
                                    let name = nextInferredTypeParameterName ()

                                    ({
                                        parameter with
                                            Type = ParsedTypeParameter(name, range)
                                     }
                                     :: parameters,
                                     name
                                     :: inferred)
                                | _ -> parameter :: parameters, inferred
                            )
                            |> fun (parameters, inferred) ->
                                List.rev parameters, List.rev inferred

                        let methodTypeParameters =
                            methodDeclaration.TypeParameters
                            @ inferredTypeParameters

                        let methodParameters =
                            HashSet<string>(
                                methodTypeParameters,
                                StringComparer.Ordinal
                            )

                        let methodParameterIndex =
                            methodTypeParameters
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

                        if not explicitTypeParametersAreUnique then
                            diagnostic
                                methodDeclaration.Range
                                "method type parameters must be unique"
                        else
                            match
                                resolveMethodConstraints [] methodDeclaration.Constraints,
                                typeParameters [] generalizedParameters,
                                methodDeclaration.Body
                            with
                            | Error error, _, _
                            | _, Error error, _ -> Error error
                            | Ok constraints, Ok parameters, ValueReference name ->
                                match
                                    parameters
                                    |> List.tryFindIndex (fun parameter ->
                                        parameter.Name = name
                                    )
                                with
                                | None ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        $"the value '{name}' is not a static-member parameter"
                                | Some parameterIndex ->
                                    let returnType = parameters.[parameterIndex].Type

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
                                                if methodDeclaration.IsInline then
                                                    "inline"
                                                else
                                                    "non-inline"
                                                "generic-parameters"
                                                yield! methodTypeParameters
                                                "constraints"

                                                yield!
                                                    constraints
                                                    |> List.map
                                                        TypeIdentity.methodConstraintIdentity

                                                "parameters"
                                                yield! parameters |> List.map TypeIdentity.parameter
                                                "return"
                                                TypeIdentity.cliType returnType
                                                "parameter-reference"
                                                name
                                            ]

                                        Ok {
                                            StableId = methodStableId
                                            Name = methodDeclaration.Name
                                            IsPublic = true
                                            GenericParameters = methodTypeParameters
                                            Constraints = constraints
                                            Attributes = []
                                            Parameters = parameters
                                            ReturnType = returnType
                                            Body = TypedParameterReference parameterIndex
                                            EmitHiddenEntrySequencePoint = false
                                            ExportFingerprint = exportFingerprint
                                            Range = methodDeclaration.Range
                                        }
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
                                                    if methodDeclaration.IsInline then
                                                        "inline"
                                                    else
                                                        "non-inline"
                                                    "generic-parameters"
                                                    yield! methodTypeParameters
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
                                                GenericParameters = methodTypeParameters
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
                                                EmitHiddenEntrySequencePoint = false
                                                ExportFingerprint = exportFingerprint
                                                Range = methodDeclaration.Range
                                            }
                            | Ok _, Ok _, IntegerLiteral _
                            | Ok _, Ok _, UnitLiteral
                            | Ok _, Ok _, BooleanLiteral _
                            | Ok _, Ok _, StringLiteral _
                            | Ok _, Ok _, NullLiteral
                            | Ok _, Ok _, GenericMemberCall _
                            | Ok _, Ok _, StaticTypeMemberCall _
                            | Ok _, Ok _, AddressOfExpression _
                            | Ok _, Ok _, UnitApplication _
                            | Ok _, Ok _, BoundInstanceMember _
                            | Ok _, Ok _, MemberAssignment _
                            | Ok _, Ok _, SequentialExpression _
                            | Ok _, Ok _, FunctionApplication _
                            | Ok _, Ok _, ExpressionMemberCall _
                            | Ok _, Ok _, ExpressionMemberAccess _
                            | Ok _, Ok _, ConditionalExpression _
                            | Ok _, Ok _, ExplicitUpcastExpression _
                            | Ok _, Ok _, SequentialValueExpression _
                            | Ok _, Ok _, LocalAssignment _
                            | Ok _, Ok _, BooleanNegationExpression _
                            | Ok _, Ok _, EqualityExpression _
                            | Ok _, Ok _, TryWithExpression _
                            | Ok _, Ok _, LetExpression _
                            | Ok _, Ok _, LambdaExpression _
                            | Ok _, Ok _, UnitLambdaExpression _
                            | Ok _, Ok _, TupleExpression _
                            | Ok _, Ok _, StructTupleExpression _
                            | Ok _, Ok _, TypeConstruction _
                            | Ok _, Ok _, BindReturnFromComputation _
                            | Ok _, Ok _, ObjectExpression _
                            | Ok _, Ok _, MatchExpression _ ->
                                diagnostic
                                    methodDeclaration.BodyRange
                                    "static inline members require a constrained trait call"

                    let typeMethod (methodDeclaration: ParsedStaticMethodDeclaration) =
                        if List.isEmpty methodDeclaration.Constraints then
                            let syntheticObjectType =
                                ParsedObjectType {
                                    Container = OrdinaryObjectType
                                    Name = declaration.Name
                                    BaseType = None
                                    Methods = [ ParsedStaticObjectMethod methodDeclaration ]
                                    ConstructorRange = declaration.Range
                                    Range = declaration.Range
                                }

                            typeDeclaration syntheticObjectType
                            |> Result.bind (fun typedDeclaration ->
                                match typedDeclaration with
                                | TypedObjectType typedObjectType ->
                                    match typedObjectType.Methods with
                                    | [ TypedStaticObjectMethod typedMethod ] -> Ok typedMethod
                                    | _ ->
                                        diagnostic
                                            methodDeclaration.Range
                                            "the shared static-member checker returned an invalid method set"
                                | _ ->
                                    diagnostic
                                        methodDeclaration.Range
                                        "the shared static-member checker returned an invalid declaration"
                            )
                        else
                            typeConstrainedMethod methodDeclaration

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
                                IsPublic = declaration.IsPublic
                                Name = declaration.Name
                                Methods = methods
                                ExportFingerprint =
                                    Fingerprint.parts [
                                        if declaration.IsPublic then "public" else "internal"
                                        yield! methods |> List.map _.ExportFingerprint
                                    ]
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
                        | ParsedCurrentModuleAugmentation targetTypeName ->
                            parsed.StableId
                            + "/augmentation:"
                            + StableIdentity.qualifiedTypeName targetTypeName
                        | ParsedExtensionModule(moduleName, _, targetTypeName) ->
                            parsed.StableId
                            + "/module:"
                            + moduleName
                            + "/extension:"
                            + StableIdentity.qualifiedTypeName targetTypeName

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

                    let isFSharpFunctionTypeReference (typeReference: CliTypeReference) =
                        typeReference.AssemblyName = "FSharp.Core"
                        && typeReference.TypeName.Namespace = "Microsoft.FSharp.Core"
                        && typeReference.TypeName.Name = "FSharpFunc`2"

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

                    let inferObjectMethodParameterTypes expression =
                        let containsGenericParameter cliType =
                            let rec loop =
                                function
                                | CliTypeParameter _
                                | CliMethodTypeParameter _ -> true
                                | CliByRef elementType -> loop elementType
                                | CliGenericType(_, arguments) -> arguments |> List.exists loop
                                | CliInt32
                                | CliBoolean
                                | CliString
                                | CliObject
                                | CliNativeInt
                                | CliVoid
                                | CliNamedType _ -> false

                            loop cliType

                        let addConstraint name cliType constraints =
                            if containsGenericParameter cliType then
                                constraints
                            else
                                constraints
                                |> Map.change name (fun existing ->
                                    existing
                                    |> Option.defaultValue Set.empty
                                    |> Set.add cliType
                                    |> Some
                                )

                        let collectStaticCallConstraints
                            (genericArity: int option)
                            receiverName
                            memberName
                            (arguments: ParsedExpression list)
                            constraints
                            =
                            match
                                references.Resolve(
                                    parsed.Namespace,
                                    parsed.OpenedNamespaces,
                                    {
                                        Namespace = String.Empty
                                        Name = receiverName
                                    },
                                    0
                                )
                            with
                            | Error _ -> constraints
                            | Ok resolvedType ->
                                let candidates =
                                    references.Methods(
                                        resolvedType.DeclarationId,
                                        memberName,
                                        true
                                    )
                                    |> List.filter (fun methodDefinition ->
                                        arguments.Length
                                        >= methodDefinition.ParameterTypes.Length
                                           - methodDefinition.OptionalParameterCount
                                        && arguments.Length
                                           <= methodDefinition.ParameterTypes.Length
                                        && (genericArity
                                            |> Option.forall (fun arity ->
                                                methodDefinition.GenericArity = arity
                                            ))
                                    )

                                arguments
                                |> List.mapi (fun index argument -> index, argument)
                                |> List.fold (fun state (index, argument) ->
                                    match argument with
                                    | ValueReference parameterName ->
                                        let candidateTypes =
                                            candidates
                                            |> List.map (fun candidate ->
                                                candidate.ParameterTypes.[index]
                                            )
                                            |> List.filter (containsGenericParameter >> not)
                                            |> List.distinct

                                        match candidateTypes with
                                        | [ candidateType ] ->
                                            addConstraint parameterName candidateType state
                                        | _ -> state
                                    | _ -> state
                                ) constraints

                        let rec collect constraints =
                            function
                            | MemberCall(receiverName, memberName, arguments) ->
                                let constraints =
                                    collectStaticCallConstraints
                                        None
                                        receiverName
                                        memberName
                                        arguments
                                        constraints

                                (constraints, arguments)
                                ||> List.fold collect
                            | StaticTypeMemberCall(_, _, arguments) ->
                                (constraints, arguments)
                                ||> List.fold collect
                            | GenericMemberCall(receiverName,
                                                memberName,
                                                typeArguments,
                                                arguments) ->
                                let constraints =
                                    collectStaticCallConstraints
                                        (Some typeArguments.Length)
                                        receiverName
                                        memberName
                                        arguments
                                        constraints

                                (constraints, arguments)
                                ||> List.fold collect
                            | TypeConstruction(_, arguments, _) ->
                                (constraints, arguments)
                                ||> List.fold collect
                            | BindReturnFromComputation(_, bindings, _, returnFrom, _) ->
                                let constraints =
                                    (constraints, bindings)
                                    ||> List.fold (fun state (_, input) -> collect state input)

                                collect constraints returnFrom
                            | MemberAssignment(_, _, value)
                            | LocalAssignment(_, value)
                            | BooleanNegationExpression(value, _)
                            | ExplicitUpcastExpression(value, _) -> collect constraints value
                            | EqualityExpression(left, right, _) ->
                                let constraints = collect constraints left
                                collect constraints right
                            | SequentialExpression expressions ->
                                (constraints, expressions)
                                ||> List.fold collect
                            | SequentialValueExpression expressions ->
                                (constraints, expressions)
                                ||> List.fold (fun state (value, _) -> collect state value)
                            | FunctionApplication(functionExpression, argumentExpression) ->
                                let constraints = collect constraints functionExpression
                                collect constraints argumentExpression
                            | ExpressionMemberAccess(receiver, _) ->
                                collect constraints receiver
                            | ExpressionMemberCall(receiver, _, arguments) ->
                                let constraints = collect constraints receiver
                                (constraints, arguments) ||> List.fold collect
                            | ConditionalExpression(condition, ifTrue, ifFalse, _, _, _) ->
                                let constraints = collect constraints condition
                                let constraints = collect constraints ifTrue
                                collect constraints ifFalse
                            | TryWithExpression(body, _, handler, _, _, _, _, _) ->
                                let constraints = collect constraints body
                                collect constraints handler
                            | MatchExpression(input, clauses, _, _) ->
                                let constraints = collect constraints input

                                (constraints, clauses)
                                ||> List.fold (fun state (_, guard, body, _) ->
                                    let state =
                                        match guard with
                                        | Some(guardExpression, _) ->
                                            collect state guardExpression
                                        | None -> state

                                    collect state body
                                )
                            | LetExpression(_, _, _, value, body, _, _) ->
                                let constraints = collect constraints value
                                collect constraints body
                            | LambdaExpression(_, _, body, _)
                            | UnitLambdaExpression(body, _) -> collect constraints body
                            | TupleExpression(elements, _)
                            | StructTupleExpression(elements, _) ->
                                (constraints, elements)
                                ||> List.fold collect
                            | ObjectExpression(_, arguments, members, _) ->
                                let constraints =
                                    (constraints, arguments)
                                    ||> List.fold collect

                                (constraints, members)
                                ||> List.fold (fun state memberDeclaration ->
                                    collect state memberDeclaration.Body
                                )
                            | IntegerLiteral _
                            | UnitLiteral
                            | BooleanLiteral _
                            | StringLiteral _
                            | NullLiteral
                            | ValueReference _
                            | AddressOfExpression _
                            | UnitApplication _
                            | BoundInstanceMember _ -> constraints

                        collect Map.empty expression
                        |> Map.toList
                        |> List.choose (fun (name, candidates) ->
                            if candidates.Count = 1 then
                                candidates
                                |> Seq.exactlyOne
                                |> fun candidate -> Some(name, candidate)
                            else
                                None
                        )
                        |> Map.ofList

                    let typeObjectMethodParameters
                        methodParameterIndex
                        declaredMethodParameters
                        inferredParameterTypes
                        (parameters: ParsedParameter list)
                        =
                        parameters
                        |> List.map (fun parameter ->
                            (match parameter.Type with
                             | ParsedWildcardType _ ->
                                 match
                                     inferredParameterTypes
                                     |> Map.tryFind parameter.Name
                                 with
                                 | Some parameterType -> Ok parameterType
                                 | None ->
                                     diagnostic
                                         parameter.Range
                                         $"the type of parameter '{parameter.Name}' could not be inferred"
                             | _ ->
                                 parameter.Type
                                 |> expandTypeAbbreviations Set.empty
                                 |> resolveType declaredMethodParameters
                                 |> Result.bind (
                                     toCliType methodParameterIndex parameter.Range
                                 ))
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
                                    if completion.IsInline then "inline" else "non-inline"
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

                                    if completion.IsInline then
                                        "inline-body"
                                        TypeIdentity.inlineBody completion.Body
                                    else
                                        "non-inline-body-hidden"
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
                                EmitHiddenEntrySequencePoint =
                                    completion.EmitHiddenEntrySequencePoint
                                ExportFingerprint = exportFingerprint
                                Range = completion.Range
                            }

                    let typeStaticMethod (methodDeclaration: ParsedStaticMethodDeclaration) =
                        let expandedBodyRange =
                            InlineExpansion.sourceRange
                                methodDeclaration.BodyRange
                                methodDeclaration.Body

                        let expandedBody, emitHiddenEntrySequencePoint =
                            InlineExpansion.expand methodDeclaration.Body

                        let methodDeclaration = {
                            methodDeclaration with
                                Body = expandedBody
                                BodyRange = expandedBodyRange
                        }

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
                            let inferredParameterTypes =
                                inferObjectMethodParameterTypes methodDeclaration.Body

                            let usedTypeParameterNames =
                                HashSet<string>(
                                    collectMethodTypeParameters
                                        methodDeclaration.TypeParameters
                                        methodDeclaration.Parameters
                                        methodDeclaration.ReturnType,
                                    StringComparer.Ordinal
                                )

                            let mutable inferredTypeParameterIndex = 0

                            let rec nextInferredTypeParameterName () =
                                let index = inferredTypeParameterIndex

                                inferredTypeParameterIndex <-
                                    inferredTypeParameterIndex
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
                                    nextInferredTypeParameterName ()

                            let generalizedParameters =
                                methodDeclaration.Parameters
                                |> List.map (fun parameter ->
                                    match parameter.Type with
                                    | ParsedWildcardType range when
                                        inferredParameterTypes
                                        |> Map.containsKey parameter.Name
                                        |> not
                                        ->
                                        {
                                            parameter with
                                                Type =
                                                    ParsedTypeParameter(
                                                        nextInferredTypeParameterName (),
                                                        range
                                                    )
                                        }
                                    | _ -> parameter
                                )

                            let methodTypeParameters =
                                collectMethodTypeParameters
                                    methodDeclaration.TypeParameters
                                    generalizedParameters
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
                                    inferredParameterTypes
                                    generalizedParameters

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

                                let rewriteFunctionLambdaCaptures
                                    lambdaParameterCount
                                    (captures: TypedFunctionLambdaCapture list)
                                    expression
                                    =
                                    let rec rewrite =
                                        function
                                        | TypedParameterReference index when
                                            index >= lambdaParameterCount
                                            && index < lambdaParameterCount + captures.Length
                                            ->
                                            TypedInstanceFieldGet(
                                                TypedReceiverReference,
                                                captures.[index - lambdaParameterCount].Field
                                            )
                                        | TypedLet(localIndex,
                                                   name,
                                                   isMutable,
                                                   localType,
                                                   value,
                                                   body,
                                                   bindingRange,
                                                   bodyRange) ->
                                            TypedLet(
                                                localIndex,
                                                name,
                                                isMutable,
                                                localType,
                                                rewrite value,
                                                rewrite body,
                                                bindingRange,
                                                bodyRange
                                            )
                                        | TypedLocalAssignment(localIndex, localType, value) ->
                                            TypedLocalAssignment(localIndex, localType, rewrite value)
                                        | TypedInstanceFieldGet(receiver, field) ->
                                            TypedInstanceFieldGet(rewrite receiver, field)
                                        | TypedStaticMethodCall(target, genericArguments, arguments) ->
                                            TypedStaticMethodCall(
                                                target,
                                                genericArguments,
                                                arguments |> List.map rewrite
                                            )
                                        | TypedObjectConstruction(target, arguments) ->
                                            TypedObjectConstruction(target, arguments |> List.map rewrite)
                                        | TypedFunctionApplication(functionType,
                                                                   domainType,
                                                                   rangeType,
                                                                   functionExpression,
                                                                   argumentExpression) ->
                                            TypedFunctionApplication(
                                                functionType,
                                                domainType,
                                                rangeType,
                                                rewrite functionExpression,
                                                rewrite argumentExpression
                                            )
                                        | TypedInstanceMethodCall(target, receiver, arguments) ->
                                            TypedInstanceMethodCall(
                                                target,
                                                rewrite receiver,
                                                arguments |> List.map rewrite
                                            )
                                        | TypedConditional(condition,
                                                           ifTrue,
                                                           ifFalse,
                                                           conditionRange,
                                                           ifTrueRange,
                                                           ifFalseRange) ->
                                            TypedConditional(
                                                rewrite condition,
                                                rewrite ifTrue,
                                                rewrite ifFalse,
                                                conditionRange,
                                                ifTrueRange,
                                                ifFalseRange
                                            )
                                        | TypedUpcast(sourceType,
                                                      targetType,
                                                      targetResolvedType,
                                                      value) ->
                                            TypedUpcast(
                                                sourceType,
                                                targetType,
                                                targetResolvedType,
                                                rewrite value
                                            )
                                        | TypedSequential expressions ->
                                            TypedSequential(
                                                expressions
                                                |> List.map (fun (value, valueType, range) ->
                                                    rewrite value, valueType, range
                                                )
                                            )
                                        | TypedBooleanNegation(value, range) ->
                                            TypedBooleanNegation(rewrite value, range)
                                        | TypedEquality(left, right, range) ->
                                            TypedEquality(rewrite left, rewrite right, range)
                                        | TypedTryWith(body,
                                                       handlerLocalIndex,
                                                       handlerName,
                                                       catchType,
                                                       handler,
                                                       tryRange,
                                                       withRange,
                                                       bodyRange,
                                                       handlerRange,
                                                       range) ->
                                            TypedTryWith(
                                                rewrite body,
                                                handlerLocalIndex,
                                                handlerName,
                                                catchType,
                                                rewrite handler,
                                                tryRange,
                                                withRange,
                                                bodyRange,
                                                handlerRange,
                                                range
                                            )
                                        | TypedNullMatch(input,
                                                         inputType,
                                                         localIndex,
                                                         bindingName,
                                                         ifNull,
                                                         ifNotNull,
                                                         matchHeaderRange,
                                                         ifNullRange,
                                                         ifNotNullRange,
                                                         range) ->
                                            TypedNullMatch(
                                                rewrite input,
                                                inputType,
                                                localIndex,
                                                bindingName,
                                                rewrite ifNull,
                                                rewrite ifNotNull,
                                                matchHeaderRange,
                                                ifNullRange,
                                                ifNotNullRange,
                                                range
                                            )
                                        | TypedTypeTestMatch(input,
                                                             targetType,
                                                             localIndex,
                                                             bindingName,
                                                             guard,
                                                             ifMatched,
                                                             ifNotMatched,
                                                             matchHeaderRange,
                                                             ifMatchedRange,
                                                             ifNotMatchedRange,
                                                             range) ->
                                            TypedTypeTestMatch(
                                                rewrite input,
                                                targetType,
                                                localIndex,
                                                bindingName,
                                                (guard
                                                 |> Option.map (fun (value, guardRange) ->
                                                     rewrite value, guardRange
                                                 )),
                                                rewrite ifMatched,
                                                rewrite ifNotMatched,
                                                matchHeaderRange,
                                                ifMatchedRange,
                                                ifNotMatchedRange,
                                                range
                                            )
                                        | TypedObjectExpression(typeReference,
                                                                baseType,
                                                                constructorArguments,
                                                                members,
                                                                range) ->
                                            TypedObjectExpression(
                                                typeReference,
                                                baseType,
                                                constructorArguments |> List.map rewrite,
                                                members
                                                |> List.map (fun memberDeclaration -> {
                                                    memberDeclaration with
                                                        Body = rewrite memberDeclaration.Body
                                                }),
                                                range
                                            )
                                        | expression -> expression

                                    rewrite expression

                                let contextualFunctionCallCandidates receiverName memberName =
                                    match declaredReturnType with
                                    | None -> []
                                    | Some expectedReturnType ->
                                        match
                                            references.Resolve(
                                                parsed.Namespace,
                                                parsed.OpenedNamespaces,
                                                {
                                                    Namespace = String.Empty
                                                    Name = receiverName
                                                },
                                                0
                                            )
                                        with
                                        | Error _ -> []
                                        | Ok resolvedType ->
                                            references.Methods(
                                                resolvedType.DeclarationId,
                                                memberName,
                                                true
                                            )
                                            |> List.choose (fun methodDefinition ->
                                                let requiredParameterCount =
                                                    methodDefinition.ParameterTypes.Length
                                                    - methodDefinition.OptionalParameterCount

                                                if
                                                    requiredParameterCount > 1
                                                    || methodDefinition.ParameterTypes.Length < 1
                                                then
                                                    None
                                                else
                                                    let substitutions =
                                                        Array.create
                                                            methodDefinition.GenericArity
                                                            None

                                                    let returnTypeMatches =
                                                        inferReferenceMethodTypeArgument
                                                            []
                                                            substitutions
                                                            methodDefinition.ReturnType
                                                            expectedReturnType

                                                    if
                                                        not returnTypeMatches
                                                        || (substitutions
                                                            |> Array.exists Option.isNone)
                                                    then
                                                        None
                                                    else
                                                        let substitute =
                                                            substituteReferenceMethodTypeArguments
                                                                []
                                                                substitutions

                                                        let parameterType =
                                                            methodDefinition.ParameterTypes.Head
                                                            |> substitute

                                                        match parameterType with
                                                        | CliGenericType(functionReference,
                                                                         [ _; _ ]) when
                                                            isFSharpFunctionTypeReference
                                                                functionReference
                                                            ->
                                                            let target = {
                                                                DeclaringType =
                                                                    CliNamedType
                                                                        methodDefinition.DeclaringType
                                                                StableId =
                                                                    methodDefinition.StableId
                                                                Name = methodDefinition.Name
                                                                GenericArity =
                                                                    methodDefinition.GenericArity
                                                                ParameterTypes =
                                                                    methodDefinition.ParameterTypes
                                                                ReturnType =
                                                                    methodDefinition.ReturnType
                                                            }

                                                            Some(
                                                                target,
                                                                substitutions
                                                                |> Array.choose id
                                                                |> Array.toList,
                                                                parameterType,
                                                                substitute methodDefinition.ReturnType
                                                            )
                                                        | _ -> None
                                            )

                                let rec typeStaticExpressionFor
                                    (expressionReceiver: (string * CliType) option)
                                    (expressionParameters: TypedParameter list)
                                    localBindings
                                    nextLocalIndex
                                    expression
                                    =
                                    let typeStaticExpression =
                                        typeStaticExpressionFor
                                            expressionReceiver
                                            expressionParameters

                                    match expression with
                                    | IntegerLiteral value ->
                                        Ok(TypedIntegerLiteral value, CliInt32, nextLocalIndex)
                                    | BooleanLiteral value ->
                                        Ok(
                                            TypedIntegerLiteral(if value then 1 else 0),
                                            CliBoolean,
                                            nextLocalIndex
                                        )
                                    | StringLiteral value ->
                                        Ok(TypedStringLiteral value, CliString, nextLocalIndex)
                                    | NullLiteral ->
                                        Ok(TypedNullLiteral, CliObject, nextLocalIndex)
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
                                                expressionParameters
                                                |> List.tryFindIndex (fun parameter ->
                                                    parameter.Name = name
                                                )
                                            with
                                            | Some index ->
                                                Ok(
                                                    TypedParameterReference index,
                                                    expressionParameters.[index].Type,
                                                    nextLocalIndex
                                                )
                                            | None ->
                                                match expressionReceiver with
                                                | Some(receiverName, receiverType) when
                                                    receiverName = name
                                                    ->
                                                    Ok(
                                                        TypedReceiverReference,
                                                        receiverType,
                                                        nextLocalIndex
                                                    )
                                                | _ ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        $"the value '{name}' is not a static-member parameter, receiver, or local binding"
                                    | BindReturnFromComputation(
                                        "valueTask",
                                        [ bindingName, ValueReference sourceName ],
                                        returnKind,
                                        FunctionApplication(
                                            ValueReference binderName,
                                            ValueReference returnedName
                                        ),
                                        range
                                      ) when returnedName = bindingName ->
                                        let sourceParameterIndex =
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = sourceName
                                            )

                                        let binderParameterIndex =
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = binderName
                                            )

                                        match sourceParameterIndex, binderParameterIndex with
                                        | None, _ ->
                                            diagnostic
                                                range
                                                $"the computation source '{sourceName}' is not a method parameter"
                                        | _, None ->
                                            diagnostic
                                                range
                                                $"the computation binder '{binderName}' is not a method parameter"
                                        | Some sourceParameterIndex,
                                          Some binderParameterIndex ->
                                            let sourceType =
                                                expressionParameters.[sourceParameterIndex].Type

                                            let binderType =
                                                expressionParameters.[binderParameterIndex].Type

                                            let (|ValueTaskComputationTypes|_|) =
                                                function
                                                | CliGenericType(
                                                      valueTaskTypeReference,
                                                      [ inputType ]
                                                  ),
                                                  CliGenericType(
                                                      functionTypeReference,
                                                      [ binderInputType; binderOutputType ]
                                                  ),
                                                  returnKind when
                                                    valueTaskTypeReference.TypeName.Namespace
                                                    = "System.Threading.Tasks"
                                                    && valueTaskTypeReference.TypeName.Name
                                                       = "ValueTask`1"
                                                    && isFSharpFunctionTypeReference
                                                        functionTypeReference
                                                    && binderInputType = inputType
                                                    ->
                                                    match returnKind, binderOutputType with
                                                    | ComputationReturnFrom,
                                                      (CliGenericType(
                                                          outputValueTaskTypeReference,
                                                          [ outputType ]
                                                       ) as outputValueTaskType) when
                                                        outputValueTaskTypeReference.DeclarationId
                                                        = valueTaskTypeReference.DeclarationId
                                                        ->
                                                        Some(
                                                            valueTaskTypeReference,
                                                            inputType,
                                                            outputType,
                                                            outputValueTaskType
                                                        )
                                                    | ComputationReturn, outputType ->
                                                        Some(
                                                            valueTaskTypeReference,
                                                            inputType,
                                                            outputType,
                                                            CliGenericType(
                                                                valueTaskTypeReference,
                                                                [ outputType ]
                                                            )
                                                        )
                                                    | _ -> None
                                                | _ -> None

                                            match sourceType, binderType, returnKind with
                                            | ValueTaskComputationTypes(
                                                _,
                                                inputType,
                                                outputType,
                                                outputValueTaskType
                                              ) ->
                                                let resolveTypeReference arity namespaceName name =
                                                    resolveNamedType
                                                        arity
                                                        {
                                                            Namespace = namespaceName
                                                            Name = name
                                                        }
                                                        range
                                                    |> Result.bind (fun typedType ->
                                                        match typedType with
                                                        | TypedNamedType resolvedType ->
                                                            Ok {
                                                                DeclarationId =
                                                                    resolvedType.DeclarationId
                                                                AssemblyName =
                                                                    resolvedType.AssemblyName
                                                                TypeName = {
                                                                    Namespace =
                                                                        resolvedType.TypeName.Namespace
                                                                    Name =
                                                                        if arity = 0 then
                                                                            resolvedType.TypeName.Name
                                                                        else
                                                                            resolvedType.TypeName.Name
                                                                            + "`"
                                                                            + arity.ToString(
                                                                                CultureInfo.InvariantCulture
                                                                            )
                                                                }
                                                                IsValueType =
                                                                    resolvedType.IsValueType
                                                            }
                                                        | _ ->
                                                            diagnostic
                                                                range
                                                                $"the required type '{namespaceName}.{name}' did not resolve to a named type"
                                                    )

                                                [
                                                    resolveTypeReference
                                                        1
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        1
                                                        "System.Runtime.CompilerServices"
                                                        "TaskAwaiter"
                                                    resolveTypeReference 3 "System" "Func"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading"
                                                        "CancellationToken"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskContinuationOptions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskScheduler"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskExtensions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "OperationCanceledException"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "Exception"
                                                ]
                                                |> collectResults []
                                                |> Result.map (fun requiredTypes ->
                                                    match requiredTypes with
                                                    | [ taskTypeReference
                                                        taskAwaiterTypeReference
                                                        funcTypeReference
                                                        cancellationTokenTypeReference
                                                        taskContinuationOptionsTypeReference
                                                        taskSchedulerTypeReference
                                                        taskExtensionsTypeReference
                                                        nonGenericTaskTypeReference
                                                        operationCanceledExceptionTypeReference
                                                        exceptionTypeReference ] ->
                                                        TypedValueTaskBind {
                                                            BuilderName = "valueTask"
                                                            ReturnKind = returnKind
                                                            BinderParameterIndex =
                                                                binderParameterIndex
                                                            SourceParameterIndex =
                                                                sourceParameterIndex
                                                            InputType = inputType
                                                            OutputType = outputType
                                                            BinderType = binderType
                                                            InputValueTaskType = sourceType
                                                            OutputValueTaskType =
                                                                outputValueTaskType
                                                            TaskTypeReference =
                                                                taskTypeReference
                                                            TaskAwaiterTypeReference =
                                                                taskAwaiterTypeReference
                                                            FuncTypeReference =
                                                                funcTypeReference
                                                            CancellationTokenType =
                                                                CliNamedType
                                                                    cancellationTokenTypeReference
                                                            TaskContinuationOptionsType =
                                                                CliNamedType
                                                                    taskContinuationOptionsTypeReference
                                                            TaskSchedulerType =
                                                                CliNamedType
                                                                    taskSchedulerTypeReference
                                                            TaskExtensionsTypeReference =
                                                                taskExtensionsTypeReference
                                                            NonGenericTaskTypeReference =
                                                                nonGenericTaskTypeReference
                                                            OperationCanceledExceptionType =
                                                                CliNamedType
                                                                    operationCanceledExceptionTypeReference
                                                            ExceptionType =
                                                                CliNamedType exceptionTypeReference
                                                            Range = range
                                                        },
                                                        outputValueTaskType,
                                                        nextLocalIndex
                                                    | _ ->
                                                        invalidOp
                                                            "the value-task computation type set is incomplete"
                                                )
                                            | _ ->
                                                diagnostic
                                                    range
                                                    (match returnKind with
                                                     | ComputationReturn ->
                                                         "valueTask map requires an F# function from the source ValueTask result to the returned value"
                                                     | ComputationReturnFrom ->
                                                         "valueTask bind requires an F# function from the source ValueTask result to another ValueTask")
                                    | BindReturnFromComputation(
                                        "valueTask",
                                        [ applierBindingName, ValueReference applicableName
                                          inputBindingName, ValueReference inputName ],
                                        ComputationReturn,
                                        FunctionApplication(
                                            ValueReference returnedApplierName,
                                            ValueReference returnedInputName
                                        ),
                                        range
                                      ) when
                                        returnedApplierName = applierBindingName
                                        && returnedInputName = inputBindingName
                                        ->
                                        let applicableParameterIndex =
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = applicableName
                                            )

                                        let inputParameterIndex =
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = inputName
                                            )

                                        match applicableParameterIndex, inputParameterIndex with
                                        | None, _ ->
                                            diagnostic
                                                range
                                                $"the computation source '{applicableName}' is not a method parameter"
                                        | _, None ->
                                            diagnostic
                                                range
                                                $"the computation source '{inputName}' is not a method parameter"
                                        | Some applicableParameterIndex,
                                          Some inputParameterIndex ->
                                            let applicableValueTaskType =
                                                expressionParameters.[applicableParameterIndex].Type

                                            let inputValueTaskType =
                                                expressionParameters.[inputParameterIndex].Type

                                            match applicableValueTaskType, inputValueTaskType with
                                            | (CliGenericType(
                                                valueTaskTypeReference,
                                                [ (CliGenericType(
                                                    functionTypeReference,
                                                    [ applierInputType; outputType ]
                                                   ) as applierType) ]
                                               ) as applicableValueTaskType),
                                              (CliGenericType(
                                                  inputValueTaskTypeReference,
                                                  [ inputType ]
                                               ) as inputValueTaskType) when
                                                valueTaskTypeReference.TypeName.Namespace
                                                = "System.Threading.Tasks"
                                                && valueTaskTypeReference.TypeName.Name
                                                   = "ValueTask`1"
                                                && inputValueTaskTypeReference.DeclarationId
                                                   = valueTaskTypeReference.DeclarationId
                                                && isFSharpFunctionTypeReference
                                                    functionTypeReference
                                                && applierInputType = inputType
                                                ->
                                                let resolveTypeReference arity namespaceName name =
                                                    resolveNamedType
                                                        arity
                                                        {
                                                            Namespace = namespaceName
                                                            Name = name
                                                        }
                                                        range
                                                    |> Result.bind (fun typedType ->
                                                        match typedType with
                                                        | TypedNamedType resolvedType ->
                                                            Ok {
                                                                DeclarationId =
                                                                    resolvedType.DeclarationId
                                                                AssemblyName =
                                                                    resolvedType.AssemblyName
                                                                TypeName = {
                                                                    Namespace =
                                                                        resolvedType.TypeName.Namespace
                                                                    Name =
                                                                        if arity = 0 then
                                                                            resolvedType.TypeName.Name
                                                                        else
                                                                            resolvedType.TypeName.Name
                                                                            + "`"
                                                                            + arity.ToString(
                                                                                CultureInfo.InvariantCulture
                                                                            )
                                                                }
                                                                IsValueType =
                                                                    resolvedType.IsValueType
                                                            }
                                                        | _ ->
                                                            diagnostic
                                                                range
                                                                $"the required type '{namespaceName}.{name}' did not resolve to a named type"
                                                    )

                                                [
                                                    resolveTypeReference
                                                        1
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        1
                                                        "System.Runtime.CompilerServices"
                                                        "TaskAwaiter"
                                                    resolveTypeReference 3 "System" "Func"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading"
                                                        "CancellationToken"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskContinuationOptions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskScheduler"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskExtensions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "OperationCanceledException"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "Exception"
                                                ]
                                                |> collectResults []
                                                |> Result.map (fun requiredTypes ->
                                                    match requiredTypes with
                                                    | [ taskTypeReference
                                                        taskAwaiterTypeReference
                                                        funcTypeReference
                                                        cancellationTokenTypeReference
                                                        taskContinuationOptionsTypeReference
                                                        taskSchedulerTypeReference
                                                        taskExtensionsTypeReference
                                                        nonGenericTaskTypeReference
                                                        operationCanceledExceptionTypeReference
                                                        exceptionTypeReference ] ->
                                                        let outputValueTaskType =
                                                            CliGenericType(
                                                                valueTaskTypeReference,
                                                                [ outputType ]
                                                            )

                                                        TypedValueTaskApply {
                                                            BuilderName = "valueTask"
                                                            ApplicableParameterIndex =
                                                                applicableParameterIndex
                                                            InputParameterIndex =
                                                                inputParameterIndex
                                                            InputType = inputType
                                                            OutputType = outputType
                                                            ApplierType = applierType
                                                            ApplicableValueTaskType =
                                                                applicableValueTaskType
                                                            InputValueTaskType =
                                                                inputValueTaskType
                                                            OutputValueTaskType =
                                                                outputValueTaskType
                                                            TaskTypeReference =
                                                                taskTypeReference
                                                            TaskAwaiterTypeReference =
                                                                taskAwaiterTypeReference
                                                            FuncTypeReference =
                                                                funcTypeReference
                                                            CancellationTokenType =
                                                                CliNamedType
                                                                    cancellationTokenTypeReference
                                                            TaskContinuationOptionsType =
                                                                CliNamedType
                                                                    taskContinuationOptionsTypeReference
                                                            TaskSchedulerType =
                                                                CliNamedType
                                                                    taskSchedulerTypeReference
                                                            TaskExtensionsTypeReference =
                                                                taskExtensionsTypeReference
                                                            NonGenericTaskTypeReference =
                                                                nonGenericTaskTypeReference
                                                            OperationCanceledExceptionType =
                                                                CliNamedType
                                                                    operationCanceledExceptionTypeReference
                                                            ExceptionType =
                                                                CliNamedType exceptionTypeReference
                                                            Range = range
                                                        },
                                                        outputValueTaskType,
                                                        nextLocalIndex
                                                    | _ ->
                                                        invalidOp
                                                            "the value-task computation type set is incomplete"
                                                )
                                            | _ ->
                                                diagnostic
                                                    range
                                                    "valueTask apply requires ValueTask sources containing a compatible F# function and argument"
                                    | BindReturnFromComputation(
                                        "valueTask",
                                        [ leftBindingName, ValueReference leftName
                                          rightBindingName, ValueReference rightName ],
                                        ComputationReturn,
                                        TupleExpression(
                                            [ ValueReference returnedLeftName
                                              ValueReference returnedRightName ],
                                            _
                                        ),
                                        range
                                      ) when
                                        returnedLeftName = leftBindingName
                                        && returnedRightName = rightBindingName
                                        ->
                                        let leftParameterIndex =
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = leftName
                                            )

                                        let rightParameterIndex =
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = rightName
                                            )

                                        match leftParameterIndex, rightParameterIndex with
                                        | None, _ ->
                                            diagnostic
                                                range
                                                $"the computation source '{leftName}' is not a method parameter"
                                        | _, None ->
                                            diagnostic
                                                range
                                                $"the computation source '{rightName}' is not a method parameter"
                                        | Some leftParameterIndex, Some rightParameterIndex ->
                                            let leftValueTaskType =
                                                expressionParameters.[leftParameterIndex].Type

                                            let rightValueTaskType =
                                                expressionParameters.[rightParameterIndex].Type

                                            match leftValueTaskType, rightValueTaskType with
                                            | (CliGenericType(
                                                valueTaskTypeReference,
                                                [ leftType ]
                                               ) as leftValueTaskType),
                                              (CliGenericType(
                                                  rightValueTaskTypeReference,
                                                  [ rightType ]
                                               ) as rightValueTaskType) when
                                                valueTaskTypeReference.TypeName.Namespace
                                                = "System.Threading.Tasks"
                                                && valueTaskTypeReference.TypeName.Name
                                                   = "ValueTask`1"
                                                && rightValueTaskTypeReference.DeclarationId
                                                   = valueTaskTypeReference.DeclarationId
                                                ->
                                                let resolveTypeReference arity namespaceName name =
                                                    resolveNamedType
                                                        arity
                                                        {
                                                            Namespace = namespaceName
                                                            Name = name
                                                        }
                                                        range
                                                    |> Result.bind (fun typedType ->
                                                        match typedType with
                                                        | TypedNamedType resolvedType ->
                                                            Ok {
                                                                DeclarationId =
                                                                    resolvedType.DeclarationId
                                                                AssemblyName =
                                                                    resolvedType.AssemblyName
                                                                TypeName = {
                                                                    Namespace =
                                                                        resolvedType.TypeName.Namespace
                                                                    Name =
                                                                        if arity = 0 then
                                                                            resolvedType.TypeName.Name
                                                                        else
                                                                            resolvedType.TypeName.Name
                                                                            + "`"
                                                                            + arity.ToString(
                                                                                CultureInfo.InvariantCulture
                                                                            )
                                                                }
                                                                IsValueType =
                                                                    resolvedType.IsValueType
                                                            }
                                                        | _ ->
                                                            diagnostic
                                                                range
                                                                $"the required type '{namespaceName}.{name}' did not resolve to a named type"
                                                    )

                                                [
                                                    resolveTypeReference
                                                        1
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        1
                                                        "System.Runtime.CompilerServices"
                                                        "TaskAwaiter"
                                                    resolveTypeReference 3 "System" "Func"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading"
                                                        "CancellationToken"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskContinuationOptions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskScheduler"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskExtensions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "OperationCanceledException"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "Exception"
                                                    resolveTypeReference 2 "System" "Tuple"
                                                ]
                                                |> collectResults []
                                                |> Result.map (fun requiredTypes ->
                                                    match requiredTypes with
                                                    | [ taskTypeReference
                                                        taskAwaiterTypeReference
                                                        funcTypeReference
                                                        cancellationTokenTypeReference
                                                        taskContinuationOptionsTypeReference
                                                        taskSchedulerTypeReference
                                                        taskExtensionsTypeReference
                                                        nonGenericTaskTypeReference
                                                        operationCanceledExceptionTypeReference
                                                        exceptionTypeReference
                                                        tupleTypeReference ] ->
                                                        let tupleType =
                                                            CliGenericType(
                                                                tupleTypeReference,
                                                                [ leftType; rightType ]
                                                            )

                                                        let outputValueTaskType =
                                                            CliGenericType(
                                                                valueTaskTypeReference,
                                                                [ tupleType ]
                                                            )

                                                        TypedValueTaskZip {
                                                            BuilderName = "valueTask"
                                                            LeftParameterIndex =
                                                                leftParameterIndex
                                                            RightParameterIndex =
                                                                rightParameterIndex
                                                            LeftType = leftType
                                                            RightType = rightType
                                                            TupleType = tupleType
                                                            LeftValueTaskType =
                                                                leftValueTaskType
                                                            RightValueTaskType =
                                                                rightValueTaskType
                                                            OutputValueTaskType =
                                                                outputValueTaskType
                                                            TupleTypeReference =
                                                                tupleTypeReference
                                                            TaskTypeReference =
                                                                taskTypeReference
                                                            TaskAwaiterTypeReference =
                                                                taskAwaiterTypeReference
                                                            FuncTypeReference =
                                                                funcTypeReference
                                                            CancellationTokenType =
                                                                CliNamedType
                                                                    cancellationTokenTypeReference
                                                            TaskContinuationOptionsType =
                                                                CliNamedType
                                                                    taskContinuationOptionsTypeReference
                                                            TaskSchedulerType =
                                                                CliNamedType
                                                                    taskSchedulerTypeReference
                                                            TaskExtensionsTypeReference =
                                                                taskExtensionsTypeReference
                                                            NonGenericTaskTypeReference =
                                                                nonGenericTaskTypeReference
                                                            OperationCanceledExceptionType =
                                                                CliNamedType
                                                                    operationCanceledExceptionTypeReference
                                                            ExceptionType =
                                                                CliNamedType exceptionTypeReference
                                                            Range = range
                                                        },
                                                        outputValueTaskType,
                                                        nextLocalIndex
                                                    | _ ->
                                                        invalidOp
                                                            "the value-task zip type set is incomplete"
                                                )
                                            | _ ->
                                                diagnostic
                                                    range
                                                    "valueTask zip requires two generic ValueTask sources"
                                    | BindReturnFromComputation(
                                        "valueTask",
                                        [],
                                        ComputationReturnFrom,
                                        ValueReference sourceName,
                                        range
                                      ) ->
                                        match
                                            expressionParameters
                                            |> List.tryFindIndex (fun parameter ->
                                                parameter.Name = sourceName
                                            )
                                        with
                                        | None ->
                                            diagnostic
                                                range
                                                $"the computation source '{sourceName}' is not a method parameter"
                                        | Some sourceParameterIndex ->
                                            let sourceValueTaskType =
                                                expressionParameters.[sourceParameterIndex].Type

                                            match sourceValueTaskType with
                                            | CliNamedType sourceValueTaskTypeReference when
                                                sourceValueTaskTypeReference.TypeName.Namespace
                                                = "System.Threading.Tasks"
                                                && sourceValueTaskTypeReference.TypeName.Name
                                                   = "ValueTask"
                                                ->
                                                let resolveTypeReference arity namespaceName name =
                                                    resolveNamedType
                                                        arity
                                                        {
                                                            Namespace = namespaceName
                                                            Name = name
                                                        }
                                                        range
                                                    |> Result.bind (fun typedType ->
                                                        match typedType with
                                                        | TypedNamedType resolvedType ->
                                                            Ok {
                                                                DeclarationId =
                                                                    resolvedType.DeclarationId
                                                                AssemblyName =
                                                                    resolvedType.AssemblyName
                                                                TypeName = {
                                                                    Namespace =
                                                                        resolvedType.TypeName.Namespace
                                                                    Name =
                                                                        if arity = 0 then
                                                                            resolvedType.TypeName.Name
                                                                        else
                                                                            resolvedType.TypeName.Name
                                                                            + "`"
                                                                            + arity.ToString(
                                                                                CultureInfo.InvariantCulture
                                                                            )
                                                                }
                                                                IsValueType =
                                                                    resolvedType.IsValueType
                                                            }
                                                        | _ ->
                                                            diagnostic
                                                                range
                                                                $"the required type '{namespaceName}.{name}' did not resolve to a named type"
                                                    )

                                                [
                                                    resolveTypeReference
                                                        1
                                                        "System.Threading.Tasks"
                                                        "ValueTask"
                                                    resolveTypeReference
                                                        1
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "Task"
                                                    resolveTypeReference
                                                        0
                                                        "System.Runtime.CompilerServices"
                                                        "TaskAwaiter"
                                                    resolveTypeReference 3 "System" "Func"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading"
                                                        "CancellationToken"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskContinuationOptions"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskScheduler"
                                                    resolveTypeReference
                                                        0
                                                        "System.Threading.Tasks"
                                                        "TaskExtensions"
                                                    resolveTypeReference
                                                        0
                                                        "System"
                                                        "OperationCanceledException"
                                                    resolveTypeReference 0 "System" "Exception"
                                                ]
                                                |> collectResults []
                                                |> Result.map (fun requiredTypes ->
                                                    match requiredTypes with
                                                    | [ outputValueTaskTypeReference
                                                        taskTypeReference
                                                        nonGenericTaskTypeReference
                                                        nonGenericTaskAwaiterTypeReference
                                                        funcTypeReference
                                                        cancellationTokenTypeReference
                                                        taskContinuationOptionsTypeReference
                                                        taskSchedulerTypeReference
                                                        taskExtensionsTypeReference
                                                        operationCanceledExceptionTypeReference
                                                        exceptionTypeReference ] ->
                                                        let unitType =
                                                            CliNamedType fsharpUnitType

                                                        let outputValueTaskType =
                                                            CliGenericType(
                                                                outputValueTaskTypeReference,
                                                                [ unitType ]
                                                            )

                                                        TypedValueTaskOfUnit {
                                                            BuilderName = "valueTask"
                                                            SourceParameterIndex =
                                                                sourceParameterIndex
                                                            SourceValueTaskType =
                                                                sourceValueTaskType
                                                            UnitType = unitType
                                                            OutputValueTaskType =
                                                                outputValueTaskType
                                                            TaskTypeReference =
                                                                taskTypeReference
                                                            NonGenericTaskTypeReference =
                                                                nonGenericTaskTypeReference
                                                            NonGenericTaskAwaiterTypeReference =
                                                                nonGenericTaskAwaiterTypeReference
                                                            FuncTypeReference =
                                                                funcTypeReference
                                                            CancellationTokenType =
                                                                CliNamedType
                                                                    cancellationTokenTypeReference
                                                            TaskContinuationOptionsType =
                                                                CliNamedType
                                                                    taskContinuationOptionsTypeReference
                                                            TaskSchedulerType =
                                                                CliNamedType
                                                                    taskSchedulerTypeReference
                                                            TaskExtensionsTypeReference =
                                                                taskExtensionsTypeReference
                                                            OperationCanceledExceptionType =
                                                                CliNamedType
                                                                    operationCanceledExceptionTypeReference
                                                            ExceptionType =
                                                                CliNamedType exceptionTypeReference
                                                            Range = range
                                                        },
                                                        outputValueTaskType,
                                                        nextLocalIndex
                                                    | _ ->
                                                        invalidOp
                                                            "the value-task unit conversion type set is incomplete"
                                                )
                                            | _ ->
                                                diagnostic
                                                    range
                                                    "valueTask unit conversion requires a non-generic ValueTask source"
                                    | BindReturnFromComputation(_, _, _, _, range) ->
                                        diagnostic
                                            range
                                            "this computation expression shape is not yet supported"
                                    | UnitLambdaExpression(ValueReference captureName, lambdaRange) ->
                                        match declaredReturnType with
                                        | Some(CliGenericType(functionReference,
                                                              [ domainType; rangeType ]) as functionType) when
                                            isFSharpFunctionTypeReference functionReference
                                            ->
                                            let unitType = CliNamedType fsharpUnitType

                                            match
                                                expressionParameters
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
                                                expressionParameters.[captureParameterIndex].Type
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
                                                                lambdaRange.Start.Line
                                                            Range = lambdaRange
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
                                                expressionParameters
                                                |> List.tryFindIndex (fun parameter ->
                                                    parameter.Name = rootName
                                                )
                                                |> Option.map (fun parameterIndex ->
                                                    let parameterType =
                                                        expressionParameters.[parameterIndex].Type

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
                                    | MemberCall(receiverName, memberName, arguments) when
                                        (localBindings |> Map.containsKey receiverName)
                                        || (expressionParameters
                                            |> List.exists (fun parameter ->
                                                parameter.Name = receiverName
                                            ))
                                        || (match expressionReceiver with
                                            | Some(name, _) -> name = receiverName
                                            | None -> false)
                                        ->
                                        typeStaticExpression
                                            localBindings
                                            nextLocalIndex
                                            (ExpressionMemberCall(
                                                ValueReference receiverName,
                                                memberName,
                                                arguments
                                            ))
                                    | MemberCall(
                                        receiverName,
                                        memberName,
                                        [ LambdaExpression(
                                            parameterName,
                                            parameterAnnotation,
                                            lambdaBody,
                                            lambdaRange
                                          ) ]
                                      ) when
                                        contextualFunctionCallCandidates receiverName memberName
                                        |> List.isEmpty
                                        |> not
                                        ->
                                        match
                                            contextualFunctionCallCandidates receiverName memberName
                                        with
                                        | [ target,
                                            genericArguments,
                                            (CliGenericType(
                                                functionReference,
                                                [ parameterType; returnType ]
                                             ) as functionType),
                                            callReturnType ] when
                                            isFSharpFunctionTypeReference functionReference
                                            ->
                                            let typedParameterType =
                                                match parameterAnnotation with
                                                | None -> Ok parameterType
                                                | Some annotation ->
                                                    annotation
                                                    |> expandTypeAbbreviations Set.empty
                                                    |> resolveType declaredMethodParameters
                                                    |> Result.bind (
                                                        toCliType
                                                            methodParameterIndex
                                                            annotation.Range
                                                    )
                                                    |> Result.bind (fun annotatedType ->
                                                        if annotatedType = parameterType then
                                                            Ok parameterType
                                                        else
                                                            diagnostic
                                                                annotation.Range
                                                                "the lambda parameter type does not match the contextual F# function domain"
                                                    )

                                            match typedParameterType with
                                            | Error error -> Error error
                                            | Ok typedParameterType ->
                                                let closureStableId =
                                                    stableId
                                                    + "/method:"
                                                    + methodDeclaration.Name
                                                    + "/closure:function-lambda:"
                                                    + lambdaRange.Start.Offset.ToString(
                                                        CultureInfo.InvariantCulture
                                                    )

                                                let closureName =
                                                    methodDeclaration.Name
                                                    + "@"
                                                    + lambdaRange.Start.Line.ToString(
                                                        CultureInfo.InvariantCulture
                                                    )
                                                    + "-"
                                                    + lambdaRange.Start.Column.ToString(
                                                        CultureInfo.InvariantCulture
                                                    )

                                                let closureTypeReference = {
                                                    DeclarationId = closureStableId
                                                    AssemblyName = String.Empty
                                                    TypeName = {
                                                        Namespace = String.Empty
                                                        Name =
                                                            if List.isEmpty methodTypeParameters then
                                                                closureName
                                                            else
                                                                closureName
                                                                + "`"
                                                                + methodTypeParameters.Length.ToString(
                                                                    CultureInfo.InvariantCulture
                                                                )
                                                    }
                                                    IsValueType = false
                                                }

                                                let closureType =
                                                    match methodTypeParameters with
                                                    | [] -> CliNamedType closureTypeReference
                                                    | genericParameters ->
                                                        CliGenericType(
                                                            closureTypeReference,
                                                            genericParameters
                                                            |> List.mapi (fun index _ ->
                                                                CliMethodTypeParameter index
                                                            )
                                                        )

                                                let captures: TypedFunctionLambdaCapture list =
                                                    expressionParameters
                                                    |> List.mapi (fun index parameter ->
                                                        let fieldStableId =
                                                            closureStableId
                                                            + "/field:"
                                                            + parameter.Name

                                                        {
                                                            OuterParameterIndex = index
                                                            Name = parameter.Name
                                                            Type = parameter.Type
                                                            Field = {
                                                                DeclaringType = closureType
                                                                Name = parameter.Name
                                                                FieldType = parameter.Type
                                                                TargetStableId = Some fieldStableId
                                                            }
                                                        }
                                                    )

                                                let lambdaParameter: TypedParameter = {
                                                    Name = parameterName
                                                    Type = typedParameterType
                                                    Attributes = []
                                                }

                                                match
                                                    typeStaticExpressionFor
                                                        None
                                                        (lambdaParameter :: expressionParameters)
                                                        Map.empty
                                                        0
                                                        lambdaBody
                                                with
                                                | Error error -> Error error
                                                | Ok(_, bodyType, _) when bodyType <> returnType ->
                                                    diagnostic
                                                        lambdaRange
                                                        "the lambda body does not match the contextual F# function range"
                                                | Ok(typedBody, _, _) ->
                                                    let converterName = {
                                                        Namespace = "System"
                                                        Name = "Converter"
                                                    }

                                                    match
                                                        resolveNamedType
                                                            2
                                                            converterName
                                                            lambdaRange
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

                                                        let typedLambda =
                                                            TypedFunctionLambda {
                                                                FunctionType = functionType
                                                                ConverterType =
                                                                    CliGenericType(
                                                                        converterReference,
                                                                        [
                                                                            parameterType
                                                                            returnType
                                                                        ]
                                                                    )
                                                                ClosureType = closureType
                                                                ClosureName = closureName
                                                                ParameterName = parameterName
                                                                ParameterType = parameterType
                                                                ReturnType = returnType
                                                                Body =
                                                                    rewriteFunctionLambdaCaptures
                                                                        1
                                                                        captures
                                                                        typedBody
                                                                Captures = captures
                                                                SourceLine = lambdaRange.Start.Line
                                                                LambdaRange = lambdaRange
                                                                ConstructionRange = lambdaRange
                                                            }

                                                        Ok(
                                                            TypedStaticMethodCall(
                                                                target,
                                                                genericArguments,
                                                                [ typedLambda ]
                                                            ),
                                                            callReturnType,
                                                            nextLocalIndex
                                                        )
                                                    | Ok _ ->
                                                        diagnostic
                                                            lambdaRange
                                                            "System.Converter did not resolve to a named CLI type"
                                        | _ ->
                                            diagnostic
                                                lambdaRange
                                                "the contextual static F# function call is ambiguous"
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
                                    | StaticTypeMemberCall(receiverType,
                                                           memberName,
                                                           arguments) ->
                                        let rec typeCallArguments
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
                                                    typeCallArguments
                                                        (typedArgument :: typedArguments)
                                                        (argumentType :: argumentTypes)
                                                        nextArgumentLocalIndex
                                                        remaining

                                        let typedReceiverType =
                                            receiverType
                                            |> expandTypeAbbreviations Set.empty
                                            |> resolveType declaredMethodParameters
                                            |> Result.bind (
                                                toCliType
                                                    methodParameterIndex
                                                    methodDeclaration.BodyRange
                                            )

                                        match
                                            typedReceiverType,
                                            typeCallArguments [] [] nextLocalIndex arguments
                                        with
                                        | Error error, _
                                        | _, Error error -> Error error
                                        | Ok declaringType,
                                          Ok(typedArguments,
                                             argumentTypes,
                                             nextArgumentLocalIndex) ->
                                            match
                                                resolveStaticMethodOnType
                                                    declaringType
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
                                    | GenericMemberCall(receiverName,
                                                        memberName,
                                                        genericArguments,
                                                        arguments) ->
                                        let rec typeCallArguments
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
                                                    typeCallArguments
                                                        (typedArgument :: typedArguments)
                                                        (argumentType :: argumentTypes)
                                                        nextArgumentLocalIndex
                                                        remaining

                                        let typedGenericArguments =
                                            genericArguments
                                            |> List.map (fun genericArgument ->
                                                genericArgument
                                                |> expandTypeAbbreviations Set.empty
                                                |> resolveType declaredMethodParameters
                                                |> Result.bind (
                                                    toCliType
                                                        methodParameterIndex
                                                        methodDeclaration.BodyRange
                                                )
                                            )
                                            |> collectResults []

                                        match
                                            typedGenericArguments,
                                            typeCallArguments [] [] nextLocalIndex arguments
                                        with
                                        | Error error, _
                                        | _, Error error -> Error error
                                        | Ok typedGenericArguments,
                                          Ok(typedArguments,
                                             argumentTypes,
                                             nextArgumentLocalIndex) ->
                                            match
                                                resolveExplicitStaticMethod
                                                    receiverName
                                                    memberName
                                                    typedGenericArguments
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
                                    | StructTupleExpression(elements, range) ->
                                        let rec typeElements
                                            typedElements
                                            elementTypes
                                            elementLocalIndex
                                            =
                                            function
                                            | [] ->
                                                Ok(
                                                    List.rev typedElements,
                                                    List.rev elementTypes,
                                                    elementLocalIndex
                                                )
                                            | element :: remaining ->
                                                match
                                                    typeStaticExpression
                                                        localBindings
                                                        elementLocalIndex
                                                        element
                                                with
                                                | Error error -> Error error
                                                | Ok(typedElement,
                                                     elementType,
                                                     nextElementLocalIndex) ->
                                                    typeElements
                                                        (typedElement :: typedElements)
                                                        (elementType :: elementTypes)
                                                        nextElementLocalIndex
                                                        remaining

                                        match typeElements [] [] nextLocalIndex elements with
                                        | Error error -> Error error
                                        | Ok(typedElements, elementTypes, nextElementLocalIndex) ->
                                            let tupleTypeName = {
                                                Namespace = "System"
                                                Name = "ValueTuple"
                                            }

                                            match
                                                resolveNamedType
                                                    elementTypes.Length
                                                    tupleTypeName
                                                    range
                                            with
                                            | Error error -> Error error
                                            | Ok(TypedNamedType resolvedTupleType) ->
                                                let tupleTypeReference = {
                                                    DeclarationId =
                                                        resolvedTupleType.DeclarationId
                                                    AssemblyName = resolvedTupleType.AssemblyName
                                                    TypeName = {
                                                        Namespace =
                                                            resolvedTupleType.TypeName.Namespace
                                                        Name =
                                                            resolvedTupleType.TypeName.Name
                                                            + "`"
                                                            + elementTypes.Length.ToString(
                                                                CultureInfo.InvariantCulture
                                                            )
                                                    }
                                                    IsValueType = true
                                                }

                                                let tupleType =
                                                    CliGenericType(
                                                        tupleTypeReference,
                                                        elementTypes
                                                    )

                                                let rec substituteTupleTypes =
                                                    function
                                                    | CliTypeParameter index when
                                                        index < elementTypes.Length
                                                        ->
                                                        elementTypes.[index]
                                                    | CliGenericType(typeReference,
                                                                     arguments) ->
                                                        CliGenericType(
                                                            typeReference,
                                                            arguments
                                                            |> List.map substituteTupleTypes
                                                        )
                                                    | CliByRef elementType ->
                                                        CliByRef(
                                                            substituteTupleTypes elementType
                                                        )
                                                    | cliType -> cliType

                                                let constructors =
                                                    references.Methods(
                                                        tupleTypeReference.DeclarationId,
                                                        ".ctor",
                                                        false
                                                    )
                                                    |> List.filter (fun constructor ->
                                                        constructor.GenericArity = 0
                                                        && (constructor.ParameterTypes
                                                            |> List.map substituteTupleTypes)
                                                           = elementTypes
                                                    )

                                                match constructors with
                                                | [ constructor ] ->
                                                    Ok(
                                                        TypedObjectConstruction(
                                                            {
                                                                DeclaringType = tupleType
                                                                StableId = constructor.StableId
                                                                ParameterTypes =
                                                                    constructor.ParameterTypes
                                                            },
                                                            typedElements
                                                        ),
                                                        tupleType,
                                                        nextElementLocalIndex
                                                    )
                                                | [] ->
                                                    diagnostic
                                                        range
                                                        "no visible struct tuple constructor matches the element types"
                                                | _ ->
                                                    diagnostic
                                                        range
                                                        "the struct tuple constructor is ambiguous"
                                            | Ok _ ->
                                                diagnostic
                                                    range
                                                    "System.ValueTuple did not resolve to a named CLI type"
                                    | TypeConstruction(constructedType,
                                                       [ (LambdaExpression _ as lambda) ],
                                                       argumentRange) ->
                                        let rec collectLambdaParameters parameters expression =
                                            match expression with
                                            | LambdaExpression(name, parameterType, body, _) ->
                                                collectLambdaParameters
                                                    ((name, parameterType) :: parameters)
                                                    body
                                            | body -> List.rev parameters, body

                                        let resolvedConstructedType =
                                            constructedType
                                            |> expandTypeAbbreviations Set.empty
                                            |> resolveType declaredMethodParameters
                                            |> Result.bind (
                                                toCliType
                                                    methodParameterIndex
                                                    argumentRange
                                            )

                                        match resolvedConstructedType with
                                        | Error error -> Error error
                                        | Ok constructedCliType ->
                                            let declaringType, declaringTypeArguments =
                                                match constructedCliType with
                                                | CliNamedType typeReference ->
                                                    Some typeReference, []
                                                | CliGenericType(typeReference, typeArguments) ->
                                                    Some typeReference, typeArguments
                                                | _ -> None, []

                                            let rec substituteTypeArguments =
                                                function
                                                | CliTypeParameter index when
                                                    index
                                                    < declaringTypeArguments.Length
                                                    ->
                                                    declaringTypeArguments.[index]
                                                | CliGenericType(typeReference, typeArguments) ->
                                                    CliGenericType(
                                                        typeReference,
                                                        typeArguments
                                                        |> List.map substituteTypeArguments
                                                    )
                                                | CliByRef elementType ->
                                                    CliByRef(substituteTypeArguments elementType)
                                                | cliType -> cliType

                                            match declaringType with
                                            | Some typeReference when
                                                references.IsFSharpDelegate(
                                                    typeReference.DeclarationId
                                                )
                                                ->
                                                let invokeCandidates =
                                                    references.Methods(
                                                        typeReference.DeclarationId,
                                                        "Invoke",
                                                        false
                                                    )
                                                    |> List.filter (fun methodDefinition ->
                                                        methodDefinition.GenericArity = 0
                                                    )

                                                match invokeCandidates with
                                                | [ invokeMethod ] ->
                                                    let parameterTypes =
                                                        invokeMethod.ParameterTypes
                                                        |> List.map substituteTypeArguments

                                                    let returnType =
                                                        invokeMethod.ReturnType
                                                        |> substituteTypeArguments

                                                    let parsedParameters, lambdaBody =
                                                        collectLambdaParameters [] lambda

                                                    let lambdaRange =
                                                        match lambda with
                                                        | LambdaExpression(_, _, _, range) -> range
                                                        | _ -> argumentRange

                                                    let closureStableId =
                                                        stableId
                                                        + "/method:"
                                                        + methodDeclaration.Name
                                                        + "/closure:delegate-lambda:"
                                                        + lambdaRange.Start.Offset.ToString(
                                                            CultureInfo.InvariantCulture
                                                        )

                                                    let closureName =
                                                        methodDeclaration.Name
                                                        + "@"
                                                        + lambdaRange.Start.Line.ToString(
                                                            CultureInfo.InvariantCulture
                                                        )
                                                        + "-"
                                                        + lambdaRange.Start.Column.ToString(
                                                            CultureInfo.InvariantCulture
                                                        )

                                                    let closureTypeReference = {
                                                        DeclarationId = closureStableId
                                                        AssemblyName = String.Empty
                                                        TypeName = {
                                                            Namespace = String.Empty
                                                            Name =
                                                                if List.isEmpty methodTypeParameters then
                                                                    closureName
                                                                else
                                                                    closureName
                                                                    + "`"
                                                                    + methodTypeParameters.Length.ToString(
                                                                        CultureInfo.InvariantCulture
                                                                    )
                                                        }
                                                        IsValueType = false
                                                    }

                                                    let closureType =
                                                        match methodTypeParameters with
                                                        | [] -> CliNamedType closureTypeReference
                                                        | genericParameters ->
                                                            CliGenericType(
                                                                closureTypeReference,
                                                                genericParameters
                                                                |> List.mapi (fun index _ ->
                                                                    CliMethodTypeParameter index
                                                                )
                                                            )

                                                    let captures: TypedFunctionLambdaCapture list =
                                                        expressionParameters
                                                        |> List.mapi (fun index parameter ->
                                                            let fieldStableId =
                                                                closureStableId
                                                                + "/field:"
                                                                + parameter.Name

                                                            {
                                                                OuterParameterIndex = index
                                                                Name = parameter.Name
                                                                Type = parameter.Type
                                                                Field = {
                                                                    DeclaringType = closureType
                                                                    Name = parameter.Name
                                                                    FieldType = parameter.Type
                                                                    TargetStableId = Some fieldStableId
                                                                }
                                                            }
                                                        )

                                                    if
                                                        parsedParameters.Length
                                                        <> parameterTypes.Length
                                                    then
                                                        diagnostic
                                                            argumentRange
                                                            "the F# delegate lambda parameter count does not match Invoke"
                                                    else
                                                        let typedParameters =
                                                            (parsedParameters, parameterTypes)
                                                            ||> List.map2 (fun (name, annotation) parameterType ->
                                                                match annotation with
                                                                | None ->
                                                                    Ok({
                                                                        Name = name
                                                                        Type = parameterType
                                                                        Attributes = []
                                                                    }: TypedParameter)
                                                                | Some annotation ->
                                                                    annotation
                                                                    |> expandTypeAbbreviations Set.empty
                                                                    |> resolveType declaredMethodParameters
                                                                    |> Result.bind (
                                                                        toCliType
                                                                            methodParameterIndex
                                                                            annotation.Range
                                                                    )
                                                                    |> Result.bind (fun annotatedType ->
                                                                        if
                                                                            annotatedType
                                                                            = parameterType
                                                                        then
                                                                            Ok({
                                                                                Name = name
                                                                                Type = parameterType
                                                                                Attributes = []
                                                                            }: TypedParameter)
                                                                        else
                                                                            diagnostic
                                                                                annotation.Range
                                                                                "the lambda parameter type does not match the F# delegate Invoke parameter"
                                                                    )
                                                            )
                                                            |> collectResults []

                                                        match typedParameters with
                                                        | Error error -> Error error
                                                        | Ok typedParameters ->
                                                            match
                                                                typeStaticExpressionFor
                                                                    None
                                                                    (typedParameters
                                                                     @ expressionParameters)
                                                                    Map.empty
                                                                    0
                                                                    lambdaBody
                                                            with
                                                            | Error error -> Error error
                                                            | Ok(typedBody,
                                                                 bodyType,
                                                                 _) when
                                                                bodyType = returnType
                                                                ->
                                                                Ok(
                                                                    TypedDelegateLambda {
                                                                        DelegateType = constructedCliType
                                                                        ClosureType = closureType
                                                                        ClosureName = closureName
                                                                        Captures = captures
                                                                        LambdaParameterNames =
                                                                            typedParameters
                                                                            |> List.map _.Name
                                                                        LambdaParameterTypes = parameterTypes
                                                                        LambdaReturnType = returnType
                                                                        LambdaBody =
                                                                            rewriteFunctionLambdaCaptures
                                                                                typedParameters.Length
                                                                                captures
                                                                                typedBody
                                                                        LambdaSourceLine =
                                                                            lambdaRange.Start.Line
                                                                        LambdaRange = lambdaRange
                                                                        ConstructionRange =
                                                                            argumentRange
                                                                    },
                                                                    constructedCliType,
                                                                    nextLocalIndex
                                                                )
                                                            | Ok _ ->
                                                                diagnostic
                                                                    argumentRange
                                                                    "the lambda body does not match the F# delegate Invoke return type"
                                                | [] ->
                                                    diagnostic
                                                        argumentRange
                                                        "the F# delegate has no visible Invoke method"
                                                | _ ->
                                                    diagnostic
                                                        argumentRange
                                                        "the F# delegate Invoke method is ambiguous"
                                            | _ ->
                                                diagnostic
                                                    argumentRange
                                                    "lambda construction currently requires an F# delegate type"
                                    | TypeConstruction(constructedType,
                                                       arguments,
                                                       argumentRange) ->
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

                                        let resolvedConstructedType =
                                            constructedType
                                            |> expandTypeAbbreviations Set.empty
                                            |> resolveType declaredMethodParameters
                                            |> Result.bind (
                                                toCliType
                                                    methodParameterIndex
                                                    argumentRange
                                            )

                                        match
                                            resolvedConstructedType,
                                            typeArguments [] [] nextLocalIndex arguments
                                        with
                                        | Error error, _
                                        | _, Error error -> Error error
                                        | Ok constructedCliType,
                                          Ok(typedArguments,
                                             argumentTypes,
                                             nextArgumentLocalIndex) ->
                                            let declaringType, declaringTypeArguments =
                                                match constructedCliType with
                                                | CliNamedType typeReference ->
                                                    Some typeReference, []
                                                | CliGenericType(typeReference, typeArguments) ->
                                                    Some typeReference, typeArguments
                                                | _ -> None, []

                                            let rec substituteTypeArguments =
                                                function
                                                | CliTypeParameter index when
                                                    index
                                                    < declaringTypeArguments.Length
                                                    ->
                                                    declaringTypeArguments.[index]
                                                | CliGenericType(typeReference, typeArguments) ->
                                                    CliGenericType(
                                                        typeReference,
                                                        typeArguments
                                                        |> List.map substituteTypeArguments
                                                    )
                                                | CliByRef elementType ->
                                                    CliByRef(substituteTypeArguments elementType)
                                                | cliType -> cliType

                                            let substituteBaseTypeArguments
                                                (typeArguments: CliType list)
                                                =
                                                let rec substitute =
                                                    function
                                                    | CliTypeParameter index when
                                                        index < typeArguments.Length
                                                        ->
                                                        typeArguments.[index]
                                                    | CliGenericType(typeReference, arguments) ->
                                                        CliGenericType(
                                                            typeReference,
                                                            arguments |> List.map substitute
                                                        )
                                                    | CliByRef elementType ->
                                                        CliByRef(substitute elementType)
                                                    | cliType -> cliType

                                                substitute

                                            let rec isAssignableTo
                                                (visited: Set<string>)
                                                expectedType
                                                actualType
                                                =
                                                if expectedType = actualType then
                                                    true
                                                else
                                                    match expectedType, actualType with
                                                    | CliObject, CliNamedType typeReference
                                                    | CliObject, CliGenericType(typeReference, _) when
                                                        not typeReference.IsValueType
                                                        ->
                                                        true
                                                    | _, CliNamedType typeReference
                                                    | _, CliGenericType(typeReference, _) when
                                                        visited
                                                        |> Set.contains typeReference.DeclarationId
                                                        ->
                                                        false
                                                    | _, CliNamedType typeReference ->
                                                        references.BaseType(typeReference.DeclarationId)
                                                        |> Option.exists (
                                                            isAssignableTo
                                                                (visited
                                                                 |> Set.add typeReference.DeclarationId)
                                                                expectedType
                                                        )
                                                    | _, CliGenericType(typeReference, typeArguments) ->
                                                        references.BaseType(typeReference.DeclarationId)
                                                        |> Option.map (
                                                            substituteBaseTypeArguments typeArguments
                                                        )
                                                        |> Option.exists (
                                                            isAssignableTo
                                                                (visited
                                                                 |> Set.add typeReference.DeclarationId)
                                                                expectedType
                                                        )
                                                    | _ -> false

                                            let candidates =
                                                declaringType
                                                |> Option.map (fun typeReference ->
                                                    references.Methods(
                                                        typeReference.DeclarationId,
                                                        ".ctor",
                                                        false
                                                    )
                                                    |> List.choose (fun constructor ->
                                                        if constructor.GenericArity <> 0 then
                                                            None
                                                        else
                                                            let parameterTypes =
                                                                constructor.ParameterTypes
                                                                |> List.map substituteTypeArguments

                                                            if
                                                                parameterTypes.Length
                                                                = argumentTypes.Length
                                                                && ((parameterTypes, argumentTypes)
                                                                    ||> List.forall2 (
                                                                        isAssignableTo Set.empty
                                                                    ))
                                                            then
                                                                Some {
                                                                    DeclaringType = constructedCliType
                                                                    StableId = constructor.StableId
                                                                    ParameterTypes =
                                                                        constructor.ParameterTypes
                                                                }
                                                            else
                                                                None
                                                    )
                                                )
                                                |> Option.defaultValue []

                                            match candidates with
                                            | [ target ] ->
                                                Ok(
                                                    TypedObjectConstruction(
                                                        target,
                                                        typedArguments
                                                    ),
                                                    constructedCliType,
                                                    nextArgumentLocalIndex
                                                )
                                            | [] when
                                                List.isEmpty arguments
                                                && (match constructedCliType with
                                                    | CliNamedType typeReference
                                                    | CliGenericType(typeReference, _) ->
                                                        typeReference.IsValueType
                                                    | _ -> false)
                                                ->
                                                Ok(
                                                    TypedDefaultValue(
                                                        constructedCliType,
                                                        nextArgumentLocalIndex
                                                    ),
                                                    constructedCliType,
                                                    nextArgumentLocalIndex + 1
                                                )
                                            | [] ->
                                                diagnostic
                                                    argumentRange
                                                    "no visible object constructor matches the argument types"
                                            | _ ->
                                                diagnostic
                                                    argumentRange
                                                    "the object constructor call is ambiguous"
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
                                            isFSharpFunctionTypeReference functionType
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
                                    | BoundInstanceMember(receiverName, memberName) ->
                                        typeStaticExpression
                                            localBindings
                                            nextLocalIndex
                                            (ExpressionMemberAccess(
                                                ValueReference receiverName,
                                                memberName
                                            ))
                                    | ExpressionMemberAccess(receiver, memberName) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                receiver
                                        with
                                        | Error error -> Error error
                                        | Ok(typedReceiver,
                                             receiverType,
                                             nextReceiverLocalIndex) ->
                                            let ownerType =
                                                match receiverType with
                                                | CliByRef elementType -> elementType
                                                | cliType -> cliType

                                            let declaringType, typeArguments =
                                                match ownerType with
                                                | CliNamedType typeReference ->
                                                    Some typeReference, []
                                                | CliGenericType(typeReference, arguments) ->
                                                    Some typeReference, arguments
                                                | _ -> None, []

                                            match declaringType with
                                            | None ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    $"the expression type has no readable field or property '{memberName}'"
                                            | Some typeReference ->
                                                let sourceFieldCandidates =
                                                    checkedSourceStructs
                                                    |> Seq.choose (fun sourceStruct ->
                                                        if
                                                            sourceStruct.Declaration.StableId
                                                            <> typeReference.DeclarationId
                                                        then
                                                            None
                                                        else
                                                            sourceStruct.Declaration.Fields
                                                            |> List.tryFind (fun field ->
                                                                field.Name = memberName
                                                            )
                                                            |> Option.map (fun field ->
                                                                ({
                                                                    DeclaringType = ownerType
                                                                    Name = field.Name
                                                                    FieldType = field.Type
                                                                    TargetStableId =
                                                                        Some field.StableId
                                                                 }
                                                                 : TypedFieldAddress),
                                                                substituteTypeArguments
                                                                    typeArguments
                                                                    field.Type
                                                            )
                                                    )

                                                let referenceFieldCandidates =
                                                    references.Fields(
                                                        typeReference.DeclarationId,
                                                        memberName,
                                                        false
                                                    )
                                                    |> List.map (fun field ->
                                                        ({
                                                            DeclaringType = ownerType
                                                            Name = field.Name
                                                            FieldType = field.FieldType
                                                            TargetStableId = Some field.StableId
                                                         }
                                                         : TypedFieldAddress),
                                                        substituteTypeArguments
                                                            typeArguments
                                                            field.FieldType
                                                    )

                                                let fieldCandidates =
                                                    Seq.append
                                                        sourceFieldCandidates
                                                        referenceFieldCandidates
                                                    |> Seq.distinctBy (fun (field, _) ->
                                                        field.TargetStableId
                                                    )
                                                    |> Seq.toList

                                                match fieldCandidates with
                                                | [ field, resultType ] ->
                                                    Ok(
                                                        TypedInstanceFieldGet(
                                                            typedReceiver,
                                                            field
                                                        ),
                                                        resultType,
                                                        nextReceiverLocalIndex
                                                    )
                                                | _ :: _ ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        $"the field access '{memberName}' is ambiguous"
                                                | [] ->
                                                    let propertyCandidates =
                                                        references.Methods(
                                                            typeReference.DeclarationId,
                                                            "get_"
                                                            + memberName,
                                                            false
                                                        )
                                                        |> List.filter (fun methodDefinition ->
                                                            methodDefinition.GenericArity = 0
                                                            && List.isEmpty
                                                                methodDefinition.ParameterTypes
                                                        )
                                                        |> List.map (fun methodDefinition ->
                                                            methodDefinition,
                                                            substituteTypeArguments
                                                                typeArguments
                                                                methodDefinition.ReturnType
                                                        )
                                                        |> List.distinctBy (fun (methodDefinition, _) ->
                                                            methodDefinition.StableId
                                                        )

                                                    match propertyCandidates with
                                                    | [ methodDefinition, resultType ] ->
                                                        Ok(
                                                            TypedInstanceMethodCall(
                                                                {
                                                                    DeclaringType = ownerType
                                                                    Name = methodDefinition.Name
                                                                    ParameterTypes =
                                                                        methodDefinition.ParameterTypes
                                                                    ReturnType =
                                                                        methodDefinition.ReturnType
                                                                    ResultType = resultType
                                                                },
                                                                typedReceiver,
                                                                []
                                                            ),
                                                            resultType,
                                                            nextReceiverLocalIndex
                                                        )
                                                    | [] ->
                                                        diagnostic
                                                            methodDeclaration.BodyRange
                                                            $"the expression type has no readable field or property '{memberName}'"
                                                    | _ ->
                                                        diagnostic
                                                            methodDeclaration.BodyRange
                                                            $"the property access '{memberName}' is ambiguous"
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
                                        | Ok(typedReceiver,
                                             receiverType,
                                             nextReceiverLocalIndex) ->
                                            let ownerType =
                                                match receiverType with
                                                | CliByRef elementType -> elementType
                                                | cliType -> cliType

                                            let declaringType, declaringTypeArguments =
                                                match ownerType with
                                                | CliNamedType typeReference ->
                                                    Some typeReference, []
                                                | CliGenericType(typeReference, typeArguments) ->
                                                    Some typeReference, typeArguments
                                                | _ -> None, []

                                            let rec typeArguments'
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
                                                        typeArguments'
                                                            (typedArgument :: typedArguments)
                                                            (argumentType :: argumentTypes)
                                                            nextArgumentLocalIndex
                                                            remaining

                                            match
                                                declaringType,
                                                typeArguments'
                                                    []
                                                    []
                                                    nextReceiverLocalIndex
                                                    arguments
                                            with
                                            | None, _ ->
                                                diagnostic
                                                    methodDeclaration.BodyRange
                                                    $"the expression type has no instance member '{memberName}'"
                                            | _, Error error -> Error error
                                            | Some typeReference,
                                              Ok(typedArguments,
                                                 argumentTypes,
                                                 nextArgumentLocalIndex) ->
                                                let candidates =
                                                    references.Methods(
                                                        typeReference.DeclarationId,
                                                        memberName,
                                                        false
                                                    )
                                                    |> List.filter (fun methodDefinition ->
                                                        methodDefinition.GenericArity = 0
                                                    )
                                                    |> List.choose (fun methodDefinition ->
                                                        tryInferReferenceStaticMethod
                                                            ownerType
                                                            declaringTypeArguments
                                                            methodDefinition
                                                            argumentTypes
                                                    )
                                                    |> List.distinctBy (fun (target, _, _) ->
                                                        target.StableId
                                                    )

                                                match candidates with
                                                | [ target, _, resultType ] ->
                                                    Ok(
                                                        TypedInstanceMethodCall(
                                                            {
                                                                DeclaringType = target.DeclaringType
                                                                Name = target.Name
                                                                ParameterTypes = target.ParameterTypes
                                                                ReturnType = target.ReturnType
                                                                ResultType = resultType
                                                            },
                                                            typedReceiver,
                                                            typedArguments
                                                        ),
                                                        resultType,
                                                        nextArgumentLocalIndex
                                                    )
                                                | [] ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        $"the expression type has no matching instance member '{memberName}'"
                                                | _ ->
                                                    diagnostic
                                                        methodDeclaration.BodyRange
                                                        $"the instance member call '{memberName}' is ambiguous"
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
                                    | EqualityExpression(left, right, range) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                left
                                        with
                                        | Error error -> Error error
                                        | Ok(typedLeft, leftType, nextLeftLocalIndex) ->
                                            match
                                                typeStaticExpression
                                                    localBindings
                                                    nextLeftLocalIndex
                                                    right
                                            with
                                            | Error error -> Error error
                                            | Ok(typedRight, rightType, nextRightLocalIndex) when
                                                leftType = rightType
                                                && (leftType = CliInt32
                                                    || leftType = CliBoolean)
                                                ->
                                                Ok(
                                                    TypedEquality(typedLeft, typedRight, range),
                                                    CliBoolean,
                                                    nextRightLocalIndex
                                                )
                                            | Ok _ ->
                                                diagnostic
                                                    range
                                                    "equality currently requires operands of the same primitive type"
                                    | TryWithExpression(body,
                                                        bindingName,
                                                        handler,
                                                        tryRange,
                                                        withRange,
                                                        bodyRange,
                                                        handlerRange,
                                                        range) ->
                                        match
                                            typeStaticExpression
                                                localBindings
                                                nextLocalIndex
                                                body
                                        with
                                        | Error error -> Error error
                                        | Ok(typedBody, bodyType, nextBodyLocalIndex) ->
                                            let catchTypeResult =
                                                resolveNamedType
                                                    0
                                                    {
                                                        Namespace = "System"
                                                        Name = "Exception"
                                                    }
                                                    range
                                                |> Result.bind (
                                                    toCliType methodParameterIndex range
                                                )

                                            match catchTypeResult with
                                            | Error error -> Error error
                                            | Ok catchType ->
                                                let handlerLocalIndex = nextBodyLocalIndex

                                                match
                                                    typeStaticExpression
                                                        (localBindings
                                                         |> Map.add
                                                             bindingName
                                                             (handlerLocalIndex,
                                                              catchType,
                                                              false))
                                                        (handlerLocalIndex
                                                         + 1)
                                                        handler
                                                with
                                                | Error error -> Error error
                                                | Ok(typedHandler,
                                                     handlerType,
                                                     nextHandlerLocalIndex) when
                                                    bodyType = CliVoid
                                                    && handlerType = CliVoid
                                                    ->
                                                    Ok(
                                                        TypedTryWith(
                                                            typedBody,
                                                            handlerLocalIndex,
                                                            bindingName,
                                                            catchType,
                                                            typedHandler,
                                                            tryRange,
                                                            withRange,
                                                            bodyRange,
                                                            handlerRange,
                                                            range
                                                        ),
                                                        CliVoid,
                                                        nextHandlerLocalIndex
                                                    )
                                                | Ok(_, handlerType, _) when
                                                    bodyType <> handlerType
                                                    ->
                                                    diagnostic
                                                        range
                                                        "the try body and exception handler must have the same type"
                                                | Ok _ ->
                                                    diagnostic
                                                        range
                                                        "only unit-valued try-with expressions are currently supported"
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
                                                || (localType = CliObject
                                                    && match valueType with
                                                       | CliString
                                                       | CliObject
                                                       | CliNamedType _
                                                       | CliGenericType _ -> true
                                                       | _ -> false)
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
                                                    _,
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
                                        | [ (ParsedNullPattern _,
                                             None,
                                             ifNull,
                                             ifNullRange)
                                            (ParsedNamedPattern(bindingName, _),
                                             None,
                                             ifNotNull,
                                             ifNotNullRange) ] ->
                                            match
                                                typeStaticExpression
                                                    localBindings
                                                    nextLocalIndex
                                                    inputExpression
                                            with
                                            | Error error -> Error error
                                            | Ok(typedInput,
                                                 (CliObject | CliString | CliNamedType _ | CliGenericType _ as inputType),
                                                 nextInputLocalIndex) ->
                                                let localIndex = nextInputLocalIndex

                                                match
                                                    typeStaticExpression
                                                        localBindings
                                                        (localIndex
                                                         + 1)
                                                        ifNull
                                                with
                                                | Error error -> Error error
                                                | Ok(typedIfNull,
                                                     ifNullType,
                                                     nextNullLocalIndex) ->
                                                    let nonNullBindings =
                                                        if bindingName = "_" then
                                                            localBindings
                                                        else
                                                            localBindings
                                                            |> Map.add
                                                                bindingName
                                                                (localIndex,
                                                                 inputType,
                                                                 false)

                                                    match
                                                        typeStaticExpression
                                                            nonNullBindings
                                                            nextNullLocalIndex
                                                            ifNotNull
                                                    with
                                                    | Error error -> Error error
                                                    | Ok(typedIfNotNull,
                                                         ifNotNullType,
                                                         nextNotNullLocalIndex) when
                                                        ifNullType = ifNotNullType
                                                        ->
                                                        Ok(
                                                            TypedNullMatch(
                                                                typedInput,
                                                                inputType,
                                                                localIndex,
                                                                bindingName,
                                                                typedIfNull,
                                                                typedIfNotNull,
                                                                matchHeaderRange,
                                                                ifNullRange,
                                                                ifNotNullRange,
                                                                range
                                                            ),
                                                            ifNullType,
                                                            nextNotNullLocalIndex
                                                        )
                                                    | Ok _ ->
                                                        diagnostic
                                                            range
                                                            "the null-pattern match arms must have the same type"
                                            | Ok _ ->
                                                diagnostic
                                                    range
                                                    "the null-pattern match requires a reference type"
                                        | [ (ParsedTypeTestPattern(targetType,
                                                                   bindingName,
                                                                   patternRange),
                                             guard,
                                             ifMatched,
                                             ifMatchedRange)
                                            (ParsedNamedPattern("_", _),
                                             None,
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

                                                    let matchedBindings =
                                                        localBindings
                                                        |> Map.add
                                                            bindingName
                                                            (localIndex, targetType, false)

                                                    let typedGuardResult =
                                                        match guard with
                                                        | None -> Ok(None, localIndex + 1)
                                                        | Some(guardExpression, guardRange) ->
                                                            match
                                                                typeStaticExpression
                                                                    matchedBindings
                                                                    (localIndex + 1)
                                                                    guardExpression
                                                            with
                                                            | Error error -> Error error
                                                            | Ok(typedGuard,
                                                                 CliBoolean,
                                                                 nextGuardLocalIndex) ->
                                                                Ok(
                                                                    Some(typedGuard, guardRange),
                                                                    nextGuardLocalIndex
                                                                )
                                                            | Ok _ ->
                                                                diagnostic
                                                                    guardRange
                                                                    "a match guard must have type bool"

                                                    match typedGuardResult with
                                                    | Error error -> Error error
                                                    | Ok(typedGuard, nextGuardLocalIndex) ->
                                                        match
                                                            typeStaticExpression
                                                                matchedBindings
                                                                nextGuardLocalIndex
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
                                                                        typedGuard,
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
                                                "only a null or type-test clause followed by a named fallback clause is currently supported"
                                    | ObjectExpression(baseType,
                                                       constructorArguments,
                                                       members,
                                                       range) ->
                                        if not (List.isEmpty constructorArguments) then
                                            diagnostic
                                                range
                                                "object-expression constructor arguments are not yet supported"
                                        else
                                            match
                                                baseType
                                                |> expandTypeAbbreviations Set.empty
                                                |> resolveType declaredMethodParameters
                                            with
                                            | Error error -> Error error
                                            | Ok typedBaseType ->
                                                match
                                                    toCliType
                                                        methodParameterIndex
                                                        range
                                                        typedBaseType
                                                with
                                                | Error error -> Error error
                                                | Ok baseCliType ->
                                                    let resolvedBase =
                                                        match typedBaseType with
                                                        | TypedNamedType resolved -> Some resolved
                                                        | TypedGenericTypeApplication(
                                                            TypedNamedType resolved,
                                                            _
                                                          ) -> Some resolved
                                                        | _ -> None

                                                    let baseMethodOwner =
                                                        match baseCliType, resolvedBase with
                                                        | CliObject, Some resolved ->
                                                            Some(
                                                                {
                                                                    DeclarationId =
                                                                        resolved.DeclarationId
                                                                    AssemblyName =
                                                                        resolved.AssemblyName
                                                                    TypeName = resolved.TypeName
                                                                    IsValueType =
                                                                        resolved.IsValueType
                                                                },
                                                                []
                                                            )
                                                        | CliNamedType typeReference, _ ->
                                                            Some(typeReference, [])
                                                        | CliGenericType(typeReference, arguments), _ ->
                                                            Some(typeReference, arguments)
                                                        | _ -> None

                                                    match baseMethodOwner with
                                                    | None ->
                                                        diagnostic
                                                            range
                                                            "an object expression requires a named reference base type"
                                                    | Some(baseTypeReference, typeArguments) ->
                                                        let typeMember memberDeclaration =
                                                            let memberParameterNames =
                                                                memberDeclaration.ParameterNames

                                                            if not memberDeclaration.IsOverride then
                                                                diagnostic
                                                                    memberDeclaration.Range
                                                                    "only object-expression overrides are currently supported"
                                                            elif
                                                                (memberParameterNames
                                                                 |> Set.ofList
                                                                 |> Set.count)
                                                                <> memberParameterNames.Length
                                                            then
                                                                diagnostic
                                                                    memberDeclaration.Range
                                                                    "object-expression parameter names must be unique"
                                                            else
                                                                let indexedCandidates =
                                                                    references.Methods(
                                                                        baseTypeReference.DeclarationId,
                                                                        memberDeclaration.Name,
                                                                        false
                                                                    )
                                                                    |> List.filter (fun candidate ->
                                                                        candidate.GenericArity = 0
                                                                        && candidate.ParameterTypes.Length
                                                                           = memberParameterNames.Length
                                                                    )
                                                                    |> List.map (fun candidate ->
                                                                        let parameterTypes =
                                                                            candidate.ParameterTypes
                                                                            |> List.map (
                                                                                substituteTypeArguments
                                                                                    typeArguments
                                                                            )

                                                                        let returnType =
                                                                            candidate.ReturnType
                                                                            |> substituteTypeArguments
                                                                                typeArguments

                                                                        parameterTypes, returnType
                                                                    )

                                                                let intrinsicCandidates =
                                                                    match
                                                                        baseCliType,
                                                                        memberDeclaration.Name
                                                                    with
                                                                    | CliObject, "ToString" ->
                                                                        [ [], CliString ]
                                                                    | CliObject, "Equals" ->
                                                                        [ [ CliObject ], CliBoolean ]
                                                                    | CliObject, "GetHashCode" ->
                                                                        [ [], CliInt32 ]
                                                                    | _ -> []

                                                                let candidates =
                                                                    intrinsicCandidates
                                                                    @ indexedCandidates
                                                                    |> List.filter (fun (parameterTypes, _) ->
                                                                        List.length parameterTypes =
                                                                            memberParameterNames.Length
                                                                    )
                                                                    |> List.distinct

                                                                match candidates with
                                                                | [] ->
                                                                    diagnostic
                                                                        memberDeclaration.Range
                                                                        $"the base type has no instance member '{memberDeclaration.Name}' with {memberParameterNames.Length} parameter(s)"
                                                                | [ parameterTypes, memberReturnType ] ->
                                                                    let memberParameters: TypedParameter list =
                                                                        (memberParameterNames, parameterTypes)
                                                                        ||> List.map2 (fun name parameterType ->
                                                                            ({
                                                                                Name = name
                                                                                Type = parameterType
                                                                                Attributes = []
                                                                             }
                                                                             : TypedParameter)
                                                                        )

                                                                    match
                                                                        typeStaticExpressionFor
                                                                            (Some(
                                                                                memberDeclaration.ReceiverName,
                                                                                baseCliType
                                                                            ))
                                                                            memberParameters
                                                                            Map.empty
                                                                            0
                                                                            memberDeclaration.Body
                                                                    with
                                                                    | Error error -> Error error
                                                                    | Ok(_, inferredReturnType, _) when
                                                                        inferredReturnType
                                                                        <> memberReturnType
                                                                        ->
                                                                        diagnostic
                                                                            memberDeclaration.BodyRange
                                                                            $"the object-expression member body has type '{TypeIdentity.cliType inferredReturnType}', but the overridden member returns '{TypeIdentity.cliType memberReturnType}'"
                                                                    | Ok(typedMemberBody, _, _) ->
                                                                        Ok {
                                                                            IsOverride = true
                                                                            ReceiverName =
                                                                                memberDeclaration.ReceiverName
                                                                            Name = memberDeclaration.Name
                                                                            Parameters = memberParameters
                                                                            ReturnType = memberReturnType
                                                                            Body = typedMemberBody
                                                                            BodyRange =
                                                                                memberDeclaration.BodyRange
                                                                            Range = memberDeclaration.Range
                                                                        }
                                                                | _ ->
                                                                    diagnostic
                                                                        memberDeclaration.Range
                                                                        $"the object-expression member '{memberDeclaration.Name}' is ambiguous on the base type"

                                                        match
                                                            members
                                                            |> List.map typeMember
                                                            |> collectResults []
                                                        with
                                                        | Error error -> Error error
                                                        | Ok typedMembers ->
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

                                                            Ok(
                                                                TypedObjectExpression(
                                                                    objectTypeReference,
                                                                    baseCliType,
                                                                    [],
                                                                    typedMembers,
                                                                    range
                                                                ),
                                                                baseCliType,
                                                                nextLocalIndex
                                                            )
                                    | UnitApplication _
                                    | MemberAssignment _
                                    | SequentialExpression _
                                    | LambdaExpression _
                                    | TupleExpression _ ->
                                        diagnostic
                                            methodDeclaration.BodyRange
                                            "this static-member expression is not yet supported"

                                let typeStaticExpression =
                                    typeStaticExpressionFor None parameters

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
                                    | TypedEquality(left, right, _) ->
                                        inferredSubtypeConstraints left
                                        @ inferredSubtypeConstraints right
                                    | TypedTryWith(body, _, _, _, handler, _, _, _, _, _) ->
                                        inferredSubtypeConstraints body
                                        @ inferredSubtypeConstraints handler
                                    | TypedNullMatch(input,
                                                     _,
                                                     _,
                                                     _,
                                                     ifNull,
                                                     ifNotNull,
                                                     _,
                                                     _,
                                                     _,
                                                     _) ->
                                        inferredSubtypeConstraints input
                                        @ inferredSubtypeConstraints ifNull
                                        @ inferredSubtypeConstraints ifNotNull
                                    | TypedStaticMethodCall(_, _, arguments) ->
                                        arguments
                                        |> List.collect inferredSubtypeConstraints
                                    | TypedObjectConstruction(_, arguments) ->
                                        arguments
                                        |> List.collect inferredSubtypeConstraints
                                    | TypedFunctionApplication(_,
                                                               _,
                                                               _,
                                                               functionExpression,
                                                               argumentExpression) ->
                                        inferredSubtypeConstraints functionExpression
                                        @ inferredSubtypeConstraints argumentExpression
                                    | TypedInstanceFieldGet(receiver, _) ->
                                        inferredSubtypeConstraints receiver
                                    | TypedInstanceMethodCall(_, receiver, arguments) ->
                                        inferredSubtypeConstraints receiver
                                        @ (arguments
                                           |> List.collect inferredSubtypeConstraints)
                                    | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                                        inferredSubtypeConstraints condition
                                        @ inferredSubtypeConstraints ifTrue
                                        @ inferredSubtypeConstraints ifFalse
                                    | TypedObjectExpression(_, _, arguments, members, _) ->
                                        (arguments
                                         |> List.collect inferredSubtypeConstraints)
                                        @ (members
                                           |> List.collect (fun memberDeclaration ->
                                               inferredSubtypeConstraints memberDeclaration.Body
                                           ))
                                    | TypedTypeTestMatch(input,
                                                         _,
                                                         _,
                                                         _,
                                                         guard,
                                                         ifMatched,
                                                         ifNotMatched,
                                                         _,
                                                         _,
                                                         _,
                                                         _) ->
                                        inferredSubtypeConstraints input
                                        @ (guard
                                           |> Option.map (fst >> inferredSubtypeConstraints)
                                           |> Option.defaultValue [])
                                        @ inferredSubtypeConstraints ifMatched
                                        @ inferredSubtypeConstraints ifNotMatched
                                    | TypedSequential expressions ->
                                        expressions
                                        |> List.collect (fun (expression, _, _) ->
                                            inferredSubtypeConstraints expression
                                        )
                                    | TypedFunctionLambda expression ->
                                        inferredSubtypeConstraints expression.Body
                                    | TypedDelegateLambda expression ->
                                        inferredSubtypeConstraints expression.LambdaBody
                                    | TypedIntegerLiteral _
                                    | TypedStringLiteral _
                                    | TypedNullLiteral
                                    | TypedUnitLiteral
                                    | TypedReceiverReference
                                    | TypedParameterReference _
                                    | TypedLocalReference _
                                    | TypedAddressOf _
                                    | TypedDefaultValue _
                                    | TypedBoundInstanceMethod _
                                    | TypedUnitLambda _
                                    | TypedResumableCode _
                                    | TypedResumableTryFinally _
                                    | TypedTraitCall _ -> []
                                    | TypedValueTaskBind _
                                    | TypedValueTaskApply _
                                    | TypedValueTaskZip _
                                    | TypedValueTaskOfUnit _ -> []

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
                                            IsInline = methodDeclaration.IsInline
                                            IsPublic = true
                                            GenericParameters = methodTypeParameters
                                            Constraints =
                                                body
                                                |> inferredSubtypeConstraints
                                                |> List.distinctBy
                                                    TypeIdentity.methodConstraintIdentity
                                            ParsedAttributes = methodDeclaration.Attributes
                                            ParsedParameters = generalizedParameters
                                            Parameters = parameters
                                            ReturnType = returnType
                                            Body = body
                                            EmitHiddenEntrySequencePoint =
                                                emitHiddenEntrySequencePoint
                                            Range =
                                                match body with
                                                | TypedFunctionLambda expression ->
                                                    expression.ConstructionRange
                                                | TypedDelegateLambda expression ->
                                                    expression.ConstructionRange
                                                | _ -> methodDeclaration.BodyRange
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
                                (inferObjectMethodParameterTypes methodDeclaration.Body)
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
                                                isFSharpFunctionTypeReference functionReference
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
                                | NullLiteral -> Ok(TypedNullLiteral, CliObject)
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
                                                                [ LambdaExpression(lambdaParameter,
                                                                                   lambdaParameterType,
                                                                                   lambdaBody,
                                                                                   _) ],
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
                                | GenericMemberCall _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "explicit generic static calls are not yet supported in instance members"
                                | StaticTypeMemberCall _ ->
                                    diagnostic
                                        methodDeclaration.BodyRange
                                        "constructed-type static calls are not yet supported in instance members"
                                | TypeConstruction(constructedType,
                                                   [ LambdaExpression(lambdaParameter,
                                                                      lambdaParameterType,
                                                                      lambdaBody,
                                                                      _) ],
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
                                | ExpressionMemberAccess _
                                | ConditionalExpression _
                                | ExplicitUpcastExpression _
                                | SequentialValueExpression _
                                | LocalAssignment _
                                | BooleanNegationExpression _
                                | EqualityExpression _
                                | TryWithExpression _
                                | LetExpression _
                                | LambdaExpression _
                                | UnitLambdaExpression _
                                | TupleExpression _
                                | StructTupleExpression _
                                | TypeConstruction _
                                | BindReturnFromComputation _
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
                                    | TypedNullLiteral
                                    | TypedUnitLiteral
                                    | TypedReceiverReference
                                    | TypedParameterReference _
                                    | TypedLocalReference _
                                    | TypedLet _
                                    | TypedAddressOf _
                                    | TypedInstanceFieldGet _
                                    | TypedStaticMethodCall _
                                    | TypedObjectConstruction _
                                    | TypedDefaultValue _
                                    | TypedFunctionApplication _
                                    | TypedInstanceMethodCall _
                                    | TypedBoundInstanceMethod _
                                    | TypedUnitLambda _
                                    | TypedFunctionLambda _
                                    | TypedDelegateLambda _
                                    | TypedValueTaskBind _
                                    | TypedValueTaskApply _
                                    | TypedValueTaskZip _
                                    | TypedValueTaskOfUnit _
                                    | TypedConditional _
                                    | TypedUpcast _
                                    | TypedSequential _
                                    | TypedLocalAssignment _
                                    | TypedBooleanNegation _
                                    | TypedEquality _
                                    | TypedTryWith _
                                    | TypedResumableTryFinally _
                                    | TypedObjectExpression _
                                    | TypedNullMatch _
                                    | TypedTypeTestMatch _
                                    | TypedTraitCall _ -> methodDeclaration.BodyRange

                                finishObjectMethod {
                                    Kind = InstanceObjectMethod
                                    Name = methodDeclaration.Name
                                    IsInline = true
                                    IsPublic = methodDeclaration.IsPublic
                                    GenericParameters = methodTypeParameters
                                    Constraints = constraints
                                    ParsedAttributes = methodDeclaration.Attributes
                                    ParsedParameters = generalizedParameters
                                    Parameters = parameters
                                    ReturnType = returnType
                                    Body = body
                                    EmitHiddenEntrySequencePoint = false
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
                            | ParsedCurrentModuleAugmentation targetTypeName ->
                                resolveNamedType 0 targetTypeName declaration.ConstructorRange
                                |> Result.bind (toCliType Map.empty declaration.ConstructorRange)
                                |> Result.map TypedCurrentModuleAugmentation
                            | ParsedExtensionModule(moduleName, attributes, targetTypeName) ->
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

                        let typedBaseType =
                            match declaration.BaseType with
                            | None -> Ok None
                            | Some baseType ->
                                baseType
                                |> resolveType (HashSet<string>(StringComparer.Ordinal))
                                |> Result.bind (toCliType Map.empty declaration.ConstructorRange)
                                |> Result.map Some

                        match typedContainer, typedBaseType with
                        | Error error, _
                        | _, Error error -> Error error
                        | Ok typedContainer, Ok typedBaseType ->
                            let exportFingerprint =
                                match typedContainer with
                                | OrdinaryTypedObjectType ->
                                    Fingerprint.parts [
                                        stableId
                                        "constructor:unit"
                                        "base-type"
                                        typedBaseType
                                        |> Option.defaultValue CliObject
                                        |> TypeIdentity.cliType

                                        yield!
                                            methods
                                            |> List.map (fun methodDeclaration ->
                                                methodDeclaration.Method.ExportFingerprint
                                            )
                                    ]
                                | TypedCurrentModuleAugmentation targetType ->
                                    Fingerprint.parts [
                                        stableId
                                        "current-module-augmentation"
                                        TypeIdentity.cliType targetType

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
                                    BaseType = typedBaseType
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
                        DocumentIndex = documentIndex
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

        let rec nestedModuleContentHash (moduleDeclaration: TypedNestedModuleDeclaration) =
            Fingerprint.parts [
                moduleDeclaration.StableId
                moduleDeclaration.ExportFingerprint

                yield!
                    moduleDeclaration.Values
                    |> List.collect (fun value -> [
                        value.StableId
                        value.ExportFingerprint

                        match value.Initializer with
                        | TypedModuleValueConstruction target ->
                            "construct"
                            target.StableId
                        | TypedModuleValueAlias targetStableId ->
                            "alias"
                            targetStableId
                    ])

                yield!
                    moduleDeclaration.Methods
                    |> List.collect (fun methodDeclaration -> [
                        methodDeclaration.StableId
                        methodDeclaration.ExportFingerprint
                        methodImplementationHash methodDeclaration
                    ])

                yield!
                    moduleDeclaration.Modules
                    |> List.map nestedModuleContentHash
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
                            | TypedNestedModule moduleDeclaration ->
                                nestedModuleContentHash moduleDeclaration
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
                                    if typeDeclaration.IsPublic then "public" else "internal"

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

        let sourceModules =
            typedModules
            |> List.distinctBy _.DocumentIndex
            |> List.sortBy _.DocumentIndex

        let contentFingerprint =
            sourceModules
            |> List.map _.ContentFingerprint
            |> String.concat "|"
            |> Fingerprint.text

        let debugFingerprint =
            sourceModules
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
                sourceModules
                |> List.map (fun typed -> {
                    SchemaVersion = querySchema
                    StableId =
                        moduleStableId
                        + "/document:"
                        + typed.DocumentIndex.ToString()
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
                        | TypedNestedModule _
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

            let mapTypedParameterType (parameter: TypedParameter) = {
                parameter with
                    Type = methodTypeParametersToTypeParameters parameter.Type
            }

            let mapTypedFieldAddress (field: TypedFieldAddress) = {
                field with
                    DeclaringType = methodTypeParametersToTypeParameters field.DeclaringType
                    FieldType = methodTypeParametersToTypeParameters field.FieldType
            }

            let mapTypedStaticMethodTarget (target: TypedStaticMethodCallTarget) = {
                target with
                    DeclaringType = methodTypeParametersToTypeParameters target.DeclaringType
                    ParameterTypes =
                        target.ParameterTypes
                        |> List.map methodTypeParametersToTypeParameters
                    ReturnType = methodTypeParametersToTypeParameters target.ReturnType
            }

            let mapTypedObjectConstructionTarget (target: TypedObjectConstructionTarget) = {
                target with
                    DeclaringType = methodTypeParametersToTypeParameters target.DeclaringType
                    ParameterTypes =
                        target.ParameterTypes
                        |> List.map methodTypeParametersToTypeParameters
            }

            let mapTypedInstanceMethodTarget (target: TypedInstanceMethodCallTarget) = {
                target with
                    DeclaringType = methodTypeParametersToTypeParameters target.DeclaringType
                    ParameterTypes =
                        target.ParameterTypes
                        |> List.map methodTypeParametersToTypeParameters
                    ReturnType = methodTypeParametersToTypeParameters target.ReturnType
                    ResultType = methodTypeParametersToTypeParameters target.ResultType
            }

            let mapTypedResumableCode (expression: TypedResumableCodeExpression) = {
                expression with
                    DelegateType = methodTypeParametersToTypeParameters expression.DelegateType
                    StateMachineType =
                        methodTypeParametersToTypeParameters expression.StateMachineType
                    DataType = methodTypeParametersToTypeParameters expression.DataType
            }

            let rec methodExpressionTypesToTypeParameters =
                function
                | TypedIntegerLiteral value -> TypedIntegerLiteral value
                | TypedStringLiteral value -> TypedStringLiteral value
                | TypedNullLiteral -> TypedNullLiteral
                | TypedUnitLiteral -> TypedUnitLiteral
                | TypedReceiverReference -> TypedReceiverReference
                | TypedParameterReference index -> TypedParameterReference index
                | TypedLocalReference index -> TypedLocalReference index
                | TypedLet(localIndex,
                           name,
                           isMutable,
                           localType,
                           value,
                           body,
                           bindingRange,
                           bodyRange) ->
                    TypedLet(
                        localIndex,
                        name,
                        isMutable,
                        methodTypeParametersToTypeParameters localType,
                        methodExpressionTypesToTypeParameters value,
                        methodExpressionTypesToTypeParameters body,
                        bindingRange,
                        bodyRange
                    )
                | TypedLocalAssignment(localIndex, localType, value) ->
                    TypedLocalAssignment(
                        localIndex,
                        methodTypeParametersToTypeParameters localType,
                        methodExpressionTypesToTypeParameters value
                    )
                | TypedAddressOf(source, fields) ->
                    let source =
                        match source with
                        | TypedParameterAddress(index, parameterType) ->
                            TypedParameterAddress(
                                index,
                                methodTypeParametersToTypeParameters parameterType
                            )
                        | TypedLocalAddress(index, localType) ->
                            TypedLocalAddress(
                                index,
                                methodTypeParametersToTypeParameters localType
                            )

                    TypedAddressOf(source, fields |> List.map mapTypedFieldAddress)
                | TypedInstanceFieldGet(receiver, field) ->
                    TypedInstanceFieldGet(
                        methodExpressionTypesToTypeParameters receiver,
                        mapTypedFieldAddress field
                    )
                | TypedStaticMethodCall(target, genericArguments, arguments) ->
                    TypedStaticMethodCall(
                        mapTypedStaticMethodTarget target,
                        genericArguments
                        |> List.map methodTypeParametersToTypeParameters,
                        arguments
                        |> List.map methodExpressionTypesToTypeParameters
                    )
                | TypedObjectConstruction(target, arguments) ->
                    TypedObjectConstruction(
                        mapTypedObjectConstructionTarget target,
                        arguments
                        |> List.map methodExpressionTypesToTypeParameters
                    )
                | TypedDefaultValue(valueType, localIndex) ->
                    TypedDefaultValue(
                        methodTypeParametersToTypeParameters valueType,
                        localIndex
                    )
                | TypedFunctionApplication(functionType,
                                             domainType,
                                             rangeType,
                                             functionExpression,
                                             argumentExpression) ->
                    TypedFunctionApplication(
                        methodTypeParametersToTypeParameters functionType,
                        methodTypeParametersToTypeParameters domainType,
                        methodTypeParametersToTypeParameters rangeType,
                        methodExpressionTypesToTypeParameters functionExpression,
                        methodExpressionTypesToTypeParameters argumentExpression
                    )
                | TypedInstanceMethodCall(target, receiver, arguments) ->
                    TypedInstanceMethodCall(
                        mapTypedInstanceMethodTarget target,
                        methodExpressionTypesToTypeParameters receiver,
                        arguments
                        |> List.map methodExpressionTypesToTypeParameters
                    )
                | TypedBoundInstanceMethod expression ->
                    TypedBoundInstanceMethod {
                        expression with
                            FunctionType =
                                methodTypeParametersToTypeParameters expression.FunctionType
                            DelegateType =
                                methodTypeParametersToTypeParameters expression.DelegateType
                            ReceiverType =
                                methodTypeParametersToTypeParameters expression.ReceiverType
                            Target = mapTypedInstanceMethodTarget expression.Target
                            DomainType =
                                methodTypeParametersToTypeParameters expression.DomainType
                            RangeType =
                                methodTypeParametersToTypeParameters expression.RangeType
                    }
                | TypedUnitLambda expression ->
                    TypedUnitLambda {
                        expression with
                            FunctionType =
                                methodTypeParametersToTypeParameters expression.FunctionType
                            DelegateType =
                                methodTypeParametersToTypeParameters expression.DelegateType
                            CaptureType =
                                methodTypeParametersToTypeParameters expression.CaptureType
                            DomainType =
                                methodTypeParametersToTypeParameters expression.DomainType
                            RangeType =
                                methodTypeParametersToTypeParameters expression.RangeType
                    }
                | TypedFunctionLambda expression ->
                    TypedFunctionLambda {
                        expression with
                            FunctionType =
                                methodTypeParametersToTypeParameters expression.FunctionType
                            ConverterType =
                                methodTypeParametersToTypeParameters expression.ConverterType
                            ClosureType =
                                methodTypeParametersToTypeParameters expression.ClosureType
                            ParameterType =
                                methodTypeParametersToTypeParameters expression.ParameterType
                            ReturnType =
                                methodTypeParametersToTypeParameters expression.ReturnType
                            Body = methodExpressionTypesToTypeParameters expression.Body
                            Captures =
                                expression.Captures
                                |> List.map (fun capture -> {
                                    capture with
                                        Type =
                                            methodTypeParametersToTypeParameters capture.Type
                                        Field = mapTypedFieldAddress capture.Field
                                })
                    }
                | TypedDelegateLambda expression ->
                    TypedDelegateLambda {
                        expression with
                            DelegateType =
                                methodTypeParametersToTypeParameters expression.DelegateType
                            ClosureType =
                                methodTypeParametersToTypeParameters expression.ClosureType
                            Captures =
                                expression.Captures
                                |> List.map (fun capture -> {
                                    capture with
                                        Type =
                                            methodTypeParametersToTypeParameters capture.Type
                                        Field = mapTypedFieldAddress capture.Field
                                })
                            LambdaParameterTypes =
                                expression.LambdaParameterTypes
                                |> List.map methodTypeParametersToTypeParameters
                            LambdaReturnType =
                                methodTypeParametersToTypeParameters expression.LambdaReturnType
                            LambdaBody =
                                methodExpressionTypesToTypeParameters expression.LambdaBody
                    }
                | TypedValueTaskBind expression ->
                    TypedValueTaskBind {
                        expression with
                            InputType =
                                methodTypeParametersToTypeParameters expression.InputType
                            OutputType =
                                methodTypeParametersToTypeParameters expression.OutputType
                            BinderType =
                                methodTypeParametersToTypeParameters expression.BinderType
                            InputValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.InputValueTaskType
                            OutputValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.OutputValueTaskType
                            CancellationTokenType =
                                methodTypeParametersToTypeParameters
                                    expression.CancellationTokenType
                            TaskContinuationOptionsType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskContinuationOptionsType
                            TaskSchedulerType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskSchedulerType
                            OperationCanceledExceptionType =
                                methodTypeParametersToTypeParameters
                                    expression.OperationCanceledExceptionType
                            ExceptionType =
                                methodTypeParametersToTypeParameters expression.ExceptionType
                    }
                | TypedValueTaskApply expression ->
                    TypedValueTaskApply {
                        expression with
                            InputType =
                                methodTypeParametersToTypeParameters expression.InputType
                            OutputType =
                                methodTypeParametersToTypeParameters expression.OutputType
                            ApplierType =
                                methodTypeParametersToTypeParameters expression.ApplierType
                            ApplicableValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.ApplicableValueTaskType
                            InputValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.InputValueTaskType
                            OutputValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.OutputValueTaskType
                            CancellationTokenType =
                                methodTypeParametersToTypeParameters
                                    expression.CancellationTokenType
                            TaskContinuationOptionsType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskContinuationOptionsType
                            TaskSchedulerType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskSchedulerType
                            OperationCanceledExceptionType =
                                methodTypeParametersToTypeParameters
                                    expression.OperationCanceledExceptionType
                            ExceptionType =
                                methodTypeParametersToTypeParameters expression.ExceptionType
                    }
                | TypedValueTaskZip expression ->
                    TypedValueTaskZip {
                        expression with
                            LeftType =
                                methodTypeParametersToTypeParameters expression.LeftType
                            RightType =
                                methodTypeParametersToTypeParameters expression.RightType
                            TupleType =
                                methodTypeParametersToTypeParameters expression.TupleType
                            LeftValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.LeftValueTaskType
                            RightValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.RightValueTaskType
                            OutputValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.OutputValueTaskType
                            CancellationTokenType =
                                methodTypeParametersToTypeParameters
                                    expression.CancellationTokenType
                            TaskContinuationOptionsType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskContinuationOptionsType
                            TaskSchedulerType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskSchedulerType
                            OperationCanceledExceptionType =
                                methodTypeParametersToTypeParameters
                                    expression.OperationCanceledExceptionType
                            ExceptionType =
                                methodTypeParametersToTypeParameters expression.ExceptionType
                    }
                | TypedValueTaskOfUnit expression ->
                    TypedValueTaskOfUnit {
                        expression with
                            SourceValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.SourceValueTaskType
                            UnitType =
                                methodTypeParametersToTypeParameters expression.UnitType
                            OutputValueTaskType =
                                methodTypeParametersToTypeParameters
                                    expression.OutputValueTaskType
                            CancellationTokenType =
                                methodTypeParametersToTypeParameters
                                    expression.CancellationTokenType
                            TaskContinuationOptionsType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskContinuationOptionsType
                            TaskSchedulerType =
                                methodTypeParametersToTypeParameters
                                    expression.TaskSchedulerType
                            OperationCanceledExceptionType =
                                methodTypeParametersToTypeParameters
                                    expression.OperationCanceledExceptionType
                            ExceptionType =
                                methodTypeParametersToTypeParameters expression.ExceptionType
                    }
                | TypedConditional(condition,
                                   ifTrue,
                                   ifFalse,
                                   conditionRange,
                                   ifTrueRange,
                                   ifFalseRange) ->
                    TypedConditional(
                        methodExpressionTypesToTypeParameters condition,
                        methodExpressionTypesToTypeParameters ifTrue,
                        methodExpressionTypesToTypeParameters ifFalse,
                        conditionRange,
                        ifTrueRange,
                        ifFalseRange
                    )
                | TypedUpcast(sourceType, targetType, targetResolvedType, expression) ->
                    TypedUpcast(
                        methodTypeParametersToTypeParameters sourceType,
                        methodTypeParametersToTypeParameters targetType,
                        targetResolvedType,
                        methodExpressionTypesToTypeParameters expression
                    )
                | TypedSequential expressions ->
                    expressions
                    |> List.map (fun (expression, expressionType, range) ->
                        methodExpressionTypesToTypeParameters expression,
                        methodTypeParametersToTypeParameters expressionType,
                        range
                    )
                    |> TypedSequential
                | TypedBooleanNegation(expression, range) ->
                    TypedBooleanNegation(
                        methodExpressionTypesToTypeParameters expression,
                        range
                    )
                | TypedEquality(left, right, range) ->
                    TypedEquality(
                        methodExpressionTypesToTypeParameters left,
                        methodExpressionTypesToTypeParameters right,
                        range
                    )
                | TypedTryWith(body,
                               handlerLocalIndex,
                               handlerName,
                               catchType,
                               handler,
                               tryRange,
                               withRange,
                               bodyRange,
                               handlerRange,
                               range) ->
                    TypedTryWith(
                        methodExpressionTypesToTypeParameters body,
                        handlerLocalIndex,
                        handlerName,
                        methodTypeParametersToTypeParameters catchType,
                        methodExpressionTypesToTypeParameters handler,
                        tryRange,
                        withRange,
                        bodyRange,
                        handlerRange,
                        range
                    )
                | TypedNullMatch(input,
                                 inputType,
                                 localIndex,
                                 bindingName,
                                 ifNull,
                                 ifNotNull,
                                 matchHeaderRange,
                                 ifNullRange,
                                 ifNotNullRange,
                                 range) ->
                    TypedNullMatch(
                        methodExpressionTypesToTypeParameters input,
                        methodTypeParametersToTypeParameters inputType,
                        localIndex,
                        bindingName,
                        methodExpressionTypesToTypeParameters ifNull,
                        methodExpressionTypesToTypeParameters ifNotNull,
                        matchHeaderRange,
                        ifNullRange,
                        ifNotNullRange,
                        range
                    )
                | TypedTypeTestMatch(input,
                                     targetType,
                                     localIndex,
                                     bindingName,
                                     guard,
                                     ifMatched,
                                     ifNotMatched,
                                     matchHeaderRange,
                                     ifMatchedRange,
                                     ifNotMatchedRange,
                                     range) ->
                    TypedTypeTestMatch(
                        methodExpressionTypesToTypeParameters input,
                        methodTypeParametersToTypeParameters targetType,
                        localIndex,
                        bindingName,
                        (guard
                         |> Option.map (fun (guardExpression, guardRange) ->
                             methodExpressionTypesToTypeParameters guardExpression,
                             guardRange
                         )),
                        methodExpressionTypesToTypeParameters ifMatched,
                        methodExpressionTypesToTypeParameters ifNotMatched,
                        matchHeaderRange,
                        ifMatchedRange,
                        ifNotMatchedRange,
                        range
                    )
                | TypedResumableCode expression ->
                    expression
                    |> mapTypedResumableCode
                    |> TypedResumableCode
                | TypedResumableTryFinally expression ->
                    TypedResumableTryFinally {
                        expression with
                            ResumableCodeModuleType =
                                methodTypeParametersToTypeParameters
                                    expression.ResumableCodeModuleType
                            DelegateType =
                                methodTypeParametersToTypeParameters expression.DelegateType
                            DataType =
                                methodTypeParametersToTypeParameters expression.DataType
                            ResultType =
                                methodTypeParametersToTypeParameters expression.ResultType
                            Compensation = mapTypedResumableCode expression.Compensation
                    }
                | TypedObjectExpression(typeReference,
                                        baseType,
                                        constructorArguments,
                                        members,
                                        range) ->
                    TypedObjectExpression(
                        typeReference,
                        methodTypeParametersToTypeParameters baseType,
                        constructorArguments
                        |> List.map methodExpressionTypesToTypeParameters,
                        members
                        |> List.map (fun memberDeclaration -> {
                            memberDeclaration with
                                Parameters =
                                    memberDeclaration.Parameters
                                    |> List.map mapTypedParameterType
                                ReturnType =
                                    methodTypeParametersToTypeParameters
                                        memberDeclaration.ReturnType
                                Body =
                                    methodExpressionTypesToTypeParameters
                                        memberDeclaration.Body
                        }),
                        range
                    )
                | TypedTraitCall(receiverName, memberName, arguments) ->
                    TypedTraitCall(receiverName, memberName, arguments)

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

            let valueTaskBindHelperLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedValueTaskBindExpression)
                =
                let stableId =
                    methodDeclaration.StableId
                    + "/value-task-bind:"
                    + expression.Range.Start.Offset.ToString(CultureInfo.InvariantCulture)

                let name =
                    methodDeclaration.Name
                    + "@ValueTaskBind"
                    + expression.Range.Start.Line.ToString(CultureInfo.InvariantCulture)

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

                {| StableId = stableId
                   Name = name
                   MethodType = instantiateClosure typeReference methodArguments
                   DefinitionType = instantiateClosure typeReference definitionArguments
                   DefinitionExpression =
                       match
                           methodExpressionTypesToTypeParameters (
                               TypedValueTaskBind expression
                           )
                       with
                       | TypedValueTaskBind mapped -> mapped
                       | _ ->
                           invalidOp
                               "the value-task bind expression mapping changed its shape"
                   CompletedStableId = stableId + "/method:InvokeCompleted"
                   ContinuationStableId = stableId + "/method:Continue" |}

            let valueTaskApplyHelperLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedValueTaskApplyExpression)
                =
                let stableId =
                    methodDeclaration.StableId
                    + "/value-task-apply:"
                    + expression.Range.Start.Offset.ToString(CultureInfo.InvariantCulture)

                let name =
                    methodDeclaration.Name
                    + "@ValueTaskApply"
                    + expression.Range.Start.Line.ToString(CultureInfo.InvariantCulture)

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

                {| StableId = stableId
                   Name = name
                   MethodType = instantiateClosure typeReference methodArguments
                   DefinitionType = instantiateClosure typeReference definitionArguments
                   DefinitionExpression =
                       match
                           methodExpressionTypesToTypeParameters (
                               TypedValueTaskApply expression
                           )
                       with
                       | TypedValueTaskApply mapped -> mapped
                       | _ ->
                           invalidOp
                               "the value-task apply expression mapping changed its shape"
                   CompletedStableId = stableId + "/method:InvokeCompleted"
                   ApplicableContinuationStableId =
                       stableId
                       + "/method:ContinueApplicable"
                   InputContinuationStableId = stableId + "/method:ContinueInput" |}

            let valueTaskZipHelperLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedValueTaskZipExpression)
                =
                let stableId =
                    methodDeclaration.StableId
                    + "/value-task-zip:"
                    + expression.Range.Start.Offset.ToString(CultureInfo.InvariantCulture)

                let name =
                    methodDeclaration.Name
                    + "@ValueTaskZip"
                    + expression.Range.Start.Line.ToString(CultureInfo.InvariantCulture)

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

                {| StableId = stableId
                   Name = name
                   MethodType = instantiateClosure typeReference methodArguments
                   DefinitionType = instantiateClosure typeReference definitionArguments
                   DefinitionExpression =
                       match
                           methodExpressionTypesToTypeParameters (
                               TypedValueTaskZip expression
                           )
                       with
                       | TypedValueTaskZip mapped -> mapped
                       | _ ->
                           invalidOp
                               "the value-task zip expression mapping changed its shape"
                   CompletedStableId = stableId + "/method:InvokeCompleted"
                   LeftContinuationStableId = stableId + "/method:ContinueLeft"
                   RightContinuationStableId = stableId + "/method:ContinueRight" |}

            let valueTaskOfUnitHelperLayout
                (methodDeclaration: TypedMethodDeclaration)
                (expression: TypedValueTaskOfUnitExpression)
                =
                let stableId =
                    methodDeclaration.StableId
                    + "/value-task-of-unit:"
                    + expression.Range.Start.Offset.ToString(CultureInfo.InvariantCulture)

                let name =
                    methodDeclaration.Name
                    + "@ValueTaskOfUnit"
                    + expression.Range.Start.Line.ToString(CultureInfo.InvariantCulture)

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

                {| StableId = stableId
                   Name = name
                   MethodType = instantiateClosure typeReference methodArguments
                   DefinitionType = instantiateClosure typeReference definitionArguments
                   DefinitionExpression =
                       match
                           methodExpressionTypesToTypeParameters (
                               TypedValueTaskOfUnit expression
                           )
                       with
                       | TypedValueTaskOfUnit mapped -> mapped
                       | _ ->
                           invalidOp
                               "the value-task unit expression mapping changed its shape"
                   ContinuationStableId = stableId + "/method:Continue" |}

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
                | ModuleValueGetter
                | StaticConstructor
                | StaticTypeExtensionMember
                | StaticInlineMemberStub
                | InternalStaticInlineMemberStub
                | ClosureConstructor -> parameterIndex
                | ClosureInvoke -> parameterIndex + 1

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

            let functionLambdaConstructionInstructions
                kind
                (expression: TypedFunctionLambdaExpression)
                =
                let closureStableId =
                    match expression.ClosureType with
                    | CliNamedType typeReference
                    | CliGenericType(typeReference, _) -> typeReference.DeclarationId
                    | _ -> invalidOp "an F# function lambda closure must be a named CLI type"

                let constructor = {
                    DeclaringType = CliDeclaringType expression.ClosureType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = expression.Captures |> List.map _.Type
                    ReturnType = CliVoid
                    TargetStableId = Some(closureStableId + "/constructor")
                }

                let invoke = {
                    DeclaringType = CliDeclaringType expression.ClosureType
                    Name = "Invoke"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [ expression.ParameterType ]
                    ReturnType = expression.ReturnType
                    TargetStableId = Some(closureStableId + "/method:Invoke")
                }

                let delegateConstructor = {
                    DeclaringType = CliDeclaringType expression.ConverterType
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
                    match expression.FunctionType, expression.ConverterType with
                    | CliGenericType(functionReference, _), CliGenericType(converterReference, _) -> {
                        DeclaringType = CliDeclaringType expression.FunctionType
                        Name = "FromConverter"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            CliGenericType(
                                converterReference,
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
                            "function lambda conversion requires generic F# function and converter types"

                [
                    yield!
                        expression.Captures
                        |> List.map (fun capture ->
                            LoadArgument(
                                methodArgumentIndex kind capture.OuterParameterIndex
                            )
                        )

                    NewObject constructor
                    LoadFunctionPointer invoke
                    NewObject delegateConstructor
                    CallMethod fromConverter
                ]

            let delegateLambdaConstructionInstructions
                kind
                (expression: TypedDelegateLambdaExpression)
                =
                let closureStableId =
                    match expression.ClosureType with
                    | CliNamedType typeReference
                    | CliGenericType(typeReference, _) -> typeReference.DeclarationId
                    | _ -> invalidOp "a delegate lambda closure must be a named CLI type"

                let constructor = {
                    DeclaringType = CliDeclaringType expression.ClosureType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = expression.Captures |> List.map _.Type
                    ReturnType = CliVoid
                    TargetStableId = Some(closureStableId + "/constructor")
                }

                let invoke = {
                    DeclaringType = CliDeclaringType expression.ClosureType
                    Name = "Invoke"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = expression.LambdaParameterTypes
                    ReturnType = expression.LambdaReturnType
                    TargetStableId = Some(closureStableId + "/method:Invoke")
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
                    yield!
                        expression.Captures
                        |> List.map (fun capture ->
                            LoadArgument(
                                methodArgumentIndex kind capture.OuterParameterIndex
                            )
                        )

                    NewObject constructor
                    LoadFunctionPointer invoke
                    NewObject delegateConstructor
                ]

            let valueTaskOfUnitInstructions
                (methodDeclaration: TypedMethodDeclaration)
                kind
                (expression: TypedValueTaskOfUnitExpression)
                =
                let layout =
                    valueTaskOfUnitHelperLayout methodDeclaration expression

                let definition = layout.DefinitionExpression

                let nonGenericTaskType =
                    CliNamedType expression.NonGenericTaskTypeReference

                let definitionNonGenericTaskType =
                    CliNamedType definition.NonGenericTaskTypeReference

                let taskUnitType =
                    CliGenericType(expression.TaskTypeReference, [ expression.UnitType ])

                let definitionTaskUnitType =
                    CliGenericType(
                        definition.TaskTypeReference,
                        [ definition.UnitType ]
                    )

                let continuationHelper = {
                    DeclaringType = CliDeclaringType layout.MethodType
                    Name = "Continue"
                    GenericArity = 0
                    IsInstance = false
                    ParameterTypes = [
                        definitionNonGenericTaskType
                        CliObject
                    ]
                    ReturnType = definitionTaskUnitType
                    TargetStableId = None
                }

                let asTask = {
                    DeclaringType = CliDeclaringType expression.SourceValueTaskType
                    Name = "AsTask"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = []
                    ReturnType = nonGenericTaskType
                    TargetStableId = None
                }

                let continuationDelegateType =
                    CliGenericType(
                        expression.FuncTypeReference,
                        [
                            nonGenericTaskType
                            CliObject
                            taskUnitType
                        ]
                    )

                let continuationDelegateConstructor = {
                    DeclaringType = CliDeclaringType continuationDelegateType
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

                let continuationDefinitionDelegateType =
                    CliGenericType(
                        definition.FuncTypeReference,
                        [
                            definitionNonGenericTaskType
                            CliObject
                            CliMethodTypeParameter 0
                        ]
                    )

                let continueWith = {
                    DeclaringType = CliDeclaringType nonGenericTaskType
                    Name = "ContinueWith"
                    GenericArity = 1
                    IsInstance = true
                    ParameterTypes = [
                        continuationDefinitionDelegateType
                        CliObject
                        expression.CancellationTokenType
                        expression.TaskContinuationOptionsType
                        expression.TaskSchedulerType
                    ]
                    ReturnType =
                        CliGenericType(
                            expression.TaskTypeReference,
                            [ CliMethodTypeParameter 0 ]
                        )
                    TargetStableId = None
                }

                let cancellationTokenNone = {
                    DeclaringType = CliDeclaringType expression.CancellationTokenType
                    Name = "get_None"
                    GenericArity = 0
                    IsInstance = false
                    ParameterTypes = []
                    ReturnType = expression.CancellationTokenType
                    TargetStableId = None
                }

                let taskSchedulerDefault = {
                    DeclaringType = CliDeclaringType expression.TaskSchedulerType
                    Name = "get_Default"
                    GenericArity = 0
                    IsInstance = false
                    ParameterTypes = []
                    ReturnType = expression.TaskSchedulerType
                    TargetStableId = None
                }

                let unwrap = {
                    DeclaringType =
                        CliDeclaringType(
                            CliNamedType expression.TaskExtensionsTypeReference
                        )
                    Name = "Unwrap"
                    GenericArity = 1
                    IsInstance = false
                    ParameterTypes = [
                        CliGenericType(
                            expression.TaskTypeReference,
                            [
                                CliGenericType(
                                    expression.TaskTypeReference,
                                    [ CliMethodTypeParameter 0 ]
                                )
                            ]
                        )
                    ]
                    ReturnType =
                        CliGenericType(
                            expression.TaskTypeReference,
                            [ CliMethodTypeParameter 0 ]
                        )
                    TargetStableId = None
                }

                let valueTaskConstructor = {
                    DeclaringType = CliDeclaringType expression.OutputValueTaskType
                    Name = ".ctor"
                    GenericArity = 0
                    IsInstance = true
                    ParameterTypes = [
                        CliGenericType(
                            expression.TaskTypeReference,
                            [ CliTypeParameter 0 ]
                        )
                    ]
                    ReturnType = CliVoid
                    TargetStableId = None
                }

                [
                    LoadArgumentAddress(
                        methodArgumentIndex kind expression.SourceParameterIndex
                    )
                    CallMethod asTask
                    LoadNull
                    LoadFunctionPointer continuationHelper
                    NewObject continuationDelegateConstructor
                    LoadNull
                    CallMethod cancellationTokenNone
                    LoadInt32(
                        int
                            System.Threading.Tasks.TaskContinuationOptions.ExecuteSynchronously
                    )
                    CallMethod taskSchedulerDefault
                    CallGenericMethod(continueWith, [ taskUnitType ])
                    CallGenericMethod(unwrap, [ expression.UnitType ])
                    NewObject valueTaskConstructor
                ]
            let rec valueExpressionInstructions methodDeclaration freshLabel kind expression =
                let valueExpressionInstructions =
                    valueExpressionInstructions methodDeclaration

                match expression with
                | TypedIntegerLiteral value -> [ LoadInt32 value ], []
                | TypedStringLiteral value -> [ LoadString value ], []
                | TypedNullLiteral -> [ LoadNull ], []
                | TypedUnitLiteral -> [], []
                | TypedReceiverReference -> [ LoadArgument 0 ], []
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
                | TypedInstanceFieldGet(receiver, field) ->
                    let receiverInstructions, receiverLocals =
                        valueExpressionInstructions freshLabel kind receiver

                    receiverInstructions
                    @ [
                        LoadField {
                            DeclaringType = CliDeclaringType field.DeclaringType
                            Name = field.Name
                            FieldType = field.FieldType
                            TargetStableId = field.TargetStableId
                        }
                    ],
                    receiverLocals
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
                | TypedEquality(left, right, _) ->
                    let leftInstructions, leftLocals =
                        valueExpressionInstructions freshLabel kind left

                    let rightInstructions, rightLocals =
                        valueExpressionInstructions freshLabel kind right

                    leftInstructions
                    @ rightInstructions
                    @ [ CompareEqual ],
                    leftLocals @ rightLocals
                | TypedTryWith(body,
                               handlerLocalIndex,
                               handlerName,
                               catchType,
                               handler,
                               tryRange,
                               withRange,
                               bodyRange,
                               handlerRange,
                               _) ->
                    let tryStart = freshLabel ()
                    let tryEnd = freshLabel ()
                    let handlerStart = freshLabel ()
                    let handlerEnd = freshLabel ()
                    let endLabel = freshLabel ()

                    let bodyInstructions, bodyLocals =
                        valueExpressionInstructions freshLabel kind body

                    let handlerInstructions, handlerLocals =
                        valueExpressionInstructions freshLabel kind handler

                    let initialSequencePoint expression range =
                        match expression with
                        | TypedLet _
                        | TypedSequential _
                        | TypedConditional _
                        | TypedBooleanNegation _
                        | TypedTryWith _
                        | TypedNullMatch _
                        | TypedTypeTestMatch _ -> []
                        | _ -> [ MarkSequencePoint range ]

                    [
                        MarkLabel tryStart
                        MarkSequencePoint tryRange
                        Nop
                    ]
                    @ initialSequencePoint body bodyRange
                    @ bodyInstructions
                    @ [
                        Leave endLabel
                        MarkLabel tryEnd
                        MarkLabel handlerStart
                        StoreLocal handlerLocalIndex
                    ]
                    @ (match handler with
                       | TypedTypeTestMatch _ -> []
                       | _ -> [
                           MarkSequencePoint withRange
                           Nop
                       ])
                    @ initialSequencePoint handler handlerRange
                    @ handlerInstructions
                    @ [
                        Leave endLabel
                        MarkLabel handlerEnd
                        MarkLabel endLabel
                        DefineCatchRegion(
                            tryStart,
                            tryEnd,
                            handlerStart,
                            handlerEnd,
                            catchType
                        )
                    ],
                    bodyLocals
                    @ [
                        {
                            Index = handlerLocalIndex
                            Name = handlerName
                            Type = catchType
                        }
                    ]
                    @ handlerLocals
                | TypedLet(localIndex, name, _, localType, value, body, bindingRange, bodyRange) ->
                    let valueInstructions, valueLocals =
                        valueExpressionInstructions freshLabel kind value

                    let bodyInstructions, bodyLocals =
                        valueExpressionInstructions freshLabel kind body

                    let bodySequencePoint =
                        match body with
                        | TypedLet _
                        | TypedSequential _
                        | TypedConditional _
                        | TypedBooleanNegation _
                        | TypedTryWith _
                        | TypedNullMatch _
                        | TypedTypeTestMatch _ -> []
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

                    let omittedOptionalArguments =
                        List.replicate
                            (target.ParameterTypes.Length
                             - arguments.Length)
                            LoadNull

                    let methodReference = {
                        DeclaringType = CliDeclaringType target.DeclaringType
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

                    let intrinsicSequencePoints =
                        if target.StableId.EndsWith("|intrinsic|isNull", StringComparison.Ordinal) then
                            [
                                MarkHiddenSequencePoint
                                Nop
                                MarkHiddenSequencePoint
                                Nop
                                MarkHiddenSequencePoint
                                Nop
                            ]
                        else
                            []

                    (loweredArguments
                     |> List.collect fst)
                    @ omittedOptionalArguments
                    @ [ callInstruction ]
                    @ intrinsicSequencePoints,
                    (loweredArguments
                     |> List.collect snd)
                | TypedObjectConstruction(target, arguments) ->
                    let loweredArguments =
                        arguments
                        |> List.map (valueExpressionInstructions freshLabel kind)

                    let constructorReference = {
                        DeclaringType = CliDeclaringType target.DeclaringType
                        Name = ".ctor"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = target.ParameterTypes
                        ReturnType = CliVoid
                        TargetStableId = Some target.StableId
                    }

                    (loweredArguments
                     |> List.collect fst)
                    @ [ NewObject constructorReference ],
                    (loweredArguments
                     |> List.collect snd)
                | TypedDefaultValue(valueType, localIndex) ->
                    [
                        LoadLocalAddress localIndex
                        InitializeObject valueType
                        LoadLocal localIndex
                    ],
                    [
                        {
                            Index = localIndex
                            Name = "defaultValue"
                            Type = valueType
                        }
                    ]
                | TypedValueTaskOfUnit expression ->
                    valueTaskOfUnitInstructions methodDeclaration kind expression,
                    []
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

                    let isValueTypeReceiver =
                        match target.DeclaringType with
                        | CliNamedType typeReference
                        | CliGenericType(typeReference, _) -> typeReference.IsValueType
                        | CliInt32
                        | CliBoolean
                        | CliNativeInt -> true
                        | CliString
                        | CliObject
                        | CliVoid
                        | CliByRef _
                        | CliTypeParameter _
                        | CliMethodTypeParameter _ -> false

                    let receiverInstructions, receiverLocals, callInstruction =
                        if isValueTypeReceiver then
                            match receiver with
                            | TypedParameterReference index ->
                                [
                                    LoadArgumentAddress(
                                        methodArgumentIndex kind index
                                    )
                                ],
                                [],
                                CallMethod methodReference
                            | TypedLocalReference index ->
                                [ LoadLocalAddress index ], [], CallMethod methodReference
                            | _ ->
                                invalidOp
                                    "a value-type instance call requires an addressable receiver"
                        else
                            let receiverInstructions, receiverLocals =
                                valueExpressionInstructions freshLabel kind receiver

                            receiverInstructions,
                            receiverLocals,
                            CallVirtualMethod methodReference

                    receiverInstructions
                    @ (loweredArguments
                       |> List.collect fst)
                    @ [ callInstruction ],
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
                                | TypedUnitLiteral
                                | TypedConditional _
                                | TypedBooleanNegation _
                                | TypedLet _
                                | TypedSequential _
                                | TypedTryWith _
                                | TypedNullMatch _
                                | TypedTypeTestMatch _ -> []
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

                    let conditionInstructions, conditionLocals, conditionPrefix =
                        match condition with
                        | TypedConditional(andLeft,
                                           andRight,
                                           TypedIntegerLiteral 0,
                                           andLeftRange,
                                           andRightRange,
                                           syntheticFalseRange) when
                            syntheticFalseRange.Start = syntheticFalseRange.End
                            ->
                            let andFalseLabel = freshLabel ()
                            let andEndLabel = freshLabel ()

                            let andLeftInstructions, andLeftLocals =
                                valueExpressionInstructions freshLabel kind andLeft

                            let andRightInstructions, andRightLocals =
                                valueExpressionInstructions freshLabel kind andRight

                            [ MarkSequencePoint andLeftRange ]
                            @ andLeftInstructions
                            @ [
                                BranchIfFalse andFalseLabel
                                MarkSequencePoint andRightRange
                              ]
                            @ andRightInstructions
                            @ [
                                Branch andEndLabel
                                MarkLabel andFalseLabel
                                LoadInt32 0
                                MarkLabel andEndLabel
                                MarkHiddenSequencePoint
                                Nop
                                MarkHiddenSequencePoint
                                Nop
                                MarkHiddenSequencePoint
                                Nop
                                MarkHiddenSequencePoint
                                Nop
                              ],
                            andLeftLocals @ andRightLocals,
                            [
                                MarkSequencePoint conditionRange
                                Nop
                            ]
                        | _ ->
                            let instructions, locals =
                                valueExpressionInstructions freshLabel kind condition

                            let boundary =
                                match instructions with
                                | MarkSequencePoint _ :: _
                                | MarkHiddenSequencePoint :: _ -> [ Nop ]
                                | _ -> []

                            instructions,
                            locals,
                            [
                                MarkSequencePoint conditionRange
                                Nop
                                MarkHiddenSequencePoint
                            ]
                            @ boundary

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

                    conditionPrefix
                    @ conditionInstructions
                    @ [ BranchIfFalse falseLabel ]
                    @ ifTrueSequencePoint
                    @ ifTrueInstructions
                    @ joinInstructions,
                    conditionLocals
                    @ ifTrueLocals
                    @ ifFalseLocals
                | TypedNullMatch(input,
                                 inputType,
                                 localIndex,
                                 bindingName,
                                 ifNull,
                                 ifNotNull,
                                 matchHeaderRange,
                                 ifNullRange,
                                 ifNotNullRange,
                                 _) ->
                    let notNullLabel = freshLabel ()
                    let endLabel = freshLabel ()

                    let inputInstructions, inputLocals =
                        valueExpressionInstructions freshLabel kind input

                    let nullInstructions, nullLocals =
                        valueExpressionInstructions freshLabel kind ifNull

                    let nullArmInstructions =
                        match ifNull with
                        | TypedUnitLiteral -> [ Nop ]
                        | _ -> nullInstructions

                    let notNullInstructions, notNullLocals =
                        valueExpressionInstructions freshLabel kind ifNotNull

                    [
                        MarkSequencePoint matchHeaderRange
                        Nop
                        MarkHiddenSequencePoint
                    ]
                    @ inputInstructions
                    @ [
                        StoreLocal localIndex
                        LoadLocal localIndex
                        LoadNull
                        CompareEqual
                        BranchIfFalse notNullLabel
                        MarkSequencePoint ifNullRange
                    ]
                    @ nullArmInstructions
                    @ [
                        MarkHiddenSequencePoint
                        Branch endLabel
                        MarkLabel notNullLabel
                        MarkSequencePoint ifNotNullRange
                    ]
                    @ notNullInstructions
                    @ [ MarkLabel endLabel ],
                    inputLocals
                    @ [
                        {
                            Index = localIndex
                            Name = bindingName
                            Type = inputType
                        }
                    ]
                    @ nullLocals
                    @ notNullLocals
                | TypedTypeTestMatch(input,
                                     targetType,
                                     localIndex,
                                     bindingName,
                                     guard,
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

                    let initialSequencePoint expression range =
                        match expression with
                        | TypedLet _
                        | TypedSequential _
                        | TypedConditional _
                        | TypedBooleanNegation _
                        | TypedTryWith _
                        | TypedNullMatch _
                        | TypedTypeTestMatch _ -> []
                        | _ -> [ MarkSequencePoint range ]

                    let guardInstructions, guardLocals =
                        match guard with
                        | Some(guardExpression, guardRange) ->
                            let instructions, locals =
                                valueExpressionInstructions freshLabel kind guardExpression

                            initialSequencePoint guardExpression guardRange
                            @ instructions
                            @ [
                                BranchIfFalse fallbackLabel
                                MarkHiddenSequencePoint
                                Nop
                            ],
                            locals
                        | None -> [], []

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
                    ]
                    @ guardInstructions
                    @ initialSequencePoint ifMatched ifMatchedRange
                    @ matchedInstructions
                    @ [
                        Branch endLabel
                        MarkLabel fallbackLabel
                    ]
                    @ initialSequencePoint ifNotMatched ifNotMatchedRange
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
                    @ guardLocals
                    @ matchedLocals
                    @ notMatchedLocals
                | TypedObjectExpression(typeReference,
                                        _,
                                        constructorArguments,
                                        _,
                                        _) ->
                    if not (List.isEmpty constructorArguments) then
                        invalidOp
                            "object-expression constructor arguments reached an unsupported lowering path"

                    [ NewObject(objectExpressionConstructorReference typeReference) ], []
                | TypedFunctionLambda expression ->
                    functionLambdaConstructionInstructions kind expression, []
                | TypedDelegateLambda expression ->
                    delegateLambdaConstructionInstructions kind expression, []
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedValueTaskBind _
                | TypedValueTaskApply _
                | TypedValueTaskZip _
                | TypedValueTaskOfUnit _
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
                | TypedValueTaskOfUnit expression ->
                    valueTaskOfUnitInstructions methodDeclaration kind expression
                    @ [ Return ],
                    []
                | TypedValueTaskBind expression ->
                    let layout = valueTaskBindHelperLayout methodDeclaration expression
                    let definition = layout.DefinitionExpression
                    let slowPath = freshLabel ()

                    let taskInputType =
                        CliGenericType(expression.TaskTypeReference, [ expression.InputType ])

                    let taskOutputType =
                        CliGenericType(expression.TaskTypeReference, [ expression.OutputType ])

                    let definitionTaskInputType =
                        CliGenericType(
                            definition.TaskTypeReference,
                            [ definition.InputType ]
                        )

                    let definitionTaskOutputType =
                        CliGenericType(
                            definition.TaskTypeReference,
                            [ definition.OutputType ]
                        )

                    let completedHelper = {
                        DeclaringType = CliDeclaringType layout.MethodType
                        Name = "InvokeCompleted"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            definition.BinderType
                            definition.InputValueTaskType
                        ]
                        ReturnType = definition.OutputValueTaskType
                        TargetStableId = None
                    }

                    let continuationHelper = {
                        DeclaringType = CliDeclaringType layout.MethodType
                        Name = "Continue"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            definitionTaskInputType
                            CliObject
                        ]
                        ReturnType = definitionTaskOutputType
                        TargetStableId = None
                    }

                    let isCompletedSuccessfully = {
                        DeclaringType = CliDeclaringType expression.InputValueTaskType
                        Name = "get_IsCompletedSuccessfully"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType = CliBoolean
                        TargetStableId = None
                    }

                    let asTask = {
                        DeclaringType = CliDeclaringType expression.InputValueTaskType
                        Name = "AsTask"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let continuationDelegateType =
                        CliGenericType(
                            expression.FuncTypeReference,
                            [
                                taskInputType
                                CliObject
                                taskOutputType
                            ]
                        )

                    let continuationDelegateConstructor = {
                        DeclaringType = CliDeclaringType continuationDelegateType
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

                    let continuationDefinitionDelegateType =
                        CliGenericType(
                            definition.FuncTypeReference,
                            [
                                CliGenericType(
                                    definition.TaskTypeReference,
                                    [ CliTypeParameter 0 ]
                                )
                                CliObject
                                CliMethodTypeParameter 0
                            ]
                        )

                    let continueWith = {
                        DeclaringType = CliDeclaringType taskInputType
                        Name = "ContinueWith"
                        GenericArity = 1
                        IsInstance = true
                        ParameterTypes = [
                            continuationDefinitionDelegateType
                            CliObject
                            expression.CancellationTokenType
                            expression.TaskContinuationOptionsType
                            expression.TaskSchedulerType
                        ]
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliMethodTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let cancellationTokenNone = {
                        DeclaringType = CliDeclaringType expression.CancellationTokenType
                        Name = "get_None"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = []
                        ReturnType = expression.CancellationTokenType
                        TargetStableId = None
                    }

                    let taskSchedulerDefault = {
                        DeclaringType = CliDeclaringType expression.TaskSchedulerType
                        Name = "get_Default"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = []
                        ReturnType = expression.TaskSchedulerType
                        TargetStableId = None
                    }

                    let unwrap = {
                        DeclaringType =
                            CliDeclaringType(
                                CliNamedType expression.TaskExtensionsTypeReference
                            )
                        Name = "Unwrap"
                        GenericArity = 1
                        IsInstance = false
                        ParameterTypes = [
                            CliGenericType(
                                expression.TaskTypeReference,
                                [
                                    CliGenericType(
                                        expression.TaskTypeReference,
                                        [ CliMethodTypeParameter 0 ]
                                    )
                                ]
                            )
                        ]
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliMethodTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let valueTaskConstructor = {
                        DeclaringType = CliDeclaringType expression.OutputValueTaskType
                        Name = ".ctor"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = [
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliTypeParameter 0 ]
                            )
                        ]
                        ReturnType = CliVoid
                        TargetStableId = None
                    }

                    [
                        LoadArgumentAddress(
                            methodArgumentIndex kind expression.SourceParameterIndex
                        )
                        CallMethod isCompletedSuccessfully
                        BranchIfFalse slowPath
                        LoadArgument(
                            methodArgumentIndex kind expression.BinderParameterIndex
                        )
                        LoadArgument(
                            methodArgumentIndex kind expression.SourceParameterIndex
                        )
                        CallMethod completedHelper
                        Return
                        MarkLabel slowPath
                        LoadArgumentAddress(
                            methodArgumentIndex kind expression.SourceParameterIndex
                        )
                        CallMethod asTask
                        LoadNull
                        LoadFunctionPointer continuationHelper
                        NewObject continuationDelegateConstructor
                        LoadArgument(
                            methodArgumentIndex kind expression.BinderParameterIndex
                        )
                        CallMethod cancellationTokenNone
                        LoadInt32(
                            int
                                System.Threading.Tasks.TaskContinuationOptions.ExecuteSynchronously
                        )
                        CallMethod taskSchedulerDefault
                        CallGenericMethod(continueWith, [ taskOutputType ])
                        CallGenericMethod(unwrap, [ expression.OutputType ])
                        NewObject valueTaskConstructor
                        Return
                    ],
                    []
                | TypedValueTaskApply expression ->
                    let layout = valueTaskApplyHelperLayout methodDeclaration expression
                    let definition = layout.DefinitionExpression
                    let slowPath = freshLabel ()

                    let taskApplicableType =
                        CliGenericType(
                            expression.TaskTypeReference,
                            [ expression.ApplierType ]
                        )

                    let taskOutputType =
                        CliGenericType(expression.TaskTypeReference, [ expression.OutputType ])

                    let definitionTaskApplicableType =
                        CliGenericType(
                            definition.TaskTypeReference,
                            [ definition.ApplierType ]
                        )

                    let definitionTaskOutputType =
                        CliGenericType(
                            definition.TaskTypeReference,
                            [ definition.OutputType ]
                        )

                    let completedHelper = {
                        DeclaringType = CliDeclaringType layout.MethodType
                        Name = "InvokeCompleted"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            definition.ApplierType
                            definition.InputValueTaskType
                        ]
                        ReturnType = definition.OutputValueTaskType
                        TargetStableId = None
                    }

                    let applicableContinuationHelper = {
                        DeclaringType = CliDeclaringType layout.MethodType
                        Name = "ContinueApplicable"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            definitionTaskApplicableType
                            CliObject
                        ]
                        ReturnType = definitionTaskOutputType
                        TargetStableId = None
                    }

                    let isCompletedSuccessfully = {
                        DeclaringType =
                            CliDeclaringType expression.ApplicableValueTaskType
                        Name = "get_IsCompletedSuccessfully"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType = CliBoolean
                        TargetStableId = None
                    }

                    let applicableResult = {
                        DeclaringType =
                            CliDeclaringType expression.ApplicableValueTaskType
                        Name = "get_Result"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType = CliTypeParameter 0
                        TargetStableId = None
                    }

                    let asTask = {
                        DeclaringType =
                            CliDeclaringType expression.ApplicableValueTaskType
                        Name = "AsTask"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let continuationDelegateType =
                        CliGenericType(
                            expression.FuncTypeReference,
                            [
                                taskApplicableType
                                CliObject
                                taskOutputType
                            ]
                        )

                    let continuationDelegateConstructor = {
                        DeclaringType = CliDeclaringType continuationDelegateType
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

                    let continuationDefinitionDelegateType =
                        CliGenericType(
                            definition.FuncTypeReference,
                            [
                                CliGenericType(
                                    definition.TaskTypeReference,
                                    [ CliTypeParameter 0 ]
                                )
                                CliObject
                                CliMethodTypeParameter 0
                            ]
                        )

                    let continueWith = {
                        DeclaringType = CliDeclaringType taskApplicableType
                        Name = "ContinueWith"
                        GenericArity = 1
                        IsInstance = true
                        ParameterTypes = [
                            continuationDefinitionDelegateType
                            CliObject
                            expression.CancellationTokenType
                            expression.TaskContinuationOptionsType
                            expression.TaskSchedulerType
                        ]
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliMethodTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let cancellationTokenNone = {
                        DeclaringType =
                            CliDeclaringType expression.CancellationTokenType
                        Name = "get_None"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = []
                        ReturnType = expression.CancellationTokenType
                        TargetStableId = None
                    }

                    let taskSchedulerDefault = {
                        DeclaringType = CliDeclaringType expression.TaskSchedulerType
                        Name = "get_Default"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = []
                        ReturnType = expression.TaskSchedulerType
                        TargetStableId = None
                    }

                    let unwrap = {
                        DeclaringType =
                            CliDeclaringType(
                                CliNamedType expression.TaskExtensionsTypeReference
                            )
                        Name = "Unwrap"
                        GenericArity = 1
                        IsInstance = false
                        ParameterTypes = [
                            CliGenericType(
                                expression.TaskTypeReference,
                                [
                                    CliGenericType(
                                        expression.TaskTypeReference,
                                        [ CliMethodTypeParameter 0 ]
                                    )
                                ]
                            )
                        ]
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliMethodTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let valueTaskConstructor = {
                        DeclaringType = CliDeclaringType expression.OutputValueTaskType
                        Name = ".ctor"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = [
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliTypeParameter 0 ]
                            )
                        ]
                        ReturnType = CliVoid
                        TargetStableId = None
                    }

                    [
                        LoadArgumentAddress(
                            methodArgumentIndex
                                kind
                                expression.ApplicableParameterIndex
                        )
                        CallMethod isCompletedSuccessfully
                        BranchIfFalse slowPath
                        LoadArgumentAddress(
                            methodArgumentIndex
                                kind
                                expression.ApplicableParameterIndex
                        )
                        CallMethod applicableResult
                        LoadArgument(
                            methodArgumentIndex kind expression.InputParameterIndex
                        )
                        CallMethod completedHelper
                        Return
                        MarkLabel slowPath
                        LoadArgumentAddress(
                            methodArgumentIndex
                                kind
                                expression.ApplicableParameterIndex
                        )
                        CallMethod asTask
                        LoadNull
                        LoadFunctionPointer applicableContinuationHelper
                        NewObject continuationDelegateConstructor
                        LoadArgument(
                            methodArgumentIndex kind expression.InputParameterIndex
                        )
                        Box expression.InputValueTaskType
                        CallMethod cancellationTokenNone
                        LoadInt32(
                            int
                                System.Threading.Tasks.TaskContinuationOptions.ExecuteSynchronously
                        )
                        CallMethod taskSchedulerDefault
                        CallGenericMethod(continueWith, [ taskOutputType ])
                        CallGenericMethod(unwrap, [ expression.OutputType ])
                        NewObject valueTaskConstructor
                        Return
                    ],
                    []
                | TypedValueTaskZip expression ->
                    let layout = valueTaskZipHelperLayout methodDeclaration expression
                    let definition = layout.DefinitionExpression
                    let slowPath = freshLabel ()

                    let taskLeftType =
                        CliGenericType(expression.TaskTypeReference, [ expression.LeftType ])

                    let taskOutputType =
                        CliGenericType(expression.TaskTypeReference, [ expression.TupleType ])

                    let definitionTaskLeftType =
                        CliGenericType(
                            definition.TaskTypeReference,
                            [ definition.LeftType ]
                        )

                    let definitionTaskOutputType =
                        CliGenericType(
                            definition.TaskTypeReference,
                            [ definition.TupleType ]
                        )

                    let completedHelper = {
                        DeclaringType = CliDeclaringType layout.MethodType
                        Name = "InvokeCompleted"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            definition.LeftType
                            definition.RightValueTaskType
                        ]
                        ReturnType = definition.OutputValueTaskType
                        TargetStableId = None
                    }

                    let leftContinuationHelper = {
                        DeclaringType = CliDeclaringType layout.MethodType
                        Name = "ContinueLeft"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = [
                            definitionTaskLeftType
                            CliObject
                        ]
                        ReturnType = definitionTaskOutputType
                        TargetStableId = None
                    }

                    let isCompletedSuccessfully = {
                        DeclaringType = CliDeclaringType expression.LeftValueTaskType
                        Name = "get_IsCompletedSuccessfully"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType = CliBoolean
                        TargetStableId = None
                    }

                    let leftResult = {
                        DeclaringType = CliDeclaringType expression.LeftValueTaskType
                        Name = "get_Result"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType = CliTypeParameter 0
                        TargetStableId = None
                    }

                    let asTask = {
                        DeclaringType = CliDeclaringType expression.LeftValueTaskType
                        Name = "AsTask"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = []
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let continuationDelegateType =
                        CliGenericType(
                            expression.FuncTypeReference,
                            [
                                taskLeftType
                                CliObject
                                taskOutputType
                            ]
                        )

                    let continuationDelegateConstructor = {
                        DeclaringType = CliDeclaringType continuationDelegateType
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

                    let continuationDefinitionDelegateType =
                        CliGenericType(
                            definition.FuncTypeReference,
                            [
                                CliGenericType(
                                    definition.TaskTypeReference,
                                    [ CliTypeParameter 0 ]
                                )
                                CliObject
                                CliMethodTypeParameter 0
                            ]
                        )

                    let continueWith = {
                        DeclaringType = CliDeclaringType taskLeftType
                        Name = "ContinueWith"
                        GenericArity = 1
                        IsInstance = true
                        ParameterTypes = [
                            continuationDefinitionDelegateType
                            CliObject
                            expression.CancellationTokenType
                            expression.TaskContinuationOptionsType
                            expression.TaskSchedulerType
                        ]
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliMethodTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let cancellationTokenNone = {
                        DeclaringType =
                            CliDeclaringType expression.CancellationTokenType
                        Name = "get_None"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = []
                        ReturnType = expression.CancellationTokenType
                        TargetStableId = None
                    }

                    let taskSchedulerDefault = {
                        DeclaringType = CliDeclaringType expression.TaskSchedulerType
                        Name = "get_Default"
                        GenericArity = 0
                        IsInstance = false
                        ParameterTypes = []
                        ReturnType = expression.TaskSchedulerType
                        TargetStableId = None
                    }

                    let unwrap = {
                        DeclaringType =
                            CliDeclaringType(
                                CliNamedType expression.TaskExtensionsTypeReference
                            )
                        Name = "Unwrap"
                        GenericArity = 1
                        IsInstance = false
                        ParameterTypes = [
                            CliGenericType(
                                expression.TaskTypeReference,
                                [
                                    CliGenericType(
                                        expression.TaskTypeReference,
                                        [ CliMethodTypeParameter 0 ]
                                    )
                                ]
                            )
                        ]
                        ReturnType =
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliMethodTypeParameter 0 ]
                            )
                        TargetStableId = None
                    }

                    let valueTaskConstructor = {
                        DeclaringType = CliDeclaringType expression.OutputValueTaskType
                        Name = ".ctor"
                        GenericArity = 0
                        IsInstance = true
                        ParameterTypes = [
                            CliGenericType(
                                expression.TaskTypeReference,
                                [ CliTypeParameter 0 ]
                            )
                        ]
                        ReturnType = CliVoid
                        TargetStableId = None
                    }

                    [
                        LoadArgumentAddress(
                            methodArgumentIndex kind expression.LeftParameterIndex
                        )
                        CallMethod isCompletedSuccessfully
                        BranchIfFalse slowPath
                        LoadArgumentAddress(
                            methodArgumentIndex kind expression.LeftParameterIndex
                        )
                        CallMethod leftResult
                        LoadArgument(
                            methodArgumentIndex kind expression.RightParameterIndex
                        )
                        CallMethod completedHelper
                        Return
                        MarkLabel slowPath
                        LoadArgumentAddress(
                            methodArgumentIndex kind expression.LeftParameterIndex
                        )
                        CallMethod asTask
                        LoadNull
                        LoadFunctionPointer leftContinuationHelper
                        NewObject continuationDelegateConstructor
                        LoadArgument(
                            methodArgumentIndex kind expression.RightParameterIndex
                        )
                        Box expression.RightValueTaskType
                        CallMethod cancellationTokenNone
                        LoadInt32(
                            int
                                System.Threading.Tasks.TaskContinuationOptions.ExecuteSynchronously
                        )
                        CallMethod taskSchedulerDefault
                        CallGenericMethod(continueWith, [ taskOutputType ])
                        CallGenericMethod(unwrap, [ expression.TupleType ])
                        NewObject valueTaskConstructor
                        Return
                    ],
                    []
                | (TypedStringLiteral _ | TypedNullLiteral | TypedUnitLiteral | TypedReceiverReference | TypedLocalReference _ | TypedLet _ | TypedLocalAssignment _ | TypedAddressOf _ | TypedInstanceFieldGet _ | TypedStaticMethodCall _ | TypedObjectConstruction _ | TypedDefaultValue _ | TypedFunctionApplication _ | TypedInstanceMethodCall _ | TypedFunctionLambda _ | TypedDelegateLambda _ | TypedConditional _ | TypedUpcast _ | TypedSequential _ | TypedBooleanNegation _ | TypedEquality _ | TypedTryWith _ | TypedNullMatch _ | TypedTypeTestMatch _ | TypedObjectExpression _) as expression ->
                    let instructions, locals =
                        valueExpressionInstructions
                            methodDeclaration
                            freshLabel
                            kind
                            expression

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

            let instructionDependencyIds instructions =
                instructions
                |> List.collect (
                    function
                    | LoadField fieldReference
                    | LoadFieldAddress fieldReference
                    | StoreField fieldReference
                    | LoadStaticField fieldReference
                    | StoreStaticField fieldReference -> [ fieldReference.DependencyId ]
                    | CallMethod methodReference
                    | CallVirtualMethod methodReference
                    | LoadFunctionPointer methodReference
                    | NewObject methodReference -> [ methodReference.DependencyId ]
                    | CallGenericMethod(methodReference, genericArguments) ->
                        methodReference.DependencyId
                        :: (genericArguments
                            |> List.collect cliTypeDependencyIds)
                    | Box cliType
                    | UnboxAny cliType
                    | CastClass cliType
                    | IsInstance cliType
                    | InitializeObject cliType -> cliTypeDependencyIds cliType
                    | DefineCatchRegion(_, _, _, _, catchType) ->
                        cliTypeDependencyIds catchType
                    | MarkHiddenSequencePoint
                    | MarkLabel _
                    | BranchIfFalse _
                    | Branch _
                    | Leave _
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

            let methodFragment
                kind
                documentIndex
                documentChecksum
                fragmentStableId
                contentHash
                (methodDeclaration: TypedMethodDeclaration)
                =
                let instructions, locals = methodInstructions kind methodDeclaration

                let instructions =
                    match
                        methodDeclaration.EmitHiddenEntrySequencePoint,
                        methodDeclaration.Body
                    with
                    | true, _ ->
                        let visibleBodyPoint =
                            if
                                instructions
                                |> List.exists (function
                                    | MarkSequencePoint _ -> true
                                    | _ -> false)
                            then
                                []
                            else
                                [ MarkSequencePoint methodDeclaration.Range ]

                        [
                            MarkHiddenSequencePoint
                            Nop
                        ]
                        @ visibleBodyPoint
                        @ instructions
                    | false, TypedStaticMethodCall(target, _, _) when
                        target.StableId.EndsWith(
                            "|intrinsic|isNull",
                            StringComparison.Ordinal
                        )
                        ->
                        MarkSequencePoint methodDeclaration.Range
                        :: instructions
                    | _ -> instructions

                let instructionDependencies = instructionDependencyIds instructions

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
                        | TypedFunctionLambda _
                        | TypedDelegateLambda _
                        | TypedValueTaskBind _
                        | TypedValueTaskApply _
                        | TypedValueTaskZip _
                        | TypedValueTaskOfUnit _
                        | TypedResumableTryFinally _ -> 8
                        | TypedIntegerLiteral _
                        | TypedStringLiteral _
                        | TypedNullLiteral
                        | TypedUnitLiteral
                        | TypedReceiverReference
                        | TypedParameterReference _
                        | TypedLocalReference _
                        | TypedLet _
                        | TypedAddressOf _
                        | TypedDefaultValue _
                        | TypedObjectExpression _
                        | TypedTraitCall _ -> 1
                        | TypedInstanceFieldGet _
                        | TypedStaticMethodCall _
                        | TypedObjectConstruction _
                        | TypedFunctionApplication _
                        | TypedInstanceMethodCall _
                        | TypedConditional _
                        | TypedUpcast _
                        | TypedSequential _
                        | TypedLocalAssignment _
                        | TypedBooleanNegation _
                        | TypedEquality _
                        | TypedTryWith _
                        | TypedNullMatch _
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
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

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
                            | TypedNestedModule _
                            | TypedTypeAbbreviation _
                            | TypedStaticType _
                            | TypedObjectType _
                            | TypedStructType _ -> None
                        )

                    let methods =
                        declarationsWithContentHashes
                        |> List.collect (fun (declaration, contentHash) ->
                            match declaration with
                            | TypedMethod methodDeclaration -> [
                                methodFragment
                                    ModuleFunction
                                    documentIndex
                                    typed.SourceChecksum
                                    (typeStableId
                                     + "/method:"
                                     + methodDeclaration.StableId)
                                    contentHash
                                    methodDeclaration
                              ]
                            | TypedObjectType({
                                                  Container = TypedCurrentModuleAugmentation extendedType
                                              } as typeDeclaration) ->
                                typeDeclaration.Methods
                                |> List.map (fun objectMethodDeclaration ->
                                    let kind, methodDeclaration, extensionMethod =
                                        match objectMethodDeclaration with
                                        | TypedInstanceObjectMethod(receiverName,
                                                                    methodDeclaration) ->
                                            TypeExtensionMember,
                                            methodDeclaration,
                                            {
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
                                                            "type-augmentation"
                                                            TypeIdentity.cliType extendedType
                                                        ]
                                            }
                                        | TypedStaticObjectMethod methodDeclaration ->
                                            StaticTypeExtensionMember,
                                            methodDeclaration,
                                            {
                                                methodDeclaration with
                                                    Name =
                                                        typeDeclaration.Name
                                                        + "."
                                                        + methodDeclaration.Name
                                                        + ".Static"
                                                    ExportFingerprint =
                                                        Fingerprint.parts [
                                                            methodDeclaration.ExportFingerprint
                                                            "static-type-augmentation"
                                                            TypeIdentity.cliType extendedType
                                                        ]
                                            }

                                    methodFragment
                                        kind
                                        documentIndex
                                        typed.SourceChecksum
                                        methodDeclaration.StableId
                                        (Fingerprint.parts [
                                            contentHash
                                            extensionMethod.ExportFingerprint
                                            methodImplementationHash methodDeclaration
                                        ])
                                        extensionMethod
                                )
                            | TypedLiteralField _
                            | TypedNestedModule _
                            | TypedTypeAbbreviation _
                            | TypedStaticType _
                            | TypedObjectType _
                            | TypedStructType _ -> []
                        )

                    let containsNestedType =
                        typed.ContainerKind = ModuleSource
                        && (declarationsWithContentHashes
                            |> List.exists (
                                fst
                                >> function
                                    | TypedObjectType {
                                                          Container = TypedCurrentModuleAugmentation _
                                                      } -> false
                                    | TypedObjectType _
                                    | TypedStructType _
                                    | TypedNestedModule _ -> true
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
                            StaticFields = []
                            Properties = []
                            Methods = methods
                        }
                )
                |> List.choose id

            let rec flattenNestedModule
                typed
                parentTypeStableId
                typeContentHash
                (moduleDeclaration: TypedNestedModuleDeclaration)
                =
                (typed, parentTypeStableId, typeContentHash, moduleDeclaration)
                :: (moduleDeclaration.Modules
                    |> List.collect (fun childModule ->
                        flattenNestedModule
                            typed
                            (Some moduleDeclaration.StableId)
                            (nestedModuleContentHash childModule)
                            childModule
                    ))

            let nestedModulesWithContentHashes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    let parentTypeStableId =
                        if typed.ContainerKind = ModuleSource then
                            Some(
                                moduleStableId
                                + "/type:"
                                + typed.StableId
                            )
                        else
                            None

                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, typeContentHash) ->
                        match declaration with
                        | TypedNestedModule moduleDeclaration ->
                            flattenNestedModule
                                typed
                                parentTypeStableId
                                typeContentHash
                                moduleDeclaration
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _
                        | TypedStructType _ -> []
                    )
                )

            let nestedModuleTypes =
                nestedModulesWithContentHashes
                |> List.map (fun (typed,
                                  parentTypeStableId,
                                  typeContentHash,
                                  moduleDeclaration) ->
                            let nestedTypeReference = {
                                DeclarationId = moduleDeclaration.StableId
                                AssemblyName = String.Empty
                                TypeName = {
                                    Namespace = String.Empty
                                    Name = moduleDeclaration.CompiledName
                                }
                                IsValueType = false
                            }

                            let declaringType =
                                CliDeclaringType(CliNamedType nestedTypeReference)

                            let fieldReference (value: TypedModuleValueDeclaration) = {
                                DeclaringType = declaringType
                                Name =
                                    value.Name
                                    + "@"
                                FieldType = value.Type
                                TargetStableId =
                                    Some(
                                        value.StableId
                                        + "/field"
                                    )
                            }

                            let fieldReferences =
                                moduleDeclaration.Values
                                |> List.map (fun value -> value.StableId, fieldReference value)
                                |> Map.ofList

                            let staticFields =
                                moduleDeclaration.Values
                                |> List.map (fun value ->
                                    let field = fieldReferences.[value.StableId]

                                    {
                                        SchemaVersion = querySchema
                                        StableId = field.TargetStableId.Value
                                        Name = field.Name
                                        Type = value.Type
                                        ContentHash =
                                            Fingerprint.parts [
                                                typeContentHash
                                                value.StableId
                                                TypeIdentity.cliType value.Type
                                            ]
                                    }
                                )

                            let getters =
                                moduleDeclaration.Values
                                |> List.map (fun value ->
                                    let field = fieldReferences.[value.StableId]
                                    let getterStableId = value.StableId + "/getter"

                                    {
                                        SchemaVersion = querySchema
                                        StableId = getterStableId
                                        Name =
                                            "get_"
                                            + value.Name
                                        Kind = ModuleValueGetter
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = []
                                        Locals = []
                                        ReturnType = value.Type
                                        Instructions = [
                                            LoadStaticField field
                                            Return
                                        ]
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 1
                                        DependencyIds = [ field.DependencyId ]
                                        ContentHash =
                                            Fingerprint.parts [
                                                value.ExportFingerprint
                                                field.StableId
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = value.Range
                                    }
                                )

                            let moduleFunctions =
                                moduleDeclaration.Methods
                                |> List.map (fun methodDeclaration ->
                                    methodFragment
                                        ModuleFunction
                                        typed.DocumentIndex
                                        typed.SourceChecksum
                                        methodDeclaration.StableId
                                        (methodImplementationHash methodDeclaration)
                                        methodDeclaration
                                )

                            let initializerInstructions, initializerDependencies =
                                (([], []), moduleDeclaration.Values)
                                ||> List.fold (fun (instructions, dependencies) value ->
                                    let targetField = fieldReferences.[value.StableId]

                                    match value.Initializer with
                                    | TypedModuleValueConstruction target ->
                                        let constructor = {
                                            DeclaringType = CliDeclaringType target.DeclaringType
                                            Name = ".ctor"
                                            GenericArity = 0
                                            IsInstance = true
                                            ParameterTypes = target.ParameterTypes
                                            ReturnType = CliVoid
                                            TargetStableId = Some target.StableId
                                        }

                                        instructions
                                        @ [
                                            NewObject constructor
                                            StoreStaticField targetField
                                          ],
                                        dependencies
                                        @ [
                                            constructor.DependencyId
                                            targetField.DependencyId
                                          ]
                                    | TypedModuleValueAlias targetStableId ->
                                        let sourceField = fieldReferences.[targetStableId]

                                        instructions
                                        @ [
                                            LoadStaticField sourceField
                                            StoreStaticField targetField
                                          ],
                                        dependencies
                                        @ [
                                            sourceField.DependencyId
                                            targetField.DependencyId
                                          ]
                                )

                            let staticConstructorStableId =
                                moduleDeclaration.StableId
                                + "/static-constructor"

                            let staticConstructor = {
                                SchemaVersion = querySchema
                                StableId = staticConstructorStableId
                                Name = ".cctor"
                                Kind = StaticConstructor
                                GenericParameters = []
                                Constraints = []
                                GenericParameterConstraints = []
                                Attributes = []
                                Parameters = []
                                Locals = []
                                ReturnType = CliVoid
                                Instructions = initializerInstructions @ [ Return ]
                                EmitDefaultSequencePoint = false
                                MaxStack = 1
                                DependencyIds = initializerDependencies |> List.distinct
                                ContentHash =
                                    Fingerprint.parts [
                                        staticConstructorStableId
                                        typeContentHash
                                        yield! initializerDependencies
                                    ]
                                DocumentIndex = typed.DocumentIndex
                                DocumentChecksum = typed.SourceChecksum
                                Range = moduleDeclaration.Range
                            }

                            let staticConstructors =
                                if List.isEmpty moduleDeclaration.Values then
                                    []
                                else
                                    [ staticConstructor ]

                            let properties =
                                moduleDeclaration.Values
                                |> List.map (fun value -> {
                                    SchemaVersion = querySchema
                                    StableId = value.StableId + "/property"
                                    Name = value.Name
                                    Type = value.Type
                                    GetterStableId = value.StableId + "/getter"
                                    ContentHash = value.ExportFingerprint
                                })

                            {
                                SchemaVersion = querySchema
                                StableId = moduleDeclaration.StableId
                                Namespace =
                                    if parentTypeStableId.IsNone then
                                        typed.Namespace
                                    else
                                        String.Empty
                                Name = moduleDeclaration.CompiledName
                                IsPublic = true
                                EnclosingTypeStableId = parentTypeStableId
                                Kind = ModuleContainer
                                GenericParameters = []
                                Attributes = [
                                    yield!
                                        moduleDeclaration.Attributes
                                        |> List.map customAttributeFragment

                                    compilationMappingAttribute
                                        moduleDeclaration.StableId
                                        ModuleConstruct
                                ]
                                LiteralFields = []
                                InstanceFields = []
                                StaticFields = staticFields
                                Properties = properties
                                Methods =
                                    moduleFunctions
                                    @ getters
                                    @ staticConstructors
                            }
                )

            let valueTaskBindHelperTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedNestedModule moduleDeclaration ->
                            moduleDeclaration.Methods
                            |> List.choose (fun methodDeclaration ->
                                match methodDeclaration.Body with
                                | TypedValueTaskBind expression ->
                                    let layout =
                                        valueTaskBindHelperLayout
                                            methodDeclaration
                                            expression

                                    let expression = layout.DefinitionExpression

                                    let taskInputType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.InputType ]
                                        )

                                    let taskOutputType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.OutputType ]
                                        )

                                    let taskAwaiterInputType =
                                        CliGenericType(
                                            expression.TaskAwaiterTypeReference,
                                            [ expression.InputType ]
                                        )

                                    let inputValueTaskResult = {
                                        DeclaringType =
                                            CliDeclaringType expression.InputValueTaskType
                                        Name = "get_Result"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let binderInvoke = {
                                        DeclaringType = CliDeclaringType expression.BinderType
                                        Name = "Invoke"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [ CliTypeParameter 0 ]
                                        ReturnType = CliTypeParameter 1
                                        TargetStableId = None
                                    }

                                    let outputValueTaskAsTask = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = "AsTask"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let taskGetAwaiter = {
                                        DeclaringType = CliDeclaringType taskInputType
                                        Name = "GetAwaiter"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskAwaiterTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let awaiterGetResult = {
                                        DeclaringType = CliDeclaringType taskAwaiterInputType
                                        Name = "GetResult"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let cancellationToken = {
                                        DeclaringType =
                                            CliDeclaringType
                                                expression.OperationCanceledExceptionType
                                        Name = "get_CancellationToken"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = expression.CancellationTokenType
                                        TargetStableId = None
                                    }

                                    let fromCanceled = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromCanceled"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [
                                            expression.CancellationTokenType
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromException = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromException"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ expression.ExceptionType ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromResult = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromResult"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ CliMethodTypeParameter 0 ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let outputValueTaskConstructor = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let outputValueTaskValueConstructor = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [ CliTypeParameter 0 ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let completedInstructions = [
                                        MarkLabel 0
                                        LoadArgument 0
                                        LoadArgumentAddress 1
                                        CallMethod inputValueTaskResult
                                        CallVirtualMethod binderInvoke

                                        yield!
                                            match expression.ReturnKind with
                                            | ComputationReturn -> [
                                                NewObject outputValueTaskValueConstructor
                                              ]
                                            | ComputationReturnFrom -> []

                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 1
                                        LoadLocal 1
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.OutputType ]
                                        )
                                        NewObject outputValueTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 2
                                        LoadLocal 2
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.OutputType ]
                                        )
                                        NewObject outputValueTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 0
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let completedMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.CompletedStableId
                                        Name = "InvokeCompleted"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "binder"
                                                Type = expression.BinderType
                                                Attributes = []
                                            }
                                            {
                                                Name = "source"
                                                Type = expression.InputValueTaskType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "result"
                                                Type = expression.OutputValueTaskType
                                            }
                                            {
                                                Index = 1
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 2
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                        ]
                                        ReturnType = expression.OutputValueTaskType
                                        Instructions = completedInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 3
                                        DependencyIds =
                                            instructionDependencyIds completedInstructions
                                            @ (cliTypeDependencyIds
                                                expression.BinderType)
                                            @ (cliTypeDependencyIds
                                                expression.InputValueTaskType)
                                            @ (cliTypeDependencyIds
                                                expression.OutputValueTaskType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.CompletedStableId
                                                methodImplementationHash
                                                    methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let continuationInstructions = [
                                        MarkLabel 0
                                        LoadArgument 1
                                        CastClass expression.BinderType
                                        LoadArgument 0
                                        CallVirtualMethod taskGetAwaiter
                                        StoreLocal 0
                                        LoadLocalAddress 0
                                        CallMethod awaiterGetResult
                                        CallVirtualMethod binderInvoke
                                        StoreLocal 1

                                        yield!
                                            match expression.ReturnKind with
                                            | ComputationReturn -> [
                                                LoadLocal 1
                                                CallGenericMethod(
                                                    fromResult,
                                                    [ expression.OutputType ]
                                                )
                                              ]
                                            | ComputationReturnFrom -> [
                                                LoadLocalAddress 1
                                                CallMethod outputValueTaskAsTask
                                              ]

                                        StoreLocal 4
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 2
                                        LoadLocal 2
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 4
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 3
                                        LoadLocal 3
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 4
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 4
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let continuationMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.ContinuationStableId
                                        Name = "Continue"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "source"
                                                Type = taskInputType
                                                Attributes = []
                                            }
                                            {
                                                Name = "state"
                                                Type = CliObject
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "awaiter"
                                                Type = taskAwaiterInputType
                                            }
                                            {
                                                Index = 1
                                                Name =
                                                    match expression.ReturnKind with
                                                    | ComputationReturn -> "mappedResult"
                                                    | ComputationReturnFrom ->
                                                        "valueTaskResult"
                                                Type =
                                                    match expression.ReturnKind with
                                                    | ComputationReturn ->
                                                        expression.OutputType
                                                    | ComputationReturnFrom ->
                                                        expression.OutputValueTaskType
                                            }
                                            {
                                                Index = 2
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 3
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                            {
                                                Index = 4
                                                Name = "result"
                                                Type = taskOutputType
                                            }
                                        ]
                                        ReturnType = taskOutputType
                                        Instructions = continuationInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 3
                                        DependencyIds =
                                            instructionDependencyIds
                                                continuationInstructions
                                            @ (cliTypeDependencyIds taskInputType)
                                            @ (cliTypeDependencyIds taskOutputType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.ContinuationStableId
                                                methodImplementationHash
                                                    methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }


                                    Some {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace = String.Empty
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            Some moduleDeclaration.StableId
                                        Kind = ClosureContainer
                                        GenericParameters =
                                            methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = []
                                        StaticFields = []
                                        Properties = []
                                        Methods = [
                                            completedMethod
                                            continuationMethod
                                        ]
                                    }
                                | _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _
                        | TypedStructType _ -> []
                    )
                )

            let valueTaskApplyHelperTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedNestedModule moduleDeclaration ->
                            moduleDeclaration.Methods
                            |> List.choose (fun methodDeclaration ->
                                match methodDeclaration.Body with
                                | TypedValueTaskApply applyExpression ->
                                    let layout =
                                        valueTaskApplyHelperLayout
                                            methodDeclaration
                                            applyExpression

                                    let expression = layout.DefinitionExpression

                                    let taskApplicableType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.ApplierType ]
                                        )

                                    let taskInputType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.InputType ]
                                        )

                                    let taskOutputType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.OutputType ]
                                        )

                                    let taskAwaiterApplicableType =
                                        CliGenericType(
                                            expression.TaskAwaiterTypeReference,
                                            [ expression.ApplierType ]
                                        )

                                    let taskAwaiterInputType =
                                        CliGenericType(
                                            expression.TaskAwaiterTypeReference,
                                            [ expression.InputType ]
                                        )

                                    let inputValueTaskIsCompletedSuccessfully = {
                                        DeclaringType =
                                            CliDeclaringType expression.InputValueTaskType
                                        Name = "get_IsCompletedSuccessfully"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliBoolean
                                        TargetStableId = None
                                    }

                                    let inputValueTaskResult = {
                                        DeclaringType =
                                            CliDeclaringType expression.InputValueTaskType
                                        Name = "get_Result"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let inputValueTaskAsTask = {
                                        DeclaringType =
                                            CliDeclaringType expression.InputValueTaskType
                                        Name = "AsTask"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let outputValueTaskAsTask = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = "AsTask"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let applierInvoke = {
                                        DeclaringType = CliDeclaringType expression.ApplierType
                                        Name = "Invoke"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [ CliTypeParameter 0 ]
                                        ReturnType = CliTypeParameter 1
                                        TargetStableId = None
                                    }

                                    let applicableTaskGetAwaiter = {
                                        DeclaringType = CliDeclaringType taskApplicableType
                                        Name = "GetAwaiter"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskAwaiterTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let inputTaskGetAwaiter = {
                                        DeclaringType = CliDeclaringType taskInputType
                                        Name = "GetAwaiter"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskAwaiterTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let applicableAwaiterGetResult = {
                                        DeclaringType =
                                            CliDeclaringType taskAwaiterApplicableType
                                        Name = "GetResult"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let inputAwaiterGetResult = {
                                        DeclaringType = CliDeclaringType taskAwaiterInputType
                                        Name = "GetResult"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let cancellationToken = {
                                        DeclaringType =
                                            CliDeclaringType
                                                expression.OperationCanceledExceptionType
                                        Name = "get_CancellationToken"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = expression.CancellationTokenType
                                        TargetStableId = None
                                    }

                                    let fromCanceled = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromCanceled"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [
                                            expression.CancellationTokenType
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromException = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromException"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ expression.ExceptionType ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromResult = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromResult"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ CliMethodTypeParameter 0 ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let outputValueTaskValueConstructor = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [ CliTypeParameter 0 ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let outputValueTaskTaskConstructor = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let inputContinuationHelper = {
                                        DeclaringType = CliDeclaringType layout.DefinitionType
                                        Name = "ContinueInput"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = [
                                            taskInputType
                                            CliObject
                                        ]
                                        ReturnType = taskOutputType
                                        TargetStableId = None
                                    }

                                    let inputContinuationDelegateType =
                                        CliGenericType(
                                            expression.FuncTypeReference,
                                            [
                                                taskInputType
                                                CliObject
                                                taskOutputType
                                            ]
                                        )

                                    let inputContinuationDelegateConstructor = {
                                        DeclaringType =
                                            CliDeclaringType inputContinuationDelegateType
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

                                    let inputContinuationDefinitionDelegateType =
                                        CliGenericType(
                                            expression.FuncTypeReference,
                                            [
                                                CliGenericType(
                                                    expression.TaskTypeReference,
                                                    [ CliTypeParameter 0 ]
                                                )
                                                CliObject
                                                CliMethodTypeParameter 0
                                            ]
                                        )

                                    let inputContinueWith = {
                                        DeclaringType = CliDeclaringType taskInputType
                                        Name = "ContinueWith"
                                        GenericArity = 1
                                        IsInstance = true
                                        ParameterTypes = [
                                            inputContinuationDefinitionDelegateType
                                            CliObject
                                            expression.CancellationTokenType
                                            expression.TaskContinuationOptionsType
                                            expression.TaskSchedulerType
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let cancellationTokenNone = {
                                        DeclaringType =
                                            CliDeclaringType expression.CancellationTokenType
                                        Name = "get_None"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = []
                                        ReturnType = expression.CancellationTokenType
                                        TargetStableId = None
                                    }

                                    let taskSchedulerDefault = {
                                        DeclaringType =
                                            CliDeclaringType expression.TaskSchedulerType
                                        Name = "get_Default"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = []
                                        ReturnType = expression.TaskSchedulerType
                                        TargetStableId = None
                                    }

                                    let unwrap = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.TaskExtensionsTypeReference
                                            )
                                        Name = "Unwrap"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [
                                                    CliGenericType(
                                                        expression.TaskTypeReference,
                                                        [ CliMethodTypeParameter 0 ]
                                                    )
                                                ]
                                            )
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let completedInstructions = [
                                        MarkLabel 0
                                        LoadArgumentAddress 1
                                        CallMethod inputValueTaskIsCompletedSuccessfully
                                        BranchIfFalse 7
                                        LoadArgument 0
                                        LoadArgumentAddress 1
                                        CallMethod inputValueTaskResult
                                        CallVirtualMethod applierInvoke
                                        NewObject outputValueTaskValueConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 7
                                        LoadArgumentAddress 1
                                        CallMethod inputValueTaskAsTask
                                        LoadNull
                                        LoadFunctionPointer inputContinuationHelper
                                        NewObject inputContinuationDelegateConstructor
                                        LoadArgument 0
                                        CallMethod cancellationTokenNone
                                        LoadInt32(
                                            int
                                                System.Threading.Tasks.TaskContinuationOptions.ExecuteSynchronously
                                        )
                                        CallMethod taskSchedulerDefault
                                        CallGenericMethod(
                                            inputContinueWith,
                                            [ taskOutputType ]
                                        )
                                        CallGenericMethod(
                                            unwrap,
                                            [ expression.OutputType ]
                                        )
                                        NewObject outputValueTaskTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 1
                                        LoadLocal 1
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.OutputType ]
                                        )
                                        NewObject outputValueTaskTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 2
                                        LoadLocal 2
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.OutputType ]
                                        )
                                        NewObject outputValueTaskTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 0
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let completedMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.CompletedStableId
                                        Name = "InvokeCompleted"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "applier"
                                                Type = expression.ApplierType
                                                Attributes = []
                                            }
                                            {
                                                Name = "source"
                                                Type = expression.InputValueTaskType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "result"
                                                Type = expression.OutputValueTaskType
                                            }
                                            {
                                                Index = 1
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 2
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                        ]
                                        ReturnType = expression.OutputValueTaskType
                                        Instructions = completedInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds =
                                            instructionDependencyIds completedInstructions
                                            @ (cliTypeDependencyIds expression.ApplierType)
                                            @ (cliTypeDependencyIds
                                                expression.InputValueTaskType)
                                            @ (cliTypeDependencyIds
                                                expression.OutputValueTaskType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.CompletedStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let inputContinuationInstructions = [
                                        MarkLabel 0
                                        LoadArgument 1
                                        CastClass expression.ApplierType
                                        LoadArgument 0
                                        CallVirtualMethod inputTaskGetAwaiter
                                        StoreLocal 0
                                        LoadLocalAddress 0
                                        CallMethod inputAwaiterGetResult
                                        CallVirtualMethod applierInvoke
                                        StoreLocal 1
                                        LoadLocal 1
                                        CallGenericMethod(
                                            fromResult,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 4
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 2
                                        LoadLocal 2
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 4
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 3
                                        LoadLocal 3
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 4
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 4
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let inputContinuationMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.InputContinuationStableId
                                        Name = "ContinueInput"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "source"
                                                Type = taskInputType
                                                Attributes = []
                                            }
                                            {
                                                Name = "state"
                                                Type = CliObject
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "awaiter"
                                                Type = taskAwaiterInputType
                                            }
                                            {
                                                Index = 1
                                                Name = "mappedResult"
                                                Type = expression.OutputType
                                            }
                                            {
                                                Index = 2
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 3
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                            {
                                                Index = 4
                                                Name = "result"
                                                Type = taskOutputType
                                            }
                                        ]
                                        ReturnType = taskOutputType
                                        Instructions = inputContinuationInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 3
                                        DependencyIds =
                                            instructionDependencyIds
                                                inputContinuationInstructions
                                            @ (cliTypeDependencyIds taskInputType)
                                            @ (cliTypeDependencyIds taskOutputType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.InputContinuationStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let completedHelper = {
                                        DeclaringType = CliDeclaringType layout.DefinitionType
                                        Name = "InvokeCompleted"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = [
                                            expression.ApplierType
                                            expression.InputValueTaskType
                                        ]
                                        ReturnType = expression.OutputValueTaskType
                                        TargetStableId = None
                                    }

                                    let applicableContinuationInstructions = [
                                        MarkLabel 0
                                        LoadArgument 0
                                        CallVirtualMethod applicableTaskGetAwaiter
                                        StoreLocal 0
                                        LoadLocalAddress 0
                                        CallMethod applicableAwaiterGetResult
                                        StoreLocal 1
                                        LoadArgument 1
                                        UnboxAny expression.InputValueTaskType
                                        StoreLocal 2
                                        LoadLocal 1
                                        LoadLocal 2
                                        CallMethod completedHelper
                                        StoreLocal 3
                                        LoadLocalAddress 3
                                        CallMethod outputValueTaskAsTask
                                        StoreLocal 6
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 4
                                        LoadLocal 4
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 6
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 5
                                        LoadLocal 5
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.OutputType ]
                                        )
                                        StoreLocal 6
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 6
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let applicableContinuationMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.ApplicableContinuationStableId
                                        Name = "ContinueApplicable"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "source"
                                                Type = taskApplicableType
                                                Attributes = []
                                            }
                                            {
                                                Name = "state"
                                                Type = CliObject
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "awaiter"
                                                Type = taskAwaiterApplicableType
                                            }
                                            {
                                                Index = 1
                                                Name = "applier"
                                                Type = expression.ApplierType
                                            }
                                            {
                                                Index = 2
                                                Name = "inputSource"
                                                Type = expression.InputValueTaskType
                                            }
                                            {
                                                Index = 3
                                                Name = "valueTaskResult"
                                                Type = expression.OutputValueTaskType
                                            }
                                            {
                                                Index = 4
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 5
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                            {
                                                Index = 6
                                                Name = "result"
                                                Type = taskOutputType
                                            }
                                        ]
                                        ReturnType = taskOutputType
                                        Instructions = applicableContinuationInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 3
                                        DependencyIds =
                                            instructionDependencyIds
                                                applicableContinuationInstructions
                                            @ (cliTypeDependencyIds taskApplicableType)
                                            @ (cliTypeDependencyIds taskOutputType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.ApplicableContinuationStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }


                                    Some {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace = String.Empty
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            Some moduleDeclaration.StableId
                                        Kind = ClosureContainer
                                        GenericParameters =
                                            methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = []
                                        StaticFields = []
                                        Properties = []
                                        Methods = [
                                            completedMethod
                                            inputContinuationMethod
                                            applicableContinuationMethod
                                        ]
                                    }
                                | _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _
                        | TypedStructType _ -> []
                    )
                )

            let valueTaskZipHelperTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedNestedModule moduleDeclaration ->
                            moduleDeclaration.Methods
                            |> List.choose (fun methodDeclaration ->
                                match methodDeclaration.Body with
                                | TypedValueTaskZip zipExpression ->
                                    let layout =
                                        valueTaskZipHelperLayout
                                            methodDeclaration
                                            zipExpression

                                    let expression = layout.DefinitionExpression

                                    let taskLeftType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.LeftType ]
                                        )

                                    let taskRightType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.RightType ]
                                        )

                                    let taskOutputType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.TupleType ]
                                        )

                                    let taskAwaiterLeftType =
                                        CliGenericType(
                                            expression.TaskAwaiterTypeReference,
                                            [ expression.LeftType ]
                                        )

                                    let taskAwaiterRightType =
                                        CliGenericType(
                                            expression.TaskAwaiterTypeReference,
                                            [ expression.RightType ]
                                        )

                                    let rightValueTaskIsCompletedSuccessfully = {
                                        DeclaringType =
                                            CliDeclaringType expression.RightValueTaskType
                                        Name = "get_IsCompletedSuccessfully"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliBoolean
                                        TargetStableId = None
                                    }

                                    let rightValueTaskResult = {
                                        DeclaringType =
                                            CliDeclaringType expression.RightValueTaskType
                                        Name = "get_Result"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let rightValueTaskAsTask = {
                                        DeclaringType =
                                            CliDeclaringType expression.RightValueTaskType
                                        Name = "AsTask"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let outputValueTaskAsTask = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = "AsTask"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let tupleConstructor = {
                                        DeclaringType = CliDeclaringType expression.TupleType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [
                                            CliTypeParameter 0
                                            CliTypeParameter 1
                                        ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let leftTaskGetAwaiter = {
                                        DeclaringType = CliDeclaringType taskLeftType
                                        Name = "GetAwaiter"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskAwaiterTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let rightTaskGetAwaiter = {
                                        DeclaringType = CliDeclaringType taskRightType
                                        Name = "GetAwaiter"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskAwaiterTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let leftAwaiterGetResult = {
                                        DeclaringType = CliDeclaringType taskAwaiterLeftType
                                        Name = "GetResult"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let rightAwaiterGetResult = {
                                        DeclaringType = CliDeclaringType taskAwaiterRightType
                                        Name = "GetResult"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliTypeParameter 0
                                        TargetStableId = None
                                    }

                                    let cancellationToken = {
                                        DeclaringType =
                                            CliDeclaringType
                                                expression.OperationCanceledExceptionType
                                        Name = "get_CancellationToken"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = expression.CancellationTokenType
                                        TargetStableId = None
                                    }

                                    let fromCanceled = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromCanceled"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [
                                            expression.CancellationTokenType
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromException = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromException"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ expression.ExceptionType ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromResult = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.NonGenericTaskTypeReference
                                            )
                                        Name = "FromResult"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ CliMethodTypeParameter 0 ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let outputValueTaskValueConstructor = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [ CliTypeParameter 0 ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let outputValueTaskTaskConstructor = {
                                        DeclaringType =
                                            CliDeclaringType expression.OutputValueTaskType
                                        Name = ".ctor"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = [
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliTypeParameter 0 ]
                                            )
                                        ]
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let rightContinuationHelper = {
                                        DeclaringType = CliDeclaringType layout.DefinitionType
                                        Name = "ContinueRight"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = [
                                            taskRightType
                                            CliObject
                                        ]
                                        ReturnType = taskOutputType
                                        TargetStableId = None
                                    }

                                    let rightContinuationDelegateType =
                                        CliGenericType(
                                            expression.FuncTypeReference,
                                            [
                                                taskRightType
                                                CliObject
                                                taskOutputType
                                            ]
                                        )

                                    let rightContinuationDelegateConstructor = {
                                        DeclaringType =
                                            CliDeclaringType rightContinuationDelegateType
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

                                    let rightContinuationDefinitionDelegateType =
                                        CliGenericType(
                                            expression.FuncTypeReference,
                                            [
                                                CliGenericType(
                                                    expression.TaskTypeReference,
                                                    [ CliTypeParameter 0 ]
                                                )
                                                CliObject
                                                CliMethodTypeParameter 0
                                            ]
                                        )

                                    let rightContinueWith = {
                                        DeclaringType = CliDeclaringType taskRightType
                                        Name = "ContinueWith"
                                        GenericArity = 1
                                        IsInstance = true
                                        ParameterTypes = [
                                            rightContinuationDefinitionDelegateType
                                            CliObject
                                            expression.CancellationTokenType
                                            expression.TaskContinuationOptionsType
                                            expression.TaskSchedulerType
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let cancellationTokenNone = {
                                        DeclaringType =
                                            CliDeclaringType expression.CancellationTokenType
                                        Name = "get_None"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = []
                                        ReturnType = expression.CancellationTokenType
                                        TargetStableId = None
                                    }

                                    let taskSchedulerDefault = {
                                        DeclaringType =
                                            CliDeclaringType expression.TaskSchedulerType
                                        Name = "get_Default"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = []
                                        ReturnType = expression.TaskSchedulerType
                                        TargetStableId = None
                                    }

                                    let unwrap = {
                                        DeclaringType =
                                            CliDeclaringType(
                                                CliNamedType
                                                    expression.TaskExtensionsTypeReference
                                            )
                                        Name = "Unwrap"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [
                                                    CliGenericType(
                                                        expression.TaskTypeReference,
                                                        [ CliMethodTypeParameter 0 ]
                                                    )
                                                ]
                                            )
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let completedInstructions = [
                                        MarkLabel 0
                                        LoadArgumentAddress 1
                                        CallMethod rightValueTaskIsCompletedSuccessfully
                                        BranchIfFalse 7
                                        LoadArgument 0
                                        LoadArgumentAddress 1
                                        CallMethod rightValueTaskResult
                                        NewObject tupleConstructor
                                        NewObject outputValueTaskValueConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 7
                                        LoadArgumentAddress 1
                                        CallMethod rightValueTaskAsTask
                                        LoadNull
                                        LoadFunctionPointer rightContinuationHelper
                                        NewObject rightContinuationDelegateConstructor
                                        LoadArgument 0
                                        Box expression.LeftType
                                        CallMethod cancellationTokenNone
                                        LoadInt32(
                                            int
                                                System.Threading.Tasks.TaskContinuationOptions.ExecuteSynchronously
                                        )
                                        CallMethod taskSchedulerDefault
                                        CallGenericMethod(
                                            rightContinueWith,
                                            [ taskOutputType ]
                                        )
                                        CallGenericMethod(
                                            unwrap,
                                            [ expression.TupleType ]
                                        )
                                        NewObject outputValueTaskTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 1
                                        LoadLocal 1
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.TupleType ]
                                        )
                                        NewObject outputValueTaskTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 2
                                        LoadLocal 2
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.TupleType ]
                                        )
                                        NewObject outputValueTaskTaskConstructor
                                        StoreLocal 0
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 0
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let completedMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.CompletedStableId
                                        Name = "InvokeCompleted"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "left"
                                                Type = expression.LeftType
                                                Attributes = []
                                            }
                                            {
                                                Name = "rightSource"
                                                Type = expression.RightValueTaskType
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "result"
                                                Type = expression.OutputValueTaskType
                                            }
                                            {
                                                Index = 1
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 2
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                        ]
                                        ReturnType = expression.OutputValueTaskType
                                        Instructions = completedInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 8
                                        DependencyIds =
                                            instructionDependencyIds completedInstructions
                                            @ (cliTypeDependencyIds expression.LeftType)
                                            @ (cliTypeDependencyIds
                                                expression.RightValueTaskType)
                                            @ (cliTypeDependencyIds
                                                expression.OutputValueTaskType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.CompletedStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let rightContinuationInstructions = [
                                        MarkLabel 0
                                        LoadArgument 1
                                        UnboxAny expression.LeftType
                                        StoreLocal 1
                                        LoadArgument 0
                                        CallVirtualMethod rightTaskGetAwaiter
                                        StoreLocal 0
                                        LoadLocalAddress 0
                                        CallMethod rightAwaiterGetResult
                                        StoreLocal 2
                                        LoadLocal 1
                                        LoadLocal 2
                                        NewObject tupleConstructor
                                        CallGenericMethod(
                                            fromResult,
                                            [ expression.TupleType ]
                                        )
                                        StoreLocal 5
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 3
                                        LoadLocal 3
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.TupleType ]
                                        )
                                        StoreLocal 5
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 4
                                        LoadLocal 4
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.TupleType ]
                                        )
                                        StoreLocal 5
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 5
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let rightContinuationMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.RightContinuationStableId
                                        Name = "ContinueRight"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "source"
                                                Type = taskRightType
                                                Attributes = []
                                            }
                                            {
                                                Name = "state"
                                                Type = CliObject
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "awaiter"
                                                Type = taskAwaiterRightType
                                            }
                                            {
                                                Index = 1
                                                Name = "left"
                                                Type = expression.LeftType
                                            }
                                            {
                                                Index = 2
                                                Name = "right"
                                                Type = expression.RightType
                                            }
                                            {
                                                Index = 3
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 4
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                            {
                                                Index = 5
                                                Name = "result"
                                                Type = taskOutputType
                                            }
                                        ]
                                        ReturnType = taskOutputType
                                        Instructions = rightContinuationInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 3
                                        DependencyIds =
                                            instructionDependencyIds
                                                rightContinuationInstructions
                                            @ (cliTypeDependencyIds taskRightType)
                                            @ (cliTypeDependencyIds taskOutputType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.RightContinuationStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    let completedHelper = {
                                        DeclaringType = CliDeclaringType layout.DefinitionType
                                        Name = "InvokeCompleted"
                                        GenericArity = 0
                                        IsInstance = false
                                        ParameterTypes = [
                                            expression.LeftType
                                            expression.RightValueTaskType
                                        ]
                                        ReturnType = expression.OutputValueTaskType
                                        TargetStableId = None
                                    }

                                    let leftContinuationInstructions = [
                                        MarkLabel 0
                                        LoadArgument 0
                                        CallVirtualMethod leftTaskGetAwaiter
                                        StoreLocal 0
                                        LoadLocalAddress 0
                                        CallMethod leftAwaiterGetResult
                                        StoreLocal 1
                                        LoadArgument 1
                                        UnboxAny expression.RightValueTaskType
                                        StoreLocal 2
                                        LoadLocal 1
                                        LoadLocal 2
                                        CallMethod completedHelper
                                        StoreLocal 3
                                        LoadLocalAddress 3
                                        CallMethod outputValueTaskAsTask
                                        StoreLocal 6
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 4
                                        LoadLocal 4
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.TupleType ]
                                        )
                                        StoreLocal 6
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 5
                                        LoadLocal 5
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.TupleType ]
                                        )
                                        StoreLocal 6
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 6
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let leftContinuationMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.LeftContinuationStableId
                                        Name = "ContinueLeft"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "source"
                                                Type = taskLeftType
                                                Attributes = []
                                            }
                                            {
                                                Name = "state"
                                                Type = CliObject
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "awaiter"
                                                Type = taskAwaiterLeftType
                                            }
                                            {
                                                Index = 1
                                                Name = "left"
                                                Type = expression.LeftType
                                            }
                                            {
                                                Index = 2
                                                Name = "rightSource"
                                                Type = expression.RightValueTaskType
                                            }
                                            {
                                                Index = 3
                                                Name = "valueTaskResult"
                                                Type = expression.OutputValueTaskType
                                            }
                                            {
                                                Index = 4
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 5
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                            {
                                                Index = 6
                                                Name = "result"
                                                Type = taskOutputType
                                            }
                                        ]
                                        ReturnType = taskOutputType
                                        Instructions = leftContinuationInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 3
                                        DependencyIds =
                                            instructionDependencyIds
                                                leftContinuationInstructions
                                            @ (cliTypeDependencyIds taskLeftType)
                                            @ (cliTypeDependencyIds taskOutputType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.LeftContinuationStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    Some {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace = String.Empty
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            Some moduleDeclaration.StableId
                                        Kind = ClosureContainer
                                        GenericParameters =
                                            methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = []
                                        StaticFields = []
                                        Properties = []
                                        Methods = [
                                            completedMethod
                                            rightContinuationMethod
                                            leftContinuationMethod
                                        ]
                                    }
                                | _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _
                        | TypedStructType _ -> []
                    )
                )

            let rec valueTaskOfUnitExpressions =
                function
                | TypedValueTaskOfUnit expression -> [ expression ]
                | TypedLet(_, _, _, _, value, body, _, _) ->
                    valueTaskOfUnitExpressions value
                    @ valueTaskOfUnitExpressions body
                | TypedLocalAssignment(_, _, value)
                | TypedBooleanNegation(value, _)
                | TypedUpcast(_, _, _, value) -> valueTaskOfUnitExpressions value
                | TypedEquality(left, right, _) ->
                    valueTaskOfUnitExpressions left
                    @ valueTaskOfUnitExpressions right
                | TypedStaticMethodCall(_, _, arguments)
                | TypedObjectConstruction(_, arguments) ->
                    arguments
                    |> List.collect valueTaskOfUnitExpressions
                | TypedFunctionApplication(_, _, _, functionExpression, argumentExpression) ->
                    valueTaskOfUnitExpressions functionExpression
                    @ valueTaskOfUnitExpressions argumentExpression
                | TypedInstanceFieldGet(receiver, _) ->
                    valueTaskOfUnitExpressions receiver
                | TypedInstanceMethodCall(_, receiver, arguments) ->
                    valueTaskOfUnitExpressions receiver
                    @ (arguments
                       |> List.collect valueTaskOfUnitExpressions)
                | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                    valueTaskOfUnitExpressions condition
                    @ valueTaskOfUnitExpressions ifTrue
                    @ valueTaskOfUnitExpressions ifFalse
                | TypedTryWith(body, _, _, _, handler, _, _, _, _, _) ->
                    valueTaskOfUnitExpressions body
                    @ valueTaskOfUnitExpressions handler
                | TypedNullMatch(input, _, _, _, ifNull, ifNotNull, _, _, _, _) ->
                    valueTaskOfUnitExpressions input
                    @ valueTaskOfUnitExpressions ifNull
                    @ valueTaskOfUnitExpressions ifNotNull
                | TypedTypeTestMatch(input, _, _, _, guard, ifMatched, ifNotMatched, _, _, _, _) ->
                    valueTaskOfUnitExpressions input
                    @ (guard
                       |> Option.map (fst >> valueTaskOfUnitExpressions)
                       |> Option.defaultValue [])
                    @ valueTaskOfUnitExpressions ifMatched
                    @ valueTaskOfUnitExpressions ifNotMatched
                | TypedSequential expressions ->
                    expressions
                    |> List.collect (fun (expression, _, _) ->
                        valueTaskOfUnitExpressions expression
                    )
                | TypedFunctionLambda expression ->
                    valueTaskOfUnitExpressions expression.Body
                | TypedDelegateLambda expression ->
                    valueTaskOfUnitExpressions expression.LambdaBody
                | TypedObjectExpression(_, _, constructorArguments, members, _) ->
                    (constructorArguments
                     |> List.collect valueTaskOfUnitExpressions)
                    @ (members
                       |> List.collect (fun memberDeclaration ->
                           valueTaskOfUnitExpressions memberDeclaration.Body
                       ))
                | TypedIntegerLiteral _
                | TypedStringLiteral _
                | TypedNullLiteral
                | TypedUnitLiteral
                | TypedReceiverReference
                | TypedParameterReference _
                | TypedLocalReference _
                | TypedAddressOf _
                | TypedDefaultValue _
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedValueTaskBind _
                | TypedValueTaskApply _
                | TypedValueTaskZip _
                | TypedResumableCode _
                | TypedResumableTryFinally _
                | TypedTraitCall _ -> []

            let valueTaskOfUnitHelperTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        match declaration with
                        | TypedNestedModule moduleDeclaration ->
                            moduleDeclaration.Methods
                            |> List.collect (fun methodDeclaration ->
                                valueTaskOfUnitExpressions methodDeclaration.Body
                                |> List.map (fun unitExpression ->
                                    let layout =
                                        valueTaskOfUnitHelperLayout
                                            methodDeclaration
                                            unitExpression

                                    let expression = layout.DefinitionExpression

                                    let nonGenericTaskType =
                                        CliNamedType expression.NonGenericTaskTypeReference

                                    let taskUnitType =
                                        CliGenericType(
                                            expression.TaskTypeReference,
                                            [ expression.UnitType ]
                                        )

                                    let taskAwaiterType =
                                        CliNamedType
                                            expression.NonGenericTaskAwaiterTypeReference

                                    let taskGetAwaiter = {
                                        DeclaringType =
                                            CliDeclaringType nonGenericTaskType
                                        Name = "GetAwaiter"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = taskAwaiterType
                                        TargetStableId = None
                                    }

                                    let awaiterGetResult = {
                                        DeclaringType =
                                            CliDeclaringType taskAwaiterType
                                        Name = "GetResult"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = CliVoid
                                        TargetStableId = None
                                    }

                                    let cancellationToken = {
                                        DeclaringType =
                                            CliDeclaringType
                                                expression.OperationCanceledExceptionType
                                        Name = "get_CancellationToken"
                                        GenericArity = 0
                                        IsInstance = true
                                        ParameterTypes = []
                                        ReturnType = expression.CancellationTokenType
                                        TargetStableId = None
                                    }

                                    let fromCanceled = {
                                        DeclaringType =
                                            CliDeclaringType nonGenericTaskType
                                        Name = "FromCanceled"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [
                                            expression.CancellationTokenType
                                        ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromException = {
                                        DeclaringType =
                                            CliDeclaringType nonGenericTaskType
                                        Name = "FromException"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ expression.ExceptionType ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let fromResult = {
                                        DeclaringType =
                                            CliDeclaringType nonGenericTaskType
                                        Name = "FromResult"
                                        GenericArity = 1
                                        IsInstance = false
                                        ParameterTypes = [ CliMethodTypeParameter 0 ]
                                        ReturnType =
                                            CliGenericType(
                                                expression.TaskTypeReference,
                                                [ CliMethodTypeParameter 0 ]
                                            )
                                        TargetStableId = None
                                    }

                                    let continuationInstructions = [
                                        MarkLabel 0
                                        LoadArgument 0
                                        CallVirtualMethod taskGetAwaiter
                                        StoreLocal 0
                                        LoadLocalAddress 0
                                        CallMethod awaiterGetResult
                                        LoadNull
                                        CallGenericMethod(
                                            fromResult,
                                            [ expression.UnitType ]
                                        )
                                        StoreLocal 3
                                        Leave 6
                                        MarkLabel 1
                                        MarkLabel 2
                                        StoreLocal 1
                                        LoadLocal 1
                                        CallVirtualMethod cancellationToken
                                        CallGenericMethod(
                                            fromCanceled,
                                            [ expression.UnitType ]
                                        )
                                        StoreLocal 3
                                        Leave 6
                                        MarkLabel 3
                                        MarkLabel 4
                                        StoreLocal 2
                                        LoadLocal 2
                                        CallGenericMethod(
                                            fromException,
                                            [ expression.UnitType ]
                                        )
                                        StoreLocal 3
                                        Leave 6
                                        MarkLabel 5
                                        MarkLabel 6
                                        LoadLocal 3
                                        Return
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            2,
                                            3,
                                            expression.OperationCanceledExceptionType
                                        )
                                        DefineCatchRegion(
                                            0,
                                            1,
                                            4,
                                            5,
                                            expression.ExceptionType
                                        )
                                    ]

                                    let continuationMethod = {
                                        SchemaVersion = querySchema
                                        StableId = layout.ContinuationStableId
                                        Name = "Continue"
                                        Kind = ModuleFunction
                                        GenericParameters = []
                                        Constraints = []
                                        GenericParameterConstraints = []
                                        Attributes = []
                                        Parameters = [
                                            {
                                                Name = "source"
                                                Type = nonGenericTaskType
                                                Attributes = []
                                            }
                                            {
                                                Name = "state"
                                                Type = CliObject
                                                Attributes = []
                                            }
                                        ]
                                        Locals = [
                                            {
                                                Index = 0
                                                Name = "awaiter"
                                                Type = taskAwaiterType
                                            }
                                            {
                                                Index = 1
                                                Name = "cancellationError"
                                                Type =
                                                    expression.OperationCanceledExceptionType
                                            }
                                            {
                                                Index = 2
                                                Name = "error"
                                                Type = expression.ExceptionType
                                            }
                                            {
                                                Index = 3
                                                Name = "result"
                                                Type = taskUnitType
                                            }
                                        ]
                                        ReturnType = taskUnitType
                                        Instructions = continuationInstructions
                                        EmitDefaultSequencePoint = true
                                        MaxStack = 2
                                        DependencyIds =
                                            instructionDependencyIds
                                                continuationInstructions
                                            @ (cliTypeDependencyIds nonGenericTaskType)
                                            @ (cliTypeDependencyIds taskUnitType)
                                            |> List.distinct
                                        ContentHash =
                                            Fingerprint.parts [
                                                layout.ContinuationStableId
                                                methodImplementationHash methodDeclaration
                                            ]
                                        DocumentIndex = typed.DocumentIndex
                                        DocumentChecksum = typed.SourceChecksum
                                        Range = expression.Range
                                    }

                                    {
                                        SchemaVersion = querySchema
                                        StableId = layout.StableId
                                        Namespace = String.Empty
                                        Name = layout.Name
                                        IsPublic = false
                                        EnclosingTypeStableId =
                                            Some moduleDeclaration.StableId
                                        Kind = ClosureContainer
                                        GenericParameters =
                                            methodDeclaration.GenericParameters
                                        Attributes = []
                                        LiteralFields = []
                                        InstanceFields = []
                                        StaticFields = []
                                        Properties = []
                                        Methods = [ continuationMethod ]
                                    }
                                )
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _
                        | TypedStructType _ -> []
                    )
                )

            let staticTypes =
                modulesWithContentHashes
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

                    declarationsWithContentHashes
                    |> List.choose (fun (declaration, typeContentHash) ->
                        match declaration with
                        | TypedStaticType typeDeclaration ->
                            let methods =
                                typeDeclaration.Methods
                                |> List.map (fun methodDeclaration ->
                                    let kind =
                                        if typeDeclaration.IsPublic then
                                            StaticInlineMemberStub
                                        else
                                            InternalStaticInlineMemberStub

                                    let contentHash =
                                        Fingerprint.parts [
                                            typeContentHash
                                            methodDeclaration.ExportFingerprint
                                        ]

                                    methodFragment
                                        kind
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
                                IsPublic = typeDeclaration.IsPublic
                                EnclosingTypeStableId = None
                                Kind = StaticMemberContainer
                                GenericParameters = []
                                Attributes = []
                                LiteralFields = []
                                InstanceFields = []
                                StaticFields = []
                                Properties = []
                                Methods = methods
                            }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedObjectType _
                        | TypedStructType _ -> None
                    )
                )
                |> List.collect id

            let rec objectExpressions =
                function
                | (TypedObjectExpression(_, _, constructorArguments, members, _) as expression) -> [
                    yield expression

                    for argument in constructorArguments do
                        yield! objectExpressions argument

                    for memberDeclaration in members do
                        yield! objectExpressions memberDeclaration.Body
                  ]
                | TypedLet(_, _, _, _, value, body, _, _) ->
                    objectExpressions value
                    @ objectExpressions body
                | TypedLocalAssignment(_, _, value)
                | TypedBooleanNegation(value, _)
                | TypedUpcast(_, _, _, value) -> objectExpressions value
                | TypedEquality(left, right, _) ->
                    objectExpressions left
                    @ objectExpressions right
                | TypedStaticMethodCall(_, _, arguments) ->
                    arguments
                    |> List.collect objectExpressions
                | TypedObjectConstruction(_, arguments) ->
                    arguments
                    |> List.collect objectExpressions
                | TypedFunctionApplication(_, _, _, functionExpression, argumentExpression) ->
                    objectExpressions functionExpression
                    @ objectExpressions argumentExpression
                | TypedInstanceFieldGet(receiver, _) -> objectExpressions receiver
                | TypedInstanceMethodCall(_, receiver, arguments) ->
                    objectExpressions receiver
                    @ (arguments
                       |> List.collect objectExpressions)
                | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                    objectExpressions condition
                    @ objectExpressions ifTrue
                    @ objectExpressions ifFalse
                | TypedTryWith(body, _, _, _, handler, _, _, _, _, _) ->
                    objectExpressions body
                    @ objectExpressions handler
                | TypedNullMatch(input, _, _, _, ifNull, ifNotNull, _, _, _, _) ->
                    objectExpressions input
                    @ objectExpressions ifNull
                    @ objectExpressions ifNotNull
                | TypedTypeTestMatch(input, _, _, _, guard, ifMatched, ifNotMatched, _, _, _, _) ->
                    objectExpressions input
                    @ (guard
                       |> Option.map (fst >> objectExpressions)
                       |> Option.defaultValue [])
                    @ objectExpressions ifMatched
                    @ objectExpressions ifNotMatched
                | TypedSequential expressions ->
                    expressions
                    |> List.collect (fun (expression, _, _) -> objectExpressions expression)
                | TypedFunctionLambda expression -> objectExpressions expression.Body
                | TypedDelegateLambda expression ->
                    objectExpressions expression.LambdaBody
                | TypedIntegerLiteral _
                | TypedStringLiteral _
                | TypedNullLiteral
                | TypedUnitLiteral
                | TypedReceiverReference
                | TypedParameterReference _
                | TypedLocalReference _
                | TypedAddressOf _
                | TypedDefaultValue _
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedValueTaskBind _
                | TypedValueTaskApply _
                | TypedValueTaskZip _
                | TypedValueTaskOfUnit _
                | TypedResumableCode _
                | TypedResumableTryFinally _
                | TypedTraitCall _ -> []

            let rec functionLambdaExpressions =
                function
                | TypedFunctionLambda expression ->
                    expression
                    :: functionLambdaExpressions expression.Body
                | TypedDelegateLambda expression ->
                    functionLambdaExpressions expression.LambdaBody
                | TypedLet(_, _, _, _, value, body, _, _) ->
                    functionLambdaExpressions value
                    @ functionLambdaExpressions body
                | TypedLocalAssignment(_, _, value)
                | TypedBooleanNegation(value, _)
                | TypedUpcast(_, _, _, value) -> functionLambdaExpressions value
                | TypedEquality(left, right, _) ->
                    functionLambdaExpressions left
                    @ functionLambdaExpressions right
                | TypedStaticMethodCall(_, _, arguments)
                | TypedObjectConstruction(_, arguments) ->
                    arguments
                    |> List.collect functionLambdaExpressions
                | TypedFunctionApplication(_, _, _, functionExpression, argumentExpression) ->
                    functionLambdaExpressions functionExpression
                    @ functionLambdaExpressions argumentExpression
                | TypedInstanceFieldGet(receiver, _) -> functionLambdaExpressions receiver
                | TypedInstanceMethodCall(_, receiver, arguments) ->
                    functionLambdaExpressions receiver
                    @ (arguments
                       |> List.collect functionLambdaExpressions)
                | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                    functionLambdaExpressions condition
                    @ functionLambdaExpressions ifTrue
                    @ functionLambdaExpressions ifFalse
                | TypedTryWith(body, _, _, _, handler, _, _, _, _, _) ->
                    functionLambdaExpressions body
                    @ functionLambdaExpressions handler
                | TypedNullMatch(input, _, _, _, ifNull, ifNotNull, _, _, _, _) ->
                    functionLambdaExpressions input
                    @ functionLambdaExpressions ifNull
                    @ functionLambdaExpressions ifNotNull
                | TypedTypeTestMatch(input, _, _, _, guard, ifMatched, ifNotMatched, _, _, _, _) ->
                    functionLambdaExpressions input
                    @ (guard
                       |> Option.map (fst >> functionLambdaExpressions)
                       |> Option.defaultValue [])
                    @ functionLambdaExpressions ifMatched
                    @ functionLambdaExpressions ifNotMatched
                | TypedSequential expressions ->
                    expressions
                    |> List.collect (fun (expression, _, _) ->
                        functionLambdaExpressions expression
                    )
                | TypedObjectExpression(_, _, constructorArguments, members, _) ->
                    (constructorArguments
                     |> List.collect functionLambdaExpressions)
                    @ (members
                       |> List.collect (fun memberDeclaration ->
                           functionLambdaExpressions memberDeclaration.Body
                       ))
                | TypedIntegerLiteral _
                | TypedStringLiteral _
                | TypedNullLiteral
                | TypedUnitLiteral
                | TypedReceiverReference
                | TypedParameterReference _
                | TypedLocalReference _
                | TypedAddressOf _
                | TypedDefaultValue _
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedValueTaskBind _
                | TypedValueTaskApply _
                | TypedValueTaskZip _
                | TypedValueTaskOfUnit _
                | TypedResumableCode _
                | TypedResumableTryFinally _
                | TypedTraitCall _ -> []

            let rec delegateLambdaExpressions =
                function
                | TypedDelegateLambda expression ->
                    expression
                    :: delegateLambdaExpressions expression.LambdaBody
                | TypedFunctionLambda expression ->
                    delegateLambdaExpressions expression.Body
                | TypedLet(_, _, _, _, value, body, _, _) ->
                    delegateLambdaExpressions value
                    @ delegateLambdaExpressions body
                | TypedLocalAssignment(_, _, value)
                | TypedBooleanNegation(value, _)
                | TypedUpcast(_, _, _, value) -> delegateLambdaExpressions value
                | TypedEquality(left, right, _) ->
                    delegateLambdaExpressions left
                    @ delegateLambdaExpressions right
                | TypedStaticMethodCall(_, _, arguments)
                | TypedObjectConstruction(_, arguments) ->
                    arguments
                    |> List.collect delegateLambdaExpressions
                | TypedFunctionApplication(_, _, _, functionExpression, argumentExpression) ->
                    delegateLambdaExpressions functionExpression
                    @ delegateLambdaExpressions argumentExpression
                | TypedInstanceFieldGet(receiver, _) -> delegateLambdaExpressions receiver
                | TypedInstanceMethodCall(_, receiver, arguments) ->
                    delegateLambdaExpressions receiver
                    @ (arguments
                       |> List.collect delegateLambdaExpressions)
                | TypedConditional(condition, ifTrue, ifFalse, _, _, _) ->
                    delegateLambdaExpressions condition
                    @ delegateLambdaExpressions ifTrue
                    @ delegateLambdaExpressions ifFalse
                | TypedTryWith(body, _, _, _, handler, _, _, _, _, _) ->
                    delegateLambdaExpressions body
                    @ delegateLambdaExpressions handler
                | TypedNullMatch(input, _, _, _, ifNull, ifNotNull, _, _, _, _) ->
                    delegateLambdaExpressions input
                    @ delegateLambdaExpressions ifNull
                    @ delegateLambdaExpressions ifNotNull
                | TypedTypeTestMatch(input, _, _, _, guard, ifMatched, ifNotMatched, _, _, _, _) ->
                    delegateLambdaExpressions input
                    @ (guard
                       |> Option.map (fst >> delegateLambdaExpressions)
                       |> Option.defaultValue [])
                    @ delegateLambdaExpressions ifMatched
                    @ delegateLambdaExpressions ifNotMatched
                | TypedSequential expressions ->
                    expressions
                    |> List.collect (fun (expression, _, _) ->
                        delegateLambdaExpressions expression
                    )
                | TypedObjectExpression(_, _, constructorArguments, members, _) ->
                    (constructorArguments
                     |> List.collect delegateLambdaExpressions)
                    @ (members
                       |> List.collect (fun memberDeclaration ->
                           delegateLambdaExpressions memberDeclaration.Body
                       ))
                | TypedIntegerLiteral _
                | TypedStringLiteral _
                | TypedNullLiteral
                | TypedUnitLiteral
                | TypedReceiverReference
                | TypedParameterReference _
                | TypedLocalReference _
                | TypedAddressOf _
                | TypedDefaultValue _
                | TypedBoundInstanceMethod _
                | TypedUnitLambda _
                | TypedValueTaskBind _
                | TypedValueTaskApply _
                | TypedValueTaskZip _
                | TypedValueTaskOfUnit _
                | TypedResumableCode _
                | TypedResumableTryFinally _
                | TypedTraitCall _ -> []

            let objectTypes =
                modulesWithContentHashes
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

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
                            | TypedCurrentModuleAugmentation _ -> None
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
                                        | TypedStaticObjectMethod methodDeclaration ->
                                            let extensionMethod = {
                                                methodDeclaration with
                                                    Name =
                                                        typeDeclaration.Name
                                                        + "."
                                                        + methodDeclaration.Name
                                                        + ".Static"
                                                    ExportFingerprint =
                                                        Fingerprint.parts [
                                                            methodDeclaration.ExportFingerprint
                                                            "static-type-extension"
                                                            TypeIdentity.cliType extendedType
                                                        ]
                                            }

                                            methodFragment
                                                StaticTypeExtensionMember
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
                                    )

                                Some {
                                    SchemaVersion = querySchema
                                    StableId = typeDeclaration.StableId
                                    Namespace =
                                        if isNested then
                                            String.Empty
                                        else
                                            typed.Namespace
                                    Name = moduleName
                                    IsPublic = true
                                    EnclosingTypeStableId =
                                        if isNested then
                                            Some moduleTypeStableId
                                        else
                                            None
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
                                    StaticFields = []
                                    Properties = []
                                    Methods = methods
                                }
                            | OrdinaryTypedObjectType ->
                                let constructorStableId =
                                    typeDeclaration.StableId
                                    + "/constructor:unit"

                                let baseType =
                                    typeDeclaration.BaseType
                                    |> Option.defaultValue CliObject

                                let baseDeclaringType, baseConstructorStableId =
                                    match baseType with
                                    | CliObject ->
                                        CoreDeclaringType {
                                            Namespace = "System"
                                            Name = "Object"
                                        },
                                        None
                                    | CliNamedType typeReference
                                    | CliGenericType(typeReference, _) ->
                                        CliDeclaringType baseType,
                                        if String.IsNullOrEmpty(typeReference.AssemblyName) then
                                            Some(
                                                typeReference.DeclarationId
                                                + "/constructor:unit"
                                            )
                                        else
                                            None
                                    | CliInt32
                                    | CliBoolean
                                    | CliString
                                    | CliNativeInt
                                    | CliVoid
                                    | CliByRef _
                                    | CliTypeParameter _
                                    | CliMethodTypeParameter _ ->
                                        invalidOp "an object type has an invalid base type"

                                let objectConstructor = {
                                    DeclaringType = baseDeclaringType
                                    Name = ".ctor"
                                    GenericArity = 0
                                    IsInstance = true
                                    ParameterTypes = []
                                    ReturnType = CliVoid
                                    TargetStableId = baseConstructorStableId
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
                                    DependencyIds = [ objectConstructor.DependencyId ]
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
                                    Kind = ObjectContainer baseType
                                    GenericParameters = []
                                    Attributes = [
                                        compilationMappingAttribute
                                            typeDeclaration.StableId
                                            ObjectTypeConstruct
                                    ]
                                    LiteralFields = []
                                    InstanceFields = []
                                    StaticFields = []
                                    Properties = []
                                    Methods =
                                        constructor
                                        :: methods
                                }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> None
                    )
                )
                |> List.collect id

            let objectExpressionTypes =
                modulesWithContentHashes
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

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
                            | TypedNestedModule _
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
                                                    members,
                                                    range) ->
                                if not (List.isEmpty constructorArguments) then
                                    invalidOp
                                        "object-expression constructor arguments reached symbolic lowering"

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

                                let overrideMethods =
                                    members
                                    |> List.map (fun memberDeclaration ->
                                        if not memberDeclaration.IsOverride then
                                            invalidOp
                                                "an object-expression member must be an override"

                                        let overrideStableId =
                                            typeReference.DeclarationId
                                            + "/method:"
                                            + memberDeclaration.Name

                                        let overrideDeclaration = {
                                            StableId = overrideStableId
                                            Name = memberDeclaration.Name
                                            IsPublic = true
                                            GenericParameters = []
                                            Constraints = []
                                            Attributes = []
                                            Parameters = memberDeclaration.Parameters
                                            ReturnType = memberDeclaration.ReturnType
                                            Body = memberDeclaration.Body
                                            EmitHiddenEntrySequencePoint = false
                                            ExportFingerprint =
                                                Fingerprint.parts [
                                                    overrideStableId
                                                    "override"
                                                    memberDeclaration.ReceiverName
                                                    yield!
                                                        memberDeclaration.Parameters
                                                        |> List.map TypeIdentity.parameter
                                                    TypeIdentity.cliType
                                                        memberDeclaration.ReturnType
                                                    TypeIdentity.inlineBody memberDeclaration.Body
                                                ]
                                            Range = memberDeclaration.BodyRange
                                        }

                                        methodFragment
                                            ObjectExpressionOverride
                                            documentIndex
                                            typed.SourceChecksum
                                            overrideStableId
                                            (methodImplementationHash overrideDeclaration)
                                            overrideDeclaration
                                    )

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
                                    StaticFields = []
                                    Properties = []
                                    Methods = constructor :: overrideMethods
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
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

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
                                    | TypedNullLiteral
                                    | TypedUnitLiteral
                                    | TypedReceiverReference
                                    | TypedParameterReference _
                                    | TypedLocalReference _
                                    | TypedLet _
                                    | TypedAddressOf _
                                    | TypedInstanceFieldGet _
                                    | TypedStaticMethodCall _
                                    | TypedObjectConstruction _
                                    | TypedDefaultValue _
                                    | TypedFunctionApplication _
                                    | TypedInstanceMethodCall _
                                    | TypedBoundInstanceMethod _
                                    | TypedUnitLambda _
                                    | TypedFunctionLambda _
                                    | TypedDelegateLambda _
                                    | TypedValueTaskBind _
                                    | TypedValueTaskApply _
                                    | TypedValueTaskZip _
                                    | TypedValueTaskOfUnit _
                                    | TypedConditional _
                                    | TypedUpcast _
                                    | TypedSequential _
                                    | TypedLocalAssignment _
                                    | TypedBooleanNegation _
                                    | TypedEquality _
                                    | TypedTryWith _
                                    | TypedNullMatch _
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
                                        StaticFields = []
                                        Properties = []
                                        Methods = [
                                            constructor
                                            invoke
                                        ]
                                    }
                                | None -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> []
                    )
                )
                |> List.collect id

            let boundMemberClosureTypes =
                modulesWithContentHashes
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

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
                                        StaticFields = []
                                        Properties = []
                                        Methods = [
                                            constructor
                                            invoke
                                        ]
                                    }
                                | TypedIntegerLiteral _
                                | TypedStringLiteral _
                                | TypedNullLiteral
                                | TypedUnitLiteral
                                | TypedReceiverReference
                                | TypedParameterReference _
                                | TypedLocalReference _
                                | TypedLet _
                                | TypedLocalAssignment _
                                | TypedAddressOf _
                                | TypedInstanceFieldGet _
                                | TypedStaticMethodCall _
                                | TypedObjectConstruction _
                                | TypedDefaultValue _
                                | TypedFunctionApplication _
                                | TypedInstanceMethodCall _
                                | TypedUnitLambda _
                                | TypedFunctionLambda _
                                | TypedDelegateLambda _
                                | TypedValueTaskBind _
                                | TypedValueTaskApply _
                                | TypedValueTaskZip _
                                | TypedValueTaskOfUnit _
                                | TypedConditional _
                                | TypedUpcast _
                                | TypedSequential _
                                | TypedBooleanNegation _
                                | TypedEquality _
                                | TypedTryWith _
                                | TypedNullMatch _
                                | TypedTypeTestMatch _
                                | TypedResumableCode _
                                | TypedResumableTryFinally _
                                | TypedObjectExpression _
                                | TypedTraitCall _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> []
                    )
                )
                |> List.collect id

            let unitLambdaClosureTypes =
                modulesWithContentHashes
                |> List.map (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

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
                                        StaticFields = []
                                        Properties = []
                                        Methods = [
                                            constructor
                                            invoke
                                        ]
                                    }
                                | TypedIntegerLiteral _
                                | TypedStringLiteral _
                                | TypedNullLiteral
                                | TypedUnitLiteral
                                | TypedReceiverReference
                                | TypedParameterReference _
                                | TypedLocalReference _
                                | TypedLet _
                                | TypedLocalAssignment _
                                | TypedAddressOf _
                                | TypedInstanceFieldGet _
                                | TypedStaticMethodCall _
                                | TypedObjectConstruction _
                                | TypedDefaultValue _
                                | TypedFunctionApplication _
                                | TypedInstanceMethodCall _
                                | TypedBoundInstanceMethod _
                                | TypedFunctionLambda _
                                | TypedDelegateLambda _
                                | TypedValueTaskBind _
                                | TypedValueTaskApply _
                                | TypedValueTaskZip _
                                | TypedValueTaskOfUnit _
                                | TypedConditional _
                                | TypedUpcast _
                                | TypedSequential _
                                | TypedBooleanNegation _
                                | TypedEquality _
                                | TypedTryWith _
                                | TypedNullMatch _
                                | TypedTypeTestMatch _
                                | TypedResumableCode _
                                | TypedResumableTryFinally _
                                | TypedObjectExpression _
                                | TypedTraitCall _ -> None
                            )
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedStructType _ -> []
                    )
                )
                |> List.collect id

            let functionLambdaClosureTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested = typed.ContainerKind = ModuleSource

                    let declarationMethods =
                        function
                        | TypedMethod methodDeclaration -> [ methodDeclaration ]
                        | TypedStaticType typeDeclaration -> typeDeclaration.Methods
                        | TypedObjectType typeDeclaration ->
                            typeDeclaration.Methods
                            |> List.map _.Method
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStructType _ -> []

                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        declaration
                        |> declarationMethods
                        |> List.collect (fun methodDeclaration ->
                            methodDeclaration.Body
                            |> functionLambdaExpressions
                            |> List.map (fun expression ->
                                let closureStableId =
                                    match expression.ClosureType with
                                    | CliNamedType typeReference
                                    | CliGenericType(typeReference, _) ->
                                        typeReference.DeclarationId
                                    | _ ->
                                        invalidOp
                                            "an F# function lambda closure must be a named CLI type"

                                let constructorStableId =
                                    closureStableId
                                    + "/constructor"

                                let invokeStableId =
                                    closureStableId
                                    + "/method:Invoke"

                                let captures: TypedFunctionLambdaCapture list =
                                    expression.Captures
                                    |> List.map (fun capture -> {
                                        capture with
                                            Type =
                                                methodTypeParametersToTypeParameters capture.Type
                                            Field = mapTypedFieldAddress capture.Field
                                    })

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

                                let captureFieldReference capture = {
                                    DeclaringType =
                                        CliDeclaringType capture.Field.DeclaringType
                                    Name = capture.Field.Name
                                    FieldType = capture.Field.FieldType
                                    TargetStableId = capture.Field.TargetStableId
                                }

                                let constructorInstructions = [
                                    LoadArgument 0
                                    CallMethod objectConstructor

                                    for index, capture in captures |> List.indexed do
                                        LoadArgument 0
                                        LoadArgument(index + 1)
                                        StoreField(captureFieldReference capture)

                                    Return
                                ]

                                let constructor = {
                                    SchemaVersion = querySchema
                                    StableId = constructorStableId
                                    Name = ".ctor"
                                    Kind = ClosureConstructor
                                    GenericParameters = []
                                    Constraints = []
                                    GenericParameterConstraints = []
                                    Attributes = []
                                    Parameters =
                                        captures
                                        |> List.map (fun capture -> {
                                            Name = capture.Name
                                            Type = capture.Type
                                            Attributes = []
                                        })
                                    Locals = []
                                    ReturnType = CliVoid
                                    Instructions = constructorInstructions
                                    EmitDefaultSequencePoint = true
                                    MaxStack = 8
                                    DependencyIds =
                                        objectConstructor.DependencyId
                                        :: (captures
                                            |> List.map (
                                                captureFieldReference
                                                >> _.DependencyId
                                            ))
                                    ContentHash =
                                        Fingerprint.parts [
                                            constructorStableId
                                            objectConstructor.StableId

                                            yield!
                                                captures
                                                |> List.map (fun capture ->
                                                    Fingerprint.parts [
                                                        capture.Name
                                                        TypeIdentity.cliType capture.Type
                                                        capture.Field.Name
                                                    ]
                                                )
                                        ]
                                    DocumentIndex = documentIndex
                                    DocumentChecksum = typed.SourceChecksum
                                    Range = expression.LambdaRange
                                }

                                let invokeBody =
                                    expression.Body
                                    |> methodExpressionTypesToTypeParameters

                                let invokeDeclaration: TypedMethodDeclaration = {
                                    StableId = invokeStableId
                                    Name = "Invoke"
                                    IsPublic = false
                                    GenericParameters = []
                                    Constraints = []
                                    Attributes = []
                                    Parameters = [
                                        {
                                            Name = expression.ParameterName
                                            Type =
                                                methodTypeParametersToTypeParameters
                                                    expression.ParameterType
                                            Attributes = []
                                        }
                                    ]
                                    ReturnType =
                                        methodTypeParametersToTypeParameters
                                            expression.ReturnType
                                    Body = invokeBody
                                    EmitHiddenEntrySequencePoint = false
                                    ExportFingerprint =
                                        Fingerprint.parts [
                                            invokeStableId
                                            TypeIdentity.inlineBody invokeBody
                                        ]
                                    Range = expression.LambdaRange
                                }

                                let invoke =
                                    methodFragment
                                        ClosureInvoke
                                        documentIndex
                                        typed.SourceChecksum
                                        invokeStableId
                                        (methodImplementationHash invokeDeclaration)
                                        invokeDeclaration

                                {
                                    SchemaVersion = querySchema
                                    StableId = closureStableId
                                    Namespace =
                                        if isNested then String.Empty else typed.Namespace
                                    Name = expression.ClosureName
                                    IsPublic = false
                                    EnclosingTypeStableId =
                                        if isNested then Some moduleTypeStableId else None
                                    Kind = ClosureContainer
                                    GenericParameters = methodDeclaration.GenericParameters
                                    Attributes = []
                                    LiteralFields = []
                                    InstanceFields =
                                        captures
                                        |> List.map (fun capture -> {
                                            SchemaVersion = querySchema
                                            StableId =
                                                capture.Field.TargetStableId
                                                |> Option.defaultWith (fun () ->
                                                    closureStableId
                                                    + "/field:"
                                                    + capture.Name
                                                )
                                            Name = capture.Field.Name
                                            Type = capture.Type
                                            Attributes = []
                                            ContentHash =
                                                Fingerprint.parts [
                                                    capture.Name
                                                    TypeIdentity.cliType capture.Type
                                                ]
                                        })
                                    StaticFields = []
                                    Properties = []
                                    Methods = [
                                        constructor
                                        invoke
                                    ]
                                }
                            )
                        )
                    )
                )

            let delegateLambdaClosureTypes =
                modulesWithContentHashes
                |> List.collect (fun (typed, declarationsWithContentHashes) ->
                    let documentIndex = typed.DocumentIndex

                    let moduleTypeStableId =
                        moduleStableId
                        + "/type:"
                        + typed.StableId

                    let isNested = typed.ContainerKind = ModuleSource

                    let declarationMethods =
                        function
                        | TypedMethod methodDeclaration -> [ methodDeclaration ]
                        | TypedStaticType typeDeclaration -> typeDeclaration.Methods
                        | TypedObjectType typeDeclaration ->
                            typeDeclaration.Methods
                            |> List.map _.Method
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStructType _ -> []

                    declarationsWithContentHashes
                    |> List.collect (fun (declaration, _) ->
                        declaration
                        |> declarationMethods
                        |> List.collect (fun methodDeclaration ->
                            methodDeclaration.Body
                            |> delegateLambdaExpressions
                            |> List.map (fun expression ->
                                let closureStableId =
                                    match expression.ClosureType with
                                    | CliNamedType typeReference
                                    | CliGenericType(typeReference, _) ->
                                        typeReference.DeclarationId
                                    | _ ->
                                        invalidOp
                                            "a delegate lambda closure must be a named CLI type"

                                let constructorStableId =
                                    closureStableId
                                    + "/constructor"

                                let invokeStableId =
                                    closureStableId
                                    + "/method:Invoke"

                                let captures: TypedFunctionLambdaCapture list =
                                    expression.Captures
                                    |> List.map (fun capture -> {
                                        capture with
                                            Type =
                                                methodTypeParametersToTypeParameters capture.Type
                                            Field = mapTypedFieldAddress capture.Field
                                    })

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

                                let captureFieldReference capture = {
                                    DeclaringType =
                                        CliDeclaringType capture.Field.DeclaringType
                                    Name = capture.Field.Name
                                    FieldType = capture.Field.FieldType
                                    TargetStableId = capture.Field.TargetStableId
                                }

                                let constructorInstructions = [
                                    LoadArgument 0
                                    CallMethod objectConstructor

                                    for index, capture in captures |> List.indexed do
                                        LoadArgument 0
                                        LoadArgument(index + 1)
                                        StoreField(captureFieldReference capture)

                                    Return
                                ]

                                let constructor = {
                                    SchemaVersion = querySchema
                                    StableId = constructorStableId
                                    Name = ".ctor"
                                    Kind = ClosureConstructor
                                    GenericParameters = []
                                    Constraints = []
                                    GenericParameterConstraints = []
                                    Attributes = []
                                    Parameters =
                                        captures
                                        |> List.map (fun capture -> {
                                            Name = capture.Name
                                            Type = capture.Type
                                            Attributes = []
                                        })
                                    Locals = []
                                    ReturnType = CliVoid
                                    Instructions = constructorInstructions
                                    EmitDefaultSequencePoint = true
                                    MaxStack = 8
                                    DependencyIds =
                                        objectConstructor.DependencyId
                                        :: (captures
                                            |> List.map (
                                                captureFieldReference
                                                >> _.DependencyId
                                            ))
                                    ContentHash =
                                        Fingerprint.parts [
                                            constructorStableId
                                            objectConstructor.StableId

                                            yield!
                                                captures
                                                |> List.map (fun capture ->
                                                    Fingerprint.parts [
                                                        capture.Name
                                                        TypeIdentity.cliType capture.Type
                                                        capture.Field.Name
                                                    ]
                                                )
                                        ]
                                    DocumentIndex = documentIndex
                                    DocumentChecksum = typed.SourceChecksum
                                    Range = expression.LambdaRange
                                }

                                let invokeParameters: TypedParameter list =
                                    (expression.LambdaParameterNames,
                                     expression.LambdaParameterTypes)
                                    ||> List.map2 (fun name parameterType -> ({
                                        Name = name
                                        Type =
                                            methodTypeParametersToTypeParameters
                                                parameterType
                                        Attributes = []
                                    }: TypedParameter))

                                let invokeBody =
                                    expression.LambdaBody
                                    |> methodExpressionTypesToTypeParameters

                                let invokeDeclaration: TypedMethodDeclaration = {
                                    StableId = invokeStableId
                                    Name = "Invoke"
                                    IsPublic = false
                                    GenericParameters = []
                                    Constraints = []
                                    Attributes = []
                                    Parameters = invokeParameters
                                    ReturnType =
                                        methodTypeParametersToTypeParameters
                                            expression.LambdaReturnType
                                    Body = invokeBody
                                    EmitHiddenEntrySequencePoint = false
                                    ExportFingerprint =
                                        Fingerprint.parts [
                                            invokeStableId
                                            TypeIdentity.inlineBody invokeBody
                                        ]
                                    Range = expression.LambdaRange
                                }

                                let invoke =
                                    methodFragment
                                        ClosureInvoke
                                        documentIndex
                                        typed.SourceChecksum
                                        invokeStableId
                                        (methodImplementationHash invokeDeclaration)
                                        invokeDeclaration

                                {
                                    SchemaVersion = querySchema
                                    StableId = closureStableId
                                    Namespace =
                                        if isNested then String.Empty else typed.Namespace
                                    Name = expression.ClosureName
                                    IsPublic = false
                                    EnclosingTypeStableId =
                                        if isNested then Some moduleTypeStableId else None
                                    Kind = ClosureContainer
                                    GenericParameters = methodDeclaration.GenericParameters
                                    Attributes = []
                                    LiteralFields = []
                                    InstanceFields =
                                        captures
                                        |> List.map (fun capture -> {
                                            SchemaVersion = querySchema
                                            StableId =
                                                capture.Field.TargetStableId
                                                |> Option.defaultWith (fun () ->
                                                    closureStableId
                                                    + "/field:"
                                                    + capture.Name
                                                )
                                            Name = capture.Field.Name
                                            Type = capture.Type
                                            Attributes = []
                                            ContentHash =
                                                Fingerprint.parts [
                                                    capture.Name
                                                    TypeIdentity.cliType capture.Type
                                                ]
                                        })
                                    StaticFields = []
                                    Properties = []
                                    Methods = [
                                        constructor
                                        invoke
                                    ]
                                }
                            )
                        )
                    )
                )

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
                                StaticFields = []
                                Properties = []
                                Methods = []
                            }
                        | TypedMethod _
                        | TypedLiteralField _
                        | TypedNestedModule _
                        | TypedTypeAbbreviation _
                        | TypedStaticType _
                        | TypedObjectType _ -> None
                    )
                )

            let types =
                moduleTypes
                @ nestedModuleTypes
                @ valueTaskBindHelperTypes
                @ valueTaskApplyHelperTypes
                @ valueTaskZipHelperTypes
                @ valueTaskOfUnitHelperTypes
                @ staticTypes
                @ objectTypes
                @ objectExpressionTypes
                @ structTypes
                @ closureTypes
                @ boundMemberClosureTypes
                @ unitLambdaClosureTypes
                @ functionLambdaClosureTypes
                @ delegateLambdaClosureTypes

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

        let rec parseAll documentIndex parsed keys remaining =
            match remaining with
            | [] -> Ok(List.rev parsed, List.rev keys)
            | source :: tail ->
                match parse defines source with
                | Error diagnostic -> Error diagnostic
                | Ok(parsedModules, key) ->
                    let sourceModules =
                        parsedModules
                        |> List.map (fun parsedModule ->
                            source, documentIndex, parsedModule
                        )

                    parseAll
                        (documentIndex + 1)
                        ((parsed, sourceModules)
                         ||> List.fold (fun state parsedModule ->
                             parsedModule
                             :: state
                         ))
                        (key
                         :: keys)
                        tail

        let rec checkAll typed keys remaining =
            match remaining with
            | [] -> Ok(List.rev typed, List.rev keys)
            | (source, documentIndex, parsedModule) :: tail ->
                match check references source.Path documentIndex parsedModule with
                | Error diagnostic -> Error diagnostic
                | Ok(typedModule, key) ->
                    checkAll
                        (typedModule
                         :: typed)
                        (key
                         :: keys)
                        tail

        let parseStarted = Stopwatch.GetTimestamp()

        match parseAll 0 [] [] sources with
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
                        |> List.distinctBy (fun (_, documentIndex, _) -> documentIndex)
                        |> List.map (fun (_, _, parsedModule) ->
                            parsedModule.ContentFingerprint
                        ))
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
