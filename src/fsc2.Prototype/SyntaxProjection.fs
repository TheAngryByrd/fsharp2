namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Globalization

[<RequireQualifiedAccess>]
type internal SyntaxProjectionResult =
    | Projected of ParsedModule list
    | ProjectionUnsupported of SourceRange

module internal SyntaxProjection =
    let private projectConstant (constant: SyntaxConstant) range =
        match constant with
        | SyntaxConstant.Numeric text when
            text
            |> Seq.forall Char.IsDigit
            ->
            match Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, value -> Ok(IntegerLiteral value)
            | false, _ -> Error range
        | SyntaxConstant.String text when
            text.Length
            >= 2
            && text.StartsWith("\"", StringComparison.Ordinal)
            && text.EndsWith("\"", StringComparison.Ordinal)
            && not (text.Contains('\\'))
            && not (text.StartsWith("\"\"\"", StringComparison.Ordinal))
            ->
            Ok(
                StringLiteral(
                    text.Substring(
                        1,
                        text.Length
                        - 2
                    )
                )
            )
        | SyntaxConstant.Boolean value -> Ok(BooleanLiteral value)
        | SyntaxConstant.Unit -> Ok UnitLiteral
        | _ -> Error range

    let private projectBody (expression: SyntaxExpression) =
        match expression with
        | SyntaxExpression.Constant(constant, range) ->
            projectConstant constant range
            |> Result.map (fun value -> value, range)
        | SyntaxExpression.Identifier name when name.Parts.Length = 1 ->
            Ok(ValueReference name.Parts[0].Text, name.Range)
        | other -> Error other.Range

    let private isEntryPointAttribute (attributes: ImmutableArray<SyntaxAttributeList>) =
        match List.ofSeq attributes with
        | [] -> Ok false
        | [ list ] ->
            match List.ofSeq list.Attributes with
            | [ attribute ] when
                attribute.Target.IsNone
                && attribute.Argument.IsNone
                && attribute.Name.Text = "EntryPoint"
                && attribute.Range.Start.Offset = list.Range.Start.Offset
                                                  + 2
                && attribute.Range.End.Offset
                   + 2 = list.Range.End.Offset
                ->
                Ok true
            | _ -> Error list.Range
        | list :: _ -> Error list.Range

    let private projectKind isEntryPoint (binding: SyntaxBinding) =
        match List.ofSeq binding.Parameters, isEntryPoint with
        | [], false -> Ok(ParsedMethodKind.Regular, false)
        | [ SyntaxPattern.Constant(SyntaxConstant.Unit, _) ], false ->
            Ok(ParsedMethodKind.Regular, true)
        | [ SyntaxPattern.Named parameter ], false ->
            Ok(ParsedMethodKind.RegularFunction parameter.Text, false)
        | [ SyntaxPattern.Named parameter ], true ->
            Ok(ParsedMethodKind.EntryPoint parameter.Text, false)
        | _ -> Error binding.Range

    let private projectBinding (binding: SyntaxBinding) (declarationRange: SourceRange) =
        let name =
            match binding.Head with
            | SyntaxPattern.Named name when
                binding.Accessibility.IsNone
                && binding.Skipped.IsNone
                ->
                Ok name.Text
            | _ -> Error binding.Range

        name
        |> Result.bind (fun name ->
            isEntryPointAttribute binding.Attributes
            |> Result.bind (fun isEntryPoint ->
                projectKind isEntryPoint binding
                |> Result.bind (fun (kind, isUnitFunction) ->
                    projectBody binding.Body
                    |> Result.map (fun (body, bodyRange) ->
                        let body =
                            if isEntryPoint then
                                SequentialValueExpression [ body, bodyRange ]
                            else
                                body

                        ParsedMethod {
                            Name = name
                            IsUnitFunction = isUnitFunction
                            Kind = kind
                            DeclaredType = None
                            Body = body
                            BodyRange = bodyRange
                            Range = {
                                Start = declarationRange.Start
                                End = bodyRange.End
                            }
                        },
                        isEntryPoint
                    )
                )
            )
        )

    let private projectDeclaration declaration =
        match declaration with
        | ImplementationDeclaration.Let(SyntaxLetKeyword.Let, false, bindings, range) when
            bindings.Length = 1
            ->
            projectBinding bindings[0] range
        | other -> Error other.Range

    let private projectDeclarations (root: ModuleOrNamespaceSyntax<ImplementationDeclaration>) =
        let rec loop projected (remaining: ImplementationDeclaration list) =
            match remaining with
            | [] -> Ok(List.rev projected)
            | declaration :: rest ->
                projectDeclaration declaration
                |> Result.bind (fun (projectedDeclaration, isEntryPoint) ->
                    if
                        isEntryPoint
                        && not rest.IsEmpty
                    then
                        Error declaration.Range
                    else
                        loop
                            (projectedDeclaration
                             :: projected)
                            rest
                )

        match List.ofSeq root.Declarations with
        | [] -> Error root.Range
        | declarations -> loop [] declarations

    let project
        (contentFingerprint: string)
        (sourceChecksum: ImmutableArray<byte>)
        (file: ImplementationFileSyntax)
        =
        let projected =
            match List.ofSeq file.Contents with
            | [ root ] ->
                match root.Kind with
                | ModuleOrNamespaceKind.NamedModule name when
                    name.Parts.Length = 1
                    && root.DiscardedByRecovery.IsEmpty
                    ->
                    projectDeclarations root
                    |> Result.map (fun declarations -> [
                        {
                            StableId =
                                "module:"
                                + name.Text
                            ContainerKind = ModuleSource
                            Namespace = String.Empty
                            Name = name.Text
                            IsPublic = true
                            OpenedNamespaces = []
                            SourceChecksum = sourceChecksum
                            ContentFingerprint = contentFingerprint
                            Attributes = []
                            AssemblyAttributes = []
                            Declarations = declarations
                        }
                    ])
                | _ -> Error root.Range
            | root :: _ -> Error root.Range
            | [] ->
                Error {
                    Start = { Offset = 0; Line = 1; Column = 1 }
                    End = { Offset = 0; Line = 1; Column = 1 }
                }

        match projected with
        | Ok modules -> SyntaxProjectionResult.Projected modules
        | Error range -> SyntaxProjectionResult.ProjectionUnsupported range
