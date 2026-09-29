namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Globalization

[<RequireQualifiedAccess>]
type internal SyntaxProjectionResult =
    | Projected of ParsedModule list
    | ProjectionUnsupported of SourceRange

type internal ReferenceNamespaces = {
    ReferencesFingerprint: string
    Names: ImmutableHashSet<string>
}

/// The Compatibility Oracle accepts an implicit module only in the last file of an executable.
[<RequireQualifiedAccess>]
type internal ImplicitModule =
    | Rejected
    | Accepted of ReferenceNamespaces

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
                not binding.IsMutable
                && binding.Accessibility.IsNone
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

    let private projectDeclarations rootRange (declarations: ImplementationDeclaration list) =
        let rec loop projected (remaining: ImplementationDeclaration list) =
            match remaining with
            | [] -> Ok(List.rev projected, false)
            | declaration :: rest ->
                projectDeclaration declaration
                |> Result.bind (fun (projectedDeclaration, isEntryPoint) ->
                    match isEntryPoint, rest with
                    | true, [] ->
                        Ok(
                            List.rev (
                                projectedDeclaration
                                :: projected
                            ),
                            true
                        )
                    | true, _ -> Error declaration.Range
                    | false, _ ->
                        loop
                            (projectedDeclaration
                             :: projected)
                            rest
                )

        match declarations with
        | [] -> Error rootRange
        | declarations -> loop [] declarations

    let private implicitModuleName (logicalPath: string) =
        let stem = IO.Path.GetFileNameWithoutExtension logicalPath

        if
            stem.Length > 0
            && Char.IsAsciiLetter stem[0]
            && stem
               |> Seq.forall (fun character ->
                   Char.IsAsciiLetterOrDigit character
                   || character = '_'
               )
        then
            Some(
                string (Char.ToUpperInvariant stem[0])
                + stem.Substring 1
            )
        else
            None

    let private projectOpens (namespaces: ReferenceNamespaces) declarations =
        let rec loop opened (remaining: ImplementationDeclaration list) =
            match remaining with
            | ImplementationDeclaration.Open(SyntaxOpenTarget.ModuleOrNamespace name, range) :: rest ->
                if namespaces.Names.Contains name.Text then
                    loop
                        (name.Text
                         :: opened)
                        rest
                else
                    Error range
            | ImplementationDeclaration.Open(_, range) :: _ -> Error range
            | rest -> Ok(List.rev opened, rest)

        loop [] declarations

    let private parsedModule
        contentFingerprint
        sourceChecksum
        (name: string)
        openedNamespaces
        declarations
        =
        {
            StableId =
                "module:"
                + name
            ContainerKind = ModuleSource
            Namespace = String.Empty
            Name = name
            IsPublic = true
            OpenedNamespaces = openedNamespaces
            SourceChecksum = sourceChecksum
            ContentFingerprint = contentFingerprint
            Attributes = []
            AssemblyAttributes = []
            Declarations = declarations
        }

    let project
        (implicitModule: ImplicitModule)
        (contentFingerprint: string)
        (sourceChecksum: ImmutableArray<byte>)
        (file: ImplementationFileSyntax)
        =
        let projected =
            match List.ofSeq file.Contents with
            | [ root ] when not root.DiscardedByRecovery.IsEmpty -> Error root.Range
            | [ root ] ->
                match root.Kind, implicitModule with
                | ModuleOrNamespaceKind.NamedModule name, _ when name.Parts.Length = 1 ->
                    projectDeclarations root.Range (List.ofSeq root.Declarations)
                    |> Result.map (fun (declarations, _) -> [
                        parsedModule contentFingerprint sourceChecksum name.Text [] declarations
                    ])
                | ModuleOrNamespaceKind.AnonymousModule, ImplicitModule.Accepted namespaces ->
                    match implicitModuleName file.LogicalPath with
                    | None -> Error root.Range
                    | Some name ->
                        projectOpens namespaces (List.ofSeq root.Declarations)
                        |> Result.bind (fun (openedNamespaces, rest) ->
                            projectDeclarations root.Range rest
                            |> Result.bind (fun (declarations, endsWithEntryPoint) ->
                                if endsWithEntryPoint then
                                    Ok [
                                        parsedModule
                                            contentFingerprint
                                            sourceChecksum
                                            name
                                            openedNamespaces
                                            declarations
                                    ]
                                else
                                    Error root.Range
                            )
                        )
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
