namespace FSharp2.Compiler

open System
open System.Globalization
open System.Security.Cryptography
open System.Text

module internal SymbolicEmission =
    let private fingerprint (values: string seq) =
        values
        |> Seq.map (fun value ->
            value.Length.ToString(CultureInfo.InvariantCulture)
            + ":"
            + value
        )
        |> String.concat String.Empty
        |> Encoding.UTF8.GetBytes
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let private contentHash kind stableId loweredHash =
        fingerprint [
            kind
            stableId
            loweredHash
        ]

    let private emitAttribute
        (attribute: LoweredCustomAttribute)
        : SymbolicCustomAttributeFragment =
        {
            SchemaVersion = attribute.SchemaVersion
            StableId = attribute.StableId
            Kind = attribute.Kind
            ConstructorArguments = attribute.ConstructorArguments
            ContentHash = contentHash "attribute" attribute.StableId attribute.SemanticFingerprint
        }

    let private emitParameter (parameter: LoweredParameter) : SymbolicParameterFragment = {
        Name = parameter.Name
        Type = parameter.Type
        Attributes =
            parameter.Attributes
            |> List.map emitAttribute
    }

    let private emitLocal (local: LoweredLocal) : SymbolicLocalFragment = {
        Index = local.Index
        Name = local.Name
        Type = local.Type
    }

    let private emitMethod (methodDeclaration: LoweredMethodDeclaration) : SymbolicMethodFragment =
        let body = methodDeclaration.Body

        {
            SchemaVersion = methodDeclaration.SchemaVersion
            StableId = methodDeclaration.StableId
            Name = methodDeclaration.Name
            Kind = methodDeclaration.Kind
            GenericParameters = methodDeclaration.GenericParameters
            Constraints = methodDeclaration.Constraints
            GenericParameterConstraints = methodDeclaration.GenericParameterConstraints
            Attributes =
                methodDeclaration.Attributes
                |> List.map emitAttribute
            Parameters =
                methodDeclaration.Parameters
                |> List.map emitParameter
            Locals =
                body.Locals
                |> List.map emitLocal
            ReturnType = methodDeclaration.ReturnType
            Instructions = body.ControlFlow
            EmitDefaultSequencePoint = methodDeclaration.EmitDefaultSequencePoint
            MaxStack = body.MaxStack
            DependencyIds = methodDeclaration.DependencyIds
            ContentHash =
                contentHash
                    "method"
                    methodDeclaration.StableId
                    methodDeclaration.SemanticFingerprint
            DocumentIndex = methodDeclaration.DocumentIndex
            DocumentChecksum = methodDeclaration.DocumentChecksum
            Range = methodDeclaration.Range
        }

    let private emitLiteralField
        (field: LoweredLiteralFieldDeclaration)
        : SymbolicLiteralFieldFragment =
        {
            SchemaVersion = field.SchemaVersion
            StableId = field.StableId
            Name = field.Name
            Value = field.Value
            ContentHash = contentHash "literal-field" field.StableId field.SemanticFingerprint
        }

    let private emitInstanceField
        (field: LoweredInstanceFieldDeclaration)
        : SymbolicInstanceFieldFragment =
        {
            SchemaVersion = field.SchemaVersion
            StableId = field.StableId
            Name = field.Name
            Type = field.Type
            Attributes =
                field.Attributes
                |> List.map emitAttribute
            ContentHash = contentHash "instance-field" field.StableId field.SemanticFingerprint
        }

    let private emitStaticField
        (field: LoweredStaticFieldDeclaration)
        : SymbolicStaticFieldFragment =
        {
            SchemaVersion = field.SchemaVersion
            StableId = field.StableId
            Name = field.Name
            Type = field.Type
            ContentHash = contentHash "static-field" field.StableId field.SemanticFingerprint
        }

    let private emitProperty (property: LoweredPropertyDeclaration) : SymbolicPropertyFragment = {
        SchemaVersion = property.SchemaVersion
        StableId = property.StableId
        Name = property.Name
        Type = property.Type
        GetterStableId = property.GetterStableId
        ContentHash = contentHash "property" property.StableId property.SemanticFingerprint
    }

    let private emitType (typeDeclaration: LoweredTypeDeclaration) : SymbolicTypeFragment = {
        SchemaVersion = typeDeclaration.SchemaVersion
        StableId = typeDeclaration.StableId
        Namespace = typeDeclaration.Namespace
        Name = typeDeclaration.Name
        IsPublic = typeDeclaration.IsPublic
        EnclosingTypeStableId = typeDeclaration.EnclosingTypeStableId
        Kind = typeDeclaration.Kind
        GenericParameters = typeDeclaration.GenericParameters
        Attributes =
            typeDeclaration.Attributes
            |> List.map emitAttribute
        LiteralFields =
            typeDeclaration.LiteralFields
            |> List.map emitLiteralField
        InstanceFields =
            typeDeclaration.InstanceFields
            |> List.map emitInstanceField
        StaticFields =
            typeDeclaration.StaticFields
            |> List.map emitStaticField
        Properties =
            typeDeclaration.Properties
            |> List.map emitProperty
        Methods =
            typeDeclaration.Methods
            |> List.map emitMethod
    }

    let private emitDocument (document: LoweredDocument) : SymbolicDocumentFragment = {
        SchemaVersion = document.SchemaVersion
        StableId = document.StableId
        Checksum = document.Checksum
    }

    let private emitTypeAbbreviation
        (typeAbbreviation: LoweredTypeAbbreviationDeclaration)
        : SymbolicTypeAbbreviationFragment =
        {
            SchemaVersion = typeAbbreviation.SchemaVersion
            StableId = typeAbbreviation.StableId
            Name = typeAbbreviation.Name
            TypeParameters = typeAbbreviation.TypeParameters
            Constraints = typeAbbreviation.Constraints
            TargetType = typeAbbreviation.TargetType
            AllowsNull = typeAbbreviation.AllowsNull
            ContentHash =
                contentHash
                    "type-abbreviation"
                    typeAbbreviation.StableId
                    typeAbbreviation.SemanticFingerprint
        }

    let private emitNamedArgument
        (argument: LoweredNamedStringArgument)
        : SymbolicNamedStringArgument =
        {
            Name = argument.Name
            Value = argument.Value
        }

    let private emitAssemblyAttribute
        (attribute: LoweredAssemblyAttributeDeclaration)
        : SymbolicAssemblyAttributeFragment =
        {
            SchemaVersion = attribute.SchemaVersion
            StableId = attribute.StableId
            Kind = attribute.Kind
            AttributeType = attribute.AttributeType
            ConstructorArguments = attribute.ConstructorArguments
            NamedArguments =
                attribute.NamedArguments
                |> List.map emitNamedArgument
            ContentHash =
                contentHash "assembly-attribute" attribute.StableId attribute.SemanticFingerprint
        }

    let emit (lowered: LoweredCompilation) : SymbolicAssembly = {
        SchemaVersion = lowered.SchemaVersion
        StableId = lowered.StableId
        AssemblyName = lowered.AssemblyName
        AssemblyVersion = lowered.AssemblyVersion
        PublicFingerprint = lowered.PublicFingerprint
        Documents =
            lowered.Documents
            |> List.map emitDocument
        AssemblyAttributes =
            lowered.AssemblyAttributes
            |> List.map emitAssemblyAttribute
        Module = {
            SchemaVersion = lowered.Module.SchemaVersion
            StableId = lowered.Module.StableId
            Name = lowered.Module.Name
            TypeAbbreviations =
                lowered.Module.TypeAbbreviations
                |> List.map emitTypeAbbreviation
            Types =
                lowered.Module.Types
                |> List.map emitType
        }
    }
