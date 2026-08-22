namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Security.Cryptography
open System.Text
open System.Threading

module internal Ecma335ReferenceImporter =
    exception private DecodeFailure of ReferenceImportErrorKind * string * int option * string
    exception private DemandFailure of ReferenceImportError

    type private ReferenceAssemblyScope = {
        Name: string
        Version: ReferenceVersion
        Culture: string option
        Flags: int
        PublicKeyToken: byte list
    }

    type private EnumDefinitionKey = {
        Assembly: ReferenceAssemblyScope
        Namespace: string
        EnclosingTypes: string list
        MetadataName: string
        GenericArity: int
    }

    type private EnumDefinitionDemand = ReferenceSemanticDemand<PrimitiveTypeCode option>

    type private DecodeContext = {
        Limits: ReferenceImportLimits
        Snapshot: TargetReferenceSnapshot
        Metadata: MetadataReader
        ModuleName: string
        EnumDefinitions: Dictionary<EnumDefinitionKey, EnumDefinitionDemand>
        CancellationToken: CancellationToken
        mutable RowsVisited: int
    }

    type private BudgetedCliType = {
        Value: ReferenceCliType
        Depth: int
        Nodes: int
    }

    let private failure
        (snapshot: TargetReferenceSnapshot)
        (kind: ReferenceImportErrorKind)
        (location: string option)
        (offset: int option)
        (related: string option)
        (message: string)
        : ReferenceImportError =
        {
            Kind = kind
            ReferenceStableId = snapshot.StableId.Value
            LogicalPath = snapshot.LogicalPath
            MetadataLocation = location
            DecoderOffset = offset
            RelatedIdentity = related
            Message =
                if
                    message.Length
                    <= 512
                then
                    message
                else
                    message.Substring(0, 512)
        }

    let private raiseDecode
        (kind: ReferenceImportErrorKind)
        (location: string)
        (offset: int option)
        (message: string)
        =
        raise (DecodeFailure(kind, location, offset, message))

    let private checkedAdd (left: int) (right: int) =
        let value =
            int64 left
            + int64 right

        if
            value > int64 Int32.MaxValue
            || value < int64 Int32.MinValue
        then
            raise (OverflowException())

        int value

    let private checkedUInt32ToInt (value: uint32) =
        if value > uint32 Int32.MaxValue then
            raise (OverflowException())

        int value

    let private checkedInt64ToInt (value: int64) =
        if
            value > int64 Int32.MaxValue
            || value < int64 Int32.MinValue
        then
            raise (OverflowException())

        int value

    let private visitRow (context: DecodeContext) =
        context.RowsVisited <- checkedAdd context.RowsVisited 1

        if context.RowsVisited % 4_096 = 0 then
            context.CancellationToken.ThrowIfCancellationRequested()

    let private checkHeapLengthLimit
        (limits: ReferenceImportLimits)
        (location: string)
        (length: int)
        =
        if length > limits.MaximumHeapValueBytes then
            raiseDecode
                ReferenceImportErrorKind.HeapValueTooLarge
                location
                None
                "A metadata heap value exceeds the configured limit."

    let private checkHeapLength (context: DecodeContext) (location: string) (length: int) =
        checkHeapLengthLimit context.Limits location length

    let private getStringValue
        (limits: ReferenceImportLimits)
        (metadata: MetadataReader)
        (location: string)
        (handle: StringHandle)
        =
        try
            let reader = metadata.GetBlobReader(handle)
            checkHeapLengthLimit limits location reader.Length
            let value = metadata.GetString(handle)
            checkHeapLengthLimit limits location (Encoding.UTF8.GetByteCount(value))
            value
        with :? BadImageFormatException as exceptionValue ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                location
                None
                exceptionValue.Message

    let private getString (context: DecodeContext) (location: string) (handle: StringHandle) =
        getStringValue context.Limits context.Metadata location handle

    let private optionalString (context: DecodeContext) (location: string) (handle: StringHandle) =
        if handle.IsNil then
            None
        else
            getString context location handle
            |> Some

    let private getBlobArray (context: DecodeContext) (location: string) (handle: BlobHandle) =
        if handle.IsNil then
            Array.empty
        else
            let mutable reader = context.Metadata.GetBlobReader(handle)
            checkHeapLength context location reader.Length
            reader.ReadBytes(reader.Length)

    let private getBlob (context: DecodeContext) (location: string) (handle: BlobHandle) =
        getBlobArray context location handle
        |> List.ofArray

    let private optionalGuid (metadata: MetadataReader) (handle: GuidHandle) =
        if handle.IsNil then
            None
        else
            metadata.GetGuid(handle)
            |> Some

    let private version (value: Version) = {
        Major = value.Major
        Minor = value.Minor
        Build = value.Build
        Revision = value.Revision
    }

    let private publicKeyToken (publicKey: byte list) =
        match publicKey with
        | [] -> []
        | values ->
            values
            |> List.toArray
            |> SHA1.HashData
            |> Array.rev
            |> Array.take 8
            |> List.ofArray

    let private assemblyIdentity (context: DecodeContext) =
        let definition = context.Metadata.GetAssemblyDefinition()
        let publicKey = getBlob context "Assembly.PublicKey" definition.PublicKey

        {
            Name = getString context "Assembly.Name" definition.Name
            Version = version definition.Version
            Culture = optionalString context "Assembly.Culture" definition.Culture
            Flags = int definition.Flags
            HashAlgorithm = uint32 definition.HashAlgorithm
            PublicKey = publicKey
            PublicKeyToken = publicKeyToken publicKey
        }

    let private assemblyReference (context: DecodeContext) (handle: AssemblyReferenceHandle) =
        visitRow context
        let reference = context.Metadata.GetAssemblyReference(handle)

        let keyOrToken =
            getBlob context "AssemblyRef.PublicKeyOrToken" reference.PublicKeyOrToken

        let hasPublicKey =
            (reference.Flags
             &&& AssemblyFlags.PublicKey) = AssemblyFlags.PublicKey

        {
            Identity = {
                Name = getString context "AssemblyRef.Name" reference.Name
                Version = version reference.Version
                Culture = optionalString context "AssemblyRef.Culture" reference.Culture
                Flags = int reference.Flags
                HashAlgorithm = 0u
                PublicKey = if hasPublicKey then keyOrToken else []
                PublicKeyToken =
                    if hasPublicKey then
                        publicKeyToken keyOrToken
                    else
                        keyOrToken
            }
            HashValue = getBlob context "AssemblyRef.HashValue" reference.HashValue
        }

    let private assemblyScope (identity: ReferenceAssemblyIdentity) = {
        Name = identity.Name
        Version = identity.Version
        Culture = identity.Culture
        Flags =
            identity.Flags
            &&& 0x0000FF00
        PublicKeyToken = identity.PublicKeyToken
    }

    let rec private typeAssemblyScope (identity: ReferenceTypeIdentity) =
        match identity.ResolutionScope with
        | Some(ReferenceResolutionScope.Assembly assembly) ->
            assembly
            |> assemblyScope
            |> Some
        | Some(ReferenceResolutionScope.Type parent) -> typeAssemblyScope parent
        | _ -> None

    let private enumDefinitionKey
        (assembly: ReferenceAssemblyScope)
        (identity: ReferenceTypeIdentity)
        =
        {
            Assembly = assembly
            Namespace = identity.Namespace
            EnclosingTypes = identity.EnclosingTypes
            MetadataName = identity.MetadataName
            GenericArity = identity.GenericArity
        }

    let private metadataArity (name: string) =
        let marker = name.LastIndexOf('`')

        if marker > 0 then
            match Int32.TryParse(name.Substring(marker + 1)) with
            | true, arity when arity >= 0 -> arity
            | _ -> 0
        else
            0

    let private typeDefinitionIdentity (context: DecodeContext) (handle: TypeDefinitionHandle) =
        let visited = HashSet<int>()

        let rec loop depth (current: TypeDefinitionHandle) =
            if depth > context.Limits.MaximumResolutionDepth then
                raiseDecode
                    ReferenceImportErrorKind.ResolutionDepthExceeded
                    "TypeDef.Nesting"
                    None
                    "The declaring-type path exceeds the configured depth limit."

            let row = MetadataTokens.GetRowNumber(current)

            if not (visited.Add(row)) then
                raiseDecode
                    ReferenceImportErrorKind.MalformedCliMetadata
                    $"TypeDef:{row}"
                    None
                    "The declaring-type path contains a cycle."

            visitRow context
            let definition = context.Metadata.GetTypeDefinition(current)
            let name = getString context $"TypeDef:{row}.Name" definition.Name

            if definition.IsNested then
                let parent = loop (depth + 1) (definition.GetDeclaringType())

                {
                    ModuleName = context.ModuleName
                    Namespace = parent.Namespace
                    EnclosingTypes =
                        parent.EnclosingTypes
                        @ [ parent.MetadataName ]
                    MetadataName = name
                    GenericArity =
                        definition.GetGenericParameters()
                        |> Seq.length
                    ResolutionScope = None
                }
            else
                {
                    ModuleName = context.ModuleName
                    Namespace = getString context $"TypeDef:{row}.Namespace" definition.Namespace
                    EnclosingTypes = []
                    MetadataName = name
                    GenericArity =
                        definition.GetGenericParameters()
                        |> Seq.length
                    ResolutionScope = None
                }

        loop 0 handle

    let private typeReferenceIdentity (context: DecodeContext) (handle: TypeReferenceHandle) =
        let visited = HashSet<int>()

        let rec loop depth (current: TypeReferenceHandle) =
            if depth > context.Limits.MaximumResolutionDepth then
                raiseDecode
                    ReferenceImportErrorKind.ResolutionDepthExceeded
                    "TypeRef.Scope"
                    None
                    "The type-reference scope exceeds the configured depth limit."

            let row = MetadataTokens.GetRowNumber(current)

            if not (visited.Add(row)) then
                raiseDecode
                    ReferenceImportErrorKind.MalformedCliMetadata
                    $"TypeRef:{row}"
                    None
                    "The type-reference scope contains a cycle."

            visitRow context
            let reference = context.Metadata.GetTypeReference(current)
            let name = getString context $"TypeRef:{row}.Name" reference.Name

            let scope, namespaceName, enclosingTypes =
                match reference.ResolutionScope.Kind with
                | HandleKind.ModuleDefinition ->
                    ReferenceResolutionScope.CurrentModule,
                    getString context $"TypeRef:{row}.Namespace" reference.Namespace,
                    []
                | HandleKind.ModuleReference ->
                    let moduleReference =
                        reference.ResolutionScope
                        |> ModuleReferenceHandle.op_Explicit
                        |> context.Metadata.GetModuleReference

                    ReferenceResolutionScope.Module(
                        getString context $"TypeRef:{row}.Module" moduleReference.Name
                    ),
                    getString context $"TypeRef:{row}.Namespace" reference.Namespace,
                    []
                | HandleKind.AssemblyReference ->
                    let identity =
                        try
                            reference.ResolutionScope
                            |> AssemblyReferenceHandle.op_Explicit
                            |> assemblyReference context
                            |> _.Identity
                        with :? BadImageFormatException as exceptionValue ->
                            raiseDecode
                                ReferenceImportErrorKind.MalformedCliMetadata
                                $"TypeRef:{row}.ResolutionScope"
                                None
                                exceptionValue.Message

                    ReferenceResolutionScope.Assembly identity,
                    getString context $"TypeRef:{row}.Namespace" reference.Namespace,
                    []
                | HandleKind.TypeReference ->
                    let parent =
                        reference.ResolutionScope
                        |> TypeReferenceHandle.op_Explicit
                        |> loop (depth + 1)

                    ReferenceResolutionScope.Type parent,
                    parent.Namespace,
                    parent.EnclosingTypes
                    @ [ parent.MetadataName ]
                | kind ->
                    raiseDecode
                        ReferenceImportErrorKind.MalformedCliMetadata
                        $"TypeRef:{row}.ResolutionScope"
                        None
                        $"The type-reference scope kind '{kind}' is invalid."

            {
                ModuleName = context.ModuleName
                Namespace = namespaceName
                EnclosingTypes = enclosingTypes
                MetadataName = name
                GenericArity = metadataArity name
                ResolutionScope = Some scope
            }

        loop 0 handle

    let private typeAccessibility (attributes: TypeAttributes) =
        match
            attributes
            &&& TypeAttributes.VisibilityMask
        with
        | TypeAttributes.Public -> ReferenceTypeAccessibility.Public
        | TypeAttributes.NotPublic -> ReferenceTypeAccessibility.NotPublic
        | TypeAttributes.NestedPublic -> ReferenceTypeAccessibility.NestedPublic
        | TypeAttributes.NestedPrivate -> ReferenceTypeAccessibility.NestedPrivate
        | TypeAttributes.NestedFamily -> ReferenceTypeAccessibility.NestedFamily
        | TypeAttributes.NestedAssembly -> ReferenceTypeAccessibility.NestedAssembly
        | TypeAttributes.NestedFamANDAssem -> ReferenceTypeAccessibility.NestedFamilyAndAssembly
        | TypeAttributes.NestedFamORAssem -> ReferenceTypeAccessibility.NestedFamilyOrAssembly
        | value ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                "TypeDef.Attributes"
                None
                $"The type visibility '{value}' is invalid."

    let private methodAccessibility (attributes: MethodAttributes) =
        match
            attributes
            &&& MethodAttributes.MemberAccessMask
        with
        | MethodAttributes.PrivateScope -> ReferenceMemberAccessibility.PrivateScope
        | MethodAttributes.Private -> ReferenceMemberAccessibility.Private
        | MethodAttributes.FamANDAssem -> ReferenceMemberAccessibility.FamilyAndAssembly
        | MethodAttributes.Assembly -> ReferenceMemberAccessibility.Assembly
        | MethodAttributes.Family -> ReferenceMemberAccessibility.Family
        | MethodAttributes.FamORAssem -> ReferenceMemberAccessibility.FamilyOrAssembly
        | MethodAttributes.Public -> ReferenceMemberAccessibility.Public
        | value ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                "MethodDef.Attributes"
                None
                $"The method accessibility '{value}' is invalid."

    let private fieldAccessibility (attributes: FieldAttributes) =
        match
            attributes
            &&& FieldAttributes.FieldAccessMask
        with
        | FieldAttributes.PrivateScope -> ReferenceMemberAccessibility.PrivateScope
        | FieldAttributes.Private -> ReferenceMemberAccessibility.Private
        | FieldAttributes.FamANDAssem -> ReferenceMemberAccessibility.FamilyAndAssembly
        | FieldAttributes.Assembly -> ReferenceMemberAccessibility.Assembly
        | FieldAttributes.Family -> ReferenceMemberAccessibility.Family
        | FieldAttributes.FamORAssem -> ReferenceMemberAccessibility.FamilyOrAssembly
        | FieldAttributes.Public -> ReferenceMemberAccessibility.Public
        | value ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                "Field.Attributes"
                None
                $"The field accessibility '{value}' is invalid."

    let private primitiveType =
        function
        | PrimitiveTypeCode.Boolean -> ReferencePrimitiveType.Boolean
        | PrimitiveTypeCode.Byte -> ReferencePrimitiveType.Byte
        | PrimitiveTypeCode.SByte -> ReferencePrimitiveType.SByte
        | PrimitiveTypeCode.Char -> ReferencePrimitiveType.Char
        | PrimitiveTypeCode.Int16 -> ReferencePrimitiveType.Int16
        | PrimitiveTypeCode.UInt16 -> ReferencePrimitiveType.UInt16
        | PrimitiveTypeCode.Int32 -> ReferencePrimitiveType.Int32
        | PrimitiveTypeCode.UInt32 -> ReferencePrimitiveType.UInt32
        | PrimitiveTypeCode.Int64 -> ReferencePrimitiveType.Int64
        | PrimitiveTypeCode.UInt64 -> ReferencePrimitiveType.UInt64
        | PrimitiveTypeCode.IntPtr -> ReferencePrimitiveType.IntPtr
        | PrimitiveTypeCode.UIntPtr -> ReferencePrimitiveType.UIntPtr
        | PrimitiveTypeCode.Single -> ReferencePrimitiveType.Single
        | PrimitiveTypeCode.Double -> ReferencePrimitiveType.Double
        | PrimitiveTypeCode.String -> ReferencePrimitiveType.String
        | PrimitiveTypeCode.Object -> ReferencePrimitiveType.Object
        | PrimitiveTypeCode.TypedReference -> ReferencePrimitiveType.TypedReference
        | PrimitiveTypeCode.Void -> ReferencePrimitiveType.Void
        | value ->
            raiseDecode
                ReferenceImportErrorKind.InvalidSignature
                "Signature.PrimitiveType"
                None
                $"The primitive type code '{value}' is invalid."

    let private callingConvention (header: SignatureHeader) =
        match header.CallingConvention with
        | SignatureCallingConvention.Default -> ReferenceSignatureCallingConvention.Default
        | SignatureCallingConvention.CDecl -> ReferenceSignatureCallingConvention.CDecl
        | SignatureCallingConvention.StdCall -> ReferenceSignatureCallingConvention.StdCall
        | SignatureCallingConvention.ThisCall -> ReferenceSignatureCallingConvention.ThisCall
        | SignatureCallingConvention.FastCall -> ReferenceSignatureCallingConvention.FastCall
        | SignatureCallingConvention.VarArgs -> ReferenceSignatureCallingConvention.VarArgs
        | SignatureCallingConvention.Unmanaged -> ReferenceSignatureCallingConvention.Unmanaged
        | value -> ReferenceSignatureCallingConvention.Unknown(byte value)

    let private budgeted
        (context: DecodeContext)
        (value: ReferenceCliType)
        (children: BudgetedCliType list)
        =
        let nodes =
            children
            |> List.fold (fun total child -> checkedAdd total child.Nodes) 1

        let depth =
            1
            + (children
               |> List.map _.Depth
               |> List.fold max 0)

        if depth > context.Limits.MaximumSignatureDepth then
            raiseDecode
                ReferenceImportErrorKind.SignatureDepthExceeded
                "Signature"
                None
                "A decoded signature exceeds the configured depth limit."

        if nodes > context.Limits.MaximumSignatureNodes then
            raiseDecode
                ReferenceImportErrorKind.SignatureNodeLimitExceeded
                "Signature"
                None
                "A decoded signature exceeds the configured node limit."

        {
            Value = value
            Depth = depth
            Nodes = nodes
        }

    let private methodSignature
        (context: DecodeContext)
        (signature: MethodSignature<BudgetedCliType>)
        =
        let children =
            signature.ReturnType
            :: (signature.ParameterTypes
                |> List.ofSeq)

        let nodes =
            children
            |> List.fold (fun total child -> checkedAdd total child.Nodes) 1

        let depth =
            1
            + (children
               |> List.map _.Depth
               |> List.fold max 0)

        if depth > context.Limits.MaximumSignatureDepth then
            raiseDecode
                ReferenceImportErrorKind.SignatureDepthExceeded
                "MethodSignature"
                None
                "A decoded method signature exceeds the configured depth limit."

        if nodes > context.Limits.MaximumSignatureNodes then
            raiseDecode
                ReferenceImportErrorKind.SignatureNodeLimitExceeded
                "MethodSignature"
                None
                "A decoded method signature exceeds the configured node limit."

        {
            CallingConvention = callingConvention signature.Header
            IsInstance = signature.Header.IsInstance
            IsExplicitThis = signature.Header.HasExplicitThis
            GenericParameterCount = signature.GenericParameterCount
            RequiredParameterCount = signature.RequiredParameterCount
            ReturnType = signature.ReturnType.Value
            ParameterTypes =
                signature.ParameterTypes
                |> Seq.map _.Value
                |> List.ofSeq
        },
        depth,
        nodes

    type private SignatureProvider(context: DecodeContext) as this =
        let typeSpecifications = HashSet<int>()

        interface ISignatureTypeProvider<BudgetedCliType, unit> with
            member _.GetArrayType(elementType, shape) =
                budgeted
                    context
                    (ReferenceCliType.Array(
                        elementType.Value,
                        {
                            Rank = shape.Rank
                            Sizes =
                                shape.Sizes
                                |> List.ofSeq
                            LowerBounds =
                                shape.LowerBounds
                                |> List.ofSeq
                        }
                    ))
                    [ elementType ]

            member _.GetByReferenceType(elementType) =
                budgeted context (ReferenceCliType.ByReference elementType.Value) [ elementType ]

            member _.GetFunctionPointerType(signature) =
                let decoded, depth, nodes = methodSignature context signature

                let child = {
                    Value = ReferenceCliType.FunctionPointer decoded
                    Depth = depth
                    Nodes = nodes
                }

                budgeted context (ReferenceCliType.FunctionPointer decoded) [ child ]

            member _.GetGenericInstantiation(genericType, typeArguments) =
                let arguments =
                    typeArguments
                    |> List.ofSeq

                budgeted
                    context
                    (ReferenceCliType.GenericInstantiation(
                        genericType.Value,
                        arguments
                        |> List.map _.Value
                    ))
                    (genericType
                     :: arguments)

            member _.GetGenericMethodParameter(_, index) =
                budgeted context (ReferenceCliType.GenericMethodParameter index) []

            member _.GetGenericTypeParameter(_, index) =
                budgeted context (ReferenceCliType.GenericTypeParameter index) []

            member _.GetModifiedType(modifier, unmodifiedType, isRequired) =
                budgeted
                    context
                    (ReferenceCliType.Modified(modifier.Value, unmodifiedType.Value, isRequired))
                    [
                        modifier
                        unmodifiedType
                    ]

            member _.GetPinnedType(elementType) =
                budgeted context (ReferenceCliType.Pinned elementType.Value) [ elementType ]

            member _.GetPointerType(elementType) =
                budgeted context (ReferenceCliType.Pointer elementType.Value) [ elementType ]

            member _.GetPrimitiveType(code) =
                budgeted context (ReferenceCliType.Primitive(primitiveType code)) []

            member _.GetSZArrayType(elementType) =
                budgeted context (ReferenceCliType.SzArray elementType.Value) [ elementType ]

            member _.GetTypeFromDefinition(_, handle, rawTypeKind) =
                budgeted
                    context
                    (ReferenceCliType.Definition(
                        typeDefinitionIdentity context handle,
                        rawTypeKind = byte SignatureTypeKind.ValueType
                    ))
                    []

            member _.GetTypeFromReference(_, handle, rawTypeKind) =
                budgeted
                    context
                    (ReferenceCliType.Reference(
                        typeReferenceIdentity context handle,
                        rawTypeKind = byte SignatureTypeKind.ValueType
                    ))
                    []

            member _.GetTypeFromSpecification(_, genericContext, handle, _) =
                let row = MetadataTokens.GetRowNumber(handle)

                if not (typeSpecifications.Add(row)) then
                    raiseDecode
                        ReferenceImportErrorKind.InvalidSignature
                        $"TypeSpec:{row}"
                        None
                        "The type-specification signature contains a cycle."

                try
                    visitRow context

                    let decoded =
                        context.Metadata
                            .GetTypeSpecification(handle)
                            .DecodeSignature(this, genericContext)

                    budgeted context (ReferenceCliType.Specification decoded.Value) [ decoded ]
                finally
                    typeSpecifications.Remove(row)
                    |> ignore

    let private signatureProvider (context: DecodeContext) =
        SignatureProvider(context) :> ISignatureTypeProvider<BudgetedCliType, unit>

    let private blobOffset (handle: BlobHandle) =
        if handle.IsNil then
            None
        else
            MetadataTokens.GetHeapOffset(handle)
            |> Some

    let private decodeMethodSignature
        (context: DecodeContext)
        (location: string)
        (handle: BlobHandle)
        decode
        =
        let offset = blobOffset handle

        try
            let decoded = decode (signatureProvider context)

            methodSignature context decoded
            |> fun (value, _, _) -> value
        with
        | DecodeFailure _ -> reraise ()
        | :? BadImageFormatException as exceptionValue ->
            raiseDecode
                ReferenceImportErrorKind.InvalidSignature
                location
                offset
                exceptionValue.Message

    let private decodeCliType
        (context: DecodeContext)
        (location: string)
        (handle: BlobHandle)
        decode
        =
        let offset = blobOffset handle

        try
            decode (signatureProvider context)
            |> _.Value
        with
        | DecodeFailure _ -> reraise ()
        | :? BadImageFormatException as exceptionValue ->
            raiseDecode
                ReferenceImportErrorKind.InvalidSignature
                location
                offset
                exceptionValue.Message

    let private entityType (context: DecodeContext) (location: string) (handle: EntityHandle) =
        match handle.Kind with
        | HandleKind.TypeDefinition ->
            ReferenceCliType.Definition(
                typeDefinitionIdentity context (TypeDefinitionHandle.op_Explicit handle),
                false
            )
        | HandleKind.TypeReference ->
            ReferenceCliType.Reference(
                typeReferenceIdentity context (TypeReferenceHandle.op_Explicit handle),
                false
            )
        | HandleKind.TypeSpecification ->
            let specification =
                context.Metadata.GetTypeSpecification(TypeSpecificationHandle.op_Explicit handle)

            decodeCliType
                context
                location
                specification.Signature
                (fun provider -> specification.DecodeSignature(provider, ()))
        | kind ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                location
                None
                $"The type handle kind '{kind}' is invalid."

    let private methodDefinitionIdentity (context: DecodeContext) (handle: MethodDefinitionHandle) =
        visitRow context
        let definition = context.Metadata.GetMethodDefinition(handle)
        let row = MetadataTokens.GetRowNumber(handle)

        let signature =
            decodeMethodSignature
                context
                $"MethodDef:{row}.Signature"
                definition.Signature
                (fun provider -> definition.DecodeSignature(provider, ()))

        {
            DeclaringType = Some(typeDefinitionIdentity context (definition.GetDeclaringType()))
            Parent = None
            Kind = ReferenceMemberKind.Method
            MetadataName = getString context $"MethodDef:{row}.Name" definition.Name
            GenericArity = signature.GenericParameterCount
            MethodSignature = Some signature
            FieldType = None
            PropertySignature = None
            EventType = None
        }

    let private fieldDefinitionIdentity (context: DecodeContext) (handle: FieldDefinitionHandle) =
        visitRow context
        let definition = context.Metadata.GetFieldDefinition(handle)
        let row = MetadataTokens.GetRowNumber(handle)

        let fieldType =
            decodeCliType
                context
                $"Field:{row}.Signature"
                definition.Signature
                (fun provider -> definition.DecodeSignature(provider, ()))

        {
            DeclaringType = Some(typeDefinitionIdentity context (definition.GetDeclaringType()))
            Parent = None
            Kind = ReferenceMemberKind.Field
            MetadataName = getString context $"Field:{row}.Name" definition.Name
            GenericArity = 0
            MethodSignature = None
            FieldType = Some fieldType
            PropertySignature = None
            EventType = None
        }

    let private memberParent (context: DecodeContext) (handle: EntityHandle) =
        match handle.Kind with
        | HandleKind.TypeDefinition ->
            handle
            |> TypeDefinitionHandle.op_Explicit
            |> typeDefinitionIdentity context
            |> ReferenceMemberParent.Type
        | HandleKind.TypeReference ->
            handle
            |> TypeReferenceHandle.op_Explicit
            |> typeReferenceIdentity context
            |> ReferenceMemberParent.Type
        | HandleKind.ModuleReference ->
            let moduleReference =
                handle
                |> ModuleReferenceHandle.op_Explicit
                |> context.Metadata.GetModuleReference

            moduleReference.Name
            |> getString context "MemberRef.Parent.Module"
            |> ReferenceMemberParent.Module
        | HandleKind.MethodDefinition ->
            handle
            |> MethodDefinitionHandle.op_Explicit
            |> methodDefinitionIdentity context
            |> ReferenceMemberParent.Method
        | HandleKind.TypeSpecification ->
            handle
            |> entityType context "MemberRef.Parent.TypeSpec"
            |> ReferenceMemberParent.TypeSpecification
        | kind ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                "MemberRef.Parent"
                None
                $"The member-reference parent kind '{kind}' is invalid."

    let private memberReferenceIdentity (context: DecodeContext) (handle: MemberReferenceHandle) =
        visitRow context
        let reference = context.Metadata.GetMemberReference(handle)
        let row = MetadataTokens.GetRowNumber(handle)
        let parent = memberParent context reference.Parent

        let declaringType =
            match parent with
            | ReferenceMemberParent.Type identity -> Some identity
            | _ -> None

        match reference.GetKind() with
        | MemberReferenceKind.Method ->
            let signature =
                decodeMethodSignature
                    context
                    $"MemberRef:{row}.Signature"
                    reference.Signature
                    (fun provider -> reference.DecodeMethodSignature(provider, ()))

            {
                DeclaringType = declaringType
                Parent = Some parent
                Kind = ReferenceMemberKind.Method
                MetadataName = getString context $"MemberRef:{row}.Name" reference.Name
                GenericArity = signature.GenericParameterCount
                MethodSignature = Some signature
                FieldType = None
                PropertySignature = None
                EventType = None
            }
        | MemberReferenceKind.Field ->
            let fieldType =
                decodeCliType
                    context
                    $"MemberRef:{row}.Signature"
                    reference.Signature
                    (fun provider -> reference.DecodeFieldSignature(provider, ()))

            {
                DeclaringType = declaringType
                Parent = Some parent
                Kind = ReferenceMemberKind.Field
                MetadataName = getString context $"MemberRef:{row}.Name" reference.Name
                GenericArity = 0
                MethodSignature = None
                FieldType = Some fieldType
                PropertySignature = None
                EventType = None
            }
        | kind ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                $"MemberRef:{row}"
                None
                $"The member-reference kind '{kind}' is invalid."

    let private serializedTypeIdentity (serializedName: string) =
        let typeName =
            match serializedName.IndexOf(',') with
            | -1 -> serializedName
            | separator -> serializedName.Substring(0, separator)

        let parts =
            typeName.Split('+')
            |> List.ofArray

        let outer = parts.Head
        let namespaceSeparator = outer.LastIndexOf('.')

        let namespaceName, outerName =
            if namespaceSeparator < 0 then
                "", outer
            else
                outer.Substring(0, namespaceSeparator),
                outer.Substring(
                    namespaceSeparator
                    + 1
                )

        let enclosingTypes, metadataName =
            match parts.Tail with
            | [] -> [], outerName
            | nested ->
                outerName
                :: (nested
                    |> List.take (
                        nested.Length
                        - 1
                    )),
                nested
                |> List.last

        {
            ModuleName = ""
            Namespace = namespaceName
            EnclosingTypes = enclosingTypes
            MetadataName = metadataName
            GenericArity = metadataArity metadataName
            ResolutionScope = None
        }

    let private systemType =
        ReferenceCliType.Reference(
            {
                ModuleName = ""
                Namespace = "System"
                EnclosingTypes = []
                MetadataName = "Type"
                GenericArity = 0
                ResolutionScope = None
            },
            false
        )

    let private isSystemType =
        function
        | ReferenceCliType.Reference(identity, false)
        | ReferenceCliType.Definition(identity, false) ->
            identity.Namespace = "System"
            && identity.EnclosingTypes.IsEmpty
            && identity.MetadataName = "Type"
        | _ -> false

    let private primitiveTypeCode =
        function
        | ReferencePrimitiveType.Boolean -> PrimitiveTypeCode.Boolean
        | ReferencePrimitiveType.Byte -> PrimitiveTypeCode.Byte
        | ReferencePrimitiveType.SByte -> PrimitiveTypeCode.SByte
        | ReferencePrimitiveType.Char -> PrimitiveTypeCode.Char
        | ReferencePrimitiveType.Int16 -> PrimitiveTypeCode.Int16
        | ReferencePrimitiveType.UInt16 -> PrimitiveTypeCode.UInt16
        | ReferencePrimitiveType.Int32 -> PrimitiveTypeCode.Int32
        | ReferencePrimitiveType.UInt32 -> PrimitiveTypeCode.UInt32
        | ReferencePrimitiveType.Int64 -> PrimitiveTypeCode.Int64
        | ReferencePrimitiveType.UInt64 -> PrimitiveTypeCode.UInt64
        | ReferencePrimitiveType.Single -> PrimitiveTypeCode.Single
        | ReferencePrimitiveType.Double -> PrimitiveTypeCode.Double
        | value ->
            raiseDecode
                ReferenceImportErrorKind.InvalidCustomAttribute
                "CustomAttribute.Enum"
                None
                $"The enum backing type '{value}' is invalid."

    let private sameTypeIdentity (left: ReferenceTypeIdentity) (right: ReferenceTypeIdentity) =
        left.ModuleName = right.ModuleName
        && left.Namespace = right.Namespace
        && left.EnclosingTypes = right.EnclosingTypes
        && left.MetadataName = right.MetadataName
        && left.GenericArity = right.GenericArity

    let private tryEnumUnderlyingType (context: DecodeContext) (handle: TypeDefinitionHandle) =
        let definition = context.Metadata.GetTypeDefinition(handle)

        definition.GetFields()
        |> Seq.tryPick (fun fieldHandle ->
            let field = context.Metadata.GetFieldDefinition(fieldHandle)

            if getString context "CustomAttribute.Enum.Field" field.Name = "value__" then
                let value =
                    decodeCliType
                        context
                        "CustomAttribute.Enum.FieldSignature"
                        field.Signature
                        (fun provider -> field.DecodeSignature(provider, ()))

                match value with
                | ReferenceCliType.Primitive primitive ->
                    primitive
                    |> primitiveTypeCode
                    |> Some
                | _ ->
                    raiseDecode
                        ReferenceImportErrorKind.InvalidCustomAttribute
                        "CustomAttribute.Enum.FieldSignature"
                        (blobOffset field.Signature)
                        "The enum backing field does not have an integral primitive type."
            else
                None
        )

    let private enumUnderlyingType (context: DecodeContext) (enumType: ReferenceCliType) =
        let identity =
            match enumType with
            | ReferenceCliType.Definition(value, _)
            | ReferenceCliType.Reference(value, _) -> value
            | _ ->
                raiseDecode
                    ReferenceImportErrorKind.InvalidCustomAttribute
                    "CustomAttribute.Enum"
                    None
                    "The custom-attribute enum type is invalid."

        let localDefinition =
            context.Metadata.TypeDefinitions
            |> Seq.tryFind (fun handle ->
                sameTypeIdentity (typeDefinitionIdentity context handle) identity
            )
            |> Option.bind (tryEnumUnderlyingType context)

        let externalDefinition =
            typeAssemblyScope identity
            |> Option.bind (fun assembly ->
                let key = enumDefinitionKey assembly identity

                match context.EnumDefinitions.TryGetValue(key) with
                | true, demand ->
                    match demand.Get(context.CancellationToken) with
                    | Ok primitive -> primitive
                    | Error error -> raise (DemandFailure error)
                | _ -> None
            )

        localDefinition
        |> Option.orElse externalDefinition
        |> Option.defaultWith (fun () ->
            raiseDecode
                ReferenceImportErrorKind.InvalidCustomAttribute
                "CustomAttribute.Enum"
                None
                "The enum definition is not available in the supplied metadata universe."
        )

    type private AttributeDecodedType = {
        CliType: ReferenceCliType
        SerializedName: string option
    }

    type private AttributeTypeProvider(context: DecodeContext) =
        interface ICustomAttributeTypeProvider<AttributeDecodedType> with
            member _.GetPrimitiveType(code) = {
                CliType = ReferenceCliType.Primitive(primitiveType code)
                SerializedName = None
            }

            member _.GetSystemType() = {
                CliType = systemType
                SerializedName = None
            }

            member _.GetTypeFromDefinition(_, handle, rawTypeKind) = {
                CliType =
                    ReferenceCliType.Definition(
                        typeDefinitionIdentity context handle,
                        rawTypeKind = byte SignatureTypeKind.ValueType
                    )
                SerializedName = None
            }

            member _.GetTypeFromReference(_, handle, rawTypeKind) = {
                CliType =
                    ReferenceCliType.Reference(
                        typeReferenceIdentity context handle,
                        rawTypeKind = byte SignatureTypeKind.ValueType
                    )
                SerializedName = None
            }

            member _.GetSZArrayType(elementType) = {
                CliType = ReferenceCliType.SzArray elementType.CliType
                SerializedName = None
            }

            member _.GetTypeFromSerializedName(name) = {
                CliType = ReferenceCliType.Reference(serializedTypeIdentity name, false)
                SerializedName = Some name
            }

            member _.GetUnderlyingEnumType(value) =
                enumUnderlyingType context value.CliType

            member _.IsSystemType(value) = isSystemType value.CliType

    let private constantFromObject (value: obj) =
        match value with
        | :? bool as item -> Some(ReferenceConstant.Boolean item)
        | :? char as item -> Some(ReferenceConstant.Char item)
        | :? sbyte as item -> Some(ReferenceConstant.SByte item)
        | :? byte as item -> Some(ReferenceConstant.Byte item)
        | :? int16 as item -> Some(ReferenceConstant.Int16 item)
        | :? uint16 as item -> Some(ReferenceConstant.UInt16 item)
        | :? int32 as item -> Some(ReferenceConstant.Int32 item)
        | :? uint32 as item -> Some(ReferenceConstant.UInt32 item)
        | :? int64 as item -> Some(ReferenceConstant.Int64 item)
        | :? uint64 as item -> Some(ReferenceConstant.UInt64 item)
        | :? single as item -> Some(ReferenceConstant.Single item)
        | :? double as item -> Some(ReferenceConstant.Double item)
        | _ -> None

    let rec private attributeValue (argumentType: AttributeDecodedType) (value: obj) =
        if isNull value then
            match argumentType.CliType with
            | ReferenceCliType.Primitive ReferencePrimitiveType.String ->
                ReferenceAttributeValue.String None
            | ReferenceCliType.SzArray elementType ->
                ReferenceAttributeValue.Array(elementType, None)
            | valueType when isSystemType valueType -> ReferenceAttributeValue.TypeName None
            | _ -> ReferenceAttributeValue.Null
        else
            match value with
            | :? AttributeDecodedType as item when isSystemType argumentType.CliType ->
                ReferenceAttributeValue.TypeName item.SerializedName
            | :? string as item when isSystemType argumentType.CliType ->
                ReferenceAttributeValue.TypeName(Some item)
            | :? string as item -> ReferenceAttributeValue.String(Some item)
            | :? CustomAttributeTypedArgument<AttributeDecodedType> as item ->
                attributeValue item.Type item.Value
                |> ReferenceAttributeValue.Boxed
            | :? ImmutableArray<CustomAttributeTypedArgument<AttributeDecodedType>> as items ->
                let elementType =
                    match argumentType.CliType with
                    | ReferenceCliType.SzArray item -> item
                    | _ -> argumentType.CliType

                items
                |> Seq.map (fun item -> attributeValue item.Type item.Value)
                |> List.ofSeq
                |> fun values -> ReferenceAttributeValue.Array(elementType, Some values)
            | item ->
                match constantFromObject item with
                | Some constant ->
                    match argumentType.CliType with
                    | ReferenceCliType.Primitive _ -> ReferenceAttributeValue.Primitive constant
                    | _ -> ReferenceAttributeValue.Enum(argumentType.CliType, constant)
                | None ->
                    raiseDecode
                        ReferenceImportErrorKind.InvalidCustomAttribute
                        "CustomAttribute.Value"
                        None
                        "The custom-attribute value is invalid."

    let private attributeConstructor (context: DecodeContext) (handle: EntityHandle) =
        match handle.Kind with
        | HandleKind.MethodDefinition ->
            handle
            |> MethodDefinitionHandle.op_Explicit
            |> methodDefinitionIdentity context
            |> ReferenceAttributeConstructor.MethodDefinition
        | HandleKind.MemberReference ->
            handle
            |> MemberReferenceHandle.op_Explicit
            |> memberReferenceIdentity context
            |> ReferenceAttributeConstructor.MemberReference
        | kind ->
            raiseDecode
                ReferenceImportErrorKind.InvalidCustomAttribute
                "CustomAttribute.Constructor"
                None
                $"The custom-attribute constructor kind '{kind}' is invalid."

    let private customAttribute (context: DecodeContext) (handle: CustomAttributeHandle) =
        visitRow context
        let attribute = context.Metadata.GetCustomAttribute(handle)
        let row = MetadataTokens.GetRowNumber(handle)
        let rawBlob = getBlob context $"CustomAttribute:{row}.Value" attribute.Value
        let constructor = attributeConstructor context attribute.Constructor

        let decoded =
            try
                attribute.DecodeValue(AttributeTypeProvider(context))
            with
            | DecodeFailure _ -> reraise ()
            | :? BadImageFormatException as exceptionValue ->
                raiseDecode
                    ReferenceImportErrorKind.InvalidCustomAttribute
                    $"CustomAttribute:{row}"
                    (blobOffset attribute.Value)
                    exceptionValue.Message

        let declaredParameterTypes =
            let identity =
                match constructor with
                | ReferenceAttributeConstructor.MethodDefinition value
                | ReferenceAttributeConstructor.MemberReference value -> value

            identity.MethodSignature
            |> Option.map _.ParameterTypes
            |> Option.defaultValue []

        {
            Constructor = constructor
            FixedArguments =
                decoded.FixedArguments
                |> Seq.mapi (fun index argument ->
                    let value = attributeValue argument.Type argument.Value

                    match
                        declaredParameterTypes
                        |> List.tryItem index
                    with
                    | Some(ReferenceCliType.Primitive ReferencePrimitiveType.Object) ->
                        ReferenceAttributeValue.Boxed value
                    | _ -> value
                )
                |> List.ofSeq
            NamedArguments =
                decoded.NamedArguments
                |> Seq.map (fun argument -> {
                    Name = argument.Name
                    IsField = argument.Kind = CustomAttributeNamedArgumentKind.Field
                    ArgumentType = argument.Type.CliType
                    Value = attributeValue argument.Type argument.Value
                })
                |> List.ofSeq
            RawBlob = rawBlob
        }

    let private customAttributes
        (context: DecodeContext)
        (handles: CustomAttributeHandleCollection)
        =
        handles
        |> Seq.map (customAttribute context)
        |> List.ofSeq

    let private constantValue (context: DecodeContext) (handle: ConstantHandle) =
        if handle.IsNil then
            None
        else
            visitRow context
            let constant = context.Metadata.GetConstant(handle)
            let row = MetadataTokens.GetRowNumber(handle)
            let mutable reader = context.Metadata.GetBlobReader(constant.Value)
            checkHeapLength context $"Constant:{row}.Value" reader.Length

            try
                match constant.TypeCode with
                | ConstantTypeCode.NullReference -> ReferenceConstant.Null
                | ConstantTypeCode.Boolean -> ReferenceConstant.Boolean(reader.ReadBoolean())
                | ConstantTypeCode.Char -> ReferenceConstant.Char(char (reader.ReadUInt16()))
                | ConstantTypeCode.SByte -> ReferenceConstant.SByte(reader.ReadSByte())
                | ConstantTypeCode.Byte -> ReferenceConstant.Byte(reader.ReadByte())
                | ConstantTypeCode.Int16 -> ReferenceConstant.Int16(reader.ReadInt16())
                | ConstantTypeCode.UInt16 -> ReferenceConstant.UInt16(reader.ReadUInt16())
                | ConstantTypeCode.Int32 -> ReferenceConstant.Int32(reader.ReadInt32())
                | ConstantTypeCode.UInt32 -> ReferenceConstant.UInt32(reader.ReadUInt32())
                | ConstantTypeCode.Int64 -> ReferenceConstant.Int64(reader.ReadInt64())
                | ConstantTypeCode.UInt64 -> ReferenceConstant.UInt64(reader.ReadUInt64())
                | ConstantTypeCode.Single -> ReferenceConstant.Single(reader.ReadSingle())
                | ConstantTypeCode.Double -> ReferenceConstant.Double(reader.ReadDouble())
                | ConstantTypeCode.String ->
                    ReferenceConstant.String(reader.ReadUTF16(reader.Length))
                | value ->
                    raiseDecode
                        ReferenceImportErrorKind.MalformedCliMetadata
                        $"Constant:{row}.Type"
                        (blobOffset constant.Value)
                        $"The constant type code '{value}' is invalid."
                |> Some
            with
            | DecodeFailure _ -> reraise ()
            | :? BadImageFormatException as exceptionValue ->
                raiseDecode
                    ReferenceImportErrorKind.MalformedCliMetadata
                    $"Constant:{row}.Value"
                    (blobOffset constant.Value)
                    exceptionValue.Message

    let private genericParameter (context: DecodeContext) (handle: GenericParameterHandle) =
        visitRow context
        let parameter = context.Metadata.GetGenericParameter(handle)
        let row = MetadataTokens.GetRowNumber(handle)
        let constraintHandles = parameter.GetConstraints()

        let constraintCount =
            constraintHandles
            |> Seq.length

        if constraintCount > context.Limits.MaximumGenericConstraints then
            raiseDecode
                ReferenceImportErrorKind.GenericConstraintLimitExceeded
                $"GenericParam:{row}.Constraints"
                None
                "A generic parameter exceeds the configured constraint limit."

        let attributes = parameter.Attributes

        let variance =
            match
                attributes
                &&& GenericParameterAttributes.VarianceMask
            with
            | GenericParameterAttributes.Covariant -> ReferenceGenericVariance.Covariant
            | GenericParameterAttributes.Contravariant -> ReferenceGenericVariance.Contravariant
            | _ -> ReferenceGenericVariance.Invariant

        {
            Index = parameter.Index
            Name = getString context $"GenericParam:{row}.Name" parameter.Name
            Variance = variance
            SpecialConstraints = {
                ReferenceType =
                    (attributes
                     &&& GenericParameterAttributes.ReferenceTypeConstraint)
                    <> enum<GenericParameterAttributes> 0
                ValueType =
                    (attributes
                     &&& GenericParameterAttributes.NotNullableValueTypeConstraint)
                    <> enum<GenericParameterAttributes> 0
                DefaultConstructor =
                    (attributes
                     &&& GenericParameterAttributes.DefaultConstructorConstraint)
                    <> enum<GenericParameterAttributes> 0
                AllowByRefLike =
                    (int attributes
                     &&& 0x20)
                    <> 0
            }
            TypeConstraints =
                constraintHandles
                |> Seq.map (fun constraintHandle ->
                    visitRow context

                    context.Metadata.GetGenericParameterConstraint(constraintHandle).Type
                    |> entityType
                        context
                        $"GenericParamConstraint:{MetadataTokens.GetRowNumber(constraintHandle)}"
                )
                |> List.ofSeq
        }

    let private parameterDefinition (context: DecodeContext) (handle: ParameterHandle) =
        visitRow context
        let parameter = context.Metadata.GetParameter(handle)
        let row = MetadataTokens.GetRowNumber(handle)

        {
            Sequence = parameter.SequenceNumber
            Name = optionalString context $"Param:{row}.Name" parameter.Name
            Attributes = int parameter.Attributes
            DefaultValue = constantValue context (parameter.GetDefaultValue())
            CustomAttributes =
                parameter.GetCustomAttributes()
                |> customAttributes context
        }

    let private methodDefinition
        (context: DecodeContext)
        (declaringType: ReferenceTypeIdentity)
        (handle: MethodDefinitionHandle)
        =
        let identity = methodDefinitionIdentity context handle
        let definition = context.Metadata.GetMethodDefinition(handle)

        {
            DeclaringType = declaringType
            Name = identity.MetadataName
            Attributes = int definition.Attributes
            ImplementationAttributes = int definition.ImplAttributes
            Accessibility = methodAccessibility definition.Attributes
            Signature =
                identity.MethodSignature
                |> Option.get
            Parameters =
                definition.GetParameters()
                |> Seq.map (parameterDefinition context)
                |> List.ofSeq
            GenericParameters =
                definition.GetGenericParameters()
                |> Seq.map (genericParameter context)
                |> List.ofSeq
            CustomAttributes =
                definition.GetCustomAttributes()
                |> customAttributes context
        }

    let private fieldDefinition
        (context: DecodeContext)
        (declaringType: ReferenceTypeIdentity)
        (handle: FieldDefinitionHandle)
        =
        let identity = fieldDefinitionIdentity context handle
        let definition = context.Metadata.GetFieldDefinition(handle)

        {
            DeclaringType = declaringType
            Name = identity.MetadataName
            Attributes = int definition.Attributes
            Accessibility = fieldAccessibility definition.Attributes
            FieldType =
                identity.FieldType
                |> Option.get
            DefaultValue = constantValue context (definition.GetDefaultValue())
            CustomAttributes =
                definition.GetCustomAttributes()
                |> customAttributes context
        }

    let private accessorAccessibility (context: DecodeContext) (handle: MethodDefinitionHandle) =
        if handle.IsNil then
            None
        else
            visitRow context

            context.Metadata.GetMethodDefinition(handle).Attributes
            |> methodAccessibility
            |> Some

    let private propertyDefinitionIdentity
        (context: DecodeContext)
        (handle: PropertyDefinitionHandle)
        =
        visitRow context
        let definition = context.Metadata.GetPropertyDefinition(handle)
        let row = MetadataTokens.GetRowNumber(handle)

        let signature =
            decodeMethodSignature
                context
                $"Property:{row}.Signature"
                definition.Signature
                (fun provider -> definition.DecodeSignature(provider, ()))

        {
            DeclaringType = Some(typeDefinitionIdentity context (definition.GetDeclaringType()))
            Parent = None
            Kind = ReferenceMemberKind.Property
            MetadataName = getString context $"Property:{row}.Name" definition.Name
            GenericArity = 0
            MethodSignature = None
            FieldType = None
            PropertySignature = Some signature
            EventType = None
        }

    let private eventDefinitionIdentity (context: DecodeContext) (handle: EventDefinitionHandle) =
        visitRow context
        let definition = context.Metadata.GetEventDefinition(handle)
        let row = MetadataTokens.GetRowNumber(handle)

        {
            DeclaringType = Some(typeDefinitionIdentity context (definition.GetDeclaringType()))
            Parent = None
            Kind = ReferenceMemberKind.Event
            MetadataName = getString context $"Event:{row}.Name" definition.Name
            GenericArity = 0
            MethodSignature = None
            FieldType = None
            PropertySignature = None
            EventType = Some(entityType context $"Event:{row}.Type" definition.Type)
        }

    let private propertyDefinition
        (context: DecodeContext)
        (declaringType: ReferenceTypeIdentity)
        (handle: PropertyDefinitionHandle)
        =
        let identity = propertyDefinitionIdentity context handle
        let definition = context.Metadata.GetPropertyDefinition(handle)
        let accessors = definition.GetAccessors()

        {
            Identity = identity
            DeclaringType = declaringType
            Name = identity.MetadataName
            Attributes = int definition.Attributes
            Signature =
                identity.PropertySignature
                |> Option.get
            GetterAccessibility = accessorAccessibility context accessors.Getter
            SetterAccessibility = accessorAccessibility context accessors.Setter
            OtherAccessibilities =
                accessors.Others
                |> Seq.choose (accessorAccessibility context)
                |> List.ofSeq
            DefaultValue = constantValue context (definition.GetDefaultValue())
            CustomAttributes =
                definition.GetCustomAttributes()
                |> customAttributes context
        }

    let private eventDefinition
        (context: DecodeContext)
        (declaringType: ReferenceTypeIdentity)
        (handle: EventDefinitionHandle)
        =
        let identity = eventDefinitionIdentity context handle
        let definition = context.Metadata.GetEventDefinition(handle)
        let accessors = definition.GetAccessors()

        {
            Identity = identity
            DeclaringType = declaringType
            Name = identity.MetadataName
            Attributes = int definition.Attributes
            EventType =
                identity.EventType
                |> Option.get
            AddAccessibility = accessorAccessibility context accessors.Adder
            RemoveAccessibility = accessorAccessibility context accessors.Remover
            RaiseAccessibility = accessorAccessibility context accessors.Raiser
            OtherAccessibilities =
                accessors.Others
                |> Seq.choose (accessorAccessibility context)
                |> List.ofSeq
            CustomAttributes =
                definition.GetCustomAttributes()
                |> customAttributes context
        }

    let private methodEntityIdentity
        (context: DecodeContext)
        (location: string)
        (handle: EntityHandle)
        =
        match handle.Kind with
        | HandleKind.MethodDefinition ->
            handle
            |> MethodDefinitionHandle.op_Explicit
            |> methodDefinitionIdentity context
        | HandleKind.MemberReference ->
            handle
            |> MemberReferenceHandle.op_Explicit
            |> memberReferenceIdentity context
        | kind ->
            raiseDecode
                ReferenceImportErrorKind.MalformedCliMetadata
                location
                None
                $"The method identity kind '{kind}' is invalid."

    let private enclosingAccessibilities (context: DecodeContext) (definition: TypeDefinition) =
        let rec loop values (handle: TypeDefinitionHandle) =
            if handle.IsNil then
                values
            else
                visitRow context
                let parent = context.Metadata.GetTypeDefinition(handle)

                loop
                    (typeAccessibility parent.Attributes
                     :: values)
                    (parent.GetDeclaringType())

        loop [] (definition.GetDeclaringType())

    let private decodeTypeSemantics (context: DecodeContext) (handle: TypeDefinitionHandle) =
        visitRow context
        let definition = context.Metadata.GetTypeDefinition(handle)
        let identity = typeDefinitionIdentity context handle

        {
            BaseType =
                if definition.BaseType.IsNil then
                    None
                else
                    entityType
                        context
                        $"TypeDef:{MetadataTokens.GetRowNumber(handle)}.BaseType"
                        definition.BaseType
                    |> Some
            GenericParameters =
                definition.GetGenericParameters()
                |> Seq.map (genericParameter context)
                |> List.ofSeq
            Methods =
                definition.GetMethods()
                |> Seq.map (methodDefinition context identity)
                |> List.ofSeq
            Fields =
                definition.GetFields()
                |> Seq.map (fieldDefinition context identity)
                |> List.ofSeq
            Properties =
                definition.GetProperties()
                |> Seq.map (propertyDefinition context identity)
                |> List.ofSeq
            Events =
                definition.GetEvents()
                |> Seq.map (eventDefinition context identity)
                |> List.ofSeq
            MethodImplementations =
                definition.GetMethodImplementations()
                |> Seq.map (fun implementationHandle ->
                    visitRow context

                    let implementation =
                        context.Metadata.GetMethodImplementation(implementationHandle)

                    let row = MetadataTokens.GetRowNumber(implementationHandle)

                    {
                        DeclaringType = identity
                        MethodBody =
                            methodEntityIdentity
                                context
                                $"MethodImpl:{row}.MethodBody"
                                implementation.MethodBody
                        MethodDeclaration =
                            methodEntityIdentity
                                context
                                $"MethodImpl:{row}.MethodDeclaration"
                                implementation.MethodDeclaration
                    }
                )
                |> List.ofSeq
            Interfaces =
                definition.GetInterfaceImplementations()
                |> Seq.map (fun implementationHandle ->
                    visitRow context

                    let implementation =
                        context.Metadata.GetInterfaceImplementation(implementationHandle)

                    let row = MetadataTokens.GetRowNumber(implementationHandle)

                    {
                        DeclaringType = identity
                        InterfaceType =
                            entityType
                                context
                                $"InterfaceImpl:{row}.Interface"
                                implementation.Interface
                        CustomAttributes =
                            implementation.GetCustomAttributes()
                            |> customAttributes context
                    }
                )
                |> List.ofSeq
            CustomAttributes =
                definition.GetCustomAttributes()
                |> customAttributes context
        }

    let private parseFriendAssembly =
        function
        | {
              Constructor = constructor
              FixedArguments = ReferenceAttributeValue.String(Some value) :: _
          } ->
            let identity =
                match constructor with
                | ReferenceAttributeConstructor.MethodDefinition identity
                | ReferenceAttributeConstructor.MemberReference identity -> identity

            match identity.DeclaringType with
            | Some declaringType when
                declaringType.Namespace = "System.Runtime.CompilerServices"
                && declaringType.MetadataName = "InternalsVisibleToAttribute"
                ->
                let parts =
                    value.Split(',')
                    |> Array.map _.Trim()

                let publicKey =
                    parts
                    |> Array.skip 1
                    |> Array.filter _.StartsWith("PublicKey=", StringComparison.OrdinalIgnoreCase)
                    |> function
                        | [||] -> []
                        | [| part |] ->
                            let text = part.Substring("PublicKey=".Length)

                            if String.IsNullOrWhiteSpace(text) then
                                raiseDecode
                                    ReferenceImportErrorKind.InvalidCustomAttribute
                                    "InternalsVisibleTo.PublicKey"
                                    None
                                    "The InternalsVisibleTo public key is empty."

                            try
                                Convert.FromHexString(text)
                                |> List.ofArray
                            with :? FormatException ->
                                raiseDecode
                                    ReferenceImportErrorKind.InvalidCustomAttribute
                                    "InternalsVisibleTo.PublicKey"
                                    None
                                    ("The InternalsVisibleTo public key is not valid "
                                     + "hexadecimal text.")
                        | _ ->
                            raiseDecode
                                ReferenceImportErrorKind.InvalidCustomAttribute
                                "InternalsVisibleTo.PublicKey"
                                None
                                "The InternalsVisibleTo identity contains duplicate public keys."

                if String.IsNullOrWhiteSpace(parts.[0]) then
                    raiseDecode
                        ReferenceImportErrorKind.InvalidCustomAttribute
                        "InternalsVisibleTo.Name"
                        None
                        "The InternalsVisibleTo assembly name is empty."

                Some {
                    Name = parts.[0]
                    PublicKey = publicKey
                }
            | _ -> None
        | _ -> None

    let private decodeEcmaSemantics (context: DecodeContext) =
        let typeSpecifications =
            seq {
                for row in 1 .. context.Metadata.GetTableRowCount(TableIndex.TypeSpec) do
                    yield MetadataTokens.TypeSpecificationHandle(row)
            }
            |> Seq.map (fun handle ->
                visitRow context
                let specification = context.Metadata.GetTypeSpecification(handle)

                decodeCliType
                    context
                    $"TypeSpec:{MetadataTokens.GetRowNumber(handle)}"
                    specification.Signature
                    (fun provider -> specification.DecodeSignature(provider, ()))
            )
            |> List.ofSeq

        let memberReferences =
            context.Metadata.MemberReferences
            |> Seq.map (fun handle -> {
                Identity = memberReferenceIdentity context handle
            })
            |> List.ofSeq

        let methodSpecifications =
            seq {
                for row in 1 .. context.Metadata.GetTableRowCount(TableIndex.MethodSpec) do
                    yield MetadataTokens.MethodSpecificationHandle(row)
            }
            |> Seq.map (fun handle ->
                visitRow context
                let specification = context.Metadata.GetMethodSpecification(handle)
                let row = MetadataTokens.GetRowNumber(handle)

                {
                    Method =
                        methodEntityIdentity
                            context
                            $"MethodSpec:{row}.Method"
                            specification.Method
                    TypeArguments =
                        decodeCliType
                            context
                            $"MethodSpec:{row}.Signature"
                            specification.Signature
                            (fun provider ->
                                let arguments =
                                    specification.DecodeSignature(provider, ())
                                    |> List.ofSeq

                                budgeted
                                    context
                                    (ReferenceCliType.GenericInstantiation(
                                        ReferenceCliType.Primitive ReferencePrimitiveType.Object,
                                        arguments
                                        |> List.map _.Value
                                    ))
                                    arguments
                            )
                        |> function
                            | ReferenceCliType.GenericInstantiation(_, arguments) -> arguments
                            | _ -> []
                }
            )
            |> List.ofSeq

        let standaloneSignatures =
            seq {
                for row in 1 .. context.Metadata.GetTableRowCount(TableIndex.StandAloneSig) do
                    yield MetadataTokens.StandaloneSignatureHandle(row)
            }
            |> Seq.map (fun handle ->
                visitRow context
                let signature = context.Metadata.GetStandaloneSignature(handle)
                let row = MetadataTokens.GetRowNumber(handle)

                try
                    let mutable blobReader = context.Metadata.GetBlobReader(signature.Signature)

                    let kind = blobReader.ReadSignatureHeader().Kind
                    blobReader.Reset()

                    match kind with
                    | SignatureKind.Method ->
                        decodeMethodSignature
                            context
                            $"StandAloneSig:{row}"
                            signature.Signature
                            (fun provider -> signature.DecodeMethodSignature(provider, ()))
                        |> ReferenceStandaloneSignature.Method
                    | SignatureKind.Field ->
                        let decoder =
                            SignatureDecoder<BudgetedCliType, unit>(
                                signatureProvider context,
                                context.Metadata,
                                ()
                            )

                        decoder.DecodeFieldSignature(&blobReader).Value
                        |> ReferenceStandaloneSignature.Field
                    | SignatureKind.LocalVariables ->
                        let values =
                            signature.DecodeLocalSignature(signatureProvider context, ())
                            |> List.ofSeq

                        let nodes =
                            values
                            |> List.fold (fun total value -> checkedAdd total value.Nodes) 1

                        let depth =
                            1
                            + (values
                               |> List.map _.Depth
                               |> List.fold max 0)

                        if depth > context.Limits.MaximumSignatureDepth then
                            raiseDecode
                                ReferenceImportErrorKind.SignatureDepthExceeded
                                $"StandAloneSig:{row}"
                                (blobOffset signature.Signature)
                                "A local signature exceeds the configured depth limit."

                        if nodes > context.Limits.MaximumSignatureNodes then
                            raiseDecode
                                ReferenceImportErrorKind.SignatureNodeLimitExceeded
                                $"StandAloneSig:{row}"
                                (blobOffset signature.Signature)
                                "A local signature exceeds the configured node limit."

                        values
                        |> List.map _.Value
                        |> ReferenceStandaloneSignature.LocalVariables
                    | kind ->
                        raiseDecode
                            ReferenceImportErrorKind.InvalidSignature
                            $"StandAloneSig:{row}"
                            (blobOffset signature.Signature)
                            $"The standalone signature kind '{kind}' is invalid."
                with
                | DecodeFailure _ -> reraise ()
                | :? BadImageFormatException as exceptionValue ->
                    raiseDecode
                        ReferenceImportErrorKind.InvalidSignature
                        $"StandAloneSig:{row}"
                        (blobOffset signature.Signature)
                        exceptionValue.Message
            )
            |> List.ofSeq

        let attributes =
            if context.Metadata.IsAssembly then
                context.Metadata.GetAssemblyDefinition().GetCustomAttributes()
                |> customAttributes context
            else
                context.Metadata.GetModuleDefinition().GetCustomAttributes()
                |> customAttributes context

        {
            TypeSpecifications = typeSpecifications
            MemberReferences = memberReferences
            MethodSpecifications = methodSpecifications
            StandaloneSignatures = standaloneSignatures
            CustomAttributes = attributes
            FriendAssemblies =
                attributes
                |> List.choose parseFriendAssembly
        }

    let private protect
        (snapshot: TargetReferenceSnapshot)
        (defaultKind: ReferenceImportErrorKind)
        (location: string)
        action
        =
        try
            action ()
            |> Ok
        with
        | DemandFailure error -> Error error
        | DecodeFailure(kind, errorLocation, offset, message) ->
            Error(failure snapshot kind (Some errorLocation) offset None message)
        | :? BadImageFormatException as exceptionValue ->
            Error(failure snapshot defaultKind (Some location) None None exceptionValue.Message)
        | :? ArgumentOutOfRangeException as exceptionValue ->
            Error(failure snapshot defaultKind (Some location) None None exceptionValue.Message)
        | :? IndexOutOfRangeException as exceptionValue ->
            Error(failure snapshot defaultKind (Some location) None None exceptionValue.Message)
        | :? OverflowException as exceptionValue ->
            Error(failure snapshot defaultKind (Some location) None None exceptionValue.Message)
        | :? InvalidOperationException as exceptionValue ->
            Error(failure snapshot defaultKind (Some location) None None exceptionValue.Message)
        | :? ArgumentException as exceptionValue ->
            Error(failure snapshot defaultKind (Some location) None None exceptionValue.Message)

    let private openContext
        (limits: ReferenceImportLimits)
        (enumDefinitions: Dictionary<EnumDefinitionKey, EnumDefinitionDemand>)
        (snapshot: TargetReferenceSnapshot)
        (cancellationToken: CancellationToken)
        action
        =
        cancellationToken.ThrowIfCancellationRequested()
        use pe = new PEReader(snapshot.PeImage)

        if not pe.HasMetadata then
            raiseDecode
                ReferenceImportErrorKind.MissingCliMetadata
                "PE.CliHeader"
                None
                "The PE image has no CLI metadata."

        let metadata = pe.GetMetadataReader()
        let moduleDefinition = metadata.GetModuleDefinition()
        let moduleName = getStringValue limits metadata "Module.Name" moduleDefinition.Name

        let context = {
            Limits = limits
            Snapshot = snapshot
            Metadata = metadata
            ModuleName = moduleName
            EnumDefinitions = enumDefinitions
            CancellationToken = cancellationToken
            RowsVisited = 0
        }

        action pe context

    let private createDemand
        (limits: ReferenceImportLimits)
        (enumDefinitions: Dictionary<EnumDefinitionKey, EnumDefinitionDemand>)
        (snapshot: TargetReferenceSnapshot)
        (location: string)
        action
        =
        ReferenceSemanticDemand(fun cancellationToken ->
            protect
                snapshot
                ReferenceImportErrorKind.MalformedCliMetadata
                location
                (fun () ->
                    openContext
                        limits
                        enumDefinitions
                        snapshot
                        cancellationToken
                        (fun _ context -> action context)
                )
        )

    let private exportedTypeIdentity (context: DecodeContext) (handle: ExportedTypeHandle) =
        let visited = HashSet<int>()

        let rec loop depth (current: ExportedTypeHandle) =
            if depth > context.Limits.MaximumResolutionDepth then
                raiseDecode
                    ReferenceImportErrorKind.ResolutionDepthExceeded
                    "ExportedType.Parent"
                    None
                    "The exported-type parent path exceeds the configured depth limit."

            let row = MetadataTokens.GetRowNumber(current)

            if not (visited.Add(row)) then
                raiseDecode
                    ReferenceImportErrorKind.InvalidForwarderParent
                    $"ExportedType:{row}.Parent"
                    None
                    "The exported-type parent path contains a cycle."

            visitRow context
            let exportedType = context.Metadata.GetExportedType(current)
            let name = getString context $"ExportedType:{row}.Name" exportedType.Name

            if exportedType.Implementation.Kind = HandleKind.ExportedType then
                let parent =
                    exportedType.Implementation
                    |> ExportedTypeHandle.op_Explicit
                    |> loop (depth + 1)

                {
                    ModuleName = context.ModuleName
                    Namespace = parent.Namespace
                    EnclosingTypes =
                        parent.EnclosingTypes
                        @ [ parent.MetadataName ]
                    MetadataName = name
                    GenericArity = metadataArity name
                    ResolutionScope = None
                }
            else
                {
                    ModuleName = context.ModuleName
                    Namespace =
                        getString context $"ExportedType:{row}.Namespace" exportedType.Namespace
                    EnclosingTypes = []
                    MetadataName = name
                    GenericArity = metadataArity name
                    ResolutionScope = None
                }

        loop 0 handle

    let private exportedTarget (context: DecodeContext) (handle: EntityHandle) =
        match handle.Kind with
        | HandleKind.AssemblyReference ->
            handle
            |> AssemblyReferenceHandle.op_Explicit
            |> assemblyReference context
            |> _.Identity
            |> ReferenceForwarderTarget.Assembly
        | HandleKind.AssemblyFile ->
            let file =
                handle
                |> AssemblyFileHandle.op_Explicit
                |> context.Metadata.GetAssemblyFile

            let row = MetadataTokens.GetRowNumber(handle)

            ReferenceForwarderTarget.File {
                Name = getString context $"File:{row}.Name" file.Name
                ContainsMetadata = file.ContainsMetadata
                HashValue = getBlob context $"File:{row}.HashValue" file.HashValue
            }
        | HandleKind.ExportedType ->
            handle
            |> ExportedTypeHandle.op_Explicit
            |> exportedTypeIdentity context
            |> ReferenceForwarderTarget.ExportedType
        | kind ->
            raiseDecode
                ReferenceImportErrorKind.InvalidForwarderParent
                "ExportedType.Implementation"
                None
                $"The exported-type implementation kind '{kind}' is invalid."

    let private resourceImplementation (context: DecodeContext) (handle: EntityHandle) =
        if handle.IsNil then
            ReferenceResourceImplementation.Embedded
        else
            match handle.Kind with
            | HandleKind.AssemblyFile ->
                let file =
                    handle
                    |> AssemblyFileHandle.op_Explicit
                    |> context.Metadata.GetAssemblyFile

                let row = MetadataTokens.GetRowNumber(handle)

                ReferenceResourceImplementation.File {
                    Name = getString context $"File:{row}.Name" file.Name
                    ContainsMetadata = file.ContainsMetadata
                    HashValue = getBlob context $"File:{row}.HashValue" file.HashValue
                }
            | HandleKind.AssemblyReference ->
                handle
                |> AssemblyReferenceHandle.op_Explicit
                |> assemblyReference context
                |> _.Identity
                |> ReferenceResourceImplementation.Assembly
            | HandleKind.ExportedType ->
                handle
                |> ExportedTypeHandle.op_Explicit
                |> exportedTypeIdentity context
                |> ReferenceResourceImplementation.ExportedType
            | kind ->
                raiseDecode
                    ReferenceImportErrorKind.InvalidResource
                    "ManifestResource.Implementation"
                    None
                    $"The manifest-resource implementation kind '{kind}' is invalid."

    let private embeddedResource
        (context: DecodeContext)
        (pe: PEReader)
        (row: int)
        (resource: ManifestResource)
        =
        let directory = pe.PEHeaders.CorHeader.ResourcesDirectory

        if
            directory.RelativeVirtualAddress = 0
            || directory.Size < 4
        then
            raiseDecode
                ReferenceImportErrorKind.InvalidResource
                $"ManifestResource:{row}"
                None
                "The embedded resource directory is missing."

        let offset = checkedInt64ToInt resource.Offset
        let lengthPositionEnd = checkedAdd offset 4

        if
            offset < 0
            || lengthPositionEnd > directory.Size
        then
            raiseDecode
                ReferenceImportErrorKind.InvalidResource
                $"ManifestResource:{row}.Offset"
                (Some offset)
                "The embedded resource offset is outside the resource directory."

        let section = pe.GetSectionData(directory.RelativeVirtualAddress)

        let mutable reader =
            section.GetReader(
                offset,
                directory.Size
                - offset
            )

        let length = checkedUInt32ToInt (reader.ReadUInt32())
        checkHeapLength context $"ManifestResource:{row}.Value" length

        if
            length < 0
            || length > reader.RemainingBytes
        then
            raiseDecode
                ReferenceImportErrorKind.InvalidResource
                $"ManifestResource:{row}.Length"
                (Some offset)
                "The embedded resource length is outside the resource directory."

        let fingerprint =
            reader.ReadBytes(length)
            |> SHA256.HashData
            |> Convert.ToHexString
            |> _.ToLowerInvariant()

        Some length, Some fingerprint

    let private rawMemberIdentityKey
        (context: DecodeContext)
        (declaringType: ReferenceTypeIdentity)
        (kind: ReferenceMemberKind)
        (name: string)
        (genericArity: int)
        (signature: BlobHandle)
        =
        let signatureBytes = getBlobArray context "Member.Signature" signature

        let signatureFingerprint =
            signatureBytes
            |> SHA256.HashData
            |> Convert.ToHexString

        declaringType, kind, name, genericArity, signatureFingerprint

    let private validateMemberDuplicates (context: DecodeContext) =
        let identities =
            HashSet<ReferenceTypeIdentity * ReferenceMemberKind * string * int * string>()

        for typeHandle in context.Metadata.TypeDefinitions do
            let declaringType = typeDefinitionIdentity context typeHandle
            let definition = context.Metadata.GetTypeDefinition(typeHandle)

            for methodHandle in definition.GetMethods() do
                visitRow context
                let methodDefinition = context.Metadata.GetMethodDefinition(methodHandle)

                let name =
                    getString
                        context
                        $"MethodDef:{MetadataTokens.GetRowNumber(methodHandle)}.Name"
                        methodDefinition.Name

                let key =
                    rawMemberIdentityKey
                        context
                        declaringType
                        ReferenceMemberKind.Method
                        name
                        (methodDefinition.GetGenericParameters()
                         |> Seq.length)
                        methodDefinition.Signature

                if not (identities.Add(key)) then
                    raiseDecode
                        ReferenceImportErrorKind.DuplicateIdentity
                        $"MethodDef:{MetadataTokens.GetRowNumber(methodHandle)}"
                        None
                        "The metadata contains a duplicate method identity."

            for fieldHandle in definition.GetFields() do
                visitRow context
                let fieldDefinition = context.Metadata.GetFieldDefinition(fieldHandle)

                let name =
                    getString
                        context
                        $"Field:{MetadataTokens.GetRowNumber(fieldHandle)}.Name"
                        fieldDefinition.Name

                let key =
                    rawMemberIdentityKey
                        context
                        declaringType
                        ReferenceMemberKind.Field
                        name
                        0
                        fieldDefinition.Signature

                if not (identities.Add(key)) then
                    raiseDecode
                        ReferenceImportErrorKind.DuplicateIdentity
                        $"Field:{MetadataTokens.GetRowNumber(fieldHandle)}"
                        None
                        "The metadata contains a duplicate field identity."

            for propertyHandle in definition.GetProperties() do
                visitRow context
                let propertyDefinition = context.Metadata.GetPropertyDefinition(propertyHandle)

                let name =
                    getString
                        context
                        $"Property:{MetadataTokens.GetRowNumber(propertyHandle)}.Name"
                        propertyDefinition.Name

                let key =
                    rawMemberIdentityKey
                        context
                        declaringType
                        ReferenceMemberKind.Property
                        name
                        0
                        propertyDefinition.Signature

                if not (identities.Add(key)) then
                    raiseDecode
                        ReferenceImportErrorKind.DuplicateIdentity
                        $"Property:{MetadataTokens.GetRowNumber(propertyHandle)}"
                        None
                        "The metadata contains a duplicate property identity."

            for eventHandle in definition.GetEvents() do
                let identity = eventDefinitionIdentity context eventHandle

                let eventTypeFingerprint =
                    identity.EventType
                    |> Option.get
                    |> sprintf "%A"
                    |> Encoding.UTF8.GetBytes
                    |> SHA256.HashData
                    |> Convert.ToHexString

                let key =
                    declaringType,
                    ReferenceMemberKind.Event,
                    identity.MetadataName,
                    0,
                    eventTypeFingerprint

                if not (identities.Add(key)) then
                    raiseDecode
                        ReferenceImportErrorKind.DuplicateIdentity
                        $"Event:{MetadataTokens.GetRowNumber(eventHandle)}"
                        None
                        "The metadata contains a duplicate event identity."

    let private indexEnumDefinitions
        (context: DecodeContext)
        (assembly: ReferenceAssemblyIdentity option)
        =
        match assembly with
        | None -> ()
        | Some identity ->
            let scope = assemblyScope identity

            for handle in context.Metadata.TypeDefinitions do
                let row = MetadataTokens.GetRowNumber(handle)
                let typeIdentity = typeDefinitionIdentity context handle
                let key = enumDefinitionKey scope typeIdentity

                let demand =
                    createDemand
                        context.Limits
                        context.EnumDefinitions
                        context.Snapshot
                        $"TypeDef:{row}.EnumUnderlyingType"
                        (fun demandContext ->
                            MetadataTokens.TypeDefinitionHandle(row)
                            |> tryEnumUnderlyingType demandContext
                        )

                context.EnumDefinitions.TryAdd(key, demand)
                |> ignore

    type private IndexedSnapshot = {
        Source: TargetReferenceSnapshot
        Value: ReferenceEcmaSnapshot
    }

    let private indexOne
        (limits: ReferenceImportLimits)
        (cancellationToken: CancellationToken)
        (enumDefinitions: Dictionary<EnumDefinitionKey, EnumDefinitionDemand>)
        (snapshot: TargetReferenceSnapshot)
        =
        let invalid kind location message =
            Error(failure snapshot kind location None None message)

        cancellationToken.ThrowIfCancellationRequested()
        let claimed = snapshot.ContentFingerprint

        if
            isNull claimed
            || claimed.Length
               <> 64
            || claimed
               |> Seq.exists (
                   Uri.IsHexDigit
                   >> not
               )
        then
            invalid
                ReferenceImportErrorKind.InvalidContentFingerprint
                None
                "The claimed content fingerprint must contain exactly 64 hexadecimal characters."
        else
            let computed =
                SHA256.HashData(snapshot.PeImage.AsSpan())
                |> Convert.ToHexString
                |> _.ToLowerInvariant()

            if
                not (String.Equals(claimed.ToLowerInvariant(), computed, StringComparison.Ordinal))
            then
                invalid
                    ReferenceImportErrorKind.ContentFingerprintMismatch
                    None
                    "The claimed content fingerprint does not match the PE image."
            elif int64 snapshot.PeImage.Length > limits.MaximumPeImageBytes then
                invalid
                    ReferenceImportErrorKind.ImageTooLarge
                    None
                    "The PE image exceeds the configured limit."
            else
                protect
                    snapshot
                    ReferenceImportErrorKind.InvalidPortableExecutable
                    "PE"
                    (fun () ->
                        use pe = new PEReader(snapshot.PeImage)

                        if not pe.HasMetadata then
                            raiseDecode
                                ReferenceImportErrorKind.MissingCliMetadata
                                "PE.CliHeader"
                                None
                                "The PE image has no CLI metadata."

                        let metadataBlock = pe.GetMetadata()

                        if int64 metadataBlock.Length > limits.MaximumMetadataBytes then
                            raiseDecode
                                ReferenceImportErrorKind.MetadataTooLarge
                                "MetadataRoot"
                                None
                                "The CLI metadata block exceeds the configured limit."

                        let metadata =
                            try
                                pe.GetMetadataReader()
                            with :? BadImageFormatException as exceptionValue ->
                                raiseDecode
                                    ReferenceImportErrorKind.MalformedCliMetadata
                                    "MetadataRoot"
                                    None
                                    exceptionValue.Message

                        let moduleDefinition = metadata.GetModuleDefinition()

                        let moduleName =
                            getStringValue limits metadata "Module.Name" moduleDefinition.Name

                        let context = {
                            Limits = limits
                            Snapshot = snapshot
                            Metadata = metadata
                            ModuleName = moduleName
                            EnumDefinitions = enumDefinitions
                            CancellationToken = cancellationToken
                            RowsVisited = 0
                        }

                        let rowCounts =
                            Enum.GetValues<TableIndex>()
                            |> Seq.distinct
                            |> Seq.map (fun table -> table, metadata.GetTableRowCount(table))
                            |> List.ofSeq

                        match
                            rowCounts
                            |> List.tryFind (fun (_, count) -> count > limits.MaximumRowsPerTable)
                        with
                        | Some(table, _) ->
                            raiseDecode
                                ReferenceImportErrorKind.TableRowLimitExceeded
                                (string table)
                                None
                                "A metadata table exceeds the configured row limit."
                        | None -> ()

                        let totalRows =
                            rowCounts
                            |> List.fold
                                (fun total (_, count) ->
                                    total
                                    + int64 count
                                )
                                0L

                        if totalRows > int64 limits.MaximumRowsAcrossTables then
                            raiseDecode
                                ReferenceImportErrorKind.TotalRowLimitExceeded
                                "MetadataTables"
                                None
                                "The metadata tables exceed the configured total row limit."

                        let typeIdentities = HashSet<ReferenceTypeIdentity>()

                        let typeDefinitions =
                            metadata.TypeDefinitions
                            |> Seq.map (fun handle ->
                                let row = MetadataTokens.GetRowNumber(handle)
                                let definition = metadata.GetTypeDefinition(handle)
                                let identity = typeDefinitionIdentity context handle

                                if not (typeIdentities.Add(identity)) then
                                    raiseDecode
                                        ReferenceImportErrorKind.DuplicateIdentity
                                        $"TypeDef:{row}"
                                        None
                                        "The metadata contains a duplicate type identity."

                                {
                                    Identity = identity
                                    Attributes = int definition.Attributes
                                    Accessibility = typeAccessibility definition.Attributes
                                    EnclosingAccessibilities =
                                        enclosingAccessibilities context definition
                                    Semantics =
                                        createDemand
                                            limits
                                            enumDefinitions
                                            snapshot
                                            $"TypeDef:{row}"
                                            (fun demandContext ->
                                                MetadataTokens.TypeDefinitionHandle(row)
                                                |> decodeTypeSemantics demandContext
                                            )
                                }
                            )
                            |> List.ofSeq

                        let indexedAssembly =
                            if metadata.IsAssembly then
                                Some(assemblyIdentity context)
                            else
                                None

                        indexEnumDefinitions context indexedAssembly

                        validateMemberDuplicates context

                        let exportedIdentities = HashSet<ReferenceTypeIdentity>()

                        let exportedTypes =
                            metadata.ExportedTypes
                            |> Seq.map (fun handle ->
                                let row = MetadataTokens.GetRowNumber(handle)
                                let definition = metadata.GetExportedType(handle)
                                let identity = exportedTypeIdentity context handle

                                if not (exportedIdentities.Add(identity)) then
                                    raiseDecode
                                        ReferenceImportErrorKind.DuplicateIdentity
                                        $"ExportedType:{row}"
                                        None
                                        "The metadata contains a duplicate exported-type identity."

                                {
                                    SourceIdentity = identity
                                    Attributes = int definition.Attributes
                                    Target = exportedTarget context definition.Implementation
                                    Resolution =
                                        ReferenceSemanticDemand(fun _ ->
                                            Error(
                                                failure
                                                    snapshot
                                                    ReferenceImportErrorKind.Unsupported
                                                    (Some $"ExportedType:{row}")
                                                    None
                                                    None
                                                    ("The forwarder resolution has not "
                                                     + "been attached.")
                                            )
                                        )
                                }
                            )
                            |> List.ofSeq

                        let files =
                            metadata.AssemblyFiles
                            |> Seq.map (fun handle ->
                                visitRow context
                                let file = metadata.GetAssemblyFile(handle)
                                let row = MetadataTokens.GetRowNumber(handle)

                                {
                                    Name = getString context $"File:{row}.Name" file.Name
                                    ContainsMetadata = file.ContainsMetadata
                                    HashValue =
                                        getBlob context $"File:{row}.HashValue" file.HashValue
                                }
                            )
                            |> List.ofSeq

                        let manifestResources =
                            metadata.ManifestResources
                            |> Seq.map (fun handle ->
                                visitRow context
                                let resource = metadata.GetManifestResource(handle)
                                let row = MetadataTokens.GetRowNumber(handle)

                                let implementation =
                                    resourceImplementation context resource.Implementation

                                let length, fingerprint =
                                    match implementation with
                                    | ReferenceResourceImplementation.Embedded ->
                                        embeddedResource context pe row resource
                                    | _ -> None, None

                                {
                                    Name =
                                        getString
                                            context
                                            $"ManifestResource:{row}.Name"
                                            resource.Name
                                    Attributes = int resource.Attributes
                                    Implementation = implementation
                                    EmbeddedContentLength = length
                                    EmbeddedContentFingerprint = fingerprint
                                }
                            )
                            |> List.ofSeq

                        {
                            Source = snapshot
                            Value = {
                                StableId = snapshot.StableId.Value
                                LogicalPath = snapshot.LogicalPath
                                ContentFingerprint = computed
                                Assembly = indexedAssembly
                                Module = {
                                    Name = moduleName
                                    Generation = moduleDefinition.Generation
                                    ModuleVersionId = metadata.GetGuid(moduleDefinition.Mvid)
                                    GenerationId =
                                        optionalGuid metadata moduleDefinition.GenerationId
                                    BaseGenerationId =
                                        optionalGuid metadata moduleDefinition.BaseGenerationId
                                }
                                AssemblyReferences =
                                    metadata.AssemblyReferences
                                    |> Seq.map (assemblyReference context)
                                    |> List.ofSeq
                                ModuleReferences =
                                    seq {
                                        for row in
                                            1 .. metadata.GetTableRowCount(TableIndex.ModuleRef) do
                                            yield MetadataTokens.ModuleReferenceHandle(row)
                                    }
                                    |> Seq.map (fun handle ->
                                        visitRow context
                                        let definition = metadata.GetModuleReference(handle)
                                        let row = MetadataTokens.GetRowNumber(handle)

                                        {
                                            Name =
                                                getString
                                                    context
                                                    $"ModuleRef:{row}.Name"
                                                    definition.Name
                                        }
                                    )
                                    |> List.ofSeq
                                Files = files
                                TypeDefinitions = typeDefinitions
                                TypeReferences =
                                    metadata.TypeReferences
                                    |> Seq.map (typeReferenceIdentity context)
                                    |> List.ofSeq
                                ExportedTypes = exportedTypes
                                ManifestResources = manifestResources
                                Semantics =
                                    createDemand
                                        limits
                                        enumDefinitions
                                        snapshot
                                        "EcmaSemantics"
                                        decodeEcmaSemantics
                            }
                        }
                    )

    let private forwardedTypeIdentityEqual
        (left: ReferenceTypeIdentity)
        (right: ReferenceTypeIdentity)
        =
        left.Namespace = right.Namespace
        && left.EnclosingTypes = right.EnclosingTypes
        && left.MetadataName = right.MetadataName
        && left.GenericArity = right.GenericArity

    let private normalizedAssemblyFlags (flags: int) =
        flags
        &&& 0x0000FF00

    let private assemblyMatches
        (definition: ReferenceAssemblyIdentity)
        (requested: ReferenceAssemblyIdentity)
        =
        definition.Name = requested.Name
        && definition.Version = requested.Version
        && definition.Culture = requested.Culture
        && definition.PublicKeyToken = requested.PublicKeyToken
        && normalizedAssemblyFlags definition.Flags = normalizedAssemblyFlags requested.Flags

    let private fileHash
        (cancellationToken: CancellationToken)
        (algorithm: uint32)
        (content: ImmutableArray<byte>)
        =
        cancellationToken.ThrowIfCancellationRequested()

        let algorithmName =
            match algorithm with
            | 0u -> None
            | 0x8003u -> Some HashAlgorithmName.MD5
            | 0x8004u -> Some HashAlgorithmName.SHA1
            | 0x800Cu -> Some HashAlgorithmName.SHA256
            | value ->
                raiseDecode
                    ReferenceImportErrorKind.IncompatibleForwarderTarget
                    "File.HashAlgorithm"
                    None
                    $"The assembly hash algorithm '{value}' is not supported."

        match algorithmName with
        | None -> []
        | Some name ->
            use hash = IncrementalHash.CreateHash(name)
            let mutable offset = 0

            while offset < content.Length do
                cancellationToken.ThrowIfCancellationRequested()

                let count =
                    min
                        (64 * 1024)
                        (content.Length
                         - offset)

                hash.AppendData(content.AsSpan().Slice(offset, count))

                offset <-
                    offset
                    + count

            cancellationToken.ThrowIfCancellationRequested()

            hash.GetHashAndReset()
            |> List.ofArray

    let private forwarderFailure
        (snapshot: IndexedSnapshot)
        (kind: ReferenceImportErrorKind)
        (related: string option)
        (message: string)
        =
        failure snapshot.Source kind (Some "ExportedType") None related message

    let private attachForwarders
        (limits: ReferenceImportLimits)
        (cancellationToken: CancellationToken)
        (snapshots: IndexedSnapshot list)
        =
        cancellationToken.ThrowIfCancellationRequested()
        let assemblyIdentities = Dictionary<ReferenceAssemblyIdentity, IndexedSnapshot>()

        let mutable duplicateAssembly: ReferenceImportError option = None

        for snapshot in snapshots do
            cancellationToken.ThrowIfCancellationRequested()

            match snapshot.Value.Assembly with
            | Some identity when not (assemblyIdentities.TryAdd(identity, snapshot)) ->
                if duplicateAssembly.IsNone then
                    duplicateAssembly <-
                        Some(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.DuplicateIdentity
                                (Some(sprintf "%A" identity))
                                "The supplied snapshots contain a duplicate assembly identity."
                        )
            | Some identity -> assemblyIdentities.[identity] <- snapshot
            | None -> ()

        let rec resolve
            (cancellationToken: CancellationToken)
            (path: (ReferenceAssemblyIdentity option * ReferenceTypeIdentity) list)
            (snapshot: IndexedSnapshot)
            (exportedType: ReferenceExportedType)
            =
            cancellationToken.ThrowIfCancellationRequested()
            let visit = snapshot.Value.Assembly, exportedType.SourceIdentity

            if
                path
                |> List.exists (fun item -> item = visit)
            then
                Error(
                    forwarderFailure
                        snapshot
                        ReferenceImportErrorKind.ForwarderCycle
                        (Some(
                            sprintf
                                "%A"
                                (List.rev (
                                    visit
                                    :: path
                                ))
                        ))
                        "The exported-type forwarding path contains a cycle."
                )
            elif
                path.Length
                >= limits.MaximumResolutionDepth
            then
                Error(
                    forwarderFailure
                        snapshot
                        ReferenceImportErrorKind.ResolutionDepthExceeded
                        (Some(
                            sprintf
                                "%A"
                                (List.rev (
                                    visit
                                    :: path
                                ))
                        ))
                        "The exported-type forwarding path exceeds the configured depth limit."
                )
            else
                let nextPath =
                    visit
                    :: path

                let step = {
                    SourceAssembly = snapshot.Value.Assembly
                    SourceType = exportedType.SourceIdentity
                    Target = exportedType.Target
                }

                let destination (target: IndexedSnapshot) =
                    cancellationToken.ThrowIfCancellationRequested()

                    match
                        target.Value.TypeDefinitions
                        |> List.filter (fun definition ->
                            forwardedTypeIdentityEqual
                                definition.Identity
                                exportedType.SourceIdentity
                        )
                    with
                    | [ definition ] ->
                        Ok {
                            Steps = [ step ]
                            DestinationStableId = target.Value.StableId
                            DestinationType = definition.Identity
                        }
                    | [] ->
                        match
                            target.Value.ExportedTypes
                            |> List.filter (fun candidate ->
                                forwardedTypeIdentityEqual
                                    candidate.SourceIdentity
                                    exportedType.SourceIdentity
                            )
                        with
                        | [ forwarded ] ->
                            resolve cancellationToken nextPath target forwarded
                            |> Result.map (fun result -> {
                                result with
                                    Steps =
                                        step
                                        :: result.Steps
                            })
                        | [] ->
                            Error(
                                forwarderFailure
                                    snapshot
                                    ReferenceImportErrorKind.IncompatibleForwarderTarget
                                    (Some(sprintf "%A" exportedType.SourceIdentity))
                                    ("The forwarder target does not define or forward "
                                     + "the requested type.")
                            )
                        | _ ->
                            Error(
                                forwarderFailure
                                    snapshot
                                    ReferenceImportErrorKind.DuplicateIdentity
                                    (Some(sprintf "%A" exportedType.SourceIdentity))
                                    ("The forwarder target contains duplicate "
                                     + "exported-type identities.")
                            )
                    | _ ->
                        Error(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.DuplicateIdentity
                                (Some(sprintf "%A" exportedType.SourceIdentity))
                                "The forwarder target contains duplicate type identities."
                        )

                match exportedType.Target with
                | ReferenceForwarderTarget.Assembly identity ->
                    match
                        snapshots
                        |> List.filter (fun candidate ->
                            candidate.Value.Assembly
                            |> Option.exists (fun definition ->
                                assemblyMatches definition identity
                            )
                        )
                    with
                    | [ target ] -> destination target
                    | [] ->
                        Error(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.MissingForwarderTarget
                                (Some(sprintf "%A" identity))
                                "The exported type refers to an unavailable assembly."
                        )
                    | _ ->
                        Error(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.AmbiguousForwarderTarget
                                (Some(sprintf "%A" identity))
                                "The exported-type assembly destination is ambiguous."
                        )
                | ReferenceForwarderTarget.File file ->
                    if not file.ContainsMetadata then
                        Error(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.IncompatibleForwarderTarget
                                (Some file.Name)
                                "The exported-type file is marked as containing no metadata."
                        )
                    else
                        match
                            snapshots
                            |> List.filter (fun candidate ->
                                candidate.Value.Module.Name = file.Name
                            )
                        with
                        | [ target ] ->
                            let algorithm =
                                snapshot.Value.Assembly
                                |> Option.map _.HashAlgorithm
                                |> Option.defaultValue 0u

                            let actualHash =
                                fileHash cancellationToken algorithm target.Source.PeImage

                            if
                                not file.HashValue.IsEmpty
                                && actualHash
                                   <> file.HashValue
                            then
                                Error(
                                    forwarderFailure
                                        snapshot
                                        ReferenceImportErrorKind.IncompatibleForwarderTarget
                                        (Some file.Name)
                                        ("The exported-type file hash does not match "
                                         + "the supplied module.")
                                )
                            else
                                destination target
                        | [] ->
                            Error(
                                forwarderFailure
                                    snapshot
                                    ReferenceImportErrorKind.MissingForwarderTarget
                                    (Some file.Name)
                                    "The exported type refers to an unavailable file."
                            )
                        | _ ->
                            Error(
                                forwarderFailure
                                    snapshot
                                    ReferenceImportErrorKind.AmbiguousForwarderTarget
                                    (Some file.Name)
                                    "The exported-type file destination is ambiguous."
                            )
                | ReferenceForwarderTarget.ExportedType parentIdentity ->
                    match
                        snapshot.Value.ExportedTypes
                        |> List.filter (fun candidate ->
                            forwardedTypeIdentityEqual candidate.SourceIdentity parentIdentity
                        )
                    with
                    | [ parent ] ->
                        resolve cancellationToken nextPath snapshot parent
                        |> Result.bind (fun parentResolution ->
                            let target =
                                snapshots
                                |> List.find (fun candidate ->
                                    candidate.Value.StableId = parentResolution.DestinationStableId
                                )

                            match
                                target.Value.TypeDefinitions
                                |> List.filter (fun definition ->
                                    forwardedTypeIdentityEqual
                                        definition.Identity
                                        exportedType.SourceIdentity
                                )
                            with
                            | [ definition ] ->
                                Ok {
                                    Steps =
                                        step
                                        :: parentResolution.Steps
                                    DestinationStableId = target.Value.StableId
                                    DestinationType = definition.Identity
                                }
                            | [] ->
                                Error(
                                    forwarderFailure
                                        snapshot
                                        ReferenceImportErrorKind.IncompatibleForwarderTarget
                                        (Some(sprintf "%A" exportedType.SourceIdentity))
                                        ("The nested forwarder target does not define "
                                         + "the requested type.")
                                )
                            | _ ->
                                Error(
                                    forwarderFailure
                                        snapshot
                                        ReferenceImportErrorKind.DuplicateIdentity
                                        (Some(sprintf "%A" exportedType.SourceIdentity))
                                        ("The nested forwarder target contains duplicate "
                                         + "type identities.")
                                )
                        )
                    | [] ->
                        Error(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.InvalidForwarderParent
                                (Some(sprintf "%A" parentIdentity))
                                "The nested exported type has no exported parent."
                        )
                    | _ ->
                        Error(
                            forwarderFailure
                                snapshot
                                ReferenceImportErrorKind.DuplicateIdentity
                                (Some(sprintf "%A" parentIdentity))
                                "The nested exported type has duplicate exported parents."
                        )

        match duplicateAssembly with
        | Some importError -> Error [ importError ]
        | None ->
            let attached =
                snapshots
                |> List.map (fun snapshot ->
                    cancellationToken.ThrowIfCancellationRequested()

                    let exportedTypes =
                        snapshot.Value.ExportedTypes
                        |> List.map (fun exportedType ->
                            cancellationToken.ThrowIfCancellationRequested()

                            {
                                exportedType with
                                    Resolution =
                                        ReferenceSemanticDemand(fun cancellationToken ->
                                            cancellationToken.ThrowIfCancellationRequested()

                                            protect
                                                snapshot.Source
                                                ReferenceImportErrorKind.IncompatibleForwarderTarget
                                                "ExportedType.Resolution"
                                                (fun () ->
                                                    resolve
                                                        cancellationToken
                                                        []
                                                        snapshot
                                                        exportedType
                                                )
                                            |> Result.bind id
                                        )
                            }
                        )

                    {
                        snapshot.Value with
                            ExportedTypes = exportedTypes
                    }
                )

            Ok attached

    let import (request: ReferenceImportRequest) =
        let enumDefinitions = Dictionary<EnumDefinitionKey, EnumDefinitionDemand>()

        let rec loop imported remaining =
            request.CancellationToken.ThrowIfCancellationRequested()

            match remaining with
            | [] ->
                imported
                |> List.rev
                |> attachForwarders request.Limits request.CancellationToken
            | snapshot :: tail ->
                match
                    indexOne request.Limits request.CancellationToken enumDefinitions snapshot
                with
                | Ok indexed ->
                    loop
                        (indexed
                         :: imported)
                        tail
                | Error importError -> Error [ importError ]

        loop [] request.Snapshots
