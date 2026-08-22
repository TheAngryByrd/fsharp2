using System.Collections.Immutable;
using System.Reflection.Metadata;

namespace FSharp2.Conformance;

internal sealed class MetadataTypeNameProvider : ISignatureTypeProvider<string, object?>
{
    public string GetArrayType(string elementType, ArrayShape shape) =>
        $"{elementType}[rank:{shape.Rank};sizes:{string.Join(',', shape.Sizes)};lower:{string.Join(',', shape.LowerBounds)}]";

    public string GetByReferenceType(string elementType) => $"{elementType}&";

    public string GetFunctionPointerType(MethodSignature<string> signature) =>
        $"fnptr:{Format(signature)}";

    public string GetGenericInstantiation(string genericType, ImmutableArray<string> typeArguments) =>
        $"{genericType}<{string.Join(',', typeArguments)}>";

    public string GetGenericMethodParameter(object? genericContext, int index) => $"!!{index}";

    public string GetGenericTypeParameter(object? genericContext, int index) => $"!{index}";

    public string GetModifiedType(string modifier, string unmodifiedType, bool isRequired) =>
        $"{unmodifiedType} mod{(isRequired ? "req" : "opt")}({modifier})";

    public string GetPinnedType(string elementType) => $"{elementType} pinned";

    public string GetPointerType(string elementType) => $"{elementType}*";

    public string GetPrimitiveType(PrimitiveTypeCode typeCode) => typeCode.ToString();

    public string GetSZArrayType(string elementType) => $"{elementType}[]";

    public string GetTypeFromDefinition(
        MetadataReader reader,
        TypeDefinitionHandle handle,
        byte rawTypeKind) => MetadataIdentity.Type(reader, handle);

    public string GetTypeFromReference(
        MetadataReader reader,
        TypeReferenceHandle handle,
        byte rawTypeKind) => MetadataIdentity.Type(reader, handle);

    public string GetTypeFromSpecification(
        MetadataReader reader,
        object? genericContext,
        TypeSpecificationHandle handle,
        byte rawTypeKind) => reader.GetTypeSpecification(handle).DecodeSignature(this, genericContext);

    public static string Format(MethodSignature<string> signature) =>
        $"{signature.Header.RawValue}:{signature.GenericParameterCount}:{signature.RequiredParameterCount}:({string.Join(',', signature.ParameterTypes)})->{signature.ReturnType}";
}

internal static class MetadataIdentity
{
    public static string Method(MetadataReader reader, MethodDefinitionHandle handle)
    {
        if (handle.IsNil)
        {
            return string.Empty;
        }
        var method = reader.GetMethodDefinition(handle);
        var declaringType = MetadataIdentity.Type(reader, method.GetDeclaringType());
        var signature = MetadataTypeNameProvider.Format(
            method.DecodeSignature(new MetadataTypeNameProvider(), null));
        return $"{declaringType}.{reader.GetString(method.Name)}:{signature}";
    }

    public static string Type(MetadataReader reader, EntityHandle handle)
    {
        if (handle.IsNil)
        {
            return string.Empty;
        }
        return handle.Kind switch
        {
            HandleKind.TypeDefinition => Type(reader, (TypeDefinitionHandle)handle),
            HandleKind.TypeReference => Type(reader, (TypeReferenceHandle)handle),
            HandleKind.TypeSpecification => reader.GetTypeSpecification((TypeSpecificationHandle)handle)
                .DecodeSignature(new MetadataTypeNameProvider(), null),
            _ => throw new BadImageFormatException($"Handle kind '{handle.Kind}' does not identify a metadata type."),
        };
    }

    public static string Type(MetadataReader reader, TypeDefinitionHandle handle)
    {
        var definition = reader.GetTypeDefinition(handle);
        var name = reader.GetString(definition.Name);
        var declaringType = definition.GetDeclaringType();
        if (!declaringType.IsNil)
        {
            return $"{Type(reader, declaringType)}+{name}";
        }
        var namespaceName = reader.GetString(definition.Namespace);
        return namespaceName.Length == 0 ? name : $"{namespaceName}.{name}";
    }

    public static string Type(MetadataReader reader, TypeReferenceHandle handle)
    {
        var reference = reader.GetTypeReference(handle);
        var name = reader.GetString(reference.Name);
        if (reference.ResolutionScope.Kind == HandleKind.TypeReference)
        {
            return $"{Type(reader, (TypeReferenceHandle)reference.ResolutionScope)}+{name}";
        }
        var namespaceName = reader.GetString(reference.Namespace);
        var typeName = namespaceName.Length == 0 ? name : $"{namespaceName}.{name}";
        return reference.ResolutionScope.Kind == HandleKind.AssemblyReference
            ? $"[{reader.GetString(reader.GetAssemblyReference((AssemblyReferenceHandle)reference.ResolutionScope).Name)}]{typeName}"
            : typeName;
    }
}
