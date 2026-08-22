using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class ManagedMetadataComparator
{
    public static ComparisonResult Compare(
        ImmutableArray<byte> expected,
        ImmutableArray<byte> actual)
    {
        try
        {
            var expectedIdentities = ReadPublicApi(expected);
            var actualIdentities = ReadPublicApi(actual);
            var expectedCanonical = CanonicalJson.Canonicalize(
                JsonSerializer.SerializeToElement(expectedIdentities));
            var actualCanonical = CanonicalJson.Canonicalize(
                JsonSerializer.SerializeToElement(actualIdentities));
            var passed = expectedIdentities.SequenceEqual(actualIdentities, StringComparer.Ordinal);
            var difference = passed
                ? null
                : ComparisonSupport.TextDifference(
                    expectedIdentities.Except(actualIdentities, StringComparer.Ordinal),
                    actualIdentities.Except(expectedIdentities, StringComparer.Ordinal));
            return ComparisonSupport.Create(
                "managed-metadata-semantic",
                "canonical-semantic",
                "metadata-v1",
                passed,
                difference,
                expected.AsSpan(),
                actual.AsSpan(),
                expectedCanonical,
                actualCanonical);
        }
        catch (BadImageFormatException exception)
        {
            var expectedHash = Hashing.Sha256(expected.AsSpan());
            var actualHash = Hashing.Sha256(actual.AsSpan());
            return ComparisonSupport.Create(
                "managed-metadata-semantic",
                "canonical-semantic",
                "metadata-v1",
                false,
                $"Managed metadata could not be read: {exception.Message}",
                expected.AsSpan(),
                actual.AsSpan(),
                Encoding.UTF8.GetBytes(expectedHash),
                Encoding.UTF8.GetBytes(actualHash));
        }
    }

    internal static ImmutableArray<string> ReadPublicApi(ImmutableArray<byte> image)
    {
        using var peReader = new PEReader(image);
        if (!peReader.HasMetadata)
        {
            throw new BadImageFormatException("The PE image does not contain managed metadata.");
        }

        var metadata = peReader.GetMetadataReader();
        var provider = new MetadataTypeNameProvider();
        var values = ImmutableArray.CreateBuilder<string>();
        foreach (var handle in metadata.TypeDefinitions.Where(handle => IsPublic(metadata, handle)))
        {
            var definition = metadata.GetTypeDefinition(handle);
            var typeIdentity = MetadataIdentity.Type(metadata, handle);
            values.Add($"type|{typeIdentity}|{definition.Attributes}|base:{MetadataIdentity.Type(metadata, definition.BaseType)}");
            AddGenericParameters(values, metadata, typeIdentity, definition.GetGenericParameters());
            AddCustomAttributes(values, metadata, $"type:{typeIdentity}", definition.GetCustomAttributes());
            foreach (var implementationHandle in definition.GetInterfaceImplementations())
            {
                var implementation = metadata.GetInterfaceImplementation(implementationHandle);
                values.Add($"interface|{typeIdentity}|{MetadataIdentity.Type(metadata, implementation.Interface)}");
                AddCustomAttributes(
                    values,
                    metadata,
                    $"interface:{typeIdentity}:{MetadataIdentity.Type(metadata, implementation.Interface)}",
                    implementation.GetCustomAttributes());
            }
            foreach (var fieldHandle in definition.GetFields())
            {
                var field = metadata.GetFieldDefinition(fieldHandle);
                if (!IsPublicApi(field.Attributes))
                {
                    continue;
                }
                var fieldIdentity = $"{typeIdentity}.{metadata.GetString(field.Name)}";
                values.Add($"field|{fieldIdentity}|{field.Attributes}|{field.DecodeSignature(provider, null)}|{Constant(metadata, field.GetDefaultValue())}|marshal:{Blob(metadata, field.GetMarshallingDescriptor())}");
                AddCustomAttributes(values, metadata, $"field:{fieldIdentity}", field.GetCustomAttributes());
            }
            foreach (var methodHandle in definition.GetMethods())
            {
                var method = metadata.GetMethodDefinition(methodHandle);
                if (!IsPublicApi(method.Attributes))
                {
                    continue;
                }
                var methodSignature = MetadataTypeNameProvider.Format(method.DecodeSignature(provider, null));
                var methodIdentity = $"{typeIdentity}.{metadata.GetString(method.Name)}:{methodSignature}";
                values.Add($"method|{methodIdentity}|{method.Attributes}|{method.ImplAttributes}");
                AddGenericParameters(values, metadata, methodIdentity, method.GetGenericParameters());
                AddCustomAttributes(values, metadata, $"method:{methodIdentity}", method.GetCustomAttributes());
                foreach (var parameterHandle in method.GetParameters())
                {
                    var parameter = metadata.GetParameter(parameterHandle);
                    var parameterIdentity = $"{methodIdentity}:{parameter.SequenceNumber}:{metadata.GetString(parameter.Name)}";
                    values.Add($"parameter|{parameterIdentity}|{parameter.Attributes}|{Constant(metadata, parameter.GetDefaultValue())}|marshal:{Blob(metadata, parameter.GetMarshallingDescriptor())}");
                    AddCustomAttributes(values, metadata, $"parameter:{parameterIdentity}", parameter.GetCustomAttributes());
                }
            }
            foreach (var propertyHandle in definition.GetProperties())
            {
                var property = metadata.GetPropertyDefinition(propertyHandle);
                var accessors = property.GetAccessors();
                if (!IsPublicApi(metadata, accessors.Getter)
                    && !IsPublicApi(metadata, accessors.Setter)
                    && !accessors.Others.Any(accessor => IsPublicApi(metadata, accessor)))
                {
                    continue;
                }
                var propertyIdentity = $"{typeIdentity}.{metadata.GetString(property.Name)}:{MetadataTypeNameProvider.Format(property.DecodeSignature(provider, null))}";
                values.Add($"property|{propertyIdentity}|{property.Attributes}|{Constant(metadata, property.GetDefaultValue())}");
                AddCustomAttributes(values, metadata, $"property:{propertyIdentity}", property.GetCustomAttributes());
            }
            foreach (var eventHandle in definition.GetEvents())
            {
                var eventDefinition = metadata.GetEventDefinition(eventHandle);
                var accessors = eventDefinition.GetAccessors();
                if (!IsPublicApi(metadata, accessors.Adder)
                    && !IsPublicApi(metadata, accessors.Remover)
                    && !IsPublicApi(metadata, accessors.Raiser)
                    && !accessors.Others.Any(accessor => IsPublicApi(metadata, accessor)))
                {
                    continue;
                }
                var eventIdentity = $"{typeIdentity}.{metadata.GetString(eventDefinition.Name)}:{MetadataIdentity.Type(metadata, eventDefinition.Type)}";
                values.Add($"event|{eventIdentity}|{eventDefinition.Attributes}");
                AddCustomAttributes(values, metadata, $"event:{eventIdentity}", eventDefinition.GetCustomAttributes());
            }
        }
        return [.. values.OrderBy(static value => value, StringComparer.Ordinal)];
    }

    internal static ImmutableArray<string> ReadPublicTypeIdentities(ImmutableArray<byte> image)
    {
        using var peReader = new PEReader(image);
        if (!peReader.HasMetadata)
        {
            throw new BadImageFormatException("The PE image does not contain managed metadata.");
        }

        var metadata = peReader.GetMetadataReader();
        return
        [
            .. metadata.TypeDefinitions
                .Where(handle => IsPublic(metadata, handle))
                .Select(handle => MetadataIdentity.Type(metadata, handle))
                .OrderBy(static value => value, StringComparer.Ordinal),
        ];
    }

    private static bool IsPublic(MetadataReader metadata, TypeDefinitionHandle handle)
    {
        var definition = metadata.GetTypeDefinition(handle);
        var visibility = definition.Attributes & TypeAttributes.VisibilityMask;
        if (visibility == TypeAttributes.Public)
        {
            return true;
        }
        if (visibility is not (
            TypeAttributes.NestedPublic
            or TypeAttributes.NestedFamily
            or TypeAttributes.NestedFamORAssem))
        {
            return false;
        }
        var declaringType = definition.GetDeclaringType();
        return !declaringType.IsNil && IsPublic(metadata, declaringType);
    }

    private static bool IsPublicApi(FieldAttributes attributes)
    {
        var access = attributes & FieldAttributes.FieldAccessMask;
        return access is FieldAttributes.Public or FieldAttributes.Family or FieldAttributes.FamORAssem;
    }

    private static bool IsPublicApi(MethodAttributes attributes)
    {
        var access = attributes & MethodAttributes.MemberAccessMask;
        return access is MethodAttributes.Public or MethodAttributes.Family or MethodAttributes.FamORAssem;
    }

    private static bool IsPublicApi(MetadataReader metadata, MethodDefinitionHandle handle) =>
        !handle.IsNil && IsPublicApi(metadata.GetMethodDefinition(handle).Attributes);

    private static void AddGenericParameters(
        ImmutableArray<string>.Builder values,
        MetadataReader metadata,
        string owner,
        GenericParameterHandleCollection handles)
    {
        foreach (var handle in handles)
        {
            var parameter = metadata.GetGenericParameter(handle);
            var identity = $"{owner}:{parameter.Index}:{metadata.GetString(parameter.Name)}";
            values.Add($"generic|{identity}|{parameter.Attributes}");
            foreach (var constraintHandle in parameter.GetConstraints())
            {
                var constraint = metadata.GetGenericParameterConstraint(constraintHandle);
                values.Add($"constraint|{identity}|{MetadataIdentity.Type(metadata, constraint.Type)}");
                AddCustomAttributes(values, metadata, $"constraint:{identity}", constraint.GetCustomAttributes());
            }
            AddCustomAttributes(values, metadata, $"generic:{identity}", parameter.GetCustomAttributes());
        }
    }

    private static void AddCustomAttributes(
        ImmutableArray<string>.Builder values,
        MetadataReader metadata,
        string owner,
        CustomAttributeHandleCollection handles)
    {
        foreach (var handle in handles)
        {
            var attribute = metadata.GetCustomAttribute(handle);
            values.Add($"attribute|{owner}|{AttributeConstructor(metadata, attribute.Constructor)}|{Blob(metadata, attribute.Value)}");
        }
    }

    private static string AttributeConstructor(MetadataReader metadata, EntityHandle handle) => handle.Kind switch
    {
        HandleKind.MethodDefinition => MetadataIdentity.Type(
            metadata,
            metadata.GetMethodDefinition((MethodDefinitionHandle)handle).GetDeclaringType()),
        HandleKind.MemberReference => MetadataIdentity.Type(
            metadata,
            metadata.GetMemberReference((MemberReferenceHandle)handle).Parent),
        _ => handle.Kind.ToString(),
    };

    private static string Constant(MetadataReader metadata, ConstantHandle handle)
    {
        if (handle.IsNil)
        {
            return string.Empty;
        }
        var constant = metadata.GetConstant(handle);
        return $"{constant.TypeCode}:{Blob(metadata, constant.Value)}";
    }

    private static string Blob(MetadataReader metadata, BlobHandle handle) => handle.IsNil
        ? string.Empty
        : Convert.ToHexString(metadata.GetBlobBytes(handle)).ToLowerInvariant();
}
