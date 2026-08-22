using System.Collections.Immutable;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;
using System.Text;
using System.Text.Json;

namespace FSharp2.Conformance;

public static class PortablePdbComparator
{
    public static ComparisonResult Compare(
        ImmutableArray<byte> expected,
        ImmutableArray<byte> actual,
        JsonElement rule) => CompareCore(
            expected,
            null,
            expected.ToArray(),
            actual,
            null,
            actual.ToArray(),
            rule);

    public static ComparisonResult Compare(
        ImmutableArray<byte> expectedPe,
        ImmutableArray<byte> expectedPdb,
        ImmutableArray<byte> actualPe,
        ImmutableArray<byte> actualPdb,
        JsonElement rule)
    {
        using var expectedPeReader = new PEReader(expectedPe);
        using var actualPeReader = new PEReader(actualPe);
        if (!expectedPeReader.HasMetadata || !actualPeReader.HasMetadata)
        {
            var expectedEvidence = EvidenceBytes(expectedPe, expectedPdb);
            var actualEvidence = EvidenceBytes(actualPe, actualPdb);
            return ComparisonSupport.Create(
                "portable-pdb-semantic",
                "canonical-semantic",
                ComparisonSupport.RuleId(rule, "portable-pdb-v1"),
                false,
                "A PE image does not contain managed metadata.",
                expectedEvidence,
                actualEvidence,
                Encoding.UTF8.GetBytes(Hashing.Sha256(expectedEvidence)),
                Encoding.UTF8.GetBytes(Hashing.Sha256(actualEvidence)));
        }
        return CompareCore(
            expectedPdb,
            expectedPeReader.GetMetadataReader(),
            EvidenceBytes(expectedPe, expectedPdb),
            actualPdb,
            actualPeReader.GetMetadataReader(),
            EvidenceBytes(actualPe, actualPdb),
            rule);
    }

    private static ComparisonResult CompareCore(
        ImmutableArray<byte> expected,
        MetadataReader? expectedPe,
        byte[] expectedRaw,
        ImmutableArray<byte> actual,
        MetadataReader? actualPe,
        byte[] actualRaw,
        JsonElement rule)
    {
        try
        {
            var expectedValues = ReadSemanticValues(expected, expectedPe);
            var actualValues = ReadSemanticValues(actual, actualPe);
            var expectedCanonical = CanonicalJson.Canonicalize(
                JsonSerializer.SerializeToElement(expectedValues));
            var actualCanonical = CanonicalJson.Canonicalize(
                JsonSerializer.SerializeToElement(actualValues));
            var passed = expectedValues.SequenceEqual(actualValues, StringComparer.Ordinal);
            return ComparisonSupport.Create(
                "portable-pdb-semantic",
                "canonical-semantic",
                ComparisonSupport.RuleId(rule, "portable-pdb-v1"),
                passed,
                passed
                    ? null
                    : ComparisonSupport.TextDifference(
                        expectedValues.Except(actualValues, StringComparer.Ordinal),
                        actualValues.Except(expectedValues, StringComparer.Ordinal)),
                expectedRaw,
                actualRaw,
                expectedCanonical,
                actualCanonical);
        }
        catch (BadImageFormatException exception)
        {
            return ComparisonSupport.Create(
                "portable-pdb-semantic",
                "canonical-semantic",
                ComparisonSupport.RuleId(rule, "portable-pdb-v1"),
                false,
                $"Portable PDB metadata could not be read: {exception.Message}",
                expectedRaw,
                actualRaw,
                Encoding.UTF8.GetBytes(Hashing.Sha256(expectedRaw)),
                Encoding.UTF8.GetBytes(Hashing.Sha256(actualRaw)));
        }
    }

    private static byte[] EvidenceBytes(
        ImmutableArray<byte> pe,
        ImmutableArray<byte> pdb) =>
        CanonicalJson.Canonicalize(JsonSerializer.SerializeToElement(new
        {
            pe = Hashing.Sha256(pe.AsSpan()),
            pdb = Hashing.Sha256(pdb.AsSpan()),
        }));

    private static ImmutableArray<string> ReadSemanticValues(
        ImmutableArray<byte> image,
        MetadataReader? peReader)
    {
        using var stream = new MemoryStream(image.ToArray(), writable: false);
        using var provider = MetadataReaderProvider.FromPortablePdbStream(stream);
        var reader = provider.GetMetadataReader();
        var values = ImmutableArray.CreateBuilder<string>();
        var importScopes = new Dictionary<ImportScopeHandle, string>();
        var localScopes = new Dictionary<LocalScopeHandle, string>();
        var localVariables = new Dictionary<LocalVariableHandle, string>();
        var localConstants = new Dictionary<LocalConstantHandle, string>();
        foreach (var handle in reader.Documents)
        {
            var document = reader.GetDocument(handle);
            values.Add(
                $"document|{reader.GetString(document.Name)}|{reader.GetGuid(document.Language):D}|{reader.GetGuid(document.HashAlgorithm):D}|{Convert.ToHexString(reader.GetBlobBytes(document.Hash)).ToLowerInvariant()}");
        }
        foreach (var handle in reader.MethodDebugInformation)
        {
            var information = reader.GetMethodDebugInformation(handle);
            var method = MethodIdentity(peReader, handle.ToDefinitionHandle());
            if (!information.LocalSignature.IsNil)
            {
                values.Add(
                    $"local-signature|{method}|{LocalSignature(peReader, information.LocalSignature)}");
            }
            var kickoffMethod = information.GetStateMachineKickoffMethod();
            if (!kickoffMethod.IsNil)
            {
                values.Add(
                    $"state-machine|{method}|kickoff:{MethodIdentity(peReader, kickoffMethod)}");
            }
            foreach (var point in information.GetSequencePoints())
            {
                if (point.IsHidden)
                {
                    continue;
                }
                var documentHandle = point.Document.IsNil ? information.Document : point.Document;
                var documentName = documentHandle.IsNil
                    ? ""
                    : reader.GetString(reader.GetDocument(documentHandle).Name);
                values.Add(
                    $"sequence-point|{method}|{documentName}|{point.StartLine}:{point.StartColumn}-{point.EndLine}:{point.EndColumn}");
            }
        }

        foreach (var handle in reader.ImportScopes)
        {
            values.Add($"import-scope|{ImportScopeIdentity(reader, peReader, handle, importScopes)}");
        }

        var scopeOrdinals = new Dictionary<string, int>(StringComparer.Ordinal);
        foreach (var handle in reader.LocalScopes)
        {
            var scope = reader.GetLocalScope(handle);
            var method = MethodIdentity(peReader, scope.Method);
            scopeOrdinals.TryGetValue(method, out var scopeOrdinal);
            scopeOrdinals[method] = scopeOrdinal + 1;
            var scopeIdentity = $"{method}|scope:{scopeOrdinal}";
            localScopes[handle] = scopeIdentity;
            var importScopeIdentity = scope.ImportScope.IsNil
                ? string.Empty
                : ImportScopeIdentity(reader, peReader, scope.ImportScope, importScopes);
            values.Add($"local-scope|{scopeIdentity}|imports:{importScopeIdentity}");
            foreach (var variableHandle in scope.GetLocalVariables())
            {
                var variable = reader.GetLocalVariable(variableHandle);
                var variableIdentity =
                    $"{scopeIdentity}|index:{variable.Index}|name:{reader.GetString(variable.Name)}|attributes:{variable.Attributes}";
                localVariables[variableHandle] = variableIdentity;
                values.Add($"local-variable|{variableIdentity}");
            }
            foreach (var constantHandle in scope.GetLocalConstants())
            {
                var constant = reader.GetLocalConstant(constantHandle);
                var constantIdentity =
                    $"{scopeIdentity}|name:{reader.GetString(constant.Name)}|signature:{Blob(reader, constant.Signature)}";
                localConstants[constantHandle] = constantIdentity;
                values.Add($"local-constant|{constantIdentity}");
            }
        }

        foreach (var handle in reader.CustomDebugInformation)
        {
            var information = reader.GetCustomDebugInformation(handle);
            var kind = reader.GetGuid(information.Kind);
            var value = kind == StateMachineHoistedLocalScopes
                ? "raw-il-offsets-omitted"
                : Blob(reader, information.Value);
            values.Add(
                $"custom-debug|{DebugParentIdentity(reader, peReader, information.Parent, importScopes, localScopes, localVariables, localConstants)}|{kind:D}|{value}");
        }
        return [.. values.OrderBy(static value => value, StringComparer.Ordinal)];
    }

    private static string ImportScopeIdentity(
        MetadataReader reader,
        MetadataReader? peReader,
        ImportScopeHandle handle,
        Dictionary<ImportScopeHandle, string> identities)
    {
        if (identities.TryGetValue(handle, out var existing))
        {
            return existing;
        }

        var scope = reader.GetImportScope(handle);
        var parent = scope.Parent.IsNil
            ? string.Empty
            : ImportScopeIdentity(reader, peReader, scope.Parent, identities);
        var imports = scope.GetImports().Select(import =>
            $"{import.Kind}:alias:{Utf8(reader, import.Alias)}:assembly:{AssemblyIdentity(peReader, import.TargetAssembly)}:namespace:{ImportNamespace(reader, import)}:type:{ImportType(peReader, import)}");
        var identity = $"parent:[{parent}]|imports:[{string.Join(';', imports)}]";
        identities[handle] = identity;
        return identity;
    }

    private static string DebugParentIdentity(
        MetadataReader reader,
        MetadataReader? peReader,
        EntityHandle handle,
        Dictionary<ImportScopeHandle, string> importScopes,
        Dictionary<LocalScopeHandle, string> localScopes,
        Dictionary<LocalVariableHandle, string> localVariables,
        Dictionary<LocalConstantHandle, string> localConstants) => handle.Kind switch
        {
            HandleKind.ModuleDefinition => "module",
            HandleKind.MethodDefinition => MethodIdentity(peReader, (MethodDefinitionHandle)handle),
            HandleKind.Document => $"document:{reader.GetString(reader.GetDocument((DocumentHandle)handle).Name)}",
            HandleKind.ImportScope => $"import-scope:{ImportScopeIdentity(reader, peReader, (ImportScopeHandle)handle, importScopes)}",
            HandleKind.LocalScope when localScopes.TryGetValue((LocalScopeHandle)handle, out var identity) =>
                $"local-scope:{identity}",
            HandleKind.LocalVariable when localVariables.TryGetValue((LocalVariableHandle)handle, out var identity) =>
                $"local-variable:{identity}",
            HandleKind.LocalConstant when localConstants.TryGetValue((LocalConstantHandle)handle, out var identity) =>
                $"local-constant:{identity}",
            _ => HandleIdentity(peReader, handle),
        };

    private static string HandleIdentity(MetadataReader? peReader, EntityHandle handle)
    {
        if (handle.IsNil)
        {
            return string.Empty;
        }
        if (peReader is not null)
        {
            return handle.Kind switch
            {
                HandleKind.TypeDefinition
                    or HandleKind.TypeReference
                    or HandleKind.TypeSpecification => MetadataIdentity.Type(peReader, handle),
                HandleKind.MethodDefinition => MethodIdentity(peReader, (MethodDefinitionHandle)handle),
                _ => $"{handle.Kind}:{MetadataTokens.GetRowNumber(handle)}",
            };
        }
        return $"{handle.Kind}:{MetadataTokens.GetRowNumber(handle)}";
    }

    private static string MethodIdentity(MetadataReader? peReader, MethodDefinitionHandle handle) =>
        peReader is null
            ? $"method:{MetadataTokens.GetRowNumber(handle)}"
            : $"method:{MetadataIdentity.Method(peReader, handle)}";

    private static string LocalSignature(MetadataReader? peReader, StandaloneSignatureHandle handle)
    {
        if (peReader is null)
        {
            return $"row:{MetadataTokens.GetRowNumber(handle)}";
        }
        var locals = peReader.GetStandaloneSignature(handle)
            .DecodeLocalSignature(new MetadataTypeNameProvider(), null);
        return $"locals:({string.Join(',', locals)})";
    }

    private static string AssemblyIdentity(MetadataReader? peReader, AssemblyReferenceHandle handle)
    {
        if (handle.IsNil)
        {
            return peReader is null ? "0" : string.Empty;
        }
        if (peReader is null)
        {
            return MetadataTokens.GetRowNumber((EntityHandle)handle).ToString(
                System.Globalization.CultureInfo.InvariantCulture);
        }
        var reference = peReader.GetAssemblyReference(handle);
        return $"{peReader.GetString(reference.Name)},{reference.Version},{peReader.GetString(reference.Culture)}:{Blob(peReader, reference.PublicKeyOrToken)}:{reference.Flags}";
    }

    private static string ImportNamespace(MetadataReader reader, ImportDefinition import) => import.Kind switch
    {
        ImportDefinitionKind.ImportNamespace
            or ImportDefinitionKind.ImportAssemblyNamespace
            or ImportDefinitionKind.ImportXmlNamespace
            or ImportDefinitionKind.AliasNamespace
            or ImportDefinitionKind.AliasAssemblyNamespace => Utf8(reader, import.TargetNamespace),
        _ => string.Empty,
    };

    private static string ImportType(MetadataReader? peReader, ImportDefinition import) => import.Kind switch
    {
        ImportDefinitionKind.ImportType or ImportDefinitionKind.AliasType => HandleIdentity(peReader, import.TargetType),
        _ => string.Empty,
    };

    private static string Utf8(MetadataReader reader, BlobHandle handle) => handle.IsNil
        ? string.Empty
        : Encoding.UTF8.GetString(reader.GetBlobBytes(handle));

    private static string Blob(MetadataReader reader, BlobHandle handle) => handle.IsNil
        ? string.Empty
        : Convert.ToHexString(reader.GetBlobBytes(handle)).ToLowerInvariant();

    private static readonly Guid StateMachineHoistedLocalScopes =
        new("6da9a61e-f8c7-4874-be62-68bc5630df71");
}
