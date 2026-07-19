using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Text.Json;

return await DebuggerProbe.RunAsync(args);

internal static class DebuggerProbe
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        WriteIndented = true
    };

    public static async Task<int> RunAsync(string[] arguments)
    {
        ProbeOptions options;

        try
        {
            options = ProbeOptions.Parse(arguments);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine(ex.Message);
            Console.Error.WriteLine(
                "Usage: --rid <rid> --archive <asset> --adapter <netcoredbg> --program <debuggee.dll> "
                + "--pdb <debuggee.pdb> --source <mapped Program.fs> --bootstrap <bootstrap.json> "
                + "--evidence <result.json>");
            return 2;
        }

        BreakpointRequest[] requestedBreakpoints = [];
        var breakpointBindings = new List<BreakpointBinding>();
        var observations = new List<StopObservation>();
        PdbObservation? pdbObservation = null;
        var driverProbeVerdict = DriverProbeVerdict.InfraError;
        var compilerVerdict = CompilerVerdict.InfraError;
        var failureClass = ProbeFailureClass.Infrastructure;
        string? failure = null;
        DapSession? session = null;

        try
        {
            pdbObservation = ValidatePortablePdb(options.PdbPath, options.SourcePath);
            var sourceLines = await File.ReadAllLinesAsync(options.SourcePath);
            requestedBreakpoints = FindBreakpoints(sourceLines, pdbObservation.SequencePoints);
            session = await DapSession.StartAsync(options.AdapterPath);

            await session.RequestAsync(
                "initialize",
                new
                {
                    clientID = "fsharp2-harness",
                    clientName = "FSharp2 debugger conformance probe",
                    adapterID = "coreclr",
                    pathFormat = "path",
                    linesStartAt1 = true,
                    columnsStartAt1 = true,
                    supportsVariableType = true,
                    supportsVariablePaging = false,
                    supportsRunInTerminalRequest = false,
                    locale = "en-US"
                });

            var launchSequence = await session.SendRequestAsync(
                "launch",
                new
                {
                    name = "FSharp2 debugger conformance probe",
                    type = "coreclr",
                    request = "launch",
                    program = options.ProgramPath,
                    cwd = Path.GetDirectoryName(options.ProgramPath),
                    stopAtEntry = false,
                    justMyCode = false
                });

            await session.WaitForEventAsync("initialized");

            var breakpointResponse = await session.RequestAsync(
                "setBreakpoints",
                new
                {
                    source = new
                    {
                        name = Path.GetFileName(options.SourcePath),
                        path = options.SourcePath
                    },
                    breakpoints = requestedBreakpoints
                        .Select(item => new { line = item.Line })
                        .ToArray(),
                    sourceModified = false
                });

            breakpointBindings.AddRange(
                ReadInitialBreakpointBindings(breakpointResponse, requestedBreakpoints));

            await session.RequestAsync("configurationDone", new { });
            await session.WaitForResponseAsync(launchSequence);
            await ResolvePendingBreakpointsAsync(session, breakpointBindings, options.SourcePath);

            RequireExactBinding(breakpointBindings[0], options.SourcePath);

            var ordinaryStop = await session.WaitForEventAsync("stopped");
            observations.Add(await CaptureStopAsync(session, "ordinary", ordinaryStop));
            ValidateStop(observations[^1], options.SourcePath, requestedBreakpoints[0].Line);

            var ordinaryThread = observations[^1].ThreadId;
            await StepAsync(session, observations, "stepIn", ordinaryThread, options.SourcePath);
            await StepAsync(session, observations, "stepOut", ordinaryThread, options.SourcePath);
            await StepAsync(session, observations, "next", ordinaryThread, options.SourcePath);

            await session.RequestAsync("continue", new { threadId = ordinaryThread });
            var taskStop = await session.WaitForEventAsync("stopped");
            observations.Add(await CaptureStopAsync(session, "task", taskStop));
            RequireExactBinding(breakpointBindings[1], options.SourcePath);
            ValidateStop(observations[^1], options.SourcePath, requestedBreakpoints[1].Line);

            if (!observations.Any(item => item.Variables.Count > 0))
            {
                throw new DriverProbeFailureException(
                    ProbeFailureClass.Locals,
                    "The driver returned no local variables at any stopped state.");
            }

            await session.RequestAsync(
                "disconnect",
                new { restart = false, terminateDebuggee = true });

            driverProbeVerdict = DriverProbeVerdict.Pass;
            compilerVerdict = CompilerVerdict.NotRun;
            failureClass = ProbeFailureClass.None;
        }
        catch (DriverProbeFailureException ex)
        {
            driverProbeVerdict = DriverProbeVerdict.Fail;
            compilerVerdict = CompilerVerdict.InfraError;
            failureClass = ex.FailureClass;
            failure = ex.ToString();
        }
        catch (Exception ex)
        {
            failure = ex.ToString();
        }
        finally
        {
            if (session is not null)
            {
                try
                {
                    await session.DisposeAsync();
                }
                catch (Exception ex)
                {
                    driverProbeVerdict = DriverProbeVerdict.InfraError;
                    compilerVerdict = CompilerVerdict.InfraError;
                    failureClass = ProbeFailureClass.Cleanup;
                    failure = string.Join(
                        Environment.NewLine,
                        new[] { failure, ex.ToString() }
                            .Where(item => !string.IsNullOrEmpty(item)));
                }
            }

            if (session?.Cleanup is { } cleanup
                && (!cleanup.AdapterExited || cleanup.RemainingProcessIds.Count > 0))
            {
                driverProbeVerdict = DriverProbeVerdict.InfraError;
                compilerVerdict = CompilerVerdict.InfraError;
                failureClass = ProbeFailureClass.Cleanup;
                failure = string.Join(
                    Environment.NewLine,
                    new[]
                    {
                        failure,
                        $"Debugger cleanup left adapterExited={cleanup.AdapterExited}, "
                        + $"remainingProcessIds=[{string.Join(",", cleanup.RemainingProcessIds)}]."
                    }
                        .Where(item => !string.IsNullOrEmpty(item)));
            }

            var evidenceDirectory = Path.GetDirectoryName(options.EvidencePath);

            if (!string.IsNullOrEmpty(evidenceDirectory))
            {
                Directory.CreateDirectory(evidenceDirectory);
            }

            var semanticFingerprint = SemanticFingerprint(
                driverProbeVerdict,
                failureClass,
                breakpointBindings,
                observations,
                pdbObservation,
                session?.Cleanup);

            var evidence = new
            {
                schemaVersion = 2,
                probeId = "netcoredbg-focused-candidate-v1",
                scope = "Focused launch-mode go/no-go for the pinned candidate's ordinary and task breakpoint behavior.",
                finalConformanceClaimed = false,
                driverProbeVerdict = driverProbeVerdict.ToWireValue(),
                compilerVerdict = compilerVerdict.ToWireValue(),
                failureClass = failureClass.ToWireValue(),
                failure,
                semanticFingerprint,
                rid = options.Rid,
                runtime = new
                {
                    framework = RuntimeInformation.FrameworkDescription,
                    os = RuntimeInformation.OSDescription,
                    architecture = RuntimeInformation.ProcessArchitecture.ToString(),
                    runtimeIdentifier = RuntimeInformation.RuntimeIdentifier,
                    environmentVersion = Environment.Version.ToString()
                },
                archive = FileIdentity.Create(options.ArchivePath),
                bootstrap = FileIdentity.Create(options.BootstrapPath),
                adapter = options.AdapterPath,
                adapterSha256 = FileHash.Sha256(options.AdapterPath),
                program = options.ProgramPath,
                programSha256 = FileHash.Sha256(options.ProgramPath),
                pdb = FileIdentity.Create(options.PdbPath),
                pdbObservation,
                source = options.SourcePath,
                sourceSha256 = FileHash.Sha256(options.SourcePath),
                requestedBreakpoints,
                breakpointBindings,
                observations,
                transcript = session?.Transcript ?? [],
                rawDapFrames = session?.RawDapFrames ?? [],
                adapterStandardError = session?.StandardError ?? string.Empty,
                cleanup = session?.Cleanup
            };

            await File.WriteAllTextAsync(
                options.EvidencePath,
                JsonSerializer.Serialize(evidence, JsonOptions) + Environment.NewLine);
        }

        if (driverProbeVerdict == DriverProbeVerdict.Pass)
        {
            Console.WriteLine(
                $"Focused debugger candidate probe passed; full conformance is not established. Evidence: {options.EvidencePath}");
            return 0;
        }

        Console.Error.WriteLine(failure);
        Console.Error.WriteLine(
            $"Focused debugger candidate probe {driverProbeVerdict.ToWireValue()}; "
            + $"compiler verdict is {compilerVerdict.ToWireValue()}. Evidence: {options.EvidencePath}");
        return 1;
    }

    private static BreakpointRequest[] FindBreakpoints(
        string[] sourceLines,
        IReadOnlyList<SequencePointObservation> sequencePoints)
    {
        var result = sourceLines
            .Select((text, index) => new { text, line = index + 1 })
            .Where(item => item.text.Contains("// BREAKPOINT:", StringComparison.Ordinal))
            .Select(item =>
            {
                var candidates = sequencePoints
                    .Where(point => point.StartLine <= item.line && item.line <= point.EndLine)
                    .OrderBy(point => point.EndLine - point.StartLine)
                    .ThenBy(point => point.StartLine)
                    .ThenBy(point => point.StartColumn)
                    .ThenBy(point => point.MethodRow)
                    .ThenBy(point => point.IlOffset)
                    .ToArray();

                if (candidates.Length == 0)
                {
                    throw new InvalidDataException(
                        $"Breakpoint marker at line {item.line} is not covered by a Portable PDB sequence point.");
                }

                var canonicalLines = candidates
                    .Select(point => point.StartLine)
                    .Distinct()
                    .ToArray();

                if (canonicalLines.Length != 1)
                {
                    throw new InvalidDataException(
                        $"Breakpoint marker at line {item.line} has ambiguous canonical sequence-point lines: "
                        + string.Join(", ", canonicalLines));
                }

                var sequencePoint = candidates[0];
                return new BreakpointRequest(
                    item.text[(item.text.IndexOf("// BREAKPOINT:", StringComparison.Ordinal) + 14)..].Trim(),
                    item.line,
                    sequencePoint.StartLine,
                    sequencePoint.EndLine,
                    sequencePoint.MethodRow,
                    sequencePoint.IlOffset);
            })
            .ToArray();

        if (result.Length != 2 || result[0].Name != "ordinary" || result[1].Name != "task")
        {
            throw new InvalidOperationException("The fixture must contain ordered ordinary and task breakpoint markers.");
        }

        return result;
    }

    private static IReadOnlyList<BreakpointBinding> ReadInitialBreakpointBindings(
        JsonElement response,
        IReadOnlyList<BreakpointRequest> requested)
    {
        var breakpoints = response.GetProperty("body").GetProperty("breakpoints");

        if (breakpoints.GetArrayLength() != requested.Count)
        {
                throw new DriverProbeFailureException(
                ProbeFailureClass.BreakpointBinding,
                "The driver did not return every requested breakpoint.");
        }

        var result = new List<BreakpointBinding>(requested.Count);

        for (var index = 0; index < requested.Count; index++)
        {
            var actual = breakpoints[index];
            var verified = actual.TryGetProperty("verified", out var verifiedProperty)
                && verifiedProperty.GetBoolean();
            var actualLine = actual.TryGetProperty("line", out var lineProperty)
                ? lineProperty.GetInt32()
                : -1;
            var actualEndLine = actual.TryGetProperty("endLine", out var endLineProperty)
                ? endLineProperty.GetInt32()
                : (int?) null;
            var message = actual.TryGetProperty("message", out var messageProperty)
                ? messageProperty.GetString()
                : null;
            var pending = !verified
                && message?.Contains("pending", StringComparison.OrdinalIgnoreCase) == true;
            var id = actual.TryGetProperty("id", out var idProperty)
                ? idProperty.GetInt32()
                : -1;

            if ((!verified && !pending) || actualLine != requested[index].Line || id < 0)
            {
                throw new DriverProbeFailureException(
                    ProbeFailureClass.BreakpointBinding,
                    $"Breakpoint '{requested[index].Name}' was not bound exactly: "
                    + $"verified={verified}, requested={requested[index].Line}, actual={actualLine}, message={message}");
            }

            result.Add(
                new BreakpointBinding(
                    requested[index].Name,
                    id,
                    requested[index].MarkerLine,
                    requested[index].Line,
                    requested[index].SequencePointEndLine,
                    requested[index].MethodRow,
                    requested[index].IlOffset,
                    verified,
                    pending,
                    actualLine,
                    actualEndLine,
                    message)
                {
                    ResolvedVerified = verified,
                    ResolvedLine = verified ? actualLine : null,
                    ResolvedEndLine = verified ? actualEndLine : null
                });
        }

        return result;
    }

    private static async Task ResolvePendingBreakpointsAsync(
        DapSession session,
        IReadOnlyList<BreakpointBinding> bindings,
        string expectedSourcePath)
    {
        var pendingById = bindings
            .Where(item => item.InitialPending)
            .ToDictionary(item => item.Id);
        var pendingCount = pendingById.Count;

        for (var index = 0; index < pendingCount; index++)
        {
            var changed = await session.WaitForEventAsync("breakpoint");
            var body = changed.GetProperty("body");
            var reason = body.GetProperty("reason").GetString();
            var breakpoint = body.GetProperty("breakpoint");
            var id = breakpoint.GetProperty("id").GetInt32();

            if (!pendingById.TryGetValue(id, out var binding))
            {
                throw new DriverProbeFailureException(
                    ProbeFailureClass.BreakpointBinding,
                    $"The driver emitted a verification transition for unknown breakpoint id {id}.");
            }

            var sourcePath = breakpoint.TryGetProperty("source", out var source)
                && source.TryGetProperty("path", out var path)
                    ? path.GetString()
                    : null;

            binding.ResolutionReason = reason;
            binding.ResolvedVerified = breakpoint.TryGetProperty("verified", out var verified)
                && verified.GetBoolean();
            binding.ResolvedLine = breakpoint.TryGetProperty("line", out var line)
                ? line.GetInt32()
                : null;
            binding.ResolvedEndLine = breakpoint.TryGetProperty("endLine", out var endLine)
                ? endLine.GetInt32()
                : null;
            binding.ResolvedSourcePath = sourcePath;

            if (!string.Equals(reason, "changed", StringComparison.Ordinal)
                || !binding.ResolvedVerified)
            {
                throw new DriverProbeFailureException(
                    ProbeFailureClass.BreakpointBinding,
                    $"Breakpoint '{binding.Name}' did not transition from pending to verified: "
                    + $"reason={reason}, verified={binding.ResolvedVerified}.");
            }

            if (!PathsEqual(sourcePath, expectedSourcePath))
            {
                throw new DriverProbeFailureException(
                    ProbeFailureClass.SourceMapping,
                    $"Breakpoint '{binding.Name}' resolved to '{sourcePath}' instead of '{expectedSourcePath}'.");
            }

            pendingById.Remove(id);
        }

        if (pendingById.Count > 0)
        {
            throw new DriverProbeFailureException(
                ProbeFailureClass.BreakpointBinding,
                "The driver did not emit every pending breakpoint verification transition.");
        }
    }

    private static void RequireExactBinding(BreakpointBinding binding, string expectedSourcePath)
    {
        var sourceMatches = binding.ResolvedSourcePath is null
            || PathsEqual(binding.ResolvedSourcePath, expectedSourcePath);

        if (!binding.ResolvedVerified
            || binding.ResolvedLine != binding.RequestedLine
            || binding.ResolvedEndLine != binding.SequencePointEndLine
            || !sourceMatches)
        {
            throw new DriverProbeFailureException(
                ProbeFailureClass.BreakpointBinding,
                $"Breakpoint '{binding.Name}' did not resolve exactly: "
                + $"requested={binding.RequestedLine}-{binding.SequencePointEndLine}, "
                + $"resolved={binding.ResolvedLine}-{binding.ResolvedEndLine}, "
                + $"verified={binding.ResolvedVerified}, source={binding.ResolvedSourcePath}.");
        }
    }

    private static async Task StepAsync(
        DapSession session,
        ICollection<StopObservation> observations,
        string command,
        int threadId,
        string sourcePath)
    {
        await session.RequestAsync(command, new { threadId, granularity = "line" });
        var stopped = await session.WaitForEventAsync("stopped");
        observations.Add(await CaptureStopAsync(session, command, stopped));
        ValidateStop(observations.Last(), sourcePath, null);
    }

    private static async Task<StopObservation> CaptureStopAsync(
        DapSession session,
        string label,
        JsonElement stopped)
    {
        var body = stopped.GetProperty("body");
        var reason = body.GetProperty("reason").GetString() ?? string.Empty;
        var threadId = body.TryGetProperty("threadId", out var threadProperty)
            ? threadProperty.GetInt32()
            : await FirstThreadIdAsync(session);

        var stackResponse = await session.RequestAsync(
            "stackTrace",
            new { threadId, startFrame = 0, levels = 50 });
        var stackFrames = stackResponse.GetProperty("body").GetProperty("stackFrames");

        if (stackFrames.GetArrayLength() == 0)
        {
            throw new DriverProbeFailureException(
                ProbeFailureClass.Stack,
                $"Stopped state '{label}' returned no stack frames.");
        }

        var frames = new List<FrameObservation>();

        foreach (var frame in stackFrames.EnumerateArray())
        {
            var sourcePath = frame.TryGetProperty("source", out var source)
                && source.TryGetProperty("path", out var path)
                    ? path.GetString()
                    : null;

            frames.Add(new FrameObservation(
                frame.GetProperty("name").GetString() ?? string.Empty,
                sourcePath,
                frame.TryGetProperty("line", out var line) ? line.GetInt32() : 0,
                frame.TryGetProperty("column", out var column) ? column.GetInt32() : 0));
        }

        var topFrame = stackFrames[0];
        var frameId = topFrame.GetProperty("id").GetInt32();
        var scopesResponse = await session.RequestAsync("scopes", new { frameId });
        var variables = new List<VariableObservation>();

        foreach (var scope in scopesResponse.GetProperty("body").GetProperty("scopes").EnumerateArray())
        {
            var reference = scope.GetProperty("variablesReference").GetInt32();

            if (reference == 0)
            {
                continue;
            }

            var variablesResponse = await session.RequestAsync(
                "variables",
                new { variablesReference = reference });

            foreach (var variable in variablesResponse.GetProperty("body").GetProperty("variables").EnumerateArray())
            {
                variables.Add(new VariableObservation(
                    scope.GetProperty("name").GetString() ?? string.Empty,
                    variable.GetProperty("name").GetString() ?? string.Empty,
                    variable.TryGetProperty("type", out var type) ? type.GetString() : null,
                    variable.GetProperty("value").GetString() ?? string.Empty));
            }
        }

        return new StopObservation(label, reason, threadId, frames, variables);
    }

    private static void ValidateStop(
        StopObservation observation,
        string expectedSourcePath,
        int? expectedLine)
    {
        var topFrame = observation.Frames[0];

        if (!PathsEqual(topFrame.SourcePath, expectedSourcePath))
        {
            throw new DriverProbeFailureException(
                ProbeFailureClass.SourceMapping,
                $"Stopped state '{observation.Label}' mapped to '{topFrame.SourcePath}' instead of '{expectedSourcePath}'.");
        }

        if (expectedLine is not null && topFrame.Line != expectedLine.Value)
        {
            throw new DriverProbeFailureException(
                ProbeFailureClass.BreakpointBinding,
                $"Stopped state '{observation.Label}' reached line {topFrame.Line} instead of {expectedLine.Value}.");
        }
    }

    private static async Task<int> FirstThreadIdAsync(DapSession session)
    {
        var response = await session.RequestAsync("threads", new { });
        var threads = response.GetProperty("body").GetProperty("threads");

        if (threads.GetArrayLength() == 0)
        {
            throw new DriverProbeFailureException(
                ProbeFailureClass.Stack,
                "The driver reported no threads while stopped.");
        }

        return threads[0].GetProperty("id").GetInt32();
    }

    private static bool PathsEqual(string? left, string right)
    {
        if (left is null)
        {
            return false;
        }

        var comparison = OperatingSystem.IsWindows()
            ? StringComparison.OrdinalIgnoreCase
            : StringComparison.Ordinal;

        return string.Equals(Path.GetFullPath(left), Path.GetFullPath(right), comparison);
    }

    private static PdbObservation ValidatePortablePdb(string pdbPath, string sourcePath)
    {
        var expectedSourceHash = SHA256.HashData(File.ReadAllBytes(sourcePath));
        var sha256DocumentHashAlgorithm = new Guid("8829d00f-11b8-4213-878b-770e8597ac16");

        using var stream = File.OpenRead(pdbPath);
        using var provider = MetadataReaderProvider.FromPortablePdbStream(stream);
        var reader = provider.GetMetadataReader();

        DocumentHandle matchingDocumentHandle = default;
        string? matchingDocumentPath = null;
        Guid matchingAlgorithm = default;
        byte[]? matchingHash = null;

        foreach (var handle in reader.Documents)
        {
            var document = reader.GetDocument(handle);
            var documentPath = reader.GetString(document.Name);

            if (!PathsEqual(documentPath, sourcePath))
            {
                continue;
            }

            var algorithm = reader.GetGuid(document.HashAlgorithm);
            var recordedHash = reader.GetBlobBytes(document.Hash);

            if (algorithm != sha256DocumentHashAlgorithm)
            {
                throw new InvalidDataException(
                    $"The mapped PDB document uses unsupported checksum algorithm '{algorithm}'.");
            }

            if (!CryptographicOperations.FixedTimeEquals(expectedSourceHash, recordedHash))
            {
                throw new InvalidDataException(
                    "The mapped PDB document checksum does not match the materialized source bytes.");
            }

            matchingDocumentHandle = handle;
            matchingDocumentPath = documentPath;
            matchingAlgorithm = algorithm;
            matchingHash = recordedHash;
            break;
        }

        if (matchingDocumentHandle.IsNil || matchingDocumentPath is null || matchingHash is null)
        {
            throw new InvalidDataException(
                $"The portable PDB does not contain the mapped source document '{sourcePath}'.");
        }

        var sequencePoints = new List<SequencePointObservation>();

        foreach (var handle in reader.MethodDebugInformation)
        {
            var method = reader.GetMethodDebugInformation(handle);

            foreach (var point in method.GetSequencePoints())
            {
                if (point.IsHidden)
                {
                    continue;
                }

                var documentHandle = point.Document.IsNil ? method.Document : point.Document;

                if (documentHandle != matchingDocumentHandle)
                {
                    continue;
                }

                sequencePoints.Add(new SequencePointObservation(
                    MetadataTokens.GetRowNumber(handle),
                    point.Offset,
                    point.StartLine,
                    point.StartColumn,
                    point.EndLine,
                    point.EndColumn));
            }
        }

        if (sequencePoints.Count == 0)
        {
            throw new InvalidDataException(
                $"The portable PDB contains no visible sequence points for '{sourcePath}'.");
        }

        return new PdbObservation(
            matchingDocumentPath,
            matchingAlgorithm,
            Convert.ToHexString(matchingHash).ToLowerInvariant(),
            true,
            sequencePoints
                .OrderBy(point => point.StartLine)
                .ThenBy(point => point.StartColumn)
                .ThenBy(point => point.MethodRow)
                .ThenBy(point => point.IlOffset)
                .ToArray());
    }

    private static string SemanticFingerprint(
        DriverProbeVerdict driverProbeVerdict,
        ProbeFailureClass failureClass,
        IReadOnlyList<BreakpointBinding> bindings,
        IReadOnlyList<StopObservation> observations,
        PdbObservation? pdbObservation,
        CleanupObservation? cleanup)
    {
        var projection = new
        {
            driverProbeVerdict = driverProbeVerdict.ToWireValue(),
            failureClass = failureClass.ToWireValue(),
            breakpoints = bindings.Select(item => new
            {
                item.Name,
                item.MarkerLine,
                item.RequestedLine,
                item.SequencePointEndLine,
                item.MethodRow,
                item.IlOffset,
                item.InitialVerified,
                item.InitialPending,
                item.ResolvedVerified,
                item.ResolvedLine,
                item.ResolvedEndLine,
                resolvedSource = item.ResolvedSourcePath is null
                    ? null
                    : Path.GetFileName(item.ResolvedSourcePath)
            }),
            stops = observations.Select(item => new
            {
                item.Label,
                item.Reason,
                frames = item.Frames.Select(frame => new
                {
                    frame.Name,
                    source = frame.SourcePath is null ? null : Path.GetFileName(frame.SourcePath),
                    frame.Line,
                    frame.Column
                }),
                variables = item.Variables
                    .OrderBy(variable => variable.Scope, StringComparer.Ordinal)
                    .ThenBy(variable => variable.Name, StringComparer.Ordinal)
                    .Select(variable => new
                    {
                        variable.Scope,
                        variable.Name,
                        variable.Type,
                        variable.Value
                    })
            }),
            pdb = pdbObservation is null
                ? null
                : new
                {
                    source = Path.GetFileName(pdbObservation.DocumentPath),
                    pdbObservation.HashAlgorithm,
                    pdbObservation.Checksum,
                    pdbObservation.ChecksumVerified,
                    pdbObservation.SequencePoints
                },
            cleanup = cleanup is null
                ? null
                : new
                {
                    cleanup.AdapterExited,
                    cleanup.TreeKillRequested,
                    debuggeeProcessCount = cleanup.DebuggeeProcessIds.Count,
                    remainingProcessCount = cleanup.RemainingProcessIds.Count
                }
        };

        return FileHash.Sha256(JsonSerializer.SerializeToUtf8Bytes(projection));
    }
}

internal sealed record ProbeOptions(
    string Rid,
    string ArchivePath,
    string AdapterPath,
    string ProgramPath,
    string PdbPath,
    string SourcePath,
    string BootstrapPath,
    string EvidencePath)
{
    public static ProbeOptions Parse(string[] arguments)
    {
        var values = new Dictionary<string, string>(StringComparer.Ordinal);

        for (var index = 0; index < arguments.Length; index += 2)
        {
            if (index + 1 >= arguments.Length || !arguments[index].StartsWith("--", StringComparison.Ordinal))
            {
                throw new ArgumentException("Every option must use '--name value'.");
            }

            values.Add(arguments[index][2..], arguments[index + 1]);
        }

        return new ProbeOptions(
            Required(values, "rid"),
            RequiredPath(values, "archive"),
            RequiredPath(values, "adapter"),
            RequiredPath(values, "program"),
            RequiredPath(values, "pdb"),
            RequiredPath(values, "source"),
            RequiredPath(values, "bootstrap"),
            Path.GetFullPath(Required(values, "evidence")));
    }

    private static string RequiredPath(IReadOnlyDictionary<string, string> values, string name)
    {
        var path = Path.GetFullPath(Required(values, name));

        if (!File.Exists(path))
        {
            throw new FileNotFoundException($"The --{name} file does not exist.", path);
        }

        return path;
    }

    private static string Required(IReadOnlyDictionary<string, string> values, string name) =>
        values.TryGetValue(name, out var value) && !string.IsNullOrWhiteSpace(value)
            ? value
            : throw new ArgumentException($"Missing required --{name} option.");
}

internal static class FileHash
{
    public static string Sha256(string path)
    {
        using var stream = File.OpenRead(path);
        return Convert.ToHexString(SHA256.HashData(stream)).ToLowerInvariant();
    }

    public static string Sha256(byte[] bytes) =>
        Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant();
}

internal sealed record BreakpointRequest(
    string Name,
    int MarkerLine,
    int Line,
    int SequencePointEndLine,
    int MethodRow,
    int IlOffset);

internal sealed class BreakpointBinding(
    string name,
    int id,
    int markerLine,
    int requestedLine,
    int sequencePointEndLine,
    int methodRow,
    int ilOffset,
    bool initialVerified,
    bool initialPending,
    int initialLine,
    int? initialEndLine,
    string? initialMessage)
{
    public string Name { get; } = name;
    public int Id { get; } = id;
    public int MarkerLine { get; } = markerLine;
    public int RequestedLine { get; } = requestedLine;
    public int SequencePointEndLine { get; } = sequencePointEndLine;
    public int MethodRow { get; } = methodRow;
    public int IlOffset { get; } = ilOffset;
    public bool InitialVerified { get; } = initialVerified;
    public bool InitialPending { get; } = initialPending;
    public int InitialLine { get; } = initialLine;
    public int? InitialEndLine { get; } = initialEndLine;
    public string? InitialMessage { get; } = initialMessage;
    public string? ResolutionReason { get; set; }
    public bool ResolvedVerified { get; set; }
    public int? ResolvedLine { get; set; }
    public int? ResolvedEndLine { get; set; }
    public string? ResolvedSourcePath { get; set; }
}

internal enum DriverProbeVerdict
{
    Pass,
    Fail,
    InfraError
}

internal enum CompilerVerdict
{
    NotRun,
    InfraError
}

internal enum ProbeFailureClass
{
    None,
    Infrastructure,
    Cleanup,
    Locals,
    BreakpointBinding,
    SourceMapping,
    Stack
}

internal static class ProbeWireValues
{
    public static string ToWireValue(this DriverProbeVerdict verdict) => verdict switch
    {
        DriverProbeVerdict.Pass => "pass",
        DriverProbeVerdict.Fail => "fail",
        DriverProbeVerdict.InfraError => "infra-error",
        _ => throw new ArgumentOutOfRangeException(nameof(verdict), verdict, null)
    };

    public static string ToWireValue(this CompilerVerdict verdict) => verdict switch
    {
        CompilerVerdict.NotRun => "not-run",
        CompilerVerdict.InfraError => "infra-error",
        _ => throw new ArgumentOutOfRangeException(nameof(verdict), verdict, null)
    };

    public static string ToWireValue(this ProbeFailureClass failureClass) => failureClass switch
    {
        ProbeFailureClass.None => string.Empty,
        ProbeFailureClass.Infrastructure => "infrastructure",
        ProbeFailureClass.Cleanup => "cleanup",
        ProbeFailureClass.Locals => "locals",
        ProbeFailureClass.BreakpointBinding => "breakpoint-binding",
        ProbeFailureClass.SourceMapping => "source-mapping",
        ProbeFailureClass.Stack => "stack",
        _ => throw new ArgumentOutOfRangeException(nameof(failureClass), failureClass, null)
    };
}

internal sealed class DriverProbeFailureException(ProbeFailureClass failureClass, string message)
    : Exception(message)
{
    public ProbeFailureClass FailureClass { get; } = failureClass;
}

internal sealed record FileIdentity(string Path, long Length, string Sha256)
{
    public static FileIdentity Create(string path)
    {
        var file = new FileInfo(path);
        return new FileIdentity(file.FullName, file.Length, FileHash.Sha256(file.FullName));
    }
}

internal sealed record PdbObservation(
    string DocumentPath,
    Guid HashAlgorithm,
    string Checksum,
    bool ChecksumVerified,
    IReadOnlyList<SequencePointObservation> SequencePoints);

internal sealed record SequencePointObservation(
    int MethodRow,
    int IlOffset,
    int StartLine,
    int StartColumn,
    int EndLine,
    int EndColumn);

internal sealed record CleanupObservation(
    int AdapterProcessId,
    bool AdapterExited,
    int? AdapterExitCode,
    bool TreeKillRequested,
    IReadOnlyList<int> DebuggeeProcessIds,
    IReadOnlyList<int> RemainingProcessIds);

internal sealed record DapMessageObservation(
    DateTimeOffset TimestampUtc,
    string Direction,
    JsonElement Message);

internal sealed record RawDapFrame(
    DateTimeOffset TimestampUtc,
    string Direction,
    int Length,
    string Sha256,
    string FrameBase64);

internal sealed record FrameObservation(string Name, string? SourcePath, int Line, int Column);

internal sealed record VariableObservation(string Scope, string Name, string? Type, string Value);

internal sealed record StopObservation(
    string Label,
    string Reason,
    int ThreadId,
    IReadOnlyList<FrameObservation> Frames,
    IReadOnlyList<VariableObservation> Variables);
