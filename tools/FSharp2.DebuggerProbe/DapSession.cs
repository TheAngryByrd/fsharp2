using System.Diagnostics;
using System.Text;
using System.Text.Json;

internal sealed class DapSession : IAsyncDisposable
{
    private static readonly TimeSpan MessageTimeout = TimeSpan.FromSeconds(20);
    private readonly Process process;
    private readonly Stream input;
    private readonly Stream output;
    private readonly Task<string> standardError;
    private readonly List<JsonElement> backlog = [];
    private readonly HashSet<int> debuggeeProcessIds = [];
    private int nextSequence = 1;

    private DapSession(Process process)
    {
        this.process = process;
        input = process.StandardInput.BaseStream;
        output = process.StandardOutput.BaseStream;
        standardError = process.StandardError.ReadToEndAsync();
    }

    public List<DapMessageObservation> Transcript { get; } = [];

    public List<RawDapFrame> RawDapFrames { get; } = [];

    public CleanupObservation? Cleanup { get; private set; }

    public string StandardError => standardError.IsCompletedSuccessfully
        ? standardError.Result
        : string.Empty;

    public static Task<DapSession> StartAsync(string adapterPath)
    {
        var startInfo = new ProcessStartInfo(adapterPath)
        {
            UseShellExecute = false,
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            WorkingDirectory = Path.GetDirectoryName(adapterPath)
        };
        startInfo.ArgumentList.Add("--interpreter=vscode");

        var process = new Process { StartInfo = startInfo };

        if (!process.Start())
        {
            throw new InvalidOperationException("Failed to start the debugger adapter.");
        }

        return Task.FromResult(new DapSession(process));
    }

    public async Task<JsonElement> RequestAsync(string command, object arguments)
    {
        var sequence = await SendRequestAsync(command, arguments);
        return await WaitForResponseAsync(sequence);
    }

    public async Task<int> SendRequestAsync(string command, object arguments)
    {
        var sequence = nextSequence++;
        var message = new
        {
            seq = sequence,
            type = "request",
            command,
            arguments
        };
        var payload = JsonSerializer.SerializeToUtf8Bytes(message);
        var header = Encoding.ASCII.GetBytes($"Content-Length: {payload.Length}\r\n\r\n");
        using (var document = JsonDocument.Parse(payload))
        {
            RecordFrame("client-to-adapter", header, payload, document.RootElement.Clone());
        }

        await input.WriteAsync(header);
        await input.WriteAsync(payload);
        await input.FlushAsync();
        return sequence;
    }

    public async Task<JsonElement> WaitForResponseAsync(int requestSequence)
    {
        var response = await WaitForMessageAsync(message =>
            PropertyEquals(message, "type", "response")
            && message.TryGetProperty("request_seq", out var sequence)
            && sequence.GetInt32() == requestSequence);

        if (!response.TryGetProperty("success", out var success) || !success.GetBoolean())
        {
            var command = response.TryGetProperty("command", out var commandProperty)
                ? commandProperty.GetString()
                : "unknown";
            var message = response.TryGetProperty("message", out var messageProperty)
                ? messageProperty.GetString()
                : response.GetRawText();
            throw new InvalidOperationException($"DAP request '{command}' failed: {message}");
        }

        return response;
    }

    public Task<JsonElement> WaitForEventAsync(string eventName) =>
        WaitForMessageAsync(message =>
            PropertyEquals(message, "type", "event")
            && PropertyEquals(message, "event", eventName));

    public async ValueTask DisposeAsync()
    {
        var processId = process.Id;
        var treeKillRequested = false;
        input.Close();

        if (!process.HasExited)
        {
            try
            {
                if (!process.WaitForExit(2_000))
                {
                    treeKillRequested = true;
                    process.Kill(true);
                }
            }
            catch (InvalidOperationException)
            {
            }
        }

        if (!process.HasExited)
        {
            await process.WaitForExitAsync();
        }

        await standardError;
        var observedDebuggeeProcessIds = debuggeeProcessIds.Order().ToArray();
        var remainingProcessIds = new List<int>();

        foreach (var debuggeeProcessId in observedDebuggeeProcessIds)
        {
            try
            {
                using var debuggee = Process.GetProcessById(debuggeeProcessId);

                if (!debuggee.HasExited)
                {
                    remainingProcessIds.Add(debuggeeProcessId);
                }
            }
            catch (ArgumentException)
            {
            }
            catch (InvalidOperationException)
            {
            }
        }

        Cleanup = new CleanupObservation(
            processId,
            process.HasExited,
            process.HasExited ? process.ExitCode : null,
            treeKillRequested,
            observedDebuggeeProcessIds,
            remainingProcessIds);
        process.Dispose();
    }

    private async Task<JsonElement> WaitForMessageAsync(Func<JsonElement, bool> predicate)
    {
        var existingIndex = backlog.FindIndex(item => predicate(item));

        if (existingIndex >= 0)
        {
            var existing = backlog[existingIndex];
            backlog.RemoveAt(existingIndex);
            return existing;
        }

        using var timeout = new CancellationTokenSource(MessageTimeout);

        while (true)
        {
            timeout.Token.ThrowIfCancellationRequested();
            var message = await ReadMessageAsync(timeout.Token);

            if (predicate(message))
            {
                return message;
            }

            backlog.Add(message);
        }
    }

    private async Task<JsonElement> ReadMessageAsync(CancellationToken cancellationToken)
    {
        var headerBytes = new List<byte>();
        var tail = new Queue<byte>(4);

        while (true)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var buffer = new byte[1];
            var count = await output.ReadAsync(buffer, cancellationToken);

            if (count == 0)
            {
                throw new EndOfStreamException(
                    $"Debugger adapter exited before the next DAP message. stderr: {await standardError}");
            }

            headerBytes.Add(buffer[0]);
            tail.Enqueue(buffer[0]);

            if (tail.Count > 4)
            {
                tail.Dequeue();
            }

            if (tail.SequenceEqual(new byte[] { 13, 10, 13, 10 }))
            {
                break;
            }

            if (headerBytes.Count > 16_384)
            {
                throw new InvalidDataException("DAP header exceeded 16 KiB.");
            }
        }

        var rawHeader = headerBytes.ToArray();
        var header = Encoding.ASCII.GetString(rawHeader);
        var contentLengthLine = header
            .Split("\r\n", StringSplitOptions.RemoveEmptyEntries)
            .SingleOrDefault(line => line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase));

        if (contentLengthLine is null
            || !int.TryParse(contentLengthLine["Content-Length:".Length..].Trim(), out var contentLength)
            || contentLength < 0
            || contentLength > 16 * 1024 * 1024)
        {
            throw new InvalidDataException($"Invalid DAP Content-Length header: {header}");
        }

        var payload = new byte[contentLength];
        var offset = 0;

        while (offset < payload.Length)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var count = await output.ReadAsync(payload.AsMemory(offset), cancellationToken);

            if (count == 0)
            {
                throw new EndOfStreamException("Debugger adapter closed a DAP payload early.");
            }

            offset += count;
        }

        using var document = JsonDocument.Parse(payload);
        var message = document.RootElement.Clone();
        RecordFrame("adapter-to-client", rawHeader, payload, message);
        return message;
    }

    private void RecordFrame(
        string direction,
        byte[] header,
        byte[] payload,
        JsonElement message)
    {
        var timestamp = DateTimeOffset.UtcNow;
        var frame = GC.AllocateUninitializedArray<byte>(header.Length + payload.Length);
        Buffer.BlockCopy(header, 0, frame, 0, header.Length);
        Buffer.BlockCopy(payload, 0, frame, header.Length, payload.Length);
        Transcript.Add(new DapMessageObservation(timestamp, direction, message));
        RecordDebuggeeProcess(direction, message);
        RawDapFrames.Add(new RawDapFrame(
            timestamp,
            direction,
            frame.Length,
            FileHash.Sha256(frame),
            Convert.ToBase64String(frame)));
    }

    private void RecordDebuggeeProcess(string direction, JsonElement message)
    {
        if (direction != "adapter-to-client"
            || !PropertyEquals(message, "type", "event")
            || !PropertyEquals(message, "event", "process")
            || !message.TryGetProperty("body", out var body)
            || !body.TryGetProperty("systemProcessId", out var processId)
            || !processId.TryGetInt32(out var value)
            || value <= 0)
        {
            return;
        }

        debuggeeProcessIds.Add(value);
    }

    private static bool PropertyEquals(JsonElement element, string property, string expected) =>
        element.TryGetProperty(property, out var value)
        && string.Equals(value.GetString(), expected, StringComparison.Ordinal);
}
