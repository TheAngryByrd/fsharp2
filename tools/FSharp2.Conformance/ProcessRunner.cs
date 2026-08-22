using System.Collections.Immutable;
using System.Diagnostics;

namespace FSharp2.Conformance;

public static class ProcessRunner
{
    public static async Task<ProcessResult> RunAsync(
        ProcessSpec specification,
        CancellationToken cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(specification);
        if (specification.Timeout <= TimeSpan.Zero)
        {
            throw new ArgumentOutOfRangeException(nameof(specification), "The process timeout must be positive.");
        }

        Directory.CreateDirectory(specification.WorkingDirectory);
        Directory.CreateDirectory(Path.GetDirectoryName(specification.StandardOutputPath)!);
        Directory.CreateDirectory(Path.GetDirectoryName(specification.StandardErrorPath)!);
        var startInfo = new ProcessStartInfo(specification.FileName)
        {
            WorkingDirectory = specification.WorkingDirectory,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
        };
        foreach (var argument in specification.Arguments)
        {
            startInfo.ArgumentList.Add(argument);
        }
        foreach (var variable in specification.Environment)
        {
            startInfo.Environment[variable.Key] = variable.Value;
        }

        var startedAt = DateTimeOffset.UtcNow;
        using var process = new Process { StartInfo = startInfo };
        process.Start();
        await using var processTree = new ProcessTreeCapture(process, specification.Arguments);
        await using var standardOutput = new MemoryStream();
        await using var standardError = new MemoryStream();
        var outputRead = process.StandardOutput.BaseStream.CopyToAsync(standardOutput, CancellationToken.None);
        var errorRead = process.StandardError.BaseStream.CopyToAsync(standardError, CancellationToken.None);
        var timedOut = false;
        using var timeout = new CancellationTokenSource(specification.Timeout);
        using var wait = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeout.Token);
        try
        {
            await process.WaitForExitAsync(wait.Token).ConfigureAwait(false);
        }
        catch (OperationCanceledException) when (timeout.IsCancellationRequested && !cancellationToken.IsCancellationRequested)
        {
            timedOut = true;
            processTree.CaptureNow();
            Kill(process);
        }
        catch
        {
            Kill(process);
            throw;
        }

        await Task.WhenAll(outputRead, errorRead).ConfigureAwait(false);
        var outputBytes = standardOutput.ToArray();
        var errorBytes = standardError.ToArray();
        await File.WriteAllBytesAsync(specification.StandardOutputPath, outputBytes, CancellationToken.None)
            .ConfigureAwait(false);
        await File.WriteAllBytesAsync(specification.StandardErrorPath, errorBytes, CancellationToken.None)
            .ConfigureAwait(false);
        if (cancellationToken.IsCancellationRequested && !timedOut)
        {
            cancellationToken.ThrowIfCancellationRequested();
        }

        return new ProcessResult(
            process.ExitCode,
            timedOut,
            startedAt,
            DateTimeOffset.UtcNow,
            [.. outputBytes],
            [.. errorBytes],
            processTree.Observations);
    }

    private static void Kill(Process process)
    {
        if (!process.HasExited)
        {
            process.Kill(entireProcessTree: true);
        }
        process.WaitForExit();
    }
}
