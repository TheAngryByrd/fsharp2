using Microsoft.Build.Framework;
using Microsoft.Build.Logging.StructuredLogger;

namespace FSharp2.Conformance;

public static class MsBuildBinlogReader
{
    public static BinlogEvidence Read(string binlogPath)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(binlogPath);
        var path = Path.GetFullPath(binlogPath);
        if (!File.Exists(path))
        {
            throw new ConformanceContractException(
                [new("binlog-missing", path, "The MSBuild binary log does not exist.")]);
        }

        var invocations = new CompilerInvocationsReader().Read(path).ToArray();
        if (invocations.Length != 0)
        {
            return new BinlogEvidence(
                invocations.Length,
                false,
                [.. invocations.Select(static invocation => invocation.CommandLineArguments)]);
        }

        var coreCompileStarted = false;
        var fscTasks = 0;
        var commandLines = new List<string>();
        var replay = new BinLogReader();
        replay.TargetStarted += (_, target) =>
        {
            if (string.Equals(target.TargetName, "CoreCompile", StringComparison.OrdinalIgnoreCase))
            {
                coreCompileStarted = true;
            }
        };
        replay.TaskStarted += (_, task) =>
        {
            if (string.Equals(task.TaskName, "Fsc", StringComparison.OrdinalIgnoreCase))
            {
                fscTasks++;
            }
        };
        replay.MessageRaised += (_, message) =>
        {
            if (message is TaskCommandLineEventArgs command
                && string.Equals(command.TaskName, "Fsc", StringComparison.OrdinalIgnoreCase))
            {
                commandLines.Add(command.CommandLine);
            }
        };
        replay.Replay(path);
        if (commandLines.Count == 0 && fscTasks != 0)
        {
            commandLines.AddRange(Enumerable.Repeat("Fsc command line was not logged.", fscTasks));
        }
        return new BinlogEvidence(
            fscTasks,
            !coreCompileStarted || fscTasks == 0,
            [.. commandLines]);
    }
}
