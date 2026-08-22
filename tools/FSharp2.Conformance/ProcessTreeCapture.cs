using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace FSharp2.Conformance;

internal sealed class ProcessTreeCapture : IAsyncDisposable
{
    private readonly int _rootProcessId;
    private readonly ConcurrentDictionary<(int ProcessId, long StartedAtTicks), ProcessObservation> _observations = new();
    private readonly CancellationTokenSource _stopping = new();
    private readonly Task _monitor;

    public ProcessTreeCapture(Process root, ImmutableArray<string> rootArguments)
    {
        _rootProcessId = root.Id;
        Add(root, Environment.ProcessId, rootArguments);
        _monitor = MonitorAsync();
    }

    public ImmutableArray<ProcessObservation> Observations =>
    [
        .. _observations.Values
            .OrderBy(observation => observation.ProcessId == _rootProcessId ? 0 : 1)
            .ThenBy(observation => observation.StartedAt)
            .ThenBy(observation => observation.ProcessId),
    ];

    public void CaptureNow()
    {
        if (OperatingSystem.IsWindows())
        {
            CaptureWindowsDescendants();
        }
    }

    public async ValueTask DisposeAsync()
    {
        _stopping.Cancel();
        await _monitor.ConfigureAwait(false);
        _stopping.Dispose();
    }

    private async Task MonitorAsync()
    {
        if (!OperatingSystem.IsWindows())
        {
            return;
        }
        try
        {
            while (!_stopping.IsCancellationRequested)
            {
                CaptureNow();
                await Task.Delay(TimeSpan.FromMilliseconds(10), _stopping.Token).ConfigureAwait(false);
            }
        }
        catch (OperationCanceledException) when (_stopping.IsCancellationRequested)
        {
        }
    }

    private void CaptureWindowsDescendants()
    {
        var processes = SnapshotWindowsProcesses();
        foreach (var entry in processes.Values)
        {
            if (entry.ProcessId != _rootProcessId
                && !IsDescendant(entry.ProcessId, processes))
            {
                continue;
            }
            try
            {
                using var process = Process.GetProcessById(entry.ProcessId);
                Add(
                    process,
                    entry.ProcessId == _rootProcessId ? Environment.ProcessId : entry.ParentProcessId,
                    entry.ProcessId == _rootProcessId
                        ? _observations.Values.First(observation => observation.ProcessId == _rootProcessId).Arguments
                        : ReadArguments(process));
            }
            catch (Exception exception) when (
                exception is ArgumentException
                or InvalidOperationException
                or System.ComponentModel.Win32Exception)
            {
            }
        }
    }

    private bool IsDescendant(int processId, IReadOnlyDictionary<int, ProcessEntry> processes)
    {
        var seen = new HashSet<int>();
        var current = processId;
        while (seen.Add(current) && processes.TryGetValue(current, out var entry))
        {
            if (entry.ParentProcessId == _rootProcessId)
            {
                return true;
            }
            current = entry.ParentProcessId;
        }
        return false;
    }

    private void Add(Process process, int? parentProcessId, ImmutableArray<string> arguments)
    {
        var startedAt = new DateTimeOffset(process.StartTime).ToUniversalTime();
        var executablePath = process.MainModule?.FileName ?? process.ProcessName;
        var observation = new ProcessObservation(
            process.Id,
            parentProcessId,
            executablePath,
            arguments)
        {
            StartedAt = startedAt,
        };
        _observations[(process.Id, startedAt.UtcTicks)] = observation;
    }

    private static Dictionary<int, ProcessEntry> SnapshotWindowsProcesses()
    {
        var result = new Dictionary<int, ProcessEntry>();
        var snapshot = CreateToolhelp32Snapshot(0x00000002, 0);
        if (snapshot == new IntPtr(-1))
        {
            return result;
        }
        try
        {
            var entry = new ProcessEntry32 { Size = (uint)Marshal.SizeOf<ProcessEntry32>() };
            if (!Process32First(snapshot, ref entry))
            {
                return result;
            }
            do
            {
                result[(int)entry.ProcessId] = new ProcessEntry(
                    (int)entry.ProcessId,
                    (int)entry.ParentProcessId);
                entry.Size = (uint)Marshal.SizeOf<ProcessEntry32>();
            }
            while (Process32Next(snapshot, ref entry));
            return result;
        }
        finally
        {
            CloseHandle(snapshot);
        }
    }

    private static ImmutableArray<string> ReadArguments(Process process)
    {
        var commandLine = ReadCommandLine(process);
        if (string.IsNullOrWhiteSpace(commandLine))
        {
            return [];
        }
        var values = CommandLineToArgvW(commandLine, out var count);
        if (values == IntPtr.Zero)
        {
            return [commandLine];
        }
        try
        {
            var arguments = ImmutableArray.CreateBuilder<string>(count);
            for (var index = 0; index < count; index++)
            {
                var value = Marshal.ReadIntPtr(values, index * IntPtr.Size);
                arguments.Add(Marshal.PtrToStringUni(value) ?? string.Empty);
            }
            return arguments.ToImmutable();
        }
        finally
        {
            LocalFree(values);
        }
    }

    private static string? ReadCommandLine(Process process)
    {
        _ = NtQueryInformationProcess(process.Handle, 60, IntPtr.Zero, 0, out var length);
        if (length <= 0)
        {
            return null;
        }
        var buffer = Marshal.AllocHGlobal(length);
        try
        {
            var status = NtQueryInformationProcess(process.Handle, 60, buffer, length, out _);
            if (status < 0)
            {
                return null;
            }
            var value = Marshal.PtrToStructure<UnicodeString>(buffer);
            return value.Buffer == IntPtr.Zero
                ? null
                : Marshal.PtrToStringUni(value.Buffer, value.Length / 2);
        }
        finally
        {
            Marshal.FreeHGlobal(buffer);
        }
    }

    private sealed record ProcessEntry(int ProcessId, int ParentProcessId);

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    private struct ProcessEntry32
    {
        public uint Size;
        public uint Usage;
        public uint ProcessId;
        public IntPtr DefaultHeapId;
        public uint ModuleId;
        public uint Threads;
        public uint ParentProcessId;
        public int PriorityClassBase;
        public uint Flags;

        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 260)]
        public string ExecutableFile;
    }

    [StructLayout(LayoutKind.Sequential)]
    private readonly struct UnicodeString
    {
        public readonly ushort Length;
        public readonly ushort MaximumLength;
        public readonly IntPtr Buffer;
    }

    [DllImport("kernel32.dll", SetLastError = true)]
    private static extern IntPtr CreateToolhelp32Snapshot(uint flags, uint processId);

    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool Process32First(IntPtr snapshot, ref ProcessEntry32 entry);

    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool Process32Next(IntPtr snapshot, ref ProcessEntry32 entry);

    [DllImport("kernel32.dll", SetLastError = true)]
    [return: MarshalAs(UnmanagedType.Bool)]
    private static extern bool CloseHandle(IntPtr handle);

    [DllImport("ntdll.dll")]
    private static extern int NtQueryInformationProcess(
        IntPtr processHandle,
        int processInformationClass,
        IntPtr processInformation,
        int processInformationLength,
        out int returnLength);

    [DllImport("shell32.dll", SetLastError = true)]
    private static extern IntPtr CommandLineToArgvW(
        [MarshalAs(UnmanagedType.LPWStr)] string commandLine,
        out int argumentCount);

    [DllImport("kernel32.dll")]
    private static extern IntPtr LocalFree(IntPtr value);
}
