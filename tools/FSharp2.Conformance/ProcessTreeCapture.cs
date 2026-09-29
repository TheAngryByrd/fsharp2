using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;

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
        CaptureDescendants(ProcessSnapshot.Take());
    }

    public async ValueTask DisposeAsync()
    {
        _stopping.Cancel();
        await _monitor.ConfigureAwait(false);
        _stopping.Dispose();
    }

    private async Task MonitorAsync()
    {
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

    private void CaptureDescendants(IReadOnlyDictionary<int, ProcessSnapshot.Entry> processes)
    {
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
                        : ProcessSnapshot.ReadArguments(process));
            }
            catch (Exception exception) when (
                exception is ArgumentException
                or InvalidOperationException
                or System.ComponentModel.Win32Exception)
            {
            }
        }
    }

    private bool IsDescendant(int processId, IReadOnlyDictionary<int, ProcessSnapshot.Entry> processes)
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
}

internal static class ProcessSnapshot
{
    internal sealed record Entry(int ProcessId, int ParentProcessId);

    public static Dictionary<int, Entry> Take()
    {
        if (OperatingSystem.IsWindows())
        {
            return Windows.Snapshot();
        }
        if (OperatingSystem.IsLinux())
        {
            return Linux.Snapshot();
        }
        if (OperatingSystem.IsMacOS())
        {
            return MacOS.Snapshot();
        }
        return [];
    }

    public static ImmutableArray<string> ReadArguments(Process process)
    {
        if (OperatingSystem.IsWindows())
        {
            return Windows.ReadArguments(process);
        }
        if (OperatingSystem.IsLinux())
        {
            return Linux.ReadArguments(process.Id);
        }
        if (OperatingSystem.IsMacOS())
        {
            return MacOS.ReadArguments(process.Id);
        }
        return [];
    }

    private static ImmutableArray<string> SplitNullTerminated(ReadOnlySpan<byte> bytes, int maximumCount)
    {
        var arguments = ImmutableArray.CreateBuilder<string>();
        while (arguments.Count < maximumCount && !bytes.IsEmpty)
        {
            var end = bytes.IndexOf((byte)0);
            if (end < 0)
            {
                arguments.Add(Encoding.UTF8.GetString(bytes));
                break;
            }
            arguments.Add(Encoding.UTF8.GetString(bytes[..end]));
            bytes = bytes[(end + 1)..];
        }
        return arguments.ToImmutable();
    }

    private static class Linux
    {
        public static Dictionary<int, Entry> Snapshot()
        {
            var result = new Dictionary<int, Entry>();
            foreach (var directory in Directory.EnumerateDirectories("/proc"))
            {
                if (!int.TryParse(Path.GetFileName(directory), out var processId))
                {
                    continue;
                }
                var parentProcessId = ReadParentProcessId(directory);
                if (parentProcessId.HasValue)
                {
                    result[processId] = new Entry(processId, parentProcessId.Value);
                }
            }
            return result;
        }

        public static ImmutableArray<string> ReadArguments(int processId)
        {
            try
            {
                return SplitNullTerminated(File.ReadAllBytes($"/proc/{processId}/cmdline"), int.MaxValue);
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                return [];
            }
        }

        private static int? ReadParentProcessId(string processDirectory)
        {
            string stat;
            try
            {
                stat = File.ReadAllText(Path.Combine(processDirectory, "stat"));
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                return null;
            }

            // The comm field is parenthesized and can contain spaces, so the parse starts after its closing parenthesis.
            var commandEnd = stat.LastIndexOf(')');
            if (commandEnd < 0)
            {
                return null;
            }
            var fields = stat[(commandEnd + 1)..].Split(' ', StringSplitOptions.RemoveEmptyEntries);
            return fields.Length > 1 && int.TryParse(fields[1], out var parentProcessId)
                ? parentProcessId
                : null;
        }
    }

    private static class MacOS
    {
        private const int ProcessBsdInfoFlavor = 3;
        private const int ProcessBsdInfoSize = 136;
        private const int ParentProcessIdOffset = 16;
        private const int ControlKernel = 1;
        private const int KernelArgumentMaximum = 8;
        private const int KernelProcessArguments2 = 49;

        public static Dictionary<int, Entry> Snapshot()
        {
            var result = new Dictionary<int, Entry>();
            var processIds = ListProcessIds();
            var buffer = Marshal.AllocHGlobal(ProcessBsdInfoSize);
            try
            {
                foreach (var processId in processIds)
                {
                    if (proc_pidinfo(processId, ProcessBsdInfoFlavor, 0, buffer, ProcessBsdInfoSize) != ProcessBsdInfoSize)
                    {
                        continue;
                    }
                    result[processId] = new Entry(processId, Marshal.ReadInt32(buffer, ParentProcessIdOffset));
                }
            }
            finally
            {
                Marshal.FreeHGlobal(buffer);
            }
            return result;
        }

        public static ImmutableArray<string> ReadArguments(int processId)
        {
            var argumentMaximum = ReadArgumentMaximum();
            if (argumentMaximum <= 0)
            {
                return [];
            }
            var buffer = Marshal.AllocHGlobal(argumentMaximum);
            try
            {
                var length = (nint)argumentMaximum;
                if (sysctl([ControlKernel, KernelProcessArguments2, processId], 3, buffer, ref length, IntPtr.Zero, 0) != 0
                    || length < sizeof(int))
                {
                    return [];
                }
                var bytes = new byte[length];
                Marshal.Copy(buffer, bytes, 0, (int)length);
                return ParseProcessArguments(bytes);
            }
            finally
            {
                Marshal.FreeHGlobal(buffer);
            }
        }

        private static ImmutableArray<string> ParseProcessArguments(ReadOnlySpan<byte> bytes)
        {
            // KERN_PROCARGS2 layout: int32 argc, executable path, zero padding, argc arguments, then the environment.
            var argumentCount = BitConverter.ToInt32(bytes[..sizeof(int)]);
            var remaining = bytes[sizeof(int)..];
            var executableEnd = remaining.IndexOf((byte)0);
            if (executableEnd < 0)
            {
                return [];
            }
            remaining = remaining[executableEnd..];
            var padding = remaining.IndexOfAnyExcept((byte)0);
            if (padding < 0)
            {
                return [];
            }
            return SplitNullTerminated(remaining[padding..], argumentCount);
        }

        private static int[] ListProcessIds()
        {
            var count = proc_listallpids(IntPtr.Zero, 0);
            if (count <= 0)
            {
                return [];
            }
            var capacity = count * 2;
            var buffer = Marshal.AllocHGlobal(capacity * sizeof(int));
            try
            {
                var filled = proc_listallpids(buffer, capacity * sizeof(int));
                if (filled <= 0)
                {
                    return [];
                }
                var processIds = new int[filled];
                Marshal.Copy(buffer, processIds, 0, filled);
                return processIds;
            }
            finally
            {
                Marshal.FreeHGlobal(buffer);
            }
        }

        private static int ReadArgumentMaximum()
        {
            var buffer = Marshal.AllocHGlobal(sizeof(int));
            try
            {
                var length = (nint)sizeof(int);
                return sysctl([ControlKernel, KernelArgumentMaximum], 2, buffer, ref length, IntPtr.Zero, 0) == 0
                    ? Marshal.ReadInt32(buffer)
                    : 0;
            }
            finally
            {
                Marshal.FreeHGlobal(buffer);
            }
        }

        [DllImport("libproc")]
        private static extern int proc_listallpids(IntPtr buffer, int bufferSize);

        [DllImport("libproc")]
        private static extern int proc_pidinfo(int processId, int flavor, ulong argument, IntPtr buffer, int bufferSize);

        [DllImport("libc")]
        private static extern int sysctl(int[] name, uint nameLength, IntPtr oldValue, ref nint oldLength, IntPtr newValue, nint newLength);
    }

    private static class Windows
    {
        public static Dictionary<int, Entry> Snapshot()
        {
            var result = new Dictionary<int, Entry>();
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
                    result[(int)entry.ProcessId] = new Entry(
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

        public static ImmutableArray<string> ReadArguments(Process process)
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
}
