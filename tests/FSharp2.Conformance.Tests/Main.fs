namespace FSharp2.Conformance.Tests

open System
open System.Diagnostics
open System.IO
open System.Threading
open Expecto

module ExpectoEntryPoint =
    let private runFallbackSentinel receiptPath (arguments: string array) =
        File.AppendAllLines(receiptPath, arguments)
        1

    let private startConformanceTestChild receiptPath childArgumentPath =
        let startInfo = ProcessStartInfo(TestSupport.dotnetPath)
        startInfo.UseShellExecute <- false
        startInfo.CreateNoWindow <- true
        startInfo.ArgumentList.Add(TestSupport.testAssemblyPath)
        startInfo.ArgumentList.Add("--conformance-test-child")
        startInfo.ArgumentList.Add(receiptPath)

        if File.Exists(childArgumentPath) then
            let childArgument = File.ReadAllText(childArgumentPath)

            if not (String.IsNullOrWhiteSpace(childArgument)) then
                startInfo.ArgumentList.Add(childArgument)

        Process.Start(startInfo)

    [<EntryPoint>]
    let main argv =
        let fallbackReceiptPath =
            Environment.GetEnvironmentVariable("FSharp2FallbackSentinelReceiptPath")

        if not (String.IsNullOrWhiteSpace(fallbackReceiptPath)) then
            runFallbackSentinel fallbackReceiptPath argv
        elif
            (argv.Length = 2
             || argv.Length = 3)
            && argv[0] = "--conformance-test-child"
        then
            File.WriteAllText(argv[1], string (Process.GetCurrentProcess().Id))
            Thread.Sleep(Timeout.Infinite)
            0
        elif
            argv.Length = 4
            && argv[0] = "--conformance-test-parent"
        then
            use childProcess = startConformanceTestChild argv[2] argv[3]
            File.WriteAllText(argv[1], string (Process.GetCurrentProcess().Id))
            childProcess.WaitForExit()
            childProcess.ExitCode
        elif
            argv.Length = 1
            && argv[0] = "--conformance-probe"
        then
            Console.Out.WriteLine("probe-ok")
            0
        else
            Tests.runTestsInAssemblyWithCLIArgs [] argv
