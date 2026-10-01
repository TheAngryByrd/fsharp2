namespace fsharp2.Tests

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open Expecto

module XParsecVendorTests =

    let private repositoryRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    let private parseHostProject =
        Path.Combine(
            repositoryRoot,
            "tests",
            "FSharp2.XParsec.ParseHost",
            "FSharp2.XParsec.ParseHost.fsproj"
        )

    type private ProcessResult = { ExitCode: int; Output: string }

    let private run (timeout: TimeSpan) (fileName: string) (arguments: string list) =
        let startInfo = ProcessStartInfo(fileName)
        startInfo.WorkingDirectory <- repositoryRoot
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        for argument in arguments do
            startInfo.ArgumentList.Add(argument)

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEndAsync()
        let standardError = child.StandardError.ReadToEndAsync()

        if not (child.WaitForExit(timeout)) then
            child.Kill(true)
            failtest $"{fileName} did not exit within {timeout.TotalSeconds} seconds"

        {
            ExitCode = child.ExitCode
            Output =
                standardOutput.Result
                + standardError.Result
        }

    let private publishNativeParseHost () =
        let runtime = RuntimeInformation.RuntimeIdentifier

        if
            runtime
            <> "win-x64"
            && runtime
               <> "linux-x64"
        then
            skiptest (
                "The parse host lock file covers win-x64 and linux-x64, not "
                + runtime
            )

        let output =
            Path.Combine(Path.GetTempPath(), "fsharp2-xparsec-aot", Guid.NewGuid().ToString("N"))

        let publish =
            run (TimeSpan.FromMinutes 10.0) "dotnet" [
                "publish"
                parseHostProject
                "--configuration"
                "Release"
                "--runtime"
                runtime
                "--output"
                output
            ]

        Expect.equal publish.ExitCode 0 publish.Output

        let executable =
            Path.Combine(
                output,
                if OperatingSystem.IsWindows() then
                    "FSharp2.XParsec.ParseHost.exe"
                else
                    "FSharp2.XParsec.ParseHost"
            )

        output, executable, publish.Output

    [<Tests>]
    let tests =
        testSequenced
        <| testList "XParsec vendored parser" [
            testCase
                "the parse host publishes under NativeAOT without trim or AOT warnings and parses a module"
            <| fun () ->
                let output, executable, publishOutput = publishNativeParseHost ()

                try
                    let warnings =
                        publishOutput.Split('\n')
                        |> Array.filter (fun line ->
                            line.Contains(": warning ")
                            || line.Contains(" warning IL")
                        )

                    Expect.isEmpty warnings (String.concat "\n" warnings)

                    let source = Path.Combine(output, "Sample.fs")

                    File.WriteAllText(
                        source,
                        "module Sample\n\nlet pairs = [ for i in 1 .. 3 -> (i, i * 2) ]\n"
                    )

                    let parse = run (TimeSpan.FromMinutes 1.0) executable [ source ]

                    Expect.equal parse.ExitCode 0 parse.Output
                    Expect.equal (parse.Output.Trim()) "Sample.fs diagnostics=0" parse.Output
                finally
                    Directory.Delete(output, true)
        ]
