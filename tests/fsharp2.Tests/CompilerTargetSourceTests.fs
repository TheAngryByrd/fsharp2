namespace fsharp2.Tests

open System
open System.Diagnostics
open System.IO
open System.Text
open Expecto

module CompilerTargetSourceTests =
    type private InvocationResult = {
        ExitCode: int
        StandardOutput: string
        StandardError: string
    }

    let private repositoryRoot =
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    let private configuration =
        if
            AppContext.BaseDirectory.Contains(
                $"{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}",
                StringComparison.OrdinalIgnoreCase
            )
        then
            "Release"
        else
            "Debug"

    let private invokeFsc2 workingDirectory arguments =
        let responsePath = Path.Combine(workingDirectory, $"compile-{Guid.NewGuid():N}.rsp")
        File.WriteAllLines(responsePath, arguments)

        let startInfo = ProcessStartInfo("dotnet")
        startInfo.WorkingDirectory <- workingDirectory
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        for argument in
            [
                "run"
                "--no-build"
                "--configuration"
                configuration
                "--project"
                $"{repositoryRoot}/src/fsc2.Prototype/fsc2.Prototype.fsproj"
                "--"
                "@"
                + responsePath
            ] do
            startInfo.ArgumentList.Add argument

        use child = new Process(StartInfo = startInfo)

        child.Start()
        |> ignore

        let standardOutput = child.StandardOutput.ReadToEndAsync()
        let standardError = child.StandardError.ReadToEndAsync()

        if not (child.WaitForExit(30_000)) then
            child.Kill(true)
            failtest "fsc2 did not exit within 30 seconds"

        {
            ExitCode = child.ExitCode
            StandardOutput = standardOutput.Result
            StandardError = standardError.Result
        }

    let private withRoot name action =
        let root =
            Path.Combine(Path.GetTempPath(), $"fsharp2-issue28-{name}-{Guid.NewGuid():N}")

        Directory.CreateDirectory(root)
        |> ignore

        try
            action root
        finally
            Directory.Delete(root, true)

    let private compile root mode sourceName =
        let sourcePath = Path.Combine(root, sourceName)
        let outputPath = Path.Combine(root, "SourceCase.dll")
        let pdbPath = Path.Combine(root, "SourceCase.pdb")

        let result =
            invokeFsc2 root [|
                "--nologo"
                "--target:library"
                "--deterministic+"
                "--debug:portable"
                $"--langversion:{mode}"
                $"--out:{outputPath}"
                $"--pdb:{pdbPath}"
                sourcePath
            |]

        result, outputPath

    [<Tests>]
    let tests =
        testList "Issue28.CompilerTargetSource" [
            testCase "selects a supported language mode and rejects an unknown mode"
            <| fun _ ->
                withRoot
                    "language"
                    (fun root ->
                        let sourcePath = Path.Combine(root, "SourceCase.fs")
                        File.WriteAllText(sourcePath, "module SourceCase\nlet answer () = 42\n")
                        let supported, supportedOutput = compile root "10.0" "SourceCase.fs"

                        Expect.equal
                            supported.ExitCode
                            0
                            (supported.StandardOutput
                             + supported.StandardError)

                        Expect.isTrue (File.Exists supportedOutput) "Supported language output"

                        File.Delete supportedOutput
                        let unknown, unknownOutput = compile root "11.0" "SourceCase.fs"
                        Expect.equal unknown.ExitCode 1 "Unknown language exit"

                        Expect.stringContains
                            unknown.StandardError
                            "Language version '11.0' is not supported."
                            "Unknown language diagnostic"

                        Expect.isFalse
                            (File.Exists unknownOutput)
                            "Unknown language publishes no output"
                    )

            testCase "decodes UTF-16 source and selects conditional compatibility text"
            <| fun _ ->
                withRoot
                    "encoding"
                    (fun root ->
                        let sourcePath = Path.Combine(root, "Encoded.fs")

                        let text =
                            "module Encoded\r\n#if FEATURE\r\nlet answer () = )\r\n#else\r\nlet answer () = 42\r\n#endif\r\n"

                        let bytes =
                            Array.append
                                (Encoding.Unicode.GetPreamble())
                                (Encoding.Unicode.GetBytes text)

                        File.WriteAllBytes(sourcePath, bytes)
                        let result, outputPath = compile root "10.0" "Encoded.fs"

                        Expect.equal
                            result.ExitCode
                            0
                            (result.StandardOutput
                             + result.StandardError)

                        Expect.isTrue (File.Exists outputPath) "UTF-16 conditional source output"
                    )
        ]
