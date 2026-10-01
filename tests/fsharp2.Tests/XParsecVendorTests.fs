namespace fsharp2.Tests

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Threading
open Expecto
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

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

    let private deepSource (open': string) (body: string) (close: string) depth =
        "module Deep

let y = "
        + String.replicate depth open'
        + body
        + String.replicate depth close
        + "
"

    // The Compatibility Oracle (SDK 10.0.110 fsc --parseonly) accepts these three inputs with no error.
    let private evaluationDepths = [
        "Parentheses300.fs", deepSource "(" "1" ")" 300
        "Lists500.fs", deepSource "[" "1" "]" 500
        "Lambdas1000.fs", deepSource "fun x -> " "0" "" 1000
    ]

    let private onLargeStackWithin (timeout: TimeSpan) (work: unit -> 'T) =
        let mutable outcome = Unchecked.defaultof<Result<'T, exn>>

        let thread =
            Thread(
                (fun () ->
                    outcome <-
                        try
                            Ok(work ())
                        with failure ->
                            Error failure
                ),
                64
                * 1024
                * 1024
            )

        thread.IsBackground <- true
        thread.Start()

        if not (thread.Join(timeout)) then
            failtest $"The parse did not finish within {timeout.TotalSeconds} seconds"

        match outcome with
        | Ok value -> value
        | Error failure -> raise failure

    let private onLargeStack work =
        onLargeStackWithin (TimeSpan.FromMinutes 2.0) work

    let private parseVendored (source: string) =
        let lexed = Lexing.lexString source
        let reader = Reader.ofParseInput (lexed.WithDefines Set.empty)
        let result = FSharpAst.parse reader
        result, List.rev reader.State.Diagnostics

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
                    Expect.equal (parse.Output.Trim()) "Sample.fs tree diagnostics=0" parse.Output
                finally
                    Directory.Delete(output, true)

            testCase "input that nests deeper than the limit gives FSC2P1001 and no tree"
            <| fun () ->
                for name, source in evaluationDepths do
                    let result, diagnostics = onLargeStack (fun () -> parseVendored source)

                    Expect.isError result $"{name} must give no tree"

                    match diagnostics with
                    | [ diagnostic ] ->
                        Expect.equal
                            diagnostic.Code
                            (DiagnosticCode.NestingLimitExceeded Reader.MaxNestingDepth)
                            name

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            name

                        Expect.notEqual
                            diagnostic.Token.Index
                            TokenIndex.Virtual
                            $"{name} must point at a token"
                    | other -> failtest $"{name} must give one diagnostic, not {other.Length}"

            testCase "nesting inside the limit still gives a tree"
            <| fun () ->
                let result, diagnostics =
                    onLargeStack (fun () -> parseVendored (deepSource "(" "1" ")" 90))

                Expect.isOk result "90 nested parentheses must give a tree"
                Expect.isEmpty diagnostics "90 nested parentheses must give no diagnostic"

            testCase "nested records and computation expressions parse in time linear in depth"
            <| fun () ->
                let shapes = [
                    "record", deepSource "{ A = " "1" " }" 60
                    "anonymous record", deepSource "{| A = " "1" " |}" 60
                    "record copy", deepSource "{ r with A = " "1" " }" 60
                    "seq", deepSource "seq { " "1" " }" 60
                    "seq in record", deepSource "{ A = seq { " "1" " } }" 30
                ]

                for name, source in shapes do
                    let result, diagnostics =
                        onLargeStackWithin
                            (TimeSpan.FromSeconds 10.0)
                            (fun () -> parseVendored source)

                    Expect.isOk result $"{name} must give a tree"
                    Expect.isEmpty diagnostics $"{name} must give no diagnostic"

            testCase
                "the NativeAOT parse host reports the nesting limit instead of a stack overflow"
            <| fun () ->
                let output, executable, _ = publishNativeParseHost ()

                try
                    for name, source in evaluationDepths do
                        let path = Path.Combine(output, name)
                        File.WriteAllText(path, source)

                        let parse = run (TimeSpan.FromMinutes 1.0) executable [ path ]

                        Expect.equal parse.ExitCode 0 $"{name}: {parse.Output}"

                        Expect.equal
                            (parse.Output.Trim().Replace("\r\n", "\n"))
                            $"{name} no-tree diagnostics=1\n  NestingLimitExceeded FSC2P1001"
                            name
                finally
                    Directory.Delete(output, true)
        ]
