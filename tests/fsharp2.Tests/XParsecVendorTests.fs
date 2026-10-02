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
                "-p:NativeIntermediateOutputPath="
                + Path.Combine(output, "ilc")
                + string Path.DirectorySeparatorChar
            ]

        Expect.equal publish.ExitCode 0 publish.Output

        Expect.stringContains
            publish.Output
            "Generating native code"
            "the publish must run the ILC compiler, so that its warnings are visible"

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

                    // The process must parse a measure type argument before it parses any measure constant.
                    let measureSource = Path.Combine(output, "Measure.fs")

                    File.WriteAllText(
                        measureSource,
                        "module Measure\n\nlet speed : float<m/s> = 1.0<m/s>\n"
                    )

                    let measureParse = run (TimeSpan.FromMinutes 1.0) executable [ measureSource ]

                    Expect.equal measureParse.ExitCode 0 measureParse.Output

                    Expect.equal
                        (measureParse.Output.Trim())
                        "Measure.fs tree diagnostics=0"
                        measureParse.Output
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

            testCase "diagnostic text and trace lines keep the diagnostic payload"
            <| fun () ->
                let source = "module M\n\nlet x = { A = 1 |}\nlet y = (1\n#if (A &&\n#endif\n"
                let lexed = (Lexing.lexString source).WithDefines Set.empty
                use traceText = new StringWriter()
                let trace = WriterTraceCallback(lexed.Lexed, traceText)
                let reader = Reader.ofParseInputWithTracing lexed trace

                FSharpAst.parse reader
                |> ignore

                let invalidIf =
                    "Invalid #if expression: Operator parsing failed: Not a valid LHS #if operator: Not a valid #if term"

                Expect.contains
                    (reader.State.Diagnostics
                     |> List.map _.Code)
                    (DiagnosticCode.Other invalidIf)
                    "the #if diagnostic must describe the failure, not print a type name"

                let diagnosticLines =
                    traceText.ToString().Split('\n')
                    |> Array.map _.TrimEnd()
                    |> Array.filter _.StartsWith("DIAGNOSTIC ")
                    |> Array.distinct
                    |> List.ofArray

                Expect.equal
                    diagnosticLines
                    [
                        "DIAGNOSTIC MismatchedDelimiter(KWLBrace, KWRBrace) @26"
                        "DIAGNOSTIC Other("
                        + invalidIf
                        + ") @40"
                        "DIAGNOSTIC UnclosedDelimiter(KWLParen, KWRParen) @57"
                    ]
                    "trace lines must keep the payload of each diagnostic"

            testCase "a same-line bar after an open rule body gives FSC2P1001 at the bar"
            <| fun () ->
                // Compatibility Oracle (SDK 10.0.110 fsc --parseonly): the first error of each text is FS0010 at this column.
                let rejected = [
                    "let y = match x with A -> f x |> fun y -> y | B -> 0\n", 45
                    "let y = match x with A -> while c do d | B -> 0\n", 40
                    "let y = match x with A -> z <- if c then 1 else 2 | B -> 0\n", 51
                    "let y = match x with A -> try a finally b | B -> 0\n", 43
                    "let y = match x with A -> f x, if c then 1 else 2 | B -> 0\n", 51
                    "let y = match x with A -> if c then d else e |> f | B -> 0\n", 51
                ]

                for source, column in rejected do
                    let _, diagnostics = parseVendored source

                    let bars =
                        diagnostics
                        |> List.filter (fun diagnostic ->
                            diagnostic.Code = DiagnosticCode.SameLineBarEndsBody
                        )

                    match bars with
                    | [ diagnostic ] ->
                        Expect.equal diagnostic.Token.StartIndex (column - 1) source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | other ->
                        failtest
                            $"{source} must give one SameLineBarEndsBody diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                let accepted = [
                    "let y = match x with A -> let z = 1 in z | B -> 0\n"
                    "let y = match x with A -> lazy (if c then 1 else 2) | B -> 0\n"
                    "let y = match x with A -> try a with _ -> b | B -> 0\n"
                    "let y = match x with A -> if c then d else e\n                   | B -> 0\n"
                    "let y = match x with A -> (fun z -> z) | B -> id\n"
                    "let y =\n    try f () with | _ -> ()\n"
                ]

                for source in accepted do
                    let result, diagnostics = parseVendored source
                    Expect.isOk result source
                    Expect.isEmpty diagnostics source

            testCase "a module element at another column than the first element gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle (SDK 10.0.110 fsc --parseonly): FS0010 at (4,3) with FS3118, and FS0010 at (5,7) with FS3118.
                let rejected = [
                    "module A\nlet f =\n    g\n  1\n", 4, 3
                    "module N\nmodule M =\n    let f =\n        g\n      1\n    let h = 2\n", 5, 7
                ]

                for source, line, column in rejected do
                    let _, diagnostics = parseVendored source

                    let misaligned =
                        diagnostics
                        |> List.filter (fun diagnostic ->
                            diagnostic.Code = DiagnosticCode.MisalignedModuleElement
                        )

                    match misaligned with
                    | [ diagnostic ] ->
                        let lines = source.Substring(0, diagnostic.Token.StartIndex).Split('\n')

                        Expect.equal
                            (lines.Length,
                             lines[lines.Length
                                   - 1]
                                 .Length
                             + 1)
                            (line, column)
                            source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | other ->
                        failtest
                            $"{source} must give one MisalignedModuleElement diagnostic, not {other.Length}"

            testCase "an IL intrinsic on its own line stays in the function body"
            <| fun () ->
                // FSharp.Core array2.fs `zeroCreate` has this layout. Only FSharp.Core may use `(# ... #)`.
                let source =
                    "module M\nlet z (a: int) =\n    if a < 0 then f a\n    (# \"newarr\" type (int) a : int[] #)\n"

                match parseVendored source with
                | Ok(FSharpAst.ImplementationFile(ImplementationFile.NamedModule(NamedModule.NamedModule(
                    elements = elements)))),
                  [] ->
                    Expect.equal
                        elements.Length
                        1
                        "the IL intrinsic must not become a second module element"
                | _, diagnostics ->
                    failtest
                        $"the source must give a named module and no diagnostic, not {diagnostics.Length}"

            testCase "an adjacent bracket indexes only after an identifier, as in FCS"
            <| fun () ->
                let first source =
                    match parseVendored source with
                    | Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elements)),
                      [] ->
                        match elements[0] with
                        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(
                            bindings = bindings)) -> bindings[0].expr
                        | other -> failtest $"{source} must start with a let, not {other}"
                    | _, diagnostics ->
                        failtest
                            $"{source} must give a tree and no diagnostic, not {diagnostics.Length}"

                // FCS 43.10.101 gives SynExpr.App with a list argument for these texts.
                for source in
                    [
                        "let y = (f x)[0]\n"
                        "let y = xs.[0][1]\n"
                        "let y = [ 1 ][0]\n"
                        "let y = u[i][j]\n"
                        "let y = opt.Split([| 1 |])[0]\n"
                    ] do
                    match first source with
                    | Expr.App(_, args) ->
                        match
                            args[args.Length
                                 - 1]
                        with
                        | Expr.EnclosedBlock(ParenKind.List _, _, _) -> ()
                        | other -> failtest $"{source} must apply to a list, not {other}"
                    | other -> failtest $"{source} must give an application, not {other}"

                // FCS 43.10.101 gives an index (fnorm B) for these texts.
                for source in
                    [
                        "let y = a.b[0]\n"
                        "let y = xs[0]\n"
                    ] do
                    match first source with
                    | Expr.IndexedLookup(dot = ValueNone) -> ()
                    | other -> failtest $"{source} must give an index, not {other}"

            testCase "a multiline list argument next to a closing bracket gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0058 at (2,1) for the first text. It accepts the second text,
                // but the FCS 43.10.101 `while` range ends before the `]`.
                for source in
                    [
                        "let y = g (while c do xs[0][\n1])\n"
                        "let y = g (while c do xs[0][\n            1])\n"
                    ] do
                    let _, diagnostics = parseVendored source

                    match diagnostics with
                    | [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.MultilineAdjacentList source

                        Expect.equal
                            diagnostic.Token.StartIndex
                            27
                            "the diagnostic must point at the list bracket"

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | other -> failtest $"{source} must give one diagnostic, not {other.Length}"

                let result, diagnostics = parseVendored "let y = (f x)[0]\n"
                Expect.isOk result "a list argument on one line"
                Expect.isEmpty diagnostics "a list argument on one line must give no diagnostic"

            testCase "a let body that is not at the let column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: error FS0010: Unexpected identifier in expression. Expected 'in' or other token, at (3,5).
                let source = "let f () =\n    a; let x = 1\n    x\n"

                match parseVendored source with
                | _, [ diagnostic ] ->
                    Expect.equal diagnostic.Code DiagnosticCode.MisalignedLetBody source

                    Expect.equal
                        diagnostic.Token.StartIndex
                        32
                        "the diagnostic must point at the body at (3,5)"

                    Expect.equal
                        (DiagnosticCode.fsharp2Code diagnostic.Code)
                        (ValueSome "FSC2P1001")
                        source
                | _, other -> failtest $"{source} must give one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let f () =\n    let x = 1\n    x\n"
                        "let f () =\n    match y with\n    | A ->\n        let x = 1\n        x\n    | B -> 0\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "nested records and computation expressions parse in time linear in depth"
            <| fun () ->
                let shapes = [
                    "record", deepSource "{ A = " "1" " }" 60
                    "anonymous record", deepSource "{| A = " "1" " |}" 60
                    "record copy", deepSource "{ r with A = " "1" " }" 60
                    "seq", deepSource "seq { " "1" " }" 60
                    "seq in record", deepSource "{ A = seq { " "1" " } }" 30
                    "match in record", deepSource "{ A = match x with _ -> " "1" " }" 30
                    "try in record", deepSource "{ A = try " "1" " with _ -> 0 }" 30
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
