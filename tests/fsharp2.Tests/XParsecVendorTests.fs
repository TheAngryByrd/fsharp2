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

    type private ThrowingTrace() =
        inherit TraceCallback()

        override _.TokenConsumed(_, _, _) =
            raise (InvalidOperationException "trace failure")

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

            testCase "an exception in the parser gives one FSC2P1001 diagnostic and no tree"
            <| fun () ->
                let lexed = (Lexing.lexString "module M\nlet x = 1\n").WithDefines Set.empty
                let reader = Reader.ofParseInputWithTracing lexed (ThrowingTrace())
                let result = FSharpAst.parse reader

                Expect.isError result "a parser exception must give no tree"

                match reader.State.Diagnostics with
                | [ diagnostic ] ->
                    Expect.equal
                        diagnostic.Code
                        (DiagnosticCode.ParserFault "InvalidOperationException: trace failure")
                        "the diagnostic must keep the exception"

                    Expect.equal
                        (DiagnosticCode.fsharp2Code diagnostic.Code)
                        (ValueSome "FSC2P1001")
                        "code"
                | other ->
                    failtest $"a parser exception must give one diagnostic, not {other.Length}"

            testCase
                "a cast keyword after an expression gives a diagnostic at the keyword, as FCS does"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,11), (2,11), (2,12), (2,13), (2,15), (2,14), (2,16),
                // (2,11), (2,12), and (2,13). The fourth text also gives (2,9) FS0598.
                for source, offset in
                    [
                        "module M\nlet y = g upcast x\n", 19
                        "module M\nlet y = g downcast x\n", 19
                        "module M\nlet y = (g upcast x)\n", 20
                        "module M\nlet y = [ g upcast x ]\n", 21
                        "module M\nlet y = a + b upcast c\n", 23
                        "module M\nlet f () = g upcast x\n", 22
                        "module M\nlet y = x |> g upcast\n", 24
                        "module M\nlet y = g downcast\n", 19
                        "module M\nlet y = (g downcast)\n", 20
                        "module M\nlet y = f x upcast\n", 21
                    ] do
                    let result, diagnostics = parseVendored source
                    Expect.isOk result source

                    Expect.contains
                        (diagnostics
                         |> List.map (fun diagnostic ->
                             diagnostic.Code, diagnostic.Token.StartIndex
                         ))
                        (DiagnosticCode.CastKeywordAfterExpression, offset)
                        source

                // Compatibility Oracle: FS0010 at (3,18), (4,7), (3,18), (3,8), (3,23), (3,20), (3,30), (2,41), (2,36),
                // (2,27), (4,18), and (3,18). The seventh text also gives (3,14) FS0550.
                for source, offset in
                    [
                        "module M\ntype T() =\n  member _.M = g upcast x\n", 37
                        "module M\ntype T() =\n  member _.M =\n    g upcast x\n", 41
                        "module M\ntype T() =\n  member _.M = g upcast x\nlet z = 1\n", 37
                        "module M\ntype T() =\n  do g upcast x\n", 27
                        "module M\ntype T =\n  static member M = g upcast x\n", 40
                        "module M\ntype T() =\n  member val P = g upcast x\n", 39
                        "module M\ntype T() =\n  member _.M with get () = g upcast x\n", 49
                        "module M\ntype R = { A: int } with member _.M = g upcast x\n", 49
                        "module M\ntype U = A | B with member _.M = g upcast x\n", 44
                        "module M\ntype T() = member _.M = g upcast x\n", 35
                        "module M\ntype T() =\n  member _.M = x\n  member _.N = g upcast x\n", 54
                        "module M\ntype T() =\n  member _.M = g downcast x\n  member _.N = 1\n", 37
                    ] do
                    let _, diagnostics = parseVendored source

                    Expect.equal
                        (diagnostics
                         |> List.tryHead
                         |> Option.map (fun diagnostic ->
                             diagnostic.Code, diagnostic.Token.StartIndex
                         ))
                        (Some(DiagnosticCode.CastKeywordAfterExpression, offset))
                        source

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "module M\nlet y = upcast x\n"
                        "module M\nlet y = g (upcast x)\n"
                        "module M\nlet y = a + upcast x\n"
                        "module M\ntype T() =\n  member _.M = 1\nupcast x\n"
                        "module M\nlet f () =\n  g ()\n  upcast x\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "an exception in the lexer gives one FSC2P1001 diagnostic and no tree"
            <| fun () ->
                match FSharpAst.parseText Set.empty null with
                | ParsedText.LexerFault diagnostic ->
                    Expect.equal
                        (DiagnosticCode.fsharp2Code diagnostic.Code)
                        (ValueSome "FSC2P1001")
                        "code"

                    Expect.equal
                        diagnostic.Token.Index
                        TokenIndex.Virtual
                        "a lexer fault has no token"
                | ParsedText.Lexed _ -> failtest "a null text must give a lexer fault"

                match FSharpAst.parseText Set.empty "module M\nlet x = 1\n" with
                | ParsedText.Lexed(_, reader, result) ->
                    Expect.isOk result "a valid text must give a tree"
                    Expect.isEmpty reader.State.Diagnostics "a valid text must give no diagnostic"
                | ParsedText.LexerFault diagnostic ->
                    failtest $"a valid text gave {diagnostic.Code}"

            testCase "a keyword construct after a prefix operator or a cast keyword gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,16), (2,11), (2,11), (2,13), (2,12), (2,18), (2,14), and (3,11).
                // The seventh text also gives (2,11) FS0598.
                for source, offset in
                    [
                        "module M\nlet y = upcast try a finally b\n", 24
                        "module M\nlet y = - match x with _ -> a\n", 19
                        "module M\nlet y = ! if c then a else b\n", 19
                        "module M\nlet y = ~~~ fun v -> v\n", 21
                        "module M\nlet y = %% let v = 1 in v\n", 20
                        "module M\nlet y = downcast lazy x\n", 26
                        "module M\nlet y = xs[^ yield x ]\n", 22
                        "module M\nlet y = -\n          try a finally b\n", 29
                    ] do
                    let _, diagnostics = parseVendored source

                    Expect.equal
                        (diagnostics
                         |> List.tryHead
                         |> Option.map (fun diagnostic ->
                             diagnostic.Code, diagnostic.Token.StartIndex
                         ))
                        (Some(DiagnosticCode.ConstructAfterPrefixOperator, offset))
                        source

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "module M\nlet y = - (try a finally b)\n"
                        "module M\nlet y = upcast new T()\n"
                        "module M\nlet y = - f x\n"
                        "module M\nlet y = lazy try a finally b\n"
                        "module M\nlet y = assert if c then a else b\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a keyword construct after an expression on the same line gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (1,3), (2,13), (3,18), (2,30), (2,31), (2,26), (1,11), (1,11), (2,7), and
                // (2,15). The fourth text also gives (2,43) FS0010, and the sixth text gives (2,15) FS0604.
                for source, offset in
                    [
                        "g try a finally b\n", 2
                        "module M\nlet y = a.M if c then a else b\n", 21
                        "module M\ntype T() =\n  member _.M = g match x with _ -> a\n", 37
                        "module M\nlet h = List.map (fun v -> g while c do ())\n", 38
                        "module M\nlet m = match q with A -> g x fun v -> v | B -> 1\n", 39
                        "module M\nlet c = async { return g return x }\n", 34
                        "let y = g try a finally b\n", 10
                        "let x = 1 let y = 2\n", 10
                        "let f () =\n    g match x with _ -> 1\n", 17
                        "module N =\n    let x = 1 let y = 2\n", 25
                    ] do
                    let _, diagnostics = parseVendored source

                    Expect.equal
                        (diagnostics
                         |> List.tryHead
                         |> Option.map (fun diagnostic ->
                             diagnostic.Code, diagnostic.Token.StartIndex
                         ))
                        (Some(DiagnosticCode.ConstructAfterExpression, offset))
                        source

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "module M\nlet y = for v in xs do g v\n"
                        "module M\nlet y = while g c do ()\n"
                        "module M\nlet y = let v = g x in v\n"
                        "module M\nlet y = x |> fun v -> v\n"
                        "module M\nlet y = g x; if c then a else b\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a loop that ends at a directive with no tokens after it gives a diagnostic"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (5,1) and (4,1), "#else has no matching #if".
                for source in
                    [
                        "let f () =\n    for x in 1 .. 3 do\n      a\n      b\n     #else z\n"
                        "let f () =\n    for x in 1 .. 3 do\n      a\n     #else z\n"
                    ] do
                    let _, diagnostics = parseVendored source
                    Expect.isNonEmpty diagnostics source

                    Expect.all
                        diagnostics
                        (fun diagnostic ->
                            match diagnostic.Code with
                            | DiagnosticCode.ParserFault _ -> false
                            | _ -> true
                        )
                        source

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

            testCase "an undented block start or a token after an undented close gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0058 at (2,1) and (3,5), then FS0010 at (3,3) and (2,16).
                for source, code, offset in
                    [
                        "let y = g (while c do xs[0][\n1])\n", DiagnosticCode.UndentedBlockStart, 29
                        "let f () =\n    if c then xs[0][\n    1] else z\n",
                        DiagnosticCode.UndentedBlockStart,
                        36
                        "let f () =\n    g (a\n) |> h\n", DiagnosticCode.TokenAfterUndentedClose, 22
                        "let y = [ try g (a\n        ) with _ -> z ]\n",
                        DiagnosticCode.TokenAfterUndentedClose,
                        29
                        // Compatibility Oracle: FS0058 at (4,5) for both texts.
                        "module N =\n    let x =\n      g (\n    1)\n",
                        DiagnosticCode.UndentedBlockStart,
                        37
                        "module N =\n    do\n      g (\n    1)\n",
                        DiagnosticCode.UndentedBlockStart,
                        32
                        // Compatibility Oracle: FS0058 at (3,1) for both texts.
                        "let rec y = 1\nand w = g (\n1)\n", DiagnosticCode.UndentedBlockStart, 26
                        "let rec y = 1\nand w = begin\nxs[0] end\n",
                        DiagnosticCode.UndentedBlockStart,
                        28
                    ] do
                    match parseVendored source with
                    | _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code code source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other -> failtest $"{source} must give one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = g (while c do xs[0][\n            1])\n"
                        "let f () =\n    xs[0][\n    1]\n"
                        "let f () =\n    g (a\n)\n"
                        "let f () =\n    (g (a\n) + 1)\n"
                        "module N =\n    let x =\n      g (\n     1)\n"
                        "module N =\n    do\n      g (\n     1)\n"
                        "module N =\n    module O =\n        let x = g (\n         1)\n"
                        "do\n  g (\n1)\n"
                        "let rec y = 1\nand w = g (\n 1)\n"
                        "let rec y = 1\nand w = begin\n xs[0] end\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a bar after an undented rule body or an undented else block gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,20), and FS0058 at (2,9).
                for source, code, offset in
                    [
                        "let y = match x with A -> [ 1\n            .. 3 ] | B -> z\n",
                        DiagnosticCode.BarAfterUndentedRule,
                        49
                        "let y = match x with A -> g (if c then a else\n        b) | B -> z\n",
                        DiagnosticCode.UndentedBlockStart,
                        54
                        // Compatibility Oracle: FS0058 at (2,1) for both texts, and FS0010 at (2,15).
                        "let y = (if c then a\nelse b)\n", DiagnosticCode.UndentedBlockStart, 21
                        "let y = g (if c then [\n] else z)\n", DiagnosticCode.UndentedBlockStart, 23
                        "let y = [ fun v -> [| 1\n            |]; z ]\n",
                        DiagnosticCode.TokenAfterUndentedClose,
                        38
                    ] do
                    match parseVendored source with
                    | _, diagnostic :: _ ->
                        Expect.equal diagnostic.Code code source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, [] -> failtest $"{source} must give a diagnostic"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = match x with A when c ->\n         0 + 1 | _ -> 1\n"
                        "let f () =\n    match x with\n    | P p ->\n        [ 1\n          ]\n      |> g\n    | _ -> []\n"
                        "let f () =\n    match x with\n    | A -> f {\n        a = 1 }\n    | _ -> z\n"
                        "let y = [| if c then 1\n   else 2 |]\n"
                        "let y = [| if c then 1\n   else\n  2 |]\n"
                        "let y = match x with A -> [\n            xs[0][1] ] | B -> z\n"
                        "let y = try z with _ -> [ 1; 2\n        ] |> g\n"
                        "do begin\nxs[0] end\n"
                        "let y = g (fun v -> [| 1\n            |]) z\n"
                        "let y = [ fun v -> [| 1 .. 2 .. 9\n|]; z ]\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a with or finally after a nested try on the same line gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,1), the end of the input, for both texts.
                for source, offset in
                    [
                        "let y = try try g with _ -> z with _ -> z\n", 30
                        "let y = try try g finally z with _ -> z\n", 28
                    ] do
                    match parseVendored source with
                    | _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.SameLineNestedTry source

                        Expect.equal
                            diagnostic.Token.StartIndex
                            offset
                            "the diagnostic must point at the outer 'with'"

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other -> failtest $"{source} must give one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = try match x with A -> z with _ -> z\n"
                        "let y = try try g with _ -> z\n        with _ -> z\n"
                        "let y = try (try g with _ -> z) with _ -> z\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a range after an integer literal splits a signed operator, as FCS does"
            <| fun () ->
                // FCS 43.10.101: L(RG(C[1],C[-1])) and AR(RG(C[1],C[+1])). The Compatibility Oracle accepts both.
                for source, signedStart in
                    [
                        "let y = [1..-1]\n", 12
                        "let y = [| 1..+1 |]\n", 14
                    ] do
                    let lexed = Lexing.lexString source

                    let tokens =
                        lexed.Tokens
                        |> Seq.filter (fun token ->
                            token.TokenWithoutCommentFlags
                            <> Token.Whitespace
                        )
                        |> Seq.map (fun token -> token.StartIndex, token.TokenWithoutCommentFlags)
                        |> List.ofSeq

                    Expect.contains
                        tokens
                        (signedStart
                         - 2,
                         Token.OpRange)
                        $"{source} must give a range token"

                    Expect.isTrue
                        (tokens
                         |> List.exists (fun (start, _) -> start = signedStart))
                        $"{source} must start a new token at the sign"

            testCase
                "a ..^ operator splits into a range and a from-end index after any start, as FCS does"
            <| fun () ->
                // FCS 43.10.101 gives RG(start,?IndexFromEnd) for each text. The Compatibility Oracle accepts each text.
                let tokensOf source =
                    (Lexing.lexString source).Tokens
                    |> Seq.filter (fun token ->
                        token.TokenWithoutCommentFlags
                        <> Token.Whitespace
                    )
                    |> Seq.map (fun token -> token.StartIndex, token.TokenWithoutCommentFlags)
                    |> List.ofSeq

                for source, rangeStart in
                    [
                        "let y = xs[i..^1]\n", 12
                        "let y = xs[i ..^j]\n", 13
                        "let y = xs[(i)..^1]\n", 14
                        "let y = xs.[i..^1]\n", 13
                        "let y = [i..^2]\n", 10
                        "let y = xs[..^1]\n", 11
                        "let y = a[i..^1, 0]\n", 11
                    ] do
                    let tokens = tokensOf source

                    Expect.contains
                        tokens
                        (rangeStart, Token.OpRange)
                        $"{source} must give a range token"

                    Expect.contains
                        tokens
                        (rangeStart
                         + 2,
                         Token.OpConcatenate)
                        $"{source} must give a '^' token"

                    let result, diagnostics = parseVendored source
                    Expect.isOk result source
                    Expect.isEmpty diagnostics source

                // FCS 43.10.101 gives the operator `..^-` here: X[..^-](I[i],C[1]).
                let tokens = tokensOf "let y = xs[i..^-1]\n"

                Expect.isFalse
                    (tokens
                     |> List.exists (fun (start, _) -> start = 14))
                    "`..^-` must stay one operator"

            testCase
                "a minus-class prefix binds tighter than an infix operator and looser than application, as in FCS"
            <| fun () ->
                let topExpression source =
                    match parseVendored source with
                    | Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elements)),
                      [] ->
                        match elements[0] with
                        | ModuleElem.Expression expr -> expr
                        | other -> failtest $"{source} must give an expression, not {other}"
                    | _, diagnostics ->
                        failtest
                            $"{source} must give a tree and no diagnostic, not {diagnostics.Length}"

                // FCS 43.10.101: X[*](U[-](I[a]),I[b]), X[**](U[-](I[a]),I[b]), X[**](?IndexFromEnd,I[b]),
                // X[*](U[+](I[a]),I[b]), and X[|>](U[-](I[x]),I[f]). The Compatibility Oracle accepts each text.
                for source in
                    [
                        "-a * b\n"
                        "-a ** b\n"
                        "^a ** b\n"
                        "+a * b\n"
                        "-x |> f\n"
                    ] do
                    match topExpression source with
                    | Expr.InfixApp(Expr.PrefixApp _, _, _) -> ()
                    | other ->
                        failtest
                            $"{source} must give an infix application of a prefix application, not {other}"

                // FCS 43.10.101: U[-](A(I[f],I[x])) and A(U[!](I[f]),I[x]).
                match topExpression "-f x\n" with
                | Expr.PrefixApp(_, Expr.App _) -> ()
                | other ->
                    failtest $"-f x must give a prefix application of an application, not {other}"

                match topExpression "!f x\n" with
                | Expr.App(Expr.PrefixApp _, _) -> ()
                | other ->
                    failtest $"!f x must give an application of a prefix application, not {other}"

            testCase "a loop ends before a closing delimiter left of its body and do, as in FCS"
            <| fun () ->
                // FCS 43.10.101: While(...)@1:9-1:23 for the first text, so the loop ends after `1`, before `]` at (2,1).
                // The second text gives While(...)@1:9-2:18: the closer has its opener on the same line.
                for source, doneStart in
                    [
                        "let y = while c do [ 1\n]\n", 22
                        "let y = while c do fun v ->\n        xs.[0..1]\n", 46
                    ] do
                    match parseVendored source with
                    | Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elements)),
                      [] ->
                        match elements[0] with
                        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(
                            bindings = bindings)) ->
                            match bindings[0].expr with
                            | Expr.While(doneToken = doneTok) ->
                                Expect.equal
                                    doneTok.Index
                                    TokenIndex.Virtual
                                    "the done must be virtual"

                                Expect.equal
                                    doneTok.StartIndex
                                    doneStart
                                    $"{source}: the loop must end at the FCS position"
                            | other -> failtest $"{source} must give a while loop, not {other}"
                        | other -> failtest $"{source} must start with a let, not {other}"
                    | _, diagnostics ->
                        failtest
                            $"{source} must give a tree and no diagnostic, not {diagnostics.Length}"

            testCase
                "the loop end and the bar rule skip the tokens of an inactive conditional branch"
            <| fun () ->
                // FCS 43.10.101: While(...)@2:5-7:12 and ForIn(...)@2:5-7:12, so each loop ends after the active closer.
                for source in
                    [
                        "let f () =\n    while c do\n        g (a\n#if NEVER\n)\n#else\n          )\n#endif\n"
                        "let f () =\n    for v in xs do\n        g [a\n#if NEVER\n]\n#endif\n          ]\n"
                    ] do
                    match parseVendored source with
                    | Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elements)),
                      [] ->
                        match elements[0] with
                        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(
                            bindings = bindings)) ->
                            match bindings[0].expr with
                            | Expr.While(doneToken = doneTok)
                            | Expr.ForIn(doneToken = doneTok) ->
                                Expect.isGreaterThan
                                    doneTok.StartIndex
                                    (source.LastIndexOfAny [|
                                        ')'
                                        ']'
                                    |])
                                    $"{source}: the loop must end after the active closer"
                            | other -> failtest $"{source} must give a loop, not {other}"
                        | other -> failtest $"{source} must start with a let, not {other}"
                    | _, diagnostics ->
                        failtest
                            $"{source} must give a tree and no diagnostic, not {diagnostics.Length}"

                // Compatibility Oracle: FS0010 at (4,20) and (5,20).
                for source, offset in
                    [
                        "let y = match x with A -> [ 1\n#if NEVER\n#endif\n            .. 3 ] | B -> z\n",
                        66
                        "let y = match x with A -> [ 1\n#if NEVER\n; 2\n#endif\n            .. 3 ] | B -> z\n",
                        70
                    ] do
                    match parseVendored source with
                    | _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.BarAfterUndentedRule source
                        Expect.equal diagnostic.Token.StartIndex offset source
                    | _, other -> failtest $"{source} must give one diagnostic, not {other.Length}"

            testCase "a from-end index parses in an index or a range, as in FCS"
            <| fun () ->
                // The Compatibility Oracle accepts each text. FCS 43.10.101 gives SynExpr.IndexFromEnd for each `^`.
                for source in
                    [
                        "let y = xs[1..^1]\n"
                        "let y = [1..^2]\n"
                        "let y = xs[^1]\n"
                        "let y = xs[0, ^1]\n"
                    ] do
                    match parseVendored source with
                    | Ok(FSharpAst.ImplementationFile(ImplementationFile.AnonymousModule elements)),
                      [] ->
                        let tree = sprintf "%A" elements[0]

                        Expect.stringContains
                            tree
                            "PrefixApp"
                            $"{source} must give a prefix application"

                        Expect.stringContains
                            tree
                            "OpConcatenate"
                            $"{source} must keep the '^' token"
                    | _, diagnostics ->
                        failtest
                            $"{source} must give a tree and no diagnostic, not {diagnostics.Length}"

            testCase "a lambda pattern or arrow that is not right of the fun column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,9) for each text.
                for source, offset in
                    [
                        "let y = g (fun\n        v -> v)\n", 23
                        "let y = g (fun v\n        -> v)\n", 25
                        "let f = fun x\n        y -> x\n", 22
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.UndentedLambdaHead source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = g (fun\n            v -> v)\n"
                        "let y = g (fun v ->\n        v)\n"
                        "let f = fun x\n          y -> x\n"
                        "let f =\n    xs |> List.map (fun (a,\n                         b) -> a)\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "an expression that ends a loop body and is not at the block column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,13), (3,7), and (2,13).
                for source, offset in
                    [
                        "let y = g (for x in xs do f x;\n            z)\n", 43
                        "let f () =\n    while c do f x\n      z\n", 36
                        "let y = g (for x in xs do f\n            x; z)\n", 40
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.UnalignedAfterLoop source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = g (for x in xs do f x\n           z)\n"
                        "let f () =\n    for x in xs do f x\n    z\n"
                        "let f () =\n    for x in xs do f x\n      |> ignore\n"
                        "let r = { A = for x in xs do f x\n              B = 1 }\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "an expression after an open construct that is not at the block column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,13), (2,13), (2,11), (2,14), (2,11), (2,11), (2,11), and (2,11).
                for source, offset in
                    [
                        "let y = fun v -> a;\n            b\n", 32
                        "let y = try z finally a;\n            b\n", 37
                        "let y = if a then 0 elif c then a;\n          b\n", 45
                        "let y = match x with A -> a\n             b\n", 41
                        "let y = function A -> a | B -> b\n          c\n", 43
                        "let y = try a with _ -> b\n          c\n", 36
                        "let y = if c then a else b\n          c\n", 37
                        "let y = (fun v -> a;\n          b)\n", 31
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal
                            diagnostic.Code
                            DiagnosticCode.UnalignedAfterOpenConstruct
                            source

                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y =\n    match x with\n    | A -> a\n    |> f\n"
                        "let f () =\n    if c then a\n    b\n"
                        "let y =\n    try a\n    finally b\n    c\n"
                        "let y =\n    fun v ->\n        a\n    b\n"
                        "let y =\n    match x with\n    | A -> a\n    | B -> b\n    z\n"
                        "let y = f (if c then a else b)\n          z\n"
                        "let y = if c then a\n          else b\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "a keyword on a later line right of a context that an open construct keeps gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,30), (2,13), (2,27), (3,15), and (3,12).
                for source, offset in
                    [
                        "let y = match x with A when fun v -> v\n                             -> a | B -> b\n",
                        68
                        "let y = if fun v -> v\n            then d else b\n", 34
                        "let y = if a then 0 elif fun v -> v\n                          then d else b\n",
                        62
                        "let f () =\n    for x in fun v -> v\n              do f x\n", 49
                        "let f () =\n    while fun v -> v\n           do f x\n", 43
                    ] do
                    let _, diagnostics = parseVendored source

                    match diagnostics with
                    | diagnostic :: _ ->
                        Expect.equal diagnostic.Code DiagnosticCode.KeywordAfterOpenConstruct source
                        Expect.equal diagnostic.Token.StartIndex offset source
                    | [] -> failtest $"{source} must give a diagnostic"

                // The Compatibility Oracle reports no diagnostic for these texts. The keyword is at or left of the
                // `fun` column, left of the `do` or `else` column, or right of a `finally`.
                for accepted in
                    [
                        "let y = match x with A when fun v -> v\n                            -> a | B -> b\n"
                        "let y = match x with A when for x in xs do f x\n                             -> a | B -> b\n"
                        "let y = if fun v -> v\n           then d else b\n"
                        "let y = if for x in xs do f x\n            then d else b\n"
                        "let f () =\n    for x in fun v -> v\n             do f x\n"
                        "let f () =\n    for x in if c then a else b\n              do f x\n"
                        "let y = if c then a elif try a finally b\n                                then d else b\n"
                        "let y = match x with A when try a finally b\n                                   -> a | B -> b\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "a later rule body that is not right of the first rule body on its line gives FSC2P1001"
            <| fun () ->
                let below spaces =
                    "\n"
                    + String.replicate spaces " "
                    + "z\n"

                // Compatibility Oracle: FS0010 at (2,20), (2,26), and (4,9).
                for source, offset in
                    [
                        "let y = function A -> a | B ->"
                        + below 19,
                        50
                        "let y = match x with A -> a | B ->"
                        + below 25,
                        60
                        "let y =\n    match x with\n    | A -> a | B ->\n        z\n", 53
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.UndentedLaterRuleBody source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = function A -> a | B ->"
                        + below 23
                        "let y = match x with A -> a | B ->"
                        + below 27
                        "let y =\n    match x with\n    | A -> a | B ->\n             z\n"
                        "let y = match x with A -> a | B -> b | C ->"
                        + below 28
                        "let y = match x with A ->"
                        + below 8
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a record copy line at or left of the with column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,13), (3,9), and (2,14).
                for source, offset in
                    [
                        "let y = { r with X = 1;\n            Y = z }\n", 36
                        "let f () =\n    { r with X = 1;\n        Y = z }\n", 39
                        "let y = {| r with X = 1;\n             Y = z |}\n", 38
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.UndentedCopyField source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = { r with X = 1;\n             Y = z }\n"
                        "let f () =\n    { r with X = 1;\n         Y = z }\n"
                        "let y = {| r with X = 1;\n              Y = z |}\n"
                        "let y = { X = 1;\nY = z }\n"
                        "let y =\n    { r with\n        X = 1\n        Y = z }\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "an elif or else after a then that an open construct takes, or after a nested if else, gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (1,38), (1,41), (1,30), and (1,30).
                for source, offset in
                    [
                        "let y = if c then if d then a else b else z\n", 37
                        "let y = if c then a; if d then b else e else z\n", 40
                        "let y = if fun v -> v then d else b\n", 29
                        "let y = if fun v -> v then d elif e then f else g\n", 29
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.ElseAfterOpenConstruct source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y =\n    if c then if d then a else b\n    else z\n"
                        "let y = if c then (if d then a else b) else z\n"
                        "let y = if c then match x with A -> a else z\n"
                        "let y = if c then fun v -> v else z\n"
                        "let y = if c then if d then a else b\n"
                        "let y = if c then a else if d then b else e\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a rule arrow that starts a line left of the clause column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 and FS0596 at (2,9), (2,8), and (4,5).
                for source, offset in
                    [
                        "let y = match x with A when g x\n        -> a | B -> b\n", 40
                        "let y = match x with A\n       -> a | B -> b\n", 30
                        "let f () =\n    match x with\n    | A when g x\n    -> a\n    | B -> b\n",
                        49
                    ] do
                    let _, diagnostics = parseVendored source

                    match diagnostics with
                    | [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.UndentedRuleArrow source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | other -> failtest $"{source} must give one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = match x with A when g x\n                    -> a | B -> b\n"
                        "let f () =\n    match x with\n    | A when g x\n       -> a\n    | B -> b\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a module element on the line of a let or do element gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (1,11), (1,11), and (1,9). The second text also gives (1,1) FS3118 and
                // (2,1) FS0010.
                for source, offset in
                    [
                        "let x = 1 open System\n", 10
                        "let x = 1 type T = A\n", 10
                        "do f () exception E\n", 8
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.SameLineModuleElement source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let x = 1 in x\n"
                        "open System open System.IO\n"
                        "type A = int let x = 1\n"
                        "module N = let x = 1\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "a do, rule arrow, with, or finally after an open construct on the same line gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (1,37), (1,40), (1,31), (1,41), and (1,48).
                for source, code, offset in
                    [
                        "let y = for x in if c then a else b do f x\n",
                        DiagnosticCode.KeywordAfterOpenConstruct,
                        36
                        "let y = match x with A when fun v -> v -> a | B -> b\n",
                        DiagnosticCode.KeywordAfterOpenConstruct,
                        39
                        "let y = while try a finally b do f 0\n",
                        DiagnosticCode.KeywordAfterOpenConstruct,
                        30
                        "let y = try fun () -> try a with _ -> b with _ -> c\n",
                        DiagnosticCode.SameLineNestedTry,
                        40
                        "let y = try if c then a else try b with _ -> d with _ -> e\n",
                        DiagnosticCode.SameLineNestedTry,
                        47
                    ] do
                    match parseVendored source with
                    | Ok _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code code source
                        Expect.equal diagnostic.Token.StartIndex offset source

                        Expect.equal
                            (DiagnosticCode.fsharp2Code diagnostic.Code)
                            (ValueSome "FSC2P1001")
                            source
                    | _, other ->
                        failtest $"{source} must give a tree and one diagnostic, not {other.Length}"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = for x in (try a finally b) do f x\n"
                        "let y = match x with A when g (fun v -> v) -> a | B -> b\n"
                        "let y = try if c then try a finally b else z with _ -> z\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase
                "an expression after a loop in a record or computation expression gives FSC2P1001 off the field column"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,2), (2,16), and (2,17).
                for source, offset in
                    [
                        "let w = { X = for x in xs do f x;\n Y = z }\n", 35
                        "let w = { X = for x in xs do f x\n               Y = z }\n", 48
                        "let w = seq { for x in xs do f x\n                yield 2 }\n", 49
                    ] do
                    match parseVendored source with
                    | _, diagnostic :: _ ->
                        Expect.equal diagnostic.Code DiagnosticCode.UnalignedAfterLoop source
                        Expect.equal diagnostic.Token.StartIndex offset source
                    | _, [] -> failtest $"{source} must give a diagnostic"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let w = { X = for x in xs do f x;\n          Y = z }\n"
                        "let w = { X = for x in xs do f x\n              Y = z }\n"
                        "let w = seq { for x in xs do f x\n              yield 2 }\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a let body that is not at the let column gives FSC2P1001"
            <| fun () ->
                // Compatibility Oracle: error FS0010: Unexpected identifier in expression. Expected 'in' or other token, at (3,5).
                // The same FS0010 at (3,8) when a `;` or `,` comes before the `let` on its line.
                for source, offset in
                    [
                        "let f () =\n    a; let x = 1\n    x\n", 32
                        "let f () =\n    a; let x = 1\n       x\n", 35
                        "let f () =\n    a, let x = 1\n       x\n", 35
                    ] do
                    match parseVendored source with
                    | _, [ diagnostic ] ->
                        Expect.equal diagnostic.Code DiagnosticCode.MisalignedLetBody source

                        Expect.equal
                            diagnostic.Token.StartIndex
                            offset
                            "the diagnostic must point at the body"

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
                        "let y = let x = 1\n        x\n"
                        "let f () =\n    a; (let x = 1\n        x)\n"
                    ] do
                    let result, diagnostics = parseVendored accepted
                    Expect.isOk result accepted
                    Expect.isEmpty diagnostics accepted

            testCase "a match clause token left of the clause column gives a diagnostic"
            <| fun () ->
                // Compatibility Oracle: FS0010 at (2,9), (2,13), (2,9), and (2,10) with (2,15).
                // MissingRule points at the start of the failed rule. The first text has no diagnostic at the Oracle position.
                for rejected, firstOffset, oracleOffset in
                    [
                        "let y = g (match x with A -> a | B ->\n        b)\n", 33, ValueNone
                        "let y = g (try a with _\n            -> b)\n", 22, ValueSome 36
                        "let y = g (match x with A -> a |\n        B -> b)\n", 41, ValueSome 41
                        "let y = match x with A when c\n         && d -> 0 | _ -> 1\n",
                        21,
                        ValueSome 44
                    ] do
                    match parseVendored rejected with
                    | _, first :: _ & diagnostics ->
                        Expect.equal first.Code DiagnosticCode.MissingRule rejected
                        Expect.equal first.Token.StartIndex firstOffset rejected

                        match oracleOffset with
                        | ValueSome offset ->
                            Expect.isTrue
                                (diagnostics
                                 |> List.exists (fun diagnostic ->
                                     diagnostic.Token.StartIndex = offset
                                 ))
                                $"{rejected} must give a diagnostic at offset {offset}"
                        | ValueNone -> ()
                    | _, [] -> failtest $"{rejected} must give a diagnostic"

                // The Compatibility Oracle reports no diagnostic for these texts.
                for accepted in
                    [
                        "let y = match x with A ->\n        b\n"
                        "let y = g (match x with A ->\n          b)\n"
                        "let y = match x with A when c ->\n         0 + 1 | _ -> 1\n"
                        "let y = match x with A when xs[\n                    0] -> 0\n"
                        "let f () =\n    try\n        a\n    with e\n        when c -> b\n"
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
