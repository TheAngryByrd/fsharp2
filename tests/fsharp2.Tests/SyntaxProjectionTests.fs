namespace fsharp2.Tests

open System
open System.Collections.Immutable
open System.Security.Cryptography
open System.Text
open Expecto
open FSharp2.Compiler

module SyntaxProjectionTests =
    let private language =
        LanguageVersion.normalize (Some "10.0")
        |> Result.defaultWith failtest

    let private byteFingerprint (values: byte array) =
        values
        |> SHA256.HashData
        |> Convert.ToHexString
        |> _.ToLowerInvariant()

    let private fingerprint (text: string) =
        text
        |> Encoding.UTF8.GetBytes
        |> byteFingerprint

    let private withoutChecksum (modules: ParsedModule list) =
        modules
        |> List.map (fun parsedModule -> {
            parsedModule with
                SourceChecksum = ImmutableArray.Empty
        })

    let private document logicalPath text =
        SourceSnapshot.Create(StableIdentity.create logicalPath, logicalPath, text, "content")
        |> LexicalPipeline.prepare language Array.empty

    let private project text =
        match SyntaxRouting.tryProject ImplicitModule.Rejected (document "Program.fs" text) with
        | Some modules -> SyntaxProjectionResult.Projected modules
        | None ->
            SyntaxProjectionResult.ProjectionUnsupported {
                Start = { Offset = 0; Line = 1; Column = 1 }
                End = { Offset = 0; Line = 1; Column = 1 }
            }

    let private prototype text =
        Frontend.parse [] {
            Path = "Program.fs"
            Text = text
            ContentFingerprint = "content"
        }

    let private bodies = [
        "42"
        "0"
        "2147483647"
        "\"text\""
        "\"\""
        "true"
        "false"
        "()"
        "value"
        "argv"
        "1 + 2"
        "f x"
        "\"a\\\\b\""
        "2147483648"
        "0x10"
        "1L"
        "(1)"
        "[ 1 ]"
        "Values.answer"
    ]

    let private bindings = [
        fun body -> $"let answer = {body}"
        fun body -> $"let run () = {body}"
        fun body -> $"let echo value = {body}"
        fun body -> $"let echo (value: int) = {body}"
        fun body -> $"let private hidden = {body}"
        fun body -> $"[<EntryPoint>]\nlet main argv = {body}"
    ]

    let private layouts = [
        fun (lines: string list) ->
            String.Join("\n", lines)
            + "\n"
        fun (lines: string list) -> String.Join("\r\n", lines)
        fun (lines: string list) ->
            "// leading comment\n"
            + String.Join("\n\n", lines)
            + "\n"
    ]

    let private lexicalCorpus = [
        "module M\n(* c *)\nlet x = 1\n"
        "module M\nlet x = 1 (* c *)\n"
        "module M\nlet ``x`` = 1\n"
        "module ``M``\nlet x = 1\n"
        "module M\nlet f ``p`` = ``p``\n"
        "module M\nlet assembly = 1\n"
        "module M\nlet f assembly = 1\n"
        "module assembly\nlet x = 1\n"
        "module M\nlet x = isNull\n"
        "module M\nlet x = isNull\nlet y = 1\n"
        "module M\nlet x = ignore\n"
        "module M\nlet s = \"a\nb\"\n"
        "module M\nlet s = \"a\r\nb\"\n"
        "module M\rlet x = 1\r"
        "module M // c\rlet x = 1"
        "module M\nlet x = 'a'\n"
        "module M\nlet x = 0_1\n"
        "module M\nlet x = 007\n"
        "module M\n\tlet x = 1\n"
        "module M\nlet x = 1   \n"
        "module M\nlet x = \"// not a comment\"\n"
        "module M\nlet x = \"(* not a comment *)\"\n"
        "module M\nlet x' = 1\n"
        "module M\nlet x = y'\n"
        "module M\nlet λ = 1\n"
        "module M\n[<EntryPoint;>]\nlet main argv = 0\n"
        "module M\n[<EntryPoint; >]\nlet main argv = 0\n"
        "module M\nlet f x = 1\n[<EntryPoint;>]\nlet main argv = f\n"
        "module M\n[< EntryPoint >]\nlet main argv = 0\n"
    ]

    let private corpus = [
        yield! lexicalCorpus

        for header in
            [
                "module Program"
                "module Values"
                "module Sample.Values"
                "namespace Sample"
            ] do
            for layout in layouts do
                for binding in bindings do
                    for body in bodies do
                        yield
                            layout [
                                header
                                binding body
                            ]

                        yield
                            layout [
                                header
                                binding body
                                "let after = 1"
                            ]

                        yield
                            layout [
                                header
                                "let before = 1"
                                binding body
                            ]
    ]

    let private references () =
        let reference name (path: string) =
            let image = IO.File.ReadAllBytes path

            TargetReferenceSnapshot.Create(
                StableIdentity.create $"reference:{name}",
                $"{name}.dll",
                image,
                byteFingerprint image
            )

        ImmutableArray.Create(
            reference "System.Runtime" (Reflection.Assembly.Load("System.Runtime").Location),
            reference
                "FSharp.Core"
                typeof<Microsoft.FSharp.Core.EntryPointAttribute>.Assembly.Location
        )
        |> ReferenceTypeIndex.Create
        |> Result.defaultWith failtest

    let private snapshot (logicalPath: string) (text: string) =
        SourceSnapshot.Create(
            StableIdentity.create $"source:{logicalPath}",
            logicalPath,
            text,
            fingerprint text
        )

    let private compileFilesWithService target (files: (string * string) list) =
        let service = CompilerService()

        let result =
            service.Compile(
                "Program",
                language,
                [],
                references (),
                target,
                files
                |> List.map (fun (logicalPath, text) -> snapshot logicalPath text)
            )

        result, service.Statistics.SyntaxProjections

    let private compileWithService (text: string) =
        compileFilesWithService CompilationTarget.Executable [ "Program.fs", text ]

    let private implicitModule () =
        ImplicitModule.Accepted (references ()).Namespaces

    let private projectImplicit logicalPath text =
        SyntaxRouting.tryProject (implicitModule ()) (document logicalPath text)

    let private noOpenLine = "// no open line"

    let private implicitOpens = [
        []
        [ "System.Text" ]
        [ "System.Collections" ]
        [
            "Microsoft.FSharp.Core"
            "System.Text"
        ]
        [
            "System.Collections"
            "System.Collections"
        ]
    ]

    let private implicitCorpus = [
        for opens in implicitOpens do
            let openLines =
                match opens with
                | [] -> [ noOpenLine ]
                | opens ->
                    opens
                    |> List.map (fun name -> $"open {name}")

            for layout in layouts do
                for body in bodies do
                    yield
                        opens,
                        layout [
                            yield! openLines
                            "[<EntryPoint>]"
                            $"let main argv = {body}"
                        ]

                    for binding in List.take 5 bindings do
                        yield
                            opens,
                            layout [
                                yield! openLines
                                binding body
                                "[<EntryPoint>]"
                                "let main argv = 0"
                            ]
    ]

    let private withNamedHeader (text: string) =
        let openLine =
            Text.RegularExpressions.Regex(
                $"^(open [A-Za-z.]+|{noOpenLine})",
                Text.RegularExpressions.RegexOptions.Multiline
            )

        let first = openLine.Match text

        let blank =
            openLine.Replace(
                text,
                Text.RegularExpressions.MatchEvaluator(fun found ->
                    String.replicate found.Length " "
                )
            )

        blank.Substring(0, first.Index)
        + "module Program".PadRight first.Length
        + blank.Substring(
            first.Index
            + first.Length
        )

    let private compileRequest (files: (string * string) list) =
        let referenceSnapshot name (path: string) =
            let image = IO.File.ReadAllBytes path

            TargetReferenceSnapshot.Create(
                StableIdentity.create $"reference:{name}",
                $"{name}.dll",
                image,
                byteFingerprint image
            )

        let request =
            CompilationRequest.Create(
                CompilerContract.Version,
                StableIdentity.create "request:program",
                CompilationAssemblyIdentity.Create(
                    StableIdentity.create "assembly:Program",
                    "Program"
                ),
                [| for logicalPath, text in files -> snapshot logicalPath text |],
                [|
                    referenceSnapshot
                        "System.Runtime"
                        (Reflection.Assembly.Load("System.Runtime").Location)
                    referenceSnapshot
                        "FSharp.Core"
                        typeof<Microsoft.FSharp.Core.EntryPointAttribute>.Assembly.Location
                |],
                SemanticOptions.Create([||], None, OptimizationMode.Disabled, false, false, None),
                DiagnosticOptions.Create(None, [||], false, [||]),
                EmissionOptions.Create(
                    CompilationTarget.Executable,
                    true,
                    false,
                    DebugFormat.None,
                    [||],
                    [| for logicalPath, _ in files -> logicalPath |],
                    [||]
                ),
                SigningOptions.Create(SigningMode.Unsigned, [||]),
                ResourceInputs.Create([||], [||]),
                [| RequestedArtifact.ImplementationAssembly |]
            )

        Compiler().Compile(request, Threading.CancellationToken.None)

    let private typeDefinitions (image: ImmutableArray<byte>) =
        use reader = new Reflection.PortableExecutable.PEReader(image)
        let metadata = Reflection.Metadata.PEReaderExtensions.GetMetadataReader reader

        metadata.TypeDefinitions
        |> Seq.map (fun handle ->
            let definition = metadata.GetTypeDefinition handle

            metadata.GetString definition.Namespace,
            metadata.GetString definition.Name,
            definition.Attributes
            &&& Reflection.TypeAttributes.VisibilityMask
        )
        |> List.ofSeq

    [<Tests>]
    let tests =
        testList "Issue29.SyntaxProjection" [
            testCase "the compiler service parses a projected module with the syntax parser"
            <| fun _ ->
                let result, projections =
                    compileWithService
                        "module Program\nlet answer = 42\n[<EntryPoint>]\nlet main argv = 0\n"

                Expect.isOk result "The projected module compiles"
                Expect.equal projections 1 "The syntax parser produced the parsed module"

            testCase "the compiler service keeps the prototype parser for other modules"
            <| fun _ ->
                for text in
                    [
                        "namespace Sample\nmodule Values =\n    let answer = 42\n"
                        "module Program\nlet answer = 1 + 2\n"
                        "#nowarn \"25\"\nmodule Program\nlet answer = 42\n"
                        "module Program\nlet broken = )\n"
                    ] do
                    let _, projections = compileWithService text

                    Expect.equal projections 0 $"The prototype parser handles this source:\n{text}"

            testCase "every projected module equals the prototype parser result"
            <| fun _ ->
                let mutable projected = 0

                for text in corpus do
                    match project text with
                    | SyntaxProjectionResult.ProjectionUnsupported _ -> ()
                    | SyntaxProjectionResult.Projected modules ->
                        projected <-
                            projected
                            + 1

                        match prototype text with
                        | Error diagnostic ->
                            failtest
                                $"The projection accepted a source that the prototype parser rejects with {diagnostic.Code}: {diagnostic.Message}\n{text}"
                        | Ok expected ->
                            Expect.equal
                                (withoutChecksum modules)
                                (withoutChecksum expected)
                                $"The projection must equal the prototype parser result for:\n{text}"

                Expect.isGreaterThan projected 0 "The corpus contains projected sources"

            testCase
                "every name that the prototype parser matches in a pattern or a string comparison is excluded from projection"
            <| fun _ ->
                let frontend =
                    IO.File.ReadAllText(
                        IO.Path.Combine(
                            __SOURCE_DIRECTORY__,
                            "..",
                            "..",
                            "src",
                            "fsc2.Prototype",
                            "Frontend.fs"
                        )
                    )

                let names pattern =
                    Text.RegularExpressions.Regex.Matches(frontend, pattern)
                    |> Seq.map (fun found -> found.Groups[1].Value)
                    |> Set.ofSeq

                let matched =
                    Set.unionMany [
                        names "\| Identifier \"([^\"]+)\""
                        names "(?<!ToString\(\) )(?:=|<>) \"([A-Za-z_][A-Za-z0-9_]*)\""
                        names "\|\s*\"([A-Za-z_][A-Za-z0-9_]*)\"\s*(?:->|when|\|)"
                    ]

                Expect.isEmpty
                    (Set.difference matched (Set.add "EntryPoint" Frontend.specialIdentifiers))
                    "Each name that Frontend.parse matches must stay on the prototype path, except EntryPoint, which the projection checks as an exact attribute list"

            testCase "the syntax parser runs only on sources with a projectable token shape"
            <| fun _ ->
                for text in
                    [
                        "namespace Sample\nmodule Values =\n    let answer = 42\n"
                        "module Program\nopen System\nlet answer = 42\n"
                        "module Program\ntype Point = { X: int }\n"
                        "module Program\nlet answer = 1 + 2\n"
                        "module Program\nlet answer = f x\n"
                        "module Program\nlet rec answer = 42\n"
                        "module Program\nlet answer = match x with | A -> 1\n"
                        "module Program\n[<Literal>]\nlet answer = 42\n"
                        "module Program\nlet answer: int = 42\n"
                        "module Program\n"
                        "let answer = 42\n"
                    ] do
                    Expect.isFalse
                        (SyntaxRouting.isEligible
                            ImplicitModule.Rejected
                            (document "Program.fs" text))
                        $"The syntax parser must not run on this source:\n{text}"

                Expect.isFalse
                    (SyntaxRouting.isEligible
                        ImplicitModule.Rejected
                        (document "Program.fsi" "module Program\nlet answer = 42\n"))
                    "The syntax parser must not run on a signature file"

            testCase "the token shape check accepts every source that the projection accepts"
            <| fun _ ->
                let mutable projectable = 0

                for text in corpus do
                    let source = document "Program.fs" text
                    let syntax = Parser.parseImplementationFile source

                    if
                        syntax.Diagnostics.IsEmpty
                        && source.Directives.IsEmpty
                        && source.Diagnostics.IsEmpty
                        && Frontend.isTokenizedLikeLexicalDocument source
                    then
                        match
                            SyntaxProjection.project
                                ImplicitModule.Rejected
                                "content"
                                ImmutableArray.Empty
                                syntax.File
                        with
                        | SyntaxProjectionResult.Projected _ ->
                            projectable <-
                                projectable
                                + 1

                            Expect.isTrue
                                (SyntaxRouting.hasProjectableTokenShape
                                    ImplicitModule.Rejected
                                    source)
                                $"The token shape check must accept a projectable source:\n{text}"

                            Expect.isFalse
                                (SyntaxRouting.isImplicitModuleCandidate (implicitModule ()) source)
                                $"The parse key of a named module must not depend on the references:\n{text}"
                        | SyntaxProjectionResult.ProjectionUnsupported _ -> ()

                Expect.isGreaterThan projectable 0 "The corpus contains projectable sources"

            testCase "each supported construct is projected"
            <| fun _ ->
                for text in
                    [
                        "module Program\nlet answer = 42\n"
                        "module Program\nlet run () = \"text\"\n"
                        "module Program\nlet echo value = value\n"
                        "module Program\nlet flag = true\n"
                        "module Program\nlet unit = ()\n"
                        "module Program\nlet before = 1\n[<EntryPoint>]\nlet main argv = 0\n"
                    ] do
                    match project text with
                    | SyntaxProjectionResult.Projected _ -> ()
                    | SyntaxProjectionResult.ProjectionUnsupported range ->
                        failtest
                            $"Expected a projection, but the projection stopped at line {range.Start.Line}, column {range.Start.Column}:\n{text}"

            testCase
                "the compiler service parses an implicit module in the last file of an executable"
            <| fun _ ->
                // The Compatibility Oracle accepts each text. It also accepts a namespace that has only internal types, and a namespace prefix that has only such namespaces.
                for text in
                    [
                        "open System\n\n[<EntryPoint>]\nlet main argv = 0\n"
                        "open Microsoft.FSharp.Primitives.Basics\n\n[<EntryPoint>]\nlet main argv = 0\n"
                        "open Microsoft.FSharp.Text\n\n[<EntryPoint>]\nlet main argv = 0\n"
                        "[<EntryPoint>]\nlet main argv = 0\n"
                        "let before = 1\n[<EntryPoint>]\nlet main argv = 0\n"
                        "// For more information see https://aka.ms/fsharp-console-apps\r\n[<EntryPoint>]\r\nlet main argv = 0\r\n"
                    ] do
                    let result, projections = compileWithService text

                    Expect.isOk result $"The implicit module compiles:\n{text}"

                    Expect.equal
                        projections
                        1
                        $"The syntax parser produced the parsed module:\n{text}"

            testCase "the compiler service parses an implicit module after a named module"
            <| fun _ ->
                let result, projections =
                    compileFilesWithService CompilationTarget.Executable [
                        "Lib.fs", "module Lib\nlet answer = 42\n"
                        "Program.fs", "[<EntryPoint>]\nlet main argv = 0\n"
                    ]

                Expect.isOk result "The Compatibility Oracle compiles the two files"
                Expect.equal projections 2 "The syntax parser produced both parsed modules"

            testCase
                "an implicit module with the name of an earlier module is rejected (Oracle FS0248)"
            <| fun _ ->
                let result =
                    compileRequest [
                        "Program.fs", "module Program\nlet answer = 42\n"
                        "src/Program.fs", "[<EntryPoint>]\nlet main argv = 0\n"
                    ]

                Expect.equal
                    result.Outcome
                    CompilationOutcome.Failed
                    $"The Compatibility Oracle reports FS0248: Two modules named 'Program' occur in two parts of this assembly. The result is %A{result.Diagnostics}"

            testCase
                "an implicit module compiles to the public module that the Compatibility Oracle names"
            <| fun _ ->
                // The Compatibility Oracle emits the public type `Program` for each file name and text.
                for logicalPath, text in
                    [
                        "Program.fs", "open System\n\n[<EntryPoint>]\nlet main argv = 0\n"
                        "src/program.fs", "open System\n\n[<EntryPoint>]\nlet main argv = 0\n"
                        "Program.fs", "[<EntryPoint>]\nlet main argv = 0\n"
                        "src/program.fs", "[<EntryPoint>]\nlet main argv = 0\n"
                    ] do
                    let result = compileRequest [ logicalPath, text ]

                    Expect.equal
                        result.Outcome
                        CompilationOutcome.Succeeded
                        $"The implicit module in {logicalPath} compiles: %A{result.Diagnostics}"

                    let image =
                        result.Artifacts
                        |> Seq.find (fun artifact ->
                            artifact.Kind = RequestedArtifact.ImplementationAssembly
                        )
                        |> _.Bytes

                    Expect.contains
                        (typeDefinitions image)
                        ("", "Program", Reflection.TypeAttributes.Public)
                        $"The assembly for {logicalPath} contains the public module Program"

            testCase "every other implicit module keeps the prototype parser result"
            <| fun _ ->
                let entryPoint = "\n[<EntryPoint>]\nlet main argv = 0\n"

                let cases = [
                    "a library",
                    CompilationTarget.Library,
                    [ "Program.fs", $"open System{entryPoint}" ]
                    "a file before the last file",
                    CompilationTarget.Executable,
                    [
                        "Program.fs", $"open System{entryPoint}"
                        "Last.fs", "module Last\nlet y = 1\n"
                    ]
                    "a library without an open declaration (Oracle FS0222)",
                    CompilationTarget.Library,
                    [ "Program.fs", entryPoint ]
                    "no open declaration and no entry point (Oracle FS0988)",
                    CompilationTarget.Executable,
                    [ "Program.fs", "let x = 1\n" ]
                    "the dotnet new console program",
                    CompilationTarget.Executable,
                    [
                        "Program.fs",
                        "// For more information see https://aka.ms/fsharp-console-apps\r\nprintfn \"Hello from F#\"\r\n"
                    ]
                    "an unknown namespace (Oracle FS0039)",
                    CompilationTarget.Executable,
                    [ "Program.fs", $"open Nonexistent{entryPoint}" ]
                    "an unknown nested namespace (Oracle FS0039)",
                    CompilationTarget.Executable,
                    [ "Program.fs", $"open System.Nope{entryPoint}" ]
                    "an unknown namespace under a prefix with only internal types (Oracle FS0039)",
                    CompilationTarget.Executable,
                    [ "Program.fs", $"open Microsoft.FSharp.Text.Nope{entryPoint}" ]
                    "no entry point (Oracle FS0988)",
                    CompilationTarget.Executable,
                    [ "Program.fs", "open System\nlet x = 1\n" ]
                    "a file name that is not an identifier (FS0221)",
                    CompilationTarget.Executable,
                    [ "my-prog.fs", $"open System{entryPoint}" ]
                    "a file name that starts with a digit",
                    CompilationTarget.Executable,
                    [ "1prog.fs", $"open System{entryPoint}" ]
                    "an open type declaration",
                    CompilationTarget.Executable,
                    [ "Program.fs", $"open type System.Math{entryPoint}" ]
                    "an open declaration after a binding",
                    CompilationTarget.Executable,
                    [ "Program.fs", $"let x = 1\nopen System{entryPoint}" ]
                    "a global open declaration",
                    CompilationTarget.Executable,
                    [ "Program.fs", $"open global.System{entryPoint}" ]
                ]

                for name, target, files in cases do
                    let result, projections = compileFilesWithService target files
                    let logicalPath, text = List.head files

                    Expect.equal projections 0 $"The prototype parser handles {name}"

                    match
                        result,
                        Frontend.parse [] {
                            Path = logicalPath
                            Text = text
                            ContentFingerprint = "content"
                        }
                    with
                    | Error actual, Error expected ->
                        Expect.equal
                            (actual.Code, actual.Message)
                            (expected.Code, expected.Message)
                            $"The result for {name} is the prototype parser diagnostic"
                    | actual, expected ->
                        failtest
                            $"Expected the prototype parser error for {name}, but the result is %A{actual} and the prototype parser result is %A{expected}"

            testCase
                "every projected implicit module equals the prototype parser result for a named module"
            <| fun _ ->
                let mutable projected = 0

                for opens, text in implicitCorpus do
                    match projectImplicit "Program.fs" text with
                    | None -> ()
                    | Some modules ->
                        projected <-
                            projected
                            + 1

                        match prototype (withNamedHeader text) with
                        | Error diagnostic ->
                            failtest
                                $"The prototype parser rejects the named form with {diagnostic.Code}: {diagnostic.Message}\n{withNamedHeader text}"
                        | Ok expected ->
                            let expected =
                                expected
                                |> List.map (fun parsedModule -> {
                                    parsedModule with
                                        OpenedNamespaces = opens
                                        ContentFingerprint = fingerprint text
                                })

                            Expect.equal
                                (withoutChecksum modules)
                                (withoutChecksum expected)
                                $"The projection must equal the prototype parser result for:\n{text}"

                Expect.isGreaterThan projected 0 "The corpus contains projected implicit modules"

            testCase
                "the token shape check accepts every implicit module that the projection accepts"
            <| fun _ ->
                let mutable projectable = 0

                for _, text in implicitCorpus do
                    let source = document "Program.fs" text
                    let syntax = Parser.parseImplementationFile source

                    if
                        syntax.Diagnostics.IsEmpty
                        && source.Directives.IsEmpty
                        && source.Diagnostics.IsEmpty
                        && Frontend.isTokenizedLikeLexicalDocument source
                    then
                        match
                            SyntaxProjection.project
                                (implicitModule ())
                                (fingerprint text)
                                ImmutableArray.Empty
                                syntax.File
                        with
                        | SyntaxProjectionResult.Projected _ ->
                            projectable <-
                                projectable
                                + 1

                            Expect.isTrue
                                (SyntaxRouting.hasProjectableTokenShape (implicitModule ()) source)
                                $"The token shape check must accept a projectable implicit module:\n{text}"

                            Expect.isFalse
                                (SyntaxRouting.isEligible ImplicitModule.Rejected source)
                                $"The syntax parser must not run on an implicit module that the Compatibility Oracle rejects:\n{text}"

                            Expect.isTrue
                                (SyntaxRouting.isImplicitModuleCandidate (implicitModule ()) source)
                                $"The parse key of a projectable implicit module must depend on the references:\n{text}"
                        | SyntaxProjectionResult.ProjectionUnsupported _ -> ()

                Expect.isGreaterThan
                    projectable
                    0
                    "The corpus contains projectable implicit modules"

            testCase "sources with syntax diagnostics are never projected"
            <| fun _ ->
                for text in
                    [
                        "module Program\nlet broken = )\nlet answer = 42\n"
                        "module Program\nlet answer = 42\n)\nlet after = 1\n"
                    ] do
                    match project text with
                    | SyntaxProjectionResult.ProjectionUnsupported _ -> ()
                    | SyntaxProjectionResult.Projected _ ->
                        failtest
                            $"A source with a syntax error must stay on the prototype parser:\n{text}"
        ]
