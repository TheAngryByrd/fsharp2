namespace FSharp2.Compiler

open System
open System.Diagnostics
open System.IO

module CompilerHost =
    let private writeTrace (path: string) (response: ServiceCompilationResponse) =
        let directory = Path.GetDirectoryName(path)

        Directory.CreateDirectory(directory)
        |> ignore

        File.WriteAllLines(
            path,
            [|
                "schema=1"
                "servicePid="
                + response.ServiceProcessId.ToString()
                "querySchema="
                + response.QuerySchema.ToString()
                "nodeKind="
                + response.NodeKind
                "contentFingerprint="
                + response.ContentFingerprint
                "previousContentFingerprint="
                + response.PreviousContentFingerprint
                "invalidationReason="
                + response.InvalidationReason
                "parseKey="
                + response.ParseKey
                "checkKey="
                + response.CheckKey
                "lowerKey="
                + response.LowerKey
                "dependencyCount="
                + response.DependencyCount.ToString()
                "parse="
                + response.ParseDecision
                "check="
                + response.CheckDecision
                "lower="
                + response.LowerDecision
                "parseElapsedMicroseconds="
                + response.ParseElapsedMicroseconds.ToString()
                "checkElapsedMicroseconds="
                + response.CheckElapsedMicroseconds.ToString()
                "lowerElapsedMicroseconds="
                + response.LowerElapsedMicroseconds.ToString()
                "linkElapsedMicroseconds="
                + response.LinkElapsedMicroseconds.ToString()
                "publishElapsedMicroseconds="
                + response.PublishElapsedMicroseconds.ToString()
                "compileElapsedMicroseconds="
                + response.CompileElapsedMicroseconds.ToString()
                "exportFingerprint="
                + response.ExportFingerprint
                "fragmentHash="
                + response.FragmentHash
                "emitted="
                + (if response.Emitted then "true" else "false")
            |]
        )

    let private elapsedMicroseconds started =
        Stopwatch.GetElapsedTime(started).Ticks
        / 10L

    let private compileLocally (invocation: CompilerInvocation) (source: SourceInput) =
        let compileStarted = Stopwatch.GetTimestamp()
        let assemblyName = Path.GetFileNameWithoutExtension(invocation.AssemblyPath)
        let service = CompilerService()

        match service.Compile(assemblyName, source) with
        | Error diagnostic -> {
            ExitCode = 1
            Error = DiagnosticFormatter.format invocation diagnostic
            ServiceProcessId = Environment.ProcessId
            QuerySchema = 1
            NodeKind = "source"
            ContentFingerprint = String.Empty
            PreviousContentFingerprint = String.Empty
            InvalidationReason = "bypass"
            ParseKey = String.Empty
            CheckKey = String.Empty
            LowerKey = String.Empty
            DependencyCount = 0
            ParseDecision = "miss"
            CheckDecision = "bypass"
            LowerDecision = "bypass"
            ParseElapsedMicroseconds = 0L
            CheckElapsedMicroseconds = 0L
            LowerElapsedMicroseconds = 0L
            LinkElapsedMicroseconds = 0L
            PublishElapsedMicroseconds = 0L
            CompileElapsedMicroseconds = elapsedMicroseconds compileStarted
            ExportFingerprint = String.Empty
            FragmentHash = String.Empty
            Emitted = false
          }
        | Ok query ->
            let linkStarted = Stopwatch.GetTimestamp()
            let artifacts = Linker.link invocation query.SymbolicAssembly
            let linkElapsedMicroseconds = elapsedMicroseconds linkStarted
            let publishStarted = Stopwatch.GetTimestamp()
            Linker.publishTransactionally invocation artifacts
            let publishElapsedMicroseconds = elapsedMicroseconds publishStarted

            {
                ExitCode = 0
                Error = String.Empty
                ServiceProcessId = Environment.ProcessId
                QuerySchema = query.QuerySchema
                NodeKind = query.NodeKind
                ContentFingerprint = query.ContentFingerprint
                PreviousContentFingerprint = query.PreviousContentFingerprint
                InvalidationReason = query.InvalidationReason
                ParseKey = query.ParseKey
                CheckKey = query.CheckKey
                LowerKey = query.LowerKey
                DependencyCount = query.DependencyCount
                ParseDecision = query.ParseDecision
                CheckDecision = query.CheckDecision
                LowerDecision = query.LowerDecision
                ParseElapsedMicroseconds = query.ParseElapsedMicroseconds
                CheckElapsedMicroseconds = query.CheckElapsedMicroseconds
                LowerElapsedMicroseconds = query.LowerElapsedMicroseconds
                LinkElapsedMicroseconds = linkElapsedMicroseconds
                PublishElapsedMicroseconds = publishElapsedMicroseconds
                CompileElapsedMicroseconds = elapsedMicroseconds compileStarted
                ExportFingerprint = query.SymbolicAssembly.PublicFingerprint
                FragmentHash = query.SymbolicAssembly.Methods.Head.ContentHash
                Emitted = true
            }

    let private runCompilation (arguments: string array) =
        match CommandLine.parse arguments with
        | Error message ->
            Console.Error.WriteLine(
                "FSC2P0001: "
                + message
            )

            1
        | Ok invocation ->
            let sourcePath = invocation.SourcePaths.Head

            let source = {
                Path = sourcePath
                Text = File.ReadAllText(sourcePath)
            }

            let response =
                match invocation.ServerName with
                | Some pipeName -> ServiceHost.compileRemote pipeName invocation source
                | None -> compileLocally invocation source

            invocation.TracePath
            |> Option.iter (fun path -> writeTrace path response)

            if response.ExitCode = 0 then
                Console.Out.WriteLine(
                    "emitted="
                    + invocation.AssemblyPath
                )

                Console.Out.WriteLine(
                    "pdb="
                    + invocation.PdbPath
                )
            else
                Console.Error.WriteLine(response.Error)

            response.ExitCode

    let run (arguments: string array) =
        try
            let servePrefix = "--fsharp2-serve:"

            if
                arguments.Length = 1
                && arguments.[0].StartsWith(servePrefix, StringComparison.OrdinalIgnoreCase)
            then
                arguments.[0].Substring(servePrefix.Length)
                |> ServiceHost.runServer
            else
                runCompilation arguments
        with ex ->
            Console.Error.WriteLine(
                "FSC2P9999: "
                + ex.Message
            )

            1
