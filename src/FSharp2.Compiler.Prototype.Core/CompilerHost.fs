namespace FSharp2.Compiler

open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Threading

module CompilerHost =
    let private writeTrace
        (invocation: CompilerInvocation)
        (path: string)
        (response: ServiceCompilationResponse)
        =
        let directory = Path.GetDirectoryName(path)

        Directory.CreateDirectory(directory)
        |> ignore

        File.WriteAllLines(
            path,
            [|
                "schema=1"
                "sourceCount="
                + invocation.SourcePaths.Length.ToString()
                "referenceCount="
                + invocation.ReferencePaths.Length.ToString()
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

    let private compileLocally (invocation: CompilerInvocation) (sources: SourceInput list) =
        let compileStarted = Stopwatch.GetTimestamp()
        let request = CompilationPipeline.createRequest invocation sources
        let result = Compiler().Compile(request, CancellationToken.None)
        CompilationPipeline.completeInvocation compileStarted invocation result

    let private runCompilation (arguments: string array) =
        match CommandLine.parse arguments with
        | Error message ->
            Console.Error.WriteLine(
                "FSC2P0001: "
                + message
            )

            1
        | Ok invocation ->
            let sources =
                invocation.SourcePaths
                |> List.map (fun sourcePath -> {
                    Path = sourcePath
                    Text = File.ReadAllText(sourcePath)
                })

            let response =
                try
                    match invocation.ServerName with
                    | Some pipeName -> ServiceHost.compileRemote pipeName invocation sources
                    | None -> compileLocally invocation sources
                finally
                    if invocation.StrongNameKey.Length > 0 then
                        CryptographicOperations.ZeroMemory(invocation.StrongNameKey.AsSpan())

            invocation.TracePath
            |> Option.iter (fun path -> writeTrace invocation path response)

            if
                response.ExitCode
                <> 0
            then
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
