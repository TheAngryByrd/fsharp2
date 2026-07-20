namespace FSharp2.Compiler

open System
open System.Diagnostics
open System.IO
open System.Security.Cryptography

/// The host-independent compile/link/publish path shared by standalone and
/// persistent-service execution.
module internal CompilationPipeline =
    let private elapsedMicroseconds started =
        Stopwatch.GetElapsedTime(started).Ticks
        / 10L

    let private failure started message = {
        ExitCode = 1
        Error = message
        ServiceProcessId = Environment.ProcessId
        QuerySchema = CompilerSchema.Query
        NodeKind = "source"
        ContentFingerprint = String.Empty
        PreviousContentFingerprint = String.Empty
        InvalidationReason = "bypass"
        ParseKey = String.Empty
        CheckKey = String.Empty
        LowerKey = String.Empty
        DependencyCount = 0
        ParseDecision = "bypass"
        CheckDecision = "bypass"
        LowerDecision = "bypass"
        ParseElapsedMicroseconds = 0L
        CheckElapsedMicroseconds = 0L
        LowerElapsedMicroseconds = 0L
        LinkElapsedMicroseconds = 0L
        PublishElapsedMicroseconds = 0L
        CompileElapsedMicroseconds = elapsedMicroseconds started
        ExportFingerprint = String.Empty
        FragmentHash = String.Empty
        Emitted = false
    }

    let private compileCore
        (service: CompilerService)
        (invocation: CompilerInvocation)
        (sources: SourceInput list)
        =
        let compileStarted = Stopwatch.GetTimestamp()
        let assemblyName = Path.GetFileNameWithoutExtension(invocation.AssemblyPath)

        let query =
            match ReferenceTypeIndex.Create(invocation.ReferencePaths) with
            | Ok references ->
                service.Compile(
                    assemblyName,
                    invocation.Defines,
                    references,
                    sources
                )
            | Error message ->
                Error {
                    Code = "FSC2P1001"
                    Message = message
                    Path = None
                    Range = None
                }

        match query with
        | Error diagnostic ->
            diagnostic
            |> DiagnosticFormatter.format invocation
            |> failure compileStarted
        | Ok query ->
            try
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
                    FragmentHash =
                        match
                            [
                                yield!
                                    query.SymbolicAssembly.AssemblyAttributes
                                    |> List.map _.ContentHash

                                yield!
                                    query.SymbolicAssembly.Module.TypeAbbreviations
                                    |> List.map _.ContentHash

                                for typeFragment in query.SymbolicAssembly.Module.Types do
                                    yield!
                                        typeFragment.LiteralFields
                                        |> List.map _.ContentHash

                                    yield!
                                        typeFragment.Methods
                                        |> List.map _.ContentHash
                            ]
                        with
                        | [ contentHash ] -> contentHash
                        | contentHashes -> String.concat "|" contentHashes
                    Emitted = true
                }
            with ex ->
                "FSC2P9999: "
                + ex.Message
                |> failure compileStarted

    let compile
        (service: CompilerService)
        (invocation: CompilerInvocation)
        (sources: SourceInput list)
        =
        try
            compileCore service invocation sources
        finally
            if invocation.StrongNameKey.Length > 0 then
                CryptographicOperations.ZeroMemory(invocation.StrongNameKey.AsSpan())
