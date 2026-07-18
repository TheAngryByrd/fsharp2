namespace FSharp2.Compiler

open System
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Text

/// Versioned, closed-schema prototype protocol. It deliberately avoids
/// reflection-based serializers so the same service core can remain in the
/// NativeAOT production closure.
module internal ServiceHost =
    [<Literal>]
    let private ProtocolMagic = 0x46533250

    [<Literal>]
    let private ProtocolVersion = 4

    [<Literal>]
    let private CompileCommand = 1uy

    let private writeResponse (writer: BinaryWriter) response =
        writer.Write(ProtocolMagic)
        writer.Write(ProtocolVersion)
        writer.Write(response.ExitCode)
        writer.Write(response.Error)
        writer.Write(response.ServiceProcessId)
        writer.Write(response.QuerySchema)
        writer.Write(response.NodeKind)
        writer.Write(response.ContentFingerprint)
        writer.Write(response.PreviousContentFingerprint)
        writer.Write(response.InvalidationReason)
        writer.Write(response.ParseKey)
        writer.Write(response.CheckKey)
        writer.Write(response.LowerKey)
        writer.Write(response.DependencyCount)
        writer.Write(response.ParseDecision)
        writer.Write(response.CheckDecision)
        writer.Write(response.LowerDecision)
        writer.Write(response.ParseElapsedMicroseconds)
        writer.Write(response.CheckElapsedMicroseconds)
        writer.Write(response.LowerElapsedMicroseconds)
        writer.Write(response.LinkElapsedMicroseconds)
        writer.Write(response.PublishElapsedMicroseconds)
        writer.Write(response.CompileElapsedMicroseconds)
        writer.Write(response.ExportFingerprint)
        writer.Write(response.FragmentHash)
        writer.Write(response.Emitted)
        writer.Flush()

    let private readResponse (reader: BinaryReader) =
        if
            reader.ReadInt32()
            <> ProtocolMagic
        then
            raise (InvalidDataException("the compiler service returned an invalid protocol header"))

        if
            reader.ReadInt32()
            <> ProtocolVersion
        then
            raise (
                InvalidDataException(
                    "the compiler service returned an unsupported protocol version"
                )
            )

        {
            ExitCode = reader.ReadInt32()
            Error = reader.ReadString()
            ServiceProcessId = reader.ReadInt32()
            QuerySchema = reader.ReadInt32()
            NodeKind = reader.ReadString()
            ContentFingerprint = reader.ReadString()
            PreviousContentFingerprint = reader.ReadString()
            InvalidationReason = reader.ReadString()
            ParseKey = reader.ReadString()
            CheckKey = reader.ReadString()
            LowerKey = reader.ReadString()
            DependencyCount = reader.ReadInt32()
            ParseDecision = reader.ReadString()
            CheckDecision = reader.ReadString()
            LowerDecision = reader.ReadString()
            ParseElapsedMicroseconds = reader.ReadInt64()
            CheckElapsedMicroseconds = reader.ReadInt64()
            LowerElapsedMicroseconds = reader.ReadInt64()
            LinkElapsedMicroseconds = reader.ReadInt64()
            PublishElapsedMicroseconds = reader.ReadInt64()
            CompileElapsedMicroseconds = reader.ReadInt64()
            ExportFingerprint = reader.ReadString()
            FragmentHash = reader.ReadString()
            Emitted = reader.ReadBoolean()
        }

    let private failure message = {
        ExitCode = 1
        Error = message
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
        ParseDecision = "bypass"
        CheckDecision = "bypass"
        LowerDecision = "bypass"
        ParseElapsedMicroseconds = 0L
        CheckElapsedMicroseconds = 0L
        LowerElapsedMicroseconds = 0L
        LinkElapsedMicroseconds = 0L
        PublishElapsedMicroseconds = 0L
        CompileElapsedMicroseconds = 0L
        ExportFingerprint = String.Empty
        FragmentHash = String.Empty
        Emitted = false
    }

    let private elapsedMicroseconds started =
        Stopwatch.GetElapsedTime(started).Ticks
        / 10L

    let private compile
        (service: CompilerService)
        (invocation: CompilerInvocation)
        (source: SourceInput)
        =
        let compileStarted = Stopwatch.GetTimestamp()
        let assemblyName = Path.GetFileNameWithoutExtension(invocation.AssemblyPath)

        match service.Compile(assemblyName, source) with
        | Error diagnostic -> {
            failure (DiagnosticFormatter.format invocation diagnostic) with
                CompileElapsedMicroseconds = elapsedMicroseconds compileStarted
          }
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
                    FragmentHash = query.SymbolicAssembly.Methods.Head.ContentHash
                    Emitted = true
                }
            with ex -> {
                failure (
                    "FSC2P9999: "
                    + ex.Message
                ) with
                    CompileElapsedMicroseconds = elapsedMicroseconds compileStarted
            }

    let runServer pipeName =
        let service = CompilerService()

        Console.Out.WriteLine(
            "ready="
            + pipeName
        )

        Console.Out.Flush()

        while true do
            use server =
                new NamedPipeServerStream(
                    pipeName,
                    PipeDirection.InOut,
                    1,
                    PipeTransmissionMode.Byte,
                    PipeOptions.None
                )

            server.WaitForConnection()

            try
                use reader = new BinaryReader(server, Encoding.UTF8, true)
                use writer = new BinaryWriter(server, Encoding.UTF8, true)

                if
                    reader.ReadInt32()
                    <> ProtocolMagic
                then
                    writeResponse
                        writer
                        (failure "FSC2P2001: invalid compiler service protocol header")
                elif
                    reader.ReadInt32()
                    <> ProtocolVersion
                then
                    writeResponse
                        writer
                        (failure "FSC2P2002: unsupported compiler service protocol version")
                elif
                    reader.ReadByte()
                    <> CompileCommand
                then
                    writeResponse writer (failure "FSC2P2003: unsupported compiler service command")
                else
                    let sourceLinkJson = reader.ReadBytes(reader.ReadInt32())

                    let invocation = {
                        AssemblyPath = reader.ReadString()
                        PdbPath = reader.ReadString()
                        SourcePaths = [ reader.ReadString() ]
                        Deterministic = reader.ReadBoolean()
                        PortablePdb = reader.ReadBoolean()
                        SourceLinkJson = sourceLinkJson
                        FullPaths = reader.ReadBoolean()
                        FlatErrors = reader.ReadBoolean()
                        Utf8Output = reader.ReadBoolean()
                        ServerName = None
                        TracePath = None
                    }

                    let source: SourceInput = {
                        Path = invocation.SourcePaths.Head
                        Text = reader.ReadString()
                    }

                    source
                    |> compile service invocation
                    |> writeResponse writer
            with ex ->
                if server.IsConnected then
                    use writer = new BinaryWriter(server, Encoding.UTF8, true)

                    writeResponse
                        writer
                        (failure (
                            "FSC2P2004: "
                            + ex.Message
                        ))

        0

    let compileRemote (pipeName: string) (invocation: CompilerInvocation) (source: SourceInput) =
        use client =
            new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.None)

        client.Connect(10_000)

        use writer = new BinaryWriter(client, Encoding.UTF8, true)
        writer.Write(ProtocolMagic)
        writer.Write(ProtocolVersion)
        writer.Write(CompileCommand)
        writer.Write(invocation.SourceLinkJson.Length)
        writer.Write(invocation.SourceLinkJson)
        writer.Write(invocation.AssemblyPath)
        writer.Write(invocation.PdbPath)
        writer.Write(source.Path)
        writer.Write(invocation.Deterministic)
        writer.Write(invocation.PortablePdb)
        writer.Write(invocation.FullPaths)
        writer.Write(invocation.FlatErrors)
        writer.Write(invocation.Utf8Output)
        writer.Write(source.Text)
        writer.Flush()

        use reader = new BinaryReader(client, Encoding.UTF8, true)
        readResponse reader
