namespace FSharp2.Compiler

open System
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
    let private ProtocolVersion = 7

    [<Literal>]
    let private CompileCommand = 1uy

    let private writeBytes (writer: BinaryWriter) (bytes: byte array) =
        writer.Write(bytes.Length)
        writer.Write(bytes)

    let private readBytes (reader: BinaryReader) =
        reader.ReadBytes(reader.ReadInt32())

    let private writeStrongNameMode (writer: BinaryWriter) mode =
        writer.Write(
            match mode with
            | Unsigned -> 0uy
            | DelaySign -> 1uy
            | PublicSign -> 2uy
            | FullSign -> 3uy
        )

    let private readStrongNameMode (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> Unsigned
        | 1uy -> DelaySign
        | 2uy -> PublicSign
        | 3uy -> FullSign
        | _ -> raise (InvalidDataException("invalid strong-name mode"))

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
                    let sourceLinkJson = readBytes reader

                    let managedResource =
                        if reader.ReadBoolean() then
                            Some {
                                LogicalName = reader.ReadString()
                                IsPublic = reader.ReadBoolean()
                                Data = readBytes reader
                            }
                        else
                            None

                    let nativeResourceData = readBytes reader
                    let strongNameMode = readStrongNameMode reader
                    let strongNameKey = readBytes reader

                    let invocation = {
                        AssemblyPath = reader.ReadString()
                        PdbPath = reader.ReadString()
                        SourcePaths = [ reader.ReadString() ]
                        Deterministic = reader.ReadBoolean()
                        PortablePdb = reader.ReadBoolean()
                        SourceLinkJson = sourceLinkJson
                        DebugDocumentPath = reader.ReadString()
                        ManagedResource = managedResource
                        NativeResourceData = nativeResourceData
                        StrongNameMode = strongNameMode
                        StrongNameKey = strongNameKey
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
                    |> CompilationPipeline.compile service invocation
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
        writeBytes writer invocation.SourceLinkJson

        match invocation.ManagedResource with
        | Some resource ->
            writer.Write(true)
            writer.Write(resource.LogicalName)
            writer.Write(resource.IsPublic)
            writeBytes writer resource.Data
        | None -> writer.Write(false)

        writeBytes writer invocation.NativeResourceData
        writeStrongNameMode writer invocation.StrongNameMode
        writeBytes writer invocation.StrongNameKey
        writer.Write(invocation.AssemblyPath)
        writer.Write(invocation.PdbPath)
        writer.Write(source.Path)
        writer.Write(invocation.Deterministic)
        writer.Write(invocation.PortablePdb)
        writer.Write(invocation.DebugDocumentPath)
        writer.Write(invocation.FullPaths)
        writer.Write(invocation.FlatErrors)
        writer.Write(invocation.Utf8Output)
        writer.Write(source.Text)
        writer.Flush()

        use reader = new BinaryReader(client, Encoding.UTF8, true)
        readResponse reader
