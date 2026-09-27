namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.IO
open System.IO.Pipes
open System.Security.Cryptography
open System.Text
open System.Threading

/// Versioned, closed-schema prototype protocol. It deliberately avoids
/// reflection-based serializers so the same service core can remain in the
/// NativeAOT production closure.
module internal ServiceHost =
    [<Literal>]
    let private ProtocolMagic = 0x46533250

    [<Literal>]
    let private ProtocolVersion = 10

    [<Literal>]
    let private CompileCommand = 1uy

    let private writeBytes (writer: BinaryWriter) (bytes: byte array) =
        writer.Write(bytes.Length)
        writer.Write(bytes)

    let private readBytes (reader: BinaryReader) = reader.ReadBytes(reader.ReadInt32())

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

    let private writeCompilationTarget (writer: BinaryWriter) target =
        writer.Write(
            match target with
            | CompilationTarget.Library -> 0uy
            | CompilationTarget.Executable -> 1uy
        )

    let private readCompilationTarget (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> CompilationTarget.Library
        | 1uy -> CompilationTarget.Executable
        | _ -> raise (InvalidDataException("invalid compilation target"))

    let private writeStrings (writer: BinaryWriter) (values: string list) =
        writer.Write(values.Length)

        for value in values do
            writer.Write(value)

    let private readStrings (reader: BinaryReader) = [
        for _ in 1 .. reader.ReadInt32() do
            reader.ReadString()
    ]

    let private writeOptionalString (writer: BinaryWriter) (value: string option) =
        match value with
        | Some text ->
            writer.Write(true)
            writer.Write(text)
        | None -> writer.Write(false)

    let private readOptionalString (reader: BinaryReader) =
        if reader.ReadBoolean() then
            Some(reader.ReadString())
        else
            None

    let private writeOptionalInt (writer: BinaryWriter) (value: int option) =
        match value with
        | Some number ->
            writer.Write(true)
            writer.Write(number)
        | None -> writer.Write(false)

    let private readOptionalInt (reader: BinaryReader) =
        if reader.ReadBoolean() then
            Some(reader.ReadInt32())
        else
            None

    let private writeStringValues (writer: BinaryWriter) (values: seq<string>) =
        let strings = Seq.toArray values
        writer.Write(strings.Length)

        for value in strings do
            writer.Write(value)

    let private readStringValues (reader: BinaryReader) = [|
        for _ in 1 .. reader.ReadInt32() do
            reader.ReadString()
    |]

    let private writePosition (writer: BinaryWriter) (position: SourcePosition) =
        writer.Write(position.Offset)
        writer.Write(position.Line)
        writer.Write(position.Column)

    let private readPosition (reader: BinaryReader) = {
        Offset = reader.ReadInt32()
        Line = reader.ReadInt32()
        Column = reader.ReadInt32()
    }

    let private writeRange (writer: BinaryWriter) value =
        match value with
        | Some range ->
            writer.Write(true)
            writePosition writer range.Start
            writePosition writer range.End
        | None -> writer.Write(false)

    let private readRange (reader: BinaryReader) =
        if reader.ReadBoolean() then
            Some {
                Start = readPosition reader
                End = readPosition reader
            }
        else
            None

    let private writeLocalWarningAction (writer: BinaryWriter) action =
        writer.Write(
            match action with
            | LocalWarningDirectiveAction.Enable -> 0uy
            | LocalWarningDirectiveAction.Disable -> 1uy
        )

    let private readLocalWarningAction (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> LocalWarningDirectiveAction.Enable
        | 1uy -> LocalWarningDirectiveAction.Disable
        | _ -> raise (InvalidDataException("invalid local warning action"))

    let private writeDiagnosticStyle (writer: BinaryWriter) style =
        writer.Write(
            match style with
            | DiagnosticStyle.Default -> 0uy
            | DiagnosticStyle.VisualStudio -> 1uy
            | DiagnosticStyle.Gcc -> 2uy
            | DiagnosticStyle.Emacs -> 3uy
            | DiagnosticStyle.Rich -> 4uy
            | DiagnosticStyle.Flat -> 5uy
        )

    let private readDiagnosticStyle (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> DiagnosticStyle.Default
        | 1uy -> DiagnosticStyle.VisualStudio
        | 2uy -> DiagnosticStyle.Gcc
        | 3uy -> DiagnosticStyle.Emacs
        | 4uy -> DiagnosticStyle.Rich
        | 5uy -> DiagnosticStyle.Flat
        | _ -> raise (InvalidDataException("invalid diagnostic style"))

    let private writeConsoleColorMode (writer: BinaryWriter) mode =
        writer.Write(
            match mode with
            | ConsoleColorMode.Automatic -> 0uy
            | ConsoleColorMode.Enabled -> 1uy
            | ConsoleColorMode.Disabled -> 2uy
        )

    let private readConsoleColorMode (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> ConsoleColorMode.Automatic
        | 1uy -> ConsoleColorMode.Enabled
        | 2uy -> ConsoleColorMode.Disabled
        | _ -> raise (InvalidDataException("invalid console color mode"))

    let private writeCompilationPhase (writer: BinaryWriter) phase =
        writer.Write(
            match phase with
            | CompilationPhase.Source -> 0uy
            | CompilationPhase.Syntax -> 1uy
            | CompilationPhase.ResolvedSymbols -> 2uy
            | CompilationPhase.TypedDeclarations -> 3uy
            | CompilationPhase.LoweredCode -> 4uy
            | CompilationPhase.OptimizedCode -> 5uy
            | CompilationPhase.SymbolicEmission -> 6uy
            | CompilationPhase.FinalLinking -> 7uy
        )

    let private readCompilationPhase (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> CompilationPhase.Source
        | 1uy -> CompilationPhase.Syntax
        | 2uy -> CompilationPhase.ResolvedSymbols
        | 3uy -> CompilationPhase.TypedDeclarations
        | 4uy -> CompilationPhase.LoweredCode
        | 5uy -> CompilationPhase.OptimizedCode
        | 6uy -> CompilationPhase.SymbolicEmission
        | 7uy -> CompilationPhase.FinalLinking
        | _ -> raise (InvalidDataException("invalid compilation phase"))

    let private writeDiagnosticStage (writer: BinaryWriter) stage =
        match stage with
        | DiagnosticStage.CommandLine -> writer.Write(0uy)
        | DiagnosticStage.Compilation phase ->
            writer.Write(1uy)
            writeCompilationPhase writer phase
        | DiagnosticStage.Publication -> writer.Write(2uy)
        | DiagnosticStage.Host -> writer.Write(3uy)

    let private readDiagnosticStage (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> DiagnosticStage.CommandLine
        | 1uy -> DiagnosticStage.Compilation(readCompilationPhase reader)
        | 2uy -> DiagnosticStage.Publication
        | 3uy -> DiagnosticStage.Host
        | _ -> raise (InvalidDataException("invalid diagnostic stage"))

    let private writeDiagnosticSeverity (writer: BinaryWriter) severity =
        writer.Write(
            match severity with
            | DiagnosticSeverity.Hidden -> 0uy
            | DiagnosticSeverity.Information -> 1uy
            | DiagnosticSeverity.Warning -> 2uy
            | DiagnosticSeverity.Error -> 3uy
        )

    let private readDiagnosticSeverity (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> DiagnosticSeverity.Hidden
        | 1uy -> DiagnosticSeverity.Information
        | 2uy -> DiagnosticSeverity.Warning
        | 3uy -> DiagnosticSeverity.Error
        | _ -> raise (InvalidDataException("invalid diagnostic severity"))

    let private writeDiagnosticDisposition (writer: BinaryWriter) disposition =
        writer.Write(
            match disposition with
            | DiagnosticDisposition.Emitted -> 0uy
            | DiagnosticDisposition.Suppressed -> 1uy
        )

    let private readDiagnosticDisposition (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> DiagnosticDisposition.Emitted
        | 1uy -> DiagnosticDisposition.Suppressed
        | _ -> raise (InvalidDataException("invalid diagnostic disposition"))

    let private writeDiagnosticSuppression (writer: BinaryWriter) suppression =
        writer.Write(
            match suppression with
            | DiagnosticSuppression.WarningLevel -> 0uy
            | DiagnosticSuppression.GlobalNowarn -> 1uy
            | DiagnosticSuppression.LocalNowarn -> 2uy
            | DiagnosticSuppression.OffByDefault -> 3uy
            | DiagnosticSuppression.LanguageFeature -> 4uy
            | DiagnosticSuppression.MaximumErrors -> 5uy
            | DiagnosticSuppression.AbortBoundary -> 6uy
        )

    let private readDiagnosticSuppression (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> DiagnosticSuppression.WarningLevel
        | 1uy -> DiagnosticSuppression.GlobalNowarn
        | 2uy -> DiagnosticSuppression.LocalNowarn
        | 3uy -> DiagnosticSuppression.OffByDefault
        | 4uy -> DiagnosticSuppression.LanguageFeature
        | 5uy -> DiagnosticSuppression.MaximumErrors
        | 6uy -> DiagnosticSuppression.AbortBoundary
        | _ -> raise (InvalidDataException("invalid diagnostic suppression"))

    let private writeDiagnosticStream (writer: BinaryWriter) stream =
        writer.Write(
            match stream with
            | DiagnosticStream.StandardOutput -> 0uy
            | DiagnosticStream.StandardError -> 1uy
        )

    let private readDiagnosticStream (reader: BinaryReader) =
        match reader.ReadByte() with
        | 0uy -> DiagnosticStream.StandardOutput
        | 1uy -> DiagnosticStream.StandardError
        | _ -> raise (InvalidDataException("invalid diagnostic stream"))

    let private writeDiagnosticOptions (writer: BinaryWriter) (options: DiagnosticOptions) =
        writeOptionalInt writer options.WarningLevel
        writeStringValues writer options.DisabledWarnings
        writeStringValues writer options.EnabledWarnings
        writer.Write(options.TreatWarningsAsErrors)
        writeStringValues writer options.WarningsAsErrors
        writeStringValues writer options.WarningsNotAsErrors
        writeOptionalInt writer options.MaximumErrors
        writer.Write(options.AbortOnError)
        writeOptionalString writer options.PreferredUICulture
        writer.Write(options.LocalWarningDirectives.Length)

        for directive in options.LocalWarningDirectives do
            writer.Write(directive.Order)
            writeLocalWarningAction writer directive.Action
            writer.Write(directive.Code)
            writer.Write(directive.LogicalPath)
            writeRange writer directive.Range

        writer.Write(options.FullPaths)
        writer.Write(options.FlatErrors)
        writer.Write(options.Utf8Output)
        writeDiagnosticStyle writer options.DiagnosticStyle
        writeConsoleColorMode writer options.ConsoleColorMode
        writeOptionalInt writer options.LCID
        writeOptionalString writer options.PreferredUILanguage
        writer.Write(options.TestParserErrorRecovery)
        writer.Write(options.StandardOutputRedirected)
        writer.Write(options.StandardErrorRedirected)

    let private readDiagnosticOptions (reader: BinaryReader) =
        let warningLevel = readOptionalInt reader
        let disabledWarnings = readStringValues reader
        let enabledWarnings = readStringValues reader
        let treatWarningsAsErrors = reader.ReadBoolean()
        let warningsAsErrors = readStringValues reader
        let warningsNotAsErrors = readStringValues reader
        let maximumErrors = readOptionalInt reader
        let abortOnError = reader.ReadBoolean()
        let preferredUICulture = readOptionalString reader

        let localWarningDirectives = [|
            for _ in 1 .. reader.ReadInt32() do
                yield
                    LocalWarningDirective.Create(
                        reader.ReadInt64(),
                        readLocalWarningAction reader,
                        reader.ReadString(),
                        reader.ReadString(),
                        readRange reader
                    )
        |]

        let fullPaths = reader.ReadBoolean()
        let flatErrors = reader.ReadBoolean()
        let utf8Output = reader.ReadBoolean()
        let diagnosticStyle = readDiagnosticStyle reader
        let consoleColorMode = readConsoleColorMode reader
        let lcid = readOptionalInt reader
        let preferredUILanguage = readOptionalString reader
        let testParserErrorRecovery = reader.ReadBoolean()
        let standardOutputRedirected = reader.ReadBoolean()
        let standardErrorRedirected = reader.ReadBoolean()

        DiagnosticOptions.Create(
            warningLevel,
            disabledWarnings,
            enabledWarnings,
            treatWarningsAsErrors,
            warningsAsErrors,
            warningsNotAsErrors,
            maximumErrors,
            abortOnError,
            preferredUICulture,
            localWarningDirectives,
            fullPaths,
            flatErrors,
            utf8Output,
            diagnosticStyle,
            consoleColorMode,
            lcid,
            preferredUILanguage,
            testParserErrorRecovery,
            standardOutputRedirected,
            standardErrorRedirected
        )

    let private writeDiagnostic (writer: BinaryWriter) (diagnostic: CompilationDiagnostic) =
        writer.Write(diagnostic.Occurrence)
        writer.Write(diagnostic.Code)
        writer.Write(diagnostic.NumericCode)
        writeOptionalString writer diagnostic.Subcategory
        writeDiagnosticStage writer diagnostic.Stage
        writeDiagnosticSeverity writer diagnostic.OriginalSeverity
        writeDiagnosticSeverity writer diagnostic.EffectiveSeverity
        writeDiagnosticDisposition writer diagnostic.Disposition

        match diagnostic.Suppression with
        | Some suppression ->
            writer.Write(true)
            writeDiagnosticSuppression writer suppression
        | None -> writer.Write(false)

        writer.Write(diagnostic.Message)
        writeOptionalString writer diagnostic.LogicalPath
        writeRange writer diagnostic.Range
        writer.Write(diagnostic.RelatedInformation.Length)

        for related in diagnostic.RelatedInformation do
            writer.Write(related.Message)
            writeOptionalString writer related.LogicalPath
            writeRange writer related.Range

        writeStringValues writer diagnostic.Suggestions

        match diagnostic.Stream with
        | Some stream ->
            writer.Write(true)
            writeDiagnosticStream writer stream
        | None -> writer.Write(false)

    let private readDiagnostic (reader: BinaryReader) =
        let occurrence = reader.ReadInt64()
        let code = reader.ReadString()
        let numericCode = reader.ReadInt32()
        let subcategory = readOptionalString reader
        let stage = readDiagnosticStage reader
        let originalSeverity = readDiagnosticSeverity reader
        let effectiveSeverity = readDiagnosticSeverity reader
        let disposition = readDiagnosticDisposition reader

        let suppression =
            if reader.ReadBoolean() then
                Some(readDiagnosticSuppression reader)
            else
                None

        let message = reader.ReadString()
        let logicalPath = readOptionalString reader
        let range = readRange reader

        let relatedInformation = [|
            for _ in 1 .. reader.ReadInt32() do
                yield
                    DiagnosticRelatedInformation.Create(
                        reader.ReadString(),
                        readOptionalString reader,
                        readRange reader
                    )
        |]

        let suggestions = readStringValues reader

        let stream =
            if reader.ReadBoolean() then
                Some(readDiagnosticStream reader)
            else
                None

        CompilationDiagnostic.Create(
            occurrence,
            code,
            numericCode,
            subcategory,
            stage,
            originalSeverity,
            effectiveSeverity,
            disposition,
            suppression,
            message,
            logicalPath,
            range,
            relatedInformation,
            suggestions,
            stream
        )

    let private writeInvocation
        (writer: BinaryWriter)
        (invocation: CompilerInvocation)
        (diagnosticOptions: DiagnosticOptions)
        (sources: SourceInput list)
        =
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
        writeCompilationTarget writer invocation.Target
        writer.Write(invocation.AssemblyPath)
        writer.Write(invocation.PdbPath)
        writeOptionalString writer invocation.ReferenceAssemblyPath
        writeOptionalString writer invocation.DocumentationPath
        writeStrings writer invocation.SourcePaths
        writeStrings writer invocation.EmbeddedSourcePaths
        writeStrings writer invocation.ReferencePaths
        writeStrings writer invocation.Defines
        writeOptionalString writer invocation.LanguageVersion
        writer.Write(invocation.Optimize)
        writer.Write(invocation.CheckNulls)
        writer.Write(invocation.NoFramework)
        writer.Write(invocation.HighEntropyVA)
        writeOptionalString writer invocation.TargetProfile
        writer.Write(invocation.NoCopyFSharpCore)
        writer.Write(invocation.SimpleResolution)
        writeStrings writer invocation.TestFlags
        writer.Write(invocation.Deterministic)
        writer.Write(invocation.PortablePdb)
        writeStrings writer invocation.DebugDocumentPaths
        writeDiagnosticOptions writer diagnosticOptions

        writeStrings
            writer
            (sources
             |> List.map _.Text)

        writeStrings
            writer
            (sources
             |> List.map _.ContentFingerprint)

    let private readInvocation (reader: BinaryReader) =
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
        let target = readCompilationTarget reader
        let assemblyPath = reader.ReadString()
        let pdbPath = reader.ReadString()
        let referenceAssemblyPath = readOptionalString reader
        let documentationPath = readOptionalString reader
        let sourcePaths = readStrings reader
        let embeddedSourcePaths = readStrings reader
        let referencePaths = readStrings reader
        let defines = readStrings reader
        let languageVersion = readOptionalString reader
        let optimize = reader.ReadBoolean()
        let checkNulls = reader.ReadBoolean()
        let noFramework = reader.ReadBoolean()
        let highEntropyVA = reader.ReadBoolean()
        let targetProfile = readOptionalString reader
        let noCopyFSharpCore = reader.ReadBoolean()
        let simpleResolution = reader.ReadBoolean()
        let testFlags = readStrings reader
        let deterministic = reader.ReadBoolean()
        let portablePdb = reader.ReadBoolean()
        let debugDocumentPaths = readStrings reader
        let diagnosticOptions = readDiagnosticOptions reader
        let sourceTexts = readStrings reader
        let sourceFingerprints = readStrings reader

        if
            sourcePaths.Length
            <> sourceTexts.Length
            || sourcePaths.Length
               <> sourceFingerprints.Length
        then
            raise (InvalidDataException("source path/text/fingerprint cardinality mismatch"))

        {
            Target = target
            AssemblyPath = assemblyPath
            PdbPath = pdbPath
            ReferenceAssemblyPath = referenceAssemblyPath
            DocumentationPath = documentationPath
            SourcePaths = sourcePaths
            EmbeddedSourcePaths = embeddedSourcePaths
            ReferencePaths = referencePaths
            Defines = defines
            LanguageVersion = languageVersion
            Optimize = optimize
            CheckNulls = checkNulls
            NoFramework = noFramework
            WarningLevel = diagnosticOptions.WarningLevel
            DisabledWarnings = List.ofSeq diagnosticOptions.DisabledWarnings
            EnabledWarnings = List.ofSeq diagnosticOptions.EnabledWarnings
            TreatWarningsAsErrors = diagnosticOptions.TreatWarningsAsErrors
            WarningsAsErrors = List.ofSeq diagnosticOptions.WarningsAsErrors
            WarningsNotAsErrors = List.ofSeq diagnosticOptions.WarningsNotAsErrors
            MaximumErrors = diagnosticOptions.MaximumErrors
            AbortOnError = diagnosticOptions.AbortOnError
            HighEntropyVA = highEntropyVA
            TargetProfile = targetProfile
            NoCopyFSharpCore = noCopyFSharpCore
            SimpleResolution = simpleResolution
            TestFlags = testFlags
            Deterministic = deterministic
            PortablePdb = portablePdb
            SourceLinkJson = sourceLinkJson
            DebugDocumentPaths = debugDocumentPaths
            ManagedResource = managedResource
            NativeResourceData = nativeResourceData
            StrongNameMode = strongNameMode
            StrongNameKey = strongNameKey
            FullPaths = diagnosticOptions.FullPaths
            FlatErrors = diagnosticOptions.FlatErrors
            Utf8Output = diagnosticOptions.Utf8Output
            ServerName = None
            TracePath = None
        },
        diagnosticOptions,
        (sourcePaths, sourceTexts, sourceFingerprints)
        |||> List.map3 (fun path text contentFingerprint -> {
            Path = path
            Text = text
            ContentFingerprint = contentFingerprint
        })

    let private writeResponse (writer: BinaryWriter) (response: CompilationPipeline.Response) =
        writer.Write(ProtocolMagic)
        writer.Write(ProtocolVersion)
        writer.Write(response.ExitCode)
        writeDiagnosticOptions writer response.DiagnosticOptions
        writer.Write(response.Diagnostics.Length)

        for diagnostic in response.Diagnostics do
            writeDiagnostic writer diagnostic

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

        let exitCode = reader.ReadInt32()
        let diagnosticOptions = readDiagnosticOptions reader

        let diagnostics =
            ImmutableArray.CreateRange [|
                for _ in 1 .. reader.ReadInt32() do
                    readDiagnostic reader
            |]

        {
            ExitCode = exitCode
            DiagnosticOptions = diagnosticOptions
            Diagnostics = diagnostics
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
        : CompilationPipeline.Response

    let private failure code numericCode message =
        CompilationPipeline.serviceFailure
            (CompilationPipeline.defaultDiagnosticOptions ())
            code
            numericCode
            message

    let runServer pipeName =
        let compiler = Compiler()

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
                        (failure "FSC2P2001" 2001 "invalid compiler service protocol header")
                elif
                    reader.ReadInt32()
                    <> ProtocolVersion
                then
                    writeResponse
                        writer
                        (failure "FSC2P2002" 2002 "unsupported compiler service protocol version")
                elif
                    reader.ReadByte()
                    <> CompileCommand
                then
                    writeResponse
                        writer
                        (failure "FSC2P2003" 2003 "unsupported compiler service command")
                else
                    let invocation, diagnosticOptions, sources = readInvocation reader

                    let response =
                        try
                            let compileStarted = System.Diagnostics.Stopwatch.GetTimestamp()

                            let request =
                                CompilationPipeline.createRequestWithDiagnosticOptions
                                    invocation
                                    diagnosticOptions
                                    sources

                            let result = compiler.Compile(request, CancellationToken.None)

                            CompilationPipeline.completeInvocation
                                compileStarted
                                invocation
                                diagnosticOptions
                                result
                        finally
                            if invocation.StrongNameKey.Length > 0 then
                                CryptographicOperations.ZeroMemory(
                                    invocation.StrongNameKey.AsSpan()
                                )

                    writeResponse writer response
            with ex ->
                if server.IsConnected then
                    use writer = new BinaryWriter(server, Encoding.UTF8, true)

                    writeResponse writer (failure "FSC2P2004" 2004 ex.Message)

        0

    let compileRemote
        (pipeName: string)
        (invocation: CompilerInvocation)
        (diagnosticOptions: DiagnosticOptions)
        (sources: SourceInput list)
        =
        use client =
            new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.None)

        client.Connect(10_000)

        use writer = new BinaryWriter(client, Encoding.UTF8, true)
        writer.Write(ProtocolMagic)
        writer.Write(ProtocolVersion)
        writer.Write(CompileCommand)
        writeInvocation writer invocation diagnosticOptions sources
        writer.Flush()

        use reader = new BinaryReader(client, Encoding.UTF8, true)
        readResponse reader
