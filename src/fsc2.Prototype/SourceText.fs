namespace FSharp2.Compiler

open System
open System.Collections.Immutable
open System.Text

[<RequireQualifiedAccess>]
type internal SourceEncoding =
    | Utf8
    | Utf16LittleEndian
    | Utf16BigEndian

[<RequireQualifiedAccess>]
type internal NewlineForm =
    | None
    | LineFeed
    | CarriageReturn
    | CarriageReturnLineFeed

type internal SourceLineMapping = {
    PhysicalLine: int
    LogicalLine: int
    LogicalPath: string option
}

type internal MappedSourcePosition = {
    Offset: int
    Line: int
    Column: int
    LogicalPath: string option
}

type internal SourceMap = {
    TextLength: int
    LineStarts: ImmutableArray<int>
    Newlines: ImmutableArray<NewlineForm>
    LineMappings: ImmutableArray<SourceLineMapping>
}

module internal SourceMap =
    let create (text: string) =
        if isNull text then
            nullArg "text"

        let starts = ResizeArray<int>()
        let newlines = ResizeArray<NewlineForm>()
        starts.Add 0

        let mutable offset = 0

        while offset < text.Length do
            match text[offset] with
            | '\r' when offset + 1 < text.Length && text[offset + 1] = '\n' ->
                newlines.Add NewlineForm.CarriageReturnLineFeed
                offset <- offset + 2
                starts.Add offset
            | '\r' ->
                newlines.Add NewlineForm.CarriageReturn
                offset <- offset + 1
                starts.Add offset
            | '\n' ->
                newlines.Add NewlineForm.LineFeed
                offset <- offset + 1
                starts.Add offset
            | _ -> offset <- offset + 1

        newlines.Add NewlineForm.None

        {
            TextLength = text.Length
            LineStarts = ImmutableArray.CreateRange starts
            Newlines = ImmutableArray.CreateRange newlines
            LineMappings = ImmutableArray<SourceLineMapping>.Empty
        }

    let positionAt (sourceMap: SourceMap) offset =
        if offset < 0 || offset > sourceMap.TextLength then
            invalidArg "offset" "offset must identify a UTF-16 position in the source text."

        let mutable low = 0
        let mutable high = sourceMap.LineStarts.Length - 1
        let mutable lineIndex = 0

        while low <= high do
            let middle = low + (high - low) / 2

            if sourceMap.LineStarts[middle] <= offset then
                lineIndex <- middle
                low <- middle + 1
            else
                high <- middle - 1

        {
            Offset = offset
            Line = lineIndex + 1
            Column = offset - sourceMap.LineStarts[lineIndex] + 1
        }

    let addLineMapping sourceMap physicalLine logicalLine logicalPath =
        if physicalLine < 1 then
            invalidArg "physicalLine" "physicalLine must be positive."

        if logicalLine < 1 then
            invalidArg "logicalLine" "logicalLine must be positive."

        logicalPath
        |> Option.iter (fun value ->
            if String.IsNullOrWhiteSpace value then
                invalidArg "logicalPath" "logicalPath must contain text."
        )

        let mapping = {
            PhysicalLine = physicalLine
            LogicalLine = logicalLine
            LogicalPath = logicalPath
        }

        {
            sourceMap with
                LineMappings =
                    sourceMap.LineMappings
                    |> Seq.append [ mapping ]
                    |> Seq.sortBy _.PhysicalLine
                    |> ImmutableArray.CreateRange
        }

    let mapPosition sourceMap (position: SourcePosition) =
        let mapping =
            sourceMap.LineMappings
            |> Seq.filter (fun candidate -> candidate.PhysicalLine <= position.Line)
            |> Seq.tryLast

        match mapping with
        | None ->
            {
                Offset = position.Offset
                Line = position.Line
                Column = position.Column
                LogicalPath = None
            }
        | Some mapping ->
            {
                Offset = position.Offset
                Line = mapping.LogicalLine + position.Line - mapping.PhysicalLine
                Column = position.Column
                LogicalPath = mapping.LogicalPath
            }

type internal DecodedSource = {
    Text: string
    Encoding: SourceEncoding
    BomLength: int
    Map: SourceMap
}

module internal SourceText =
    let private decodeWith (encoding: Encoding) sourceEncoding bomLength (bytes: byte array) =
        let text = encoding.GetString(bytes, bomLength, bytes.Length - bomLength)

        {
            Text = text
            Encoding = sourceEncoding
            BomLength = bomLength
            Map = SourceMap.create text
        }

    let decode (bytes: ImmutableArray<byte>) =
        if bytes.IsDefault then
            invalidArg "bytes" "bytes must be initialized."

        let values = bytes |> Seq.toArray

        if values.Length >= 3 && values[0] = 0xEFuy && values[1] = 0xBBuy && values[2] = 0xBFuy then
            decodeWith (UTF8Encoding(false, false)) SourceEncoding.Utf8 3 values
        elif values.Length >= 2 && values[0] = 0xFFuy && values[1] = 0xFEuy then
            decodeWith Encoding.Unicode SourceEncoding.Utf16LittleEndian 2 values
        elif values.Length >= 2 && values[0] = 0xFEuy && values[1] = 0xFFuy then
            decodeWith Encoding.BigEndianUnicode SourceEncoding.Utf16BigEndian 2 values
        else
            decodeWith (UTF8Encoding(false, false)) SourceEncoding.Utf8 0 values

    let fromString text =
        if isNull text then
            nullArg "text"

        {
            Text = text
            Encoding = SourceEncoding.Utf8
            BomLength = 0
            Map = SourceMap.create text
        }
