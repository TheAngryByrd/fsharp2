namespace fsharp2.Tests

open System
open System.Collections.Immutable
open System.Text
open Expecto
open FSharp2.Compiler

module SourceTextTests =
    let private immutableBytes (bytes: byte array) = ImmutableArray.CreateRange bytes

    let private decode (encoding: Encoding) (preamble: byte array) (text: string) =
        Array.append preamble (encoding.GetBytes text)
        |> immutableBytes
        |> SourceText.decode

    [<Tests>]
    let tests =
        testList "Issue28.SourceText" [
            testCase "decodes supported BOMs and replaces invalid UTF-8"
            <| fun _ ->
                let utf8 =
                    decode
                        (UTF8Encoding(false))
                        [|
                            0xEFuy
                            0xBBuy
                            0xBFuy
                        |]
                        "module A"

                let utf16le =
                    decode
                        Encoding.Unicode
                        [|
                            0xFFuy
                            0xFEuy
                        |]
                        "module B"

                let utf16be =
                    decode
                        Encoding.BigEndianUnicode
                        [|
                            0xFEuy
                            0xFFuy
                        |]
                        "module C"

                let invalid =
                    SourceText.decode (
                        immutableBytes [|
                            0xC3uy
                            0x28uy
                        |]
                    )

                Expect.equal
                    (utf8.Encoding, utf8.BomLength, utf8.Text)
                    (SourceEncoding.Utf8, 3, "module A")
                    "UTF-8 BOM"

                Expect.equal
                    (utf16le.Encoding, utf16le.BomLength, utf16le.Text)
                    (SourceEncoding.Utf16LittleEndian, 2, "module B")
                    "UTF-16 LE BOM"

                Expect.equal
                    (utf16be.Encoding, utf16be.BomLength, utf16be.Text)
                    (SourceEncoding.Utf16BigEndian, 2, "module C")
                    "UTF-16 BE BOM"

                Expect.equal invalid.Text "\uFFFD(" "Invalid UTF-8 uses replacement decoding"

            testCase "maps UTF-16 offsets and mixed newline forms"
            <| fun _ ->
                let decoded = SourceText.fromString "a\r\nβ\n😀\r"

                let starts =
                    decoded.Map.LineStarts
                    |> Seq.toArray

                let newlines =
                    decoded.Map.Newlines
                    |> Seq.toArray

                Expect.sequenceEqual
                    starts
                    [|
                        0
                        3
                        5
                        8
                    |]
                    "Line starts use UTF-16 offsets"

                Expect.sequenceEqual
                    newlines
                    [|
                        NewlineForm.CarriageReturnLineFeed
                        NewlineForm.LineFeed
                        NewlineForm.CarriageReturn
                        NewlineForm.None
                    |]
                    "Newline forms"

                Expect.equal
                    (SourceMap.positionAt decoded.Map 7)
                    { Offset = 7; Line = 3; Column = 3 }
                    "Surrogate pairs occupy two UTF-16 columns"

                Expect.equal
                    (SourceMap.positionAt decoded.Map 8)
                    { Offset = 8; Line = 4; Column = 1 }
                    "Trailing carriage return starts a final line"

            testCase "preserves embedded NUL and no-final-newline positions"
            <| fun _ ->
                let decoded = SourceText.fromString "let value = 1\u0000"

                Expect.equal decoded.Text.Length 14 "NUL remains in source text"
                Expect.equal decoded.Text[13] '\u0000' "NUL is not discarded"

                Expect.equal
                    (SourceMap.positionAt decoded.Map decoded.Text.Length)
                    { Offset = 14; Line = 1; Column = 15 }
                    "EOF position"

            testCase "maps positions after a line directive"
            <| fun _ ->
                let sourceMap =
                    SourceText.fromString "#line 40 \"mapped.fs\"\nlet a = 1\nlet b = 2"
                    |> _.Map

                let mapped = SourceMap.addLineMapping sourceMap 2 40 (Some "mapped.fs")

                let position =
                    SourceMap.positionAt mapped 40
                    |> SourceMap.mapPosition mapped

                Expect.equal position.LogicalPath (Some "mapped.fs") "Mapped path"

                Expect.equal
                    (position.Line, position.Column)
                    (41, 10)
                    "Mapped line and physical column"
        ]
