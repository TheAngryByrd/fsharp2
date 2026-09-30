namespace FSharp2.Compiler

open System
open System.Globalization
open System.Numerics
open System.Text.RegularExpressions

type internal NumericLiteralRangeError = { Code: string; Message: string }

[<RequireQualifiedAccess>]
type internal NumericLiteralRange =
    | Fits
    | FitsOnlyAfterMinus of NumericLiteralRangeError
    | Outside of NumericLiteralRangeError

type internal NumericLiteral = private {
    takesSign: bool
    range: NumericLiteralRange
} with

    // FCS merges an adjacent sign into every numeric literal except an unsigned integer.
    member this.TakesSign = this.takesSign
    member this.Range = this.range

module internal NumericLiteral =
    let private decimalDigits = "[0-9](?:_*[0-9])*"

    let private radixDigits =
        "(?:0[xX][0-9a-fA-F](?:_*[0-9a-fA-F])*|0[oO][0-7](?:_*[0-7])*|0[bB][01](?:_*[01])*)"

    let private floating =
        $"(?:{decimalDigits}\\.(?:{decimalDigits})?(?:[eE][+-]?{decimalDigits})?|{decimalDigits}[eE][+-]?{decimalDigits})"

    let private form pattern =
        Regex($"^(?:{pattern})$", RegexOptions.CultureInvariant)

    let private unsignedForm =
        form $"(?:{decimalDigits}|{radixDigits})(?:u|uy|us|ul|uL|UL|un)"

    let private signedIntegerForm =
        form $"(?:{decimalDigits}|{radixDigits})(?:y|s|l|n|L)?"

    let private otherForm =
        form (
            String.concat "|" [
                $"{decimalDigits}[QRZING]"
                $"{radixDigits}(?:lf|LF)"
                $"(?:{floating}|{decimalDigits})[fF]"
                floating
                $"(?:{floating}|{decimalDigits})[mM]"
            ]
        )

    let private outside code kind = {
        Code = code
        Message = $"This number is outside the allowable range for {kind}"
    }

    let private sbyteRange = outside "FS1142" "8-bit signed integers"
    let private radixSbyteRange = outside "FS1143" "hexadecimal 8-bit signed integers"
    let private byteRange = outside "FS1144" "8-bit unsigned integers"
    let private int16Range = outside "FS1145" "16-bit signed integers"
    let private uint16Range = outside "FS1146" "16-bit unsigned integers"
    let private int32Range = outside "FS1147" "32-bit signed integers"
    let private uint32Range = outside "FS1148" "32-bit unsigned integers"
    let private int64Range = outside "FS1149" "64-bit signed integers"
    let private uint64Range = outside "FS1150" "64-bit unsigned integers"
    let private nativeIntRange = outside "FS1151" "signed native integers"
    let private unativeIntRange = outside "FS1152" "unsigned native integers"

    let private doubleBitsRange = {
        Code = "FS1153"
        Message = "Invalid floating point number"
    }

    let private decimalRange = outside "FS1154" "decimal literals"
    let private singleBitsRange = outside "FS1155" "32-bit floats"

    let rangeErrorCodes =
        set [
            for error in
                [
                    sbyteRange
                    radixSbyteRange
                    byteRange
                    int16Range
                    uint16Range
                    int32Range
                    uint32Range
                    int64Range
                    uint64Range
                    nativeIntRange
                    unativeIntRange
                    doubleBitsRange
                    decimalRange
                    singleBitsRange
                ] -> error.Code
        ]

    let private isRadix (text: string) =
        text.Length > 1
        && text[0] = '0'
        && "xXoObB".IndexOf text[1]
           >= 0

    let private magnitude (text: string) (suffixLength: int) =
        let digits =
            text.Substring(
                0,
                text.Length
                - suffixLength
            )

        let radix, digits =
            if isRadix digits then
                (match Char.ToLowerInvariant digits[1] with
                 | 'x' -> 16
                 | 'o' -> 8
                 | _ -> 2),
                digits.Substring 2
            else
                10, digits

        digits
        |> Seq.filter (fun digit ->
            digit
            <> '_'
        )
        |> Seq.fold
            (fun (value: BigInteger) digit ->
                value
                * BigInteger radix
                + BigInteger("0123456789abcdef".IndexOf(Char.ToLowerInvariant digit))
            )
            BigInteger.Zero

    let private below bits (value: BigInteger) =
        value < BigInteger.Pow(BigInteger 2, bits)

    let private unsignedRange (text: string) =
        let suffix, bits, error =
            if text.EndsWith("uy", StringComparison.Ordinal) then
                "uy", 8, byteRange
            elif text.EndsWith("us", StringComparison.Ordinal) then
                "us", 16, uint16Range
            elif text.EndsWith("un", StringComparison.Ordinal) then
                "un", 64, unativeIntRange
            elif text.EndsWith("ul", StringComparison.Ordinal) then
                "ul", 32, uint32Range
            elif
                text.EndsWith("uL", StringComparison.Ordinal)
                || text.EndsWith("UL", StringComparison.Ordinal)
            then
                "uL", 64, uint64Range
            else
                "u", 32, uint32Range

        if below bits (magnitude text suffix.Length) then
            NumericLiteralRange.Fits
        else
            NumericLiteralRange.Outside error

    // A radix literal takes every bit pattern of its type, and a decimal literal at the signed limit is valid only after '-'.
    let private signedIntegerRange (text: string) =
        let suffixLength, bits, error =
            match
                text[text.Length
                     - 1]
            with
            | 'y' -> 1, 8, sbyteRange
            | 's' -> 1, 16, int16Range
            | 'l' -> 1, 32, int32Range
            | 'L' -> 1, 64, int64Range
            | 'n' -> 1, 64, nativeIntRange
            | _ -> 0, 32, int32Range

        let value = magnitude text suffixLength

        if isRadix text then
            if below bits value then
                NumericLiteralRange.Fits
            elif bits = 8 then
                NumericLiteralRange.Outside radixSbyteRange
            else
                NumericLiteralRange.Outside error
        elif below (bits - 1) value then
            NumericLiteralRange.Fits
        elif value = BigInteger.Pow(BigInteger 2, bits - 1) then
            NumericLiteralRange.FitsOnlyAfterMinus error
        else
            NumericLiteralRange.Outside error

    let private otherRange (text: string) =
        if text.EndsWith("lf", StringComparison.Ordinal) then
            if below 32 (magnitude text 2) then
                NumericLiteralRange.Fits
            else
                NumericLiteralRange.Outside singleBitsRange
        elif text.EndsWith("LF", StringComparison.Ordinal) then
            if below 64 (magnitude text 2) then
                NumericLiteralRange.Fits
            else
                NumericLiteralRange.Outside doubleBitsRange
        elif
            "mM".IndexOf
                text[text.Length
                     - 1]
            >= 0
        then
            let readable, _ =
                Decimal.TryParse(
                    text
                        .Substring(
                            0,
                            text.Length
                            - 1
                        )
                        .Replace("_", ""),
                    NumberStyles.AllowExponent
                    ||| NumberStyles.Number,
                    CultureInfo.InvariantCulture
                )

            if readable then
                NumericLiteralRange.Fits
            else
                NumericLiteralRange.Outside decimalRange
        else
            NumericLiteralRange.Fits

    let tryParse (text: string) =
        if unsignedForm.IsMatch text then
            Some {
                takesSign = false
                range = unsignedRange text
            }
        elif signedIntegerForm.IsMatch text then
            Some {
                takesSign = true
                range = signedIntegerRange text
            }
        elif otherForm.IsMatch text then
            Some {
                takesSign = true
                range = otherRange text
            }
        else
            None
