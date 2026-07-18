namespace FSharp2.Compiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Security.Cryptography

type internal StrongNamePlan = {
    Mode: StrongNameMode
    PublicKey: byte array
    SignatureSize: int
    PrivateKey: RSAParameters option
}

with
    override _.ToString() = "StrongNamePlan"

/// Independently authored adapter for the standard CAPI key blobs used by
/// strong-name key files and the public ManagedPEBuilder signing seam.
module internal StrongName =
    let private readExactly (reader: BinaryReader) count =
        let bytes = reader.ReadBytes(count)

        if bytes.Length <> count then
            invalidArg "keyBytes" "the strong-name key blob is truncated"

        bytes

    let private exponentBytes (value: uint32) =
        let bytes = BitConverter.GetBytes(value) |> Array.rev
        let firstNonZero = bytes |> Array.tryFindIndex ((<>) 0uy)

        match firstNonZero with
        | Some index -> bytes.[index..]
        | None -> invalidArg "keyBytes" "the strong-name exponent must be non-zero"

    let private assemblyPublicKey (capiPublicKey: byte array) =
        use stream = new MemoryStream()
        use writer = new BinaryWriter(stream)
        writer.Write(0x00002400u)
        writer.Write(0x00008004u)
        writer.Write(uint32 capiPublicKey.Length)
        writer.Write(capiPublicKey)
        writer.Flush()
        stream.ToArray()

    let private parseCapiKey (keyBytes: byte array) =
        use stream = new MemoryStream(keyBytes, false)
        use reader = new BinaryReader(stream)
        let blobType = reader.ReadByte()
        let version = reader.ReadByte()
        let reserved = reader.ReadUInt16()
        let algorithm = reader.ReadUInt32()

        if blobType <> 0x06uy && blobType <> 0x07uy then
            invalidArg "keyBytes" "the strong-name key must be a CAPI public or private key blob"

        if version <> 0x02uy || reserved <> 0us || algorithm <> 0x00002400u then
            invalidArg "keyBytes" "the strong-name key has an unsupported CAPI header"

        let magic = reader.ReadUInt32()
        let expectedMagic = if blobType = 0x07uy then 0x32415352u else 0x31415352u

        if magic <> expectedMagic then
            invalidArg "keyBytes" "the strong-name key has an invalid RSA header"

        let bitLength = reader.ReadUInt32()

        if bitLength = 0u || bitLength % 16u <> 0u then
            invalidArg "keyBytes" "the strong-name key has an invalid modulus size"

        let signatureSize = int bitLength / 8
        let halfSize = signatureSize / 2
        let exponent = reader.ReadUInt32()
        let modulusLittleEndian = readExactly reader signatureSize

        use publicStream = new MemoryStream()
        use publicWriter = new BinaryWriter(publicStream)
        publicWriter.Write(0x06uy)
        publicWriter.Write(version)
        publicWriter.Write(reserved)
        publicWriter.Write(algorithm)
        publicWriter.Write(0x31415352u)
        publicWriter.Write(bitLength)
        publicWriter.Write(exponent)
        publicWriter.Write(modulusLittleEndian)
        publicWriter.Flush()

        let privateKey =
            if blobType = 0x07uy then
                let mutable parameters = RSAParameters()
                parameters.Modulus <- Array.rev modulusLittleEndian
                parameters.Exponent <- exponentBytes exponent
                parameters.P <- readExactly reader halfSize |> Array.rev
                parameters.Q <- readExactly reader halfSize |> Array.rev
                parameters.DP <- readExactly reader halfSize |> Array.rev
                parameters.DQ <- readExactly reader halfSize |> Array.rev
                parameters.InverseQ <- readExactly reader halfSize |> Array.rev
                parameters.D <- readExactly reader signatureSize |> Array.rev

                Some parameters
            else
                None

        if stream.Position <> stream.Length then
            invalidArg "keyBytes" "the strong-name key contains trailing data"

        assemblyPublicKey (publicStream.ToArray()), signatureSize, privateKey

    let private clearPrivateKey (parameters: RSAParameters) =
        for bytes in
            [|
                parameters.D
                parameters.DP
                parameters.DQ
                parameters.InverseQ
                parameters.P
                parameters.Q
            |] do
            if not (isNull bytes) then
                CryptographicOperations.ZeroMemory(bytes.AsSpan())

    let createPlan mode (keyBytes: byte array) =
        match mode with
        | Unsigned ->
            if keyBytes.Length <> 0 then
                invalidArg "keyBytes" "unsigned output must not include a strong-name key"

            {
                Mode = mode
                PublicKey = Array.empty
                SignatureSize = 0
                PrivateKey = None
            }
        | DelaySign
        | PublicSign
        | FullSign ->
            let publicKey, signatureSize, privateKey = parseCapiKey keyBytes

            if mode = FullSign && privateKey.IsNone then
                invalidArg "keyBytes" "full signing requires a private key"

            let retainedPrivateKey =
                match mode, privateKey with
                | FullSign, _ -> privateKey
                | _, Some parameters ->
                    clearPrivateKey parameters
                    None
                | _, None -> None

            {
                Mode = mode
                PublicKey = publicKey
                SignatureSize = signatureSize
                PrivateKey = retainedPrivateKey
            }

    let assemblyFlags plan =
        if plan.Mode = Unsigned then
            enum<AssemblyFlags> 0
        else
            AssemblyFlags.PublicKey

    let assemblyHashAlgorithm plan =
        if plan.Mode = Unsigned then
            AssemblyHashAlgorithm.Sha256
        else
            AssemblyHashAlgorithm.Sha1

    let corFlags plan =
        match plan.Mode with
        | PublicSign
        | FullSign -> CorFlags.ILOnly ||| CorFlags.StrongNameSigned
        | Unsigned
        | DelaySign -> CorFlags.ILOnly

    let sign (builder: ManagedPEBuilder) (image: BlobBuilder) plan =
        match plan.Mode, plan.PrivateKey with
        | FullSign, Some parameters ->
            use rsa = RSA.Create()
            rsa.ImportParameters(parameters)

            try
                builder.Sign(
                    image,
                    Func<IEnumerable<Blob>, byte array>(fun blobs ->
                        use hash = IncrementalHash.CreateHash(HashAlgorithmName.SHA1)

                        for blob in blobs do
                            hash.AppendData(blob.GetBytes().AsSpan())

                        let signature =
                            rsa.SignHash(
                                hash.GetHashAndReset(),
                                HashAlgorithmName.SHA1,
                                RSASignaturePadding.Pkcs1
                            )

                        Array.Reverse(signature)
                        signature
                    )
                )
            finally
                clearPrivateKey parameters
        | _ -> ()
