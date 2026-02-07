namespace Hearts.Learn

open System
open MathNet.Numerics.LinearAlgebra

open Hearts.Model

/// Binary serialization for AdvantageSample records.
/// Uses bit-packing for encoding (540 bits -> 68 bytes).
module SampleSerializer =

    /// Size of encoding in bits.
    let private encodingBits = Encoding.encodedLength   // 540

    /// Size of encoding in bytes (rounded up).
    let private encodingBytes = (encodingBits + 7) / 8   // 68

    /// Number of regret values.
    let private numRegrets = Model.outputSize   // 52

    /// Size of regrets in bytes.
    let private regretsBytes = numRegrets * 4   // 208

    /// Size of iteration in bytes.
    let private iterationBytes = 4

    /// Total size of a serialized sample in bytes.
    let recordSize = encodingBytes + regretsBytes + iterationBytes   // 280

    /// Packs encoding flags into bytes.
    let private packEncoding (encoding : Encoding) =
        let bytes = Array.zeroCreate encodingBytes
        for i = 0 to encodingBits - 1 do
            if encoding[i] > 0.5f then
                let byteIndex = i / 8
                let bitIndex = i % 8
                bytes[byteIndex] <- bytes[byteIndex] ||| (1uy <<< bitIndex)
        bytes

    /// Unpacks bytes into encoding flags.
    let private unpackEncoding (bytes : byte[]) : Encoding =
        let flags = Array.zeroCreate encodingBits
        for i = 0 to encodingBits - 1 do
            let byteIndex = i / 8
            let bitIndex = i % 8
            if bytes[byteIndex] &&& (1uy <<< bitIndex) <> 0uy then
                flags[i] <- 1f
        SparseVector.ofArray flags

    /// Writes a sample to the given buffer at the given offset.
    let write (buffer : byte[]) offset (sample : AdvantageSample) =
        // Pack and write encoding
        let encodingPacked = packEncoding sample.Encoding
        Buffer.BlockCopy(encodingPacked, 0, buffer, offset, encodingBytes)

        // Write regrets
        let regretsArray = sample.Regrets.ToArray()
        Buffer.BlockCopy(regretsArray, 0, buffer, offset + encodingBytes, regretsBytes)

        // Write iteration
        let iterBytes = BitConverter.GetBytes(sample.Iteration)
        Buffer.BlockCopy(iterBytes, 0, buffer, offset + encodingBytes + regretsBytes, iterationBytes)

    /// Reads a sample from the given buffer at the given offset.
    let read (buffer : byte[]) offset : AdvantageSample =
        // Read and unpack encoding
        let encodingBytes' = Array.zeroCreate encodingBytes
        Buffer.BlockCopy(buffer, offset, encodingBytes', 0, encodingBytes)
        let encoding = unpackEncoding encodingBytes'

        // Read regrets
        let regretsArray = Array.zeroCreate<float32> numRegrets
        Buffer.BlockCopy(buffer, offset + encodingBytes, regretsArray, 0, regretsBytes)
        let regrets = DenseVector.ofArray regretsArray

        // Read iteration
        let iteration = BitConverter.ToInt32(buffer, offset + encodingBytes + regretsBytes)

        {
            Encoding = encoding
            Regrets = regrets
            Iteration = iteration
        }

    /// Writes a sample directly to a memory-mapped view accessor.
    let writeTo (accessor : IO.MemoryMappedFiles.MemoryMappedViewAccessor) offset (sample : AdvantageSample) =
        let buffer = Array.zeroCreate recordSize
        write buffer 0 sample
        accessor.WriteArray(offset, buffer, 0, recordSize)

    /// Reads a sample directly from a memory-mapped view accessor.
    let readFrom (accessor : IO.MemoryMappedFiles.MemoryMappedViewAccessor) offset : AdvantageSample =
        let buffer = Array.zeroCreate recordSize
        accessor.ReadArray(offset, buffer, 0, recordSize) |> ignore
        read buffer 0
