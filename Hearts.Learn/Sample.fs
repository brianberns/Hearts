namespace Hearts.Learn

open System
open System.IO

open MathNet.Numerics.LinearAlgebra

open Hearts.Model

/// An observed advantage event.
type AdvantageSample =
    {
        /// Encoded info set.
        Encoding : Encoding

        /// Observed regrets.
        Regrets : Vector<float32>

        /// 1-based iteration number.
        Iteration : int
    }

module AdvantageSample =

    /// Creates an advantage sample.
    let create encoding regrets iteration =
        assert(Array.length encoding = Model.inputSize)
        assert(Vector.length regrets = Model.outputSize)
        assert(iteration >= 1)
        {
            Encoding = encoding
            Regrets = regrets
            Iteration = iteration
        }

/// Persistent store for advantage samples.
type AdvantageSampleStore =
    {
        Stream : FileStream
    }

    /// Cleanup.
    member this.Dispose() =
        this.Stream.Dispose()

    /// Cleanup.
    interface IDisposable with
        member this.Dispose() =
            this.Dispose()

module AdvantageSampleStore =

    /// Packs the given encoding into bytes.
    let private packEncoding (encoding : Encoding) =
        [|
            for chunk in Array.chunkBySize 8 encoding do   // 8 bits/byte
                (0uy, Array.indexed chunk)
                    ||> Array.fold (fun byte (i, flag) ->
                        if flag then byte ||| (1uy <<< i)
                        else byte)
        |]

    /// Unpacks the given bytes into an encoding.
    let private unpackEncoding encodingLength bytes =
        bytes
            |> Array.collect (fun byte ->
                Array.init 8 (fun i ->
                    (byte &&& (1uy <<< i)) <> 0uy))
            |> Array.take encodingLength

    /// Number of bytes in an encoded model input.
    let private encodingLength =
        (Model.inputSize + 7) / 8   // round up

    /// Number of bytes in a serialized sample.
    let private sampleLength =
        encodingLength * sizeof<byte>              // encoding
            + Model.outputSize * sizeof<float32>   // regrets
            + sizeof<byte>                         // iteration
            |> int64

    /// Is the given store in a valid state?
    let private isValid store =
        store.Stream.Length % sampleLength = 0L

    /// Opens or creates a sample store at the given location.
    let openOrCreate path =
        let stream =
            new FileStream(path, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        let store = { Stream = stream }
        assert(isValid store)
        store

    /// Gets the number of samples in the given store.
    let getSampleCount store =
        assert(isValid store)
        store.Stream.Length / int64 sampleLength

    /// Reads the sample at the given index in the given store.
    let readSample (idx : int64) store =
        assert(isValid store)
        assert(idx >= 0)
        assert(idx < getSampleCount store)
        store.Stream.Position <- idx * sampleLength

        use rdr =
            new BinaryReader(
                store.Stream, Text.Encoding.UTF8, leaveOpen = true)
        let encoding =
            rdr.ReadBytes(encodingLength)
                |> unpackEncoding Model.inputSize
        let regrets =
            Seq.init Model.outputSize (fun _ -> rdr.ReadSingle())
                |> DenseVector.ofSeq
        let iteration = int (rdr.ReadByte())
        AdvantageSample.create encoding regrets iteration

    /// Appends the given samples to the end of the given store.
    let writeSamples samples store =
        assert(isValid store)
        store.Stream.Position <- store.Stream.Length

        use wtr =
            new BinaryWriter(
                store.Stream, Text.Encoding.UTF8, leaveOpen = true)

        for sample in samples do

            wtr.Write(packEncoding sample.Encoding)

            for regret in sample.Regrets do
                wtr.Write(regret)

            assert(sample.Iteration >= int Byte.MinValue)
            assert(sample.Iteration <= int Byte.MaxValue)
            wtr.Write(byte sample.Iteration)

        assert(isValid store)

type AdvantageSampleStore with

    /// The number of samples in this store.
    member store.Count =
        AdvantageSampleStore.getSampleCount store

    /// Gets the sample at the given index in this store.
    member store.Item
        with get(idx) =
            AdvantageSampleStore.readSample idx store
