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

    /// Number of bytes in a packed encoding.
    let private packedEncodingSize =
        (Model.inputSize + 7) / 8   // round up

    /// Packs the given encoding into bytes.
    let private packEncoding (wtr : BinaryWriter) (encoding : Encoding) =
        assert(encoding.Length = Model.inputSize)
        for chunk in Array.chunkBySize 8 encoding do   // 8 bits/byte
            (0uy, Array.indexed chunk)
                ||> Array.fold (fun byte (i, flag) ->
                    if flag then byte ||| (1uy <<< i)
                    else byte)
                |> wtr.Write

    /// Unpacks bytes into an encoding.
    let private unpackEncoding (rdr : BinaryReader) : Encoding =
        rdr.ReadBytes(packedEncodingSize)
            |> Array.collect (fun byte ->
                Array.init 8 (fun i ->      // 8 bits/byte
                    (byte &&& (1uy <<< i)) <> 0uy))
            |> Array.take Model.inputSize   // trim padding

    /// Maximum possible number of actions available in an info
    /// set.
    let private maxActionCount =
        Hearts.ClosedDeal.numCardsPerHand

    /// Packs the given regrets into a sparse representation.
    let private packRegrets (wtr : BinaryWriter) (regrets : Vector<float32>) =

        let tuples =
            [|
                for i, regret in Seq.indexed regrets do
                    if regret <> 0f then
                        i, regret
            |]
        assert(tuples.Length <= maxActionCount)

        for i, regret in tuples do
            assert(i < int Byte.MaxValue)
            wtr.Write(byte i)
            wtr.Write(regret)

        for _ = tuples.Length to maxActionCount - 1 do
            wtr.Write(Byte.MaxValue)
            wtr.Write(0f)

    /// Unpacks sparse bytes into a regrets vector.
    let private unpackRegrets (rdr : BinaryReader) =
        seq {
            for _ = 0 to maxActionCount - 1 do
                let i = rdr.ReadByte()
                let regret = rdr.ReadSingle()
                if regret = 0f then
                    assert(i = Byte.MaxValue)
                else
                    int i, regret
        }
            |> SparseVector.ofSeqi Model.outputSize
            |> CreateVector.DenseOfVector

    /// Number of bytes in packed regrets.
    let private regretsSize =
        maxActionCount * (sizeof<byte> + sizeof<float32>)

    /// Number of bytes in a serialized sample.
    let private sampleSize =
        packedEncodingSize
            + regretsSize
            + sizeof<byte> (*iteration*)
            |> int64

    /// Is the given store in a valid state?
    let private isValid store =
        store.Stream.Length % sampleSize = 0L

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
        store.Stream.Length / int64 sampleSize

    /// Reads the sample at the given index in the given store.
    let readSample (idx : int64) store =
        assert(isValid store)
        assert(idx >= 0)
        assert(idx < getSampleCount store)
        store.Stream.Position <- idx * sampleSize

        use rdr =
            new BinaryReader(
                store.Stream, Text.Encoding.UTF8, leaveOpen = true)
        let encoding = unpackEncoding rdr
        let regrets = unpackRegrets rdr
        let iteration = int (rdr.ReadByte())
        assert(iteration >= int Byte.MinValue)
        assert(iteration <= int Byte.MaxValue)
        AdvantageSample.create encoding regrets iteration

    /// Appends the given samples to the end of the given store.
    let writeSamples samples store =
        assert(isValid store)
        store.Stream.Position <- store.Stream.Length

        use wtr =
            new BinaryWriter(
                store.Stream, Text.Encoding.UTF8, leaveOpen = true)

        for sample in samples do
            packEncoding wtr sample.Encoding
            packRegrets wtr sample.Regrets
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
