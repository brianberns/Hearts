namespace Hearts.Learn

open System
open System.IO

open MathNet.Numerics.LinearAlgebra

open Hearts.Model

/// Persistent store for advantage samples.
type AdvantageSampleStore =
    {
        /// Underlying binary file.
        Stream : FileStream

        /// Iteration for all samples in this store.
        Iteration : int
    }

    /// Cleanup.
    member this.Dispose() =
        this.Stream.Dispose()

    /// Cleanup.
    interface IDisposable with
        member this.Dispose() =
            this.Dispose()

module AdvantageSampleStore =

    module private Header =

        /// File format identifier.
        let private magic = "Hrts"B

        /// Writes a header to the given stream.
        let write (wtr : BinaryWriter) (iteration : int32) =
            assert(wtr.BaseStream.Length = 0)
            assert(wtr.BaseStream.Position = 0)

            wtr.Write(magic)

            assert(iteration >= 0)
            wtr.Write(iteration)

        /// Reads a header from the given stream.
        let read (rdr : BinaryReader) =
            assert(rdr.BaseStream.Position = 0)

            let magic' = rdr.ReadBytes(magic.Length)
            if magic' <> magic then
                failwith $"Invalid magic bytes: {magic'}"

            let iter = rdr.ReadInt32()
            assert(iter >= 0)
            iter

        /// Number of bytes in a packed header.
        let packedSize =
            (magic.Length * sizeof<byte>)
                + sizeof<int32>

    module private Encoding =

        /// Number of bytes in a packed encoding.
        let packedSize =
            (Model.inputSize + 7) / 8   // round up

        /// Writes the given encoding to the given stream.
        let write (wtr : BinaryWriter) (encoding : Encoding) =
            assert(encoding.Length = Model.inputSize)
            for chunk in Array.chunkBySize 8 encoding do   // 8 bits/byte
                (0uy, Array.indexed chunk)
                    ||> Array.fold (fun byte (i, flag) ->
                        if flag then byte ||| (1uy <<< i)
                        else byte)
                    |> wtr.Write

        /// Reads an encoding from the given stream.
        let read (rdr : BinaryReader) : Encoding =
            rdr.ReadBytes(packedSize)
                |> Array.collect (fun byte ->
                    Array.init 8 (fun i ->      // 8 bits/byte
                        (byte &&& (1uy <<< i)) <> 0uy))
                |> Array.take Model.inputSize   // trim padding

    module private Regrets =

        /// Maximum possible number of actions available in an info
        /// set.
        let private maxActionCount =
            Hearts.ClosedDeal.numCardsPerHand

        /// Writes the given regrets to the given stream.
        let write (wtr : BinaryWriter) (regrets : Vector<float32>) =

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

        /// Reads regrets from the given stream.
        let read (rdr : BinaryReader) =
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
        let packedSize =
            maxActionCount * (sizeof<byte> + sizeof<float32>)

    /// Number of bytes in a serialized sample.
    let private packedSampleSize =
        Encoding.packedSize + Regrets.packedSize

    /// Is the given store in a valid state?
    let private isValid store =
        (store.Stream.Length - int64 Header.packedSize)
            % int64 packedSampleSize = 0L

    /// Creates a writer on the given stream.
    let private getWriter stream =
        new BinaryWriter(
            stream, Text.Encoding.UTF8, leaveOpen = true)

    /// Creates a new sample store at the given location.
    let create iteration path =

            // open stream
        let stream =
            new FileStream(
                path,
                FileMode.CreateNew,
                FileAccess.Write,
                FileShare.Read)
        use wtr = getWriter stream

            // build store
        let store =
            {
                Stream = stream
                Iteration = iteration
            }
        Header.write wtr iteration
        assert(isValid store)
        store

    /// Creates a reader on the given stream.
    let private getReader stream =
        new BinaryReader(
            stream, Text.Encoding.UTF8, leaveOpen = true)

    /// Opens an existing sample store at the given location.
    let openRead path =

            // open stream
        let stream =
            new FileStream(path, FileMode.Open, FileAccess.Read)
        use rdr = getReader stream

            // build store
        let store =
            {
                Stream = stream
                Iteration = Header.read rdr
            }
        assert(isValid store)
        store

    /// Gets the number of samples in the given store.
    let getSampleCount store =
        assert(isValid store)
        (store.Stream.Length - int64 Header.packedSize)
            / int64 packedSampleSize

    /// Reads the sample at the given index in the given store.
    let readSample (idx : int64) store =
        assert(isValid store)
        assert(idx >= 0)
        assert(idx < getSampleCount store)
        store.Stream.Position <-
            int64 Header.packedSize
                + idx * int64 packedSampleSize

        use rdr = getReader store.Stream
        let encoding = Encoding.read rdr
        let regrets = Regrets.read rdr
        AdvantageSample.create encoding regrets store.Iteration

    /// Appends the given samples to the end of the given store.
    let writeSamples samples store =
        assert(isValid store)
        store.Stream.Position <- store.Stream.Length

        use wtr = getWriter store.Stream

        for sample in samples do
            Encoding.write wtr sample.Encoding
            Regrets.write wtr sample.Regrets

        assert(isValid store)

type AdvantageSampleStore with

    /// The number of samples in this store.
    member store.Count =
        AdvantageSampleStore.getSampleCount store

    /// Gets the sample at the given index in this store.
    member store.Item
        with get(idx) =
            AdvantageSampleStore.readSample idx store

/// A group of sample stores.
type AdvantageSampleStoreGroup =
    {
        Stores : AdvantageSampleStore[]
    }

    /// Number of stores in this group.
    member this.Count =
        this.Stores.Length

    /// Store indexer.
    member this.Item(iStore) =
        this.Stores[iStore]

    /// Iteration represented by this group.
    member this.Iteration =
        this.Stores
            |> Seq.map _.Iteration
            |> Seq.max

    /// Number of samples in this group.
    member this.NumSamples =
        this.Stores
            |> Seq.sumBy _.Count
