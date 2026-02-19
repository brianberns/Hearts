namespace Hearts.Learn

open System
open System.IO

open Microsoft.Win32.SafeHandles

open MathNet.Numerics.LinearAlgebra

open Hearts.Model

/// Persistent store for advantage samples.
type AdvantageSampleStore =
    {
        /// Underlying file handle.
        Handle : SafeFileHandle

        /// Path of the underlying file.
        Path : string

        /// Iteration for all samples in this store.
        Iteration : int
    }

    /// Cleanup.
    member this.Dispose() =
        this.Handle.Dispose()

    /// Cleanup.
    interface IDisposable with
        member this.Dispose() =
            this.Dispose()

module AdvantageSampleStore =

    module private Header =

        /// File format identifier.
        let private magic = "Hrts"B

        /// Number of bytes in a packed header.
        let packedSize =
            (magic.Length * sizeof<byte>)
                + sizeof<int32>

        /// Writes a header to the given file.
        let write (handle : SafeFileHandle) (iteration : int32) =
            assert(RandomAccess.GetLength(handle) = 0L)

            RandomAccess.Write(
                handle, ReadOnlySpan(magic), 0L)

            assert(iteration >= 0)
            let buf = BitConverter.GetBytes(iteration)
            RandomAccess.Write(
                handle, ReadOnlySpan(buf), int64 magic.Length)

        /// Reads a header from the given file.
        let read (handle : SafeFileHandle) =
            let magic' = Array.zeroCreate magic.Length
            let bytesRead =
                RandomAccess.Read(handle, Span(magic'), 0L)
            assert(bytesRead = magic.Length)
            if magic' <> magic then
                failwith $"Invalid magic bytes: {magic'}"

            let buf = Array.zeroCreate sizeof<int32>
            let bytesRead =
                RandomAccess.Read(
                    handle, Span(buf), int64 magic.Length)
            assert(bytesRead = sizeof<int32>)
            let iter = BitConverter.ToInt32(buf, 0)
            assert(iter >= 0)
            iter

    module private Encoding =

        /// Number of bytes in a packed encoding.
        let packedSize =
            (Model.inputSize + 7) / 8   // round up

        /// Writes the given encoding to the given file.
        let write (handle : SafeFileHandle) (fileOffset : int64) (encoding : Encoding) =
            assert(encoding.Length = Model.inputSize)
            let buf =
                [|
                    for chunk in Array.chunkBySize 8 encoding do   // 8 bits/byte
                        (0uy, Array.indexed chunk)
                            ||> Array.fold (fun byte (i, flag) ->
                                if flag then byte ||| (1uy <<< i)
                                else byte)
                |]
            RandomAccess.Write(
                handle, ReadOnlySpan(buf), fileOffset)

        /// Reads an encoding from the given file.
        let read (handle : SafeFileHandle) (fileOffset : int64) : Encoding =
            let buf = Array.zeroCreate packedSize
            let bytesRead =
                RandomAccess.Read(handle, Span(buf), fileOffset)
            assert(bytesRead = packedSize)
            buf
                |> Array.collect (fun byte ->
                    Array.init 8 (fun i ->      // 8 bits/byte
                        (byte &&& (1uy <<< i)) <> 0uy))
                |> Array.take Model.inputSize   // trim padding

    module private Regrets =

        /// Maximum possible number of actions available in an info
        /// set.
        let private maxActionCount =
            Hearts.ClosedDeal.numCardsPerHand

        /// Size of one regret entry.
        let private entrySize = sizeof<byte> + sizeof<float32>

        /// Number of bytes in packed regrets.
        let packedSize =
            maxActionCount * entrySize

        /// Writes the given regrets to the given file.
        let write (handle : SafeFileHandle) (fileOffset : int64) (regrets : Vector<float32>) =

            let tuples =
                [|
                    for i, regret in Seq.indexed regrets do
                        if regret <> 0f then
                            i, regret
                |]
            assert(tuples.Length <= maxActionCount)

            let buf =
                [|
                    for i, regret in tuples do
                        assert(i < int Byte.MaxValue)
                        byte i
                        yield! BitConverter.GetBytes(regret)

                    for _ = tuples.Length to maxActionCount - 1 do
                        Byte.MaxValue
                        yield! BitConverter.GetBytes(0f)
                |]

            RandomAccess.Write(
                handle, ReadOnlySpan(buf), fileOffset)

        /// Reads regrets from the given file.
        let read (handle : SafeFileHandle) (fileOffset : int64) =
            let buf = Array.zeroCreate packedSize
            let bytesRead =
                RandomAccess.Read(handle, Span(buf), fileOffset)
            assert(bytesRead = packedSize)
            seq {
                for j = 0 to maxActionCount - 1 do
                    let pos = j * entrySize
                    let i = buf[pos]
                    let regret = BitConverter.ToSingle(buf, pos + 1)
                    if regret = 0f then
                        assert(i = Byte.MaxValue)
                    else
                        int i, regret
            }
                |> SparseVector.ofSeqi Model.outputSize
                |> CreateVector.DenseOfVector

    /// Number of bytes in a serialized sample.
    let private packedSampleSize =
        Encoding.packedSize + Regrets.packedSize

    /// Is the given store in a valid state?
    let private isValid store =
        (RandomAccess.GetLength(store.Handle)
            - int64 Header.packedSize)
                % int64 packedSampleSize = 0L

    /// Creates a new sample store at the given location.
    let create iteration path =

            // open handle
        let handle =
            File.OpenHandle(
                path,
                FileMode.CreateNew,
                FileAccess.Write,
                FileShare.Read)

            // build store
        let store =
            {
                Handle = handle
                Path = path
                Iteration = iteration
            }
        Header.write handle iteration
        assert(isValid store)
        store

    /// Opens an existing sample store at the given location.
    let openRead path =

            // open handle
        let handle =
            File.OpenHandle(
                path,
                FileMode.Open,
                FileAccess.Read)

            // build store
        let store =
            {
                Handle = handle
                Path = path
                Iteration = Header.read handle
            }
        assert(isValid store)
        store

    /// Gets the number of samples in the given store.
    let getSampleCount store =
        assert(isValid store)
        (RandomAccess.GetLength(store.Handle)
            - int64 Header.packedSize)
                / int64 packedSampleSize

    /// Reads the sample at the given index in the given store.
    let readSample (idx : int64) store =
        assert(isValid store)
        assert(idx >= 0)
        assert(idx < getSampleCount store)
        let fileOffset =
            int64 Header.packedSize
                + idx * int64 packedSampleSize

        let encoding =
            Encoding.read store.Handle fileOffset
        let regrets =
            Regrets.read store.Handle
                (fileOffset + int64 Encoding.packedSize)
        AdvantageSample.create encoding regrets store.Iteration

    /// Appends the given samples to the end of the given store.
    let appendSamples samples store =
        assert(isValid store)
        let mutable fileOffset =
            RandomAccess.GetLength(store.Handle)

        for sample in samples do
            Encoding.write store.Handle fileOffset sample.Encoding
            Regrets.write store.Handle
                (fileOffset + int64 Encoding.packedSize)
                sample.Regrets
            fileOffset <- fileOffset + int64 packedSampleSize

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
