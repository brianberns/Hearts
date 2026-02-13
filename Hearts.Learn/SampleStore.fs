namespace Hearts.Learn

open System
open System.IO

open MathNet.Numerics.LinearAlgebra

open Hearts.Model

type AdvantageSampleStore =
    {
        Stream : FileStream
    }

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.Stream.Dispose()

module AdvantageSampleStore =

    let private pack flags =
        [|
            for chunk in Array.chunkBySize 8 flags do   // 8 bits/byte
                (0uy, Array.indexed chunk)
                    ||> Array.fold (fun byte (i, flag) ->
                        if flag then byte ||| (1uy <<< i)
                        else byte)
        |]

    let private unpack nFlags bytes =
        bytes
            |> Array.collect (fun byte ->
                Array.init 8 (fun i ->
                    (byte &&& (1uy <<< i)) <> 0uy))
            |> Array.take nFlags

    let private encodingLength =
        (Model.inputSize + 7) / 8   // round up

    let private sampleLength =
        encodingLength * sizeof<byte>   // encoding
            + Model.outputSize * sizeof<float32>        // regrets
            + sizeof<byte>                              // iteration
            |> int64

    let private isValid store =
        store.Stream.Length % sampleLength = 0L

    let openOrCreate path =
        let stream =
            new FileStream(path, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        let store = { Stream = stream }
        assert(isValid store)
        store

    let getSampleCount store =
        assert(isValid store)
        store.Stream.Length / int64 sampleLength

    let readSample idx store =
        assert(isValid store)
        store.Stream.Position <- int64 idx * sampleLength

        use rdr =
            new BinaryReader(
                store.Stream, Text.Encoding.UTF8, leaveOpen = true)
        let encoding =
            rdr.ReadBytes(encodingLength)
                |> unpack Model.inputSize
        let regrets =
            Seq.init Model.outputSize (fun _ -> rdr.ReadSingle())
                |> SparseVector.ofSeq
        let iteration = int (rdr.ReadByte())
        AdvantageSample.create encoding regrets iteration

    let writeSamples samples store =
        assert(isValid store)
        store.Stream.Position <- store.Stream.Length

        use wtr =
            new BinaryWriter(
                store.Stream, Text.Encoding.UTF8, leaveOpen = true)

        for sample in samples do

            wtr.Write(pack sample.Encoding)

            for regret in sample.Regrets do
                wtr.Write(regret)

            assert(sample.Iteration >= int Byte.MinValue)
            assert(sample.Iteration <= int Byte.MaxValue)
            wtr.Write(byte sample.Iteration)

        assert(isValid store)

type AdvantageSampleStore with

    member store.Count =
        AdvantageSampleStore.getSampleCount store

    member store.Item
        with get(idx) =
            AdvantageSampleStore.readSample idx store

    member store.Samples =
        [|
            for i = 0L to store.Count - 1L do
                store[i]
        |]