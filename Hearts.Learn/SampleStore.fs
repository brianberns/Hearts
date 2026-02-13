namespace Hearts.Learn

open System
open System.IO
open System.Text

open MathNet.Numerics.LinearAlgebra

open Hearts.Model

type AdvantageSampleStore =
    {
        Stream : FileStream
        Version : uint16
        SampleCounts : Map<int (*iteration*), int (*count*)>
    }

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.Stream.Dispose()

module AdvantageSampleStore =

    let private magic = "Hearts"B

    let private version = 1us

    let private pack flags (wtr : BinaryWriter) =
        for chunk in Array.chunkBySize 8 flags do   // 8 bits/byte
            let byte =
                (0uy, Array.indexed chunk)
                    ||> Array.fold (fun byte (i, flag) ->
                        if flag then byte ||| (1uy <<< i)
                        else byte)
            wtr.Write(byte)

    let private unpack nFlags (rdr : BinaryReader) =
        rdr.ReadBytes((nFlags + 7) / 8)   // round up
            |> Array.collect (fun byte ->
                Array.init 8 (fun i ->
                    (byte &&& (1uy <<< i)) <> 0uy))
            |> Array.take nFlags

    let private create stream =
        use wtr = new BinaryWriter(stream, Encoding.UTF8, leaveOpen = true)
        wtr.Write(magic)
        wtr.Write(version)
        {
            Version = version
            Stream = stream
            SampleCounts = Map.empty
        }

    let private readSample rdr =
        let encoding = unpack Model.inputSize rdr
        let regrets =
            Seq.init Model.outputSize (fun _ -> rdr.ReadSingle())
                |> SparseVector.ofSeq
        let iteration = rdr.ReadInt32()
        AdvantageSample.create encoding regrets iteration

    let private getSampleCounts (rdr : BinaryReader) =
        let samples =
            seq {
                while rdr.BaseStream.Position < rdr.BaseStream.Length do
                    yield readSample rdr
            }
        samples
            |> Seq.map _.Iteration
            |> Seq.groupBy id
            |> Seq.map (fun (iter, group) ->
                iter, Seq.length group)
            |> Map

    let private openFrom (stream : FileStream) =
        assert(stream.Position = 0L)
        use rdr = new BinaryReader(stream, Encoding.UTF8, leaveOpen = true)
        if rdr.ReadBytes(magic.Length) <> magic then
            failwith "Invalid header: magic"
        if rdr.ReadUInt16() <> version then
            failwith "Invalid header: version"
        {
            Stream = stream
            Version = version
            SampleCounts = getSampleCounts rdr
        }

    let attach path : AdvantageSampleStore =
        let stream =
            new FileStream(path, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        if stream.Length = 0 then
            create stream
        else
            openFrom stream
