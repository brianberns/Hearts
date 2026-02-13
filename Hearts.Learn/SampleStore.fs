namespace Hearts.Learn

open System
open System.IO
open System.Text

type AdvantageSampleStore =
    {
        Stream : FileStream
        Version : uint16
    }

    interface IDisposable with

        /// Cleanup.
        member this.Dispose() =
            this.Stream.Dispose()

module AdvantageSampleStore =

    let private magic = "Hearts"B

    let private version = 1us

    let private create stream =
        use wtr = new BinaryWriter(stream, Encoding.UTF8, leaveOpen = true)
        wtr.Write(magic)
        wtr.Write(version)
        {
            Version = version
            Stream = stream
        }

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
        }

    let attach path : AdvantageSampleStore =
        let stream =
            new FileStream(path, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        if stream.Length = 0 then
            create stream
        else
            openFrom stream
