namespace Hearts.FastCfr

open System.IO

module Strategy =

    let save (strategyMap : Map<byte[], float[]>) =
        let path = "Hearts.strategy"
        use stream = new FileStream(path, FileMode.Create)
        use wtr = new BinaryWriter(stream)
        wtr.Write(strategyMap.Count)
        for (KeyValue(key, strategy)) in strategyMap do
            wtr.Write(key.Length)
            wtr.Write(key)
            wtr.Write(strategy.Length)
            for prob in strategy do
                wtr.Write(prob)

    let load path =
        use stream = new FileStream(path, FileMode.Open)
        use rdr = new BinaryReader(stream)
        let mapCount = rdr.ReadInt32()
        Map [|
            for _ = 1 to mapCount do
                let keyLength = rdr.ReadInt32()
                let key = rdr.ReadBytes(keyLength)
                let strategyLength = rdr.ReadInt32()
                let strategy =
                    [|
                        for _ = 1 to strategyLength do
                            rdr.ReadDouble()
                    |]
                key, strategy
            assert(stream.Position = stream.Length)
        |]