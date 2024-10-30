namespace Hearts.FastCfr

open System.IO

module Strategy =

    let save (strategyMap : Map<string, float[]>) =
        let path = "Hearts.strategy"
        use stream = new FileStream(path, FileMode.Create)
        use wtr = new BinaryWriter(stream)
        wtr.Write(strategyMap.Count)
        for (KeyValue(key, strategy)) in strategyMap do
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
                let key = rdr.ReadString()
                let strategyCount = rdr.ReadInt32()
                let strategy =
                    [|
                        for _ = 1 to strategyCount do
                            rdr.ReadDouble()
                    |]
                key, strategy
            assert(stream.Position = stream.Length)
        |]