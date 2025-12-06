namespace Hearts.TrainKiller

open System
open System.IO
open System.Text

open Hearts
open Hearts.Model
open Hearts.ParseKiller

module Program =

    let getExchangeActions exchangeOpt deal =
        exchangeOpt
            |> Option.map (fun exchange ->
                let player = OpenDeal.currentPlayer deal
                seq {
                    for seat in Seat.cycle player do
                        yield! exchange.PassMap[seat]
                })
            |> Option.defaultValue Seq.empty
            |> Seq.map (fun card -> ActionType.Pass, card)

    let getPlayoutActions tricks =
        seq {
            for trick in tricks do
                for _, card in Trick.plays trick do
                    ActionType.Play, card
        }

    module Encoding =

        let getByteLength nBits =
            (nBits + 7) / 8

        let fromBytes nBits (bytes : byte[]) : Encoding =
            let bits = Encoding(bytes)
            assert(nBits <= bits.Length)
            assert(
                [nBits .. bits.Length - 1]
                    |> Seq.forall (fun i -> not bits[i]))   // padding must be empty
            bits.Length <- nBits
            bits

        let toBytes (bits : Encoding) =
            let nBytes = getByteLength bits.Length
            let bytes = Array.zeroCreate<byte> nBytes
            bits.CopyTo(bytes, 0)
            bytes

    let encode () =

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let entries =
            Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.zero.json"
        printfn $"Loaded {entries.Length} deals in {stopwatch.Elapsed}"

        use stream = File.Open("KHearts.dat", FileMode.Create)
        use wtr = new BinaryWriter(stream, Encoding.UTF8, false)

        let pairs =
            [|
                for entry in entries do
                    let actions =
                        [|
                            yield! getExchangeActions
                                entry.ExchangeOpt entry.InitialDeal
                            yield! getPlayoutActions entry.Tricks
                        |]
                    let deals =
                        (entry.InitialDeal, actions)
                            ||> Array.scan (fun deal (actionType, card) ->
                                OpenDeal.addAction actionType card deal)
                    assert(deals.Length = actions.Length + 1)
                    for deal, (actionType, card) in Seq.zip deals actions do   // ignore final deal state
                        let infoSet = OpenDeal.currentInfoSet deal
                        assert(infoSet.LegalActionType = actionType)
                        assert(infoSet.LegalActions |> Array.contains card)
                        if infoSet.LegalActions.Length > 1 then
                            infoSet, card
            |]
        printfn $"Encoding {pairs.Length} pairs"
        for infoSet, card in pairs do
            Encoding.encode infoSet
                |> Encoding.toBytes
                |> wtr.Write
            Encoding.encodeCards [card]
                |> Encoding
                |> Encoding.toBytes
                |> wtr.Write

    let decode () =
        use stream = File.OpenRead(@"C:\Users\brian\OneDrive\Desktop\KHearts.dat")
        let nBytesPerPair =
            Encoding.getByteLength Model.inputSize
                + Encoding.getByteLength Model.outputSize
        printfn $"Expecting {stream.Length / int64 nBytesPerPair} encoded pairs"
        assert(stream.Length % int64 nBytesPerPair = 0)
        use rdr = new BinaryReader(stream)
        [|
            while stream.Position < stream.Length do
                let input : Encoding =
                    Model.inputSize
                        |> Encoding.getByteLength
                        |> rdr.ReadBytes
                        |> Encoding.fromBytes Model.inputSize
                let output : Encoding =
                    Model.outputSize
                        |> Encoding.getByteLength
                        |> rdr.ReadBytes
                        |> Encoding.fromBytes Model.outputSize
                input, output
        |]

    Console.OutputEncoding <- Encoding.Unicode
    // encode ()
    let pairs = decode ()
    printfn $"Decoded {pairs.Length} pairs"
