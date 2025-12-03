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

        let private headerLength = 2

        let fromBytes (bytes : byte[]) : Encoding =

                // read header
            assert(headerLength = sizeof<Int16>)
            let payloadLength = int (BitConverter.ToInt16(bytes, 0))

                // read payload
            let payload = bytes[headerLength ..]
            let bits = Encoding(payload)
            assert(payloadLength <= bits.Length)
            assert(
                [payloadLength .. bits.Length - 1]
                    |> Seq.forall (fun i -> not bits[i]))   // padding must be empty
            bits.Length <- payloadLength
            bits

        let toBytes (bits : Encoding) =
        
                // allocate header + payload
            let payloadLength = (bits.Length + 7) / 8
            let bytes =
                Array.zeroCreate<byte> (
                    headerLength + payloadLength)

                // write header
            assert(bits.Length <= int Int16.MaxValue)
            let header = BitConverter.GetBytes(int16 bits.Length)
            assert(header.Length = headerLength)
            Array.Copy(header, 0, bytes, 0, headerLength)

                // write payload
            bits.CopyTo(bytes, headerLength)
            assert(
                Array.forall2 (=)
                    (fromBytes bytes |> Seq.cast<bool> |> Seq.toArray)
                    (bits |> Seq.cast<bool> |> Seq.toArray))
            bytes

    let encode () =

        Console.OutputEncoding <- Encoding.Unicode

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let entries =
            Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.zero.json"
        printfn $"Loaded {entries.Length} deals in {stopwatch.Elapsed}"

        use stream = File.Open("KHearts.dat", FileMode.Create)
        use wtr = new BinaryWriter(stream, Encoding.UTF8, false)

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
                    Encoding.encode infoSet
                        |> Encoding.toBytes
                        |> wtr.Write
                    Encoding.encodeCards [card]
                        |> Encoding
                        |> Encoding.toBytes
                        |> wtr.Write

    encode ()
