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

        let toString (encoding : Encoding) =
            encoding
                |> Seq.cast<bool>
                |> Seq.map (function true -> '1' | false -> '0')
                |> Seq.toArray
                |> String

    let encode () =

        Console.OutputEncoding <- Encoding.Unicode

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let entries =
            Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.zero.json"
        printfn $"Loaded {entries.Length} deals in {stopwatch.Elapsed}"

        use wtr = new StreamWriter("KHearts.dat")

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
                    let inputEncoding =
                        Encoding.encode infoSet
                            |> Encoding.toString
                    let outputEncoding =
                        Encoding.encodeCards [card]
                            |> Encoding
                            |> Encoding.toString
                    wtr.Write($"{inputEncoding}|{outputEncoding}")

    encode ()
