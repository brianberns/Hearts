namespace Hearts.TrainKiller

open System
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

    let run () =

        Console.OutputEncoding <- Encoding.Unicode

        let entries =
            Json.loadEntries @"C:\Users\brian\OneDrive\Desktop\KHearts.zero.json"
        printfn $"Loaded {entries.Length} deals"

        for entry in entries do
            let actions =
                Seq.append
                    (getExchangeActions
                        entry.ExchangeOpt entry.InitialDeal)
                    (getPlayoutActions entry.Tricks)
            let deals =
                seq {
                    entry.InitialDeal
                    yield! (entry.InitialDeal, actions)
                        ||> Seq.scan (fun deal (actionType, card) ->
                            OpenDeal.addAction actionType card deal)
                }
            printfn ""
            for deal in deals do
                if not (ClosedDeal.isComplete deal.ClosedDeal) then
                    let encoding =
                        OpenDeal.currentInfoSet deal
                            |> Encoding.encode
                    printfn "%s" (
                        encoding
                            |> Seq.cast<bool>
                            |> Seq.map (function true -> '1' | false -> '0')
                            |> Seq.toArray
                            |> String)

    run ()
