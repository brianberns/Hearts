namespace Hearts.FastCfr

open System
open Hearts
open PlayingCards
open FastCfr

module Program =

    let getInfoSetKey hand deal =
        [|
            Hand.toString hand
            for trick in ClosedDeal.tricks deal do
                sprintf " %c" trick.Leader.Char
                for card in Seq.rev trick.Cards do
                    sprintf " %A" card
        |] |> String.concat ""

    let canTryFinalize deal =
        deal.ClosedDeal.CurrentTrickOpt
            |> Option.map (fun trick ->
                trick.Cards.IsEmpty)
            |> Option.defaultValue true

    let rec createGameState deal =
        let scoreOpt =
            if canTryFinalize deal then
                OpenDeal.tryFinalScore deal
            else None
        match scoreOpt with
            | Some score ->
                let payoff = float score[Seat.South]
                TerminalGameState.create 0 payoff
                    |> Terminal
            | None ->
                let seat = OpenDeal.currentPlayer deal
                let hand = deal.UnplayedCardMap[seat]
                let playerIdx =
                    if seat = Seat.South then 0
                    else 1
                NonTerminal {
                    ActivePlayerIdx = playerIdx
                    InfoSetKey = getInfoSetKey hand deal.ClosedDeal
                    LegalActions =
                        ClosedDeal.legalPlays hand deal.ClosedDeal
                            |> Seq.toArray
                    AddAction =
                        fun card ->
                            OpenDeal.addPlay card deal
                                |> createGameState
                }

    let train numGames chunkSize =
        let rng = Random(0)
        let gameChunks =
            Seq.init numGames (fun iGame ->
                let deck = Deck.shuffle rng
                let dealer = enum<Seat> (iGame % Seat.numSeats)
                OpenDeal.fromDeck
                    dealer
                    ExchangeDirection.Hold
                    deck
                    |> OpenDeal.startPlay
                    |> createGameState)
                |> Seq.chunkBySize chunkSize
        Trainer.train (rng.Next()) gameChunks

    let run () =

            // train
        let numGames = 100
        let chunkSize = 2
        let util, infoSetMap = train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"

        printfn ""
        for (KeyValue(key, infoSet)) in infoSetMap do
            let strategy =
                InformationSet.getAverageStrategy infoSet
            printfn $"{key}: %A{strategy.ToArray()}"

    Console.OutputEncoding <- Text.Encoding.UTF8
    run ()
