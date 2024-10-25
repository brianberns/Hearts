﻿namespace Hearts.FastCfr

open System
open Hearts
open PlayingCards
open FastCfr

module Program =

    let getInfoSetKey (hand : Hand) deal =
        let sb = Text.StringBuilder()

            // hand and other unplayed cards
        for card in Card.allCards do
            sb.Append(
                if hand.Contains(card) then
                    assert(deal.UnplayedCards.Contains(card))
                    'o'
                elif deal.UnplayedCards.Contains(card) then 'x'
                else '.')
                |> ignore

            // current trick
        let trick = ClosedDeal.currentTrick deal
        sb.Append(trick.Leader.Char) |> ignore
        let cards =
            trick.Cards
                |> List.rev
                |> List.toArray
        for iCard = 0 to Seat.numSeats - 1 do
            sb.Append(
                if iCard < cards.Length then
                    cards[iCard].String
                else "..")
                |> ignore

            // voids
        let player = Trick.currentPlayer trick
        for seat in Enum.getValues<Seat> do
            if seat <> player then
                for suit in Enum.getValues<Suit> do
                    sb.Append(
                        if deal.Voids.Contains(seat, suit) then 'x'
                        else '.')
                        |> ignore

            // score
        for score in deal.Score.ScoreMap.Values do
            assert(score >= 0 && score < 10)
            sb.Append(score) |> ignore

        sb.ToString()

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
                let otherAvg =
                    (score.ScoreMap
                        |> Map.toSeq
                        |> Seq.where (fun (seat, _) -> seat <> Seat.South)
                        |> Seq.sumBy snd
                        |> float)
                        / float (Seat.numSeats - 1)
                let payoff = otherAvg - float score[Seat.South]
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
                |> Seq.mapi (fun iChunk chunk ->
                    printfn $"Chunk {iChunk}"
                    chunk)
        Trainer.train (rng.Next()) gameChunks

    let run () =

            // train
        let numGames = 10000
        let chunkSize = 1000
        let util, infoSetMap = train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"
        printfn $"# of info sets: {infoSetMap.Count}"

        printfn ""
        let pairs =
            infoSetMap
                |> Map.toSeq
                |> Seq.map (fun (key, infoSet) ->
                    key,
                    InformationSet.getAverageStrategy infoSet)
                |> Seq.where (fun (_, strategy) ->
                    strategy
                        |> Seq.distinct
                        |> Seq.length > 1)
                |> Seq.truncate 30
        for (key, strategy) in pairs do
            printfn $"{key}: %A{strategy.ToArray()}"

    Console.OutputEncoding <- Text.Encoding.UTF8
    run ()
