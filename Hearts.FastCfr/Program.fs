namespace Hearts.FastCfr

open System
open System.IO

open Hearts
open PlayingCards
open FastCfr
open MathNet.Numerics.LinearAlgebra

module Program =

    let getInfoSetKey (hand : Hand) deal =
        let key = ResizeArray<uint16>(12)

            // hand
        assert(Card.allCards.Length <= 16)
        Seq.sum [|
            for i, card in Seq.indexed Card.allCards do
                if hand.Contains(card) then
                    let value = 1us <<< i
                    assert(value >>> i = 1us)
                    value
        |] |> key.Add

            // other unplayed cards
        Seq.sum [|
            for i, card in Seq.indexed Card.allCards do
                if not (hand.Contains(card))
                    && deal.UnplayedCards.Contains(card) then
                    let value = 1us <<< i
                    assert(value >>> i = 1us)
                    value
        |] |> key.Add

            // current trick
        let trick = ClosedDeal.currentTrick deal
        key.Add(uint16 trick.Leader)
        let cards =
            trick.Cards
                |> List.rev
                |> List.toArray
        for iCard = 0 to Seat.numSeats - 1 do
            key.Add(
                if iCard < cards.Length then
                    let value =
                        (uint16 cards[iCard].Rank <<< 8) +
                            uint16 cards[iCard].Suit
                    assert((value / (1us <<< 8)) = uint16 cards[iCard].Rank)
                    assert((value % (1us <<< 8)) = uint16 cards[iCard].Suit)
                    value
                else 0us)

            // voids
        let player = Trick.currentPlayer trick
        let pairs =
            [|
                for seat in Enum.getValues<Seat> do
                    if seat <> player then
                        for suit in Enum.getValues<Suit> do
                            yield seat, suit
            |]
        Seq.sum [|
            for i, pair in Seq.indexed pairs do
                if deal.Voids.Contains(pair) then
                    1us <<< i
        |] |> key.Add

            // score
        for score in deal.Score.ScoreMap.Values do
            key.Add(uint16 score)

        key.ToArray()

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

    let save (strategyMap : Map<uint16[], Vector<float>>) =
        let path = "Hearts.strategy"
        use stream = new FileStream(path, FileMode.Create)
        use wtr = new BinaryWriter(stream)
        wtr.Write(strategyMap.Count)
        for (KeyValue(key, strategy)) in strategyMap do
            for value in key do
                wtr.Write(value)
            wtr.Write(uint8 strategy.Count)
            for prob in strategy do
                wtr.Write(prob)

    let run () =

            // train
        let numGames = 10_000_000
        let chunkSize = 100_000
        let util, infoSetMap = train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"
        printfn $"# of info sets: {infoSetMap.Count}"

        printfn ""
        let strategyMap =
            infoSetMap
                |> Map.map (fun _ infoSet ->
                    InformationSet.getAverageStrategy infoSet)
        save strategyMap

    Console.OutputEncoding <- Text.Encoding.UTF8
    run ()
