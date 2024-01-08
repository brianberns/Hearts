namespace Hearts.Cfrm

#if !FABLE_COMPILER
open Cfrm
#endif

open PlayingCards
open Hearts

module GameStateKey =

    let private getShootStatus (deal : ClosedDeal) =
        let (ScoreMap scoreMap) = deal.Score
        let nPlayers =
            scoreMap
                |> Map.toSeq
                |> Seq.where (fun (_, points) ->
                    points > 0)
                |> Seq.length
        match nPlayers with
            | 0 -> true, true
            | 1 ->
                let curPlayer = ClosedDeal.currentPlayer deal
                let curPlayerFlag = scoreMap[curPlayer] > 0
                curPlayerFlag, not curPlayerFlag
            | _ -> false, false

module GameState =

    let private canTryFinalize deal =
        deal.ClosedDeal.CurrentTrickOpt
            |> Option.map (fun trick ->
                trick.Cards.IsEmpty)
            |> Option.defaultValue true

    let terminalValuesOpt deal =
        if canTryFinalize deal then
            deal
                |> OpenDeal.tryFinalize
                |> Option.map (fun (ScoreMap scoreMap) ->
                    scoreMap
                        |> Map.values
                        |> Seq.map (fun points ->
                            float -points)
                        |> Seq.toArray)
        else None

    let private getChar (card : Card) =
        let offset =
            Suit.numSuits * (int card.Rank - int Rank.Two)
                + (int card.Suit)
        char (int 'a' + offset)

    let getKey deal =
        let compCards =
            deal.ClosedDeal.CompletedTricks
                |> Seq.collect (fun trick -> trick.Cards)
                |> Seq.sort
        let curCards =
            deal.ClosedDeal.CurrentTrickOpt
                |> Option.map (fun trick -> trick.Cards)
                |> Option.defaultValue List.empty
        [|
            for card in compCards do
                yield getChar card
            for card in curCards do
                yield getChar card

            yield '|'
            let pairs =
                let curPlayer =
                    ClosedDeal.currentPlayer deal.ClosedDeal
                deal.ClosedDeal.Voids
                    |> Seq.where (fun (seat, _) ->
                        seat <> curPlayer)
                    |> Seq.map (fun (seat, suit) ->
                        let offset =
                            (int seat - int curPlayer + Seat.numSeats) % Seat.numSeats
                        offset, suit)
                    |> Seq.sort
            for (offset, suit) in pairs do
                yield char (int '0' + offset)
                yield suit.Char

            yield '|'
            for card in OpenDeal.currentHand deal do
                yield getChar card

        |] |> System.String

#if !FABLE_COMPILER
type GameState(deal : OpenDeal) =
    inherit GameState<Card>()

    override _.CurrentPlayerIdx =
        deal
            |> OpenDeal.currentPlayer
            |> int

    override _.Key = GameState.getKey deal

    override _.TerminalValuesOpt =
        GameState.terminalValuesOpt deal

    override _.LegalActions =
        let hand = OpenDeal.currentHand deal
        ClosedDeal.legalPlays hand deal.ClosedDeal
            |> Seq.toArray

    override _.AddAction(card) =
        OpenDeal.addPlay card deal
            |> GameState
            :> _
#endif
