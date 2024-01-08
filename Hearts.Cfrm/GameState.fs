namespace Hearts.Cfrm

#if !FABLE_COMPILER
open Cfrm
#endif

open PlayingCards
open Hearts

module HeartsGameState =

    let canTryFinalize deal =
        deal.ClosedDeal.CurrentTrickOpt
            |> Option.map (fun trick -> trick.Cards.IsEmpty)
            |> Option.defaultValue true

    let terminalValuesOpt deal =
        if canTryFinalize deal then
            deal
                |> OpenDeal.tryFinalize
                |> Option.map (fun (ScoreMap scoreMap) ->
                    scoreMap
                        |> Map.values
                        |> Seq.map (fun points -> float -points)
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
type HeartsGameState(deal : OpenDeal) =
    inherit GameState<Card>()

    override _.CurrentPlayerIdx =
        deal
            |> OpenDeal.currentPlayer
            |> int

    override _.Key = HeartsGameState.getKey deal

    override _.TerminalValuesOpt =
        HeartsGameState.terminalValuesOpt deal

    override _.LegalActions =
        let hand = OpenDeal.currentHand deal
        ClosedDeal.legalPlays hand deal.ClosedDeal
            |> Seq.toArray

    override _.AddAction(card) =
        OpenDeal.addPlay card deal
            |> HeartsGameState
            :> _
#endif
