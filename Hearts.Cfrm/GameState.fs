namespace Hearts.Cfrm

#if !FABLE_COMPILER
open Cfrm
open PlayingCards
#endif

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

    let getChar (card : Card) =
        let offset =
            Suit.numSuits * (int card.Rank - int Rank.Two)
                + (int card.Suit)
        char (int 'a' + offset)

    let getKey deal =
        [|
            for trick in ClosedDeal.tricks deal.ClosedDeal do
                for (_, card) in Trick.plays trick do
                    yield getChar card
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
