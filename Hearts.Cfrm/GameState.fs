namespace Hearts.Cfrm

open Cfrm
open PlayingCards
open Hearts

type HeartsGameState(openDeal : OpenDeal) =
    inherit GameState<Card>()

    let canTryFinalize =
        openDeal.ClosedDeal.CurrentTrickOpt
            |> Option.map (fun trick -> trick.Cards.IsEmpty)
            |> Option.defaultValue true

    override _.CurrentPlayerIdx =
        openDeal
            |> OpenDeal.currentPlayer
            |> int

    override _.Key =
        seq {
            for trick in ClosedDeal.tricks openDeal.ClosedDeal do
                for (_, card) in Trick.plays trick do
                    yield card.String
                yield "|"
            yield "|"
            for card in OpenDeal.currentHand openDeal do
                yield card.String
        } |> String.concat ""

    override _.TerminalValuesOpt =
        if canTryFinalize then
            openDeal
                |> OpenDeal.tryFinalize
                |> Option.map (fun (ScoreMap scoreMap) ->
                    scoreMap
                        |> Map.values
                        |> Seq.map (fun points -> float -points)
                        |> Seq.toArray)
        else None

    override _.LegalActions =
        let hand = OpenDeal.currentHand openDeal
        ClosedDeal.legalPlays hand openDeal.ClosedDeal
            |> Seq.toArray

    override _.AddAction(card) =
        OpenDeal.addPlay card openDeal
            |> HeartsGameState
            :> _
