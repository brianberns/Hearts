namespace Hearts.Cfrm

#if !FABLE_COMPILER
open Cfrm
#endif

open System
open PlayingCards
open Hearts

module GameStateKey =

    let private getShootStatus deal =
        let (ScoreMap scoreMap) = deal.Score
        let nPlayers =
            scoreMap
                |> Map.toSeq
                |> Seq.where (fun (_, points) ->
                    points > 0)
                |> Seq.length
        match nPlayers with
            | 0 -> 'A'
            | 1 ->
                let curPlayer = ClosedDeal.currentPlayer deal
                if scoreMap[curPlayer] > 0 then 'B' else 'C'
            | _ -> 'D'

    let private getCardCounts deal =
        let countMap =
            deal.PlayedCards
                |> Seq.groupBy (fun card -> card.Suit)
                |> Seq.map (fun (suit, cards) ->
                    suit, Seq.length cards)
                |> Map
        seq {
            for suit in Enum.getValues<Suit> do
                countMap
                    |> Map.tryFind suit
                    |> Option.defaultValue 0
                    |> Char.fromHexDigit
        }

    let private suitFlagMap =
        Enum.getValues<Suit>
            |> Seq.map (fun suit ->
                suit, 1 <<< int suit)
            |> Map

    let private getVoids deal =
        let voidMap =
            deal.Voids
                |> Seq.groupBy fst
                |> Seq.map (fun (seat, group) ->
                    seat, Seq.map snd group)
                |> Map
        let curPlayer = ClosedDeal.currentPlayer deal
        let seats =
            curPlayer
                |> Seat.cycle
                |> Seq.skip 1
        seq {
            for seat in seats do
                voidMap
                    |> Map.tryFind seat
                    |> Option.defaultValue Seq.empty
                    |> Seq.sumBy (fun suit -> suitFlagMap[suit])
                    |> Char.fromHexDigit
        }

    let getKey deal =
        let key =
            [|
                yield getShootStatus deal.ClosedDeal
                yield! getCardCounts deal.ClosedDeal
                yield! getVoids deal.ClosedDeal
            |] |> String
        printfn "%s" key
        key

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
        GameStateKey.getKey deal

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
