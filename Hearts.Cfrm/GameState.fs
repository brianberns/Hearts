namespace Hearts.Cfrm

#if !FABLE_COMPILER
open Cfrm
#endif

open System
open PlayingCards
open Hearts

type CardRange =
    {
        Suit : Suit
        MinRank : Rank
        MaxRank : Rank
    }

module CardRange =

    let private allCardSet = set Card.allCards

    let getRanges (hand : Hand) deal =
        let outstandingCardMap =
            allCardSet - (hand + deal.PlayedCards)
                |> Seq.groupBy (fun card -> card.Suit)
                |> Map
        ClosedDeal.legalPlays hand deal
            |> Seq.groupBy (fun card -> card.Suit)
            |> Seq.collect (fun (suit, legalPlays) ->
                let rankPairs =
                    let inRankPairs =
                        legalPlays
                            |> Seq.map (fun card ->
                                card.Rank, true)
                    let outRankPairs =
                        outstandingCardMap
                            |> Map.tryFind suit
                            |> Option.defaultValue Seq.empty
                            |> Seq.map (fun card ->
                                card.Rank, false)
                    seq {
                        yield! inRankPairs
                        yield! outRankPairs
                        yield (enum<Rank> Rank.numRanks, false)   // sentinel value at end
                    } |> Seq.sort
                ((None, []), rankPairs)
                    ||> Seq.fold (fun (curRangeOpt, ranges) (rank, flag) ->
                        match curRangeOpt, flag with
                            | None, true ->
                                let curRangeOpt' =
                                    Some {
                                        Suit = suit
                                        MinRank = rank
                                        MaxRank = rank
                                    }
                                curRangeOpt', ranges
                            | None, false -> None, ranges
                            | Some range, true ->
                                let curRangeOpt' =
                                    assert(rank > range.MaxRank)
                                    Some { range with MaxRank = rank }
                                curRangeOpt', ranges
                            | Some range, false ->
                                None, range :: ranges)
                    |> snd)

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
            | 0 -> 0
            | 1 ->
                let curPlayer = ClosedDeal.currentPlayer deal
                if scoreMap[curPlayer] > 0 then 1 else 2
            | _ -> 3
            |> Char.fromDigit

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

    let private getCurrentTrick (deal : ClosedDeal) =
        let trick = ClosedDeal.currentTrick deal
        trick.SuitLedOpt
            |> Option.map (fun suit ->
                seq {
                    yield Suit.toLetter suit
                    yield Char.fromHexDigit
                        (Trick.pointValue trick)
                })
            |> Option.defaultValue "00"

    let getKey deal =
        let key =
            [|
                yield getShootStatus deal.ClosedDeal
                yield! getCardCounts deal.ClosedDeal
                yield! getVoids deal.ClosedDeal
                yield! getCurrentTrick deal.ClosedDeal
            |] |> String
        printfn "%s" key
        let hand = OpenDeal.currentHand deal
        CardRange.getRanges hand deal.ClosedDeal
            |> printfn "%A"
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

    let getKey deal =
        GameStateKey.getKey deal

#if !FABLE_COMPILER
type GameState(deal : OpenDeal) =
    inherit GameState<CardRange>()

    override _.CurrentPlayerIdx =
        deal
            |> OpenDeal.currentPlayer
            |> int

    override _.Key = GameState.getKey deal

    override _.TerminalValuesOpt =
        GameState.terminalValuesOpt deal

    override _.LegalActions =
        let hand = OpenDeal.currentHand deal
        deal.ClosedDeal
            |> CardRange.getRanges hand
            |> Seq.toArray

    override _.AddAction(range) =
        let card = Card.create range.MaxRank range.Suit
        OpenDeal.addPlay card deal
            |> GameState
            :> _
#endif
