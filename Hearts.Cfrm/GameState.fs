﻿namespace Hearts.Cfrm

#if !FABLE_COMPILER
open Cfrm
#endif

open System
open PlayingCards
open Hearts

module Seq =

    /// Chunks the given sequence using keys generated by the
    /// given projection.
    let chunkBy projection source =
        (source, [])
            ||> Seq.foldBack (fun item chunks ->
                let key = projection item
                match chunks with
                    | (chunkKey, items) :: tail
                        when key = chunkKey ->
                        (key, item :: items) :: tail   // add to current chunk
                    | _ ->
                        (key, [item]) :: chunks)       // start new chunk
            |> Seq.map (fun (key, items) ->
                key, List.toSeq items)

/// A range of cards in a given suit. All unplayed cards in this
/// range are either present in the current user's hand or are
/// held in another user's hand (i.e. "outstanding"). Example:
/// * Range 2-4H, present
///   * 2H: Present in user's hand
///   * 3H: Previously played
///   * 4H: Present in user's hand
type CardRange =
    {
        /// Suit of all cards in this range.
        Suit : Suit

        /// Rank of lowest card in this range.
        MinRank : Rank

        /// Rank of highest card in this range.
        MaxRank : Rank

        /// Card is present in current user's hand?
        Present : bool
    }

module CardRange =

    /// Creates a card range.
    let create suit minRank maxRank present =
        assert(maxRank >= minRank)
        {
            Suit = suit
            MinRank = minRank
            MaxRank = maxRank
            Present = present
        }

    /// All cards in a deck.
    let private allCardSet = set Card.allCards

    /// Answers the ranges determined by legal plays in the given
    /// hand.
    let getLegalRanges (hand : Hand) deal =

            // group outstanding cards by suit
        let outstandingCardMap =
            allCardSet - hand - deal.PlayedCards
                |> Seq.groupBy (fun card -> card.Suit)
                |> Map

            // group legal plays by suit
        ClosedDeal.legalPlays hand deal
            |> Seq.groupBy (fun card -> card.Suit)
            |> Seq.map (fun (suit, legalPlays) ->

                    // ranks of present cards in this suit
                let inRankPairs =
                    legalPlays
                        |> Seq.map (fun card ->
                            card.Rank, true)

                    // ranks of outstanding cards in this suit
                let outRankPairs =
                    outstandingCardMap
                        |> Map.tryFind suit
                        |> Option.defaultValue Seq.empty
                        |> Seq.map (fun card ->
                            card.Rank, false)

                    // interleave to create ranges
                let ranges =
                    Seq.append inRankPairs outRankPairs
                        |> Seq.sort
                        |> Seq.chunkBy snd
                        |> Seq.map (fun (flag, rankPairs) ->
                            let ranks =
                                rankPairs
                                    |> Seq.map fst
                                    |> Seq.toArray
                            assert(ranks.Length > 0)
                            create
                                suit
                                (Array.head ranks)
                                (Array.last ranks)
                                flag)
                        |> Seq.toArray

                suit, ranges)

    let split rank ranges =
        (ranges, ([], []))
            ||> Seq.foldBack (fun range (low, high) ->
                if rank < range.MinRank then
                    low, range :: high
                elif rank > range.MaxRank then
                    range :: low, high
                else
                    assert(rank > range.MinRank)
                    assert(rank < range.MaxRank)
                    { range with
                        MaxRank = enum<Rank> (int rank - 1) }
                        :: low,
                    { range with
                        MinRank = enum<Rank> (int rank + 1) }
                        :: high)
            |> fun (low, high) ->
                Seq.toArray low, Seq.toArray high

module GameStateKey =

    let private getShootStatus deal =
        let nPlayers =
            deal.Score.ScoreMap
                |> Map.toSeq
                |> Seq.where (fun (_, points) ->
                    points > 0)
                |> Seq.length
        match nPlayers with
            | 0 -> 0
            | 1 ->
                let curPlayer = ClosedDeal.currentPlayer deal
                if deal.Score[curPlayer] > 0 then 1 else 2
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

    let private getCurrentTrick deal =
        let trick = ClosedDeal.currentTrick deal
        trick.SuitLedOpt
            |> Option.map (fun suit ->
                seq {
                    yield Suit.toLetter suit
                    yield Char.fromHexDigit
                        (Trick.pointValue trick)
                })
            |> Option.defaultValue "00"

    let private getRangeChar range =
        let length = int range.MaxRank - int range.MinRank + 1
        assert(length > 0)
        assert(length <= Rank.numRanks)
        let index = 2 * length - (if range.Present then 1 else 0)
        Char.fromHexDigit index

    let private getRangesChars ranges =
        seq {
            yield '|'
            for range in ranges do
                yield getRangeChar range
        }

    let private getLegalRanges hand deal =
        let rangeMap =
            CardRange.getLegalRanges hand deal
                |> Map
        let followPairOpt =
            option {
                let! trick = deal.CurrentTrickOpt
                let! (_, highCard) = trick.HighPlayOpt
                let! ranges = Map.tryFind highCard.Suit rangeMap
                assert(rangeMap.Count = 1)
                return highCard.Rank, ranges
            }
        match followPairOpt with
            | Some (highRank, ranges) ->
                [|
                    let lowRanges, highRanges =
                        CardRange.split highRank ranges
                    yield! getRangesChars lowRanges
                    yield! getRangesChars highRanges
                |]
            | None ->
                [|
                    for suit in Enum.getValues<Suit> do
                        yield! rangeMap
                            |> Map.tryFind suit
                            |> Option.defaultValue Array.empty
                            |> getRangesChars
                |]

    let getKey deal =
        let hand = OpenDeal.currentHand deal
        [|
            yield getShootStatus deal.ClosedDeal
            yield! getCardCounts deal.ClosedDeal
            yield! getVoids deal.ClosedDeal
            yield! getCurrentTrick deal.ClosedDeal
            yield! getLegalRanges hand deal.ClosedDeal
        |] |> String

module GameState =

    let private canTryFinalize deal =
        deal.ClosedDeal.CurrentTrickOpt
            |> Option.map (fun trick ->
                trick.Cards.IsEmpty)
            |> Option.defaultValue true

    let terminalValuesOpt deal =
        if canTryFinalize deal then
            deal
                |> OpenDeal.tryFinalScore
                |> Option.map (fun score ->
                    score.ScoreMap
                        |> Map.values
                        |> Seq.map (fun points ->
                            float -points)
                        |> Seq.toArray)
        else None

    let getLegalActions deal =
        let hand = OpenDeal.currentHand deal
        deal.ClosedDeal
            |> CardRange.getLegalRanges hand
            |> Seq.collect snd
            |> Seq.where (fun range -> range.Present)
            |> Seq.toArray

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
        GameState.getLegalActions deal

    override _.AddAction(range) =
        assert(range.Present)
        let card = Card.create range.MaxRank range.Suit
        OpenDeal.addPlay card deal
            |> GameState
            :> _
#endif
