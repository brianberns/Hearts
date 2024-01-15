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

/// A range of unplayed cards in a given suit.
type CardRange =
    {
        /// Suit of all cards in this range.
        Suit : Suit

        /// Number of unplayed cards in this range.
        Count : int

        /// Cards are present in current user's hand?
        Present : bool
    }

module CardRange =

    /// Creates a card range.
    let create suit count present =
        {
            Suit = suit
            Count = count
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

            // split on high card of current trick?
        let splitCardOpt =
            let trick = ClosedDeal.currentTrick deal
            Option.map snd trick.HighPlayOpt

            // group legal plays by suit
        ClosedDeal.legalPlays hand deal
            |> Seq.groupBy (fun card -> card.Suit)
            |> Seq.map (fun (suit, legalPlays) ->

                    // gather relevant cards
                let rankPairs =
                    seq {
                            // ranks of present cards in this suit
                        for card in legalPlays do
                            yield card.Rank, Some true

                            // ranks of outstanding cards in this suit
                        let outCards =
                            outstandingCardMap
                                |> Map.tryFind suit
                                |> Option.defaultValue Seq.empty
                        for card in outCards do
                            yield card.Rank, Some false

                            // rank of split card, if in this suit
                        match splitCardOpt with
                            | Some (card : Card) when card.Suit = suit ->
                                yield card.Rank, None
                            | _ -> ()
                    }
                assert(
                    Seq.length (Seq.distinctBy fst rankPairs)
                        = Seq.length rankPairs)

                    // interleave to create ranges
                let ranges =
                    rankPairs
                        |> Seq.sort
                        |> Seq.chunkBy snd
                        |> Seq.choose (fun (presentOpt, pairs) ->
                            presentOpt
                                |> Option.map (fun present ->
                                    let count = Seq.length pairs
                                    create suit count present))
                        |> Seq.toArray

                suit, ranges)

module GameStateKey =

    /// '0': Any player can shoot
    /// '1': Only the current player can shoot
    /// '2': Only another player can shoot
    /// '3': No player can shoot
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

    /// Bit flag for each suit: 1, 2, 4, 8.
    let private suitFlagMap =
        Enum.getValues<Suit>
            |> Seq.map (fun suit ->
                suit, 1 <<< int suit)
            |> Map

    /// One hex character for each other player, indicating which
    /// suits that player is void in.
    let private getVoids deal =
        let voidMap =
            deal.Voids
                |> Seq.groupBy fst
                |> Seq.map (fun (seat, group) ->
                    seat, Seq.map snd group)
                |> Map
        let seats =
            deal
                |> ClosedDeal.currentPlayer
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
        trick.HighPlayOpt
            |> Option.map (fun (seat, card) ->

                let iPlayer =
                    ClosedDeal.currentPlayer deal
                        |> Seat.getIndex seat
                assert(iPlayer > 0)

                let hasPoints =
                    Trick.pointValue trick > 0

                seq {
                    yield Char.fromDigit iPlayer
                    yield Suit.toLetter card.Suit
                    yield if hasPoints then '1' else '0'
                })
            |> Option.defaultValue "000"

    let private getRangeChar range =
        let index =
            (2 * range.Count)
                - (if range.Present then 1 else 0)
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
        seq {
            for suit in Enum.getValues<Suit> do
                yield! rangeMap
                    |> Map.tryFind suit
                    |> Option.defaultValue Array.empty
                    |> getRangesChars
        }

    let getKey (deal : OpenDeal) =
        let hand = OpenDeal.currentHand deal
        [|
            yield getShootStatus deal.ClosedDeal
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
