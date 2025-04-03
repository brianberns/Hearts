﻿namespace Hearts

open PlayingCards

module Card =

    /// Point value of the given card.
    let pointValue (card : Card) =
        match card.Rank, card.Suit with
            | _, Suit.Hearts -> 1
            | Rank.Queen, Suit.Spades -> Rank.numRanks
            | _ -> 0

/// One card played by each player in turn during a deal.
type Trick =
    {
        /// Player who starts or started this trick.
        Leader : Seat

        /// Cards played by seat in this trick, in reverse chronological order.
        Cards : List<Card>

        /// Play that takes this trick, so far, if any.
        HighPlayOpt : Option<Seat * Card>
    }
        /// Suit of first card played in this trick, if any.
    member trick.SuitLedOpt =
        trick.HighPlayOpt
            |> Option.map (snd >> Card.suit)

module Trick =

    /// Creates a trick with the given leader to play first.
    let create leader =
        {
            Leader = leader
            Cards = List.empty
            HighPlayOpt = None
        }

    /// Current player on the given trick.
    let currentPlayer trick =
        trick.Leader
            |> Seat.incr trick.Cards.Length

    /// The high player on this trick, if any.
    let highPlayerOpt trick =
        trick.HighPlayOpt
            |> Option.map fst

    /// Plays the given card on the given trick.
    let addPlay card trick =
        assert(trick.Cards.Length < Seat.numSeats)
        {
            trick with
                Cards = card :: trick.Cards
                HighPlayOpt =
                    let isHigh =
                        trick.HighPlayOpt
                            |> Option.map (fun (_, prevCard) ->
                                if card.Suit = prevCard.Suit then
                                    card.Rank > prevCard.Rank
                                else false)
                            |> Option.defaultValue true
                    if isHigh then
                        Some (currentPlayer trick, card)
                    else trick.HighPlayOpt
        }

    /// Point value of the given trick.
    let pointValue trick =
        trick.Cards
            |> Seq.sumBy Card.pointValue

    /// Indicates whether the given trick has finished.
    let isComplete trick =
        assert(trick.Cards.Length <= Seat.numSeats)
        trick.Cards.Length = Seat.numSeats

    /// Each card in the given trick and its player, in chronological order.
    let plays trick =
        let seats = Seat.cycle trick.Leader
        let cards = Seq.rev trick.Cards
        Seq.zip seats cards
