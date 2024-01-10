namespace Hearts

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
        /// Player who starts this trick.
        Leader : Seat

        /// Cards played by seat in this trick, in reverse chronological order.
        Cards : List<Card>

        /// Suit of first card played in this trick, if any.
        SuitLedOpt : Option<Suit>

        /// Play that takes this trick, so far, if any.
        HighPlayOpt : Option<Seat * Card>
    }

module Trick =

    /// Creates a trick with the given leader.
    let create leader =
        {
            Leader = leader
            Cards = List.empty
            SuitLedOpt = None
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
                SuitLedOpt =
                    trick.SuitLedOpt
                        |> Option.orElse (Some card.Suit)
                HighPlayOpt =
                    let isHigh =
                        match trick.HighPlayOpt with
                            | Some (_, prevCard) ->
                                if card.Suit = prevCard.Suit then
                                    card.Rank > prevCard.Rank
                                else false
                            | None -> true
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
        assert(trick.Cards.Length >= 0)
        assert(trick.Cards.Length <= Seat.numSeats)
        trick.Cards.Length = Seat.numSeats

    /// Each card in the given trick and its player, in chronological order.
    let plays trick =
        let seats = Seat.cycle trick.Leader
        let cards = Seq.rev trick.Cards
        Seq.zip seats cards
