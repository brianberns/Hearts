namespace Hearts

open PlayingCards

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

    /// Plays the given card on the given trick.
    let addPlay card trick =
        assert(trick.Cards.Length < Seat.numSeats)
        {
            trick with
                Cards = card :: trick.Cards
                SuitLedOpt =
                    if trick.SuitLedOpt |> Option.isSome then
                        trick.SuitLedOpt
                    else
                        Some card.Suit
                HighPlayOpt =
                    let isHigh =
                        match trick.HighPlayOpt with
                            | Some (_, prevCard) ->
                                if card.Suit = prevCard.Suit then
                                    card.Rank > prevCard.Rank
                                else false
                            | None -> true
                    if isHigh then
                        let player = trick |> currentPlayer
                        Some (player, card)
                    else trick.HighPlayOpt
        }

    /// Indicates whether the given trick has finished.
    let isComplete trick =
        assert(trick.Cards.Length >= 0)
        assert(trick.Cards.Length <= Seat.numSeats)
        trick.Cards.Length = Seat.numSeats

    /// Each card in the given trick and its player, in chronological order.
    let plays trick =
        let seats = trick.Leader |> Seat.cycle
        let cards = trick.Cards |> Seq.rev
        Seq.zip seats cards
