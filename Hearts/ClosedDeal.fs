namespace Hearts

open PlayingCards

/// A deal is a round of play within a game. A closed deal contains
/// no information about unplayed cards, which are kept private by
/// each player. Cards played during a deal are grouped into tricks.
type ClosedDeal =
    {
        /// Dealer of this deal's cards.
        Dealer : Seat

        /// Current active trick, if the deal is not yet complete.
        CurrentTrickOpt : Option<Trick>

        /// Completed tricks, in reverse chronological order.
        CompletedTricks : List<Trick>

        /// Is it legal to lead a heart?
        HeartsBroken : bool

        /// Suits that players are known to be void in.
        Voids : Set<Seat * Suit>
    }

module ClosedDeal =

    /// Creates a closed deal.
    let create dealer =
        {
            Dealer = dealer
            CurrentTrickOpt =
                dealer
                    |> Seat.next
                    |> Trick.create
                    |> Some
            CompletedTricks = List.empty
            HeartsBroken = false
            Voids = Set.empty
        }

    /// Current trick in the given deal.
    let currentTrick deal =
        match deal.CurrentTrickOpt with
            | Some trick -> trick
            | None -> failwith "Deal is complete"

    /// Two of clubs.
    let private card2C = Card(Rank.Two, Suit.Clubs)

    let pointValue (card : Card) =
        match card.Rank, card.Suit with
            | _, Suit.Hearts -> 1
            | Rank.Queen, Suit.Spades -> 13
            | _ -> 0

    /// What cards can be played from the given hand?
    let legalPlays (hand : Hand) deal =
        let trick = currentTrick deal
        match deal.CompletedTricks.Length, trick.SuitLedOpt with

                // must lead 2C on first trick
            | 0, None ->
                assert(hand |> Seq.contains(card2C))
                Seq.singleton card2C

                // can't lead a heart until they're broken
            | _, None ->
                if deal.HeartsBroken then hand
                else hand |> Seq.where (fun card -> card.Suit <> Suit.Hearts)

            | iTrick, Some suitLed ->

                let isSuitLed (card : Card) =
                    card.Suit = suitLed

                    // first trick; must follow suit, if possible, and no point cards
                if iTrick = 0 then
                    hand |> Seq.where (fun card ->
                        isSuitLed card && pointValue card = 0)

                    // must follow suit, if possible
                elif hand |> Seq.exists isSuitLed then
                    hand |> Seq.where isSuitLed

                    // may play any card
                else hand

    /// Number of cards dealt to each player.
    let numCardsPerHand = 6

    /// Number of cards in a deal.
    let numCardsPerDeal = numCardsPerHand * Seat.numSeats

    /// Is the given player known to be void in the given suit?
    let isVoid seat suit playout =
        playout.Voids.Contains(seat, suit)

    /// Validates the structure of the given playout.
    let private validate playout =
        assert(playout.NumCompletedTricks = playout.CompletedTricks.Length)
        assert(playout.NumCompletedTricks <= numCardsPerHand)
        assert((playout.NumCompletedTricks = numCardsPerHand) = playout.CurrentTrickOpt.IsNone)
        assert(playout.CompletedTricks |> Seq.forall (fun trick -> trick.IsComplete))

    /// Indicates whether the given playout has finished.
    let isComplete playout =
        validate playout
        playout.NumCompletedTricks = numCardsPerHand

    /// Plays the next player's card on the given playout.
    let addPlay (card : Card) playout =
        assert(isComplete playout |> not)

            // determine trump suit
        let trump =
            playout.TrumpOpt
                |> Option.defaultValue card.Suit

            // play card on current trick
        let updatedTrick, player =
            let curTrick = playout |> currentTrick
            let player = curTrick.NextPlayer
            assert(playout |> isVoid player card.Suit |> not)
            let updatedTrick =
                curTrick |> Trick.addPlay trump card
            updatedTrick, player

            // player is void in suit led?
        let voids =
            if card.Suit = trump then
                playout.Voids
            elif card.Suit = updatedTrick.SuitLed then
                assert(playout |> isVoid player card.Suit |> not)
                playout.Voids
            else
                playout.Voids.Add(player, updatedTrick.SuitLed)

            // complete trick?
        validate playout
        let curTrickOpt, completedTricks, numCompletedTricks =
            if updatedTrick.IsComplete then
                let winner = updatedTrick.Winner
                let tricks = updatedTrick :: playout.CompletedTricks
                let numCompletedTricks = playout.NumCompletedTricks + 1
                let curTrickOpt =
                    if numCompletedTricks < numCardsPerHand then
                        Trick.create winner |> Some
                    else None
                curTrickOpt, tricks, numCompletedTricks
            else
                Some updatedTrick, playout.CompletedTricks, playout.NumCompletedTricks

        {
            CurrentTrickOpt = curTrickOpt
            CompletedTricks = completedTricks
            NumCompletedTricks = numCompletedTricks
            TrumpOpt = Some trump
            Voids = voids
        }

    /// Tricks in the given playout, in chronological order, including the
    /// current trick (if any).
    let tricks playout =
        seq {
            yield! playout.CompletedTricks
                |> List.rev
            match playout.CurrentTrickOpt with
                | Some trick -> yield trick
                | None -> ()
        }

    /// Number of cards played so far.
    let numCardsPlayed playout =
        validate playout
        let nCompleted =
            playout.NumCompletedTricks * Seat.numSeats
        let nCurrent =
            match playout.CurrentTrickOpt with
                | Some trick -> trick.NumCards
                | None -> 0
        nCompleted + nCurrent

    /// Plays in the given playout, in chronological order.
    let plays playout =
        seq {
            for trick in tricks playout do
                yield! trick.Plays
        }
