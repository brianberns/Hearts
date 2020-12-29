namespace Hearts

open PlayingCards

/// A deal is a round of play within a game. A closed deal contains
/// no information about unplayed cards, which are kept private by
/// each player. Cards played during a deal are grouped into tricks.
type ClosedDeal =
    {
        /// Current active trick, if play is in progress.
        CurrentTrickOpt : Option<Trick>

        /// Completed tricks, in reverse chronological order.
        CompletedTricks : List<Trick>

        /// Is it legal to lead a heart?
        HeartsBroken : bool

        /// Suits that players are known to be void in.
        Voids : Set<Seat * Suit>

        /// Points taken so far by each player.
        Score : Score
    }

module ClosedDeal =

    /// Initial state of all closed deals.
    let initial =
        {
            CurrentTrickOpt = None
            CompletedTricks = List.empty
            HeartsBroken = false
            Voids = Set.empty
            Score = Score.zero
        }

    /// Starts the given deal with the given first trick leader.
    let start leader deal =
        {
            deal with
                CurrentTrickOpt =
                    Trick.create leader |> Some
        }

    /// Number of cards played so far.
    let numCardsPlayed deal =
        let nCompleted =
            deal.CompletedTricks.Length * Seat.numSeats
        let nCurrent =
            deal.CurrentTrickOpt
                |> Option.map (fun trick ->
                    trick.Cards.Length)
                |> Option.defaultValue 0
        nCompleted + nCurrent

    /// Number of cards dealt to each player.
    let numCardsPerHand = Card.numCards / Seat.numSeats

    /// Number of cards in a deal.
    let numCardsPerDeal = numCardsPerHand * Seat.numSeats

    /// Validates the structure of the given deal.
    let private validate deal =
        assert(deal.CompletedTricks.Length <= numCardsPerHand)
        assert(
            let nCards = numCardsPlayed deal
            (nCards = 0) || (nCards = numCardsPerDeal) = deal.CurrentTrickOpt.IsNone)
        assert(deal.CompletedTricks |> Seq.forall Trick.isComplete)

    /// Current trick in the given deal.
    let currentTrick deal =
        match deal.CurrentTrickOpt with
            | Some trick -> trick
            | None -> failwith "Deal is complete"

    /// Current player in the given deal.
    let currentPlayer deal =
        deal
            |> currentTrick
            |> Trick.currentPlayer

    /// Two of clubs.
    let card2C = Card.fromString "2♣"

    /// What cards can be played from the given hand?
    let legalPlays (hand : Hand) deal =
        validate deal
        let trick = currentTrick deal
        match deal.CompletedTricks.Length, trick.SuitLedOpt with

                // must lead 2♣ on first trick
            | 0, None ->
                assert(hand |> Seq.contains(card2C))
                Seq.singleton card2C

                // can't lead a heart until they're broken
            | _, None ->
                if deal.HeartsBroken then hand
                else
                    hand
                        |> Seq.where (fun card ->
                            card.Suit <> Suit.Hearts)

                // following
            | iTrick, Some suitLed ->

                    // must follow suit, if possible
                let cards =
                    let isSuitLed (card : Card) =
                        card.Suit = suitLed
                    if hand |> Seq.exists isSuitLed then
                        hand |> Seq.where isSuitLed
                    else hand

                    // no point cards on first trick
                if iTrick = 0 then
                    cards
                        |> Seq.where (fun card ->
                            Card.pointValue card = 0)
                else cards

    /// Is the given player known to be void in the given suit?
    let isVoid seat suit deal =
        deal.Voids.Contains(seat, suit)

    /// Indicates whether the given deal has finished.
    let isComplete deal =
        validate deal
        deal.CompletedTricks.Length = numCardsPerHand

    /// Plays the given card on the given deal.
    let addPlay (card : Card) deal =
        assert(isComplete deal |> not)

            // play card on current trick
        let updatedTrick, player =
            let curTrick = deal |> currentTrick
            let player = curTrick |> Trick.currentPlayer
            assert(deal |> isVoid player card.Suit |> not)
            let updatedTrick = curTrick |> Trick.addPlay card
            updatedTrick, player

            // player is void in suit led?
        let voids =
            match updatedTrick.SuitLedOpt with
                | Some suitLed ->
                    if card.Suit = suitLed then
                        assert(deal |> isVoid player card.Suit |> not)
                        deal.Voids
                    else
                        deal.Voids.Add(player, suitLed)
                | None -> failwith "Unexpected"

            // complete trick?
        let curTrickOpt, completedTricks, score =
            if updatedTrick |> Trick.isComplete then
                let taker =
                    match updatedTrick.HighPlayOpt with
                        | Some (seat, _) -> seat
                        | None -> failwith "Unexpected"
                let tricks = updatedTrick :: deal.CompletedTricks
                let curTrickOpt =
                    if tricks.Length < numCardsPerHand then
                        taker |> Trick.create |> Some
                    else None
                let score =
                    let trickScore =
                        updatedTrick
                            |> Trick.pointValue
                            |> Score.create taker
                    deal.Score + trickScore
                curTrickOpt, tricks, score
            else
                Some updatedTrick, deal.CompletedTricks, deal.Score

        {
            deal with
                CurrentTrickOpt = curTrickOpt
                CompletedTricks = completedTricks
                Voids = voids
                Score = score
        }

    /// Tricks in the given deal, in chronological order, including the
    /// current trick (if any).
    let tricks deal =
        seq {
            yield! deal.CompletedTricks
                |> List.rev
            match deal.CurrentTrickOpt with
                | Some trick -> yield trick
                | None -> ()
        }
