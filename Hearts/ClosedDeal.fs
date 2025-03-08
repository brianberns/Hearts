namespace Hearts

open PlayingCards

/// Direction in which cards are passed prior to playout.
type ExchangeDirection =
    | Left = 0
    | Right = 1
    | Across = 2
    | Hold = 3

module ExchangeDirection =

    /// Total number of exchange directions.
    let numDirections =
        Enum.getValues<ExchangeDirection>.Length

    /// Applies the given exchange direction to the given seat.
    let apply seat dir =
        let n =
            match dir with
                | ExchangeDirection.Hold -> 0
                | ExchangeDirection.Left -> 1
                | ExchangeDirection.Across -> 2
                | ExchangeDirection.Right -> 3
                | _ -> failwith "Unexpected"
        seat |> Seat.incr n

    /// Finds the seat that passes cards to the given seat.
    let unapply seat dir =
        let n =
            match dir with
                | ExchangeDirection.Hold -> 0
                | ExchangeDirection.Left -> 3
                | ExchangeDirection.Across -> 2
                | ExchangeDirection.Right -> 1
                | _ -> failwith "Unexpected"
        seat |> Seat.incr n

/// A deal is a round of play within a game. A closed deal is the
/// "public" view of a deal, so it contains no information about
/// how unplayed cards are distributed among the players, and no
/// information about the exchange (other than its direction).
type ClosedDeal =
    {
        /// Player who dealt this deal.
        Dealer : Seat

        /// Card exchange direction.
        ExchangeDirection : ExchangeDirection

        /// Current active trick, if play is in progress. No
        /// trick is active during an exchange, nor after the
        /// last card of the deal is played.
        CurrentTrickOpt : Option<Trick>

        /// Completed tricks, in reverse chronological order.
        CompletedTricks : List<Trick>

        /// Cards not yet played.
        UnplayedCards : Set<Card>

        /// Is it legal to lead a heart?
        HeartsBroken : bool

        /// Suits that players are known to be void in.
        Voids : Set<Seat * Suit>

        /// Points taken so far by each player.
        Score : Score
    }

module ClosedDeal =

    /// Creates a closed deal.
    let create dealer dir =
        {
            Dealer = dealer
            ExchangeDirection = dir
            CurrentTrickOpt = None
            CompletedTricks = List.empty
            UnplayedCards = set Card.allCards
            HeartsBroken = false
            Voids = Set.empty
            Score = Score.zero
        }

    /// Starts play in the given deal with the given first
    /// trick leader.
    let startPlay leader deal =
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
    let numCardsPerHand =
        assert(Card.numCards % Seat.numSeats = 0)
        Card.numCards / Seat.numSeats

    /// Validates the structure of the given deal.
    let private validate deal =
        assert(deal.CompletedTricks.Length <= numCardsPerHand)
        assert(
            let nCards = numCardsPlayed deal
            (nCards = 0) || (nCards = Card.numCards) = deal.CurrentTrickOpt.IsNone)
        assert(deal.CompletedTricks |> Seq.forall Trick.isComplete)

    /// Current trick in the given deal.
    let currentTrick deal =
        match deal.CurrentTrickOpt with
            | Some trick -> trick
            | None -> failwith "No current trick"

    /// Current player in the given deal, once the exchange
    /// has completed. (There is no current player during the
    /// exchange from the public point of view.)
    let currentPlayer deal =
        deal
            |> currentTrick
            |> Trick.currentPlayer

    /// Lowest club.
    let lowestClub =
        let rank = Enum.getValues<Rank>[0]
        Card.create rank Suit.Clubs

    /// What cards can be played from the given hand?
    let legalPlays (hand : Hand) deal =
        validate deal
        let trick = currentTrick deal
        match deal.CompletedTricks.Length, trick.SuitLedOpt with

                // must lead 2♣ on first trick
            | 0, None ->
                assert(hand.Contains(lowestClub))
                Seq.singleton lowestClub

                // can't lead a heart until they're broken
            | _, None ->
                if deal.HeartsBroken then hand
                else
                    let nonHeartsCards =
                        hand
                            |> Seq.where (fun card ->
                                card.Suit <> Suit.Hearts)
                    if nonHeartsCards |> Seq.isEmpty then hand   // !!!
                    else nonHeartsCards

                // following
            | iTrick, Some suitLed ->

                    // must follow suit, if possible
                let cards =
                    let isSuitLed (card : Card) =
                        card.Suit = suitLed
                    if hand |> Seq.exists isSuitLed then
                        hand |> Seq.where isSuitLed
                    else hand

                    // no point cards on first trick (unless it's unavoidable)
                if iTrick = 0 then
                    let nonPointCards =
                        cards
                            |> Seq.where (fun card ->
                                Card.pointValue card = 0)
                    if nonPointCards |> Seq.isEmpty then cards   // !!!
                    else nonPointCards
                else cards

    /// Is the given player known to be void in the given suit?
    let private isVoid seat suit deal =
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

            // remove from unplayed cards
        let unplayedCards =
            assert(deal.UnplayedCards.Contains(card))
            deal.UnplayedCards.Remove(card)

            // hearts broken?
        let heartsBroken =
            if deal.HeartsBroken
                || Card.pointValue card > 0 then true   // Q♠ also breaks hearts
            else false

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

            // leader is void in all non-hearts suits? (very rare)
        let voids =
            if card.Suit = Suit.Hearts
                && not deal.HeartsBroken
                && updatedTrick.Leader = player then
                deal.Voids
                    .Add(player, Suit.Clubs)
                    .Add(player, Suit.Diamonds)
                    .Add(player, Suit.Spades)
            else voids

        {
            deal with
                CurrentTrickOpt = curTrickOpt
                CompletedTricks = completedTricks
                HeartsBroken = heartsBroken
                UnplayedCards = unplayedCards
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
