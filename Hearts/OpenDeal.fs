namespace Hearts

open PlayingCards

type OpenDeal =
    {
        /// Cards passed by each player.
        Exchange : Exchange

        /// Base deal.
        ClosedDeal : ClosedDeal

        /// Each player's unplayed cards.
        UnplayedCardMap : Map<Seat, Set<Card>>
    }

module OpenDeal =

    /// Creates a deal from the given hands.
    let fromHands dealer dir handMap =
        assert(
            let nCards =
                handMap
                    |> Map.toSeq
                    |> Seq.collect snd
                    |> Seq.distinct
                    |> Seq.length
            nCards = ClosedDeal.numCardsPerDeal)
        {
            Exchange = Exchange.create dealer dir
            ClosedDeal = ClosedDeal.initial
            UnplayedCardMap = handMap
        }

    /// Deals cards from the given deck to each player.
    let fromDeck dealer dir deck =
        deck.Cards
            |> Seq.indexed
            |> Seq.groupBy (fun (iCard, _) ->
                let n = (iCard + 1) % Seat.numSeats
                dealer |> Seat.incr n)
            |> Seq.map (fun (seat, group) ->
                let cards =
                    group
                        |> Seq.map snd
                        |> set
                seat, cards)
            |> Map
            |> fromHands dealer dir

    /// Passes the given cards in the given deal.
    let addPass cards deal =
        assert(deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0)

            // remove outgoing cards from passer's hand
        let cardMap = deal.UnplayedCardMap
        let passer =
            deal.Exchange |> Exchange.currentPasser
        assert(cardMap.[passer].Count = ClosedDeal.numCardsPerHand)
        let unplayedCards = cardMap.[passer]
        assert(Set.intersect unplayedCards cards = cards)
        let unplayedCards = Set.difference unplayedCards cards
        let cardMap = cardMap |> Map.add passer unplayedCards

        {
            deal with
                Exchange =
                    deal.Exchange |> Exchange.addPass cards
                UnplayedCardMap = cardMap
        }

    /// Receives cards from the given passer in the given deal.
    let private receivePass passer cards deal =

            // add incoming cards to receiver's hand
        let cardMap = deal.UnplayedCardMap
        let receiver =
            deal.Exchange.ExchangeDirection
                |> ExchangeDirection.apply passer
        let unplayedCards = cardMap.[receiver]
        assert(Set.intersect unplayedCards cards |> Set.isEmpty)
        let unplayedCards = Set.union unplayedCards cards
        let cardMap = cardMap |> Map.add receiver unplayedCards

        {
            deal with
                UnplayedCardMap = cardMap
        }

    /// Starts play in the given deal.
    let startPlay deal =
        assert(deal.Exchange |> Exchange.isComplete)
        assert(deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0)

            // receive passed cards?
        let deal =
            if deal.Exchange |> Exchange.isHold then deal
            else
                assert(
                    deal.UnplayedCardMap
                        |> Map.forall (fun _ cards ->
                            cards.Count =
                                ClosedDeal.numCardsPerHand - Exchange.numCards))
                let seatPasses =
                    deal.Exchange
                        |> Exchange.seatPasses
                (deal, seatPasses)
                    ||> Seq.fold (fun deal (passer, cards) ->
                        receivePass passer cards deal)

            // determine first trick leader (must wait until cards are passed)
        let leader =
            assert(
                deal.UnplayedCardMap
                    |> Map.forall (fun _ cards ->
                        cards.Count = ClosedDeal.numCardsPerHand))
            deal.UnplayedCardMap
                |> Map.toSeq
                |> Seq.find (fun (_, cards) ->
                    cards |> Set.contains ClosedDeal.card2C)
                |> fst
        {
            deal with
                ClosedDeal =
                    deal.ClosedDeal |> ClosedDeal.start leader
        }

    /// Current player in the given deal.
    let currentPlayer deal =
        if deal.Exchange |> Exchange.isComplete then
            deal.ClosedDeal |> ClosedDeal.currentPlayer
        else
            deal.Exchange |> Exchange.currentPasser

    /// Answers the current player's unplayed cards.
    let currentHand deal =
        deal.UnplayedCardMap.[currentPlayer deal]

    /// Plays the given card on the given deal.
    let addPlay card deal =
        {
            deal with
                ClosedDeal =
                    deal.ClosedDeal
                        |> ClosedDeal.addPlay card
                UnplayedCardMap =
                    let seat = deal.ClosedDeal |> ClosedDeal.currentPlayer
                    let unplayedCards = deal.UnplayedCardMap.[seat]
                    assert(unplayedCards.Contains(card))
                    let unplayedCards = unplayedCards.Remove(card)
                    deal.UnplayedCardMap |> Map.add seat unplayedCards
        }

    /// Total number of points in a deal.
    let numPointsPerDeal =
        Card.allCards
            |> Seq.sumBy Card.pointValue

    /// Determines the final additional score of the given deal, if
    /// possible.
    let tryFinalize deal =

            // applies only at trick boundaries
        assert(
            deal.ClosedDeal.CurrentTrickOpt
                |> Option.map (fun trick -> trick.Cards.IsEmpty)
                |> Option.defaultValue true)

            // deal is actually complete?
        if deal.ClosedDeal |> ClosedDeal.isComplete then
            Some Score.zero

            // all points have been taken?
        elif deal.ClosedDeal.Score |> Score.sum = numPointsPerDeal then
            Some Score.zero

            // current player takes all remaining tricks?
        elif ClosedDeal.numCardsPerHand - deal.ClosedDeal.CompletedTricks.Length > 1 then   // KH doesn't consider this on last trick

            let player = currentPlayer deal
            let cardMap =
                deal.UnplayedCardMap
                    |> Map.toSeq
                    |> Seq.where (fun (seat, _) -> seat <> player)
                    |> Seq.collect snd
                    |> Seq.groupBy (fun card -> card.Suit)
                    |> Seq.map (fun (suit, cards) ->
                        let ranks =
                            cards
                                |> Seq.map (fun card -> card.Rank)
                                |> Seq.toArray
                        suit, ranks)
                    |> Map

            /// Is the given card guaranteed to win any trick in which
            /// it is led?
            let isWinner (card : Card) =
                cardMap
                    |> Map.tryFind card.Suit
                    |> Option.defaultValue Array.empty
                    |> Array.forall (fun rank ->
                        assert(rank <> card.Rank)
                        rank < card.Rank)

            let hand = deal.UnplayedCardMap.[player]
            if hand |> Seq.forall (fun card -> isWinner card) then
                let points =
                    deal.UnplayedCardMap
                        |> Map.toSeq
                        |> Seq.collect snd
                        |> Seq.sumBy Card.pointValue
                Score.create player points |> Some
            else None

        else None
