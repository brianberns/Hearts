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

    /// 2♣ leads on the first trick.
    let private twoClubs = Card.fromString("2♣")

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
        let leader =
            handMap
                |> Map.toSeq
                |> Seq.find (fun (_, cards) ->
                    cards |> Set.contains twoClubs)
                |> fst
        {
            Exchange = Exchange.create dealer dir
            ClosedDeal = ClosedDeal.create leader
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

    /// Completes the exchange phase of the given deal.
    let completeExchange deal =
        assert(deal.Exchange |> Exchange.isComplete)
        assert(deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0)
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
                    deal.ClosedDeal |> ClosedDeal.addPlay card
                UnplayedCardMap =
                    let seat = deal.ClosedDeal |> ClosedDeal.currentPlayer
                    let unplayedCards = deal.UnplayedCardMap.[seat]
                    assert(unplayedCards.Contains(card))
                    let unplayedCards = unplayedCards.Remove(card)
                    deal.UnplayedCardMap |> Map.add seat unplayedCards
        }
