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
            ClosedDeal = ClosedDeal.create dealer
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
        let unplayedCards = cardMap.[passer]
        assert(Set.intersect unplayedCards cards = cards)
        let unplayedCards = Set.difference unplayedCards cards
        let cardMap = cardMap |> Map.add passer unplayedCards

            // add incoming cards to receiver's hand
        let receiver =
            deal.Exchange.ExchangeDirection
                |> ExchangeDirection.apply passer
        let unplayedCards = cardMap.[receiver]
        assert(Set.intersect unplayedCards cards |> Set.isEmpty)
        let unplayedCards = Set.union unplayedCards cards
        let cardMap = cardMap |> Map.add receiver unplayedCards

        {
            deal with
                Exchange =
                    deal.Exchange |> Exchange.addPass cards
                UnplayedCardMap = cardMap
        }

    /// Answers the current player's unplayed cards.
    let currentHand deal =
        let seat =
            deal.ClosedDeal
                |> ClosedDeal.currentPlayer
        deal.UnplayedCardMap.[seat]

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
