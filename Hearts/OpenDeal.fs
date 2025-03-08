namespace Hearts

open PlayingCards

/// An open deal contains all information about a deal,
/// including private information, such as each player's
/// hand.
type OpenDeal =
    {
        /// Base deal.
        ClosedDeal : ClosedDeal

        /// Cards passed by each player on non-hold deals.
        ExchangeOpt : Option<Exchange>

        /// Each player's unplayed cards.
        UnplayedCardMap : Map<Seat, Hand>
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
            nCards = Card.numCards)
        {
            ClosedDeal = ClosedDeal.create dealer dir
            ExchangeOpt =
                if dir = ExchangeDirection.Hold then None
                else
                    Seat.next dealer   // dealer passes last
                        |> Exchange.create
                        |> Some
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

    /// Passes the given card in the given deal.
    let addPass card deal =
        assert(
            ClosedDeal.numCardsPlayed deal.ClosedDeal = 0)
        assert(
            deal.ClosedDeal.ExchangeDirection
                <> ExchangeDirection.Hold)

        match deal.ExchangeOpt with
            | Some exchange ->

                    // remove outgoing card from passer's hand
                let cardMap =
                    let passer = Exchange.currentPasser exchange
                    let cardMap = deal.UnplayedCardMap
                    let unplayedCards =
                        assert(cardMap[passer].Contains(card))
                        cardMap[passer] |> Set.remove card
                    cardMap |> Map.add passer unplayedCards

                {
                    deal with
                        ExchangeOpt =
                            exchange
                                |> Exchange.addPass card
                                |> Some
                        UnplayedCardMap = cardMap
                }

            | None -> failwith "Unexpected"

    /// Receives cards from the given passer in the given deal.
    let private receivePass passer (cards : Pass) deal =
        assert(deal.ExchangeOpt.IsSome)
        assert(
            deal.ClosedDeal.ExchangeDirection
                <> ExchangeDirection.Hold)

            // add incoming cards to receiver's hand
        let cardMap =
            let receiver =
                deal.ClosedDeal.ExchangeDirection
                    |> ExchangeDirection.apply passer
            let cardMap = deal.UnplayedCardMap
            let unplayedCards =
                assert(
                    Set.intersect cardMap[receiver] cards
                        |> Set.isEmpty)
                Set.union cardMap[receiver] cards
            cardMap |> Map.add receiver unplayedCards

        {
            deal with
                UnplayedCardMap = cardMap
        }

    /// Receives passed cards (if any) and starts play in the
    /// given deal.
    let startPlay deal =
        assert(
            deal.ClosedDeal
                |> ClosedDeal.numCardsPlayed = 0)
        assert(
            (deal.ClosedDeal.ExchangeDirection
                = ExchangeDirection.Hold)
                = deal.ExchangeOpt.IsNone)

            // receive passed cards?
        let deal =
            match deal.ExchangeOpt with
                | Some exchange ->
                    assert(Exchange.isComplete exchange)
                    assert(
                        deal.UnplayedCardMap
                            |> Map.forall (fun _ cards ->
                                cards.Count =
                                    ClosedDeal.numCardsPerHand - Pass.numCards))
                    (deal, exchange.PassMap)
                        ||> Seq.fold (fun deal (KeyValue(passer, pass)) ->
                            receivePass passer pass deal)
                | None -> deal

            // determine first trick leader (must wait until cards are passed)
        let leader =
            assert(
                deal.UnplayedCardMap
                    |> Map.forall (fun _ cards ->
                        cards.Count = ClosedDeal.numCardsPerHand))
            deal.UnplayedCardMap
                |> Map.toSeq
                |> Seq.find (fun (_, cards) ->
                    cards.Contains(ClosedDeal.lowestClub))
                |> fst

        {
            deal with
                ClosedDeal =
                    deal.ClosedDeal
                        |> ClosedDeal.startPlay leader
        }

    /// Current player in the given deal.
    let currentPlayer deal =
        match deal.ExchangeOpt with
            | Some exchange
                when not (Exchange.isComplete exchange) ->
                assert(
                    deal.ClosedDeal.ExchangeDirection
                        <> ExchangeDirection.Hold)
                Exchange.currentPasser exchange
            | _ ->
                ClosedDeal.currentPlayer deal.ClosedDeal

    /// Answers the current player's unplayed cards.
    let currentHand deal =
        deal.UnplayedCardMap[currentPlayer deal]

    /// Answers the current player's information set.
    let currentInfoSet deal =

        let player = currentPlayer deal

        let secret =
            let hand = deal.UnplayedCardMap[player]
            let outOpt, inOpt =
                deal.ExchangeOpt
                    |> Option.map (
                        Exchange.getPassOpts
                            player
                            deal.ClosedDeal.ExchangeDirection)
                    |> Option.defaultValue (None, None)
            Secret.create hand outOpt inOpt

        InformationSet.create
            player secret deal.ClosedDeal

    /// Plays the given card on the given deal.
    let addPlay card deal =
        {
            deal with
                ClosedDeal =
                    deal.ClosedDeal
                        |> ClosedDeal.addPlay card
                UnplayedCardMap =
                    let seat = deal.ClosedDeal |> ClosedDeal.currentPlayer
                    let unplayedCards = deal.UnplayedCardMap[seat]
                    assert(unplayedCards.Contains(card))
                    let unplayedCards = unplayedCards.Remove(card)
                    deal.UnplayedCardMap |> Map.add seat unplayedCards
        }

    /// Total number of points in a deal.
    let numPointsPerDeal =
        Card.allCards
            |> Seq.sumBy Card.pointValue

    /// Determines the inevitable additional score of the given deal,
    /// if possible.
    let tryFindInevitable deal =

            // applies only at trick boundaries
        let isApplicable =
            match deal.ExchangeOpt,
                deal.ClosedDeal.CurrentTrickOpt with
                | Some exchange, _
                    when not (
                        Exchange.isComplete exchange) -> false
                | _, Some trick -> trick.Cards.IsEmpty
                | _, None ->
                    assert(
                        ClosedDeal.isComplete deal.ClosedDeal)
                    true   // end of deal

        if isApplicable then

                // deal is actually complete?
            if ClosedDeal.isComplete deal.ClosedDeal then
                Some Score.zero

                // all points have been taken?
            elif Score.sum deal.ClosedDeal.Score = numPointsPerDeal then
                Some Score.zero

                // current player takes all remaining tricks?
            else
                    // gather opponent cards
                let player = currentPlayer deal
                assert(
                    (ClosedDeal.currentTrick deal.ClosedDeal).Leader
                        = player)
                let cardMap =
                    deal.UnplayedCardMap
                        |> Map.toSeq
                        |> Seq.where (fst >> (<>) player)
                        |> Seq.collect snd
                        |> Seq.groupBy Card.suit
                        |> Seq.map (fun (suit, cards) ->
                            let ranks =
                                cards
                                    |> Seq.map Card.rank
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

                    // player's cards are all winners?
                let hand = deal.UnplayedCardMap[player]
                if hand |> Seq.forall isWinner then
                    let points =
                        deal.UnplayedCardMap
                            |> Map.toSeq
                            |> Seq.collect snd
                            |> Seq.sumBy Card.pointValue
                    Score.create player points |> Some
                else None

        else None
