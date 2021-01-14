namespace Hearts

open PlayingCards

type Player =
    private {

        /// Function that passes cards in the given deal.
        MakePass : OpenDeal -> Score (*game score*) -> Set<Card>

        /// Function that plays a card in the given deal.
        MakePlay : OpenDeal -> Score (*game score*) -> Card
    }

    /// Passes cards in the given deal.
    member player.Pass(deal, gameScore) =
        let cards = player.MakePass deal gameScore
        assert(cards.Count = Exchange.numCards)
        cards

    /// Plays a card in the given deal.
    member player.Play(deal, gameScore) =
        let hand = deal |> OpenDeal.currentHand
        let legalPlays = 
            deal.ClosedDeal
                |> ClosedDeal.legalPlays hand
        let card =
            legalPlays
                |> Seq.tryExactlyOne
                |> Option.defaultWith (fun () ->
                    player.MakePlay deal gameScore)
        assert(legalPlays |> Seq.contains card)
        card

module Player =

    /// Creates a player.
    let create makePass makePlay =
        {
            MakePass = makePass
            MakePlay = makePlay
        }
