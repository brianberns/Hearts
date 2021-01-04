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
        player.MakePass deal gameScore

    /// Plays a card in the given deal.
    member player.Play(deal, gameScore) =
        let hand = deal |> OpenDeal.currentHand
        deal.ClosedDeal
            |> ClosedDeal.legalPlays hand
            |> Seq.tryExactlyOne
            |> Option.defaultWith (fun () ->
                player.MakePlay deal gameScore)

module Player =

    /// Creates a player.
    let create makePass makePlay =
        {
            MakePass = makePass
            MakePlay = makePlay
        }
