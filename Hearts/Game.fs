namespace Hearts

open PlayingCards

type Player =
    {
        /// Function that passes cards in the given deal.
        MakePass : OpenDeal -> Score -> Set<Card>

        /// Function plays a card in the given deal.
        MakePlay : OpenDeal -> Score -> Card
    }

type Game =
    {
        /// Player that occupies each seat.
        PlayerMap : Map<Seat, Player>

        /// Current deal.
        CurrentDeal : OpenDeal

        /// Score of the game.
        Score : Score
    }

module Game =

    let playDeal game =

            // initial deal
        let deal = game.CurrentDeal
        assert(deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0)

            // exchange
        let deal =
            let dir = deal.ExchangeDirection
            if dir = ExchangeDirection.Hold then deal
            else
                let seatCards =
                    Seat.allSeats
                        |> Array.map (fun seat ->
                            let cards =
                                game.PlayerMap.[seat].MakePass deal game.Score
                            seat, cards)
                deal |> OpenDeal.exchange seatCards

        let deal =
            assert(deal.ClosedDeal.Score = Score.zero)
            (deal, seq { 1 .. ClosedDeal.numCardsPerDeal })
                ||> Seq.fold (fun deal _ ->
                    let seat = deal.ClosedDeal |> ClosedDeal.currentPlayer
                    let player = game.PlayerMap.[seat]
                    let card = player.MakePlay deal game.Score
                    deal |> OpenDeal.addPlay card)
        assert(deal.ClosedDeal |> ClosedDeal.isComplete)

        {
            game with
                CurrentDeal = deal
                Score = game.Score + deal.ClosedDeal.Score   // to-do: shoot the moon
        }
