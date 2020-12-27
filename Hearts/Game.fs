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

    /// Plays a deal in the given game.
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

            // playout
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

    /// Number of points that ends the game.
    let gameOverThreshold = 100

    /// Determines seats that won the given game, if any.
    let winningSeats game =

            // is game over?
        let seatPoints =
            let (ScoreMap scoreMap) = game.Score
            scoreMap |> Map.toSeq
        let points =
            seatPoints |> Seq.map snd
        let maxPoints =
            points |> Seq.max
        if maxPoints >= gameOverThreshold then

                // find winning seats
            let minPoints =
                points |> Seq.max
            seatPoints
                |> Seq.where (fun (_, points) ->
                    points = minPoints)
                |> Seq.map fst
                |> Seq.toArray
        else
            Array.empty

    /// Plays the given game.
    let playGame rng dealer dir game =

            // play deals with rotating dealer and exchange direction
        let rec loop dealer dir game =
            seq {
                    // play one deal
                let game =
                    {
                        game with
                            CurrentDeal =
                                let deck = Deck.shuffle rng
                                OpenDeal.fromDeck dealer dir deck
                    }
                let game = game |> playDeal
                yield game

                    // continue this game?
                if game |> winningSeats |> Array.isEmpty then
                    let dealer = dealer.Next
                    let dir = ExchangeDirection.next dir
                    yield! game |> loop dealer dir
            }

        game
            |> loop dealer dir
            |> Seq.toArray
