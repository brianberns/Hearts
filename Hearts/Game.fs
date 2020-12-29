﻿namespace Hearts

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

        /// Current deal, if game has started.
        CurrentDealOpt : Option<OpenDeal>

        /// Score of the game.
        Score : Score
    }

module Game =

    /// Creates a game.
    let create playerMap =
        {
            PlayerMap = playerMap
            CurrentDealOpt = None
            Score = Score.zero
        }

    /// Plays a trick in the given game's current deal.
    let playTrick deal game =
        assert(game.CurrentDealOpt = Some deal)
        assert(deal.ClosedDeal.CurrentTrickOpt.Value.Cards.IsEmpty)
        (deal, Seq.init Seat.numSeats id)
            ||> Seq.fold (fun deal _ ->
                let seat = deal.ClosedDeal |> ClosedDeal.currentPlayer
                let player = game.PlayerMap.[seat]
                let card = player.MakePlay deal game.Score
                deal |> OpenDeal.addPlay card)

    /// Plays tricks in the given game's current deal.
    let rec playTricks deal game =

            // play a trick
        let deal = playTrick deal game

            // update game status with result of trick
        let game =
            {
                game with
                    CurrentDealOpt = Some deal
                    Score = game.Score + deal.ClosedDeal.Score
            }

            // play another trick?
        if deal.ClosedDeal |> ClosedDeal.isComplete then
            deal, game
        else playTricks deal game

    /// Plays a deal in the given game.
    let playDeal game =
        match game.CurrentDealOpt with
            | Some deal ->
                assert(deal.ClosedDeal |> ClosedDeal.numCardsPlayed = 0)

                    // exchange
                let deal =
                    if deal.Exchange |> Exchange.isHold then
                        deal
                    else
                        (deal, Seq.init Seat.numSeats id)
                            ||> Seq.fold (fun deal _ ->
                                let seat = deal.Exchange |> Exchange.currentPasser
                                let player = game.PlayerMap.[seat]
                                let cards = player.MakePass deal game.Score
                                deal |> OpenDeal.addPass cards)
                            |> OpenDeal.startPlay
                let game =
                    { game with CurrentDealOpt = Some deal }

                    // playout
                let _, game =
                    assert(deal.ClosedDeal.Score = Score.zero)
                    ((deal, game), Seq.init ClosedDeal.numCardsPerHand id)
                        ||> Seq.fold (fun (deal, game) _ ->
                            playTricks deal game)
                assert(deal.ClosedDeal |> ClosedDeal.isComplete)

                // to-do: shoot the moon
                game

            | None -> failwith "Game has not started"

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
                            CurrentDealOpt =
                                let deck = Deck.shuffle rng
                                OpenDeal.fromDeck dealer dir deck
                                    |> Some
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
