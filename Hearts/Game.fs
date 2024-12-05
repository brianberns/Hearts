namespace Hearts

open PlayingCards

/// Interface for a Hearts player.
type Player =
    {
        /// Plays a card from the given hand on the given deal.
        Play : Hand -> ClosedDeal -> Card
    }

module Game =

    /// Answers the seat of the player who shot the moon, if
    /// one did. The given score must not already include the
    /// shoot reward.
    let tryFindShooter dealScore =
        option {
            assert(Score.sum dealScore = OpenDeal.numPointsPerDeal)
            let! seat, _ =
                dealScore.ScoreMap
                    |> Map.toSeq
                    |> Seq.tryFind (fun (_, points) ->
                        points = OpenDeal.numPointsPerDeal)
            return seat
        }

    /// Finds leaders in the given game score.
    let private findGameLeaders gameScore =
        let minPoints = Seq.min gameScore.ScoreMap.Values
        gameScore.ScoreMap
            |> Map.toSeq
            |> Seq.where (snd >> (=) minPoints)
            |> Seq.map fst
            |> set

    /// End of game point threshold.
#if MINI
    let endThreshold = 40
#else
    let endThreshold = 100
#endif

    /// Finds game winners, if any, in the given game score.
    let findGameWinners gameScore =
        let isOver =
            gameScore.ScoreMap.Values
                |> Seq.exists (fun points ->
                    points >= endThreshold)
        if isOver then findGameLeaders gameScore
        else Set.empty

    /// Applies the shoot reward to the given game score for
    /// the given shooter.
    let private applyShootReward gameScore shooter =

        let toScore seatPoints =
            { ScoreMap = Map seatPoints }

            // add points to non-shooters
        let gameScore' =
            let dealScore =
                Enum.getValues<Seat>
                    |> Seq.map (fun seat ->
                        let points =
                            if seat = shooter then 0
                            else OpenDeal.numPointsPerDeal
                        seat, points)
                    |> toScore
            gameScore + dealScore

            // subtract points from shooter instead?
        let leaders = findGameLeaders gameScore'
        if leaders.Contains(shooter) then
            gameScore'
        else
            let dealScore =
                Enum.getValues<Seat>
                    |> Seq.map (fun seat ->
                        let points =
                            if seat = shooter then
                                -OpenDeal.numPointsPerDeal
                            else 0
                        seat, points)
                    |> toScore
            gameScore + dealScore

    /// Updates the given game score, including shoot
    /// reward, if possible.
    let tryUpdateScore deal gameScore =
        option {
            let! inevitable = OpenDeal.tryFindInevitable deal
            let dealScore = deal.ClosedDeal.Score + inevitable
            assert(Score.sum dealScore = OpenDeal.numPointsPerDeal)
            return tryFindShooter dealScore
                |> Option.map (applyShootReward gameScore)
                |> Option.defaultValue (gameScore + dealScore)
        }

    /// Creates and plays one deal.
    let playDeal rng dealer (playerMap : Map<_, _>) gameScore =

        let rec loop deal gameScore =
            let deal =
                let seat = OpenDeal.currentPlayer deal
                let card =
                    let hand = deal.UnplayedCardMap[seat]
                    playerMap[seat].Play hand deal.ClosedDeal
                OpenDeal.addPlay card deal
            match tryUpdateScore deal gameScore with
                | Some gameScore -> gameScore
                | None -> loop deal gameScore

        let deal =
            let deck = Deck.shuffle rng
            OpenDeal.fromDeck
                dealer
                ExchangeDirection.Hold
                deck
                |> OpenDeal.startPlay
        loop deal gameScore

    /// Plays the given number of deals.
    let playDeals rng numDeals playerMap =
        (Score.zero, seq { 0 .. numDeals - 1})
            ||> Seq.fold (fun gameScore iDeal ->
                let dealer = enum<Seat> (iDeal % Seat.numSeats)
                playDeal rng dealer playerMap gameScore)
