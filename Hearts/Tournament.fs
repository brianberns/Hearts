namespace Hearts

open PlayingCards
open Hearts

/// Model Hearts as a zero-sum game.
module ZeroSum =

    /// Gets the payoff for the given raw score from each
    /// player's point of view.
    let getPayoff score =
        let points = score.Points
        assert(points.Length = Seat.numSeats)
        let sum = Seq.sum points
        [|
            for pt in points do
                let otherAvg =
                    float32 (sum - pt)
                        / float32 (Seat.numSeats - 1)
                otherAvg - float32 pt
        |]

    /// Computes the payoff for the given deal, if it is
    /// inevitable.
    let tryGetPayoff deal =
        Game.tryUpdateScore deal Score.zero
            |> Option.map getPayoff

module Tournament =

    /// Plays one deal.
    let private playDeal (playerMap : Map<_, _>) deal =

        let rec loop deal score =
            let deal =
                let infoSet = OpenDeal.currentInfoSet deal
                let action =
                    match Seq.tryExactlyOne infoSet.LegalActions with
                        | Some action -> action
                        | None -> playerMap[infoSet.Player].Act infoSet
                OpenDeal.addAction
                    infoSet.LegalActionType action deal
            match Game.tryUpdateScore deal score with
                | Some score -> score
                | None -> loop deal score

        loop deal Score.zero

    /// Plays the given number of deals.
    let private playDeals rng inParallel numDeals playerMap =
        OpenDeal.playDeals rng inParallel numDeals (
            playDeal playerMap)
            |> Seq.reduce (+)

    /// Runs a 2v2 tournament between two players.
    let run rng inParallel numDeals champion challenger =
        let challengerSeats = set [ Seat.East; Seat.West ]
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if challengerSeats.Contains(seat) then
                            challenger
                        else champion
                    seat, player)
                |> Map
        let score =
            playDeals rng inParallel numDeals playerMap
        let payoff =
            let payoffs = ZeroSum.getPayoff score
            let sum =
                challengerSeats
                    |> Seq.sumBy (fun seat -> payoffs[int seat])
            sum / float32 (challengerSeats.Count * numDeals)
        score, payoff
