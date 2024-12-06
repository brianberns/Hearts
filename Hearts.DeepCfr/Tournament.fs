namespace Hearts.DeepCfr

open MathNet.Numerics.Distributions

open PlayingCards

open Hearts
open Hearts.FastCfr
open Hearts.Web

module Tournament =

    let run champion challenger =
        let playerMap =
            Enum.getValues<Seat>
                |> Seq.map (fun seat ->
                    let player =
                        if seat = Seat.South then challenger
                        else champion
                    seat, player)
                |> Map
        let score =
            Game.playDeals
                (System.Random(0))   // use same deals each iteration
                settings.NumEvaluationDeals
                playerMap
        printfn "\nTournament score:"
        for (KeyValue(seat, points)) in score.ScoreMap do
            printfn $"   {seat}: {points}"

    let randomPlayer =

        let play hand deal =
            let legalPlays =
                ClosedDeal.legalPlays hand deal
                    |> Seq.toArray
            legalPlays[settings.Random.Next(legalPlays.Length)]

        { Play = play }

module Database =

    /// Database connection.
    let private conn = Database.connect "."

    /// Plays a card from the given hand in the given deal.
    let private play hand deal =
        let legalPlays =
            ClosedDeal.legalPlays hand deal
                |> Seq.toArray
        let index =
            if legalPlays.Length = 1 then 0
            else
                deal
                    |> ClosedDeal.adjustDeal Seat.South
                    |> GameState.getInfoSetKey hand
                    |> Database.tryGetStrategy conn
                    |> Option.map (fun strategy ->
                        Categorical.Sample(settings.Random, strategy))
                    |> Option.defaultValue 0
        legalPlays[index]

    /// Database player.
    let player =
        { Play = play }
