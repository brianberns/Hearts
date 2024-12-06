namespace Hearts.DeepCfr

open MathNet.Numerics.Distributions

open Hearts
open Hearts.FastCfr
open Hearts.Web

module Champion =

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

    /// Champion player.
    let player =
        { Play = play }
