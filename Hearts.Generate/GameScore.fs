namespace Hearts.Generate

open System

open MathNet.Numerics.Distributions

open PlayingCards
open Hearts

/// Simulates the game scores of realistic games under the arena's
/// rule that a shoot always increases opponents' scores. Constants
/// were fitted to 100,000 games of the original deal-level production
/// model (AdvantageModel-i005-e027) playing itself. Written by
/// Claude, with my changes.
module GameScore =

    /// Chance that a deal is a shoot.
    let private shootRate = 0.185

    /// Hearts constants.
    let private queenPoints = Card.pointValue Card.queenOfSpades
    let private numHearts = Rank.numRanks

    /// Relative chance that the player who takes the Q♠ also takes
    /// 0, 1, ..., 12 hearts without shooting the moon.
    let private queenHeartWeights =
        let values =
            [| 118.0; 94.0; 84.0;
                94.0; 91.0; 81.0;
                74.0; 70.0; 69.0;
                65.0; 56.0; 60.0; 42.0 |]
        assert(values.Length = numHearts)
        values

    /// How evenly the remaining hearts are shared among the other
    /// three players. Each heart goes to a player with weight
    /// alpha + the number of hearts that player already has (a
    /// Pólya urn), so a small alpha bunches the hearts together,
    /// as tricks do.
    let private alpha = 0.4

    /// Picks an index at random, in proportion to the given weights.
    let private pickWeighted rng weights =
        Categorical.Sample(rng, weights)

    /// Generates a realistic deal score, including the shoot reward.
    let private generateDealScore (rng : Random) =

            // moon shot?
        if rng.NextDouble() < shootRate then
            let shooter = rng.Next(Seat.numSeats)
            Array.init Seat.numSeats (fun iSeat ->
                if iSeat = shooter then 0
                else ClosedDeal.numPointsPerDeal)
                |> Score.ofPoints

            // not a moon shot
        else
                // who took the Q♠?
            let queenSeat = enum<Seat> (rng.Next(Seat.numSeats))

                // how many hearts the Q♠ taker also takes 
            let nQueenHearts = pickWeighted rng queenHeartWeights

                // number of hearts taken by other players, in playing order after the Q♠ taker
            let others =
                (Array.zeroCreate (Seat.numSeats - 1),
                seq { 1 .. numHearts - nQueenHearts })
                    ||> Seq.fold (fun counts _ ->
                        let iOther =
                            counts
                                |> Array.map (fun n -> alpha + float n)
                                |> pickWeighted rng
                        counts |> Array.updateAt iOther (counts[iOther] + 1))

                // build final deal score
            Seat.allSeats
                |> Array.map (fun seat ->
                    let offset = Seat.getIndex seat queenSeat
                    if offset = 0 then queenPoints + nQueenHearts
                    else others[offset - 1])
                |> Score.ofPoints

    /// Simulates a game, answering the game score at the start of
    /// each of its deals, starting with 0-0-0-0. Deal i of the game
    /// passes in direction i % 4.
    let generate rules rng =
        assert(rules.MoonShotScoring
            = MoonShotScoringRule.AlwaysAddToOthers)
        Score.zero
            |> Seq.unfold (fun score ->
                if GameScore.isOver rules.TieGame score then None
                else Some (score, score + generateDealScore rng))
