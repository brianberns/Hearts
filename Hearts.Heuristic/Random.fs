namespace Hearts.Heuristic

open System
open Hearts

module Random =

    let private rng = Random.Shared

    let private act infoSet =
        let i = rng.Next(infoSet.LegalActions.Length)
        infoSet.LegalActions[i]

    let player =
        { Act = act }
