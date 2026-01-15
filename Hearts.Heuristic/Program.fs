namespace Hearts.Heuristic

open System
open Hearts

module Program =

    let numDeals = 200_000

    let geminiPayoff =
        let rng = Random(0)
        Tournament.run rng true numDeals Claude.player Gemini.player
            |> snd

    let claudePayoff =
        let rng = Random(0)
        Tournament.run rng true numDeals Gemini.player Claude.player
            |> snd

    printfn $"Gemini payoff: {geminiPayoff - claudePayoff}"
    printfn $"Claude payoff: {claudePayoff - geminiPayoff}"
