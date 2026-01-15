namespace Hearts.Heuristic

open Hearts

module Gemini =

    let player =
        let act (infoSet : InformationSet) =
            infoSet.LegalActions[0]   // placeholder
        { Act = act }
