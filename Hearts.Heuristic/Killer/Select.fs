namespace Hearts.Heuristic.Killer

/// The card for Killer Hearts' situation (select_best_card_for_situation).
module Select =

    /// The card the situation calls for.
    let card (d : Decision) (sit : int) =
        let a = d.Analysis
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        // Killer Hearts' suit_card answers 0 past the end of a suit
        let at s i = if s < 0 || i >= a.SuitCards[s].Length then 0 else a.SuitCards[s][i]
        let hsl, lsl, lgtW, hltW = d.Hsl, d.Lsl, d.LgtW, a.HltW
        let hnQS, hltQS, pQS = a.HnQS, a.HltQS, a.PQS
        let fnv = a.FollowersNotVoid
        let lenS = a.SuitLen[spades]
        let mutable rank = none
        let mutable suit = none
        match sit with
            | 1 ->
                rank <- if hltQS = 0 then hnQS else hltQS
                suit <- spades
            | 2 ->
                rank <- queen
                suit <- spades
            | 3 ->
                suit <- suitLed
                rank <-
                    if hltW > 0 then hltW
                    elif suitLed = spades && hnQS > 0 then hnQS
                    elif fnv = 0 then hsl
                    else lgtW
            | 4 ->
                rank <- hsl
                suit <- suitLed
            | 5 ->
                rank <- lgtW
                suit <- suitLed
                if lgtW > 0 && t.Seq < 3 then ()
                elif suit = spades && lgtW > 0 && ((a.OppOwnsQS && not d.QSOwnerHasPlayed) || hsl = queen) then ()
                else rank <- hsl
            | 6 ->
                rank <- if a.HU[hearts] > 0 then (at hearts (a.HU[hearts] - 1)) else a.HighIn[hearts]
                suit <- hearts
            | 7 ->
                suit <- spades
                rank <- if d.SpadesSafe then hnQS elif a.LowIn[spades] = queen then hnQS else a.LowIn[spades]
                if hnQS = 0 then rank <- queen
            | 11 ->
                rank <- lsl
                suit <- suitLed
                if suit = spades && lsl = queen then rank <- (at spades (pQS - 1))
            | 12 ->
                rank <- d.BestLeadRank
                suit <- d.BestLeadSuit
                if suit <> spades || rank < queen || t.QSPlayed then ()
                elif rank >= king && a.QSInHand then ()
                elif rank >= king && (d.Mode = Mode.Equivocate || d.Mode = Mode.ProtectHiScorer) then ()
                elif hltQS > 0 then rank <- hltQS
                elif hnQS > 0 then rank <- hnQS
            | 13 ->
                rank <- d.SafeCard
                suit <- suitLed
            | 14 ->
                rank <- d.RankToSluff
                suit <- d.SuitToSluff
                let z = a.SuitIsLive[spades] && not t.QSPlayed
                if a.SuitIsLive[suit] then ()
                elif a.SuitIsLive[hearts] || a.ALiveSuit = none then ()
                elif z then ()
                else
                    rank <- a.HighIn[a.ALiveSuit]
                    suit <- a.ALiveSuit
                let lenH = a.SuitLen[hearts]
                if suit = spades && lenS > 1 then rank <- hnQS
                elif suit <> hearts || lenH = 1 then ()
                elif lenH <= a.OppsHigherRanks[a.HighIn[hearts], hearts] then ()
                elif a.PtsSit = PointsSit.unplayed then rank <- a.SecondHighIn[hearts]
            | 15 ->
                suit <- d.SuitWithHU
                rank <- (at suit (a.HU[suit] - 1))
                if suit = spades && rank = queen then rank <- a.HighIn[spades]
            | 16 ->
                rank <- if lgtW > 0 then lgtW elif suitLed = spades && a.QSInHand && queen > w then queen else lsl
                suit <- suitLed
            | 17 ->
                let s = d.SuitOfBestNonpoint
                if s >= 0 then
                    suit <- s
                    rank <- a.LowIn[s]
                    if a.HU[s] >= a.OppsHave[s] then
                        rank <- if a.SuitLen[s] > a.OppsHave[s] then (at s a.OppsHave[s]) else a.LowIn[s]
                    elif a.SuitLen[s] > a.EstTr[s] then rank <- (at s a.EstTr[s])
                    if suit <> spades || rank <> queen then ()
                    elif pQS > 0 then rank <- (at spades (pQS - 1))
                    else rank <- (at spades (pQS + 1))
                elif a.QSInHand then
                    rank <- queen
                    suit <- spades
                    if d.Mode = Mode.ShootTheMoon && lenS > 1 then
                        rank <- a.LowIn[spades]
                        if rank = queen then rank <- (at spades (pQS - 1))
                elif lenS > 0 then
                    rank <- hnQS
                    suit <- spades
                else
                    rank <- a.HighIn[hearts]
                    suit <- hearts
            | 18 ->
                let mutable i = a.OffWinners[suitLed]
                if i = a.LenSuitLed then i <- i - 1
                if i = 0 && a.LenSuitLed > d.EstTrSl then i <- 1
                else i <- min i (d.EstTrSl + 1)
                rank <- (at suitLed (i))
                if rank > hltW && hltW > 0 then rank <- hltW
                suit <- suitLed
            | 20 ->
                rank <- a.HighIn[d.PowerSuit]
                suit <- d.PowerSuit
            | 21 ->
                rank <- if lgtW > 0 then (if suitLed = spades && lgtW > queen && not d.QSOwnerHasPlayed then lsl else lgtW) else hsl
                if suitLed = spades && (a.QSInHand || d.QSOwnerHasPlayed) then
                    if fnv = 0 && lgtW > w then rank <- lgtW
                    elif hnQS > w then rank <- hnQS
                    elif lsl < queen then rank <- lsl
                suit <- suitLed
            | 22 ->
                rank <- a.SecondHighIn[suitLed]
                if suitLed = spades && rank = queen then rank <- (if hltQS > 0 then hltQS else hnQS)
                suit <- suitLed
            | 23 ->
                rank <- d.RankToSluff
                suit <- d.SuitToSluff
            | 25 ->
                rank <- hnQS
                suit <- spades
                if hnQS = 0 then rank <- queen
                elif a.HU[spades] > 0 then
                    rank <- (at spades (a.HU[spades] - 1))
                    if rank = queen then rank <- hnQS
            | 27 ->
                suit <- suitLed
                rank <- (at suit (a.LenSuitLed / 2))
                if suit = spades && rank = queen then rank <- (if hltQS > 0 then hltQS else hnQS)
            | 28 ->
                rank <- hsl
                if not d.SluffPts then
                    rank <- if a.LenSuitLed >= 5 then a.SecondLowIn[suitLed] else lsl
                elif fnv = 0 then
                    if lgtW > 0 then rank <- lgtW
                elif a.HU[suitLed] > 0 then rank <- (at suitLed (a.HU[suitLed] - 1))
                if suitLed = spades && rank >= queen && hltQS > w then rank <- hltQS
                suit <- suitLed
            | 29 ->
                rank <- (at hearts a.HU[hearts])
                suit <- hearts
            | 30 ->
                suit <- suitLed
                rank <- lgtW
            | 31 ->
                rank <- hnQS
                suit <- spades
            | 32 ->
                suit <- a.WeakShootSuit
                rank <- (at suit a.HU[suit])
                if rank = a.HighIn[suit] && a.SuitLen[suit] > 1 then rank <- a.SecondHighIn[suit]
                if lenS = 1 || suit <> spades || rank <> queen then ()
                elif pQS < lenS - 1 then rank <- (at spades (pQS + 1))
            | 33 ->
                suit <- suitLed
                rank <-
                    if hltW = 0 then (if fnv = 0 then hsl else lgtW)
                    elif hltW < hsl then hltW
                    else (at suitLed (a.PHltW + 1))
            | 34 ->
                suit <- spades
                rank <- a.SecondHighIn[spades]
            | 35 ->
                suit <- spades
                rank <- a.SecondHighIn[spades]
                if rank = queen then rank <- (if hltQS > 0 then hltQS else hnQS)
            | _ -> failwith $"Situation {sit} belongs to modes and skill levels this port leaves out"

        // the first trick, when the two of clubs leads
        if d.SluffPts then ()
        elif not a.Sluffing then
            if d.Mode = Mode.Equivocate || d.Mode = Mode.ShootTheMoon then ()
            elif a.SuitLen[clubs] >= 5 then rank <- a.SecondHighIn[clubs]
            else rank <- a.HighIn[clubs]
        elif suit = diamonds || (suit = spades && rank <> queen) then ()
        elif not a.QSInHand && a.AKSInHand > 0 && lenS <= 3 + a.AKS then
            rank <- hnQS
            suit <- spades
        elif a.SuitLen[diamonds] > 0 then
            suit <- diamonds
            rank <- if a.SuitLen[diamonds] >= 5 then a.SecondHighIn[diamonds] else a.HighIn[diamonds]
        else
            rank <- hnQS
            suit <- spades
        { Rank = rank; Suit = suit }
