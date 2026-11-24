namespace Hearts.Heuristic.KillerEngine

/// Killer Hearts' scripts: for each mode, the situation (sit) that names how to choose a card, and the
/// card each situation chooses (select_best_card_for_situation).
module Scripts =

    /// The situation for a seat Shooting the Moon (best_script_for_Shooting_the_Moon). It may also
    /// change the suit of HU cards to lead.
    let private shooting (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        let lenH = a.SuitLen[hearts]
        let lenS = a.SuitLen[spades]
        let hu = a.HU
        let canLeadH = a.CanLeadH
        let wss = a.WeakShootSuit
        let z = wss >= 0 && a.OppsLowerRanks[a.SuitCards[wss][hu[wss]], wss] >= 2
        let helpers = d.ShootHelpers
        let mutable sit = 0
        if t.Seq = 0 then
            sit <- 15
            let swh = d.SuitWithHU
            if a.TotalOffWinners = a.TLeft then
                if hu[hearts] > 0 && (canLeadH || lenH = a.TLeft) then d.SuitWithHU <- hearts
                elif hu[spades] > 0 && t.PointsPlayed = 13 then d.SuitWithHU <- spades
                elif hu[clubs] > 0 then d.SuitWithHU <- clubs
                elif hu[diamonds] > 0 then d.SuitWithHU <- diamonds
                elif hu[spades] > 0 then d.SuitWithHU <- spades
                elif lenH = a.TLeft - 1 && a.QSInHand then sit <- 2
                else sit <- 20
            elif a.WinsAllH <= -11 && hu[hearts] > 0 && helpers >= 2 && canLeadH then d.SuitWithHU <- hearts
            elif a.WinsAllH = -11 && hu[hearts] < a.EstTr[hearts] - 1 && canLeadH then sit <- 29
            elif t.QSPlayed then
                if a.WinsAllH = 2 then d.SuitWithHU <- hearts
                elif swh >= 0 then ()
                else sit <- 20
            elif a.QSInHand then
                if a.WinsAllH >= 1 then
                    if a.WinsAllH = 2 && hu[spades] > a.PQS then sit <- 2
                    elif swh >= clubs then ()
                    elif hu[spades] > 0 && a.HighIn[spades] >= king then d.SuitWithHU <- spades
                    elif hu[spades] = 0 && a.OppsHave[spades] = 1 && canLeadH then d.SuitWithHU <- hearts
                    else sit <- 2
                elif swh >= clubs && helpers >= 2 then ()
                elif swh = spades && helpers >= 2 && (d.PullSpades || a.CaseSuit[spades])
                    && (hu[spades] > 1 || a.HighIn[spades] > queen || t.PointsPlayed >= 10) then ()
                elif a.PointsExposed >= 10 && swh >= 0 && a.Table.Trick >= 11 && a.EstTr[swh] - a.OffWinners[swh] <= 1 then
                    if swh = spades && a.HighIn[spades] = queen && hu[spades] = 1 then sit <- 20
                elif swh = hearts && canLeadH && helpers >= 2 then ()
                elif hu[hearts] > 0 && canLeadH && helpers >= 2 then d.SuitWithHU <- hearts
                elif hu[spades] = 0 && swh >= hearts then ()
                elif hu[spades] = 0 && a.TotalOffWinners + lenS = a.TLeft && lenS >= 3 then sit <- 25
                elif wss >= 0 && t.SuitPulls[wss] = 0 && (wss <> spades || lenS > 1) then sit <- 32
                elif swh >= clubs then ()
                elif hu[spades] > 1 then d.SuitWithHU <- spades
                elif hu[spades] = 1 && (t.PointsPlayed = 13 || a.HighIn[spades] <> queen) then d.SuitWithHU <- spades
                elif swh = hearts && canLeadH then ()
                elif swh = spades && d.ImScore[a.Me] >= 93 then ()
                elif lenS = 1 && lenH = 0 then sit <- 20
                elif a.SoloOwner[hearts] >= 0 && d.ShootHelper[a.SoloOwner[hearts]] then sit <- 2
                elif helpers >= 2 && a.SoloOwner[hearts] < 0 then sit <- 2
                elif d.ImScore[a.Me] >= 93 then sit <- 2
                elif t.PointsPlayed = 13 || a.WinsAllH = 1 then sit <- 2
                elif lenH > 0 && canLeadH then sit <- 6
                elif a.WinsAllH = -11 then sit <- 12
                elif d.PullSpades then sit <- 25
                else sit <- 20
            elif a.WinsAllH >= 1 && swh >= clubs then ()
            elif a.WinsAllH >= 1 && swh = hearts && canLeadH then ()
            elif a.WinsAllH >= 1 && hu[spades] > 0 then d.SuitWithHU <- spades
            elif a.WinsAllH >= 1 && a.HighIn[spades] = king then sit <- 31
            elif wss >= 0 && a.EstOppsVoidIn[wss] = 0 && (wss <> spades || d.PullSpades) then sit <- 32
            elif z && a.PointsExposed <= 7 then sit <- 32
            elif swh >= 0 then ()
            elif lenH > 0 && canLeadH then sit <- 6
            elif t.PointsPlayed = 13 && a.HnQS > queen then sit <- 31
            else sit <- 20
        elif a.LenSuitLed > 0 then
            let hsl, lsl, lgtW = d.Hsl, d.Lsl, d.LgtW
            sit <-
                if t.QSOnTrick then (if hsl > w && d.RelinquishQS then 3 else 4)
                elif hsl < w then 11
                elif t.Seq = 3 && t.PointsInTrick = 0 && a.AllSuitsCovered then 11
                elif a.QSInHand then
                    if suitLed <> spades then 28
                    elif queen > w && (t.PointsPlayed = 13 || a.WinsAllH >= 1) then 2
                    elif queen > w && t.PointsPlayed = 12 && a.SoloOwner[spades] = t.TrickLeader then 11
                    elif queen > w && hu[spades] <= a.PQS && a.TotalOffWinners = a.TLeft then 2
                    elif queen > w && a.WinsAllH = -11 && lenH >= a.EstTr[hearts] then
                        if a.FollowersNotVoid = 3 - t.Seq && t.PointsInTrick = 0 then 11
                        elif lgtW > 0 then 16
                        else 2
                    elif w > queen && d.RelinquishQS then 2
                    elif a.FollowersNotVoid = 0 && lgtW > 0 then 16
                    elif a.HnQS > w then 25
                    elif a.TotalOffWinners >= a.TLeft - 1 || a.WinsAllH = 2 then 4
                    else 11
                elif t.QSPlayed then 28
                elif suitLed = spades && lgtW >= king && not d.QSOwnerHasPlayed then
                    if a.PointsExposed = 13 || a.WinsAllH >= 1 || a.TotalOffWinners >= a.TLeft - 1 then 28
                    elif d.ShootHelpers >= 2 then 28
                    else 11
                elif t.Seq = 1 && hsl > w && a.OppsHave[suitLed] <= 3 then 4
                elif t.PointsInTrick > 0 then 28
                elif a.TotalOffWinners >= a.TLeft - 2 then 28
                elif a.OppsHave[suitLed] <= 2 && a.WinsAllH <= -11 && a.OppsHigherRanks[max 0 a.SecondHighIn[suitLed], suitLed] = 0 then 28
                elif a.AKSInHand >= a.EstTr[spades] then 28
                elif a.OffWinners[suitLed] = 0 && t.SuitPulls[suitLed] = 1 then 11
                elif t.Seq = 3 then 11
                else 28
        elif d.SluffPts && a.QSInHand && d.RelinquishQS then sit <- 2
        else sit <- 17
        sit

    /// The situation for a seat stopping a Shoot (best_script_for_stopping_a_Shoot).
    let private stopping (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        let shooter = a.Shooter
        let nonShooter = d.LikelyTWinnerIsNonShootingOpp
        if t.Seq = 0 then
            if a.QSInHand then
                if d.EatQS && a.PQS < a.HU[spades] then 2
                elif a.HighestOutcard[spades] < queen then 12
                elif a.OppWithAKS <> shooter && a.OppWithAKS >= 0 then 2
                elif not d.EatQS && not d.CanHoldQS then 2
                else 12
            elif not t.QSPlayed && d.EatQS && d.SuitWithHU >= 0 then 15
            else 12
        elif a.LenSuitLed > 0 then
            let hsl, lsl, lgtW = d.Hsl, d.Lsl, d.LgtW
            if suitLed = spades && not t.QSPlayed then
                let eat = d.EatQS
                if a.QSInHand then
                    if not eat && w < a.HnQS && t.TrickWinner = shooter && a.NumberOfOutcards > 0 then 31
                    elif eat && w < queen && t.TrickWinner = shooter then 2
                    elif eat && a.PQS < a.HU[spades] && w < queen && not nonShooter then 2
                    elif w < queen then 7
                    elif eat && not nonShooter then 7
                    elif not d.CanHoldQS || nonShooter then 2
                    else 7
                elif hsl > w && d.QSOwnerHasPlayed then 4
                // Killer Hearts' And here is bitwise, so A♠/K♠ in hand counts as true
                elif eat && a.AKSInHand > 0 && not d.QSOwnerHasPlayed && a.QSOwner >= 0 && a.QSOwner <> shooter then 4
                elif a.HltQS > w then 1
                else 22
            elif t.QSOnTrick then
                let pit = t.PointsInTrick
                if lgtW > 0 && not nonShooter
                    && ((suitLed <> hearts && t.PointsPlayed = 26 - pit)
                        || (suitLed = hearts && t.PointsPlayed >= 26 - pit - a.FollowersNotVoid)) then 5
                elif d.EatQS && lgtW > 0 && t.TrickWinner = shooter then 5
                elif d.EatQS && hsl > w && not nonShooter then 4
                else 33
            elif lsl > w then 4
            elif lgtW > 0 then (if t.TrickLeader <> shooter then 3 else 5)
            elif a.SuitStopper[suitLed] then 22
            elif a.LenSuitLed >= a.OppsHigherRanks[hsl, suitLed] then 22
            elif a.LenSuitLed >= d.EstTrSl then 22
            elif a.LenSuitLed > a.LU[suitLed] || not t.QSPlayed then 4
            else 22
        elif nonShooter then
            if a.QSInHand && d.OppTarget[d.LikelyTWinner] then 2
            elif a.QSInHand && not d.CanHoldQS && not d.EatQS then 2
            elif a.SuitLen[hearts] > 0 then 6
            else 23
        elif a.QSInHand then
            if d.OppTarget[d.LikelyTWinner] then 2
            elif d.SuitToSluff = spades && a.SuitLen[spades] >= 2 then 35
            elif not d.CanHoldQS && not d.EatQS then 2
            else 23
        elif a.AKSInHand = 0 then 23
        elif not d.EatQS then 31
        elif a.SuitLen[spades] >= 3 && d.SuitToSluff = spades then 34
        elif a.SuitLen[spades] = 2 && d.SuitToSluff = spades then 34
        else 23

    /// Following with Q♠ in hand when spades are led, in the modes that keep their points low.
    let private followSpadesWithQS (d : Decision) (a : Analysis) (lowMode : bool) =
        let t = a.Table
        let w = t.W
        let lgtW = d.LgtW
        if w > queen && (not d.CanHoldQS || d.OppTarget[d.LikelyTWinner] || (a.PtsSit = PointsSit.unplayed && a.SuitStopper[hearts])) then
            Some 2
        elif lowMode && not d.CanHoldQS && (if d.Mode = Mode.GetLowScorer then a.SuitLen[spades] = 3 && (a.PtsSit = PointsSit.split || a.SuitLen[spades] <= d.EstTrSl)
                                            else a.LenSuitLed = 3 && a.PtsSit = PointsSit.split) then
            if lgtW = 0 || (d.Mode = Mode.BeLowScorer && a.OppsHave[spades] = 0) then Some 11 else Some 16
        elif d.TakePoints && a.NumberOfOutcards > 1 && lgtW > 0 && (d.SpadesSafe || a.HnQS > lgtW) then
            if a.FollowersNotVoid > 0 && (a.HU[spades] >= 2 || d.SpadesSafe) then Some 25 else Some 16
        elif d.Mode = Mode.BeLowScorer && t.Seq = 3 && w < king && t.TrickWinner = t.TrickLeader && d.CanHoldQS
            && a.HltW > 0 && a.AKS > 0 then
            Some 3
        else None

    /// Following spades without Q♠ while it is out, in the modes that keep their points low.
    let private followSpadesWithoutQS (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let lenS = a.SuitLen[spades]
        if d.QSOwnerHasPlayed && a.AKSInHand > 0
            && (a.HnQS < w || not d.SpadesSafe || a.QSOwnerCertainty >= 11 || t.Seq = 3 || lenS - a.AKSInHand <= 1
                || (a.FollowersNotVoid = 0 && a.QSOwnerCertainty >= 8)) then Some 25
        elif d.SmartS > 0 then None
        elif w > queen then Some 3
        elif a.HltQS > w && d.TakePoints then Some 1
        else None

    /// The situation for a seat in a mode that keeps its points low: getting the low scorer, being the
    /// low scorer, or lying low.
    let private keepingLow (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        let mode = d.Mode
        let qs = a.QSInHand
        if t.Seq = 0 then
            if qs && a.HighestOutcard[spades] >= queen && (d.AKSOwnerIsTarget || not d.CanHoldQS) then 2
            else
                match mode with
                    | Mode.GetLowScorer ->
                        if d.SpecialLead >= 0 then 12
                        elif d.Priority > 0 then 12
                        elif t.QSPlayed || not d.PullSpades then 12
                        elif qs && a.HnQS > 0 && a.LU[spades] < 2 then 1
                        elif a.Opp.IsVoid(d.LowScorer, spades) then 12
                        elif a.AKS > 0 && d.LowScorerSitsLast then 12
                        elif d.OppTargetHasQS then 1
                        else 12
                    | Mode.BeLowScorer ->
                        if d.PullSpades && not qs then 1
                        elif d.Priority > 0 then 12
                        elif d.PullSpades && qs && a.HnQS > 0 && a.LU[spades] < 2 then 1
                        else 12
                    | _ ->
                        if d.SpecialLead >= 0 then 12
                        elif d.PullSpades && not qs && (a.AKS = 0 || not d.LowScorerSitsLast) then 1
                        elif d.Priority > 0 then 12
                        elif d.PullSpades && qs && a.HnQS > 0 && a.LU[spades] < 2 then 1
                        else 12
        elif a.LenSuitLed > 0 then
            let lgtW = d.LgtW
            let lts = d.LikelyTWinner
            if suitLed = spades && not t.QSPlayed then
                let choice =
                    if qs then followSpadesWithQS d a (mode <> Mode.LieLow)
                    else followSpadesWithoutQS d a
                defaultArg choice 13
            else
                let shooterTakes = lts = a.Shooter
                match mode with
                    | Mode.GetLowScorer ->
                        if d.ImScore[a.Me] >= 100 - 15 then 3
                        elif shooterTakes && a.SuitStopper[suitLed] && a.SecondHighIn[suitLed] < w then 22
                        elif t.QSOnTrick then 13
                        elif lts = d.QSPoison && lgtW > 0 && a.NumberOfOutcards > 0 then 5
                        elif d.TakePoints && d.QSOwnerHasPlayed && suitLed <> hearts then 4
                        elif not a.SuitIsLive[suitLed] then 3
                        else 13
                    | Mode.BeLowScorer ->
                        if shooterTakes && suitLed <> hearts && t.PointsInTrick = 1 && d.QSOwnerHasPlayed && d.Hsl > w then 5
                        elif shooterTakes && a.SuitStopper[suitLed] && a.SecondHighIn[suitLed] < w then 22
                        elif t.QSOnTrick then 13
                        elif lts = d.QSPoison && lgtW > 0 && a.NumberOfOutcards > 0 then 5
                        elif not d.QSOwnerHasPlayed || a.QSOwner = none then 13
                        elif d.TakePoints then 4
                        else 13
                    | _ ->
                        if shooterTakes && suitLed <> hearts && t.PointsInTrick = 1 && d.QSOwnerHasPlayed && d.Hsl > w then 5
                        elif shooterTakes && a.SuitStopper[suitLed] && a.SecondHighIn[suitLed] < w then 22
                        elif t.QSOnTrick then 13
                        elif lts = d.QSPoison && lgtW > 0 && a.NumberOfOutcards > 0 then 5
                        elif not d.QSOwnerHasPlayed || a.QSOwner = none then 13
                        elif d.TakePoints && suitLed <> hearts then 4
                        elif not a.SuitIsLive[suitLed] then 3
                        else 13
        elif not qs then 14
        elif d.OppTarget[d.LikelyTWinner] then 2
        elif mode = Mode.BeLowScorer then (if not d.CanHoldQS then 2 else 14)
        elif d.ImScore[d.LikelyTWinner] + t.PointsInTrick + 13 >= 100 then 14   // Q♠ would end the game
        elif not d.CanHoldQS then 2
        else 14

    /// The situation for a seat equivocating between Shooting and not (best_script_for_equivocating).
    let private equivocating (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        let lenS = a.SuitLen[spades]
        if t.Seq = 0 then
            if a.CanLeadH && a.SuitLen[hearts] > a.HU[hearts] && a.OppsHave[hearts] > a.HU[hearts] then 29
            elif a.PointsExposed >= 13 && d.SuitWithHU >= 0 then 15
            else 12
        elif a.LenSuitLed > 0 then
            let hsl, lgtW = d.Hsl, d.LgtW
            if a.QSInHand && w >= king && suitLed = spades && not d.CanHoldQS && (lenS = 2 || t.GameScore[t.TrickWinner] = d.LowScore) then 2
            elif t.QSOnTrick then 18
            elif hsl < w then
                if suitLed <> spades || d.SpadesSafe then 27
                elif not a.QSInHand then 4
                else 11
            elif t.PointsInTrick > 0 then
                if suitLed <> spades || a.QSInHand || d.QSOwnerHasPlayed then 21
                elif a.HltQS > w then 1
                else 18
            elif t.Seq = 3 then
                if suitLed = spades && a.QSInHand && a.HltQS < w then 11
                elif a.LenSuitLed > a.HU[suitLed] then 11
                elif a.LenSuitLed >= 5 then 3
                else 30
            elif suitLed = spades && not d.SpadesSafe then 11
            elif t.Seq = 1 && a.LenSuitLed > a.OppsHave[suitLed] && not a.QSInHand && not d.QSOwnerHasPlayed then 13
            elif t.SuitPulls[suitLed] = 1 then 27
            elif t.SuitPulls[suitLed] = 2 && t.PassDirection = hold then 27
            else 21
        elif d.SpadesSafe || lenS = 0 then 17
        elif not a.QSInHand then 25
        elif d.SluffPts && (d.ImScore[d.LikelyTWinner] + t.PointsInTrick + 13 < 100 || d.ImScore[a.Me] = d.LowScore) then 2
        else
            if d.SuitOfBestNonpoint <> spades then ()
            elif a.SuitLen[clubs] > 0 then d.SuitOfBestNonpoint <- clubs
            elif a.SuitLen[diamonds] > 0 then d.SuitOfBestNonpoint <- diamonds
            17

    /// The situation for a seat helping another seat Shoot, because the Shoot's success would help it or
    /// do it no harm (best_script_for_helping_Shooter).
    let private helping (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        let lts = d.LikelyTWinner
        if t.Seq = 0 then 12
        elif a.LenSuitLed > 0 then
            if t.TrickWinner = a.Shooter then
                if suitLed <> spades then 3
                elif a.QSInHand && w > queen then 2
                elif a.HltW > 0 then 3
                else 1
            elif d.ImScore[lts] = d.LowScore && a.OppShooterHasPlayed && (d.QSOwnerHasPlayed || a.QSInHand) then
                if suitLed <> spades then 4
                elif w > queen && a.QSInHand && d.Deficit <= 13 then 2
                elif d.Hsl <> queen then 4
                elif d.LgtW > 0 then 21
                else 13
            else 13
        elif lts <> a.Shooter then 14
        elif a.QSInHand then 2
        elif d.SuitToSluff = hearts then 6
        else 14

    /// The situation for a seat, neither the low scorer nor the high scorer, taking points it otherwise
    /// would not, so that the high scorer, close to going out, does not end the game
    /// (best_script_for_protecting_hi_scorer).
    let private protecting (d : Decision) (a : Analysis) =
        let t = a.Table
        let w = t.W
        let suitLed = t.SuitLed
        let lts = d.LikelyTWinner
        let qs = a.QSInHand
        let hiScorer = d.HiScorer
        if t.Seq = 0 then
            if qs && a.HighestOutcard[spades] >= queen && d.AKSOwnerIsTarget then 2
            elif qs && a.HighestOutcard[spades] >= queen && not d.CanHoldQS
                && ((a.OppWithAKS >= 0 && a.OppWithAKS <> hiScorer) || a.Opp.IsVoid(hiScorer, spades)) then 2
            elif d.Priority > 0 then 12
            elif t.QSPlayed || not d.PullSpades then 12
            elif qs && a.HnQS > 0 && a.LU[spades] < 2 then 1
            elif hiScorer = t.PNotLeadingS then 12
            elif d.OppTargetHasQS then 1
            else 12
        elif a.LenSuitLed > 0 then
            let lgtW = d.LgtW
            let ltsHigh = d.ImScore[lts] = d.HighScore
            let pit = t.PointsInTrick
            if ltsHigh && pit > 0 && lgtW > 0 && suitLed <> spades then 5   // take the points the high scorer would take
            elif ltsHigh && t.QSOnTrick && t.PlayerPlayedQS = lts && t.PointsPlayed = 0 && suitLed = spades
                && t.SuitPulls[spades] = 1 then
                13   // the high scorer may be Shooting
            elif ltsHigh && pit > 0 && lgtW > 0 && suitLed = spades
                && (lgtW <= jack || qs || d.QSOwnerHasPlayed || d.ImScore[lts] + pit >= 100) then
                5
            elif d.ImScore[lts] >= 100 && suitLed = spades && qs && queen > w then 5
            elif suitLed = spades && not t.QSPlayed then
                if qs then
                    if w > queen && not d.OppTarget[lts] && d.ImScore[lts] + 13 >= 100 then 11
                    elif w > queen && (d.OppTarget[lts] || (not d.CanHoldQS && d.ImScore[lts] <> d.HighScore)) then 2
                    elif d.TakePoints && a.NumberOfOutcards > 1 && lgtW > 0 && (d.SpadesSafe || a.HnQS > lgtW) then
                        if a.FollowersNotVoid > 0 && (a.HU[spades] >= 2 || d.SpadesSafe) then 25 else 16
                    else 13
                else defaultArg (followSpadesWithoutQS d a) 13
            elif suitLed = hearts && lts = a.Shooter && a.SuitStopper[hearts] && a.SecondHighIn[hearts] < w then 22
            elif t.QSOnTrick then 13
            elif lts = d.QSPoison && lgtW > 0 && a.NumberOfOutcards > 0 then 5
            elif d.TakePoints && d.QSOwnerHasPlayed && suitLed <> hearts then 4
            else 13
        elif not qs then 14
        elif d.OppTarget[lts] || (not d.CanHoldQS && d.ImScore[lts] <> d.HighScore && d.ImScore[lts] < 85) then 2
        else 14

    /// The situation for the seat's mode.
    let situation (d : Decision) =
        let a = d.Analysis
        match d.Mode with
            | Mode.ShootTheMoon -> shooting d a
            | Mode.StopShoot -> stopping d a
            | Mode.GetLowScorer | Mode.BeLowScorer | Mode.LieLow -> keepingLow d a
            | Mode.Equivocate -> equivocating d a
            | Mode.HelpShooter -> helping d a
            | Mode.ProtectHiScorer -> protecting d a
            | mode -> failwith $"Unexpected mode {mode}"
