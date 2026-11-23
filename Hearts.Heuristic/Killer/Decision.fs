namespace Hearts.Heuristic.Killer

open System

/// Killer Hearts' decision after its analysis: the seat's score estimates, its likely trick winner,
/// its mode of play, and the parameters for leading, following and sluffing that its scripts read.
type Decision(a : Analysis) =

    let table = a.Table
    let me = a.Me
    let position = table.Seq
    let trick = table.Trick
    let suitLed = table.SuitLed
    let w = table.W
    let tLeft = a.TLeft
    let opp1, opp2, opp3 = a.Opp1, a.Opp2, a.Opp3
    let opponents = [ opp1; opp2; opp3 ]
    let suitLen = a.SuitLen
    let suitCards = a.SuitCards
    let opp = a.Opp

    /// Killer Hearts' arrays indexed by suit answer 0 for "no suit" (-1).
    let at (arr : int[]) i = if i < 0 then 0 else arr[i]
    let len s = at suitLen s
    let highIn s = at a.HighIn s
    let secondHighIn s = at a.SecondHighIn s
    let thirdHighIn s = at a.ThirdHighIn s
    let hu s = at a.HU s
    let oppsHave s = at a.OppsHave s
    let soloOwner s = at a.SoloOwner s
    let estTr s = at a.EstTr s
    let offWinners s = at a.OffWinners s
    let lowIn s = a.LowIn[s]
    let secondLowIn s = a.SecondLowIn[s]
    let lu s = a.LU[s]
    let ohr r s = if r < 0 || s < 0 then 0 else a.OppsHigherRanks[r, s]
    let olr r s = a.OppsLowerRanks[r, s]
    let card s i = if s < 0 || i < 0 || i >= suitCards[s].Length then 0 else suitCards[s][i]
    let qsInHand = a.QSInHand
    let lenH = suitLen[hearts]
    let lenS = suitLen[spades]
    let tatt = [| 0; 100; 99; 93; 89; 85; 80; 75; 70; 65; 60; 55; 51 |]
    let clng (x : float) = int (Math.Round(x, MidpointRounding.ToEven))

    /// Game scores before the deal, by seat, and lowest first from index 1 (score, score_between_hands).
    let score = table.GameScore
    let sbh = table.ScoreBetweenHands

    /// Killer Hearts' score arrays answer 0 for "no seat" (-1).
    let scoreOf p = if p < 0 then 0 else score[p]

    // score estimates (set_im_scores)
    let imScore = Array.copy table.ImScore
    let imScoreOf p = if p < 0 then 0 else imScore[p]
    let imScoreRank = Array.zeroCreate<int> 5
    let mutable lowScore = table.LowScore
    let mutable highScore = table.HighScore
    let mutable leadOver2ndLow = table.LeadOver2ndLow
    let mutable attic = 0
    let mutable deficit = 0
    let mutable veryHighScore = false
    let mutable lowScorerSitsLast = false
    let mutable lowScorer = none
    let mutable secondPlace = none
    let mutable thirdPlace = none
    let mutable hiScorer = none

    let mutable likelyTWinner = opp1
    let mutable likelyTWinnerIsNonShootingOpp = false
    let mutable hsl = 0
    let mutable lsl = 0
    let mutable lgtW = 0

    // mode
    let oppTarget = Array.zeroCreate<bool> 4
    let mutable canHoldQS = false
    let mutable goShoot = false
    let mutable offType = none
    let mutable defType = Mode.NoMode
    let shootHelper = Array.zeroCreate<bool> 4
    let mutable shootHelpers = 0
    let mutable quitShoot = false
    let mutable relinquishQS = true
    let mutable modGorenCount = 0
    let mutable threshold = 0
    let mutable thresholdMinor = 0
    let mutable mode = Mode.NoMode
    let mutable opposeShoot = false
    let mutable lastH = false
    let mutable meTarget = false
    let mutable aksOwnerIsTarget = false
    let mutable oppTargetHasQS = false
    let mutable eatQS = false
    let mutable shootTry = table.ShootTry
    let mutable suitWithLU = a.SuitWithLU

    // leading
    let mutable spadesSafe = false
    let mutable pullSpades = false
    let mutable bestLeadRank = none
    let mutable bestLeadSuit = none
    let mutable priority = 0
    let mutable powerSuit = none
    let mutable suitOfLowestRisk = none
    let riskRank = Array.zeroCreate<int> 4
    let riskRankOfSuit = Array.create 5 0
    let mutable numberOfLeadableSuits = 0
    let mutable suitSmallest = none
    let mutable lenSS = 0
    let mutable suitNotAllLosers = none
    let mutable specialLead = none
    let mutable manoAMano = false

    // following and sluffing
    let mutable estTrSl = 0
    let mutable pLgtW = none
    let mutable equivalentLow = 1
    let mutable qsOwnerHasPlayed = false
    let mutable sStillSafe = false
    let mutable estMax = 0
    let mutable estMax0 = 0
    let mutable gentleCard = none
    let mutable expectedHOnTrick = 0
    let mutable takePoints = false
    let mutable smartCard = none
    let mutable smartS = none
    let mutable safeCard = none
    let mutable suitToSluff = none
    let mutable rankToSluff = none
    let mutable shootStopperSuit = none
    let mutable suitOfBestNonpoint = none
    let mutable suitOfOutcard = a.SuitOfOutcard
    let mutable suitWithHU = a.SuitWithHU
    let mutable qsPoison = none
    let oppReach = Array.create 4 none
    let mutable oppReachCount = 0
    let suitConduit = Array.create 4 none
    let mutable solutionDelivered = none
    let oppWinsAll = Array.zeroCreate<bool> 4
    let mutable saveBestLeadRank = none
    let mutable saveBestLeadSuit = none

    /// The seat's and opponents' score estimates: game scores plus points taken, and the trick's
    /// points for its likely winner (set_im_scores).
    let setImScores () =
        for p in allSeats do
            imScore[p] <- table.ScoreInGame[p] + (if position > 0 && p = likelyTWinner then table.PointsInTrick else 0)
        let sorted = Array.sort imScore
        for i in 1 .. 4 do imScoreRank[i] <- sorted[i - 1]
        lowScore <- imScoreRank[1]
        highScore <- imScoreRank[4]
        attic <- 100 - highScore
        deficit <- imScore[me] - lowScore
        leadOver2ndLow <- imScoreRank[2] - lowScore
        veryHighScore <- imScore[me] >= 90
        lowScorerSitsLast <- position = 0 && imScore[opp3] = lowScore
        lowScorer <- none
        secondPlace <- none
        thirdPlace <- none
        hiScorer <- none
        for p in [ opp1; opp2; opp3; me ] do
            if imScore[p] = imScoreRank[1] && lowScorer = none then lowScorer <- p
            elif imScore[p] = imScoreRank[2] && secondPlace = none then secondPlace <- p
            elif imScore[p] = imScoreRank[3] && thirdPlace = none then thirdPlace <- p
            else hiScorer <- p
        if position = 0 || not a.PossOppShoot then ()
        elif imScore[a.Shooter] >= 90 && table.TrickWinner = a.Shooter && (suitLed = hearts || table.PointsPlayed >= 7) then
            a.ProbOppShoot <- true

    /// Winners a solo owner of the suit must shed under the seat's cards (opp_winners).
    let oppWinners s =
        if s < 0 then none
        else
            let mutable result = 0
            let mutable k = 0
            let mutable p = hu s
            for i in hu s .. len s - 1 do
                let j = ohr (suitCards[s][i]) s
                if j > k then
                    p <- p - (j - k)
                    if p < 0 then result <- result - p
                    p <- 1
                else p <- p + 1
                k <- j
            result

    /// The opponents' highest card in the suit, and how many opponents may hold it.
    let oppOwnerOfHighCard s =
        let hcard =
            seq { 2 .. ace } |> Seq.filter (fun r -> not table.CardsPlayed[r, s] && not table.Hand[r, s])
                |> Seq.fold (fun _ r -> r) none
        let owners = opponents |> List.filter (fun p -> hcard >= 0 && opp[p, hcard, s] >= 0) |> List.length
        hcard, owners

    /// Shoot-related parameters: which opponents would help the seat Shoot, given the game score, and
    /// whether to try for, or give up on, a Shoot (compute_Shoot_specific_parameters).
    let computeShootSpecificParameters () =
        Array.fill shootHelper 0 4 false
        goShoot <- false
        quitShoot <- false
        relinquishQS <- true
        shootHelpers <- 0
        if a.PtsSit <> PointsSit.unplayed && a.PtsSit <> me then ()
        else
            // opponents for whom the seat's Shoot would do more good than harm
            for p in opponents do
                let helps =
                    if not table.QSPlayed then   // Q♠ is unplayed or on the trick
                        if score[p] = sbh[1] then
                            // Killer Hearts subtracts two equal scores here; the pass compares the two lowest
                            (if table.FixQuirks then sbh[2] - sbh[1] else sbh[1] - score[p]) >= 20
                                && score[me] - 26 >= sbh[1] + 10
                                && (score[me] < sbh[4] || score[me] < 100 - 25)
                        else score[me] - 26 >= sbh[1]
                    elif score[p] = sbh[1] then false
                    elif score[p] >= 100 - 10 then true   // p cannot afford to stop the Shoot
                    elif score[me] - 26 >= sbh[1] then true   // success makes no new low scorer
                    elif score[me] + 25 >= 100 then true   // p loses if it stops the Shoot
                    else score[me] >= 100 - 10   // p wants the seat's score down
                shootHelper[p] <- helps
                if helps then shootHelpers <- shootHelpers + 1

            if position = 0 then ()
            elif a.TotalOffWinners >= tLeft - 1 then relinquishQS <- false
            elif table.PointsPlayed = 13 then relinquishQS <- false
            elif a.WinsAllH >= 1 then relinquishQS <- false
            elif table.QSOnTrick then ()
            elif deficit >= 30 then relinquishQS <- false
            elif score[me] = lowScore && leadOver2ndLow >= 35 then relinquishQS <- false
            elif qsInHand && (canHoldQS || score[table.TrickWinner] > lowScore) then relinquishQS <- false

            let longSuit = a.LongSuit
            if longSuit = none then ()
            elif longSuit = hearts || hu longSuit = 0 || a.WinsAllH <= -12 then ()
            elif soloOwner longSuit = none || shootHelper[soloOwner longSuit] then goShoot <- true

            let helps p = p >= 0 && shootHelper[p]
            if not a.Sluffing then
                // 1] leading the twelfth trick with Q♠ and a loser
                if tLeft <> 2 then ()
                elif a.PtsSit <> me || position > 0 || a.QSIsHU = 0 || a.TotalOffWinners <> 1 then ()
                elif imScore[me] >= 96 then ()
                elif table.PointsPlayed = 13 then ()
                elif table.PointsPlayed = 12 && helps (soloOwner hearts) then ()
                else
                    let sw =
                        if a.WeakShootSuit >= 0 then a.WeakShootSuit
                        elif lenH > 0 then hearts
                        elif lenS > 1 then spades
                        elif suitLen[clubs] > 0 then clubs
                        else diamonds
                    let i = if sw = spades then lowIn spades else highIn sw
                    if sw = spades && oppsHave spades = 1 then ()
                    elif sw = spades && ohr i spades = 1 && helps (soloOwner spades) then ()
                    elif sw = spades && a.OppsVoidIn[spades] <= 1 then ()
                    elif soloOwner sw >= 0 then
                        if not shootHelper[soloOwner sw] then quitShoot <- true
                    else
                        let hcard, _ = oppOwnerOfHighCard sw
                        let helpingOwners =
                            opponents |> List.filter (fun p -> hcard >= 0 && opp[p, hcard, sw] >= 0 && shootHelper[p]) |> List.length
                        if helpingOwners = 0 then quitShoot <- true

                // 2] an unfriendly opponent holds the rest of a suit in which the Shooter has a loser
                let wss = a.WeakShootSuit
                if quitShoot || wss = none then ()
                elif table.QSPlayed || soloOwner wss = none then ()
                elif len wss > oppsHave wss then ()
                elif not shootHelper[soloOwner wss] then quitShoot <- true

                // 3] the opponent with only hearts left owns a suit with a stopper
                if quitShoot || a.OnlyHasH = none then ()
                elif position > 0 || wss = none then ()
                elif soloOwner wss = none then ()
                elif a.TotalOffWinners < oppWinners wss then quitShoot <- true

                // 4] the Shooter has little power left
                if quitShoot || position > 0 || a.TotalOffWinners > 1 || tLeft = 2 || imScore[me] >= 100
                    || table.PointsPlayed < 6 then ()
                elif table.QSPlayed || qsInHand then ()
                elif a.ALiveSuit <> hearts && len a.ALiveSuit = tLeft then ()
                elif table.PointsPlayed = 13 && a.SuitWithHU = spades then ()
                elif table.PointsPlayed = 12 && lenH = 0 && helps (soloOwner hearts) then ()
                elif lenH = tLeft && shootHelpers >= 2 then ()
                else quitShoot <- true

                // 5] Q♠ is on the trick, or is the seat's only card that can win a trick with a heart on it
                if quitShoot || position = 0 then ()
                elif table.PointsPlayed > 6 || table.PointsInTrick = 0 || shootHelpers >= 2 then ()
                elif table.QSOnTrick && not a.Sluffing && lsl < w then quitShoot <- true
                elif suitLed = spades && qsInHand && queen > w && a.HnQS < w then quitShoot <- true

    /// The strength of the seat's hand for Shooting (compute_modified_Goren_count).
    let computeModifiedGorenCount () =
        for s in allSuits do
            let j = suitLen[s]
            if j > 0 then
                let mutable headsSuit = false
                let mutable examined = 0
                let mutable points = 4
                let mutable r = ace
                let mutable fin = false
                while not fin && r >= 2 do
                    if not table.CardsPlayed[r, s] then
                        if table.Hand[r, s] then
                            modGorenCount <- modGorenCount + points
                            headsSuit <- true
                            examined <- examined + 1
                        points <- points - 1
                        if points = 0 || examined = j then fin <- true
                    r <- r - 1
                let i = oppsHave s + j
                let k = 13 * j
                if not headsSuit then ()
                elif k >= 7 * i then modGorenCount <- modGorenCount + 3
                elif k >= 6 * i then modGorenCount <- modGorenCount + 2
                elif k >= 5 * i then modGorenCount <- modGorenCount + 1
            if trick <= 4 && j <= 2 then modGorenCount <- modGorenCount + 3 - j
        if not qsInHand || not spadesSafe then ()
        elif trick <= 4 then modGorenCount <- modGorenCount + 1
        elif trick <= 9 then modGorenCount <- modGorenCount + 2
        else modGorenCount <- modGorenCount + 3
        if a.WinsAllH = -1 then modGorenCount <- modGorenCount + 3 * hu hearts

    /// Chooses the mode of play (best_determine_mode).
    let determineMode () =
        let numberOfOutcards = a.NumberOfOutcards
        let ptsSit = a.PtsSit
        let pointsExposed = a.PointsExposed
        canHoldQS <-
            if not qsInHand || not spadesSafe then false
            elif a.CDHEntries = 0 then true
            elif numberOfOutcards >= a.CDHEntries then true
            elif numberOfOutcards >= 2 then true
            elif numberOfOutcards = 1 && oppsHave suitOfOutcard >= 2 then true
            elif position > 0 && imScore[me] > lowScore && imScore[likelyTWinner] + table.PointsInTrick + 13 >= 100 then true
            elif a.LiveSuits > 1 || position = 0 || (a.ALiveSuit >= 0 && likelyTWinner = soloOwner a.ALiveSuit) then false
            elif imScore[me] = lowScore then false
            elif deficit <= 13 && highScore <= 100 - 25 then false
            elif trick >= 10 then false
            else a.EstOppsVoidIn[a.ALiveSuit] >= 2

        computeShootSpecificParameters ()
        if position > 0 && likelyTWinner <> me then
            oppTarget[likelyTWinner] <- imScore[likelyTWinner] <= lowScore + 5

        modGorenCount <- 0
        computeModifiedGorenCount ()

        // the Goren count needed to Shoot: lower when opponents would help, or the seat needs a Shoot
        threshold <- 10000
        if ptsSit = PointsSit.unplayed || ptsSit = me then
            threshold <-
                if shootHelpers <= 1 then 25
                elif shootHelpers = 2 then 22
                else 19
            if veryHighScore && score[me] - lowScore >= 20 then threshold <- threshold - 3
            elif highScore >= 100 - 16 && score[me] - lowScore >= 26 then threshold <- threshold - 1
            threshold <- clng (float (threshold * tatt[trick]) / 100.0)
        let lowLoserH = if a.WinsAllH < -10 then card hearts (hu hearts - 11 - a.WinsAllH) else 0
        thresholdMinor <- threshold * 90 / 100

        // whether the hand is strong enough to Shoot
        let winsAllH = a.WinsAllH
        let sluffing = a.Sluffing
        let helps p = p >= 0 && shootHelper[p]
        let strongHand =
            if ptsSit <> PointsSit.unplayed && ptsSit <> me then false
            elif (sluffing || hsl < w) && table.PointsInTrick > 0 then false
            elif (sluffing || hsl < w) && a.CaseCards + a.TotalLU + a.TotalDeadCards = tLeft then false
            elif (sluffing || hu hearts = 0) && lenH = tLeft then false
            elif sluffing && qsInHand && lenH = tLeft - 1 then false
            elif winsAllH < 0 && a.OnlyHasH >= 0 && lu hearts > 0 then false
            elif winsAllH >= 1 then true
            elif table.PointsPlayed = 13 && not table.QSPlayed then true
            elif veryHighScore && modGorenCount >= threshold && shootHelpers >= 2 && lowIn hearts >= 6 then true
            elif veryHighScore && modGorenCount >= threshold && shootHelpers >= 1 && lenH = 0 then true
            elif a.TotalOffWinners >= tLeft - 1 then true
            elif quitShoot then false
            elif winsAllH < 0 && soloOwner hearts >= 0 && not shootHelper[soloOwner hearts] then false
            elif ohr (highIn hearts) hearts >= 6 then false
            elif ohr (highIn hearts) hearts >= 2 && a.TotalOffWinners < (7 - shootHelpers) / 3 then false
            elif ohr (secondHighIn hearts) hearts >= 1 + hu hearts && shootHelpers < 2 then false
            elif ohr (thirdHighIn hearts) hearts >= 4 + hu hearts then false
            elif qsInHand && position = 0 && trick >= 11 && a.Entries = 0 && oppsHave hearts >= 4 && a.WeakShootSuit >= 0 then false
            elif winsAllH = -12 && a.OppOwnsQS && (card hearts (hu hearts) < jack || lowLoserH < 6) then false
            elif lenH > 0 && lowIn hearts <= 7 && oppsHave hearts >= 2 * lenH then false
            elif lenH <= 3 && lenH > 0 && oppsHave hearts >= 8 && lowIn hearts <= 4 && shootHelpers < 2 then false
            elif sluffing && lenH <= 3 && lenH > 0 && oppsHave hearts >= 8 && lowIn hearts <= 8 && shootHelpers < 2 then false
            elif winsAllH <= -13 && (a.OppHWinners > hu hearts || shootHelpers < 2)
                && (lowLoserH < 7 || score[me] < 100 - 25 || shootHelpers < 2) then false
            elif winsAllH <= -12 && not qsInHand && not veryHighScore && a.DefLosers[hearts] >= 2
                && (hu hearts = 0 || shootHelpers < 2) then false
            elif winsAllH <= -12 && table.QSOnTrick && a.HltW > 0 && (score[me] < 100 - 10 || table.PointsPlayed < 10) then false
            elif goShoot then true
            elif table.PointsPlayed >= 11 && not table.QSPlayed then true
            elif modGorenCount >= threshold then true
            elif modGorenCount >= thresholdMinor && a.NumberOfVoids + a.NumberOfSingletons >= 2 then true
            elif modGorenCount >= thresholdMinor && qsInHand && shootHelpers >= 2 then true
            elif lenH > 0 && ohr (secondHighIn hearts) hearts < 1 + hu hearts && shootHelpers >= 2 then true
            elif winsAllH < -10 && lenH >= estTr hearts && oppWinners hearts = lenH then true
            elif winsAllH < -10 && lenH >= estTr hearts && lu hearts = 0 && table.PointsPlayed >= 13 - 2 * lenH then true
            elif ohr (highIn a.WeakShootSuit) a.WeakShootSuit <= 1 && a.TotalUnbeatables + len a.WeakShootSuit = tLeft then true
            elif winsAllH = 0 && float a.TotalWinR < 0.8 then false
            else true

        // the offensive mode type: how strongly the hand can Shoot
        let totalWinR = float a.TotalWinR
        let tow = a.TotalOffWinners
        let scoreSoloOwnerH = if soloOwner hearts >= 0 then score[soloOwner hearts] else none
        let p = if lenS > 0 then ohr (lowIn spades) spades else 20
        let off =
            if not strongHand || table.NoShoot then none
            elif imScore[me] >= 100 then 10
            elif tow = tLeft then 10
            elif tow = tLeft - 1 then
                if lenH > 0 && hu hearts = 0 && table.QSOnTrick && a.HltW > 0 then none else 9
            elif winsAllH >= 1 then 9
            elif winsAllH = -11 && lenH >= estTr hearts && (table.QSPlayed || qsInHand || a.AKSInHand > 0) then
                if hu hearts > 0 then 8
                elif score[me] - 26 < sbh[1] && score[me] < sbh[4] && sbh[4] + 26 >= 100 then none   // the others won't let a Shoot win the game
                elif score[me] - 26 < sbh[1] && scoreSoloOwnerH >= 0 && scoreSoloOwnerH <= sbh[1] + 5 then none   // the Shooter would become the low scorer
                else 8
            elif table.PointsPlayed = 13 && not table.QSPlayed then 8
            elif totalWinR >= 0.8 then
                if a.WeakShootSuit = none || oppWinners a.WeakShootSuit > 0 then 7
                elif winsAllH >= -1 && tow < tLeft - 1 then 2
                elif tow < tLeft + (winsAllH + 10) - 1 then 2
                else 7
            elif winsAllH <= -1 && lenH >= estTr hearts && tow = offWinners hearts then
                let t = if hu hearts > 0 && oppsHave hearts <= 2 && shootHelpers >= 2 then 3 else 2
                if hu hearts >= 2 && table.QSPlayed then 3 else t
            elif winsAllH >= -11 && trick >= 8 && totalWinR >= 0.5 then 7
            elif winsAllH >= -11 && trick <= 7 && tow >= tLeft - 3 && not table.QSPlayed then
                if shootHelpers = 0 then 2 else 6
            elif winsAllH >= -11 && trick <= 7 && tow >= tLeft - 1 then 6
            elif winsAllH >= -1 && a.OppOwnsQS && p = 1 && tow - offWinners spades + lenS = tLeft then 5
            elif qsInHand && totalWinR >= 0.8 && deficit > 26 then 4
            elif tow = 1 && a.QSIsHU = 1 && a.SuitsInHand = 2 && oppsHave a.SuitWithoutHU = 1 && shootHelpers >= 2 then 4
            elif winsAllH < -10 && table.PointsPlayed >= 13 - 2 * lenH then 2
            elif trick >= 8 && tow - a.QSIsHU >= 1 then 2
            elif trick <= 7 && tow - a.QSIsHU >= 2 then 1
            elif not a.OppOwnsQS && a.LongSuit >= 0 && a.LongSuit <> hearts then 1
            elif shootHelpers >= 2 then 1
            else none

        // a point-free lead that shows a Shoot
        let mutable ptFreeShootingLead = none
        if ptsSit <> PointsSit.split && position > 0 then
            if a.Shooter = none && table.PointsInTrick > 0 && ptsSit = PointsSit.unplayed && likelyTWinner <> me then
                a.Shooter <- likelyTWinner
            let leader = table.TrickLeader
            if likelyTWinner <> leader || table.PointsPlayed > 0 then ()
            elif trick <= 7 && table.VoidInSuit[suitLed] + a.PrivateVoidInSuit[suitLed] = 3 then ptFreeShootingLead <- leader
            elif trick <= 7 && a.OppsVoidIn[suitLed] = 2 && a.LenSuitLed = 0 then
                if suitLed <> spades || a.QSOwner = leader then ptFreeShootingLead <- leader
            elif a.QSOwner <> leader then
                if (trick <= 7 || (table.HasSluffed[leader] && not table.HasSluffedAKS[leader])) && not a.SuitStopper[hearts]
                    && suitLed = spades && w >= king && (qsInHand || a.QSOwner >= 0) then
                    ptFreeShootingLead <- leader
            elif suitLed <> spades && a.FollowersNotVoid = 0 && table.Sluffs = position - 1 then
                if (trick <= 7 || table.HasSluffed[leader]) && a.OppsVoidIn[suitLed] = 2 && a.LenSuitLed = lu suitLed then
                    ptFreeShootingLead <- leader
        if a.Shooter >= 0 then
            a.PossOppShoot <- true
            if ptFreeShootingLead >= 0 then a.ProbOppShoot <- true
        elif ptFreeShootingLead >= 0 then
            a.Shooter <- ptFreeShootingLead
            a.PossOppShoot <- true
            a.ProbOppShoot <- true

        // whether the suspected Shooter's Shoot would win the game
        if a.PossOppShoot then a.ComputeOppShootWinsGame()
        else a.OppShootWinsGame <- false
        let oppShootWinsGame = a.OppShootWinsGame
        let shooter = a.Shooter

        // whether to oppose a Shoot
        if shooter = none || veryHighScore then ()
        elif oppShootWinsGame then opposeShoot <- true
        elif deficit = 0 then   // the seat is the low scorer
            if leadOver2ndLow >= 30 - table.PointsPlayed then opposeShoot <- true
            elif scoreOf shooter - 26 < score[me] then opposeShoot <- true
            elif highScore >= 100 && leadOver2ndLow > 26 - pointsExposed && (position = 0 || hsl < w) then opposeShoot <- true
            elif imScoreOf shooter + 24 - pointsExposed >= 100 && 2 * leadOver2ndLow > 26 - pointsExposed
                && (position = 0 || hsl < w) then
                opposeShoot <- true
            elif numberOfOutcards = 0 then ()
            elif leadOver2ndLow <= 26 - pointsExposed && highScore >= 100 - 13 then ()
            elif imScoreOf shooter < highScore || highScore - imScoreRank[3] < 20 then ()
            elif table.QSPlayed then opposeShoot <- true
            elif qsInHand && spadesSafe then opposeShoot <- true
            elif a.OppOwnsQS && qsOwnerHasPlayed && a.QSOwnerCertainty >= 10 then opposeShoot <- true
        elif scoreOf shooter + 25 >= 100 - 15 then ()   // the game is over, or nearly, if the Shoot fails
        elif scoreOf shooter - 26 > score[me] then ()
        elif shooter = a.QSOwner && numberOfOutcards = 0 then ()
        elif sbh[1] > scoreOf shooter - 26 then opposeShoot <- true
        elif (table.QSPlayed || shooter <> a.QSOwner) && scoreOf shooter - 26 < score[me] then opposeShoot <- true
        elif deficit >= 35 then ()
        elif canHoldQS then opposeShoot <- true
        elif scoreOf shooter - 26 <= score[me] - 10 then opposeShoot <- true

        lastH <- shooter >= 0 && lenH = 1 && a.CaseSuit[hearts]

        // the defensive mode type
        let probOppShoot = a.ProbOppShoot
        let z = probOppShoot && scoreOf shooter + 25 >= 100 - 8   // the game is over, or nearly, if the Shoot fails
        let mutable def =
            if imScore[me] = highScore && not opposeShoot then Mode.GetLowScorer
            elif table.ScoreInGame[me] + table.PointsInTrick >= 100 then Mode.GetLowScorer
            elif table.ScoreInGame[me] + table.PointsInTrick + 1 >= 100 && position > 0 && lgtW > 0 && suitLed = hearts then
                Mode.GetLowScorer
            elif oppShootWinsGame && probOppShoot then Mode.StopShoot   // everybody fights!
            elif imScoreOf shooter >= 100 && imScore[me] > lowScore && not oppShootWinsGame then Mode.HelpShooter   // or the game is over
            elif highScore >= 100 && a.PtsSit = PointsSit.split then
                if deficit = 0 then Mode.BeLowScorer else Mode.ProtectHiScorer   // or the game is over
            elif lowScore >= 100 - 15 then Mode.GetLowScorer
            elif imScore[me] >= 100 - 10 then   // everyone's score is very high
                if deficit = 0 then Mode.BeLowScorer else Mode.LieLow
            elif veryHighScore || (qsInHand && imScore[me] + 13 >= 100 && numberOfOutcards <= 1) then
                if deficit = 0 then Mode.BeLowScorer   // the seat can still be the low scorer
                elif z then Mode.HelpShooter
                elif qsInHand && a.Entries = 0 then Mode.LieLow
                elif highScore < 100 - 10 then Mode.LieLow
                elif highScore - table.PointsInTrick < 100 then Mode.ProtectHiScorer
                else Mode.LieLow
            elif probOppShoot then
                if deficit = 0 then
                    if opposeShoot then Mode.StopShoot else Mode.BeLowScorer
                elif z then Mode.HelpShooter
                elif scoreOf shooter - 26 > lowScore then Mode.HelpShooter
                elif shooter = a.QSOwner && numberOfOutcards = 0 then Mode.LieLow
                else Mode.StopShoot   // the Shooter would be the new low scorer
            elif deficit = 0 then
                if a.PossOppShoot && pointsExposed >= 9 && leadOver2ndLow >= 26 - pointsExposed then Mode.StopShoot
                else Mode.BeLowScorer
            elif deficit < 10 && highScore < 100 - 20 then Mode.LieLow
            elif highScore + 26 - pointsExposed < 100 then Mode.GetLowScorer
            elif deficit <= 3 && trick <= 6 then Mode.GetLowScorer
            elif highScore < 100 - 10 then Mode.GetLowScorer
            elif position = 0 then Mode.ProtectHiScorer
            elif imScore[me] < 100 - 17 && table.PointsInTrick < 13 then Mode.ProtectHiScorer
            elif highScore >= 100 && highScore - table.PointsInTrick < 100 then
                if a.PtsSit = PointsSit.unplayed && imScore[table.TrickLeader] < highScore then Mode.ProtectHiScorer
                else Mode.HelpShooter
            else Mode.GetLowScorer
        if def = Mode.LieLow then
            if not qsInHand then ()
            elif not spadesSafe then ()
            elif a.TotalDefLosers < 3 && a.Entries > 0 then ()
            elif leadOver2ndLow >= 15 || highScore >= 100 - 15 || a.Entries = 0 then def <- Mode.GetLowScorer

        // targets
        meTarget <-
            if deficit <= 12 then true
            elif a.QSOwner = none then false
            else imScore[a.QSOwner] = lowScore && highScore - imScore[me] <= 10
        match def with
            | Mode.StopShoot ->
                opposeShoot <- true
                a.ProbOppShoot <- true
                for p in opponents do
                    if p = shooter then ()
                    elif score[me] = lowScore then
                        if not qsInHand || not spadesSafe then oppTarget[p] <- true
                        elif score[p] <= sbh[2] + 4 then oppTarget[p] <- true
                        elif score[p] = sbh[4] then oppTarget[p] <- true
                    elif qsInHand && spadesSafe then
                        if score[p] <= lowScore + 4 then oppTarget[p] <- true
                    elif score[p] < 100 - table.PointsUnplayed then oppTarget[p] <- true
                eatQS <-
                    if table.QSPlayed && not table.QSOnTrick then false
                    elif table.PointsPlayed < 10 && tLeft > 3 then false   // eat Q♠ only when urgent
                    elif score[me] + 13 >= 100 then false   // eating Q♠ loses the game
                    elif scoreOf shooter - 26 >= sbh[1] then false   // no new leader
                    elif sbh[4] <= 25 then false
                    elif score[me] = sbh[1] && leadOver2ndLow >= 14 then true
                    elif oppShootWinsGame then true   // save the game
                    else sbh[1] > scoreOf shooter - 13   // resist a new leader
            | Mode.GetLowScorer ->
                if a.Entries <= numberOfOutcards then Array.fill oppTarget 0 4 false
                let i =
                    if tLeft <= 2 && imScore[likelyTWinner] <= imScoreRank[2] then 10
                    elif deficit <= 10 && imScore[me] <= 50 then 4
                    elif highScore <= 50 then 4
                    elif a.Entries <= numberOfOutcards then 0
                    elif imScore[me] >= imScoreRank[3] then 5
                    else 0
                for p in opponents do
                    if imScore[p] <= lowScore + i then oppTarget[p] <- true
                if tLeft >= 7 then ()
                elif leadOver2ndLow >= 26 || highScore >= 100 - 15 then ()
                elif a.Entries <= numberOfOutcards then ()
                elif tLeft >= 5 && imScore[me] <> imScoreRank[2] && imScoreRank[2] < 100 - 26 + pointsExposed then
                    oppTarget[secondPlace] <- true
                elif imScore[me] >= imScoreRank[3] && imScoreRank[3] >= 75 then oppTarget[secondPlace] <- true
                else
                    for p in opponents do
                        if imScore[p] <= lowScore + 10 && imScore[p] < 100 - 26 + pointsExposed then oppTarget[p] <- true
            | Mode.BeLowScorer ->
                for p in opponents do
                    if imScore[p] >= highScore - 5 then oppTarget[p] <- true
                    elif imScore[p] <= imScore[me] + 15 then oppTarget[p] <- true
                    elif imScore[p] >= 100 - 13 then oppTarget[p] <- true
            | Mode.LieLow ->
                for p in opponents do
                    if imScore[p] < 100 - 26 + pointsExposed then oppTarget[p] <- true
            | Mode.HelpShooter -> opposeShoot <- false
            | Mode.ProtectHiScorer ->
                if a.Entries <= numberOfOutcards then Array.fill oppTarget 0 4 false
                meTarget <- false
                for p in opponents do
                    if imScore[p] < highScore && imScore[p] + 18 < 100 then oppTarget[p] <- true
                opposeShoot <- false
            | _ -> ()
        let mutable numTargets = opponents |> List.filter (fun p -> oppTarget[p]) |> List.length
        if numTargets = 0 then
            oppTarget[opp1] <- true   // arbitrary
            numTargets <- 1
        aksOwnerIsTarget <-
            if a.OppWithAKS >= 0 then oppTarget[a.OppWithAKS]
            else a.AKS > 0 && numTargets = 3
        oppTargetHasQS <-
            if a.QSOwner >= 0 then oppTarget[a.QSOwner]
            else a.OppOwnsQS && numTargets = 3

        let mutable p = none
        if pullSpades && a.OppOwnsQS && a.AKSInHand + estTr spades <= lenS then
            p <- ohr (card spades (a.AKSInHand + estTr spades - 1)) spades

        mode <-
            if ptsSit = me && imScore[me] >= 100 && not table.NoShoot then Mode.ShootTheMoon   // no choice but to continue
            elif off = none && shootHelpers <= 1 then def
            elif ptsSit = me && pointsExposed >= 14 && tLeft - tow <= 2 && a.WeakShootSuit >= 0 && not table.NoShoot
                && (soloOwner a.WeakShootSuit = none || helps (soloOwner a.WeakShootSuit)) then
                Mode.ShootTheMoon
            elif off >= 3 then Mode.ShootTheMoon
            elif off = none then def
            elif imScore[me] = lowScore && leadOver2ndLow >= attic / 6 then Mode.BeLowScorer
            elif position > 0 then Mode.Equivocate
            elif oppTargetHasQS && p = 1 then def
            elif a.HighestOutcard[spades] = queen && aksOwnerIsTarget then def
            else Mode.Equivocate
        if mode <> Mode.Equivocate then ()
        elif trick >= 11 || pointsExposed >= 17 then mode <- Mode.ShootTheMoon
        elif shootHelpers >= 2 && pointsExposed >= 13 then mode <- Mode.ShootTheMoon
        elif deficit >= 50 && 100 - imScore[me] <= 15 then mode <- Mode.ShootTheMoon   // desperation!

        offType <- off
        defType <- def
        if mode = Mode.ShootTheMoon then shootTry <- me
        elif shootTry = none then ()
        elif table.PointsPlayed = 0 then shootTry <- none

    member _.Analysis = a

    /// The seat's main choices, in the same form as Killer Hearts' KhEngine.Brief, for checking the port.
    member _.Brief(sit : int) =
        let seats (xs : 'T[]) = String.Join(",", xs)
        let b (x : bool) = if x then "True" else "False"
        String.Join(" ",
            [|
                $"mode={int mode} def={int defType} off={offType} sit={sit}"
                $"Shooter={a.Shooter} poss={b a.PossOppShoot} prob={b a.ProbOppShoot} oppose={b opposeShoot} wins={b a.OppShootWinsGame} helpers={shootHelpers}"
                $"im={seats imScore} low={lowScore} high={highScore} deficit={deficit} ltw={likelyTWinner} tgt={seats (Array.map b oppTarget)}"
                $"hold={b canHoldQS} eat={b eatQS} take={b takePoints} safe={safeCard} sluff={suitToSluff}/{rankToSluff} lead={bestLeadSuit}/{bestLeadRank}"
                $"thr={threshold} goren={modGorenCount} go={b goShoot} quit={b quitShoot} rel={b relinquishQS}"
            |])
    member _.ImScore = imScore
    member _.LowScore = lowScore
    member _.HighScore = highScore
    member _.LeadOver2ndLow = leadOver2ndLow
    member _.Deficit = deficit
    member _.LowScorer = lowScorer
    member _.HiScorer = hiScorer
    member _.ShootHelper = shootHelper
    member _.ShootHelpers = shootHelpers
    member _.LikelyTWinner = likelyTWinner
    member _.OppTarget = oppTarget
    member _.CanHoldQS = canHoldQS
    member _.GoShoot = goShoot
    member _.QuitShoot = quitShoot
    member _.RelinquishQS = relinquishQS
    member _.ModGorenCount = modGorenCount
    member _.Threshold = threshold
    member _.Mode = mode
    member _.OpposeShoot = opposeShoot
    member _.LastH = lastH
    member _.MeTarget = meTarget
    member _.AKSOwnerIsTarget = aksOwnerIsTarget
    member _.OppTargetHasQS = oppTargetHasQS
    member _.EatQS = eatQS
    member _.ShootTry = shootTry
    member _.SpadesSafe = spadesSafe
    member _.PullSpades = pullSpades
    member _.BestLeadRank = bestLeadRank
    member _.BestLeadSuit = bestLeadSuit
    member _.Priority = priority
    member _.PowerSuit = powerSuit
    member _.SuitOfLowestRisk = suitOfLowestRisk
    member _.RiskRankOfSuit = riskRankOfSuit
    member _.NumberOfLeadableSuits = numberOfLeadableSuits
    member _.SuitSmallest = suitSmallest
    member _.LenSS = lenSS
    member _.SuitNotAllLosers = suitNotAllLosers
    member _.SpecialLead = specialLead
    member _.ManoAMano = manoAMano
    member _.SuitWithLU = suitWithLU
    member _.Hsl = hsl
    member _.Lsl = lsl
    member _.LgtW = lgtW
    member _.EstTrSl = estTrSl
    member _.QSOwnerHasPlayed = qsOwnerHasPlayed
    member _.SStillSafe = sStillSafe
    member _.GentleCard = gentleCard
    member _.TakePoints = takePoints
    member _.SmartCard = smartCard
    member _.SmartS = smartS
    member _.SafeCard = safeCard
    member _.LikelyTWinnerIsNonShootingOpp = likelyTWinnerIsNonShootingOpp

    member _.SuitToSluff = suitToSluff
    member _.RankToSluff = rankToSluff
    member _.ShootStopperSuit = shootStopperSuit
    member _.SuitOfBestNonpoint with get () = suitOfBestNonpoint and set v = suitOfBestNonpoint <- v
    member _.SuitWithHU with get () = suitWithHU and set v = suitWithHU <- v
    member _.QSPoison = qsPoison
    member _.LowScorerSitsLast = lowScorerSitsLast
    member _.SluffPts = a.SluffPts
    member _.VeryHighScore = veryHighScore

    /// Which seat will probably win the trick (best_determine_likely_T_winner).
    member private this.DetermineLikelyTWinner() =
        let fnv = a.FollowersNotVoid
        let tw = table.TrickWinner
        let shooter = a.Shooter
        let prob = a.ProbOppShoot
        if position < 3 then
            let flag = a.PossOppShoot && opp.IsVoid(shooter, suitLed)
            let i = a.HigherRanks(w, suitLed)
            likelyTWinner <-
                if oppsHave suitLed = 1 && fnv = 1 && i > 0 && soloOwner suitLed >= 0 && (not prob || tw <> shooter) then
                    soloOwner suitLed
                elif lsl > w then me
                elif lsl = 0 && fnv = 0 then tw
                elif i = 0 then tw
                elif i = 1 && suitLed = spades && not table.QSPlayed then tw   // only Q♠ beats the trick
                elif prob && hu suitLed > 0 then me
                elif prob && not a.OppShooterHasPlayed && not flag then shooter
                elif prob && lgtW > 0 then me
                elif prob && a.OppShooterHasPlayed then
                    if tw <> shooter then tw
                    elif table.TrickLeader = shooter && w <= 10 && table.SuitPulls[suitLed] <= 2 && i >= 2 && not table.QSOnTrick then opp1
                    elif w >= jack && suitLed = hearts then tw
                    elif fnv > 0 && i >= 6 then tw
                    elif table.ScoreInGame[shooter] - a.PointsExposed - 26 > sbh[1] then tw   // the Shoot wouldn't make the Shooter the low scorer
                    elif fnv > 0 && i >= 3 then
                        if position = 1 && table.IsVoid[opp1, suitLed] && not table.IsVoid[opp2, suitLed] then opp1 else opp2
                    else tw
                elif fnv = 0 then tw
                else
                    // neither this seat nor the Shooter can win the trick
                    let mutable k = none
                    let mutable lowRank = w
                    for p in opponents |> List.take (3 - position) do
                        if a.OppLowIn[p, suitLed] > lowRank then
                            k <- p
                            lowRank <- a.OppLowIn[p, suitLed]
                    if a.LowerRanks(w, suitLed) > 3 && fnv > 0 then tw
                    elif k >= 0 then k
                    else tw
        elif lsl > w then likelyTWinner <- me
        else likelyTWinner <- tw
        likelyTWinnerIsNonShootingOpp <- a.PossOppShoot && likelyTWinner <> a.Shooter && likelyTWinner <> me
        if likelyTWinnerIsNonShootingOpp && table.PointsInTrick > 0 then
            a.Shooter <- none
            a.PossOppShoot <- false
            a.ProbOppShoot <- false
            a.OppShooterHasPlayed <- false

    /// The parameters for leading (best_compute_parameters_related_to_leading), up to its endgame
    /// analysis.
    member this.ComputeLeadingParameters() =
        let lowestRiskBySize = Array.zeroCreate<int> 14
        let qsOwner = a.QSOwner
        let opps s = oppsHave s
        let canLeadH = a.CanLeadH

        // whether spades are safe to hold, and whether to pull them
        if lenS = 0 || table.QSPlayed || a.CaseSuit[spades] then spadesSafe <- true
        elif highIn spades >= queen && lenS = 2 && opps spades = 2 then
            if a.OppsVoidIn[spades] = 1 then spadesSafe <- true
        elif qsInHand then
            if lenS > estTr spades then pullSpades <- true
            elif lenS - 1 >= opps spades then pullSpades <- true
            elif lenS >= 5 && soloOwner spades = none then pullSpades <- true
            if pullSpades then spadesSafe <- true
            elif lenS = 1 && estTr spades = 1 && opps spades > a.AKS then ()
            elif lenS = 1 && estTr spades = 2 && opps spades = a.AKS + 1 then spadesSafe <- true
            elif lenS > estTr spades then spadesSafe <- true
            elif lenS = estTr spades && lenS >= 3 then spadesSafe <- true
        elif a.AKSInHand = 0 then
            pullSpades <- true
            spadesSafe <- true
        elif lenS >= a.AKSInHand + estTr spades || lenS >= a.AKSInHand + opps spades then
            pullSpades <- true
            spadesSafe <- true
        elif soloOwner spades >= 0 then
            spadesSafe <- lenS - a.AKSInHand >= opps spades - 1
            pullSpades <- spadesSafe && lenS > 2 * a.AKSInHand && lu spades >= a.AKSInHand
        elif lenS >= estTr spades then
            if lenS - a.AKSInHand >= 2 && opps spades <= 7 then pullSpades <- true
            spadesSafe <- true
        elif lenS - a.AKSInHand = 3 && tLeft <= 7 then spadesSafe <- true

        setImScores ()
        determineMode ()

        let hCondition = canLeadH && (not veryHighScore || (lenH = tLeft - 1 && qsInHand))
        let sCondition =
            if lenS = tLeft then true
            elif not table.QSPlayed && not pullSpades && (not spadesSafe || lenS < 4) then false
            elif a.QSOwner = none then true
            elif oppTargetHasQS then true
            elif opps spades = 1 && a.OutcardSuitCount >= 2 then false
            else true
        if suitWithLU = spades && not sCondition then
            suitWithLU <- none
            for i in hearts .. diamonds do
                if lu i = 0 then ()
                elif i = hearts && not canLeadH then ()
                elif suitWithLU = none then suitWithLU <- i
                elif lu i > lu suitWithLU then suitWithLU <- i
                elif lu i = lu suitWithLU && i = hearts then suitWithLU <- i

        // power_suit: the non-HU suit whose high card no opponent but a Shoot helper may beat
        let mutable helped = none
        let rec findPower i =
            if i <= diamonds then
                if len i = 0 || hu i > 0 || i = hearts then findPower (i + 1)
                elif i = spades && not spadesSafe then findPower (i + 1)
                else
                    let hcard, owners = oppOwnerOfHighCard i
                    let helperOwners =
                        opponents |> List.filter (fun p -> hcard >= 0 && opp[p, hcard, i] >= 0 && shootHelper[p]) |> List.length
                    if owners = helperOwners then powerSuit <- i
                    else
                        if helperOwners > 0 then helped <- i
                        findPower (i + 1)
        findPower spades
        if powerSuit = none && helped >= 0 then powerSuit <- helped
        if powerSuit = none then
            let mutable j = 20
            for i in clubs .. diamonds do
                if len i > 0 && ohr (highIn i) i < j then
                    j <- ohr (highIn i) i
                    powerSuit <- i
            if powerSuit >= 0 then ()
            elif lenH > 0 && canLeadH then powerSuit <- hearts
            else powerSuit <- spades

        // suit_of_lowest_risk
        let mutable j = 0
        for i in allSuits do
            let m = suitLen[i]
            if m = 0 || hu i = m || a.CaseSuit[i] then ()
            elif i = hearts && not hCondition then ()
            elif i = spades && not sCondition then ()
            else
                let p = olr (lowIn i) i
                let mutable k =
                    if p > 0 then
                        let k = clng (float (10 * ohr (lowIn i) i) / float p * 10000.0)
                        let k = if k >= 400000 && a.DefLosers[i] >= 2 then 2000100 else k
                        if k >= 500000 && a.DefLosers[i] >= 1 then 2000100 else k
                    elif a.DefLosers[i] = 1 && m >= 3 then 290100
                    else 2000000 + 100 * lu i
                k <- k + a.DefLosers[i]
                riskRank[i] <- k
                if k >= j then
                    j <- k
                    suitOfLowestRisk <- i
                let l = lowestRiskBySize[m]
                if l = 0 then lowestRiskBySize[m] <- i + 10
                elif riskRank[l - 10] < k then lowestRiskBySize[m] <- i + 10
                elif l - 10 = hearts && riskRank[hearts] = k then lowestRiskBySize[m] <- i + 10
        if suitOfLowestRisk >= 0 then
            if suitOfLowestRisk = spades then ()
            elif sCondition || lenS = 0 then ()
            elif lowIn spades >= queen && lu spades = 0 then ()
            elif riskRank[suitOfLowestRisk] <= 50000 then
                let p = olr (lowIn spades) spades
                let k =
                    (if p > 0 then clng (float (10 * ohr (lowIn spades) spades) / float p * 10000.0)
                     else 2000000 + 100 * lu spades) + a.DefLosers[spades]
                if k <= riskRank[suitOfLowestRisk] then ()
                elif riskRank[suitOfLowestRisk] = 0 && hu spades < lenS then suitOfLowestRisk <- spades
                elif qsInHand then
                    if lenS >= 4 then suitOfLowestRisk <- spades
                elif lenS - a.AKSInHand >= 3 then suitOfLowestRisk <- spades
                if suitOfLowestRisk = spades then lowestRiskBySize[lenS] <- spades + 10
        else
            if not a.CaseSuit[hearts] && lenH > hu hearts && canLeadH then suitOfLowestRisk <- hearts
            elif a.CaseSuit[spades] || lenS = 0 then ()
            elif lenS > 1 || not qsInHand then suitOfLowestRisk <- spades
            if suitOfLowestRisk >= 0 then ()
            elif not a.CaseSuit[hearts] && lenH > 0 && canLeadH then suitOfLowestRisk <- hearts
            elif not a.CaseSuit[clubs] && suitLen[clubs] > 0 then suitOfLowestRisk <- clubs
            elif not a.CaseSuit[diamonds] && suitLen[diamonds] > 0 then suitOfLowestRisk <- diamonds
            else suitOfLowestRisk <- a.ACaseSuit
            lowestRiskBySize[len suitOfLowestRisk] <- suitOfLowestRisk + 10

        // the leadable suits, lowest risk first
        numberOfLeadableSuits <- 0
        for k in allSuits do
            riskRankOfSuit[k + 1] <- none
            if suitLen[k] > 0 && (k <> hearts || canLeadH) then
                let l = riskRank[k]
                let pos =
                    seq { 1 .. numberOfLeadableSuits }
                        |> Seq.tryFind (fun i -> l > riskRank[riskRankOfSuit[i]])
                match pos with
                    | Some i ->
                        for j in numberOfLeadableSuits .. -1 .. i do riskRankOfSuit[j + 1] <- riskRankOfSuit[j]
                        riskRankOfSuit[i] <- k
                        numberOfLeadableSuits <- numberOfLeadableSuits + 1
                    | None ->
                        numberOfLeadableSuits <- numberOfLeadableSuits + 1
                        riskRankOfSuit[numberOfLeadableSuits] <- k

        // the card to lead in the suit of lowest risk
        let mutable rankInSuitOfLowestRisk = lowIn suitOfLowestRisk
        let sr = suitOfLowestRisk
        if len sr < 2 then ()
        elif sr = spades && rankInSuitOfLowestRisk = queen then rankInSuitOfLowestRisk <- highIn spades
        elif olr rankInSuitOfLowestRisk sr = olr (secondLowIn sr) sr then ()
        elif a.EstOppsVoidIn[sr] = 0 || table.QSPlayed then
            match suitCards[sr] |> Array.tryFind (fun r -> 3 * ohr r sr >= 5 * olr r sr) with
                | Some r -> rankInSuitOfLowestRisk <- r
                | None -> ()

        // the smallest suit in which every card is a likely winner
        let suitOfAllWinners =
            seq { 1 .. a.DefSuits[0] }
                |> Seq.map (fun j -> a.DefSuits[j])
                |> Seq.tryFind (fun i ->
                    if i = spades && not sCondition then false
                    elif i = hearts && not hCondition then false
                    else a.DefWinners[i] = suitLen[i])
                |> Option.defaultValue none

        // the shortest suit (of lowest risk among the shortest)
        match seq { 1 .. 13 } |> Seq.tryFind (fun n -> lowestRiskBySize[n] >= 10) with
            | Some n ->
                lenSS <- n
                suitSmallest <- lowestRiskBySize[n] - 10
            | None -> lenSS <- 14

        // the smallest suit in which not every card is a loser
        suitNotAllLosers <- suitOfLowestRisk
        let mutable j = 20
        for i in allSuits do
            let m = suitLen[i]
            if m = 0 || a.CaseSuit[i] then ()
            elif i = spades && not table.QSPlayed && not pullSpades then ()
            elif i = hearts && not hCondition then ()
            elif m <= j && m > a.DefLosers[i] then
                j <- m
                suitNotAllLosers <- i

        // whether, and how urgently, to pull out an opponent's Q♠
        let mutable pullOutQS = 0
        let mutable substituteSuit = none
        if pullSpades && a.OppOwnsQS then
            if imScore[me] = lowScore then pullOutQS <- 10
            elif mode = Mode.Equivocate then ()
            elif opps spades >= 2 && table.SuitPulls[spades] <= 1 then pullOutQS <- 10
            elif qsOwner = none then ()
            elif oppTarget[qsOwner] then pullOutQS <- 10
            if pullOutQS > 0 then ()
            elif a.CDHEntries = 0 then ()
            elif meTarget && (opps spades > 1 || a.OutcardSuitCount < 2) then pullOutQS <- 5
            elif qsOwner = none && a.NumberOfOutcards <= 1 then pullOutQS <- 3

            // a substitute suit for spades
            if pullOutQS <> 0 || qsOwner = none then ()
            elif a.PtsSit = PointsSit.unplayed || a.PtsSit = me then ()
            elif a.NumberOfOutcards > a.CDHEntries - 2 && a.NumberOfOutcards > 0 then
                let substituteCode = Array.create 4 0
                let rec scan i =
                    if i > diamonds then true
                    else
                        let z = opp.IsVoid(qsOwner, i)
                        let m = suitLen[i]
                        let n = opps i
                        substituteCode[i] <- 7
                        let level =
                            if m = 0 || a.CaseSuit[i] then 0
                            elif i = hearts && not canLeadH then 0
                            elif z && lu i > 0 && soloOwner i >= 0 then
                                if imScore[soloOwner i] = imScore[lowScorer] then 7 else 6
                            elif lu i > 0 && a.EstOppsVoidIn[i] = 2 then
                                if soloOwner i = none then 5 else 4
                            elif z && a.HighestOutcard[i] > 0 then 3
                            elif a.DefLosers[i] >= min 2 (min m (estTr i)) then 2
                            elif table.SuitPulls[i] = 0 then 1
                            elif a.EstOppsVoidIn[i] = 0 && deficit >= 20 then 1
                            else none
                        if level = none then false
                        else
                            if level > 0 then
                                substituteCode[i] <- level * 1000000 + (13 - n) * 10000 + (13 - m) * 100 + i
                            scan (i + 1)
                if scan hearts then
                    let n = max substituteCode[hearts] (max substituteCode[clubs] substituteCode[diamonds])
                    let i = n % 10
                    if i > diamonds then ()
                    elif n / 1000000 <= 1 then ()
                    else substituteSuit <- i

        // special_lead: leading low here will probably make Q♠ fall on the low scorer
        specialLead <- none
        if not a.OppOwnsQS || qsOwner = none then ()
        elif mode <> Mode.GetLowScorer && mode <> Mode.LieLow then ()
        elif imScore[me] = lowScore then ()
        else
            for i in hearts .. diamonds do
                if (i = hearts && not canLeadH) || lu i = 0 then ()
                elif not (opp.IsVoid(qsOwner, i)) then ()
                elif not (opp.IsVoid(lowScorer, i)) then specialLead <- i

        // the lead
        let soo = a.SuitOfOutcard
        let thirdLowest = min (len suitNotAllLosers) 3
        let useItOrLoseIt =
            a.NumberOfOutcards > 0
                && ((opps soo <= 2 && a.OppsVoidIn[soo] < 2)
                    || (opps soo = 1 && (soo <> spades || table.QSPlayed || pullSpades)))
        let lead r s =
            bestLeadRank <- r
            bestLeadSuit <- s
        let ss = suitSmallest
        let snal = suitNotAllLosers
        if specialLead >= 0 then lead (lowIn specialLead) specialLead
        elif qsInHand && not a.CaseSuit[spades] && (lenH = 0 || not canLeadH) && lenH + a.CaseCards = tLeft - 1 then
            // Q♠ and nothing but hearts and case cards
            if a.AKS > 0 && opps spades = a.AKS then lead queen spades
            elif lenH > 0 && a.CaseCards > 0 then lead (lowIn a.ACaseSuit) a.ACaseSuit
            else lead queen spades
        elif a.Endgame then
            if suitWithLU >= 0 then lead (card suitWithLU (len suitWithLU - lu suitWithLU)) suitWithLU
            elif a.NumberOfOutcards > 0 then lead a.HighestOutcard[soo] soo
            else lead rankInSuitOfLowestRisk suitOfLowestRisk
        elif qsInHand then
            if suitOfAllWinners >= 0 then
                lead (highIn suitOfAllWinners) suitOfAllWinners
                if bestLeadSuit = hearts && lenH >= 2 then bestLeadRank <- secondLowIn hearts
                priority <- 10
            elif useItOrLoseIt then
                lead a.HighestOutcard[soo] soo
                priority <- 9
            elif a.NumberOfOutcards = 1 && a.NumberOfVoids = 2 && lenS = 2 && soo <> spades && spadesSafe
                && not a.CaseSuit[spades] then
                lead a.HnQS spades
            elif a.LiveSuits = 1 && a.NumberOfOutcards > 0 && a.ALiveSuit >= clubs then
                lead (highIn a.ALiveSuit) a.ALiveSuit
            elif ss <> hearts && lenSS >= 3 && estTr ss >= 3 then lead (card ss (lenSS - 3)) ss
            else lead (card ss (max 0 (lenSS - 2))) ss
            if bestLeadSuit <> spades || bestLeadRank <> queen then ()
            elif opps spades = a.AKS && not a.CaseSuit[spades] then ()
            elif soloOwner spades >= 0 && lenH > hu hearts && a.PtsSit = PointsSit.unplayed && a.ACaseSuit >= clubs then
                lead (highIn a.ACaseSuit) a.ACaseSuit
            elif a.HnQS > 0 then bestLeadRank <- a.HnQS
        elif table.QSPlayed then
                // Under the arena's rules, the Q♠ doesn't break hearts, so hearts may not be leadable here. Killer
                // Hearts' two heart leads below assume they are; the CanLeadH guards (ARENA_PATCHES in the
                // transpiler) keep them to its own play under its own rules, where QSPlayed implies CanLeadH.
            let z = suitOfAllWinners >= 0 && a.OppsVoidIn[suitOfAllWinners] < 2 && len suitOfAllWinners = 1
            if estTr hearts > 0 && lu hearts >= min (estTr hearts) 2 && a.CanLeadH then
                lead (lowIn hearts) hearts
                priority <- 10
            elif z && suitOfAllWinners <> hearts then
                lead (highIn suitOfAllWinners) suitOfAllWinners
                priority <- 10
            elif useItOrLoseIt then
                lead a.HighestOutcard[soo] soo
                priority <- 9
            elif a.PtsSit = me && hu hearts < 2 && lenH > hu hearts && mode <> Mode.Equivocate && a.CanLeadH then
                lead (card hearts (hu hearts)) hearts   // fishing for a sucker
            elif lenSS <= 2 && (a.EstOppsVoidIn[ss] < 2 || lenSS = lu ss)
                && (ss <> hearts || (hu hearts = 0 && (a.DefLosers[hearts] > 0 || lenH = 1))) then
                lead (highIn ss) ss
                priority <- 6
            elif a.PtsSit <> PointsSit.split && snal <> hearts then
                lead (card snal (max 0 (hu snal - 1))) snal
            elif ohr (card snal (len snal - thirdLowest)) snal > 4 - a.EstOppsVoidIn[snal] then
                lead (card snal (len snal - thirdLowest)) snal
            else
                lead rankInSuitOfLowestRisk suitOfLowestRisk
                let s = bestLeadSuit
                if len s >= 4 || a.PtsSit <> PointsSit.split || s = hearts then ()
                elif a.DefLosers[s] >= 2 || table.SuitSluffs[me, s] > 0 then ()
                elif opps s + len s >= 12 then bestLeadRank <- highIn s
        elif useItOrLoseIt && (soo <> spades || sCondition) then lead a.HighestOutcard[soo] soo
        elif pullOutQS > 0 then
            lead (if soloOwner spades >= 0 then lowIn spades else a.HltQS) spades
        elif not spadesSafe && lenSS = 1 && table.SuitPulls[ss] <= 1 && ss <> spades then lead (lowIn ss) ss
        elif a.NoOppVoid[snal] then
            let s = snal
            let i =
                if table.PassDirection = hold && len s < estTr s && a.EstOppsVoidIn[s] = 0 then 0
                elif a.DefLosers[s] >= 2 then len s - a.DefLosers[s]
                elif opps s >= 10 then
                    let i = len s - thirdLowest
                    if i > 0 && card s i <= 8 then i - 1 else i
                elif offWinners s < len s then hu s
                else 0
            lead (card s i) s
        elif suitWithLU >= 0 && (a.TotalLU >= 2 || tLeft <= 5) then
            lead (card suitWithLU (len suitWithLU - lu suitWithLU)) suitWithLU
        elif not (opp.IsVoid(qsOwner, snal)) && a.NumberOfOutcards = 1 && a.HighestOutcard[snal] > 0 && len snal = 2
            && opps snal >= 7 then
            lead (highIn snal) snal
        elif (not spadesSafe || lenS = 0) && (ss <> spades || lowIn spades < queen) && not (opp.IsVoid(qsOwner, ss))
            && (opps ss >= 5 || (soloOwner ss = qsOwner && qsOwner >= 0)) then
            lead (lowIn ss) ss
            if lenSS = 1 || lu ss >= 2 then ()
            elif a.NoOppVoid[ss] then bestLeadRank <- highIn ss
            elif lenSS >= estTr ss && table.SuitPulls[ss] = 0 then bestLeadRank <- secondLowIn ss
            elif lenSS > 2 && olr (lowIn suitOfLowestRisk) suitOfLowestRisk < olr bestLeadRank ss then
                lead (lowIn suitOfLowestRisk) suitOfLowestRisk
        elif a.DefLosers[suitOfLowestRisk] >= 3 then lead (secondLowIn suitOfLowestRisk) suitOfLowestRisk
        elif olr (lowIn ss) ss < ohr (highIn ss) ss then
            lead (lowIn ss) ss
            if a.NoOppVoid[ss] then bestLeadRank <- highIn ss
            elif ss = spades then
                bestLeadRank <- if soloOwner spades >= 0 then lowIn spades else a.HltQS
            elif lenSS > estTr ss && lu ss < 2 then bestLeadRank <- secondLowIn ss
        else lead rankInSuitOfLowestRisk suitOfLowestRisk

        if substituteSuit >= 0 then lead (lowIn substituteSuit) substituteSuit

        // the mano-a-mano condition: this seat and one opponent hold a suit the other two lack
        let s = bestLeadSuit
        if suitLen[s] = 1 then ()
        elif a.OppsVoidIn[s] < 2 then
            if opps s <> 2 || a.OppOwnsQS then ()
            elif lu s > 0 then bestLeadRank <- secondLowIn s
            elif ohr (lowIn s) s >= 1 then bestLeadRank <- lowIn s
        elif lu s = 0 then bestLeadRank <- lowIn s
        elif a.PointsExposed = 25 && a.PtsSit = PointsSit.split then
            manoAMano <- true
            bestLeadRank <- lowIn s
            if opps s > 1 && soloOwner hearts = soloOwner s then bestLeadRank <- secondLowIn s
        else
            manoAMano <- true
            bestLeadRank <- lowIn s
            if opps s = 1 then ()
            elif a.OppOwnsQS && qsOwner = none then ()
            elif a.OppOwnsQS && soloOwner s = none then ()
            elif a.OppOwnsQS && soloOwner s <> qsOwner then ()
            else bestLeadRank <- secondLowIn s

        // never lead Q♠ needlessly
        if bestLeadSuit <> spades || lenS = 1 || bestLeadRank < queen || table.QSPlayed then ()
        elif bestLeadRank = queen then bestLeadRank <- a.HnQS
        elif qsInHand || lowIn spades >= king then ()
        else bestLeadRank <- a.HltQS

        // avoid a lead that reaches a Q♠ owner who has an entry into this seat's hand
        if qsOwner = none || a.QSOwnerCertainty < 8 then ()
        elif bestLeadSuit = spades && opps spades = 1 then ()
        elif soloOwner bestLeadSuit <> qsOwner then ()
        elif olr bestLeadRank bestLeadSuit > 0 then ()
        elif a.SuitWithHU = none || a.SuitWithHU = bestLeadSuit then ()
        elif soloOwner a.SuitWithHU <> qsOwner || a.NumberOfOutcards > 1 then ()
        elif a.SuitWithHU <> spades then lead (highIn a.SuitWithHU) a.SuitWithHU
        elif spadesSafe && a.HltQS > 0 then lead a.HltQS spades

        // with a singleton Q♠, avoid a lead that the spade owner can win
        if not qsInHand || lenS > 1 || manoAMano then ()
        elif soloOwner spades = none || opps spades = a.AKS then ()
        elif ohr bestLeadRank bestLeadSuit = 0 then ()
        elif table.IsVoid[soloOwner spades, bestLeadSuit] then ()
        else
            let mutable z = false
            for i in hearts .. diamonds do
                if lu i > 0 && soloOwner spades >= 0 && opp.SuitLenPoss[soloOwner spades, i] = 0 && (i <> hearts || canLeadH) then
                    lead (lowIn i) i
                    z <- true
            if not z then
                this.PlayerReaches()
                this.ReachedPlayerSuits(true, true, spades)
                if oppReachCount > 0 then
                    bestLeadSuit <- this.SuitSolution()
                    bestLeadRank <- lowIn bestLeadSuit

    /// The best lead's last adjustments and Killer Hearts' endgame analysis, when its mode calls for it.
    member this.FinishLeading() =
        saveBestLeadRank <- bestLeadRank
        saveBestLeadSuit <- bestLeadSuit
        if mode = Mode.StopShoot || mode = Mode.Equivocate || mode = Mode.HelpShooter || mode = Mode.ProtectHiScorer then
            this.PerformEndgameAnalysisForLeading()
        elif mode = Mode.ShootTheMoon || manoAMano || not a.Endgame || specialLead >= 0 then ()
        else this.PerformEndgameAnalysisForLeading()

    /// Killer Hearts' endgame analysis for leading (perform_endgame_analysis_for_leading).
    member private this.PerformEndgameAnalysisForLeading() =
        let qsOwner = a.QSOwner
        let lead r s =
            bestLeadRank <- r
            bestLeadSuit <- s
        let lowestImScore candidates =
            candidates |> List.fold (fun j i -> if j = none || imScore[i] < imScore[j] then i else j) none
        let mutable finished = false
        // protecting the high scorer, a lead in a suit only someone else holds is fine
        if mode = Mode.ProtectHiScorer && manoAMano && imScoreOf (soloOwner bestLeadSuit) < highScore then finished <- true
        if not finished && mode <> Mode.Equivocate && mode <> Mode.HelpShooter then
            // a lead that makes a target holding all winners take the trick
            this.PlayerReaches()
            this.ComputeOppWinsAll()
            let j = opponents |> List.filter (fun i -> oppWinsAll[i] && oppTarget[i] && suitConduit[i] >= 0) |> lowestImScore
            if j >= 0 then
                lead (lowIn suitConduit[j]) suitConduit[j]
                finished <- true
            // with Q♠ but weak spades, a lead that makes an opponent void in spades win the trick
            if not finished && qsInHand && not spadesSafe then
                this.ReachedPlayerSuits(true, true, spades)
                if oppReachCount > 0 then
                    let s = this.SuitSolution()
                    lead (lowIn s) s
                    finished <- true
            // a low lead that a target wins, in a suit Q♠'s owner lacks
            if not finished && qsOwner >= 0 then
                let mutable j = none
                for i in opponents do
                    if oppTarget[i] then
                        this.PlayerReaches()
                        this.RemovePlayer(false, i)
                        this.ReachedPlayerSuits(true, false, qsOwner)
                        if oppReachCount <= 0 then ()
                        elif j = none || imScore[i] < imScore[j] then
                            j <- i
                            let s = this.SuitSolution()
                            lead (lowIn s) s
                if j >= 0 then finished <- true
        if finished then ()
        elif mode = Mode.StopShoot then
            let shooter = a.Shooter
            bestLeadSuit <- none
            this.PlayerReaches()
            this.RemovePlayer(true, shooter)
            if qsInHand then
                if a.HighestOutcard[spades] = queen && a.OppWithAKS <> shooter then lead queen spades
                elif lenH = 1 && table.PointsPlayed = 12 && a.CanLeadH then lead (lowIn hearts) hearts
                elif spadesSafe && oppReachCount > 0 then
                    let s = this.SuitSolution()
                    lead (lowIn s) s
                else
                    this.ReachedPlayerSuits(true, true, spades)
                    if oppReachCount > 0 then
                        let s = this.SuitSolution()
                        lead (lowIn s) s
                    elif suitWithHU >= 0 && (suitWithHU <> spades || (pullSpades && (a.PQS > 0 || hu spades > 1))) then
                        lead (highIn suitWithHU) suitWithHU
                        if bestLeadSuit = spades && bestLeadRank = queen then bestLeadRank <- secondHighIn spades
                    elif pullSpades then lead a.HnQS spades
            elif qsOwner = shooter then
                let z = pullSpades && ohr a.HltQS spades = 1
                if a.Endgame && suitOfLowestRisk <> spades then lead (lowIn suitOfLowestRisk) suitOfLowestRisk
                elif pullSpades && (a.EstOppsVoidIn[spades] <= 1 || z) then lead a.HltQS spades
                elif opp.SuitLenInf[shooter, hearts] > 0 && hu hearts > 0 && a.CanLeadH then lead (highIn hearts) hearts
            elif not table.QSPlayed then
                let mutable reachedLow = false
                if qsOwner >= 0 && lowScore < imScore[shooter] then
                    // a lead the low scorer wins, in a suit Q♠'s owner holds
                    this.PlayerReaches()
                    this.RemovePlayer(false, lowScorer)
                    this.ReachedPlayerSuits(false, false, qsOwner)
                    if oppReachCount > 0 then
                        let s = this.SuitSolution()
                        lead (lowIn s) s
                        reachedLow <- true
                        finished <- true
                if reachedLow then ()
                elif qsOwner >= 0 then
                    this.SuitCompare(false, qsOwner, Some false, shooter)
                    if oppReachCount > 0 then
                        let s = this.SuitSolution()
                        lead (lowIn s) s
                    elif opp.SuitLenInf[qsOwner, hearts] > 0 && hu hearts > 0 && a.CanLeadH then lead (highIn hearts) hearts
                    elif oppTarget[qsOwner] && a.QSOwnerCertainty >= 9 && pullSpades then lead a.HltQS spades
                    else
                        this.SuitCompare(false, qsOwner, None, 0)
                        if oppReachCount > 0 then
                            let s = this.SuitSolution()
                            lead (lowIn s) s
                elif pullSpades then lead a.HltQS spades
            else
                if lenH = 1 && table.PointsPlayed = 25 then lead (lowIn hearts) hearts
                elif suitWithHU >= 0 then lead (highIn suitWithHU) suitWithHU
            if finished then ()
            elif bestLeadSuit = none then
                // the safest lead in a suit the Shooter lacks
                this.SuitCompare(false, shooter, None, 0)
                let mutable found = false
                if oppReachCount > 0 then
                    let mutable j = 100
                    for _ in 1 .. oppReachCount do
                        if not found then
                            let i = this.SuitSolution()
                            if i = spades && not spadesSafe then ()
                            elif i = hearts && not a.CanLeadH then ()
                            elif olr (lowIn i) i < j && not a.CaseSuit[i] then
                                j <- olr (lowIn i) i
                                lead (lowIn i) i
                                if i <> spades || bestLeadRank <> queen then found <- true
                                elif lenS > 1 then
                                    bestLeadRank <- card spades (a.PQS - 1)
                                    found <- true
                                elif eatQS then found <- true
                                else bestLeadSuit <- none
                    this.ResetSuitSolution()
                if found then ()
                elif suitLen[diamonds] > 0 then lead (lowIn diamonds) diamonds
                elif suitLen[clubs] > 0 then lead (lowIn clubs) clubs
                elif a.NumberOfVoids = 3 then lead (lowIn suitOfLowestRisk) suitOfLowestRisk
                elif a.CanLeadH then lead (lowIn hearts) hearts
                else lead saveBestLeadRank saveBestLeadSuit
        elif mode = Mode.GetLowScorer then
            this.PlayerReaches()
            let mutable taken = false
            if qsInHand then
                // a suit a target can take
                let j = opponents |> List.filter (fun i -> oppTarget[i] && suitConduit[i] >= 0) |> lowestImScore
                if j < 0 then ()
                elif suitConduit[j] = spades && soloOwner spades >= 0 && lenS = 2 && oppsHave spades = 2 && a.AKS < 2 then
                    taken <- true
                else
                    lead (lowIn suitConduit[j]) suitConduit[j]
                    taken <- true
            elif table.QSPlayed || qsOwner = none then ()
            elif oppTarget[qsOwner] then
                // a suit the target Q♠ owner is not void in
                this.ReachedPlayerSuits(false, false, qsOwner)
                for _ in 1 .. oppReachCount do
                    if not taken then
                        let j = this.SuitSolution()
                        if j <> spades || pullSpades then
                            lead (lowIn j) j
                            taken <- true
                this.ResetSuitSolution()
            if not taken then
                let scrap = qsOwner >= 0 && opp.SuitLenPoss[qsOwner, bestLeadSuit] >= 0
                if oppReach[bestLeadSuit] >= 0 then ()
                elif scrap then ()
                elif oppReachCount > 0 then
                    let s = this.SuitSolution()
                    lead (lowIn s) s
        elif mode = Mode.LieLow || mode = Mode.BeLowScorer then
            if qsOwner >= 0 then
                this.PlayerReaches()
                this.ReachedPlayerSuits(false, false, qsOwner)
                if suitConduit[opp1] >= 0 then lead (lowIn suitConduit[opp1]) suitConduit[opp1]
        elif mode = Mode.Equivocate then
            if a.CanLeadH && lenH > 0 && hu hearts = 0 && spadesSafe then lead (highIn hearts) hearts
            elif a.OppOwnsQS then
                let inTheBag =
                    if suitWithHU = none then false
                    elif a.WinsAllH >= 1 || (a.WinsAllH = -11 && lenH >= estTr hearts) then
                        if table.VoidInSuit[hearts] + a.PrivateVoidInSuit[hearts] < 3 then
                            lead (card suitWithHU (hu suitWithHU - 1)) suitWithHU
                            true
                        elif hu spades > 0 then
                            lead (card spades (hu spades - 1)) spades
                            true
                        else false
                    else false
                if inTheBag then ()
                elif a.WeakShootSuit >= 0 then
                    let wss = a.WeakShootSuit
                    lead (card wss (hu wss)) wss
                    if wss = spades && bestLeadRank >= king && a.HltQS > 0 then bestLeadRank <- a.HltQS
                elif lenS > 0 && lowIn spades < queen then
                    lead (lowIn spades) spades
                    if lenS = tLeft && a.HltQS > 0 then bestLeadRank <- a.HltQS
                else lead saveBestLeadRank saveBestLeadSuit
            else
                let tryLead (pick : int -> (int * int) option) suits =
                    suits |> List.tryPick pick |> Option.map (fun (r, s) -> lead r s) |> Option.isSome
                let rev = [ diamonds; clubs; hearts; spades ]
                let leadable i = i <> hearts || a.CanLeadH
                // with Q♠ and opponents who would help a Shoot, lead the lowest winner of a suit that wins
                let helperLead =
                    if qsInHand && shootHelpers >= 2 then
                        bestLeadSuit <- none
                        let j = if suitWithHU = hearts && hu spades > 0 then spades else suitWithHU
                        let s =
                            if suitWithHU >= clubs then suitWithHU
                            elif j = spades && (pullSpades || a.CaseSuit[spades]) && (hu spades > 1 || highIn spades > queen) then spades
                            elif suitWithHU = hearts && lenH = hu hearts then hearts
                            else none
                        if s >= 0 then
                            lead (card s (hu s - 1)) s
                            if bestLeadRank = queen && bestLeadSuit = spades then bestLeadRank <- card spades (a.PQS - 1)
                            true
                        else false
                    else false
                let done1 =
                    helperLead
                        || tryLead (fun i ->
                        if len i <= 1 || hu i <> 1 then None
                        elif not (leadable i) then None
                        elif oppsHave i <= 6 && (i <> spades || highIn spades <> queen) then Some (highIn i, i)
                        else None) rev
                let done2 =
                    done1
                        || tryLead (fun i ->
                            if len i = 0 || hu i > 0 then None
                            elif a.EstOppsVoidIn[i] > 0 then None
                            elif not (leadable i) then None
                            elif i <> spades || table.QSPlayed || pullSpades then
                                Some ((if len i >= 5 then card i 3 else lowIn i), i)
                            else None) rev
                let done5 =
                    done2
                        || tryLead (fun i ->
                            if len i < 2 || hu i <> 1 then None
                            elif not (leadable i) then None
                            elif i <> spades || table.QSPlayed || (pullSpades && (secondHighIn spades <> queen || lenS > 2)) then
                                Some ((if i = spades && secondHighIn i = queen then thirdHighIn spades else secondHighIn i), i)
                            else None) rev
                let done6 =
                    done5
                        || tryLead (fun i ->
                            if len i < 3 || hu i > 2 then None
                            elif a.EstOppsVoidIn[i] > 0 then None
                            elif not (leadable i) then None
                            elif i <> spades || table.QSPlayed || pullSpades then
                                Some ((if len i = 4 then lowIn i else card i 2), i)
                            else None) rev
                if done6 then ()
                elif (a.WeakShootSuit = spades && a.AKS <> 0) && suitWithHU >= 0 then
                    lead (card suitWithHU (hu suitWithHU - 1)) suitWithHU
                elif lenS > 0 && hu spades <= 1 && lowIn spades < queen && (not qsInHand || pullSpades) then
                    lead (lowIn spades) spades
                elif suitWithHU <> none && len suitWithHU <> hu suitWithHU then
                    lead (card suitWithHU (hu suitWithHU - 1)) suitWithHU
                    if bestLeadSuit <> spades || bestLeadRank <> queen then ()
                    elif a.AKSInHand > 0 then bestLeadRank <- card spades (a.PQS - 1)
                    else bestLeadRank <- card spades (a.PQS + 1)
                elif suitWithHU <> none && not (suitWithHU = spades && qsInHand && lenS = 1) then
                    lead (card suitWithHU (hu suitWithHU - 1)) suitWithHU
                    if bestLeadSuit <> spades || bestLeadRank <> queen then ()
                    elif a.AKSInHand > 0 then bestLeadRank <- card spades (a.PQS - 1)
                else lead saveBestLeadRank saveBestLeadSuit

        elif mode = Mode.HelpShooter then
            let shooter = a.Shooter
            let leadable i = len i > 0 && len i <> hu i && (i <> hearts || a.CanLeadH)
            // a suit that reaches the Shooter for sure, else one the Shooter may hold
            let mutable j = none
            let mutable k = none
            for i in allSuits do
                if len i = 0 || opp.SuitLenPoss[shooter, i] = 0 then ()
                elif len i = hu i then ()
                elif i = spades && not table.QSPlayed && not pullSpades then ()
                elif i = hearts && not a.CanLeadH then ()
                elif a.HigherRanks(a.OppHighIn[shooter, i], i) = 0 then
                    k <- i
                    if opp.SuitLenInf[shooter, i] > 0 then j <- i
            if j >= 0 then bestLeadSuit <- j
            elif k >= 0 then bestLeadSuit <- k
            else
                // the suit in which the low card has the most higher cards out, one the Shooter isn't void in
                let mutable most = none
                for i in allSuits do
                    if len i = 0 || table.IsVoid[shooter, i] then ()
                    elif not (leadable i) then ()
                    elif i = spades && lenS = 1 && lowIn spades = queen then ()
                    else
                        let l = ohr (lowIn i) i
                        if l > most || (l = most && l > 0) then
                            most <- l
                            j <- i
                if j >= 0 then bestLeadSuit <- j
                else
                    // the suit the low scorer is void in in which the Shooter may hold the most
                    let mutable most = 0
                    for i in allSuits do
                        if len i = 0 || not table.IsVoid[lowScorer, i] then ()
                        elif not (leadable i) then ()
                        elif i = spades && lenS = 1 && lowIn spades = queen then ()
                        else
                            let l = opp.SuitLenPoss[shooter, i]
                            if l > most || (l = most && l > 0) then
                                most <- l
                                j <- i
                    if j >= 0 then bestLeadSuit <- j
            bestLeadRank <- lowIn bestLeadSuit
        elif mode = Mode.ProtectHiScorer then
            let removeHiScorers () =
                this.PlayerReaches()
                this.RemovePlayer(true, hiScorer)
                if highScore = imScoreRank[3] then this.RemovePlayer(true, thirdPlace)
            // the low card of a suit solution, skipping a lone low Q♠
            let solutionLow () =
                if oppReachCount > 0 then
                    let mutable i = this.SuitSolution()
                    let mutable r = lowIn i
                    if i = spades && r = queen && oppReachCount > 1 then
                        i <- this.SuitSolution()
                        r <- lowIn i
                    if i <> spades || r <> queen then Some (i, r) else None
                else None
            removeHiScorers ()
            this.ReachedPlayerSuits(true, false, hiScorer)
            // a lead the high scorer is void in, of a suit with an unbeatable low card
            let mutable found = false
            for _ in 1 .. oppReachCount do
                if not found && lu (this.SuitSolution()) > 0 then
                    let j = solutionDelivered
                    if j <> bestLeadSuit || not manoAMano then lead (lowIn j) j
                    found <- true
            if not found then
                this.ResetSuitSolution()
                match solutionLow () with
                    | Some (i, r) -> lead r i   // a low lead the high scorer is void in
                    | None ->
                        // a lead that reaches anyone but the high scorer
                        removeHiScorers ()
                        match solutionLow () with
                            | Some (i, r) -> lead r i
                            | None ->
                                if olr bestLeadRank bestLeadSuit <> 0 then ()   // the lead isn't unbeatable
                                elif soloOwner bestLeadSuit = none then ()
                                elif imScore[soloOwner bestLeadSuit] < highScore then ()
                                else
                                    // the high scorer alone holds the suit: lead the next riskier suit
                                    let mutable z = false
                                    let mutable stop = false
                                    for n in 1 .. numberOfLeadableSuits do
                                        if not stop then
                                            bestLeadSuit <- riskRankOfSuit[n]
                                            bestLeadRank <- lowIn bestLeadSuit
                                            if bestLeadSuit = spades && bestLeadRank = queen then bestLeadRank <- highIn spades
                                            elif lu bestLeadSuit >= 2 then ()
                                            elif a.EstOppsVoidIn[bestLeadSuit] = 0 || table.QSPlayed then
                                                match suitCards[bestLeadSuit] |> Array.tryFind (fun r -> 3 * ohr r bestLeadSuit >= 5 * olr r bestLeadSuit) with
                                                    | Some r -> bestLeadRank <- r
                                                    | None -> ()
                                            if bestLeadSuit = spades && bestLeadRank = queen && imScore[me] + 19 >= 100
                                                && (oppsHave spades = 0 || olr queen spades > 0) then
                                                z <- true
                                            elif soloOwner bestLeadSuit = none then stop <- true
                                            elif imScore[soloOwner bestLeadSuit] < highScore then stop <- true
                                    if not stop then
                                        // every suit is mano a mano with the protectee, or Q♠ is chosen to lead
                                        bestLeadSuit <- saveBestLeadSuit
                                        bestLeadRank <- highIn bestLeadSuit
                                        if not eatQS && oppsHave bestLeadSuit = 1 && lu bestLeadSuit > 0 then
                                            bestLeadRank <- lowIn bestLeadSuit
                                        if bestLeadSuit <> spades || table.QSPlayed then ()
                                        elif a.OppOwnsQS then
                                            if imScoreOf qsOwner + 14 < 100 then bestLeadRank <- lowIn spades   // pull Q♠ from the protectee
                                            elif imScore[me] + 14 >= 100 then bestLeadRank <- lowIn spades
                                            elif bestLeadRank >= king || oppsHave spades > 1 then ()
                                            elif numberOfLeadableSuits > 1 then
                                                match [ 1 .. numberOfLeadableSuits ] |> List.tryFind (fun n -> riskRankOfSuit[n] <> spades) with
                                                    | Some n ->
                                                        bestLeadSuit <- riskRankOfSuit[n]
                                                        bestLeadRank <- highIn bestLeadSuit
                                                    | None -> ()
                                        elif z then failwith "Shouldn't lead QS"
                                        elif oppsHave spades > a.AKS then ()
                                        elif (soloOwner spades >= 0 && imScore[soloOwner spades] + 14 < 100) || imScore[me] + 14 >= 100 then
                                            bestLeadRank <- queen

    // ---- reach: which opponent a lead in each suit makes win the trick ----

    /// Starts suit solutions over (reset_suit_solution).
    member private this.ResetSuitSolution() = solutionDelivered <- none

    /// For each suit, the opponent this seat can make win a trick by leading it: the one whose lowest
    /// card is known and above everyone else's (player_reaches).
    member private this.PlayerReaches() =
        Array.fill suitConduit 0 4 none
        oppReachCount <- 0
        this.ResetSuitSolution()
        for s in allSuits do
            oppReach[s] <- none
            if suitLen[s] > 0 && not a.CaseSuit[s] && (s <> hearts || a.CanLeadH) then
                for o in opponents do
                    let others = opponents |> List.filter ((<>) o)
                    let j, k = others[0], others[1]
                    let i = a.OppLowIn[o, s]
                    if i <= 2 then ()
                    elif opp[o, i, s] = 0 then ()
                    elif i < lowIn s then ()
                    elif i < a.OppLowIn[j, s] || i < a.OppLowIn[k, s] then ()
                    else
                        let holdsLow p = a.OppLowIn[p, s] < 2 || opp[p, a.OppLowIn[p, s], s] > 0
                        if holdsLow j && holdsLow k then
                            oppReach[s] <- o
                            suitConduit[o] <- s
                            oppReachCount <- oppReachCount + 1

    /// Keeps the reachable suits whose opponent is void (or not) in a suit, or in the suits another
    /// opponent is void in (reached_player_suits).
    member private this.ReachedPlayerSuits(voidIn : bool, immediate : bool, arg3 : int) =
        this.ResetSuitSolution()
        if oppReachCount = 0 then ()
        else
            for s in allSuits do
                let j = oppReach[s]
                if j = none then ()
                elif immediate then
                    if voidIn && opp.SuitLenPoss[j, arg3] > 0 then
                        oppReach[s] <- none
                        oppReachCount <- oppReachCount - 1
                else
                    let wipe =
                        allSuits |> List.tryPick (fun i ->
                            if voidIn then
                                if opp.SuitLenPoss[j, i] > 0 then None
                                elif opp.SuitLenPoss[arg3, i] > 0 then Some true
                                else None
                            else
                                if opp.SuitLenPoss[j, i] = 0 then None
                                elif opp.SuitLenPoss[arg3, i] = 0 then Some true
                                else None)
                    if wipe = Some true then
                        oppReach[s] <- none
                        oppReachCount <- oppReachCount - 1

    /// Removes the reachable suits of one opponent, or of all but one (remove_player).
    member private this.RemovePlayer(only : bool, p : KSeat) =
        this.ResetSuitSolution()
        for s in allSuits do
            if oppReach[s] = none then ()
            elif only && oppReach[s] <> p then ()
            elif not only && oppReach[s] = p then ()
            else
                if oppReach[s] >= 0 then suitConduit[oppReach[s]] <- none
                oppReach[s] <- none
                oppReachCount <- oppReachCount - 1

    /// The suits this seat holds that one opponent, and maybe another, hold (or lack) (suit_compare).
    member private this.SuitCompare(shares1 : bool, p2 : KSeat, act2 : bool option, p3 : KSeat) =
        oppReachCount <- 0
        this.ResetSuitSolution()
        for s in allSuits do
            if s <> hearts || a.CanLeadH then
                oppReach[s] <- -100
                if suitLen[s] > 0 then
                    oppReach[s] <- 100
                    oppReachCount <- oppReachCount + 1
        let apply shares p =
            for s in allSuits do
                if oppReach[s] = 100 then
                    let z = opp.SuitLenPoss[p, s] > 0
                    if (shares && not z) || (not shares && z) then
                        oppReach[s] <- -100
                        oppReachCount <- oppReachCount - 1
        apply shares1 p2
        match act2 with
            | Some shares2 when oppReachCount > 0 -> apply shares2 p3
            | _ -> ()

    /// The next suit of the solution (suit_solution).
    member private this.SuitSolution() =
        let rec next n =
            if n > 0 then
                solutionDelivered <- solutionDelivered + 1
                if solutionDelivered <= 3 && oppReach[solutionDelivered] < 0 then next (n - 1)
        next 4
        solutionDelivered

    /// Which opponents could win every remaining trick once on lead (compute_opp_wins_all).
    member private this.ComputeOppWinsAll() =
        for p in opponents do
            oppWinsAll[p] <-
                allSuits |> List.forall (fun i ->
                    if opp.SuitLenPoss[p, i] = 0 then true
                    elif hu i > 0 || 3 * opp.SuitLenInf[p, i] + 2 < 2 * opp.SuitLenPoss[p, i] then false
                    else
                        let j = a.OppHighIn[p, i]
                        not (a.HigherRanks(j, i) > 0 || opp[p, j, i] = 0))

    /// The parameters for following suit (best_compute_parameters_related_to_following_suit).
    member this.ComputeFollowingParameters() =
        let lenSL = a.LenSuitLed
        let cards = suitCards[suitLed]
        let fnv = a.FollowersNotVoid
        let hltW = a.HltW
        let ptsSit = a.PtsSit
        let qsOwner = a.QSOwner
        let opps s = oppsHave s
        estTrSl <- estTr suitLed
        hsl <- highIn suitLed
        lsl <- lowIn suitLed
        match [ lenSL - 1 .. -1 .. 0 ] |> List.tryFind (fun i -> cards[i] > w && (suitLed <> spades || cards[i] <> queen)) with
            | Some i ->
                lgtW <- cards[i]
                pLgtW <- i
            | None -> ()
        let j = olr lsl suitLed
        equivalentLow <- 1 + (seq { lenSL - 2 .. -1 .. 0 } |> Seq.takeWhile (fun i -> olr cards[i] suitLed = j) |> Seq.length)
        if a.OppsVoidIn[suitLed] = 2 && opps suitLed > 0 then manoAMano <- true

        // whether spades are safe, and whether Q♠'s owner has played to the trick
        if qsInHand && suitLed = spades then
            spadesSafe <-
                if lenS - 1 > opps spades - a.AKS then true
                elif a.OppsVoidIn[spades] = 2 then lenS - 1 > opps spades - a.AKS
                else lenSL > estTrSl
        elif qsInHand then
            if lenS > opps spades then spadesSafe <- true
            elif lenS = 1 && estTr spades = 1 && opps spades > a.AKS then ()
            else spadesSafe <- lenS >= estTr spades
        elif table.QSPlayed then
            qsOwnerHasPlayed <- true
            spadesSafe <- true
        elif position = 3 then qsOwnerHasPlayed <- true
        elif suitLed = spades && fnv = 0 then qsOwnerHasPlayed <- true
        elif qsOwner >= 0 then qsOwnerHasPlayed <- table.PlayerHasPlayed[qsOwner]
        elif opponents |> List.take (3 - position) |> List.forall (fun p -> opp[p, queen, spades] < 0) then
            qsOwnerHasPlayed <- true
        let i = if suitLed = spades then 1 else 0
        let aksIn = a.AKSInHand
        if qsInHand || table.QSPlayed then ()
        elif aksIn = 0 then spadesSafe <- true
        elif lenS - aksIn > estTr spades then spadesSafe <- true
        elif lenS - aksIn >= opps spades then spadesSafe <- true
        elif lenS - aksIn >= 4 then spadesSafe <- true
        elif lenS - aksIn = 3 && estTr spades <= 3 then spadesSafe <- true
        elif qsOwner = opp3 && lenS > aksIn + i then
            spadesSafe <- true
            sStillSafe <- lenS - aksIn >= 2 + i

        // a Shooter who wins this trick and has nothing left to fear
        if table.TrickWinner = a.Shooter && (qsInHand || not qsOwnerHasPlayed) && a.HigherRanks(w, suitLed) = 0 && lgtW = 0
            && (table.SuitPulls[suitLed] > 2 || (table.PassDirection <> hold && (table.SuitPulls[suitLed] > 1 || table.PointsPlayed >= 5)))
            && (suitLed <> spades || (w >= king && table.TrickWinner = table.TrickLeader)) then
            a.PossOppShoot <- true
            a.ProbOppShoot <- true
            a.OppShooterHasPlayed <- table.PlayerHasPlayed[a.Shooter]

        // est_max: the card the seat expects to play when the suit runs out
        if estTrSl > 0 then
            let mutable i = max 0 (lenSL - estTrSl)
            estMax <- cards[i]
            estMax0 <- estMax
            while not (i = lenSL - 1 || estMax < w) do
                i <- i + 1
                let j = cards[i]
                if ohr j suitLed = ohr estMax suitLed then estMax <- j
        else estMax <- lsl
        if suitLed <> spades then ()
        elif qsOwnerHasPlayed || estMax < queen then ()
        elif a.HltQS > 0 then estMax <- a.HltQS
        else estMax <- a.HnQS

        this.DetermineLikelyTWinner()
        setImScores ()
        determineMode ()

        // gentle_card: the card that should lose the trick
        let secondLow = secondLowIn suitLed
        let secondHigh = secondHighIn suitLed
        gentleCard <-
            if a.CaseSuit[suitLed] then lsl
            elif not a.SuitIsLive[suitLed] then hltW
            elif hsl < w then hsl
            elif lu suitLed >= 3 then
                if suitLed = spades then lsl
                elif ptsSit = PointsSit.split then hltW
                elif secondHigh < w then secondHigh
                else lsl
            elif table.QSOnTrick then
                if hltW > 0 then hltW elif fnv = 0 then hsl else lsl
            elif lu suitLed >= opps suitLed then hltW
            elif not qsOwnerHasPlayed && opp.IsVoid(qsOwner, suitLed) && hltW > 0
                && (not veryHighScore || imScore[qsOwner] = lowScore || imScore[me] = lowScore) then hltW
            elif suitLed = hearts && manoAMano && lenH = 2 && lu hearts = 1 && table.QSPlayed then
                if fnv > 0 then
                    if opps hearts + lenH - 2 >= table.PointsInTrick + 2 then hsl else lsl
                elif opps hearts + 1 >= lenH then hsl
                else lsl
            elif a.OppsVoidIn[suitLed] = 2 && opps suitLed = 1 && lenSL >= 2 && lu suitLed >= 1 && trick < 12 then secondLow
            elif position = 3 && hsl - hltW <= 4 && hltW > a.HighestOutcard[suitLed] then hltW
            elif position = 3 && table.PointsInTrick = 0 && a.NumberOfOutcards > 0 then
                if lenSL <= estTrSl then hsl
                elif hsl > hltW && hltW >= estMax then hltW
                elif ptsSit <> PointsSit.split && a.SuitStopper[suitLed] then secondHigh
                elif lenSL >= 3 && hltW > secondLow then hltW
                else hsl
            elif table.SuitPulls[suitLed] = 1 && table.QSPlayed && table.PointsInTrick = 0 && a.PHltW >= lenSL - 2
                && lenSL >= 3 && a.DefLosers[suitLed] >= 1 then cards[lenSL - 3]
            elif table.SuitPulls[suitLed] = 1 && position < 3 && lenSL = 2 && table.PointsInTrick = 0 then secondLow
            elif hltW > 0 then hltW
            elif fnv = 0 then hsl
            elif suitLed = hearts && ohr lsl suitLed > 0 then lsl
            elif lenSL >= 2 && opps suitLed >= fnv + 3 then secondLow
            else lsl
        if gentleCard = hsl && lenSL > estTrSl && lenSL > 1 && (a.PossOppShoot || ptsSit = PointsSit.unplayed) then
            gentleCard <- cards[lenSL - estTrSl]
            if gentleCard < secondHigh && mode <> Mode.ShootTheMoon && mode <> Mode.Equivocate then gentleCard <- secondHigh
        if suitLed <> spades then ()
        elif gentleCard < queen || qsOwnerHasPlayed then ()
        elif a.HltQS > 0 then gentleCard <- a.HltQS
        else gentleCard <- a.HnQS

        expectedHOnTrick <-
            if suitLed = hearts then table.PointsInTrick + 1 + fnv
            else table.PointsInTrick + 3 - position - fnv

        // take_points: usually, when a Shoot is possible and points are on the trick
        let pit = table.PointsInTrick
        if pit = 0 || ptsSit = PointsSit.split then ()
        elif a.Shooter >= 0 && not a.PossOppShoot then ()
        elif table.QSOnTrick || hsl < w || veryHighScore then ()
        elif a.TotalDefLosers = 0 && table.PointsPlayed <= 21 then ()
        elif mode = Mode.BeLowScorer && leadOver2ndLow <= pit && score[likelyTWinner] - 26 > score[me] then ()
        elif mode = Mode.HelpShooter then ()
        elif suitLed <> hearts then takePoints <- true
        elif not likelyTWinnerIsNonShootingOpp && a.ProbOppShoot then takePoints <- true
        elif not likelyTWinnerIsNonShootingOpp && mode = Mode.BeLowScorer && leadOver2ndLow >= 20 && opposeShoot then takePoints <- true
        elif a.DefLosers[hearts] >= opps hearts && qsOwner = a.Shooter then takePoints <- true

        // smart_card: the highest card to play when no card can change who wins the trick
        smartCard <-
            if opps suitLed = 0 then lsl
            elif ptsSit = PointsSit.split then hsl
            elif opps suitLed = 1 then hsl
            elif hsl <= 10 then hsl
            elif lenSL > opps suitLed then secondHigh
            elif lenSL > estTrSl then secondHigh
            elif lenSL = estTrSl && opposeShoot then secondHigh
            else hsl

        // smart_S: for pulling an opponent's Q♠ when spades are safe
        let z =
            if suitLed <> spades || not spadesSafe then false
            elif qsInHand || table.QSPlayed || veryHighScore then false
            elif lenS - aksIn < 1 then false
            elif table.SuitPulls[spades] = 1 && hsl >= queen && lenSL = 4 then false
            elif a.OppsVoidIn[spades] = 2 && opps spades > lenS - aksIn then false
            elif estTrSl > 1 then false
            elif meTarget || qsOwner = none then true
            else oppTarget[qsOwner]
        if not z then ()
        elif qsOwnerHasPlayed && a.HltQS > w then
            smartS <-
                if lenSL <= estTrSl + 2 then a.HltQS
                elif secondLow < queen then secondLow
                else lsl
        elif a.HltQS > w then smartS <- a.HltQS
        elif qsOwnerHasPlayed && a.HnQS > w then smartS <- a.HnQS
        elif not qsOwnerHasPlayed && secondLow > queen then smartS <- lsl
        else smartS <- secondLow

        this.ComputeSafeCard()

    /// safe_card: a card in the suit led relatively safe from winning Q♠, though it may win the trick.
    member private this.ComputeSafeCard() =
        let lenSL = a.LenSuitLed
        let cards = suitCards[suitLed]
        let fnv = a.FollowersNotVoid
        let hltW = a.HltW
        let ptsSit = a.PtsSit
        let qsOwner = a.QSOwner
        let opps s = oppsHave s
        let secondLow = secondLowIn suitLed
        let secondHigh = secondHighIn suitLed
        let pit = table.PointsInTrick
        let aksIn = a.AKSInHand
        let hnQS = a.HnQS
        let hltQS = a.HltQS
        let pQS = a.PQS
        let fi, oppRanksLTHltW = if hltW > 0 then ohr hltW suitLed, olr hltW suitLed else none, none
        let set c = safeCard <- c
        if a.PointsExposed = 26 && pit > 0 then ()
        elif a.CaseSuit[suitLed] then set lsl
        elif imScore[me] >= 100 - expectedHOnTrick then
            // the hearts likely on the trick would end the game: lose it
            if hltW > 0 then set hltW
            elif suitLed = spades && lsl = queen then set (a.SecondLowIn[spades])
            elif fnv > 0 then set lsl
            elif suitLed = spades && hsl = queen then set (secondHighIn spades)
            else set hsl
        elif table.QSOnTrick || tLeft <= 2 then ()
        elif suitLed = spades && qsInHand && lenS = 2 then set hnQS
        elif suitLed = hearts && table.QSPlayed && position = 3 && hltW > 0 && pit > lenH - 2 + opps hearts then set hltW
        elif a.TotalOffWinners = tLeft - 1 && fi = 0 then set hltW
        elif smartS > 0 then set smartS
        elif a.NumberLTW >= 3 && not qsInHand then
            if suitLed = spades && not spadesSafe then set lsl
            elif suitLed <> hearts then
                if suitLed <> spades || hltW <= jack then
                    set hltW
                    if hltW = hsl then set secondHigh
                else set hnQS
            elif not a.SuitStopper[hearts] || lenSL < estTrSl then ()
            elif likelyTWinner = a.Shooter || ptsSit = PointsSit.unplayed || ptsSit >= 0 then
                set secondHigh
                if secondHigh > w then set hltW
        elif hsl < w then
            set smartCard
            if suitLed <> hearts || ptsSit = PointsSit.split then ()
            elif not a.SuitStopper[hearts] || lenSL < estTrSl then ()
            elif likelyTWinner = a.Shooter || ptsSit = PointsSit.unplayed then set secondHigh
        elif manoAMano && lu suitLed = 0 && lsl < w && fnv = 0 && (suitLed <> spades || not qsInHand) then
            set lgtW
            if hltW > 0 then set hltW
        elif manoAMano && suitLed = spades && not canHoldQS && fnv = 0 && lgtW > 0 && qsInHand then set lgtW
        elif opps suitLed = 1 && pit < table.PointsUnplayed && (qsOwnerHasPlayed || qsOwner = none) then
            if secondLow < w then set secondLow
            elif fnv = 1 then set lsl
            elif lsl < w && secondLow < a.LowOppCard[suitLed] then set lsl
            else set secondLow
            if suitLed = spades && safeCard = queen then set hnQS
        elif manoAMano && suitLed = spades && qsInHand && not canHoldQS && ohr hnQS spades = 0 then
            let i = suitCards[spades][1]
            if i <> hnQS && i <> queen then set i
            elif secondLowIn spades <> hnQS && secondLowIn spades <> queen then set (secondLowIn spades)
            else set lsl
        elif manoAMano && opps suitLed > 1 && lu suitLed = 1 && hsl > w && (qsInHand || qsOwner = table.TrickLeader || table.QSPlayed) then
            if a.NumberLTW >= 2 then
                set hltW
                if suitLed = spades && hltW = queen then set (cards[a.PHltW + 1])
            else set hsl
        elif opps suitLed = 1 && hltW > 0 then set hltW
        elif opps suitLed = 2 && (qsOwnerHasPlayed || qsInHand) then
            set secondLow
            if suitLed = spades && not spadesSafe then
                set hnQS
                if hnQS < w then set lsl
            elif hltW > 0 && equivalentLow >= 2 then set hltW
            elif hltW > secondLow then set hltW
            elif secondLow > w && fnv = 0 && ptsSit = PointsSit.split then set hsl
            if suitLed <> spades || safeCard <> queen then ()
            elif pQS > 0 then set (suitCards[spades][pQS - 1])
            else set (suitCards[spades][pQS + 1])
        elif a.NumberOfOutcards = 1 && (qsOwnerHasPlayed || qsInHand) && a.Entries = 1 && a.SuitOfOutcard = suitLed && opps suitLed > 5 then
            if secondLow <> queen || suitLed <> spades then set secondLow
        elif a.NumberOfOutcards = 1 && (qsOwnerHasPlayed || qsInHand) && a.Entries = 1 && manoAMano && a.SuitOfOutcard = suitLed then
            set hsl
            if hsl = queen && suitLed = spades then set (secondHighIn spades)
        elif qsInHand && suitLed = spades then
            let i =
                if hnQS > w && fnv = 0 then hnQS
                elif lsl < hltQS then suitCards[spades][pQS + 2]
                elif lsl = queen then suitCards[spades][pQS - 1]
                else lsl
            if not spadesSafe then
                set lsl
                if lsl = queen then set secondLow
            elif hnQS > w && lenS > estTrSl then set hnQS
            elif aksIn = 1 && lenS >= 3 && lenS = estTrSl + 1 then
                set lsl
                if hltQS > w then set hltQS
            elif lgtW = 0 && lsl < hltQS && opps spades > 1 then set (suitCards[spades][pQS + 2])
            elif a.DefSuits[0] = 1 && (a.DefSuits[1] <> hearts || a.CanLeadH) then set i
            elif a.DefSuits[0] >= 2 then set i
            elif a.NumberOfVoids = 0 then set i
            elif takePoints && (a.ProbOppShoot || a.NumberOfOutcards > 0) then set i
        elif qsInHand then
            if not spadesSafe then
                if a.NumberOfOutcards = 1 && a.SuitOfOutcard = suitLed && opps suitLed > fnv then set (max secondLow hltW)
                elif suitLed = hearts && hltW > lsl then set hltW
                elif a.CDHEntries <= 1 && a.NumberOfVoids > 0 then ()
                elif hsl > w then set hsl
                else set secondLow
            elif a.DefSuits[0] >= 2
                || (a.DefSuits[0] = 1 && (a.DefSuits[1] <> hearts || a.CanLeadH) && (suitLed <> hearts || a.NumberLTW < estTrSl - 1)) then
                set hsl
                if hltW > lsl then set hltW
                elif ptsSit = PointsSit.split || fnv > 0 then
                    if secondLow > lsl then set secondLow
                elif lenSL <= opps suitLed + 2 then ()
                elif secondHigh > w || hsl < w then set secondHigh
            elif takePoints && (a.ProbOppShoot || a.NumberOfOutcards > 0) then set hsl
            elif a.NumberLTW >= estTrSl - 1 then set hltW
            elif a.NumberOfVoids = 0 then set hsl
            elif suitLed = hearts then ()
            elif table.PassDirection = hold && table.SuitPulls[suitLed] = 1 then set estMax0
        elif lsl > w then
            // the seat must win the trick
            if fnv = 0 then
                if suitLed <> spades || smartCard < queen || qsOwnerHasPlayed then set smartCard
                elif hltQS > 0 then set hltQS
                else set hnQS
            elif qsOwnerHasPlayed then
                if suitLed = hearts && equivalentLow >= 3 then set lsl
                elif table.QSPlayed then set secondLow
                else set estMax
            elif suitLed <> spades && position = 1 then
                set lsl
                if manoAMano && soloOwner suitLed <> qsOwner && soloOwner suitLed <> table.TrickLeader then ()
                elif opps suitLed = 2 || table.SuitPulls[suitLed] = 1 then set secondLow
            elif suitLed = spades && not spadesSafe then
                set lsl
                if qsOwner = none && position = 2 && lenS = aksIn + 1 && opps spades >= 7 then set hsl
        elif qsOwnerHasPlayed then
            let z = if qsOwner >= 0 then oppTarget[qsOwner] || meTarget else meTarget
            if not spadesSafe && suitLed = spades then
                set lsl
                if hsl > w then set hsl
            elif takePoints && (a.ProbOppShoot || a.NumberOfOutcards > 0) then
                set hsl
                if a.Shooter >= 0 && a.OppShooterHasPlayed && lgtW > 0 then set lgtW
            elif suitLed = hearts then
                if z && lenS - aksIn >= 1 && estTr spades <= 1 && deficit = 0 && leadOver2ndLow > expectedHOnTrick + 15 then set hsl
            elif z && lenS - aksIn >= 1 && estTr spades <= 1 then set hsl
            elif position = 2 then set estMax
            elif position = 3 && gentleCard > w then
                if ptsSit = PointsSit.split then set hsl
            elif hltW > estMax && ptsSit = PointsSit.split then set hltW
            elif position = 3 && pit = 0 && a.NumberOfOutcards > 0 && ptsSit = PointsSit.split then set estMax
            elif hltW > lsl then
                set hltW
                let i = if lgtW = 0 && hsl > w && fnv = 0 then hsl else lgtW
                if pit > 0 || i = 0 then ()
                elif suitLed <> spades then
                    if table.QSPlayed then set hltW
                    elif lenS > 0 && spadesSafe then set i
                elif qsOwner = none || z then set i
            elif lu suitLed = 1 && estTrSl >= lenSL then
                set secondLow
                if secondLow > w && ptsSit = PointsSit.split && fnv = 0 then set hsl
            elif a.DefLosers[suitLed] >= 2 then ()
            elif position < 3 && a.OppOwnsQS && table.SuitPulls[suitLed] = 1 then set estMax
            elif a.DefLosers[suitLed] = 1 && opps suitLed <= fnv + 1 then set estMax
            elif a.NumberOfOutcards <= 2 then set estMax
        elif suitLed = spades then
            if not spadesSafe then
                set lsl
                if qsOwner = none && position = 2 && lenS = aksIn + 1 && opps spades >= 7 then set hsl
            elif hltQS > 0 then set hltQS
        elif suitLed = hearts || hltW > lsl then ()
        elif opp.IsVoid(qsOwner, suitLed) then ()
        elif position < 3 && lu suitLed > 0 && a.DefLosers[suitLed] >= 3 && olr secondLow suitLed = 0 then ()
        elif table.PassDirection = hold && table.SuitPulls[suitLed] <= 2 && (estMax < w || lenSL <= 5) then set estMax
        elif qsOwner = none && fnv = 3 - position then set estMax
        elif position <> 2 then ()
        elif a.EstOppsVoidIn[suitLed] <> table.Sluffs then ()
        elif takePoints then set hsl
        else set estMax
        if safeCard = none then set gentleCard

        // override 1: play the highest card of a virgin suit
        if safeCard = hsl || suitLed <= hearts || position = 1 || table.QSOnTrick then ()
        elif lenSL > 4 + table.Sluffs || lenSL + opps suitLed + position - table.Sluffs < 13 then ()
        elif safeCard < w && olr safeCard suitLed = olr hsl suitLed then ()
        elif not qsOwnerHasPlayed && opp.IsVoid(qsOwner, suitLed)
            && (imScore[me] < 100 - 13 || imScoreOf qsOwner = lowScore || imScore[me] = lowScore) then ()
        elif pit = 1 && ptsSit = PointsSit.unplayed then set hsl
        elif pit >= 1 && pit <= 2 && ptsSit >= 0 then set hsl
        elif a.NumberLTW <= 2 then set hsl

        // overrides 2-4: play HltW rather than a trick winner when they are equivalent, when nearly every
        // remaining point is on the trick, or when the opponents have few cards left in the suit
        let i = if suitLed = hearts then 3 else 1
        if safeCard <= hltW || hltW = 0 then ()
        elif qsInHand && suitLed = spades then ()
        elif olr safeCard suitLed = oppRanksLTHltW then set hltW
        elif pit >= 26 - a.PointsExposed - i && pit > 0 then set hltW
        elif mode <> Mode.Equivocate && table.QSPlayed && hltW > lsl && opps suitLed <= 3 then set hltW

        // override 5: play the highest card rather than a lower sure winner
        if safeCard > w && fnv = 0 then
            if table.QSPlayed && ptsSit = PointsSit.split then set hsl
            elif suitLed = spades && hnQS > 0 then set hnQS
            elif ptsSit <> PointsSit.split && ptsSit <> me then ()
            else set hsl
        elif suitLed = spades && qsInHand then ()
        // override 6: play HltW rather than a lower card unless the suit is long or points are split
        elif (lenSL <= estTrSl || ptsSit = PointsSit.split) && safeCard < hltW then set hltW

    /// The parameters for sluffing (best_compute_parameters_related_to_sluffing).
    member this.ComputeSluffingParameters() =
        let ptsSit = a.PtsSit
        let qsOwner = a.QSOwner
        let opps s = oppsHave s
        let aksIn = a.AKSInHand
        if table.TrickWinner = a.Shooter && a.FollowersNotVoid = 0 && table.Sluffs = position - 1 then
            // the Shooter leads a case card
            a.PossOppShoot <- true
            a.ProbOppShoot <- true
            a.OppShooterHasPlayed <- table.PlayerHasPlayed[a.Shooter]

        // whether spades are safe, now and after sluffing one
        if lenS = 0 || a.CaseSuit[spades] || table.QSPlayed then
            spadesSafe <- true
            sStillSafe <- true
        elif qsInHand then
            if lenS > opps spades && a.OutcardSuitCount > 0 && (a.SuitOfOutcard <> spades || a.OutcardSuitCount > 1) then
                spadesSafe <- true
                if lenS - 1 > estTr spades then sStillSafe <- true
            elif lenS = 1 then
                if opps spades <= a.AKS + 1 then spadesSafe <- true
            elif lenS >= 4 then
                spadesSafe <- true
                if lenS - 1 > estTr spades then sStillSafe <- true
            elif lenS >= estTr spades && lu spades > 0 then
                spadesSafe <- true
                if lenS - 1 > estTr spades then sStillSafe <- true
        elif aksIn = 0 then
            spadesSafe <- true
            sStillSafe <- true
        elif lenS - aksIn >= opps spades then
            spadesSafe <- true
            sStillSafe <- lenS - aksIn - 1 >= opps spades
        elif lenS - aksIn >= estTr spades || lenS - aksIn >= 3 then
            spadesSafe <- true
            sStillSafe <- lenS - aksIn - 1 >= estTr spades
        elif qsOwner = opp3 && lenS > aksIn then
            spadesSafe <- true
            sStillSafe <- lenS - aksIn >= 2

        if qsInHand then ()
        elif table.QSPlayed then qsOwnerHasPlayed <- true
        elif position = 3 then qsOwnerHasPlayed <- true
        elif suitLed = spades && a.FollowersNotVoid = 0 then qsOwnerHasPlayed <- true
        elif qsOwner >= 0 then qsOwnerHasPlayed <- table.PlayerHasPlayed[qsOwner]
        elif opponents |> List.take (3 - position) |> List.forall (fun p -> opp[p, queen, spades] < 0) then
            qsOwnerHasPlayed <- true

        if suitOfOutcard = spades && not sStillSafe then suitOfOutcard <- none

        // Shoot_stopper_suit: the suit most likely to stop an opponent's Shoot
        for i in allSuits do
            if suitLen[i] = 0 then ()
            elif opp.IsVoid(a.Shooter, i) || not a.SuitStopper[i] then ()
            elif soloOwner i >= 0 && soloOwner i <> a.Shooter then ()
            elif shootStopperSuit = none then shootStopperSuit <- i
            elif i <= hearts then ()
            elif soloOwner i >= 0 then
                if soloOwner shootStopperSuit <> a.Shooter then shootStopperSuit <- i
                elif suitLen[i] < suitLen[shootStopperSuit] then shootStopperSuit <- i
            elif soloOwner shootStopperSuit >= 0 then ()
            elif suitLen[i] < suitLen[shootStopperSuit] then shootStopperSuit <- i

        this.DetermineLikelyTWinner()
        setImScores ()
        determineMode ()

        // suit_to_sluff, not considering Q♠
        let defSuits = a.DefSuits
        let i = defSuits[0]
        let m = defSuits[1]
        let k = len m
        let defWinners s = if s < 0 then 0 else a.DefWinners[s]
        let set s = suitToSluff <- s
        if not spadesSafe && not qsInHand && aksIn > 0 then set spades
        elif ptsSit = PointsSit.split && lenH = 1 && opps hearts = 0 && oppTarget[likelyTWinner] then set hearts
        elif i >= 1 && k <= 2 && spadesSafe && defWinners m = k && (m <> spades || table.QSPlayed) then set m
        elif lenH > 0 && table.PointsInTrick = 0 && opposeShoot && likelyTWinnerIsNonShootingOpp then set hearts
        elif not spadesSafe && not qsInHand then set spades
        elif i >= 1 && k = defWinners m && m <> spades then set m
        elif i >= 1 && k = defWinners m && table.QSPlayed then set m
        elif i >= 1 && k = defWinners m && qsInHand && sStillSafe then set m
        elif i >= 1 && k = defWinners m && ptsSit = PointsSit.split then set m
        elif i >= 1 && k = 1 && (m <> spades || not qsInHand) then set m
        elif i >= 2 && len defSuits[2] = defWinners defSuits[2] then set defSuits[2]
        elif lenS > 0 && lu spades = 0 && a.OppOwnsQS && a.OppsVoidIn[spades] = 2 && opps spades > lenS - aksIn then set spades
        elif highIn hearts >= 5 && table.PointsInTrick = 0 && ptsSit <> PointsSit.split && ptsSit <> me then set hearts
        elif i >= 1 && (m <> spades || table.QSPlayed) then set m
        elif i >= 2 then set defSuits[2]
        elif a.Entries > 0 then ()
        elif oppTarget[likelyTWinner] && ptsSit < 0 && lenH > lu hearts then set hearts
        elif oppTarget[likelyTWinner] && ptsSit = PointsSit.unplayed && lenH > 0 then set hearts
        elif a.SuitsInHand = 2 && a.CaseCards > 0 && a.CaseCards < tLeft && (suitOfOutcard <> spades || lenS > 1 || not qsInHand) then
            set suitOfOutcard

        if suitToSluff = none then
            let mutable k = 0
            let mutable l = 20
            let mutable n = 0
            let rec scan i =
                if i <= diamonds then
                    let m = suitLen[i]
                    if not a.CaseSuit[i] && m > lu i && estTr i > lu i then
                        let p = a.DefWinners[i]
                        if (i <> spades || table.QSPlayed) && (i <> hearts || ptsSit <> me || a.WinsAllH <= -13) then
                            let j = olr (lowIn i) i
                            if p > n || (p = n && j > k) || (p = n && j = k && m < l) then
                                k <- j
                                l <- m
                                n <- p
                                set i
                            scan (i + 1)
                        elif i = spades && not spadesSafe && a.HnQS > queen && not qsInHand then set spades
                        else scan (i + 1)
                    else scan (i + 1)
            scan spades
            let als = a.ALiveSuit
            if suitToSluff >= 0 then ()
            elif a.LiveSuits = 1 && (als <> hearts || a.Shooter <> likelyTWinner || ptsSit = PointsSit.split)
                && (als <> spades || not qsInHand || sStillSafe) then set als
            elif als >= clubs then set als
            elif oppTarget[likelyTWinner] && lenH > 0 && a.Shooter <> likelyTWinner then set hearts
            elif lenS > 0 && not qsInHand then set spades
            elif lenS > 1 && qsInHand && sStillSafe then set spades
            elif a.ACaseSuit >= clubs then set a.ACaseSuit
            elif a.ACaseSuit = hearts && ptsSit = PointsSit.split then set hearts
            elif suitLen[diamonds] > 0 then set diamonds
            elif suitLen[clubs] > 0 then set clubs
            elif lenH > 0 then set hearts
            else set spades

        // when an opponent owns Q♠, be careful sluffing spades
        if suitToSluff <> spades || not a.OppOwnsQS then ()
        elif lu spades > 0 || lowIn spades >= king then ()
        elif lenS <> opps spades || lenS = tLeft then ()
        elif a.ALiveSuit <> spades then set a.ALiveSuit
        elif suitLen[clubs] > 0 && not a.CaseSuit[clubs] then set clubs
        elif suitLen[diamonds] > 0 && not a.CaseSuit[diamonds] then set diamonds
        elif lenH > 0 && not a.CaseSuit[hearts] then set hearts
        elif a.ACaseSuit >= 0 then set a.ACaseSuit

        // keep the last heart for a Shooter
        if mode <> Mode.HelpShooter && lastH && suitToSluff = hearts then
            suitToSluff <- none
            let mutable k = 100
            for i in allSuits do
                if i = hearts || suitLen[i] = 0 then ()
                elif i = spades && qsInHand then
                    k <- 4
                    set spades
                elif a.SuitStopper[i] && k > 3 then
                    k <- 3
                    set i
                elif a.CaseSuit[i] && k > 2 then
                    k <- 2
                    set i
                elif lu i > 0 && k > 1 then
                    k <- 1
                    set i
                elif k = 1 && lu i > 0 && suitLen[i] < suitLen[suitToSluff] then set i
                elif k > 0 then
                    k <- 0
                    set i
                elif k = 0 && suitLen[i] < suitLen[suitToSluff] then set i

        // rank_to_sluff
        let s = suitToSluff
        rankToSluff <- highIn s
        if len s = 1 then ()
        elif rankToSluff = queen && s = spades then rankToSluff <- a.HnQS
        elif ptsSit = PointsSit.split || ptsSit = me then ()
        elif s = hearts && likelyTWinnerIsNonShootingOpp then ()
        elif not opposeShoot && (ptsSit <> PointsSit.unplayed || s <> hearts || (lenH < estTr hearts && lenH < 3)) then
            if a.SuitStopper[s] && len s > estTr s then
                rankToSluff <- secondHighIn s
                if rankToSluff = queen && s = spades then rankToSluff <- a.HnQS
                elif imScore[me] >= 100 - 4 then rankToSluff <- highIn s   // too near 100 to keep a stopper
        elif s = hearts then
            if not a.SuitStopper[hearts] || veryHighScore then
                if lenH >= estTr hearts then rankToSluff <- secondHighIn hearts
            elif lenH >= estTr hearts || lenH >= 3 then rankToSluff <- secondHighIn hearts
            elif opposeShoot && (eatQS || table.QSPlayed) then rankToSluff <- secondHighIn hearts
        elif len s > estTr s then
            rankToSluff <- secondHighIn s
            if rankToSluff = queen && s = spades then rankToSluff <- a.HnQS

        if mode = Mode.ShootTheMoon || mode = Mode.Equivocate then
            // suit_of_best_nonpoint: the non-point suit weakest for Shooting
            suitOfBestNonpoint <- none
            let wss = a.WeakShootSuit
            if a.WinsAllH >= 1 && lenS >= 2 && (qsInHand || highIn spades >= king) then suitOfBestNonpoint <- spades
            elif wss = hearts then ()
            elif wss = spades && lenS = 1 && qsInHand then ()
            elif wss >= 0 then suitOfBestNonpoint <- wss
            if suitOfBestNonpoint = none then
                let mutable k = 100
                for i in allSuits do
                    if suitLen[i] = 0 || i = hearts then ()
                    elif i = spades && qsInHand && lenS = 1 then ()
                    elif offWinners i < k then
                        suitOfBestNonpoint <- i
                        k <- offWinners i
            // rather than make a void by sluffing a singleton winner, sluff from a suit of all winners
            let sbn = suitOfBestNonpoint
            if sbn = none then ()
            elif suitLen[sbn] > 1 then ()
            elif a.CaseSuit[sbn] then ()
            elif hu sbn = 1 then
                let rec scan i =
                    if i <= diamonds then
                        if suitLen[i] < 2 || i = hearts then scan (i + 1)
                        elif i = spades && not sStillSafe then scan (i + 1)
                        elif i = spades && qsInHand && lenS = 1 then scan (i + 1)
                        elif a.CaseSuit[i] || offWinners i < suitLen[i] then suitOfBestNonpoint <- i
                        elif suitLen[suitOfBestNonpoint] = 1 then
                            suitOfBestNonpoint <- i
                            scan (i + 1)
                        else scan (i + 1)
                scan spades
        elif mode = Mode.StopShoot then
            // sluff points when this seat is sure to stop the Shoot
            let sss = shootStopperSuit
            let guaranteed = sss >= 0 && a.OnlyHasH >= 0
            if guaranteed && qsInHand && sss <> spades then
                suitToSluff <- spades
                rankToSluff <- queen
            elif guaranteed && lenH > 0 && sss <> hearts then
                suitToSluff <- hearts
                rankToSluff <- highIn hearts
            elif guaranteed && a.SuitWithHU >= 0 && sss <> a.SuitWithHU then
                suitToSluff <- a.SuitWithHU
                rankToSluff <- highIn a.SuitWithHU
            else
                // suits to sluff, by the highest priority bit of each suit's bitmap
                let bit n = 1 <<< n
                let sbms =
                    allSuits |> List.map (fun i ->
                        let l = suitLen[i]
                        let mutable b = i
                        if l = 0 then ()
                        elif i = sss && a.SurplusLength[sss] = 0 && suitLen[i] < tLeft then ()
                        elif i = hearts && (a.PointsExposed = 12 || a.PointsExposed = 25) then ()
                        elif i = spades && qsInHand && (l = 1 || not sStillSafe) then ()
                        elif i = spades && not sStillSafe then
                            b <- b ||| bit 24
                            if not spadesSafe then b <- b ||| bit 11
                        else
                            if a.CaseSuit[i] then b <- b ||| bit 22
                            elif a.Shooter >= 0 && table.IsVoid[a.Shooter, i] then b <- b ||| bit 21
                            if hu i >= 2 then b <- b ||| bit 23
                            elif a.SluffableSuitStopper = i && a.OppOwnsQS then
                                b <- b ||| bit 20
                                if hu i = 1 then b <- b ||| bit 12
                            elif not a.SuitStopper[i] then b <- b ||| bit 19
                            elif a.SluffableSuitStopper = i then
                                b <- b ||| bit 18
                                if hu i = 1 then b <- b ||| bit 12
                            elif hu i = 1 then b <- b ||| bit 12
                            if a.DefLosers[i] >= 2 then
                                b <- b ||| bit 16
                                if lu i >= 2 then b <- b ||| bit 15
                            elif l = 1 then ()
                            elif a.DefLosers[i] = 1 then
                                b <- b ||| bit 14
                                if lu i = 1 then b <- b ||| bit 13
                            else b <- b ||| bit 17
                        b)
                let selected = List.max sbms
                if selected &&& -4 > 0 then
                    let j = [ 30 .. -1 .. 0 ] |> List.find (fun j -> selected &&& bit j > 0)
                    suitToSluff <- selected &&& 3
                    let l = suitLen[suitToSluff]
                    let i =
                        if suitToSluff = sss then 1
                        elif j = 21 then (if l > 1 then 1 else 0)
                        elif j = 24 then (if eatQS && l > 1 then 1 else 0)
                        elif l < 2 then 0
                        elif j = 20 || j = 23 then 1
                        elif j >= 13 && j <= 18 then 1
                        else 0
                    rankToSluff <- suitCards[suitToSluff][i]
                    if rankToSluff <> queen || suitToSluff <> spades then ()
                    elif highIn spades = queen then rankToSluff <- secondHighIn spades
                    else rankToSluff <- suitCards[spades][a.PQS - 1]
                elif not qsInHand then
                    if eatQS && (sss <> spades || highIn sss >= king) then
                        rankToSluff <- highIn hearts
                        suitToSluff <- hearts
                    else
                        rankToSluff <- lowIn sss
                        suitToSluff <- sss
                elif lenS > 1 then
                    rankToSluff <- if highIn spades = queen then secondHighIn spades else highIn spades
                    suitToSluff <- spades
                elif eatQS && (a.PointsExposed = 25 || sss = spades) then
                    if lenH > 0 then
                        rankToSluff <- highIn hearts
                        suitToSluff <- hearts
                    else
                        rankToSluff <- highIn sss
                        suitToSluff <- sss
                else
                    rankToSluff <- queen
                    suitToSluff <- spades
        elif mode = Mode.HelpShooter then
            let shooter = a.Shooter
            // the suit whose high card has the most lower cards out, against the most higher
            let bestByRanks exclude =
                let mutable k = -1
                let mutable l = none
                for i in allSuits do
                    if not (exclude i) then
                        let j = olr (highIn i) i - ohr (highIn i) i + 13
                        if j > k then
                            k <- j
                            l <- i
                l
            let firstHeld () = allSuits |> List.findBack (fun i -> suitLen[i] > 0)
            if likelyTWinner <> shooter then
                // the trick goes to a seat other than the Shooter: sluff a high card the Shooter would have to beat
                let l =
                    bestByRanks (fun i ->
                        suitLen[i] = 0 || a.CaseSuit[i] || i = hearts || (i = spades && not sStillSafe)
                            || opp.IsVoid(shooter, i))
                if l >= 0 then
                    suitToSluff <- l
                    rankToSluff <- highIn l
                    if l = spades && rankToSluff = queen then rankToSluff <- a.HnQS
                elif a.ACaseSuit >= clubs then
                    suitToSluff <- a.ACaseSuit
                    rankToSluff <- highIn a.ACaseSuit
                elif a.ACaseSuit = spades && (lenS > 1 || not qsInHand) then
                    suitToSluff <- spades
                    rankToSluff <- a.HnQS
                elif lenS > 1 then
                    suitToSluff <- spades
                    rankToSluff <- a.HnQS
                elif qsInHand && lenH = tLeft - 1 then
                    suitToSluff <- spades
                    rankToSluff <- queen
                elif lenH = tLeft then
                    suitToSluff <- hearts
                    rankToSluff <- highIn hearts
                else
                    suitToSluff <- firstHeld ()
                    rankToSluff <- highIn suitToSluff
            elif qsInHand then ()   // the script handles Q♠
            else
                // the Shooter takes the trick: sluff the points, or a stopper the Shooter would have to beat
                let mutable l =
                    if a.OppPtCards = 0 && lenH > 0 && a.EstOppsVoidIn[suitLed] = 2 && oppsHave suitLed + 1 >= lenH then
                        hearts   // the seat has the only points left
                    elif shootStopperSuit >= 0 then shootStopperSuit
                    else
                        allSuits |> List.fold (fun l i -> if soloOwner i = shooter && a.SuitStopper[i] then i else l) none
                if l = none && lenH > 0 then l <- hearts
                if l = none then
                    l <- bestByRanks (fun i -> suitLen[i] = 0 || a.CaseSuit[i] || table.IsVoid[shooter, i])
                suitToSluff <-
                    if l >= 0 then l
                    elif a.ACaseSuit >= 0 then a.ACaseSuit
                    else firstHeld ()
                rankToSluff <- highIn suitToSluff
        elif mode = Mode.ProtectHiScorer then
            if suitToSluff <> hearts || imScore[likelyTWinner] <> highScore then ()
            else
                // the high scorer would take a heart: sluff a non-point card instead
                let mutable keepHeart = false
                if a.DefSuits[0] >= 2 && a.DefSuits[2] <> hearts && (a.DefSuits[2] <> spades || lenS > 1 || not qsInHand) then
                    suitToSluff <- a.DefSuits[2]
                else
                    let mutable k = 0
                    let mutable l = 20
                    let mutable n = 0
                    let mutable stop = false
                    for i in allSuits do
                        let m = suitLen[i]
                        if not stop && m > 0 && not a.CaseSuit[i] && m > lu i && i <> hearts then
                            let p = a.DefWinners[i]
                            if i <> spades || table.QSPlayed then
                                let j = olr (lowIn i) i
                                if p > n || (p = n && j > k) || (p = n && j = k && m < l) then
                                    k <- j
                                    l <- m
                                    n <- p
                                    suitToSluff <- i
                            elif i = spades && not spadesSafe && a.HnQS > queen && not qsInHand then
                                suitToSluff <- spades   // a high-priority sluff
                                stop <- true
                    if suitToSluff <> hearts then ()
                    elif lenS > 0 && not qsInHand then suitToSluff <- spades
                    elif lenS > 1 && qsInHand && sStillSafe then suitToSluff <- spades
                    elif a.ACaseSuit >= clubs then suitToSluff <- a.ACaseSuit
                    elif suitLen[diamonds] > 0 then suitToSluff <- diamonds
                    elif suitLen[clubs] > 0 then suitToSluff <- clubs
                    elif lenS = 1 && qsInHand then keepHeart <- true
                    elif lenS > 0 then suitToSluff <- spades
                    else keepHeart <- true
                if not keepHeart then
                    let s = suitToSluff
                    rankToSluff <- highIn s
                    if len s = 1 then ()
                    elif rankToSluff = queen && s = spades then rankToSluff <- a.HnQS
                    elif a.PtsSit <> PointsSit.unplayed then ()
                    elif len s > estTr s then
                        rankToSluff <- secondHighIn s
                        if rankToSluff = queen && s = spades then rankToSluff <- a.HnQS

    /// The opponent who will probably have to take Q♠ from this seat (QS_poison).
    member this.ComputeQSPoison() =
        qsPoison <-
            if not qsInHand || table.QSPlayed || spadesSafe then none
            elif a.OppsVoidIn[spades] = 2 then soloOwner spades
            elif table.PossibleQSPoison >= 0 && table.PossibleQSPoison <> me then table.PossibleQSPoison
            else none
    member _.SuitOfOutcard = suitOfOutcard
