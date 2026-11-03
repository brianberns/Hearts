namespace Hearts.Heuristic.Killer

/// How a decision ended before Killer Hearts' scripts ran.
type EarlyExit =
    /// The seat's remaining cards win every trick, so it claims them.
    | AllWinners
    /// The rules or the hand force the card.
    | Forced of KCard

/// Killer Hearts' analysis of the seat's situation at a decision (initialize_general_parameters, up
/// to its specialized parameters for leading, following and sluffing): what it infers of the
/// opponents' hands, whether one of them is Shooting, and how the seat's own suits stand.
///
/// The analysis is a sequence of steps, each building on the ones before, as in Killer Hearts. Its
/// results are this object's properties; the later stages of the decision read them, and change a
/// few of them.
type Analysis(table : Table) =

    let me = table.Me
    let position = table.Seq   // Killer Hearts' seq: plays so far in the trick
    let trick = table.Trick
    let suitLed = table.SuitLed
    let w = table.W
    let tLeft = 14 - trick
    let opp1, opp2, opp3 = nextSeat me, nextSeat (nextSeat me), previousSeat me
    let opponents = [ opp1; opp2; opp3 ]

    /// The seat's cards in each suit, highest first (suit_card), and their number (suit_len).
    let suitCards =
        Array.init 4 (fun s -> [| for r in ace .. -1 .. 2 do if table.Hand[r, s] then r |])
    let suitLen = suitCards |> Array.map Array.length
    let holds r s = table.Hand[r, s]
    let played r s = table.CardsPlayed[r, s]
    let qsInHand = holds queen spades

    /// The seat to whom this seat passed a card, if it passed the card.
    let passedTo r s =
        if table.Passed |> Array.exists (fun c -> c.Rank = r && c.Suit = s) then passTarget table.PassDirection me
        else none

    let privateInfo = Array3D.init 4 15 4 (fun p r s -> table.PrivateInfo[p, r, s])
    let opp = OppHands(me)

    /// Opponents' unplayed cards below and above a rank (lower_ranks, higher_ranks).
    let lowerRanks start s = seq { 2 .. start - 1 } |> Seq.filter (fun r -> not (played r s) && not (holds r s)) |> Seq.length
    let higherRanks start s = seq { start + 1 .. ace } |> Seq.filter (fun r -> not (played r s) && not (holds r s)) |> Seq.length

    /// Opponents' cards strictly between two ranks (opp_ranks_between).
    let oppRanksBetween r1 r2 s = seq { r1 + 1 .. r2 - 1 } |> Seq.filter (fun r -> not (played r s) && not (holds r s)) |> Seq.length

    /// The opponents' next rank above a rank, if any (get_opps_next_higher_card).
    let oppsNextHigherCard r s =
        seq { r + 1 .. ace } |> Seq.tryFind (fun r -> not (played r s) && not (holds r s)) |> Option.defaultValue none

    let pPosn p = 1 <<< p

    // ---- results ----
    let oppsHave = Array.zeroCreate<int> 4
    let highIn = Array.create 4 none
    let secondHighIn = Array.create 4 none
    let thirdHighIn = Array.create 4 none
    let lowIn = Array.create 4 none
    let secondLowIn = Array.create 4 none
    let oppsLowerRanks = Array2D.zeroCreate<int> 15 4
    let oppsHigherRanks = Array2D.zeroCreate<int> 15 4
    let privateVoidInSuit = Array.zeroCreate<int> 4
    let absoluteOwnersOfSuits = Array.zeroCreate<int> 4
    let soloOwner = Array.create 4 none
    let oppsVoidIn = Array.zeroCreate<int> 4
    let estOppsVoidIn = Array.zeroCreate<int> 4
    let noOppVoid = Array.zeroCreate<bool> 4
    let caseSuit = Array.zeroCreate<bool> 4
    let hu = Array.zeroCreate<int> 4
    let huPrime = Array.zeroCreate<int> 4
    let estTr = Array.zeroCreate<int> 4
    let offWinners = Array.zeroCreate<int> 4
    let oppLowIn = Array2D.zeroCreate<int> 4 4
    let oppHighIn = Array2D.zeroCreate<int> 4 4
    let lowOppCard = Array.zeroCreate<int> 4
    let deadCardsBySuit = Array.zeroCreate<int> 4
    let highestOutcard = Array.zeroCreate<int> 4
    let lu = Array.zeroCreate<int> 4
    let defLosers = Array.zeroCreate<int> 4
    let defWinners = Array.zeroCreate<int> 4
    let suitIsLive = Array.zeroCreate<bool> 4
    let defSuits = Array.zeroCreate<int> 5
    let suitStopper = Array.zeroCreate<bool> 4
    let surplusLength = Array.create 4 none

    let mutable canLeadH = table.CanLeadH
    let mutable sluffPts = table.SluffPts
    let mutable ptsSit = table.PointsSit
    let mutable shooter = table.ArcShooter[me]
    let mutable possOppShoot = table.ArcPossOppShoot[me]
    let mutable probOppShoot = table.ArcProbOppShoot[me]
    let mutable qsOwner = none
    let mutable qsOwnerCertainty = none
    let mutable anOppNotVoid = none
    let mutable comfortLevel = 0
    let mutable lenSuitLed = 0
    let mutable hltW = 0
    let mutable hltWQS = 0
    let mutable pHltW = none
    let mutable numberLTW = 0
    let mutable sluffing = false
    let mutable endgame = false
    let mutable longSuit = none
    let mutable suitsInHand = 0
    let mutable oppShooterHasPlayed = false
    let mutable aCaseSuit = none
    let mutable caseCards = 0
    let mutable numberOfOutSuits = 0
    let mutable numberOfSingletons = 0
    let mutable numberOfVoids = 0
    let mutable suitWithHU = none
    let mutable suitWithoutHU = none
    let mutable sumOfHUs = 0
    let mutable totalUnbeatables = 0
    let mutable nonHUBeatsW = 0
    let mutable huBeatsW = 0
    let mutable qsIsHU = 0
    let mutable followersNotVoid = none
    let mutable onlyHasH = none
    let mutable totalOffWinners = 0
    let mutable allSuitsCovered = true
    let mutable totalWinR = 0.0f
    let mutable allWinnersType = 0
    let mutable earlyExit = None
    let mutable oppHWinners = none
    let mutable winsAllH = 0
    let mutable hltQS = 0
    let mutable hnQS = 0
    let mutable oppWithAKS = none
    let mutable totalDeadCards = 0
    let mutable totalLU = 0
    let mutable numberOfOutcards = 0
    let mutable outcardSuitCount = 0
    let mutable suitOfOutcard = none
    let mutable suitWithLU = none
    let mutable totalDefLosers = 0
    let mutable aLiveSuit = none
    let mutable cdhEntries = 0
    let mutable entries = 0
    let mutable entriesClipped = 0
    let mutable liveSuits = 0
    let mutable aSuitStopper = none
    let mutable countOfSuitStoppers = 0
    let mutable sluffableSuitStopper = none
    let mutable weakShootSuit = none

    let pointsExposed = table.PointsPlayed + table.PointsInTrick
    let oppOwnsQS = not table.QSPlayed && not qsInHand
    let aksInHand = (if holds ace spades then 1 else 0) + (if holds king spades then 1 else 0)
    let aks =
        2 - (if played ace spades then 1 else 0) - (if played king spades then 1 else 0) - aksInHand
    let pQS = if qsInHand then aksInHand else none
    let oppPtCards =
        let n = 26 - pointsExposed - suitLen[hearts]
        if qsInHand then n - 13 elif not table.QSPlayed then n - 12 else n

    /// Records this seat's private knowledge of a card (Playing_update_private_info).
    let updatePrivateInfo code p r s =
        if code >= 512 && code <= 767 then
            if Info.certainty privateInfo[p, r, s] <= Info.certainty code then privateInfo[p, r, s] <- code
        else
            let certainty = Info.certainty code
            let known = allSeats |> List.exists (fun i -> privateInfo[i, r, s] >= 1024 && Info.certainty privateInfo[i, r, s] >= certainty)
            if not known then
                privateInfo[p, r, s] <- code
                for i in allSeats do
                    if i <> p && Info.certainty privateInfo[i, r, s] < certainty then
                        privateInfo[i, r, s] <- 512 ||| certainty

    /// Names the opponent believed to hold Q♠, with a certainty of 15 (known) or less (set_QS_owner).
    let setQSOwner p c =
        qsOwner <- p
        qsOwnerCertainty <- c
        let j = if c = 15 then 2 else 1
        for k in opponents do
            let m = opp[k, queen, spades]
            if m = 2 || m = -2 then ()
            elif k = p then opp.Update(j, k, queen, spades, true)
            else opp.Update(-j, k, queen, spades, true)

    /// The one opponent who holds (or may hold) a card, if only one can (player_owns). When the two
    /// others cannot, the third is inferred to hold it.
    let playerOwns r s =
        let mutable owners = 0
        let mutable nonOwners = 0
        let mutable result = 0
        for i in opponents do
            if opp[i, r, s] >= 1 then
                owners <- owners + 1
                result <- i
            elif opp[i, r, s] <= -1 then nonOwners <- nonOwners + 1
            else result <- i
        if owners = 1 then result
        elif nonOwners = 2 && opp[result, r, s] >= 0 then
            if opp[result, r, s] = 0 then opp.Update(1, result, r, s, true)
            result
        else none

    /// How many opponents this seat infers are void in the suit, and one that is not (opps_void_funct).
    let oppsVoidFunct s =
        if oppsHave[s] = 0 then
            anOppNotVoid <- none
            3
        else
            let mutable n = 0
            let mutable t = 0
            let mutable u = none
            anOppNotVoid <- none
            for i in opponents do
                if not (opp.IsVoid(i, s)) then anOppNotVoid <- i else n <- n + 1
                if not table.IsVoid[i, s] then u <- i else t <- t + 1
            let firstPossible () =
                anOppNotVoid <- u
                match opponents |> List.tryFind (fun i -> opp.SuitLenPoss[i, s] > 0) with
                    | Some i -> anOppNotVoid <- i
                    | None -> ()
            if n = 2 then
                if oppsHave[s] > table.HandSize[anOppNotVoid] then n <- 1
                elif (oppsHave[s] = 1 || oppsHave[s] = 2) && t = 2 then anOppNotVoid <- u
            elif oppsHave[s] = 1 then
                n <- 2
                firstPossible ()
            elif oppsHave[s] = 2 then
                if t = 2 then
                    n <- 2
                    anOppNotVoid <- u
                elif n <> 1 then
                    n <- 1
                    firstPossible ()
            elif oppsHave[s] > 0 && n = 3 then
                n <- t
                firstPossible ()
            n

    /// The number of opponents following this seat who are void in the suit (void_followers).
    let voidFollowers s =
        let mutable n = opponents |> List.take (3 - position) |> List.filter (fun p -> opp.IsVoid(p, s)) |> List.length
        if n = 3 && not caseSuit[s] then n <- 2
        if oppsHave[s] = 1 && position <= 1 && n = 0 then n <- 2 - position
        n

    /// How often the seat can lead the suit before the opponents run out of it (est_tricks).
    let estTricks s =
        let i = oppsVoidIn[s]
        let j = oppsHave[s]
        let n =
            if caseSuit[s] || i = 3 then 0
            elif i = 2 then min suitLen[s] j
            elif table.PassDirection <> hold then Tables.estTricksPass[i, j]
            else Tables.estTricksHold[i, j]
        min n tLeft

    let setEstimatedVoids s =
        estOppsVoidIn[s] <- oppsVoidIn[s]
        if oppsVoidIn[s] < 2 && oppsHave[s] <= 3 then estOppsVoidIn[s] <- 2
        elif oppsVoidIn[s] = 0 && oppsHave[s] <= 7 then estOppsVoidIn[s] <- 1
        noOppVoid[s] <-
            if position > 0 && s = suitLed then oppsHave[s] >= comfortLevel - position + table.Sluffs && oppsVoidIn[s] = 0
            else oppsHave[s] >= comfortLevel && oppsVoidIn[s] = 0

    /// Refines the inferred opponents' hands until nothing changes (refine_opponents_inferred_hands).
    let refineOpponentsInferredHands () =
        let mutable loopCounter = 0
        let mutable change = true
        while change && loopCounter < 10 do
            change <- false
            loopCounter <- loopCounter + 1

            // 1] an opponent who alone among the opponents is not void in a suit holds all of it
            for p in opponents do
                let k = if opp1 = p then opp2 else opp1
                let m = if opp3 = p then opp2 else opp3
                for s in allSuits do
                    if table.IsVoid[k, s] && table.IsVoid[m, s] && not table.IsVoid[p, s] && opp.CardsNotHeld[p, s] < 13 then
                        if soloOwner[s] <> p then
                            change <- true
                            soloOwner[s] <- p
                        for r in 2 .. ace do
                            let j = opp[p, r, s]
                            if j > -2 && j < 2 then
                                change <- true
                                if not (holds r s) && not (played r s) then opp.Update(2, p, r, s, true)
                                else opp.Update(-2, p, r, s, true)

            // 2] an opponent whose hand is completely known
            for p in opponents do
                let firm = allSuits |> List.sumBy (fun s -> opp.SuitLenFirm[p, s])
                let notHeld = allSuits |> List.sumBy (fun s -> opp.CardsNotHeld[p, s])
                if firm = table.HandSize[p] then
                    for s in allSuits do
                        if opp.SuitLenFirm[p, s] > 0 then
                            for r in 2 .. ace do
                                let j = opp[p, r, s]
                                if j = 2 then
                                    for k in opponents do
                                        if k <> p && opp[k, r, s] > -2 then
                                            change <- true
                                            opp.Update(-2, k, r, s, true)
                                elif j > -2 then
                                    change <- true
                                    opp.Update(-2, p, r, s, true)
                elif 52 - notHeld = table.HandSize[p] then
                    for s in allSuits do
                        if opp.CardsNotHeld[p, s] < 13 then
                            for r in 2 .. ace do
                                let j = opp[p, r, s]
                                if j > -2 && j < 2 then
                                    change <- true
                                    opp.Update(2, p, r, s, true)
                                    for k in opponents do
                                        if k <> p then opp.Update(-2, k, r, s, true)

            // 3] every opponent's card in a suit is known
            for s in allSuits do
                if opp.SuitLenFirm[opp1, s] + opp.SuitLenFirm[opp2, s] + opp.SuitLenFirm[opp3, s] = oppsHave[s] then
                    for p in opponents do
                        for r in 2 .. ace do
                            let j = opp[p, r, s]
                            if j < 2 && j > -2 then
                                change <- true
                                opp.Update(-2, p, r, s, true)

            // 4] a card the other two opponents probably lack, this one probably holds
            for p in opponents do
                for s in allSuits do
                    for r in 2 .. ace do
                        if opp[p, r, s] = 0 then
                            let j = opponents |> List.filter (fun i -> i <> p && opp[i, r, s] <= -1) |> List.length
                            if j = 2 then
                                change <- true
                                opp.Update(1, p, r, s, true)

            // 5] a card that no opponent is believed to hold
            for s in allSuits do
                for r in 2 .. ace do
                    let unheld =
                        opponents |> List.filter (fun p -> not (holds r s) && not (played r s) && opp[p, r, s] <= -1)
                    let maybes = unheld |> List.filter (fun p -> opp[p, r, s] = -1)
                    if unheld.Length < 3 then ()
                    else
                        match maybes with
                            | [ p ] ->
                                change <- true
                                opp.Update(2, p, r, s, true)
                            | [ p; q ] ->
                                change <- true
                                opp.Update(0, p, r, s, true)
                                opp.Update(0, q, r, s, true)
                            | _ -> ()

            // 6] an opponent whose hand is all suits the other two are void in
            for p in opponents do
                let hasSuit = Array.zeroCreate<bool> 4
                let k = if opp1 = p then opp2 else opp1
                let m = if opp3 = p then opp2 else opp3
                let mutable l = 0
                for s in allSuits do
                    if oppsHave[s] > 0 && table.IsVoid[k, s] && table.IsVoid[m, s] then
                        hasSuit[s] <- true
                        l <- l + oppsHave[s]
                if not oppOwnsQS then ()
                elif hasSuit[spades] then
                    if qsOwner <> p || qsOwnerCertainty < 15 then
                        change <- true
                        setQSOwner p 15
                elif l = table.HandSize[p] then
                    if qsOwner = p then
                        change <- true
                        qsOwner <- none
                        qsOwnerCertainty <- none
                        opp.Update(-2, p, queen, spades, true)
                if not hasSuit[spades] && p = qsOwner then l <- l + 1
                if l = table.HandSize[p] && opp.HandLen[p] <> l then
                    change <- true
                    for s in allSuits do
                        if hasSuit[s] then ()
                        elif qsOwner = p then
                            let k = if qsOwnerCertainty < 15 then -1 else -2
                            if s = spades then
                                opp.UpdateRun(k, p, 2, jack, spades, true)
                                opp.UpdateRun(k, p, king, ace, spades, true)
                            else opp.UpdateRun(k, p, 2, ace, s, true)
                        else opp.UpdateRun(-2, p, 2, ace, s, true)

            // 7] revise the voids and solo owners
            if change then
                for s in allSuits do
                    oppsVoidIn[s] <- oppsVoidFunct s
                    soloOwner[s] <- if oppsVoidIn[s] = 2 then anOppNotVoid else none
                    if soloOwner[s] = none then ()
                    elif opp.CardsNotHeld[soloOwner[s], s] = 13 then
                        soloOwner[s] <- none
                        anOppNotVoid <- none
                    if soloOwner[s] >= 0 then
                        for r in 2 .. ace do
                            if opp[soloOwner[s], r, s] >= -1 then opp.Update(1, soloOwner[s], r, s, true)
                        if opp.SuitLenPoss[soloOwner[s], s] <> oppsHave[s] || opp.SuitLenInf[soloOwner[s], s] <> oppsHave[s] then
                            soloOwner[s] <- none
                    setEstimatedVoids s

            // 8] an opponent known to hold no card of a suit is void in it
            for p in opponents do
                for s in allSuits do
                    if not (opp.IsVoid(p, s)) && opp.CardsNotHeld[p, s] = 13 then
                        change <- true
                        opp.SuitLenPoss[p, s] <- 0
                        opp.SuitLenInf[p, s] <- 0
                        oppsVoidIn[s] <- oppsVoidFunct s
                        soloOwner[s] <- if oppsVoidIn[s] = 2 then anOppNotVoid else none
                        noOppVoid[s] <- false

        // the one opponent who can hold Q♠ holds it (Killer Hearts asks twice, and asking can infer)
        let owner = playerOwns queen spades
        if oppOwnsQS && qsOwner = none && owner >= 0 then setQSOwner (playerOwns queen spades) 8

    member _.Table = table
    member _.Me = me
    member _.Opp1 = opp1
    member _.Opp2 = opp2
    member _.Opp3 = opp3
    member _.TLeft = tLeft
    member _.SuitCards = suitCards
    member _.SuitLen = suitLen
    member _.QSInHand = qsInHand
    member _.Opp = opp
    member _.PrivateInfo = privateInfo
    member _.PointsExposed = pointsExposed
    member _.OppOwnsQS = oppOwnsQS
    member _.AKSInHand = aksInHand
    member _.AKS = aks
    member _.PQS = pQS
    member _.OppPtCards = oppPtCards
    member _.OppsHave = oppsHave
    member _.HighIn = highIn
    member _.SecondHighIn = secondHighIn
    member _.ThirdHighIn = thirdHighIn
    member _.LowIn = lowIn
    member _.SecondLowIn = secondLowIn
    member _.OppsLowerRanks = oppsLowerRanks
    member _.OppsHigherRanks = oppsHigherRanks
    member _.PrivateVoidInSuit = privateVoidInSuit
    member _.SoloOwner = soloOwner
    member _.OppsVoidIn = oppsVoidIn
    member _.EstOppsVoidIn = estOppsVoidIn
    member _.NoOppVoid = noOppVoid
    member _.CaseSuit = caseSuit
    member _.HU = hu
    member _.EstTr = estTr
    member _.OffWinners = offWinners
    member _.OppLowIn = oppLowIn
    member _.OppHighIn = oppHighIn
    member _.LowOppCard = lowOppCard
    member _.DeadCardsBySuit = deadCardsBySuit
    member _.HighestOutcard = highestOutcard
    member _.LU = lu
    member _.DefLosers = defLosers
    member _.DefWinners = defWinners
    member _.SuitIsLive = suitIsLive
    member _.DefSuits = defSuits
    member _.SuitStopper = suitStopper
    member _.SurplusLength = surplusLength
    member _.CanLeadH with get () = canLeadH and set v = canLeadH <- v
    member _.SluffPts with get () = sluffPts and set v = sluffPts <- v
    member _.PtsSit with get () = ptsSit and set v = ptsSit <- v
    member _.Shooter with get () = shooter and set v = shooter <- v
    member _.PossOppShoot with get () = possOppShoot and set v = possOppShoot <- v
    member _.ProbOppShoot with get () = probOppShoot and set v = probOppShoot <- v
    member _.QSOwner with get () = qsOwner and set v = qsOwner <- v
    member _.QSOwnerCertainty = qsOwnerCertainty
    member _.AnOppNotVoid = anOppNotVoid
    member _.ComfortLevel = comfortLevel
    member _.LenSuitLed = lenSuitLed
    member _.HltW = hltW
    member _.PHltW = pHltW
    member _.NumberLTW = numberLTW
    member _.Sluffing = sluffing
    member _.Endgame = endgame
    member _.LongSuit = longSuit
    member _.SuitsInHand = suitsInHand
    member _.OppShooterHasPlayed with get () = oppShooterHasPlayed and set v = oppShooterHasPlayed <- v
    member _.ACaseSuit = aCaseSuit
    member _.CaseCards = caseCards
    member _.NumberOfOutSuits = numberOfOutSuits
    member _.NumberOfSingletons = numberOfSingletons
    member _.NumberOfVoids = numberOfVoids
    member _.SuitWithHU = suitWithHU
    member _.SuitWithoutHU = suitWithoutHU
    member _.SumOfHUs = sumOfHUs
    member _.TotalUnbeatables = totalUnbeatables
    member _.QSIsHU = qsIsHU
    member _.FollowersNotVoid = followersNotVoid
    member _.OnlyHasH = onlyHasH
    member _.TotalOffWinners = totalOffWinners
    member _.AllSuitsCovered = allSuitsCovered
    member _.TotalWinR = totalWinR
    member _.AllWinnersType = allWinnersType
    member _.EarlyExit = earlyExit
    member _.OppHWinners = oppHWinners
    member _.WinsAllH = winsAllH
    member _.HltQS = hltQS
    member _.HnQS = hnQS
    member _.OppWithAKS = oppWithAKS
    member _.TotalDeadCards = totalDeadCards
    member _.TotalLU = totalLU
    member _.NumberOfOutcards = numberOfOutcards
    member _.OutcardSuitCount = outcardSuitCount
    member _.SuitOfOutcard = suitOfOutcard
    member _.SuitWithLU = suitWithLU
    member _.TotalDefLosers = totalDefLosers
    member _.ALiveSuit = aLiveSuit
    member _.CDHEntries = cdhEntries
    member _.Entries = entries
    member _.EntriesClipped = entriesClipped
    member _.LiveSuits = liveSuits
    member _.ASuitStopper = aSuitStopper
    member _.CountOfSuitStoppers = countOfSuitStoppers
    member _.SluffableSuitStopper = sluffableSuitStopper
    member _.WeakShootSuit = weakShootSuit

    member this.LowerRanks(start, s) = lowerRanks start s
    member this.HigherRanks(start, s) = higherRanks start s
    member this.OppRanksBetween(r1, r2, s) = oppRanksBetween r1 r2 s
    member this.PlayerOwns(r, s) = playerOwns r s
    member this.SetQSOwner(p, c) = setQSOwner p c
    member this.UpdatePrivateInfo(code, p, r, s) = updatePrivateInfo code p r s
    member this.OppsVoidFunct(s) = oppsVoidFunct s
    member this.VoidFollowers(s) = voidFollowers s
    member this.EstTricks(s) = estTricks s
    member this.Refine() = refineOpponentsInferredHands ()
    member this.PassedTo(r, s) = passedTo r s

    /// Runs the analysis.
    member this.Run() =
        // the seat may lead hearts once they are all it has
        if not canLeadH then canLeadH <- suitLen[hearts] = tLeft

        // the seat's cards in the suit led, relative to the card winning the trick
        if position > 0 then
            lenSuitLed <- suitLen[suitLed]
            if lenSuitLed > 0 then
                let cards = suitCards[suitLed]
                let rec loop i =
                    if i < cards.Length then
                        let j = cards[i]
                        if j < w then
                            hltWQS <- j
                            if suitLed <> spades || j <> queen then
                                hltW <- j
                                pHltW <- i
                                numberLTW <- lenSuitLed - pHltW
                            else loop (i + 1)
                        else loop (i + 1)
                loop 0
            else sluffing <- true

        // who may be Shooting, as remembered from the seat's last decision
        if ptsSit = me || ptsSit = PointsSit.split then
            shooter <- none
            possOppShoot <- false
            probOppShoot <- false
        elif ptsSit >= 0 then
            if shooter >= 0 && shooter <> ptsSit then probOppShoot <- false
            shooter <- ptsSit
            possOppShoot <- true
        elif shooter = none then ()
        elif table.PointsInTrick > 0 && table.PlayerHasPlayed[shooter] && table.TrickWinner <> shooter then
            shooter <- none
            possOppShoot <- false
            probOppShoot <- false
        elif not possOppShoot then
            possOppShoot <- true
            probOppShoot <- true

        if tLeft <= 4 && pointsExposed >= 14 then endgame <- true
        elif tLeft = 3 || table.ScoreInHand[me] >= 95 then endgame <- true

        // the seat's suits
        for s in allSuits do
            let m = suitLen[s]
            oppsHave[s] <- 13 - table.CountPlayed[s] - m
            if m > 0 then
                suitsInHand <- suitsInHand + 1
                highIn[s] <- suitCards[s][0]
                lowIn[s] <- suitCards[s][m - 1]
                if m >= 2 then
                    secondHighIn[s] <- suitCards[s][1]
                    secondLowIn[s] <- suitCards[s][m - 2]
                if m >= 3 then thirdHighIn[s] <- suitCards[s][2]
                if 10 * m > 6 * tLeft then longSuit <- s
                for k in suitCards[s] do
                    oppsLowerRanks[k, s] <- lowerRanks k s
                    oppsHigherRanks[k, s] <- higherRanks k s

        // if an opponent's hand can only be suits the other opponents are void in, it is void in the rest
        let mutable change = true
        while change do
            change <- false
            for p in opponents do
                let pHas = Array.zeroCreate<int> 4
                for i in [ spades; hearts; clubs ] do
                    if table.VoidInSuit[i] + privateVoidInSuit[i] = 2 && not table.IsVoid[p, i] && suitLen[i] > 0 then
                        pHas[i] <- oppsHave[i]
                if Array.sum pHas = table.HandSize[p] then
                    for i in allSuits do
                        if pHas[i] = 0 && not table.IsVoid[p, i] && privateVoidInSuit[i] = 0 then
                            for j in 2 .. ace do
                                if played j i then ()
                                elif privateInfo[p, j, i] <> 527 then
                                    privateInfo[p, j, i] <- 527
                                    change <- true
                            privateVoidInSuit[i] <- 1

        // whether the Shooter is still Shooting after this trick so far
        if shooter >= 0 && position > 0 then
            oppShooterHasPlayed <- table.PlayerHasPlayed[shooter]
            let higher = higherRanks w suitLed
            if table.PointsInTrick = 0 then ()
            elif table.TrickWinner = shooter && lowIn[suitLed] < w then ()
            elif not oppShooterHasPlayed && higher > 0 then ()
            elif ptsSit = PointsSit.unplayed then
                shooter <- none
                possOppShoot <- false
                probOppShoot <- false
            else ptsSit <- PointsSit.split
        if ptsSit = PointsSit.split then
            shooter <- none
            possOppShoot <- false
            probOppShoot <- false

        this.InferOpponentsHands()
        this.ProcessInferences()
        if earlyExit.IsNone then this.AnalyzeSuits()

    /// Fills in the opponents' hands from what is known and inferred of each card.
    member private this.InferOpponentsHands() =
        for i in allSuits do
            for j in 2 .. ace do
                let mutable prAbsYes = 0
                let mutable puAbsYes = 0
                let mutable prInfYes = 0
                let mutable puInfYes = 0
                let mutable prAbsNo = 0
                let mutable puAbsNo = 0
                let mutable prInfNo = 0
                let mutable puInfNo = 0
                if played j i || holds j i then prAbsNo <- 15
                else
                    for p in opponents do
                        let m = privateInfo[p, j, i]
                        let n = table.PublicInfo[p, j, i]
                        let k = pPosn p
                        if table.IsVoid[p, i] then puAbsNo <- puAbsNo ||| k
                        elif m &&& 0xF0F = 1039 then prAbsYes <- prAbsYes ||| k
                        elif n &&& 0xF0F = 1039 then puAbsYes <- puAbsYes ||| k
                        elif m &&& 0xF00 = 1024 then prInfYes <- prInfYes ||| k
                        elif n &&& 0xF00 = 1024 then puInfYes <- puInfYes ||| k
                        elif m &&& 0xF0F = 527 then prAbsNo <- prAbsNo ||| k
                        elif n &&& 0xF0F = 527 then puAbsNo <- puAbsNo ||| k
                        elif m &&& 0xF00 = 512 then prInfNo <- prInfNo ||| k
                        elif n &&& 0xF00 = 512 then puInfNo <- puInfNo ||| k
                prAbsYes <- prAbsYes ||| puAbsYes
                prAbsNo <- prAbsNo ||| puAbsNo
                if prInfYes > 0 && puInfYes > 0 && prInfYes <> puInfYes then
                    // private inference outweighs public
                    puInfYes <- 0
                    puInfNo <- 0
                if prAbsYes > 0 then
                    for p in opponents do opp[p, j, i] <- -2
                    let owner = [ 0 .. 3 ] |> List.find (fun p -> pPosn p = prAbsYes)
                    opp[owner, j, i] <- 2
                    absoluteOwnersOfSuits[i] <- absoluteOwnersOfSuits[i] ||| prAbsYes
                else
                    for p in opponents do
                        let k = pPosn p
                        if prAbsNo &&& k <> 0 then opp[p, j, i] <- -2
                        elif prInfYes &&& k <> 0 then opp[p, j, i] <- 1
                        elif prInfNo &&& k <> 0 then opp[p, j, i] <- -1
                        elif puInfYes &&& k <> 0 then opp[p, j, i] <- 1
                        elif puInfNo &&& k <> 0 then opp[p, j, i] <- -1

                // an unplayed card no opponent seems to hold goes to the likeliest
                if played j i || holds j i then ()
                elif opponents |> List.exists (fun p -> opp[p, j, i] >= 0) then ()
                else
                    let stack = opponents |> List.filter (fun p -> opp[p, j, i] = -1)
                    let shooterHasSpades = shooter >= 0 && not table.IsVoid[shooter, spades]
                    match stack with
                        | [] -> ()
                        | [ p ] -> opp[p, j, i] <- 1
                        | _ when j = queen && i = spades && shooterHasSpades -> opp[shooter, queen, spades] <- 1
                        | [ p; q ] ->
                            opp[p, j, i] <- 0
                            opp[q, j, i] <- 0
                        | _ when prInfNo > 0 && puInfNo > 0 ->
                            for p in stack do
                                if p <> me && puInfNo &&& pPosn p <> 0 then opp[p, j, i] <- 0
                        | _ ->
                            let certainty p = max (Info.certainty privateInfo[p, j, i]) (Info.certainty table.PublicInfo[p, j, i])
                            let l = stack |> List.map certainty |> List.min
                            for p in stack do
                                if certainty p = l then opp[p, j, i] <- 0
        opp.CountSuitLengths()

        // an opponent whose non-void suits number exactly its cards holds all of them
        for p in opponents do
            let stack = allSuits |> List.filter (fun i -> opp.SuitLenPoss[p, i] > 0)
            let m = stack |> List.sumBy (fun i -> oppsHave[i])
            if m = table.HandSize[p] then
                let flag =
                    stack |> List.forall (fun i -> not (absoluteOwnersOfSuits[i] > 0 && absoluteOwnersOfSuits[i] <> pPosn p))
                if flag then
                    for i in stack do
                        soloOwner[i] <- p
                        for k in 2 .. ace do
                            if not (holds k i) && not (played k i) then
                                if privateInfo[p, k, i] < 1032 then
                                    privateInfo[p, k, i] <- 1032
                                    opp.Update(1, p, k, i, true)
                                for l in opponents do
                                    if l <> p && (privateInfo[l, k, i] < 520 || privateInfo[l, k, i] > 527) then
                                        privateInfo[l, k, i] <- 520
                                        opp.Update(-1, l, k, i, true)

        // inferences about Q♠
        if table.QSPlayed then ()
        elif qsInHand then
            // if the opponents have only A/K♠ left, a spade leader without them is now void
            if position > 0 && suitLed = spades && aks > 0 && oppsHave[spades] = aks then
                let mutable i = 0
                if not (holds ace spades) && not (played ace spades) then
                    opp.Update(-1, table.TrickLeader, ace, spades, true)
                    updatePrivateInfo 522 table.TrickLeader ace spades
                    i <- 1
                if aks > i then
                    opp.Update(-1, table.TrickLeader, king, spades, true)
                    updatePrivateInfo 522 table.TrickLeader king spades
        elif passedTo queen spades >= 0 then setQSOwner (passedTo queen spades) 15
        elif position > 0 && table.SuitPulls[suitLed] >= 2 && numberLTW >= 3 && suitLed <> spades then
            // an opponent leading a suit again in which this seat holds three lower cards owns Q♠
            opp.Update(1, table.TrickLeader, queen, spades, true)
        elif position = 3 && suitLed = spades && oppsHave[spades] = 1 && suitLen[spades] > 0 && table.Sluffs = 1 && canLeadH then
            let p = if table.IsVoid[opp2, spades] then opp3 else opp2
            opp.Update(1, p, queen, spades, true)
            updatePrivateInfo 1034 p queen spades
            opp.UpdateRun(-1, opp1, 2, ace, spades, true)
        elif table.FollowsWithAKSOnFirstPull = none || table.FollowsWithAKSOnFirstPull = me then ()
        elif table.PassDirection <> passRight then
            opp.Update(1, table.FollowsWithAKSOnFirstPull, queen, spades, true)
        elif table.FollowsWithAKSOnFirstPull <> opp1 then
            let p = if table.FollowsWithAKSOnFirstPull = opp2 then opp3 else opp1
            if opp[p, queen, spades] = 0 then opp.Update(-1, p, queen, spades, true)

        // delayed inferences about Q♠
        let pnls = table.PNotLeadingS
        if table.QSPlayed || (ptsSit >= 0 && table.PointsPlayed >= 8) then ()
        elif position > 0 && qsInHand && aks = 0 && suitLed <> spades && table.ScoreInHand[table.TrickLeader] = table.LowScore then
            opp.UpdateRun(-1, table.TrickLeader, 2, ace, spades, true)
        elif not sluffPts then ()
        elif me = pnls || pnls = none then ()
        elif opp.IsVoid(pnls, spades) || shooter = pnls then ()
        elif table.HasLedSpades[me] = 1 then
            // a seat that has not led spades probably has spade trouble
            let i = opp[pnls, queen, spades]
            let j = opp[pnls, king, spades]
            let k = opp[pnls, ace, spades]
            if i < 0 && j < 0 && k < 0 then opp.UpdateRun(-1, pnls, 2, ace, spades, true)
            elif j < 0 && k < 0 then opp.Update(1, pnls, queen, spades, true)
            elif i < 0 && j < 0 then opp.Update(1, pnls, ace, spades, true)
            elif i < 0 && k < 0 then opp.Update(1, pnls, king, spades, true)

        // voids and solo owners
        comfortLevel <- if table.PassDirection = hold then 7 else 11
        for i in allSuits do
            oppsVoidIn[i] <- oppsVoidFunct i
            soloOwner[i] <- none
            if oppsVoidIn[i] = 2 then
                soloOwner[i] <- anOppNotVoid
                if anOppNotVoid >= 0 then opp.UpdateRun(1, anOppNotVoid, 2, ace, i, true)
            if suitLen[i] > 0 && oppsVoidIn[i] = 2 && anOppNotVoid >= 0 then
                opp.UpdateRun(1, anOppNotVoid, 2, ace, i, true)
            setEstimatedVoids i
        refineOpponentsInferredHands ()

        // late in the deal, an opponent whose likely cards fill its hand holds no others
        if trick >= 7 && trick <= 12 then
            let mutable change = false
            for p in opponents do
                let typeTotal k =
                    seq { for s in allSuits do for r in 2 .. ace do if opp[p, r, s] = k then 1 } |> Seq.length
                if typeTotal 2 + typeTotal 1 = table.HandSize[p] && typeTotal 0 > 0 then
                    for s in allSuits do opp.UpdateRun(-1, p, 2, ace, s, false)
                    change <- true
            if change then refineOpponentsInferredHands ()

    /// Sizes up the seat's suits, and whether it must claim or play a forced card.
    member private this.ProcessInferences() =
        for i in allSuits do
            caseSuit[i] <- false
            let m = suitLen[i]
            if m > 0 then
                if m = 1 then numberOfSingletons <- numberOfSingletons + 1
                if m + table.CountPlayed[i] = 13 || oppsVoidIn[i] = 3 then
                    caseSuit[i] <- true
                    aCaseSuit <- i
                    caseCards <- caseCards + m
                elif oppsLowerRanks[lowIn[i], i] < oppsHave[i] then
                    numberOfOutSuits <- numberOfOutSuits + 1
                let rec loop j =
                    if j >= 0 then
                        let q = suitCards[i][j]
                        if oppsHigherRanks[q, i] = 0 then
                            if i <> hearts || (position = 0 && canLeadH) then suitWithHU <- i
                            hu[i] <- j + 1
                            sumOfHUs <- sumOfHUs + hu[i]
                            huPrime[i] <- hu[i]
                            if hu[i] >= oppsHave[i] then huPrime[i] <- m
                            totalUnbeatables <- totalUnbeatables + huPrime[i]
                        else
                            if position = 0 || i <> suitLed then ()
                            elif q > w && 3 - position = voidFollowers suitLed && nonHUBeatsW = 0 then nonHUBeatsW <- q
                            loop (j - 1)
                loop (m - 1)
                if hu[i] = 0 then suitWithoutHU <- i
                elif i = suitLed && highIn[i] > w then huBeatsW <- highIn[i]
            else numberOfVoids <- numberOfVoids + 1

        if qsInHand && hu[spades] > pQS then qsIsHU <- 1
        if position > 0 then followersNotVoid <- 3 - voidFollowers suitLed - position

        // who holds the points
        if table.PointsInTrick = 0 || ptsSit <> PointsSit.unplayed || position = 0 then ()
        else
            let higher = if highIn[suitLed] < w then higherRanks w suitLed else 0
            if highIn[suitLed] < w && (followersNotVoid = 0 || higher = 0) then ptsSit <- table.TrickWinner
            elif table.QSLedOnTrick then ptsSit <- table.TrickWinner
        if ptsSit >= 0 && ptsSit <> me then
            shooter <- ptsSit
            possOppShoot <- true

        let k = playerOwns queen spades
        if oppOwnsQS && k >= 0 then
            if opp[k, queen, spades] = 2 then setQSOwner k 15 else setQSOwner k 8

        if soloOwner[hearts] >= 0 && oppsHave[hearts] = table.HandSize[soloOwner[hearts]] then
            onlyHasH <- soloOwner[hearts]

        // whether an opponent is probably Shooting
        if possOppShoot then
            let cardLed = table.CardLed
            if position > 0 && soloOwner[suitLed] = shooter && shooter >= 0 && oppsHave[suitLed] = tLeft - 1 && hu[suitLed] > 0 then
                probOppShoot <- false
            elif pointsExposed >= 15 then probOppShoot <- true
            elif pointsExposed >= 8 && pointsExposed < 13 then probOppShoot <- true
            elif table.QSLedOnTrick
                && (oppsHave[spades] > aks || aks = 0 || table.VoidInSuit[spades] + privateVoidInSuit[spades] = 3) then
                probOppShoot <- true
            elif table.FollowsWithQSOnFirstPull >= 0 && table.FollowsWithQSOnFirstPull <> me then probOppShoot <- true
            elif trick >= 10 then probOppShoot <- true
            elif position = 0 || table.TrickLeader <> shooter || table.TrickWinner <> shooter then ()
            elif table.PointsPlayed >= 13 && suitLed = hearts then probOppShoot <- true
            elif pointsExposed >= 15 && w = cardLed && table.SuitPulls[suitLed] >= 3 then probOppShoot <- true
            elif table.PointsPlayed = 0 && suitLed = hearts && w = cardLed then
                // the Shooter has nothing but hearts
                if higherRanks w hearts = 0 then probOppShoot <- true
                elif lowerRanks w hearts = 0 then ()
                elif suitLen[hearts] = 0 then
                    if position = 3 then probOppShoot <- true
                elif hu[hearts] = 0 && position = 3 then probOppShoot <- true
                elif tLeft + suitLen[hearts] + position - table.Sluffs = 13
                    && oppsHigherRanks[highIn[hearts], hearts] = tLeft then
                    probOppShoot <- true
            elif table.QSLedOnTrick then probOppShoot <- true
            elif oppsVoidIn[suitLed] < 2 || highIn[suitLed] > w || pointsExposed < 8 then ()
            elif suitLed = spades then
                if table.QSPlayed && not table.QSOnTrick then probOppShoot <- true
                elif w >= queen && qsOwner <> table.TrickLeader then probOppShoot <- true
            else probOppShoot <- true

        // the seat's winners in each suit
        for i in allSuits do
            estTr[i] <- estTricks i
            if hu[i] > 0 then
                offWinners[i] <- hu[i]
                if position = 0 || table.PointsInTrick = 0 then
                    if hu[i] >= estTr[i] then offWinners[i] <- suitLen[i]
                elif suitCards[i][suitLen[i] - hu[i]] > w && followersNotVoid > 0 then
                    // the highest non-HU can win the trick with points
                    if hu[i] >= estTr[i] then offWinners[i] <- suitLen[i]
                elif hu[i] - 1 >= estTr[i] then offWinners[i] <- suitLen[i]
                totalOffWinners <- totalOffWinners + offWinners[i]
            elif oppsHave[i] > 0 && (i <> hearts || canLeadH) then allSuitsCovered <- false
        totalWinR <- float32 totalOffWinners / float32 tLeft

        // whether the seat's remaining cards win every trick
        let mutable allWinners = false
        let protectedFromPoints () =
            if hu[hearts] < oppsHave[hearts] then false
            elif table.QSPlayed || qsIsHU = 1 then true
            else hu[spades] >= oppsHave[spades]
        if ptsSit <> PointsSit.unplayed && ptsSit <> me then
            if trick >= 12 then ()
            elif position = 0 then
                if sumOfHUs = tLeft then allWinners <- true
            else
                if lenSuitLed = 0 then ()
                elif lowIn[suitLed] < w then ()
                elif sumOfHUs = tLeft then allWinners <- true
                elif sumOfHUs = tLeft - 1 && nonHUBeatsW > 0 && lenSuitLed = 1 then allWinners <- true
        elif position > 0 then
            if lenSuitLed = 0 then ()
            elif totalUnbeatables = tLeft then allWinners <- true
            elif nonHUBeatsW > 0 then
                if protectedFromPoints () then
                    allWinners <- true
                    allWinnersType <- 1
            elif huBeatsW > 0 then
                let i =
                    let i = hu[suitLed] - 1
                    if i > oppsHave[suitLed] then lenSuitLed - 1 else i
                let j = totalUnbeatables - huPrime[suitLed] + i
                if j = tLeft - 1 then allWinners <- true
                elif protectedFromPoints () then allWinners <- true
                if allWinners then allWinnersType <- 1
        else
            if totalUnbeatables = tLeft then allWinners <- true
            elif protectedFromPoints () then allWinners <- true

        if allWinners then earlyExit <- Some AllWinners
        elif lenSuitLed = 1 then earlyExit <- Some (Forced { Rank = highIn[suitLed]; Suit = suitLed })
        elif position > 0 then ()
        elif suitLen[hearts] < tLeft - 1 || canLeadH then ()
        elif qsInHand then
            // a hand of Q♠ and hearts must lead Q♠
            sluffPts <- true
            earlyExit <- Some (Forced { Rank = queen; Suit = spades })
        else
            // a hand of hearts and one other card must lead the other card
            let s = allSuits |> List.find (fun s -> suitLen[s] = 1)
            earlyExit <- Some (Forced { Rank = highIn[s]; Suit = s })

    /// Analyzes the seat's suits for defense and offense.
    member private this.AnalyzeSuits() =
        if suitLen[hearts] = 0 then winsAllH <- 0
        elif hu[hearts] >= oppsHave[hearts] then winsAllH <- 2
        elif offWinners[hearts] >= estTr[hearts] then winsAllH <- 1
        elif hu[hearts] >= 4 then winsAllH <- 1
        elif hu[hearts] = suitLen[hearts] then winsAllH <- -1
        else
            let n = min suitLen[hearts] estTr[hearts]
            winsAllH <- offWinners[hearts] - n - 10
            oppHWinners <- oppsHigherRanks[suitCards[hearts][n - 1], hearts]

        if possOppShoot && table.LedAKS >= 0 && table.LedAKS = shooter && ((qsOwner >= 0 && qsOwner <> table.LedAKS) || qsInHand) then
            probOppShoot <- true

        // the lowest and highest cards each opponent may hold in each suit
        for p in opponents do
            for i in allSuits do
                if not (opp.IsVoid(p, i)) then
                    for j in 2 .. ace do
                        if opp[p, j, i] >= 0 then
                            if oppLowIn[p, i] = 0 then oppLowIn[p, i] <- j
                            oppHighIn[p, i] <- j
        for i in allSuits do
            match seq { 2 .. ace } |> Seq.tryFind (fun j -> not (played j i) && not (holds j i)) with
                | Some j -> lowOppCard[i] <- j
                | None -> ()
        hltQS <- suitCards[spades] |> Array.tryFind (fun r -> r < queen) |> Option.defaultValue 0
        if suitLen[spades] > 0 then
            if highIn[spades] <> queen then hnQS <- highIn[spades]
            elif suitLen[spades] >= 2 then hnQS <- secondHighIn[spades]

        oppWithAKS <-
            if aks = 0 then none
            elif oppsVoidIn[spades] = 2 then soloOwner[spades]
            elif passedTo ace spades >= 0 && not (played ace spades) then passedTo ace spades
            elif passedTo king spades >= 0 && not (played king spades) then passedTo king spades
            elif playerOwns ace spades >= 0 then playerOwns ace spades
            else playerOwns king spades

        this.AnalyzeOutcards()
        this.AnalyzeDefense()
        this.AnalyzeEntries()
        this.RankProblemSuits()
        this.AnalyzeStoppers()

    /// Cards that can never win a trick (outcards, LU: lowest unbeatables).
    member private this.AnalyzeOutcards() =
        let mutable k = 20
        for i in allSuits do
            let mutable flag = false
            if suitLen[i] = 0 then ()
            elif caseSuit[i] then ()
            elif position = 0 && i = hearts && not canLeadH then ()
            elif position > 0 && i = suitLed && lowIn[i] > w then ()
            elif oppsLowerRanks[lowIn[i], i] = 0 then
                flag <- true
                outcardSuitCount <- outcardSuitCount + 1
                let p = suitLen[i] - oppsHave[i] - (if position > 0 then 1 else 0)
                deadCardsBySuit[i] <- max 0 p
                totalDeadCards <- totalDeadCards + deadCardsBySuit[i]
                let rec loop j =
                    if j >= deadCardsBySuit[i] then
                        let l = suitCards[i][j]
                        if position > 0 && i = suitLed && l > w && oppsLowerRanks[l, i] > 0 then ()
                        elif oppsLowerRanks[l, i] = 0 then
                            highestOutcard[i] <- l
                            lu[i] <- lu[i] + 1
                            totalLU <- totalLU + 1
                            numberOfOutcards <- numberOfOutcards + 1
                            loop (j - 1)
                loop (suitLen[i] - 1)
                if i = spades && qsInHand && oppsLowerRanks[queen, spades] = 0 then highestOutcard[spades] <- queen
            elif oppsLowerRanks[lowIn[i], i] <= 2 - estOppsVoidIn[i] then
                // nearly an outcard
                flag <- true
                outcardSuitCount <- outcardSuitCount + 1
                highestOutcard[i] <- lowIn[i]
                numberOfOutcards <- numberOfOutcards + 1

            if not flag then ()
            elif suitOfOutcard = none then
                suitOfOutcard <- i
                k <- oppsHave[i]
            elif lu[suitOfOutcard] = 1 && suitLen[suitOfOutcard] = 1 then
                if lu[i] = 1 && suitLen[i] = 1 && oppsHave[i] <= k then
                    suitOfOutcard <- i
                    k <- oppsHave[i]
            elif lu[i] >= lu[suitOfOutcard] && oppsHave[i] <= k then
                suitOfOutcard <- i
                k <- oppsHave[i]
            elif position = 0 then
                if suitOfOutcard <> spades || not qsInHand || oppsHave[spades] < suitLen[spades] then ()
                elif i <> hearts || canLeadH then
                    suitOfOutcard <- i
                    k <- oppsHave[i]
            elif sluffing && qsInHand && suitOfOutcard = spades then
                suitOfOutcard <- i
                k <- oppsHave[i]

            if lu[i] = 0 then ()
            elif suitWithLU = none then suitWithLU <- i
            elif lu[i] > lu[suitWithLU] then suitWithLU <- i
            elif lu[i] = lu[suitWithLU] && i = hearts then suitWithLU <- i

    /// Defensive winners and losers in each suit: cards likely to win, or to lose, a trick.
    member private this.AnalyzeDefense() =
        for i in allSuits do
            let l = suitLen[i]
            if l > 0 then
                let mutable k = oppsHave[i]
                let mutable n = estOppsVoidIn[i]
                let p = min estTr[i] l
                let mutable m = 0
                for j in l - 1 .. -1 .. l - p do
                    if k < 3 * (oppsLowerRanks[suitCards[i][j], i] - (m + 1) / 2) || j < hu[i] then
                        if defWinners[i] = 0 then defLosers[i] <- min p (l - 1 - j)
                        defWinners[i] <- defWinners[i] + 1
                    m <- min oppsHave[i] (m + 3 - n)
                    k <- max 0 (k - (3 - n))
                    if n = 0 && m >= 3 then n <- 1
                    elif n = 1 && m >= 6 then n <- 2
                if defWinners[i] = 0 && not caseSuit[i] then defLosers[i] <- p
                let q = defLosers[i]
                if q > 0 then
                    if soloOwner[i] >= 0 then defLosers[i] <- min lu[i] oppsHave[i]
                    if position = 0 || oppsHave[i] > followersNotVoid then
                        totalDefLosers <- totalDefLosers + defLosers[i]

    /// Entries: the seat's cards that can lose a trick, so that the seat need not win it.
    member private this.AnalyzeEntries() =
        for s in allSuits do
            let mutable l = suitLen[s]
            let mutable p = 0
            let mutable skip = false
            if l = 0 || l = lu[s] + deadCardsBySuit[s] then ()
            elif caseSuit[s] then
                if position = 0 || s <> suitLed then ()
                elif lowIn[s] > w then
                    entries <- entries + 1
                    if s <> spades then cdhEntries <- cdhEntries + 1
                skip <- true
            elif position = 0 || sluffing then
                let mutable q = 0
                for r in l - lu[s] - 1 .. -1 .. deadCardsBySuit[s] do
                    let k = suitCards[s][r]
                    let j = oppRanksBetween 1 k s
                    if j - lu[s] - q <= 0 then p <- p + 1
                    q <- q + 1
            elif s <> suitLed then
                let rec loop r =
                    if r >= deadCardsBySuit[s] then
                        let k = suitCards[s][r]
                        let j = oppRanksBetween 1 k s
                        if j = 1 && r = l - 1 && lu[s] = 0 then loop (r - 1)
                        elif j - p <= 1 then
                            p <- p + 1
                            loop (r - 1)
                loop (l - lu[s] - 1)
            else
                p <- -lu[s]
                let mutable j = 1
                let rec loop r =
                    if r >= deadCardsBySuit[s] then
                        let k = suitCards[s][r]
                        if k = hltWQS then loop (r - 1)
                        elif suitCards[s][l - 1] > w && k = highIn[s] then loop (r - 1)
                        else
                            j <- oppsNextHigherCard j s
                            if j <> none then
                                if k < j then p <- p + 1
                                loop (r - 1)
                loop (l - 1)
                if suitCards[s][l - 1] > w && p > 0 then p <- p - 1
                l <- l - 1
            if not skip then
                entriesClipped <- entriesClipped + p
                let m = l - lu[s] - deadCardsBySuit[s] - p
                entries <- entries + m
                if s <> spades then cdhEntries <- cdhEntries + m
                if m > 0 then
                    suitIsLive[s] <- true
                    liveSuits <- liveSuits + 1
                    aLiveSuit <- s

    /// Sorts the problem suits, worst first, into DefSuits (DefSuits[0] is their number). A problem
    /// suit is not a case suit, has a defensive winner, and has at most one defensive loser.
    member private this.RankProblemSuits() =
        for i in allSuits do
            let m = defWinners[i]
            if m > 0 && not caseSuit[i] && defLosers[i] <= 1 then
                let goesBefore p =
                    if m = suitLen[i] then
                        if defWinners[p] < suitLen[p] then true
                        elif sluffing && totalDefLosers = 1 && (i = hearts || p = hearts) then i = hearts
                        elif m < defWinners[p] then true
                        else m = defWinners[p] && oppsLowerRanks[lowIn[i], i] > oppsLowerRanks[lowIn[p], p]
                    elif suitLen[p] = defWinners[p] then false
                    elif suitLen[i] < suitLen[p] then true
                    elif suitLen[i] > suitLen[p] then false
                    elif defWinners[i] > defWinners[p] then true
                    elif defWinners[i] = defWinners[p] then
                        let mutable l = oppsLowerRanks[lowIn[i], i]
                        let mutable q = oppsLowerRanks[lowIn[p], p]
                        if l * q > 0 && l > q then true
                        else
                            if l > 0 && float oppsHigherRanks[lowIn[i], i] / float l >= 5.0 then l <- 0
                            if q > 0 && float oppsHigherRanks[lowIn[p], p] / float q >= 5.0 then q <- 0
                            if l > q then true
                            elif l < q || p = hearts then false
                            else i = hearts
                    else false
                let k =
                    seq { 1 .. defSuits[0] }
                        |> Seq.tryFind (fun j -> goesBefore defSuits[j])
                        |> Option.defaultValue (defSuits[0] + 1)
                for j in defSuits[0] .. -1 .. k do defSuits[j + 1] <- defSuits[j]
                defSuits[0] <- defSuits[0] + 1
                defSuits[k] <- i

    /// Suits that stop a Shoot, and the weakest suit for a Shoot.
    member private this.AnalyzeStoppers() =
        for i in allSuits do
            suitStopper[i] <- false
            surplusLength[i] <- none
            if suitLen[i] = 0 || caseSuit[i] then ()
            else
                let k = if i = suitLed && w > highIn[i] then 1 else 0
                let j = suitLen[i] - k - oppsHigherRanks[highIn[i], i]
                if j > 0 && oppsHave[i] > oppsHigherRanks[highIn[i], i] then
                    suitStopper[i] <- true
                    aSuitStopper <- i
                    countOfSuitStoppers <- countOfSuitStoppers + 1
                    surplusLength[i] <- j - 1 - k
                    if surplusLength[i] <= 0 then ()
                    elif sluffableSuitStopper = none then sluffableSuitStopper <- i
                    elif defLosers[i] < defLosers[sluffableSuitStopper] then sluffableSuitStopper <- i
                    elif defLosers[i] = defLosers[sluffableSuitStopper] && surplusLength[i] > surplusLength[sluffableSuitStopper] then
                        sluffableSuitStopper <- i

        let mutable j = 100
        let mutable l = 0
        for i in diamonds .. -1 .. spades do
            if suitLen[i] = 0 then ()
            elif offWinners[i] = suitLen[i] || offWinners[i] >= 2 then ()
            elif position = 0 && i = spades && (hltQS = 0 || oppsHave[spades] < 3) then ()
            elif position = 0 && i = spades && oppsHave[spades] = 1 then ()
            elif position = 0 && i = hearts && not canLeadH then ()
            elif i = spades && suitLen[spades] = 1 && qsInHand && not sluffPts then ()
            elif offWinners[i] < j then
                j <- offWinners[i]
                l <- estTr[i] - offWinners[i]
                weakShootSuit <- i
            elif offWinners[i] = j && estTr[i] - offWinners[i] > l then
                j <- offWinners[i]
                l <- estTr[i] - offWinners[i]
                weakShootSuit <- i
            elif offWinners[i] = j && estTr[i] - offWinners[i] = l
                && oppsHigherRanks[lowIn[i], i] > oppsHigherRanks[lowIn[weakShootSuit], weakShootSuit] then
                weakShootSuit <- i
