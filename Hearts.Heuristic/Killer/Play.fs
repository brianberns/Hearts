namespace Hearts.Heuristic.Killer

open PlayingCards
open Hearts

/// Killer Hearts' play decision, ported: the card a seat plays, and what the decision leaves behind
/// for the seat's later decisions.
module Play =

    /// Can the seat play the card, under the rules Killer Hearts' table enforces?
    let private isLegal (t : Table) (c : KCard) =
        let held = [ for s in allSuits do for r in 2 .. ace do if t.Hand[r, s] then { Rank = r; Suit = s } ]
        if not t.Hand[c.Rank, c.Suit] then false
        elif t.Seq = 0 then
            if t.TwoCLeads then c.Rank = 2 && c.Suit = clubs
            else c.Suit <> hearts || t.CanLeadH || held |> List.forall (fun h -> h.Suit = hearts)
        elif held |> List.exists (fun h -> h.Suit = t.SuitLed) then c.Suit = t.SuitLed
        else c.Points = 0 || t.SluffPts || held |> List.forall (fun h -> h.Points > 0)

    /// The seat's highest legal card (the first suit's, among equals), which the seat plays when it
    /// claims the remaining tricks or once every point is played.
    let private highestLegalCard (t : Table) =
        let mutable best = None
        for s in allSuits do
            for r in 2 .. ace do
                let c = { Rank = r; Suit = s }
                if isLegal t c && (best |> Option.forall (fun (b : KCard) -> r > b.Rank)) then best <- Some c
        best.Value

    /// What the seat remembers before deciding.
    let private memoryOf (t : Table) =
        {
            Mode = t.Mode
            ImScore = Array.copy t.ImScore
            LowScore = t.LowScore
            HighScore = t.HighScore
            LeadOver2ndLow = t.LeadOver2ndLow
            CanLeadH = t.CanLeadH
            SluffPts = t.SluffPts
            Shooter = t.ArcShooter[t.Me]
            PossOppShoot = t.ArcPossOppShoot[t.Me]
            ProbOppShoot = t.ArcProbOppShoot[t.Me]
            ShootTry = t.ShootTry
            PrivateInfo = Array3D.init 4 15 4 (fun p r s -> t.PrivateInfo[p, r, s])
        }

    /// How the decision was made, for diagnosis.
    type Path =
        | NoChoice
        | Claim
        | Forced
        | Scripted of Mode * sit : int

    /// The seat's card at the table, how it was chosen, and what the decision leaves behind.
    let decide (t : Table) : KCard * Path * Memory =
        let memory = memoryOf t
        let hand = [ for s in allSuits do for r in ace .. -1 .. 2 do if t.Hand[r, s] then { Rank = r; Suit = s } ]
        let lenH = hand |> List.filter (fun c -> c.Suit = hearts) |> List.length
        let qsInHand = t.Hand[queen, spades]
        if t.PointsPlayed = 26 then highestLegalCard t, NoChoice, memory
        elif t.Trick = 13 then hand.Head, NoChoice, memory
        elif lenH = 13 then highestLegalCard t, Claim, { memory with SluffPts = true }
        elif t.Seq = 0 && t.TwoCLeads then { Rank = 2; Suit = clubs }, NoChoice, memory
        else
            let a = Analysis(t)
            if lenH = 14 - t.Trick - 1 && qsInHand then a.SluffPts <- true
            a.Run()
            let analyzed = { memory with CanLeadH = a.CanLeadH; SluffPts = a.SluffPts; PrivateInfo = a.PrivateInfo }
            match a.EarlyExit with
                | Some AllWinners -> highestLegalCard t, Claim, analyzed
                | Some (EarlyExit.Forced c) -> c, Forced, analyzed
                | None ->
                    let d = Decision(a)
                    if t.Seq = 0 then
                        d.ComputeLeadingParameters()
                        d.FinishLeading()
                    elif a.LenSuitLed > 0 then d.ComputeFollowingParameters()
                    else d.ComputeSluffingParameters()
                    d.ComputeQSPoison()
                    let decided =
                        { analyzed with
                            Mode = d.Mode
                            ImScore = Array.copy d.ImScore
                            LowScore = d.LowScore
                            HighScore = d.HighScore
                            LeadOver2ndLow = d.LeadOver2ndLow
                            Shooter = a.Shooter
                            PossOppShoot = a.PossOppShoot
                            ProbOppShoot = a.ProbOppShoot
                            ShootTry = d.ShootTry }
                    let sit = Scripts.situation d
                    let card = Select.card d sit
                    let path = Scripted (d.Mode, sit)
                    card, path, decided

    /// The table for a seat's view of a deal, replayed with the seat's own decisions at its turns.
    let replay (me : KSeat) (dealer : KSeat) (direction : int) (held : KCard[]) (passed : KCard[]) (received : KCard[])
               (plays : (KSeat * KCard) list) =
        let playedBy p = plays |> List.filter (fst >> (=) p) |> List.map snd |> List.toArray
        let t = Table(me, dealer, direction, held, passed, received, playedBy)
        for seat, card in plays do
            if seat = me then
                let _, _, memory = decide t
                t.Remember(memory)
            t.Play(card)
        t

    /// Every card played in the deal so far, in order, with its player.
    let private plays (deal : ClosedDeal) =
        let tricks =
            [ yield! List.rev deal.CompletedTricks
              match deal.CurrentTrickOpt with
                | Some trick -> yield trick
                | None -> () ]
        [ for trick in tricks do
            let leader = int trick.Leader
            for i, card in List.indexed (List.rev trick.Cards) ->
                (leader + i) % 4, KCard.Of card ]

    /// Killer Hearts' card for the information set. Like Killer Hearts' own table, it replays the deal
    /// from the seat's point of view, deciding again at each of the seat's earlier turns, for those
    /// decisions leave behind what the seat suspects of a Shoot, among other things.
    let play (infoSet : InformationSet) =
        let me = int infoSet.Player
        let plays = plays infoSet.Deal
        let mine = plays |> List.filter (fst >> (=) me) |> List.map snd
        let held = Array.append (infoSet.Hand |> Seq.map KCard.Of |> Seq.toArray) (List.toArray mine)
        let cards (passOpt : Option<Set<Card>>) =
            passOpt |> Option.map (Seq.map KCard.Of >> Seq.toArray) |> Option.defaultValue [||]
        let deal = infoSet.Deal
        let t =
            replay me (int deal.Dealer) (int deal.ExchangeDirection) held
                (cards infoSet.OutgoingPassOpt) (cards infoSet.IncomingPassOpt) plays
        let card, _, _ = decide t
        card.ToCard()
