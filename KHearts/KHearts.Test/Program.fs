/// Checks of the Killer Hearts port.
///
///    smoke <games>    plays Killer Hearts against three Claude heuristics in Hearts' own tournament
///    gamescore <games>
///                     plays Killer Hearts that sees the game score against three that don't, and vice versa
///    agree <deals>    compares the one-seat player with the exact four-seat table, decision by decision
///    fsport <deals>   compares the F# port of Killer Hearts (Hearts.Heuristic/Killer) with the
///                     one-seat player, at every pass and play decision of Killer Hearts self-play
///    client <shared file> <transcript> kh|random
///                     plays Client to the original program as Host, and compares the Host's decisions
///                     with the port's (see HostClient.fs)
///    replay <transcript> [deal]
///                     checks a Client session's transcript again, printing one deal in full
///    reference <deals> <file>
///                     records one-seat decisions on an exact table, for checking another port (see referenceDeal)
///    features <transcript> <csv>
///                     records the port's state at each of the Host's sluffs, for finding patterns in mismatches
module KHearts.Test.Program

open System

open PlayingCards
open Hearts
open Hearts.Heuristic
open KHearts

/// Generates an infinite sequence of deals, rotating dealer and exchange direction, from the given RNG. This is
/// the sequence the Hearts library's OpenDeal.generate produced, so results compare with earlier runs.
let generateDeals (rng : Random) =
    Seq.initInfinite (fun iDeal ->
        let dir = enum<ExchangeDirection> (iDeal % ExchangeDirection.numDirections)
        let deck = Deck.shuffle rng
        let dealer = enum<Seat> (iDeal % Seat.numSeats)
        OpenDeal.fromDeck dealer dir deck)

/// The current player's information set in the given deal, played as if it were the first deal of a game.
let currentInfoSet deal =
    deal |> Game.createFirst |> Game.currentInfoSet Rules.standard

/// Plays one deal on an exact table, with Killer Hearts in every seat, alongside Hearts' own deal.
/// At each decision, asks the one-seat player too, and counts how often the two agree.
let agreeOnDeal (deal : OpenDeal) =
    let table = KhEngine()
    let dealer = int deal.ClosedDeal.Dealer
    let dir = int deal.ClosedDeal.ExchangeDirection
    let hands =
        [| for seat in Enum.getValues<Seat> ->
            deal.UnplayedCardMap[seat] |> Seq.map KillerHearts.ToKh |> Seq.toArray |]
    table.StartDeal(dealer, dir, Array.zeroCreate 4, hands, true)

    let mutable agree = 0
    let mutable differ = 0
    let mutable claims = 0
    let mutable passDiffer = 0
    let rec loop (deal : OpenDeal) (passQueue : Card list) =
        if not (ClosedDeal.isComplete deal.ClosedDeal) then
            let infoSet = currentInfoSet deal
            let seat = int infoSet.Player
            match infoSet.LegalActionType with
                | ActionType.Pass ->
                        // the table chooses each seat's three cards at once
                    let queue =
                        if passQueue.IsEmpty then
                            if table.Passer <> seat then failwith $"Passer mismatch: {table.Passer} vs {seat}"
                            let pass = table.ComputePass()
                            table.ApplyPass(pass, true)
                            pass |> Seq.map KillerHearts.FromKh |> Seq.toList
                        else passQueue
                    let card = queue.Head
                    let adapter = KillerHearts.Act(infoSet, Array.zeroCreate 4)
                    if adapter = card then agree <- agree + 1
                    else
                        differ <- differ + 1
                        passDiffer <- passDiffer + 1
                    loop (OpenDeal.addPass card deal) queue.Tail
                | ActionType.Play ->
                    if table.CurrentPlayer <> seat then failwith $"Player mismatch: {table.CurrentPlayer} vs {seat}"
                    let card = table.ComputePlay() |> KillerHearts.FromKh
                    if table.Claimed then claims <- claims + 1
                    if not (infoSet.LegalActions |> Array.contains card) then
                        failwith $"Table chose illegal {card} for {infoSet.Player}"
                    let adapter = KillerHearts.Act(infoSet, Array.zeroCreate 4)
                    if infoSet.LegalActions.Length > 1 then
                        if adapter = card then agree <- agree + 1 else differ <- differ + 1
                    table.ApplyPlay(KillerHearts.ToKh card)
                    loop (OpenDeal.addPlay Rules.standard card deal) passQueue
    loop deal []
    agree, differ, claims, passDiffer

/// Plays one deal on an exact table, as agreeOnDeal does but with a random game score, and records each
/// decision with the one-seat player's answer, for checking another port of the one-seat player:
///
///    D <dealer> <direction> <4 scores> <13 cards dealt to each seat, west first>
///    P <seat> <card the table passes> <one-seat player's card>
///    X <seat> <card the table plays> <one-seat player's card> <number of legal cards>
///
/// Cards and seats are Killer Hearts': see Table.cs.
let referenceDeal (rng : Random) (writer : IO.TextWriter) (deal : OpenDeal) =
    let table = KhEngine()
    let dealer = int deal.ClosedDeal.Dealer
    let dir = int deal.ClosedDeal.ExchangeDirection
    let gameScore =
        if rng.Next(4) = 0 then Array.zeroCreate 4
        else Array.init 4 (fun _ -> rng.Next(100))
    let hands =
        [| for seat in Enum.getValues<Seat> ->
            deal.UnplayedCardMap[seat] |> Seq.map KillerHearts.ToKh |> Seq.toArray |]
    table.StartDeal(dealer, dir, gameScore, hands, gameScore |> Array.forall ((=) 0))
    let numbers (xs : seq<int>) = String.Join(" ", xs)
    writer.WriteLine($"D {dealer} {dir} {numbers gameScore} {numbers (Seq.concat hands)}")
    let rec loop (deal : OpenDeal) (passQueue : Card list) =
        if not (ClosedDeal.isComplete deal.ClosedDeal) then
            let infoSet = currentInfoSet deal
            let seat = int infoSet.Player
            let answer = KillerHearts.Act(infoSet, gameScore) |> KillerHearts.ToKh
            match infoSet.LegalActionType with
                | ActionType.Pass ->
                    let queue =
                        if passQueue.IsEmpty then
                            let pass = table.ComputePass()
                            table.ApplyPass(pass, true)
                            pass |> Seq.map KillerHearts.FromKh |> Seq.toList
                        else passQueue
                    let card = queue.Head
                    writer.WriteLine($"P {seat} {KillerHearts.ToKh card} {answer}")
                    loop (OpenDeal.addPass card deal) queue.Tail
                | ActionType.Play ->
                    let card = table.ComputePlay()
                    writer.WriteLine($"X {seat} {card} {answer} {infoSet.LegalActions.Length}")
                    table.ApplyPlay(card)
                    loop (OpenDeal.addPlay Rules.standard (KillerHearts.FromKh card) deal) passQueue
    loop deal []

/// How often the F# port of Killer Hearts chooses the same card as the one-seat player.
type PortAgreement =
    {
        PassAgree : int
        PassDiffer : int
        PlayAgree : int
        PlayDiffer : int
    }

/// Plays one deal with Killer Hearts in every seat, and counts how often the F# port of Killer Hearts
/// chooses the same card, at each pass, and at each play decision with a choice.
let agreeWithFSharpPort (kh : Player) (deal : OpenDeal) =
    let rec loop deal counts =
        let infoSet = currentInfoSet deal
        let action =
            match Seq.tryExactlyOne infoSet.LegalActions with
                | Some action -> action
                | None -> kh.Act infoSet
        let counts =
            match infoSet.LegalActionType with
                | ActionType.Pass ->
                    if KillerEngine.Pass.pass KillerEngine.Pass.Bold infoSet = action then { counts with PassAgree = counts.PassAgree + 1 }
                    else { counts with PassDiffer = counts.PassDiffer + 1 }
                | ActionType.Play when infoSet.LegalActions.Length > 1 ->
                    if KillerEngine.Play.play infoSet = action then { counts with PlayAgree = counts.PlayAgree + 1 }
                    else { counts with PlayDiffer = counts.PlayDiffer + 1 }
                | _ -> counts
        let deal = OpenDeal.addAction infoSet.Rules infoSet.LegalActionType action deal
        match Game.tryUpdateScore Rules.arena.MoonShotScoring (Game.createFirst deal) with
            | Some _ -> counts
            | None -> loop deal counts
    loop deal { PassAgree = 0; PassDiffer = 0; PlayAgree = 0; PlayDiffer = 0 }

/// Plays whole games under arena rules with score-aware Killer Hearts in every seat, and counts how often
/// the F# port of Killer Hearts, which sees the same game score, chooses the same card. Answers the counts
/// and the first few information sets where the two differ.
let agreeWithFSharpPortInGames numGames =
    let kh = KillerHearts.CreatePlayer()
    let passAgree, passDiffer, playAgree, playDiffer = ref 0, ref 0, ref 0, ref 0
    let mismatches = Collections.Concurrent.ConcurrentQueue<InformationSet * Card * Card * string>()
    let player =
        let act (infoSet : InformationSet) =
            let action = kh.Act infoSet
            let port, agree, differ =
                match infoSet.LegalActionType with
                    | ActionType.Pass -> KillerEngine.Pass.pass KillerEngine.Pass.Bold infoSet, passAgree, passDiffer
                    | ActionType.Play -> KillerEngine.Play.play infoSet, playAgree, playDiffer
            if port = action then Threading.Interlocked.Increment(&agree.contents) |> ignore
            else
                Threading.Interlocked.Increment(&differ.contents) |> ignore
                if mismatches.Count < 1000 then
                    let trace =
                        match infoSet.LegalActionType with
                            | ActionType.Play ->
                                let score = KillerHearts.EngineScores(infoSet.GameScore.Points)
                                $"
    KH:   {KillerHearts.TracePlay(infoSet, score)}
    port: {KillerEngine.Play.lastExplanation ()}"
                            | ActionType.Pass ->
                                let score = KillerHearts.EngineScores(infoSet.GameScore.Points)
                                $"
    KH:   {KillerHearts.TracePass(infoSet, score)}
    port: {KillerEngine.Pass.lastExplanation ()}"
                    mismatches.Enqueue(infoSet, action, port, trace)
            action
        { Act = act }
    let playerMap = Enum.getValues<Seat> |> Seq.map (fun seat -> seat, player) |> Map
    Array.Parallel.init numGames (fun iGame ->
        let rng = SplitMixRandom(SplitMixRandom.GetSeed(0, iGame))
        Game.playGame Rules.arena rng playerMap (enum<Seat> (iGame % Seat.numSeats)))
        |> ignore
    { PassAgree = passAgree.Value; PassDiffer = passDiffer.Value; PlayAgree = playAgree.Value; PlayDiffer = playDiffer.Value },
    Seq.toArray mismatches

/// Describes a decision where the F# port differs from Killer Hearts.
let describeMismatch (infoSet : InformationSet, action : Card, port : Card, trace : string) =
    let deal = infoSet.Deal
    let cards (cs : seq<Card>) = cs |> Seq.map string |> String.concat " "
    let tricks = deal.CompletedTricks.Length + 1
    let score = infoSet.GameScore.Points |> Seq.map string |> String.concat ","
    $"{infoSet.LegalActionType} seat {infoSet.Player} score {score} "
        + $"dealer {deal.Dealer} dir {deal.ExchangeDirection} trick {tricks}: KH {action}, port {port}; hand {cards infoSet.Hand}; "
        + $"legal {cards infoSet.LegalActions}; hearts broken {deal.HeartsBroken}; "
        + $"played {cards (Seq.rev deal.CompletedTricks |> Seq.collect (fun t -> Seq.rev t.Cards))}{trace}"

/// Claude as it was before it saw the game score: Killer Hearts' play as if every score were zero, under
/// Killer Hearts' own moon rule, shooting from the cautious pass whatever the score.
let scoreBlindClaude =
    let blind (infoSet : InformationSet) = { infoSet with GameScore = Score.zero }
    let act (infoSet : InformationSet) =
        match infoSet.LegalActionType with
            | ActionType.Pass -> KillerEngine.Pass.pass KillerEngine.Pass.Cautious (blind infoSet)
            | ActionType.Play -> KillerEngine.Play.play (blind infoSet)
    { Act = act }

/// Plays whole games under arena rules with the given challenger at South and the given champion
/// elsewhere, and counts the challenger's moon shots, and those that lost it the game.
let losingMoons = Collections.Concurrent.ConcurrentQueue<string>()

let countMoons numGames (champion : Player) (challenger : Player) =
    let rules = Rules.arena
    let playerMap =
        Enum.getValues<Seat> |> Seq.map (fun seat -> seat, (if seat = Seat.South then challenger else champion)) |> Map
    let results =
        Array.Parallel.init numGames (fun iGame ->
            let rng = SplitMixRandom(SplitMixRandom.GetSeed(1, iGame))
            let rec playDeal game =
                let infoSet = Game.currentInfoSet rules game
                let action =
                    match Seq.tryExactlyOne infoSet.LegalActions with
                        | Some action -> action
                        | None -> playerMap[infoSet.Player].Act infoSet
                let game = Game.addAction rules infoSet.LegalActionType action game
                match Game.tryUpdateScore rules.MoonShotScoring game with
                    | Some game -> game
                    | None -> playDeal game
            let rec loop (game : Game) moons losing =
                let before = game.Score.Points
                let dealt = game.Deal.UnplayedCardMap[Seat.South]
                let game = playDeal game
                let delta = Array.map2 (-) game.Score.Points before
                let moon = delta[int Seat.South] = 0 && delta |> Array.sum = 3 * ClosedDeal.numPointsPerDeal
                let winners = GameScore.findGameWinners rules.TieGame game.Score
                let moons = if moon then moons + 1 else moons
                let lost = moon && not winners.IsEmpty && not (winners.Contains Seat.South)
                if lost then
                    let cards (cs : seq<Card>) = cs |> Seq.map string |> String.concat " "
                    let deal = game.Deal
                    let passes =
                        match deal.ExchangeOpt with
                            | Some exchange -> exchange.PassMap |> Map.toSeq |> Seq.map (fun (s, cs) -> $"{s}:{cards cs}") |> String.concat " "
                            | None -> "hold"
                    let tricks =
                        deal.ClosedDeal.CompletedTricks
                            |> List.rev
                            |> List.mapi (fun i trick ->
                                let plays = Trick.plays trick |> Seq.map (fun (s, c) -> $"{s}:{c}") |> String.concat " "
                                $"    {i + 1,2}. {plays}  -> {Trick.highPlayerOpt trick |> Option.get}")
                            |> String.concat "\n"
                    losingMoons.Enqueue(
                        $"game {iGame} score before {String.Join(',', before)} dealer {deal.ClosedDeal.Dealer} dir {deal.ClosedDeal.ExchangeDirection}\n"
                            + $"  South dealt {cards dealt}\n  passes {passes}\n{tricks}")
                let losing = if lost then losing + 1 else losing
                if not winners.IsEmpty then moons, losing
                else
                    let deal =
                        let dealer = Seat.incr 1 game.Deal.ClosedDeal.Dealer
                        let dir = ExchangeDirection.next game.Deal.ClosedDeal.ExchangeDirection
                        Deck.shuffle rng |> OpenDeal.fromDeck dealer dir
                    loop (Game.create deal game.Score) moons losing
            let deal = Deck.shuffle rng |> OpenDeal.fromDeck (enum<Seat> (iGame % Seat.numSeats)) ExchangeDirection.Left
            loop (Game.createFirst deal) 0 0)
    results |> Array.sumBy fst, results |> Array.sumBy snd

[<EntryPoint>]
let main argv =
    match argv with
        | [| "quirks"; games |] ->
            let games = int games
            let kh = KillerHearts.CreatePlayer()
            let fixedClaude, quirkyClaude = Killer.playerWith true, Killer.playerWith false
            let report name (rate : float32) = printfn $"%-58s{name} %.4f{rate}"
            report "Fixed Claude vs three quirky Claude:" (Tournament.run 0 games quirkyClaude fixedClaude)
            report "Quirky Claude vs three fixed Claude:" (Tournament.run 0 games fixedClaude quirkyClaude)
            report "Fixed Claude vs three Killer Hearts:" (Tournament.run 0 games kh fixedClaude)
            report "Quirky Claude vs three Killer Hearts:" (Tournament.run 0 games kh quirkyClaude)
            0
        | [| "daring"; games |] ->
            let games = int games
            let kh = KillerHearts.CreatePlayer()
            let cautious, bold = Killer.create true KillerEngine.Pass.Cautious, Killer.create true KillerEngine.Pass.Bold
            let report name (rate : float32) = printfn $"%-58s{name} %.4f{rate}"
            report "Bold Claude vs three cautious Claude:" (Tournament.run 0 games cautious bold)
            report "Cautious Claude vs three bold Claude:" (Tournament.run 0 games bold cautious)
            report "Bold Claude vs three Killer Hearts:" (Tournament.run 0 games kh bold)
            report "Cautious Claude vs three Killer Hearts:" (Tournament.run 0 games kh cautious)
            0
        | [| "moons"; games |] ->
            let moons, losing = countMoons (int games) (KillerHearts.CreatePlayer()) Killer.player
            printfn $"Claude vs three Killer Hearts, {games} games: {moons} moons, {losing} of them lost the game"
            for s in losingMoons do printfn "%s" s
            0
        | [| "claude"; games |] ->
            let games = int games
            let kh = KillerHearts.CreatePlayer()
            let report name (rate : float32) = printfn $"%-58s{name} %.4f{rate}"
            report "Claude vs three score-blind Claude:" (Tournament.run 0 games scoreBlindClaude Killer.player)
            report "Score-blind Claude vs three Claude:" (Tournament.run 0 games Killer.player scoreBlindClaude)
            report "Claude vs three score-aware Killer Hearts:" (Tournament.run 0 games kh Killer.player)
            report "Score-blind Claude vs three score-aware Killer Hearts:" (Tournament.run 0 games kh scoreBlindClaude)
            for name, challenger in [ "Claude", Killer.player; "Score-blind Claude", scoreBlindClaude ] do
                let moons, losing = countMoons games kh challenger
                printfn $"{name} vs three Killer Hearts, {games} games: {moons} moons, {losing} of them lost the game"
            0
        | [| "fsgames"; games |] | [| "fsgames"; games; _ |] ->
            let counts, mismatches = agreeWithFSharpPortInGames (int games)
            let report name agree differ =
                printfn $"{agree + differ} {name}: the F# port agrees in {agree} (%.4f{100.0 * float agree / float (agree + differ)}%%)"
            report "passes" counts.PassAgree counts.PassDiffer
            report "play decisions" counts.PlayAgree counts.PlayDiffer
            let shown = if argv.Length > 2 then int argv[2] else 10
            for m in Seq.truncate shown mismatches do printfn "  %s" (describeMismatch m)
            0
        | [| "fsport"; deals |] ->
            let kh = KillerHearts.CreatePlayer()
            let results =
                generateDeals (Random(0))
                    |> Seq.take (int deals)
                    |> Seq.toArray
                    |> Array.Parallel.map (agreeWithFSharpPort kh)
            let report name agree differ =
                printfn $"{agree + differ} {name}: the F# port agrees in {agree} (%.4f{100.0 * float agree / float (agree + differ)}%%)"
            report "passes" (results |> Array.sumBy _.PassAgree) (results |> Array.sumBy _.PassDiffer)
            report "play decisions with a choice" (results |> Array.sumBy _.PlayAgree) (results |> Array.sumBy _.PlayDiffer)
            0
        | [| "smoke"; games |] ->
            let winRate =
                Tournament.run 0 (int games) Killer.player (KillerHearts.CreatePlayer())
            printfn $"Killer Hearts vs three Claude heuristics: wins %.4f{winRate} of games (0.25 is even)"
            0
        | [| "gamescore"; games |] ->
            let aware = KillerHearts.CreatePlayer()
            let blind = KillerHearts.CreateScoreBlindPlayer()
            let awareRate = Tournament.run 0 (int games) blind aware
            printfn $"Score-aware Killer Hearts vs three score-blind: wins %.4f{awareRate} of games (0.25 is even)"
            let blindRate = Tournament.run 0 (int games) aware blind
            printfn $"Score-blind Killer Hearts vs three score-aware: wins %.4f{blindRate} of games (0.25 is even)"
            0
        | [| "agree"; deals |] ->
            let results =
                generateDeals (Random(0))
                    |> Seq.take (int deals)
                    |> Seq.toArray
                    |> Array.Parallel.map agreeOnDeal
            let agree = results |> Array.sumBy (fun (a, _, _, _) -> a)
            let differ = results |> Array.sumBy (fun (_, d, _, _) -> d)
            let claims = results |> Array.sumBy (fun (_, _, c, _) -> c)
            let passDiffer = results |> Array.sumBy (fun (_, _, _, p) -> p)
            printfn $"{agree + differ} decisions with a choice: one-seat player agrees with the exact table in {agree} (%.2f{100.0 * float agree / float (agree + differ)}%%)"
            printfn $"differences: {differ} ({passDiffer} passing, {differ - passDiffer} playing); claims: {claims}"
            0
        | [| "client"; sharedFile; transcriptPath; policyName |] ->
            let policy =
                match policyName with
                    | "kh" -> HostClient.ClientPolicy.killerHearts
                    | "random" -> HostClient.ClientPolicy.random (Random(0))
                    | _ -> failwith $"Unknown policy {policyName}"
            use transcript = new IO.StreamWriter(transcriptPath, AutoFlush = true)
            let link = HostClient.SharedFile.link sharedFile
            printfn "Waiting for the Host..."
            let progress (tally : HostClient.Tally) =
                printfn $"{DateTime.Now:T} {tally.Report()}"
            let tally = HostClient.run link policy transcript progress 0
            printfn "%s" (tally.Report())
            for n in tally.Notes do printfn "  %s" n
            0
        | [| "reference"; deals; path |] ->
            use writer = new IO.StreamWriter(path: string)
            let rng = Random(1)
            for deal in generateDeals (Random(2)) |> Seq.take (int deals) do
                referenceDeal rng writer deal
            0
        | [| "features"; transcriptPath; csvPath |] ->
            use csv = new IO.StreamWriter(csvPath)
            csv.WriteLine("agree,host_suit,port_suit," + KhEngine.FeatureHeader)
            HostClient.featureLog <- csv
            let tally = HostClient.replay transcriptPath 0
            printfn "%s" (tally.Report())
            0
        | [| "replay"; transcriptPath |] | [| "replay"; transcriptPath; _ |] ->
            let traceDeal = if argv.Length > 2 then int argv[2] else 0
            let tally = HostClient.replay transcriptPath traceDeal
            printfn "%s" (tally.Report())
            for n in tally.Notes do printfn "  %s" n
            0
        | _ ->
            eprintfn "usage: KHearts.Test fsport <deals> | fsgames <games> [shown] | claude <games> | quirks <games> | daring <games> | moons <games> | smoke <games> | gamescore <games> | agree <deals> | client <shared file> <transcript> kh|random | replay <transcript> [deal to trace] | reference <deals> <file> | features <transcript> <csv>"
            1
