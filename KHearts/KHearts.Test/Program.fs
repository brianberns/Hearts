/// Checks of the Killer Hearts port.
///
///    smoke <deals>    plays Killer Hearts against the Claude heuristic in Hearts' own tournament
///    agree <deals>    compares the one-seat player with the exact four-seat table, decision by decision
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
            let infoSet = OpenDeal.currentInfoSet deal
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
                    loop (OpenDeal.addPlay card deal) passQueue
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
            let infoSet = OpenDeal.currentInfoSet deal
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
                    loop (OpenDeal.addPlay (KillerHearts.FromKh card) deal) passQueue
    loop deal []

[<EntryPoint>]
let main argv =
    match argv with
        | [| "smoke"; deals |] ->
            let payoff =
                Tournament.run 0 true (int deals) Claude.player (KillerHearts.CreatePlayer())
            printfn $"Killer Hearts vs Claude heuristic: payoff per seat-deal %+.4f{payoff}"
            0
        | [| "agree"; deals |] ->
            let results =
                OpenDeal.generate (Random(0))
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
            for deal in OpenDeal.generate (Random(2)) |> Seq.take (int deals) do
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
            eprintfn "usage: KHearts.Test smoke <deals> | agree <deals> | client <shared file> <transcript> kh|random | replay <transcript> [deal to trace] | reference <deals> <file> | features <transcript> <csv>"
            1
