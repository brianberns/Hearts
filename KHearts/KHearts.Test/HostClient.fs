/// Checks the port against the original program: plays Client to Killer Hearts running as Host, with
/// the port's exact table following along, and compares every decision the Host's players make with
/// the port's. The Client's own seats play by a policy that doesn't depend on the Host's decisions.
///
/// Every message is written to a transcript, which `replay` checks again offline.
module KHearts.Test.HostClient

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Threading

open KHearts

/// Six fields: key, then five values.
type Message = int[]

type Link =
    {
        Read : unit -> Message
        Write : Message -> unit
    }

/// The shared file of Killer Hearts' Host-Client mode: one 55-byte record, which the Host and the
/// Client take turns overwriting.
module SharedFile =

    let private recordLength = 55

    let private fields (s : string) =
        [| for j in 1 .. 6 -> Int32.Parse(s.Substring(8 * j - 4, 7).Trim()) |]

    let private open' path =
        File.OpenHandle(path, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite)

    /// A link through the shared file.
    let link path =
        let handle = open' path
        let buffer = Array.zeroCreate<byte> recordLength
        let readText () =
            let n = RandomAccess.Read(handle, Span buffer, 0L)
            Encoding.ASCII.GetString(buffer, 0, n)

        // as the original Client does, render a previous session's leftover record harmless, unless a
        // Host is already waiting
        let text = readText ()
        if not (text.StartsWith "HCs" && text.EndsWith "HCe" && (fields text)[0] = 1) then
            let harmless = "xxx,0000000,".PadRight(recordLength)
            RandomAccess.Write(handle, ReadOnlySpan(Encoding.ASCII.GetBytes harmless), 0L)
        let read () =
            let watch = Stopwatch.StartNew()
            let rec loop () =
                let text = readText ()
                // the Host's write isn't atomic: accept its record only once it holds still
                if text.Length = recordLength && text.StartsWith "HCs" && text.EndsWith "HCe"
                    && (Thread.Yield() |> ignore; readText () = text) then
                    fields text
                else
                    // the Host answers within a millisecond while it plays; it may also be thinking
                    if watch.ElapsedMilliseconds < 20L then Thread.Yield() |> ignore
                    else Thread.Sleep(1)
                    loop ()
            loop ()
        let write (message : Message) =
            let text =
                "CHs," + String.Join(",", message |> Array.map (sprintf "%-7d")) + ",CHe"
            assert(text.Length = recordLength)
            RandomAccess.Write(handle, ReadOnlySpan(Encoding.ASCII.GetBytes text), 0L)
        { Read = read; Write = write }

/// How the Client plays its seats.
type ClientPolicy =
    {
        /// Chooses a pass, given the seat's hand and Killer Hearts' pass for it.
        Pass : int[] -> int[] -> int[]

        /// Chooses a card, given the legal cards and the port's choice.
        Play : int[] -> int -> int
    }

module ClientPolicy =

    /// Random legal cards.
    let random (rng : Random) =
        let choose (cards : int[]) n = cards |> Array.sortBy (fun _ -> rng.Next()) |> Array.take n
        {
            Pass = fun hand _ -> choose hand 3
            Play = fun legal _ -> (choose legal 1)[0]
        }

    /// Killer Hearts' own choices, from the port.
    let killerHearts =
        {
            Pass = fun _ khPass -> khPass
            Play = fun _ khCard -> khCard
        }

/// Statistics of a session.
type Tally() =
    member val Games = 0 with get, set
    member val ScoreMismatches = 0 with get, set
    member val Passes = 0 with get, set
    member val PassMismatches = 0 with get, set
    member val Plays = 0 with get, set
    member val PlayMismatches = 0 with get, set
    member val Claims = 0 with get, set
    member val ClaimMismatches = 0 with get, set
    member val Deals = 0 with get, set
    member val DealsWithMismatch = 0 with get, set
    member val Notes = ResizeArray<string>() with get

    member tally.Report() =
        $"{tally.Games} games, {tally.Deals} deals; " +
        $"passes {tally.Passes - tally.PassMismatches}/{tally.Passes} agree; " +
        $"plays {tally.Plays - tally.PlayMismatches}/{tally.Plays} agree; " +
        $"claims {tally.Claims - tally.ClaimMismatches}/{tally.Claims} agree; " +
        $"final scores {tally.Games - tally.ScoreMismatches}/{tally.Games} agree; " +
        $"deals with any mismatch: {tally.DealsWithMismatch}"

let private cardText card = string (KillerHearts.FromKh card)

/// When set, receives the features of every Host decision to sluff, for finding patterns in mismatches.
let mutable featureLog : TextWriter = null

/// Runs a session against the Host. `progress` hears of each finished game; the deal numbered
/// `traceDeal` is printed in full.
let run (link : Link) (policy : ClientPolicy) (transcript : TextWriter) (progress : Tally -> unit) traceDeal =
    let engine = KhEngine()
    let tally = Tally()
    let mutable clientSeats = Set.empty
    let mutable dealer = 0
    let mutable direction = 0
    let mutable gameScore = Array.zeroCreate<int> 4
    let mutable firstDeal = true
    let hands = Array.create 4 Array.empty<int>
    let mutable handsReceived = 0
    let mutable dealMismatch = false
    let mutable finished = false

    let note (text : string) =
        if tally.Notes.Count < 5000 then tally.Notes.Add($"game {tally.Games + 1}, deal {tally.Deals}: {text}")
        dealMismatch <- true

    let answer (message : Message) (response : Message) =
        transcript.WriteLine(String.Join(" ", message) + " | " + String.Join(" ", response))
        link.Write response

    let trace (text : string) = if tally.Deals = traceDeal then printfn "%s" text
    let cardsText (cards : int[]) = cards |> Array.map cardText |> String.concat " "

    let ack (message : Message) =
        answer message [| message[0] + 100; -1; -1; -1; -1; -1 |]

    let bits (bitmap : int) suit =
        [| for rank in 2 .. 14 do
            if bitmap &&& (1 <<< rank) <> 0 then yield 4 * (rank - 2) + suit |]

    /// Killer Hearts' pass for a seat, from a table of its own: the Host computes passes only for its
    /// own seats, so asking the shared table would disturb its state.
    let khPass seat =
        let table = KhEngine()
        table.StartObservedPassing(seat, dealer, direction, gameScore, hands[seat], firstDeal)
        table.ComputePass()

    while not finished do
        let m = link.Read()
        match m[0] with
            | 1 ->                                      // session start
                if m[5] <> 0 then failwith "The two of clubs must lead"
                clientSeats <- set [ for seat in 0 .. 3 do if m[1] &&& (1 <<< seat) <> 0 then yield seat ]
                ack m
            | 2 ->                                      // game start
                dealer <- m[1]
                ack m
            | 3 ->                                      // first deal of a game
                gameScore <- m[1..4]
                direction <- m[5]
                firstDeal <- true
                ack m
            | 4 ->                                      // a hand
                hands[m[1]] <- [| for suit in 0 .. 3 do yield! bits m[2 + suit] suit |]
                handsReceived <- handsReceived + 1
                if handsReceived = 4 then
                    handsReceived <- 0
                    tally.Deals <- tally.Deals + 1
                    dealMismatch <- false
                    engine.StartDeal(dealer, direction, gameScore, Array.copy hands, firstDeal)
                    trace $"deal {tally.Deals}: dealer {dealer}, direction {direction}, score %A{gameScore}, first {firstDeal}"
                    for seat in 0 .. 3 do trace $"  seat {seat}: {cardsText hands[seat]}"
                ack m
            | 5 ->                                      // a pass
                let passer = m[1]
                if engine.Passer <> passer then note $"passer {passer}, engine expects {engine.Passer}"
                if clientSeats.Contains passer then
                    let cards = policy.Pass (engine.HandOf passer) (khPass passer)
                    trace $"pass {passer} (Client): {cardsText cards}"
                    engine.ApplyPass(cards, false)
                    answer m [| m[0] + 100; cards[0]; cards[1]; cards[2]; -1; -1 |]
                else
                    let host = m[3..5]
                    let mine = engine.ComputePass()
                    tally.Passes <- tally.Passes + 1
                    let agree = Set.ofArray mine = Set.ofArray host
                    if not agree then
                        tally.PassMismatches <- tally.PassMismatches + 1
                        note $"seat {passer} passes {host |> Array.map cardText}, port {mine |> Array.map cardText}"
                    trace $"pass {passer}: {cardsText host}; port {cardsText mine}"
                    engine.ApplyPass(host, agree)
                    ack m
            | 6 | 7 | 9 -> ack m                        // pass received, trick start, trick end
            | 8 ->                                      // a play
                let seat = m[1]
                if engine.CurrentPlayer <> seat then note $"player {seat}, engine expects {engine.CurrentPlayer}"
                let mine = engine.ComputePlay()         // the Host decides for every seat, Client's too
                if engine.Claimed then
                    // the Host would have ended the hand instead
                    tally.Claims <- tally.Claims + 1
                    tally.ClaimMismatches <- tally.ClaimMismatches + 1
                    note $"trick {engine.TrickNumber}: port claims for seat {seat}; Host plays on"
                if clientSeats.Contains seat then
                    let card = policy.Play (engine.LegalCards()) mine
                    trace $"T{engine.TrickNumber}.{engine.Seq} seat {seat} (Client) holds {cardsText (engine.HandOf seat)}: plays {cardText card}"
                    engine.ApplyPlay card
                    answer m [| m[0] + 100; card; -1; -1; -1; -1 |]
                else
                    let host = m[2]
                    tally.Plays <- tally.Plays + 1
                    if mine <> host && not engine.Claimed then
                        tally.PlayMismatches <- tally.PlayMismatches + 1
                        let kind =
                            if engine.Seq = 0 then "lead"
                            elif engine.HandOf seat |> Array.exists (fun c -> c % 4 = engine.SuitLed) then "follow"
                            else "sluff"
                        note $"trick {engine.TrickNumber}.{engine.Seq} {kind} mode {engine.Mode}: seat {seat} plays {cardText host}, port {cardText mine}"
                    if not (isNull featureLog) && engine.Seq > 0
                        && not (engine.HandOf seat |> Array.exists (fun c -> c % 4 = engine.SuitLed)) then
                        featureLog.WriteLine($"{(if mine = host then 1 else 0)},{host % 4},{mine % 4},{engine.Features()}")
                    let mark = if mine <> host then "   <<<<<<\n      " + engine.DebugState() else ""
                    trace $"T{engine.TrickNumber}.{engine.Seq} seat {seat} holds {cardsText (engine.HandOf seat)}: plays {cardText host}, port {cardText mine} (mode {engine.Mode}){mark}"
                    engine.ApplyPlay host
                    ack m
            | 10 ->                                     // the hand ends
                // Unless all points are played or the game is decided, the current player claimed
                let claimed = engine.PointsPlayed < 26 && not engine.GameIsOver
                if claimed then
                    let seat = engine.CurrentPlayer
                    engine.ComputePlay() |> ignore
                    tally.Claims <- tally.Claims + 1
                    if not engine.Claimed then
                        tally.ClaimMismatches <- tally.ClaimMismatches + 1
                        note $"trick {engine.TrickNumber}: Host claims for seat {seat}; port does not"
                gameScore <- engine.FinishHand(claimed)
                firstDeal <- false
                dealer <- (dealer + 1) % 4
                direction <- (direction + 1) % 4
                if dealMismatch then tally.DealsWithMismatch <- tally.DealsWithMismatch + 1
                ack m
            | 11 ->                                     // game end
                let host = m[1..4]
                if host <> gameScore then
                    tally.ScoreMismatches <- tally.ScoreMismatches + 1
                    note $"final score %A{host}, port %A{gameScore}"
                tally.Games <- tally.Games + 1
                progress tally
                ack m
            | 12 ->                                     // session end: no answer
                transcript.WriteLine(String.Join(" ", m) + " |")
                finished <- true
            | key -> failwith $"Unexpected key {key}"
    tally

/// Replays a transcript: the Host's messages, and the Client's recorded answers.
let replay path traceDeal =
    let lines = File.ReadAllLines(path)
    let mutable i = 0
    let mutable lastAnswer = Array.empty<int>
    let parse (s : string) =
        s.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse
    let link =
        {
            Read = fun () ->
                if i < lines.Length then
                    let parts = lines[i].Split('|')
                    lastAnswer <- parse parts[1]
                    i <- i + 1
                    parse parts[0]
                else [| 12; -1; -1; -1; -1; -1 |]       // the session was cut short
            Write = ignore
        }
    let policy =
        {
            Pass = fun _ _ -> lastAnswer[1 .. 3]
            Play = fun _ _ -> lastAnswer[1]
        }
    run link policy TextWriter.Null ignore traceDeal
