namespace Hearts.Experiments

open System
open System.Diagnostics
open System.IO

open PlayingCards
open Hearts
open Hearts.Model

/// Data for W, the chance that each seat goes on to win the game from the start of a deal: whole games
/// under arena rules with the given model in every seat, recording at the start of each deal the four game
/// scores and the pass direction, and the game's eventual winner.
module WData =

    let private rules = Rules.arena

    /// Plays one game, answering the state at the start of each deal and the winner.
    let private playGame (makePlayer : uint64 -> Player) gameSeed =
        let player = makePlayer (SplitMixRandom.GetChildSeed(gameSeed, 0))
        let rng = SplitMixRandom(gameSeed)
        let createDeal dealer dir = Deck.shuffle rng |> OpenDeal.fromDeck dealer dir

        let rec playDeal (game : Game) =
            let infoSet = Game.currentInfoSet rules game
            let card = Player.act infoSet player
            let game = Game.addAction rules infoSet.LegalActionType card game
            match Game.tryUpdateScore rules.MoonShotScoring game with
                | Some game -> game
                | None -> playDeal game

        let rec loop states (game : Game) =
            let closed = game.Deal.ClosedDeal
            let states = (game.Score, closed.ExchangeDirection) :: states
            let game = playDeal game
            if GameScore.isOver rules.TieGame game.Score then
                List.rev states, GameScore.findGameWinners rules.TieGame game.Score |> Seq.exactlyOne
            else
                let deal = createDeal (Seat.incr 1 closed.Dealer) (ExchangeDirection.next closed.ExchangeDirection)
                loop states { game with Deal = deal }

        createDeal Seat.South ExchangeDirection.Left   // games always start with a Left pass
            |> Game.createFirst
            |> loop []

    /// Writes the data for the given number of games, one line per deal: the game scores of West, North,
    /// East and South at the start of the deal, its pass direction (0 left, 1 right, 2 across, 3 hold),
    /// and the game's winner (0 West to 3 South). Games are played in chunks, each written as it finishes,
    /// on at most one thread per core: one long parallel loop over every game grew by gigabytes an hour.
    let run modelPath rngSeed numGames (outPath : string) =
        use model = Players.loadModel modelPath
        let watch = Stopwatch.StartNew()
        let chunkSize = 1000
        let options = Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = Environment.ProcessorCount)
        use writer = new StreamWriter(outPath)
        writer.WriteLine("west,north,east,south,direction,winner")
        let numDeals =
            Seq.fold (fun numDeals iChunk ->
                let first = iChunk * chunkSize
                let count = min chunkSize (numGames - first)
                let games : (_ list * Seat)[] = Array.zeroCreate count
                Threading.Tasks.Parallel.For(0, count, options, fun i ->
                    games[i] <- playGame (Strategy.createPlayerSeeded model) (SplitMixRandom.GetSeed(rngSeed, first + i)))
                    |> ignore
                for states, winner in games do
                    for score, dir in states do
                        let points = score.Points
                        writer.WriteLine($"{points[0]},{points[1]},{points[2]},{points[3]},{int dir},{int winner}")
                writer.Flush()
                use proc = Process.GetCurrentProcess()
                printfn $"{first + count} games, %.0f{watch.Elapsed.TotalMinutes} min, %.2f{float proc.PrivateMemorySize64 / 1e9} GB, {proc.Threads.Count} threads"
                numDeals + (games |> Array.sumBy (fst >> List.length))) 0 (seq { 0 .. (numGames - 1) / chunkSize })
        printfn $"{numGames} games, {numDeals} deals (%.1f{float numDeals / float numGames} a game), %.0f{watch.Elapsed.TotalMinutes} min"
