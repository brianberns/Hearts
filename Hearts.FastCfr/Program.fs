namespace Hearts.FastCfr

open System
open System.IO

open FastCfr
open Hearts
open PlayingCards
open MathNet.Numerics.LinearAlgebra

module Program =

    let train numGames chunkSize =
        let rng = Random(0)
        let gameChunks =
            Seq.init numGames (fun iGame ->
                let deck = Deck.shuffle rng
                let dealer = enum<Seat> (iGame % Seat.numSeats)
                OpenDeal.fromDeck
                    dealer
                    ExchangeDirection.Hold
                    deck
                    |> OpenDeal.startPlay
                    |> GameState.create)
                |> Seq.chunkBySize chunkSize
                |> Seq.mapi (fun iChunk chunk ->
                    printfn $"Chunk {iChunk}"
                    chunk)
        Trainer.train (rng.Next()) gameChunks

    let run () =

            // train
        let numGames = 100_000_000
        let chunkSize = 1_000_000
        let util, infoSetMap = train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"
        printfn $"# of info sets: {infoSetMap.Count}"

        printfn ""
        let strategyMap =
            infoSetMap
                |> Map.map (fun _ infoSet ->
                    InformationSet.getAverageStrategy infoSet)
        Strategy.save strategyMap

    Console.OutputEncoding <- Text.Encoding.UTF8
    run ()
