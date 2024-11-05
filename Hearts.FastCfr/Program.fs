namespace Hearts.FastCfr

open System
open FastCfr
open Hearts
open PlayingCards

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

    let isSeat (seat : Seat) (infoSetKey : byte[]) =
        let leaderIdx = Card.allCards.Length
        let leader = infoSetKey[leaderIdx] |> int |> enum<Seat> 
        let nPlayed =
            infoSetKey
                |> Seq.skip (leaderIdx + 1)
                |> Seq.take (2 * (Seat.numSeats - 1))
                |> Seq.chunkBySize 2
                |> Seq.where (fun bytes ->
                    bytes <> [| Byte.MaxValue; Byte.MaxValue |])
                |> Seq.map (fun bytes ->
                    let rank = bytes[0] |> int |> enum<Rank>
                    let suit = bytes[1] |> int |> enum<Suit>
                    assert(rank >= Rank.Nine && rank <= Rank.King)
                    assert(int suit < Suit.numSuits)
                    Card(rank, suit))
                |> Seq.length
        Seat.incr nPlayed leader = seat

    let run () =

            // train
        let numGames = 10_000_000
        let chunkSize = 100_000
        let util, infoSetMap = train numGames chunkSize

            // expected overall utility
        printfn $"Average game value for first player: %0.5f{util}\n"
        printfn $"# of info sets: {infoSetMap.Count}"

        printfn ""
        let strategyMap =
            infoSetMap
                |> Map.toSeq
                |> Seq.where (fst >> isSeat Seat.South)
                |> Seq.map (fun (key, infoSet) ->
                    let strategy =
                        InformationSet.getAverageStrategy infoSet
                    key, strategy.ToArray())
                |> Map
        Strategy.save strategyMap

    Console.OutputEncoding <- Text.Encoding.UTF8
    printfn $"Server garbage collection: {Runtime.GCSettings.IsServerGC}\n"
    run ()
