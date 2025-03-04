namespace Hearts.Learn

open System

open PlayingCards
open Hearts

/// A move is either a pass (during the exchange) or
/// a play (after the exchange).
type MoveType = Pass | Play

module ClosedDeal =

    /// What moves can be made from the given hand?
    let legalMoves hand exchange deal =
        match exchange.CurrentPassOpt with
            | Some pass ->
                assert(Exchange.isComplete exchange |> not)
                let legalPasses = Set.difference hand pass
                Pass, Seq.toArray legalPasses
            | None ->
                assert(Exchange.isComplete exchange)
                let legalPlays = ClosedDeal.legalPlays hand deal
                Play, Seq.toArray legalPlays

module OpenDeal =

    open Hearts.Model

    /// Plays the given number of deals in parallel.
    let generate (rng : Random) numDeals playFun =

        let map =
#if DEBUG
            Array.map
#else
                // controlling max degree of parallelism seems to be necessary when running a PyTorch model on the CPU
            Array.mapParallel Environment.ProcessorCount
#endif

        Array.init numDeals (fun iDeal ->
            let deck = Deck.shuffle rng
            let dealer =
                enum<Seat> (iDeal % Seat.numSeats)
            let dir =
                enum<ExchangeDirection>
                    (iDeal % ExchangeDirection.numDirections)
            deck, dealer, dir)
            |> map (fun (deck, dealer, dir) ->
                OpenDeal.fromDeck
                    dealer dir deck
                    |> playFun)

    /// Makes the given move in the given deal.
    let addMove moveType move deal =
        let addCard =
            match moveType with
                | Pass -> OpenDeal.addPass
                | Play -> OpenDeal.addPlay
        addCard move deal
