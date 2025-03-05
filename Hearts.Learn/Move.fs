namespace Hearts.Learn

open System

open PlayingCards
open Hearts
open Hearts.Model

/// A move is either a pass (during the exchange) or
/// a play (after the exchange).
type MoveType = Pass | Play


module OpenDeal =

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
                let deal =
                    let deal = OpenDeal.fromDeck dealer dir deck
                    if dir = ExchangeDirection.Hold then   // can start play immediately?
                        OpenDeal.startPlay deal
                    else deal
                playFun deal)

    /// What moves can be made from the given hand?
    let legalMoves hand deal =
        match deal.ExchangeOpt with
            | Some exchange
                when not (Exchange.isComplete exchange) ->
                assert(
                    deal.ClosedDeal.ExchangeDirection
                        <> ExchangeDirection.Hold)
                assert(
                    let pass =
                        let passer =
                            Exchange.currentPasser exchange
                        exchange.PassMap[passer]
                    Set.intersect hand pass |> Set.isEmpty)
                Pass, Seq.toArray hand
            | _ ->
                let legalPlays =
                    ClosedDeal.legalPlays hand deal.ClosedDeal
                Play, Seq.toArray legalPlays

    /// Makes the given move in the given deal.
    let addMove moveType move deal =
        match moveType with

            | Pass ->

                let deal = OpenDeal.addPass move deal

                    // start play?
                let canStartPlay =
                    deal.ExchangeOpt
                        |> Option.map Exchange.isComplete
                        |> Option.defaultValue true   // no exchange
                if canStartPlay then OpenDeal.startPlay deal
                else deal

            | Play -> OpenDeal.addPlay move deal
