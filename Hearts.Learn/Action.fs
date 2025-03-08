namespace Hearts.Learn

open System

open PlayingCards
open Hearts
open Hearts.Model

/// An action is either a pass (during the exchange) or
/// a play (after the exchange).
type ActionType = Pass | Play

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

    /// What actions can be taken in the given deal?
    let legalActions hand deal exchangeOpt =
        match exchangeOpt with
            | Some exchange
                when not (Exchange.isComplete exchange) ->
                assert(
                    deal.ExchangeDirection
                        <> ExchangeDirection.Hold)
                assert(
                    let pass =
                        let passer =
                            Exchange.currentPasser exchange
                        exchange.PassMap[passer]
                    Set.intersect hand pass |> Set.isEmpty)
                Pass, Seq.toArray hand   // pass any card in hand
            | _ ->
                let legalPlays =
                    ClosedDeal.legalPlays hand deal
                Play, Seq.toArray legalPlays

    /// Takes the given action in the given deal.
    let addAction actionType action deal =
        match actionType with

            | Pass ->

                let deal = OpenDeal.addPass action deal

                    // start play?
                let canStartPlay =
                    deal.ExchangeOpt
                        |> Option.map Exchange.isComplete
                        |> Option.defaultValue true   // no exchange
                if canStartPlay then OpenDeal.startPlay deal
                else deal

            | Play -> OpenDeal.addPlay action deal
