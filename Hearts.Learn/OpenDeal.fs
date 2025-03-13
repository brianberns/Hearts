namespace Hearts.Learn

open System

open PlayingCards
open Hearts

module OpenDeal =

    /// Plays the given number of deals in parallel.
    let generate (rng : Random) numDeals playFun =

        let map =
#if DEBUG
            Array.map
#else
                // controlling max degree of parallelism seems to be necessary when running a PyTorch model on the CPU
            Array.mapParallel settings.MaxDegreeOfParallelism
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

    /// Takes the given action in the given deal.
    let addAction actionType action deal =
        match actionType with

            | ActionType.Pass ->

                    // add pass to deal
                let deal = OpenDeal.addPass action deal

                    // start play?
                let canStartPlay =
                    deal.ExchangeOpt
                        |> Option.map Exchange.isComplete
                        |> Option.defaultValue true   // no exchange
                if canStartPlay then
                    OpenDeal.startPlay deal
                else deal

            | ActionType.Play ->
                OpenDeal.addPlay action deal
