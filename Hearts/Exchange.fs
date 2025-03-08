namespace Hearts

open PlayingCards

/// All cards passed in a deal.
type Exchange =
    {
        /// Player whose turn it is to pass, if the exchange
        /// is not yet complete.
        CurrentPasserOpt : Option<Seat>

        /// Passes made by each player so far.
        PassMap : Map<Seat, Pass>
    }

module Exchange =

    /// Creates an exchange
    let create currentPasser =
        {
            CurrentPasserOpt = Some currentPasser
            PassMap = Map [ currentPasser, Pass.empty ]
        }

    /// An exchange is complete when all players have passed the
    /// full number of cards.
    let isComplete exchange =
        assert(
            exchange.CurrentPasserOpt.IsSome
                || (exchange.PassMap.Count = Seat.numSeats
                    && exchange.PassMap.Values
                        |> Seq.forall (fun pass ->
                            pass.Count = Pass.numCards)))
        exchange.CurrentPasserOpt.IsNone

    /// Whose turn is it to pass cards in the given incomplete
    /// exchange?
    let currentPasser exchange =
        assert(isComplete exchange |> not)
        match exchange.CurrentPasserOpt with
            | Some passer -> passer
            | None -> failwith "Exchange is complete"

    /// Adds the given card to the given exchange.
    let addPass card exchange =
        assert(isComplete exchange |> not)

            // get current pass
        let curPasser = currentPasser exchange
        let curPass = exchange.PassMap[curPasser]
        assert(curPass.Count < Pass.numCards)

            // add given card to current pass
        let curPass = Pass.add card curPass
        assert(curPass.Count <= Pass.numCards)

            // update current pass in pass map
        let passMap =
            exchange.PassMap
                |> Map.add curPasser curPass
        assert(passMap.Count <= Seat.numSeats)

            // update exchange
        let curPasserOpt, passMap =

                // current pass continues?
            if curPass.Count < Pass.numCards then
                Some curPasser, passMap

                // start a new pass?
            elif passMap.Count < Seat.numSeats then
                let curPasser = Seat.next curPasser
                Some curPasser,
                passMap |> Map.add curPasser Pass.empty

                // exchange is complete
            else
                assert(curPass.Count = Pass.numCards)
                None, passMap

        {
            CurrentPasserOpt = curPasserOpt
            PassMap = passMap
        }

    let getPassOpts player dir exchange =
        let outOpt =
            exchange.PassMap |> Map.tryFind player
        let inOpt =
            let sender =
                ExchangeDirection.unapply player dir
            exchange.PassMap |> Map.tryFind sender
        outOpt, inOpt
