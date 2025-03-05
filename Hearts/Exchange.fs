namespace Hearts

open PlayingCards

/// Cards passed from one player to another.
type Pass = Set<Card>

module Pass =

    /// Number of cards passed by each player.
    let numCards = 3

    let empty : Pass = Set.empty

    let isComplete (pass : Pass) =
        assert(pass.Count <= numCards)
        pass.Count = numCards

    let add card (pass : Pass) : Pass =
        assert(pass.Count < numCards)
        assert(pass.Contains(card) |> not)
        pass.Add(card)

/// All cards passed.
type Exchange =
    {
        /// Current deal.
        Deal : ClosedDeal

        /// Current pass, if any.
        CurrentPassOpt : Option<Pass>

        /// Complete passes in reverse chronological order.
        CompletePasses : List<Pass>
    }

    /// Player who dealt this deal.
    member this.Dealer = this.Deal.Dealer

    /// Card exchange direction.
    member this.ExchangeDirection = this.Deal.ExchangeDirection

module Exchange =

    /// Creates an empty exchange.
    let create deal =
        {
            Deal = deal
            CurrentPassOpt =
                if deal.ExchangeDirection = ExchangeDirection.Hold then None
                else Some Pass.empty
            CompletePasses = List.empty
        }

    /// No exchange?
    let isHold (exchange : Exchange) =
        exchange.ExchangeDirection = ExchangeDirection.Hold

    /// An exchange is complete when it is a hold hand, or all
    /// players have passed the full number of cards.
    let isComplete exchange =
        assert(
            isHold exchange
                || exchange.CurrentPassOpt.IsSome
                || exchange.CompletePasses.Length = Seat.numSeats)
        assert(
            exchange.CompletePasses
                |> Seq.forall (fun pass ->
                    pass.Count = Pass.numCards))
        exchange.CurrentPassOpt.IsNone

    /// Whose turn is it to pass cards in the given exchange?
    let currentPasser exchange =
        assert(isHold exchange |> not)
        assert(isComplete exchange |> not)
        exchange.Dealer
            |> Seat.incr (exchange.CompletePasses.Length + 1)   // dealer passes last

    /// Adds the given card to the given exchange.
    let addPass card exchange =
        assert(isHold exchange |> not)
        assert(isComplete exchange |> not)

            // get current pass
        let curPass =
            match exchange.CurrentPassOpt with
                | Some curPass ->
                    assert(curPass.Count < Pass.numCards)
                    curPass
                | None -> failwith "Unexpected"

            // add given card to pass
        let curPassOpt, completePasses =
            let curPass = Pass.add card curPass
            assert(curPass.Count <= Pass.numCards)
            if curPass.Count = Pass.numCards then
                let completePasses =
                    curPass :: exchange.CompletePasses
                let curPassOpt =
                    if completePasses.Length < Seat.numSeats then
                        Some Pass.empty
                    else None
                curPassOpt, completePasses
            else
                Some curPass, exchange.CompletePasses

        {
            exchange with
                CurrentPassOpt = curPassOpt
                CompletePasses = completePasses
        }

    /// Cards passed by each player in the given exchange, in
    /// chronological order.
    let seatPasses exchange =
        assert(isHold exchange |> not)
        assert(isComplete exchange)
        let seats = exchange.Dealer.Next |> Seat.cycle
        let passes = exchange.CompletePasses |> List.rev
        Seq.zip seats passes
