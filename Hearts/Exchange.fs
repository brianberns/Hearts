namespace Hearts

open PlayingCards

/// Cards passed from one player to another.
type Pass = Set<Card>

module Pass =

    /// Number of cards passed by each player.
    let numCards = 3

    /// Empty pass to which cards will be added.
    let empty : Pass = Set.empty

    /// Is the given pass ready to be delivered?
    let isComplete (pass : Pass) =
        assert(pass.Count <= numCards)
        pass.Count = numCards

    /// Adds the given card to the given pass.
    let add card (pass : Pass) : Pass =
        assert(pass.Count < numCards)
        assert(pass.Contains(card) |> not)
        pass.Add(card)

/// All cards passed.
type Exchange =
    {
        /// Current pass, if any.
        CurrentPassOpt : Option<Pass>

        /// Complete passes in reverse chronological order.
        CompletePasses : List<Pass>
    }

module Exchange =

    /// Initial exchange state.
    let empty =
        {
            CurrentPassOpt = Some Pass.empty
            CompletePasses = List.empty
        }

    /// An exchange is complete when all players have passed the
    /// full number of cards.
    let isComplete exchange =
        assert(
            exchange.CurrentPassOpt.IsSome
                || exchange.CompletePasses.Length = Seat.numSeats)
        assert(
            exchange.CompletePasses
                |> Seq.forall (fun pass ->
                    pass.Count = Pass.numCards))
        exchange.CurrentPassOpt.IsNone

    /// Whose turn is it to pass cards in the given exchange?
    let currentPasser dealer exchange =
        assert(isComplete exchange |> not)
        dealer
            |> Seat.incr (
                exchange.CompletePasses.Length + 1)   // dealer passes last

    /// Adds the given card to the given exchange.
    let addPass card exchange =
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
    let seatPasses dealer exchange =
        assert(isComplete exchange)
        let seats = Seat.next dealer |> Seat.cycle   // dealer passes last
        let passes = exchange.CompletePasses |> List.rev
        Seq.zip seats passes
