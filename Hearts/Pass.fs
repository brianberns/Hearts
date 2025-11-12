namespace Hearts

open PlayingCards

/// Cards passed from one player to another.
type Pass = Set<Card>

module Pass =

    /// Number of cards passed by each player.
    let numCards = 2

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
