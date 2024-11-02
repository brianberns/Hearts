namespace PlayingCards

/// A shuffled deck of cards.
type Deck =
    {
        Cards : Card[]
    }

module Deck =

    /// Creates a shuffled deck of cards.
    let shuffle rng =
        {
            Cards =
                Card.allCards
                    |> Array.randomShuffleWith rng
        }
