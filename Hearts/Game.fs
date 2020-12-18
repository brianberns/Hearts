namespace Hearts

open PlayingCards

type Player =
    {
        /// Function that passes cards in the given deal.
        MakePass : OpenDeal -> Score -> Set<Card>
    }

type Game =
    {
        /// Current deal.
        CurrentDeal : OpenDeal

        /// Score of the game.
        Score : Score
    }
