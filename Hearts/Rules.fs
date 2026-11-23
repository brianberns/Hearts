namespace Hearts

/// How a moon shot affects players' scores.
type MoonShotScoringRule =

    /// Shooter scores zero and the other players' scores
    /// increase, even if the shooter doesn't end up in the
    /// lead.
    | AlwaysAddToOthers

    /// Shooter scores zero and the other players' scores
    /// increase, except shooter's score drops and other players
    /// score zero if the shoot doesn't put the shooter in the
    /// lead.
    | AllowSubtraction

/// How tie games are handled.
type TieGameRule =

    /// Low-scorers tie for the win.
    | MultipleWinners

    /// Keep playing until there is only one winner.
    | KeepPlaying

/// Rule variations.
type Rules =
    {
        /// How a moon shot affects players' scores.
        MoonShotScoring : MoonShotScoringRule

        /// How tie games are handled.
        TieGame : TieGameRule
    }

module Rules =

    /// Our standard rules.
    let standard =
        {
            MoonShotScoring = AllowSubtraction
            TieGame = MultipleWinners
        }

    /// Hearts Arena rules.
    /// https://hearts-arena.snowfrog.dev/guide/rules
    let arena =
        {
            MoonShotScoring = AlwaysAddToOthers
            TieGame = KeepPlaying
        }
