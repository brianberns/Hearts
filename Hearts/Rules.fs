namespace Hearts

type MoonShotScoringRule =

    /// Shooter scores zero and the other players's scores
    /// increase, even if the shooter doesn't end up in the
    /// lead.
    | AlwaysAddToOthers

    /// Shooter scores zero and the other players's scores
    /// increase, except shooter's score drops if the shoot
    /// doesn't put her in the lead.
    | AllowSubtraction

/// Rule variations.
type Rules =
    {
        /// How a moon shot affects players' scores.
        MoonShotScoringRule : MoonShotScoringRule
    }

module Rules =

    /// Our standard rules.
    let standard =
        {
            MoonShotScoringRule = AllowSubtraction
        }

    /// Hearts Arena rules.
    /// https://hearts-arena.snowfrog.dev/guide/rules
    let arena =
        {
            MoonShotScoringRule = AlwaysAddToOthers
        }
