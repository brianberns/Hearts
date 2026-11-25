# Changes since the production model

The production model, `AdvantageModel-i005-e027.pt`, came from a training run whose sample stores are
dated 2026-02-17 to 2026-03-06. These are the design-level changes and major bug fixes to training
made since then.

## Samples

- **Random Gen 1** (1c7e20b, 2026-03-17). Iteration 1 samples now come from uniform random play.
  Before this change, they came from the heuristic player's choices, so each regret was measured
  against the single card the heuristic chose (since 28461da, 2026-01-18).
- **Incoming pass hidden while passing** (61e5716, 2026-10-29). A player's info set used to show the
  cards it would receive before it chose its own pass. In the production run's Gen 1 stores, that
  affected about 9% of all samples.

## Rules (`Rules.arena`, Hearts Arena's `std-1`)

- **Only a heart breaks hearts** (10f0996, 2026-11-23). The Q♠ no longer does. Leading a heart before
  hearts are broken now reveals voids even after the Q♠ has been played.
- **A moon always adds 26 to each opponent.** The shooter can no longer subtract 26 instead.
- **A tie for low at 100 or more continues the game** (703d7fb).

## Game awareness (in progress)

- **Deal and game scores in the encoding** (ad5bebe, 2026-11-25). Each seat's deal score used to be
  one flag: had it taken points? It is now the number of points, 0 to 26, as 26 flags per seat. Each
  seat's game score is added as 126 flags. The encoding grew from 540 inputs to 1,144.
- **Whole games generated** (cfc3066, 741779d). Each deal's dealer and pass direction now follow from
  its place in its game.
- **W, the win-probability model** (f382bb5, 5cd335a). W gives each seat's chance of winning the game
  from the game score. It will provide game-level payoffs (`Settings.GameLevelPayoff`), which are not
  yet implemented.
