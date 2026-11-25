# Notes for coding agents

What an agent working in this repository needs to know that `docs/README.md` doesn't say. The README
belongs to Brian Berns, the author; this file is kept up to date by Claude, his coding agent. Where the
two disagree, the code is right, and this file should be fixed.

## Working agreements

- **Who owns what.** Brian writes the core code: `PlayingCards`, `Hearts`, `Hearts.Model`,
  `Hearts.Learn`, `Hearts.Generate`, `Hearts.Train`, `Hearts.PlayModel`, `Hearts.Web` and the README.
  Agents propose changes to these in chat rather than making them. Agents own `Hearts.Heuristic`'s
  Killer.fs and KillerEngine, `Hearts.Experiments`, `Hearts.ValidateSamples`, `SplitMixRandom`, the HeartsArenaBot repository
  (see Hearts Arena, below), and this file, and keep them building against Brian's changes.
- **Separate checkouts.** Brian develops in his own clone; code moves between him and an agent only
  through git. Pull before work that depends on his latest changes. Commit and push only when asked.
- **Style.** F#, written in a pure functional style where practical (folds rather than mutation).
  The Killer Hearts engine is the exception: it follows the VB6 original's imperative structure, as
  its Rust twin in HeartsArenaBot does, so the two can be compared line by line.
- **Windows.** Run Python as `py`, not `python`.
- **Everything must stay in `Hearts.slnx`**, so that a change that breaks a project shows up in the
  solution build.

## Building

- Every project builds into the shared `Artifacts\<Configuration>\` folder (`Directory.Build.props`).
  So a running program locks the DLLs that other builds need: while a long experiment runs from
  `Artifacts\Release`, build elsewhere with `-p:OutDir=<another folder>/`.
- For the same reason, every project that references TorchSharp uses the same native package
  (`TorchSharp-cuda-windows` / `-linux`). A project on `TorchSharp-cpu` would overwrite the CUDA
  libtorch DLLs in `Artifacts`.
- `Hearts.Web/Client` is compiled by Fable; its `.fsproj` lists the core `Hearts` files it links, in
  compile order, so a new or reordered core file must be mirrored there.

## Rules

`Rules` (`Hearts/Rules.fs`) has three variations, and every game is played under one of two sets:

| | `Rules.standard` | `Rules.arena` |
|---|---|---|
| What breaks hearts | a heart or the Q♠ | only a heart |
| Moon shot | the Shooter may subtract 26 instead | always adds 26 to each opponent |
| Tie for low at 100 or more | every tied player wins | play continues |

`Rules.arena` is Hearts Arena's `std-1` (<https://hearts-arena.snowfrog.dev/guide/rules>), and is what
`Tournament`, `Hearts.Generate` and the experiments use. The web app uses `Rules.standard`. Results
recorded before 2026-11-23 (commit 10f0996) used the Q♠-breaks-hearts rule even under `Rules.arena`.

## Players and tournaments

- `Tournament.runSeeded rngSeed numGames makeChampion makeChallenger` plays whole games with duplicate
  deals. Each cycle is one deal sequence played four times, with the challenger in each seat against
  three champions. It answers the challenger's game win rate (outright wins only, as the arena rates
  them), so 0.25 is even. (The README's "2v2 tournament" predates this.) Cycles run in parallel; at
  the start of each, `makeChampion` and `makeChallenger` are called with seeds of their own
  (`SplitMixRandom.GetChildSeed`, apart from the deal's stream), and the two players serve all four
  games of the cycle, the champion in three seats at once. So a tournament repeats exactly for a
  given `rngSeed`, and players need not be thread-safe, but must keep no state across games other than
  their random numbers. `Tournament.run` is the same for players that need no seed.
- `Game.playGame` asserts (in Debug builds) that every action a player returns is legal. Run a Debug
  build to check a new player for illegal plays; Release builds skip the check.
- `Strategy.createPlayerSeeded model seed` samples the model's strategy with a `SplitMixRandom`: use it
  in tournaments. `Strategy.createPlayer` samples with `Random.Shared`, so it is thread-safe but not
  repeatable: for the web app and other one-off play. Sampling beat a deterministic player (the most
  probable class of equivalent cards; removed in 2463804) by about a point: 0.2610 vs 0.2363 head to
  head, on 10,000 games each.
- **Model inference** on the CPU is thread-safe and gives identical results in parallel. Throughput
  stops improving at about 8 threads. The GPU also works from many threads (no crash, identical
  results), but holds about 3 GB more for the life of the process and is no faster for tournaments, so
  tournaments run models on the CPU (the trainer copies its model to the CPU to evaluate it).

## Heuristic players (`Hearts.Heuristic`)

- **Killer Hearts V8** (`Killer.fs`, module `Killer`; until 2026-11-23 `Claude.fs`) is the strongest
  heuristic player and the benchmark for model training. Killer Hearts is Gerald Berns' VB6 program,
  whose last version, V7.2, is in a separate repository (a sibling folder named `KHearts`). V8 is V7.2
  plus the arena's moon rule (it never passes or plays to Shoot when a Shoot would lose the game, and
  then breaks up its own moon: `shootLoses`, `breakMoon`) and a readier Shoot when it trails the low
  score by 10 or more (`trailingShootShift`, worth about +0.004 game win rate).
- `KillerEngine/` is V7.2's engine, ported to F# by hand. `Options` holds what V8 changes inside it
  (`NoShoot`, `ShootShift`); `Options.v72` is V7.2 exactly. The port was checked against a C# engine
  translated from the VB6 source, now retired with the rest of the old Killer Hearts tooling (C#
  engine, `KHearts.Test`, `Hearts.PlayKiller`, `Hearts.ParseKiller` and the translator; see the commit
  that removed them).
- **Hearts Arena runs the same V8 in Rust** (HeartsArenaBot's `khearts/`). After any change to
  Killer.fs or KillerEngine, make the same change there and run its reference test (below), which
  must agree on every pass and play.

## Experiments (`Hearts.Experiments`)

`Hearts.Experiments` holds one-off measurements; its `Program.fs` lists the modes:

- `bench` times the model and checks that it plays the same in parallel as sequentially.
- `phase0` compares the production and weakened models with each other and with Killer Hearts V8.
- `gpu` stress-tests TorchSharp inference on the GPU from many threads.
- `benchmark <model> <seed> <games>` runs the benchmark for model training: the model vs three Killer
  Hearts V8. The target for game-aware models is the production model's 0.2708 (40,000 games, seed
  20261125, built from bb2d763, the last commit whose encoding loads it). On 2026-11-23, before the
  arena's hearts-break rule and Killer's readier trailing Shoot, it had measured 0.3020 (10,000 games).
- `wdata <model> <seed> <games> <output.csv>` plays whole games with the model in every seat and writes
  each deal's starting scores, pass direction and the game's winner: the data for W, each seat's chance
  of winning the game from the start of a deal. It plays in chunks of 1,000 games on at most one thread
  per core: a single parallel loop over 100,000 games grew past 14 GB in 80 minutes.
- `wfit <data.csv> <output.pt> <epochs>` fits W, a small network from the four game scores to a softmax
  over the seats. On 100,000 games of the i005 model (2026-11-24) its validation loss was 1.0397 (a
  uniform guess: 1.3863), well calibrated; the pass direction improved it by only 0.0001, so W leaves it
  out.
- `tune <seed> <games> [variation ...]` compares variations of Killer Hearts V8 (`Tune.variations`)
  against three V8. Confirm a winner on a fresh seed before adopting it.

A difference of 0.01 in win rate needs about 10,000 games to see, and 40,000 to trust.

## Validating sample stores (`Hearts.ValidateSamples`)

`Hearts.ValidateSamples <store.bin | folder> ...` checks Hearts.Generate's `AdvantageSamples-*.bin`
stores. It reads them with its own description of each version of the encoding (`Layout.fs`: the 540
inputs before ad5bebe and the 1,144 since), not with `AdvantageSampleStore`, then cross-checks a few
samples against that reader. Beyond the header, file name (iteration, payoff level) and truncation, it
decodes every sample into a deal state and checks it against the rules: cards in one place each, hand
and trick sizes, passes, voids, deal and game scores, regrets only for legal actions and not all of
one sign, and regrets within the payoff level's range (±52 for a deal, ±2 for a game). A game-level
store must also have game scores other than 0-0-0-0. The report says which payoff level the regrets look
like: deal-level regrets are in points (typically about 3, most beyond ±2), game-level regrets are in
chances of winning (all within ±2, typically well under 0.5). It warns when that disagrees with the name.
At iteration 1, with no model, the strategy is uniform random, so every sample's regrets sum to zero.
Before 1c7e20b (2026-03-17), iteration 1 followed the heuristic player instead: regret against the one
card it chose, so many of those regrets were zero and went unstored. **A change to `Encoding.fs` needs a new layout
in `Layout.fs`**; the program warns when the current encoding matches none. About 115,000 samples a
second.

## Hearts Arena

The model and Killer Hearts V8 play on Hearts Arena (<https://hearts-arena.snowfrog.dev>), whose games
are public. The bots live in a separate repository, `HeartsArenaBot`, next to this one:

- the model bot (Rust) at its root, with `tools/Reference`, which plays deals of `Rules.arena` with the
  model in the F# engine and writes its decisions for the bot's differential test (`tests/reference.rs`:
  the inputs must match exactly). The bot follows the F# engine's inferences under the arena's rules,
  such as the voids revealed by leading a heart before hearts are broken, even after the Q♠;
- the Killer Hearts V8 bot in `khearts/`: Rust first generated from the VB6 source, plus `v8.rs`. Its
  `tools/Reference` plays whole arena games with Killer.fs and writes one seat's view as the arena would
  send it; `tests/reference.rs` replays them through the bot and must agree on every pass and play;
- `scripts/local-games.sh`, which plays whole games on the arena's own runner through WSL (the kit lives
  at `~/hearts-arena/hearts-arena-kit` in WSL);
- `scripts/duel.py`, which plays two modules against each other on that runner with duplicate deals
  (the challenger in each seat against three champions, like `Tournament.runSeeded`) and answers the
  challenger's win rate. Each module carries its own encoding, so this is how to compare models whose
  encodings differ, such as a game-aware model against production. About 400 model games a minute.

Never print or commit the arena key (`HEARTS_ARENA_KEY`), or model weights (`weights/` is ignored).
Since 2026-11-24 the arena runs the production model (`AdvantageModel-i005-e027.pt`) as Deep-CFR; the
weakened model it ran before (`AdvantageModel-i003-e024.pt`) is retired. The arena's ratings carry a
margin: two bots whose ranges don't overlap are in that order at least 95% of the time
(<https://hearts-arena.snowfrog.dev/guide/ratings>).
