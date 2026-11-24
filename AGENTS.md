# Notes for coding agents

What an agent working in this repository needs to know that `docs/README.md` doesn't say. The README
belongs to Brian Berns, the author; this file is kept up to date by Claude, his coding agent. Where the
two disagree, the code is right, and this file should be fixed.

## Working agreements

- **Who owns what.** Brian writes the core code: `PlayingCards`, `Hearts`, `Hearts.Model`,
  `Hearts.Learn`, `Hearts.Generate`, `Hearts.Train`, `Hearts.PlayModel`, `Hearts.Web` and the README.
  Agents propose changes to these in chat rather than making them. Agents own `Hearts.Heuristic`'s
  Killer.fs and KillerEngine, `KHearts` (including `KHearts.Test` and the transpiler),
  `Hearts.PlayKiller`, `Hearts.ParseKiller`, `Hearts.Experiments`, `SplitMixRandom`, and this file,
  and keep them building against Brian's changes.
- **Separate checkouts.** Brian develops in his own clone; code moves between him and an agent only
  through git. Pull before work that depends on his latest changes. Commit and push only when asked.
- **Style.** F#, written in a pure functional style where practical (folds rather than mutation).
  The Killer Hearts port is the exception: it follows the VB6 original's imperative structure, so it can
  be checked line by line against it.
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
`Tournament`, `Hearts.Generate` and the experiments use. Killer Hearts itself plays `Rules.standard`, so
the projects that replay its games or talk to the original program (`KHearts.Test`'s exact-table modes,
`Hearts.PlayKiller`, `Hearts.ParseKiller`) and the web app use that. Results recorded before 2026-11-23
(commit 10f0996) used the Q♠-breaks-hearts rule even under `Rules.arena`.

## Players and tournaments

- `Tournament.runSeeded rngSeed numGames makeChampion makeChallenger` plays whole games with duplicate
  deals. Each rotation is one deal sequence played four times, with the challenger in each seat against
  three champions. It answers the challenger's game win rate (outright wins only, as the arena rates
  them), so 0.25 is even. (The README's "2v2 tournament" predates this.) Rotations run in parallel; at
  the start of each, `makeChampion` and `makeChallenger` are called with seeds of their own
  (`SplitMixRandom.GetChildSeed`, apart from the deal's stream), and the two players serve all four
  games of the rotation, the champion in three seats at once. So a tournament repeats exactly for a
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

- **Killer Hearts** is Gerald Berns' VB6 program. Its source lives in a separate repository (a sibling
  folder named `KHearts`). `KHearts/Transpiler/vb2cs.py` translates its playing engine to C#
  (`KHearts/KHearts/Generated/*.g.cs`, never edited by hand: change the translator and regenerate with
  `py KHearts/Transpiler/vb2cs.py <KHearts source folder> KHearts/KHearts/Generated`). `vb2rs.py`
  translates it to Rust for the Hearts Arena bot, in a separate repository.
- `ARENA_PATCHES` in `vb2cs.py` change three lines of the original for the arena's hearts-only rule;
  under Killer Hearts' own rules they make no difference. The adapter (`KillerHearts.cs`) also calls
  `UnbreakHearts` after each Q♠ under `HeartsOnly`.
- `Hearts.Heuristic/KillerEngine/` is a hand port of the same engine to F#, exact against the C# engine.
  `Options.faithful` is Killer Hearts exactly; the other options depart from it (no Shoot, quirk
  fixes, a shift in the strength needed to Shoot).
- **Killer Hearts V8** (`Killer.fs`, module `Killer`; until 2026-11-23 `Claude.fs`) is the strongest
  heuristic player and the benchmark for model training: the F# port with its quirks fixed, its bold pass, the arena's moon rule (it never shoots when a moon
  would lose the game, and breaks up its own moon then), and a readier Shoot when it trails the low
  score by 10 or more (`trailingShootShift`, worth about +0.004). Against three Killer Hearts it wins about
  0.25 under arena rules.
- **The Hearts Arena Killer Hearts bot stays faithful** to the original program, quirks and all,
  changing only what the arena's rules require. The ports follow V7.1; the VB source is now V7.2, which
  fixes five quirks exactly as `FixQuirks` does. Killer Hearts V8 replaces it there
  as a new version of the same bot.

### Checking the port

After any change to the port, the C# engine, or the core rules, run `KHearts.Test` (Release):

- `fsport <deals>`: the F# port against the C# exact table, under Killer Hearts' own rules. Must be
  100% for both passes and plays.
- `fsgames <games> [shown]`: the F# port against the C# engine in whole arena games, at real game
  scores. Must be 100%. Mismatches print both engines' traces (`Brief`, `LeadDetail`), which
  usually show where the two part ways.
- `claude <games>`: Killer Hearts V8 against a score-blind version of itself and V7.1, with moon counts. Running it
  as a Debug build also checks for illegal plays.

Other modes: `quirks`, `daring`, `moons`, `smoke`, `gamescore`, `agree` (against the one-seat
adapter), `reference`, `features`, and `client` / `replay`, which drive or replay the original Windows
program through its Host-Client feature.

## Experiments (`Hearts.Experiments`)

`Hearts.Experiments` holds one-off measurements; its `Program.fs` lists the modes:

- `bench` times the model and checks that it plays the same in parallel as sequentially.
- `phase0` compares the production and weakened models with each other and with Killer Hearts V8.
- `gpu` stress-tests TorchSharp inference on the GPU from many threads.
- `benchmark <model> <seed> <games>` runs the benchmarks for model training: Killer Hearts V8 vs three
  V7.1, and the model vs three V8.
- `tune <seed> <games> [variation ...]` compares variations of Killer Hearts V8 (`Tune.variations`)
  against three V8 and three V7.1. Confirm a winner on a fresh seed before adopting it.

A difference of 0.01 in win rate needs about 10,000 games to see, and 40,000 to trust.

## Hearts Arena

The model and Killer Hearts play on Hearts Arena (<https://hearts-arena.snowfrog.dev>), whose games
are public. The bots live in a separate repository,
`HeartsArenaBot`, next to this one: the model bot (Rust) at its root, and the Killer Hearts bot in
`khearts/` (Rust generated from the VB source by `KHearts/Transpiler/vb2rs.py`), each with its tests
and the scripts that play whole games in the arena's runner through WSL. Never print or commit the arena key
(`HEARTS_ARENA_KEY`). The production model's weights are not on the arena: only a weakened model is.
