# Crushing Hearts with Deep CFR

[![Screenshot](Bernsrite.png)](https://www.bernsrite.com/Hearts/)
♠️♥️♦️♣️ [Play Hearts against a superhuman AI](https://www.bernsrite.com/Hearts/) ♣️♦️♥️♠️

## Overview

The card game [Hearts](https://en.wikipedia.org/wiki/Hearts_(card_game)) is difficult for a computer to master because it has a large game tree with elements of chance and uncertainty. [Deep Counterfactual Regret Minimization](https://arxiv.org/abs/1811.00164) (Deep CFR) is an algorithm designed specifically to tackle such games. We use a simplified version of Deep CFR to train an AI that plays Hearts at a superhuman level!

## Background

### Perfect information games

Games like tic-tac-toe and chess are called "[perfect information](https://en.wikipedia.org/wiki/Perfect_information)" games because each player knows all relevant information about the state of the game. Nothing is hidden from the players in such games.

Playing tic-tac-toe well is much easier than playing chess well, though, because chess has a much larger "[game tree](https://en.wikipedia.org/wiki/Game_tree)" of possible moves. An AI that plays tic-tac-toe perfectly can be written a few lines of code, but a program that plays chess well is much more difficult to create. In recent years, great progress towards this goal has been achieved by deep learning programs like [AlphaZero](https://en.wikipedia.org/wiki/AlphaZero), which mastered chess by playing against itself for a few hours.

### Imperfect information games

AlphaZero doesn't do as well with "imperfect information" games like rock-paper-scissors or poker, however. In these games, some information is hidden from the players,[^1] which introduces a role for chance in the game. A beginner poker player can sometimes beat an expert, depending on the luck of the draw. Card games typically have imperfect information, because players have "hands" that they keep private. Bridge is another example of such a card game.

In imperfect information games, a good strategy sometimes involves a degree of randomness. For example, the best strategy in rock-paper-scissors is to choose your hand shapes randomly.[^2] Bluffing in poker is another example of strategic random behavior. Over time, a poker player who never bluffs isn't going to win as much as a player who bluffs well at the right time.

We have to rethink the idea of a game tree for imperfect information games, since multiple game states might be indistinguishable to a player, given their imperfect knowledge (e.g. they don't know the cards in other players' hands). Instead, we consider a tree where each node contains all of the active player's information about the state of the game when it is their turn to play. Each such node is called an "information set".

### Counterfactual regret minimization

As with perfect information games, imperfect information games have game trees that can be small (e.g. rock-paper-scissors) or large (e.g. bridge). There's a powerful machine learning technique for solving imperfect information games called [counterfactual regret minimization (CFR)](https://github.com/brianberns/CFR-Explained), but it is only practical for fairly small game trees.

### Deep CFR

Unlike Chess or Go, [Hearts](https://en.wikipedia.org/wiki/Hearts_(card_game)) is an "imperfect information" game in which each player has private information about their own hand. This shifts the challenge of mastering the game from a pure game tree search to finding a strategy that works well for all possible opponent hands. Such a strategy approximates an ideal .

[Deep Counterfactual Regret Minimization](https://arxiv.org/abs/1811.00164) (Deep CFR) is a powerful technique for solving such games. However, applying it to a game as complex as Hearts is challenging technically. Fortunately, empirical results demonstrate that we can simplify Deep CFR specifically for Hearts. The basic idea is:

1. Start with a model that plays randomly.
2. Play the current model against itself for thousands of games. At various points within these game, compare the predicted outcome of each legal action to the actual outcome of taking that action. (This is called "regret" in CFR parlance.)
3. Train a new version of the model using the comparisons generated in the previous step.
4. Repeat from step 2 for multiple iterations.

[^1]: I think "hidden information" would have been a better name for these types of games. "Incomplete information" might have also been a good name, but that actually means something [completely different](https://web.stanford.edu/~jdlevin/Econ%20203/Bayesian.pdf). Game theory is confusing sometimes.

[^2]: Such a strategy is called a "[Nash equilibrium](https://en.wikipedia.org/wiki/Nash_equilibrium)". A Nash equilibrium can't be beaten over the long run, but it doesn't do anything to exploit weaknesses that might exist in other strategies. Note that an imperfect information game might have multiple Nash equilibria, rather than a single best strategy.