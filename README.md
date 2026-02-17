# Solving Hearts with Deep Learning

## Overview

This repository solves Hearts (the card game) using a simplified version of [Deep Counterfactual Regret Minimization](https://arxiv.org/abs/1811.00164), aka Deep CFR. The basic idea is:

1. Start with a model that considers all actions to be equally advantageous. This model plays randomly, since all actions are equally likely to be chosen.
2. Play the model against itself for thousands of games. Compare the predicted outcome of each action to the actual outcome of taking that action. (This is called "regret".)
3. Train a new version of the model using the comparisons generated in the previous step.
4. Repeat from step 2 for multiple iterations.

## Building and running

1. Build the training programs `Hearts.Generate` and `Hearts.Train` and copy the executables into the same directory.
2. Generate training data by running `Hearts.Generate`. This will take several hours to complete, and the generated samples will be saved in the `./Models` directory. You can track progress via the TensorBoard directory, `./runs`. E.g. `tensorboard --bind_all --logdir ./runs`.
3. Use the generated data to train a model by running `Hearts.Train`. This will take several more hours to complete, and the model will be saved in the `./Models` directory. You can continue to track progress via TensorBoard.
4. Build the web server, `Hearts.Web.Server`.
5. Copy one of the trained models to the web server's runtime directory and rename it to `AdvantageModel.pt`.
6. Start the web server by building and running `Hearts.Web.Harness`.
7. In the `Hearts.Web/Client` directory, start the client via `npm install` followed by `npm start`.
8. Browse to [`http://localhost:8081/`](http://localhost:8081/) to play the game.

You can also play the game online [on my website](https://www.bernsrite.com/Hearts/).

## Modeling Hearts

Because Hearts ends when one of the players reaches 100 points, it can sometimes benefit players to cooperate near the end of a game, in order to avoid going over the limit. This model ignores that aspect of the game entirely, and focuses only on the score within the current deal.

## Differences from Deep CFR

* Since Hearts strategy is the same for all players, there is no need to train a separate model for each player. Instead, all players share the same model.
* Because misdirection/bluffing is not a major part of Hearts, there is no need to train a separate "strategy" model from the advantage models at the end of the run. Instead, the advantage model converges on a strategy after a few iterations.
