# Solving Hearts with Deep Learning

This repository solves Hearts (the card game) using a simplified version of [Deep Counterfactual Regret Minimization](https://arxiv.org/abs/1811.00164), aka Deep CFR. The basic idea is:

1. Start with a model that considers all actions to be equally advantageous. This model plays randomly, since all actions are equally likely to be chosen.
2. Play the model against itself for thousands of games. Compare the predicted outcome of each action to the actual outcome of taking that action. (This is called "regret".)
3. Train a new version of the model using the comparisons generated in the previous step.
4. Repeat from step 2 for multiple iterations.
