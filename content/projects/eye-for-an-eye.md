---
title: "Evolutionary Prisoner's Dilemma Simulator"
github: "https://github.com/gfarrell/eye-for-an-eye"
language: "haskell"
draft: false
abstract: >-
  Simulator written in Haskell to run through various scenarios of the
  prisoner's dilemma and evolving them in time. Includes some visualisation
  capabilities.
date: 2020-03-01
category: programming
---

I was discussing with a friend whether purely selfish behaviour
is really the best way to succeed (assuming the rest of
the world does not behave as such), based on the phrase
"el vivo vive del bobo". Having recently started [learning
Haskell](/musings/coding-in-haskell-a-new-adventure), I wanted to
build a basic simulator to test my hypothesis that, in a society in
which members remember how each other behave, behaving selfishly (i.e.
defecting) is actually less successful than cooperating).

To do this I created a basic `Agent` which can perform some `Actions` on
each other in each round (or `Frame`) of the simulation. An `Action` can
either be to `Cooperate` or `Defect`.

`Agents` are defined with two main parameters:

- generosity: a generous `Agent` is more likely to cooperate even when in
  the last round the other `Agent` defected.
- selfishness: a selfish `Agent` is more likely to defect no matter what.

The basic behaviour of the simulator is "eye for an eye": given two `Agents` `A`
and `B`, and the `Actions` of those two `Agents` in the previous `Frame` (or
round) of the simulation, the resulting action for `A` is:

- `B` cooperated &rarr; `A` cooperates
- `B` defected &rarr; `A` defects

Once all the inter-`Agent` `Interaction`s have been computed, a scoring matrix
is applied to each `Agent` (called the `RewardsVector`). This determines the
score increment for each `Agent` in the following scenarios:

- `A` cooperates, `B` cooperates
- `A` cooperates, `B` defects
- `A` defects, `B` cooperates
- `A` defects, `B` defects

The scores are then used to calculate the probability of a given `Agent`
reproducing at the end of the round. An `Agent` which reproduces will
spawn a new `Agent` (in addition to itself) with the same parameters
(generosity and selfishness).

Thus the simulation tries to encode some ideas about how the real world
works in terms of selfishness and generosity, as well as a "mistake
factor" (the probability that the `Agent` acts "irrationally" according
to its own parameters). The problem was that the results are extremely
sensitive (for obvious reasons) to the choices of magic numbers in the
`RewardsVector`. I had no real way of determining what these should
be (at least no way which was not somehow begging the question) and
therefore did not really obtain any interesting results (other than that
of having built the simulator itself, which was a fascinating way to
learn Haskell).
