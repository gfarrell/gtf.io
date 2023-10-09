---
title: Developer Tools Considered Harmful
date: 2023-10-09
draft: false
tags:
- software
- practices
- considered-harmful
abstract: >-
  Following in the footsteps of many other "Considered Harmful" lists
  and papers (hello, Dijkstra), I am proposing that modern developer tools
  like LSPs and IDEs should be considered harmful, not because they are,
  in themselves, not useful tools, but rather because of the corrosive
  effects on the mindset of programmers.
citations:
- id: "dijkstra-goto"
  src: "https://dl.acm.org/doi/10.1145/362929.362947"
  label: "Letters to the editor: go to statement considered harmful"
  retrieved: 2023-10-01
- id: "foc-goto"
  src: "https://futureofcoding.org/episodes/067"
  label: "Future of Coding: Episode 67 \"Considered Harmful\""
  retrieved: 2023-10-01

---

Back in 1968, {{< cite dijkstra-goto >}}Edsger Dijkstra wrote a letter
in the journal "Communications of the ACM"{{< /cite >}} in which he
argued that "go to" statements were, essentially, an anti-pattern in
programming and ought to be avoided:

> For a number of years I have been familiar with the observation that the
> quality of programmers is a decreasing function of the density of **go to**
> statements in the programs [sic] they produce. More recently I discovered why
> the use of the **go to** statement has such disastrous effects, and I became
> convinced that the **go to** statement should be abolished from all "higher
> level" programming languages [...]

I was reminded of this in {{< cite foc-goto >}}a recent episode of
the Future of Coding podcast{{< /cite >}} in which the hosts discuss
this paper and, furthermore, discuss their own (occasionally wacky)
"considered harmful" lists. This prompted me to think of my own list
of "considered harmful"s, and one which I have encountered the most
in recent years: developer tools (henceforth: "devtools"). With some
caveats, I have been slowly nurturing the opinion that the whole
ecosystem of devtools should be considered harmful.

What is a devtool? For me these are programmes, separate from those
directly required to write a programme (like a compiler) whose features
are designed to improve "developer experience". This is a broad
category: one might even say that `vim`, with its text objects and
motions, is a "devtool", and while this is probably true, I'm going
to arbitrarily restrict this to programmes which have some knowledge
of the specifics of the programming language in which the programmer
is working, counting things like LSP clients and servers (and similar
functionality within IDEs), code formatters, and so on. There are
diminishing returns on more fully defining this category, so that will
suffice to illuminate the point I'm trying to make.

I should note that I do use many of these tools: I often use an LSP
client in my editor (`neovim` or `helix` depending on my mood), and I
actually think code formatters are a good way to stop people arguing
about formatting (and use one myself: `ormolu`). Having said that, I
really do not think that these tools have any significant impact on
my productivity when writing programmes, and this is often the main
argument I encounter when programmers either try to persuade me that
an IDE really is better, or that the lack of good LSP support rules a
toolchain out of contention for a project (and so on). Even `(neo)vim`,
which I love, I could perfectly easily programme without if I had to (I
did use VS Code for a few weeks while my laptop was being repaired and
it was fine).

In my experience, these devtools, in a large number of cases, has
engendered a form of learned helplessness in programmers such that they
struggle to conceive of programming without them, and believe that they
are necessary for their own productivity. I would go further and posit,
in the spirit of Dijkstra, that the ability of programmers to _just get
on with it_ is a decreasing function of the amount of time they spend
talking about devtools.

A (very) large number of really complex, high-quality programmes were
written before these tools existed. Devtools, such as they are, improve
the experience but not so much that programming is impossible (or even
unpleasant) without them. In that sense, while devtools are helpful and
make our lives as a programmers more pleasant, this side-effect induces
an over-reliance on them and therefore I would consider them harmful in
general.
