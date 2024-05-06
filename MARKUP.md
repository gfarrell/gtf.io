# Markup language

I'm using [`djot`](djot) as the basis for authoring pages for
this website, but with a few extensions in order to bring
forward implementations of [quotation attribution](attr-issue),
[citations](citations-issue) and [cross-references](refs-issue) into
`djot`.

## Captions (TODO)

Many block-level elements can have captions attached to them, like
images, tables, blockquotations, etc.. Captions are delimited with the
caret (`^`) symbol, for example:

```
![an exciting graph][graph]
^ Number of visitors to this website from 1856 to 2030
```

### Attributing Quotations (TODO)

If you have a block-quotation, you can use the caption (`^`) delimeter
to attribute it:

```
> If you give me six lines written by the hand of the most honest of men,
> I will find something in them which will hang him.
^ Cardinal Richelieu (apocryphal) as in [+@Hoyt, p. 763]
```

## Citations (TODO)

You can insert inline citations using the `[+@label, loc; @label2]`
syntax for author-in-text citations, and simply `[@label, loc]` for
regular citations. You can have multiple (`;` separated) citations,
referenced using labels prefixed with `@`, and optional locators
(separated by `,`).

```
As can be seen in [+@farr24, p. 42], this sort of thing has been
previously discussed. Generally, citations are useful [@farr24, p. 27].
```

This will render as:

> As can be seen in Farrell (2024, p. 42), this sort of thing has been
> previously discussed. Generally, citations are useful (Farrell 2024, p. 27).

## Cross-referencing (TODO)

In order to cross-reference one part of your text in another, you need
two things: an anchor (the source of the reference) and a reference
point (or many reference points).

In this implementation, there is a restriction as to the types of things
which can be cross-referenced:

* Sections
* Figures
* Tables
* Code-Listings

You can attach a label to any of these block-level elements using
`:{label}` immediately after the block, and you can then reference the
label using `@{label}`.

```
![a cat in a hat][cat-hat]
:{fig:cat}

You'll see in @{fig:cat} that cats can, indeed, wear hats!
```

This will be rendered according to the type of thing labelled, with
separate numbering for each type, so in the above example, it would
be rendered as: "You'll see in figure 1 that cats can, indeed, wear
hats!". In the case in which a caption has also been defined with `^`,
the "Figure 1" will be inserted before the caption.

[djot]: https://github.com/jgm/djot
[attr-issue]: https://github.com/jgm/djot/issues/198
[citations-issue]: https://github.com/jgm/djot/issues/32
[refs-issue]: https://github.com/jgm/djot/issues/30
