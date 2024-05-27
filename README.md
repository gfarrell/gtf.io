# GTF Website Software Stack

[My website](website) is written in Haskell with WAI and Warp. Webpages
are written in `djot`, but with some extensions which are detailed in
`MARKUP.md` because I wanted to include some extra features.


[website]: https://www.gtf.io
[djot]: https://github.com/jgm/djot

## Structure

We have a few types of page:

* General pages (about, colophon, index, etc.): these have specific content written directly in Haskell, and are found in `src/GTF/Pages/<page>.hs`
* Musings, which are written in djot and are found in `src/GTF/Pages/Musings/content`
  * Essays (general)
  * Essays (informatics)
  * Personal reflections
* Projects, which are written in djot and are found in `src/GTF/Pages/Projects/content`

## Usage

There is a nix flake which you can either use to build (`nix build`), run (`nix run .#default -- 2712`), or develop in (`nix develop`).

The server binary takes a single argument, the port to listen on, otherwise it will return an error (`WrongNumberOfArgs` or `InvalidPort` if you have given it a non-integer).
