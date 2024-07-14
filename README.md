# GTF Website Software Stack

[My website][website] is written in Haskell with WAI and Warp. Webpages
are written in [`djot`][djot], but with some extensions which are detailed in
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

## Running the website

There is a nix flake which you can either use to build (`nix build`), run (`nix run .#default -- 2712`), or develop in (`nix develop`).

The server binary takes a single argument, the port to listen on, otherwise it will return an error (`WrongNumberOfArgs` or `InvalidPort` if you have given it a non-integer).

## Preview tool

To make authoring easier when writing in a markup language (even a very readable one like djot) there is a small application called `preview` which runs a server on 8080 (websockets on 8081) and will render the document you're working on as well as automatically reloading the browser when the document changes.

You can run this with cabal (`cabal run preview -- -h`) or with nix (`nix run .#preview -- -h`). It takes two arguments:

* the document type (`-t`) which will control how the document is rendered (e.g.
  `project` or `musing`);
* the file you're working on (`--file`), whose _directory_ will be watched for
  changes.

You can then open up your browser to `localhost:8080` and you will see the document.
