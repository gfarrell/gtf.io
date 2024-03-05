# GTF Website Software Stack

[My website](website) is written in Haskell with WAI and Warp. Webpages
are written in `djot`, but with some extensions which are detailed in
`MARKUP.md` because I wanted to include some extra features.


[website]: https://www.gtf.io
[djot]: https://github.com/jgm/djot

## Structure

We have a few types of page:

* General pages (about, colophon, index, etc.): these have specific content written directly in Haskell, and are found in `src/GTF/Pages/<page>.hs`
* Musings, which are written in djot and are found in `src/GTF/Pages/Musings/content/<category>/<musing>/main.djot`
  * Essays (general)
  * Essays (informatics)
  * Personal reflections
* Codices, which are written in djot and are found in `src/GTF/Pages/Codices/content/<codex>/main.djot`
* Projects, which are written in djot and are found in `src/GTF/Pages/Projects/content/<project>/main.djot`
