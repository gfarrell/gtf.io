{-# LANGUAGE QuasiQuotes #-}

module GTF.Pages.Colophon (content) where

import CommonPrelude
import GTF.Pages.Helpers (djot)
import GTF.Pages.Layout (defaultLayout)
import GTF.URL (UrlPath)
import Lucid (Html, ToHtml (toHtmlRaw))

content :: UrlPath -> Maybe (Html ())
content currentPath =
  Just
    $ defaultLayout
      currentPath
      "Colophon"
    $ toHtmlRaw
      [djot|
      # About this site

      This site is two things -- primarily, it's a place to put my thoughts, insofar as I am able to complete them enough to actually commit them to source; it also serves as a bit of a playground for me to test new things when the urge takes me.

      ## Tools

      While it has been through many iterations, the current version is built in [Haskell](https://www.haskell.org) using [WAI](https://hackage.haskell.org/package/wai), [Warp](https://hackage.haskell.org/package/warp), [Lucid](https://hackage.haskell.org/package/lucid), and [Clay](https://hackage.haskell.org/package/clay). I write the content in a mixture of direct Haskell (using Lucid as an EDSL) and [Djot](https://www.djot.net), although I have written my own extensions in a [custom fork of the Haskell implementation](https://github.com/gfarrell/djoths). The previous iteration was a static site bilt with [Hugo](https://gohugo.io) and served with [nginx](https://nginx.org).

      The site is hosted on a [Digital Ocean](https://www.digitalocean.com) droplet. I have used Digital Ocean since about 2014, and continue to do so mostly out of habit. The host ("Pharos") is configured using [NixOS](https://nixos.org/), which is described in my [infra repository](https://github.com/gfarrell/infra). Everything is written using [neovim](https://neovim.io) on [my machine][anaximander] which runs [archlinux (btw)](https://archlinux.org).

      You can find the source of this site [on Github](https://github.com/gfarrell/gtf.io) and my editor (&c.) configurations in my [dotfiles](https://github.com/gfarrell/dotfiles).

      ## Design and Taxonomy

      This site has several types of page: there are general pages (like this one), musings (essays and reflections), and projects. While I have changed the structure over time, the site also obeys the Cool URIs idea [laid out by Tim Berners-Lee](https://www.w3.org/Provider/Style/URI) to ensure the URIs will always work. [Dorian Taylor has written about this](https://doriantaylor.com/policy/the-uri-naming-conundrum) eloquently enough that I see no reason to replicate his work.

      Such as they are, the design principles of this site are to keep it as fast and readable as possible with minimal, if any, Javascript. As far as possible the pages are semantically structured such that they can easily be parsed for an alternative representation.

      ## \#Inspo

      There are several websites which have inspired the structure, tooling, and content of this one:

      * [Colin Woodbury](https://www.fosskers.ca/about);
      * [Dorian Taylor](https://doriantaylor.com/colophon);
      * and many others, which I'll add here as I remember them.

      [anaximander]: https://gtf21.notion.site/Anaximander-Codex-b3ef43aa24904ad2a7a574fd3d857fb9
    |]
