module GTF.Pages.Colophon (content) where

import CommonPrelude
import GTF.Pages.Helpers (commaList, internal)
import GTF.Pages.Layout (defaultLayout)
import GTF.URL (UrlPath)
import Lucid (Html, ToHtml (toHtmlRaw))
import Lucid.Html5

content :: UrlPath -> Maybe (Html ())
content currentPath = Just
  $ defaultLayout
    currentPath
    "Colophon"
  $ do
    h1_ "About this site"
    p_ $ do
      "This site is two things "
      toHtmlRaw ("&mdash;" :: String)
      " primarily, it's a place to put my thoughts, insofar as I am able to complete them enough to actually commit them to the source; it also serves as a bit of a playground for me to test new things when the urge takes me."

    section_ $ do
      h2_ "Tools"
      p_ $ do
        "While it has been through many iterations, the current version is built in "
        a_ [href_ "https://www.haskell.org", title_ "Haskell language website"] "Haskell"
        " using "
        commaList
          [ a_ [href_ "https://hackage.haskell.org/package/wai", title_ "WAI on Hackage"] "WAI",
            a_ [href_ "https://hackage.haskell.org/package/warp", title_ "Warp on Hackage"] "Warp",
            a_ [href_ "https://hackage.haskell.org/package/lucid", title_ "Lucid on Hackage"] "Lucid"
          ]
        ". I write the content in a mixture of direct Haskell (using Lucid as an EDSL) and "
        a_ [href_ "https://www.djot.net/", title_ "djot website"] "Djot"
        ", although I have written my own extensions in a "
        a_ [href_ "https://github.com/gfarrell/djoths", title_ "djoths custom fork"] "custom fork of the Djot implementation for Haskell"
        ". The previous iteration was a static site built with "
        a_ [href_ "https://gohugo.io", title_ "Hugo website"] "Hugo"
        " and served by "
        a_ [href_ "https://nginx.org", title_ "nginx website"] "nginx"
        "."
      p_ $ do
        "The site is hosted on a "
        a_ [href_ "https://www.digitalocean.com", title_ "Digital Ocean website"] "Digital Ocean"
        " droplet. I have used Digital Ocean since around 2014 and have found them mostly quite reliable, but at this point it's more of a habit than anything else and I do have my gripes."
      p_ $ do
        "Other than that, everything is written using "
        a_ [href_ "https://neovim.io/", title_ "neovim website"] "neovim"
        " on my machine which runs "
        a_ [href_ "https://archlinux.org/", title_ "ArchLinux website"] "archlinux (btw)"
        " for which you can find the configuration details in the "
        internal "codices/anaximander" "Anaximander Codex"
        "."
      p_ $ do
        "You can find the source of the site "
        a_ [href_ "https://github.com/gfarrell/gtf.io", title_ "website source repository"] "on Github"
        " and my own editor (etc.) configurations in my "
        a_ [href_ "https://github.com/gfarrell/dotfiles", title_ "Gideon's dotfiles"] "dotfiles"
        "."

    section_ $ do
      h2_ "Design and Taxonomy"
      p_ $ do
        "The site has a several types of page: "
        commaList
          [ "there are the general pages (like this one)",
            "musings (essays and reflections)",
            "codices (digital \"books\" on a particular topic)",
            "projects"
          ]
        ". While I have changed the structure over time, the site also obeys the Cool URIs idea laid out by "
        a_ [href_ "https://www.w3.org/Provider/Style/URI", title_ "Cool URIs don't change"] "Tim Berners-Lee"
        " to ensure the URIs will always work. "
        a_ [href_ "https://doriantaylor.com/policy/the-uri-naming-conundrum", title_ "The URI Naming Conundrum"] "Dorian Taylor"
        " has written about this eloquently enough that I see no reason to replicate his work."
      p_ "Such as they are, the design principles of this site are to keep it as fast and readable as possible, with minimal, if any, Javascript. The structure of the pages is intended to be semantically structured so that, should one ever want to parse it for an alternative representation, it would be easy to do so."

    section_ $ do
      h2_ "Inspiration"
      p_ "There are several websites which have inspired both how I have structured this one, but also how I have built it:"
      ul_
        $ foldMap
          li_
          [ a_ [href_ "https://www.fosskers.ca/en/about", title_ "Fosskers' website"] "Colin Woodbury",
            a_ [href_ "https://doriantaylor.com/colophon", title_ "Dorian Taylor's website"] "Dorian Taylor",
            "and many others, which I'll add here as I remember them!"
          ]
