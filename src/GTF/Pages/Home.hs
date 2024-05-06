{-# LANGUAGE QuasiQuotes #-}

module GTF.Pages.Home (content) where

import CommonPrelude
import GTF.Pages.Helpers (djot)
import GTF.Pages.Layout (PageMeta (..), plainLayoutWithMeta)
import GTF.Pages.Partials.Nav (navbar)
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, toHtmlRaw)
import Lucid.Html5

content :: Html ()
content = plainLayoutWithMeta
  PageMeta
    { pageTitle = "home"
    , pageDescription = Just "Gideon's corner of the internet"
    , pageKeywords = Nothing
    }
  $ div_ [class_ "content-container"]
  $ main_ [class_ "page-home content-container"]
  $ do
    header_ [class_ "home-page-header"] $ do
      h1_ [class_ "brand"] "~gtf"
      p_ [class_ "health-warning"] "a work in progress"
    navbar (UrlPath "/")
    section_ [class_ "bio"]
      $ toHtmlRaw
        [djot|
      I am one of two cofounders of [Converge](https://www.converge.io), a startup we founded in late 2014 to build a physical intelligence layer for the construction industry in order to address the glaring efficiency and sustainability problems in what I would argue is one of the world's most fundamental industries. As CTO I lead the technical functions in the business and spend most of my time on technical architecture and vision.

      Before founding Converge, I was a star-fancier-in-training at King's College, Cambridge, where I read Natural Sciences, followed by an MSci in Astrophysics in which I completed my thesis on [the temperature bounds on x-ray jets in the solar atmosphere](/projects/x-ray-jets-solar-atmosphere).

      I have been programming for most of my life, and have picked up a nice handful of languages along the way, some of which I have really enjoyed (Haskell, Clojure, Javascript) and some of which I really have not (most especially Golang and Java). I have very strong views on programming, [some of which you might find on this site](/musings), and, these days, write almost everything in Haskell. I also use arch, btw.
      |]
    img_ [src_ "/assets/desk.png", alt_ "keyboard with archlinux slate"]
    toHtmlRaw
      [djot|
      My core interests are in architecture and the built environment (especially the construction of future environments and how such environments influence the social structures that develop); the intersection of human rights and cyberspace; and inducing social change to tackle large-scale human problems like climate change (as a counterpoint to techno-optimism/fatalism).

      You can see my projects either on [sourcehut](https://sr.ht/~gtf) or [github](https://github.com/gfarrell) (I can't make my mind up, but am transitioning away from github very slowly), and you can talk to me by emailing gideon [at] gtf [dot] io.
      |]
