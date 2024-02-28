{-# LANGUAGE TemplateHaskell #-}

module GTF.Pages.Layout
  ( defaultLayout,
  )
where

import CommonPrelude
import Lucid.Base (Html, toHtml)
import Lucid.Html5
import GTF.Assets (cssFile)

siteCss :: Html ()
siteCss = link_ [
    rel_ "stylesheet",
    href_ $(cssFile "main")
  ]

defaultLayout :: Text -> Html () -> Html ()
defaultLayout pageTitle pageContent = html_ $ do
  head_ $ do
    title_ (toHtml pageTitle)
    siteCss
  body_ $ do
    pageContent
