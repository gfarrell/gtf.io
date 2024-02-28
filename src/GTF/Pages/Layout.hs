module GTF.Pages.Layout
  ( defaultLayout,
  )
where

import CommonPrelude
import Lucid.Base (Html, toHtml)
import Lucid.Html5

siteCss :: Html ()
siteCss = link_ [
    rel_ "stylesheet",
    href_ "main.css"
  ]

defaultLayout :: Text -> Html () -> Html ()
defaultLayout pageTitle pageContent = html_ $ do
  head_ $ do
    title_ (toHtml pageTitle)
    siteCss
  body_ $ do
    pageContent
