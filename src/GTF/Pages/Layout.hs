module GTF.Pages.Layout
  ( defaultLayout,
  )
where

import CommonPrelude
import GTF.Pages.Partials.Nav (navbar)
import GTF.URL (UrlPath)
import Lucid.Base (Html, toHtml)
import Lucid.Html5

siteCss :: Html ()
siteCss =
  link_
    [ rel_ "stylesheet",
      href_ "/styles/main.css"
    ]

defaultLayout :: UrlPath -> Text -> Html () -> Html ()
defaultLayout currentPath pageTitle pageContent = html_ $ do
  head_ $ do
    title_ $ "GTF :: " <> toHtml pageTitle
    siteCss
  body_ $ do
    header_ $ do
      navbar currentPath
    main_ [class_ "page-home content-container", role_ "main"] pageContent
