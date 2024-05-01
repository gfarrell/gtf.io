module GTF.Pages.Layout (
  defaultLayout,
  defaultLayoutWithMeta,
  plainLayoutWithMeta,
  PageMeta (..),
)
where

import CommonPrelude
import Data.Text (intercalate)
import GTF.Pages.Partials.Footer (footer)
import GTF.Pages.Partials.Nav (navbar)
import GTF.URL (UrlPath)
import Lucid.Base (Html, toHtml)
import Lucid.Html5

data PageMeta = PageMeta
  { pageTitle :: Text
  , pageDescription :: Maybe Text
  , pageKeywords :: Maybe [Text]
  }
  deriving (Show, Eq)

basicMeta :: Text -> PageMeta
basicMeta title = PageMeta title Nothing Nothing

siteCss :: Html ()
siteCss =
  link_
    [ rel_ "stylesheet"
    , href_ "/styles/main.css"
    ]

defaultLayout :: UrlPath -> Text -> Html () -> Html ()
defaultLayout currentPath pageTitle =
  defaultLayoutWithMeta currentPath (basicMeta pageTitle)

defaultLayoutWithMeta :: UrlPath -> PageMeta -> Html () -> Html ()
defaultLayoutWithMeta currentPath metadata pageContent = html_ $ do
  head_ $ do
    title_ $ "GTF :: " <> toHtml (pageTitle metadata)
    maybe mempty (\c -> meta_ [name_ "description", content_ c])
      $ pageDescription metadata
    maybe mempty (\tags -> meta_ [name_ "keywords", content_ $ intercalate ", " tags])
      $ pageKeywords metadata
    siteCss
  body_ $ do
    header_ [class_ "site-header"] $ do
      navbar currentPath
    div_ [class_ "top-container"]
      $ div_ [class_ "content-container"]
      $ main_ [class_ "content-container", role_ "main"] pageContent
    footer

plainLayoutWithMeta :: PageMeta -> Html () -> Html ()
plainLayoutWithMeta metadata content = html_ $ do
  head_ $ do
    title_ $ "GTF :: " <> toHtml (pageTitle metadata)
    maybe mempty (\c -> meta_ [name_ "description", content_ c])
      $ pageDescription metadata
    maybe mempty (\tags -> meta_ [name_ "keywords", content_ $ intercalate ", " tags])
      $ pageKeywords metadata
    siteCss
  body_ $ div_ [class_ "top-container"] content
