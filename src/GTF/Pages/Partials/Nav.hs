module GTF.Pages.Partials.Nav (navbar) where

import CommonPrelude
import Data.ByteString (ByteString, isPrefixOf)
import Data.Text (unwords)
import Data.Text.Encoding (decodeUtf8)
import GTF.URL (UrlPath (..))
import Lucid.Base (Html, toHtml)
import Lucid.Html5

navbar :: UrlPath -> Html ()
navbar currentPath =
  nav_ $ do
    input_ [type_ "checkbox", id_ "nav-control"]
    label_ [class_ "nav-label", for_ "nav-control"] "oh the places you'll go"
    div_ [class_ "nav-content"]
      $ foldMap
        mkMenuItem
        [ ("home", "/")
        , ("musings", "/musings")
        , ("projects", "/projects")
        , ("colophon", "/colophon")
        ]
 where
  mkMenuItem :: (Text, ByteString) -> Html ()
  mkMenuItem (label, url) =
    span_ [class_ $ mkClass url]
      $ a_ [href_ . decodeUtf8 $ url] (toHtml label)

  mkClass :: ByteString -> Text
  mkClass url =
    Data.Text.unwords
      $ let c = unUrlPath currentPath
         in "nav-item"
              : ["active" | c == "/" && url == "/" || (url `isPrefixOf` unUrlPath currentPath && url /= "/")]
