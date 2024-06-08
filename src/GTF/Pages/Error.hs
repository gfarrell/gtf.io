module GTF.Pages.Error (
  error404,
)
where

import CommonPrelude
import Data.Text.Encoding (decodeUtf8)
import GTF.Pages.Layout (defaultLayout)
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, toHtml)
import Lucid.Html5

error404 :: UrlPath -> Html ()
error404 currentPath@(UrlPath path) = defaultLayout currentPath "We've lost it, precious!"
  $ section_ [class_ "error-page error-404"]
  $ do
    h1_ "I just don't know where it is!"
    p_
      . toHtml
      $ "Sadly "
      <> decodeUtf8 path
      <> " isn't something I know about, perhaps go back and try again."
