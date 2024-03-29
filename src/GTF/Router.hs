module GTF.Router (routes) where

import CommonPrelude
import Data.ByteString (ByteString, fromStrict)
import GTF.Assets (Asset (..), AssetClass (..), loadAsset)
import GTF.Pages.Colophon qualified as Colophon
import GTF.Pages.Error qualified as Pages
import GTF.Pages.Musings qualified as Musings
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, renderBS)
import Network.HTTP.Types (ResponseHeaders, Status, status200, status404)
import Network.Mime (defaultMimeMap, defaultMimeType, mimeByExt)
import Network.Wai (Application, Request (rawPathInfo), Response, ResponseReceived, pathInfo, responseLBS)

standardHeaders :: ResponseHeaders
standardHeaders =
  [ ("Content-Type", "text/html")
  ]

sendWith :: Status -> Html () -> Response
sendWith status =
  responseLBS status standardHeaders . renderBS

routes :: Application
routes req res = case pathInfo req of
  ["styles", n] -> renderAsset (Asset Stylesheet n)
  ["musings"] -> page Musings.indexPage
  ["musings", n] -> page (Musings.itemPage n)
  ["colophon"] -> page Colophon.content
  _ -> page (const Nothing)
  where
    page :: (UrlPath -> Maybe (Html ())) -> IO ResponseReceived
    page content =
      let path = UrlPath . rawPathInfo $ req
       in res $ case content path of
            Just html -> sendWith status200 html
            Nothing -> sendWith status404 $ Pages.error404 path

    renderAsset :: Asset -> IO ResponseReceived
    renderAsset asset =
      case loadAsset asset of
        Nothing -> page (const Nothing)
        Just content ->
          res
            $ responseLBS
              status200
              [("Content-Type", getMimeType asset)]
            . fromStrict
            $ content

    getMimeType :: Asset -> ByteString
    getMimeType (Asset Stylesheet _) = "text/css"
    getMimeType (Asset Script _) = "text/javascript"
    getMimeType (Asset _ f) = mimeByExt defaultMimeMap defaultMimeType f
