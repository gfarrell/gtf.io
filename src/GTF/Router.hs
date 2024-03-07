module GTF.Router (routes) where

import CommonPrelude
import Data.ByteString (fromStrict)
import GTF.Assets (Asset (..), AssetClass (..), loadAsset)
import GTF.Pages.Colophon qualified as Colophon
import GTF.Pages.Error qualified as Pages
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, renderBS)
import Network.HTTP.Types (ResponseHeaders, Status, status200, status404)
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
  ["styles", n] -> case loadAsset $ Asset Stylesheet n of
    Nothing -> res . sendWith status404 . Pages.error404 . UrlPath $ rawPathInfo req
    Just content -> res . responseLBS status200 [("Content-Type", "text/css")] . fromStrict $ content
  ["colophon"] -> page . Colophon.content . UrlPath $ rawPathInfo req
  _ -> res . sendWith status404 . Pages.error404 . UrlPath $ rawPathInfo req
  where
    page :: Html () -> IO ResponseReceived
    page = res . sendWith status200
