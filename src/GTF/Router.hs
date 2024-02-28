module GTF.Router (routes) where

import CommonPrelude
import GTF.Pages.Error qualified as Pages
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, renderBS)
import Network.HTTP.Types (ResponseHeaders, Status, status404)
import Network.Wai (Application, Request (rawPathInfo), Response, pathInfo, responseLBS)

standardHeaders :: ResponseHeaders
standardHeaders =
  [ ("Content-Type", "text/html")
  ]

sendWith :: Status -> Html () -> Response
sendWith status =
  responseLBS status standardHeaders . renderBS

routes :: Application
routes req res = case pathInfo req of
  _ -> res . sendWith status404 . Pages.error404 . UrlPath $ rawPathInfo req
