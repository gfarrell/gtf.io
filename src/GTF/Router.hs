module GTF.Router (routes) where

import CommonPrelude
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString (ByteString, fromStrict)
import GTF.Assets (Asset (..), Musing, WholeSite, loadAsset)
import GTF.Pages.Colophon qualified as Colophon
import GTF.Pages.Error qualified as Pages
import GTF.Pages.Musings qualified as Musings
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, renderBS)
import Network.HTTP.Types (ResponseHeaders, Status, status200, status404)
import Network.Mime (defaultMimeMap, defaultMimeType, mimeByExt)
import Network.Wai (
  Application,
  Request (rawPathInfo),
  Response,
  ResponseReceived,
  pathInfo,
  responseLBS,
 )

standardHeaders :: ResponseHeaders
standardHeaders =
  [ ("Content-Type", "text/html; charset=utf8")
  ]

sendWith :: Status -> Html () -> Response
sendWith status =
  responseLBS status standardHeaders . renderBS

routes :: Application
routes req res = case pathInfo req of
  ["styles", n] -> renderAsset (Stylesheet n :: Asset WholeSite)
  ["musings"] -> page Musings.indexPage
  ["musings", n] -> page (Musings.itemPage n)
  ["musings", n, "assets", f] -> renderAsset (Image n f :: Asset Musing)
  ["colophon"] -> page Colophon.content
  _ -> page (const Nothing)
 where
  page :: (UrlPath -> Maybe (Html ())) -> IO ResponseReceived
  page content =
    let path = UrlPath . rawPathInfo $ req
     in res $ case content path of
          Just html -> sendWith status200 html
          Nothing -> sendWith status404 $ Pages.error404 path

  renderAsset :: Asset t -> IO ResponseReceived
  renderAsset asset =
    runMaybeT (loadAsset asset) >>= \case
      Nothing -> page (const Nothing)
      Just content ->
        res
          $ responseLBS
            status200
            [("Content-Type", getMimeType asset)]
          . fromStrict
          $ content

  getMimeType :: Asset t -> ByteString
  getMimeType (Stylesheet{}) = "text/css"
  getMimeType (Image _ f) = mimeByExt defaultMimeMap defaultMimeType f
