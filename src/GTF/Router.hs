{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module GTF.Router (app) where

import CommonPrelude hiding (elem)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString (ByteString, fromStrict)
import Data.Text (elem, isSuffixOf, pack, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import GTF.Assets (Asset (..), IsPageType, Musing, Project, WholeSite, loadAsset)
import GTF.Pages.Colophon qualified as Colophon
import GTF.Pages.Error qualified as Pages
import GTF.Pages.Home qualified as Home
import GTF.Pages.Musings qualified as Musings
import GTF.Pages.Projects qualified as Projects
import GTF.URL (UrlPath (UrlPath))
import Lucid.Base (Html, renderBS)
import Network.HTTP.Types (ResponseHeaders, Status, status200, status301, status404)
import Network.Mime (defaultMimeMap, defaultMimeType, mimeByExt)
import Network.Wai (
  Application,
  Request (rawPathInfo),
  Response,
  ResponseReceived,
  pathInfo,
  responseLBS,
 )
import System.FilePath (takeFileName)

app :: Application
app req res = case redirects $ pathInfo req of
  Just newloc -> res $ responseLBS status301 [("Location", encodeUtf8 newloc)] mempty
  Nothing -> routes req res

standardHeaders :: ResponseHeaders
standardHeaders =
  [ ("Content-Type", "text/html; charset=utf8")
  ]

sendWith :: Status -> Html () -> Response
sendWith status =
  responseLBS status standardHeaders . renderBS

redirects :: [Text] -> Maybe Text
-- I used to have a project page for this website, but now we have the colophon
redirects ["projects", "gtf_io_website"] = pure "/colophon"
-- There wasm some inconsistency previously between underscores and hyphens. I
-- now use the latter, but we want to preserve the links
redirects [c, n]
  | (c == "musings" || c == "projects") && '_' `elem` n =
      pure $ "/" <> c <> "/" <> replace "_" "-" n
redirects _ = Nothing

routes :: Application
routes req res =
  case pathInfo req of
    -- ASSETS
    ["styles", n] -> renderAsset (Stylesheet n :: Asset WholeSite)
    ["assets", f] -> renderAsset $ mkAsset @WholeSite "/" f
    -- MUSINGS
    ["musings"] -> page Musings.indexPage
    ["musings", n] -> page (Musings.itemPage n)
    ["musings", n, "assets", f] -> renderAsset $ mkAsset @Musing n f
    -- PROJECTS
    ["projects"] -> page Projects.indexPage
    ["projects", n] -> page (Projects.itemPage n)
    ["projects", n, "assets", f] -> renderAsset $ mkAsset @Project n f
    -- OTHER
    ["colophon"] -> page Colophon.content
    [] -> res $ sendWith status200 Home.content
    -- NOT FOUND
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
  getMimeType (File _ f) = mimeByExt defaultMimeMap defaultMimeType f

  mkAsset :: forall t. (IsPageType t) => Text -> Text -> Asset t
  mkAsset scope (pack . takeFileName . unpack -> assetName)
    -- we use `takeFileName` to strip any attempts at exploring the filesystem
    | ".png" `isSuffixOf` assetName = Image scope assetName
    | otherwise = File scope assetName
