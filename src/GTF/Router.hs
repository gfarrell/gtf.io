{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module GTF.Router (app, LogLevel (..)) where

import CommonPrelude hiding (elem, putStrLn, unwords)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString (ByteString, fromStrict)
import Data.Text (elem, isSuffixOf, pack, replace, unpack, unwords)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (hPutStrLn)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
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
  Request (..),
  Response,
  ResponseReceived,
  pathInfo,
  responseLBS,
 )
import System.FilePath (takeFileName)
import System.IO (hFlush, stdout)

data LogLevel = None | RequestsOnly | Verbose
  deriving (Show, Eq, Ord)

putLog :: Text -> IO ()
putLog msg =
  let h = stdout
   in getCurrentTime >>= \now -> do
        hPutStrLn h $ pack (iso8601Show now) <> ": " <> msg
        hFlush h

app :: LogLevel -> Application
app logLevel req res =
  logRequest
    >> case redirects $ pathInfo req of
      Just newloc -> logRedir newloc >> res (responseLBS status301 [("Location", encodeUtf8 newloc)] mempty)
      Nothing -> routes req res
 where
  logRequest :: IO ()
  logRequest
    | logLevel == None = pure ()
    | otherwise =
        putLog
          . unwords
          . (decodeUtf8 <$>)
          $ [ "REQ"
            , requestMethod req
            , rawPathInfo req
            ]

  logRedir :: Text -> IO ()
  logRedir newloc
    | logLevel < Verbose = pure ()
    | otherwise =
        putLog
          . unwords
          $ [ "RDR"
            , decodeUtf8 $ rawPathInfo req
            , "->"
            , newloc
            ]

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
