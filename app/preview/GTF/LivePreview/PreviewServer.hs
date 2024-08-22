{-# LANGUAGE QuasiQuotes #-}

module GTF.LivePreview.PreviewServer (
  runPreviewServer,
) where

import Clay (render)
import CommonPrelude
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (iii)
import GTF.Content.Doc (ParsedDoc (..))
import GTF.Content.Loader (FileLoadError (..), loadFile)
import GTF.Content.Musings (Musing)
import GTF.Content.Projects (Project)
import GTF.LivePreview.Options (DocType (..))
import GTF.Pages.Musings (renderMusingContent)
import GTF.Pages.Projects (renderProjectContent)
import GTF.Style.Main qualified as Styles
import Lucid (Html, renderBS, toHtml, toHtmlRaw)
import Lucid.Html5
import Network.HTTP.Types (ResponseHeaders, status200, status404, status500)
import Network.Wai (Application, Response, responseLBS)

layout :: FilePath -> Html () -> Html ()
layout filename content = doctypehtml_ $ do
  head_ $ do
    title_ $ "Live Preview: " <> toHtml filename
    link_
      [ rel_ "stylesheet"
      , href_
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/base16/papercolor-light.min.css"
      ]
    style_ [] $ toHtmlRaw $ render Styles.stylesheet
  body_ $ do
    header_ [class_ "site-header"] $ do
      div_ [class_ "brand"] "~gtf"
      div_ [] $ "Viewing: " <> toHtml filename
      div_ [id_ "lr-indicator"] "waiting..."
    div_ [class_ "top-container"]
      $ div_ [class_ "content-container"]
      $ main_ [class_ "content-container", role_ "main"] content
    script_ [] $ toHtmlRaw wsScript
 where
  wsScript :: Text
  wsScript =
    [iii|
      function doReload () {
        window.location.reload();
      }

      function connectWS () {
        const i = document.querySelector("\#lr-indicator");

        const setIndicator = (state) => {
          if(i !== null) {
            switch(state) {
              case 1:
                i.innerHTML = "waiting for connection";
                break;
              case 2:
                i.innerHTML = "connected";
                break;
              case 3:
                i.innerHTML = "reloading...";
                break;
              default:
                i.innerHTML = "unknown state!";
            }
          }
        };

        setIndicator(1);

        const ws = new WebSocket("ws://127.0.0.1:8081");
        ws.addEventListener("open", () => {
          console.log("LR: Connected");
          setIndicator(2);
        });
        ws.addEventListener("message", (event) => {
          console.log("LR: Received: ", event.data);
          if(event.data.startsWith("LRUpdate")) {
            setIndicator(3);
            setTimeout(doReload, 100);
            ws.close();
          }
        });
      }

      connectWS();
    |]

-- | File renderer
renderFile :: forall m. (MonadIO m) => DocType -> FilePath -> ExceptT FileLoadError m (Html ())
renderFile MusingDoc filename = loadFile (Proxy @Musing) filename >>= makeHtml
 where
  makeHtml :: ParsedDoc Musing -> ExceptT FileLoadError m (Html ())
  makeHtml doc = ExceptT $ return . Right $ layout filename $ renderMusingContent doc
renderFile ProjectDoc filename = loadFile (Proxy @Project) filename >>= makeHtml
 where
  makeHtml :: ParsedDoc Project -> ExceptT FileLoadError m (Html ())
  makeHtml doc = ExceptT $ return . Right $ layout filename $ renderProjectContent doc

standardHeaders :: ResponseHeaders
standardHeaders =
  [ ("Content-Type", "text/html; charset=utf8")
  ]

runPreviewServer :: DocType -> FilePath -> Application
runPreviewServer docType filepath _ res = do
  runExceptT (renderFile docType filepath) >>= \case
    Left err -> do
      putStrLn $ "Error: " <> show err
      res $ errorResponse err
    Right content -> do
      putStrLn $ "Loading " <> filepath
      res $ contentResponse content
 where
  errorResponse :: FileLoadError -> Response
  errorResponse err =
    let status = case err of
          FileNotFound _ -> status404
          _ -> status500
     in responseLBS status standardHeaders
          . renderBS
          $ html_
          $ do
            head_ $ title_ "Something went wrong"
            body_ $ do
              h1_ "Unable to load document"
              p_ $ "When loading " <> toHtml filepath <> " an error occurred: "
              pre_ $ toHtml (show err)

  contentResponse :: Html () -> Response
  contentResponse = responseLBS status200 standardHeaders . renderBS
