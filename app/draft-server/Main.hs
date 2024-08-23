{-# LANGUAGE DerivingStrategies #-}

module Main where

import Clay (render)
import CommonPrelude
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Text.Encoding (encodeUtf8)
import GTF.Content.Doc (DocParseFailure, parseContentDoc)
import GTF.Content.Musings (Musing)
import GTF.Content.Projects (Project)
import GTF.DraftServ.Github (DocFile (..), DocType (MusingDoc, ProjectDoc), GitRef (..), getDoc)
import GTF.Pages.Musings (renderMusingContent)
import GTF.Pages.Projects (renderProjectContent)
import GTF.Style.Main qualified as Styles
import Lucid (Html, ToHtml (..), renderBS)
import Lucid.Html5
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (ResponseHeaders, Status (..), status200, status404, status500)
import Network.Wai (Application, ResponseReceived, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import Servant.Client (BaseUrl (BaseUrl), ClientError (..), Scheme (Https), mkClientEnv, runClientM)
import Servant.Client.Core.Response (responseBody, responseStatusCode)
import System.Environment (getArgs)
import Text.Read (readEither)

renderDoc :: DocType -> ByteString -> Either DocParseFailure (Html ())
renderDoc MusingDoc = fmap renderMusingContent . parseContentDoc @Musing
renderDoc ProjectDoc = fmap renderProjectContent . parseContentDoc @Project

data AppError
  = FetchFailure ClientError
  | ParseFailure DocParseFailure
  deriving (Show, Eq)

class HasAppError e where
  toAppError :: e -> AppError

instance HasAppError ClientError where
  toAppError = FetchFailure

instance HasAppError DocParseFailure where
  toAppError = ParseFailure

standardHeaders :: ResponseHeaders
standardHeaders =
  [ ("Content-Type", "text/html; charset=utf8")
  ]

router :: Manager -> Application
router manager req res = case pathInfo req of
  [ref, "musing", fname] ->
    respond (GitRef ref) MusingDoc (DocFile fname)
  [ref, "project", fname] ->
    respond (GitRef ref) ProjectDoc (DocFile fname)
  _ ->
    res
      $ responseLBS status404 standardHeaders
      . renderBS
      $ errorPage status404 ("Unknown path" :: Text)
 where
  respond :: GitRef -> DocType -> DocFile -> IO ResponseReceived
  respond ref dtype fname =
    renderDocPage ref dtype fname >>= \case
      (Left (FetchFailure (FailureResponse _ res'))) ->
        let status = responseStatusCode res'
            msg = responseBody res'
         in res
              $ responseLBS status standardHeaders
              . renderBS
              $ errorPage status msg
      (Left err) ->
        res
          $ responseLBS status500 standardHeaders
          . renderBS
          $ errorPage status500 (show err)
      (Right content) ->
        res
          $ responseLBS status200 standardHeaders
          . renderBS
          . layout ref dtype fname
          $ content

  errorPage :: (ToHtml a) => Status -> a -> Html ()
  errorPage status msg = doctypehtml_
    $ do
      head_ $ title_ "Something went wrong"
      body_ $ do
        h1_ $ "Unable to load document (" <> (toHtml . show . statusCode $ status) <> ")"
        pre_ $ toHtml msg

  renderDocPage ref dtype fname =
    fetchContent ref dtype fname
      <&> (first toAppError >=> (first toAppError . renderDoc dtype . encodeUtf8))

  fetchContent ref dtype fname =
    let baseUrl = BaseUrl Https "raw.githubusercontent.com" 443 ""
     in runClientM (getDoc ref dtype fname) (mkClientEnv manager baseUrl)

layout :: GitRef -> DocType -> DocFile -> Html () -> Html ()
layout ref dtype fname content = doctypehtml_ $ do
  let titleText =
        fold
          [ "DRAFT@"
          , toHtml ref
          , " of "
          , toHtml dtype
          , "/"
          , toHtml fname
          ]
  head_ $ do
    title_ titleText
    link_
      [ rel_ "stylesheet"
      , href_
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/base16/papercolor-light.min.css"
      ]
    style_ [] $ toHtmlRaw $ render Styles.stylesheet
  body_ $ do
    header_ [class_ "site-header"] $ do
      div_ [class_ "brand"] "~gtf"
      div_ [] titleText
    div_ [class_ "top-container"]
      $ div_ [class_ "content-container"]
      $ main_ [class_ "content-container", role_ "main"] content

newtype Args = Args {port :: Int}
  deriving (Show, Eq)

data ProgrammeError = InvalidPort | WrongNumberOfArgs
  deriving (Show, Eq)

getAppArgs :: ExceptT ProgrammeError IO Args
getAppArgs =
  ExceptT $ getArgs <&> \case
    [portString] -> Args <$> (first (const InvalidPort) . readEither $ portString)
    _ -> Left WrongNumberOfArgs

main :: IO ()
main = do
  res <-
    runExceptT $ getAppArgs >>= \(Args port) -> do
      liftIO . putStrLn $ "Running on " <> show port
      liftIO $ newManager tlsManagerSettings >>= run port . router
  case res of
    Left err -> putStrLn $ "An error occured: " <> show err
    Right _ -> putStrLn "Exited"
