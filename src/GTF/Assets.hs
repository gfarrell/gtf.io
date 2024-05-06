{-# LANGUAGE ScopedTypeVariables #-}

module GTF.Assets (
  loadAsset,
  Asset (..),
  Musing,
  Project,
  WholeSite,
  IsPageType (..),
)
where

import Clay (Css, render)
import CommonPrelude hiding (readFile)
import Control.Exception (IOException, try)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.ByteString (ByteString, readFile)
import Data.Either.Combinators (rightToMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import GTF.Content.Musings (Musing)
import GTF.Content.Projects (Project)
import GTF.Style.Main qualified as MainStylesheet
import Paths_gtf_website (getDataFileName)
import System.Posix (fileExist)

data WholeSite

class IsPageType t where
  getAssetDir :: Proxy t -> Text -> FilePath

instance IsPageType Musing where
  getAssetDir _ p = unpack $ "src/GTF/Pages/Musings/content/" <> p <> "/assets"

instance IsPageType WholeSite where
  getAssetDir _ _ = "src/GTF/Pages/content/assets"

instance IsPageType Project where
  getAssetDir _ p = unpack $ "src/GTF/Pages/Projects/content/" <> p <> "/assets"

data Asset t where
  Stylesheet :: (IsPageType t) => Text -> Asset t
  Image :: (IsPageType t) => Text -> Text -> Asset t
  File :: (IsPageType t) => Text -> Text -> Asset t

loadFile :: FilePath -> MaybeT IO ByteString
loadFile fp =
  lift (fileExist fp) >>= \case
    True -> MaybeT . fmap rightToMaybe $ readExcept fp
    False -> MaybeT $ pure Nothing
 where
  readExcept :: FilePath -> IO (Either IOException ByteString)
  readExcept = try . readFile

loadAsset :: forall t. Asset t -> MaybeT IO ByteString
loadAsset (Stylesheet name) =
  MaybeT
    . pure
    $ encodeUtf8
    . toStrict
    . render
    <$> getCss name
loadAsset (Image p n) =
  let px = Proxy :: Proxy t
   in lift (getDataFileName $ getAssetDir px p <> "/" <> unpack n) >>= loadFile
loadAsset (File p n) =
  let px = Proxy :: Proxy t
   in lift (getDataFileName $ getAssetDir px p <> "/" <> unpack n) >>= loadFile

getCss :: Text -> Maybe Css
getCss "main.css" = pure MainStylesheet.stylesheet
getCss _ = Nothing
