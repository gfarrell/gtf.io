{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTF.Assets
  ( cssFile,
    loadAsset,
    Asset (..),
    AssetClass (..),
  )
where

import Clay (Css, render)
import CommonPrelude
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import GTF.Style.Main qualified as MainStylesheet
import Language.Haskell.TH (ExpQ, runIO)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension)

-- | Test if one string is the prefix of another. NB the empty string is the
-- prefix of all strings, so you have to be careful with that case.
isPrefixOf :: String -> String -> Bool
isPrefixOf a b = take (length a) b == a

data Asset = Asset AssetClass Text

loadAsset :: Asset -> Maybe ByteString
loadAsset (Asset Stylesheet name) = encodeUtf8 . toStrict . render <$> getCss name
loadAsset _ = Nothing

getCss :: Text -> Maybe Css
getCss "main.css" = pure MainStylesheet.stylesheet
getCss _ = Nothing

data AssetClass
  = Stylesheet
  | Script
  | Image
  | File
  deriving (Show, Eq)

localAssetsRoot :: FilePath
localAssetsRoot = "dist"

assetDir :: AssetClass -> FilePath
assetDir Stylesheet = "assets/styles"
assetDir Script = "assets/scripts"
assetDir Image = "assets/img"
assetDir File = "assets/misc"

matchVersionedAsset :: String -> String -> [FilePath] -> Maybe FilePath
matchVersionedAsset name ext files =
  case filter match' files of
    [] -> Nothing
    (f : _) -> Just f
  where
    match' :: FilePath -> Bool
    match' p = "." <> ext == takeExtension p && name `isPrefixOf` takeBaseName p

cssFile :: String -> ExpQ
cssFile fname = do
  let localDir = intercalate "/" [localAssetsRoot, assetDir Stylesheet]
  exists <- runIO . doesDirectoryExist $ localDir
  if not exists
    then fail $ "local directory " <> localDir <> " does not exist"
    else do
      files <- runIO . listDirectory $ localDir
      case matchVersionedAsset fname "css" files of
        Nothing -> fail $ "Cannot locate asset " <> fname
        Just f ->
          let path = "/" <> intercalate "/" [assetDir Stylesheet, f]
           in [e|path|]
