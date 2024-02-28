{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTF.Assets
  ( cssFile,
  )
where

import CommonPrelude
import Data.List (intercalate)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GTF.URL (UrlPath (..))
import Language.Haskell.TH (ExpQ, Q, runIO)
import System.Directory (doesDirectoryExist, getDirectoryContents, listDirectory)
import System.FilePath (takeFileName)

-- | Test if one string is the prefix of another. NB the empty string is the
-- prefix of all strings, so you have to be careful with that case.
isPrefixOf :: String -> String -> Bool
isPrefixOf a b = take (length a) b == a

data AssetClass
  = Stylesheet
  | Script
  | Image
  | File
  deriving (Show, Eq)

{-# INLINE localAssetsRoot #-}
localAssetsRoot :: FilePath
localAssetsRoot = "dist"

{-# INLINE assetDir #-}
assetDir :: AssetClass -> FilePath
assetDir Stylesheet = "assets/styles"
assetDir Script = "assets/scripts"
assetDir Image = "assets/img"
assetDir File = "assets/misc"

cssFile :: String -> ExpQ
cssFile fname = do
  let localDir = intercalate "/" [localAssetsRoot, assetDir Stylesheet]
  exists <- runIO . doesDirectoryExist $ localDir
  if not exists
    then fail $ "local directory " <> localDir <> " does not exist"
    else do
      files <- runIO . listDirectory $ localDir
      case filter (matchFile fname) files of
        [] -> fail $ "Cannot locate asset " <> fname
        (f : _) -> [e|pack . intercalate "/" $ [assetDir Stylesheet, f]|]
  where
    matchFile :: String -> FilePath -> Bool
    matchFile test actual =
      test `isPrefixOf` takeFileName actual
