{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTF.Assets
  ( cssFile,
  )
where

import CommonPrelude
import Data.List (intercalate)
import Data.Text (pack)
import Language.Haskell.TH (ExpQ, runIO)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, takeExtension, takeBaseName)

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
