{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module GTF.Content.Musings (Category (..), Musing, DocMeta (..)) where

import CommonPrelude
import Data.Text (unpack)
import Data.Time (Day)
import Data.Yaml (FromJSON (..), withText)
import GHC.Generics (Generic)
import GTF.Content.Doc (ContentDoc (..))
import Language.Haskell.TH.Syntax (Lift)

data Musing

data Category
  = General
  | Reflection
  | Informatics
  deriving (Show, Eq, Lift)

instance FromJSON Category where
  parseJSON = withText "MusingCategory" $ \case
    "general" -> pure General
    "reflection" -> pure Reflection
    "informatics" -> pure Informatics
    s -> fail $ "Unknown category string: " <> unpack s

instance ContentDoc Musing where
  data DocMeta Musing = MusingMeta
    { title :: Text
    , slug :: Text
    , category :: Category
    , created :: Day
    , updated :: Maybe Day
    , abstract :: Maybe Text
    , tags :: Maybe [Text]
    , toc :: Bool
    }
    deriving (Show, Eq, Generic, Lift)
    deriving anyclass (FromJSON)
