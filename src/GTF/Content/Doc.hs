{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveLift #-}

module GTF.Content.Doc
  ( -- * Document data containers
    ContentDoc (..),
    ParsedDoc (..),

    -- * Tools for parsing documents
    DocParseFailure (..),
    parseContentDoc,
  )
where

import CommonPrelude
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Yaml (FromJSON)
import Djot (Doc, ParseOptions (ParseOptions), SourcePosOption (BlockSourcePos), parseDoc)
import GTF.Content.Meta (MetadataParseError (..), parseMeta)
import Language.Haskell.TH.Syntax (Lift)

data DocParseFailure
  = MetadataParseFailure MetadataParseError
  | DocumentParseFailure Text
  deriving (Show, Eq)

class (Lift (DocMeta a), FromJSON (DocMeta a)) => ContentDoc a where
  data DocMeta a

data ParsedDoc a = ParsedDoc
  { meta :: DocMeta a,
    doc :: Doc
  }

deriving instance (Show (DocMeta a)) => Show (ParsedDoc a)

deriving instance (Eq (DocMeta a)) => Eq (ParsedDoc a)

deriving instance (Lift (DocMeta a)) => Lift (ParsedDoc a)

parseContentDoc ::
  (ContentDoc a, FromJSON (DocMeta a)) =>
  ByteString ->
  Either DocParseFailure (ParsedDoc a)
parseContentDoc input = do
  (meta, rawDoc) <- first MetadataParseFailure $ parseMeta input
  processedDoc <- first (DocumentParseFailure . pack) $ parseDoc (ParseOptions BlockSourcePos) rawDoc
  pure $ ParsedDoc meta processedDoc
