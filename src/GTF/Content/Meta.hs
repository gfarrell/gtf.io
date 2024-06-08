module GTF.Content.Meta (
  parseMeta,
  MetadataParseError (..),
) where

import CommonPrelude
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.ByteString (ByteString, pack)
import Data.Void (Void)
import Data.Yaml (FromJSON, decodeEither')
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, manyTill, parse)
import Text.Megaparsec.Byte (string)

type Parser = Parsec Void ByteString

data MetadataParseError
  = MetaSectionMissing
  | MetaParseFailure
  deriving (Show, Eq)

docSep_ :: Parser ()
docSep_ = void $ string "---"

rawMeta_ :: Parser ByteString
rawMeta_ = pack <$> (docSep_ >> manyTill anySingle docSep_)

restOfDoc_ :: Parser ByteString
restOfDoc_ = pack <$> manyTill anySingle eof

parseMeta :: (FromJSON a) => ByteString -> Either MetadataParseError (a, ByteString)
parseMeta bs = do
  (metaBS, docBS) <-
    first (const MetaSectionMissing) $ parse ((,) <$> rawMeta_ <*> restOfDoc_) "file meta" bs
  meta <- first (const MetaParseFailure) $ decodeEither' metaBS
  pure (meta, docBS)
