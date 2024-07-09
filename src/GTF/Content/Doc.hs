{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GTF.Content.Doc (
  -- * Document data containers
  ContentDoc (..),
  ParsedDoc (..),

  -- * Tools for parsing documents
  DocParseFailure (..),
  parseContentDoc,

  -- * Computing information about documents

  -- ** Tables of contents
  TableOfContents (..),
  SectionLink (..),
  generateToc,

  -- ** Counting words
  wordcount,
)
where

import CommonPrelude
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (fold), toList)
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml (FromJSON)
import Djot (
  Attr (Attr),
  Block (..),
  Caption (Caption),
  Cell (Cell),
  Doc (docBlocks),
  Inline (..),
  Inlines,
  Many (Many),
  Node (Node),
  ParseOptions (ParseOptions),
  SourcePosOption (BlockSourcePos),
  parseDoc,
 )
import GTF.Content.Meta (MetadataParseError (..), parseMeta)
import Language.Haskell.TH.Syntax (Lift)

data DocParseFailure
  = MetadataParseFailure MetadataParseError
  | DocumentParseFailure Text
  deriving (Show, Eq)

class (Lift (DocMeta a), FromJSON (DocMeta a)) => ContentDoc a where
  data DocMeta a

data ParsedDoc a = ParsedDoc
  { meta :: DocMeta a
  , doc :: Doc
  }

newtype SectionLink = SectionLink Text
  deriving (Show, Eq)

data TableOfContents
  = TOC [TableOfContents]
  | TOCSection Text SectionLink [TableOfContents]
  deriving (Show, Eq)

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

toToc :: Node Block -> Maybe TableOfContents
toToc (Node _ attrs (Section (Many contents))) =
  case contents of
    ((Node _ _ (Heading _ ils)) Seq.:<| rest) ->
      TOCSection (getTextContent ils)
        <$> getSectionLink attrs
        <*> pure (mapMaybe toToc . toList $ rest)
    _ -> Nothing
 where
  getTextContent :: Inlines -> Text
  getTextContent = fold . mapMaybe asText . toList
   where
    asText :: Node Inline -> Maybe Text
    asText (Node _ _ il) = case il of
      (Str bs) -> pure $ decodeUtf8 bs
      (Emph ils) -> pure $ getTextContent ils
      (Strong ils) -> pure $ getTextContent ils
      (Highlight ils) -> pure $ getTextContent ils
      (Insert ils) -> pure $ getTextContent ils
      (Delete ils) -> pure $ getTextContent ils
      (Superscript ils) -> pure $ getTextContent ils
      (Subscript ils) -> pure $ getTextContent ils
      (Verbatim bs) -> pure $ decodeUtf8 bs
      (Symbol bs) -> pure $ decodeUtf8 bs
      (Math _ bs) -> pure $ decodeUtf8 bs
      (Link ils _) -> pure $ getTextContent ils
      (Image ils _) -> pure $ getTextContent ils
      (Span ils) -> pure $ getTextContent ils
      (FootnoteReference bs) -> pure $ decodeUtf8 bs
      (UrlLink bs) -> pure $ decodeUtf8 bs
      (EmailLink bs) -> pure $ decodeUtf8 bs
      (RawInline _ bs) -> pure $ decodeUtf8 bs
      NonBreakingSpace -> Nothing
      (Quoted _ ils) -> pure $ getTextContent ils
      SoftBreak -> Nothing
      HardBreak -> Nothing
  getSectionLink :: Attr -> Maybe SectionLink
  getSectionLink (Attr (("id", sectionId) : _)) = pure $ SectionLink (decodeUtf8 sectionId)
  getSectionLink (Attr []) = Nothing
  getSectionLink (Attr (_ : rest)) = getSectionLink (Attr rest)
toToc _ = Nothing

generateToc :: Doc -> TableOfContents
generateToc doc = TOC . mapMaybe toToc . toList $ docBlocks doc

class HasWordCount a where
  wordcount :: a -> Int

instance HasWordCount Inline where
  wordcount (Str bs) = wordcount bs
  wordcount (Emph ils) = wordcount ils
  wordcount (Strong ils) = wordcount ils
  wordcount (Highlight ils) = wordcount ils
  wordcount (Insert ils) = wordcount ils
  wordcount (Delete ils) = wordcount ils
  wordcount (Superscript ils) = wordcount ils
  wordcount (Subscript ils) = wordcount ils
  wordcount (Verbatim bs) = wordcount bs
  wordcount (Symbol bs) = wordcount bs
  wordcount (Math _ bs) = wordcount bs
  wordcount (Link ils _) = wordcount ils
  wordcount (Image ils _) = wordcount ils
  wordcount (Span ils) = wordcount ils
  wordcount (FootnoteReference bs) = wordcount bs
  wordcount (UrlLink bs) = wordcount bs
  wordcount (EmailLink bs) = wordcount bs
  wordcount (RawInline _ bs) = wordcount bs
  wordcount NonBreakingSpace = 0
  wordcount (Quoted _ ils) = wordcount ils
  wordcount SoftBreak = 0
  wordcount HardBreak = 0

instance HasWordCount ByteString where
  wordcount = length . BS.words

instance (HasWordCount a, Foldable t) => HasWordCount (t a) where
  wordcount = foldr ((+) . wordcount) 0

instance HasWordCount Caption where
  wordcount (Caption bls) = wordcount bls

instance HasWordCount Cell where
  wordcount (Cell _ _ ils) = wordcount ils

instance HasWordCount Block where
  wordcount (Para ils) = wordcount ils
  wordcount (Section bls) = wordcount bls
  wordcount (Heading _ ils) = wordcount ils
  wordcount (BlockQuote bls) = wordcount bls
  wordcount (CodeBlock _ _) = 0
  wordcount (Div bls) = wordcount bls
  wordcount (OrderedList _ _ ls) = foldr ((+) . wordcount) 0 ls
  wordcount (BulletList _ ls) = foldr ((+) . wordcount) 0 ls
  wordcount (TaskList _ ls) = sum . map (wordcount . snd) $ ls
  wordcount (DefinitionList _ ls) = sum . map (\x -> wordcount (fst x) + wordcount (snd x)) $ ls
  wordcount ThematicBreak = 0
  wordcount (Table maybeCaption cells) = wordcount maybeCaption + (sum . map wordcount $ cells)
  wordcount (RawBlock _ bs) = wordcount bs

instance HasWordCount Doc where
  wordcount = wordcount . docBlocks
