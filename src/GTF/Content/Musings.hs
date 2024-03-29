{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module GTF.Content.Musings (Category (..), Musing, DocMeta (..), wordcount) where

import CommonPrelude
import Data.ByteString.Char8 qualified as BS
import Data.Text (unpack)
import Data.Time (Day)
import Data.Yaml (FromJSON (..), withText)
import Djot.AST (Doc (docBlocks), Block (..), Inline (..), Caption (..), Cell (Cell))
import GHC.Generics (Generic)
import GTF.Content.Doc (ContentDoc (..))
import Language.Haskell.TH.Syntax (Lift)
import Data.ByteString (ByteString)

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
    { title :: Text,
      slug :: Text,
      category :: Category,
      created :: Day,
      updated :: Maybe Day,
      abstract :: Maybe Text
    }
    deriving (Show, Eq, Generic, Lift)
    deriving anyclass (FromJSON)

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
  wordcount (docBlocks -> d) = wordcount d
