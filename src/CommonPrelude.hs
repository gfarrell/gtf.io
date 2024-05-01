{-# OPTIONS_GHC -Wno-orphans #-}

module CommonPrelude (
  module X,

  -- * Text Utilities
  Text,

  -- * Useful operators for monads and functors
  ($>),
  (<&>),
  (>=>),
  (<|>),
)
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.ByteString.Builder (toLazyByteString)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import Data.Time (Day)
import Djot (Doc, renderHtml)
import Djot.Djot (RenderOptions (RenderOptions))
import Language.Haskell.TH.Syntax (Lift (..), liftData)
import Lucid (ToHtml (..))
import Prelude as X

instance Lift Day where
  lift = liftData

instance ToHtml Doc where
  toHtmlRaw = toHtmlRaw . toLazyByteString . renderHtml (RenderOptions False)
  toHtml = toHtml . toLazyByteString . renderHtml (RenderOptions False)
