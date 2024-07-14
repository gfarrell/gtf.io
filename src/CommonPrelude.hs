{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Text (Text, unpack)
import Data.Time (Day)
import Djot (Doc, renderHtml)
import Djot.Djot (RenderOptions (RenderOptions))
import Language.Haskell.TH.Syntax (Lift (..), liftData)
import Lucid (ToHtml (..))
import Prelude as X
#if !MIN_VERSION_aeson(2,2,0)
import Data.Aeson (FromJSON (parseJSON), withText)
import Network.URI (URI, parseURI)
#endif

instance Lift Day where
  lift = liftData

instance ToHtml Doc where
  toHtmlRaw = toHtmlRaw . toLazyByteString . renderHtml (RenderOptions False)
  toHtml = toHtml . toLazyByteString . renderHtml (RenderOptions False)

#if !MIN_VERSION_aeson(2,2,0)
-- The reason we don't specify the aeson version is that nixpkgs defaults to a
-- lower version, which is _fine_ except if we enforce >=2.2.0.0 then we get
-- build errors with various HLS plugins and this is just very tedious.
instance FromJSON URI where
  parseJSON = withText "URI" $ \(unpack -> t) ->
    case parseURI t of
      Just uri -> pure uri
      Nothing -> fail $ "invalid uri: " <> t
#endif
