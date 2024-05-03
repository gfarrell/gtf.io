module GTF.Pages.Helpers (commaList, internal, datetime, humantime, now, djot, uri) where

import CommonPrelude
import Data.ByteString.Char8 (pack)
import Data.String (IsString)
import Data.Text (pack)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Djot (ParseOptions (..), SourcePosOption (..), parseDoc)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Q, lift, runIO)
import Lucid (Html, toHtml)
import Lucid.Html5
import Network.URI (parseURI)

commaList :: (IsString s, Monoid s) => [s] -> s
commaList (x : [y]) = x <> " and " <> y
commaList [] = mempty
commaList xs = go' xs
 where
  go' (x : [y]) = x <> ", and " <> y
  go' [x] = x
  go' [] = mempty
  go' (x : rest) = x <> ", " <> go' rest

internal :: Text -> Text -> Html ()
internal target label =
  a_ [href_ $ "/" <> target, title_ label] $ toHtml label

datetime :: Day -> Html ()
datetime d =
  time_ [datetime_ . Data.Text.pack $ iso8601Show d]
    $ toHtml
    $ formatTime defaultTimeLocale "%F" d

humantime :: Day -> Html ()
humantime d =
  time_ [datetime_ . Data.Text.pack $ iso8601Show d]
    $ toHtml
    $ formatTime defaultTimeLocale "%e %b, %Y" d

now :: Q Exp
now = runIO (utctDay <$> getCurrentTime) >>= lift

djot :: QuasiQuoter
djot =
  QuasiQuoter
    { quoteExp = djot' . Data.ByteString.Char8.pack
    , quotePat = error "Usage as a pattern is not supported"
    , quoteType = error "Usage as a type is not supported"
    , quoteDec = error "Usage as a declaration is not supported"
    }
 where
  djot' inp = case parseDoc (ParseOptions NoSourcePos) inp of
    Left err -> fail $ "Invalid source text: " <> err
    Right doc -> lift doc

uri :: QuasiQuoter
uri =
  QuasiQuoter
    { quoteExp = uri'
    , quotePat = error "Usage as a pattern is not supported"
    , quoteType = error "Usage as a type is not supported"
    , quoteDec = error "Usage as a declaration is not supported"
    }
 where
  uri' inp = case parseURI inp of
    Just result -> lift result
    Nothing -> fail $ "Invalid URI: " <> inp
