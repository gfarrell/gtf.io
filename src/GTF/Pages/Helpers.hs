module GTF.Pages.Helpers (commaList, internal, datetime, humantime) where

import CommonPrelude
import Data.String (IsString)
import Data.Text (pack)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid (Html, toHtml)
import Lucid.Html5

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
  time_ [datetime_ . pack $ iso8601Show d]
    $ toHtml
    $ formatTime defaultTimeLocale "%F" d

humantime :: Day -> Html ()
humantime d =
  time_ [datetime_ . pack $ iso8601Show d]
    $ toHtml
    $ formatTime defaultTimeLocale "%e %b, %Y" d
