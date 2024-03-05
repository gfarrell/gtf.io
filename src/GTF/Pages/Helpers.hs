module GTF.Pages.Helpers (commaList, internal) where

import CommonPrelude
import Data.String (IsString)
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
