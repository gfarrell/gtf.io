{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTF.Pages.Musings (
  indexPage,
  itemPage,
  renderMusingContent,
)
where

import CommonPrelude
import Data.ByteString.Builder (toLazyByteString)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Proxy (Proxy (Proxy))
import Djot (RenderOptions (..))
import Djot.Html (renderHtml)
import GTF.Content.Doc (ParsedDoc (..), generateToc, wordcount)
import GTF.Content.Loader (isDjot, loadFilesTH)
import GTF.Content.Musings (Category (..), DocMeta (..), Musing)
import GTF.Pages.Helpers (datetime, humantime)
import GTF.Pages.Layout (PageMeta (..), defaultLayout, defaultLayoutWithMeta)
import GTF.Pages.Partials.Highlight (highlight)
import GTF.Pages.Partials.TableOfContents (renderToc)
import GTF.URL (UrlPath)
import Lucid (Html, ToHtml (toHtml, toHtmlRaw))
import Lucid.Html5

musings :: [ParsedDoc Musing]
musings =
  sortOn
    (Down . created . meta)
    $(loadFilesTH (Proxy @Musing) "src/GTF/Pages/Musings/content" isDjot)

indexPage :: UrlPath -> Maybe (Html ())
indexPage currentPath = Just $ defaultLayout currentPath "All Musings" $ do
  header_ $ do
    h1_ $ toHtmlRaw ("muse | mju&#720;z |" :: String)
    p_ [class_ "defn-type"] $ i_ "verb [no object]"
    p_ [class_ "defn-descr"] $ do
      "be absorbed in thought: "
      span_
        [class_ "defn-example"]
        "he was musing on the problems he faced."
  hr_ mempty
  makeSection "General Essays" $ filter ((== General) . category . meta) musings
  makeSection "Informatics" $ filter ((== Informatics) . category . meta) musings
  makeSection "Reflections" $ filter ((== Reflection) . category . meta) musings
 where
  makeSection :: Text -> [ParsedDoc Musing] -> Html ()
  makeSection sectionTitle list =
    if null list
      then mempty
      else section_ [class_ "index_section"] $ do
        h2_ $ toHtml sectionTitle
        ul_ [class_ "page-list"]
          $ foldMap
            ( \(ParsedDoc m _) ->
                li_
                  $ a_
                    [ title_ $ title m
                    , href_ ("/musings/" <> slug m)
                    ]
                  $ toHtml (title m)
                  <> " ("
                  <> (datetime . created $ m)
                  <> ")"
            )
            list

renderMusingContent :: ParsedDoc Musing -> Html ()
renderMusingContent (ParsedDoc m d) = article_ $ do
  header_ $ do
    h1_ . toHtml $ title m
    p_ [class_ "subtitle"] $ do
      humantime $ created m
      ", " <> toHtml (show $ wordcount d) <> " words"
  if toc m then div_ [class_ "table-of-contents"] $ renderToc $ generateToc d else mempty
  div_ [class_ "item-content"]
    . toHtmlRaw
    . toLazyByteString
    $ renderHtml (RenderOptions False) d
  highlight

itemPage :: Text -> UrlPath -> Maybe (Html ())
itemPage name currentPath =
  case filter ((== name) . slug . meta) musings of
    [doc@(ParsedDoc m _)] ->
      pure
        $ defaultLayoutWithMeta currentPath (PageMeta (title m) (abstract m) (tags m))
        $ renderMusingContent doc
    _ -> Nothing
