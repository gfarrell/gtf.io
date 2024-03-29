{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTF.Pages.Musings
  ( indexPage,
    itemPage,
  )
where

import CommonPrelude
import Data.ByteString.Builder (toLazyByteString)
import Data.Proxy (Proxy (Proxy))
import Djot (RenderOptions (..))
import Djot.Html (renderHtml)
import GTF.Content.Doc (ParsedDoc (..))
import GTF.Content.Loader (loadFilesTH)
import GTF.Content.Musings (Category (..), DocMeta (..), Musing)
import GTF.Pages.Helpers (datetime, humantime)
import GTF.Pages.Layout (defaultLayout)
import GTF.URL (UrlPath)
import Lucid (Html, ToHtml (toHtml, toHtmlRaw))
import Lucid.Html5

musings :: [ParsedDoc Musing]
musings = $(loadFilesTH (Proxy @Musing) "src/GTF/Pages/Musings/content" (const True))

indexPage :: UrlPath -> Html ()
indexPage currentPath = defaultLayout currentPath "All Musings" $ do
  h1_ $ toHtmlRaw ("muse | mju&#720;z |" :: String)
  p_ $ i_ "verb [no object]"
  p_ $ "by absorbed in thought: " <> i_ "he was musing on the problems he faced."
  hr_ []
  makeSection "General Essays" $ filter ((== General) . category . meta) musings
  makeSection "Informatics" $ filter ((== Informatics) . category . meta) musings
  makeSection "Reflections" $ filter ((== Reflection) . category . meta) musings
  where
    makeSection :: Text -> [ParsedDoc Musing] -> Html ()
    makeSection sectionTitle list =
      section_ [class_ "index_section"] $ do
        h2_ $ toHtml sectionTitle
        ul_
          $ foldMap
            ( \(ParsedDoc m _) ->
                li_
                  $ a_
                    [ title_ $ title m,
                      href_ ("/musings/" <> slug m)
                    ]
                  $ toHtml (title m)
                  <> " ("
                  <> (datetime . created $ m)
                  <> ")"
            )
            list

itemPage :: Text -> UrlPath -> Maybe (Html ())
itemPage name currentPath =
  case filter ((== name) . slug . meta) musings of
    [ParsedDoc m d] -> Just $ defaultLayout currentPath (title m) $ do
      article_ $ do
        header_ $ do
          h1_ . toHtml $ title m
          p_ [class_ "subtitle"] . humantime $ created m
        div_ [class_ "item-content"] . toHtmlRaw . toLazyByteString $ renderHtml (RenderOptions False) d
    _ -> Nothing
