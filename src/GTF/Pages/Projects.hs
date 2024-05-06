{-# LANGUAGE TemplateHaskell #-}

module GTF.Pages.Projects (
  indexPage,
  itemPage,
)
where

import CommonPrelude
import Data.ByteString.Builder (toLazyByteString)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Proxy (Proxy (..))
import Data.Text (pack)
import Djot (RenderOptions (..), renderHtml)
import GTF.Content.Doc (ParsedDoc (..))
import GTF.Content.Loader (isDjot, loadFilesTH)
import GTF.Content.Projects (DocMeta (..), Project, ProjectDetails (..))
import GTF.Pages.Helpers (datetime, humantime)
import GTF.Pages.Layout (PageMeta (PageMeta), defaultLayout, defaultLayoutWithMeta)
import GTF.URL (UrlPath)
import Lucid.Base (Html, ToHtml (..))
import Lucid.Html5

mwrap :: (Monoid m) => m -> m -> m -> m
mwrap o c i = o <> i <> c

parens :: Html () -> Html ()
parens = mwrap "(" ")"

projects :: [ParsedDoc Project]
projects =
  sortOn
    (Down . date . meta)
    $(loadFilesTH (Proxy @Project) "src/GTF/Pages/Projects/content" isDjot)

indexPage :: UrlPath -> Maybe (Html ())
indexPage currentPath = Just $ defaultLayout currentPath "All Projects" $ do
  header_ $ do
    h1_ $ toHtmlRaw ("project | ˈprɒdʒɛkt |" :: String)
    p_ [class_ "defn-type"] $ i_ "noun"
    p_ [class_ "defn-descr"] $ do
      "an individual or collaborative enterprise that is carefully planned to achieve a particular aim: "
      span_
        [class_ "defn-example"]
        "a research project | a project to build a new power station"
  hr_ mempty
  ul_ $ foldMap mkProjectRow projects
 where
  mkProjectRow :: ParsedDoc Project -> Html ()
  mkProjectRow (ParsedDoc m _) =
    li_
      $ a_
        [ title_ $ title m
        , href_ $ "/projects/" <> slug m
        ]
      $ toHtml (title m)
      <> " "
      <> parens (datetime . date $ m)

itemPage :: Text -> UrlPath -> Maybe (Html ())
itemPage name currentPath =
  case filter ((== name) . slug . meta) projects of
    [ParsedDoc m d] -> pure
      $ defaultLayoutWithMeta currentPath (PageMeta (title m) (abstract m) (tags m))
      $ do
        article_ $ do
          header_ $ do
            h1_ . toHtml $ title m
            p_ [class_ "projects__info"]
              $ span_ [class_ "projects__info-item"] (humantime . date $ m)
              <> mkInfoSection m
          div_ [class_ "item-content"]
            . toHtmlRaw
            . toLazyByteString
            $ renderHtml (RenderOptions False) d
    _ -> Nothing
 where
  mkInfoSection :: DocMeta Project -> Html ()
  mkInfoSection m = case details m of
    DocProject filename ->
      a_
        [ class_ "projects__info-item"
        , href_ $ "/projects/" <> slug m <> "/assets/" <> pack filename
        , title_ "download"
        ]
        "download"
    CodeProject repo lang -> do
      a_
        [class_ "projects__info-item", href_ $ pack . show $ repo, title_ "Project repository"]
        "go to repo"
      span_ [class_ "projects__info-item", role_ "project-language"] $ toHtml lang
