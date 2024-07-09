module GTF.Pages.Partials.TableOfContents (renderToc) where

import CommonPrelude
import GTF.Content.Doc (SectionLink (..), TableOfContents (..))
import Lucid (Html, ToHtml (..))
import Lucid.Html5

renderToc :: TableOfContents -> Html ()
renderToc (TOC sections) = ol_ $ foldMap renderToc sections
renderToc (TOCSection name (SectionLink link) subsections) =
  li_ $ do
    a_ [href_ $ "#" <> link] (toHtml name)
    ol_ $ foldMap renderToc subsections
