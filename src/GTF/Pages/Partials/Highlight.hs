module GTF.Pages.Partials.Highlight (highlight) where

import CommonPrelude
import Lucid (Html)
import Lucid.Html5

highlight :: Html ()
highlight = do
  script_
    [src_ "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js"]
    (mempty :: Html ())
  script_ [] ("hljs.highlightAll();" :: Html ())
  foldMap
    ( \l ->
        script_
          [ src_
              $ "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/languages/"
              <> l
              <> ".min.js"
          ]
          (mempty :: Html ())
    )
    ["haskell", "bash", "python", "typescript"]
