module GTF.Style.Lists (stylesheet) where

import Clay
import CommonPrelude ((<>))

stylesheet :: Css
stylesheet = do
  (ul <> ol) ? textAlign start

  ul # ".page-list" ? do
    listStyleType none
    padding (em 0) (em 0) (em 0) (em 0)

    li ? do
      marginBottom (em 1)

      lastChild & marginBottom none

      before & content (stringContent "🖋 ")
