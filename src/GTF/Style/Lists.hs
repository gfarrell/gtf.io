module GTF.Style.Lists (stylesheet) where

import Clay
import CommonPrelude ((<>))

stylesheet :: Css
stylesheet = do
  (ul <> ol) ? textAlign start

  ul # ".page-list" ? do
    listStyleType none

    li ? do
      marginBottom (em 1)

      lastChild & marginBottom none
