module GTF.Style.Main (stylesheet) where

import Clay

stylesheet :: Css
stylesheet = do
  main_ ? do
    textAlign justify

  nav ? do
    display flex
    flexDirection row
  
    ".nav-item" ? do
      padding (em 0) (em 1) (em 0) (em 1)
      borderRight (px 1) solid (rgb 204 204 204)

      ".active" & do
        fontWeight bold

        a ? do
          color black
          textDecoration none

      lastChild & borderRight none none none
