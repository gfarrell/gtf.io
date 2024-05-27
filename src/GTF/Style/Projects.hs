module GTF.Style.Projects (stylesheet) where

import Clay

stylesheet :: Css
stylesheet = do
  ".projects__info" ? do
    display flex
    flexDirection row

    ".projects__info-item" ? do
      padding (em 0) (em 1) (em 0) (em 1)
      borderRight (px 1) solid (rgb 204 204 204)

      firstChild & paddingLeft (em 0)
      lastChild & borderRight 0 none white
