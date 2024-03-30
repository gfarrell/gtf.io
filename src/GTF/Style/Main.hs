module GTF.Style.Main (stylesheet) where

import Clay
import Clay.Media qualified as Media
import CommonPrelude

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

      lastChild
        -- this should be "border-right: none" but see https://github.com/sebastiaanvisser/clay/issues/263
        & borderRight 0 none (rgb 204 204 204)

  header # ".site-header" ? do
    display flex
    flexDirection row
    marginBottom $ em 2.5

    query Media.print [] $ display none

  footer # ".site-footer" ? do
    marginTop $ em 3
    fontSize $ em 0.8
    display flex
    flexDirection row
    justifyContent center

    ".footer-item" ? do
      padding (em 0) (em 1) (em 0) (em 1)
      borderRight (px 1) solid (rgb 204 204 204)

      lastChild
        -- this should be "border-right: none" but see https://github.com/sebastiaanvisser/clay/issues/263
        & borderRight 0 none (rgb 204 204 204)

    query Media.print [] $ display none
