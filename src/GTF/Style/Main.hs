module GTF.Style.Main (stylesheet) where

import Clay
import Clay.Media qualified as Media
import CommonPrelude (($))

stylesheet :: Css
stylesheet = do
  div # ".top-container" ? do
    display flex
    flexDirection row
    justifyContent center

  div # ".content-container" ? do
    padding 0 (em 1) 0 (em 1)
    maxWidth $ px 600

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
        & borderRight 0 none white

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
        & borderRight 0 none white

    query Media.print [] $ display none

  hr ? do
    margin (em 2) auto (em 2) auto
    width $ pct 80
    border 0 none white
    height $ px 0
    borderTop (px 1) solid (rgba 0 0 0 0.1)
    borderBottom (px 1) solid (rgba 255 255 255 0.3)
    textAlign center

    after & do
      content $ stringContent "\\1F74E"
      position relative
      top $ em $ -0.6
      padding 0 (em 0.5) 0 (em 0.5)
      background white

  div # ".item-content" ? do
    marginTop $ em 2
    textAlign justify

  header |> ".subtitle" ? do
    fontSize $ em 0.9
    margin (em 0.1) 0 (em 0.1) 0
