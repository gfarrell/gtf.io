module GTF.Style.Nav (stylesheet) where

import Clay
import CommonPrelude (($))
import GTF.Style.Helpers (forMobile, print)

stylesheet :: Css
stylesheet = do
  nav ? do
    -- We use this to control open / close behaviour
    input ? do
      position absolute
      opacity 0
      zIndex (-1)

    ".nav-label" ? display none

    ".nav-content" ? do
      display flex
      flexDirection row
      overflow hidden

    ".nav-item" ? do
      padding (em 0) (em 1) (em 0) (em 1)
      borderRight (px 1) solid (rgb 204 204 204)

      ".active" & do
        fontWeight bold

        a ? do
          color black
          textDecoration none

      lastChild
        -- this should be "border-right: none" but see
        -- https://github.com/sebastiaanvisser/clay/issues/263
        & borderRight 0 none white

    forMobile $ do
      ".nav-label" ? do
        display flex
        cursor pointer

        before & do
          content (stringContent "⯈")
          paddingRight (em 0.5)
          transition "all" (sec 0.35) ease (sec 0)

      input # checked |~ ".nav-content" ? maxHeight (vh 100)
      input
        # checked
        |~ ".nav-label"
        ? before
        & do
          transform (rotate (deg 90))
          paddingTop (em 0.5)

      ".nav-content" ? do
        flexDirection column
        maxHeight (vh 0)
        transition "max-height" (sec 0.35) ease (sec 0)

      ".nav-item" ? do
        borderRight 0 solid white

    query print [] $ display none
