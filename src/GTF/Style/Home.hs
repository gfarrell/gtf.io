module GTF.Style.Home (stylesheet) where

import Clay
import CommonPrelude (($))
import GTF.Style.Helpers (forMobile)

stylesheet :: Css
stylesheet = do
  ".page-home" ? do
    display flex
    flexDirection column
    justifyContent center
    alignItems center
    minHeight (px 300)

    ".bio" ? do
      marginTop (em 2)
      textAlign justify
      display flex
      flexDirection column

    img ? do
      maxWidth (pct 100)
      alignSelf center

    ".health-warning" ? fontStyle italic

    ".home-page-header" ? do
      display flex
      flexDirection row
      alignItems center

      h1 # ".brand" ? marginRight (em 1)

    forMobile $ do
      nav ? alignSelf flexStart

      ".home-page-header" ? alignSelf flexStart

      h1 # ".brand" ? fontSize (em 1.2)
