module GTF.Style.Home (stylesheet) where

import Clay

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
