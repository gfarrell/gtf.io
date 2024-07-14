module GTF.Style.DraftTools (stylesheet) where

import Clay
import CommonPrelude (($))

stylesheet :: Css
stylesheet = do
  ".draft-note" ? do
    background $ rgba 122 180 122 0.25
    padding (em 1) (em 1) (em 1) (em 1)
    border (px 1) solid (rgb 122 180 122)
    fontStyle italic

  "#lr-indicator" ? do
    textAlign end
    flexGrow 2
    flexShrink 1
    flexBasis (pct 0)
