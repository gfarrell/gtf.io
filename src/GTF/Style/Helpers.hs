module GTF.Style.Helpers (
  module Media,
  forMobile,
) where

import Clay
import Clay.Media as Media

forMobile :: Css -> Css
forMobile = query Media.screen [Media.maxWidth (px 400)]
