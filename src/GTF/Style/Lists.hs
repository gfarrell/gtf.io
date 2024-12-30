module GTF.Style.Lists (stylesheet) where

import Clay
import Clay.Flexbox qualified as FB
import CommonPrelude ((<>))

stylesheet :: Css
stylesheet = do
  (ul <> ol) ? textAlign start

  ul # ".page-list" ? do
    listStyleType none
    padding (em 0) (em 0) (em 0) (em 0)

    li ? do
      marginBottom (em 1)

      lastChild & marginBottom none

      before & content (stringContent "🖋 ")

  ".table-of-contents" ** ol ? paddingLeft (em 1.2)

  ".table-of-contents" |> ol ? do
    listStyleType decimal

    ol ? do
      listStyleType lowerAlpha

      ol ? do
        listStyleType lowerRoman

  dl ? do
    dt ? do
      fontWeight bold

  dl # ".dl-single-line" ? do
    display flex
    flexWrap FB.wrap

    dt ? do
      flexBasis (pct 20)
      paddingRight (em 1.6)

    dd ? do
      flexBasis (pct 75)
      marginLeft (em 0)

      p ? margin (em 0) (em 0) (em 0) (em 0)

    dd <> dt ? marginBottom (em 1)
