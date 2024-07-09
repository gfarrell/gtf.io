module GTF.Content.MusingsSpec (spec) where

import CommonPrelude hiding (unlines)
import Data.ByteString.Char8 (unlines)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import GTF.Content.Doc (DocParseFailure (..), ParsedDoc (..), parseContentDoc)
import GTF.Content.Meta (MetadataParseError (MetaParseFailure))
import GTF.Content.Musings
import Test.Hspec

type Result = Either DocParseFailure (ParsedDoc Musing)

spec :: Spec
spec = describe "parseContentDoc" $ do
  it "correctly parses the metadata and document"
    $ let doc =
            unlines
              [ "---"
              , "title: A Great Document"
              , "slug: great-document"
              , "category: reflection"
              , "created: 2024-01-01"
              , "updated: 2024-01-06"
              , "abstract: This is just the best document"
              , "tags:"
              , "- something something"
              , "toc: true"
              , ""
              , "---"
              , "# Ok, a heading"
              , "Good start, better ending"
              ]
       in (parseContentDoc doc :: Result) `shouldSatisfy` \case
            Left _ -> False
            Right (ParsedDoc m d) ->
              d
                /= mempty
                && m
                == MusingMeta
                  "A Great Document"
                  "great-document"
                  Reflection
                  (fromOrdinalDate 2024 1)
                  (Just (fromOrdinalDate 2024 6))
                  (Just "This is just the best document")
                  (Just ["something something"])
                  True

  it "fails if the metadata section fails to parse"
    $ let doc =
            unlines
              [ "---"
              , "title = A Great Document"
              , "---"
              , "# Ok, a heading"
              , "Good start, better ending"
              ]
       in (parseContentDoc doc :: Result) `shouldBe` Left (MetadataParseFailure MetaParseFailure)
