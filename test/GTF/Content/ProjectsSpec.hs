{-# LANGUAGE QuasiQuotes #-}

module GTF.Content.ProjectsSpec (spec) where

import CommonPrelude hiding (unlines)
import Data.ByteString.Char8 (unlines)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import GTF.Content.Doc (DocParseFailure (..), ParsedDoc (..), parseContentDoc)
import GTF.Content.Meta (MetadataParseError (MetaParseFailure))
import GTF.Content.Projects
import GTF.Pages.Helpers (uri)
import Test.Hspec

type Result = Either DocParseFailure (ParsedDoc Project)

spec :: Spec
spec = describe "parseContentDoc" $ do
  it "correctly parses the metadata and document for a doc project"
    $ let doc =
            unlines
              [ "---"
              , "title: A Great Document"
              , "slug: great-document"
              , "category: document"
              , "date: 2024-01-01"
              , "abstract: This is just the best document"
              , "file: a-great-file.pdf"
              , "tags:"
              , "- a cool tag"
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
                == ProjectMeta
                  "A Great Document"
                  "great-document"
                  (fromOrdinalDate 2024 1)
                  (Just "This is just the best document")
                  (Just ["a cool tag"])
                  (DocProject "a-great-file.pdf")

  it "correctly parses the metadata and document for a programming project"
    $ let doc =
            unlines
              [ "---"
              , "title: A Great Project"
              , "slug: great-project"
              , "category: programming"
              , "date: 2024-01-01"
              , "abstract: This is just the best code"
              , "repo: https://github.com/gfarrell/gtf.io"
              , "language: haskell"
              , "tags:"
              , "- a cool tag"
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
                == ProjectMeta
                  "A Great Project"
                  "great-project"
                  (fromOrdinalDate 2024 1)
                  (Just "This is just the best code")
                  (Just ["a cool tag"])
                  (CodeProject [uri|https://github.com/gfarrell/gtf.io|] "haskell")

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
