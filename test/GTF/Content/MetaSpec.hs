{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module GTF.Content.MetaSpec (spec) where

import CommonPrelude hiding (unlines)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unlines)
import Data.Time (Day)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Yaml (FromJSON (..))
import GHC.Generics (Generic)
import GTF.Content.Meta (MetadataParseError (..), parseMeta)
import Test.Hspec

newtype MyTag = MyTag Text
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON)

data MyMetaSection = MyMetaSection
  { title :: Text
  , date :: Day
  , summary :: Maybe Text
  , tags :: Maybe [MyTag]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)

type Result = Either MetadataParseError (MyMetaSection, ByteString)

spec :: Spec
spec =
  describe "parseMeta" $ do
    it "fails if the meta section is missing"
      $ let doc = "Something something, this is a document"
         in (parseMeta doc :: Result) `shouldBe` Left MetaSectionMissing

    context "fails when the meta is malformed" $ do
      it "by having an invalid format"
        $ let doc =
                unlines
                  [ "---"
                  , "title = something"
                  , "date = 2024-01-01"
                  , "---"
                  , "Blah blah blah"
                  ]
           in (parseMeta doc :: Result) `shouldBe` Left MetaParseFailure

      it "by having the wrong data for certain fields"
        $ let doc =
                unlines
                  [ "---"
                  , "title: A Great Title"
                  , "date: 2574-18-09"
                  , "---"
                  , "Blah blah blah"
                  ]
           in (parseMeta doc :: Result) `shouldBe` Left MetaParseFailure

      it "by having the wrong / missing fields"
        $ let doc =
                unlines
                  [ "---"
                  , "title: A Great Title"
                  , "what: did the fox say"
                  , "---"
                  , "Blah blah blah"
                  ]
           in (parseMeta doc :: Result) `shouldBe` Left MetaParseFailure

    it "correctly parses the meta section leaving the rest of the document untouched"
      $ let doc =
              unlines
                [ "---"
                , "title: A Great Title"
                , "date: 1990-01-11"
                , "summary: this is an amazing article"
                , "tags:"
                , "- cool stuff"
                , "---"
                , "What a great document"
                , "Blah Blah Blah!"
                ]
            result = parseMeta doc :: Result
         in result
              `shouldBe` Right
                ( MyMetaSection
                    "A Great Title"
                    (fromOrdinalDate 1990 11)
                    (Just "this is an amazing article")
                    (Just [MyTag "cool stuff"])
                , "\nWhat a great document\nBlah Blah Blah!\n"
                )
