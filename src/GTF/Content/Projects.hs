{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module GTF.Content.Projects (Project, ProjectDetails (..), ProjectLink (..), DocMeta (..)) where

import CommonPrelude
import Data.Time.Calendar (Day)
import Data.Yaml (FromJSON (parseJSON), withObject, (.:))
import GHC.Generics (Generic)
import GTF.Content.Doc (ContentDoc (..))
import Language.Haskell.TH.Syntax (Lift)
import Network.URI (URI)

data Project

data ProjectLink
  = Repository URI
  | File FilePath
  deriving (Show, Eq, Lift)

data ProjectDetails
  = DocProject
      { file :: FilePath
      }
  | CodeProject
      { repoOrFile :: ProjectLink,
        language :: Text
      }
  | OtherProject
  deriving (Show, Eq, Generic, Lift)

instance ContentDoc Project where
  data DocMeta Project = ProjectMeta
    { title :: Text,
      slug :: Text,
      date :: Day,
      abstract :: Maybe Text,
      tags :: Maybe [Text],
      details :: ProjectDetails
    }
    deriving (Show, Eq, Generic, Lift)

instance FromJSON (DocMeta Project) where
  parseJSON = withObject "Project DocMeta" $ \o ->
    ProjectMeta
      <$> o
      .: "title"
      <*> o
      .: "slug"
      <*> (o .: "date" >>= parseJSON)
      <*> o
      .: "abstract"
      <*> o
      .: "tags"
      <*> ( o .: "category" >>= \case
              "programming" ->
                CodeProject
                  <$> ((Repository <$> o .: "repo") <|> (File <$> o .: "file"))
                  <*> (o .: "language")
              "document" -> DocProject <$> o .: "file"
              "other" -> pure OtherProject
              c -> fail $ "unknown project category " <> c
          )
