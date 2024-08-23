{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module GTF.DraftServ.Github (
  DocAPI,
  DocType (..),
  GitRef (..),
  DocFile (..),
  getDoc,
) where

import CommonPrelude
import Data.Proxy (Proxy (..))
import Lucid (ToHtml (..))
import Servant.API (Capture, Get, PlainText, ToHttpApiData (..), (:>))
import Servant.Client (ClientM, client)

newtype GitRef = GitRef {unGitRef :: Text}
  deriving newtype (Show, Eq, ToHttpApiData, ToHtml)

-- TODO: generalise this across the different codebases as it is a common theme
-- throughout.
data DocType = MusingDoc | ProjectDoc
  deriving (Show, Eq)

instance ToHtml DocType where
  toHtml MusingDoc = "Musing"
  toHtml ProjectDoc = "Project"

  toHtmlRaw MusingDoc = "Musing"
  toHtmlRaw ProjectDoc = "Project"

instance ToHttpApiData DocType where
  toUrlPiece MusingDoc = "Musings"
  toUrlPiece ProjectDoc = "Projects"

newtype DocFile = DocFile {getFileName :: Text}
  deriving newtype (Show, Eq, ToHttpApiData, ToHtml)

type DocAPI =
  "gfarrell"
    :> "gtf.io"
    :> Capture "git-ref" GitRef
    :> "src"
    :> "GTF"
    :> "Pages"
    :> Capture "doc-type" DocType
    :> "content"
    :> Capture "file-name" DocFile
    :> Get '[PlainText] Text

getDoc :: GitRef -> DocType -> DocFile -> ClientM Text
getDoc = client $ Proxy @DocAPI
