module GTF.LivePreview.Options (
  Options (..),
  DocType (..),
  previewProgrammeDescr,
)
where

import CommonPrelude
import Options.Applicative qualified as Opt

data DocType = MusingDoc | ProjectDoc
  deriving (Show, Eq)

data Options = Options
  { filePath :: FilePath
  , docType :: DocType
  }
  deriving (Show, Eq)

parseOptions :: Opt.Parser Options
parseOptions =
  Options
    <$> Opt.option Opt.str (Opt.long "file" <> Opt.help "the file to preview")
    <*> Opt.option
      parseDocType
      (Opt.short 't' <> Opt.help "the document type (how we render it): one of [musing, project]")
    Opt.<**> Opt.helper
 where
  parseDocType :: Opt.ReadM DocType
  parseDocType = Opt.maybeReader
    $ \case
      "musing" -> Just MusingDoc
      "project" -> Just ProjectDoc
      _ -> Nothing

previewProgrammeDescr :: Opt.ParserInfo Options
previewProgrammeDescr =
  Opt.info parseOptions
    $ Opt.briefDesc
    <> Opt.progDesc
      "Run a preview server for djot files to avoid having to rebuild the website all the time"
