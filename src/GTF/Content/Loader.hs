module GTF.Content.Loader (loadFilesTH) where

import CommonPrelude hiding (readFile)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Char8 (readFile)
import Data.Proxy (Proxy)
import GTF.Content.Doc (ContentDoc (..), DocParseFailure, ParsedDoc (..), parseContentDoc)
import Language.Haskell.TH (Exp (ListE), Q, runIO)
import Language.Haskell.TH.Syntax (Lift (lift), addDependentFile)
import System.Directory (listDirectory)

loadFiles ::
  forall m a.
  (MonadIO m) =>
  (ContentDoc a) =>
  Proxy a ->
  FilePath ->
  (FilePath -> Bool) ->
  ExceptT DocParseFailure m [(FilePath, ParsedDoc a)]
loadFiles _ dir fileFilter = do
  files <- filter fileFilter <$> liftIO (listDirectory dir)
  traverse loadAndParse files
  where
    loadAndParse :: FilePath -> ExceptT DocParseFailure m (FilePath, ParsedDoc a)
    loadAndParse fp =
      let fqPath = dir <> "/" <> fp
       in (fqPath,)
        <$> ExceptT (liftIO (readFile fqPath) <&> parseContentDoc)

loadFilesTH :: (ContentDoc a) => Proxy a -> FilePath -> (FilePath -> Bool) -> Q Exp
loadFilesTH p dir fileFilter =
  runIO (runExceptT $ loadFiles p dir fileFilter) >>= \case
    Left err -> fail $ "Unable to load content files: " <> show err
    Right docs ->
      ListE <$> mapM (\(fp, d) -> addDependentFile fp >> lift d) docs
