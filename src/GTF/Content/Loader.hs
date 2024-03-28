module GTF.Content.Loader (loadFilesTH) where

import CommonPrelude hiding (readFile)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Char8 (readFile)
import Data.Proxy (Proxy)
import GTF.Content.Doc (ContentDoc (..), DocParseFailure, ParsedDoc (..), parseContentDoc)
import Language.Haskell.TH (Exp (ListE), Q, runIO)
import System.Directory (listDirectory)
import Language.Haskell.TH.Syntax (Lift(lift))

loadFiles ::
  forall m a.
  (MonadIO m) =>
  (ContentDoc a) =>
  Proxy a ->
  FilePath ->
  (FilePath -> Bool) ->
  ExceptT DocParseFailure m [ParsedDoc a]
loadFiles _ dir fileFilter = do
  files <- filter fileFilter <$> liftIO (listDirectory dir)
  traverse loadAndParse files
  where
    loadAndParse :: FilePath -> ExceptT DocParseFailure m (ParsedDoc a)
    loadAndParse fp = ExceptT (liftIO (readFile $ dir <> "/" <> fp) <&> parseContentDoc)

loadFilesTH :: (ContentDoc a) => Proxy a -> FilePath -> (FilePath -> Bool) -> Q Exp
loadFilesTH p dir fileFilter =
  runIO (runExceptT $ loadFiles p dir fileFilter) >>= \case
    Left err -> fail $ "Unable to load content files: " <> show err
    Right docs -> ListE <$> mapM lift docs
