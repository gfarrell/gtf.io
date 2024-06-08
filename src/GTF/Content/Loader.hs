module GTF.Content.Loader (loadFilesTH, isDjot) where

import CommonPrelude hiding (readFile)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Char8 (readFile)
import Data.Proxy (Proxy)
import GTF.Content.Doc (ContentDoc (..), DocParseFailure, ParsedDoc (..), parseContentDoc)
import Language.Haskell.TH (Exp (ListE), Q, runIO)
import Language.Haskell.TH.Syntax (Lift (lift), addDependentFile)
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import System.Posix.Files (getFileStatus, isDirectory, isRegularFile)

data FileType = Dir | File | Other deriving (Show, Eq)

isDjot :: FilePath -> Bool
isDjot = (== ".djot") . takeExtension

listWholeDirectory :: FilePath -> IO [FilePath]
listWholeDirectory root =
  listDirectory root
    >>= mapM addFileType
    >>= fmap mconcat
    . mapM recurseIfDir
 where
  qual :: FilePath -> FilePath
  qual f = root <> "/" <> f
  addFileType :: FilePath -> IO (FilePath, FileType)
  addFileType f =
    getFileStatus (qual f) <&> \case
      s
        | isDirectory s -> (qual f, Dir)
        | isRegularFile s -> (qual f, File)
        | otherwise -> (qual f, Other)
  recurseIfDir :: (FilePath, FileType) -> IO [FilePath]
  recurseIfDir (f, Dir) = listWholeDirectory f
  recurseIfDir (f, File) = pure [f]
  recurseIfDir _ = pure []

loadFiles ::
  forall m a.
  (MonadIO m) =>
  (ContentDoc a) =>
  Proxy a ->
  FilePath ->
  (FilePath -> Bool) ->
  ExceptT DocParseFailure m [(FilePath, ParsedDoc a)]
loadFiles _ dir fileFilter = do
  files <- filter fileFilter <$> liftIO (listWholeDirectory dir)
  traverse loadAndParse files
 where
  loadAndParse :: FilePath -> ExceptT DocParseFailure m (FilePath, ParsedDoc a)
  loadAndParse fp = (fp,) <$> ExceptT (liftIO (readFile fp) <&> parseContentDoc)

loadFilesTH :: (ContentDoc a) => Proxy a -> FilePath -> (FilePath -> Bool) -> Q Exp
loadFilesTH p dir fileFilter =
  runIO (runExceptT $ loadFiles p dir fileFilter) >>= \case
    Left err -> fail $ "Unable to load content files: " <> show err
    Right docs ->
      ListE <$> mapM (\(fp, d) -> addDependentFile fp >> lift d) docs
