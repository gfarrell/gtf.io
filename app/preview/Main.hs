module Main where

import CommonPrelude
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTChan)
import GTF.LivePreview.LiveReload (runLivereloadServer, watchFiles)
import GTF.LivePreview.Options (Options (..), previewProgrammeDescr)
import GTF.LivePreview.PreviewServer (runPreviewServer)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath (takeDirectory)

main :: IO ()
main =
  atomically newTChan >>= \chan ->
    execParser previewProgrammeDescr >>= \(Options fpath dtype) ->
      doesFileExist fpath >>= \case
        True -> do
          _ <- forkIO $ watchFiles (takeDirectory fpath) chan
          _ <- forkIO $ runLivereloadServer chan
          run 8080 $ runPreviewServer dtype fpath
        False -> die $ "File not found: " <> fpath
