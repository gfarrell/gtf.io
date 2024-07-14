module GTF.LivePreview.LiveReload (runLivereloadServer, watchFiles) where

import CommonPrelude
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (TMVar, atomically, newTMVarIO, putTMVar, readTMVar, takeTMVar)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Monad (MonadPlus (mzero), forever, when)
import Data.Function (on)
import Data.Set qualified as Set
import Data.Text (pack)
import GTF.Content.Loader (isDjot)
import Network.WebSockets qualified as WS
import System.FSNotify (Event (..), EventIsDirectory (IsFile), watchTree, withManager)

data LRMessage = LRHello Int | LRUpdate FilePath
  deriving (Show, Eq, Ord)

data LRClientState
  = LRConnected
  | LRClosed
  deriving (Show, Eq)

data LRClient = LRClient {clientId :: Int, clientConn :: WS.Connection, clientState :: LRClientState}

-- We only want to trigger on write events to djot files
eventFilter :: Event -> Bool
eventFilter event = isWrite event && eventIsDirectory event == IsFile && isDjot (eventPath event)
 where
  isWrite :: Event -> Bool
  isWrite (Added{}) = True
  isWrite (Modified{}) = True
  isWrite (Removed{}) = True
  isWrite _ = False

{- | Watch all source files in our content directories and apply some action
on the path which changes
-}
watchFiles :: FilePath -> TChan LRMessage -> IO ()
watchFiles dir chan = withManager $ \mgr -> do
  putStrLn $ "Watching " <> dir
  _ <- watchTree mgr dir eventFilter (atomically . writeTChan chan . LRUpdate . eventPath)
  forever $ threadDelay 1_000_000

instance Eq LRClient where
  (==) = (==) `on` clientId

instance Ord LRClient where
  compare = compare `on` clientId

type LRState = Set.Set LRClient

runLivereloadServer :: TChan LRMessage -> IO ()
runLivereloadServer updateChan = do
  state <- newTMVarIO (mempty :: LRState)
  _ <-
    forkIO
      $ forever
      $ atomically (readTChan updateChan)
      >>= \case
        msg@(LRUpdate fp) -> do
          putStrLn $ "File updated: " <> fp
          clients <- atomically $ readTMVar state
          flip mapConcurrently_ clients $ \client ->
            when (clientState client == LRConnected)
              $ WS.sendTextData (clientConn client) (pack . show $ msg)
              >> putStrLn ("Sending update to client#" <> show (clientId client))
        msg -> putStrLn $ "Channel update: " <> show msg
  WS.runServer "127.0.0.1" 8081 $ liveReloadApp state

liveReloadApp :: TMVar LRState -> WS.ServerApp
liveReloadApp state pending = do
  conn <- WS.acceptRequest pending
  newClient <- atomically $ do
    clients <- takeTMVar state
    let maxId = Set.foldr (max . clientId) 0 clients
        newClient = LRClient (maxId + 1) conn LRConnected
    putTMVar state $ Set.insert newClient clients
    pure newClient
  WS.withPingThread conn 30 (return ()) $ do
    WS.sendTextData conn (pack . show $ LRHello $ clientId newClient)
    forever $ waitForClose newClient >> threadDelay 100
 where
  waitForClose :: LRClient -> IO ()
  waitForClose client = do
    WS.receive (clientConn client) >>= \case
      WS.ControlMessage (WS.Close{}) -> do
        putStrLn $ "Received close message for client#" <> show (clientId client)
        atomically
          $ takeTMVar state
          >>= putTMVar state
          . Set.insert (client{clientState = LRClosed})
          . Set.delete client
        mzero
      _ -> pure ()
