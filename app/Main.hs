module Main where

import CommonPrelude
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor (first))
import GTF.Router (LogLevel (..), app)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Text.Read (readEither)

newtype Args = Args {port :: Int}
  deriving (Show, Eq)

data ProgrammeError = InvalidPort | WrongNumberOfArgs
  deriving (Show, Eq)

getAppArgs :: ExceptT ProgrammeError IO Args
getAppArgs =
  ExceptT $ getArgs <&> \case
    [portString] -> Args <$> (first (const InvalidPort) . readEither $ portString)
    _ -> Left WrongNumberOfArgs

main :: IO ()
main = do
  res <-
    runExceptT $ getAppArgs >>= \(Args port) -> do
      liftIO . putStrLn $ "Running on " <> show port
      liftIO $ run port $ app Verbose
  case res of
    Left err -> putStrLn $ "An error occured: " <> show err
    Right _ -> putStrLn "Exited"
