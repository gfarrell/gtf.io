module Main where

import CommonPrelude
import GTF.Router (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn "Running on 2712"
  run 2712 app
