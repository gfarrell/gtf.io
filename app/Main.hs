module Main where

import CommonPrelude
import Network.Wai.Handler.Warp (run)
import GTF.Router (routes)

main :: IO ()
main = do
  putStrLn "Running on 2712"
  run 2712 routes
