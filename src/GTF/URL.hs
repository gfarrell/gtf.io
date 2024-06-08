{-# LANGUAGE DerivingStrategies #-}

module GTF.URL (
  UrlPath (..),
) where

import CommonPrelude
import Data.ByteString (ByteString)

newtype UrlPath = UrlPath {unUrlPath :: ByteString}
  deriving newtype (Show, Eq)
