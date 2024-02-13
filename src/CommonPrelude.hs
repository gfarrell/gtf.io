module CommonPrelude
  ( module X,

    -- * Text Utilities
    Text,

    -- * Useful operators for monads and functors
    ($>),
    (<&>),
    (>=>),
    (<|>),
  )
where

import Control.Monad ((>=>))
import Control.Applicative ((<|>))
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import Prelude as X

