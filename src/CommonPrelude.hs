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
import Language.Haskell.TH.Syntax (Lift (..), liftData)
import Data.Time (Day)
import Prelude as X

instance Lift Day where
  lift = liftData
