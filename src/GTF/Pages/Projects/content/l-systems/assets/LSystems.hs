{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

{- cabal:
build-depends: base, mtl, svg-builder, text, optparse-generic
-}

import Control.Monad.State (State, gets, modify, runState)
import Data.List.NonEmpty (NonEmpty ((:|)), singleton, (<|))
import Data.List.NonEmpty qualified as NEL
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup (Semigroup (sconcat))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Graphics.Svg qualified as SVG
import Graphics.Svg.Attributes ((<<-))
import Options.Generic (ParseField, ParseFields, ParseRecord, getRecord)
import System.Environment (getArgs)

size :: Float
size = 1600

baseLength :: Float
baseLength = 10

baseWidth :: Float
baseWidth = 5

showT :: (Show a) => a -> Text
showT = pack . show

-- | A context-free L-System consists of an alphabet of symbols, described by
-- the data family 'Alphabet a', an 'axiom' which describes the initial state of
-- the system, and a production rule 'produce', which takes a single symbol and produces
-- at least one other symbol.
class CFL (a :: k) where
  data Alphabet a
  axiom :: Proxy a -> Alphabet a
  produce :: Alphabet a -> NonEmpty (Alphabet a)

-- | Steps forward one move in a system by running the production rule on each
-- element
step :: (CFL a) => NonEmpty (Alphabet a) -> NonEmpty (Alphabet a)
step (a :| rest) = sconcat $ produce a :| map produce rest

data LSystem = BinaryTree | Koch
  deriving (Show, Eq, Read, Generic)
  deriving anyclass (ParseRecord, ParseField, ParseFields)

data CLI = CLI
  { systemName :: LSystem,
    outputFolder :: FilePath,
    maxIterations :: Int
  }
  deriving (Generic)
  deriving anyclass (ParseRecord)

main :: IO ()
main = do
  options <- getRecord "L-System renderer"
  case systemName options of
    BinaryTree -> runWorld options (Proxy @BinaryTree)
    Koch -> runWorld options (Proxy @Koch)
  where
    runWorld ::
      forall a.
      ( (CFL a),
        (Turtle (Alphabet a))
      ) =>
      CLI ->
      Proxy a ->
      IO ()
    runWorld options _ =
      let start = RenderState (singleton $ TurtleState {position = (size / 2, 0), angle = 0})
       in mapM_ (uncurry $ writeImage options)
            . take (maxIterations options)
            . zip [1 ..]
            . map (mkImage . sconcat . fst . flip runState start . traverse render)
            . iterate step
            . singleton
            $ axiom (Proxy @a)
    writeImage :: CLI -> Int -> SVG.Element -> IO ()
    writeImage options index image =
      writeFile (outputFolder options <> "/" <> show index <> ".svg") (show image)

-- --------------------------------------------------------------------------------- --
-- GRAPHICS (SVG)
-- ==============

type WorldM a = State RenderState a

mkImage :: SVG.Element -> SVG.Element
mkImage contents =
  SVG.doctype
    <> SVG.with
      ( SVG.svg11_ $
          SVG.rect_
            [ SVG.X_ <<- "0",
              SVG.Y_ <<- "0",
              SVG.Width_ <<- showT size,
              SVG.Height_ <<- showT size,
              SVG.Fill_ <<- "white"
            ]
            <> contents
      )
      [ SVG.Version_ <<- "1.1",
        SVG.Width_ <<- showT size,
        SVG.Height_ <<- showT size
      ]

data TurtleState = TurtleState
  { position :: (Float, Float),
    angle :: Float
  }

newtype RenderState = RenderState (NonEmpty TurtleState)

-- | Modifies the topmost 'TurtleState'
turtleMod :: (TurtleState -> TurtleState) -> WorldM ()
turtleMod f = modify $ \(RenderState (t :| rest)) -> RenderState (f t :| rest)

-- | Views the topmost 'TurtleState' in the stack
viewTurtle :: WorldM TurtleState
viewTurtle = gets $ \(RenderState (t :| _)) -> t

-- | Takes off the topmost 'TurtleState', but if there is only one, it leaves it
-- in place as we can't have an empty stack of states
popTurtle :: WorldM ()
popTurtle = modify $ \(RenderState (t :| rest)) -> case NEL.nonEmpty rest of
  Just states -> RenderState states
  Nothing -> RenderState (t :| rest)

-- | Puts a 'TurtleState' on top of the stack
pushTurtle :: TurtleState -> WorldM ()
pushTurtle t = modify $ \(RenderState states) -> RenderState $ t <| states

class Turtle a where
  render :: a -> WorldM SVG.Element

vecMove :: (Float, Float) -> Float -> Float -> (Float, Float)
vecMove (x1, y1) distance theta = (x1 + distance * sin theta, y1 + distance * cos theta)

-- --------------------------------------------------------------------------------- --
-- Defined L-Systems
-- =================

instance CFL 'BinaryTree where
  data Alphabet 'BinaryTree = Zero | One | LBrace | RBrace

  axiom _ = Zero

  produce One = One :| [One]
  produce Zero = One <| LBrace <| Zero <| RBrace :| [Zero]
  produce c = singleton c

instance Show (Alphabet 'BinaryTree) where
  show Zero = "0"
  show One = "1"
  show LBrace = "["
  show RBrace = "]"

mkLeaf :: (Float, Float) -> SVG.Element
mkLeaf origin =
  SVG.circle_
    [ SVG.Cx_ <<- showT (fst origin),
      SVG.Cy_ <<- showT (snd origin),
      SVG.R_ <<- showT (baseLength / 2),
      SVG.Fill_ <<- "green"
    ]

instance Turtle (Alphabet 'BinaryTree) where
  render Zero = do
    TurtleState origin theta <- viewTurtle
    let next = vecMove origin baseLength theta
    turtleMod (const $ TurtleState next theta)
    pure $
      mkLeaf next
        <> SVG.line_
          [ SVG.X1_ <<- showT (fst origin),
            SVG.Y1_ <<- showT (snd origin),
            SVG.X2_ <<- showT (fst next),
            SVG.Y2_ <<- showT (snd next),
            SVG.Stroke_ <<- "black",
            SVG.Stroke_width_ <<- showT baseWidth
          ]
  render One = do
    TurtleState origin theta <- viewTurtle
    let next = vecMove origin baseLength theta
    turtleMod (const $ TurtleState next theta)
    pure $
      SVG.line_
        [ SVG.X1_ <<- showT (fst origin),
          SVG.Y1_ <<- showT (snd origin),
          SVG.X2_ <<- showT (fst next),
          SVG.Y2_ <<- showT (snd next),
          SVG.Stroke_ <<- "black",
          SVG.Stroke_width_ <<- showT baseWidth
        ]
  render LBrace = do
    viewTurtle >>= pushTurtle -- copy the turtle
    turtleMod $ \turtle -> turtle {angle = angle turtle - (pi / 4)}
    pure mempty
  render RBrace = do
    popTurtle -- go back to the previous state
    turtleMod $ \turtle -> turtle {angle = angle turtle + (pi / 4)}
    pure mempty

instance CFL 'Koch where
  data Alphabet 'Koch = F | Plus | Minus

  axiom _ = F

  produce F = F <| Plus <| F <| Minus <| F <| Minus <| F <| Plus :| [F]
  produce Plus = singleton Plus
  produce Minus = singleton Minus

instance Turtle (Alphabet 'Koch) where
  render F = do
    TurtleState origin theta <- viewTurtle
    let next = vecMove origin (baseLength * 2) theta
    turtleMod (const $ TurtleState next theta)
    pure $
      SVG.line_
        [ SVG.X1_ <<- showT (fst origin),
          SVG.Y1_ <<- showT (snd origin),
          SVG.X2_ <<- showT (fst next),
          SVG.Y2_ <<- showT (snd next),
          SVG.Stroke_ <<- "blue",
          SVG.Stroke_width_ <<- showT baseWidth
        ]
  render Plus = do
    turtleMod $ \turtle -> turtle {angle = angle turtle + (pi / 2)}
    pure mempty
  render Minus = do
    turtleMod $ \turtle -> turtle {angle = angle turtle - (pi / 2)}
    pure mempty
