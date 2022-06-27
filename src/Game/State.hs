{-# LANGUAGE TemplateHaskell #-}
module Game.State where

import           Data.Foldable                  ( Foldable(foldr') )
import           GHC.Base                       ( RealWorld )
import           Grid
import           Import
import qualified System.Random.MWC             as MWC

data EndGameState = EndWin | EndLoss | Continue
  deriving (Show, Eq)

data CellState = CellState
  { revealed      :: Bool
  , flagged       :: Bool
  , adjacentMines :: Int
  , mined         :: Bool
  , dirty         :: Bool
  }
  deriving (Show, Eq)

initialCellState :: CellState
initialCellState = CellState { revealed      = False
                             , flagged       = False
                             , adjacentMines = 0
                             , mined         = False
                             , dirty         = False
                             }

makeProjectClassy ''CellState

data GameState = GameState
  { cellStates     :: Grid CellState
  , endGame        :: EndGameState
  , moveCount      :: Int
  , flagsRemaining :: Int
  , nRevealed      :: Int
  , gridDims       :: GridPos
  , nToReveal      :: Int
  }
  deriving (Show, Eq)

makeProjectClassy ''GameState

type instance Index GameState = GridPos
type instance IxValue GameState = CellState

instance Ixed GameState where
  ix p = cellStatesL . ix p

initialGameState :: GameState
initialGameState = GameState { cellStates     = empty
                             , endGame        = Continue
                             , moveCount      = 0
                             , flagsRemaining = 0
                             , nRevealed      = 0
                             , gridDims       = (0, 0)
                             , nToReveal      = 0
                             }

count :: (Foldable t, Num b) => (a -> Bool) -> t a -> b
count f = foldr' (bool id (+ 1) . f) 0

newGameState
  :: (MonadIO m, PrimMonad m, PrimState m ~ RealWorld)
  => Float
  -> Row
  -> Col
  -> m GameState
newGameState pct r c = do
  cellStates <- createGrid pct r c
  pure initialGameState { cellStates
                        , flagsRemaining = count mined cellStates
                        , nToReveal      = count (not . mined) cellStates
                        , gridDims       = dims cellStates
                        }

createGrid
  :: (MonadIO m, PrimMonad m, PrimState m ~ RealWorld)
  => Float
  -> Row
  -> Col
  -> m (Grid CellState)
createGrid percent nRows nCols = do
  gen   <- liftIO MWC.createSystemRandom

  mines <-
    fmap indexed
    .   replicateM nRows nCols
    $   (percent >)
    <$> MWC.uniformRM (0, 100.0) gen

  pure
    . fmap
        (\((r, c), isMined) ->
          initialCellState
            &  adjacentMinesL
            .~ (length . filter snd $ adjacent mines (r, c))
            &  minedL
            .~ isMined
        )
    $ mines
