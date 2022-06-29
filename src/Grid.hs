module Grid where

import qualified Data.Vector.Fusion.Bundle     as VB
import           Import
import qualified RIO.Vector                    as V
import qualified RIO.Vector.Partial            as VP


type Row = Int
type Col = Int
type GridPos = (Row, Col)

newtype Grid a = Grid
  { unGrid :: Vector (Vector a) }
  deriving
    ( Show
    , Eq
    , Functor
    , Foldable
    , Traversable
    , Generic
    )

instance Applicative Grid where
  pure = Grid . V.singleton . V.singleton
  Grid a <*> Grid b = Grid $ V.zipWith (V.zipWith ($)) a b

instance Wrapped (Grid a)

type instance Index (Grid a) = GridPos
type instance IxValue (Grid a) = a

instance Ixed (Grid a) where
  ix (r, c) = _Wrapped' . ix r . ix c


--------------------------------------------------------------------------------
-- Grid utilities
--------------------------------------------------------------------------------
empty :: Grid a
empty = Grid V.empty

indexed :: Grid a -> Grid (GridPos, a)
indexed =
  Grid
    . fmap (uncurry ((. V.indexed) . fmap . first . (,)))
    . V.indexed
    . unGrid

-- | Actual filtering could produce invalid grids
flatFilter :: (a -> Bool) -> Grid a -> Vector a
flatFilter f = V.unstream . VB.filter f . VB.concatVectors . V.stream . unGrid

(!) :: Grid a -> GridPos -> a
(!) (Grid g) (r, c) = (g VP.! r) VP.! c

(!?) :: Grid a -> GridPos -> Maybe a
(!?) (Grid g) (r, c) = g V.!? r >>= (V.!? c)

adjacent :: Grid a -> GridPos -> [a]
adjacent g = mapMaybe (g !?) . iAdjacent Nothing

iAdjacent :: Maybe (Row, Col) -> GridPos -> [GridPos]
iAdjacent mMax (r, c) = filter
  (maybe
    (const True)
    (\(maxR, maxC) (r', c') -> r' >= 0 && c' >= 0 && r' < maxR && c' < maxC)
    mMax
  )
  [ (r - 1, c - 1)
  , (r - 1, c)
  , (r - 1, c + 1)
  , (r    , c - 1)
  , (r    , c + 1)
  , (r + 1, c - 1)
  , (r + 1, c)
  , (r + 1, c + 1)
  ]
{-# INLINE iAdjacent #-}

iAdjacentSq :: (Row, Col) -> GridPos -> [GridPos]
iAdjacentSq (maxR, maxC) (r, c) = filter
  (\(r', c') -> r' >= 0 && c' >= 0 && r' < maxR && c' < maxC)
  [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]
{-# INLINE iAdjacentSq #-}

dims :: Grid a -> (Row, Col)
dims (Grid g) = (V.length g, V.length (VP.head g))

size :: Grid a -> Int
size = uncurry (*) . dims

replicateM :: (Monad m) => Row -> Col -> m a -> m (Grid a)
replicateM r c = fmap Grid . V.replicateM r . V.replicateM c
