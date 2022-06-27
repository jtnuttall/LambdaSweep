{-# LANGUAGE OverloadedLabels #-}
module ActionMsg where

import           Data.GI.Base
import qualified GI.Gdk                        as Gdk
import           Game.State
import           Grid                           ( GridPos )
import           Import

data ActionMsg
  = RevealCell GridPos
  | FlagCell GridPos
  deriving (Show, Eq)

type GameReactHandle = ActionMsg -> IO GameState

extractPos :: ActionMsg -> GridPos
extractPos = \case
  RevealCell p -> p
  FlagCell   p -> p

actionMsg :: GridPos -> Gdk.EventType -> Word32 -> Maybe ActionMsg
actionMsg p = \case
  Gdk.EventTypeButtonPress -> \case
    1 -> Just (RevealCell p)
    3 -> Just (FlagCell p)
    _ -> Nothing
  _ -> const Nothing

getUiAction :: (MonadIO m) => GridPos -> Gdk.EventButton -> m (Maybe ActionMsg)
getUiAction p e = actionMsg p <$> get e #type <*> get e #button
