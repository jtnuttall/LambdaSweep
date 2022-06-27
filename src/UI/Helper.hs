{-# LANGUAGE OverloadedLabels #-}
module UI.Helper where

import           Data.GI.Base.Attributes
import qualified Data.Text.Read                as T
import qualified GI.Gtk                        as Gtk
import           Game.State
import           Import
import           RIO.Char                       ( isDigit )
import qualified RIO.HashSet                   as HS
import qualified RIO.Text                      as T

data CellRendering
  = Revealed (Maybe Int)
  | Flagged
  | Hidden
  deriving (Show, Eq)

cellRendering :: CellState -> CellRendering
cellRendering CellState {..} | mined && revealed = Revealed Nothing
                             | revealed          = Revealed (Just adjacentMines)
                             | flagged           = Flagged
                             | otherwise         = Hidden


data ButtonProps = ButtonProps
  { attrs   :: [AttrOp Gtk.Button 'AttrSet]
  , classes :: [Text]
  }

maskDigits :: Text -> Text
maskDigits = T.filter isDigit

clamp :: (Ord a) => a -> a -> a -> a
clamp min max val | val < min = min
                  | val > max = max
                  | otherwise = val

maskClamp :: (Show a, Integral a) => a -> a -> Text -> Text
maskClamp min max =
  let clamp' = clamp min max
  in  either (const "") (tshow . clamp' . fst) . T.decimal

newMaskedEntry
  :: MonadIO m
  => (Text -> Text)
  -> [AttrOp Gtk.Entry 'AttrConstruct]
  -> m Gtk.Entry
newMaskedEntry mask attrs = do
  entry <- Gtk.new Gtk.Entry attrs

  Gtk.on entry #changed $ do
    newValue <- mask <$> #getText entry
    Gtk.set entry [#text := newValue]

  pure entry

newCell :: (MonadIO m) => m Gtk.Button
newCell = do
  btn <- Gtk.new Gtk.Button [#label := " ", #expand := False]
  addClasses btn ["cell", "hidden"]
  pure btn

buttonProps :: CellRendering -> ButtonProps
buttonProps = \case
  Revealed (Just n) -> ButtonProps
    { attrs   = [#label := bool (tshow n) " " (n == 0)]
    , classes = ["revealed", "revealed-" <> tshow n]
    }
  Revealed _ ->
    ButtonProps { attrs = [#label := " "], classes = ["revealed", "mined"] }
  Flagged -> ButtonProps { attrs = [], classes = ["flagged"] }
  Hidden  -> ButtonProps { attrs = [#label := " "], classes = ["hidden"] }

dropClasses
  :: (HasCallStack, Gtk.IsWidget o, MonadIO m) => o -> HashSet Text -> m ()
dropClasses o excludedClasses = do
  sc      <- Gtk.widgetGetStyleContext o
  classes <- #listClasses sc
  traverse_ (#removeClass sc)
            (filter (not . flip HS.member excludedClasses) classes)

addClasses :: (HasCallStack, Gtk.IsWidget o, MonadIO m) => o -> [Text] -> m ()
addClasses o classes = do
  sc <- Gtk.widgetGetStyleContext o
  traverse_ (#addClass sc) classes
