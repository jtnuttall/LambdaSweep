{-# LANGUAGE OverloadedLabels #-}
module UI.Helper where

import           Data.GI.Base.Attributes
import qualified Data.Text.Read                as T
import           GHC.OverloadedLabels
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           Game.State
import           Grid
import           Import
import           Paths_LambdaSweep              ( getDataFileName )
import           RIO.Char                       ( isDigit )
import           RIO.FilePath                   ( (</>) )
import qualified RIO.HashSet                   as HS
import qualified RIO.Text                      as T

-- | Should live in a conf file eventually
defaultDims :: (Row, Col)
defaultDims = (25, 25)


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

clamp :: (Ord a) => a -> a -> a -> a
clamp min max val | val < min = min
                  | val > max = max
                  | otherwise = val

maskClamp :: (Show a, Integral a) => a -> a -> Text -> Text
maskClamp min max =
  let clamp' = clamp min max
  in  either (const "") (tshow . clamp' . fst) . T.decimal

newCell :: (MonadIO m) => m Gtk.Button
newCell = do
  btn <- Gtk.new Gtk.Button [#label := " ", #expand := False]
  addClasses btn ["cell", "hidden"]
  pure btn

--------------------------------------------------------------------------------
-- GTK StyleContext helpers
--------------------------------------------------------------------------------
loadStyles :: (MonadIO m) => m ()
loadStyles = do
  cssProvider <- Gtk.new Gtk.CssProvider []
  screen <- fromMaybe (error "fail: no screen") <$> Gdk.screenGetDefault

  path <- liftIO $ getDataFileName ("assets" </> "theme" </> "default.css")

  #loadFromPath cssProvider (T.pack path)

  Gtk.styleContextAddProviderForScreen
    screen
    cssProvider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

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

--------------------------------------------------------------------------------
-- GTK Entry helpers
--------------------------------------------------------------------------------
getIntegralInput
  :: (MonadIO m, Gtk.IsEntry o, Integral a, IsLabel "getText" (o -> m Text))
  => o
  -> a
  -> m a
getIntegralInput o defaultValue =
  either (const defaultValue) fst . T.decimal <$> #getText o

    -- either (const (fst defaultDims)) fst . T.decimal <$> #getText heightEntry

-- Should be expanded to permit masks that are more aware of their input.
type Mask = Text -> Text

maskDigits :: Mask
maskDigits = T.filter isDigit

newMaskedEntry
  :: MonadIO m => Mask -> [AttrOp Gtk.Entry 'AttrConstruct] -> m Gtk.Entry
newMaskedEntry mask attrs = do
  entry <- Gtk.new Gtk.Entry attrs

  Gtk.on entry #changed $ do
    newValue <- mask <$> #getText entry
    Gtk.set entry [#text := newValue]

  pure entry
