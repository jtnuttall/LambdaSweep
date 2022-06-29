{-# LANGUAGE OverloadedLabels #-}
module UI
  ( runUi
  ) where

import           Data.GI.Base
import qualified Data.Text.Read                as T
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           Grid
import           Import
import           Paths_LambdaSweep              ( getDataFileName )
import           RIO.FilePath                   ( (</>) )
import           RIO.State                      ( MonadState(..) )
import qualified RIO.Text                      as T
import           UI.Helper
import           UI.MonadUI


runUi :: (MonadIO m) => m ()
runUi = liftIO . withGtkApp $ \application -> do
  loadStyles

  _uiForm       <- createUiForm defaultDims
  mainWindow    <- new Gtk.Window [#title := "LambdaSweep", #resizable := False]
  mainContainer <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  flip runUiT Ui { .. } $ do
    addComponents
    addFormHandlers
    #showAll =<< view mainWindowL

withGtkApp :: (Gtk.Application -> IO ()) -> IO ()
withGtkApp f = void $ do
  app <- Gtk.new Gtk.Application []
  _   <- Gtk.on app #activate (f app)
  #run app Nothing

-------------------------------------------------------------------------------
-- UI construction
-------------------------------------------------------------------------------
addComponents :: (MonadUnliftIO m, MonadFix m) => UiT m ()
addComponents = do
  ui <- ask

  #add (ui ^. formContainerL) (ui ^. statusContainerL)
  #add (ui ^. formContainerL) (ui ^. heightEntryL)
  #add (ui ^. formContainerL) (ui ^. widthEntryL)
  #add (ui ^. formContainerL) (ui ^. formSubmitL)

  #add (ui ^. statusContainerL) (ui ^. flagsRemainingL)
  #add (ui ^. statusContainerL) (ui ^. turnsL)

  #add (ui ^. mainContainerL) (ui ^. formContainerL)
  #add (ui ^. mainWindowL) (ui ^. mainContainerL)

  #addWindow (ui ^. applicationL) (ui ^. mainWindowL)

  initializeGrid defaultDims

createUiForm :: (MonadUnliftIO m, MonadFix m) => (Row, Col) -> m UiForm
createUiForm (r, c) = do
  uiGridRef     <- newSomeRef =<< createUiState (r, c)
  formContainer <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  widthEntry    <- newMaskedEntry maskDims [#text := tshow c]
  heightEntry   <- newMaskedEntry maskDims [#text := tshow r]
  formSubmit    <- Gtk.new Gtk.Button [#label := "Go!"]

  _uiStatus     <- createUiStatus
  uiStateRef    <- newSomeRef =<< createUiState (r, c)

  pure UiForm { .. }
  where maskDims = maskClamp 1 25

createUiStatus :: (MonadIO m) => m UiStatus
createUiStatus = do
  statusContainer <- Gtk.new
    Gtk.Box
    [#expand := True, #orientation := Gtk.OrientationVertical]
  flagsRemaining <- Gtk.new Gtk.Label [#expand := True]
  turns          <- Gtk.new Gtk.Label [#expand := True]

  addClasses statusContainer ["status"]
  addClasses flagsRemaining  ["status"]
  addClasses turns           ["status"]

  pure UiStatus { .. }
