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

-- | Should live in a conf file
defaultDims :: (Row, Col)
defaultDims = (25, 25)

runUi :: MonadIO m => m ()
runUi = liftIO . withGtkApp $ \application -> do
  loadStyles

  _uiForm       <- createUiForm defaultDims
  mainWindow    <- new Gtk.Window [#title := "LambdaMine", #resizable := False]
  mainContainer <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  flip runUiT Ui { .. } $ do
    constructUi
    addFormHandlers
    #showAll =<< view mainWindowL

withGtkApp :: (Gtk.Application -> IO ()) -> IO ()
withGtkApp f = void $ do
  app <- Gtk.new Gtk.Application []
  _   <- Gtk.on app #activate (f app)
  #run app Nothing

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

--------------------------------------------------------------------------------
-- UiT -- transformer for UI context and state
--------------------------------------------------------------------------------
newtype UiT m a = UiT
  { unUiT :: ReaderT Ui m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFix
    , MonadUnliftIO
    , MonadReader Ui
    )

-- Allows us to `unlift` UI actions. Also simplifies exception handling.
instance (MonadIO m) => MonadState UiState (UiT m) where
  get = view uiGridRefL >>= readSomeRef
  put s = view uiGridRefL >>= flip writeSomeRef s

runUiT :: (MonadIO m) => UiT m a -> Ui -> m a
runUiT (UiT f) = runReaderT f

instance (MonadUnliftIO m, MonadFix m) => MonadUi UiState (UiT m) where
  useGridContainer       = view mainContainerL
  useFlagsRemainingLabel = view flagsRemainingLabelL
  useTurnsLabel          = view turnsLabelL
  createUiState          = initUiState


--------------------------------------------------------------------------------
-- UI event handlers
--------------------------------------------------------------------------------
handleFormSubmit :: (MonadUnliftIO m, MonadFix m) => UiT m ()
handleFormSubmit = do
  heightEntry <- view heightEntryL
  widthEntry  <- view widthEntryL

  rows        <-
    either (const (fst defaultDims)) fst . T.decimal <$> #getText heightEntry
  cols <-
    either (const (snd defaultDims)) fst . T.decimal <$> #getText widthEntry

  initializeGrid (rows, cols)

addFormHandlers :: (MonadUnliftIO m, MonadFix m) => UiT m ()
addFormHandlers = do
  formSubmit <- view formSubmitL
  withRunInIO $ \unlift -> void $ do
    void $ on formSubmit #clicked (unlift handleFormSubmit)

-------------------------------------------------------------------------------
-- UI construction
-------------------------------------------------------------------------------
initUiState :: (MonadIO m) => (Row, Col) -> m UiState
initUiState uiGridDims@(rows, cols) = do
  grid    <- new Gtk.Grid [#rowHomogeneous := True, #columnHomogeneous := True]
  buttons <- replicateM rows cols newCell
  let react      = Nothing
      freezeGrid = False
  pure UiState { .. }

createUiStatus :: (MonadIO m) => m UiStatus
createUiStatus = do
  statusContainer <- Gtk.new
    Gtk.Box
    [#expand := True, #orientation := Gtk.OrientationVertical]
  flagsRemainingLabel <- Gtk.new Gtk.Label [#expand := True]
  turnsLabel          <- Gtk.new Gtk.Label [#expand := True]

  addClasses statusContainer     ["status"]
  addClasses flagsRemainingLabel ["status"]
  addClasses turnsLabel          ["status"]

  pure UiStatus { .. }

createUiForm :: (MonadUnliftIO m, MonadFix m) => (Row, Col) -> m UiForm
createUiForm (r, c) = do
  uiGridRef     <- newSomeRef =<< initUiState (r, c)
  formContainer <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  widthEntry    <- newMaskedEntry maskDims [#text := tshow c]
  heightEntry   <- newMaskedEntry maskDims [#text := tshow r]
  formSubmit    <- Gtk.new Gtk.Button [#label := "Go!"]

  _uiStatus     <- createUiStatus

  pure UiForm { .. }
  where maskDims = maskClamp 1 25

constructUi :: (MonadUnliftIO m, MonadFix m) => UiT m ()
constructUi = do
  ui <- ask

  #add (ui ^. formContainerL) (ui ^. statusContainerL)
  #add (ui ^. formContainerL) (ui ^. heightEntryL)
  #add (ui ^. formContainerL) (ui ^. widthEntryL)
  #add (ui ^. formContainerL) (ui ^. formSubmitL)

  #add (ui ^. statusContainerL) (ui ^. flagsRemainingLabelL)
  #add (ui ^. statusContainerL) (ui ^. turnsLabelL)

  #add (ui ^. mainContainerL) (ui ^. formContainerL)
  #add (ui ^. mainWindowL) (ui ^. mainContainerL)

  #addWindow (ui ^. applicationL) (ui ^. mainWindowL)

  initializeGrid defaultDims
