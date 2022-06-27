{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.MonadUI where

import           ActionMsg
import qualified GI.GLib                       as GLib
import qualified GI.Gtk                        as Gtk
import           Game                           ( createGame )
import           Game.State
import           Grid
import           Import
import           RIO.State                      ( MonadState(..) )
import           UI.Helper
import           UnliftIO.Concurrent            ( forkIO )

--------------------------------------------------------------------------------
-- UI Context
--------------------------------------------------------------------------------
data UiState = UiState
  { react      :: Maybe (ActionMsg -> IO GameState)
  , uiGridDims :: (Row, Col)
  , grid       :: Gtk.Grid
  , buttons    :: Grid Gtk.Button
  , freezeGrid :: Bool
  }

makeProjectClassy ''UiState

data UiStatus = UiStatus
  { statusContainer     :: Gtk.Box
  , flagsRemainingLabel :: Gtk.Label
  , turnsLabel          :: Gtk.Label
  }

makeProjectClassy ''UiStatus

data UiForm = UiForm
  { formContainer :: Gtk.Box
  , widthEntry    :: Gtk.Entry
  , heightEntry   :: Gtk.Entry
  , formSubmit    :: Gtk.Button
  , _uiStatus     :: UiStatus
  , uiGridRef     :: SomeRef UiState
  }

makeProjectClassy ''UiForm

instance HasUiStatus UiForm where
  uiStatusL = lens _uiStatus (\x y -> x { _uiStatus = y })

data Ui = Ui
  { application   :: Gtk.Application
  , mainWindow    :: Gtk.Window
  , mainContainer :: Gtk.Box
  , _uiForm       :: UiForm
  }

makeProjectClassy ''Ui

instance HasUiForm Ui where
  uiFormL = lens _uiForm (\x y -> x { _uiForm = y })

instance HasUiStatus Ui where
  uiStatusL = uiFormL . uiStatusL


--------------------------------------------------------------------------------
-- (very) weak abstraction for UI actions
--------------------------------------------------------------------------------
class (MonadUnliftIO m, MonadFix m, MonadState ui m, HasUiState ui) => MonadUi ui m | m -> ui where
  useGridContainer :: m Gtk.Box
  useFlagsRemainingLabel :: m Gtk.Label
  useTurnsLabel :: m Gtk.Label
  createUiState :: (Row, Col) -> m UiState

  addCellHandlers :: m ()
  addCellHandlers = do
    buttons <- use buttonsL
    withRunInIO $ \unlift -> for_ (indexed buttons) $ \(p, btn) ->
      void . Gtk.on btn #buttonPressEvent $ getUiAction p >=> \case
        Nothing     -> pure True
        Just action -> do
          unlift $ handleCellClick action
          pure False

  handleCellClick :: ActionMsg -> m ()
  handleCellClick msg = void . forkIO $ do
    frozen <- use freezeGridL

    unless frozen $ do
      react  <- use reactL >>= initializeGameSink
      newState <- liftIO $ react msg

      withRunInIO $ \unlift -> void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
        unlift $ do
          checkEndState newState
          updateStatus newState
          repaintGrid newState
        pure False

  checkEndState :: (HasGameState s) => s -> m ()
  checkEndState newState =
    let endClasses = case newState ^. endGameL of
          EndWin   -> ["end-win"]
          EndLoss  -> ["end-loss"]
          Continue -> []
    in  case endClasses of
          []  -> pure ()
          cls -> do
            grid <- use gridL
            reactL .= Nothing
            freezeGridL .= True
            addClasses grid cls

  updateStatus :: (HasGameState s) => s -> m ()
  updateStatus newState = do
      fr <- useFlagsRemainingLabel
      tl <- useTurnsLabel
      Gtk.set fr [#label Gtk.:= "Flags: " <> tshow (newState ^. flagsRemainingL)]
      Gtk.set tl [#label Gtk.:= "Turns: " <> tshow (newState ^. moveCountL)]

  initializeGameSink :: Maybe (ActionMsg -> IO GameState) -> m (ActionMsg -> IO GameState)
  initializeGameSink = \case
    Nothing -> do
      (r, c)  <- use uiGridDimsL
      react <- createGame =<< liftIO (newGameState 20 r c)
      reactL <?= react
    Just f -> pure f

  repaintGrid      :: GameState -> m ()
  repaintGrid GameState {..} = do
    buttons <- use buttonsL

    let dirtyCells = flatFilter (dirty . snd) $ (,) <$> buttons <*> cellStates
        props      = fmap (second (buttonProps . cellRendering)) dirtyCells

    for_ props . uncurry $ \btn ButtonProps {..} -> liftIO $ do
      Gtk.set btn attrs
      dropClasses btn ["cell"]
      addClasses btn classes

  initializeGrid :: (Row, Col) -> m ()
  initializeGrid (r, c) = do
    cont <- useGridContainer
    #remove cont =<< use gridL

    newUiState <- createUiState (r, c)
    uiStateL .= newUiState

    for_ (indexed (newUiState ^. buttonsL)) $ \(p, btn) ->
      let (r', c') = over both fromIntegral p
      in  #attach (newUiState ^. gridL) btn r' c' 1 1

    #add cont (newUiState ^. gridL)
    #showAll cont

    addCellHandlers
    updateStatus initialGameState

