{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module UI.MonadUI where

import           ActionMsg
import           Data.GI.Base
import           Data.GI.Base.Signals
import qualified Data.Text.Read                as T
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
-- UI State & Context
--------------------------------------------------------------------------------
data UiGridState = UiGridState
  { dims    :: (Row, Col)
  , grid    :: Gtk.Grid
  , buttons :: Grid Gtk.Button
  }

makeProjectClassy ''UiGridState

data UiState = UiState
  { react        :: Maybe ActionHandler
  , freezeGrid   :: Bool
  , _uiGridState :: UiGridState
  }

makeProjectClassy ''UiState

instance HasUiGridState UiState where
  uiGridStateL = lens _uiGridState (\x y -> x { _uiGridState = y })

data UiStatus = UiStatus
  { statusContainer :: Gtk.Box
  , flagsRemaining  :: Gtk.Label
  , turns           :: Gtk.Label
  }

makeProjectClassy ''UiStatus

data UiForm = UiForm
  { formContainer :: Gtk.Box
  , widthEntry    :: Gtk.Entry
  , heightEntry   :: Gtk.Entry
  , formSubmit    :: Gtk.Button
  , _uiStatus     :: UiStatus
  , uiStateRef    :: SomeRef UiState
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

-- | Thin abstraction over UI actions. I think this can be expanded into an 
-- mtl-style monad and a declarative style to make this architecture more 
-- general.
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

-- ReaderT containing a ref allows us to `unlift` UI actions. Also simplifies 
-- exception handling.
instance (MonadIO m) => MonadState UiState (UiT m) where
  get = view uiStateRefL >>= readSomeRef
  put s = view uiStateRefL >>= flip writeSomeRef s

runUiT :: (MonadIO m) => UiT m a -> Ui -> m a
runUiT (UiT f) = runReaderT f


--------------------------------------------------------------------------------
-- General UI actions
--------------------------------------------------------------------------------
updateStatus :: (MonadIO m, HasGameState s) => s -> UiT m ()
updateStatus newState = do
  UiStatus {..} <- view uiStatusL

  Gtk.set
    flagsRemaining
    [ #label Gtk.:= "Flags: " <> tshow (newState ^. nFlagsL) <> "/" <> tshow
        (newState ^. nMinesL)
    ]

  Gtk.set turns [#label Gtk.:= "Turns: " <> tshow (newState ^. nTurnsL)]

checkEnd :: (MonadIO m, HasGameState s) => s -> UiT m ()
checkEnd newState =
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

handleFormSubmit :: (MonadUnliftIO m, MonadFix m) => UiT m ()
handleFormSubmit = do
  heightEntry <- view heightEntryL
  widthEntry  <- view widthEntryL

  rows        <- getIntegralInput (fst defaultDims) heightEntry
  cols        <- getIntegralInput (snd defaultDims) widthEntry

  initializeGrid (rows, cols)

addFormHandlers :: (MonadUnliftIO m, MonadFix m) => UiT m ()
addFormHandlers = do
  formSubmit <- view formSubmitL
  withRunInIO $ \unlift -> void $ do
    void $ on formSubmit #clicked (unlift handleFormSubmit)


--------------------------------------------------------------------------------
-- Game communication
--------------------------------------------------------------------------------
initializeGameSink
  :: (MonadFix m, MonadUnliftIO m) => Maybe ActionHandler -> UiT m ActionHandler
initializeGameSink = \case
  Nothing -> do
    (r, c) <- use dimsL
    react  <- createGame =<< liftIO (newGameState 20 r c)
    reactL <?= react
  Just f -> pure f


--------------------------------------------------------------------------------
-- UI State initialization
--------------------------------------------------------------------------------
createUiState :: (MonadIO m) => (Row, Col) -> m UiState
createUiState dims = do
  _uiGridState <- createUiGridState dims
  let react      = Nothing
      freezeGrid = False
  pure UiState { .. }

createUiGridState :: (MonadIO m) => (Row, Col) -> m UiGridState
createUiGridState dims@(r, c) = do
  grid <- Gtk.new Gtk.Grid [#rowHomogeneous := True, #columnHomogeneous := True]
  buttons <- replicateM r c newCell
  pure UiGridState { .. }


--------------------------------------------------------------------------------
-- UI Grid actions
--------------------------------------------------------------------------------
initializeGrid :: (MonadFix m, MonadUnliftIO m) => (Row, Col) -> UiT m ()
initializeGrid (r, c) = do
  cont <- view mainContainerL
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

repaintGrid :: (MonadIO m) => GameState -> UiT m ()
repaintGrid GameState {..} = do
  buttons <- use buttonsL

  let dirtyCells = flatFilter (dirty . snd) $ (,) <$> buttons <*> cellStates
      props      = fmap (second (buttonProps . cellRendering)) dirtyCells

  for_ props . uncurry $ \btn ButtonProps {..} -> liftIO $ do
    Gtk.set btn attrs
    dropClasses btn ["cell"]
    addClasses btn classes

addCellHandlers :: (MonadUnliftIO m, MonadFix m) => UiT m ()
addCellHandlers = do
  buttons <- use buttonsL
  withRunInIO $ \unlift -> for_ (indexed buttons) $ \(p, btn) ->
    void . Gtk.on btn #buttonPressEvent $ getUiAction p >=> \case
      Nothing     -> pure True
      Just action -> do
        unlift $ handleCellClick action
        pure False

handleCellClick :: (MonadUnliftIO m, MonadFix m) => ActionMsg -> UiT m ()
handleCellClick msg = void . forkIO $ do
  frozen <- use freezeGridL

  unless frozen $ do
    react    <- use reactL >>= initializeGameSink
    newState <- liftIO $ react msg

    withRunInIO $ \unlift -> void . GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      unlift $ do
        checkEnd newState
        updateStatus newState
        repaintGrid newState
      pure False
