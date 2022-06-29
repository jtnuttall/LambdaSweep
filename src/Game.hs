{-# LANGUAGE Arrows #-}
module Game where

import           ActionMsg
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.InternalCore
                                                ( MSF(..) )
import           Game.FRP
import           Game.State
import           Grid
import           Import
import qualified RIO.HashSet                   as HS
import           RIO.State

createGame :: (MonadUnliftIO m, MonadFix m) => GameState -> m ActionHandler
createGame = reactBlockingWH . gameSF

--------------------------------------------------------------------------------
-- React to input
--------------------------------------------------------------------------------
gameSF :: (MonadFix m) => GameState -> MSF m ActionMsg (GameState, Bool)
gameSF initialState = proc msg -> do
  rec state  <- iPre initialState    -< state'
      state' <- turn <<< unmineFirst -< (msg, state)
  end <- arr ((Continue /=) . (^. endGameL)) -< state'
  returnA -< (state', end)

unmineFirst :: (Monad m) => MSF m (ActionMsg, GameState) (ActionMsg, GameState)
unmineFirst = dSwitch (arr unmine) (const identity)
 where
  unmine :: (ActionMsg, GameState) -> ((ActionMsg, GameState), Event ())
  unmine = \case
    (msg@(RevealCell p), s) -> (, Event ()) . (msg, ) . fromMaybe s $ do
      guard $ s ^?! ix p . minedL
      pure . recalculateAdjacent p $ (s & ix p . minedL .~ False)
    input -> (input, NoEvent)

  recalculateAdjacent :: GridPos -> GameState -> GameState
  recalculateAdjacent p s = foldl'
    (\s' p -> s' & ix p . adjacentMinesL %~ subtractGe0 1)
    s
    (iAdjacent (Just (s ^. gridDimsL)) p)

--------------------------------------------------------------------------------
-- Evaluate a single turn
--------------------------------------------------------------------------------
turn :: Monad m => MSF m (ActionMsg, GameState) GameState
turn = switch
  (proc input -> do
    state'   <- chooseAction <<< second startTurn -< input
    endState <- gameOver                          -< input & _2 .~ state'
    returnA -< (state', endState)
  )
  (\egs -> arr (\(_, s) -> s
      & endGameL .~ egs
      & cellStatesL . traverse . revealedL .~ True
      & cellStatesL . traverse . dirtyL .~ True
    )
  )

startTurn :: (Monad m) => MSF m GameState GameState
startTurn = arr (cellStatesL . traverse . dirtyL .~ False)

chooseAction :: (Monad m) => MSF m (ActionMsg, GameState) GameState
chooseAction = MSF $ \(msg, s) -> do
  (s', _) <- unMSF (chooseActionSF msg) (extractPos msg, s)
  pure (s', chooseAction)
 where
  chooseActionSF = \case
    RevealCell _ -> reveal
    FlagCell   _ -> flag

gameOver :: (Monad m) => MSF m (ActionMsg, GameState) (Event EndGameState)
gameOver = arr $ \(msg, gs) -> case msg of
  RevealCell p | gs ^?! ix p . flaggedL               -> NoEvent
               | gs ^?! ix p . minedL                 -> Event EndLoss
               | gs ^. nRevealedL >= gs ^. nToRevealL -> Event EndWin
               | otherwise                            -> NoEvent
  _ -> NoEvent

--------------------------------------------------------------------------------
-- React to a single action
--------------------------------------------------------------------------------
reveal :: forall m . (Monad m) => MSF m (GridPos, GameState) GameState
reveal = proc (startPos, state) -> do
  dirty  <- cellsToReveal -< (state, startPos)
  state' <- revealCells   -< (state, dirty)
  arr (nTurnsL +~ 1)   -< state'
 where
  revealCells = arr . uncurry $ foldr
    (\p ->
      (ix p . revealedL .~ True) . (ix p . dirtyL .~ True) . (nRevealedL +~ 1)
    )

  cellsToReveal = arr . uncurry $ \gameState ->
    let dfs :: Maybe Int -> GridPos -> State (HashSet GridPos) [GridPos]
        dfs stopIn p = gets (has (ix p)) >>= \case
          True  -> pure []
          False -> do
            at p ?= ()

            let CellState {..} = gameState ^?! ix p
                adjs           = iAdjacentSq (gridDims gameState) p
                stop           = maybe False (<= 0) stopIn

                stopIn' | adjacentMines > 0 = (subtract 1 <$> stopIn) <|> Just 2
                        | otherwise         = Nothing
                goNext = traverse (dfs stopIn') adjs

            if
              | mined || revealed || flagged -> pure []
              | stop                         -> pure [p]
              | otherwise                    -> (p :) . join <$> goNext
    in  flip evalState HS.empty . dfs Nothing

flag :: (Monad m) => MSF m (GridPos, GameState) GameState
flag = arr (\(p, s) -> execState (flag' p) s)
 where
  flag' :: GridPos -> State GameState ()
  flag' p = do
    revealed <- gets (^?! ix p . revealedL)
    flagged  <- gets (^?! ix p . flaggedL)
    nFlags   <- use nFlagsL

    let validSelection = not revealed && (nFlags > 0 || flagged)

    when validSelection $ do
      if not flagged && nFlags > 0 then nFlagsL -= 1 else nFlagsL += 1

      ix p . flaggedL .= not flagged
      ix p . dirtyL .= True

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------
subtractGe0 :: (Ord p, Num p) => p -> p -> p
subtractGe0 v n | n > 0     = n - v
                | otherwise = n

