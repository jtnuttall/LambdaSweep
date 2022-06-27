module Game.FRP where

import           Control.Applicative            ( Alternative(..) )
import qualified Control.Category              as Cat
import           Control.Monad.Trans.MSF        ( reactimateB )
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.InternalCore
import           Import
import           UnliftIO.Concurrent            ( forkIO )

--------------------------------------------------------------------------------
-- Primitive wormholes with messaging
--------------------------------------------------------------------------------
createSource :: (MonadIO m) => m (MVar a, MSF m () a)
createSource = do
  wh <- newEmptyMVar
  pure (wh, constM (takeMVar wh))

createSink :: (MonadIO m) => m (MVar a, MSF m (a, Bool) Bool)
createSink = do
  wh <- newEmptyMVar
  pure
    ( wh
    , arrM (\(gameState, end) -> evaluate gameState >>= putMVar wh >> pure end)
    )

reactBlockingWH
  :: (MonadUnliftIO m, MonadFix m) => MSF m a (b, Bool) -> m (a -> IO b)
reactBlockingWH msf = do
  (sourceWH, sourceSF) <- createSource
  (sinkWH  , sinkSF  ) <- createSink

  void . forkIO . reactimateB $ sourceSF >>> msf >>> sinkSF

  pure (\msg -> putMVar sourceWH msg >> takeMVar sinkWH)


--------------------------------------------------------------------------------
-- The following FRP utilities are based on Ivan Perez-Keera's BearRiver
-- (https://hackage.haskell.org/package/bearriver-0.13.5).
--
-- I've reimplemented them so that they no longer require any knowledge of time.
--------------------------------------------------------------------------------
data Event a
  = Event a
  | NoEvent
  deriving
    ( Show
    , Functor
    , Foldable
    , Traversable
    )

instance Applicative Event where
  pure = Event
  NoEvent   <*> _         = NoEvent
  _         <*> NoEvent   = NoEvent
  (Event f) <*> (Event a) = Event (f a)

instance Alternative Event where
  empty = NoEvent
  NoEvent <|> b = b
  a       <|> _ = a

instance Monad Event where
  NoEvent >>= _ = NoEvent
  Event a >>= f = f a

identity :: Category cat => cat a a
identity = Cat.id

-- | Switches between two signal functions. Evaluates to the first argument
-- until Just appears in the output of the signal function, then evaluates to
-- the second thereafter.
switch :: (Monad m) => MSF m a (b, Event c) -> (c -> MSF m a b) -> MSF m a b
switch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (_, Event c) -> unMSF (sfC c) a
    (b, NoEvent) -> return (b, switch ct sfC)

-- | Switch with delayed evaluation.
dSwitch :: (Monad m) => MSF m a (b, Event c) -> (c -> MSF m a b) -> MSF m a b
dSwitch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Event c) -> do
      (_, ct') <- unMSF (sfC c) a
      return (b, ct')
    (b, NoEvent) -> return (b, dSwitch ct sfC)

