module Run
  ( run
  ) where

import           Import
import           UI                             ( runUi )

run :: (MonadIO m) => m ()
run = runUi
