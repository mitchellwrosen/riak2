-- | Misc. STM helpers

module RiakSTM
  ( TCounter
  , newTCounter
  , readTCounter
  , incrTCounter
  , decrTCounter
  ) where

import Control.Concurrent.STM


newtype TCounter
  = TCounter (TVar Int)

newTCounter :: IO TCounter
newTCounter =
  TCounter <$> newTVarIO 0

readTCounter :: TCounter -> STM Int
readTCounter (TCounter var) =
  readTVar var

incrTCounter :: TCounter -> STM ()
incrTCounter (TCounter var) =
  modifyTVar' var (+1)

decrTCounter :: TCounter -> STM ()
decrTCounter (TCounter var) =
  modifyTVar' var (subtract 1)
