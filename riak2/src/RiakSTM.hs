-- | Misc. STM helpers

module RiakSTM
  ( registerOneShotEvent
  ) where

import Control.Concurrent.STM


registerOneShotEvent :: Int -> IO (STM ())
registerOneShotEvent micros = do
  var :: TVar Bool <-
    registerDelay micros

  pure
    (readTVar var >>= \case
      False -> retry
      True -> pure ())
