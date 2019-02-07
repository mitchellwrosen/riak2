{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ZZZ.Riak.Internal.Debug
  ( debug
  ) where

import Riak.Internal.Prelude

import Control.Concurrent.MVar
import System.IO
import System.IO.Unsafe


_lock :: MVar ()
_lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE _lock #-}

-- TODO: add cabal flag to control debugging
debug :: MonadIO m => String -> m ()
debug _msg =
  -- liftIO (withMVar _lock (\_ -> hPutStrLn stderr _msg))
  pure ()
