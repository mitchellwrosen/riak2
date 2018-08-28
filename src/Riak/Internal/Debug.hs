{-# LANGUAGE NoImplicitPrelude #-}

module Riak.Internal.Debug
  ( debug
  ) where

import System.IO
import System.IO.Unsafe

import Riak.Internal.Prelude

_lock :: MVar ()
_lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE _lock #-}

-- TODO: add cabal flag to control debugging
debug :: String -> IO ()
debug _msg =
  withMVar _lock (\_ -> hPutStrLn stderr _msg)
  -- pure ()
