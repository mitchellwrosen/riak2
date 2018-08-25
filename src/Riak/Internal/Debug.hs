{-# LANGUAGE NoImplicitPrelude #-}

module Riak.Internal.Debug where

import System.IO
import System.IO.Unsafe

import Riak.Internal.Prelude

lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

-- TODO: add cabal flag to control debugging
debug :: String -> IO ()
debug msg =
  -- withMVar lock (\_ -> hPutStrLn stderr msg)
  pure ()
