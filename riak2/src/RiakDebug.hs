{-# language CPP #-}

module RiakDebug
  ( debug
  ) where

#ifdef DEBUG

import System.IO.Unsafe (unsafePerformIO)
import Data.Time
import Text.Printf (printf)

lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

debug :: [Char] -> IO ()
debug msg =
  withMVar lock $ \_ -> do
    now <- getCurrentTime
    tid <- myThreadId
    printf "[%s] %s | %s\n" (show now) (show tid) msg

#else

debug :: [Char] -> IO ()
debug _ =
  pure ()

#endif
