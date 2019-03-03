{-# language CPP #-}

module RiakDebug
  ( debug
  ) where

#ifdef DEBUG

import System.IO.Unsafe (unsafePerformIO)

lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

debug :: [Char] -> IO ()
debug msg =
  withMVar lock $ \_ -> putStrLn ("[riak debug] " ++ msg)

#else

debug :: [Char] -> IO ()
debug _ =
  pure ()

#endif
