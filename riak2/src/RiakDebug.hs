module RiakDebug
  ( debug
  ) where

import System.IO.Unsafe (unsafePerformIO)


lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

debug :: [Char] -> IO ()
debug msg =
  withMVar lock $ \_ -> putStrLn ("[riak debug] " ++ msg)
