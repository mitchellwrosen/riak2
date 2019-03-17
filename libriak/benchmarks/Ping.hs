{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Monad
import GHC.Clock
import GHC.Conc                (newTVarIO)
import Libriak.Handle
import Net.IPv4                (ipv4)
import Socket.Stream.IPv4      (Endpoint(..))
import Text.Printf
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
  [readMaybe -> Just threads, readMaybe -> Just pings] <- getArgs

  Right handle <-
    connect (Endpoint (ipv4 127 0 0 1) 8087) mempty

  timeoutVar <-
    newTVarIO False

  doneVar <- newEmptyMVar
  readyVar <- newEmptyMVar
  goVar <- newEmptyMVar

  replicateM_ threads $
    forkIO $ do
      putMVar readyVar ()
      readMVar goVar

      replicateM_ pings $
        ping timeoutVar handle >>= \case
          Left err -> print err
          Right (Left err) -> print err
          Right (Right ()) -> pure ()

      putMVar doneVar ()

  replicateM_ threads (takeMVar readyVar)

  t0 <- getMonotonicTime
  putMVar goVar ()
  replicateM_ threads (takeMVar doneVar)
  t1 <- getMonotonicTime

  printf
    "%d threads, %d pings each: %.2f pings/second\n"
    threads
    pings
    (fromIntegral (threads * pings) / (t1-t0))
