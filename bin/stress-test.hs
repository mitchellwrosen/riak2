{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}

import Riak

import Control.Concurrent
import Control.Monad
import Data.Default.Class (def)
import GHC.Clock
import Net.IPv4           (ipv4)
import Socket.Stream.IPv4 (Endpoint(..))
import System.Environment
import System.Random
import Text.Read          (readMaybe)

import qualified Data.ByteString.Char8 as Latin1

main :: IO ()
main = do
  [ readMaybe -> Just putThreads,
    readMaybe -> Just getThreads,
    readMaybe -> Just listThreads ] <- getArgs

  handle <-
    createHandle
      HandleConfig
        { endpoint = Endpoint { address = ipv4 127 0 0 1, port = 8087 }
        , healthCheckInterval = 1/8
        , idleTimeout = 1/4
        , requestTimeout = 1
        , retries = 0
        , handlers =
            EventHandlers
              { onSend = \msg -> putStrLn (">>> " ++ show msg)
              , onReceive = \msg -> putStrLn ("<<< " ++ show msg)
              , onConnectError = \ex -> putStrLn ("*** " ++ show ex)
              , onConnectionError = \ex -> putStrLn ("*** " ++ show ex)
              }
        }

  bucket <- Latin1.pack . show <$> getMonotonicTimeNSec

  doneVar <- newEmptyMVar
  readyVar <- newEmptyMVar
  goVar <- newEmptyMVar

  replicateM_ putThreads $ forkIO $ do
    putMVar readyVar ()
    readMVar goVar

    replicateM_ 100 $ do
      key <- Latin1.pack . show <$> randomRIO (0::Int,999)

      put
        handle
        (newObject
          (Key defaultBucketType bucket key)
          (newContent ""))
        def >>= \case
          Left err -> print err
          Right _ -> pure ()

    putMVar doneVar ()

  replicateM_ putThreads $ forkIO $ do
    putMVar readyVar ()
    readMVar goVar

    replicateM_ 100 $ do
      key <- Latin1.pack . show <$> randomRIO (0::Int,999)

      get
        handle
        (Key defaultBucketType bucket key)
        def >>= \case
          Left err -> print err
          Right _ -> pure ()

    putMVar doneVar ()

  replicateM_ listThreads $ forkIO $ do
    putMVar readyVar ()
    readMVar goVar

    replicateM_ 5 $
      listKeys handle (Bucket defaultBucketType bucket) >>= \case
        Left err -> print err
        Right _ -> pure ()

    putMVar doneVar ()

  replicateM_ (putThreads + getThreads + listThreads) (takeMVar readyVar)
  putMVar goVar ()

  replicateM_ (putThreads + getThreads + listThreads) (takeMVar doneVar)
