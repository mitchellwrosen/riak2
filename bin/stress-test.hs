{-# LANGUAGE LambdaCase, OverloadedStrings #-}

import Riak

import Control.Concurrent
import Control.Monad
import Data.Default.Class (def)
import GHC.Clock
import Net.IPv4           (ipv4)
import Socket.Stream.IPv4 (Endpoint(..))
import System.Random

import qualified Data.ByteString.Char8 as Latin1

main :: IO ()
main = do
  handle <-
    createHandle
      HandleConfig
        { endpoint = Endpoint { address = ipv4 127 0 0 1, port = 8087 }
        , healthCheckInterval = 1/8
        , idleTimeout = 1/4
        , requestTimeout = 1
        , retries = 0
        , handlers = mempty -- EventHandlers print print print print
        }

  bucket <- Latin1.pack . show <$> getMonotonicTimeNSec

  doneVar <- newEmptyMVar
  readyVar <- newEmptyMVar
  goVar <- newEmptyMVar

  let putThreads  = 100
  let getThreads  = 100
  let listThreads = 5

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
