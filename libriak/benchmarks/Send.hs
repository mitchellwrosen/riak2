{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase, OverloadedStrings,
             StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Libriak.Connection (Connection, ConnectionError(..), send,
                           withConnection)
import Libriak.Handle
import Libriak.Request    (encodeRpbPut)
import Libriak.Response   (DecodeError(..))

import Control.Concurrent
import Control.DeepSeq
import Control.Lens       ((.~))
import Control.Monad
import Criterion
import Criterion.Main     (defaultMain)
import Data.Function      ((&))
import GHC.Conc           (newTVarIO)
import GHC.Generics
import Net.IPv4           (ipv4)
import Socket.Stream.IPv4 (Endpoint(..), receiveByteArray, withAccepted,
                           withListener)

import qualified Data.ByteString   as ByteString
import qualified Data.Riak.Proto   as Proto
import qualified System.Random.MWC as MWC

endpoint :: Endpoint
endpoint =
  Endpoint (ipv4 127 0 0 1) 13788

main :: IO ()
main = do
  readyVar <- newEmptyMVar
  forkIO (fakeRiakServer readyVar)
  takeMVar readyVar
  timeoutVar <- newTVarIO False
  withConnection timeoutVar endpoint (\_ _ -> pure ()) doMain >>= \case
    Left ex -> print ex
    Right () -> pure ()

fakeRiakServer :: MVar () -> IO ()
fakeRiakServer readyVar = do
  Right () <-
    withListener endpoint $ \listener _ -> do
      putMVar readyVar ()
      Right () <-
        withAccepted
          listener
          (\_ _ -> pure ())
          (\conn _ ->
            let loop = do
                  receiveByteArray conn 4096 >>= \case
                    Left _ -> pure ()
                    Right _ -> loop
            in loop)
      pure ()
  pure ()

doMain :: Connection -> IO ()
doMain conn = do
  gen <-
    MWC.create

  defaultMain
    [ bench "send" (perRunEnv (makeRequest gen) doSend)
    ]

  where
    makeRequest :: MWC.GenIO -> IO Proto.RpbPutReq
    makeRequest gen = do
      words <- replicateM 4000 (MWC.uniform gen)
      pure $ Proto.defMessage
        & Proto.bucket .~ "bucket"
        & Proto.content .~ (Proto.defMessage
            & Proto.value .~ ByteString.pack words)
        & Proto.w .~ 1
        & Proto.dw .~ 1
        & Proto.nVal .~ 1

    doSend :: Proto.RpbPutReq -> IO ()
    doSend req = do
      Right () <- send conn (encodeRpbPut req)
      pure ()

deriving instance Generic ConnectionError
deriving instance NFData ConnectionError

deriving instance Generic DecodeError
deriving instance NFData DecodeError

deriving instance Generic HandleError
deriving instance NFData HandleError

