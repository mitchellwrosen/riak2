module Main where

import Libriak.Request            (Request(..))
import Riak.Handle.Impl.Exclusive

import qualified Libriak.Proto as Proto

import Control.Concurrent
import Control.Exception  (throwIO)
import Control.Monad
import Data.IORef
import Net.IPv4           (ipv4)
import System.Exit

data Action
  = Send
  | Receive
  deriving (Show)

main :: IO ()
main = do
  actionsRef :: IORef [Action] <-
    newIORef []

  let
    config :: Config
    config =
      Config
        { endpoint =
            Endpoint
              { address = ipv4 127 0 0 1
              , port = 8087
              }
        , handlers =
            EventHandlers
              { onSend =
                  \_ ->
                    atomicModifyIORef
                      actionsRef
                      (\actions -> (Send : actions, ()))
              , onReceive =
                  \_ ->
                    atomicModifyIORef
                      actionsRef
                      (\actions -> (Receive : actions, ()))
              }
        }

  let threads = 1000

  doneVar <- newEmptyMVar
  cherryTree <- newCherryTree threads

  result <-
    withHandle config $ \handle -> do
      replicateM_ threads $ forkIO $ do
        chopCherryTree cherryTree

        _ <- exchange handle (ReqRpbPing Proto.defMessage)

        putMVar doneVar ()

      replicateM_ threads (takeMVar doneVar)

  actions <- readIORef actionsRef
  shouldBeSendRecv (reverse actions)

  case result of
    Left errno ->
      exitWith (ExitFailure (fromIntegral errno))

    Right () ->
      pure ()

shouldBeSendRecv :: [Action] -> IO ()
shouldBeSendRecv = \case
  [] -> pure ()
  Send : xs -> shouldBeRecvSend xs
  Receive : _ -> throwIO (userError "expected Send")

shouldBeRecvSend :: [Action] -> IO ()
shouldBeRecvSend = \case
  [] -> throwIO (userError "missing Receive")
  Receive : xs -> shouldBeSendRecv xs
  Send : _ -> throwIO (userError "expected Receive")

type CherryTree
  = (Int, MVar (Int, MVar ()))

newCherryTree :: Int -> IO CherryTree
newCherryTree n = do
  doneVar <- newEmptyMVar
  chopVar <- newMVar (1, doneVar)
  pure (n, chopVar)

chopCherryTree :: CherryTree -> IO ()
chopCherryTree (n, chopVar) =
  join $
    modifyMVar chopVar $ \(m, doneVar) ->
      if n == m
        then do
          pure ((m, doneVar), void (tryPutMVar doneVar ()))
        else do
          pure ((m+1, doneVar), readMVar doneVar)
