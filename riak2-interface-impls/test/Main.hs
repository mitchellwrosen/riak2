module Main where

import Riak.Request  (Request(..))
import Riak.Response (Response(..))
import Riak.Socket   (Socket(..))

-- import qualified Riak.Interface.Impl.Managed.Concurrent as Iface.Managed.Concurrent
-- import qualified Riak.Interface.Impl.Managed.Socket     as Iface.Managed.Socket
import qualified Riak.Interface.Impl.Socket as Iface.Socket
-- import qualified Riak.Interface.Impl.Socket.Concurrent  as Iface.Concurrent
import qualified Riak.Proto    as Proto
import qualified Riak.Socket   as Socket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

import qualified Data.ProtoLens as Proto
import qualified Network.Socket as Network

main :: IO ()
main = do
  pure () -- socketmain

socketmain :: IO ()
socketmain = do
  client <- mockServer

  let
    config :: Iface.Socket.Config
    config =
      Iface.Socket.Config
        { Iface.Socket.socket = client
        , Iface.Socket.handlers = mempty
        }

  Iface.Socket.withInterface config $ \iface -> do

    let workers = 10
    let requests = 1000

    readyVar <- newTVarIO False
    doneVar <- newEmptyMVar

    replicateM_ workers . forkIO $ do
      atomically (readTVar readyVar >>= flip unless retry)

      replicateM_ requests $ do
        request <-
          randomOneOf
            [ RequestPing Proto.defMessage
            , RequestGet Proto.defMessage
            , RequestPut Proto.defMessage
            , RequestDelete Proto.defMessage
            ]

        response <- Iface.Socket.exchange iface request

        unless (expectedResponse request response) $ do
          putStrLn "ERROR"
          putStrLn (">>> " ++ show request)
          putStrLn ("<<< " ++ show response)

      putMVar doneVar ()

    atomically (writeTVar readyVar True)

    replicateM_ workers (takeMVar doneVar)

socketPair :: IO (Socket, Socket)
socketPair = do
  (client, server) <-
    Network.socketPair
      Network.AF_UNIX
      Network.Stream
      Network.defaultProtocol

  (,)
    <$> (Socket.new3 client =<< Network.getPeerName client)
    <*> (Socket.new3 server =<< Network.getPeerName server)

mockServer :: IO Socket
mockServer = do
  (client, server) <-
    socketPair

  parentThread <- myThreadId

  void . forkIO . forever $
    let
      respond :: Proto.Message a => (a -> Response) -> IO ()
      respond f =
        Socket.sendResponse server (f Proto.defMessage)
    in
      Socket.receiveRequest server >>= \case
        Nothing -> do
          putStrLn "Riak disappeared?"
          killThread parentThread

        Just request ->
          case request of
            RequestDelete{}         -> respond ResponseDelete
            RequestDeleteIndex{}    -> respond ResponseDelete
            RequestGetBucket{}      -> respond ResponseGetBucket
            RequestGetBucketType{}  -> respond ResponseGetBucket
            RequestGetCrdt{}        -> respond ResponseGetCrdt
            RequestGetIndex{}       -> respond ResponseGetIndex
            RequestGetServerInfo{}  -> respond ResponseGetServerInfo
            RequestGetSchema{}      -> respond ResponseGetSchema
            RequestGet{}            -> respond ResponseGet
            RequestListBuckets{}    -> respond ResponseListBuckets
            RequestListKeys{}       -> respond ResponseListKeys
            -- RequestMapReduce{}   -> respond ResponseMapReduce
            RequestPing{}           -> respond ResponsePing
            RequestPut{}            -> respond ResponsePut
            RequestPutIndex{}       -> respond ResponsePut
            RequestPutSchema{}      -> respond ResponsePut
            RequestResetBucket{}    -> respond ResponseResetBucket
            RequestSecondaryIndex{} -> respond ResponseSecondaryIndex
            RequestSetBucket{}      -> respond ResponseSetBucket
            RequestSetBucketType{}  -> respond ResponseSetBucket
            RequestUpdateCrdt{}     -> respond ResponseUpdateCrdt

  pure client

expectedResponse :: Request -> Response -> Bool
expectedResponse request response =
  case request of
    RequestDelete{} ->
      case response of
        ResponseDelete{} -> True
        _ -> False
    RequestDeleteIndex{} ->
      case response of
        ResponseDelete{} -> True
        _ -> False
    RequestGetBucket{} ->
      case response of
        ResponseGetBucket{} -> True
        _ -> False
    RequestGetBucketType{} ->
      case response of
        ResponseGetBucket{} -> True
        _ -> False
    RequestGetCrdt{} ->
      case response of
        ResponseGetCrdt{} -> True
        _ -> False
    RequestGetIndex{} ->
      case response of
        ResponseGetIndex{} -> True
        _ -> False
    RequestGetServerInfo{} ->
      case response of
        ResponseGetServerInfo{} -> True
        _ -> False
    RequestGetSchema{} ->
      case response of
        ResponseGetSchema{} -> True
        _ -> False
    RequestGet{} ->
      case response of
        ResponseGet{} -> True
        _ -> False
    RequestListBuckets{} ->
      case response of
        ResponseListBuckets{} -> True
        _ -> False
    RequestListKeys{} ->
      case response of
        ResponseListKeys{} -> True
        _ -> False
    -- RequestMapReduce{} ->
    --   case response of
    --     ResponseMapReduce{} -> True
    --     _ -> False
    RequestPing{} ->
      case response of
        ResponsePing{} -> True
        _ -> False
    RequestPut{} ->
      case response of
        ResponsePut{} -> True
        _ -> False
    RequestPutIndex{} ->
      case response of
        ResponsePut{} -> True
        _ -> False
    RequestPutSchema{} ->
      case response of
        ResponsePut{} -> True
        _ -> False
    RequestResetBucket{} ->
      case response of
        ResponseResetBucket{} -> True
        _ -> False
    RequestSecondaryIndex{} ->
      case response of
        ResponseSecondaryIndex{} -> True
        _ -> False
    RequestSetBucket{} ->
      case response of
        ResponseSetBucket{} -> True
        _ -> False
    RequestSetBucketType{} ->
      case response of
        ResponseSetBucket{} -> True
        _ -> False
    RequestUpdateCrdt{} ->
      case response of
        ResponseUpdateCrdt{} -> True
        _ -> False

randomOneOf :: [a] -> IO a
randomOneOf xs =
  (xs !!) <$> randomRIO (0, length xs - 1)
