module Main where

import Riak.Request  (Request(..))
import Riak.Response (Response(..))
import Socket        (Socket(..))

-- import qualified Riak.Interface.Impl.Managed.Concurrent as Iface.Managed.Concurrent
-- import qualified Riak.Interface.Impl.Managed.Socket     as Iface.Managed.Socket
import qualified Riak.Interface.Impl.Socket             as Iface.Socket
-- import qualified Riak.Interface.Impl.Socket.Concurrent  as Iface.Concurrent
import qualified Riak.Proto                             as Proto
import qualified Riak.Request                           as Request
import qualified Riak.Response                          as Response
import qualified Socket                                 as Socket

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

import qualified Data.ProtoLens            as Proto
import qualified Network.Socket            as Network

main :: IO ()
main = do
  socketmain

socketmain :: IO ()
socketmain = do
  client <- mockServer

  iface :: Iface.Socket.Interface <-
    Iface.Socket.new
      client
      mempty
        -- { Iface.Socket.onSend = \request -> putStrLn (">>> " ++ show request)
        -- , Iface.Socket.onReceive = \response -> putStrLn ("<<< " ++ show response)
        -- }

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

      Iface.Socket.exchange iface request >>= \case
        Nothing ->
          putStrLn "Riak disappeared?"

        Just response ->
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
        Socket.send server (Response.encode (f Proto.defMessage))
    in
      Socket.receive server >>= \case
        Nothing -> do
          putStrLn "Riak disappeared?"
          killThread parentThread

        Just bytes ->
          case Request.parse bytes of
            Left err ->
              throwTo parentThread err

            Right request ->
              case request of
                RequestDelete{}                  -> respond ResponseDelete
                RequestGetBucketProperties{}     -> respond ResponseGetBucketProperties
                RequestGetBucketTypeProperties{} -> respond ResponseGetBucketProperties
                RequestGetCrdt{}                 -> respond ResponseGetCrdt
                RequestGetServerInfo{}           -> respond ResponseGetServerInfo
                RequestGet{}                     -> respond ResponseGet
                RequestListBuckets{}             -> respond ResponseListBuckets
                RequestListKeys{}                -> respond ResponseListKeys
                -- RequestMapReduce{}               -> respond ResponseMapReduce
                RequestPing{}                    -> respond ResponsePing
                RequestPut{}                     -> respond ResponsePut
                RequestResetBucketProperties{}   -> respond ResponseResetBucketProperties
                RequestSecondaryIndex{}          -> respond ResponseSecondaryIndex
                RequestSetBucketProperties{}     -> respond ResponseSetBucketProperties
                RequestSetBucketTypeProperties{} -> respond ResponseSetBucketProperties
                RequestUpdateCrdt{}              -> respond ResponseUpdateCrdt

  pure client

expectedResponse :: Request -> Response -> Bool
expectedResponse request response =
  case request of
    RequestDelete{} ->
      case response of
        ResponseDelete{} -> True
        _ -> False
    RequestGetBucketProperties{} ->
      case response of
        ResponseGetBucketProperties{} -> True
        _ -> False
    RequestGetBucketTypeProperties{} ->
      case response of
        ResponseGetBucketProperties{} -> True
        _ -> False
    RequestGetCrdt{} ->
      case response of
        ResponseGetCrdt{} -> True
        _ -> False
    RequestGetServerInfo{} ->
      case response of
        ResponseGetServerInfo{} -> True
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
    RequestResetBucketProperties{} ->
      case response of
        ResponseResetBucketProperties{} -> True
        _ -> False
    RequestSecondaryIndex{} ->
      case response of
        ResponseSecondaryIndex{} -> True
        _ -> False
    RequestSetBucketProperties{} ->
      case response of
        ResponseSetBucketProperties{} -> True
        _ -> False
    RequestSetBucketTypeProperties{} ->
      case response of
        ResponseSetBucketProperties{} -> True
        _ -> False
    RequestUpdateCrdt{} ->
      case response of
        ResponseUpdateCrdt{} -> True
        _ -> False

randomOneOf :: [a] -> IO a
randomOneOf xs =
  (xs !!) <$> randomRIO (0, length xs - 1)
