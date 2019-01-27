module Main where

import Riak.Request  (Request(..))
import Riak.Response (Response(..))
import Riak.Socket   (Socket(..))

import qualified Riak.Interface.Impl.Managed.Concurrent as Iface.Managed.Concurrent
import qualified Riak.Interface.Impl.Managed.Socket     as Iface.Managed.Socket
import qualified Riak.Interface.Impl.Socket             as Iface.Socket
import qualified Riak.Interface.Impl.Socket.Concurrent  as Iface.Concurrent
import qualified Riak.Proto                             as Proto
import qualified Riak.Response                          as Response
import qualified Riak.Socket                            as Socket

import Control.Concurrent
import Control.Monad

import qualified Network.Socket            as Network hiding (recv)
import qualified Network.Socket.ByteString as Network (recv, sendAll)

main :: IO ()
main = do
  socketmain

socketmain :: IO ()
socketmain = do
  (client, server) <-
    socketPair

  _ <- forkIO $ do
    case server of
      Socket s _ _ -> do
        -- void $ Network.recv s 4096
        Network.sendAll s (Response.encode (ResponsePing Proto.defMessage))

  iface :: Iface.Socket.Interface <-
    Iface.Socket.new
      client
      mempty
        { Iface.Socket.onSend = \request -> putStrLn (">>> " ++ show request)
        , Iface.Socket.onReceive = \response -> putStrLn ("<<< " ++ show response)
        }

  void $ Iface.Socket.exchange iface (RequestPing Proto.defMessage)

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
