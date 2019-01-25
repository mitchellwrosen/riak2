module Riak.Interface.Impl.Socket
  ( Interface
  , EventHandlers(..)
  , new
  , connect
  , disconnect
  , send
  , receive
  , exchange
  , stream
  ) where

import Riak.Request  (Request)
import Riak.Response (DecodeError, Response)

import qualified Riak.Request  as Request
import qualified Riak.Response as Response

import Control.Concurrent.MVar
import Control.Exception       (throwIO)
import Data.ByteString         (ByteString)
import Data.IORef

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as ByteString
import qualified Socket.Signature           as Socket


data Interface
  = Interface
  { socket :: !Socket.Socket
  , bufferRef :: !(IORef ByteString)
  , lock :: !(MVar ())
  , handlers :: !EventHandlers
  }

data EventHandlers
  = EventHandlers
  { onConnect :: IO ()
  , onDisconnect :: IO ()
  , onSend :: Request -> IO ()
  , onReceive :: Maybe Response -> IO ()
  }

new ::
     Socket.Socket -- ^
  -> EventHandlers -- ^
  -> IO Interface
new socket handlers = do
  bufferRef <- newIORef ByteString.empty
  lock <- newMVar ()

  pure Interface
    { socket = socket
    , bufferRef = bufferRef
    , lock = lock
    , handlers = handlers
    }

connect ::
     Interface -- ^
  -> IO ()
connect iface = do
  onConnect (handlers iface)
  Socket.connect (socket iface)

disconnect ::
     Interface -- ^
  -> IO ()
disconnect iface = do
  onDisconnect (handlers iface)
  Socket.disconnect (socket iface)
  writeIORef (bufferRef iface) ByteString.empty

send ::
     Interface -- ^
  -> Request -- ^
  -> IO ()
send iface request = do
  onSend (handlers iface) request
  Socket.send (socket iface) (Request.encode request)

receive ::
     Interface -- ^
  -> IO (Maybe Response)
receive iface = do
  buffer <- readIORef (bufferRef iface)

  result :: Maybe Response <-
    loop
      (if ByteString.null buffer
        then Atto.Partial Response.parse
        else Response.parse buffer)

  onReceive (handlers iface) result
  pure result

  where
    loop ::
         Atto.IResult ByteString (Either DecodeError Response)
      -> IO (Maybe Response)
    loop = \case
      -- The response parser is just a 4-byte length followed by that many
      -- bytes, so just assume that can only fail due to not enough bytes.
      Atto.Fail _unconsumed _context _reason ->
        pure Nothing

      Atto.Partial k -> do
        bytes <- Socket.receive (socket iface) 16384

        loop
          (if ByteString.null bytes
            then k ByteString.empty
            else Response.parse bytes)

      Atto.Done unconsumed result ->
        case result of
          Left err ->
            throwIO err

          Right response -> do
            writeIORef (bufferRef iface) unconsumed
            pure (Just response)

exchange ::
     Interface -- ^
  -> Request -- ^
  -> IO (Maybe Response)
exchange iface request =
  withMVar (lock iface) $ \_ -> do
    send iface request
    receive iface

stream ::
     Interface -- ^
  -> Request -- ^
  -> (IO (Maybe Response) -> IO r) -- ^
  -> IO r
stream iface request callback =
  withMVar (lock iface) $ \_ -> do
    send iface request
    callback (receive iface)
