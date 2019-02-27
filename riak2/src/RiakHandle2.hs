module RiakHandle2 where

import Libriak.Connection (ConnectError, ConnectionError, Endpoint)
import Libriak.Request    (Request)
import Libriak.Response   (Response)
import RiakManagedBus     (ManagedBus, ReconnectSettings, withManagedBus)

import qualified RiakManagedBus as ManagedBus

data Handle
  = Handle
  { bus :: !ManagedBus
  , handlers :: !EventHandlers
  }

data HandleConfig
  = HandleConfig
  { endpoint :: !Endpoint
  , reconnectSettings :: !(ConnectError -> Maybe ReconnectSettings)
  , handlers :: !EventHandlers
  }

-- TODO actually use event handlers
data EventHandlers
  = EventHandlers
  { onSend :: Request -> IO ()
  , onReceive :: Either ConnectionError Response -> IO ()
  }

instance Monoid EventHandlers where
  mempty = EventHandlers mempty mempty
  mappend = (<>)

instance Semigroup EventHandlers where
  EventHandlers a1 b1 <> EventHandlers a2 b2 =
    EventHandlers (a1 <> a2) (b1 <> b2)


-- | Acquire a handle.
--
-- /Throws/: This function will never throw an exception.
withHandle ::
     HandleConfig
  -> (Handle -> IO a)
  -> IO a
withHandle HandleConfig { endpoint, reconnectSettings, handlers } callback =
  withManagedBus endpoint reconnectSettings $ \bus ->
    callback (Handle bus handlers)

-- | Send a request and receive the response (a single message).
--
-- /Throws/: If another prior thread crashed while using this socket, throws
-- 'Control.Exception.BlockedIndefinitelyOnMVar'.
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
exchange ::
     Handle -- ^
  -> Request -- ^
  -> IO (Either ConnectError Response)
exchange Handle { bus } =
  ManagedBus.exchange bus


-- | Send a request and stream the response (one or more messages).
--
-- /Throws/: If response decoding fails, throws 'DecodeError'.
stream ::
     ∀ r x.
     Handle -- ^
  -> Request -- ^
  -> x
  -> (x -> Response -> IO (Either x r))
  -> IO (Either ConnectError r)
stream Handle { bus } =
  ManagedBus.stream bus
