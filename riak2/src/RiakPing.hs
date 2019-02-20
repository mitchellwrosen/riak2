module RiakPing
  ( ping
  ) where

import Libriak.Handle (Handle)

import qualified Libriak.Handle as Handle

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString)


-- | Ping the server.
ping ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.HandleConnectionError (Either ByteString ()))
ping handle =
  liftIO (Handle.ping handle)
