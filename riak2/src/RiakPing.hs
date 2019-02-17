module RiakPing
  ( ping
  ) where

import Libriak.Handle (Handle)

import qualified Libriak.Handle as Handle

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString)
import Data.Default.Class     (def)


-- | Ping the server.
ping ::
     MonadIO m
  => Handle -- ^
  -> m (Either Handle.HandleError (Either ByteString ()))
ping handle =
  liftIO (Handle.ping handle)

