module RiakPing
  ( ping
  ) where

import RiakHandle (Handle, HandleError)

import qualified RiakHandle as Handle

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString        (ByteString)


-- | Ping the server.
ping ::
     MonadIO m
  => Handle -- ^
  -> m (Either HandleError (Either ByteString ()))
ping handle = liftIO $
  (fmap.fmap) (() <$)
    (Handle.ping handle)
