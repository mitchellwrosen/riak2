{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Riak
  ( Handle
  , withHandle
  , ping
  ) where

import Control.Monad.IO.Unlift
import Data.Text               (Text)
import Network.Socket          (HostName, PortNumber)

import qualified Data.ProtoLens.Encoding as Proto

import Riak.Internal.Connection
import Riak.Internal.Message
import Riak.Internal.Panic
import Riak.Internal.Protobuf   (RpbErrorResp)

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle !Connection

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> (Handle -> m a)
  -> m a
withHandle host port f =
  withConnection host port (f . Handle)

ping :: MonadIO m => Handle -> m (Either RpbErrorResp ())
ping (Handle conn) = liftIO $ do
  send conn (Message CodeRpbPingReq mempty)

  recv conn >>= \case
    Message CodeRpbPingResp _ ->
      pure (Right ())

    Message CodeRpbErrorResp bytes ->
      case Proto.decodeMessage bytes of
        Left err ->
          panic "Protobuf decoding failure"
            ( ("expected", "RpbErrorResp" :: Text)
            , ("error", err)
            )

        Right resp ->
          pure (Left resp)

    Message code _bytes ->
      panic "Unexpected message code"
        ( ("expected", CodeRpbPingResp)
        , ("actual", code)
        )
