-- | Functionality common to multiple CRDTs.

module Riak.Internal.Crdt where

import Riak.Internal.Error
import Riak.Internal.Prelude

import qualified Libriak.Handle as Handle

import Data.Text.Encoding (decodeUtf8)


parseGetCrdtError ::
     ByteString -- Bucket type
  -> Handle.Error
  -> Error 'GetCrdtOp
parseGetCrdtError bucketType = \case
  Handle.ErrorHandle err ->
    HandleError err

  Handle.ErrorRiak err
    | isCrdtBucketTypeDoesNotExistError err ->
        BucketTypeDoesNotExistError bucketType
    | otherwise ->
        UnknownError (decodeUtf8 err)

parseUpdateCrdtError ::
     ByteString -- Bucket type
  -> Handle.Error
  -> Error 'UpdateCrdtOp
parseUpdateCrdtError bucketType = \case
  Handle.ErrorHandle err ->
    HandleError err

  Handle.ErrorRiak err
    | isCrdtBucketTypeDoesNotExistError err ->
        BucketTypeDoesNotExistError bucketType
    | otherwise ->
        UnknownError (decodeUtf8 err)
