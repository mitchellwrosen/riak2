-- | Functionality common to multiple CRDTs.

module Riak.Internal.Crdt where

import Riak.Internal.Error
import Riak.Internal.Prelude

import Data.Text.Encoding (decodeUtf8)


parseGetCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Either (Error 'GetCrdtOp) a
parseGetCrdtError bucketType err
  | isBucketTypeDoesNotExistError_Crdt err =
      Left (BucketTypeDoesNotExistError bucketType)
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

parseUpdateCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Either (Error 'UpdateCrdtOp) a
parseUpdateCrdtError bucketType err
  | isBucketTypeDoesNotExistError_Crdt err =
      Left (BucketTypeDoesNotExistError bucketType)
  | otherwise =
      Left (UnknownError (decodeUtf8 err))
