-- | Functionality common to multiple CRDTs.

module RiakCrdt where

import RiakBucketType (BucketType)
import RiakError

import Data.Text.Encoding (decodeUtf8)


parseGetCrdtError ::
     BucketType
  -> ByteString
  -> Error 'GetCrdtOp
parseGetCrdtError bucketType err
  | isBucketTypeDoesNotExistError1 err =
      BucketTypeDoesNotExistError bucketType
  | isInvalidNodesError0 err =
      InvalidNodesError
  | otherwise =
      UnknownError (decodeUtf8 err)
