-- | Functionality common to multiple CRDTs.

module RiakCrdt where

import RiakError

import Data.Text.Encoding (decodeUtf8)


parseGetCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Error 'GetCrdtOp
parseGetCrdtError bucketType err
  | isBucketTypeDoesNotExistError1 err =
      BucketTypeDoesNotExistError bucketType
  | otherwise =
      UnknownError (decodeUtf8 err)
