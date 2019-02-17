-- | Functionality common to multiple CRDTs.

module RiakCrdt where

import RiakError

import Data.Text.Encoding (decodeUtf8)


parseGetCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Either (Error 'GetCrdtOp) a
parseGetCrdtError bucketType err
  | isBucketTypeDoesNotExistError1 err =
      Left (BucketTypeDoesNotExistError bucketType)
  | otherwise =
      Left (UnknownError (decodeUtf8 err))

parseUpdateCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Either (Error 'UpdateCrdtOp) a
parseUpdateCrdtError bucketType err
  | isBucketTypeDoesNotExistError1 err =
      Left (BucketTypeDoesNotExistError bucketType)
  | otherwise =
      Left (UnknownError (decodeUtf8 err))
