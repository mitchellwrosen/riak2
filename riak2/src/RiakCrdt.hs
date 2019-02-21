-- | Functionality common to multiple CRDTs.

module RiakCrdt where

import RiakError

import Data.Text.Encoding (decodeUtf8)


parseGetCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Maybe (Error 'GetCrdtOp)
parseGetCrdtError bucketType err
  | isBucketTypeDoesNotExistError1 err =
      Just (BucketTypeDoesNotExistError bucketType)
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))

parseUpdateCrdtError ::
     ByteString -- ^ Bucket type
  -> ByteString -- ^ Error
  -> Maybe (Error 'UpdateCrdtOp)
parseUpdateCrdtError bucketType err
  | isBucketTypeDoesNotExistError1 err =
      Just (BucketTypeDoesNotExistError bucketType)
  | isUnknownMessageCode err =
      Nothing
  | otherwise =
      Just (UnknownError (decodeUtf8 err))
