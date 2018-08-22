module Riak.Internal
  ( Connection
  , withConnection
  , deleteObject
  , fetchDataType
  , fetchObject
  , getBucketProps
  , getBucketTypeProps
  , getIndex
  , storeObject
  , updateDataType
  ) where

import Proto.Riak
import Riak.Internal.Connection
import Riak.Internal.Response

deleteObject
  :: Connection
  -> RpbDelReq
  -> IO (Either RpbErrorResp RpbDelResp)
deleteObject =
  exchange

fetchDataType
  :: Connection
  -> DtFetchReq
  -> IO (Either RpbErrorResp DtFetchResp)
fetchDataType
  = exchange

fetchObject
  :: Connection
  -> RpbGetReq
  -> IO (Either RpbErrorResp RpbGetResp)
fetchObject =
  exchange

getBucketProps
  :: Connection
  -> RpbGetBucketReq
  -> IO (Either RpbErrorResp RpbGetBucketResp)
getBucketProps =
  exchange

getBucketTypeProps
  :: Connection
  -> RpbGetBucketTypeReq
  -> IO (Either RpbErrorResp RpbGetBucketResp)
getBucketTypeProps =
  exchange

getIndex
  :: Connection
  -> RpbYokozunaIndexGetReq
  -> IO (Either RpbErrorResp RpbYokozunaIndexGetResp)
getIndex =
  exchange

storeObject
  :: Connection
  -> RpbPutReq
  -> IO (Either RpbErrorResp RpbPutResp)
storeObject =
  exchange

updateDataType
  :: Connection
  -> DtUpdateReq
  -> IO (Either RpbErrorResp DtUpdateResp)
updateDataType =
  exchange
