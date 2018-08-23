module Riak.Internal
  ( RiakConnection
  , withRiakConnection
  , deleteRiakObject
  , fetchRiakDataType
  , fetchRiakObject
  , getRiakBucketProps
  , getRiakBucketTypeProps
  , getRiakIndex
  , storeRiakObject
  , updateRiakDataType
  ) where

import Proto.Riak
import Riak.Internal.Connection
import Riak.Internal.Response

deleteRiakObject
  :: RiakConnection
  -> RpbDelReq
  -> IO (Either RpbErrorResp RpbDelResp)
deleteRiakObject =
  riakExchange

fetchRiakDataType
  :: RiakConnection
  -> DtFetchReq
  -> IO (Either RpbErrorResp DtFetchResp)
fetchRiakDataType
  = riakExchange

fetchRiakObject
  :: RiakConnection
  -> RpbGetReq
  -> IO (Either RpbErrorResp RpbGetResp)
fetchRiakObject =
  riakExchange

getRiakBucketProps
  :: RiakConnection
  -> RpbGetBucketReq
  -> IO (Either RpbErrorResp RpbGetBucketResp)
getRiakBucketProps =
  riakExchange

getRiakBucketTypeProps
  :: RiakConnection
  -> RpbGetBucketTypeReq
  -> IO (Either RpbErrorResp RpbGetBucketResp)
getRiakBucketTypeProps =
  riakExchange

getRiakIndex
  :: RiakConnection
  -> RpbYokozunaIndexGetReq
  -> IO (Either RpbErrorResp RpbYokozunaIndexGetResp)
getRiakIndex =
  riakExchange

storeRiakObject
  :: RiakConnection
  -> RpbPutReq
  -> IO (Either RpbErrorResp RpbPutResp)
storeRiakObject =
  riakExchange

updateRiakDataType
  :: RiakConnection
  -> DtUpdateReq
  -> IO (Either RpbErrorResp DtUpdateResp)
updateRiakDataType =
  riakExchange
