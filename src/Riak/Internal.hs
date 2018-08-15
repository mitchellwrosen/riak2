module Riak.Internal
  ( fetchObject
  , storeObject
  ) where

import Proto.Riak
import Riak.Internal.Connection

fetchObject
  :: Connection
  -> RpbGetReq
  -> IO (Either RpbErrorResp RpbGetResp)
fetchObject =
  exchange

storeObject
  :: Connection
  -> RpbPutReq
  -> IO (Either RpbErrorResp RpbPutResp)
storeObject =
  exchange
