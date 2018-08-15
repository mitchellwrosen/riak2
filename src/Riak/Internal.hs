module Riak.Internal where

import Proto.Riak
import Riak.Internal.Connection

fetchObject
  :: Connection
  -> RpbGetReq
  -> IO (Either RpbErrorResp RpbGetResp)
fetchObject =
  exchange
