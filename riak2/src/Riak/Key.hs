module Riak.Key
  ( -- * Key
    Key(..)
  , none
    -- ** Search
  , exactQuery
  , rangeQuery
    -- ** List
  , stream
  , list
  ) where

import Riak.Bucket              (Bucket(..))
import Riak.Internal.Client     (Client, Result)
import Riak.Internal.ExactQuery (ExactQuery)
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Internal.Client     as Client
import qualified Riak.Internal.ExactQuery as ExactQuery
import qualified Riak.Internal.RangeQuery as RangeQuery
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

import Control.Foldl (FoldM(..))
import Control.Lens  (folded, to)

import qualified Control.Foldl   as Foldl
import qualified Data.ByteString as ByteString


-- | A bucket type, bucket, and key.
--
-- /Note/: The bucket type must be UTF-8 encoded.
data Key
  = Key
  { type' :: !ByteString
  , bucket :: !ByteString
  , key :: !ByteString
  } deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

-- Use 'none' to ask Riak to generate a random key when writing a new object or
-- data type:
--
-- @
-- Key
--   { type' = ...
--   , bucket = ...
--   , key = Riak.Key.none
--   }
-- @
none :: ByteString
none =
  ByteString.empty

-- TODO exact query pagination
exactQuery
  :: Client -- ^
  -> Bucket -- ^
  -> ExactQuery -- ^
  -> FoldM IO Key r -- ^
  -> IO (Result r)
exactQuery client (Bucket type' bucket) query keyFold =
  doIndex client request (makeIndexFold (Key type' bucket) keyFold)

  where
    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~ ExactQuery.name query
        & L.key .~ ExactQuery.value query
        & L.qtype .~ Proto.IndexRequest'eq
        & L.stream .~ True
        & L.type' .~ type'

-- TODO range query pagination
rangeQuery
  :: Client -- ^
  -> Bucket -- ^
  -> RangeQuery a -- ^
  -> FoldM IO Key r -- ^
  -> IO (Result r)
rangeQuery client (Bucket type' bucket) query keyFold =
  doIndex client request (makeIndexFold (Key type' bucket) keyFold)

  where
    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~ RangeQuery.name query
        & L.qtype .~ Proto.IndexRequest'range
        & L.rangeMax .~ RangeQuery.maxValue query
        & L.rangeMax .~ RangeQuery.minValue query
        & L.stream .~ True
        & L.type' .~ type'

makeIndexFold ::
     Monad m
  => (ByteString -> Key)
  -> FoldM m Key r
  -> FoldM m Proto.IndexResponse r
makeIndexFold toKey =
  Foldl.handlesM (L.keys . folded . to toKey)

doIndex ::
     Client
  -> Proto.IndexRequest
  -> FoldM IO Proto.IndexResponse r
  -> IO (Result r)
doIndex client request =
  Client.stream
    client
    (RequestIndex request)
    (\case
      ResponseIndex response -> Just response
      _ -> Nothing)
    (view L.done)

stream
  :: Client -- ^
  -> Bucket -- ^
  -> FoldM IO Key r -- ^
  -> IO (Result r)
stream client (Bucket type' bucket) keyFold =
  Client.stream
    client
    (RequestStreamKeys request)
    (\case
      ResponseStreamKeys response -> Just response
      _ -> Nothing)
    (view L.done)
    (makeStreamKeysFold (Key type' bucket) keyFold)

  where
    request :: Proto.StreamKeysRequest
    request =
      defMessage
        & L.type' .~ type'
        & L.bucket .~ bucket
        -- TODO stream keys timeout

list
  :: Client -- ^
  -> Bucket -- ^
  -> IO (Result [Key])
list client type' =
  stream client type' (Foldl.generalize Foldl.list)

makeStreamKeysFold ::
     Monad m
  => (ByteString -> Key)
  -> FoldM m Key r
  -> FoldM m Proto.StreamKeysResponse r
makeStreamKeysFold toKey =
  Foldl.handlesM (L.keys . folded . to toKey)
