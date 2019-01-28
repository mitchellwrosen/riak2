module Riak.Key
  ( -- * Key
    Key(..)
  , none
    -- ** Search
    -- TODO Move exact/range query to Riak.Bucket
  , exactQuery
  , rangeQuery
  ) where

import Riak.Bucket              (Bucket(..))
import Riak.Internal.Client     (Client, Result)
import Riak.Internal.ExactQuery (ExactQuery(..))
import Riak.Internal.Key        (Key(..))
import Riak.Internal.Prelude
import Riak.Internal.RangeQuery (RangeQuery)
import Riak.Internal.Utils      (bs2int)
import Riak.Request             (Request(..))
import Riak.Response            (Response(..))

import qualified Riak.Internal.Client     as Client
import qualified Riak.Internal.ExactQuery as ExactQuery
import qualified Riak.Internal.IndexValue as IndexValue
import qualified Riak.Internal.RangeQuery as RangeQuery
import qualified Riak.Proto               as Proto
import qualified Riak.Proto.Lens          as L

import Control.Foldl (FoldM(..))
import Control.Lens  (folded, to)

import qualified Control.Foldl   as Foldl
import qualified Data.ByteString as ByteString


-- | Use 'none' to ask Riak to generate a random key when writing a new object
-- or data type:
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

-- | Perform an exact query on a secondary index.
--
-- TODO exact query pagination
exactQuery
  :: Client -- ^
  -> ExactQuery -- ^
  -> FoldM IO Key r -- ^
  -> IO (Result r)
exactQuery client query@(ExactQuery { value }) keyFold =
  doIndex
    client
    request
    (Foldl.handlesM (L.keys . folded . to (Key type' bucket)) keyFold)

  where
    Bucket type' bucket =
      ExactQuery.bucket query

    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~ ExactQuery.name query
        & L.key .~ IndexValue.encode value
        & L.qtype .~ Proto.IndexRequest'exact
        & L.stream .~ True
        & L.type' .~ type'

-- | Perform a range query on a secondary index.
--
-- TODO range query pagination
rangeQuery
  :: forall a r.
     Client -- ^
  -> RangeQuery a -- ^
  -> FoldM IO (a, Key) r -- ^
  -> IO (Result r)
rangeQuery client query keyFold =
  doIndex
    client
    request
    (Foldl.handlesM (L.results . folded . to fromResult) keyFold)

  where
    Bucket type' bucket =
      RangeQuery.bucket query

    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~ RangeQuery.name query
        & L.qtype .~ Proto.IndexRequest'range
        & L.rangeMax .~ IndexValue.encode (RangeQuery.max query)
        & L.rangeMax .~ IndexValue.encode (RangeQuery.min query)
        & L.returnTerms .~ True
        & L.stream .~ True
        & L.type' .~ type'

    fromResult :: Proto.Pair -> (a, Key)
    fromResult pair =
      ( case RangeQuery.min query of
          IndexValue.Binary{}  -> pair ^. L.key
          IndexValue.Integer{} -> bs2int (pair ^. L.key)
      , Key type' bucket (pair ^. L.value)
      )

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
