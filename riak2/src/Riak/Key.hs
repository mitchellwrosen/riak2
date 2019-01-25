module Riak.Key
  ( -- * Key
    Key(..)
  , none
    -- ** Search
  , exactQuery
    -- ** List
  , stream
  , list
  ) where

import Riak.Bucket           (Bucket(..))
import Riak.ExactQuery       (ExactQuery)
import Riak.Internal.Client  (Client, Result)
import Riak.Internal.Prelude
import Riak.Internal.Utils   (int2bs)
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

import qualified Riak.ExactQuery      as ExactQuery
import qualified Riak.Internal.Client as Client
import qualified Riak.Proto           as Proto
import qualified Riak.Proto.Lens      as L

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
  Client.stream
    client
    (RequestIndex request)
    (\case
      ResponseIndex response -> Just response
      _ -> Nothing)
    (view L.done)
    (makeIndexFold (Key type' bucket) keyFold)

  where
    request :: Proto.IndexRequest
    request =
      defMessage
        & L.bucket .~ bucket
        & L.index .~
            case query of
              ExactQuery.Binary  name _ -> name <> "_bin"
              ExactQuery.Integer name _ -> name <> "_int"
        & L.key .~
            case query of
              ExactQuery.Binary  _ value -> value
              ExactQuery.Integer _ value -> int2bs value
        & L.qtype .~ Proto.IndexRequest'eq
        & L.stream .~ True
        & L.type' .~ type'

makeIndexFold ::
     Monad m
  => (ByteString -> Key)
  -> FoldM m Key r
  -> FoldM m Proto.IndexResponse r
makeIndexFold toKey =
  Foldl.handlesM (L.keys . folded . to toKey)

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
