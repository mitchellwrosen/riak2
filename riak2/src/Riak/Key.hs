module Riak.Key
  ( Key(..)
  , none
  , stream
  , list
  ) where

import Riak.Bucket           (Bucket(..))
import Riak.Internal.Client  (Client, Result)
import Riak.Internal.Prelude
import Riak.Request          (Request(..))
import Riak.Response         (Response(..))

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
    (makeResponseFold (Key type' bucket) keyFold)

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

makeResponseFold ::
     Monad m
  => (ByteString -> Key)
  -> FoldM m Key r
  -> FoldM m Proto.StreamKeysResponse r
makeResponseFold toKey =
  Foldl.handlesM (L.keys . folded . to toKey)
