module Riak.Internal.RangeQuery where

import Riak.Bucket              (Bucket)
import Riak.Internal.IndexValue (IndexValue)
import Riak.Internal.Prelude

import qualified Riak.Internal.IndexValue as IndexValue

-- | A range query on a secondary index.
data RangeQuery a
  = RangeQuery
  { bucket :: !Bucket
  , index :: !ByteString
  , min :: !(IndexValue a)
  , max :: !(IndexValue a)
  }

deriving stock instance Show (RangeQuery a)

name :: RangeQuery a -> ByteString
name (RangeQuery { index, min }) =
  case min of
    IndexValue.Binary{}  -> index <> "_bin"
    IndexValue.Integer{} -> index <> "_int"
