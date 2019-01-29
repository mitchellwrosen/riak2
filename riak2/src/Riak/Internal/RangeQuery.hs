module Riak.Internal.RangeQuery where

import Riak.Internal.Bucket     (Bucket)
import Riak.Internal.IndexValue (IndexValue)
import Riak.Internal.Prelude

import qualified Riak.Internal.IndexValue as IndexValue

-- | A range query on a secondary index.
--
-- /See also/: @'Riak.Bucket.rangeQuery'@, @'Riak.ExactQuery.ExactQuery'@,
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

-- | Build a query on the built-in index @\"\$key\"@, which indexes each object
-- by its key.
keysBetween ::
     Bucket -- ^
  -> IndexValue ByteString -- ^
  -> IndexValue ByteString -- ^
  -> RangeQuery ByteString
keysBetween bucket min max =
  RangeQuery
    { index = "$key"
    , ..
    }
