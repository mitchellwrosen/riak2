module Riak.Internal.RangeQuery where

import Riak.Internal.Bucket              (Bucket)
import Riak.Internal.Prelude
import Riak.Internal.SecondaryIndexValue (SecondaryIndexValue)

import qualified Riak.Internal.SecondaryIndexValue as SecondaryIndexValue

-- | A range query on a secondary index.
--
-- /See also/: @'Riak.Bucket.rangeQuery'@, @'Riak.ExactQuery.ExactQuery'@,
data RangeQuery a
  = RangeQuery
  { bucket :: !Bucket
  , index :: !ByteString
  , min :: !(SecondaryIndexValue a)
  , max :: !(SecondaryIndexValue a)
  }

deriving stock instance Show (RangeQuery a)

name :: RangeQuery a -> ByteString
name (RangeQuery { index, min }) =
  case min of
    SecondaryIndexValue.Binary{}  -> index <> "_bin"
    SecondaryIndexValue.Integer{} -> index <> "_int"

-- | Build a query on the built-in index @\"\$key\"@, which indexes each object
-- by its key.
keysBetween ::
     Bucket -- ^
  -> SecondaryIndexValue ByteString -- ^
  -> SecondaryIndexValue ByteString -- ^
  -> RangeQuery ByteString
keysBetween bucket min max =
  RangeQuery
    { index = "$key"
    , ..
    }
