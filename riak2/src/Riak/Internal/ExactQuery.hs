module Riak.Internal.ExactQuery where

import Riak.Internal.Bucket     (Bucket(..))
import Riak.Internal.Prelude
import Riak.SecondaryIndexValue (SecondaryIndexValue)

import qualified Riak.SecondaryIndexValue as SecondaryIndexValue

-- | An exact query on a secondary index.
--
-- /See also/: @'Riak.Bucket.exactQuery'@, @'Riak.RangeQuery.RangeQuery'@
data ExactQuery
  = forall a.
    ExactQuery
  { bucket :: !Bucket
  , index :: !ByteString
  , value :: !(SecondaryIndexValue a)
  }

name :: ExactQuery -> ByteString
name (ExactQuery { index, value }) =
  case value of
    SecondaryIndexValue.Binary{}  -> index <> "_bin"
    SecondaryIndexValue.Integer{} -> index <> "_int"

-- | Build a query on the built-in index @\"\$bucket\"@, which indexes each
-- object by its bucket.
inBucket ::
     Bucket -- ^
  -> ExactQuery
inBucket bucket@(Bucket _ b) =
  ExactQuery
    { bucket = bucket
    , index = "$bucket"
    , value = SecondaryIndexValue.Binary b
    }
