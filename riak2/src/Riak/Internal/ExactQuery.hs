module Riak.Internal.ExactQuery where

import Riak.IndexValue       (IndexValue)
import Riak.Internal.Bucket  (Bucket(..))
import Riak.Internal.Prelude

import qualified Riak.IndexValue as IndexValue

-- | An exact query on a secondary index.
--
-- /See also/: @'Riak.Bucket.exactQuery'@, @'Riak.RangeQuery.RangeQuery'@
data ExactQuery
  = forall a.
    ExactQuery
  { bucket :: !Bucket
  , index :: !ByteString
  , value :: !(IndexValue a)
  }

name :: ExactQuery -> ByteString
name (ExactQuery { index, value }) =
  case value of
    IndexValue.Binary{}  -> index <> "_bin"
    IndexValue.Integer{} -> index <> "_int"

-- | Build a query on the built-in index @\"\$bucket\"@, which indexes each
-- object by its bucket.
inBucket ::
     Bucket -- ^
  -> ExactQuery
inBucket bucket@(Bucket _ b) =
  ExactQuery
    { bucket = bucket
    , index = "$bucket"
    , value = IndexValue.Binary b
    }
