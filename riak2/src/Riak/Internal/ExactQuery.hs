module Riak.Internal.ExactQuery where

import Riak.Bucket           (Bucket(..))
import Riak.IndexValue       (IndexValue)
import Riak.Internal.Prelude

import qualified Riak.IndexValue as IndexValue

-- | An exact query on a secondary index.
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

inBucket :: Bucket -> ExactQuery
inBucket bucket@(Bucket _ b) =
  ExactQuery
    { bucket = bucket
    , index = "$bucket"
    , value = IndexValue.Binary b
    }
