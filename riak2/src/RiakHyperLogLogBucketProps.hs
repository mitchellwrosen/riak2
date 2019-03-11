module RiakHyperLogLogBucketProps
  ( HyperLogLogBucketProps(..)
  , fromProto
  , maybeFromProto
  ) where

import RiakIndexName        (IndexName)
import RiakNotfoundBehavior (NotfoundBehavior)
import RiakReadQuorum       (ReadQuorum)
import RiakWriteQuorum      (WriteQuorum)

import qualified RiakIndexName        as IndexName
import qualified RiakNotfoundBehavior as NotfoundBehavior
import qualified RiakReadQuorum       as ReadQuorum
import qualified RiakWriteQuorum      as WriteQuorum

import Control.Lens ((^.))

import qualified Data.Riak.Proto as Proto


-- TODO hll precision
data HyperLogLogBucketProps
  = HyperLogLogBucketProps
  { index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , notfoundBehavior :: NotfoundBehavior
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precommitHooks :: [Proto.RpbCommitHook]
  , readQuorum :: ReadQuorum
  , writeQuorum :: WriteQuorum
  } deriving stock (Eq, Generic, Show)

-- | Parse from bucket props. Does not check that datatype is "hll".
fromProto :: Proto.RpbBucketProps -> HyperLogLogBucketProps
fromProto props =
  HyperLogLogBucketProps
    { index            = IndexName.fromBucketProps props
    , nodes            = fromIntegral (props ^. Proto.nVal)
    , notfoundBehavior = NotfoundBehavior.fromProto props
    , postcommitHooks  = props ^. Proto.postcommit
    , precommitHooks   = props ^. Proto.precommit
    , readQuorum       = ReadQuorum.fromProto props
    , writeQuorum      = WriteQuorum.fromProto props
    }

maybeFromProto :: Proto.RpbBucketProps -> Maybe HyperLogLogBucketProps
maybeFromProto props = do
  "hll" <- props ^. Proto.maybe'datatype
  pure (fromProto props)
