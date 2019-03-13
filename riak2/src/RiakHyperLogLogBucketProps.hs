module RiakHyperLogLogBucketProps
  ( HyperLogLogBucketProps(..)
  , fromProto
  ) where

import RiakIndexName   (IndexName)
import RiakReadQuorum  (ReadQuorum)
import RiakWriteQuorum (WriteQuorum)

import qualified RiakIndexName   as IndexName
import qualified RiakReadQuorum  as ReadQuorum
import qualified RiakWriteQuorum as WriteQuorum

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto


data HyperLogLogBucketProps
  = HyperLogLogBucketProps
  { backend :: Maybe Text
  , index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precision :: Natural -- ^ @4-16@, inclusive
  , precommitHooks :: [Proto.RpbCommitHook]
  , readQuorum :: ReadQuorum
  , writeQuorum :: WriteQuorum
  } deriving stock (Eq, Generic, Show)

-- | Parse from bucket props. Does not check that datatype is "hll".
fromProto :: Proto.RpbBucketProps -> HyperLogLogBucketProps
fromProto props =
  HyperLogLogBucketProps
    { backend          = decodeUtf8 <$> (props ^. Proto.maybe'backend)
    , index            = IndexName.fromBucketProps props
    , nodes            = fromIntegral (props ^. Proto.nVal)
    , postcommitHooks  = props ^. Proto.postcommit
    , precision        = fromIntegral (props ^. Proto.hllPrecision)
    , precommitHooks   = props ^. Proto.precommit
    , readQuorum       = ReadQuorum.fromProto props
    , writeQuorum      = WriteQuorum.fromProto props
    }
