module RiakSetBucketProps
  ( SetBucketProps(..)
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


data SetBucketProps
  = SetBucketProps
  { index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , notfoundBehavior :: NotfoundBehavior
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precommitHooks :: [Proto.RpbCommitHook]
  , readQuorum :: ReadQuorum
  , writeQuorum :: WriteQuorum
  } deriving stock (Eq, Generic, Show)

-- | Parse from bucket props. Does not check that datatype is "set".
fromProto :: Proto.RpbBucketProps -> SetBucketProps
fromProto props =
  SetBucketProps
    { index            = IndexName.fromBucketProps props
    , nodes            = fromIntegral (props ^. Proto.nVal)
    , notfoundBehavior = NotfoundBehavior.fromProto props
    , postcommitHooks  = props ^. Proto.postcommit
    , precommitHooks   = props ^. Proto.precommit
    , readQuorum       = ReadQuorum.fromProto props
    , writeQuorum      = WriteQuorum.fromProto props
    }

maybeFromProto :: Proto.RpbBucketProps -> Maybe SetBucketProps
maybeFromProto props = do
  "set" <- props ^. Proto.maybe'datatype
  pure (fromProto props)
