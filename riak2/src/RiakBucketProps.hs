module RiakBucketProps where

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

-- TODO bucket props oldVclock, youngVclock, bigVclock, smallVclock, backend, repl, consistent, writeOnce, ttl

data BucketProps
  = BucketProps
  { conflictResolution :: Maybe ConflictResolution
  , index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , notfoundBehavior :: NotfoundBehavior
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precommitHooks :: [Proto.RpbCommitHook]
  , readQuorum :: ReadQuorum
  , writeQuorum :: WriteQuorum
  } deriving stock (Eq, Generic, Show)

-- | The conflict resolution strategy used by Riak, for normal KV (non-CRDT)
-- objects.
--
-- Allowing Riak to create siblings is highly recommended, but there are two
-- built-in strategies to resolve conflicts in Riak itself.
data ConflictResolution
  = UseTimestamps
  | LastWriteWins
  deriving stock (Eq, Show)

-- | Parse from bucket props. Does not check that datatype is nothing.
fromProto :: Proto.RpbBucketProps -> BucketProps
fromProto props =
  BucketProps
    { conflictResolution = do
        guard (not (props ^. Proto.allowMult))
        if props ^. Proto.lastWriteWins
          then Just LastWriteWins
          else Just UseTimestamps
    , index              = IndexName.fromBucketProps props
    , nodes              = fromIntegral (props ^. Proto.nVal)
    , notfoundBehavior   = NotfoundBehavior.fromProto props
    , postcommitHooks    = props ^. Proto.postcommit
    , precommitHooks     = props ^. Proto.precommit
    , readQuorum         = ReadQuorum.fromProto props
    , writeQuorum        = WriteQuorum.fromProto props
    }
