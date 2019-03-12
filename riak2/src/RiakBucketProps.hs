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
  { conflictResolution :: ConflictResolution
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
-- * 'ClientSideConflictResolution' means, in the presence of concurrent updates
--   (or updates lacking a causal context), Riak will create siblings that will
--   have to be collapsed into one value outside of Riak. This strategy is
--   highly recommended for most mutable data.
--
-- * 'TimestampBasedConflictResolution' uses objects' internal timestamps to
--    resolve conflicts. Timestamps are inherently unreliable; this strategy is
--    legacy and, in the author's opinion, less useful than
--    'LastWriteWinsConflictResolution' in all cases. Do not use it.
--
-- * 'LastWriteWinsConflictResolution', means Riak will always overwrite any
--   existing data, no matter what its internal timestamp is. This strategy is
--   useful for immutable data.
data ConflictResolution
  = ClientSideConflictResolution
  | TimestampBasedConflictResolution
  | LastWriteWinsConflictResolution
  deriving stock (Eq, Show)

-- | Parse from bucket props. Does not check that datatype is nothing.
fromProto :: Proto.RpbBucketProps -> BucketProps
fromProto props =
  BucketProps
    { conflictResolution = conflictResolution
    , index              = IndexName.fromBucketProps props
    , nodes              = fromIntegral (props ^. Proto.nVal)
    , notfoundBehavior   = NotfoundBehavior.fromProto props
    , postcommitHooks    = props ^. Proto.postcommit
    , precommitHooks     = props ^. Proto.precommit
    , readQuorum         = ReadQuorum.fromProto props
    , writeQuorum        = WriteQuorum.fromProto props
    }

  where
    conflictResolution :: ConflictResolution
    conflictResolution =
      if props ^. Proto.allowMult then
        ClientSideConflictResolution
      else if props ^. Proto.lastWriteWins then
        LastWriteWinsConflictResolution
      else
        TimestampBasedConflictResolution
