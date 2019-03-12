module RiakBucketProps where

import RiakIndexName        (IndexName)
import RiakNotfoundBehavior (NotfoundBehavior)
import RiakReadQuorum       (ReadQuorum)
import RiakWriteQuorum      (WriteQuorum)

import qualified RiakIndexName        as IndexName
import qualified RiakNotfoundBehavior as NotfoundBehavior
import qualified RiakReadQuorum       as ReadQuorum
import qualified RiakWriteQuorum      as WriteQuorum

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)
import Data.Time          (NominalDiffTime, secondsToNominalDiffTime)

import qualified Data.Riak.Proto as Proto

-- TODO bucket props writeOnce, ttl

data BucketProps
  = BucketProps
  { backend :: Maybe Text
  , conflictResolution :: ConflictResolution
  , index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , notfoundBehavior :: NotfoundBehavior
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precommitHooks :: [Proto.RpbCommitHook]
  , pruneContextSettings :: PruneContextSettings
  , readQuorum :: ReadQuorum
  , ttl :: Maybe Word32
  , writeOnce :: Bool
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

-- | To keep causal context storage requirements from growing arbitrarily large,
-- Riak prunes causal context entries using the following algorithm:
--
-- If the the causal context has /more than/ @minLength@ entries __and__ the
-- oldest entry is /older than/ @minAge@, then if there are /more tha/
-- @maxLength@ entries __or__ the oldest entry is /older than/ @maxAge@, prune
-- the oldest entry and repeat.
data PruneContextSettings
  = PruneContextSettings
  { minAge :: NominalDiffTime
  , maxAge :: NominalDiffTime
  , minLength :: Natural
  , maxLength :: Natural
  } deriving stock (Eq, Generic, Show)

-- | Parse from bucket props. Does not check that datatype is nothing.
fromProto :: Proto.RpbBucketProps -> BucketProps
fromProto props =
  BucketProps
    { backend              = decodeUtf8 <$> (props ^. Proto.maybe'backend)
    , conflictResolution   = conflictResolution
    , index                = IndexName.fromBucketProps props
    , nodes                = fromIntegral (props ^. Proto.nVal)
    , notfoundBehavior     = NotfoundBehavior.fromProto props
    , postcommitHooks      = props ^. Proto.postcommit
    , precommitHooks       = props ^. Proto.precommit
    , pruneContextSettings = pruneContextSettings
    , readQuorum           = ReadQuorum.fromProto props
    , ttl                  = props ^. Proto.maybe'ttl
    , writeOnce            = props ^. Proto.writeOnce
    , writeQuorum          = WriteQuorum.fromProto props
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

    pruneContextSettings :: PruneContextSettings
    pruneContextSettings =
      PruneContextSettings
        { minAge = secondsToNominalDiffTime (fromIntegral (props ^. Proto.youngVclock))
        , maxAge = secondsToNominalDiffTime (fromIntegral (props ^. Proto.oldVclock))
        , minLength = fromIntegral (props ^. Proto.smallVclock)
        , maxLength = fromIntegral (props ^. Proto.bigVclock)
        }
