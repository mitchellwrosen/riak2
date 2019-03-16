module RiakBucketProps where

import RiakIndexName   (IndexName)
import RiakReadQuorum  (ReadQuorum)
import RiakWriteQuorum (WriteQuorum)

import qualified RiakIndexName   as IndexName
import qualified RiakReadQuorum  as ReadQuorum
import qualified RiakWriteQuorum as WriteQuorum

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)
import Data.Time          (NominalDiffTime, secondsToNominalDiffTime)

import qualified Data.Riak.Proto as Proto

-- | Object bucket properties.
data BucketProps
  = BucketProps
  { backend :: Maybe Text
  , conflictResolution :: ConflictResolution
  , index :: Maybe IndexName -- ^ Search index
  , nodes :: Natural
  , postcommitHooks :: [Proto.RpbCommitHook]
  , precommitHooks :: [Proto.RpbCommitHook]
  , pruneContextSettings :: PruneContextSettings
  , readQuorum :: ReadQuorum
  , writeOnce :: Bool
  , writeQuorum :: WriteQuorum
  } deriving stock (Eq, Generic, Show)

-- | The conflict resolution strategy used by Riak, for normal KV (non-CRDT)
-- objects.
--
-- +------------------------------------+--------------------------------------+
-- | 'ClientSideConflictResolution'     | In the presence of concurrent        |
-- |                                    | updates (or updates lacking a causal |
-- |                                    | context), Riak will create siblings  |
-- |                                    | that will have have to be collapsed  |
-- |                                    | into one value outside of Riak. This |
-- |                                    | strategy is highly recommended for   |
-- |                                    | most cases.                          |
-- +------------------------------------+--------------------------------------+
-- | 'TimestampBasedConflictResolution' | Uses objects' internal timestamps to |
-- |                                    | resolve conflicts. Timestamps are    |
-- |                                    | inherently unreliable; this strategy |
-- |                                    | is legacy and, in the author's       |
-- |                                    | opinion, less useful than            |
-- |                                    | last-write-wins in all cases. Do not |
-- |                                    | use it.                              |
-- +------------------------------------+--------------------------------------+
-- | 'LastWriteWinsConflictResolution'  | Riak will always overwrite any       |
-- |                                    | existing data, no matter what its    |
-- |                                    | internal timestamp is. This strategy |
-- |                                    | is useful for immutable data.        |
-- +------------------------------------+--------------------------------------+
data ConflictResolution
  = ClientSideConflictResolution
  | TimestampBasedConflictResolution
  | LastWriteWinsConflictResolution
  deriving stock (Eq, Show)

-- | To keep causal context storage requirements from growing arbitrarily large,
-- Riak prunes causal context entries using the following algorithm:
--
-- If conditions @(1)@, @(2)@, and @(3)@ hold, prune the oldest entry and
-- repeat.
--
-- * @(1)@ The the causal context has more than @minLength@ entries
-- * @(2)@ The oldest entry is older than @minAge@
-- * @(3)@ There are more than @maxLength@ entries /or/ the oldest entry is
--   older than @maxAge@
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
    , postcommitHooks      = props ^. Proto.postcommit
    , precommitHooks       = props ^. Proto.precommit
    , pruneContextSettings = pruneContextSettings
    , readQuorum           = ReadQuorum.fromProto props
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
