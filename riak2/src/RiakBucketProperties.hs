module RiakBucketProperties where

import RiakIndexName   (IndexName(..))
import RiakReadQuorum  (ReadQuorum(..))
import RiakWriteQuorum (WriteQuorum(..))

import qualified RiakReadQuorum  as ReadQuorum
import qualified RiakWriteQuorum as WriteQuorum

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Riak.Proto as Proto


data BucketProperties
  = BucketPropertiesCounter CounterBucketProperties
  | BucketPropertiesHyperLogLog HyperLogLogBucketProperties
  | BucketPropertiesMap MapBucketProperties
  | BucketPropertiesObject ObjectBucketProperties
  | BucketPropertiesSet SetBucketProperties
  deriving stock (Show)

data CounterBucketProperties
  = CounterBucketProperties
  { index :: !(Maybe IndexName) -- ^ Search index
  , nodes :: !Natural
  , notfoundBehavior :: !NotfoundBehavior
  , postcommitHooks :: ![Proto.RpbCommitHook]
  , precommitHooks :: ![Proto.RpbCommitHook]
  , readQuorum :: !ReadQuorum
  , writeQuorum :: !WriteQuorum
  } deriving stock (Generic, Show)

-- TODO hll precision
data HyperLogLogBucketProperties
  = HyperLogLogBucketProperties
  { index :: !(Maybe IndexName) -- ^ Search index
  , nodes :: !Natural
  , notfoundBehavior :: !NotfoundBehavior
  , postcommitHooks :: ![Proto.RpbCommitHook]
  , precommitHooks :: ![Proto.RpbCommitHook]
  , readQuorum :: !ReadQuorum
  , writeQuorum :: !WriteQuorum
  } deriving stock (Generic, Show)

data MapBucketProperties
  = MapBucketProperties
  { index :: !(Maybe IndexName) -- ^ Search index
  , nodes :: !Natural
  , notfoundBehavior :: !NotfoundBehavior
  , postcommitHooks :: ![Proto.RpbCommitHook]
  , precommitHooks :: ![Proto.RpbCommitHook]
  , readQuorum :: !ReadQuorum
  , writeQuorum :: !WriteQuorum
  } deriving stock (Generic, Show)

data ObjectBucketProperties
  = ObjectBucketProperties
  { conflictResolution :: !(Maybe ConflictResolution)
  , index :: !(Maybe IndexName) -- ^ Search index
  , nodes :: !Natural
  , notfoundBehavior :: !NotfoundBehavior
  , postcommitHooks :: ![Proto.RpbCommitHook]
  , precommitHooks :: ![Proto.RpbCommitHook]
  , readQuorum :: !ReadQuorum
  , writeQuorum :: !WriteQuorum
  } deriving stock (Generic, Show)

data SetBucketProperties
  = SetBucketProperties
  { index :: !(Maybe IndexName) -- ^ Search index
  , nodes :: !Natural
  , notfoundBehavior :: !NotfoundBehavior
  , postcommitHooks :: ![Proto.RpbCommitHook]
  , precommitHooks :: ![Proto.RpbCommitHook]
  , readQuorum :: !ReadQuorum
  , writeQuorum :: !WriteQuorum
  } deriving stock (Generic, Show)

-- | The conflict resolution strategy used by Riak, for normal KV (non-CRDT)
-- objects.
--
-- Allowing Riak to create siblings is highly recommended, but there are two
-- built-in strategies to resolve conflicts in Riak itself.
data ConflictResolution
  = UseTimestamps
  | LastWriteWins
  deriving stock (Show)

-- | TODO better names for NotfoundBehavior constructors
data NotfoundBehavior
  = NotfoundCounts -- notfound counts towards r
  | NotfoundSkipped -- all n vnodes are queried
  | NotfoundSkippedBasic -- only a quorum of vnodes are queried
  deriving stock (Show)

fromProto :: Proto.RpbBucketProps -> BucketProperties
fromProto props =
  case props ^. Proto.maybe'datatype of
    Just "counter" ->
      BucketPropertiesCounter CounterBucketProperties
        { index            = index
        , nodes            = nodes
        , notfoundBehavior = notfoundBehavior
        , postcommitHooks  = postcommitHooks
        , precommitHooks   = precommitHooks
        , readQuorum       = readQuorum
        , writeQuorum      = writeQuorum
        }

    Just "hll" ->
      BucketPropertiesHyperLogLog HyperLogLogBucketProperties
        { index            = index
        , nodes            = nodes
        , notfoundBehavior = notfoundBehavior
        , postcommitHooks  = postcommitHooks
        , precommitHooks   = precommitHooks
        , readQuorum       = readQuorum
        , writeQuorum      = writeQuorum
        }

    Just "map" ->
      BucketPropertiesMap MapBucketProperties
        { index            = index
        , nodes            = nodes
        , notfoundBehavior = notfoundBehavior
        , postcommitHooks  = postcommitHooks
        , precommitHooks   = precommitHooks
        , readQuorum       = readQuorum
        , writeQuorum      = writeQuorum
        }

    Just "set" ->
      BucketPropertiesSet SetBucketProperties
        { index            = index
        , nodes            = nodes
        , notfoundBehavior = notfoundBehavior
        , postcommitHooks  = postcommitHooks
        , precommitHooks   = precommitHooks
        , readQuorum       = readQuorum
        , writeQuorum      = writeQuorum
        }

    _ ->
      BucketPropertiesObject ObjectBucketProperties
        { conflictResolution = conflictResolution
        , index              = index
        , nodes              = nodes
        , notfoundBehavior   = notfoundBehavior
        , postcommitHooks    = postcommitHooks
        , precommitHooks     = precommitHooks
        , readQuorum         = readQuorum
        , writeQuorum        = writeQuorum
        }

  where
    conflictResolution :: Maybe ConflictResolution
    conflictResolution = do
      guard (not (props ^. Proto.allowMult))
      if props ^. Proto.lastWriteWins
        then Just LastWriteWins
        else Just UseTimestamps

    index :: Maybe IndexName
    index =
      IndexName . decodeUtf8 <$>
        (props ^. Proto.maybe'searchIndex)

    nodes :: Natural
    nodes =
      fromIntegral (props ^. Proto.nVal)

    notfoundBehavior :: NotfoundBehavior
    notfoundBehavior =
      case (fromMaybe True (props ^. Proto.maybe'notfoundOk), props ^. Proto.basicQuorum) of
        (True, _)      -> NotfoundCounts
        (False, False) -> NotfoundSkipped
        (False, True)  -> NotfoundSkippedBasic

    postcommitHooks :: [Proto.RpbCommitHook]
    postcommitHooks =
      props ^. Proto.postcommit

    precommitHooks :: [Proto.RpbCommitHook]
    precommitHooks =
      props ^. Proto.precommit

    readQuorum :: ReadQuorum
    readQuorum =
      ReadQuorum.fromProto props

    writeQuorum :: WriteQuorum
    writeQuorum =
      WriteQuorum.fromProto props

-- _BucketProperties'oldVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'youngVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'bigVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'smallVclock :: !(Prelude.Maybe Data.Word.Word32),
--
-- _BucketProperties'backend :: !(Prelude.Maybe Data.ByteString.ByteString),
-- _BucketProperties'repl :: !(Prelude.Maybe BucketProperties'RpbReplMode),
-- _BucketProperties'consistent :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'writeOnce :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'hllPrecision :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'ttl :: !(Prelude.Maybe Data.Word.Word32),
