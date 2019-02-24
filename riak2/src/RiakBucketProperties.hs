module RiakBucketProperties where

import RiakIndexName   (IndexName(..))
import RiakReadQuorum  (ReadQuorum(..))
import RiakWriteQuorum (WriteQuorum(..))

import qualified Libriak.Proto   as Proto
import qualified RiakReadQuorum  as ReadQuorum
import qualified RiakWriteQuorum as WriteQuorum

import Control.Lens       ((^.))
import Data.Text.Encoding (decodeUtf8)

data BucketProperties
  = BucketPropertiesCounter CounterBucketProperties
  | BucketPropertiesHyperLogLog HyperLogLogBucketProperties
  | BucketPropertiesMap MapBucketProperties
  | BucketPropertiesObject ObjectBucketProperties
  | BucketPropertiesSet SetBucketProperties
  deriving stock (Show)

-- TODO counter/hll/map/set bucket properties
data CounterBucketProperties
  = CounterBucketProperties
  deriving stock (Show)

data HyperLogLogBucketProperties
  = HyperLogLogBucketProperties
  deriving stock (Show)

data MapBucketProperties
  = MapBucketProperties
  deriving stock (Show)

data ObjectBucketProperties
  = ObjectBucketProperties
  { conflictResolution :: !ConflictResolution
  , nodes :: !Natural
  , notfoundBehavior :: !NotfoundBehavior
  , postcommitHooks :: ![Proto.RpbCommitHook]
  , precommitHooks :: ![Proto.RpbCommitHook]
  , readQuorum :: !ReadQuorum
  , index :: !(Maybe IndexName) -- ^ Search index
  , writeQuorum :: !WriteQuorum
  } deriving stock (Generic, Show)

data SetBucketProperties
  = SetBucketProperties
  deriving stock (Show)

-- | The conflict resolution strategy used by Riak, for normal KV (non-CRDT)
-- objects.
--
-- Allowing Riak to create siblings is highly recommended, but there are two
-- built-in strategies to resolve conflicts in Riak.
data ConflictResolution
  = CreateSiblings
  | UseTimestamps
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
    Nothing ->
      BucketPropertiesObject ObjectBucketProperties
        { conflictResolution =
            case (props ^. Proto.allowMult, props ^. Proto.lastWriteWins) of
              (True, _)      -> CreateSiblings
              (False, False) -> UseTimestamps
              (False, True)  -> LastWriteWins
        , nodes = fromIntegral (props ^. Proto.nVal)
        , notfoundBehavior =
            case (fromMaybe True (props ^. Proto.maybe'notfoundOk), props ^. Proto.basicQuorum) of
              (True, _)      -> NotfoundCounts
              (False, False) -> NotfoundSkipped
              (False, True)  -> NotfoundSkippedBasic
        , postcommitHooks = props ^. Proto.postcommit
        , precommitHooks = props ^. Proto.precommit
        , readQuorum = ReadQuorum.fromProto props
        , index = IndexName . decodeUtf8 <$> (props ^. Proto.maybe'searchIndex)
        , writeQuorum = WriteQuorum.fromProto props
        }

    _ ->
      undefined

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
