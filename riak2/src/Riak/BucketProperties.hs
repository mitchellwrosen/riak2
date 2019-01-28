module Riak.BucketProperties
  ( BucketProperties(..)
  , ConflictResolution(..)
  , NotfoundBehavior(..)
  ) where

import Riak.Internal.Prelude

import qualified Riak.Proto      as Proto
import qualified Riak.Proto.Lens as L

import Data.Functor.Identity

data BucketProperties f
  = BucketProperties
  { conflict :: HKD f ConflictResolution
  , n :: HKD f Word32
  , notfound :: HKD f NotfoundBehavior
  }

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

-- | The conflict resolution strategy used by Riak.
--
-- Allowing Riak to create siblings is highly recommended, but there are two
-- built-in strategies to resolve conflicts in Riak.
data ConflictResolution
  = CreateSiblings
  | UseTimestamps
  | LastWriteWins

-- | TODO better names for NotfoundBehavior constructors
data NotfoundBehavior
  = NotfoundCounts
  | NotfoundSkipped
  | NotfoundSkippedBasic

fromProto :: Proto.BucketProperties -> BucketProperties Identity
fromProto props =
  BucketProperties
    { conflict =
        case (props ^. L.allowMult, props ^. L.lastWriteWins) of
          (True, _)      -> CreateSiblings
          (False, False) -> UseTimestamps
          (False, True)  -> LastWriteWins
    , n = props ^. L.n
    , notfound =
        case (fromMaybe True (props ^. L.maybe'notfoundOk), props ^. L.basicQuorum) of
          (True, _)      -> NotfoundCounts
          (False, False) -> NotfoundSkipped
          (False, True)  -> NotfoundSkippedBasic
    }

-- _BucketProperties'precommit :: ![CommitHook],
-- _BucketProperties'hasPrecommit :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'postcommit :: ![CommitHook],
-- _BucketProperties'hasPostcommit :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'chashKeyfun :: !(Prelude.Maybe ModuleFunction),
-- _BucketProperties'linkfun :: !(Prelude.Maybe ModuleFunction),
-- _BucketProperties'oldVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'youngVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'bigVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'smallVclock :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'pr :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'r :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'w :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'rw :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'backend :: !(Prelude.Maybe Data.ByteString.ByteString),
-- _BucketProperties'search :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'repl :: !(Prelude.Maybe BucketProperties'RpbReplMode),
-- _BucketProperties'searchIndex :: !(Prelude.Maybe Data.ByteString.ByteString),
-- _BucketProperties'datatype :: !(Prelude.Maybe Data.ByteString.ByteString),
-- _BucketProperties'consistent :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'writeOnce :: !(Prelude.Maybe Prelude.Bool),
-- _BucketProperties'hllPrecision :: !(Prelude.Maybe Data.Word.Word32),
-- _BucketProperties'ttl :: !(Prelude.Maybe Data.Word.Word32),

