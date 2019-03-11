module RiakNotfoundBehavior
  ( NotfoundBehavior(..)
  , fromProto
  ) where

import Control.Lens ((^.))

import qualified Data.Riak.Proto as Proto


-- | TODO better names for NotfoundBehavior constructors
data NotfoundBehavior
  = NotfoundCounts -- notfound counts towards r
  | NotfoundSkipped -- all n vnodes are queried
  | NotfoundSkippedBasic -- only a quorum of vnodes are queried
  deriving stock (Eq, Show)

fromProto :: Proto.RpbBucketProps -> NotfoundBehavior
fromProto props =
  case (fromMaybe True (props ^. Proto.maybe'notfoundOk), props ^. Proto.basicQuorum) of
    (True, _)      -> NotfoundCounts
    (False, False) -> NotfoundSkipped
    (False, True)  -> NotfoundSkippedBasic
