module Riak.Internal.Context where

import Riak.Internal.Prelude

import qualified ByteString
import qualified Data.ByteString.Base64 as Base64


-- | The opaque causal context attached to an object or data type.
newtype Context
  = Context { unContext :: ByteString }
  deriving stock (Eq)

-- | base-64 encoded for for display purposes. The actual context is an
-- opaque binary blob.
instance Show Context where
  show :: Context -> String
  show =
    coerce (("base64:" ++) . show . Base64.encode)

-- | The "new" context. Use this when writing an object or data type for the
-- first time.
--
-- Note that it is possible for two clients to simultaneously believe they are
-- writing an object or data type for the first time, and neglect to include a
-- causal context.
--
-- For objects, two siblings will be created in this case (unless the bucket
-- properties prohibit them, which is not recommended), and should be resolved
-- on a later read.
--
-- For data types, data-type-specific merge logic will collapse the versions
-- into one. This might go wrong (for example, the "register" data type uses a
-- simple last-write-wins conflict resolution strategy), but is unavoidable for
-- the very first write, without some out-of-band locking mechanism.
newContext :: Context
newContext =
  Context ByteString.empty
