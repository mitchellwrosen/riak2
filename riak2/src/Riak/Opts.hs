module Riak.Opts
  ( GetObjectOpts(..)
  , PutObjectOpts(..)
  ) where

import Riak.Internal.Prelude
import Riak.Quorum           (Quorum)


data GetObjectOpts
  = GetObjectOpts
  { basicQuorum :: !Bool
  , n :: !Quorum
  , notfoundOk :: !Bool
  , pr :: !Quorum
  , r :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32)
  } deriving stock (Generic, Show)

data PutObjectOpts
  = PutObjectOpts
  { dw :: !Quorum
  , pw :: !Quorum
  , n :: !Quorum
  , sloppyQuorum :: !Bool
  , timeout :: !(Maybe Word32)
  , w :: !Quorum
  } deriving stock (Generic, Show)
-- _RpbPutReq'ifNotModified :: !(Prelude.Maybe Prelude.Bool),
-- _RpbPutReq'ifNoneMatch :: !(Prelude.Maybe Prelude.Bool),
-- _RpbPutReq'asis :: !(Prelude.Maybe Prelude.Bool),
