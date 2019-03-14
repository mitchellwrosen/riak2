module RiakGetOpts where

import RiakReadQuorum (ReadQuorum)
import RiakUtils      (difftimeToMillis)

import qualified RiakReadQuorum as ReadQuorum

import Control.Lens                       ((.~))
import Data.Default.Class                 (Default(..))
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Data.Time                          (NominalDiffTime)

import qualified Data.Riak.Proto as Proto


data GetOpts
  = GetOpts
  { nodes :: Maybe Natural
  , quorum :: Maybe ReadQuorum
  , timeout :: Maybe NominalDiffTime
  } deriving stock (Generic, Show)

instance Default GetOpts where
  def :: GetOpts
  def =
    GetOpts
      { nodes = Nothing
      , quorum = Nothing
      , timeout = Nothing
      }

setProto ::
     ( HasLens' a "basicQuorum" Bool
     , HasLens' a "maybe'nVal" (Maybe Word32)
     , HasLens' a "maybe'timeout" (Maybe Word32)
     , HasLens' a "notfoundOk" Bool
     , HasLens' a "pr" Word32
     , HasLens' a "r" Word32
     )
  => GetOpts
  -> a
  -> a
setProto GetOpts { nodes, quorum, timeout } =
  ReadQuorum.setProto quorum .
  (Proto.maybe'nVal .~ (fromIntegral <$> nodes)) .
  (Proto.maybe'timeout .~ (difftimeToMillis <$> timeout))
