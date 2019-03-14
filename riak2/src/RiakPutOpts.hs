module RiakPutOpts where

import RiakUtils       (difftimeToMillis)
import RiakWriteQuorum (WriteQuorum)

import qualified RiakWriteQuorum as WriteQuorum

import Control.Lens                       ((.~))
import Data.Default.Class                 (Default(..))
import Data.ProtoLens.Runtime.Lens.Labels (HasLens')
import Data.Time                          (NominalDiffTime)

import qualified Data.Riak.Proto as Proto


data PutOpts
  = PutOpts
  { nodes :: Maybe Natural
  , quorum :: Maybe WriteQuorum
  , timeout :: Maybe NominalDiffTime
  } deriving stock (Generic, Show)

instance Default PutOpts where
  def :: PutOpts
  def =
    PutOpts
      { nodes = Nothing
      , quorum = Nothing
      , timeout = Nothing
      }

setProto ::
     ( HasLens' a "dw" Word32
     , HasLens' a "maybe'nVal" (Maybe Word32)
     , HasLens' a "maybe'timeout" (Maybe Word32)
     , HasLens' a "pw" Word32
     , HasLens' a "w" Word32
     )
  => PutOpts
  -> a
  -> a
setProto PutOpts { nodes, quorum, timeout } =
  WriteQuorum.setProto quorum .
  (Proto.maybe'nVal .~ (fromIntegral <$> nodes)) .
  (Proto.maybe'timeout .~ (difftimeToMillis <$> timeout))
