module Riak.Internal.MapReducePhase where

import Riak.Internal.ErlangTerm        (ErlangTerm(..))
import Riak.Internal.MapReduceFunction (MapReduceFunction)
import Riak.Internal.Prelude

import qualified Riak.Internal.ErlangTerm        as ErlangTerm
import qualified Riak.Internal.MapReduceFunction as MapReduceFunction


data MapReducePhase
  = MapReducePhaseMap !MapReduceFunction !ErlangTerm !Bool
  | MapReducePhaseReduce !MapReduceFunction !ErlangTerm !Bool
  deriving stock (Show)

toErlangTerm :: MapReducePhase -> ErlangTerm
toErlangTerm = \case
  MapReducePhaseMap fun arg accum ->
    go "map" fun arg accum

  MapReducePhaseReduce fun arg accum ->
    go "reduce" fun arg accum

  where
    go :: Text -> MapReduceFunction -> ErlangTerm -> Bool -> ErlangTerm
    go typ fun arg accum =
      ErlangTerm.tuple4
        (ErlAtomUtf8 typ)
        (MapReduceFunction.toErlangTerm fun)
        arg
        (ErlangTerm.bool accum)
