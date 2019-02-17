module RiakMapReducePhase where

import RiakErlangTerm        (ErlangTerm(..))
import RiakMapReduceFunction (MapReduceFunction)

import qualified RiakErlangTerm        as ErlangTerm
import qualified RiakMapReduceFunction as MapReduceFunction


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
