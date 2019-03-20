module RiakMapReducePhase where

import RiakErlangFunction (ErlangFunction)
import RiakErlangTerm     (ErlangTerm(..))

import qualified RiakErlangFunction as ErlangFunction
import qualified RiakErlangTerm     as ErlangTerm


data MapReducePhase
  = MapPhase ErlangFunction ErlangTerm Bool
  | ReducePhase ErlangFunction ErlangTerm Bool
  deriving stock (Show)

toErlangTerm :: MapReducePhase -> ErlangTerm
toErlangTerm = \case
  MapPhase fun arg accum ->
    go "map" fun arg accum

  ReducePhase fun arg accum ->
    go "reduce" fun arg accum

  where
    go :: Text -> ErlangFunction -> ErlangTerm -> Bool -> ErlangTerm
    go typ fun arg accum =
      ErlangTerm.tuple4
        (ErlAtomUtf8 typ)
        (ErlangFunction.toMapredErlangTerm fun)
        arg
        (ErlangTerm.bool accum)
