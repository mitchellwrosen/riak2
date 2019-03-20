module RiakMapReduceFunction where

import RiakErlangTerm (ErlangTerm(..))
import RiakErlangFunction (ErlangFunction(..))

import qualified RiakErlangTerm as ErlangTerm


data MapReduceFunction
  = MapReduceFunctionCompiled ErlangFunction
  | MapReduceFunctionInterpreted ByteString
  deriving stock (Show)

toErlangTerm :: MapReduceFunction -> ErlangTerm
toErlangTerm = \case
  MapReduceFunctionCompiled (ErlangFunction modul fun) ->
    ErlangTerm.tuple3
      (ErlAtomUtf8 "modfun")
      (ErlAtomUtf8 modul)
      (ErlAtomUtf8 fun)

  MapReduceFunctionInterpreted code ->
    ErlangTerm.tuple2
      (ErlAtomUtf8 "strfun")
      (ErlBinary code)
