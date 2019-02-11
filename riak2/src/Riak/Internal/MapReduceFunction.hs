module Riak.Internal.MapReduceFunction where

import Riak.Internal.Prelude
import Riak.Internal.ErlangTerm (ErlangTerm(..))

import qualified Riak.Internal.ErlangTerm as ErlangTerm


data MapReduceFunction
  = MapReduceFunctionCompiled !Text !Text
  | MapReduceFunctionInterpreted !ByteString
  deriving stock (Show)

toErlangTerm :: MapReduceFunction -> ErlangTerm
toErlangTerm = \case
  MapReduceFunctionCompiled modul fun ->
    ErlangTerm.tuple3
      (ErlAtomUtf8 "modfun")
      (ErlAtomUtf8 modul)
      (ErlAtomUtf8 fun)

  MapReduceFunctionInterpreted code ->
    ErlangTerm.tuple2
      (ErlAtomUtf8 "strfun")
      (ErlBinary code)
