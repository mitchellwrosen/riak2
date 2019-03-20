module RiakErlangFunction
  ( ErlangFunction(..)

  , toMapredErlangTerm
  ) where

import RiakErlangFunctionId (ErlangFunctionId(..))
import RiakErlangTerm       (ErlangTerm(..))

import qualified RiakErlangTerm as ErlangTerm


data ErlangFunction
  = CompiledFunction ErlangFunctionId
  | InterpretedFunction ByteString
  deriving stock (Show)

toMapredErlangTerm :: ErlangFunction -> ErlangTerm
toMapredErlangTerm = \case
  CompiledFunction (ErlangFunctionId modul fun) ->
    ErlangTerm.tuple3
      (ErlAtomUtf8 "modfun")
      (ErlAtomUtf8 modul)
      (ErlAtomUtf8 fun)

  InterpretedFunction code ->
    ErlangTerm.tuple2
      (ErlAtomUtf8 "strfun")
      (ErlBinary code)
