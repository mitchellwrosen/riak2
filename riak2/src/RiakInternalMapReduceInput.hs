module RiakInternalMapReduceInput
  ( MapReduceInput(..)
  , toErlangTerm
  ) where

import RiakInternalBucket     (Bucket(..))
import RiakInternalErlangTerm (ErlangTerm(..))
import RiakInternalExactQuery (ExactQuery(..))
import RiakInternalKey        (Key(..))
import RiakInternalPrelude
import RiakInternalRangeQuery (RangeQuery(..))

import qualified RiakInternalErlangTerm          as Erlang
import qualified RiakInternalExactQuery          as ExactQuery
import qualified RiakInternalRangeQuery          as RangeQuery
import qualified RiakInternalSecondaryIndexValue as SecondaryIndexValue

import qualified Data.Vector as Vector


data MapReduceInput
  = MapReduceInputBucket !Bucket
  | MapReduceInputKeys ![Key]
  | MapReduceInputFunction !Text !Text
  | MapReduceInputExactQuery !ExactQuery
  | forall a. MapReduceInputRangeQuery !(RangeQuery a)
  -- TODO MapReduceInputSearch
  -- see riak_kv_mapred_term.erl

toErlangTerm :: MapReduceInput -> ErlangTerm
toErlangTerm = \case
  MapReduceInputBucket bucket ->
    bucketToErlangTerm bucket

  -- [{{{T,B},K},_KeyData}]
  MapReduceInputKeys keys ->
    let
      keyToTerm :: Key -> ErlangTerm
      keyToTerm (Key bucketType bucket key) =
        Erlang.tuple2
          (Erlang.tuple2
            (Erlang.tuple2 (ErlBinary bucketType) (ErlBinary bucket))
            (ErlBinary key))
          Erlang.atomNone

    in
      Erlang.list (Vector.fromList (map keyToTerm keys))

  -- {modfun, Module, Function, _Options}
  MapReduceInputFunction m f ->
    Erlang.tuple4
      Erlang.atomModfun
      (ErlAtomUtf8 m)
      (ErlAtomUtf8 f)
      (Erlang.list Vector.empty)

  MapReduceInputExactQuery query ->
    exactQueryToErlangTerm query

  MapReduceInputRangeQuery query ->
    rangeQueryToErlangTerm query

-- {T, B}
bucketToErlangTerm :: Bucket -> ErlangTerm
bucketToErlangTerm (Bucket bucketType bucket) =
  Erlang.tuple2 (ErlBinary bucketType) (ErlBinary bucket)

-- {index, {Type, Bucket}, Index, Key}
exactQueryToErlangTerm :: ExactQuery -> ErlangTerm
exactQueryToErlangTerm query@(ExactQuery { bucket, value }) =
  Erlang.tuple4
    (ErlAtomUtf8 "index")
    (bucketToErlangTerm bucket)
    (ErlBinary (ExactQuery.name query))
    (ErlBinary (SecondaryIndexValue.encode value))

-- {index, {Type, Bucket}, Index, StartKey, EndKey}
rangeQueryToErlangTerm :: RangeQuery a -> ErlangTerm
rangeQueryToErlangTerm query@(RangeQuery { bucket, min, max }) =
  Erlang.tuple5
    (ErlAtomUtf8 "index")
    (bucketToErlangTerm bucket)
    (ErlBinary (RangeQuery.name query))
    (ErlBinary (SecondaryIndexValue.encode min))
    (ErlBinary (SecondaryIndexValue.encode max))
