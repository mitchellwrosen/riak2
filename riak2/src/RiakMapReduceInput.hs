module RiakMapReduceInput
  ( MapReduceInput(..)
  , toErlangTerm
  ) where

import RiakBinaryIndexQuery (BinaryIndexQuery(..))
import RiakBucket           (Bucket(..))
import RiakErlangTerm       (ErlangTerm(..))
import RiakIntIndexQuery    (IntIndexQuery(..))
import RiakKey              (Key(..))
import RiakUtils            (int2bs)

import qualified RiakBinaryIndexQuery    as BinaryIndexQuery
import qualified RiakErlangTerm          as Erlang

import qualified Data.Vector as Vector


data MapReduceInput
  = MapReduceInputBucket !Bucket
  | MapReduceInputKeys ![Key]
  | MapReduceInputFunction !Text !Text
  | MapReduceInputIntIndexQuery !IntIndexQuery
  | MapReduceInputBinaryIndexQuery !BinaryIndexQuery
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

  MapReduceInputIntIndexQuery query ->
    intIndexQueryToErlangTerm query

  MapReduceInputBinaryIndexQuery query ->
    binaryIndexQueryToErlangTerm query

intIndexQueryToErlangTerm :: IntIndexQuery -> ErlangTerm
intIndexQueryToErlangTerm IntIndexQuery { bucket, index, minValue, maxValue } =
  if minValue == maxValue
    then exactQueryToErlangTerm bucket index (int2bs minValue)
    else rangeQueryToErlangTerm bucket index (int2bs minValue) (int2bs maxValue)

binaryIndexQueryToErlangTerm :: BinaryIndexQuery -> ErlangTerm
binaryIndexQueryToErlangTerm query@(BinaryIndexQuery { bucket, minValue, maxValue }) =
  if minValue == maxValue
    then exactQueryToErlangTerm bucket index minValue
    else rangeQueryToErlangTerm bucket index minValue maxValue
  where
    index :: ByteString
    index =
      BinaryIndexQuery.indexName query

-- {T, B}
bucketToErlangTerm :: Bucket -> ErlangTerm
bucketToErlangTerm (Bucket bucketType bucket) =
  Erlang.tuple2 (ErlBinary bucketType) (ErlBinary bucket)

-- {index, {Type, Bucket}, Index, Key}
exactQueryToErlangTerm :: Bucket -> ByteString -> ByteString -> ErlangTerm
exactQueryToErlangTerm bucket index value =
  Erlang.tuple4
    atomIndex
    (bucketToErlangTerm bucket)
    (ErlBinary index)
    (ErlBinary value)

-- {index, {Type, Bucket}, Index, StartKey, EndKey}
rangeQueryToErlangTerm ::
     Bucket
  -> ByteString
  -> ByteString
  -> ByteString
  -> ErlangTerm
rangeQueryToErlangTerm bucket index minValue maxValue =
  Erlang.tuple5
    atomIndex
    (bucketToErlangTerm bucket)
    (ErlBinary index)
    (ErlBinary minValue)
    (ErlBinary maxValue)

atomIndex :: ErlangTerm
atomIndex =
  ErlAtomUtf8 "index"
