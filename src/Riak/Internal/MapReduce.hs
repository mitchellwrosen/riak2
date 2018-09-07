{-# LANGUAGE DataKinds, GADTs, LambdaCase, NoImplicitPrelude,
             OverloadedStrings #-}

module Riak.Internal.MapReduce
  ( RiakMapReduceInputs(..)
  , RiakMapReducePhase(..)
  , riakMapReducePhaseMapIdentity
  , riakMapReducePhaseMapObjectValue
  , riakMapReducePhaseReduceCount
  , riakMapReducePhaseReduceSetUnion
  , riakMapReducePhaseReduceSort
  , riakMapReducePhaseReduceSum
  , riakMapReduceRequest
  ) where

import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Vector           as Vector

import Proto.Riak            (RpbMapRedReq(..))
import Riak.Internal.Prelude
import Riak.Internal.Types

import Erlang


data RiakMapReduceInputs where
  RiakMapReduceInputsBucket
    :: !(RiakBucket 'Nothing)
    -> RiakMapReduceInputs

  RiakMapReduceInputsKeys
    :: ![RiakKey 'Nothing]
    -> RiakMapReduceInputs

  RiakMapReduceInputsFunction
    :: !Text
    -> !Text
    -> RiakMapReduceInputs

  RiakMapReduceInputsExactQuery
    :: !(RiakBucket 'Nothing)
    -> !RiakExactQuery
    -> RiakMapReduceInputs

  RiakMapReduceInputsRangeQuery
    :: !(RiakBucket 'Nothing)
    -> !(RiakRangeQuery a)
    -> RiakMapReduceInputs

  -- TODO search query
  -- see riak_kv_mapred_term.erl


data RiakMapReducePhase
  = RiakMapReducePhaseMap !Text !Text
  -- TODO more map function syntax than just modfun
  | RiakMapReducePhaseReduce !Text !Text
  -- TODO more reduce function syntax than just modfun

-- | Map an input to itself.
riakMapReducePhaseMapIdentity :: RiakMapReducePhase
riakMapReducePhaseMapIdentity =
  RiakMapReducePhaseMap "riak_kv_mapreduce" "map_identity"

-- | Map an object to its value.
riakMapReducePhaseMapObjectValue :: RiakMapReducePhase
riakMapReducePhaseMapObjectValue =
  RiakMapReducePhaseMap "riak_kv_mapreduce" "map_object_value"

-- | Reduce inputs to their count.
riakMapReducePhaseReduceCount :: RiakMapReducePhase
riakMapReducePhaseReduceCount =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_count_inputs"

-- | Reduce inputs to their union.
riakMapReducePhaseReduceSetUnion :: RiakMapReducePhase
riakMapReducePhaseReduceSetUnion =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_set_union"

-- | Reduce inputs to their sorted values.
riakMapReducePhaseReduceSort :: RiakMapReducePhase
riakMapReducePhaseReduceSort =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_sort"

-- | Reduce inputs to their sum.
riakMapReducePhaseReduceSum :: RiakMapReducePhase
riakMapReducePhaseReduceSum =
  RiakMapReducePhaseReduce "riak_kv_mapreduce" "reduce_sum"


-- Make an @application/x-erlang-binary@ MapReduce request from the inputs and
-- phases. The @application/json@ interface is deprecated.
riakMapReduceRequest
  :: RiakMapReduceInputs
  -> [RiakMapReducePhase]
  -> RpbMapRedReq
riakMapReduceRequest inputs phases =
  RpbMapRedReq
    { _RpbMapRedReq'_unknownFields = []
    , _RpbMapRedReq'contentType    = "application/x-erlang-binary"
    , _RpbMapRedReq'request        = buildErlTerm term
    }
 where
  -- [{inputs, Inputs}, {query, Query}, {timeout, Timeout}]
  -- timeout is optional
  term :: ErlTerm
  term =
    ErlList
      (Vector.fromList
        [ erlTuple2
            (ErlAtomUtf8 "inputs")
            (riakMapReduceInputsTerm inputs)
        -- TODO encode map reduce query
        , erlTuple2
            (ErlAtomUtf8 "query")
            (ErlList
              (Vector.fromList
                (map (uncurry riakMapReducePhaseTerm) (trueLast phases)))
              ErlNil)
        ])
      ErlNil


riakMapReduceInputsTerm :: RiakMapReduceInputs -> ErlTerm
riakMapReduceInputsTerm = \case
  -- {T,B}
  RiakMapReduceInputsBucket (RiakBucket type' bucket) ->
    erlTuple2
      (ErlBinary (unRiakBucketType type'))
      (ErlBinary bucket)

  -- [{{{T,B},K},_KeyData}]
  RiakMapReduceInputsKeys keys ->
    let
      toTerm :: RiakKey 'Nothing -> ErlTerm
      toTerm (RiakKey (RiakBucket (RiakBucketType type') bucket) key) =
        erlTuple2
          (erlTuple2
            (erlTuple2
              (ErlBinary type')
              (ErlBinary bucket))
            (ErlBinary key))
          atomNone

    in
      ErlList
        (Vector.fromList (map toTerm keys))
        ErlNil

  -- {modfun, Module, Function, _Options}
  RiakMapReduceInputsFunction m f ->
    erlTuple4
      atomModfun
      (ErlAtomUtf8 m)
      (ErlAtomUtf8 f)
      (ErlList mempty ErlNil)

  -- {index, {Type, Bucket}, Index, Key}
  RiakMapReduceInputsExactQuery (RiakBucket type' bucket) query ->
    erlTuple4
      (ErlAtomUtf8 "index")
      (erlTuple2
        (ErlBinary (unRiakBucketType type'))
        (ErlBinary bucket))
      (case query of
        -- TODO centralize this _bin suffix business
        RiakExactQueryInt name _ ->
          ErlBinary (unRiakIndexName name <> "_int")
        RiakExactQueryBin name _ ->
          ErlBinary (unRiakIndexName name <> "_bin"))
      (case query of
        RiakExactQueryInt _ key ->
          ErlBinary (Latin1.pack (show key))
        RiakExactQueryBin _ key ->
          ErlBinary key)

  -- {index, {Type, Bucket}, Index, StartKey, EndKey}
  RiakMapReduceInputsRangeQuery (RiakBucket type' bucket) query ->
    erlTuple5
      (ErlAtomUtf8 "index")
      (erlTuple2
        (ErlBinary (unRiakBucketType type'))
        (ErlBinary bucket))
      (case query of
        -- TODO centralize this _bin suffix business
        RiakRangeQueryInt name _ _ ->
          ErlBinary (unRiakIndexName name <> "_int")
        RiakRangeQueryBin name _ _ ->
          ErlBinary (unRiakIndexName name <> "_bin"))
      (case query of
        RiakRangeQueryInt _ key _ ->
          ErlBinary (Latin1.pack (show key))
        RiakRangeQueryBin _ key _ ->
          ErlBinary key)
      (case query of
        RiakRangeQueryInt _ _ key ->
          ErlBinary (Latin1.pack (show key))
        RiakRangeQueryBin _ _ key ->
          ErlBinary key)


riakMapReducePhaseTerm :: Bool -> RiakMapReducePhase -> ErlTerm
riakMapReducePhaseTerm keep = \case
  RiakMapReducePhaseMap m f ->
    erlTuple4
      (ErlAtomUtf8 "map")
      (erlTuple3
        atomModfun
        (ErlAtomUtf8 m)
        (ErlAtomUtf8 f))
      atomNone
      (boolToTerm keep)

  RiakMapReducePhaseReduce m f ->
    erlTuple4
      (ErlAtomUtf8 "reduce")
      (erlTuple3
        atomModfun
        (ErlAtomUtf8 m)
        (ErlAtomUtf8 f))
      atomNone
      (boolToTerm keep)


atomModfun :: ErlTerm
atomModfun =
  ErlAtomUtf8 "modfun"

atomNone :: ErlTerm
atomNone =
  ErlAtomUtf8 "none"

boolToTerm :: Bool -> ErlTerm
boolToTerm = \case
  True  -> ErlAtomUtf8 "true"
  False -> ErlAtomUtf8 "false"

erlTuple2 :: ErlTerm -> ErlTerm -> ErlTerm
erlTuple2 a b =
  ErlSmallTuple (Vector.fromList [a, b])

erlTuple3 :: ErlTerm -> ErlTerm -> ErlTerm -> ErlTerm
erlTuple3 a b c =
  ErlSmallTuple (Vector.fromList [a, b, c])

erlTuple4 :: ErlTerm -> ErlTerm -> ErlTerm -> ErlTerm -> ErlTerm
erlTuple4 a b c d =
  ErlSmallTuple (Vector.fromList [a, b, c, d])

erlTuple5 :: ErlTerm -> ErlTerm -> ErlTerm -> ErlTerm -> ErlTerm -> ErlTerm
erlTuple5 a b c d e =
  ErlSmallTuple (Vector.fromList [a, b, c, d, e])

trueLast :: [a] -> [(Bool, a)]
trueLast =
  foldr step []
 where
  step :: a -> [(Bool, a)] -> [(Bool, a)]
  step x = \case
    []  -> [(True, x)]
    xs  -> (False, x) : xs
