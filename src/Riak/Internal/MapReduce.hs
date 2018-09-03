{-# LANGUAGE DataKinds, GADTs, LambdaCase,
             NoImplicitPrelude #-}

module Riak.Internal.MapReduce where

import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Vector           as Vector

import Proto.Riak (RpbMapRedReq(..))
import Riak.Internal.Prelude
import Riak.Internal.Types

import Erlang

data RiakMapReduceInputs where
  RiakMapReduceNamespace
    :: !(RiakNamespace 'Nothing)
    -> RiakMapReduceInputs

  RiakMapReduceLocations
    :: ![RiakLocation 'Nothing]
    -> RiakMapReduceInputs

  RiakMapReduceFunction
    :: !Text
    -> !Text
    -> RiakMapReduceInputs

  RiakMapReduceExactQuery
    :: !(RiakNamespace 'Nothing )
    -> !RiakExactQuery
    -> RiakMapReduceInputs

  RiakMapReduceRangeQuery
    :: !(RiakNamespace 'Nothing)
    -> !(RiakRangeQuery a)
    -> RiakMapReduceInputs

riakMapReduceInputsTerm :: RiakMapReduceInputs -> ErlTerm
riakMapReduceInputsTerm = \case
  -- {T,B}
  RiakMapReduceNamespace (RiakNamespace type' bucket) ->
    (ErlSmallTuple . Vector.fromList)
      [ ErlBinary (unRiakBucketType type')
      , ErlBinary (unRiakBucket bucket)
      ]

  -- [{{{T,B},K},_KeyData}]
  RiakMapReduceLocations locations ->
    let
      toTerm :: RiakLocation 'Nothing -> ErlTerm
      toTerm (RiakLocation (RiakNamespace type' bucket) key) =
        (ErlSmallTuple . Vector.fromList)
          [ (ErlSmallTuple . Vector.fromList)
              [ (ErlSmallTuple . Vector.fromList)
                  [ ErlBinary (unRiakBucketType type')
                  , ErlBinary (unRiakBucket bucket)
                  ]
              , ErlBinary (unRiakKey key)
              ]
          , ErlAtomUtf8 "undefined"
          ]

    in
      ErlList
        (Vector.fromList (map toTerm locations))
        ErlNil

  -- {modfun, Module, Function, _Options}
  RiakMapReduceFunction m f ->
    (ErlSmallTuple . Vector.fromList)
      [ ErlAtomUtf8 "modfun"
      , ErlAtomUtf8 m
      , ErlAtomUtf8 f
      , ErlList mempty ErlNil
      ]

  -- {index, {Type, Bucket}, Index, Key}
  RiakMapReduceExactQuery (RiakNamespace type' bucket) query ->
    (ErlSmallTuple . Vector.fromList)
      [ ErlAtomUtf8 "index"
      , (ErlSmallTuple . Vector.fromList)
          [ ErlBinary (unRiakBucketType type')
          , ErlBinary (unRiakBucket bucket)
          ]
      , case query of
          -- TODO centralize this _bin suffix business
          RiakExactQueryInt name _ ->
            ErlBinary (unRiakIndexName name <> "_int")
          RiakExactQueryBin name _ ->
            ErlBinary (unRiakIndexName name <> "_bin")
      , case query of
          RiakExactQueryInt _ key ->
            ErlBinary (Latin1.pack (show key))
          RiakExactQueryBin _ key ->
            ErlBinary key
      ]

  -- {index, {Type, Bucket}, Index, StartKey, EndKey}
  RiakMapReduceRangeQuery (RiakNamespace type' bucket) query ->
    (ErlSmallTuple . Vector.fromList)
      [ ErlAtomUtf8 "index"
      , (ErlSmallTuple . Vector.fromList)
          [ ErlBinary (unRiakBucketType type')
          , ErlBinary (unRiakBucket bucket)
          ]
      , case query of
          -- TODO centralize this _bin suffix business
          RiakRangeQueryInt name _ _ ->
            ErlBinary (unRiakIndexName name <> "_int")
          RiakRangeQueryBin name _ _ ->
            ErlBinary (unRiakIndexName name <> "_bin")
      , case query of
          RiakRangeQueryInt _ key _ ->
            ErlBinary (Latin1.pack (show key))
          RiakRangeQueryBin _ key _ ->
            ErlBinary key
      , case query of
          RiakRangeQueryInt _ _ key ->
            ErlBinary (Latin1.pack (show key))
          RiakRangeQueryBin _ _ key ->
            ErlBinary key
      ]

riakMapReduceRequest
  :: RiakMapReduceInputs
  -> ()
  -> RpbMapRedReq
riakMapReduceRequest inputs query =
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
        [ (ErlSmallTuple . Vector.fromList)
            [ ErlAtomUtf8 "inputs"
            , riakMapReduceInputsTerm inputs
            ]
        -- TODO encode map reduce query
        , (ErlSmallTuple . Vector.fromList)
            [ ErlAtomUtf8 "query"
            , undefined
            ]
        ])
      ErlNil
