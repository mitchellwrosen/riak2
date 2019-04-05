{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Riak

import Control.Lens
import Control.Monad
import Data.ByteString       (ByteString)
import Data.Default.Class    (def)
import Data.Either           (isRight)
import Data.Generics.Product (field)
import Data.List.NonEmpty    (NonEmpty(..))
import Data.Text             (Text)
import Net.IPv4              (ipv4)
import Socket.Stream.IPv4    (Endpoint(..))
import System.Random         (randomRIO)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Foldl         as Foldl
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Text             as Text
import qualified Data.Vector           as Vector

main :: IO ()
main = do
  handle <- createHandle config
  defaultMain (testGroup "Riak integration tests" (integrationTests handle))

  where
    config :: HandleConfig
    config =
      HandleConfig
        { endpoint = Endpoint (ipv4 127 0 0 1) 8087
        , retries = 3
        , healthCheckInterval = 1
        , idleTimeout = 30
        , requestTimeout = 10
        , connectTimeout = 10
        , handlers =
            mempty
              -- { onSend = \req -> putStrLn (">>> " ++ show req)
              -- , onReceive = \resp -> putStrLn ("<<< " ++ show resp)
              -- }
        }

integrationTests :: Handle -> [TestTree]
integrationTests handle =
  [ testGroup "RiakBucket" (riakBucketTests handle)
  , testGroup "RiakBucketType" (riakBucketTypeTests handle)
  , testGroup "RiakCounter" (riakCounterTests handle)
  , testGroup "RiakHyperLogLog" (riakHyperLogLogTests handle)
  , testGroup "RiakIndex" (riakIndexTests handle)
  , testGroup "RiakMap" (riakMapTests handle)
  , testGroup "RiakMapReduce" (riakMapReduceTests handle)
  , testGroup "RiakObject" (riakObjectTests handle)
  , testGroup "RiakPing" (riakPingTests handle)
  , testGroup "RiakServerInfo" (riakServerInfoTests handle)
  , testGroup "RiakSet" (riakSetTests handle)
  ]

riakBucketTests :: Handle -> [TestTree]
riakBucketTests handle =
  [ testGroup "getBucket"
    [ testCase "object bucket" $ do
        bucket <- randomDefaultBucket
        getBucket handle bucket >>= \case
          Right SomeBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "counter bucket" $ do
        bucket <- randomCounterBucket
        getBucket handle bucket >>= \case
          Right SomeCounterBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "hll bucket" $ do
        bucket <- randomHyperLogLogBucket
        getBucket handle bucket >>= \case
          Right SomeHyperLogLogBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "map bucket" $ do
        bucket <- randomMapBucket
        getBucket handle bucket >>= \case
          Right SomeMapBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "set bucket" $ do
        bucket <- randomSetBucket
        getBucket handle bucket >>= \case
          Right SomeSetBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "empty bucket works for some reason" $
        getBucket handle (Bucket defaultBucketType "") `shouldReturnSatisfy`
          isRight

    , testGroup "failures"
      [ testCase "bucket type not found" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          getBucket handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getCounterBucket"
    [ testCase "success" $ do
        bucket <- randomCounterBucket
        getCounterBucket handle bucket `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-counter bucket" $ do
          bucket@(Bucket bucketType _) <- randomDefaultBucket
          getCounterBucket handle bucket `shouldReturn`
            Left (InvalidBucketTypeError bucketType)

      , testCase "bucket type not found" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          getCounterBucket handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getHyperLogLogBucket"
    [ testCase "success" $ do
        bucket <- randomHyperLogLogBucket
        getHyperLogLogBucket handle bucket `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-hll bucket" $ do
          bucket@(Bucket bucketType _) <- randomDefaultBucket
          getHyperLogLogBucket handle bucket `shouldReturn`
            Left (InvalidBucketTypeError bucketType)

      , testCase "bucket type not found" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          getHyperLogLogBucket handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getMapBucket"
    [ testCase "success" $ do
        bucket <- randomMapBucket
        getMapBucket handle bucket `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-map bucket" $ do
          bucket@(Bucket bucketType _) <- randomDefaultBucket
          getMapBucket handle bucket `shouldReturn`
            Left (InvalidBucketTypeError bucketType)

      , testCase "bucket type not found" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          getMapBucket handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getSetBucket"
    [ testCase "success" $ do
        bucket <- randomSetBucket
        getSetBucket handle bucket `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-set bucket" $ do
          bucket@(Bucket bucketType _) <- randomDefaultBucket
          getSetBucket handle bucket `shouldReturn`
            Left (InvalidBucketTypeError bucketType)

      , testCase "bucket type not found" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          getSetBucket handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "listKeys"
    [ testCase "empty bucket works for some reason" $
        listKeys handle (Bucket defaultBucketType "") `shouldReturn`
          Right []

    , testGroup "failures"
      [ testCase "bucket type not found" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          listKeys handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "queryBinaryIndex"
    [ testCase "one-elem index" $ do
        object <- randomObject
        let bucket = object ^. field @"key" . keyBucket
        idx <- randomByteString 32
        let object' = object & field @"content" . field @"indexes" .~ [BinaryIndex idx "x"]
        put handle object' `shouldReturnSatisfy` isRight
        queryBinaryIndex
          handle
          (BinaryIndexQuery { bucket = bucket, index = idx, minValue = "x", maxValue = "x" } )
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 1

    , testCase "in bucket" $ do
        let n = 10
        bucket <- randomObjectBucket
        replicateM_ n $
          put handle (emptyObject (generatedKey bucket)) `shouldReturnSatisfy` isRight
        queryBinaryIndex
          handle
          (inBucket bucket)
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right n

    , testCase "max > min" $ do
        bucket <- Bucket defaultBucketType <$> randomByteString 32
        idx <- randomByteString 32
        queryBinaryIndex
          handle
          BinaryIndexQuery
            { bucket = bucket
            , index = idx
            , minValue = "b"
            , maxValue = "a"
            }
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 0

    , testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucketType <- randomBucketType
          let bucket = Bucket bucketType "a"
          queryBinaryIndex
            handle
            (BinaryIndexQuery { bucket = bucket, index = "a", minValue = "a", maxValue = "a" })
            (Foldl.generalize Foldl.length)
            `shouldReturn` Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "queryBinaryIndexTerms"
    [ testCase "one-elem index" $ do
        object <- randomObject
        let bucket = object ^. field @"key" . keyBucket
        idx <- randomByteString 32
        let object' = object & field @"content" . field @"indexes" .~ [BinaryIndex idx "x"]
        put handle object' `shouldReturnSatisfy` isRight
        queryBinaryIndexTerms
          handle
          (BinaryIndexQuery { bucket = bucket, index = idx, minValue = "a", maxValue = "z" } )
          (Foldl.generalize Foldl.list)
          `shouldReturn` Right [("x", object ^. field @"key")]

    , testCase "in bucket" $ do
        let n = 10
        bucket <- randomObjectBucket
        replicateM_ n $
          put handle (emptyObject (generatedKey bucket)) `shouldReturnSatisfy` isRight
        queryBinaryIndexTerms
          handle
          (inBucket bucket)
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right n

    , testCase "keys between" $ do
        bucket <- randomByteString 32
        put handle (emptyObject (Key defaultBucketType bucket "3")) `shouldReturnSatisfy` isRight
        put handle (emptyObject (Key defaultBucketType bucket "4")) `shouldReturnSatisfy` isRight
        put handle (emptyObject (Key defaultBucketType bucket "5")) `shouldReturnSatisfy` isRight
        queryBinaryIndexTerms
          handle
          (keysBetween (Bucket defaultBucketType bucket) "1" "4")
          (Foldl.generalize Foldl.list)
          `shouldReturn` Right [("3", Key defaultBucketType bucket "3")
                               ,("4", Key defaultBucketType bucket "4")]

    , testCase "max > min" $ do
        bucket <- Bucket defaultBucketType <$> randomByteString 32
        idx <- randomByteString 32
        queryBinaryIndexTerms
          handle
          BinaryIndexQuery
            { bucket = bucket
            , index = idx
            , minValue = "b"
            , maxValue = "a"
            }
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 0

    , testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucketType <- randomBucketType
          let bucket = Bucket bucketType "a"
          queryBinaryIndexTerms
            handle
            (BinaryIndexQuery { bucket = bucket, index = "a", minValue = "a", maxValue = "a" })
            (Foldl.generalize Foldl.length)
            `shouldReturn` Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "queryIntIndex"
    [ testCase "empty index" $ do
        bucket <- randomObjectBucket
        idx <- randomByteString 32
        queryIntIndex
          handle
          (IntIndexQuery { bucket = bucket, index = idx, minValue = 1, maxValue = 1 } )
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 0

    , testCase "one-elem index" $ do
        object <- randomObject
        let bucket = object ^. field @"key" . keyBucket
        idx <- randomByteString 32
        let object' = object & field @"content" . field @"indexes" .~ [IntIndex idx 1]
        put handle object' `shouldReturnSatisfy` isRight
        queryIntIndex
          handle
          (IntIndexQuery { bucket = bucket, index = idx, minValue = 1, maxValue = 1 } )
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 1

    , testCase "max > min" $ do
        bucket <- Bucket defaultBucketType <$> randomByteString 32
        idx <- randomByteString 32
        queryIntIndex
          handle
          IntIndexQuery
            { bucket = bucket
            , index = idx
            , minValue = 2
            , maxValue = 1
            }
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 0

    , testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucketType <- randomBucketType
          let bucket = Bucket bucketType "a"
          queryIntIndex
            handle
            (IntIndexQuery { bucket = bucket, index = "a", minValue = 1, maxValue = 1 })
            (Foldl.generalize Foldl.length)
            `shouldReturn` Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "queryIntIndexTerms" [ ]

  , testGroup "resetBucket"
    [ testCase "non-existent bucket type works for some reason" $ do
        bucketType <- randomBucketType
        resetBucket handle (Bucket bucketType "a") `shouldReturn` Right ()

    , testCase "empty bucket works for some reason" $ do
        bucketType <- randomBucketType
        let bucket = Bucket bucketType ""
        resetBucket handle bucket `shouldReturn` Right ()
    ]

  , testGroup "setBucketIndex"
    [ testCase "success" $ do
        bucket <- randomDefaultBucket
        setBucketIndex handle bucket index3 `shouldReturn`
          Right ()

      , testCase "empty bucket works for some reason" $ do
          setBucketIndex handle (Bucket defaultBucketType "") index3 >>= print

    , testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          setBucketIndex handle bucket index3 `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)

      , testCase "index does not exist" $ do
          bucket <- randomDefaultBucket
          index <- unsafeMakeIndexName <$> randomText 32
          setBucketIndex handle bucket index `shouldReturn`
            Left (IndexDoesNotExistError index)

      , testCase "invalid index name" $ do
          bucket <- randomDefaultBucket
          setBucketIndex handle bucket (unsafeMakeIndexName "∙") >>= \case
            Left (UnknownError _) -> pure ()
            _ -> assertFailure ""

      , testCase "invalid n_val" $ do
          bucket <- randomDefaultBucket
          setBucketIndex handle bucket index1 `shouldReturn`
            Left InvalidNodesError
      ]
    ]

  , testGroup "unsetBucketIndex"
    [ testCase "success" $ do
        bucket <- randomDefaultBucket
        setBucketIndex handle bucket index3 `shouldReturn` Right ()
        unsetBucketIndex handle bucket `shouldReturn` Right ()

    , testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucket@(Bucket bucketType _) <- randomBucket
          unsetBucketIndex handle bucket `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]
  ]

riakBucketTypeTests :: Handle -> [TestTree]
riakBucketTypeTests handle =
  [ testGroup "getBucketType"
    [ testCase "object bucket type" $ do
        getBucketType handle defaultBucketType >>= \case
          Right SomeBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "counter bucket type" $ do
        getBucketType handle countersBucketType >>= \case
          Right SomeCounterBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "hll bucket type" $ do
        getBucketType handle hllsBucketType >>= \case
          Right SomeHyperLogLogBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "map bucket type" $ do
        getBucketType handle mapsBucketType >>= \case
          Right SomeMapBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "set bucket type" $ do
        getBucketType handle setsBucketType >>= \case
          Right SomeSetBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testGroup "failures"
      [ testCase "bucket type not found" $ do
          bucketType <- randomBucketType
          getBucketType handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getCounterBucketType"
    [ testCase "success" $ do
        getCounterBucketType handle countersBucketType `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-counter bucket type" $ do
          getCounterBucketType handle defaultBucketType `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "bucket type not found" $ do
          bucketType <- randomBucketType
          getCounterBucketType handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getHyperLogLogBucketType"
    [ testCase "success" $ do
        getHyperLogLogBucketType handle hllsBucketType `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-hll bucket type" $ do
          getHyperLogLogBucketType handle defaultBucketType `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "bucket type not found" $ do
          bucketType <- randomBucketType
          getHyperLogLogBucketType handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getMapBucketType"
    [ testCase "success" $ do
        getMapBucketType handle mapsBucketType `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-map bucket type" $ do
          getMapBucketType handle defaultBucketType `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "bucket type not found" $ do
          bucketType <- randomBucketType
          getMapBucketType handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "getSetBucketType"
    [ testCase "success" $ do
        getSetBucketType handle setsBucketType `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "non-set bucket type" $ do
          getSetBucketType handle defaultBucketType `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "bucket type not found" $ do
          bucketType <- randomBucketType
          getSetBucketType handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "setBucketTypeIndex"
    [ testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucketType <- randomBucketType
          setBucketTypeIndex handle bucketType index3 `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)

      , testCase "index does not exist" $ do
          index <- unsafeMakeIndexName <$> randomText 32
          setBucketTypeIndex handle countersBucketType index `shouldReturn`
            Left (IndexDoesNotExistError index)

      , testCase "invalid index name" $ do
          setBucketTypeIndex handle countersBucketType (unsafeMakeIndexName "∙") >>= \case
            Left (UnknownError _) -> pure ()
            _ -> assertFailure ""

      , testCase "invalid n_val" $ do
          setBucketTypeIndex handle countersBucketType index1 `shouldReturn`
            Left InvalidNodesError
      ]
    ]

  , testGroup "unsetBucketTypeIndex"
    [ testGroup "failures"
      [ testCase "bucket type does not exist" $ do
          bucketType <- randomBucketType
          unsetBucketTypeIndex handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)
      ]
    ]

  , testGroup "listBuckets"
    [ testGroup "failures"
      [ testCase "bucket type not found" $ do
          bucketType <- randomBucketType
          listBuckets handle bucketType `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)

      , testCase "empty bucket type" $ do
          listBuckets handle "" `shouldReturn`
            Left (BucketTypeDoesNotExistError "")
      ]
    ]
  ]

riakCounterTests :: Handle -> [TestTree]
riakCounterTests handle =
  [ testGroup "getCounter"
    [ testCase "default bucket succeeds for some reason" $ do
        key <- randomDefaultKey
        put handle (emptyObject key) `shouldReturn` Right key
        getCounter handle key `shouldReturnSatisfy` isRightJust
    ]

  , testGroup "incrementCounter"
    [ testCase "empty bucket works for some reason" $ do
        key <- (keyBucketSegment .~ "") <$> randomCounterKey
        incrementCounter handle (ConvergentCounter key 1) `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "default bucket" $ do
          key <- randomDefaultKey
          incrementCounter handle (ConvergentCounter key 1) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))

      , testCase "allow_mult=false (non-default)" $ do
          key <- randomNoSiblingsKey
          incrementCounter handle (ConvergentCounter key 1) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))

      , testCase "hll bucket" $ do
          key <- randomHyperLogLogKey
          incrementCounter handle (ConvergentCounter key 1) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "map bucket" $ do
          key <- randomMapKey
          incrementCounter handle (ConvergentCounter key 1) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "set bucket" $ do
          key <- randomSetKey
          incrementCounter handle (ConvergentCounter key 1) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))
      ]
    ]
  ]

riakHyperLogLogTests :: Handle -> [TestTree]
riakHyperLogLogTests handle =
  [ testGroup "getHyperLogLog"
    [ testCase "non-hll bucket succeeds for some reason" $ do
        key <- randomDefaultKey
        put handle (emptyObject key) `shouldReturn` Right key
        getHyperLogLog handle key `shouldReturnSatisfy` isRightJust
    ]

  , testGroup "updateHyperLogLog"
    [ testCase "empty bucket works for some reason" $ do
        key <- (keyBucketSegment .~ "") <$> randomHyperLogLogKey
        updateHyperLogLog handle (ConvergentHyperLogLog key ["a"]) `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "default bucket" $ do
          key <- randomDefaultKey
          updateHyperLogLog handle (ConvergentHyperLogLog key ["a"]) `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "allow_mult=false (non-default)" $ do
          key <- randomNoSiblingsKey
          updateHyperLogLog handle (ConvergentHyperLogLog key ["a"]) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))

      , testCase "counter bucket" $ do
          key <- randomCounterKey
          updateHyperLogLog handle (ConvergentHyperLogLog key ["a"]) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "map bucket" $ do
          key <- randomMapKey
          updateHyperLogLog handle (ConvergentHyperLogLog key ["a"]) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "set bucket" $ do
          key <- randomSetKey
          updateHyperLogLog handle (ConvergentHyperLogLog key ["a"]) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))
      ]
    ]
  ]

riakIndexTests :: Handle -> [TestTree]
riakIndexTests handle =
  [ testGroup "getIndex" []

  , testGroup "getIndexes" []

  , testGroup "putIndex" []

  , testGroup "deleteIndex"
    [ testCase "success" $ do
        index <- randomIndexName
        putIndex handle index defaultSchema def `shouldReturn` Right ()
        deleteIndex handle index `shouldReturn` Right True

    , testCase "delete nothing" $ do
        index <- randomIndexName
        deleteIndex handle index `shouldReturn` Right False

    , testGroup "failures"
      [ testCase "delete index with associated buckets" $ do
          index <- randomIndexName
          putIndex handle index defaultSchema def `shouldReturn` Right ()
          bucket1 <- randomDefaultBucket
          bucket2 <- randomObjectBucket
          setBucketIndex handle bucket1 index `shouldReturn` Right ()
          setBucketIndex handle bucket2 index `shouldReturn` Right ()
          deleteIndex handle index `shouldReturn`
            Left (IndexHasAssociatedBucketsError index [bucket1, bucket2])
      ]
    ]
  ]

riakMapTests :: Handle -> [TestTree]
riakMapTests handle =
  [ testGroup "getMap"
    [ testCase "non-map bucket succeeds for some reason" $ do
        key <- randomDefaultKey
        put handle (emptyObject key) `shouldReturn` Right key
        getMap handle key `shouldReturnSatisfy` isRightJust
    ]

  , testGroup "putMap"
    [ testCase "empty bucket works for some reason" $ do
        key <- (keyBucketSegment .~ "") <$> randomMapKey
        putMap handle (emptyMap key) `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "default bucket" $ do
          key <- randomDefaultKey
          putMap handle (emptyMap key) `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "allow_mult=false (non-default)" $ do
          key <- randomNoSiblingsKey
          putMap handle (emptyMap key) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))

      , testCase "counter bucket" $ do
          key <- randomCounterKey
          putMap handle (emptyMap key) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "hll bucket" $ do
          key <- randomHyperLogLogKey
          putMap handle (emptyMap key) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "set bucket" $ do
          key <- randomSetKey
          putMap handle (emptyMap key) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))
      ]
    ]
  ]

riakMapReduceTests :: Handle -> [TestTree]
riakMapReduceTests handle =
  [ testGroup "mapReduceBucket"
    [ testCase "default bucket" $ do
        key <- randomDefaultKey
        value <- randomByteString 32
        put handle (newObject key (newContent value)) `shouldReturnSatisfy` isRight
        mapReduceBucket handle (key ^. keyBucket) [mapPhaseObjectValue] (Foldl.generalize Foldl.list) `shouldReturn`
          Right [MapReduceResult
            { phase = 0
            , result = ErlList (Vector.fromList [ErlBinary value]) ErlNil
            }]

    , testCase "non-default bucket" $ do
        key <- randomObjectKey
        value <- randomByteString 32
        put handle (newObject key (newContent value)) `shouldReturnSatisfy` isRight
        mapReduceBucket handle (key ^. keyBucket) [mapPhaseObjectValue] (Foldl.generalize Foldl.list) `shouldReturn`
          Right [MapReduceResult
            { phase = 0
            , result = ErlList (Vector.fromList [ErlBinary value]) ErlNil
            }]
    ]

  , testGroup "mapReduceKeys"
    [ testCase "success" $ do
        key <- randomObjectKey
        value <- randomByteString 32
        put handle (newObject key (newContent value)) `shouldReturnSatisfy` isRight
        mapReduceKeys handle [key] [mapPhaseObjectValue] (Foldl.generalize Foldl.list) `shouldReturn`
          Right [MapReduceResult
            { phase = 0
            , result = ErlList (Vector.fromList [ErlBinary value]) ErlNil
            }]
    ]
  ]

riakObjectTests :: Handle -> [TestTree]
riakObjectTests handle =
  [ testGroup "get"
    [ testCase "404" $ do
        key <- randomObjectKey
        get handle key >>= \case
          Right Object { content = [] } -> pure ()
          result -> assertFailure (show result)

    , testCase "success" $ do
        object <- randomObject
        put handle object `shouldReturnSatisfy` isRight
        get handle (object ^. field @"key") >>= \case
          Right Object { content = [Sibling content] } ->
            (content ^. field @"value") `shouldBe`
              (object ^. field @"content" . field @"value")
          result -> assertFailure (show result)

    , testGroup "failures"
      [ testCase "bucket type not found" $ do
          key@(Key bucketType _ _) <- randomKey
          get handle key `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)

      , testCase "invalid nval" $ do
          key <- randomObjectKey
          getWith handle key ((def :: GetOpts) { nodes = Just 4 }) `shouldReturn`
            Left InvalidNodesError

      , testCase "empty key" $ do
          Bucket bucketType bucket <- randomObjectBucket
          let key = Key bucketType bucket ""
          get handle key `shouldReturn` Left (InvalidKeyError key)
      ]

    , testGroup "getHead"
      [ testCase "404" $ do
          key <- randomObjectKey
          getHead handle key >>= \case
            Right Object { content = [] } -> pure ()
            result -> assertFailure (show result)

      , testCase "success" $ do
          object <- randomObject
          put handle object `shouldReturnSatisfy` isRight
          getHead handle (object ^. field @"key") `shouldReturnSatisfy` isRight
      ]

    , testGroup "getIfModified"
      [ testCase "if modified (not modified)" $ do
          object <- randomObject
          put handle object `shouldReturnSatisfy` isRight
          get handle (object ^. field @"key") >>= \case
            Right object -> do
              getIfModified handle object `shouldReturnSatisfy` isRightNothing
            result -> assertFailure (show result)

      , testCase "if modified (modified)" $ do
          object <- randomObject
          put handle object `shouldReturnSatisfy` isRight
          get handle (object ^. field @"key") >>= \case
            Right object@(Object { content = [Sibling content] }) -> do
              let object' = object & field @"content" .~ content
              _ <- put handle object'
              getIfModified handle object' `shouldReturnSatisfy` isRightJust
            result -> assertFailure (show result)
      ]
    ]

  , testGroup "put"
    [ testCase "generated key" $ do
        bucket <- randomObjectBucket
        put handle (emptyObject (generatedKey bucket)) `shouldReturnSatisfy`
          isRight

    , testGroup "failures"
      [ testCase "bucket type not found" $ do
          key@(Key bucketType _ _) <- randomKey
          put handle (emptyObject key) `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)

      , testCase "empty bucket" $ do
          key <- Key defaultBucketType "" <$> randomByteString 32
          put handle (emptyObject key) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))

      , testCase "invalid nval" $ do
          key <- randomObjectKey
          putWith handle (emptyObject key) ((def :: PutOpts) { nodes = Just 4 }) `shouldReturn`
            Left InvalidNodesError
      ]
    ]

  , testGroup "putGet"
    [ testCase "success" $ do
        object <- randomObject
        putGet handle object >>= \case
          Right Object { content = _:|[] } -> pure ()
          result -> assertFailure (show result)

    , testCase "siblings" $ do
        object <- randomObject
        put handle object `shouldReturnSatisfy` isRight
        putGet handle object >>= \case
          Right Object { content = _:|[_] } -> pure ()
          result -> assertFailure (show result)
    ]

  , testGroup "putGetHead"
    [ testCase "success" $ do
        object  <- randomObject
        putGetHead handle object >>= \case
          Right Object { content = _:|[] } -> pure ()
          result -> assertFailure (show result)

    , testCase "siblings" $ do
        object <- randomObject
        put handle object `shouldReturnSatisfy` isRight
        putGetHead handle object >>= \case
          Right Object { content = _:|[_] } -> pure ()
          result -> assertFailure (show result)
    ]

  , testGroup "delete"
    [ testCase "non-existent object" $ do
        object <- randomObject
        delete handle object `shouldReturn` Right ()

    , testCase "tombstone" $ do
        object <- randomObject
        put handle object `shouldReturnSatisfy` isRight
        delete handle object { context = emptyContext } `shouldReturn` Right ()
        get handle (object ^. field @"key") >>= \case
          Right Object { content = [Tombstone _, Sibling _] } -> pure ()
          result -> assertFailure (show result)

    , testCase "no tombstone" $ do
        object <- randomObject
        putGet handle object >>= \case
          Right object' -> do
            delete handle object' `shouldReturn` Right ()
            get handle (object' ^. field @"key") >>= \case
              Right Object { content = [] } -> pure ()
              result -> assertFailure (show result)
          result -> assertFailure (show result)

    , testCase "empty key" $ do
        object <- randomObject
        let object' = object & field @"key" . keyKeySegment .~ ""
        delete handle object' `shouldReturn` Right ()

    , testGroup "failures"
      [ testCase "empty bucket" $ do
          key <- Key defaultBucketType "" <$> randomByteString 32
          delete handle (emptyObject key) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))
      ]
    ]
  ]

riakPingTests :: Handle -> [TestTree]
riakPingTests handle =
  [ testCase "ping" $ do
      ping handle `shouldReturn` Right (Right ())
  ]

riakServerInfoTests :: Handle -> [TestTree]
riakServerInfoTests handle =
  [ testGroup "getServerInfo"
    [ testCase "success" $ do
        getServerInfo handle `shouldReturnSatisfy` isRight
    ]
  ]

riakSetTests :: Handle -> [TestTree]
riakSetTests handle =
  [ testGroup "getSet"
    [ testCase "non-set bucket succeeds for some reason" $ do
        key <- randomDefaultKey
        put handle (emptyObject key) `shouldReturn` Right key
        getSet handle key `shouldReturnSatisfy` isRightJust
    ]

  , testGroup "putSet"
    [ testCase "empty bucket works for some reason" $ do
        key <- (keyBucketSegment .~ "") <$> randomSetKey
        putSet handle (emptySet key) `shouldReturnSatisfy` isRight

    , testGroup "failures"
      [ testCase "default bucket" $ do
          key <- randomDefaultKey
          putSet handle (emptySet key) `shouldReturn`
            Left (InvalidBucketTypeError defaultBucketType)

      , testCase "allow_mult=false (non-default)" $ do
          key <- randomNoSiblingsKey
          putSet handle (emptySet key) `shouldReturn`
            Left (InvalidBucketError (key ^. keyBucket))

      , testCase "counter bucket" $ do
          key <- randomCounterKey
          putSet handle (emptySet key) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "hll bucket" $ do
          key <- randomHyperLogLogKey
          putSet handle (emptySet key) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))

      , testCase "map bucket" $ do
          key <- randomMapKey
          putSet handle (emptySet key) `shouldReturn`
            Left (InvalidBucketTypeError (key ^. keyBucketType))
      ]
    ]
  ]

countersBucketType :: BucketType
countersBucketType =
  "counters"

hllsBucketType :: BucketType
hllsBucketType =
  "hlls"

mapsBucketType :: BucketType
mapsBucketType =
  "maps"

setsBucketType :: BucketType
setsBucketType =
  "sets"

index1 :: IndexName
index1 =
  unsafeMakeIndexName "default1"

index3 :: IndexName
index3 =
  unsafeMakeIndexName "default3"

emptyMap :: Key -> ConvergentMap ConvergentMapValue
emptyMap key =
  newMap key emptyMapValue

emptyObject :: Key -> Object (Content ByteString)
emptyObject key =
  newObject key emptyContent

emptySet :: Key -> ConvergentSet ByteString
emptySet key =
  newSet key mempty

emptyContent :: Content ByteString
emptyContent =
  newContent mempty

mapPhaseObjectValue :: MapReducePhase
mapPhaseObjectValue =
  MapPhase
    (CompiledFunction (ErlangFunctionId "riak_kv_mapreduce" "map_object_value"))
    (ErlAtomUtf8 "none")
    True

randomBucket :: IO Bucket
randomBucket =
  Bucket
    <$> randomByteString 32
    <*> randomByteString 32

randomBucketType :: IO BucketType
randomBucketType =
  randomByteString 32

randomByteString :: Int -> IO ByteString
randomByteString n =
  Latin1.pack <$> replicateM n (randomRIO ('a', 'z'))

randomCounterBucket :: IO Bucket
randomCounterBucket =
  Bucket "counters"
    <$> randomByteString 32

randomCounterKey :: IO Key
randomCounterKey =
  Key "counters"
    <$> randomByteString 32
    <*> randomByteString 32

randomDefaultBucket :: IO Bucket
randomDefaultBucket =
  Bucket defaultBucketType
    <$> randomByteString 32

randomDefaultKey :: IO Key
randomDefaultKey =
  Key defaultBucketType
    <$> randomByteString 32
    <*> randomByteString 32

randomHyperLogLogKey :: IO Key
randomHyperLogLogKey =
  Key "hlls"
    <$> randomByteString 32
    <*> randomByteString 32

randomHyperLogLogBucket :: IO Bucket
randomHyperLogLogBucket =
  Bucket "hlls"
    <$> randomByteString 32

randomIndexName :: IO IndexName
randomIndexName =
  unsafeMakeIndexName <$> randomText 32

randomKey :: IO Key
randomKey =
  Key
    <$> randomByteString 32
    <*> randomByteString 32
    <*> randomByteString 32

randomMapBucket :: IO Bucket
randomMapBucket =
  Bucket "maps"
    <$> randomByteString 32

randomMapKey :: IO Key
randomMapKey =
  Key "maps"
    <$> randomByteString 32
    <*> randomByteString 32

randomNoSiblingsKey :: IO Key
randomNoSiblingsKey =
  Key "nosiblings"
    <$> randomByteString 32
    <*> randomByteString 32

randomObject :: IO (Object (Content ByteString))
randomObject = do
  bucket <- randomByteString 32
  key <- randomByteString 32
  value <- randomByteString 6
  pure (newObject (Key "objects" bucket key) (newContent value))

randomObjectBucket :: IO Bucket
randomObjectBucket =
  Bucket "objects"
    <$> randomByteString 32

randomObjectKey :: IO Key
randomObjectKey =
  Key "objects"
    <$> randomByteString 32
    <*> randomByteString 32

randomSetKey :: IO Key
randomSetKey =
  Key "sets"
    <$> randomByteString 32
    <*> randomByteString 32

randomSetBucket :: IO Bucket
randomSetBucket =
  Bucket "sets"
    <$> randomByteString 32

randomText :: Int -> IO Text
randomText n =
  Text.pack <$> replicateM n (randomRIO ('a', 'z'))

isRightJust :: Either a (Maybe b) -> Bool
isRightJust = \case
  Right Just{} -> True
  _ -> False

isRightNothing :: Either a (Maybe b) -> Bool
isRightNothing = \case
  Right Nothing -> True
  _ -> False

shouldBe :: (Eq a, HasCallStack, Show a) => a -> a -> IO ()
shouldBe = (@?=)

shouldReturn :: (Eq a, HasCallStack, Show a) => IO a -> a -> IO ()
shouldReturn m x = m >>= (@?= x)

shouldReturnSatisfy :: (HasCallStack, Show a) => IO a -> (a -> Bool) -> IO ()
shouldReturnSatisfy m p = (`shouldSatisfy` p) =<< m

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy x f = assertBool ("Failed predicate on " ++ show x) (f x)
