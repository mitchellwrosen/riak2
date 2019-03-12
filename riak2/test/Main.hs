{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import RiakBinaryIndexQuery    (BinaryIndexQuery(..), inBucket)
import RiakBucket              (Bucket(..), getBucket, getCounterBucket,
                                getHyperLogLogBucket, getMapBucket,
                                getSetBucket, queryBinaryIndex, queryIntIndex,
                                setBucketIndex, unsetBucketIndex)
import RiakBucketProps         (BucketProps(..))
import RiakBucketType          (BucketType, defaultBucketType, getBucketType,
                                getCounterBucketType, getHyperLogLogBucketType,
                                getMapBucketType, getSetBucketType)
import RiakContent             (Content, newContent)
import RiakContext             (newContext)
import RiakError               (Error(..))
import RiakGetOpts             (GetOpts(..))
import RiakHandle              (EventHandlers(..), Handle, HandleConfig(..),
                                HandleError, createHandle)
import RiakIndex               (Index, deleteIndex, putIndex)
import RiakIndexName           (IndexName, unsafeMakeIndexName)
import RiakIntIndexQuery       (IntIndexQuery(..))
import RiakKey                 (Key(..), generatedKey, keyBucket)
import RiakObject              (Object(..), delete, get, getHead, getIfModified,
                                newObject, put, putGet, putGetHead)
import RiakPing                (ping)
import RiakPutOpts             (PutOpts(..))
import RiakSchema              (defaultSchema)
import RiakSecondaryIndex      (SecondaryIndex(..))
import RiakSecondaryIndexValue (SecondaryIndexValue(..))
import RiakServerInfo          (ServerInfo(..), getServerInfo)
import RiakSibling             (Sibling(..))
import RiakSomeBucketProps     (SomeBucketProps(..))

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

main :: IO ()
main = do
  handle <- createHandle config
  defaultMain (testGroup "Riak integration tests" (integrationTests handle))

  where
    config :: HandleConfig
    config =
      HandleConfig
        { endpoint =
            Endpoint
              { address = ipv4 127 0 0 1
              , port = 8087
              }
        , retries =
            3
        , healthCheckInterval =
            1
        , idleTimeout =
            30
        , requestTimeout =
            10
        , handlers =
            mempty
              -- { onSend = \req -> putStrLn (">>> " ++ show req)
              -- , onReceive = \resp -> putStrLn ("<<< " ++ show resp)
              -- }
        }

integrationTests :: Handle -> [TestTree]
integrationTests handle =
  [ testGroup "RiakBinaryIndexQuery" []
  , testGroup "RiakBucket" (riakBucketTests handle)
  , testGroup "RiakBucketProps" []
  , testGroup "RiakBucketType" (riakBucketTypeTests handle)
  , testGroup "RiakBucketTypeInternal" []
  , testGroup "RiakBusPool" []
  , testGroup "RiakContent" []
  , testGroup "RiakContext" []
  , testGroup "RiakConvergentCounter" []
  , testGroup "RiakConvergentHyperLogLog" []
  , testGroup "RiakConvergentMap" []
  , testGroup "RiakConvergentMapValue" []
  , testGroup "RiakConvergentSet" []
  , testGroup "RiakDebug" []
  , testGroup "RiakDeleteOpts" []
  , testGroup "RiakErlangTerm" []
  , testGroup "RiakError" []
  , testGroup "RiakGetOpts" []
  , testGroup "RiakHandle" []
  , testGroup "RiakIndex" (riakIndexTests handle)
  , testGroup "RiakIndexName" []
  , testGroup "RiakIntIndexQuery" []
  , testGroup "RiakKey" []
  , testGroup "RiakManagedBus" []
  , testGroup "RiakMapReduce" []
  , testGroup "RiakMapReduceFunction" []
  , testGroup "RiakMapReducePhase" []
  , testGroup "RiakObject" (riakObjectTests handle)
  , testGroup "RiakPing" (riakPingTests handle)
  , testGroup "RiakPutOpts" []
  , testGroup "RiakQuorum" []
  , testGroup "RiakReadQuorum" []
  , testGroup "RiakSTM" []
  , testGroup "RiakSchema" []
  , testGroup "RiakSearch" []
  , testGroup "RiakSecondaryIndex" []
  , testGroup "RiakSecondaryIndexValue" []
  , testGroup "RiakServerInfo" (riakServerInfoTests handle)
  , testGroup "RiakSibling" []
  , testGroup "RiakWriteQuorum" []
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

  , testGroup "listKeys" [ ]

  , testGroup "queryBinaryIndex"
    [ testCase "one-elem index" $ do
        object <- randomObject
        let bucket = object ^. field @"key" . keyBucket
        idx <- randomByteString 32
        let object' = object & field @"content" . field @"indexes" .~ [SecondaryIndex idx (Binary "x")]
        put handle object' def `shouldReturnSatisfy` isRight
        queryBinaryIndex
          handle
          (BinaryIndexQuery { bucket = bucket, index = idx, minValue = "x", maxValue = "x" } )
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 1

    , testCase "in bucket" $ do
        let n = 10
        bucket <- randomObjectBucket
        let object = newObject (generatedKey bucket) (newContent "")
        replicateM_ n $
          put handle object def `shouldReturnSatisfy` isRight
        queryBinaryIndex
          handle
          (inBucket bucket)
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right n
    , testGroup "failures"
      [ ]
    ]

  , testGroup "queryBinaryIndexTerms" [ ]

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
        let object' = object & field @"content" . field @"indexes" .~ [SecondaryIndex idx (Integer 1)]
        put handle object' def `shouldReturnSatisfy` isRight
        queryIntIndex
          handle
          (IntIndexQuery { bucket = bucket, index = idx, minValue = 1, maxValue = 1 } )
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 1

    , testGroup "failures"
      [ ]
    ]

  , testGroup "queryIntIndexTerms" [ ]

  , testGroup "resetBucket" [ ]

  , testGroup "setBucketIndex"
    [ testCase "success" $ do
        bucket <- randomDefaultBucket
        setBucketIndex handle bucket index3 `shouldReturn`
          Right ()

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

      , testCase "invalid n_val" $ do
          bucket <- randomDefaultBucket
          setBucketIndex handle bucket index1 `shouldReturn`
            Left InvalidNodesError
      ]
    ]

  , testGroup "streamKeys" [ ]

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
        getBucketType handle counterBucketType >>= \case
          Right SomeCounterBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "hll bucket type" $ do
        getBucketType handle hyperLogLogBucketType >>= \case
          Right SomeHyperLogLogBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "map bucket type" $ do
        getBucketType handle mapBucketType >>= \case
          Right SomeMapBucketProps{} -> pure ()
          result -> assertFailure (show result)

    , testCase "set bucket type" $ do
        getBucketType handle setBucketType >>= \case
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
        getCounterBucketType handle counterBucketType `shouldReturnSatisfy` isRight

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
        getHyperLogLogBucketType handle hyperLogLogBucketType `shouldReturnSatisfy` isRight

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
        getMapBucketType handle mapBucketType `shouldReturnSatisfy` isRight

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
        getSetBucketType handle setBucketType `shouldReturnSatisfy` isRight

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

  , testGroup "setBucketTypeIndex" []
  , testGroup "unsetBucketTypeIndex" []
  , testGroup "listBuckets" []
  , testGroup "streamBuckets" []
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

riakObjectTests :: Handle -> [TestTree]
riakObjectTests handle =
  [ testGroup "get"
    [ testCase "404" $ do
        key <- randomObjectKey
        get handle key def >>= \case
          Right Object { content = [] } -> pure ()
          result -> assertFailure (show result)

    , testCase "success" $ do
        object <- randomObject
        put handle object def `shouldReturnSatisfy` isRight
        get handle (object ^. field @"key") def >>= \case
          Right Object { content = [Sibling content] } ->
            (content ^. field @"value") `shouldBe`
              (object ^. field @"content" . field @"value")
          result -> assertFailure (show result)

    , testGroup "failures"
      [ testCase "bucket type not found" $ do
          key@(Key bucketType _ _) <- randomKey
          get handle key def `shouldReturn`
            Left (BucketTypeDoesNotExistError bucketType)

      , testCase "invalid nval" $ do
          key <- randomObjectKey
          get handle key (def { nodes = Just 4 }) `shouldReturn`
            Left InvalidNodesError
      ]

    , testGroup "getHead"
      [ testCase "404" $ do
          key <- randomObjectKey
          getHead handle key def >>= \case
            Right Object { content = [] } -> pure ()
            result -> assertFailure (show result)

      , testCase "success" $ do
          object <- randomObject
          put handle object def `shouldReturnSatisfy` isRight
          getHead handle (object ^. field @"key") def `shouldReturnSatisfy` isRight
      ]

    , testGroup "getIfModified"
      [ testCase "if modified (not modified)" $ do
          object <- randomObject
          put handle object def `shouldReturnSatisfy` isRight
          get handle (object ^. field @"key") def >>= \case
            Right object -> do
              getIfModified handle object def `shouldReturnSatisfy` isRightNothing
            result -> assertFailure (show result)

      , testCase "if modified (modified)" $ do
          object <- randomObject
          put handle object def `shouldReturnSatisfy` isRight
          get handle (object ^. field @"key") def >>= \case
            Right object@(Object { content = [Sibling content] }) -> do
              let object' = object & field @"content" .~ content
              _ <- put handle object' def
              getIfModified handle object' def `shouldReturnSatisfy` isRightJust
            result -> assertFailure (show result)
      ]
    ]

  , testGroup "put"
    [ testCase "generated key" $ do
        bucket <- randomObjectBucket
        let object = newObject (generatedKey bucket) (newContent "")
        put handle object def `shouldReturnSatisfy` isRight

    , testCase "bucket type not found" $ do
        bucketType <- randomByteString 32
        bucket <- randomByteString 32
        key <- randomByteString 32
        let object = newObject (Key bucketType bucket key) (newContent "")
        put handle object def `shouldReturn`
          Left (BucketTypeDoesNotExistError bucketType)

    , testCase "invalid nval" $ do
        key <- randomObjectKey
        let object = newObject key (newContent "")
        put handle object (def { nodes = Just 4 }) `shouldReturn`
          Left InvalidNodesError
    ]

  , testGroup "putGet"
    [ testCase "success" $ do
        object <- randomObject
        putGet handle object def >>= \case
          Right Object { content = _:|[] } -> pure ()
          result -> assertFailure (show result)

    , testCase "siblings" $ do
        object <- randomObject
        put handle object def `shouldReturnSatisfy` isRight
        putGet handle object def >>= \case
          Right Object { content = _:|[_] } -> pure ()
          result -> assertFailure (show result)
    ]

  , testGroup "putGetHead"
    [ testCase "success" $ do
        object  <- randomObject
        putGetHead handle object def >>= \case
          Right Object { content = _:|[] } -> pure ()
          result -> assertFailure (show result)

    , testCase "siblings" $ do
        object <- randomObject
        put handle object def `shouldReturnSatisfy` isRight
        putGetHead handle object def >>= \case
          Right Object { content = _:|[_] } -> pure ()
          result -> assertFailure (show result)
    ]

  , testGroup "delete"
    [ testCase "tombstone" $ do
        object <- randomObject
        put handle object def `shouldReturnSatisfy` isRight
        delete handle object { context = newContext } def `shouldReturn` Right ()
        get handle (object ^. field @"key") def >>= \case
          Right Object { content = [Tombstone _, Sibling _] } -> pure ()
          result -> assertFailure (show result)

    , testCase "no tombstone" $ do
        object <- randomObject
        putGet handle object def >>= \case
          Right object' -> do
            delete handle object' def `shouldReturn` Right ()
            get handle (object' ^. field @"key") def >>= \case
              Right Object { content = [] } -> pure ()
              result -> assertFailure (show result)
          result -> assertFailure (show result)
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

--   , testCase "exact int query" $ do
--       let ixname = RiakIndexName "foo"
--       ns <- replicateM 100 (randomRIO (-2, 2))
--       b <- randomBucketName
--       for_ ns $ \n -> do
--         key <- randomObjectKeyIn b
--         val <- randomText

--         putRiakObject h key val
--           (def & #indexes [RiakIndexInt ixname n])
--             `shouldReturn` Right ()

--       for_ [(-2)..2] $ \i -> do
--         riakExactQuery h
--           (RiakBucket (RiakBucketType "objects") b)
--           (RiakExactQueryInt ixname i)
--           (Foldl.generalize Foldl.length)
--           `shouldReturn` Right (length (filter (== i) ns))

--   , testCase "range int query" $ do
--       let ixname = RiakIndexName "foo"
--       ns <- replicateM 100 (randomRIO (1, 5))
--       b <- randomBucketName
--       for_ ns $ \n -> do
--         key <- randomObjectKeyIn b
--         val <- randomText

--         putRiakObject h key val
--           (def & #indexes [RiakIndexInt ixname n])
--             `shouldReturn` Right ()

--       for_ [1..4] $ \i -> do
--         riakRangeQuery h
--           (RiakBucket (RiakBucketType "objects") b)
--           (RiakRangeQueryInt ixname i (i+1))
--           (Foldl.generalize Foldl.length)
--           `shouldReturn` Right (length (filter (\j -> j == i || j == i+1) ns))

--   , testCase "get empty counter returns Nothing" $ do
--       key <- randomCounterKey
--       getRiakCounter h key def `shouldReturn` Right Nothing

--   , testCase "update/get counter" $ do
--       key <- randomCounterKey
--       updates <- randomRIO (1, 10)
--       incrs <- replicateM updates (randomRIO (1, 10))
--       for_ incrs $ \i -> updateRiakCounter h key i def `shouldReturn` Right 0
--       getRiakCounter h key def `shouldReturn` Right (Just (sum incrs))

--   , testCase "update counter, return body" $ do
--       let i = 10
--       key <- randomCounterKey
--       updateRiakCounter h key i (def & #return_body True) `shouldReturn` Right i

--   , testCase "new counter" $ do
--       let i = 10
--       bucket <- randomCounterBucket
--       Right key <- updateNewRiakCounter h bucket i def
--       getRiakCounter h key def `shouldReturn` Right (Just i)

--   , testCase "get counter in object bucket returns Left" $ do
--       key <- randomObjectKey
--       getRiakCounter h key def `shouldReturn`
--         Left (RiakError "`undefined` is not a supported type")

--   , testCase "update counter in object bucket returns Left" $ do
--       key <- randomObjectKey
--       updateRiakCounter h key 1 def `shouldReturn`
--         Left (RiakError "Bucket datatype `undefined` is not a supported type")

--   , testCase "get counter in non-counter bucket type throws error" $ do
--       key <- randomSetKey
--       try (getRiakCounter h key def) `shouldReturn`
--         Left (RiakCrdtError key "expected counter but found set")

--   , testCase "update counter in non-counter bucket returns Left" $ do
--       key <- randomSetKey
--       updateRiakCounter h key 1 def `shouldReturn`
--         Left (RiakError "Operation type is `counter` but  bucket type is `set`.")

--   , testCase "new set" $ do
--       bucket <- randomSetBucket
--       let op = riakSetAddOp ("foo" :: ByteString)
--       Right key <- updateNewRiakSet h bucket op def
--       getRiakSet @ByteString h key def `shouldReturn` Right (Just (Set.fromList ["foo"]))

--   , testCase "set add-remove" $ do
--       bucket <- randomSetBucket
--       let val = "foo" :: ByteString
--       Right key <- updateNewRiakSet h bucket (riakSetAddOp val) def
--       updateRiakSet h key (riakSetRemoveOp val) def `shouldReturn` Right mempty

--   , testCase "updating a new set with return_body caches its context" $ do
--       bucket <- randomSetBucket
--       let op = riakSetAddOp ("foo" :: ByteString)
--       Right key <- updateNewRiakSet h bucket op (def & #return_body True)
--       riakCacheLookup (riakHandleCache h) key `shouldReturnSatisfy` isJust

--   , testCase "reading a CRDT as an object does not overwrite its cached context" $ do
--       bucket <- randomSetBucket
--       let op = riakSetAddOp ("foo" :: ByteString)
--       Right key <- updateNewRiakSet h bucket op (def & #return_body True)
--       Just context1 <- riakCacheLookup (riakHandleCache h) key
--       Right [_] <- getRiakObjectHead h key def
--       Just context2 <- riakCacheLookup (riakHandleCache h) key
--       context1 `shouldBe` context2
--   ]


-- curl :: String -> IO ()
-- curl = callCommand . ("curl -s " ++)

-- curlPutByteString :: RiakKey -> ByteString -> IO ()
-- curlPutByteString (RiakKey (RiakBucket (RiakBucketType type') bucket) key) val =
--   curl $
--     printf
--       "-XPUT localhost:8098/types/%s/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s"
--       (Latin1.unpack type')
--       (Latin1.unpack bucket)
--       (Latin1.unpack key)
--       (Latin1.unpack val)

-- curlPutText :: RiakKey -> Text -> IO ()
-- curlPutText (RiakKey (RiakBucket (RiakBucketType type') bucket) key) val =
--   curl $
--     printf
--       "-XPUT localhost:8098/types/%s/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s"
--       (Latin1.unpack type')
--       (Latin1.unpack bucket)
--       (Latin1.unpack key)
--       (Text.unpack val)

-- riakHandleCache :: RiakHandle -> RiakCache
-- riakHandleCache (RiakHandle _ cache) = cache

-- randomBucketName :: IO ByteString
-- randomBucketName = Latin1.pack <$> randomBucketNameString

-- randomBucketNameString :: IO [Char]
-- randomBucketNameString = replicateM 32 (randomRIO ('a', 'z'))

-- randomKeyName :: IO ByteString
-- randomKeyName = Latin1.pack <$> randomKeyNameString

-- randomKeyNameString :: IO [Char]
-- randomKeyNameString = randomBucketNameString

counterBucketType :: BucketType
counterBucketType =
  "counters"

hyperLogLogBucketType :: BucketType
hyperLogLogBucketType =
  "hlls"

mapBucketType :: BucketType
mapBucketType =
  "maps"

setBucketType :: BucketType
setBucketType =
  "sets"

index1 :: IndexName
index1 =
  unsafeMakeIndexName "default1"

index3 :: IndexName
index3 =
  unsafeMakeIndexName "default3"

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

randomDefaultBucket :: IO Bucket
randomDefaultBucket =
  Bucket defaultBucketType
    <$> randomByteString 32

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
