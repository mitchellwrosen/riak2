{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Libriak.Connection      (Endpoint(..))
import RiakBinaryIndexQuery    (BinaryIndexQuery(..), inBucket)
import RiakBucket              (Bucket(..), queryBinaryIndex, queryIntIndex)
import RiakContent             (Content, newContent)
import RiakContext             (newContext)
import RiakError               (Error(..))
import RiakGetOpts             (GetOpts(..))
import RiakHandle              (EventHandlers(..), Handle, HandleConfig(..),
                                HandleError, withHandle)
import RiakIndexName           (unsafeMakeIndexName)
import RiakIntIndexQuery       (IntIndexQuery(..))
import RiakKey                 (Key(..), generatedKey, keyBucket)
import RiakObject              (Object(..), delete, get, getHead, getIfModified,
                                newObject, put, putGet, putGetHead)
import RiakPing                (ping)
import RiakPutOpts             (PutOpts(..))
import RiakSecondaryIndex      (SecondaryIndex(..))
import RiakSecondaryIndexValue (SecondaryIndexValue(..))
import RiakServerInfo          (ServerInfo(..), getServerInfo)
import RiakSibling             (Sibling(..))

import Control.Lens
import Control.Monad
import Data.ByteString       (ByteString)
import Data.Default.Class    (def)
import Data.Either           (isRight)
import Data.Generics.Product (field)
import Data.List.NonEmpty    (NonEmpty(..))
import Net.IPv4              (ipv4)
import System.Exit           (exitFailure)
import System.Random         (randomRIO)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Foldl         as Foldl
import qualified Data.ByteString.Char8 as Latin1

main :: IO ()
main = do
  withHandle
    config
    (\handle ->
      defaultMain
        (testGroup "Riak integration tests" (integrationTests handle)))

  where
    config :: HandleConfig
    config =
      HandleConfig
        { endpoint =
            Endpoint
              { address = ipv4 127 0 0 1
              , port = 8087
              }
        , reconnectSettings = \_ -> Nothing
        , handlers =
            mempty
            -- EventHandlers
            --   { onSend = \req -> putStrLn (">>> " ++ show req)
            --   , onReceive = \resp -> putStrLn ("<<< " ++ show resp)
            --   }
        }

integrationTests :: Handle -> [TestTree]
integrationTests handle =
  [ testGroup "RiakBucket" (riakBucketTests handle)
  , testGroup "RiakObject" (riakObjectTests handle)
  , testGroup "RiakPing" (riakPingTests handle)
  , testGroup "RiakServerInfo" (riakServerInfoTests handle)
  ]

riakBucketTests :: Handle -> [TestTree]
riakBucketTests handle =
  [ testGroup "exact query"
    [ testCase "empty index" $ do
        bucket <- randomObjectBucket
        idx <- randomByteString 32
        queryIntIndex
          handle
          (IntIndexQuery { bucket = bucket, index = idx, minValue = 1, maxValue = 1 } )
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right 0

    , testCase "int index" $ do
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

    , testCase "bin index" $ do
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
        replicateM_ n $ do
          put handle (newObject (generatedKey bucket) (newContent "")) def
        queryBinaryIndex
          handle
          (inBucket bucket)
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right n
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

    , testCase "bucket type not found" $ do
        bucketType <- randomByteString 32
        bucket <- randomByteString 32
        key <- randomByteString 32
        get handle (Key bucketType bucket key) def `shouldReturn`
          Left (BucketTypeDoesNotExistError bucketType)

    , testCase "invalid nval" $ do
        key <- randomObjectKey
        get handle key (def { nodes = Just 4 }) `shouldReturn`
          Left InvalidNodesError

    , testCase "head 404" $ do
        key <- randomObjectKey
        getHead handle key def >>= \case
          Right Object { content = [] } -> pure ()
          result -> assertFailure (show result)

    , testCase "head success" $ do
        object <- randomObject
        put handle object def `shouldReturnSatisfy` isRight
        getHead handle (object ^. field @"key") def `shouldReturnSatisfy` isRight

    , testCase "if modified (not modified)" $ do
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

  , testGroup "put"
    [ testCase "generated key" $ do
        bucket <- randomByteString 32
        let object = newObject (generatedKey (Bucket "objects" bucket)) (newContent "")
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

    , testCase "return body" $ do
        object <- randomObject
        putGet handle object def >>= \case
          Right Object { content = _:|[] } -> pure ()
          result -> assertFailure (show result)

    , testCase "return body (siblings)" $ do
        object <- randomObject
        put handle object def `shouldReturnSatisfy` isRight
        putGet handle object def >>= \case
          Right Object { content = _:|[_] } -> pure ()
          result -> assertFailure (show result)

    , testCase "return head" $ do
        object  <- randomObject
        putGetHead handle object def >>= \case
          Right Object { content = _:|[] } -> pure ()
          result -> assertFailure (show result)

    , testCase "return head (siblings)" $ do
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
  [ testCase "getServerInfo" $ do
      getServerInfo handle `shouldReturnSatisfy` isRight
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

-- randomText :: IO Text
-- randomText = Text.pack <$> replicateM 32 (randomRIO ('a', 'z'))

randomByteString :: Int -> IO ByteString
randomByteString n =
  Latin1.pack <$> replicateM n (randomRIO ('a', 'z'))

randomObjectBucket :: IO Bucket
randomObjectBucket =
  Bucket "objects"
    <$> randomByteString 32

randomObjectKey :: IO Key
randomObjectKey =
  Key "objects"
    <$> randomByteString 32
    <*> randomByteString 32

randomObject :: IO (Object (Content ByteString))
randomObject = do
  bucket <- randomByteString 32
  key <- randomByteString 32
  value <- randomByteString 6
  pure (newObject (Key "objects" bucket key) (newContent value))

-- randomObjectKeyIn :: ByteString -> IO RiakKey
-- randomObjectKeyIn bucket = do
--   key <- randomKeyName
--   pure (RiakKey (RiakBucket (RiakBucketType "objects") bucket) key)

-- randomObjectBucket :: IO RiakBucket
-- randomObjectBucket = do
--   bucket <- randomBucketName
--   pure (RiakBucket (RiakBucketType "objects") bucket)

-- randomCounterBucket :: IO RiakBucket
-- randomCounterBucket = do
--   RiakBucket (RiakBucketType "counters") <$> randomBucketName

-- randomCounterKey :: IO RiakKey
-- randomCounterKey =
--   RiakKey <$> randomCounterBucket <*> randomKeyName

-- randomSetBucket :: IO RiakBucket
-- randomSetBucket = do
--   RiakBucket (RiakBucketType "sets") <$> randomBucketName

-- randomSetKey :: IO RiakKey
-- randomSetKey =
--   RiakKey <$> randomSetBucket <*> randomKeyName

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
