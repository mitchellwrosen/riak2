{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Riak
import Riak.Handle.Impl.Exclusive (Config(..), EventHandlers(..), Socket,
                                withHandle)

import qualified Riak.Handle.Impl.Exclusive as Handle

import Control.Concurrent
import Control.Lens
import Data.Either           (isRight)
import Data.Generics.Product (field)
import Data.List.NonEmpty    (NonEmpty(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Random as ByteString

main :: IO ()
main = do
  socket :: Socket <-
    Handle.new1 "localhost" 8087

  let
    config :: Config
    config =
      Config
        { socket = socket
        , handlers =
            mempty
            -- EventHandlers
            --   { onSend = print
            --   , onReceive = print
            --   }
        }

  withHandle config $ \client ->
    defaultMain (testGroup "Riak integration tests" (integrationTests client))

integrationTests :: Handle -> [TestTree]
integrationTests client =
  [ testGroup "Riak"
    [ testCase "ping" $ do
        ping client `shouldReturn` Right ()
    ]

  , testGroup "Riak.Object"
    [ testGroup "get"
      [ testCase "404" $ do
          key <- randomKey
          get client key def `shouldReturn` Right []

      , testCase "success" $ do
          bucket <- ByteString.random 32
          value <- ByteString.random 6
          let content = newContent (generatedKey (Bucket "objects" bucket)) value
          Right key <- put client content def
          get client key def >>= \case
            Left err -> assertFailure (show err)
            Right [Object { content }] -> do
              (content ^. field @"value") `shouldBe` value
            _ -> undefined

      , testCase "head 404" $ do
          key <- randomKey
          getHead client key def `shouldReturn` Right []

      , testCase "head success" $ do
          bucket <- ByteString.random 32
          value <- ByteString.random 6
          let content = newContent (generatedKey (Bucket "objects" bucket)) value
          Right key <- put client content def
          getHead client key def `shouldReturnSatisfy` isRight

      , testCase "if modified (not modified)" $ do
          bucket <- ByteString.random 32
          value <- ByteString.random 6
          let content = newContent (generatedKey (Bucket "objects" bucket)) value
          Right key <- put client content def
          get client key def >>= \case
            Left err -> assertFailure (show err)
            Right [object] -> do
              getIfModified client object def `shouldReturnSatisfy` isRightNothing
            _ -> undefined

      , testCase "if modified (modified)" $ do
          bucket <- ByteString.random 32
          value <- ByteString.random 6
          let content = newContent (generatedKey (Bucket "objects" bucket)) value
          Right key <- put client content def
          get client key def >>= \case
            Left err -> assertFailure (show err)
            Right [object] -> do
              _ <- put client object def
              getIfModified client object def `shouldReturnSatisfy` isRightJust
            _ -> undefined

      , testCase "put" $ do
          bucket <- ByteString.random 32
          value <- ByteString.random 6
          let content = newContent (generatedKey (Bucket "objects" bucket)) value
          put client content def `shouldReturnSatisfy` isRight

      , testCase "putGet" $ do
          key <- randomKey
          value <- ByteString.random 6
          let content = newContent key value
          putGet client content def >>= \case
            Left err -> assertFailure (show err)
            Right (_:|[]) -> pure ()
            result -> assertFailure (show result)

      , testCase "putGet (siblings)" $ do
          key <- randomKey
          value <- ByteString.random 6
          let content = newContent key value
          put client content def `shouldReturnSatisfy` isRight
          putGet client content def >>= \case
            Left err -> assertFailure (show err)
            Right (_:|[_]) -> pure ()
            result -> assertFailure (show result)

      , testCase "putGetHead" $ do
          key <- randomKey
          value <- ByteString.random 6
          let content = newContent key value
          putGetHead client content def >>= \case
            Left err -> assertFailure (show err)
            Right (_:|[]) -> pure ()
            result -> assertFailure (show result)

      , testCase "putGetHead (siblings)" $ do
          key <- randomKey
          value <- ByteString.random 6
          let content = newContent key value
          put client content def `shouldReturnSatisfy` isRight
          putGetHead client content def >>= \case
            Left err -> assertFailure (show err)
            Right (_:|[_]) -> pure ()
            result -> assertFailure (show result)

      , testCase "delete" $ do
          key <- randomKey
          value <- ByteString.random 6
          let content = (newContent key value) { metadata = [("foo", Just "bar")], type' = Just "wawa" }
          -- Right (object:|_) <- putGetHead client content def
          -- delete client object `shouldReturn` Right ()
          _ <- put client content def
          delete client content `shouldReturn` Right ()
          get client key def `shouldReturn` Right []
      ]
    ]

  , testGroup "Riak.ServerInfo"
    [ testCase "getServerInfo" $ do
        getServerInfo client `shouldReturn`
          Right ServerInfo
            { name = "riak@172.17.0.2"
            , version = "2.2.3"
            }
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

-- randomText :: IO Text
-- randomText = Text.pack <$> replicateM 32 (randomRIO ('a', 'z'))

randomKey :: IO Key
randomKey =
  Key "objects"
    <$> ByteString.random 32
    <*> ByteString.random 32

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
