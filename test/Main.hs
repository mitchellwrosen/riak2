{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedLabels, OverloadedStrings,
             TypeApplications #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString    (ByteString)
import Data.Foldable
import Data.Maybe         (isJust)
import Data.Text          (Text)
import Lens.Family2
import System.Process
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf        (printf)

import qualified Control.Foldl         as Foldl
import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Text             as Text

import           Riak
import qualified Riak.Lenses as L

main :: IO ()
main = do
  riakh <- createRiakHandle "localhost" 8087
  defaultMain (testGroup "Tests" (tests riakh))

tests :: RiakHandle -> [TestTree]
tests h =
  [ testCase "ping" $ do
      pingRiak h `shouldReturn` Right ()

  , testCase "get server info" $ do
      Right _ <- getRiakServerInfo h
      pure ()

  , testCase "get object 404" $ do
      key <- randomObjectKey
      getRiakObject @Text h key def `shouldReturn` Right []

  , testCase "get Text object w/o charset" $ do
      key <- randomObjectKey
      let val = "foo"
      curlPutText  key val
      Right [RiakObject key' val' ctype charset encoding vtag ts meta ixs deleted ttl] <-
        getRiakObject h key def
      key' `shouldBe` key
      val' `shouldBe` val
      ctype `shouldBe` Just (ContentType "text/plain")
      charset `shouldBe` Nothing
      encoding `shouldBe` Nothing
      vtag `shouldSatisfy` isJust
      ts `shouldSatisfy` isJust
      meta `shouldBe` RiakMetadata []
      ixs `shouldBe` []
      deleted `shouldBe` False
      ttl `shouldBe` TTL Nothing

  , testCase "get ByteString object" $ do
      key <- randomObjectKey
      let val = "foo"
      curlPutByteString key val
      Right [RiakObject key' val' ctype charset encoding vtag ts meta ixs deleted ttl] <-
        getRiakObject h key def
      key' `shouldBe` key
      val' `shouldBe` val
      ctype `shouldBe` Just (ContentType "application/octet-stream")
      charset `shouldBe` Nothing
      encoding `shouldBe` Nothing
      vtag `shouldSatisfy` isJust
      ts `shouldSatisfy` isJust
      meta `shouldBe` RiakMetadata []
      ixs `shouldBe` []
      deleted `shouldBe` False
      ttl `shouldBe` TTL Nothing

  , testCase "get two siblings" $ do
      key <- randomObjectKey
      let val = "foo"
      curlPutText key val
      curlPutText key val
      Right xs <- getRiakObject @Text h key def
      length xs `shouldBe` 2

  , testCase "concurrent get/put" $ do
      let n = 250 -- objects put+got per thread
      let t = 4   -- num threads
      done <- newEmptyMVar
      let
        go =
          replicateM_ n $ do
            key <- randomObjectKey
            val <- Text.pack <$> randomKeyNameString
            Right () <- putRiakObject h key val def
            Right [x] <- getRiakObject h key def
            (x ^. L.value) `shouldBe` val
      replicateM_ t (forkFinally go (putMVar done))
      replicateM_ t (either throwIO (const (pure ())) =<< takeMVar done)

  , testCase "get object head" $ do
      key <- randomObjectKey
      curlPutByteString key "foo"
      Right [RiakObject key' () ctype charset encoding vtag ts meta ixs deleted ttl] <-
        getRiakObjectHead h key def
      key' `shouldBe` key
      ctype `shouldBe` Just (ContentType "application/octet-stream")
      charset `shouldBe` Nothing
      encoding `shouldBe` Nothing
      vtag `shouldSatisfy` isJust
      ts `shouldSatisfy` isJust
      meta `shouldBe` RiakMetadata []
      ixs `shouldBe` []
      deleted `shouldBe` False
      ttl `shouldBe` TTL Nothing

  , testCase "get object if modified (modified)" $ do
      key <- randomObjectKey
      curlPutByteString key "foo"
      Right (Modified [_]) <- getRiakObjectIfModified @ByteString h key def
      pure ()

  , testCase "get object if modified (unmodified)" $ do
      key <- randomObjectKey
      curlPutByteString key "foo"
      Right [_] <- getRiakObjectHead h key def -- cache it
      getRiakObjectIfModified @ByteString h key def `shouldReturn`
        Right Unmodified

  , testCase "exact int query" $ do
      let ixname = RiakIndexName "foo"
      ns <- replicateM 100 (randomRIO (1, 5))
      b <- randomBucketName
      for_ ns $ \n -> do
        key <- randomObjectKeyIn b
        val <- randomText

        putRiakObject h key val
          (def & #indexes [RiakIndexInt ixname n])
            `shouldReturn` Right ()

      for_ [1..5] $ \i -> do
        riakExactQuery h
          (RiakBucket (RiakBucketType "objects") b)
          (RiakExactQueryInt ixname i)
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right (length (filter (== i) ns))

  , testCase "range int query" $ do
      let ixname = RiakIndexName "foo"
      ns <- replicateM 100 (randomRIO (1, 5))
      b <- randomBucketName
      for_ ns $ \n -> do
        key <- randomObjectKeyIn b
        val <- randomText

        putRiakObject h key val
          (def & #indexes [RiakIndexInt ixname n])
            `shouldReturn` Right ()

      for_ [1..4] $ \i -> do
        riakRangeQuery h
          (RiakBucket (RiakBucketType "objects") b)
          (RiakRangeQueryInt ixname i (i+1))
          (Foldl.generalize Foldl.length)
          `shouldReturn` Right (length (filter (\j -> j == i || j == i+1) ns))

  -- , testCase "storing Text has correct content type, charset, and content encoding" $ do
  --     bucket <- randomBucketName
  --     key <- randomKeyName
  --     let loc = defaultLocation bucket key
  --     putRiakObject h loc ("foo" :: Text) def `shouldReturn` Right ()
  --     Right [x] <- getRiakObject @Text h loc def
  --     (x ^. L.contentType) `shouldBe` ContentType "text/plain"
  ]

curl :: String -> IO ()
curl = callCommand . ("curl -s " ++)

curlPutByteString :: RiakKey 'Nothing -> ByteString -> IO ()
curlPutByteString (RiakKey (RiakBucket (RiakBucketType type') bucket) key) val =
  curl $
    printf
      "-XPUT localhost:8098/types/%s/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s"
      (Latin1.unpack type')
      (Latin1.unpack bucket)
      (Latin1.unpack key)
      (Latin1.unpack val)

curlPutText :: RiakKey 'Nothing -> Text -> IO ()
curlPutText (RiakKey (RiakBucket (RiakBucketType type') bucket) key) val =
  curl $
    printf
      "-XPUT localhost:8098/types/%s/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s"
      (Latin1.unpack type')
      (Latin1.unpack bucket)
      (Latin1.unpack key)
      (Text.unpack val)

randomBucketName :: IO ByteString
randomBucketName = Latin1.pack <$> randomBucketNameString

randomBucketNameString :: IO [Char]
randomBucketNameString = replicateM 32 (randomRIO ('a', 'z'))

randomKeyName :: IO ByteString
randomKeyName = Latin1.pack <$> randomKeyNameString

randomKeyNameString :: IO [Char]
randomKeyNameString = randomBucketNameString

randomText :: IO Text
randomText = Text.pack <$> replicateM 32 (randomRIO ('a', 'z'))

randomObjectKey :: IO (RiakKey 'Nothing)
randomObjectKey = do
  bucket <- randomBucketName
  key <- randomKeyName
  pure (RiakKey (RiakBucket (RiakBucketType "objects") bucket) key)

randomObjectKeyIn :: ByteString -> IO (RiakKey 'Nothing)
randomObjectKeyIn bucket = do
  key <- randomKeyName
  pure (RiakKey (RiakBucket (RiakBucketType "objects") bucket) key)

shouldBe :: (Eq a, HasCallStack, Show a) => a -> a -> IO ()
shouldBe = (@?=)

shouldReturn :: (Eq a, HasCallStack, Show a) => IO a -> a -> IO ()
shouldReturn m x = m >>= (@?= x)

shouldSatisfy :: (Eq a, HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy x f = assertBool "" (f x)
