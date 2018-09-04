{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings,
             TypeApplications #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString    (ByteString)
import Data.Maybe         (isJust)
import Data.Text          (Text)
import Lens.Family2
import System.Process
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf        (printf)

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
      bucket <- randomBucket
      key <- randomKey
      getRiakObject @Text h (objectKey bucket key) def `shouldReturn` Right []

  , testCase "get Text object w/o charset" $ do
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s" bucket key val)
      let loc = objectKey (Latin1.pack bucket) (Latin1.pack key)
      Right [RiakContent loc' val' ctype charset encoding vtag ts meta ixs deleted ttl] <-
        getRiakObject h loc def
      loc' `shouldBe` loc
      val' `shouldBe` Text.pack val
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
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s" bucket key val)
      let loc = objectKey (Latin1.pack bucket) (Latin1.pack key)
      Right [RiakContent loc' val' ctype charset encoding vtag ts meta ixs deleted ttl] <-
        getRiakObject h loc def
      loc' `shouldBe` loc
      val' `shouldBe` Latin1.pack val
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
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s" bucket key val)
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s" bucket key val)
      let loc = objectKey (Latin1.pack bucket) (Latin1.pack key)
      Right xs <- getRiakObject @Text h loc def
      length xs `shouldBe` 2

  , testCase "concurrent get/put" $ do
      let n = 1000 -- objects put+got per thread
      let t = 4    -- num threads
      done <- newEmptyMVar
      let
        go =
          replicateM_ n $ do
            bucket <- randomBucket
            key <- randomKey
            val <- Text.pack <$> randomKeyString
            let loc = objectKey bucket key
            Right () <- putRiakObject h loc val def
            Right [x] <- getRiakObject h loc def
            (x ^. L.value) `shouldBe` val
      replicateM_ t (forkFinally go (putMVar done))
      replicateM_ t (either throwIO (const (pure ())) =<< takeMVar done)

  , testCase "get object head" $ do
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s" bucket key val)
      let loc = objectKey (Latin1.pack bucket) (Latin1.pack key)
      Right [RiakContent loc' () ctype charset encoding vtag ts meta ixs deleted ttl] <-
        getRiakObjectHead h loc def
      loc' `shouldBe` loc
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
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s" bucket key val)
      let loc = objectKey (Latin1.pack bucket) (Latin1.pack key)
      Right (Modified [_]) <- getRiakObjectIfModified @ByteString h loc def
      pure ()

  , testCase "get object if modified (unmodified)" $ do
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s" bucket key val)
      let loc = objectKey (Latin1.pack bucket) (Latin1.pack key)
      Right [_] <- getRiakObjectHead h loc def -- cache it
      getRiakObjectIfModified @ByteString h loc def `shouldReturn` Right Unmodified

  -- , testCase "storing Text has correct content type, charset, and content encoding" $ do
  --     bucket <- randomBucket
  --     key <- randomKey
  --     let loc = defaultLocation bucket key
  --     putRiakObject h loc ("foo" :: Text) def `shouldReturn` Right ()
  --     Right [x] <- getRiakObject @Text h loc def
  --     (x ^. L.contentType) `shouldBe` ContentType "text/plain"
  ]

curl :: String -> IO ()
curl = callCommand . ("curl -s " ++)

randomBucket :: IO ByteString
randomBucket = Latin1.pack <$> randomBucketString

randomBucketString :: IO [Char]
randomBucketString = replicateM 32 (randomRIO ('a', 'z'))

randomKey :: IO ByteString
randomKey = Latin1.pack <$> randomKeyString

randomKeyString :: IO [Char]
randomKeyString = randomBucketString

objectKey :: ByteString -> ByteString -> RiakKey 'Nothing
objectKey bucket =
  RiakKey (RiakBucket (RiakBucketType "objects") bucket)

shouldBe :: (Eq a, HasCallStack, Show a) => a -> a -> IO ()
shouldBe = (@?=)

shouldReturn :: (Eq a, HasCallStack, Show a) => IO a -> a -> IO ()
shouldReturn m x = m >>= (@?= x)

shouldSatisfy :: (Eq a, HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy x f = assertBool "" (f x)
