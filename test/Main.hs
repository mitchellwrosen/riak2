{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings,
             TypeApplications #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
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

  , testCase "fetch object 404" $ do
      bucket <- randomBucket
      key <- randomKey
      fetchRiakObject @Text h (objectLoc bucket key) def `shouldReturn` Right []

  , testCase "fetch Text object w/o charset" $ do
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s" bucket key val)
      let loc = objectLoc (RiakBucket (Latin1.pack bucket)) (RiakKey (Latin1.pack key))
      Right [RiakContent loc' val' ctype charset encoding vtag ts meta ixs deleted ttl] <-
        fetchRiakObject h loc def
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

  , testCase "fetch ByteString object" $ do
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: application/octet-stream' -d %s" bucket key val)
      let loc = objectLoc (RiakBucket (Latin1.pack bucket)) (RiakKey (Latin1.pack key))
      Right [RiakContent loc' val' ctype charset encoding vtag ts meta ixs deleted ttl] <-
        fetchRiakObject h loc def
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

  , testCase "fetch two siblings" $ do
      bucket <- randomBucketString
      key <- randomKeyString
      let val = "foo" :: [Char]
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s" bucket key val)
      curl (printf "-XPUT localhost:8098/types/objects/buckets/%s/keys/%s -H 'Content-Type: text/plain' -d %s" bucket key val)
      let loc = objectLoc (RiakBucket (Latin1.pack bucket)) (RiakKey (Latin1.pack key))
      Right xs <- fetchRiakObject @Text h loc def
      length xs `shouldBe` 2

  , testCase "concurrent fetch/store" $ do
      let n = 1000 -- objects stored+fetched =per thread
      let t = 4    -- num threads
      done <- newEmptyMVar
      let
        go =
          replicateM_ n $ do
            bucket <- randomBucket
            key <- randomKey
            val <- Text.pack <$> randomKeyString
            let loc = objectLoc bucket key
            Right () <- storeRiakObject h loc val def
            Right [x] <- fetchRiakObject h loc def
            (x ^. L.value) `shouldBe` val
      replicateM_ t (forkFinally go (putMVar done))
      replicateM_ t (either throwIO (const (pure ())) =<< takeMVar done)

  -- , testCase "storing Text has correct content type, charset, and content encoding" $ do
  --     bucket <- randomBucket
  --     key <- randomKey
  --     let loc = defaultLocation bucket key
  --     storeRiakObject h loc ("foo" :: Text) def `shouldReturn` Right ()
  --     Right [x] <- fetchRiakObject @Text h loc def
  --     (x ^. L.contentType) `shouldBe` ContentType "text/plain"
  ]

curl :: String -> IO ()
curl = callCommand . ("curl -s " ++)

randomBucket :: IO RiakBucket
randomBucket = RiakBucket . Latin1.pack <$> randomBucketString

randomBucketString :: IO [Char]
randomBucketString = replicateM 32 (randomRIO ('a', 'z'))

randomKey :: IO RiakKey
randomKey = RiakKey . Latin1.pack <$> randomKeyString

randomKeyString :: IO [Char]
randomKeyString = randomBucketString

objectLoc :: RiakBucket -> RiakKey -> RiakLocation 'Nothing
objectLoc bucket key =
  RiakLocation (RiakNamespace (RiakBucketType "objects") bucket) key

-- defaultNamespace :: RiakBucket -> RiakNamespace 'Nothing
-- defaultNamespace = RiakNamespace DefaultRiakBucketType

-- defaultLocation :: RiakBucket -> RiakKey -> RiakLocation 'Nothing
-- defaultLocation bucket = RiakLocation (defaultNamespace bucket)

shouldBe :: (Eq a, HasCallStack, Show a) => a -> a -> IO ()
shouldBe = (@?=)

shouldReturn :: (Eq a, HasCallStack, Show a) => IO a -> a -> IO ()
shouldReturn m x = m >>= (@?= x)

shouldSatisfy :: (Eq a, HasCallStack, Show a) => a -> (a -> Bool) -> IO ()
shouldSatisfy x f = assertBool "" (f x)
