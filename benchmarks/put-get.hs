{-# LANGUAGE DataKinds, LambdaCase, OverloadedStrings, PatternSynonyms,
             TypeApplications, ViewPatterns #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Text               (Text)
import Riak
import System.Environment
import Text.Read               (readMaybe)

main :: IO ()
main = do
  getArgs >>= \case
    [readMaybe -> Just n, readMaybe -> Just i] -> do
      putStrLn $
        "Putting then getting " ++ show n ++ " objects with " ++ show i ++
        " threads"

      h <- createRiakHandle "10.0.0.16" 8087

      let
        namespace :: RiakNamespace 'Nothing
        namespace =
          RiakNamespace DefaultRiakBucketType (RiakBucket "foo")

      let
        go =
          replicateM_ (n `div` i) $ do
            Right key <-
              putNewRiakObject h namespace ("bar" :: Text) def
            Right [_] <-
              getRiakObject @Text h (RiakLocation namespace key) def
            pure ()

      done <- newEmptyMVar
      replicateM_ i $ forkIO $ go >> putMVar done ()
      replicateM_ i $ takeMVar done

    _ ->
      putStrLn "Usage: run-benchmark put OBJECTS THREADS"
