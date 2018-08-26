{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, ViewPatterns #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Text          (Text)
import Riak
import System.Environment
import Text.Read          (readMaybe)

main :: IO ()
main = do
  getArgs >>= \case
    [readMaybe -> Just n, readMaybe -> Just i] -> do
      putStrLn ("Putting " ++ show n ++ " objects with " ++ show i ++ " threads")

      withRiakHandle "10.0.0.16" 8087 $ \h ->
        let
          go =
            replicateM_ (n `div` i) $ do
              Right _ <-
                storeNewRiakObject h
                  (RiakNamespace DefaultRiakBucketType (RiakBucket "foo"))
                  ("bar" :: Text)
                  def
              pure ()
        in do
          done <- newEmptyMVar
          replicateM_ i $ forkIO $ go >> putMVar done ()
          replicateM_ i $ takeMVar done
    _ ->
      putStrLn "Usage: run-benchmark put N M"
