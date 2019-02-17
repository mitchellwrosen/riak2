module Main where

import Riak.Handle.Impl.Exclusive (Endpoint(..))

import qualified Riak.Handle.Impl.Exclusive as Handle.Exclusive
import qualified Riak.Handle.Impl.Pipeline  as Handle.Pipeline
import qualified Riak.WithExclusiveHandle
import qualified Riak.WithPipelineHandle

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar
import Control.Monad           (join, replicateM_)
import Data.Default.Class      (def)
import GHC.Clock               (getMonotonicTime)
import Net.IPv4                (ipv4)
import Text.Printf             (printf)

import qualified Options.Applicative as Opt


main :: IO ()
main =
  join
    (Opt.customExecParser
      (Opt.prefs (Opt.showHelpOnEmpty <> Opt.showHelpOnError))
      (Opt.info
        (Opt.helper <*> commandParser)
        (Opt.progDesc "Benchmarks")))

commandParser :: Opt.Parser (IO ())
commandParser =
  Opt.hsubparser
    (mconcat
      [ Opt.command
          "exclusive-socket"
          (Opt.info
            exclusiveSocketParser
            (Opt.progDesc "Benchmark exclusive socket handle"))
      , Opt.command
          "pipeline-socket"
          (Opt.info
            pipelineSocketParser
            (Opt.progDesc "Benchmark pipeline socket handle"))
      ])

exclusiveSocketParser :: Opt.Parser (IO ())
exclusiveSocketParser =
  pure $ do
    Right () <-
      Handle.Exclusive.withHandle
        (Handle.Exclusive.HandleConfig
          { Handle.Exclusive.endpoint =
              Endpoint
                { address = ipv4 127 0 0 1
                , port = 8087
                }
          , Handle.Exclusive.handlers =
              mempty
          })
        doBenchmark

    pure ()

  where
    doBenchmark :: Handle.Exclusive.Handle -> IO ()
    doBenchmark handle = do
      let threads = 100
      let puts = 100

      doneVar <- newEmptyMVar
      readyVar <- newEmptyMVar
      goVar <- newEmptyMVar

      replicateM_ threads $
        forkIO $ do
          putMVar readyVar ()
          readMVar goVar

          replicateM_ puts $ do
            Right _ <-
              Riak.WithExclusiveHandle.put
                handle
                (Riak.WithExclusiveHandle.newObject
                  (Riak.WithExclusiveHandle.generatedKey
                    (Riak.WithExclusiveHandle.Bucket "default" "a"))
                  (Riak.WithExclusiveHandle.newContent ""))
                def

            pure ()

          putMVar doneVar ()

      replicateM_ threads (takeMVar readyVar)

      t0 <- getMonotonicTime
      putMVar goVar ()
      replicateM_ threads (takeMVar doneVar)
      t1 <- getMonotonicTime

      printf
        "%d threads, %d puts each: %.2f puts/second\n"
        threads
        puts
        (fromIntegral (threads * puts) / (t1-t0))

pipelineSocketParser :: Opt.Parser (IO ())
pipelineSocketParser =
  pure $ do
    Right () <-
      Handle.Pipeline.withHandle
        (Handle.Pipeline.HandleConfig
          { Handle.Pipeline.endpoint =
              Endpoint
                { address = ipv4 127 0 0 1
                , port = 8087
                }
          , Handle.Pipeline.handlers =
              mempty
          })
        doBenchmark

    pure ()

  where
    doBenchmark :: Handle.Pipeline.Handle -> IO ()
    doBenchmark handle = do
      let threads = 100
      let puts = 100

      doneVar <- newEmptyMVar
      readyVar <- newEmptyMVar
      goVar <- newEmptyMVar

      replicateM_ threads $
        forkIO $ do
          putMVar readyVar ()
          readMVar goVar

          replicateM_ puts $ do
            Right _ <-
              Riak.WithPipelineHandle.put
                handle
                (Riak.WithPipelineHandle.newObject
                  (Riak.WithPipelineHandle.generatedKey
                    (Riak.WithPipelineHandle.Bucket "default" "a"))
                  (Riak.WithPipelineHandle.newContent ""))
                def

            pure ()

          putMVar doneVar ()

      replicateM_ threads (takeMVar readyVar)

      t0 <- getMonotonicTime
      putMVar goVar ()
      replicateM_ threads (takeMVar doneVar)
      t1 <- getMonotonicTime

      printf
        "%d threads, %d puts each: %.2f puts/second\n"
        threads
        puts
        (fromIntegral (threads * puts) / (t1-t0))
