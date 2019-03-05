module Main where

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad           (join, replicateM_)
import Data.Default.Class      (Default(..))
import Data.Foldable           (asum)
import Data.Time
import GHC.Clock               (getMonotonicTime)
import Net.IPv4                (ipv4)
import Text.Printf             (printf)

import qualified Data.Riak.Proto              as Proto
import qualified Network.Riak.Connection      as RHK
import qualified Network.Riak.Connection.Pool as RHK
import qualified Network.Riak.Types           as RHK
import qualified Options.Applicative          as Opt
import qualified Riak

main :: IO ()
main =
  join
    (Opt.customExecParser
      (Opt.prefs (Opt.showHelpOnEmpty <> Opt.showHelpOnError))
      (Opt.info
        (Opt.helper <*> commandParser)
        (Opt.progDesc "Benchmarks")))

{-
main :: IO ()
main = do
  handle <-
    Riak.createHandle
      Riak.HandleConfig
        { Riak.endpoint =
            Riak.Endpoint
              { Riak.address = ipv4 127 0 0 1
              , Riak.port = 8087
              }
          , Riak.requestTimeout =
              1
          , Riak.retries =
              1
          , Riak.handlers =
              Riak.EventHandlers
                { Riak.onSend = \msg -> Riak.debug (">>> " ++ show msg)
                , Riak.onReceive = \msg -> Riak.debug ("<<< " ++ show msg)
                , Riak.onConnectionError = \err -> Riak.debug ("*** " ++ show err)
                , Riak.onConnectError = \err -> Riak.debug ("*** " ++ show err)
                }
          }

  doneVar <- newEmptyMVar

  _ <- forkIO $ do
    replicateM_
      500
      (Riak.put
        handle
        (Riak.newObject
          (Riak.generatedKey (Riak.Bucket "default" "a"))
          (Riak.newContent ""))
        def >>= \case
          Left err -> Riak.debug ("Put failed --> " ++ show err)
          Right _ -> pure ())
    putMVar doneVar ()

  _ <- forkIO $ do
    replicateM_
      500
      (Riak.ping handle >>= \case
        Left err -> Riak.debug ("Ping failed --> " ++ show err)
        Right _ -> pure ())
    putMVar doneVar ()

  takeMVar doneVar
  takeMVar doneVar
-}

commandParser :: Opt.Parser (IO ())
commandParser =
  asum
    [ Opt.hsubparser
        (mconcat
          [ Opt.commandGroup "riak2"
          , Opt.command
              "put"
              (Opt.info
                riak2PingParser
                (Opt.progDesc "Put empty objects to random keys"))
          ])
    , Opt.hsubparser
        (mconcat
          [ Opt.commandGroup "riak-haskell-client"
          , Opt.command
              "riak-haskell-client"
              (Opt.info
                riakPingParser
                (Opt.progDesc "Benchmark riak-haskell-client"))
          ])
    ]

riak2PingParser :: Opt.Parser (IO ())
riak2PingParser =
  pure $ do
    handle <-
      Riak.createHandle
        Riak.HandleConfig
          { Riak.endpoint =
              Riak.Endpoint
                { Riak.address = ipv4 127 0 0 1
                , Riak.port = 8087
                }
            , Riak.requestTimeout =
                1
            , Riak.retries =
                3
            , Riak.handlers =
                mempty
                -- Riak.EventHandlers
                --   { Riak.onSend = \msg -> putStrLn (">>> " ++ show msg)
                --   , Riak.onReceive = \msg -> putStrLn ("<<< " ++ show msg)
                --   , Riak.onConnectionError = print
                --   , Riak.onConnectError = print
                --   }
            }

    runBenchWith $ do
      _ <-
        Riak.put
          handle
          (Riak.newObject
            (Riak.generatedKey (Riak.Bucket "default" "a"))
            (Riak.newContent ""))
          def
      pure ()

riakPingParser :: Opt.Parser (IO ())
riakPingParser =
  pure $ do
    pool :: RHK.Pool <-
      RHK.create
        (RHK.Client "localhost" "8087" "")
        1 -- stripes
        10 -- seconds to keep idle connections open
        10 -- conns per stripe

    runBenchWith
      (RHK.withConnection pool $ \conn -> do
        _ <-
          RHK.exchange
            conn
            (Proto.defMessage & Proto.bucket .~ "a" :: Proto.RpbPutReq)
        pure ())

runBenchWith :: IO () -> IO ()
runBenchWith put = do
  let threads = 100
  let puts = 100

  doneVar <- newEmptyMVar
  readyVar <- newEmptyMVar
  goVar <- newEmptyMVar

  replicateM_ threads $
    forkIO $ do
      putMVar readyVar ()
      readMVar goVar

      replicateM_ puts put

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
