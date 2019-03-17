module Main where

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad           (join, replicateM_)
import GHC.Clock               (getMonotonicTime)
import Net.IPv4                (ipv4)
import Socket.Stream.IPv4      (Endpoint(..))
import Text.Printf             (printf)

import qualified Data.Riak.Proto              as Proto
import qualified Network.Riak.Basic           as RHK (ping)
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
        (Opt.helper <*> commandParser <*> riakHaskellClientSwitch)
        (Opt.progDesc "Benchmarks")))
  where
    riakHaskellClientSwitch :: Opt.Parser Bool
    riakHaskellClientSwitch =
      Opt.switch
        (Opt.help "Use `riak` package" <> Opt.long "riak-haskell-client")

config :: Riak.HandleConfig
config =
  Riak.HandleConfig
    { Riak.endpoint = Endpoint (ipv4 127 0 0 1) 8087
    , Riak.healthCheckInterval = 1
    , Riak.idleTimeout = 10
    , Riak.requestTimeout = 5
    , Riak.retries = 3
    , Riak.handlers = mempty
        -- Riak.EventHandlers
        --   { Riak.onSend = \msg -> putStrLn (">>> " ++ show msg)
        --   , Riak.onReceive = \msg -> putStrLn ("<<< " ++ show msg)
        --   , Riak.onConnectionError = print
        --   , Riak.onConnectError = print
        --   }
    }

rhkPool :: IO RHK.Pool
rhkPool =
  RHK.create
    (RHK.Client "localhost" "8087" "")
    1 -- stripes
    10 -- seconds to keep idle connections open
    256 -- conns per stripe

commandParser :: Opt.Parser (Bool -> IO ())
commandParser =
  Opt.hsubparser
    (mconcat
      [ Opt.command
          "ping"
          (Opt.info
            pingParser
            (Opt.progDesc "Ping"))
      , Opt.command
          "put"
          (Opt.info
            putParser
            (Opt.progDesc "Put empty objects to random keys"))
      ])

pingParser :: Opt.Parser (Bool -> IO ())
pingParser =
  pure $ \case
    True -> do
      pool <- rhkPool
      runBenchWith (RHK.withConnection pool RHK.ping)

    False -> do
      handle <- Riak.createHandle config

      runBenchWith $
        Riak.ping handle >>= \case
          Left err -> print err
          Right (Left err) -> print err
          Right (Right ()) -> pure ()

putParser :: Opt.Parser (Bool -> IO ())
putParser =
  pure $ \case
    False -> do
      handle <- Riak.createHandle config

      runBenchWith $ do
        _ <-
          Riak.put
            handle
            (Riak.newObject
              (Riak.generatedKey (Riak.Bucket "default" "a"))
              (Riak.newContent ""))
        pure ()

    True -> do
      pool <- rhkPool

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
    "%d threads, %d puts each: %.2f operations/second\n"
    threads
    puts
    (fromIntegral (threads * puts) / (t1-t0))
