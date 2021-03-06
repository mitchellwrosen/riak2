{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- TODO riakc get-hll
-- TODO fix riakc put crdt (--nodes, --timeout overlap)
--
-- TODO riakc --help has (COMMAND | COMMAND | COMMAND) output...
-- TODO riakc <command> --help shows top-level help

module Main where

import Riak

import qualified Libriak.Connection as Libriak (ConnectionError(..))
import qualified Libriak.Handle     as Libriak (HandleError(..))

import Control.Arrow         ((***))
import Control.Concurrent    (threadDelay)
import Control.Lens          (view, (.~), (^.))
import Control.Monad         (when)
import Data.ByteString       (ByteString)
import Data.Fixed            (Fixed(..))
import Data.Foldable         (asum, fold, for_, traverse_)
import Data.Function         ((&))
import Data.Generics.Product (field)
import Data.HashMap.Strict   (HashMap)
import Data.HashSet          (HashSet)
import Data.Int              (Int64)
import Data.List.Split       (splitOn)
import Data.Maybe            (fromMaybe)
import Data.Text             (Text)
import Data.Text.Encoding    (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Time             (NominalDiffTime, secondsToNominalDiffTime)
import Data.Word
import Net.IPv4              (IPv4, ipv4)
import Numeric.Natural       (Natural)
import Options.Applicative   hiding (UnknownError, infoParser)
import Socket.Stream.IPv4    (Endpoint(..))
import System.Exit           (exitFailure)
import Text.Printf           (printf)
import Text.Read             (readMaybe)

import qualified Control.Foldl          as Foldl
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.HashSet           as HashSet
import qualified Data.List              as List
import qualified Data.Riak.Proto        as Proto
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified Net.IPv4               as IPv4
import qualified Options.Applicative    as Opt

main :: IO ()
main = do
  (verbose, host, port, run) <-
    (customExecParser
      (prefs (showHelpOnEmpty <> showHelpOnError))
      (info
        (helper <*>
          ((,,,)
            <$> verboseParser
            <*> hostParser
            <*> portParser
            <*> commandParser))
        (progDesc "Riak command-line client")))

  let
    config :: HandleConfig
    config =
      HandleConfig
        { endpoint = Endpoint host port
        , retries = 0
        , healthCheckInterval = 1
        , idleTimeout = 60
        , requestTimeout = 30
        , connectTimeout = 10
        , handlers =
            if verbose
              then
                EventHandlers
                  { onConnectAttempt =
                      \uuid ->
                        Text.putStrLn ("-- " <> uuid <> " connecting")

                  , onConnectFailure =
                      \uuid ex ->
                        Text.putStrLn ("-- " <> uuid <> " " <> Text.pack (show ex))

                  , onConnectSuccess =
                      \uuid ->
                        Text.putStrLn ("-- " <> uuid <> " connected")

                  , onDisconnectAttempt =
                      \uuid reason ->
                        Text.putStrLn $
                          "-- " <> uuid <> " disconnecting (" <>
                            renderDisconnectReason reason <> ")"

                  , onDisconnectFailure =
                      \uuid ex ->
                        Text.putStrLn ("-- " <> uuid <> " " <> Text.pack (show ex))

                  , onDisconnectSuccess =
                      \uuid ->
                        Text.putStrLn ("-- " <> uuid <> " disconnected")

                  , onSend =
                      \uuid msg ->
                        Text.putStrLn ("-- " <> uuid <> " >>> " <> Text.pack (show msg))
                  , onReceive =
                      \uuid msg ->
                        Text.putStrLn ("-- " <> uuid <> " <<< " <> Text.pack (show msg))

                  , onIdleTimeout =
                      \uuid ->
                        Text.putStrLn ("-- " <> uuid <> " idle time out")
                  }
              else
                mempty
        }

  createHandle config >>= run

  where
    renderDisconnectReason :: DisconnectReason -> Text
    renderDisconnectReason = \case
      DisconnectDueToIdleTimeout ->
        "idle timeout"
      DisconnectDueToHandleError err ->
        renderHandleError err

    renderHandleError :: Libriak.HandleError -> Text
    renderHandleError = \case
      Libriak.HandleClosedError ->
        "handle closed"
      Libriak.HandleConnectionError Libriak.LocalShutdown ->
        "local shutdown"
      Libriak.HandleConnectionError Libriak.RemoteReset ->
        "remote reset"
      Libriak.HandleConnectionError Libriak.RemoteShutdown ->
        "remote shutdown"
      Libriak.HandleConnectionError Libriak.RemoteTimeout ->
        "request timeout"
      Libriak.HandleDecodeError err ->
        renderDecodeError err

    renderDecodeError :: DecodeError -> Text
    renderDecodeError = \case
      ProtobufDecodeError{} ->
        "decode error"
      UnexpectedResponse{} ->
        "unexpected message code"

hostParser :: Parser IPv4
hostParser =
  option
    (eitherReader parseHost)
    (fold
      [ help "Host"
      , long "host"
      , metavar "IPv4"
      , showDefaultWith (\_ -> "127.0.0.1")
      , Opt.value localhost
      ])

  where
    parseHost :: String -> Either String IPv4
    parseHost = \case
      "localhost" ->
        Right localhost

      host ->
        host
          & Text.pack
          & IPv4.decode
          & maybe (Left "Invalid IPv4 address") Right

    localhost :: IPv4
    localhost =
      ipv4 127 0 0 1

portParser :: Parser Word16
portParser =
  option
    auto
    (fold
      [ help "Port"
      , long "port"
      , metavar "PORT"
      , showDefault
      , Opt.value 8087
      ])

verboseParser :: Parser Bool
verboseParser =
  switch (short 'v' <> long "verbose" <> help "Verbose")

commandParser :: Parser (Handle -> IO ())
commandParser =
  asum
    [ hsubparser
        (mconcat
          [ commandGroup "Key/value"
          , command "delete" (info deleteParser (progDesc "Delete an object"))
          , command "incr-counter" (info incrementCounterParser (progDesc "Increment a convergent counter"))
          , command "get" (info getParser (progDesc "Get an object"))
          , command "get-counter" (info getCounterParser (progDesc "Get a convergent counter"))
          , command "get-map" (info getMapParser (progDesc "Get a convergent map"))
          , command "get-set" (info getSetParser (progDesc "Get a convergent set"))
          , command "put" (info putParser (progDesc "Put an object"))
          , command "put-map" (info putMapParser (progDesc "Put a convergent map"))
          , command "put-set" (info putSetParser (progDesc "Put a convergent set"))
          ])

    , hsubparser
        (mconcat
          [ commandGroup "Search / Aggregate"
          , command "list" (info listParser (progDesc "List buckets or keys"))
          , command "query" (info queryParser (progDesc "Perform a secondary index query"))
          , command "mapreduce-bucket" (info mapReduceBucketParser (progDesc "Perform a MapReduce job over all objects in a bucket"))
          , command "search" (info searchParser (progDesc "Perform a search"))
          , hidden
          ])

    , hsubparser
        (mconcat
          [ commandGroup "Administration"
          , command "delete-index" (info deleteIndexParser (progDesc "Delete an index"))
          , command "get-bucket" (info getBucketParser (progDesc "Get a bucket type or bucket"))
          , command "get-index" (info getIndexParser (progDesc "Get an index, or all indexes"))
          , command "get-schema" (info getSchemaParser (progDesc "Get a schema"))
          , command "put-index" (info putIndexParser (progDesc "Put an index"))
          , command "put-schema" (info putSchemaParser (progDesc "Put a schema"))
          , command "set-bucket-index" (info setBucketIndexParser (progDesc "Set a bucket type or bucket's index"))
          ])

    , hsubparser
        (mconcat
          [ commandGroup "Diagnostics"
          , command "info" (info infoParser (progDesc "Get server info"))
          , command "ping" (info pingParser (progDesc "Ping Riak"))
          ])
    ]

deleteParser :: Parser (Handle -> IO ())
deleteParser =
  doDelete
    <$> keyArgument
    <*> optional contextOption
    <*> optional nodesOption
    <*> writeQuorumOption
    <*> optional timeoutOption
  where
    doDelete ::
         Key
      -> Maybe Context
      -> Maybe Natural
      -> Maybe WriteQuorum
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doDelete key context nodes quorum timeout handle =
      deleteWith handle object opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right () ->
          pure ()

      where
        object :: Object ()
        object =
          Object
            { content = ()
            , context = fromMaybe emptyContext context
            , key = key
            }

        opts :: PutOpts
        opts =
          PutOpts
            { nodes = nodes
            , quorum = quorum
            , timeout = timeout
            }

deleteIndexParser :: Parser (Handle -> IO ())
deleteIndexParser =
  doDeleteIndex
    <$> indexNameArgument
  where
    doDeleteIndex ::
         IndexName
      -> Handle
      -> IO ()
    doDeleteIndex index handle =
      deleteIndex handle index >>= \case
        Left err -> do
          print err
          exitFailure

        Right False ->
          putStrLn "Not found"

        Right True ->
          pure ()

getParser :: Parser (Handle -> IO ())
getParser =
  doGet
    <$> keyArgument
    <*> getOptsOption
  where
    doGet ::
         Key
      -> GetOpts
      -> Handle
      -> IO ()
    doGet key opts handle =
      getWith handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right object ->
          case object ^. field @"content" of
            [] ->
              putStrLn "Not found"
            siblings -> do
              putStrLn ("context = " ++ show (object ^. field @"context"))
              for_ siblings $ \sibling -> do
                putStrLn ""
                printSibling sibling

      where
        printSibling :: Sibling ByteString -> IO ()
        printSibling = \case
          Sibling content ->
            printContent content

          Tombstone lastModified -> do
            printf "last modified = %s\n" (show lastModified)
            putStrLn "value = <tombstone>"

        printContent :: Content ByteString -> IO ()
        printContent content = do
          for_ (content ^. field @"charset") $ \charset ->
            Text.putStrLn ("charset = " <> decodeUtf8 charset)
          for_ (content ^. field @"encoding") $ \encoding ->
            Text.putStrLn ("encoding = " <> decodeUtf8 encoding)
          traverse_ printSecondaryIndex (content ^. field @"indexes")
          printf "last modified = %s\n" (show (content ^. field @"lastModified"))
          traverse_ printMetadata (HashMap.toList (content ^. field @"metadata"))
          for_ (content ^. field @"type'") $ \type' ->
            Text.putStrLn ("content type = " <> decodeUtf8 type')
          Text.putStrLn ("value = " <> displayByteString (content ^. field @"value"))

        printMetadata :: (ByteString, ByteString) -> IO ()
        printMetadata (key, val) =
          Text.putStrLn
            ("metadata " <> decodeUtf8 key <> " = " <> displayByteString val)

        printSecondaryIndex :: SecondaryIndex -> IO ()
        printSecondaryIndex = \case
          BinaryIndex name val -> go name (displayByteString val)
          IntIndex name val -> go name (Text.pack (show val))
          where
            go :: ByteString -> Text -> IO ()
            go name val =
              Text.putStrLn ("index " <> decodeUtf8 name <> " = " <> val)

getBucketParser :: Parser (Handle -> IO ())
getBucketParser =
  doGetBucket
    <$> bucketTypeOrBucketArgument
  where
    doGetBucket ::
         Either BucketType Bucket
      -> Handle
      -> IO ()
    doGetBucket bucketTypeOrBucket handle =
      case bucketTypeOrBucket of
        Left bucketType ->
          getBucketType handle bucketType >>= \case
            Left err -> do
              print err
              exitFailure

            Right props ->
              printSomeBucketProps props

        Right bucket ->
          getBucket handle bucket >>= \case
            Left err -> do
              print err
              exitFailure

            Right props ->
              printSomeBucketProps props

      where
        printSomeBucketProps :: SomeBucketProps -> IO ()
        printSomeBucketProps = \case
          SomeBucketProps
              BucketProps { backend, conflictResolution, index, nodes,
                            postcommitHooks, precommitHooks,
                            pruneContextSettings, readQuorum, writeOnce,
                            writeQuorum
                          } -> do

            printBackend backend
            printConflictResolution conflictResolution
            printDatatype ""
            printIndex index
            printNodes nodes
            printPostcommitHooks postcommitHooks
            printPrecommitHooks precommitHooks
            printPruneContextSettings pruneContextSettings
            printReadQuorum readQuorum
            printWriteOnce writeOnce
            printWriteQuorum writeQuorum

          SomeCounterBucketProps
              CounterBucketProps { backend, index, nodes, postcommitHooks,
                                   precommitHooks, readQuorum, writeQuorum
                                 } -> do

            printBackend backend
            printDatatype "counter"
            printIndex index
            printNodes nodes
            printPostcommitHooks postcommitHooks
            printPrecommitHooks precommitHooks
            printReadQuorum readQuorum
            printWriteQuorum writeQuorum

          SomeHyperLogLogBucketProps
              HyperLogLogBucketProps { backend, index, nodes, postcommitHooks,
                                       precision, precommitHooks, readQuorum,
                                       writeQuorum
                                     } -> do

            printBackend backend
            printDatatype "hll"
            printIndex index
            printNodes nodes
            printPostcommitHooks postcommitHooks
            printPrecision precision
            printPrecommitHooks precommitHooks
            printReadQuorum readQuorum
            printWriteQuorum writeQuorum

          SomeMapBucketProps
              MapBucketProps { backend, index, nodes, postcommitHooks,
                               precommitHooks, readQuorum, writeQuorum
                             } -> do

            printBackend backend
            printDatatype "map"
            printIndex index
            printNodes nodes
            printPostcommitHooks postcommitHooks
            printPrecommitHooks precommitHooks
            printReadQuorum readQuorum
            printWriteQuorum writeQuorum

          SomeSetBucketProps
              SetBucketProps { backend, index, nodes, postcommitHooks,
                               precommitHooks, readQuorum, writeQuorum
                             } -> do

            printBackend backend
            printDatatype "set"
            printIndex index
            printNodes nodes
            printPostcommitHooks postcommitHooks
            printPrecommitHooks precommitHooks
            printReadQuorum readQuorum
            printWriteQuorum writeQuorum

        printBackend :: Maybe Text -> IO ()
        printBackend backend =
          Text.putStrLn ("backend = " <> fromMaybe "" backend)

        printConflictResolution :: ConflictResolution -> IO ()
        printConflictResolution resolution =
          Text.putStrLn
            ("conflict_resolution = " <>
              case resolution of
                ClientSideConflictResolution ->
                  "client-side (siblings)"
                TimestampBasedConflictResolution ->
                  "timestamp-based (no siblings)"
                LastWriteWinsConflictResolution ->
                  "last-write-wins (no siblings)")

        printDatatype :: Text -> IO ()
        printDatatype s =
          Text.putStrLn ("datatype = " <> s)

        printIndex :: Maybe IndexName -> IO ()
        printIndex index =
          Text.putStrLn ("index = " <> maybe "" unIndexName index)

        printNodes :: Natural -> IO ()
        printNodes nodes =
          Text.putStrLn ("nodes = " <> Text.pack (show nodes))

        printPostcommitHooks :: [Proto.RpbCommitHook] -> IO ()
        printPostcommitHooks hooks =
          Text.putStrLn ("postcommit hooks = " <> Text.pack (show hooks))

        printPrecision :: Natural -> IO ()
        printPrecision n =
          Text.putStrLn ("precision = " <> Text.pack (show n) <> " bits")

        printPrecommitHooks :: [Proto.RpbCommitHook] -> IO ()
        printPrecommitHooks hooks =
          Text.putStrLn ("precommit hooks = " <> Text.pack (show hooks))

        printPruneContextSettings :: PruneContextSettings -> IO ()
        printPruneContextSettings
            PruneContextSettings { minAge, maxAge, minLength, maxLength } = do
          Text.putStrLn ("prune.min_age = " <> Text.pack (show minAge))
          Text.putStrLn ("prune.max_age = " <> Text.pack (show maxAge))
          Text.putStrLn ("prune.min_length = " <> Text.pack (show minLength))
          Text.putStrLn ("prune.max_length = " <> Text.pack (show maxLength))

        printReadQuorum :: ReadQuorum -> IO ()
        printReadQuorum ReadQuorum { nodes, notfoundOk, primary } = do
          Text.putStrLn ("read.nodes = " <> Text.pack (show nodes))
          Text.putStrLn
            ("read.notfound = " <>
              case notfoundOk of
                NotfoundOk -> "ok"
                NotfoundNotOk -> "not ok"
                NotfoundNotOkBasic -> "not ok (basic quorum)")
          Text.putStrLn ("read.primary = " <> Text.pack (show primary))

        printWriteOnce :: Bool -> IO ()
        printWriteOnce = \case
          True -> Text.putStrLn "write_once = true"
          False -> Text.putStrLn "write_once = false"

        printWriteQuorum :: WriteQuorum -> IO ()
        printWriteQuorum WriteQuorum { durable, nodes, primary } = do
          Text.putStrLn ("write.durable = " <> Text.pack (show durable))
          Text.putStrLn ("write.nodes = " <> Text.pack (show nodes))
          Text.putStrLn ("write.primary = " <> Text.pack (show primary))

getCounterParser :: Parser (Handle -> IO ())
getCounterParser =
  doGetCounter
    <$> keyArgument
    <*> getOptsOption
  where
    doGetCounter ::
         Key
      -> GetOpts
      -> Handle
      -> IO ()
    doGetCounter key opts handle =
      getCounterWith handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right val ->
          print val

getIndexParser :: Parser (Handle -> IO ())
getIndexParser =
  doGetIndex
    <$> optional indexNameArgument
  where
    doGetIndex ::
         Maybe IndexName
      -> Handle
      -> IO ()
    doGetIndex name handle =
      case name of
        Nothing ->
          getIndexes handle >>= \case
            Left err -> do
              print err
              exitFailure

            Right indexes ->
              for_ indexes print

        Just name ->
          getIndex handle name >>= \case
            Left err -> do
              print err
              exitFailure

            Right Nothing ->
              putStrLn "Not found"

            Right (Just Index { name, nodes, schema }) -> do
              Text.putStrLn ("name = " <> unIndexName name)
              Text.putStrLn ("nodes = " <> Text.pack (show nodes))
              Text.putStrLn ("schema = " <> schema)

getMapParser :: Parser (Handle -> IO ())
getMapParser =
  doGetMap
    <$> keyArgument
    <*> getOptsOption
  where
    doGetMap ::
         Key
      -> GetOpts
      -> Handle
      -> IO ()
    doGetMap key opts handle =
      getMapWith handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right Nothing ->
          putStrLn "Not found"

        Right (Just (view mapValue -> val)) ->
          for_ (List.sortOn fst (valuePairs val)) $ \(k, v) ->
            putStrLn (k ++ " = " ++ v)

    valuePairs :: ConvergentMapValue -> [(String, String)]
    valuePairs ConvergentMapValue { counters, flags, maps, registers, sets } =
      mconcat
        [ counterPairs counters
        , flagPairs flags
        , mapPairs maps
        , registerPairs registers
        , setPairs sets
        ]

    counterPairs :: HashMap ByteString Int64 -> [(String, String)]
    counterPairs =
      map (Text.unpack . decodeUtf8 *** show) . HashMap.toList

    flagPairs :: HashMap ByteString Bool -> [(String, String)]
    flagPairs =
      map (Text.unpack . decodeUtf8 *** show) . HashMap.toList

    mapPairs :: HashMap ByteString ConvergentMapValue -> [(String, String)]
    mapPairs m = do
      (k, v) <- HashMap.toList m
      (k', v') <- valuePairs v
      pure (Text.unpack (decodeUtf8 k) ++ "." ++ k', v')

    registerPairs :: HashMap ByteString ByteString -> [(String, String)]
    registerPairs =
      map (Text.unpack . decodeUtf8 *** Text.unpack . decodeUtf8) .
        HashMap.toList

    setPairs :: HashMap ByteString (HashSet ByteString) -> [(String, String)]
    setPairs =
      map (f *** g) . HashMap.toList

      where
        f :: ByteString -> String
        f =
          Text.unpack . decodeUtf8

        g :: HashSet ByteString -> String
        g =
          show . map f . List.sort . HashSet.toList

getSchemaParser :: Parser (Handle -> IO ())
getSchemaParser =
  doGetSchema
    <$> schemaNameArgument
  where
    doGetSchema ::
         Text
      -> Handle
      -> IO ()
    doGetSchema name handle =
      getSchema handle name >>= \case
        Left err -> do
          print err
          exitFailure

        Right val ->
          print val

getSetParser :: Parser (Handle -> IO ())
getSetParser =
  doGetSet
    <$> keyArgument
    <*> getOptsOption
  where
    doGetSet ::
         Key
      -> GetOpts
      -> Handle
      -> IO ()
    doGetSet key opts handle =
      getSetWith handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right Nothing ->
          putStrLn "Not found"

        Right (Just set) ->
          print (HashSet.toList (set ^. setValue))

infoParser :: Parser (Handle -> IO ())
infoParser =
  pure $ \handle ->
    getServerInfo handle >>= \case
      Left err -> do
        print err
        exitFailure

      Right (Left err) -> do
        Text.putStrLn (decodeUtf8 err)
        exitFailure

      Right (Right (ServerInfo name version)) -> do
        Text.putStrLn (name <> " " <> version)

listParser :: Parser (Handle -> IO ())
listParser =
  doList
    <$> bucketTypeOrBucketArgument
    <*> optional timeoutOption

  where
    doList ::
         Either BucketType Bucket
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doList bucketTypeOrBucket timeout handle =
      case bucketTypeOrBucket of
        Left bucketType ->
          streamBucketsWith handle bucketType (Foldl.mapM_ printBucket) listBucketsOpts >>= \case
            Left err -> do
              print err
              exitFailure

            Right () ->
              pure ()

        Right bucket ->
          streamKeysWith handle bucket (Foldl.mapM_ printKey) listKeysOpts >>= \case
            Left err -> do
              print err
              exitFailure

            Right () ->
              pure ()

      where
        listBucketsOpts :: ListBucketsOpts
        listBucketsOpts =
          ListBucketsOpts
            { timeout = timeout
            }

        listKeysOpts :: ListKeysOpts
        listKeysOpts =
          ListKeysOpts
            { timeout = timeout
            }

        printBucket :: Bucket -> IO ()
        printBucket (Bucket _ bucket) =
          Text.putStrLn (decodeUtf8 bucket)

        printKey :: Key -> IO ()
        printKey (Key _ _ key) =
          Text.putStrLn (decodeUtf8 key)

mapReduceBucketParser :: Parser (Handle -> IO ())
mapReduceBucketParser =
  doMapReduceBucket
    <$> bucketArgument
    <*> many mapReducePhaseOption

  where
    doMapReduceBucket ::
         Bucket
      -> [MapReducePhase]
      -> Handle
      -> IO ()
    doMapReduceBucket bucket phases handle =
      mapReduceBucket
        handle
        bucket
        phases
        (Foldl.mapM_ print) >>= \case

        Left err -> do
          print err
          exitFailure

        Right () ->
          pure ()

pingParser :: Parser (Handle -> IO ())
pingParser =
  doPing
    <$> option
          auto
          (help "Number of times to ping" <> metavar "N" <> short 'n' <>
            showDefault <> Opt.value 1)

  where
    doPing :: Int -> Handle -> IO ()
    doPing n handle =
      for_ [1..n] $ \i -> do
        ping handle >>= \case
          Left err ->
            print err

          Right (Left err) ->
            Text.putStrLn (decodeUtf8 err)

          Right (Right ()) ->
            pure ()

        when (i /= n) (threadDelay (1*1000*1000))

putParser :: Parser (Handle -> IO ())
putParser =
  doPut
    <$> bucketOrKeyArgument
    <*> strArgument (help "Value" <> metavar "VALUE")
    <*> optional contentTypeOption
    <*> optional charsetOption
    <*> optional encodingOption
    <*> many secondaryIndexOption
    <*> optional contextOption
    <*> putOptsOption
  where
    doPut ::
         Either Bucket Key
      -> Text
      -> Maybe ByteString
      -> Maybe ByteString
      -> Maybe ByteString
      -> [SecondaryIndex]
      -> Maybe Context
      -> PutOpts
      -> Handle
      -> IO ()
    doPut bucketOrKey val type' charset encoding indexes context opts handle =
      putWith handle object opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right (Key _ _ key') ->
          case bucketOrKey of
            Left _  -> Text.putStrLn (decodeUtf8 key')
            Right _ -> pure ()

      where
        object :: Object (Content ByteString)
        object =
          Object
            { content =
                (newContent (encodeUtf8 val))
                  { charset = charset
                  , encoding = encoding
                  , indexes = indexes
                  , type' = type'
                  }
            , context =
                fromMaybe emptyContext context
            , key =
                case bucketOrKey of
                  Left bucket -> generatedKey bucket
                  Right key -> key
            }

putIndexParser :: Parser (Handle -> IO ())
putIndexParser =
  doPutIndex
    <$> indexNameArgument
    <*> optional (option auto (help "Nodes" <> long "nodes" <> metavar "NODES"))
    <*> optional (strOption (help "Schema name" <> long "schema" <> metavar "SCHEMA"))
  where
    doPutIndex ::
         IndexName
      -> Maybe Natural
      -> Maybe Text
      -> Handle
      -> IO ()
    doPutIndex index nodes schema handle =
      putIndexWith handle index (fromMaybe defaultSchema schema) opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right () ->
          pure ()

      where
        opts :: PutIndexOpts
        opts =
          PutIndexOpts
            { nodes = nodes
            , timeout = Nothing
            }

putSetParser :: Parser (Handle -> IO ())
putSetParser =
  doPutSet
    <$> bucketOrKeyArgument
    <*> many (strArgument (help "Set element" <> metavar "VALUE"))
    -- <*> getOptsOption
    <*> putOptsOption

  where
    doPutSet ::
         Either Bucket Key
      -> [ByteString]
      -- -> GetOpts
      -> PutOpts
      -> Handle
      -> IO ()
    doPutSet bucketOrKey value putOpts handle =
      case bucketOrKey of
        Left bucket ->
          go (newSet (generatedKey bucket) HashSet.empty)

        Right key ->
          getSetWith handle key getOpts >>= \case
            Left err -> do
              print err
              exitFailure

            Right Nothing ->
              go (newSet key HashSet.empty)

            Right (Just set) ->
              go set
      where
        getOpts :: GetOpts
        getOpts =
          GetOpts
            { nodes = Nothing
            , quorum = Nothing
            , timeout = Nothing
            }

        go :: ConvergentSet ByteString -> IO ()
        go set =
          putSetWith handle set' putOpts >>= \case
            Left err -> do
              print err
              exitFailure

            Right _ ->
              pure ()

          where
            set' :: ConvergentSet ByteString
            set' =
              set & setValue .~ HashSet.fromList value

putMapParser :: Parser (Handle -> IO ())
putMapParser =
  doPutMap
    <$> bucketOrKeyArgument
    <*> (combineValues <$> many mapValueOption)
    <*> getOptsOption
    <*> putOptsOption

  where
    doPutMap ::
         Either Bucket Key
      -> ConvergentMapValue
      -> GetOpts
      -> PutOpts
      -> Handle
      -> IO ()
    doPutMap bucketOrKey value getOpts putOpts handle =
      case bucketOrKey of
        Left bucket ->
          go (newMap (generatedKey bucket) emptyMapValue)

        Right key ->
          getMapWith handle key getOpts >>= \case
            Left err -> do
              print err
              exitFailure

            Right Nothing ->
              go (newMap key emptyMapValue)

            Right (Just oldMap) ->
              go oldMap

      where
        go :: ConvergentMap ConvergentMapValue -> IO ()
        go oldMap =
          putMapWith handle (oldMap & mapValue .~ value) putOpts >>= \case
            Left err -> do
              print err
              exitFailure

            Right _ ->
              pure ()

    combineValues :: [ConvergentMapValue] -> ConvergentMapValue
    combineValues =
      foldr step emptyMapValue
      where
        step ::
             ConvergentMapValue
          -> ConvergentMapValue
          -> ConvergentMapValue
        step val acc =
          ConvergentMapValue
            { counters  = combineCounters  (counters  val) (counters  acc)
            , flags     = combineFlags     (flags     val) (flags     acc)
            , maps      = combineMaps      (maps      val) (maps      acc)
            , registers = combineRegisters (registers val) (registers acc)
            , sets      = combineSets      (sets      val) (sets      acc)
            }

        combineCounters ::
             HashMap ByteString Int64
          -> HashMap ByteString Int64
          -> HashMap ByteString Int64
        combineCounters =
          HashMap.unionWith (+)

        combineFlags ::
             HashMap ByteString Bool
          -> HashMap ByteString Bool
          -> HashMap ByteString Bool
        combineFlags =
          HashMap.union

        combineMaps ::
             HashMap ByteString ConvergentMapValue
          -> HashMap ByteString ConvergentMapValue
          -> HashMap ByteString ConvergentMapValue
        combineMaps =
          HashMap.unionWith step

        combineRegisters ::
             HashMap ByteString ByteString
          -> HashMap ByteString ByteString
          -> HashMap ByteString ByteString
        combineRegisters =
          HashMap.union

        combineSets ::
             HashMap ByteString (HashSet ByteString)
          -> HashMap ByteString (HashSet ByteString)
          -> HashMap ByteString (HashSet ByteString)
        combineSets =
          HashMap.unionWith HashSet.union

    mapValueOption :: Parser ConvergentMapValue
    mapValueOption =
      asum
        [ counterOption
        , enabledFlagOption
        , disabledFlagOption
        , registerOption
        , setOption
        ]

    counterOption :: Parser ConvergentMapValue
    counterOption =
      option
        (eitherReader parseCounter)
        (help "Counter" <> long "counter" <> metavar "PATH/VALUE")

      where
        parseCounter :: String -> Either String ConvergentMapValue
        parseCounter update =
          case splitOn "/" update of
            [ path, readMaybe -> Just val ] ->
              Right
                (makeMapValue
                  (\name -> toCounterValue name val)
                  (splitOn "." path))

            _ ->
              Left "Expected: 'path/value'"

          where
            toCounterValue :: String -> Int64 -> ConvergentMapValue
            toCounterValue name val =
              emptyMapValue
                { counters =
                    HashMap.singleton (Latin1.pack name) val
                }

    enabledFlagOption :: Parser ConvergentMapValue
    enabledFlagOption =
      makeMapValue toFlagValue . splitOn "." <$>
        strOption
          (help "Enabled flag" <> long "enabled-flag" <> metavar "PATH")

      where
        toFlagValue :: String -> ConvergentMapValue
        toFlagValue name =
          emptyMapValue
            { flags =
                HashMap.singleton (Latin1.pack name) True
            }

    disabledFlagOption :: Parser ConvergentMapValue
    disabledFlagOption =
      makeMapValue toFlagValue . splitOn "." <$>
        strOption
          (help "Disabled flag" <> long "disabled-flag" <> metavar "PATH")

      where
        toFlagValue :: String -> ConvergentMapValue
        toFlagValue name =
          emptyMapValue
            { flags =
                HashMap.singleton (Latin1.pack name) False
            }

    registerOption :: Parser ConvergentMapValue
    registerOption =
      option
        (eitherReader parseRegister)
        (help "Register" <> long "register" <> metavar "PATH/VALUE")

      where
        parseRegister :: String -> Either String ConvergentMapValue
        parseRegister update =
          case splitOn "/" update of
            [ path, val ] ->
              Right
                (makeMapValue
                  (\name -> toRegisterValue name val)
                  (splitOn "." path))

            _ ->
              Left "Expected: 'path/value'"

          where
            toRegisterValue :: String -> String -> ConvergentMapValue
            toRegisterValue name val =
              emptyMapValue
                { registers =
                    HashMap.singleton (Latin1.pack name) (Latin1.pack val)
                }

    setOption :: Parser ConvergentMapValue
    setOption =
      option
        (eitherReader parseElem)
        (help "Set element" <> long "elem" <> metavar "PATH/VALUE")

      where
        parseElem :: String -> Either String ConvergentMapValue
        parseElem update =
          case splitOn "/" update of
            [ path, val ] ->
              Right
                (makeMapValue
                  (\name -> toSetValue name val)
                  (splitOn "." path))

            _ ->
              Left "Expected: 'path/value'"

          where
            toSetValue :: String -> String -> ConvergentMapValue
            toSetValue name val =
              emptyMapValue
                { sets =
                    HashMap.singleton
                      (Latin1.pack name)
                      (HashSet.singleton (Latin1.pack val))
                }

    makeMapValue ::
         (String -> ConvergentMapValue)
      -> [String]
      -> ConvergentMapValue
    makeMapValue onLeaf =
      loop

      where
        loop :: [String] -> ConvergentMapValue
        loop = \case
          [] ->
            undefined

          [leaf] ->
            onLeaf leaf

          p:ps ->
            emptyMapValue
              { maps =
                  HashMap.singleton (Latin1.pack p) (loop ps)
              }



putSchemaParser :: Parser (Handle -> IO ())
putSchemaParser =
  doPutSchema
    <$> schemaNameArgument
  where
    doPutSchema ::
         Text
      -> Handle
      -> IO ()
    doPutSchema name handle = do
      content <- ByteString.readFile (Text.unpack name)

      putSchema handle Schema { name, content } >>= \case
        Left err -> do
          print err
          exitFailure

        Right () ->
          pure ()

queryParser :: Parser (Handle -> IO ())
queryParser =
  doQuery
    <$> bucketArgument
    <*> strArgument (help "Index" <> metavar "INDEX")
    <*> secondaryIndexValueArgument (help "Value" <> metavar "VALUE")
    <*> optional (secondaryIndexValueArgument (metavar "VALUE"))

  where
    doQuery ::
         Bucket
      -> ByteString
      -> Either ByteString Int64
      -> Maybe (Either ByteString Int64)
      -> Handle
      -> IO ()
    doQuery bucket index val1 val2 handle =
      doQuery_ >>= \case
        Left (UnknownError err) -> do
          Text.putStrLn err
          exitFailure

        Left err -> do
          print err
          exitFailure

        Right () ->
          pure ()

      where
        doQuery_ :: IO (Either QueryIndexError ())
        doQuery_ = do
          case (val1, val2) of
            (Left s, Nothing) ->
              queryBinaryIndexTerms
                handle
                (BinaryIndexQuery
                  { bucket = bucket
                  , index = index
                  , minValue = s
                  , maxValue = s
                  })
                (Foldl.mapM_
                  (\(val, Key _ _ key) ->
                    Text.putStrLn (decodeUtf8 val <> " " <> decodeUtf8 key)))

            (Left s1, Just (Left s2)) ->
              queryBinaryIndexTerms
                handle
                (BinaryIndexQuery
                  { bucket = bucket
                  , index = index
                  , minValue = s1
                  , maxValue = s2
                  })
                (Foldl.mapM_
                  (\(val, Key _ _ key) ->
                    Text.putStrLn (decodeUtf8 val <> " " <> decodeUtf8 key)))

            (Right n, Nothing) ->
              queryIntIndexTerms
                handle
                (IntIndexQuery
                  { bucket = bucket
                  , index = index
                  , minValue = n
                  , maxValue = n
                  })
                (Foldl.mapM_
                  (\(val, Key _ _ key) ->
                    Text.putStrLn (Text.pack (show val) <> " " <> decodeUtf8 key)))

            (Right n1, Just (Right n2)) ->
              queryIntIndexTerms
                handle
                (IntIndexQuery
                  { bucket = bucket
                  , index = index
                  , minValue = n1
                  , maxValue = n2
                  })
                (Foldl.mapM_
                  (\(val, Key _ _ key) ->
                    Text.putStrLn (Text.pack (show val) <> " " <> decodeUtf8 key)))

            _ -> do
              putStrLn "Invalid query"
              exitFailure


searchParser :: Parser (Handle -> IO ())
searchParser =
  doSearch
    <$> indexNameArgument
    <*> queryArgument
    <*> many fieldOption
    <*> optional filterOption
    <*> optional presortOption
    <*> optional rowsOption
    <*> optional sortOption
    <*> optional startOption

  where
    doSearch ::
         IndexName
      -> Text
      -> [ByteString]
      -> Maybe ByteString
      -> Maybe ByteString
      -> Maybe Word32
      -> Maybe ByteString
      -> Maybe Word32
      -> Handle
      -> IO ()
    doSearch index query fields filter presort rows sort start handle =
      search handle index (encodeUtf8 query) opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right SearchResults { documents, maxScore, numFound } -> do
          putStrLn ("Max score: " ++ show maxScore)
          putStrLn ("Num found: " ++ show numFound)
          for_ documents printDocument

      where
        opts :: SearchOpts
        opts =
          SearchOpts
            { fieldList = fields
            , filter = filter
            , presort = presort
            , rows = rows
            , sort = sort
            , start = start
            }

    queryArgument :: Parser Text
    queryArgument =
      strArgument (help "Query" <> metavar "QUERY")

    fieldOption :: Parser ByteString
    fieldOption =
      encodeUtf8 <$>
        strOption (help "Field" <> long "field" <> metavar "FIELD")

    filterOption :: Parser ByteString
    filterOption =
      encodeUtf8 <$>
        strOption (help "Filter query" <> long "filter" <> metavar "QUERY")

    presortOption :: Parser ByteString
    presortOption =
      encodeUtf8 <$>
        strOption (help "Presort" <> long "presort" <> metavar "PRESORT")

    rowsOption :: Parser Word32
    rowsOption =
      option auto (help "Rows" <> long "rows" <> metavar "N")

    sortOption :: Parser ByteString
    sortOption =
      encodeUtf8 <$>
        strOption (help "Sort" <> long "sort" <> metavar "asc|desc")

    startOption :: Parser Word32
    startOption =
      option auto (help "Start" <> long "start" <> metavar "N")

    -- Assume the keys and values are UTF-8 encoded
    printDocument :: [(ByteString, ByteString)] -> IO ()
    printDocument =
      print . map (\(key, val) -> (decodeUtf8 key, decodeUtf8 val))

setBucketIndexParser :: Parser (Handle -> IO ())
setBucketIndexParser =
  doSetBucketIndex
    <$> bucketTypeOrBucketArgument
    <*> indexNameArgument

  where
    doSetBucketIndex ::
         Either BucketType Bucket
      -> IndexName
      -> Handle
      -> IO ()
    doSetBucketIndex bucketTypeOrBucket index handle =
      case bucketTypeOrBucket of
        Left bucketType ->
          setBucketTypeIndex handle bucketType index >>= \case
            Left err -> do
              print err
              exitFailure

            Right () ->
              pure ()

        Right bucket ->
          setBucketIndex handle bucket index >>= \case
            Left err -> do
              print err
              exitFailure

            Right () ->
              pure ()

incrementCounterParser :: Parser (Handle -> IO ())
incrementCounterParser =
  doIncrementCounter
    <$> bucketOrKeyArgument
    <*> amountArgument
    <*> optional nodesOption
    <*> writeQuorumOption
    <*> optional timeoutOption

  where
    amountArgument :: Parser Int64
    amountArgument =
      argument auto (help "Amount" <> metavar "AMOUNT")

    doIncrementCounter ::
         Either Bucket Key
      -> Int64
      -> Maybe Natural
      -> Maybe WriteQuorum
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doIncrementCounter bucketOrKey amount nodes quorum timeout handle = do
      incrementCounterWith handle key amount opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right val ->
          print val

      where
        key :: Key
        key =
          case bucketOrKey of
            Left bucket -> generatedKey bucket
            Right key -> key

        opts :: PutOpts
        opts =
          PutOpts
            { nodes = nodes
            , quorum = quorum
            , timeout = timeout
            }

-- Try to display a byte string as UTF-8 (best effor)
displayByteString :: ByteString -> Text
displayByteString bytes =
  case decodeUtf8' bytes of
    Left _ -> "<binary data>"
    Right text -> text

stringToByteString :: String -> ByteString
stringToByteString =
  encodeUtf8 . Text.pack

--------------------------------------------------------------------------------
-- Arguments/options
--------------------------------------------------------------------------------

bucketArgument :: Parser Bucket
bucketArgument =
  argument
    (eitherReader parseBucket)
    (help "Bucket" <> metavar "TYPE/BUCKET")

bucketOrKeyArgument :: Parser (Either Bucket Key)
bucketOrKeyArgument =
  argument
    (Right <$> eitherReader parseKey <|> Left <$> eitherReader parseBucket)
    (help "Bucket or key" <> metavar "TYPE/BUCKET(/KEY)")

bucketTypeArgument :: Parser BucketType
bucketTypeArgument =
  encodeUtf8 <$>
    strArgument (help "Bucket type" <> metavar "TYPE")

bucketTypeOrBucketArgument :: Parser (Either BucketType Bucket)
bucketTypeOrBucketArgument =
  argument
    ((Right <$> eitherReader parseBucket) <|> (Left . encodeUtf8 <$> str))
    (help "Bucket type or bucket" <> metavar "TYPE(/BUCKET)")

charsetOption :: Parser ByteString
charsetOption =
  encodeUtf8 <$> strOption (long "charset" <> help "Character set")

contentTypeOption :: Parser ByteString
contentTypeOption =
  strOption (help "Content type" <> long "content-type" <> metavar "TYPE")

contextOption :: Parser Context
contextOption =
  option
    (eitherReader parseContext)
    (long "context" <> help "Causal context")
  where
    parseContext :: String -> Either String Context
    parseContext =
      fmap unsafeMakeContext . Base64.decode . Latin1.pack

encodingOption :: Parser ByteString
encodingOption =
  encodeUtf8 <$> strOption (long "encoding" <> help "Character encoding")

getOptsOption :: Parser GetOpts
getOptsOption =
  GetOpts
    <$> optional nodesOption
    <*> readQuorumOption
    <*> optional timeoutOption

indexNameArgument :: Parser IndexName
indexNameArgument =
  argument (eitherReader parseIndexName) (help "Index name" <> metavar "INDEX")
  where
    parseIndexName :: String -> Either String IndexName
    parseIndexName name =
      maybe
        (Left "Invalid index name.")
        Right
        (makeIndexName (Text.pack name))

keyArgument :: Parser Key
keyArgument =
  argument (eitherReader parseKey) keyMod

-- TODO riakc better MapReducePhase parsing
mapReducePhaseOption :: Parser MapReducePhase
mapReducePhaseOption =
  mapPhaseOption -- <|> reducePhaseOption

  where
    mapPhaseOption :: Parser MapReducePhase
    mapPhaseOption =
      option
        (eitherReader parseMapPhase)
        (help "Map phase" <> long "map" <> metavar "MODULE:FUNCTION")

    parseMapPhase :: String -> Either String MapReducePhase
    parseMapPhase string =
      case splitOn ":" string of
        [ modul, fun ] ->
          Right
            (MapPhase
              (CompiledFunction (ErlangFunctionId (Text.pack modul) (Text.pack fun)))
              (ErlAtomUtf8 "none")
              True)

        _ ->
          Left "Expected: module:function"

nodesOption :: Parser Natural
nodesOption =
  option auto (help "Number of nodes" <> long "nodes" <> metavar "NODES")

putOptsOption :: Parser PutOpts
putOptsOption =
  PutOpts
    <$> optional nodesOption
    <*> writeQuorumOption
    <*> optional timeoutOption

readQuorumOption :: Parser (Maybe ReadQuorum)
readQuorumOption =
  f <$> optional rOption <*> optional prOption
  where
    f :: Maybe Quorum -> Maybe Quorum -> Maybe ReadQuorum
    f r pr =
      case (r, pr) of
        (Nothing, Nothing) ->
          Nothing

        _ ->
          Just ReadQuorum
            { nodes = fromMaybe QuorumDefault r
            , notfoundOk = NotfoundOk -- TODO riakc parse notfoundOk
            , primary = fromMaybe QuorumDefault pr
            }

    prOption :: Parser Quorum
    prOption =
      option
        (eitherReader parseQuorum)
          (help "Primary read quorum" <> long "pr" <> metavar "QUORUM")

    rOption :: Parser Quorum
    rOption =
      option
        (eitherReader parseQuorum)
        (help "Read quorum" <> long "r" <> metavar "QUORUM")

schemaNameArgument :: Parser Text
schemaNameArgument =
  strArgument (help "Schema name" <> metavar "SCHEMA")

secondaryIndexOption :: Parser SecondaryIndex
secondaryIndexOption =
  option
    (eitherReader parseSecondaryIndex)
    (help "Secondary index" <> long "index" <> metavar "INDEX/VALUE")
  where
    parseSecondaryIndex :: String -> Either String SecondaryIndex
    parseSecondaryIndex string =
      case splitOn "/" string of
        [ stringToByteString -> name, val ] ->
          case readMaybe val of
            Nothing ->
              Right (BinaryIndex name (stringToByteString val))
            Just n ->
              Right (IntIndex name n)

        _ ->
          Left "Expected: index/value'"

secondaryIndexValueArgument ::
     Mod ArgumentFields (Either ByteString Int64)
  -> Parser (Either ByteString Int64)
secondaryIndexValueArgument =
  argument
    (eitherReader parseSecondaryIndexValue)
  where
    parseSecondaryIndexValue :: String -> Either String (Either ByteString Int64)
    parseSecondaryIndexValue string =
      case readMaybe string of
        Nothing -> Right (Left (stringToByteString string))
        Just n -> Right (Right n)

timeoutOption :: Parser NominalDiffTime
timeoutOption =
  millisToDifftime <$>
    option auto (help "Timeout" <> long "timeout" <> metavar "MILLIS")
  where
    millisToDifftime :: Integer -> NominalDiffTime
    millisToDifftime millis =
      secondsToNominalDiffTime (MkFixed (millis * 1000000000))

writeQuorumOption :: Parser (Maybe WriteQuorum)
writeQuorumOption =
  f <$> optional wOption <*> optional pwOption <*> optional dwOption
  where
    f :: Maybe Quorum -> Maybe Quorum -> Maybe Quorum -> Maybe WriteQuorum
    f w pw dw =
      case (w, pw, dw) of
        (Nothing, Nothing, Nothing) ->
          Nothing

        _ ->
          Just WriteQuorum
            { durable = fromMaybe QuorumDefault dw
            , nodes = fromMaybe QuorumDefault w
            , primary = fromMaybe QuorumDefault pw
            }

    dwOption :: Parser Quorum
    dwOption =
      option (eitherReader parseQuorum) (long "dw" <> help "DW")

    pwOption :: Parser Quorum
    pwOption =
      option
        (eitherReader parseQuorum)
        (help "Prmary write quorum" <> long "pw" <> metavar "QUORUM")

    wOption :: Parser Quorum
    wOption =
      option
        (eitherReader parseQuorum)
        (help "Write quorum" <> long "w" <> metavar "QUORUM")

parseBucket :: String -> Either String Bucket
parseBucket string =
  case splitOn "/" string of
    [ bucketType, bucket ] ->
      Right (Bucket (Latin1.pack bucketType) (Latin1.pack bucket))

    _ ->
      Left "Expected: 'type/bucket'"

parseKey :: String -> Either String Key
parseKey string =
  case splitOn "/" string of
    [ bucketType, bucket, key ] ->
      Right (Key (Latin1.pack bucketType) (Latin1.pack bucket) (Latin1.pack key))

    _ ->
      Left "Expected: 'type/bucket/key'"

parseQuorum :: String -> Either String Quorum
parseQuorum string =
  case readMaybe string of
    Nothing ->
      Left "Expected: integer"

    Just q ->
      Right (QuorumOf q)

keyMod :: HasMetavar f => Mod f a
keyMod =
  (help "Key" <> metavar "TYPE/BUCKET/KEY")
