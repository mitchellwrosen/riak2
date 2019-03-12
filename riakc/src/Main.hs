{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Riak

import Control.Arrow         ((***))
import Control.Lens          (view, (.~), (^.))
import Data.ByteString       (ByteString)
import Data.Default.Class    (def)
import Data.Fixed            (Fixed(..))
import Data.Foldable         (asum, for_, traverse_)
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
import System.Environment    (lookupEnv)
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
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import qualified Net.IPv4               as IPv4

main :: IO ()
main = do
  (host, port) <-
    parseHost

  (verbose, run) <-
    (customExecParser
      (prefs (showHelpOnEmpty <> showHelpOnError))
      (info
        (helper <*>
          ((,)
            <$> verboseParser
            <*> commandParser))
        (progDesc "Riak command-line client")))

  let
    config :: HandleConfig
    config =
      HandleConfig
        { endpoint =
            Endpoint
              { address = host
              , port = port
              }
        , retries =
            3
        , healthCheckInterval =
            1
        , idleTimeout =
            10
        , requestTimeout =
            5
        , handlers =
            EventHandlers
              { onSend =
                  if verbose
                    then \msg -> putStrLn (">>> " ++ show msg)
                    else mempty
              , onReceive =
                  if verbose
                    then \msg -> putStrLn ("<<< " ++ show msg)
                    else mempty
              , onConnectError =
                  print
              , onConnectionError =
                  print
              }
        }

  createHandle config >>= run

  where
    parseHost :: IO (IPv4, Word16)
    parseHost =
      lookupEnv "RIAKC_HOST" >>= \case
        Nothing ->
          pure (localhost, 8087)

        Just string ->
          case span (/= ':') string of
            (mkHost -> Just host, mkPort -> Just port) ->
              pure (host, port)
            _ -> do
              putStrLn "$RIAKC_HOST expected 'host' or 'host:port'"
              exitFailure

    mkHost :: String -> Maybe IPv4
    mkHost = \case
      "" -> Just localhost
      "localhost" -> Just localhost
      host -> IPv4.decode (Text.pack host)

    mkPort :: String -> Maybe Word16
    mkPort = \case
      "" -> Just 8087
      ':' : port -> readMaybe port
      _ -> Nothing

    localhost :: IPv4
    localhost =
      ipv4 127 0 0 1

nodeParser :: Parser (IPv4, Word16)
nodeParser =
  argument
    (eitherReader parseNode)
    (help "Riak node, e.g. localhost:8087" <> metavar "NODE")

  where
    parseNode :: String -> Either String (IPv4, Word16)
    parseNode s =
      maybe (Left "Expected: 'host' or 'host:port'") Right $ do
        case span (/= ':') s of
          (mkHost -> Just host, ':':port) ->
            case port of
              [] -> pure (host, 8087)
              _ -> (host,) <$> readMaybe port
          (mkHost -> Just host, []) ->
            pure (host, 8087)
          _ ->
            Nothing

    mkHost :: String -> Maybe IPv4
    mkHost = \case
      "" -> Just (ipv4 127 0 0 1)
      host -> IPv4.decode (Text.pack host)

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
          , command "get" (info getParser (progDesc "Get an object"))
          , command "get-counter" (info getCounterParser (progDesc "Get a counter"))
          , command "get-map" (info getMapParser (progDesc "Get a map"))
          , command "get-set" (info getSetParser (progDesc "Get a set"))
          , command "put" (info putParser (progDesc "Put an object"))
          , command "put-map" (info putMapParser (progDesc "Put a map"))
          , command "put-set" (info putSetParser (progDesc "Put a set"))
          , command "update-counter" (info updateCounterParser (progDesc "Update a counter"))
          ])

    , hsubparser
        (mconcat
          [ commandGroup "Search"
          , command "list" (info listParser (progDesc "List buckets or keys"))
          , command "query" (info queryParser (progDesc "Perform a secondary index query"))
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
    <*> readQuorumOption
    <*> writeQuorumOption
    <*> optional timeoutOption
  where
    doDelete ::
         Key
      -> Maybe Context
      -> Maybe Natural
      -> Maybe ReadQuorum
      -> Maybe WriteQuorum
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doDelete key context nodes readQuorum writeQuorum timeout handle =
      delete handle object opts >>= \case
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
            , context = fromMaybe newContext context
            , key = key
            }

        opts :: DeleteOpts
        opts =
          DeleteOpts
            { nodes = nodes
            , readQuorum = readQuorum
            , timeout = timeout
            , writeQuorum = writeQuorum
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
    <*> optional nodesOption
    <*> readQuorumOption
    <*> optional timeoutOption
  where
    doGet ::
         Key
      -> Maybe Natural
      -> Maybe ReadQuorum
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doGet key nodes quorum timeout handle =
      get handle key opts >>= \case
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
        opts :: GetOpts
        opts =
          GetOpts
            { basicQuorum = Nothing
            , nodes = nodes
            , notfoundOk = Nothing
            , quorum = quorum
            , timeout = timeout
            }

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
          for_ (content ^. field @"ttl") $ \ttl ->
            printf "ttl = %s\n" (show ttl)
          Text.putStrLn ("value = " <> displayByteString (content ^. field @"value"))

        printMetadata :: (ByteString, ByteString) -> IO ()
        printMetadata (key, val) =
          Text.putStrLn
            ("metadata " <> decodeUtf8 key <> " = " <> displayByteString val)

        printSecondaryIndex :: SecondaryIndex -> IO ()
        printSecondaryIndex (SecondaryIndex name val) =
          Text.putStrLn ("index " <> decodeUtf8 name <> " = " <> valstr)
          where
            valstr :: Text
            valstr =
              case val of
                Binary blob -> displayByteString blob
                Integer n -> Text.pack (show n)

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
              print props

        Right bucket ->
          getBucket handle bucket >>= \case
            Left err -> do
              print err
              exitFailure

            Right props ->
              print props

getCounterParser :: Parser (Handle -> IO ())
getCounterParser =
  doGetCounter
    <$> keyArgument
    <*> optional nodesOption
    <*> readQuorumOption
  where
    doGetCounter ::
         Key
      -> Maybe Natural
      -> Maybe ReadQuorum
      -> Handle
      -> IO ()
    doGetCounter key nodes quorum handle =
      getCounter handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right val ->
          print val

      where
        opts :: GetOpts
        opts =
          GetOpts
            { basicQuorum = Nothing
            , nodes = nodes
            , notfoundOk = Nothing
            , quorum = quorum
            , timeout = Nothing
            }

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

            Right index ->
              print index

getMapParser :: Parser (Handle -> IO ())
getMapParser =
  doGetMap
    <$> keyArgument
  where
    doGetMap ::
         Key
      -> Handle
      -> IO ()
    doGetMap key handle =
      getMap handle key >>= \case
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
  where
    doGetSet ::
         Key
      -> Handle
      -> IO ()
    doGetSet key handle =
      getSet handle key >>= \case
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

  where
    doList ::
         Either BucketType Bucket
      -> Handle
      -> IO ()
    doList bucketTypeOrBucket handle =
      case bucketTypeOrBucket of
        Left bucketType ->
          streamBuckets handle bucketType (Foldl.mapM_ printBucket) >>= \case
            Left err -> do
              print err
              exitFailure

            Right () ->
              pure ()

        Right bucket ->
          streamKeys handle bucket (Foldl.mapM_ printKey) >>= \case
            Left err -> do
              print err
              exitFailure

            Right () ->
              pure ()

      where
        printBucket :: Bucket -> IO ()
        printBucket (Bucket _ bucket) =
          Text.putStrLn (decodeUtf8 bucket)

        printKey :: Key -> IO ()
        printKey (Key _ _ key) =
          Text.putStrLn (decodeUtf8 key)

pingParser :: Parser (Handle -> IO ())
pingParser =
  pure $ \handle ->
    ping handle >>= \case
      Left err -> do
        print err
        exitFailure

      Right (Left err) -> do
        Text.putStrLn (decodeUtf8 err)
        exitFailure

      Right (Right ()) ->
        pure ()

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
    <*> optional nodesOption
    <*> writeQuorumOption
    <*> optional timeoutOption
  where
    doPut ::
         Either Bucket Key
      -> Text
      -> Maybe ByteString
      -> Maybe ByteString
      -> Maybe ByteString
      -> [SecondaryIndex]
      -> Maybe Context
      -> Maybe Natural
      -> Maybe WriteQuorum
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doPut bucketOrKey val type' charset encoding indexes context nodes quorum timeout handle =
      put handle object opts >>= \case
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
                fromMaybe newContext context
            , key =
                case bucketOrKey of
                  Left bucket -> generatedKey bucket
                  Right key -> key
            }

        opts :: PutOpts
        opts =
          PutOpts
            { nodes = nodes
            , quorum = quorum
            , timeout = timeout
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
      putIndex handle index (fromMaybe defaultSchema schema) opts >>= \case
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

  where
    doPutSet ::
         Either Bucket Key
      -> [ByteString]
      -> Handle
      -> IO ()
    doPutSet bucketOrKey value handle =
      case bucketOrKey of
        Left bucket ->
          go (newSet (generatedKey bucket) HashSet.empty)

        Right key ->
          getSet handle key >>= \case
            Left err -> do
              print err
              exitFailure

            Right Nothing ->
              go (newSet key HashSet.empty)

            Right (Just set) ->
              go set
      where
        go :: ConvergentSet ByteString -> IO ()
        go set =
          putSet handle  set' >>= \case
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

  where
    doPutMap ::
         Either Bucket Key
      -> ConvergentMapValue
      -> Handle
      -> IO ()
    doPutMap bucketOrKey value handle =
      case bucketOrKey of
        Left bucket ->
          go (newMap (generatedKey bucket) emptyMapValue)

        Right key ->
          getMap handle key >>= \case
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
          putMap handle (oldMap & mapValue .~ value) >>= \case
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
    <*> strArgument (help "Key" <> metavar "KEY")
    <*> secondaryIndexValueArgument
    -- <*> optional secondaryIndexValueArgument

  where
    doQuery ::
         Bucket
      -> ByteString
      -> Either ByteString Int64
      -> Handle
      -> IO ()
    doQuery bucket index val1 handle =
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
        doQuery_ :: IO (Either QueryRangeError ())
        doQuery_ =
          case val1 of
            Left s ->
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

            Right n ->
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


searchParser :: Parser (Handle -> IO ())
searchParser =
  doSearch
    <$> indexNameArgument
    <*> queryArgument

  where
    doSearch ::
         IndexName
      -> Text
      -> Handle
      -> IO ()
    doSearch index query handle =
      -- TODO search options
      search handle index (encodeUtf8 query) def >>= \case
        Left err -> do
          print err
          exitFailure

        Right SearchResults { documents, maxScore, numFound } -> do
          putStrLn ("Max score: " ++ show maxScore)
          putStrLn ("Num found: " ++ show numFound)
          for_ documents printDocument

    queryArgument :: Parser Text
    queryArgument =
      strArgument (help "Query" <> metavar "QUERY")

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

updateCounterParser :: Parser (Handle -> IO ())
updateCounterParser =
  doUpdateCounter
    <$> bucketOrKeyArgument
    <*> amountArgument
    <*> optional nodesOption
    <*> writeQuorumOption
    <*> optional timeoutOption

  where
    amountArgument :: Parser Int64
    amountArgument =
      argument auto (help "Amount" <> metavar "AMOUNT")

    doUpdateCounter ::
         Either Bucket Key
      -> Int64
      -> Maybe Natural
      -> Maybe WriteQuorum
      -> Maybe NominalDiffTime
      -> Handle
      -> IO ()
    doUpdateCounter bucketOrKey amount nodes quorum timeout handle = do
      updateCounter handle operation opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right val ->
          print val

      where
        operation :: ConvergentCounter
        operation =
          ConvergentCounter
            { key =
                case bucketOrKey of
                  Left bucket -> generatedKey bucket
                  Right key -> key
            , value =
                amount
            }

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

nodesOption :: Parser Natural
nodesOption =
  option auto (help "Number of nodes" <> long "nodes" <> metavar "NODES")

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
    (help "Secondary index" <> long "index" <> metavar "KEY/VALUE")
  where
    parseSecondaryIndex :: String -> Either String SecondaryIndex
    parseSecondaryIndex string =
      case splitOn "/" string of
        [ stringToByteString -> name, val ] ->
          case readMaybe val of
            Nothing ->
              Right (SecondaryIndex name (Binary (stringToByteString val)))
            Just n ->
              Right (SecondaryIndex name (Integer n))

        _ ->
          Left "Expected: key/value'"

secondaryIndexValueArgument :: Parser (Either ByteString Int64)
secondaryIndexValueArgument =
  argument
    (eitherReader parseSecondaryIndexValue)
    (help "Value" <> metavar "VALUE")
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
