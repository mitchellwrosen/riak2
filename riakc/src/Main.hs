{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Riak
import Riak.Handle.Impl.Exclusive (Endpoint(..), EventHandlers(..),
                                   HandleConfig(..))
import Riak.Handle.Impl.Managed   (Handle)

import qualified Riak.Handle.Impl.Managed as Handle hiding (HandleConfig)
import qualified Riak.ServerInfo          as ServerInfo

import Control.Arrow       ((***))
import Control.Lens        (view, (.~), (^.))
import Data.ByteString     (ByteString)
import Data.Foldable       (asum, for_)
import Data.Function       ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashSet        (HashSet)
import Data.Int            (Int64)
import Data.List.Split     (splitOn)
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import Data.Word
import Net.IPv4            (IPv4, ipv4)
import Numeric.Natural     (Natural)
import Options.Applicative hiding (infoParser)
import System.Exit         (exitFailure)
import Text.Read           (readMaybe)

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
  ((host, port), verbose, run) <-
    (customExecParser
      (prefs (showHelpOnEmpty <> showHelpOnError))
      (info
        (helper <*>
          ((,,)
            <$> nodeParser
            <*> verboseParser
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
              }
        }

  Handle.withHandle config run >>= \case
    Left err -> do
      print err
      exitFailure

    Right () ->
      pure ()

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
          (mkHost -> Just host, ':':port) -> do
            port' <- readMaybe port
            pure (host, port')
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
  hsubparser
    (mconcat
      [ command "delete" (info deleteParser (progDesc "Delete an object"))
      , command "delete-index" (info deleteIndexParser (progDesc "Delete an index"))
      , command "get" (info getParser (progDesc "Get an object"))
      , command "get-bucket-type" (info getBucketTypeParser (progDesc "Get a bucket type"))
      , command "get-counter" (info getCounterParser (progDesc "Get a counter"))
      , command "get-index" (info getIndexParser (progDesc "Get an index, or all indexes"))
      , command "get-map" (info getMapParser (progDesc "Get a map"))
      , command "get-schema" (info getSchemaParser (progDesc "Get a schema"))
      , command "get-set" (info getSetParser (progDesc "Get a set"))
      , command "info" (info infoParser (progDesc "Get Riak info"))
      , command "list" (info listParser (progDesc "List buckets/keys"))
      , command "ping" (info pingParser (progDesc "Ping Riak"))
      , command "put" (info putParser (progDesc "Put an object"))
      , command "put-index" (info putIndexParser (progDesc "Put an index"))
      , command "put-map" (info putMapParser (progDesc "Put a map"))
      , command "put-schema" (info putSchemaParser (progDesc "Put a schema"))
      , command "put-set" (info putSetParser (progDesc "Put a set"))
      , command "search" (info searchParser (progDesc "Perform a search"))
      , command "update-counter" (info updateCounterParser (progDesc "Update a counter"))
      ])

deleteParser :: Parser (Handle -> IO ())
deleteParser =
  doDelete
    <$> keyArgument
    <*> contextOption
  where
    doDelete ::
         Key
      -> Maybe Context
      -> Handle
      -> IO ()
    doDelete key context handle =
      -- TODO riakc delete options
      delete handle object def >>= \case
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

        Right () ->
          pure ()

getParser :: Parser (Handle -> IO ())
getParser =
  doGet
    <$> keyArgument
    <*> nodesOption
    <*> rOption
    <*> prOption
  where
    doGet ::
         Key
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Handle
      -> IO ()
    doGet key nodes r pr handle =
      get handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right object -> do
          print object

      where
        opts :: GetOpts
        opts =
          GetOpts
            { basicQuorum = Nothing
            , nodes = nodes
            , notfoundOk = Nothing
            , pr = pr
            , r = r
            , timeout = Nothing
            }

getBucketTypeParser :: Parser (Handle -> IO ())
getBucketTypeParser =
  doGetBucketType
    <$> bucketTypeArgument
  where
    doGetBucketType ::
         BucketType
      -> Handle
      -> IO ()
    doGetBucketType bucketType handle =
      getBucketType handle bucketType >>= \case
        Left err -> do
          print err
          exitFailure

        Right props ->
          print props

getCounterParser :: Parser (Handle -> IO ())
getCounterParser =
  doGetCounter
    <$> keyArgument
    <*> nodesOption
    <*> rOption
    <*> prOption
  where
    doGetCounter ::
         Key
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Handle
      -> IO ()
    doGetCounter key nodes r pr handle =
      getConvergentCounter handle key opts >>= \case
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
            , pr = pr
            , r = r
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
      getConvergentMap handle key >>= \case
        Left err -> do
          print err
          exitFailure

        Right Nothing ->
          putStrLn "Not found"

        Right (Just (view convergentMapValue -> val)) ->
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
      getConvergentSet handle key >>= \case
        Left err -> do
          print err
          exitFailure

        Right Nothing ->
          putStrLn "Not found"

        Right (Just set) ->
          print (HashSet.toList (set ^. convergentSetValue))

infoParser :: Parser (Handle -> IO ())
infoParser =
  pure $ \handle ->
    ServerInfo.getServerInfo handle >>= \case
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
    <*> contentTypeOption
    <*> contextOption
    <*> nodesOption
    <*> wOption
    <*> dwOption
    <*> pwOption
  where
    contentTypeOption :: Parser (Maybe ByteString)
    contentTypeOption =
      optional
        (strOption
          (help "Content type" <> long "content-type" <> metavar "TYPE"))

    doPut ::
         Either Bucket Key
      -> Text
      -> Maybe ByteString
      -> Maybe Context
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Handle
      -> IO ()
    doPut bucketOrKey val type' context nodes w dw pw handle =
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
                  { type' = type'
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
            { dw = dw
            , nodes = nodes
            , pw = pw
            , timeout = Nothing
            , w = w
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
          go (newConvergentSet (generatedKey bucket) HashSet.empty)

        Right key ->
          getConvergentSet handle key >>= \case
            Left err -> do
              print err
              exitFailure

            Right Nothing ->
              go (newConvergentSet key HashSet.empty)

            Right (Just set) ->
              go set
      where
        go :: ConvergentSet ByteString -> IO ()
        go set =
          putConvergentSet handle  set' >>= \case
            Left err -> do
              print err
              exitFailure

            Right _ ->
              pure ()

          where
            set' :: ConvergentSet ByteString
            set' =
              set & convergentSetValue .~ HashSet.fromList value

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
          go (newConvergentMap (generatedKey bucket) emptyConvergentMapValue)

        Right key ->
          getConvergentMap handle key >>= \case
            Left err -> do
              print err
              exitFailure

            Right Nothing ->
              go (newConvergentMap key emptyConvergentMapValue)

            Right (Just oldMap) ->
              go oldMap

      where
        go :: ConvergentMap ConvergentMapValue -> IO ()
        go oldMap =
          putConvergentMap handle newMap >>= \case
            Left err -> do
              print err
              exitFailure

            Right _ ->
              pure ()

          where
            newMap :: ConvergentMap ConvergentMapValue
            newMap =
              oldMap
                & convergentMapValue .~ value

    combineValues :: [ConvergentMapValue] -> ConvergentMapValue
    combineValues =
      foldr step emptyConvergentMapValue
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
              emptyConvergentMapValue
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
          emptyConvergentMapValue
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
          emptyConvergentMapValue
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
              emptyConvergentMapValue
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
              emptyConvergentMapValue
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
            emptyConvergentMapValue
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

updateCounterParser :: Parser (Handle -> IO ())
updateCounterParser =
  doUpdateCounter
    <$> bucketOrKeyArgument
    <*> amountArgument
    <*> nodesOption
    <*> wOption
    <*> dwOption
    <*> pwOption

  where
    amountArgument :: Parser Int64
    amountArgument =
      argument auto (help "Amount" <> metavar "AMOUNT")

    doUpdateCounter ::
         Either Bucket Key
      -> Int64
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Handle
      -> IO ()
    doUpdateCounter bucketOrKey amount nodes w dw pw handle = do
      updateConvergentCounter handle operation opts >>= \case
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
            { dw = dw
            , nodes = nodes
            , pw = pw
            , timeout = Nothing
            , w = w
            }

--------------------------------------------------------------------------------
-- Arguments/options
--------------------------------------------------------------------------------

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

contextOption :: Parser (Maybe Context)
contextOption =
  optional
    (option
      (eitherReader parseContext)
      (long "context" <> help "Causal context"))
  where
    parseContext :: String -> Either String Context
    parseContext =
      fmap unsafeMakeContext . Base64.decode . Latin1.pack

dwOption :: Parser (Maybe Quorum)
dwOption =
  optional (option (eitherReader parseQuorum) (long "dw" <> help "DW"))

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

nodesOption :: Parser (Maybe Quorum)
nodesOption =
  optional
    (option
      (eitherReader parseQuorum)
        (help "Number of nodes" <> long "nodes" <> metavar "QUORUM"))

prOption :: Parser (Maybe Quorum)
prOption =
  optional
    (option
      (eitherReader parseQuorum)
        (help "Primary read quorum" <> long "pr" <> metavar "QUORUM"))

pwOption :: Parser (Maybe Quorum)
pwOption =
  optional
    (option
      (eitherReader parseQuorum)
      (help "Prmary write quorum" <> long "pw" <> metavar "QUORUM"))

rOption :: Parser (Maybe Quorum)
rOption =
  optional
    (option
      (eitherReader parseQuorum)
      (help "Read quorum" <> long "r" <> metavar "QUORUM"))

schemaNameArgument :: Parser Text
schemaNameArgument =
  strArgument (help "Schema name" <> metavar "SCHEMA")

wOption :: Parser (Maybe Quorum)
wOption =
  optional
    (option
      (eitherReader parseQuorum)
      (help "Write quorum" <> long "w" <> metavar "QUORUM"))

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
