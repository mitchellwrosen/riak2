module Main where

import Riak
import Riak.Handle.Impl.Exclusive (Config(..), Endpoint(..), EventHandlers(..))
import Riak.Handle.Impl.Managed   (Handle)

import qualified Riak.Handle.Impl.Managed as Handle hiding (Config)
import qualified Riak.Object              as Object
import qualified Riak.ServerInfo          as ServerInfo

import Data.ByteString     (ByteString)
import Data.Foldable       (asum, for_)
import Data.List.Split     (splitOn)
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import Data.Word
import Net.IPv4            (IPv4, ipv4)
import Options.Applicative hiding (infoParser)
import System.Exit         (ExitCode(..), exitFailure, exitWith)
import Text.Read           (readMaybe)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
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
    config :: Config
    config =
      Config
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
    Left errno ->
      exitWith (ExitFailure (fromIntegral errno))

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
      [ command "get" (info getParser (progDesc "Get an object"))
      , command "get-map" (info getMapParser (progDesc "Get a map"))
      , command "delete" (info deleteParser (progDesc "Delete an object"))
      , command "info" (info infoParser (progDesc "Get Riak info"))
      , command "ping" (info pingParser (progDesc "Ping Riak"))
      , command "put" (info putParser (progDesc "Put an object"))
      , command "search" (info searchParser (progDesc "Perform a search"))
      , command "update-map" (info updateMapParser (progDesc "Update a map"))
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
      Object.delete handle object def >>= \case
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

getParser :: Parser (Handle -> IO ())
getParser =
  doGet
    <$> keyArgument
    <*> nOption
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
    doGet key n r pr handle =
      Object.get handle key opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right object -> do
          print object

      where
        opts :: GetOpts
        opts =
          GetOpts
            { basicQuorum = False
            , n = n
            , notfoundOk = Nothing
            , pr = pr
            , r = r
            , timeout = Nothing
            }

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

        Right val ->
          print val

infoParser :: Parser (Handle -> IO ())
infoParser =
  pure $ \handle ->
    ServerInfo.getServerInfo handle >>= \case
      Left err -> do
        print err
        exitFailure

      Right ServerInfo { name, version } -> do
        Text.putStrLn (name <> " " <> version)

pingParser :: Parser (Handle -> IO ())
pingParser =
  pure $ \handle ->
    ping handle >>= \case
      Left err -> do
        print err
        exitFailure

      Right () ->
        pure ()

putParser :: Parser (Handle -> IO ())
putParser =
  doPut
    <$> bucketOrKeyArgument
    <*> strArgument (help "Value" <> metavar "VALUE")
    <*> contentTypeOption
    <*> contextOption
    <*> nOption
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
    doPut bucketOrKey val type' context n w dw pw handle =
      Object.put handle object opts >>= \case
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
            , pw = pw
            , n = n
            , timeout = Nothing
            , w = w
            }



searchParser :: Parser (Handle -> IO ())
searchParser =
  doSearch
    <$> indexArgument
    <*> queryArgument

  where
    doSearch ::
         Text
      -> Text
      -> Handle
      -> IO ()
    doSearch index query handle =
      search handle index (encodeUtf8 query) def >>= \case
        Left err -> do
          print err
          exitFailure

        Right documents ->
          for_ documents printDocument

    indexArgument :: Parser Text
    indexArgument =
      strArgument (help "Search index" <> metavar "INDEX")

    queryArgument :: Parser Text
    queryArgument =
      strArgument (help "Query" <> metavar "QUERY")

    -- Assume the keys and values are UTF-8 encoded
    printDocument :: [(ByteString, ByteString)] -> IO ()
    printDocument =
      print . map (\(key, val) -> (decodeUtf8 key, decodeUtf8 val))

updateMapParser :: Parser (Handle -> IO ())
updateMapParser =
  doUpdateMap
    <$> bucketOrKeyArgument
    <*> contextOption
    <*> mapUpdateOptions

  where
    doUpdateMap ::
         Either Bucket Key
      -> Maybe Context
      -> [ConvergentMapUpdate]
      -> Handle
      -> IO ()
    doUpdateMap bucketOrKey context updates handle = do
      updateConvergentMap handle operation >>= \case
        Left err -> do
          print err
          exitFailure

        Right val ->
          print val

      where
        operation :: ConvergentMap [ConvergentMapUpdate]
        operation =
          ConvergentMap
            { context = fromMaybe newContext context
            , key =
                case bucketOrKey of
                  Left bucket -> generatedKey bucket
                  Right key -> key
            , value = updates
            }

--------------------------------------------------------------------------------
-- Arguments/options
--------------------------------------------------------------------------------

bucketOrKeyArgument :: Parser (Either Bucket Key)
bucketOrKeyArgument =
  argument
    (Right <$> eitherReader parseKey <|> Left <$> eitherReader parseBucket)
    (help "Bucket (type/bucket) or key (type/bucket/key)" <> metavar "BUCKET/KEY")

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

keyArgument :: Parser Key
keyArgument =
  argument (eitherReader parseKey) keyMod

mapUpdateOptions :: Parser [ConvergentMapUpdate]
mapUpdateOptions =
  many mapUpdateOption

  where
    mapUpdateOption :: Parser ConvergentMapUpdate
    mapUpdateOption =
      asum
        [ removeCounterOption
        , removeFlagOption
        , removeMapOption
        , removeRegisterOption
        , removeSetOption
        , incrementCounterOption
        , disableFlagOption
        , enableFlagOption
        , setRegisterOption
        , addElemOption
        , removeElemOption
        ]

    removeCounterOption :: Parser ConvergentMapUpdate
    removeCounterOption =
      makeMapUpdate (RemoveCounter . Latin1.pack) . splitOn "." <$>
        strOption (help "Remove a counter" <> long "remove-counter" <> metavar "PATH")

    removeFlagOption :: Parser ConvergentMapUpdate
    removeFlagOption =
      makeMapUpdate (RemoveFlag . Latin1.pack) . splitOn "." <$>
        strOption (help "Remove a flag" <> long "remove-flag" <> metavar "PATH")

    removeMapOption :: Parser ConvergentMapUpdate
    removeMapOption =
      makeMapUpdate (RemoveMap . Latin1.pack) . splitOn "." <$>
        strOption (help "Remove a map" <> long "remove-map" <> metavar "PATH")

    removeRegisterOption :: Parser ConvergentMapUpdate
    removeRegisterOption =
      makeMapUpdate (RemoveRegister . Latin1.pack) . splitOn "." <$>
        strOption (help "Remove a register" <> long "remove-register" <> metavar "PATH")

    removeSetOption :: Parser ConvergentMapUpdate
    removeSetOption =
      makeMapUpdate (RemoveSet . Latin1.pack) . splitOn "." <$>
        strOption (help "Remove a set" <> long "remove-set" <> metavar "PATH")

    incrementCounterOption :: Parser ConvergentMapUpdate
    incrementCounterOption =
      option
        (eitherReader parseIncrementCounter)
        (help "Increment a counter" <> long "incr-counter" <> metavar "PATH/AMOUNT")

      where
        parseIncrementCounter :: String -> Either String ConvergentMapUpdate
        parseIncrementCounter update =
          case splitOn "/" update of
            [ path, readMaybe -> Just val ] ->
              Right
                (makeMapUpdate
                  (\name -> UpdateCounter (Latin1.pack name) val)
                  (splitOn "." path))

            _ ->
              Left "Expected: 'path/amount'"

    disableFlagOption :: Parser ConvergentMapUpdate
    disableFlagOption =
      makeMapUpdate (\name -> UpdateFlag (Latin1.pack name) False) . splitOn "." <$>
        strOption (help "Disable a flag" <> long "disable-flag" <> metavar "PATH")

    enableFlagOption :: Parser ConvergentMapUpdate
    enableFlagOption =
      makeMapUpdate (\name -> UpdateFlag (Latin1.pack name) True) . splitOn "." <$>
        strOption (help "Enable a flag" <> long "enable-flag" <> metavar "PATH")

    setRegisterOption :: Parser ConvergentMapUpdate
    setRegisterOption =
      option
        (eitherReader parseSetRegister)
        (help "Set a register" <> long "set-register" <> metavar "PATH/VALUE")

      where
        parseSetRegister :: String -> Either String ConvergentMapUpdate
        parseSetRegister update =
          case splitOn "/" update of
            [ path, val ] ->
              Right
                (makeMapUpdate
                  (\name -> UpdateRegister (Latin1.pack name) (Latin1.pack val))
                  (splitOn "." path))

            _ ->
              Left "Expected: 'path/value'"


    addElemOption :: Parser ConvergentMapUpdate
    addElemOption =
      option
        (eitherReader (parseUpdateSet Add))
        (help "Add an element to a set" <> long "add-elem" <> metavar "PATH/VALUE")

    removeElemOption :: Parser ConvergentMapUpdate
    removeElemOption =
      option
        (eitherReader (parseUpdateSet Remove))
        (help "Add an element to a set" <> long "remove-elem" <> metavar "PATH/VALUE")

    parseUpdateSet ::
         (ByteString -> ConvergentSetUpdate)
      -> String
      -> Either String ConvergentMapUpdate
    parseUpdateSet toUpdate update =
      case splitOn "/" update of
        [ path, val ] ->
          Right
            (makeMapUpdate
              (\name -> UpdateSet (Latin1.pack name) [toUpdate (Latin1.pack val)])
              (splitOn "." path))

        _ ->
          Left "Expected: 'path/value'"

    makeMapUpdate ::
         (String -> ConvergentMapUpdate)
      -> [String]
      -> ConvergentMapUpdate
    makeMapUpdate onLeaf =
      loop

      where
        loop :: [String] -> ConvergentMapUpdate
        loop = \case
          [] ->
            undefined

          [leaf] ->
            onLeaf leaf

          p:ps ->
            UpdateMap (Latin1.pack p) [loop ps]

nOption :: Parser (Maybe Quorum)
nOption =
  optional
    (option
      (eitherReader parseQuorum)
        (help "N value" <> long "n" <> metavar "QUORUM"))

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
  (help "Key" <> metavar "(TYPE/)BUCKET/KEY")
