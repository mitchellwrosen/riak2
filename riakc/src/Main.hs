module Main where

import Riak                       (Bucket(..), Content(..), Context,
                                   ConvergentMap(..), ConvergentMapUpdate(..),
                                   ConvergentSetUpdate(..), GetOpts(..),
                                   Key(..), PutOpts(..), Quorum(..),
                                   ServerInfo(..), generatedKey,
                                   getConvergentMap, newContext, ping,
                                   unsafeMakeContext, updateConvergentMap)
import Riak.Handle.Impl.Exclusive (Socket)
import Riak.Handle.Impl.Managed   (Handle)

import qualified Riak.Handle.Impl.Exclusive as Handle (Config(..),
                                                       EventHandlers(..), new1)
import qualified Riak.Handle.Impl.Managed   as Handle hiding (Config)
import qualified Riak.Object                as Object
import qualified Riak.ServerInfo            as ServerInfo

import Data.ByteString     (ByteString)
import Data.Foldable       (asum, for_)
import Data.List.Split     (splitOn)
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import Network.Socket      (HostName, PortNumber)
import Options.Applicative hiding (infoParser)
import System.Exit         (exitFailure)
import Text.Read           (readMaybe)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.Text.IO           as Text

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

  socket :: Socket <-
    Handle.new1 host port

  let
    config :: Handle.Config
    config =
      Handle.Config
        { Handle.socket = socket
        , Handle.handlers =
            Handle.EventHandlers
              { Handle.onSend =
                  if verbose
                    then \msg -> putStrLn (">>> " ++ show msg)
                    else mempty
              , Handle.onReceive =
                  if verbose
                    then \msg -> putStrLn ("<<< " ++ show msg)
                    else mempty
              }
        }

  Handle.withHandle config run

nodeParser :: Parser (HostName, PortNumber)
nodeParser =
  argument
    (eitherReader parseNode)
    (help "Riak node, e.g. localhost:8087" <> metavar "NODE")

  where
    parseNode :: String -> Either String (HostName, PortNumber)
    parseNode s =
      maybe (Left "Expected: 'host' or 'host:port'") Right $ do
        case span (/= ':') s of
          (mkHost -> host, ':':port) -> do
            port' <- readMaybe port
            pure (host, port')
          (mkHost -> host, []) ->
            pure (host, 8087)
          _ -> undefined

    mkHost :: String -> String
    mkHost = \case
      "" -> "localhost"
      host -> host

verboseParser :: Parser Bool
verboseParser =
  switch (short 'v' <> long "verbose" <> help "Verbose")

commandParser :: Parser (Handle -> IO ())
commandParser =
  hsubparser
    (mconcat
      [ command "get" (info getParser (progDesc "Get an object"))
      , command "get-map" (info getMapParser (progDesc "Get a map"))
      , command "info" (info infoParser (progDesc "Get Riak info"))
      , command "ping" (info pingParser (progDesc "Ping Riak"))
      , command "put" (info putParser (progDesc "Put an object"))
      , command "update-map" (info updateMapParser (progDesc "Update a map"))
      ])

getParser :: Parser (Handle -> IO ())
getParser =
  doGet
    <$> argument (eitherReader parseKey) keyMod
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

        Right siblings -> do
          for_ siblings print

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
        Latin1.putStrLn err
        exitFailure

      Right ServerInfo { name, version } -> do
        Text.putStrLn (name <> " " <> version)

pingParser :: Parser (Handle -> IO ())
pingParser =
  pure $ \handle ->
    ping handle >>= \case
      Left err -> do
        Latin1.putStrLn err
        exitFailure

      Right () ->
        pure ()

putParser :: Parser (Handle -> IO ())
putParser =
  doPut
    <$> bucketOrKeyArgument
    <*> strArgument (help "Value" <> metavar "VALUE")
    <*> nOption
    <*> wOption
    <*> dwOption
    <*> pwOption
  where
    doPut ::
         Either Bucket Key
      -> Text
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Maybe Quorum
      -> Handle
      -> IO ()
    doPut bucketOrKey val n w dw pw handle =
      Object.put handle content opts >>= \case
        Left err -> do
          print err
          exitFailure

        Right (Key _ _ key') ->
          case bucketOrKey of
            Left _  -> Text.putStrLn (decodeUtf8 key')
            Right _ -> pure ()

      where
        content :: Content ByteString
        content =
          Content
            { charset = Nothing
            , context = newContext
            , encoding = Nothing
            , indexes = []
            , key =
                case bucketOrKey of
                  Left bucket -> generatedKey bucket
                  Right key -> key
            , metadata = []
            , ttl = Nothing
            , type' = Nothing
            , value = encodeUtf8 val
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
        strOption (long "remove-counter" <> help "Remove a counter")

    removeFlagOption :: Parser ConvergentMapUpdate
    removeFlagOption =
      makeMapUpdate (RemoveFlag . Latin1.pack) . splitOn "." <$>
        strOption (long "remove-flag" <> help "Remove a flag")

    removeMapOption :: Parser ConvergentMapUpdate
    removeMapOption =
      makeMapUpdate (RemoveMap . Latin1.pack) . splitOn "." <$>
        strOption (long "remove-map" <> help "Remove a map")

    removeRegisterOption :: Parser ConvergentMapUpdate
    removeRegisterOption =
      makeMapUpdate (RemoveRegister . Latin1.pack) . splitOn "." <$>
        strOption (long "remove-register" <> help "Remove a register")

    removeSetOption :: Parser ConvergentMapUpdate
    removeSetOption =
      makeMapUpdate (RemoveSet . Latin1.pack) . splitOn "." <$>
        strOption (long "remove-set" <> help "Remove a set")

    incrementCounterOption :: Parser ConvergentMapUpdate
    incrementCounterOption =
      option
        (eitherReader parseIncrementCounter)
        (long "incr-counter" <> help "Increment a counter")

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
        strOption (long "disable-flag" <> help "Disable a flag")

    enableFlagOption :: Parser ConvergentMapUpdate
    enableFlagOption =
      makeMapUpdate (\name -> UpdateFlag (Latin1.pack name) True) . splitOn "." <$>
        strOption (long "enable-flag" <> help "Enable a flag")

    setRegisterOption :: Parser ConvergentMapUpdate
    setRegisterOption =
      option
        (eitherReader parseSetRegister)
        (long "set-register" <> help "Set a register")

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
        (long "add-elem" <> help "Add an element to a set")

    removeElemOption :: Parser ConvergentMapUpdate
    removeElemOption =
      option
        (eitherReader (parseUpdateSet Remove))
        (long "remove-elem" <> help "Add an element to a set")

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
  optional (option (eitherReader parseQuorum) (long "n" <> help "N"))

prOption :: Parser (Maybe Quorum)
prOption =
  optional (option (eitherReader parseQuorum) (long "pr" <> help "PR"))

pwOption :: Parser (Maybe Quorum)
pwOption =
  optional (option (eitherReader parseQuorum) (long "pw" <> help "PW"))

rOption :: Parser (Maybe Quorum)
rOption =
  optional (option (eitherReader parseQuorum) (long "r" <> help "R"))

wOption :: Parser (Maybe Quorum)
wOption =
  optional (option (eitherReader parseQuorum) (long "w" <> help "W"))



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
      Right (Of q)

keyMod :: HasMetavar f => Mod f a
keyMod =
  (help "Key (type/bucket/key)" <> metavar "KEY")
