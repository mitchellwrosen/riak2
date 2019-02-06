module Main where

import Riak        (Bucket(..), Content(..), Key(..), PutOpts(..), Quorum(..),
                    ServerInfo(..))
import Riak.Client (Client)
import Riak.Socket (Socket)

import qualified Riak.Client                as Client
import qualified Riak.Context               as Context
import qualified Riak.Interface.Impl.Socket as Client
import qualified Riak.Key                   as Key
import qualified Riak.Object                as Object
import qualified Riak.ServerInfo            as ServerInfo
import qualified Riak.Socket                as Socket

import Data.ByteString     (ByteString)
import Data.Default.Class  (def)
import Data.Foldable       (for_)
import Data.List.Split     (splitOn)
import Data.Text           (Text)
import Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import Network.Socket      (HostName, PortNumber)
import Options.Applicative hiding (infoParser)
import System.Exit         (exitFailure)
import Text.Read           (readMaybe)

import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Text.IO          as Text

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
    Socket.new1 host port

  let
    config :: Client.Config
    config =
      Client.Config
        { Client.socket = socket
        , Client.handlers =
            Client.EventHandlers
              { Client.onSend =
                  if verbose
                    then \msg -> putStrLn (">>> " ++ show msg)
                    else mempty
              , Client.onReceive =
                  if verbose
                    then \msg -> putStrLn ("<<< " ++ show msg)
                    else mempty
              }
        }

  Client.withInterface config run

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

commandParser :: Parser (Client -> IO ())
commandParser =
  hsubparser
    (mconcat
      [ command "get" (info getParser (progDesc "Get an object"))
      , command "info" (info infoParser (progDesc "Get Riak info"))
      , command "ping" (info pingParser (progDesc "Ping Riak"))
      , command "put" (info putParser (progDesc "Put an object"))
      ])

getParser :: Parser (Client -> IO ())
getParser =
  doGet
    <$> argument (eitherReader parseKey) keyMod
  where
    doGet :: Key -> Client -> IO ()
    doGet key client =
      Object.get client key def >>= \case
        Left err -> do
          Text.putStrLn err
          exitFailure

        Right siblings -> do
          for_ siblings print

infoParser :: Parser (Client -> IO ())
infoParser =
  pure $ \client ->
    ServerInfo.get client >>= \case
      Left err -> do
        Text.putStrLn err
        exitFailure

      Right ServerInfo { name, version } -> do
        Text.putStrLn (name <> " " <> version)

pingParser :: Parser (Client -> IO ())
pingParser =
  pure $ \client ->
    Client.ping client >>= \case
      Left err -> do
        Text.putStrLn err
        exitFailure

      Right () ->
        pure ()

putParser :: Parser (Client -> IO ())
putParser =
      -- { dw = Quorum.Default
      -- , pw = Quorum.Default
      -- , n = Quorum.Default
      -- , sloppyQuorum = True
      -- , timeout = Nothing
      -- , w = Quorum.Default
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
      -> Client
      -> IO ()
    doPut bucketOrKey val n w dw pw client =
      Object.put client content opts >>= \case
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
            , context = Context.none
            , encoding = Nothing
            , indexes = []
            , key =
                case bucketOrKey of
                  Left (Bucket bucketType bucket) ->
                    Key bucketType bucket Key.none
                  Right key ->
                    key
            , metadata = []
            , type' = Nothing
            , value = encodeUtf8 val
            }

        opts :: PutOpts
        opts =
          PutOpts
            { dw = dw
            , pw = pw
            , n = n
            , sloppyQuorum = True
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
    (help "Bucket (type/bucket) or key (type/bucket/key)" <> metavar "BUCKET/KEY")

keyArgument :: Parser Key
keyArgument =
  argument (eitherReader parseKey) keyMod

nOption :: Parser (Maybe Quorum)
nOption =
  optional (option (eitherReader parseQuorum) (long "n" <> help "N"))

wOption :: Parser (Maybe Quorum)
wOption =
  optional (option (eitherReader parseQuorum) (long "w" <> help "W"))

dwOption :: Parser (Maybe Quorum)
dwOption =
  optional (option (eitherReader parseQuorum) (long "dw" <> help "DW"))

pwOption :: Parser (Maybe Quorum)
pwOption =
  optional (option (eitherReader parseQuorum) (long "pw" <> help "PW"))

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
