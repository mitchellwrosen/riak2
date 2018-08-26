{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, OverloadedLabels,
             OverloadedStrings, RankNTypes, ScopedTypeVariables,
             TypeApplications #-}

import Control.Monad              (join, when, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Coerce
import Data.Foldable              (asum, for_, toList, traverse_)
import Data.HashMap.Strict        (HashMap)
import Data.Int
import Data.Text                  (Text)
import Data.Word
import Lens.Family2
import List.Transformer           (runListT)
import Network.Socket             (HostName, PortNumber)
import Options.Applicative
import Prelude                    hiding (head, return)
import Text.Read                  (readMaybe)

import qualified Data.ByteString.Char8 as Latin1
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text

import Riak
import Riak.Internal.Protobuf

import qualified Riak.Lenses as L

-- TODO riak-cli take host/port as arguments

main :: IO ()
main =
  join
    (customExecParser
      (prefs (showHelpOnEmpty <> showHelpOnError))
      (info parser (progDesc "riak command-line client")))

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

parser :: Parser (IO ())
parser =
    nodeArgument
    -- TODO --help output is ugly here (COMMAND | COMMAND | ...), fix it
    <*>
    (asum . map (hsubparser . mconcat))
      [ [ commandGroup "Object operations"
        , fetchObjectParser
        , storeObjectParser
          -- TODO riak-cli store-new-object
        ]
      , [ commandGroup "Counter operations"
        , fetchCounterParser
        , updateCounterParser
          -- TODO riak-cli update-new-counter
        ]
      , [ commandGroup "Grow-only set opeations"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "HyperLogLog opeations"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "Map operators"
        , fetchMapParser
        ]
      , [ commandGroup "Set operations"
        , fetchSetParser
        ]
      , [ commandGroup "Bucket operations"
        , getBucketTypePropsParser
        , getBucketPropsParser
        , listBucketsParser
        , listKeysParser
        ]
      , [ commandGroup "MapReduce"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "Secondary indexes (2i)"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "Search 2.0"
        , getSchemaParser
        , getIndexParser
        ]
      , [ commandGroup "Server info"
        , pingParser
        , infoParser'
        ]
      ]

fetchCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
fetchCounterParser =
  command
    "get-counter"
    (info
      (doFetchCounter
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> keyArgument)
        -- TODO fetch-counter optional params
      (progDesc "Fetch a counter"))

fetchMapParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
fetchMapParser =
  command
    "get-map"
    (info
      (doFetchMap
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> keyArgument)
        -- TODO fetch-map optional params
      (progDesc "Fetch a map"))

fetchObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
fetchObjectParser =
  command
    "get"
    (info
      (doFetchObject
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> keyArgument
        <*> switch
              (mconcat
                [ long "basic-quorum"
                , help "Basic quorum"
                ])
        <*> switch
              (mconcat
                [ long "head"
                , help "Head"
                ])
        <*> switch
              (mconcat
                [ long "notfound-not-ok"
                , help "notfound not ok"
                ])
        <*> nOption
        <*> prOption
        <*> rOption
        <*> sloppyQuorumOption
        <*> timeoutOption)
      (progDesc "Fetch an object"))

fetchSetParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
fetchSetParser =
  command
    "get-set"
    (info
      (doFetchSet
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> keyArgument)
        -- TODO fetch-set optional params
      (progDesc "Fetch a set"))

getBucketPropsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getBucketPropsParser =
  command
    "get-bucket"
    (info
      (doGetBucketProps
        <$> bucketTypeArgument
        <*> bucketArgument)
      (progDesc "Get a bucket's properties"))

getBucketTypePropsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getBucketTypePropsParser =
  command
    "get-bucket-type"
    (info
      (doGetBucketTypeProps <$> bucketTypeArgument)
      (progDesc "Get a bucket type's properties"))

getIndexParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getIndexParser =
  command
    "get-index"
    (info
      (doGetIndex
        <$> (optional . fmap RiakIndexName . strArgument)
              (mconcat
                [ help "Index name"
                , metavar "INDEX"
                ]))
      (progDesc "Get a Solr index, or all Solr indexes"))

getSchemaParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getSchemaParser =
  command
    "get-schema"
    (info
      (doGetSchema
        <$> (fmap RiakSchemaName . strArgument)
              (mconcat
                [ help "Schema name"
                , metavar "SCHEMA"
                ]))
      (progDesc "Get a Solr schema"))

infoParser' :: Mod CommandFields (HostName -> PortNumber -> IO ())
infoParser' =
  command
    "info"
    (info (pure doGetServerInfo) (progDesc "Get server info"))

listBucketsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
listBucketsParser =
  command
    "list-buckets"
    (info
      (doListBuckets <$> bucketTypeArgument)
      (progDesc "List all buckets in a bucket type"))

listKeysParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
listKeysParser =
  command
    "list-keys"
    (info
      (doListKeys
        <$> bucketTypeArgument
        <*> bucketArgument)
      (progDesc "List all keys in a bucket"))

pingParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
pingParser =
  command
    "ping"
    (info (pure doPing) (progDesc "Ping the server"))

storeObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
storeObjectParser =
  command
    "put"
    (info
      (doStoreObject
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> keyArgument
        <*> strArgument
              (mconcat
                [ help "Content"
                , metavar "CONTENT"
                ])
        <*> dwOption
        <*> nOption
        <*> pwOption
        <*> asum
              [ flag'
                  'a'
                  (mconcat
                    [ long "head"
                    , help "Return head"
                    ])
              , flag'
                  'b'
                  (mconcat
                    [ long "body"
                    , help "Return body"
                    ])
              , pure 'c'
              ]
        <*> sloppyQuorumOption
        <*> timeoutOption
        <*> wOption)
      (progDesc "Store an object"))

updateCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
updateCounterParser =
  command
    "incr-counter"
    (info
      (doUpdateCounter
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> keyArgument
        <*> argument auto
              (mconcat
                [ help "Value"
                , metavar "VALUE"
                ]))
        -- TODO other update-counter optional params
      (progDesc "Update a counter"))

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

doFetchCounter
  :: RiakBucketType ('Just 'RiakCounterTy)
  -> RiakBucket
  -> RiakKey
  -> HostName
  -> PortNumber
  -> IO ()
doFetchCounter type' bucket key host port =
  withRiakHandle host port $ \h ->
    either print print =<<
      fetchRiakCounter h (RiakLocation (RiakNamespace type' bucket) key) def

doFetchMap
  :: RiakBucketType ('Just ('RiakMapTy (HashMap ByteString RiakMapValue)))
  -> RiakBucket
  -> RiakKey
  -> HostName
  -> PortNumber
  -> IO ()
doFetchMap type' bucket key host port =
  withRiakHandle host port $ \h ->
    either print print =<<
      fetchRiakMap h (RiakLocation (RiakNamespace type' bucket) key) def

doFetchObject
  :: RiakBucketType 'Nothing
  -> RiakBucket
  -> RiakKey
  -> Bool
  -> Bool
  -> Bool
  -> Maybe Word32
  -> Maybe RiakQuorum
  -> Maybe RiakQuorum
  -> Bool
  -> Maybe Word32
  -> HostName
  -> PortNumber
  -> IO ()
doFetchObject
    type' bucket key basic_quorum head notfound_not_ok n pr r no_sloppy_quorum
    timeout host port =
  withRiakHandle host port $ \h ->
    if head
      then go (fetchRiakObjectHead @Text h) (\_ _ -> pure ())
      else go (fetchRiakObject h) (\i s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))

 where
  go
    :: (RiakLocation 'Nothing -> FetchRiakObjectParams -> IO (Either RiakError [RiakContent a]))
    -> (Int -> a -> IO ())
    -> IO ()
  go fetch f = do
    eresponse <-
      fetch
        (RiakLocation (RiakNamespace type' bucket) key)
        (def
          & (if basic_quorum then #basic_quorum True else id)
          & (if notfound_not_ok then #notfound_ok False else id)
          & maybe id #n n
          & maybe id #pr pr
          & maybe id #r r
          & (if no_sloppy_quorum then #sloppy_quorum False else id)
          & maybe id #timeout timeout)

    case eresponse of
      Left err ->
        print err

      Right [] ->
        putStrLn "Not found"

      Right contents -> do
        -- printContents f contents
        for_ (zip [(0::Int)..] contents) $ \(i, content) ->
          printContent (f i) (Just i) content

doFetchSet
  :: RiakBucketType ('Just ('RiakSetTy Text))
  -> RiakBucket
  -> RiakKey
  -> HostName
  -> PortNumber
  -> IO ()
doFetchSet type' bucket key host port =
  withRiakHandle host port $ \h ->
    fetchRiakSet h (RiakLocation (RiakNamespace type' bucket) key) def >>= \case
      Left err -> print err
      Right vals -> for_ vals print -- TODO encoding?

doGetBucketProps
  :: RiakBucketType ty
  -> RiakBucket
  -> HostName
  -> PortNumber
  -> IO ()
doGetBucketProps type' bucket host port =
  withRiakHandle host port $ \h ->
    either print printBucketProps =<<
      getRiakBucketProps h (RiakNamespace type' bucket)

doGetBucketTypeProps :: RiakBucketType ty -> HostName -> PortNumber -> IO ()
doGetBucketTypeProps type' host port =
  withRiakHandle host port $ \h ->
    either print printBucketProps =<<
      getRiakBucketTypeProps h type'

doGetIndex :: Maybe RiakIndexName -> HostName -> PortNumber -> IO ()
doGetIndex index host port =
  withRiakHandle host port $ \h ->
    maybe
      (traverse_ print =<< getRiakIndexes h)
      (print <=< getRiakIndex h)
      index

doGetSchema :: RiakSchemaName -> HostName -> PortNumber -> IO ()
doGetSchema schema host port =
  withRiakHandle host port $ \h ->
    either print print =<<
      getRiakSchema h schema

doGetServerInfo :: HostName -> PortNumber -> IO ()
doGetServerInfo host port =
  withRiakHandle host port $ \h ->
    either print printServerInfo =<<
      getRiakServerInfo h

doListBuckets :: RiakBucketType ty -> HostName -> PortNumber -> IO ()
doListBuckets type' host port =
  withRiakHandle host port $ \h -> do
    result :: Either RiakError () <-
      (runExceptT . runListT)
        (listRiakBuckets h type' >>=
          liftIO . Latin1.putStrLn . coerce)
    either print (const (pure ())) result

doListKeys :: RiakBucketType ty -> RiakBucket -> HostName -> PortNumber -> IO ()
doListKeys type' bucket host port =
  withRiakHandle host port $ \h -> do
    result :: Either RiakError () <-
      (runExceptT . runListT)
        (listRiakKeys h (RiakNamespace type' bucket) >>=
          liftIO . Latin1.putStrLn . coerce)
    either print (const (pure ())) result

doPing :: HostName -> PortNumber -> IO ()
doPing host port =
  withRiakHandle host port $ \h ->
    either print (const (putStrLn "pong")) =<<
      pingRiak h

doStoreObject
  :: RiakBucketType 'Nothing
  -> RiakBucket
  -> RiakKey
  -> Text
  -> Maybe RiakQuorum
  -> Maybe Word32
  -> Maybe RiakQuorum
  -> Char
  -> Bool
  -> Maybe Word32
  -> Maybe RiakQuorum
  -> HostName
  -> PortNumber
  -> IO ()
doStoreObject
    type' bucket key content dw n pw return no_sloppy_quorum timeout w host
    port =
  withRiakHandle host port $ \h ->
    case return of
      'a' ->
        go
          (storeRiakObjectHead h)
          (\contents ->
            (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
              printContent (\_ -> pure ()) (Just i) content'))
      'b' ->
        go
          (storeRiakObjectBody h)
          (\contents ->
            (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
              printContent
                (\s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))
                (Just i)
                content'))

      'c' ->
        go (storeRiakObject h) pure
      _   -> undefined
 where
  go
    :: (RiakLocation 'Nothing -> Text -> StoreRiakObjectParams -> IO (Either RiakError a))
    -> (a -> IO ())
    -> IO ()
  go store f = do
    eresponse <-
      store
        (RiakLocation (RiakNamespace type' bucket) key)
        content
        (def
          & maybe id #dw dw
          & maybe id #n n
          & maybe id #pw pw
          & (if no_sloppy_quorum then #sloppy_quorum False else id)
          & maybe id #timeout timeout
          & maybe id #w w)

    either print f eresponse

doUpdateCounter
  :: RiakBucketType ('Just 'RiakCounterTy)
  -> RiakBucket
  -> RiakKey
  -> Int64
  -> HostName
  -> PortNumber
  -> IO ()
doUpdateCounter type' bucket key incr host port =
  withRiakHandle host port $ \h ->
    either print print =<<
      updateRiakCounter h (RiakLocation (RiakNamespace type' bucket) key) incr def

--------------------------------------------------------------------------------
-- Misc. helpers
--------------------------------------------------------------------------------

printBucketProps :: RpbBucketProps -> IO ()
printBucketProps props = do
  p "allow_mult"      (fmap showBool . view L.maybe'allowMult)
  p "backend"         (fmap show . view L.maybe'backend)
  p "basic_quorum"    (fmap showBool . view L.maybe'basicQuorum)
  p "big_vclock"      (fmap show . view L.maybe'bigVclock)
  p "chash_keyfun"    (fmap showModFun . view L.maybe'chashKeyfun)
  p "consistent"      (fmap showBool . view L.maybe'consistent)
  p "datatype"        (fmap show . view L.maybe'datatype)
  p "dw"              (fmap show . view L.maybe'dw)
  p "has_precommit"   (fmap showBool . view L.maybe'hasPrecommit)
  p "hll_precision"   (fmap show . view L.maybe'hllPrecision)
  p "last_write_wins" (fmap showBool . view L.maybe'lastWriteWins)
  p "linkfun"         (fmap showModFun . view L.maybe'linkfun)
  p "n"               (fmap show . view L.maybe'nVal)
  p "notfound_ok"     (fmap showBool . view L.maybe'notfoundOk)
  p "old_vclock"      (fmap show . view L.maybe'oldVclock)
  p "postcommit"      (fmap show . view L.maybe'hasPostcommit)
  p "pr"              (fmap showQuorum . view L.maybe'pr)
  p "precommit"       (\case { [] -> Nothing; xs -> Just (show xs) } . view L.precommit)
  p "pw"              (fmap showQuorum . view L.maybe'pw)
  p "r"               (fmap showQuorum . view L.maybe'r)
  p "repl"            (fmap show . view L.maybe'repl)
  p "rw"              (fmap showQuorum . view L.maybe'rw)
  p "search"          (fmap showBool . view L.maybe'search)
  p "search_index"    (fmap show . view L.maybe'searchIndex)
  p "small_vclock"    (fmap show . view L.maybe'smallVclock)
  p "ttl"             (fmap show . view L.maybe'ttl)
  p "w"               (fmap showQuorum . view L.maybe'w)
  p "write_once"      (fmap showBool . view L.maybe'writeOnce)
  p "young_vclock"    (fmap show . view L.maybe'youngVclock)
 where
  p :: String -> (RpbBucketProps -> Maybe String) -> IO ()
  p s f =
    for_ (f props) (\x -> putStrLn (s ++ " = " ++ x))

printContent :: (a -> IO ()) -> Maybe Int -> RiakContent a -> IO ()
printContent f mi content = do
  tag "location" (showLocation (content ^. L.location))

  f (content ^. L.value)

  for_ (content ^. L.lastMod) (tag "last_mod")

  case unRiakMetadata (content ^. L.usermeta) of
    [] -> pure ()
    xs -> tag "metadata" xs

  case content ^. L.indexes of
    [] -> pure ()
    xs -> tag "indexes" xs

  when (content ^. L.deleted)
    (tag "deleted" (content ^. L.deleted))

  for_ (unTTL (content ^. L.ttl)) (tag "ttl")
 where
  tag :: Show a => String -> a -> IO ()
  tag k v =
    putStrLn (k ++ maybe "" (\i -> "[" ++ show i ++ "]") mi ++ " = " ++ show v)

showBool :: Bool -> String
showBool = \case
  False -> "false"
  True  -> "true"

showLocation :: RiakLocation ty -> String
showLocation (RiakLocation (RiakNamespace type' bucket) key) =
  show type' ++ "/" ++ show bucket ++ "/" ++ show key

showModFun :: RpbModFun -> String
showModFun fun =
  Latin1.unpack (fun ^. L.module') ++ ":" ++ Latin1.unpack (fun ^. L.function)

showQuorum :: Word32 -> String
showQuorum = \case
  4294967291 -> "default"
  4294967292 -> "all"
  4294967293 -> "quorum"
  4294967294 -> "one"
  n          -> show n

printServerInfo :: RpbGetServerInfoResp -> IO ()
printServerInfo info' = do
  for_ (info' ^. L.maybe'node)
    (putStrLn . ("node = " ++) . Latin1.unpack)

  for_ (info' ^. L.maybe'serverVersion)
    (putStrLn . ("version = " ++) . Latin1.unpack)

bucketArgument :: Parser RiakBucket
bucketArgument =
  (fmap RiakBucket . strArgument)
    (mconcat
      [ help "Bucket"
      , metavar "BUCKET"
      ])

bucketTypeArgument :: Parser (RiakBucketType ty)
bucketTypeArgument =
  (fmap RiakBucketType . strArgument)
    (mconcat
      [ help "Bucket type"
      , metavar "TYPE"
      ])

keyArgument :: Parser RiakKey
keyArgument =
  (fmap RiakKey . strArgument)
    (mconcat
      [ help "Key"
      , metavar "KEY"
      ])

dwOption :: Parser (Maybe RiakQuorum)
dwOption =
  quorumOption "dw" "DW value"

nodeArgument :: Parser ((HostName -> PortNumber -> r) -> r)
nodeArgument =
  argument
    (maybeReader readNode)
    (mconcat
      [ help "Riak node"
      , metavar "NODE"
      ])
 where
  readNode :: String -> Maybe ((HostName -> PortNumber -> r) -> r)
  readNode s = do
    case span (/= ':') s of
      (host, ':':port) -> do
        port' <- readMaybe port
        pure (\k -> k host port')
      (host, []) ->
        pure (\k -> k host 8087)
      _ -> undefined

nOption :: Parser (Maybe Word32)
nOption =
  (optional . option auto)
    (mconcat
      [ long "n"
      , help "N value"
      , metavar "NODES"
      ])

prOption :: Parser (Maybe RiakQuorum)
prOption =
  quorumOption "pr" "PR value"

pwOption :: Parser (Maybe RiakQuorum)
pwOption =
  quorumOption "pw" "PW value"

quorumOption :: String -> String -> Parser (Maybe RiakQuorum)
quorumOption s1 s2 =
  optional (option (maybeReader readQuorum)
    (mconcat
      [ long s1
      , help s2
      , metavar "QUORUM"
      ]))
 where
  readQuorum :: String -> Maybe RiakQuorum
  readQuorum = \case
    "all"     -> pure RiakQuorumAll
    "quorum"  -> pure RiakQuorumQuorum
    s         -> RiakQuorum <$> readMaybe s

rOption :: Parser (Maybe RiakQuorum)
rOption =
  quorumOption "r" "R value"

sloppyQuorumOption :: Parser Bool
sloppyQuorumOption =
  switch
    (mconcat
      [ long "no-sloppy-quorum"
      , help "No sloppy quorum"
      ])

timeoutOption :: Parser (Maybe Word32)
timeoutOption =
  (optional . option auto)
    (mconcat
      [ long "timeout"
      , help "Timeout"
      , metavar "MILLISECONDS"
      ])

wOption :: Parser (Maybe RiakQuorum)
wOption =
  quorumOption "w" "W value"
