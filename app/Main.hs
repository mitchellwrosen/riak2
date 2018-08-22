{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, OverloadedLabels,
             OverloadedStrings, RankNTypes, ScopedTypeVariables,
             TypeApplications #-}

import Control.Monad              (join, when, (<=<))
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Coerce
import Data.Foldable              (asum, for_, toList, traverse_)
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
import Riak.Internal.Protobuf ()

import qualified Riak.Internal.Protobuf as L

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
      [ [ commandGroup "Key/value object operations"
        , fetchObjectParser
        , storeObjectParser
          -- TODO riak-cli store-new-object
        ]
      , [ commandGroup "Data type operations"
        , fetchCounterParser
        , fetchMapParser
        , updateCounterParser
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
        , getIndexParser
        ]
      , [ commandGroup "Server info"
        , command "TODO" (info empty mempty)
        ]
      ]

fetchCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
fetchCounterParser =
  command
    "fetch-counter"
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
    "fetch-map"
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
    "fetch-object"
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

getBucketPropsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getBucketPropsParser =
  command
    "get-bucket-props"
    (info
      (doGetBucketProps
        <$> bucketTypeArgument
        <*> bucketArgument)
      (progDesc "Get a bucket's properties"))

getBucketTypePropsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getBucketTypePropsParser =
  command
    "get-bucket-type-props"
    (info
      (doGetBucketTypeProps <$> bucketTypeArgument)
      (progDesc "Get a bucket type's properties"))

getIndexParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getIndexParser =
  command
    "get-index"
    (info
      (doGetIndex
        <$> (optional . fmap IndexName . strArgument)
              (mconcat
                [ help "Index name"
                , metavar "INDEX"
                ]))
      (progDesc "Get a Solr index, or all Solr indexes"))

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

storeObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
storeObjectParser =
  command
    "store-object"
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
        <*> ttlOption
        <*> wOption)
      (progDesc "Store an object"))

updateCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
updateCounterParser =
  command
    "update-counter"
    (info
      (doUpdateCounter
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> argument auto
              (mconcat
                [ help "Value"
                , metavar "VALUE"
                ])
        <*> optional keyOption)
        -- TODO other update-counter optional params
      (progDesc "Update a counter"))

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

doFetchCounter
  :: BucketType ('Just 'CounterTy)
  -> Bucket
  -> Key
  -> HostName
  -> PortNumber
  -> IO ()
doFetchCounter type' bucket key host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    print =<<
      fetchCounter h
        type'
        bucket
        key
        def

doFetchMap
  :: BucketType ('Just 'MapTy)
  -> Bucket
  -> Key
  -> HostName
  -> PortNumber
  -> IO ()
doFetchMap type' bucket key host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    print =<<
      fetchMap h
        type'
        bucket
        key
        def -- (def, def, def, def, def, def, def, def)

doFetchObject
  :: BucketType 'Nothing
  -> Bucket
  -> Key
  -> Bool
  -> Bool
  -> Bool
  -> Maybe Word32
  -> Maybe Quorum
  -> Maybe Quorum
  -> Bool
  -> Maybe Word32
  -> HostName
  -> PortNumber
  -> IO ()
doFetchObject
    type' bucket key basic_quorum head notfound_not_ok n pr r no_sloppy_quorum
    timeout host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    if head
      then go (fetchObjectHead @Text h) (\_ _ -> pure ())
      else go (fetchObject h) (\i s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))

 where
  go
    :: (BucketType 'Nothing -> Bucket -> Key -> FetchObjectParams -> IO (Either L.RpbErrorResp [Content a]))
    -> (Int -> a -> IO ())
    -> IO ()
  go fetch f = do
    eresponse <-
      fetch
        type'
        bucket
        key
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

doGetBucketProps :: BucketType ty -> Bucket -> HostName -> PortNumber -> IO ()
doGetBucketProps type' bucket host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    print =<<
      getBucketProps h type' bucket

doGetBucketTypeProps :: BucketType ty -> HostName -> PortNumber -> IO ()
doGetBucketTypeProps type' host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    print =<<
      getBucketTypeProps h type'

doGetIndex :: Maybe IndexName -> HostName -> PortNumber -> IO ()
doGetIndex index host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    maybe
      (traverse_ print =<< getIndexes h)
      (print <=< getIndex h)
      index

doListBuckets :: BucketType ty -> HostName -> PortNumber -> IO ()
doListBuckets type' host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h -> do
    result :: Either L.RpbErrorResp () <-
      (runExceptT . runListT)
        (listBuckets h type' >>= liftIO . Latin1.putStrLn . coerce)
    either print (const (pure ())) result

doListKeys :: BucketType ty -> Bucket -> HostName -> PortNumber -> IO ()
doListKeys type' bucket host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h -> do
    result :: Either L.RpbErrorResp () <-
      (runExceptT . runListT)
        (listKeys h type' bucket >>= liftIO . Latin1.putStrLn . coerce)
    either print (const (pure ())) result

doStoreObject
  :: BucketType 'Nothing
  -> Bucket
  -> Key
  -> Text
  -> Maybe Quorum
  -> Maybe Word32
  -> Maybe Quorum
  -> Char
  -> Bool
  -> Maybe Word32
  -> Maybe Word32
  -> Maybe Quorum
  -> HostName
  -> PortNumber
  -> IO ()
doStoreObject
    type' bucket key content dw n pw return no_sloppy_quorum timeout ttl w host
    port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    case return of
      'a' ->
        go
          (storeObjectHead h)
          (\contents ->
            (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
              printContent (\_ -> pure ()) (Just i) content'))
      'b' ->
        go
          (storeObjectBody h)
          (\contents ->
            (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
              printContent
                (\s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))
                (Just i)
                content'))

      'c' ->
        go (storeObject h) pure
      _   -> undefined
 where
  go
    :: (BucketType 'Nothing -> Bucket -> Key -> Text -> StoreObjectParams -> IO (Either L.RpbErrorResp a))
    -> (a -> IO ())
    -> IO ()
  go store f = do
    eresponse <-
      store
        type'
        bucket
        key
        content
        (def
          & maybe id #dw dw
          & maybe id #n n
          & maybe id #pw pw
          & (if no_sloppy_quorum then #sloppy_quorum False else id)
          & maybe id #timeout timeout
          & maybe id #ttl ttl
          & maybe id #w w)

    either print f eresponse

doUpdateCounter
  :: BucketType ('Just 'CounterTy)
  -> Bucket
  -> Int64
  -> Maybe Key
  -> HostName
  -> PortNumber
  -> IO ()
doUpdateCounter type' bucket incr key host port = do
  cache <- refVclockCache
  withHandle host port cache $ \h ->
    print =<<
      updateCounter h
        type'
        bucket
        key
        incr
        def

--------------------------------------------------------------------------------
-- Misc. helpers
--------------------------------------------------------------------------------

printContent :: (a -> IO ()) -> Maybe Int -> Content a -> IO ()
printContent f mi content = do
  tag "type" (content ^. L.type')
  tag "bucket" (content ^. L.bucket)
  tag "key" (content ^. L.key)

  f (content ^. L.value)

  for_ (content ^. L.lastMod) (tag "last_mod")

  case unMetadata (content ^. L.usermeta) of
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

bucketArgument :: Parser Bucket
bucketArgument =
  (fmap Bucket . strArgument)
    (mconcat
      [ help "Bucket"
      , metavar "BUCKET"
      ])

bucketTypeArgument :: Parser (BucketType ty)
bucketTypeArgument =
  (fmap BucketType . strArgument)
    (mconcat
      [ help "Bucket type"
      , metavar "TYPE"
      ])

keyArgument :: Parser Key
keyArgument =
  (fmap Key . strArgument)
    (mconcat
      [ help "Key"
      , metavar "KEY"
      ])

keyOption :: Parser Key
keyOption =
  (fmap Key . strOption)
    (mconcat
      [ long "key"
      , help "Key"
      , metavar "KEY"
      ])

dwOption :: Parser (Maybe Quorum)
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

prOption :: Parser (Maybe Quorum)
prOption =
  quorumOption "pr" "PR value"

pwOption :: Parser (Maybe Quorum)
pwOption =
  quorumOption "pw" "PW value"

quorumOption :: String -> String -> Parser (Maybe Quorum)
quorumOption s1 s2 =
  optional (option (maybeReader readQuorum)
    (mconcat
      [ long s1
      , help s2
      , metavar "QUORUM"
      ]))
 where
  readQuorum :: String -> Maybe Quorum
  readQuorum = \case
    "all"     -> pure QuorumAll
    "quorum"  -> pure QuorumQuorum
    s         -> Quorum <$> readMaybe s

rOption :: Parser (Maybe Quorum)
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

ttlOption :: Parser (Maybe Word32)
ttlOption =
  (optional . option auto)
    (mconcat
      [ long "ttl"
      , help "TTL"
      , metavar "MILLISECONDS"
      ])

wOption :: Parser (Maybe Quorum)
wOption =
  quorumOption "w" "W value"
