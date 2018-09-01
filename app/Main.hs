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

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1
import qualified Data.ByteString.UTF8   as Utf8
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text

import Riak
import Riak.Internal (RpbBucketProps, RpbGetServerInfoResp, RpbModFun)

import qualified Riak.Lenses as L

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
        , getBinaryObjectParser
        , getTextObjectParser
        , putObjectParser
          -- TODO riak-cli put-new-object
        , deleteObjectParser
        ]
      , [ commandGroup "Counter operations"
        , getCounterParser
        , updateCounterParser
          -- TODO riak-cli update-new-counter
        ]
      , [ commandGroup "Grow-only set operations"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "HyperLogLog operations"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "Map operations"
        , getMapParser
        ]
      , [ commandGroup "Set operations"
        , getSetParser
        ]
      , [ commandGroup "Bucket operations"
        , getBucketPropsParser
        ]
      , [ commandGroup "Full key traversal operations"
        , listParser
        ]
      , [ commandGroup "MapReduce"
        , command "TODO" (info empty mempty)
        ]
      , [ commandGroup "Secondary indexes (2i)"
        , queryParser
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

deleteObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
deleteObjectParser =
  command
    "delete"
    (info
      (doDeleteObject
        <$> locationArgument)
        -- TODO delete optional params
      (progDesc "Delete an object"))

getBinaryObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getBinaryObjectParser =
  command
    "get-binary"
    (info
      (doGetBinaryObject
        <$> locationArgument
        <*> switch
              (mconcat
                [ long "head"
                , help "Head"
                ])
        <*> getObjectParamsOptions)
      (progDesc "Get a binary object"))

getCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getCounterParser =
  command
    "get-counter"
    (info
      (doGetCounter
        <$> locationArgument)
        -- TODO get-counter optional params
      (progDesc "Get a counter"))

getMapParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getMapParser =
  command
    "get-map"
    (info
      (doGetMap
        <$> locationArgument)
        -- TODO get-map optional params
      (progDesc "Get a map"))

getTextObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getTextObjectParser =
  command
    "get-text"
    (info
      (doGetTextObject
        <$> locationArgument
        <*> switch
              (mconcat
                [ long "head"
                , help "Head"
                ])
        <*> getObjectParamsOptions)
      (progDesc "Get a text object"))

getSetParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getSetParser =
  command
    "get-set"
    (info
      (doGetSet
        <$> locationArgument)
        -- TODO get-set optional params
      (progDesc "Get a set"))

getBucketPropsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
getBucketPropsParser =
  command
    "get-bucket"
    (info
      (doGetBucketProps
        <$> bucketTypeArgument
        <*> optional bucketArgument)
      (progDesc "Get a bucket or bucket type's properties"))

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
        <$> (fmap SolrSchemaName . strArgument)
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

listParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
listParser =
  command
    "list"
    (info
      (doStream <$> bucketTypeArgument <*> optional bucketArgument)
      (progDesc "List all buckets or keys"))

pingParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
pingParser =
  command
    "ping"
    (info (pure doPing) (progDesc "Ping the server"))

putObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
putObjectParser =
  command
    "put"
    (info
      (doPutObject
        <$> locationArgument
        <*> strArgument
              (mconcat
                [ help "Content"
                , metavar "CONTENT"
                ])
        <*> many secondaryIndexOption
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
      (progDesc "Put an object"))

queryParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
queryParser =
  command
    "2i"
    (info
      (do2i
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> (RiakIndexName <$>
              strArgument
                (mconcat
                  [ help "Secondary index name"
                  , metavar "INDEX"
                  ]))
        <*> strArgument
              (mconcat
                [ help "Value (integer or string)"
                , metavar "VALUE"
                ])
        <*> optional
              (strArgument
                (mconcat
                  [ help "Value (integer or string)"
                  , metavar "VALUE"
                  ])))
      (progDesc "Search using secondary indexes"))

-- TODO riak-cli allow decrementing counters
updateCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
updateCounterParser =
  command
    "incr-counter"
    (info
      (doUpdateCounter
        <$> locationArgument
        <*> argument auto
              (mconcat
                [ help "Amount"
                , metavar "N"
                ]))
        -- TODO other update-counter optional params
      (progDesc "Increment or decrement a counter"))

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

do2i
  :: RiakBucketType 'Nothing
  -> RiakBucket
  -> RiakIndexName
  -> [Char]
  -> Maybe [Char]
  -> HostName
  -> PortNumber
  -> IO ()
do2i type' bucket index key1 key2 host port = do
  h <- createRiakHandle host port
  either print (\() -> pure ()) =<<
    case key2 of
      Nothing ->
        let
          query :: RiakExactQuery
          query =
            case readMaybe key1 of
              Just n  -> RiakExactQueryInt index n
              Nothing -> RiakExactQueryBin index (Utf8.fromString key1)
        in
          riakExactQuery h (RiakNamespace type' bucket) query
            (\resp -> (runExceptT . runListT) (resp >>= liftIO . print))

      Just key2' ->
        case (readMaybe key1, readMaybe key2') of
          (Just n, Just m) ->
            riakRangeQueryTerms
              h
              (RiakNamespace type' bucket)
              (RiakRangeQueryInt index n m)
              (\resp ->
                (runExceptT . runListT)
                (resp >>=
                  liftIO . (\(x, y) -> putStrLn (show x ++ " " ++ show y))))

          _ ->
            riakRangeQueryTerms
              h
              (RiakNamespace type' bucket)
              (RiakRangeQueryBin
                index
                (Utf8.fromString key1)
                (Utf8.fromString key2'))
              (\resp ->
                (runExceptT . runListT)
                (resp >>=
                  liftIO . (\(x, y) -> putStrLn (show x ++ " " ++ show y))))

doDeleteObject :: RiakLocation ty -> HostName -> PortNumber -> IO ()
doDeleteObject loc host port = do
  h <- createRiakHandle host port
  either print (\() -> pure ()) =<<
    deleteRiakObject h loc

doGetBinaryObject
  :: RiakLocation 'Nothing
  -> Bool
  -> GetRiakObjectParams
  -> HostName
  -> PortNumber
  -> IO ()
doGetBinaryObject =
  doGetObject
    (\i s ->
      Latin1.putStrLn
        ("value[" <> Latin1.pack (show i) <> "] = " <> Base64.encode s))

doGetBucketProps
  :: RiakBucketType ty
  -> Maybe RiakBucket
  -> HostName
  -> PortNumber
  -> IO ()
doGetBucketProps type' bucket host port = do
  h <- createRiakHandle host port
  either print printBucketProps =<<
    maybe
      (getRiakBucketTypeProps h type')
      (getRiakBucketProps h . RiakNamespace type')
      bucket

doGetCounter
  :: RiakLocation ('Just 'RiakCounterTy)
  -> HostName
  -> PortNumber
  -> IO ()
doGetCounter loc host port = do
  h <- createRiakHandle host port
  either print print =<<
    getRiakCounter h loc def

doGetIndex :: Maybe RiakIndexName -> HostName -> PortNumber -> IO ()
doGetIndex index host port = do
  h <- createRiakHandle host port
  maybe
    (traverse_ print =<< getRiakIndexes h)
    (either print print <=< getRiakIndex h)
    index

doGetMap
  :: RiakLocation ('Just ('RiakMapTy RiakMapEntries))
  -> HostName
  -> PortNumber
  -> IO ()
doGetMap loc host port = do
  h <- createRiakHandle host port
  either print print =<<
    getRiakMap h loc def

doGetObject
  :: IsRiakContent a
  => (Int -> a -> IO ())
  -> RiakLocation 'Nothing
  -> Bool
  -> GetRiakObjectParams
  -> HostName
  -> PortNumber
  -> IO ()
doGetObject f loc head params host port = do
  h <- createRiakHandle host port
  if head
    then go (getRiakObjectHead h loc params) (\_ _ -> pure ())
    else go (getRiakObject h loc params) f
 where
  go
    :: IO (Either RiakError [RiakContent a])
    -> (Int -> a -> IO ())
    -> IO ()
  go get g = do
    eresponse <-
      get

    case eresponse of
      Left err ->
        print err

      Right [] ->
        putStrLn "Not found"

      Right contents -> do
        for_ (zip [(0::Int)..] contents) $ \(i, content) ->
          printContent (g i) (Just i) content

doGetSchema :: SolrSchemaName -> HostName -> PortNumber -> IO ()
doGetSchema schema host port = do
  h <- createRiakHandle host port
  either print print =<<
    getRiakSchema h schema

doGetSet
  :: RiakLocation ('Just ('RiakSetTy Text))
  -> HostName
  -> PortNumber
  -> IO ()
doGetSet loc host port = do
  h <- createRiakHandle host port
  getRiakSet h loc def >>= \case
    Left err -> print err
    Right vals -> for_ vals print -- TODO encoding?

doGetServerInfo :: HostName -> PortNumber -> IO ()
doGetServerInfo host port = do
  h <- createRiakHandle host port
  either print printServerInfo =<<
    getRiakServerInfo h

doGetTextObject
  :: RiakLocation 'Nothing
  -> Bool
  -> GetRiakObjectParams
  -> HostName
  -> PortNumber
  -> IO ()
doGetTextObject =
  doGetObject
    (\i s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))

doPing :: HostName -> PortNumber -> IO ()
doPing host port = do
  h <- createRiakHandle host port
  either print (const (putStrLn "pong")) =<<
    pingRiak h

doPutObject
  :: RiakLocation 'Nothing
  -> Text
  -> [RiakIndex]
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
doPutObject loc content ixs dw n pw return no_sloppy_quorum timeout w host port = do
  h <- createRiakHandle host port
  case return of
    'a' ->
      go
        (putRiakObjectHead h)
        (\contents ->
          (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
            printContent (\_ -> pure ()) (Just i) content'))
    'b' ->
      go
        (putRiakObjectBody h)
        (\contents ->
          (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
            printContent
              (\s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))
              (Just i)
              content'))

    'c' ->
      go (putRiakObject h) pure
    _   -> undefined
 where
  go
    :: (RiakLocation 'Nothing -> Text -> PutRiakObjectParams -> IO (Either RiakError a))
    -> (a -> IO ())
    -> IO ()
  go put f = do
    eresponse <-
      put
        loc
        content
        (def
          & maybe id #dw dw
          & #indexes ixs
          & maybe id #n n
          & maybe id #pw pw
          & (if no_sloppy_quorum then #sloppy_quorum False else id)
          & maybe id #timeout timeout
          & maybe id #w w)

    either print f eresponse

doStream
  :: RiakBucketType ty
  -> Maybe RiakBucket
  -> HostName
  -> PortNumber
  -> IO ()
doStream type' =
  maybe (doStreamBuckets type') (doStreamKeys type')

doStreamBuckets :: RiakBucketType ty -> HostName -> PortNumber -> IO ()
doStreamBuckets type' host port = do
  h <- createRiakHandle host port
  result :: Either RiakError () <-
    streamRiakBuckets h type' $ \buckets ->
      (runExceptT . runListT) (buckets >>= liftIO . Latin1.putStrLn . coerce)
  either print (const (pure ())) result

doStreamKeys
  :: RiakBucketType ty
  -> RiakBucket
  -> HostName
  -> PortNumber
  -> IO ()
doStreamKeys type' bucket host port = do
  h <- createRiakHandle host port
  result :: Either RiakError () <-
    streamRiakKeys h (RiakNamespace type' bucket) $ \keys ->
      (runExceptT . runListT) (keys >>= liftIO . Latin1.putStrLn . coerce)
  either print (const (pure ())) result

doUpdateCounter
  :: RiakLocation ('Just 'RiakCounterTy)
  -> Int64
  -> HostName
  -> PortNumber
  -> IO ()
doUpdateCounter loc incr host port = do
  h <- createRiakHandle host port
  either print print =<<
    updateRiakCounter h loc incr def

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

  for_ (content ^. L.contentType) (tag "content_type" . showContentType)
  for_ (content ^. L.charset) (tag "charset" . showCharset)
  for_ (content ^. L.contentEncoding) (tag "contentEncoding" . showContentEncoding)
  for_ (content ^. L.lastMod) (tag "last_mod" . show)

  case unRiakMetadata (content ^. L.usermeta) of
    [] -> pure ()
    xs -> tag "metadata" (show xs)

  case content ^. L.indexes of
    [] -> pure ()
    xs -> tag "indexes" (show xs)

  when (content ^. L.deleted)
    (tag "deleted" (show (content ^. L.deleted)))

  for_ (unTTL (content ^. L.ttl)) (tag "ttl" . show)
 where
  tag :: String -> String -> IO ()
  tag k v =
    putStrLn (k ++ maybe "" (\i -> "[" ++ show i ++ "]") mi ++ " = " ++ v)

showBool :: Bool -> String
showBool = \case
  False -> "false"
  True  -> "true"

showCharset :: Charset -> String
showCharset (Charset s) =
  Latin1.unpack s

showContentEncoding :: ContentEncoding -> String
showContentEncoding (ContentEncoding s) =
  Latin1.unpack s

showContentType :: ContentType -> String
showContentType (ContentType s) =
  Latin1.unpack s

showLocation :: RiakLocation ty -> String
showLocation (RiakLocation (RiakNamespace type' bucket) key) =
  show type' ++ " " ++ show bucket ++ " " ++ show key

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

locationArgument :: Parser (RiakLocation ty)
locationArgument =
  (\type' bucket -> RiakLocation (RiakNamespace type' bucket))
    <$> bucketTypeArgument
    <*> bucketArgument
    <*> keyArgument

keyArgument :: Parser RiakKey
keyArgument =
  (fmap RiakKey . strArgument)
    (mconcat
      [ help "Key"
      , metavar "KEY"
      ])

getObjectParamsOptions :: Parser GetRiakObjectParams
getObjectParamsOptions =
  f <$> switch
          (mconcat
            [ long "basic-quorum"
            , help "Basic quorum"
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
    <*> timeoutOption
 where
  f :: Bool -> Bool -> Maybe Word32 -> Maybe RiakQuorum -> Maybe RiakQuorum
    -> Bool -> Maybe Word32 -> GetRiakObjectParams
  f basic_quorum notfound_not_ok n pr r no_sloppy_quorum timeout =
    def
      & (if basic_quorum then #basic_quorum True else id)
      & (if notfound_not_ok then #notfound_ok False else id)
      & maybe id #n n
      & maybe id #pr pr
      & maybe id #r r
      & (if no_sloppy_quorum then #sloppy_quorum False else id)
      & maybe id #timeout timeout

dwOption :: Parser (Maybe RiakQuorum)
dwOption =
  quorumOption "dw" "DW value"

nodeArgument :: Parser ((HostName -> PortNumber -> r) -> r)
nodeArgument =
  argument
    (maybeReader readNode)
    (mconcat
      [ help "Riak node, e.g. localhost:8087"
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

secondaryIndexOption :: Parser RiakIndex
secondaryIndexOption =
  option (maybeReader read2i)
    (mconcat
      [ long "ix"
      , metavar "INDEX"
      ])
 where
  read2i :: [Char] -> Maybe RiakIndex
  read2i s = do
    (key, ':':val) <-
      pure (span (/= ':') s)

    let
      key' :: RiakIndexName
      key' =
        RiakIndexName (Utf8.fromString key)

    Just $
      maybe
        (RiakIndexBin key' (Utf8.fromString val))
        (RiakIndexInt key')
        (readMaybe val)

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
