{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, OverloadedLabels,
             OverloadedStrings, RankNTypes, ScopedTypeVariables,
             TypeApplications, ViewPatterns #-}

import Control.Monad      (join, when, (<=<))
import Data.ByteString    (ByteString)
import Data.Foldable      (asum, for_, toList, traverse_)
import Data.Int
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import Lens.Family2
import Network.Socket     (HostName, PortNumber)
import Options.Micro
import Prelude            hiding (head, return)
import Text.Read          (readMaybe)

import qualified Control.Foldl          as Foldl
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
  runParser
    (Description "riak command-line client")
    (makeParser
      optionParser
      commandParser)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

optionParser :: OptionParser (HostName, PortNumber)
optionParser =
  option
    (positional
      parseNode
      (Metavar "NODE")
      (Help "Riak node, e.g. localhost:8087"))
 where
  parseNode :: Text -> Either Text (HostName, PortNumber)
  parseNode (Text.unpack -> s) =
    maybe (Left "Expected 'host' or 'host:port'") Right $ do
      case span (/= ':') s of
        (host, ':':port) -> do
          port' <- readMaybe port
          pure (host, port')
        (host, []) ->
          pure (host, 8087)
        _ -> undefined

commandParser :: CommandParser ((HostName, PortNumber) -> IO ())
commandParser =
  commands
    [ commandGroup "Object operations"

    , command
        "get-binary"
        (Description "Get a binary object")
        (makeParser
          (doGetBinaryObject
            <$> locationArgument
            <*> option
                  (flag
                    True
                    (Long "head")
                    (Help "Head")
                    `ifMissing` False)
            <*> getObjectParamsOptions)
          noCommands)

    , command
        "get-text"
        (Description "Get a text object")
        (makeParser
          (doGetTextObject
            <$> locationArgument
            <*> option
                  (flag
                    True
                    (Long "head")
                    (Help "Head")
                    `ifMissing` False)
            <*> getObjectParamsOptions)
          noCommands)

    , command
        "put"
        (Description "Put an object")
        (makeParser
          (doPutObject
            <$> locationArgument
            <*> option
                  (positional
                    text
                    (Metavar "CONTENT")
                    (Help "Content"))
            <*> option (repeated secondaryIndexOption)
            <*> dwOption
            <*> nOption
            <*> pwOption
            <*> option
                  (alternatives
                    [ flag
                        'a'
                        (Long "head")
                        (Help "Return head")
                    , flag
                        'b'
                        (Long "body")
                        (Help "Return body")
                    ]
                  `ifMissing` 'c')
            <*> sloppyQuorumOption
            <*> timeoutOption
            <*> wOption)
          noCommands)

--
--putObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--putObjectParser =
--  command
--    "put"
--    (info
--      (progDesc "Put an object"))

    ]

--parser :: Parser (IO ())
--parser =
--    nodeArgument
--    <*>
--    (asum . map (hsubparser . mconcat))
--      [ [ putObjectParser
--        , deleteObjectParser
--        ]
--      , [ commandGroup "Counter operations"
--        , getCounterParser
--        , updateCounterParser
--        ]
--      , [ commandGroup "Grow-only set operations"
--        ]
--      , [ commandGroup "HyperLogLog operations"
--        ]
--      , [ commandGroup "Map operations"
--        , getMapParser
--        ]
--      , [ commandGroup "Set operations"
--        , getSetParser
--        ]
--      , [ commandGroup "Bucket operations"
--        , getBucketPropsParser
--        ]
--      , [ commandGroup "Full key traversal operations"
--        , listParser
--        ]
--      , [ commandGroup "MapReduce"
--        ]
--      , [ commandGroup "Secondary indexes (2i)"
--        , queryParser
--        ]
--      , [ commandGroup "Search 2.0"
--        , getSchemaParser
--        , getIndexParser
--        ]
--      , [ commandGroup "Server info"
--        , pingParser
--        , infoParser'
--        ]
--      ]

--deleteObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--deleteObjectParser =
--  command
--    "delete"
--    (info
--      (doDeleteObject
--        <$> locationArgument)
--      (progDesc "Delete an object"))

--getBinaryObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getBinaryObjectParser =
--  command
--    "get-binary"
--    (info
--      (doGetBinaryObject
--        <$> locationArgument
--        <*> switch
--              (mconcat
--                [ long "head"
--                , help "Head"
--                ])
--        <*> getObjectParamsOptions)
--      (progDesc "Get a binary object"))

--getCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getCounterParser =
--  command
--    "get-counter"
--    (info
--      (doGetCounter
--        <$> locationArgument)
--      (progDesc "Get a counter"))

--getMapParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getMapParser =
--  command
--    "get-map"
--    (info
--      (doGetMap
--        <$> locationArgument)
--      (progDesc "Get a map"))

--getTextObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getTextObjectParser =
--  command
--    "get-text"
--    (info
--      (doGetTextObject
--        <$> locationArgument
--        <*> switch
--              (mconcat
--                [ long "head"
--                , help "Head"
--                ])
--        <*> getObjectParamsOptions)
--      (progDesc "Get a text object"))

--getSetParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getSetParser =
--  command
--    "get-set"
--    (info
--      (doGetSet
--        <$> locationArgument)
--      (progDesc "Get a set"))

--getBucketPropsParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getBucketPropsParser =
--  command
--    "get-bucket"
--    (info
--      (doGetBucketProps
--        <$> bucketTypeArgument
--        <*> optional bucketArgument)
--      (progDesc "Get a bucket or bucket type's properties"))

--getIndexParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getIndexParser =
--  command
--    "get-index"
--    (info
--      (doGetIndex
--        <$> (optional . fmap RiakIndexName . strArgument)
--              (mconcat
--                [ help "Index name"
--                , metavar "INDEX"
--                ]))
--      (progDesc "Get a Solr index, or all Solr indexes"))

--getSchemaParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--getSchemaParser =
--  command
--    "get-schema"
--    (info
--      (doGetSchema
--        <$> (fmap SolrSchemaName . strArgument)
--              (mconcat
--                [ help "Schema name"
--                , metavar "SCHEMA"
--                ]))
--      (progDesc "Get a Solr schema"))

--infoParser' :: Mod CommandFields (HostName -> PortNumber -> IO ())
--infoParser' =
--  command
--    "info"
--    (info (pure doGetServerInfo) (progDesc "Get server info"))

--listParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--listParser =
--  command
--    "list"
--    (info
--      (doStream <$> bucketTypeArgument <*> optional bucketArgument)
--      (progDesc "List all buckets or keys"))

--pingParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--pingParser =
--  command
--    "ping"
--    (info (pure doPing) (progDesc "Ping the server"))

--putObjectParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--putObjectParser =
--  command
--    "put"
--    (info
--      (doPutObject
--        <$> locationArgument
--        <*> strArgument
--              (mconcat
--                [ help "Content"
--                , metavar "CONTENT"
--                ])
--        <*> many secondaryIndexOption
--        <*> dwOption
--        <*> nOption
--        <*> pwOption
--        <*> asum
--              [ flag'
--                  'a'
--                  (mconcat
--                    [ long "head"
--                    , help "Return head"
--                    ])
--              , flag'
--                  'b'
--                  (mconcat
--                    [ long "body"
--                    , help "Return body"
--                    ])
--              , pure 'c'
--              ]
--        <*> sloppyQuorumOption
--        <*> timeoutOption
--        <*> wOption)
--      (progDesc "Put an object"))

--queryParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--queryParser =
--  command
--    "2i"
--    (info
--      (do2i
--        <$> namespaceArgument
--        <*> (RiakIndexName <$>
--              strArgument
--                (mconcat
--                  [ help "Secondary index name"
--                  , metavar "INDEX"
--                  ]))
--        <*> strArgument
--              (mconcat
--                [ help "Value (integer or string)"
--                , metavar "VALUE"
--                ])
--        <*> optional
--              (strArgument
--                (mconcat
--                  [ help "Value (integer or string)"
--                  , metavar "VALUE"
--                  ])))
--      (progDesc "Search using secondary indexes"))

--updateCounterParser :: Mod CommandFields (HostName -> PortNumber -> IO ())
--updateCounterParser =
--  command
--    "incr-counter"
--    (info
--      (doUpdateCounter
--        <$> locationArgument
--        <*> argument auto
--              (mconcat
--                [ help "Amount"
--                , metavar "N"
--                ]))
--      (progDesc "Increment or decrement a counter"))

----------------------------------------------------------------------------------
---- Implementations
----------------------------------------------------------------------------------

--do2i
--  :: RiakBucket
--  -> RiakIndexName
--  -> [Char]
--  -> Maybe [Char]
--  -> HostName
--  -> PortNumber
--  -> IO ()
--do2i bucket index key1 key2 host port = do
--  h <- createRiakHandle host port
--  either print (\() -> pure ()) =<<
--    case key2 of
--      Nothing ->
--        let
--          query :: RiakExactQuery
--          query =
--            case readMaybe key1 of
--              Just n  -> RiakExactQueryInt index n
--              Nothing -> RiakExactQueryBin index (Utf8.fromString key1)
--        in
--          riakExactQuery h bucket query (Foldl.mapM_ print)

--      Just key2' ->
--        case (readMaybe key1, readMaybe key2') of
--          (Just n, Just m) ->
--            riakRangeQueryTerms
--              h
--              bucket
--              (RiakRangeQueryInt index n m)
--              (Foldl.mapM_ ((\(x, y) -> putStrLn (show x ++ " " ++ show y))))

--          _ ->
--            riakRangeQueryTerms
--              h
--              bucket
--              (RiakRangeQueryBin
--                index
--                (Utf8.fromString key1)
--                (Utf8.fromString key2'))
--              (Foldl.mapM_ (\(x, y) -> putStrLn (show x ++ " " ++ show y)))

--doDeleteObject :: RiakKey -> HostName -> PortNumber -> IO ()
--doDeleteObject key host port = do
--  h <- createRiakHandle host port
--  either print (\() -> pure ()) =<<
--    deleteRiakObject h key

doGetBinaryObject
  :: RiakKey
  -> Bool
  -> GetRiakObjectParams
  -> (HostName, PortNumber)
  -> IO ()
doGetBinaryObject =
  doGetObject
    (\i s ->
      Latin1.putStrLn
        ("value[" <> Latin1.pack (show i) <> "] = " <> Base64.encode s))

--doGetBucketProps
--  :: RiakBucketType
--  -> Maybe ByteString
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doGetBucketProps type' bucket host port = do
--  h <- createRiakHandle host port
--  either print printBucketProps =<<
--    maybe
--      (getRiakBucketTypeProps h type')
--      (getRiakBucketProps h . RiakBucket type')
--      bucket

--doGetCounter
--  :: RiakKey
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doGetCounter key host port = do
--  h <- createRiakHandle host port
--  either print print =<<
--    getRiakCounter h key def

--doGetIndex :: Maybe RiakIndexName -> HostName -> PortNumber -> IO ()
--doGetIndex index host port = do
--  h <- createRiakHandle host port
--  maybe
--    (traverse_ print =<< getRiakIndexes h)
--    (either print print <=< getRiakIndex h)
--    index

--doGetMap
--  :: RiakKey
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doGetMap key host port = do
--  h <- createRiakHandle host port
--  either print print =<<
--    getRiakMap @RiakMapEntries h key def

doGetObject
  :: IsRiakObject a
  => (Int -> a -> IO ())
  -> RiakKey
  -> Bool
  -> GetRiakObjectParams
  -> (HostName, PortNumber)
  -> IO ()
doGetObject f key head params (host, port) = do
  h <- createRiakHandle host port
  if head
    then go (getRiakObjectHead h key params) (\_ _ -> pure ())
    else go (getRiakObject h key params) f
 where
  go
    :: IO (Either RiakError [RiakObject a])
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
          printObject (g i) (Just i) content

--doGetSchema :: SolrSchemaName -> HostName -> PortNumber -> IO ()
--doGetSchema schema host port = do
--  h <- createRiakHandle host port
--  either print print =<<
--    getRiakSchema h schema

--doGetSet
--  :: RiakKey
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doGetSet key host port = do
--  h <- createRiakHandle host port
--  getRiakSet @Text h key def >>= \case
--    Left err -> print err

--doGetServerInfo :: HostName -> PortNumber -> IO ()
--doGetServerInfo host port = do
--  h <- createRiakHandle host port
--  either print printServerInfo =<<
--    getRiakServerInfo h

doGetTextObject
  :: RiakKey
  -> Bool
  -> GetRiakObjectParams
  -> (HostName, PortNumber)
  -> IO ()
doGetTextObject =
  doGetObject
    (\i s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))

--doPing :: HostName -> PortNumber -> IO ()
--doPing host port = do
--  h <- createRiakHandle host port
--  either print (const (putStrLn "pong")) =<<
--    pingRiak h

doPutObject
  :: RiakKey
  -> Text
  -> [RiakIndex]
  -> Maybe RiakQuorum
  -> Maybe Word32
  -> Maybe RiakQuorum
  -> Char
  -> Bool
  -> Maybe Word32
  -> Maybe RiakQuorum
  -> (HostName, PortNumber)
  -> IO ()
doPutObject key content ixs dw n pw return no_sloppy_quorum timeout w (host, port) = do
  h <- createRiakHandle host port
  case return of
    'a' ->
      go
        (putRiakObjectHead h)
        (\contents ->
          (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
            printObject (\_ -> pure ()) (Just i) content'))
    'b' ->
      go
        (putRiakObjectBody h)
        (\contents ->
          (for_ (zip [(0::Int)..] (toList contents)) $ \(i, content') ->
            printObject
              (\s -> Text.putStrLn ("value[" <> Text.pack (show i) <> "] = " <> s))
              (Just i)
              content'))

    'c' ->
      go (putRiakObject h) pure
    _   -> undefined
 where
  go
    :: (RiakKey -> Text -> PutRiakObjectParams -> IO (Either RiakError a))
    -> (a -> IO ())
    -> IO ()
  go put f = do
    eresponse <-
      put
        key
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

--doStream
--  :: RiakBucketType
--  -> Maybe ByteString
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doStream type' =
--  maybe
--    (doStreamBuckets type')
--    (doStreamKeys . RiakBucket type')

--doStreamBuckets :: RiakBucketType -> HostName -> PortNumber -> IO ()
--doStreamBuckets type' host port = do
--  h <- createRiakHandle host port
--  result :: Either RiakError () <-
--    streamRiakBuckets h type' (Foldl.mapM_ print)
--      -- (runExceptT . runListT) (buckets >>= liftIO . print)
--  either print (const (pure ())) result

--doStreamKeys
--  :: RiakBucket
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doStreamKeys bucket host port = do
--  h <- createRiakHandle host port
--  streamRiakKeys h bucket (Foldl.mapM_ print) >>=
--    either print (const (pure ()))

--doUpdateCounter
--  :: RiakKey
--  -> Int64
--  -> HostName
--  -> PortNumber
--  -> IO ()
--doUpdateCounter key incr host port = do
--  h <- createRiakHandle host port
--  either print print =<<
--    updateRiakCounter h key incr def

----------------------------------------------------------------------------------
---- Misc. helpers
----------------------------------------------------------------------------------

--printBucketProps :: RpbBucketProps -> IO ()
--printBucketProps props = do
--  p "allow_mult"      (fmap showBool . view L.maybe'allowMult)
--  p "backend"         (fmap show . view L.maybe'backend)
--  p "basic_quorum"    (fmap showBool . view L.maybe'basicQuorum)
--  p "big_vclock"      (fmap show . view L.maybe'bigVclock)
--  p "chash_keyfun"    (fmap showModFun . view L.maybe'chashKeyfun)
--  p "consistent"      (fmap showBool . view L.maybe'consistent)
--  p "datatype"        (fmap show . view L.maybe'datatype)
--  p "dw"              (fmap show . view L.maybe'dw)
--  p "has_precommit"   (fmap showBool . view L.maybe'hasPrecommit)
--  p "hll_precision"   (fmap show . view L.maybe'hllPrecision)
--  p "last_write_wins" (fmap showBool . view L.maybe'lastWriteWins)
--  p "linkfun"         (fmap showModFun . view L.maybe'linkfun)
--  p "n"               (fmap show . view L.maybe'nVal)
--  p "notfound_ok"     (fmap showBool . view L.maybe'notfoundOk)
--  p "old_vclock"      (fmap show . view L.maybe'oldVclock)
--  p "postcommit"      (fmap show . view L.maybe'hasPostcommit)
--  p "pr"              (fmap showQuorum . view L.maybe'pr)
--  p "precommit"       (\case { [] -> Nothing; xs -> Just (show xs) } . view L.precommit)
--  p "pw"              (fmap showQuorum . view L.maybe'pw)
--  p "r"               (fmap showQuorum . view L.maybe'r)
--  p "repl"            (fmap show . view L.maybe'repl)
--  p "rw"              (fmap showQuorum . view L.maybe'rw)
--  p "search"          (fmap showBool . view L.maybe'search)
--  p "search_index"    (fmap show . view L.maybe'searchIndex)
--  p "small_vclock"    (fmap show . view L.maybe'smallVclock)
--  p "ttl"             (fmap show . view L.maybe'ttl)
--  p "w"               (fmap showQuorum . view L.maybe'w)
--  p "write_once"      (fmap showBool . view L.maybe'writeOnce)
--  p "young_vclock"    (fmap show . view L.maybe'youngVclock)
-- where
--  p :: String -> (RpbBucketProps -> Maybe String) -> IO ()
--  p s f =
--    for_ (f props) (\x -> putStrLn (s ++ " = " ++ x))

printObject :: (a -> IO ()) -> Maybe Int -> RiakObject a -> IO ()
printObject f mi object = do
  tag "key" (show (object ^. L.key))

  f (object ^. L.value)

  for_ (object ^. L.contentType) (tag "content_type" . showContentType)
  for_ (object ^. L.charset) (tag "charset" . showCharset)
  for_ (object ^. L.contentEncoding) (tag "contentEncoding" . showContentEncoding)
  for_ (object ^. L.lastMod) (tag "last_mod" . show)

  case unRiakMetadata (object ^. L.usermeta) of
    [] -> pure ()
    xs -> tag "metadata" (show xs)

  case object ^. L.indexes of
    [] -> pure ()
    xs -> tag "indexes" (show xs)

  when (object ^. L.deleted)
    (tag "deleted" (show (object ^. L.deleted)))

  for_ (unTTL (object ^. L.ttl)) (tag "ttl" . show)
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

bucketArgument :: Option Unnamed Required Text
bucketArgument =
  positional
    text
    (Metavar "BUCKET")
    (Help "Bucket")

bucketTypeArgument :: Option Unnamed Required Text
bucketTypeArgument =
  positional
    text
    (Metavar "TYPE")
    (Help "Bucket type")

locationArgument :: OptionParser RiakKey
locationArgument =
  f <$> option bucketTypeArgument
    <*> option bucketArgument
    <*> option keyArgument
 where
  f :: Text -> Text -> Text -> RiakKey
  f type' bucket key =
    RiakKey
      (RiakBucket
        (RiakBucketType (encodeUtf8 type'))
        (encodeUtf8 bucket))
      (encodeUtf8 key)

keyArgument :: Option Unnamed Required Text
keyArgument =
  positional
    text
    (Metavar "KEY")
    (Help "Key")

getObjectParamsOptions :: OptionParser GetRiakObjectParams
getObjectParamsOptions =
  f <$> option
          (flag
            True
            (Long "basic-quorum")
            (Help "Basic quorum")
            `ifMissing` False)
    <*> option
          (flag
            True
            (Long "notfound-not-ok")
            (Help "notfound not ok")
            `ifMissing` False)
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

dwOption :: OptionParser (Maybe RiakQuorum)
dwOption =
  quorumOption "dw" "DW value"

--namespaceArgument :: Parser RiakBucket
--namespaceArgument =
--  RiakBucket
--    <$> bucketTypeArgument
--    <*> bucketArgument


nOption :: OptionParser (Maybe Word32)
nOption =
  option
    (named
      (fmap Just . decimal)
      (Long "n")
      (Metavar "NODES")
      (Help "N value")
      `ifMissing` Nothing)

prOption :: OptionParser (Maybe RiakQuorum)
prOption =
  quorumOption "pr" "PR value"

pwOption :: OptionParser (Maybe RiakQuorum)
pwOption =
  quorumOption "pw" "PW value"

quorumOption :: Text -> Text -> OptionParser (Maybe RiakQuorum)
quorumOption s1 s2 =
  option
    (named
      (fmap Just . parseQuorum)
      (Long s1)
      (Metavar "QUORUM")
      (Help s2)
      `ifMissing` Nothing)
 where
  parseQuorum :: Text -> Either Text RiakQuorum
  parseQuorum = \case
    "all" ->
      pure RiakQuorumAll

    "quorum" ->
      pure RiakQuorumQuorum

    s ->
      maybe
        (Left "Expected 'all', 'quorum', or a number")
        (Right . RiakQuorum)
        (readMaybe (Text.unpack s))

rOption :: OptionParser (Maybe RiakQuorum)
rOption =
  quorumOption "r" "R value"

secondaryIndexOption :: Option 'Named 'Required RiakIndex
secondaryIndexOption =
  named
    parse2i
    (Long "ix")
    (Metavar "INDEX")
    (Help "Secondary index")
 where
  parse2i :: Text -> Either Text RiakIndex
  parse2i s =
    maybe (Left "Expected: 'key:val'") Right $ do
    (key, ':':val) <-
      pure (span (/= ':') (Text.unpack s))

    let
      key' :: RiakIndexName
      key' =
        RiakIndexName (Utf8.fromString key)

    Just $
      maybe
        (RiakIndexBin key' (Utf8.fromString val))
        (RiakIndexInt key')
        (readMaybe val)

sloppyQuorumOption :: OptionParser Bool
sloppyQuorumOption =
  option
    (flag
      True
      (Long "no-sloppy-quorum")
      (Help "No sloppy quorum")
      `ifMissing` False)

timeoutOption :: OptionParser (Maybe Word32)
timeoutOption =
  option
    (named
      (fmap Just . decimal)
      (Long "timeout")
      (Metavar "MILLISECONDS")
      (Help "timeout")
      `ifMissing` Nothing)

wOption :: OptionParser (Maybe RiakQuorum)
wOption =
  quorumOption "w" "W value"
