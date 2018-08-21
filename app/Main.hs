{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, LambdaCase, OverloadedStrings,
             RankNTypes, ScopedTypeVariables, TypeApplications #-}

import Control.Monad              (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Coerce
import Data.Foldable              (asum, for_)
import Data.Int
import Data.Text                  (Text)
import Lens.Family2
import List.Transformer           (runListT)
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
  (asum . map (hsubparser . mconcat))
    [ [ commandGroup "Key/value object operations"
      , fetchObjectParser
      , storeObjectParser
      ]
    , [ commandGroup "Data type operations"
      , fetchCounterParser
      , updateCounterParser
      ]
    , [ commandGroup "Bucket operations"
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
      , command "TODO" (info empty mempty)
      ]
    , [ commandGroup "Server info"
      , command "TODO" (info empty mempty)
      ]
    ]

fetchCounterParser :: Mod CommandFields (IO ())
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

fetchObjectParser :: Mod CommandFields (IO ())
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
        <*> nvalOption
        <*> prOption
        <*> rOption
        <*> sloppyQuorumOption
        <*> timeoutOption)
      (progDesc "Fetch an object"))

listBucketsParser :: Mod CommandFields (IO ())
listBucketsParser =
  command
    "list-buckets"
    (info
      (doListBuckets <$> bucketTypeArgument)
      (progDesc "List all buckets in a bucket type"))

listKeysParser :: Mod CommandFields (IO ())
listKeysParser =
  command
    "list-keys"
    (info
      (doListKeys
        <$> bucketTypeArgument
        <*> bucketArgument)
      (progDesc "List all keys in a bucket"))

storeObjectParser :: Mod CommandFields (IO ())
storeObjectParser =
  command
    "store-object"
    (info
      (doStoreObject
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> optional keyOption
        <*> strArgument
              (mconcat
                [ help "Content"
                , metavar "CONTENT"
                ])
        <*> dwOption
        <*> nvalOption
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

updateCounterParser :: Mod CommandFields (IO ())
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
  :: BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Key
  -> IO ()
doFetchCounter type' bucket key = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
    print =<<
      fetchCounter h
        type'
        bucket
        key
        def

doFetchObject
  :: BucketType 'Nothing
  -> Bucket
  -> Key
  -> Bool
  -> Bool
  -> Bool
  -> Nval
  -> PR
  -> R
  -> SloppyQuorum
  -> Timeout
  -> IO ()
doFetchObject
    type' bucket key basic_quorum head notfound_not_ok n_val pr r sloppy_quorum
    timeout = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
    withHead head $ \(head' :: Head Text head) -> do
      eresponse <-
        fetchObject h
          type'
          bucket
          key
          ( BasicQuorum basic_quorum
          , head'
          , NoIfModified
          , n_val
          , NotfoundOk (not notfound_not_ok)
          , pr
          , r
          , sloppy_quorum
          , timeout
          )

      case eresponse of
        Left err ->
          print err

        Right Nothing ->
          putStrLn "Not found"

        Right (Just contents) -> do
          for_ (zip [(0::Int)..] contents) $ \(i, content) -> do
            let tagtxt :: Text -> Text -> IO ()
                tagtxt k v = Text.putStrLn (k <> "[" <> Text.pack (show i) <> "] = " <> v)

            let tag :: Show a => String -> a -> IO ()
                tag k v = putStrLn (k ++ "[" ++ show i ++ "] = " ++ show v)

            () <-
              case head' of
                Head ->
                  pure ()
                NoHead ->
                  tagtxt "value" (content ^. L.value)

            for_ (content ^. L.lastMod) (tag "last_mod")

            case unMetadata (content ^. L.usermeta) of
              [] -> pure ()
              xs -> tag "metadata" xs

            case unIndexes (content ^. L.indexes) of
              [] -> pure ()
              xs -> tag "indexes" xs

            tag "deleted" (content ^. L.deleted)
            for_ (unTTL (content ^. L.ttl)) (tag "ttl")

doListBuckets :: BucketType ty -> IO ()
doListBuckets type' = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h -> do
    result :: Either L.RpbErrorResp () <-
      (runExceptT . runListT)
        (listBuckets h type' >>= liftIO . Latin1.putStrLn . coerce)
    either print (const (pure ())) result

doListKeys
  :: BucketType ty
  -> Bucket
  -> IO ()
doListKeys type' bucket = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h -> do
    result :: Either L.RpbErrorResp () <-
      (runExceptT . runListT)
        (listKeys h type' bucket >>= liftIO . Latin1.putStrLn . coerce)
    either print (const (pure ())) result

doStoreObject
  :: BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> Text
  -> DW
  -> Nval
  -> PW
  -> Char
  -> SloppyQuorum
  -> Timeout
  -> W
  -> IO ()
doStoreObject
    type' bucket key content dw n_val pw return sloppy_quorum timeout w = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
    withParamObjectReturn return $ \return' -> do
      eresponse <-
        storeObject h
          type'
          bucket
          key
          content
          ( dw
          , Indexes [] -- TODO riak-cli store-object indexes
          , Metadata [] -- TODO riak-cli store-object metadata
          , n_val
          , pw
          , return'
          , sloppy_quorum
          , timeout
          , def -- TODO riak-cli store-object ttl
          , w
          )
      print eresponse

doUpdateCounter
  :: BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Int64
  -> Maybe Key
  -> IO ()
doUpdateCounter type' bucket incr key = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
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

withHead :: Bool -> (forall head. Head a head -> r) -> r
withHead head k =
  if head
    then k Head
    else k NoHead

withParamObjectReturn
  :: Char
  -> (forall return. SingObjectReturn return => ParamObjectReturn return -> r)
  -> r
withParamObjectReturn ch k =
  case ch of
    'a' -> k ParamObjectReturnHead
    'b' -> k ParamObjectReturnBody
    'c' -> k ParamObjectReturnNone
    _   -> undefined

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

dwOption :: Parser DW
dwOption =
  DW <$> quorumOption "dw" "DW value"

nvalOption :: Parser Nval
nvalOption =
  (fmap Nval . optional . option auto)
    (mconcat
      [ long "nval"
      , help "N value"
      , metavar "NODES"
      ])

prOption :: Parser PR
prOption =
  PR <$> quorumOption "pr" "PR value"

pwOption :: Parser PW
pwOption =
  PW <$> quorumOption "pw" "PW value"

quorumOption :: String -> String -> Parser Quorum
quorumOption s1 s2 =
  option (maybeReader readQuorum)
    (mconcat
      [ long s1
      , help s2
      , metavar "QUORUM"
      , value QuorumDefault
      ])
 where
  readQuorum :: String -> Maybe Quorum
  readQuorum = \case
    "all"     -> pure QuorumAll
    "default" -> pure QuorumDefault
    "one"     -> pure QuorumOne
    "quorum"  -> pure QuorumQuorum
    s         -> Quorum <$> readMaybe s

rOption :: Parser R
rOption =
  R <$> quorumOption "r" "R value"

sloppyQuorumOption :: Parser SloppyQuorum
sloppyQuorumOption =
  SloppyQuorum <$>
    switch
      (mconcat
        [ long "sloppy-quorum"
        , help "Sloppy quorum"
        ])

timeoutOption :: Parser Timeout
timeoutOption =
  (fmap Timeout . optional . option auto)
    (mconcat
      [ long "timeout"
      , help "Timeout"
      , metavar "MILLISECONDS"
      ])

wOption :: Parser W
wOption =
  W <$> quorumOption "w" "W value"
