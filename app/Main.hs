{-# LANGUAGE DataKinds, GADTs, LambdaCase, OverloadedStrings, RankNTypes,
             ScopedTypeVariables, TypeApplications #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Coerce
import Data.Foldable              (asum, for_)
import Data.Int
import Data.Text                  (Text)
import Data.Text.Encoding         (encodeUtf8)
import Lens.Family2
import List.Transformer           (runListT)
import Options.Applicative
import Prelude                    hiding (head)
import Text.Read                  (readMaybe)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as Latin1

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
        <*> returnBodyOption
        <*> (ParamReturnHead <$>
              switch
                (mconcat
                  [ long "return-head"
                  , help "Return head"
                  ]))
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
  -> ParamNVal
  -> ParamPR
  -> ParamR
  -> ParamSloppyQuorum
  -> ParamTimeout
  -> IO ()
doFetchObject
    type' bucket key basic_quorum head notfound_not_ok n_val pr r sloppy_quorum
    timeout = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
    withParamHead head $ \head' -> do
      eresponse <-
        fetchObject h
          type'
          bucket
          key
          ( ParamBasicQuorum basic_quorum
          , head'
          , ParamNoIfModified
          , n_val
          , ParamNotfoundOk (not notfound_not_ok)
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
            let tagbs :: ByteString -> ByteString -> IO ()
                tagbs k v = Latin1.putStrLn (k <> "[" <> Latin1.pack (show i) <> "] = " <> v)
            let tag :: Show a => String -> a -> IO ()
                tag k v = putStrLn (k ++ "[" ++ show i ++ "] = " ++ show v)

            () <-
              case head' of
                ParamHead ->
                  pure ()
                ParamNoHead ->
                  tagbs "value" $
                    case content ^. L.contentType of
                      Just "text/plain" -> content ^. L.value
                      _                 -> Base64.encode (content ^. L.value)

            for_ (content ^. L.contentType)     (tagbs "content_type" . unContentType)
            for_ (content ^. L.charset)         (tagbs "charset")
            for_ (content ^. L.contentEncoding) (tagbs "content_encoding")
            for_ (content ^. L.lastMod)         (tag "last_mod")
            for_ (content ^. L.lastModUsecs)    (tag "last_mod_usecs")
            for_ (content ^. L.usermeta)        print -- TODO better usermeta printing
            for_ (content ^. L.indexes)         print -- TODO better indexes printing
            for_ (content ^. L.deleted)         (tag "deleted")
            for_ (content ^. L.ttl)             (tag "ttl")

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
  -> ParamDW
  -> ParamNVal
  -> ParamPW
  -> ParamReturnBody
  -> ParamReturnHead
  -> ParamSloppyQuorum
  -> ParamTimeout
  -> ParamW
  -> IO ()
doStoreObject
    type' bucket key content dw n_val pw return_body return_head sloppy_quorum
    timeout w = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
    print =<<
      storeObject h
        type'
        bucket
        key
        (def & L.value .~ encodeUtf8 content
             & L.contentType .~ "text/plain")
        ( dw
        , n_val
        , pw
        , return_body
        , return_head
        , sloppy_quorum
        , timeout
        , w
        )

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

withParamHead :: Bool -> (forall head. ParamHead head -> r) -> r
withParamHead head k =
  if head
    then k ParamHead
    else k ParamNoHead

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

dwOption :: Parser ParamDW
dwOption =
  ParamDW <$> quorumOption "dw" "DW value"

nvalOption :: Parser ParamNVal
nvalOption =
  (fmap ParamNVal . optional . option auto)
    (mconcat
      [ long "nval"
      , help "N value"
      , metavar "NODES"
      ])

prOption :: Parser ParamPR
prOption =
  ParamPR <$> quorumOption "pr" "PR value"

pwOption :: Parser ParamPW
pwOption =
  ParamPW <$> quorumOption "pw" "PW value"

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

rOption :: Parser ParamR
rOption =
  ParamR <$> quorumOption "r" "R value"

returnBodyOption :: Parser ParamReturnBody
returnBodyOption =
  ParamReturnBody <$>
    switch
      (mconcat
        [ long "return-body"
        , help "Return body"
        ])

sloppyQuorumOption :: Parser ParamSloppyQuorum
sloppyQuorumOption =
  ParamSloppyQuorum <$>
    switch
      (mconcat
        [ long "sloppy-quorum"
        , help "Sloppy quorum"
        ])

timeoutOption :: Parser ParamTimeout
timeoutOption =
  (fmap ParamTimeout . optional . option auto)
    (mconcat
      [ long "timeout"
      , help "Timeout"
      , metavar "MILLISECONDS"
      ])

wOption :: Parser ParamW
wOption =
  ParamW <$> quorumOption "w" "W value"
