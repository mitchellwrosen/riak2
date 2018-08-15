{-# LANGUAGE DataKinds, OverloadedStrings, ScopedTypeVariables,
             TypeApplications #-}

import Control.Monad
import Data.Foldable       (asum, for_)
import Data.Int
import Data.Proxy
import Data.Text           (Text)
import Data.Text.Encoding  (encodeUtf8)
import Data.Word
import Lens.Family2
import Options.Applicative
import Prelude             hiding (head)

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
      , command "TODO" (info empty mempty)
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
                [ long "no-notfound-ok"
                , help "No notfound ok"
                ])
        <*> (optional . option auto)
              (mconcat
                [ long "nval"
                , help "N value"
                , metavar "QUORUM"
                ])
        <*> (optional . option auto)
              (mconcat
                [ long "pr"
                , help "PR value"
                , metavar "QUORUM"
                ])
        <*> (optional . option auto)
              (mconcat
                [ long "r"
                , help "R value"
                , metavar "QUORUM"
                ])
        <*> switch
              (mconcat
                [ long "sloppy-quorum"
                , help "Sloppy quorum"
                ])
        <*> (optional . option auto)
              (mconcat
                [ long "timeout"
                , help "Timeout"
                , metavar "MILLISECONDS"
                ]))
      (progDesc "Fetch an object"))

storeObjectParser :: Mod CommandFields (IO ())
storeObjectParser =
  command
    "store-object"
    (info
      (doStoreObject
        <$> bucketTypeArgument
        <*> bucketArgument
        <*> strArgument
              (mconcat
                [ help "Content"
                , metavar "CONTENT"
                ])
        <*> optional keyOption)
        -- TODO store-object optional params
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
  -> Maybe Word32
  -> Maybe Word32
  -> Maybe Word32
  -> Bool
  -> Maybe Word32
  -> IO ()
doFetchObject
    type' bucket key basic_quorum head no_notfound_ok n_val pr r sloppy_quorum
    timeout = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h -> do
    eresponse :: Either L.RpbErrorResp (Maybe FetchObjectResp) <-
      fetchObject h
        type'
        bucket
        key
        ( Proxy := guard basic_quorum
        , Proxy := guard head
        , Proxy := Nothing
        , Proxy := n_val
        , Proxy := if no_notfound_ok then Just False else Nothing
        , Proxy := pr
        , Proxy := r
        , Proxy := guard sloppy_quorum
        , Proxy := timeout
        )

    case eresponse of
      Left err ->
        print err

      Right Nothing ->
        putStrLn "Not found"

      Right (Just response) -> do
        for_ (response ^. L.vclock)
          (Latin1.putStrLn . ("vclock = " <>) . Base64.encode . unVclock)
        for_ (response ^. L.unchanged)
          (putStrLn . ("unchanged = " ++) . show)
        for_ (zip [(0::Int)..] (response ^. L.content)) $ \(i, content) -> do
          let tagbs k v = Latin1.putStrLn (k <> "[" <> Latin1.pack (show i) <> "] = " <> v)
          let tag k v = putStrLn (k ++ "[" ++ show i ++ "] = " ++ show v)

          unless head $
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


doStoreObject
  :: BucketType 'Nothing
  -> Bucket
  -> Text
  -> Maybe Key
  -> IO ()
doStoreObject type' bucket content key = do
  cache <- refVclockCache
  withHandle "localhost" 8087 cache $ \h ->
    print =<<
      storeObject h
        type'
        bucket
        key
        (def & L.value .~ encodeUtf8 content
             & L.contentType .~ "text/plain")
        def

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
