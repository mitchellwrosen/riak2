{-# LANGUAGE DataKinds, TypeApplications #-}

import Control.Monad
import Data.ByteString     (ByteString)
import Data.Foldable       (asum)
import Data.Text           (Text)
import Data.Text.Encoding  (encodeUtf8)
import Data.Word
import Lens.Family2
import Options.Applicative

import qualified Data.ByteString.Base16 as Base16

import Riak
import Riak.Internal.Protobuf ()

import qualified Riak.Internal.Protobuf as L (value)

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
  asum
    [ hsubparser fetchCounterParser
    , hsubparser fetchObjectParser
    , hsubparser storeObjectParser
    ]

fetchCounterParser :: Mod CommandFields (IO ())
fetchCounterParser =
  command
    "fetch-counter"
    (info
      (doFetchCounter
        <$> bucketTypeArg
        <*> bucketArg
        <*> keyArg)
        -- TODO fetch-counter optional params
      (progDesc "Fetch a counter"))

fetchObjectParser :: Mod CommandFields (IO ())
fetchObjectParser =
  command
    "fetch-object"
    (info
      (doFetchObject
        <$> bucketTypeArg
        <*> bucketArg
        <*> keyArg
        <*> switch
              (mconcat
                [ long "basic-quorum"
                , help "Basic quorum"
                ])
        <*> switch
              (mconcat
                [ long "deleted-vclock"
                , help "Deleted vclock"
                ])
        <*> switch
              (mconcat
                [ long "head"
                , help "Head"
                ])
        <*> (optional . strOption)
              (mconcat
                [ long "if-modified"
                , help "If modified"
                , metavar "VCLOCK"
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
        <$> bucketTypeArg
        <*> bucketArg
        <*> strOption
              (mconcat
                [ long "content"
                , help "Content"
                ]))
        -- TODO store-object optional params
      (progDesc "Store an object"))

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

doFetchCounter
  :: BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Key
  -> IO ()
doFetchCounter type' bucket key =
  withHandle "localhost" 8087 $ \h ->
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
  -> Maybe ByteString -- vclock
  -> Bool
  -> Maybe Word32
  -> Maybe Word32
  -> Maybe Word32
  -> Bool
  -> Maybe Word32
  -> IO ()
doFetchObject
    type' bucket key basic_quorum deletedvclock head if_modified no_notfound_ok
    n_val pr r sloppy_quorum timeout =
  withHandle "localhost" 8087 $ \h ->
    print =<<
      fetchObject h
        type'
        bucket
        key
        ( Proxy := if basic_quorum then Just True else Nothing
        , Proxy := if head then Just True else Nothing
        , Proxy := (Base16.encode <$> if_modified)
        , Proxy := n_val
        , Proxy := if no_notfound_ok then Just False else Nothing
        , Proxy := pr
        , Proxy := r
        , Proxy := if sloppy_quorum then Just True else Nothing
        , Proxy := timeout
        )

doStoreObject
  :: BucketType 'Nothing
  -> Bucket
  -> Text
  -> IO ()
doStoreObject type' bucket content =
  withHandle "localhost" 8087 $ \h ->
    print =<<
      storeObject h
        type'
        bucket
        (def & L.value .~ encodeUtf8 content)
        def

--------------------------------------------------------------------------------
-- Misc. helpers
--------------------------------------------------------------------------------

bucketArg :: Parser Bucket
bucketArg =
  (fmap Bucket . strArgument)
    (mconcat
      [ help "Bucket"
      , metavar "BUCKET"
      ])

bucketTypeArg :: Parser (BucketType ty)
bucketTypeArg =
  (fmap BucketType . strArgument)
    (mconcat
      [ help "Bucket type"
      , metavar "TYPE"
      ])

keyArg :: Parser Key
keyArg =
  (fmap Key . strArgument)
    (mconcat
      [ help "Key"
      , metavar "KEY"
      ])
