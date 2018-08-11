import Control.Monad
import Data.ByteString     (ByteString)
import Data.Word
import Options.Applicative

import qualified Data.ByteString.Base16 as Base16

import Riak.Internal.Protobuf

import qualified Riak

main :: IO ()
main =
  join
    (customExecParser
      (prefs (showHelpOnEmpty <> showHelpOnError))
      (info parser (progDesc "riak command-line client")))

parser :: Parser (IO ())
parser =
  hsubparser
    (command
      "fetch-object"
      (info
        (doFetchObject
          <$> switch
                (mconcat
                  [ long "basic-quorum"
                  , help "Basic quorum"
                  ])
          <*> strOption
                (mconcat
                  [ long "bucket"
                  , help "Bucket"
                  , metavar "BUCKET"
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
          <*> strOption
                (mconcat
                  [ long "key"
                  , help "Key"
                  , metavar "KEY"
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
                  ])
          <*> (optional . strOption)
                (mconcat
                  [ long "type"
                  , help "Bucket type"
                  , metavar "TYPE"
                  ]))
        (progDesc "Fetch an object")))

doFetchObject
  :: Bool
  -> ByteString
  -> Bool
  -> Bool
  -> Maybe ByteString
  -> ByteString
  -> Bool
  -> Maybe Word32
  -> Maybe Word32
  -> Maybe Word32
  -> Bool
  -> Maybe Word32
  -> Maybe ByteString
  -> IO ()
doFetchObject
    basicQuorum bucket deletedvclock head ifModified key noNotfoundOk nVal pr r
    sloppyQuorum timeout type' =
  Riak.withHandle "localhost" 8087 $ \h ->
    print =<<
      Riak.fetchObject h
        RpbGetReq
          { _RpbGetReq'_unknownFields = mempty
          , _RpbGetReq'basicQuorum = if basicQuorum then Just True else Nothing
          , _RpbGetReq'bucket = bucket
          , _RpbGetReq'deletedvclock = if deletedvclock then Just True else Nothing
          , _RpbGetReq'head = if head then Just True else Nothing
          , _RpbGetReq'ifModified = Base16.encode <$> ifModified
          , _RpbGetReq'key = key
          , _RpbGetReq'notfoundOk = if noNotfoundOk then Just False else Nothing
          , _RpbGetReq'nVal = nVal
          , _RpbGetReq'pr = pr
          , _RpbGetReq'r = r
          , _RpbGetReq'sloppyQuorum = if sloppyQuorum then Just True else Nothing
          , _RpbGetReq'timeout = timeout
          , _RpbGetReq'type' = type'
          }
