{-# LANGUAGE DataKinds, TypeApplications #-}

import Control.Monad
import Data.ByteString     (ByteString)
import Data.Word
import Options.Applicative

import qualified Data.ByteString.Base16 as Base16

import Riak                   (Proxy(..), (&), (:=)(..), def, param)
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
          <*> (fmap Riak.Bucket . strOption)
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
          <*> (fmap Riak.Key . strOption)
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
          <*> (optional . fmap Riak.BucketType . strOption)
                (mconcat
                  [ long "type"
                  , help "Bucket type"
                  , metavar "TYPE"
                  ]))
        (progDesc "Fetch an object")))

doFetchObject
  :: Bool
  -> Riak.Bucket
  -> Bool
  -> Bool
  -> Maybe ByteString -- vclock
  -> Riak.Key
  -> Bool
  -> Maybe Word32
  -> Maybe Word32
  -> Maybe Word32
  -> Bool
  -> Maybe Word32
  -> Maybe Riak.BucketType
  -> IO ()
doFetchObject
    basic_quorum bucket deletedvclock head if_modified key no_notfound_ok n_val
    pr r sloppy_quorum timeout type' =
  Riak.withHandle "localhost" 8087 $ \h ->
    print =<<
      Riak.fetchObject h
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
        , Proxy := type'
        )
