{-# LANGUAGE NoImplicitPrelude #-}

module Riak.Internal.Debug where

import System.IO

import Riak.Internal.Prelude

-- TODO: add cabal flag to control debugging
debug :: String -> IO ()
debug =
  hPutStrLn stderr
