module Riak.Internal.Debug where

import System.IO

-- TODO: add cabal flag to control debugging
debug :: String -> IO ()
debug =
  hPutStrLn stderr
