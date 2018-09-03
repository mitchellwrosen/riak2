-- | Erlang external term format encoding.
--
-- <http://erlang.org/doc/apps/erts/erl_ext_dist.html>

{-# LANGUAGE LambdaCase #-}

module Erlang
  ( ErlTerm(..)
  , buildErlTerm
  ) where

import Data.ByteString         (ByteString)
import Data.ByteString.Builder (Builder)
-- import Data.HashMap.Strict     (HashMap)
-- import Data.Int
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector        (Vector)
-- import Data.Word

import qualified Data.ByteString         as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Text               as Text
import qualified Data.Vector             as Vector

data ErlTerm
  -- = ErlAtomCacheRef !Word8
  = ErlAtomUtf8 !Text
  | ErlBinary !ByteString
  -- | ErlBitBinary ...
  -- | ErlExport ...
  -- | ErlInteger !Int32
  -- | ErlLargeBig ...
  -- | ErlLargeTuple !(Vector ErlTerm)
  | ErlList !(Vector ErlTerm) !ErlTerm
  -- | ErlMap !(HashMap ErlTerm ErlTerm)
  -- | ErlNewFloat ...
  -- | ErlNewFun ...
  -- | ErlNewReference ...
  | ErlNil
  -- | ErlPid !ByteString !Word32 !Word32 !Word8
  -- | ErlPort !ByteString !Word32 !Word8
  -- | ErlSmallAtomUtf8 ...
  -- | ErlSmallBig ...
  -- | ErlSmallInteger !Word8
  | ErlSmallTuple !(Vector ErlTerm)
  -- | ErlString !ByteString


buildErlTerm :: ErlTerm -> ByteString
buildErlTerm =
  LazyByteString.toStrict .
  Builder.toLazyByteString .
  (Builder.word8 131 <>) .
  buildErlTerm_

-- | Like 'buildErlTerm', but without the version number.
buildErlTerm_ :: ErlTerm -> Builder
buildErlTerm_ =
  mconcat . \case
    ErlAtomUtf8 atom ->
      [ Builder.word8 118
      , Builder.int16BE (fromIntegral (Text.length atom))
      , Builder.byteString (encodeUtf8 atom)
      ]

    ErlBinary bytes ->
      [ Builder.word8 109
      , bytes32 bytes
      ]

    ErlList xs x ->
      [ Builder.word8 108
      , Builder.int32BE (fromIntegral (Vector.length xs))
      , foldMap buildErlTerm_ xs
      , buildErlTerm_ x
      ]

    ErlNil ->
      [ Builder.word8 106
      ]

    ErlSmallTuple terms ->
      [ Builder.word8 104
      , Builder.word8 (fromIntegral (Vector.length terms))
      , foldMap buildErlTerm_ terms
      ]

-- bytes16 :: ByteString -> Builder
-- bytes16 bytes =
--   Builder.int16BE (fromIntegral (ByteString.length bytes)) <>
--     Builder.byteString bytes

bytes32 :: ByteString -> Builder
bytes32 bytes =
  Builder.int32BE (fromIntegral (ByteString.length bytes)) <>
    Builder.byteString bytes
