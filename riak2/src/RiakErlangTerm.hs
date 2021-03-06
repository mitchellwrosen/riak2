-- | Erlang external term format encoding.
--
-- <http://erlang.org/doc/apps/erts/erl_ext_dist.html>

module RiakErlangTerm
  ( ErlangTerm(..)
  , renderErlangTerm

  , build
  , decode
  , decodeIO
    -- ** Smart constructors
  , bool
  , tuple2
  , tuple3
  , tuple4
  , tuple5
  , list
  ) where

import Data.Attoparsec.ByteString (Parser)
import Data.Bits
import Data.ByteString.Builder    (Builder)
import Data.Int
import Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import Data.Text.Prettyprint.Doc  (Pretty(..))
import Data.Vector                (Vector)

import qualified Data.Attoparsec.ByteString            as Atto
import qualified Data.ByteString                       as ByteString
import qualified Data.ByteString.Builder               as Builder
import qualified Data.ByteString.Lazy                  as LazyByteString
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Vector                           as Vector


-- TODO ErlSmallAtomUtf8
data ErlangTerm
  -- ErlAtomCacheRef Word8
  = ErlAtom Text -- Latin1
  | ErlAtomUtf8 Text
  | ErlBinary ByteString
  -- ErlBitBinary ...
  -- ErlExport ...
  | ErlInteger Int32
  -- ErlLargeBig ...
  -- ErlLargeTuple (Vector ErlangTerm)
  | ErlList (Vector ErlangTerm) ErlangTerm
  -- ErlMap (HashMap ErlangTerm ErlangTerm)
  -- ErlNewFloat ...
  -- ErlNewFun ...
  -- ErlNewReference ...
  | ErlNil
  -- ErlPid ByteString Word32 Word32 Word8
  -- ErlPort ByteString Word32 Word8
  -- ErlSmallAtomUtf8 ...
  -- ErlSmallBig ...
  | ErlSmallBig Integer
  | ErlSmallInteger Word8
  | ErlSmallTuple (Vector ErlangTerm)
  -- ErlString ByteString
  deriving stock (Eq, Show)

instance Pretty ErlangTerm where
  pretty :: ErlangTerm -> Pretty.Doc ann
  pretty = \case
    ErlAtom atom ->
      pretty atom

    ErlAtomUtf8 atom ->
      pretty atom

    ErlBinary binary ->
      "<<" <> pretty (show binary) <> ">>"

    ErlInteger n ->
      pretty n

    ErlList elems ErlNil ->
      pretty (Vector.toList elems)

    ErlList elems lastElem ->
      Pretty.group
        (Pretty.encloseSep
          (Pretty.flatAlt "[ " "[")
          (Pretty.line <> "| " <> pretty lastElem <> Pretty.flatAlt " ]" "]")
          ", "
          (map pretty (Vector.toList elems)))

    ErlNil ->
      "[]"

    ErlSmallBig n ->
      pretty n

    ErlSmallInteger n ->
      pretty n

    ErlSmallTuple elems ->
      Pretty.group
        (Pretty.encloseSep
          (Pretty.flatAlt "{ " "{")
          (Pretty.flatAlt " }" "}")
          ", "
          (map pretty (Vector.toList elems)))

renderErlangTerm :: ErlangTerm -> Text
renderErlangTerm =
  Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

build :: ErlangTerm -> ByteString
build =
  build_                   >>>
  (Builder.word8 131 <>)   >>>
  Builder.toLazyByteString >>>
  LazyByteString.toStrict

-- Like 'build', but without the version number.
build_ :: ErlangTerm -> Builder
build_ =
  mconcat . \case
    ErlAtom atom ->
      [ Builder.word8 100
      , Builder.int16BE (fromIntegral (Text.length atom))
      , Builder.byteString (encodeUtf8 atom)
      ]

    ErlAtomUtf8 atom ->
      [ Builder.word8 118
      , Builder.int16BE (fromIntegral (Text.length atom))
      , Builder.byteString (encodeUtf8 atom)
      ]

    ErlBinary bytes ->
      [ Builder.word8 109
      , bytes32 bytes
      ]

    ErlInteger _ ->
      undefined
      -- TODO ErlSmallBig external term format

    ErlList xs x ->
      [ Builder.word8 108
      , Builder.int32BE (fromIntegral (Vector.length xs))
      , foldMap build_ xs
      , build_ x
      ]

    ErlNil ->
      [ Builder.word8 106
      ]

    ErlSmallBig _ ->
      undefined
      -- TODO ErlSmallBig external term format

    ErlSmallInteger n ->
      [ Builder.word8 97
      , Builder.word8 n
      ]

    ErlSmallTuple terms ->
      [ Builder.word8 104
      , Builder.word8 (fromIntegral (Vector.length terms))
      , foldMap build_ terms
      ]

decode :: ByteString -> Either String ErlangTerm
decode =
  Atto.parseOnly (Atto.word8 131 *> parser <* Atto.endOfInput)

decodeIO :: ByteString -> IO ErlangTerm
decodeIO bytes =
  case decode bytes of
    Left err ->
      error err -- TODO erlang term decode error

    Right term ->
      pure term

parser :: Parser ErlangTerm
parser =
  Atto.anyWord8 >>= \case
    97 ->
      ErlSmallInteger <$> Atto.anyWord8

    98 -> do
      ErlInteger <$> int32be

    100 -> do
      n <- int16be
      ErlAtom . decodeUtf8 <$> Atto.take (fromIntegral n)

    104 -> do
      n <- Atto.anyWord8
      ErlSmallTuple <$> Vector.replicateM (fromIntegral n) parser

    106 ->
      pure ErlNil

    108 -> do
      n <- int32be
      xs <- Vector.replicateM (fromIntegral n) parser
      x <- parser
      pure (ErlList xs x)

    109 -> do
      n <- int32be
      ErlBinary <$> Atto.take (fromIntegral n)

    110 -> do
      n <- Atto.anyWord8
      let
        pf :: Parser (Integer -> Integer)
        pf = id <$ Atto.word8 0 <|> negate <$ Atto.word8 1
      let
        step :: Word8 -> Integer -> Integer
        step w acc = fromIntegral w + acc * 256
      ErlSmallBig <$>
        (pf <*> (ByteString.foldr' step 0 <$> Atto.take (fromIntegral n)))

    tag ->
      fail ("Unknown tag: " ++ show tag)

  where
    int16be :: Parser Int16
    int16be = do
      w0 <- Atto.anyWord8
      w1 <- Atto.anyWord8
      pure $
        (fromIntegral w0 `shiftL`  8) .|.
        fromIntegral w1

    int32be :: Parser Int32
    int32be = do
      w0 <- Atto.anyWord8
      w1 <- Atto.anyWord8
      w2 <- Atto.anyWord8
      w3 <- Atto.anyWord8
      pure $
        (fromIntegral w0 `shiftL` 24) .|.
        (fromIntegral w1 `shiftL` 16) .|.
        (fromIntegral w2 `shiftL`  8) .|.
        fromIntegral w3

-- bytes16 :: ByteString -> Builder
-- bytes16 bytes =
--   Builder.int16BE (fromIntegral (ByteString.length bytes)) <>
--     Builder.byteString bytes

bytes32 :: ByteString -> Builder
bytes32 bytes =
  Builder.int32BE (fromIntegral (ByteString.length bytes)) <>
    Builder.byteString bytes

bool :: Bool -> ErlangTerm
bool = \case
  True  -> ErlAtomUtf8 "true"
  False -> ErlAtomUtf8 "false"

tuple2 :: ErlangTerm -> ErlangTerm -> ErlangTerm
tuple2 a b =
  ErlSmallTuple (Vector.fromList [a, b])

tuple3 :: ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm
tuple3 a b c =
  ErlSmallTuple (Vector.fromList [a, b, c])

tuple4 :: ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm
tuple4 a b c d =
  ErlSmallTuple (Vector.fromList [a, b, c, d])

tuple5 :: ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm -> ErlangTerm
tuple5 a b c d e =
  ErlSmallTuple (Vector.fromList [a, b, c, d, e])

list :: Vector ErlangTerm -> ErlangTerm
list elems =
  ErlList elems ErlNil
