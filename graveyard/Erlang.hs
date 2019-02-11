-- | Erlang external term format encoding.
--
-- <http://erlang.org/doc/apps/erts/erl_ext_dist.html>

module ZZZ.Erlang where
  -- ( ErlTerm(..)
  -- , buildErlTerm
  -- , decodeErlTerm
  -- ) where

-- import Control.Applicative
-- import Control.Monad
-- import Data.Attoparsec.ByteString (Parser)
-- import Data.Bits
-- import Data.ByteString            (ByteString)
-- import Data.ByteString.Builder    (Builder)
-- -- import Data.HashMap.Strict     (HashMap)
-- import Data.Int
-- import Data.Text          (Text)
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- import Data.Vector        (Vector)
-- import Data.Word
-- import Prelude

-- import qualified Data.Attoparsec.ByteString as Atto
-- import qualified Data.ByteString            as ByteString
-- import qualified Data.ByteString.Builder    as Builder
-- import qualified Data.ByteString.Lazy       as LazyByteString
-- import qualified Data.Text                  as Text
-- import qualified Data.Vector                as Vector

-- data ErlTerm
  -- -- ErlAtomCacheRef !Word8
  -- = ErlAtom !Text -- Latin1
  -- | ErlAtomUtf8 !Text
  -- | ErlBinary !ByteString
  -- -- ErlBitBinary ...
  -- -- ErlExport ...
  -- | ErlInteger !Int32
  -- -- ErlLargeBig ...
  -- -- ErlLargeTuple !(Vector ErlTerm)
  -- | ErlList !(Vector ErlTerm) !ErlTerm
  -- -- ErlMap !(HashMap ErlTerm ErlTerm)
  -- -- ErlNewFloat ...
  -- -- ErlNewFun ...
  -- -- ErlNewReference ...
  -- | ErlNil
  -- -- ErlPid !ByteString !Word32 !Word32 !Word8
  -- -- ErlPort !ByteString !Word32 !Word8
  -- -- ErlSmallAtomUtf8 ...
  -- -- ErlSmallBig ...
  -- | ErlSmallBig !Integer
  -- | ErlSmallInteger !Word8
  -- | ErlSmallTuple !(Vector ErlTerm)
  -- -- ErlString !ByteString
  -- deriving (Eq, Show)


-- buildErlTerm :: ErlTerm -> ByteString
-- buildErlTerm =
  -- LazyByteString.toStrict .
  -- Builder.toLazyByteString .
  -- (Builder.word8 131 <>) .
  -- buildErlTerm_

-- -- | Like 'buildErlTerm', but without the version number.
-- buildErlTerm_ :: ErlTerm -> Builder
-- buildErlTerm_ =
  -- mconcat . \case
  --   ErlAtom atom ->
  --     [ Builder.word8 100
  --     , Builder.int16BE (fromIntegral (Text.length atom))
  --     , Builder.byteString (encodeUtf8 atom)
  --     ]

  --   ErlAtomUtf8 atom ->
  --     [ Builder.word8 118
  --     , Builder.int16BE (fromIntegral (Text.length atom))
  --     , Builder.byteString (encodeUtf8 atom)
  --     ]

  --   ErlBinary bytes ->
  --     [ Builder.word8 109
  --     , bytes32 bytes
  --     ]

  --   ErlInteger _ ->
  --     undefined
  --     -- TODO ErlSmallBig external term format

  --   ErlList xs x ->
  --     [ Builder.word8 108
  --     , Builder.int32BE (fromIntegral (Vector.length xs))
  --     , foldMap buildErlTerm_ xs
  --     , buildErlTerm_ x
  --     ]

  --   ErlNil ->
  --     [ Builder.word8 106
  --     ]

  --   ErlSmallBig _ ->
  --     undefined
  --     -- TODO ErlSmallBig external term format

  --   ErlSmallInteger n ->
  --     [ Builder.word8 97
  --     , Builder.word8 n
  --     ]

  --   ErlSmallTuple terms ->
  --     [ Builder.word8 104
  --     , Builder.word8 (fromIntegral (Vector.length terms))
  --     , foldMap buildErlTerm_ terms
  --     ]

-- decodeErlTerm :: ByteString -> Either String ErlTerm
-- decodeErlTerm =
  -- Atto.parseOnly erlTermParser

-- erlTermParser :: Parser ErlTerm
-- erlTermParser = do
  -- void (Atto.word8 131)
  -- erlTermParser_

-- erlTermParser_ :: Parser ErlTerm
-- erlTermParser_ =
  -- Atto.anyWord8 >>= \case
  --   97 ->
  --     ErlSmallInteger <$> Atto.anyWord8

  --   98 -> do
  --     ErlInteger <$> int32be

  --   100 -> do
  --     n <- int16be
  --     ErlAtom . decodeUtf8 <$> Atto.take (fromIntegral n)

  --   104 -> do
  --     n <- Atto.anyWord8
  --     ErlSmallTuple <$> Vector.replicateM (fromIntegral n) erlTermParser_

  --   106 ->
  --     pure ErlNil

  --   108 -> do
  --     n <- int32be
  --     xs <- Vector.replicateM (fromIntegral n) erlTermParser_
  --     x <- erlTermParser_
  --     pure (ErlList xs x)

  --   109 -> do
  --     n <- int32be
  --     ErlBinary <$> Atto.take (fromIntegral n)

  --   110 -> do
  --     n <- Atto.anyWord8
  --     let
  --       pf :: Parser (Integer -> Integer)
  --       pf = id <$ Atto.word8 0 <|> negate <$ Atto.word8 1
  --     let
  --       step :: Word8 -> Integer -> Integer
  --       step w acc = fromIntegral w + acc * 256
  --     ErlSmallBig <$>
  --       (pf <*> (ByteString.foldr' step 0 <$> Atto.take (fromIntegral n)))

  --   tag ->
  --     fail ("Unknown tag: " ++ show tag)

 -- where
  -- int16be :: Parser Int16
  -- int16be = do
  --   w0 <- Atto.anyWord8
  --   w1 <- Atto.anyWord8
  --   pure $
  --     (fromIntegral w0 `shiftL`  8) .|.
  --      fromIntegral w1

  -- int32be :: Parser Int32
  -- int32be = do
  --   w0 <- Atto.anyWord8
  --   w1 <- Atto.anyWord8
  --   w2 <- Atto.anyWord8
  --   w3 <- Atto.anyWord8
  --   pure $
  --     (fromIntegral w0 `shiftL` 24) .|.
  --     (fromIntegral w1 `shiftL` 16) .|.
  --     (fromIntegral w2 `shiftL`  8) .|.
  --      fromIntegral w3

-- -- bytes16 :: ByteString -> Builder
-- -- bytes16 bytes =
-- --   Builder.int16BE (fromIntegral (ByteString.length bytes)) <>
-- --     Builder.byteString bytes

-- bytes32 :: ByteString -> Builder
-- bytes32 bytes =
  -- Builder.int32BE (fromIntegral (ByteString.length bytes)) <>
  --   Builder.byteString bytes
