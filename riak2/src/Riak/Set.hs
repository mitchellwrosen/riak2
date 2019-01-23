module Riak.Set
  ( Set(..)
  , map
  , get
  , update
  , Update(..)
  ) where

import Riak.Interface        (Result)
import Riak.Internal.Client  (Client(..))
import Riak.Internal.Context (Context(..))
import Riak.Internal.Prelude hiding (Set, map)
import Riak.Key              (Key(..))
import Riak.Proto

import qualified Riak.Interface  as Interface
import qualified Riak.Proto.Lens as L

import qualified Data.ByteString as ByteString
import qualified Data.HashSet    as HashSet


data Set a
  = Set
  { context :: !Context
  , key :: !Key
  , value :: !(HashSet a)
  } deriving stock (Generic, Show)

data Update
  = Add ByteString
  | Remove ByteString

map :: (Eq b, Hashable b) => (a -> b) -> Set a -> Set b
map f =
  field @"value" %~ HashSet.map f

get ::
     MonadIO m
  => Client
  -> Key
  -> m (Result (Set ByteString))
get client k@(Key type' bucket key) = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.getCrdt (iface client) request)

  where
    request :: DtFetchReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.includeContext .~ True
        & L.key .~ key
        & L.type' .~ type'

        -- TODO get set opts
        -- & L.maybe'basicQuorum .~ undefined
        -- & L.maybe'nVal .~ undefined
        -- & L.maybe'notfoundOk .~ undefined
        -- & L.maybe'pr .~ undefined
        -- & L.maybe'r .~ undefined
        -- & L.maybe'sloppyQuorum .~ undefined
        -- & L.maybe'timeout .~ undefined

    fromResponse :: DtFetchResp -> Set ByteString
    fromResponse response =
      Set
        { context = Context (response ^. L.context)
        , key = k
        , value = HashSet.fromList (response ^. L.value . L.setValue)
        }

update ::
     MonadIO m
  => Client
  -> Set ByteString
  -> [Update]
  -> m (Result (Set ByteString))
update client (Set { context, key }) updates = liftIO $
  (fmap.fmap)
    fromResponse
    (Interface.updateCrdt (iface client) request)

  where
    request :: DtUpdateReq
    request =
      defMessage
        & L.bucket .~ bucket
        & L.includeContext .~ True
        & L.maybe'context .~
            (if ByteString.null (unContext context)
              then Nothing
              else Just (unContext context))
        & L.maybe'key .~
            (if ByteString.null k
              then Nothing
              else Just k)
        & L.op .~
            (defMessage
              & L.setOp .~ updatesToOp updates)
        & L.returnBody .~ True
        & L.type' .~ type'

-- TODO set update opts
-- _DtUpdateReq'w :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'dw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'pw :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'timeout :: !(Prelude.Maybe Data.Word.Word32),
-- _DtUpdateReq'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
-- _DtUpdateReq'nVal :: !(Prelude.Maybe Data.Word.Word32),

    Key type' bucket k =
      key

    fromResponse :: DtUpdateResp -> Set ByteString
    fromResponse response =
      Set
        { context = Context (response ^. L.context)
        , key =
            if ByteString.null k
              then key { key = response ^. L.key }
              else key
        , value = HashSet.fromList (response ^. L.setValue)
        }

updatesToOp :: [Update] -> SetOp
updatesToOp updates =
  defMessage
    & L.adds .~ adds
    & L.removes .~ removes

  where
    adds :: [ByteString]
    adds =
      [ value | Add value <- updates ]

    removes :: [ByteString]
    removes =
      [ value | Remove value <- updates ]
