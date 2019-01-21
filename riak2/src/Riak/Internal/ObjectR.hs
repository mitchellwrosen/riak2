module Riak.Internal.ObjectR
  ( ObjectR(..)
  , fromGetResp
  , fromPutResp
  ) where

import Riak.Index            (Index)
import Riak.Internal.Prelude
import Riak.Proto
import Riak.Vclock           (Vclock)
import Riak.Vclock           (Vclock(..))
import Riak.Vtag             (Vtag(..))

import qualified Riak.Internal.Content as Content
import qualified Riak.Internal.Index   as Index
import qualified Riak.Proto.Lens       as L

import qualified Data.List.NonEmpty as List1


data ObjectR a
  = ObjectR
  { charset :: Maybe ByteString
  , contentEncoding :: Maybe ByteString
  , contentType :: Maybe ByteString
  , deleted :: Bool
  , indexes :: [Index]
  , lastModified :: Maybe UTCTime
  , metadata :: [(ByteString, Maybe ByteString)]
  , ttl :: Maybe Word32 -- TODO NominalDiffTime
  , value :: a
  , vclock :: Vclock
  , vtag :: Maybe Vtag
  } deriving stock (Functor, Generic, Show)

fromContent_ ::
     ByteString
  -> RpbContent
  -> Maybe (ObjectR ByteString)
fromContent_ vclock content = do
  guard (not (content ^. L.deleted))
  pure ObjectR
    { charset = content ^. L.maybe'charset
    , contentEncoding = content ^. L.maybe'contentEncoding
    , contentType = content ^. L.maybe'contentType
    , deleted = content ^. L.deleted
    , indexes = map Index.fromPair (content ^. L.indexes)
    , lastModified = Content.lastModified content
    , metadata = Content.metadata content
    , ttl = content ^. L.maybe'ttl
    , value = content ^. L.value
    , vclock = Vclock vclock
    , vtag = coerce (content ^. L.maybe'vtag)
    }

fromGetResp :: RpbGetResp -> [ObjectR ByteString]
fromGetResp response =
  mapMaybe
    (fromContent_ (response ^. L.vclock))
    (response ^. L.content)

-- | Parse an object from a put response.
--
-- Assumes that either @return_body@ or @return_head@ was set on the request.
fromPutResp :: RpbPutResp -> NonEmpty (ObjectR ByteString)
fromPutResp response =
  List1.fromList
    (mapMaybe
      (fromContent_ (response ^. L.vclock))
      (response ^. L.content))
