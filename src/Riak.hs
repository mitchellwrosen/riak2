{-# LANGUAGE DataKinds, DeriveAnyClass, DerivingStrategies, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             InstanceSigs, KindSignatures, LambdaCase, MagicHash,
             MultiParamTypeClasses, OverloadedLabels, OverloadedStrings,
             PatternSynonyms, RankNTypes, ScopedTypeVariables,
             StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Riak
  ( -- * Riak handle
    Handle
  , withHandle
  , VclockCache
  , refVclockCache
    -- * Key/value object operations
    -- ** Fetch object
  , fetchObject
    -- ** Store object
  , StoreObjectResp(..)
  , ObjectReturn(..)
  , ObjectReturnTy
  , storeObject
    -- ** Delete object
  , deleteObject
    -- * Data type operations
  , fetchCounter
  , fetchSet
  , fetchDataType
  , updateCounter
  , updateDataType
    -- * Bucket operations
  , getBucketTypeProps
  , setBucketTypeProps
  , getBucketProps
  , setBucketProps
  , resetBucketProps
  , listBuckets
  , listKeys
    -- * MapReduce
  , mapReduce
    -- * Secondary indexes (2i)
    -- * Search 2.0
  , getSchema
  , putSchema
  , getIndex
  , putIndex
  , deleteIndex
    -- * Server info
  , ping
  , getServerInfo
    -- * Types
  , Bucket(..)
  , BucketType(..)
  , pattern BucketTypeDefault
  , Content(..)
  , ContentType(..)
  , DataType(..)
  , IfModified(..)
  , Key(..)
  , Quorum(..)
  , pattern QuorumAll
  , pattern QuorumDefault
  , pattern QuorumOne
  , pattern QuorumQuorum
  , Vclock(..)
  , Vtag(..)
    -- * Optional parameters
  , ParamBasicQuorum(..)
  , ParamDW(..)
  , ParamHead(..)
  , ParamIfModified(..)
  , ParamNotfoundOk(..)
  , ParamNVal(..)
  , ParamObjectReturn(..)
  , ParamPR(..)
  , ParamPW(..)
  , ParamR(..)
  , ParamReturnBody(..)
  , ParamReturnHead(..)
  , ParamSloppyQuorum(..)
  , ParamTimeout(..)
  , ParamW(..)
    -- * Temp
  , def
  ) where

import Control.Applicative
import Control.Monad              (guard, when)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString            (ByteString)
import Data.Coerce                (coerce)
import Data.Default.Class
import Data.Function              (fix)
import Data.Hashable              (Hashable)
import Data.HashMap.Strict        (HashMap)
import Data.Int
import Data.IORef
import Data.Kind                  (Type)
import Data.Text                  (Text)
import Data.Text.Encoding         (decodeUtf8)
import Data.Type.Bool             (If)
import Data.Word
import GHC.Exts                   (IsString)
import Lens.Family2.Unchecked     (lens)
import Lens.Labels
import List.Transformer           (ListT)
import Network.Socket             (HostName, PortNumber)
import Prelude                    hiding (head, return, (.))
import UnliftIO.Exception         (Exception, throwIO)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text              as Text
import qualified List.Transformer       as ListT

import           Proto.Riak
import qualified Riak.Internal            as Internal
import           Riak.Internal.Connection
import           Riak.Internal.Panic
import           Riak.Internal.Request
import           Riak.Internal.Response


--------------------------------------------------------------------------------
-- Handle
--------------------------------------------------------------------------------

-- | A non-thread-safe handle to Riak.
data Handle
  = Handle
      !Connection
      !VclockCache

withHandle
  :: MonadUnliftIO m
  => HostName
  -> PortNumber
  -> VclockCache
  -> (Handle -> m a)
  -> m a
withHandle host port cache f = do
  withConnection host port (\conn -> f (Handle conn cache))


data VclockCache
  = VclockCache
      { vclockCacheLookup ::
          !(forall ty. BucketType ty -> Bucket -> Key -> IO (Maybe Vclock))
      , vclockCacheInsert ::
          !(forall ty. BucketType ty -> Bucket -> Key -> Vclock -> IO ())
      , vclockCacheDelete ::
          !(forall ty. BucketType ty -> Bucket -> Key -> IO ())
      }

-- | Make a 'VclockCache' backed by an 'IORef' + 'HashMap' that never purges
-- entries. TODO Smarter cache invalidation (controllable timeout).
refVclockCache :: MonadIO m => m VclockCache
refVclockCache = liftIO $ do
  -- TODO strict (type, bucket, key)
  cacheRef :: IORef (HashMap (SomeBucketType, Bucket, Key) Vclock) <-
    liftIO (newIORef mempty)

  pure VclockCache
    { vclockCacheLookup =
        \type' bucket key -> do
          HashMap.lookup (coerce (unBucketType type'), bucket, key) <$>
            readIORef cacheRef

    , vclockCacheInsert =
        \type' bucket key vclock ->
          modifyIORef' cacheRef
            (HashMap.insert (coerce (unBucketType type'), bucket, key) vclock)

    , vclockCacheDelete =
        \type' bucket key ->
          modifyIORef cacheRef
            (HashMap.delete (coerce (unBucketType type'), bucket, key))
    }


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A Riak bucket type, tagged with the data type it contains.
newtype BucketType (ty :: Maybe DataType)
  = BucketType { unBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

pattern BucketTypeDefault :: BucketType 'Nothing
pattern BucketTypeDefault =
  BucketType "default"


-- | A Riak bucket.
newtype Bucket
  = Bucket { unBucket :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Bucket where
  show :: Bucket -> String
  show =
    Text.unpack . decodeUtf8 . unBucket


-- TODO IsContent typeclass
data Content a
  = Content
      !a                                -- Value
      !(Maybe ContentType)
      !(Maybe ByteString)               -- Charset
      !(Maybe ByteString)               -- Content encoding
      !(Maybe Vtag)
      !(Maybe Word32)                   -- Last modified
      !(Maybe Word32)                   -- Last modified usecs
      ![(ByteString, Maybe ByteString)] -- User meta
      ![(ByteString, Maybe ByteString)] -- Indexes
      !(Maybe Bool)                     -- Deleted
      !(Maybe Word32)                   -- TTL
  deriving (Show)

instance {-# OVERLAPPABLE #-}
    ( HasLens' f (Content s) x a
    , s ~ t
    , a ~ b
    ) => HasLens f (Content s) (Content t) x a b where
  lensOf = lensOf'

instance Functor f => HasLens  f (Content a) (Content b) "value"           a                                b where lensOf  _ = lens (\(Content x _ _ _ _ _ _ _ _ _ _) -> x) (\(Content _ b c d e f g h i j k) x -> Content x b c d e f g h i j k)
instance Functor f => HasLens' f (Content a)             "contentType"     (Maybe ContentType)                where lensOf' _ = lens (\(Content _ x _ _ _ _ _ _ _ _ _) -> x) (\(Content a _ c d e f g h i j k) x -> Content a x c d e f g h i j k)
instance Functor f => HasLens' f (Content a)             "charset"         (Maybe ByteString)                 where lensOf' _ = lens (\(Content _ _ x _ _ _ _ _ _ _ _) -> x) (\(Content a b _ d e f g h i j k) x -> Content a b x d e f g h i j k)
instance Functor f => HasLens' f (Content a)             "contentEncoding" (Maybe ByteString)                 where lensOf' _ = lens (\(Content _ _ _ x _ _ _ _ _ _ _) -> x) (\(Content a b c _ e f g h i j k) x -> Content a b c x e f g h i j k)
instance Functor f => HasLens' f (Content a)             "vtag"            (Maybe Vtag)                       where lensOf' _ = lens (\(Content _ _ _ _ x _ _ _ _ _ _) -> x) (\(Content a b c d _ f g h i j k) x -> Content a b c d x f g h i j k)
instance Functor f => HasLens' f (Content a)             "lastMod"         (Maybe Word32)                     where lensOf' _ = lens (\(Content _ _ _ _ _ x _ _ _ _ _) -> x) (\(Content a b c d e _ g h i j k) x -> Content a b c d e x g h i j k)
instance Functor f => HasLens' f (Content a)             "lastModUsecs"    (Maybe Word32)                     where lensOf' _ = lens (\(Content _ _ _ _ _ _ x _ _ _ _) -> x) (\(Content a b c d e f _ h i j k) x -> Content a b c d e f x h i j k)
instance Functor f => HasLens' f (Content a)             "usermeta"        [(ByteString, Maybe ByteString)]   where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ x _ _ _) -> x) (\(Content a b c d e f g _ i j k) x -> Content a b c d e f g x i j k)
instance Functor f => HasLens' f (Content a)             "indexes"         [(ByteString, Maybe ByteString)]   where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ _ x _ _) -> x) (\(Content a b c d e f g h _ j k) x -> Content a b c d e f g h x j k)
instance Functor f => HasLens' f (Content a)             "deleted"         (Maybe Bool)                       where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ _ _ x _) -> x) (\(Content a b c d e f g h i _ k) x -> Content a b c d e f g h i x k)
instance Functor f => HasLens' f (Content a)             "ttl"             (Maybe Word32)                     where lensOf' _ = lens (\(Content _ _ _ _ _ _ _ _ _ _ x) -> x) (\(Content a b c d e f g h i j _) x -> Content a b c d e f g h i j x)


newtype ContentType
  = ContentType { unContentType :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (IsString)


data DataType
  = DataTypeCounter
  | DataTypeMap
  | DataTypeSet
  -- TODO hll, gset


-- | A 'DataTypeError' is thrown when a data type operation is performed on an
-- incompatible bucket type (for example, attempting to fetch a counter from a
-- bucket type that contains sets).
data DataTypeError
  = DataTypeError
      !SomeBucketType       -- Bucket type
      !Bucket               -- Bucket
      !Key                  -- Key
      !DtFetchResp'DataType -- Actual data type
      !DtFetchResp'DataType -- Expected data type
  deriving stock (Show)
  deriving anyclass (Exception)


data IfModified a
  = Unmodified
  | Modified a


-- | A Riak key.
newtype Key
  = Key { unKey :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show Key where
  show :: Key -> String
  show =
    Text.unpack . decodeUtf8 . unKey


data ObjectReturn
  = ObjectReturnNone
  | ObjectReturnHead
  | ObjectReturnBody


newtype Quorum
  = Quorum Word32
  deriving stock (Eq)
  deriving newtype (Num)

pattern QuorumAll :: Quorum
pattern QuorumAll = 4294967292

pattern QuorumDefault :: Quorum
pattern QuorumDefault = 4294967291

pattern QuorumOne :: Quorum
pattern QuorumOne = 4294967294

pattern QuorumQuorum :: Quorum
pattern QuorumQuorum = 4294967293


newtype SomeBucketType
  = SomeBucketType { unSomeBucketType :: ByteString }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show SomeBucketType where
  show :: SomeBucketType -> String
  show =
    Text.unpack . decodeUtf8 . unSomeBucketType


newtype Vclock
  = Vclock { unVclock :: ByteString }

instance Show Vclock where
  show :: Vclock -> String
  show =
    show . Base64.encode . unVclock


newtype Vtag
  = Vtag { unVtag :: ByteString }
  deriving (Show)


--------------------------------------------------------------------------------
-- Params
--------------------------------------------------------------------------------

-- | Whether or not to use the "basic quorum" policy for not-founds.
newtype ParamBasicQuorum
  = ParamBasicQuorum Bool

instance Default ParamBasicQuorum where
  def = coerce False


newtype ParamDW
  = ParamDW Quorum

instance Default ParamDW where
  def = coerce QuorumDefault


data ParamHead :: Bool -> Type where
  ParamHead   :: ParamHead 'True
  ParamNoHead :: ParamHead 'False

instance (a ~ 'False) => Default (ParamHead a) where
  def = ParamNoHead


data ParamIfModified :: Bool -> Type where
  ParamIfModified   :: ParamIfModified 'True
  ParamNoIfModified :: ParamIfModified 'False

instance (a ~ 'False) => Default (ParamIfModified a) where
  def = ParamNoIfModified


newtype ParamIncludeContext
  = ParamIncludeContext Bool

instance Default ParamIncludeContext where
  def = coerce True


-- | Whether to treat not-found responses as successful.
newtype ParamNotfoundOk
  = ParamNotfoundOk Bool

instance Default ParamNotfoundOk where
  def = coerce True


newtype ParamNVal
  = ParamNVal (Maybe Word32)

instance Default ParamNVal where
  def = ParamNVal Nothing


data ParamObjectReturn :: ObjectReturn -> Type where
  ParamObjectReturnNone :: ParamObjectReturn 'ObjectReturnNone
  ParamObjectReturnHead :: ParamObjectReturn 'ObjectReturnHead
  ParamObjectReturnBody :: ParamObjectReturn 'ObjectReturnBody


newtype ParamPR
  = ParamPR Quorum

instance Default ParamPR where
  def = coerce QuorumDefault


newtype ParamPW
  = ParamPW Quorum

instance Default ParamPW where
  def = coerce QuorumDefault


newtype ParamR
  = ParamR Quorum

instance Default ParamR where
  def = coerce QuorumDefault


-- TODO remove ParamReturnBody
newtype ParamReturnBody
  = ParamReturnBody Bool

instance Default ParamReturnBody where
  def = coerce False


-- TODO remove ParamReturnHead
newtype ParamReturnHead
  = ParamReturnHead Bool

instance Default ParamReturnHead where
  def = coerce False


newtype ParamSloppyQuorum
  = ParamSloppyQuorum Bool

instance Default ParamSloppyQuorum where
  def = coerce False


newtype ParamTimeout
  = ParamTimeout (Maybe Word32)

instance Default ParamTimeout where
  def = ParamTimeout Nothing


newtype ParamW
  = ParamW Quorum

instance Default ParamW where
  def = coerce QuorumDefault


--------------------------------------------------------------------------------
-- Misc. fancy type helpers
--------------------------------------------------------------------------------

data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

type FetchObjectResp (head :: Bool) (if_modified :: Bool) =
  IfModifiedWrapper if_modified [Content (If head () ByteString)]

type family IfModifiedWrapper (if_modified :: Bool) (a :: Type) where
  IfModifiedWrapper 'True  a = IfModified a
  IfModifiedWrapper 'False a = a

fetchObject
  :: forall head if_modified m.
     MonadIO m
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Key
  -> ( ParamBasicQuorum
     , ParamHead head
     , ParamIfModified if_modified
     , ParamNVal
     , ParamNotfoundOk
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp (Maybe (FetchObjectResp head if_modified)))
fetchObject
    handle@(Handle conn cache) type' bucket key
    ( ParamBasicQuorum basic_quorum
    , head
    , if_modified
    , ParamNVal n_val
    , ParamNotfoundOk notfound_ok
    , ParamPR pr
    , ParamR r
    , ParamSloppyQuorum sloppy_quorum
    , ParamTimeout timeout
    ) = liftIO . runExceptT $ do

  vclock :: Maybe Vclock <-
    case if_modified of
      ParamIfModified   -> lift (vclockCacheLookup cache type' bucket key)
      ParamNoIfModified -> pure Nothing

  let
    request :: RpbGetReq
    request =
      RpbGetReq
        { _RpbGetReq'_unknownFields = []
        , _RpbGetReq'basicQuorum    = Just basic_quorum
        , _RpbGetReq'bucket         = coerce bucket
        , _RpbGetReq'deletedvclock  = Just True
        , _RpbGetReq'head           =
            case head of
              ParamHead   -> Just True
              ParamNoHead -> Just False
        , _RpbGetReq'ifModified     =
            case if_modified of
              ParamIfModified   -> coerce vclock
              ParamNoIfModified -> Nothing
        , _RpbGetReq'key            = coerce key
        , _RpbGetReq'nVal           = n_val
        , _RpbGetReq'notfoundOk     = Just notfound_ok
        , _RpbGetReq'pr             = coerce (Just pr)
        , _RpbGetReq'r              = coerce (Just r)
        , _RpbGetReq'sloppyQuorum   = Just sloppy_quorum
        , _RpbGetReq'timeout        = timeout
        , _RpbGetReq'type'          = coerce (Just type')
        }

  response :: RpbGetResp <-
    ExceptT (Internal.fetchObject conn request)

  -- Only cache the vclock if we didn't received an "unmodified" response (which
  -- doesn't contain a vclock)
  case (if_modified, response ^. #maybe'unchanged) of
    (ParamIfModified, Just True) ->
      pure ()
    _ -> do
      cacheVclock handle type' bucket key (coerce (response ^. #maybe'vclock))

  pure (mkResponse response)

 where
  mkResponse
    :: RpbGetResp
    -> Maybe (FetchObjectResp head if_modified)
  mkResponse (RpbGetResp content _ unchanged _) =
    case if_modified of
      ParamIfModified ->
        case unchanged of
          Just True ->
            Just Unmodified
          _ ->
            Modified <$> contents

      ParamNoIfModified ->
        contents

   where
    contents :: Maybe [Content (If head () ByteString)]
    contents = do
      guard (not (null content))
      pure (map (parseContent headAsBool) content)
     where
      headAsBool :: SBool head
      headAsBool =
        case head of
          ParamHead   -> STrue
          ParamNoHead -> SFalse

data StoreObjectResp (return :: ObjectReturn)
  = StoreObjectResp
      !Key
      !(ObjectReturnTy return)

deriving instance
  ( Show (ObjectReturnTy return)
  ) => Show (StoreObjectResp return)

type family ObjectReturnTy (return :: ObjectReturn) where
  ObjectReturnTy 'ObjectReturnNone = ()
  ObjectReturnTy 'ObjectReturnHead = [Content ()]
  ObjectReturnTy 'ObjectReturnBody = [Content ByteString]

-- TODO storeObject: nicer input type than RpbContent
storeObject
  :: forall m return.
     MonadIO m
  => Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> RpbContent
  -> ( ParamDW
     , ParamNVal
     , ParamPW
     , ParamObjectReturn return
     , ParamSloppyQuorum
     , ParamTimeout
     , ParamW
     )
  -> m (Either RpbErrorResp (StoreObjectResp return))
storeObject handle type' bucket key content params =
  liftIO (runExceptT (storeObject_ handle type' bucket key content params))

storeObject_
  :: forall return.
     Handle
  -> BucketType 'Nothing
  -> Bucket
  -> Maybe Key
  -> RpbContent
  -> ( ParamDW
     , ParamNVal
     , ParamPW
     , ParamObjectReturn return
     , ParamSloppyQuorum
     , ParamTimeout
     , ParamW
     )
  -> ExceptT RpbErrorResp IO (StoreObjectResp return)
storeObject_
    handle@(Handle conn cache) type' bucket key content
    ( ParamDW dw
    , ParamNVal n_val
    , ParamPW pw
    , return
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    , ParamW w
    ) = do

  -- Get the cached vclock of this object to pass in the put request.
  vclock :: Maybe Vclock <-
    maybe
      (pure Nothing) -- Riak will randomly generate a key for us. No vclock.
      (lift . vclockCacheLookup cache type' bucket)
      key

  let
    request :: RpbPutReq
    request =
      RpbPutReq
        { _RpbPutReq'_unknownFields = []
        , _RpbPutReq'asis           = Nothing
        , _RpbPutReq'bucket         = coerce bucket
        , _RpbPutReq'content        = content
        , _RpbPutReq'dw             = Just (coerce dw)
        , _RpbPutReq'ifNoneMatch    = Nothing
        , _RpbPutReq'ifNotModified  = Nothing
        , _RpbPutReq'key            = coerce key
        , _RpbPutReq'nVal           = n_val
        , _RpbPutReq'pw             = Just (coerce pw)
        , _RpbPutReq'returnBody     =
            case return of
              ParamObjectReturnNone -> Nothing
              ParamObjectReturnHead -> Nothing
              ParamObjectReturnBody -> Just True
        , _RpbPutReq'returnHead     =
            case return of
              ParamObjectReturnNone -> Nothing
              ParamObjectReturnHead -> Just True
              ParamObjectReturnBody -> Nothing
        , _RpbPutReq'sloppyQuorum   = Just sloppy_quorum
        , _RpbPutReq'timeout        = coerce timeout
        , _RpbPutReq'type'          = coerce (Just type')
        , _RpbPutReq'vclock         = coerce vclock
        , _RpbPutReq'w              = coerce (Just w)
        }

  response :: RpbPutResp <-
    ExceptT (Internal.storeObject conn request)

  let
    nonsense :: Text -> ExceptT RpbErrorResp IO void
    nonsense s =
      panic s
        ( ( "request",  request  )
        , ( "response", response )
        )

  theKey :: Key <-
    maybe
      (maybe
        (nonsense "missing key")
        pure
        (coerce (response ^. #maybe'key)))
      pure
      key

  -- Cache the vclock if asked for it with return_head or return_body.
  do
    let
      doCacheVclock :: ExceptT RpbErrorResp IO ()
      doCacheVclock =
        case response ^. #maybe'vclock of
          Nothing ->
            nonsense "missing vclock"

          Just theVclock ->
            cacheVclock handle type' bucket theKey (Just (coerce theVclock))

    () <-
      case return of
        ParamObjectReturnNone -> pure ()
        ParamObjectReturnHead -> doCacheVclock
        ParamObjectReturnBody -> doCacheVclock

    pure ()

  pure $ StoreObjectResp
    theKey
    (case return of
      ParamObjectReturnNone -> ()
      ParamObjectReturnHead -> map (parseContent STrue) (response ^. #content)
      ParamObjectReturnBody -> map (parseContent SFalse) (response ^. #content))


-- TODO deleteObject figure out when vclock is required (always?)
deleteObject
  :: MonadIO m
  => Handle
  -> RpbDelReq
  -> m (Either RpbErrorResp RpbDelResp)
deleteObject (Handle conn _) req =
  liftIO (exchange conn req)


fetchCounter
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Key
  -> ( ParamBasicQuorum
     , ParamNotfoundOk
     , ParamNVal
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp Int64)
fetchCounter
    (Handle conn _) type' bucket key
    ( ParamBasicQuorum basic_quorum
    , ParamNotfoundOk notfound_ok
    , ParamNVal n_val
    , ParamPR pr
    , ParamR r
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    ) = runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (liftIO (exchange conn request))

  case response ^. #type' of
    DtFetchResp'COUNTER ->
      pure (response ^. #value . #counterValue)

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt
          DtFetchResp'COUNTER)

 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = Just basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = Nothing
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = Just notfound_ok
      , _DtFetchReq'pr             = coerce (Just pr)
      , _DtFetchReq'r              = coerce (Just r)
      , _DtFetchReq'sloppyQuorum   = Just sloppy_quorum
      , _DtFetchReq'timeout        = coerce timeout
      , _DtFetchReq'type'          = coerce type'
      }


fetchSet
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeSet)
  -> Bucket
  -> Key
  -> ( ParamBasicQuorum
     , ParamIncludeContext
     , ParamNVal
     , ParamNotfoundOk -- TODO invert?
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp [ByteString])
fetchSet
    handle type' bucket key
    params@(_, ParamIncludeContext include_context, _, _, _, _, _, _) = liftIO . runExceptT $ do

  response :: DtFetchResp <-
    ExceptT (fetchDataType handle type' bucket key params)

  case response ^. #type' of
    DtFetchResp'SET -> do
      -- Cache set context, if it was requested (it defaults to true)
      when include_context $
        let
          vclock :: Maybe Vclock
          vclock = coerce (response ^. #maybe'context)
        in
          cacheVclock handle type' bucket key vclock

      pure (response ^. #value . #setValue)

    dt ->
      throwIO
        (DataTypeError
          (SomeBucketType (unBucketType type')) bucket key dt DtFetchResp'SET)


fetchDataType
  :: MonadIO m
  => Handle
  -> BucketType ('Just ty)
  -> Bucket
  -> Key
  -> ( ParamBasicQuorum
     , ParamIncludeContext
     , ParamNVal
     , ParamNotfoundOk
     , ParamPR
     , ParamR
     , ParamSloppyQuorum
     , ParamTimeout
     )
  -> m (Either RpbErrorResp DtFetchResp)
fetchDataType (Handle conn _) type' bucket key
    ( ParamBasicQuorum basic_quorum
    , ParamIncludeContext include_context
    , ParamNVal n_val
    , ParamNotfoundOk notfound_ok
    , ParamPR pr
    , ParamR r
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    ) =

  liftIO (exchange conn request)
 where
  request :: DtFetchReq
  request =
    DtFetchReq
      { _DtFetchReq'_unknownFields = []
      , _DtFetchReq'basicQuorum    = Just basic_quorum
      , _DtFetchReq'bucket         = coerce bucket
      , _DtFetchReq'includeContext = Just include_context
      , _DtFetchReq'key            = coerce key
      , _DtFetchReq'nVal           = n_val
      , _DtFetchReq'notfoundOk     = Just notfound_ok
      , _DtFetchReq'pr             = coerce (Just pr)
      , _DtFetchReq'r              = coerce (Just r)
      , _DtFetchReq'sloppyQuorum   = Just sloppy_quorum
      , _DtFetchReq'timeout        = coerce timeout
      , _DtFetchReq'type'          = coerce type'
      }


-- TODO better updateCounter return type
-- Facts to encode:
--   * If key provided, riak doesn't return key
--   * If key not provided, riak returns random key
--   * If return_body, riak returns counter val
--   * If not return_body, riak doesn't return counter val
updateCounter
  :: MonadIO m
  => Handle
  -> BucketType ('Just 'DataTypeCounter)
  -> Bucket
  -> Maybe Key
  -> Int64
  -> ( ParamDW
     , ParamNVal
     , ParamPW
     , ParamReturnBody
     , ParamSloppyQuorum
     , ParamTimeout
     , ParamW
     )
  -> m (Either RpbErrorResp DtUpdateResp)
updateCounter
    (Handle conn _) type' bucket key incr
    ( ParamDW dw
    , ParamNVal n_val
    , ParamPW pw
    , ParamReturnBody return_body
    , ParamSloppyQuorum sloppy_quorum
    , timeout
    , ParamW w
    ) = do
  liftIO (exchange conn request)
 where
  request :: DtUpdateReq
  request =
    DtUpdateReq
      { _DtUpdateReq'_unknownFields = []
      , _DtUpdateReq'bucket         = coerce bucket
      , _DtUpdateReq'context        = Nothing
      , _DtUpdateReq'dw             = coerce (Just dw)
      , _DtUpdateReq'includeContext = Nothing
      , _DtUpdateReq'key            = coerce key
      , _DtUpdateReq'nVal           = n_val
      , _DtUpdateReq'op             = op
      , _DtUpdateReq'pw             = coerce (Just pw)
      , _DtUpdateReq'returnBody     = Just return_body
      , _DtUpdateReq'sloppyQuorum   = Just sloppy_quorum
      , _DtUpdateReq'timeout        = coerce timeout
      , _DtUpdateReq'type'          = coerce type'
      , _DtUpdateReq'w              = coerce (Just w)
      }

  op :: DtOp
  op =
    DtOp
      { _DtOp'_unknownFields = []
      , _DtOp'counterOp      = Just (CounterOp (Just incr) [])
      , _DtOp'gsetOp         = Nothing
      , _DtOp'hllOp          = Nothing
      , _DtOp'mapOp          = Nothing
      , _DtOp'setOp          = Nothing
      }


updateDataType
  :: MonadIO m
  => Handle
  -> DtUpdateReq
  -> m (Either RpbErrorResp DtUpdateResp)
updateDataType (Handle conn _) req =
  liftIO (exchange conn req)


getBucketTypeProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketTypeReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketTypeProps (Handle conn _) req =
  liftIO (exchange conn req)


setBucketTypeProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketTypeReq
  -> m (Either RpbErrorResp ())
setBucketTypeProps (Handle conn _) req =
  liftIO (emptyResponse @RpbSetBucketTypeResp (exchange conn req))


getBucketProps
  :: MonadIO m
  => Handle
  -> RpbGetBucketReq
  -> m (Either RpbErrorResp RpbGetBucketResp)
getBucketProps (Handle conn _) req =
  liftIO (exchange conn req)


setBucketProps
  :: MonadIO m
  => Handle
  -> RpbSetBucketReq
  -> m (Either RpbErrorResp ())
setBucketProps (Handle conn _) req =
  liftIO (emptyResponse @RpbSetBucketResp (exchange conn req))


resetBucketProps
  :: MonadIO m
  => Handle
  -> RpbResetBucketReq
  -> m (Either RpbErrorResp ())
resetBucketProps (Handle conn _) req =
  liftIO (emptyResponse @RpbResetBucketResp (exchange conn req))


-- TODO listBuckets param timeout
listBuckets
  :: MonadIO m
  => Handle
  -> BucketType ty
  -> ListT (ExceptT RpbErrorResp m) Bucket
listBuckets (Handle conn _) type' = do
  liftIO (send conn request)

  fix $ \loop -> do
    resp :: RpbListBucketsResp <-
      lift (ExceptT (liftIO (recv conn >>= parseResponse)))

    let
      buckets :: [Bucket]
      buckets =
        coerce (resp ^. #buckets)

    ListT.select buckets <|>
      if resp ^. #done
        then empty
        else loop
 where
  request :: RpbListBucketsReq
  request =
    RpbListBucketsReq
      { _RpbListBucketsReq'_unknownFields = []
      , _RpbListBucketsReq'stream = Just True
      , _RpbListBucketsReq'timeout = Nothing
      , _RpbListBucketsReq'type' = coerce (Just type')
      }

-- TODO listKeys param timeout
listKeys
  :: forall m ty.
     MonadIO m
  => Handle
  -> BucketType ty
  -> Bucket
  -> ListT (ExceptT RpbErrorResp m) Key
listKeys (Handle conn _) type' bucket = do
  liftIO (send conn request)

  fix $ \loop -> do
    resp :: RpbListKeysResp <-
      lift (ExceptT (liftIO (recv conn >>= parseResponse)))

    let
      keys :: [Key]
      keys =
        coerce (resp ^. #keys)

    ListT.select keys <|>
      if resp ^. #done
        then empty
        else loop
 where
  request :: RpbListKeysReq
  request =
    RpbListKeysReq
      { _RpbListKeysReq'_unknownFields = []
      , _RpbListKeysReq'bucket         = coerce bucket
      , _RpbListKeysReq'timeout        = Nothing
      , _RpbListKeysReq'type'          = coerce (Just type')
      }

mapReduce
  :: MonadIO m
  => Handle
  -> RpbMapRedReq
  -> m (Either RpbErrorResp [RpbMapRedResp])
mapReduce (Handle conn _) req = liftIO $ do
  send conn req

  let
    loop :: ExceptT RpbErrorResp IO [RpbMapRedResp]
    loop = do
      resp :: RpbMapRedResp <-
        ExceptT (recv conn >>= parseResponse)

      if resp ^. #done
        then pure [resp]
        else (resp :) <$> loop

  runExceptT loop


getSchema
  :: MonadIO m
  => Handle
  -> RpbYokozunaSchemaGetReq
  -> m (Either RpbErrorResp RpbYokozunaSchemaGetResp)
getSchema (Handle conn _) req =
  liftIO (exchange conn req)


putSchema
  :: MonadIO m
  => Handle
  -> RpbYokozunaSchemaPutReq
  -> m (Either RpbErrorResp RpbEmptyPutResp)
putSchema (Handle conn _) req =
  liftIO (exchange conn req)


getIndex
  :: MonadIO m
  => Handle
  -> RpbYokozunaIndexGetReq
  -> m (Either RpbErrorResp RpbYokozunaIndexGetResp)
getIndex (Handle conn _) req =
  liftIO (exchange conn req)


putIndex
  :: MonadIO m
  => Handle
  -> RpbYokozunaIndexPutReq
  -> m (Either RpbErrorResp RpbEmptyPutResp)
putIndex (Handle conn _) req =
  liftIO (exchange conn req)


deleteIndex
  :: MonadIO m
  => Handle
  -> RpbYokozunaIndexDeleteReq
  -> m (Either RpbErrorResp RpbDelResp)
deleteIndex (Handle conn _) req =
  liftIO (exchange conn req)


ping :: MonadIO m => Handle -> m (Either RpbErrorResp ())
ping (Handle conn _) =
  liftIO (emptyResponse @RpbPingResp (exchange conn RpbPingReq))


getServerInfo
  :: MonadIO m
  => Handle
  -> m (Either RpbErrorResp RpbGetServerInfoResp)
getServerInfo (Handle conn _) =
  liftIO (exchange conn RpbGetServerInfoReq)


--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

-- | Given a fetched vclock, update the cache (if present) or delete it from the
-- cache (if missing).
cacheVclock
  :: MonadIO m
  => Handle
  -> BucketType ty
  -> Bucket
  -> Key
  -> Maybe Vclock
  -> m ()
cacheVclock (Handle _ cache) type' bucket key = liftIO .
  maybe
    (vclockCacheDelete cache type' bucket key)
    (vclockCacheInsert cache type' bucket key)


parseContent :: SBool head -> RpbContent -> Content (If head () ByteString)
parseContent head
    (RpbContent value content_type charset content_encoding vtag _ last_mod
                last_mod_usecs usermeta indexes deleted ttl _) =
  Content
    (case head of
      STrue  -> ()
      SFalse -> value)
    (coerce content_type)
    charset
    content_encoding
    (coerce vtag)
    last_mod
    last_mod_usecs
    (map unRpbPair usermeta)
    (map unRpbPair indexes)
    deleted
    ttl


emptyResponse :: IO (Either RpbErrorResp a) -> IO (Either RpbErrorResp ())
emptyResponse =
  fmap (() <$)


unRpbPair :: RpbPair -> (ByteString, Maybe ByteString)
unRpbPair (RpbPair k v _) =
  (k, v)
