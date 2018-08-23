{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses,
             NoImplicitPrelude #-}

module Riak.Internal.Params where

import Data.Default.Class
import GHC.OverloadedLabels

import Riak.Internal.Prelude
import Riak.Internal.Types


-- TODO default these that can't be set in props?

newtype BasicQuorum    = BasicQuorum    { unBasicQuorum    :: Maybe Bool       }
newtype IncludeContext = IncludeContext { unIncludeContext :: Maybe Bool       }
newtype DW             = DW             { unDW             :: Maybe RiakQuorum }
newtype N              = N              { unN              :: Maybe Word32     }
newtype NotfoundOk     = NotfoundOk     { unNotfoundOk     :: Maybe Bool       }
newtype PR             = PR             { unPR             :: Maybe RiakQuorum }
newtype PW             = PW             { unPW             :: Maybe RiakQuorum }
newtype R              = R              { unR              :: Maybe RiakQuorum }
newtype ReturnBody     = ReturnBody     { unReturnBody     :: Maybe Bool       }
newtype SloppyQuorum   = SloppyQuorum   { unSloppyQuorum   :: Maybe Bool       }
newtype Timeout        = Timeout        { unTimeout        :: Maybe Word32     }
newtype W              = W              { unW              :: Maybe RiakQuorum }


-- | Optional @fetch data type@ parameters.
data FetchRiakDataTypeParams
  = FetchRiakDataTypeParams
      !BasicQuorum
      !IncludeContext
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default FetchRiakDataTypeParams where
  def = FetchRiakDataTypeParams (BasicQuorum Nothing) (IncludeContext Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"    (Bool       -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \a (FetchRiakDataTypeParams _ b c d e f g h) -> FetchRiakDataTypeParams (coerce (Just a)) b c d e f g h
instance IsLabel "include_context" (Bool       -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \b (FetchRiakDataTypeParams a _ c d e f g h) -> FetchRiakDataTypeParams a (coerce (Just b)) c d e f g h
instance IsLabel "n"               (Word32     -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \c (FetchRiakDataTypeParams a b _ d e f g h) -> FetchRiakDataTypeParams a b (coerce (Just c)) d e f g h
instance IsLabel "notfound_ok"     (Bool       -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \d (FetchRiakDataTypeParams a b c _ e f g h) -> FetchRiakDataTypeParams a b c (coerce (Just d)) e f g h
instance IsLabel "pr"              (RiakQuorum -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \e (FetchRiakDataTypeParams a b c d _ f g h) -> FetchRiakDataTypeParams a b c d (coerce (Just e)) f g h
instance IsLabel "r"               (RiakQuorum -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \f (FetchRiakDataTypeParams a b c d e _ g h) -> FetchRiakDataTypeParams a b c d e (coerce (Just f)) g h
instance IsLabel "sloppy_quorum"   (Bool       -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \g (FetchRiakDataTypeParams a b c d e f _ h) -> FetchRiakDataTypeParams a b c d e f (coerce (Just g)) h
instance IsLabel "timeout"         (Word32     -> FetchRiakDataTypeParams -> FetchRiakDataTypeParams) where fromLabel = \h (FetchRiakDataTypeParams a b c d e f g _) -> FetchRiakDataTypeParams a b c d e f g (coerce (Just h))


-- | Optional @fetch object@ parameters.
data FetchRiakObjectParams
  = FetchRiakObjectParams
      !BasicQuorum
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default FetchRiakObjectParams where
  def = FetchRiakObjectParams (BasicQuorum Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"  (Bool       -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \a (FetchRiakObjectParams _ b c d e f g) -> FetchRiakObjectParams (coerce (Just a)) b c d e f g
instance IsLabel "n"             (Word32     -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \b (FetchRiakObjectParams a _ c d e f g) -> FetchRiakObjectParams a (coerce (Just b)) c d e f g
instance IsLabel "notfound_ok"   (Bool       -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \c (FetchRiakObjectParams a b _ d e f g) -> FetchRiakObjectParams a b (coerce (Just c)) d e f g
instance IsLabel "pr"            (RiakQuorum -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \d (FetchRiakObjectParams a b c _ e f g) -> FetchRiakObjectParams a b c (coerce (Just d)) e f g
instance IsLabel "r"             (RiakQuorum -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \e (FetchRiakObjectParams a b c d _ f g) -> FetchRiakObjectParams a b c d (coerce (Just e)) f g
instance IsLabel "sloppy_quorum" (Bool       -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \f (FetchRiakObjectParams a b c d e _ g) -> FetchRiakObjectParams a b c d e (coerce (Just f)) g
instance IsLabel "timeout"       (Word32     -> FetchRiakObjectParams -> FetchRiakObjectParams) where fromLabel = \g (FetchRiakObjectParams a b c d e f _) -> FetchRiakObjectParams a b c d e f (coerce (Just g))


-- | Optional @store object@ parameters.
data StoreRiakObjectParams
  = StoreRiakObjectParams
      !DW
      ![RiakSecondaryIndex]
      !RiakMetadata
      !N
      !PW
      !SloppyQuorum
      !Timeout
      !W

instance Default StoreRiakObjectParams where
  def = StoreRiakObjectParams (DW Nothing) [] (RiakMetadata []) (N Nothing) (PW Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

instance IsLabel "dw"            (RiakQuorum                       -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \a (StoreRiakObjectParams _ b c d e f g h) -> StoreRiakObjectParams (coerce (Just a)) b c d e f g h
instance IsLabel "indexes"       ([RiakSecondaryIndex]             -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \b (StoreRiakObjectParams a _ c d e f g h) -> StoreRiakObjectParams a b c d e f g h
instance IsLabel "metadata"      ([(ByteString, Maybe ByteString)] -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \c (StoreRiakObjectParams a b _ d e f g h) -> StoreRiakObjectParams a b (RiakMetadata c) d e f g h
instance IsLabel "n"             (Word32                           -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \d (StoreRiakObjectParams a b c _ e f g h) -> StoreRiakObjectParams a b c (coerce (Just d)) e f g h
instance IsLabel "pw"            (RiakQuorum                       -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \e (StoreRiakObjectParams a b c d _ f g h) -> StoreRiakObjectParams a b c d (coerce (Just e)) f g h
instance IsLabel "sloppy_quorum" (Bool                             -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \f (StoreRiakObjectParams a b c d e _ g h) -> StoreRiakObjectParams a b c d e (coerce (Just f)) g h
instance IsLabel "timeout"       (Word32                           -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \g (StoreRiakObjectParams a b c d e f _ h) -> StoreRiakObjectParams a b c d e f (coerce (Just g)) h
instance IsLabel "w"             (RiakQuorum                       -> StoreRiakObjectParams -> StoreRiakObjectParams) where fromLabel = \h (StoreRiakObjectParams a b c d e f g _) -> StoreRiakObjectParams a b c d e f g (coerce (Just h))


-- | Optional @update data type@ parameters.
data UpdateRiakDataTypeParams
  = UpdateRiakDataTypeParams
     !DW
     !N
     !PW
     !ReturnBody
     !SloppyQuorum
     !Timeout
     !W

instance Default UpdateRiakDataTypeParams where
  def = UpdateRiakDataTypeParams (DW Nothing) (N Nothing) (PW Nothing) (ReturnBody Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

instance IsLabel "dw"            (RiakQuorum -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \a (UpdateRiakDataTypeParams _ b c d e f g) -> UpdateRiakDataTypeParams (coerce (Just a)) b c d e f g
instance IsLabel "n"             (Word32     -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \b (UpdateRiakDataTypeParams a _ c d e f g) -> UpdateRiakDataTypeParams a (coerce (Just b)) c d e f g
instance IsLabel "pw"            (RiakQuorum -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \c (UpdateRiakDataTypeParams a b _ d e f g) -> UpdateRiakDataTypeParams a b (coerce (Just c)) d e f g
instance IsLabel "return_body"   (Bool       -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \d (UpdateRiakDataTypeParams a b c _ e f g) -> UpdateRiakDataTypeParams a b c (coerce (Just d)) e f g
instance IsLabel "sloppy_quorum" (Bool       -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \e (UpdateRiakDataTypeParams a b c d _ f g) -> UpdateRiakDataTypeParams a b c d (coerce (Just e)) f g
instance IsLabel "timeout"       (Word32     -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \f (UpdateRiakDataTypeParams a b c d e _ g) -> UpdateRiakDataTypeParams a b c d e (coerce (Just f)) g
instance IsLabel "w"             (RiakQuorum -> UpdateRiakDataTypeParams -> UpdateRiakDataTypeParams) where fromLabel = \g (UpdateRiakDataTypeParams a b c d e f _) -> UpdateRiakDataTypeParams a b c d e f (coerce (Just g))
