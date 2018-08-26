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
data FetchRiakCrdtParams
  = FetchRiakCrdtParams
      !BasicQuorum
      !IncludeContext
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default FetchRiakCrdtParams where
  def = FetchRiakCrdtParams (BasicQuorum Nothing) (IncludeContext Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"    (Bool       -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \a (FetchRiakCrdtParams _ b c d e f g h) -> FetchRiakCrdtParams (coerce (Just a)) b c d e f g h
instance IsLabel "include_context" (Bool       -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \b (FetchRiakCrdtParams a _ c d e f g h) -> FetchRiakCrdtParams a (coerce (Just b)) c d e f g h
instance IsLabel "n"               (Word32     -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \c (FetchRiakCrdtParams a b _ d e f g h) -> FetchRiakCrdtParams a b (coerce (Just c)) d e f g h
instance IsLabel "notfound_ok"     (Bool       -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \d (FetchRiakCrdtParams a b c _ e f g h) -> FetchRiakCrdtParams a b c (coerce (Just d)) e f g h
instance IsLabel "pr"              (RiakQuorum -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \e (FetchRiakCrdtParams a b c d _ f g h) -> FetchRiakCrdtParams a b c d (coerce (Just e)) f g h
instance IsLabel "r"               (RiakQuorum -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \f (FetchRiakCrdtParams a b c d e _ g h) -> FetchRiakCrdtParams a b c d e (coerce (Just f)) g h
instance IsLabel "sloppy_quorum"   (Bool       -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \g (FetchRiakCrdtParams a b c d e f _ h) -> FetchRiakCrdtParams a b c d e f (coerce (Just g)) h
instance IsLabel "timeout"         (Word32     -> FetchRiakCrdtParams -> FetchRiakCrdtParams) where fromLabel = \h (FetchRiakCrdtParams a b c d e f g _) -> FetchRiakCrdtParams a b c d e f g (coerce (Just h))


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
data UpdateRiakCrdtParams
  = UpdateRiakCrdtParams
     !DW
     !N
     !PW
     !ReturnBody
     !SloppyQuorum
     !Timeout
     !W

instance Default UpdateRiakCrdtParams where
  def = UpdateRiakCrdtParams (DW Nothing) (N Nothing) (PW Nothing) (ReturnBody Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

instance IsLabel "dw"            (RiakQuorum -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \a (UpdateRiakCrdtParams _ b c d e f g) -> UpdateRiakCrdtParams (coerce (Just a)) b c d e f g
instance IsLabel "n"             (Word32     -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \b (UpdateRiakCrdtParams a _ c d e f g) -> UpdateRiakCrdtParams a (coerce (Just b)) c d e f g
instance IsLabel "pw"            (RiakQuorum -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \c (UpdateRiakCrdtParams a b _ d e f g) -> UpdateRiakCrdtParams a b (coerce (Just c)) d e f g
instance IsLabel "return_body"   (Bool       -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \d (UpdateRiakCrdtParams a b c _ e f g) -> UpdateRiakCrdtParams a b c (coerce (Just d)) e f g
instance IsLabel "sloppy_quorum" (Bool       -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \e (UpdateRiakCrdtParams a b c d _ f g) -> UpdateRiakCrdtParams a b c d (coerce (Just e)) f g
instance IsLabel "timeout"       (Word32     -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \f (UpdateRiakCrdtParams a b c d e _ g) -> UpdateRiakCrdtParams a b c d e (coerce (Just f)) g
instance IsLabel "w"             (RiakQuorum -> UpdateRiakCrdtParams -> UpdateRiakCrdtParams) where fromLabel = \g (UpdateRiakCrdtParams a b c d e f _) -> UpdateRiakCrdtParams a b c d e f (coerce (Just g))
