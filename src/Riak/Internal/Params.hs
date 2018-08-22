{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}

module Riak.Internal.Params where

import Data.ByteString      (ByteString)
import Data.Coerce          (coerce)
import Data.Default.Class
import Data.Word
import GHC.OverloadedLabels

import Riak.Internal.Types


-- TODO default these that can't be set in props?

newtype BasicQuorum    = BasicQuorum    { unBasicQuorum    :: Maybe Bool   }
newtype IncludeContext = IncludeContext { unIncludeContext :: Maybe Bool   }
newtype DW             = DW             { unDW             :: Maybe Quorum }
newtype N              = N              { unN              :: Maybe Word32 }
newtype NotfoundOk     = NotfoundOk     { unNotfoundOk     :: Maybe Bool   }
newtype PR             = PR             { unPR             :: Maybe Quorum }
newtype PW             = PW             { unPW             :: Maybe Quorum }
newtype R              = R              { unR              :: Maybe Quorum }
newtype ReturnBody     = ReturnBody     { unReturnBody     :: Maybe Bool   }
newtype SloppyQuorum   = SloppyQuorum   { unSloppyQuorum   :: Maybe Bool   }
newtype Timeout        = Timeout        { unTimeout        :: Maybe Word32 }
newtype W              = W              { unW              :: Maybe Quorum }


data FetchDataTypeParams
  = FetchDataTypeParams
      !BasicQuorum
      !IncludeContext
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default FetchDataTypeParams where
  def = FetchDataTypeParams (BasicQuorum Nothing) (IncludeContext Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"    (Bool   -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \a (FetchDataTypeParams _ b c d e f g h) -> FetchDataTypeParams (coerce (Just a)) b c d e f g h
instance IsLabel "include_context" (Bool   -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \b (FetchDataTypeParams a _ c d e f g h) -> FetchDataTypeParams a (coerce (Just b)) c d e f g h
instance IsLabel "n"               (Word32 -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \c (FetchDataTypeParams a b _ d e f g h) -> FetchDataTypeParams a b (coerce (Just c)) d e f g h
instance IsLabel "notfound_ok"     (Bool   -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \d (FetchDataTypeParams a b c _ e f g h) -> FetchDataTypeParams a b c (coerce (Just d)) e f g h
instance IsLabel "pr"              (Quorum -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \e (FetchDataTypeParams a b c d _ f g h) -> FetchDataTypeParams a b c d (coerce (Just e)) f g h
instance IsLabel "r"               (Quorum -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \f (FetchDataTypeParams a b c d e _ g h) -> FetchDataTypeParams a b c d e (coerce (Just f)) g h
instance IsLabel "sloppy_quorum"   (Bool   -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \g (FetchDataTypeParams a b c d e f _ h) -> FetchDataTypeParams a b c d e f (coerce (Just g)) h
instance IsLabel "timeout"         (Word32 -> FetchDataTypeParams -> FetchDataTypeParams) where fromLabel = \h (FetchDataTypeParams a b c d e f g _) -> FetchDataTypeParams a b c d e f g (coerce (Just h))


data FetchObjectParams
  = FetchObjectParams
      !BasicQuorum
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default FetchObjectParams where
  def = FetchObjectParams (BasicQuorum Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"  (Bool   -> FetchObjectParams -> FetchObjectParams) where fromLabel = \a (FetchObjectParams _ b c d e f g) -> FetchObjectParams (coerce (Just a)) b c d e f g
instance IsLabel "n"             (Word32 -> FetchObjectParams -> FetchObjectParams) where fromLabel = \b (FetchObjectParams a _ c d e f g) -> FetchObjectParams a (coerce (Just b)) c d e f g
instance IsLabel "notfound_ok"   (Bool   -> FetchObjectParams -> FetchObjectParams) where fromLabel = \c (FetchObjectParams a b _ d e f g) -> FetchObjectParams a b (coerce (Just c)) d e f g
instance IsLabel "pr"            (Quorum -> FetchObjectParams -> FetchObjectParams) where fromLabel = \d (FetchObjectParams a b c _ e f g) -> FetchObjectParams a b c (coerce (Just d)) e f g
instance IsLabel "r"             (Quorum -> FetchObjectParams -> FetchObjectParams) where fromLabel = \e (FetchObjectParams a b c d _ f g) -> FetchObjectParams a b c d (coerce (Just e)) f g
instance IsLabel "sloppy_quorum" (Bool   -> FetchObjectParams -> FetchObjectParams) where fromLabel = \f (FetchObjectParams a b c d e _ g) -> FetchObjectParams a b c d e (coerce (Just f)) g
instance IsLabel "timeout"       (Word32 -> FetchObjectParams -> FetchObjectParams) where fromLabel = \g (FetchObjectParams a b c d e f _) -> FetchObjectParams a b c d e f (coerce (Just g))


data StoreObjectParams
  = StoreObjectParams
      !DW
      ![SecondaryIndex]
      !Metadata
      !N
      !PW
      !SloppyQuorum
      !Timeout
      !TTL
      !W

instance Default StoreObjectParams where
  def = StoreObjectParams (DW Nothing) [] (Metadata []) (N Nothing) (PW Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (TTL Nothing) (W Nothing)

instance IsLabel "dw"            (Quorum                           -> StoreObjectParams -> StoreObjectParams) where fromLabel = \a (StoreObjectParams _ b c d e f g h i) -> StoreObjectParams (coerce (Just a)) b c d e f g h i
instance IsLabel "indexes"       ([SecondaryIndex]                 -> StoreObjectParams -> StoreObjectParams) where fromLabel = \b (StoreObjectParams a _ c d e f g h i) -> StoreObjectParams a b c d e f g h i
instance IsLabel "metadata"      ([(ByteString, Maybe ByteString)] -> StoreObjectParams -> StoreObjectParams) where fromLabel = \c (StoreObjectParams a b _ d e f g h i) -> StoreObjectParams a b (Metadata c) d e f g h i
instance IsLabel "n"             (Word32                           -> StoreObjectParams -> StoreObjectParams) where fromLabel = \d (StoreObjectParams a b c _ e f g h i) -> StoreObjectParams a b c (coerce (Just d)) e f g h i
instance IsLabel "pw"            (Quorum                           -> StoreObjectParams -> StoreObjectParams) where fromLabel = \e (StoreObjectParams a b c d _ f g h i) -> StoreObjectParams a b c d (coerce (Just e)) f g h i
instance IsLabel "sloppy_quorum" (Bool                             -> StoreObjectParams -> StoreObjectParams) where fromLabel = \f (StoreObjectParams a b c d e _ g h i) -> StoreObjectParams a b c d e (coerce (Just f)) g h i
instance IsLabel "timeout"       (Word32                           -> StoreObjectParams -> StoreObjectParams) where fromLabel = \g (StoreObjectParams a b c d e f _ h i) -> StoreObjectParams a b c d e f (coerce (Just g)) h i
instance IsLabel "ttl"           (Word32                           -> StoreObjectParams -> StoreObjectParams) where fromLabel = \h (StoreObjectParams a b c d e f g _ i) -> StoreObjectParams a b c d e f g (coerce (Just h)) i
instance IsLabel "w"             (Quorum                           -> StoreObjectParams -> StoreObjectParams) where fromLabel = \i (StoreObjectParams a b c d e f g h _) -> StoreObjectParams a b c d e f g h (coerce (Just i))

data UpdateDataTypeParams
  = UpdateDataTypeParams
     !DW
     !N
     !PW
     !ReturnBody
     !SloppyQuorum
     !Timeout
     !W

instance Default UpdateDataTypeParams where
  def = UpdateDataTypeParams (DW Nothing) (N Nothing) (PW Nothing) (ReturnBody Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

instance IsLabel "dw"            (Quorum -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \a (UpdateDataTypeParams _ b c d e f g) -> UpdateDataTypeParams (coerce (Just a)) b c d e f g
instance IsLabel "n"             (Word32 -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \b (UpdateDataTypeParams a _ c d e f g) -> UpdateDataTypeParams a (coerce (Just b)) c d e f g
instance IsLabel "pw"            (Quorum -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \c (UpdateDataTypeParams a b _ d e f g) -> UpdateDataTypeParams a b (coerce (Just c)) d e f g
instance IsLabel "return_body"   (Bool   -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \d (UpdateDataTypeParams a b c _ e f g) -> UpdateDataTypeParams a b c (coerce (Just d)) e f g
instance IsLabel "sloppy_quorum" (Bool   -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \e (UpdateDataTypeParams a b c d _ f g) -> UpdateDataTypeParams a b c d (coerce (Just e)) f g
instance IsLabel "timeout"       (Word32 -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \f (UpdateDataTypeParams a b c d e _ g) -> UpdateDataTypeParams a b c d e (coerce (Just f)) g
instance IsLabel "w"             (Quorum -> UpdateDataTypeParams -> UpdateDataTypeParams) where fromLabel = \g (UpdateDataTypeParams a b c d e f _) -> UpdateDataTypeParams a b c d e f (coerce (Just g))
