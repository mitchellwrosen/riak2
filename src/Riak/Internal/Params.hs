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
newtype DF             = DF             { unDF             :: Maybe ByteString }
newtype DW             = DW             { unDW             :: Maybe RiakQuorum }
newtype Filter         = Filter         { unFilter         :: Maybe ByteString }
newtype FL             = FL             { unFL             :: [ByteString]     }
newtype N              = N              { unN              :: Maybe Word32     }
newtype NotfoundOk     = NotfoundOk     { unNotfoundOk     :: Maybe Bool       }
newtype Op             = Op             { unOp             :: Maybe ByteString }
newtype PR             = PR             { unPR             :: Maybe RiakQuorum }
newtype Presort        = Presort        { unPresort        :: Maybe ByteString }
newtype PW             = PW             { unPW             :: Maybe RiakQuorum }
newtype R              = R              { unR              :: Maybe RiakQuorum }
newtype ReturnBody     = ReturnBody     { unReturnBody     :: Maybe Bool       }
newtype Rows           = Rows           { unRows           :: Maybe Word32     }
newtype SloppyQuorum   = SloppyQuorum   { unSloppyQuorum   :: Maybe Bool       }
newtype Sort           = Sort           { unSort           :: Maybe ByteString }
newtype Start          = Start          { unStart          :: Maybe Word32     }
newtype Timeout        = Timeout        { unTimeout        :: Maybe Word32     }
newtype W              = W              { unW              :: Maybe RiakQuorum }


-- | Optional @get data type@ parameters.
data GetRiakCrdtParams
  = GetRiakCrdtParams
      !BasicQuorum
      !IncludeContext
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default GetRiakCrdtParams where
  def = GetRiakCrdtParams (BasicQuorum Nothing) (IncludeContext Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"    (Bool       -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \a (GetRiakCrdtParams _ b c d e f g h) -> GetRiakCrdtParams (coerce (Just a)) b c d e f g h
instance IsLabel "include_context" (Bool       -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \b (GetRiakCrdtParams a _ c d e f g h) -> GetRiakCrdtParams a (coerce (Just b)) c d e f g h
instance IsLabel "n"               (Word32     -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \c (GetRiakCrdtParams a b _ d e f g h) -> GetRiakCrdtParams a b (coerce (Just c)) d e f g h
instance IsLabel "notfound_ok"     (Bool       -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \d (GetRiakCrdtParams a b c _ e f g h) -> GetRiakCrdtParams a b c (coerce (Just d)) e f g h
instance IsLabel "pr"              (RiakQuorum -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \e (GetRiakCrdtParams a b c d _ f g h) -> GetRiakCrdtParams a b c d (coerce (Just e)) f g h
instance IsLabel "r"               (RiakQuorum -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \f (GetRiakCrdtParams a b c d e _ g h) -> GetRiakCrdtParams a b c d e (coerce (Just f)) g h
instance IsLabel "sloppy_quorum"   (Bool       -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \g (GetRiakCrdtParams a b c d e f _ h) -> GetRiakCrdtParams a b c d e f (coerce (Just g)) h
instance IsLabel "timeout"         (Word32     -> GetRiakCrdtParams -> GetRiakCrdtParams) where fromLabel = \h (GetRiakCrdtParams a b c d e f g _) -> GetRiakCrdtParams a b c d e f g (coerce (Just h))


-- | Optional @get object@ parameters.
data GetRiakObjectParams
  = GetRiakObjectParams
      !BasicQuorum
      !N
      !NotfoundOk
      !PR
      !R
      !SloppyQuorum
      !Timeout

instance Default GetRiakObjectParams where
  def = GetRiakObjectParams (BasicQuorum Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

instance IsLabel "basic_quorum"  (Bool       -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \a (GetRiakObjectParams _ b c d e f g) -> GetRiakObjectParams (coerce (Just a)) b c d e f g
instance IsLabel "n"             (Word32     -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \b (GetRiakObjectParams a _ c d e f g) -> GetRiakObjectParams a (coerce (Just b)) c d e f g
instance IsLabel "notfound_ok"   (Bool       -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \c (GetRiakObjectParams a b _ d e f g) -> GetRiakObjectParams a b (coerce (Just c)) d e f g
instance IsLabel "pr"            (RiakQuorum -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \d (GetRiakObjectParams a b c _ e f g) -> GetRiakObjectParams a b c (coerce (Just d)) e f g
instance IsLabel "r"             (RiakQuorum -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \e (GetRiakObjectParams a b c d _ f g) -> GetRiakObjectParams a b c d (coerce (Just e)) f g
instance IsLabel "sloppy_quorum" (Bool       -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \f (GetRiakObjectParams a b c d e _ g) -> GetRiakObjectParams a b c d e (coerce (Just f)) g
instance IsLabel "timeout"       (Word32     -> GetRiakObjectParams -> GetRiakObjectParams) where fromLabel = \g (GetRiakObjectParams a b c d e f _) -> GetRiakObjectParams a b c d e f (coerce (Just g))


-- | Optional @store object@ parameters.
data PutRiakObjectParams
  = PutRiakObjectParams
      !DW
      ![RiakSecondaryIndex]
      !RiakMetadata
      !N
      !PW
      !SloppyQuorum
      !Timeout
      !W

instance Default PutRiakObjectParams where
  def = PutRiakObjectParams (DW Nothing) [] (RiakMetadata []) (N Nothing) (PW Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

instance IsLabel "dw"            (RiakQuorum                       -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \a (PutRiakObjectParams _ b c d e f g h) -> PutRiakObjectParams (coerce (Just a)) b c d e f g h
instance IsLabel "indexes"       ([RiakSecondaryIndex]             -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \b (PutRiakObjectParams a _ c d e f g h) -> PutRiakObjectParams a b c d e f g h
instance IsLabel "metadata"      ([(ByteString, Maybe ByteString)] -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \c (PutRiakObjectParams a b _ d e f g h) -> PutRiakObjectParams a b (RiakMetadata c) d e f g h
instance IsLabel "n"             (Word32                           -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \d (PutRiakObjectParams a b c _ e f g h) -> PutRiakObjectParams a b c (coerce (Just d)) e f g h
instance IsLabel "pw"            (RiakQuorum                       -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \e (PutRiakObjectParams a b c d _ f g h) -> PutRiakObjectParams a b c d (coerce (Just e)) f g h
instance IsLabel "sloppy_quorum" (Bool                             -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \f (PutRiakObjectParams a b c d e _ g h) -> PutRiakObjectParams a b c d e (coerce (Just f)) g h
instance IsLabel "timeout"       (Word32                           -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \g (PutRiakObjectParams a b c d e f _ h) -> PutRiakObjectParams a b c d e f (coerce (Just g)) h
instance IsLabel "w"             (RiakQuorum                       -> PutRiakObjectParams -> PutRiakObjectParams) where fromLabel = \h (PutRiakObjectParams a b c d e f g _) -> PutRiakObjectParams a b c d e f g (coerce (Just h))


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


-- | Optional @search@ parameters.
data RiakSearchParams
  = RiakSearchParams
      !DF
      !Filter
      !FL
      !Op
      !Presort
      !Rows
      !Sort
      !Start

instance Default RiakSearchParams where
  def = RiakSearchParams (DF Nothing) (Filter Nothing) (FL []) (Op Nothing) (Presort Nothing) (Rows Nothing) (Sort Nothing) (Start Nothing)

instance IsLabel "df"      (ByteString   -> RiakSearchParams -> RiakSearchParams) where fromLabel = \a (RiakSearchParams _ b c d e f g h) -> RiakSearchParams (coerce (Just a)) b c d e f g h
instance IsLabel "filter"  (ByteString   -> RiakSearchParams -> RiakSearchParams) where fromLabel = \b (RiakSearchParams a _ c d e f g h) -> RiakSearchParams a (coerce (Just b)) c d e f g h
instance IsLabel "fl"      ([ByteString] -> RiakSearchParams -> RiakSearchParams) where fromLabel = \c (RiakSearchParams a b _ d e f g h) -> RiakSearchParams a b (coerce c) d e f g h
instance IsLabel "op"      (ByteString   -> RiakSearchParams -> RiakSearchParams) where fromLabel = \d (RiakSearchParams a b c _ e f g h) -> RiakSearchParams a b c (coerce (Just d)) e f g h
instance IsLabel "presort" (ByteString   -> RiakSearchParams -> RiakSearchParams) where fromLabel = \e (RiakSearchParams a b c d _ f g h) -> RiakSearchParams a b c d (coerce (Just e)) f g h
instance IsLabel "rows"    (Word32       -> RiakSearchParams -> RiakSearchParams) where fromLabel = \f (RiakSearchParams a b c d e _ g h) -> RiakSearchParams a b c d e (coerce (Just f)) g h
instance IsLabel "sort"    (ByteString   -> RiakSearchParams -> RiakSearchParams) where fromLabel = \g (RiakSearchParams a b c d e f _ h) -> RiakSearchParams a b c d e f (coerce (Just g)) h
instance IsLabel "start"   (Word32       -> RiakSearchParams -> RiakSearchParams) where fromLabel = \h (RiakSearchParams a b c d e f g _) -> RiakSearchParams a b c d e f g (coerce (Just h))
