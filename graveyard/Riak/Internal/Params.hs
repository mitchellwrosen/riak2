module ZZZ.Riak.Internal.Params where

-- import Data.Default.Class
-- import GHC.OverloadedLabels

-- import Riak.Internal.Prelude
-- import ZZZ.Riak.Internal.Types


-- TODO default these that can't be set in props?

-- newtype BasicQuorum    = BasicQuorum    { unBasicQuorum    :: Maybe Bool       }
-- newtype IncludeContext = IncludeContext { unIncludeContext :: Maybe Bool       }
-- newtype DF             = DF             { unDF             :: Maybe ByteString }
-- newtype DW             = DW             { unDW             :: Maybe RiakQuorum }
-- newtype Filter         = Filter         { unFilter         :: Maybe ByteString }
-- newtype FL             = FL             { unFL             :: [ByteString]     }
-- newtype N              = N              { unN              :: Maybe Word32     }
-- newtype NotfoundOk     = NotfoundOk     { unNotfoundOk     :: Maybe Bool       }
-- newtype Op             = Op             { unOp             :: Maybe ByteString }
-- newtype PR             = PR             { unPR             :: Maybe RiakQuorum }
-- newtype Presort        = Presort        { unPresort        :: Maybe ByteString }
-- newtype PW             = PW             { unPW             :: Maybe RiakQuorum }
-- newtype R              = R              { unR              :: Maybe RiakQuorum }
-- newtype ReturnBody     = ReturnBody     { unReturnBody     :: Maybe Bool       }
-- newtype Rows           = Rows           { unRows           :: Maybe Word32     }
-- newtype SloppyQuorum   = SloppyQuorum   { unSloppyQuorum   :: Maybe Bool       }
-- newtype Sort           = Sort           { unSort           :: Maybe ByteString }
-- newtype Start          = Start          { unStart          :: Maybe Word32     }
-- newtype Timeout        = Timeout        { unTimeout        :: Maybe Word32     }
-- newtype W              = W              { unW              :: Maybe RiakQuorum }


-- | Optional @get data type@ parameters.
-- data GetRiakCrdtParams
--   = GetRiakCrdtParams
--       !BasicQuorum
--       !IncludeContext
--       !N
--       !NotfoundOk
--       !PR
--       !R
--       !SloppyQuorum
--       !Timeout

--instance Default GetRiakCrdtParams where
--  def = GetRiakCrdtParams (BasicQuorum Nothing) (IncludeContext Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

--instance (a ~ GetRiakCrdtParams) => IsLabel "basic_quorum"    (Bool       -> a -> GetRiakCrdtParams) where fromLabel = \a (GetRiakCrdtParams _ b c d e f g h) -> GetRiakCrdtParams (coerce (Just a)) b c d e f g h
--instance (a ~ GetRiakCrdtParams) => IsLabel "include_context" (Bool       -> a -> GetRiakCrdtParams) where fromLabel = \b (GetRiakCrdtParams a _ c d e f g h) -> GetRiakCrdtParams a (coerce (Just b)) c d e f g h
--instance (a ~ GetRiakCrdtParams) => IsLabel "n"               (Word32     -> a -> GetRiakCrdtParams) where fromLabel = \c (GetRiakCrdtParams a b _ d e f g h) -> GetRiakCrdtParams a b (coerce (Just c)) d e f g h
--instance (a ~ GetRiakCrdtParams) => IsLabel "notfound_ok"     (Bool       -> a -> GetRiakCrdtParams) where fromLabel = \d (GetRiakCrdtParams a b c _ e f g h) -> GetRiakCrdtParams a b c (coerce (Just d)) e f g h
--instance (a ~ GetRiakCrdtParams) => IsLabel "pr"              (RiakQuorum -> a -> GetRiakCrdtParams) where fromLabel = \e (GetRiakCrdtParams a b c d _ f g h) -> GetRiakCrdtParams a b c d (coerce (Just e)) f g h
--instance (a ~ GetRiakCrdtParams) => IsLabel "r"               (RiakQuorum -> a -> GetRiakCrdtParams) where fromLabel = \f (GetRiakCrdtParams a b c d e _ g h) -> GetRiakCrdtParams a b c d e (coerce (Just f)) g h
--instance (a ~ GetRiakCrdtParams) => IsLabel "sloppy_quorum"   (Bool       -> a -> GetRiakCrdtParams) where fromLabel = \g (GetRiakCrdtParams a b c d e f _ h) -> GetRiakCrdtParams a b c d e f (coerce (Just g)) h
--instance (a ~ GetRiakCrdtParams) => IsLabel "timeout"         (Word32     -> a -> GetRiakCrdtParams) where fromLabel = \h (GetRiakCrdtParams a b c d e f g _) -> GetRiakCrdtParams a b c d e f g (coerce (Just h))


---- | Optional @get object@ parameters.
--data GetRiakObjectParams
--  = GetRiakObjectParams
--      !BasicQuorum
--      !N
--      !NotfoundOk
--      !PR
--      !R
--      !SloppyQuorum
--      !Timeout

--instance Default GetRiakObjectParams where
--  def = GetRiakObjectParams (BasicQuorum Nothing) (N Nothing) (NotfoundOk Nothing) (PR Nothing) (R Nothing) (SloppyQuorum Nothing) (Timeout Nothing)

--instance (a ~ GetRiakObjectParams) => IsLabel "basic_quorum"  (Bool       -> a -> GetRiakObjectParams) where fromLabel = \a (GetRiakObjectParams _ b c d e f g) -> GetRiakObjectParams (coerce (Just a)) b c d e f g
--instance (a ~ GetRiakObjectParams) => IsLabel "n"             (Word32     -> a -> GetRiakObjectParams) where fromLabel = \b (GetRiakObjectParams a _ c d e f g) -> GetRiakObjectParams a (coerce (Just b)) c d e f g
--instance (a ~ GetRiakObjectParams) => IsLabel "notfound_ok"   (Bool       -> a -> GetRiakObjectParams) where fromLabel = \c (GetRiakObjectParams a b _ d e f g) -> GetRiakObjectParams a b (coerce (Just c)) d e f g
--instance (a ~ GetRiakObjectParams) => IsLabel "pr"            (RiakQuorum -> a -> GetRiakObjectParams) where fromLabel = \d (GetRiakObjectParams a b c _ e f g) -> GetRiakObjectParams a b c (coerce (Just d)) e f g
--instance (a ~ GetRiakObjectParams) => IsLabel "r"             (RiakQuorum -> a -> GetRiakObjectParams) where fromLabel = \e (GetRiakObjectParams a b c d _ f g) -> GetRiakObjectParams a b c d (coerce (Just e)) f g
--instance (a ~ GetRiakObjectParams) => IsLabel "sloppy_quorum" (Bool       -> a -> GetRiakObjectParams) where fromLabel = \f (GetRiakObjectParams a b c d e _ g) -> GetRiakObjectParams a b c d e (coerce (Just f)) g
--instance (a ~ GetRiakObjectParams) => IsLabel "timeout"       (Word32     -> a -> GetRiakObjectParams) where fromLabel = \g (GetRiakObjectParams a b c d e f _) -> GetRiakObjectParams a b c d e f (coerce (Just g))


---- | Optional @store object@ parameters.
--data PutRiakObjectParams
--  = PutRiakObjectParams
--      !DW
--      ![RiakIndex]
--      !RiakMetadata
--      !N
--      !PW
--      !SloppyQuorum
--      !Timeout
--      !W

--instance Default PutRiakObjectParams where
--  def = PutRiakObjectParams (DW Nothing) [] (RiakMetadata []) (N Nothing) (PW Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

--instance (a ~ PutRiakObjectParams) => IsLabel "dw"            (RiakQuorum                       -> a -> PutRiakObjectParams) where fromLabel = \a (PutRiakObjectParams _ b c d e f g h) -> PutRiakObjectParams (coerce (Just a)) b c d e f g h
--instance (a ~ PutRiakObjectParams) => IsLabel "indexes"       ([RiakIndex]                      -> a -> PutRiakObjectParams) where fromLabel = \b (PutRiakObjectParams a _ c d e f g h) -> PutRiakObjectParams a b c d e f g h
--instance (a ~ PutRiakObjectParams) => IsLabel "metadata"      ([(ByteString, Maybe ByteString)] -> a -> PutRiakObjectParams) where fromLabel = \c (PutRiakObjectParams a b _ d e f g h) -> PutRiakObjectParams a b (RiakMetadata c) d e f g h
--instance (a ~ PutRiakObjectParams) => IsLabel "n"             (Word32                           -> a -> PutRiakObjectParams) where fromLabel = \d (PutRiakObjectParams a b c _ e f g h) -> PutRiakObjectParams a b c (coerce (Just d)) e f g h
--instance (a ~ PutRiakObjectParams) => IsLabel "pw"            (RiakQuorum                       -> a -> PutRiakObjectParams) where fromLabel = \e (PutRiakObjectParams a b c d _ f g h) -> PutRiakObjectParams a b c d (coerce (Just e)) f g h
--instance (a ~ PutRiakObjectParams) => IsLabel "sloppy_quorum" (Bool                             -> a -> PutRiakObjectParams) where fromLabel = \f (PutRiakObjectParams a b c d e _ g h) -> PutRiakObjectParams a b c d e (coerce (Just f)) g h
--instance (a ~ PutRiakObjectParams) => IsLabel "timeout"       (Word32                           -> a -> PutRiakObjectParams) where fromLabel = \g (PutRiakObjectParams a b c d e f _ h) -> PutRiakObjectParams a b c d e f (coerce (Just g)) h
--instance (a ~ PutRiakObjectParams) => IsLabel "w"             (RiakQuorum                       -> a -> PutRiakObjectParams) where fromLabel = \h (PutRiakObjectParams a b c d e f g _) -> PutRiakObjectParams a b c d e f g (coerce (Just h))


---- | Optional @update data type@ parameters.
--data UpdateRiakCrdtParams
--  = UpdateRiakCrdtParams
--     !DW
--     !N
--     !PW
--     !ReturnBody
--     !SloppyQuorum
--     !Timeout
--     !W

--instance Default UpdateRiakCrdtParams where
--  def = UpdateRiakCrdtParams (DW Nothing) (N Nothing) (PW Nothing) (ReturnBody Nothing) (SloppyQuorum Nothing) (Timeout Nothing) (W Nothing)

--instance (a ~ UpdateRiakCrdtParams) => IsLabel "dw"            (RiakQuorum -> a -> UpdateRiakCrdtParams) where fromLabel = \a (UpdateRiakCrdtParams _ b c d e f g) -> UpdateRiakCrdtParams (coerce (Just a)) b c d e f g
--instance (a ~ UpdateRiakCrdtParams) => IsLabel "n"             (Word32     -> a -> UpdateRiakCrdtParams) where fromLabel = \b (UpdateRiakCrdtParams a _ c d e f g) -> UpdateRiakCrdtParams a (coerce (Just b)) c d e f g
--instance (a ~ UpdateRiakCrdtParams) => IsLabel "pw"            (RiakQuorum -> a -> UpdateRiakCrdtParams) where fromLabel = \c (UpdateRiakCrdtParams a b _ d e f g) -> UpdateRiakCrdtParams a b (coerce (Just c)) d e f g
--instance (a ~ UpdateRiakCrdtParams) => IsLabel "return_body"   (Bool       -> a -> UpdateRiakCrdtParams) where fromLabel = \d (UpdateRiakCrdtParams a b c _ e f g) -> UpdateRiakCrdtParams a b c (coerce (Just d)) e f g
--instance (a ~ UpdateRiakCrdtParams) => IsLabel "sloppy_quorum" (Bool       -> a -> UpdateRiakCrdtParams) where fromLabel = \e (UpdateRiakCrdtParams a b c d _ f g) -> UpdateRiakCrdtParams a b c d (coerce (Just e)) f g
--instance (a ~ UpdateRiakCrdtParams) => IsLabel "timeout"       (Word32     -> a -> UpdateRiakCrdtParams) where fromLabel = \f (UpdateRiakCrdtParams a b c d e _ g) -> UpdateRiakCrdtParams a b c d e (coerce (Just f)) g
--instance (a ~ UpdateRiakCrdtParams) => IsLabel "w"             (RiakQuorum -> a -> UpdateRiakCrdtParams) where fromLabel = \g (UpdateRiakCrdtParams a b c d e f _) -> UpdateRiakCrdtParams a b c d e f (coerce (Just g))


---- | Optional @search@ parameters.
----
---- +---------+--------------------------------------------------------------------+
---- | df      | Default field                                                      |
---- +---------+--------------------------------------------------------------------+
---- | filter  | Filters search with additional query scoped to inline fields       |
---- +---------+--------------------------------------------------------------------+
---- | fl      | Fields to return information about                                 |
---- +---------+--------------------------------------------------------------------+
---- | op      | Default operator, @and@ or @or@                                    |
---- +---------+--------------------------------------------------------------------+
---- | presort | Presort, @key@ or @score@                                          |
---- +---------+--------------------------------------------------------------------+
---- | rows    | The maximum number of rows to return                               |
---- +---------+--------------------------------------------------------------------+
---- | sort    | How the search results are to be sorted, @asc@ or @desc@           |
---- +---------+--------------------------------------------------------------------+
---- | start   | A start offset; the number of keys to skip before returning values |
---- +---------+--------------------------------------------------------------------+
--data RiakSearchParams
--  = RiakSearchParams
--      !DF
--      !Filter
--      !FL
--      !Op
--      !Presort
--      !Rows
--      !Sort
--      !Start

--instance Default RiakSearchParams where
--  def = RiakSearchParams (DF Nothing) (Filter Nothing) (FL []) (Op Nothing) (Presort Nothing) (Rows Nothing) (Sort Nothing) (Start Nothing)

--instance (a ~ RiakSearchParams) => IsLabel "df"      (ByteString   -> a -> RiakSearchParams) where fromLabel = \a (RiakSearchParams _ b c d e f g h) -> RiakSearchParams (coerce (Just a)) b c d e f g h
--instance (a ~ RiakSearchParams) => IsLabel "filter"  (ByteString   -> a -> RiakSearchParams) where fromLabel = \b (RiakSearchParams a _ c d e f g h) -> RiakSearchParams a (coerce (Just b)) c d e f g h
--instance (a ~ RiakSearchParams) => IsLabel "fl"      ([ByteString] -> a -> RiakSearchParams) where fromLabel = \c (RiakSearchParams a b _ d e f g h) -> RiakSearchParams a b (coerce c) d e f g h
--instance (a ~ RiakSearchParams) => IsLabel "op"      (ByteString   -> a -> RiakSearchParams) where fromLabel = \d (RiakSearchParams a b c _ e f g h) -> RiakSearchParams a b c (coerce (Just d)) e f g h
--instance (a ~ RiakSearchParams) => IsLabel "presort" (ByteString   -> a -> RiakSearchParams) where fromLabel = \e (RiakSearchParams a b c d _ f g h) -> RiakSearchParams a b c d (coerce (Just e)) f g h
--instance (a ~ RiakSearchParams) => IsLabel "rows"    (Word32       -> a -> RiakSearchParams) where fromLabel = \f (RiakSearchParams a b c d e _ g h) -> RiakSearchParams a b c d e (coerce (Just f)) g h
--instance (a ~ RiakSearchParams) => IsLabel "sort"    (ByteString   -> a -> RiakSearchParams) where fromLabel = \g (RiakSearchParams a b c d e f _ h) -> RiakSearchParams a b c d e f (coerce (Just g)) h
--instance (a ~ RiakSearchParams) => IsLabel "start"   (Word32       -> a -> RiakSearchParams) where fromLabel = \h (RiakSearchParams a b c d e f g _) -> RiakSearchParams a b c d e f g (coerce (Just h))
