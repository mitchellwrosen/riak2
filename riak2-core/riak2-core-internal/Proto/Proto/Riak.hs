{- This file was auto-generated from proto/riak.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Proto.Riak
       (AuthenticationRequest(), BucketProperties(),
        BucketProperties'RpbReplMode(..), BucketProperties'RpbReplMode(),
        CommitHook(), Content(), CounterUpdate(), Crdt(), CrdtUpdate(),
        DeleteRequest(), DeleteResponse(), EmptyPutResponse(),
        ErrorResponse(), GSetOp(), GetBucketPropertiesRequest(),
        GetBucketPropertiesResponse(), GetBucketTypePropertiesRequest(),
        GetCrdtRequest(), GetCrdtResponse(), GetCrdtResponse'CrdtType(..),
        GetCrdtResponse'CrdtType(), GetRequest(), GetResponse(),
        GetServerInfoRequest(), GetServerInfoResponse(), HllOp(),
        IndexRequest(), IndexRequest'IndexQueryType(..),
        IndexRequest'IndexQueryType(), IndexResponse(), Link(), MapEntry(),
        MapField(), MapField'MapFieldType(..), MapField'MapFieldType(),
        MapFieldUpdate(), MapFieldUpdate'FlagUpdate(..),
        MapFieldUpdate'FlagUpdate(), MapReduceRequest(),
        MapReduceResponse(), MapUpdate(), ModuleFunction(), Pair(),
        PingRequest(), PingResponse(), PutRequest(), PutResponse(),
        ResetBucketPropertiesRequest(), ResetBucketPropertiesResponse(),
        SetBucketPropertiesRequest(), SetBucketPropertiesResponse(),
        SetBucketTypePropertiesRequest(),
        SetBucketTypePropertiesResponse(), SetUpdate(),
        StreamBucketsRequest(), StreamBucketsResponse(),
        StreamKeysRequest(), StreamKeysResponse(), UpdateCrdtRequest(),
        UpdateCrdtResponse())
       where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq
       as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Lens.Labels.Prism
       as Lens.Labels.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Lens.Labels as Lens.Labels
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read

{- | Fields :

    * 'Proto.Proto.Riak_Fields.user' @:: Lens' AuthenticationRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.password' @:: Lens' AuthenticationRequest Data.ByteString.ByteString@
 -}
data AuthenticationRequest = AuthenticationRequest{_AuthenticationRequest'user
                                                   :: !Data.ByteString.ByteString,
                                                   _AuthenticationRequest'password ::
                                                   !Data.ByteString.ByteString,
                                                   _AuthenticationRequest'_unknownFields ::
                                                   !Data.ProtoLens.FieldSet}
                               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AuthenticationRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' AuthenticationRequest "user"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AuthenticationRequest'user
                 (\ x__ y__ -> x__{_AuthenticationRequest'user = y__}))
              Prelude.id
instance Lens.Labels.HasLens' AuthenticationRequest "password"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _AuthenticationRequest'password
                 (\ x__ y__ -> x__{_AuthenticationRequest'password = y__}))
              Prelude.id
instance Data.ProtoLens.Message AuthenticationRequest where
        messageName _ = Data.Text.pack "AuthenticationRequest"
        fieldsByTag
          = let user__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "user"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "user")))
                      :: Data.ProtoLens.FieldDescriptor AuthenticationRequest
                password__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "password"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "password")))
                      :: Data.ProtoLens.FieldDescriptor AuthenticationRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, user__field_descriptor),
                 (Data.ProtoLens.Tag 2, password__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _AuthenticationRequest'_unknownFields
              (\ x__ y__ -> x__{_AuthenticationRequest'_unknownFields = y__})
        defMessage
          = AuthenticationRequest{_AuthenticationRequest'user =
                                    Data.ProtoLens.fieldDefault,
                                  _AuthenticationRequest'password = Data.ProtoLens.fieldDefault,
                                  _AuthenticationRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData AuthenticationRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_AuthenticationRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_AuthenticationRequest'user x__)
                   (Control.DeepSeq.deepseq (_AuthenticationRequest'password x__)
                      (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.n' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'n' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.allowMult' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'allowMult' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.lastWriteWins' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'lastWriteWins' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.precommit' @:: Lens' BucketProperties [CommitHook]@
    * 'Proto.Proto.Riak_Fields.hasPrecommit' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'hasPrecommit' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.postcommit' @:: Lens' BucketProperties [CommitHook]@
    * 'Proto.Proto.Riak_Fields.hasPostcommit' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'hasPostcommit' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.chashKeyfun' @:: Lens' BucketProperties ModuleFunction@
    * 'Proto.Proto.Riak_Fields.maybe'chashKeyfun' @:: Lens' BucketProperties (Prelude.Maybe ModuleFunction)@
    * 'Proto.Proto.Riak_Fields.linkfun' @:: Lens' BucketProperties ModuleFunction@
    * 'Proto.Proto.Riak_Fields.maybe'linkfun' @:: Lens' BucketProperties (Prelude.Maybe ModuleFunction)@
    * 'Proto.Proto.Riak_Fields.oldVclock' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'oldVclock' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.youngVclock' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'youngVclock' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.bigVclock' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'bigVclock' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.smallVclock' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'smallVclock' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pr' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pr' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.r' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'r' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.w' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'w' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pw' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pw' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.dw' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'dw' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.rw' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'rw' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.basicQuorum' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'basicQuorum' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.notfoundOk' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'notfoundOk' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.backend' @:: Lens' BucketProperties Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'backend' @:: Lens' BucketProperties (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.search' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'search' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.repl' @:: Lens' BucketProperties BucketProperties'RpbReplMode@
    * 'Proto.Proto.Riak_Fields.maybe'repl' @:: Lens' BucketProperties (Prelude.Maybe BucketProperties'RpbReplMode)@
    * 'Proto.Proto.Riak_Fields.searchIndex' @:: Lens' BucketProperties Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'searchIndex' @:: Lens' BucketProperties (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.datatype' @:: Lens' BucketProperties Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'datatype' @:: Lens' BucketProperties (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.consistent' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'consistent' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.writeOnce' @:: Lens' BucketProperties Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'writeOnce' @:: Lens' BucketProperties (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.hllPrecision' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'hllPrecision' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.ttl' @:: Lens' BucketProperties Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'ttl' @:: Lens' BucketProperties (Prelude.Maybe Data.Word.Word32)@
 -}
data BucketProperties = BucketProperties{_BucketProperties'n ::
                                         !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'allowMult ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'lastWriteWins ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'precommit :: ![CommitHook],
                                         _BucketProperties'hasPrecommit ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'postcommit :: ![CommitHook],
                                         _BucketProperties'hasPostcommit ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'chashKeyfun ::
                                         !(Prelude.Maybe ModuleFunction),
                                         _BucketProperties'linkfun ::
                                         !(Prelude.Maybe ModuleFunction),
                                         _BucketProperties'oldVclock ::
                                         !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'youngVclock ::
                                         !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'bigVclock ::
                                         !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'smallVclock ::
                                         !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'pr :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'r :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'w :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'pw :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'dw :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'rw :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'basicQuorum ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'notfoundOk ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'backend ::
                                         !(Prelude.Maybe Data.ByteString.ByteString),
                                         _BucketProperties'search :: !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'repl ::
                                         !(Prelude.Maybe BucketProperties'RpbReplMode),
                                         _BucketProperties'searchIndex ::
                                         !(Prelude.Maybe Data.ByteString.ByteString),
                                         _BucketProperties'datatype ::
                                         !(Prelude.Maybe Data.ByteString.ByteString),
                                         _BucketProperties'consistent ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'writeOnce ::
                                         !(Prelude.Maybe Prelude.Bool),
                                         _BucketProperties'hllPrecision ::
                                         !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'ttl :: !(Prelude.Maybe Data.Word.Word32),
                                         _BucketProperties'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                          deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BucketProperties where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' BucketProperties "n"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'n
                 (\ x__ y__ -> x__{_BucketProperties'n = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'n"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'n
                 (\ x__ y__ -> x__{_BucketProperties'n = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "allowMult"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'allowMult
                 (\ x__ y__ -> x__{_BucketProperties'allowMult = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'allowMult"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'allowMult
                 (\ x__ y__ -> x__{_BucketProperties'allowMult = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "lastWriteWins"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'lastWriteWins
                 (\ x__ y__ -> x__{_BucketProperties'lastWriteWins = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties
           "maybe'lastWriteWins"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'lastWriteWins
                 (\ x__ y__ -> x__{_BucketProperties'lastWriteWins = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "precommit"
           ([CommitHook])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'precommit
                 (\ x__ y__ -> x__{_BucketProperties'precommit = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "hasPrecommit"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'hasPrecommit
                 (\ x__ y__ -> x__{_BucketProperties'hasPrecommit = y__}))
              (Data.ProtoLens.maybeLens Prelude.False)
instance Lens.Labels.HasLens' BucketProperties "maybe'hasPrecommit"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'hasPrecommit
                 (\ x__ y__ -> x__{_BucketProperties'hasPrecommit = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "postcommit"
           ([CommitHook])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'postcommit
                 (\ x__ y__ -> x__{_BucketProperties'postcommit = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "hasPostcommit"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'hasPostcommit
                 (\ x__ y__ -> x__{_BucketProperties'hasPostcommit = y__}))
              (Data.ProtoLens.maybeLens Prelude.False)
instance Lens.Labels.HasLens' BucketProperties
           "maybe'hasPostcommit"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'hasPostcommit
                 (\ x__ y__ -> x__{_BucketProperties'hasPostcommit = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "chashKeyfun"
           (ModuleFunction)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'chashKeyfun
                 (\ x__ y__ -> x__{_BucketProperties'chashKeyfun = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' BucketProperties "maybe'chashKeyfun"
           (Prelude.Maybe ModuleFunction)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'chashKeyfun
                 (\ x__ y__ -> x__{_BucketProperties'chashKeyfun = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "linkfun"
           (ModuleFunction)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'linkfun
                 (\ x__ y__ -> x__{_BucketProperties'linkfun = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' BucketProperties "maybe'linkfun"
           (Prelude.Maybe ModuleFunction)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'linkfun
                 (\ x__ y__ -> x__{_BucketProperties'linkfun = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "oldVclock"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'oldVclock
                 (\ x__ y__ -> x__{_BucketProperties'oldVclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'oldVclock"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'oldVclock
                 (\ x__ y__ -> x__{_BucketProperties'oldVclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "youngVclock"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'youngVclock
                 (\ x__ y__ -> x__{_BucketProperties'youngVclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'youngVclock"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'youngVclock
                 (\ x__ y__ -> x__{_BucketProperties'youngVclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "bigVclock"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'bigVclock
                 (\ x__ y__ -> x__{_BucketProperties'bigVclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'bigVclock"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'bigVclock
                 (\ x__ y__ -> x__{_BucketProperties'bigVclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "smallVclock"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'smallVclock
                 (\ x__ y__ -> x__{_BucketProperties'smallVclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'smallVclock"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'smallVclock
                 (\ x__ y__ -> x__{_BucketProperties'smallVclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "pr"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'pr
                 (\ x__ y__ -> x__{_BucketProperties'pr = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'pr"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'pr
                 (\ x__ y__ -> x__{_BucketProperties'pr = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "r"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'r
                 (\ x__ y__ -> x__{_BucketProperties'r = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'r"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'r
                 (\ x__ y__ -> x__{_BucketProperties'r = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "w"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'w
                 (\ x__ y__ -> x__{_BucketProperties'w = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'w"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'w
                 (\ x__ y__ -> x__{_BucketProperties'w = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "pw"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'pw
                 (\ x__ y__ -> x__{_BucketProperties'pw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'pw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'pw
                 (\ x__ y__ -> x__{_BucketProperties'pw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "dw"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'dw
                 (\ x__ y__ -> x__{_BucketProperties'dw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'dw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'dw
                 (\ x__ y__ -> x__{_BucketProperties'dw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "rw"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'rw
                 (\ x__ y__ -> x__{_BucketProperties'rw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'rw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'rw
                 (\ x__ y__ -> x__{_BucketProperties'rw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "basicQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'basicQuorum
                 (\ x__ y__ -> x__{_BucketProperties'basicQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'basicQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'basicQuorum
                 (\ x__ y__ -> x__{_BucketProperties'basicQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "notfoundOk"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'notfoundOk
                 (\ x__ y__ -> x__{_BucketProperties'notfoundOk = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'notfoundOk"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'notfoundOk
                 (\ x__ y__ -> x__{_BucketProperties'notfoundOk = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "backend"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'backend
                 (\ x__ y__ -> x__{_BucketProperties'backend = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'backend"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'backend
                 (\ x__ y__ -> x__{_BucketProperties'backend = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "search"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'search
                 (\ x__ y__ -> x__{_BucketProperties'search = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'search"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'search
                 (\ x__ y__ -> x__{_BucketProperties'search = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "repl"
           (BucketProperties'RpbReplMode)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'repl
                 (\ x__ y__ -> x__{_BucketProperties'repl = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'repl"
           (Prelude.Maybe BucketProperties'RpbReplMode)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'repl
                 (\ x__ y__ -> x__{_BucketProperties'repl = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "searchIndex"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'searchIndex
                 (\ x__ y__ -> x__{_BucketProperties'searchIndex = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'searchIndex"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'searchIndex
                 (\ x__ y__ -> x__{_BucketProperties'searchIndex = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "datatype"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'datatype
                 (\ x__ y__ -> x__{_BucketProperties'datatype = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'datatype"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'datatype
                 (\ x__ y__ -> x__{_BucketProperties'datatype = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "consistent"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'consistent
                 (\ x__ y__ -> x__{_BucketProperties'consistent = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'consistent"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'consistent
                 (\ x__ y__ -> x__{_BucketProperties'consistent = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "writeOnce"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'writeOnce
                 (\ x__ y__ -> x__{_BucketProperties'writeOnce = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'writeOnce"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'writeOnce
                 (\ x__ y__ -> x__{_BucketProperties'writeOnce = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "hllPrecision"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'hllPrecision
                 (\ x__ y__ -> x__{_BucketProperties'hllPrecision = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'hllPrecision"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'hllPrecision
                 (\ x__ y__ -> x__{_BucketProperties'hllPrecision = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BucketProperties "ttl"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'ttl
                 (\ x__ y__ -> x__{_BucketProperties'ttl = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' BucketProperties "maybe'ttl"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BucketProperties'ttl
                 (\ x__ y__ -> x__{_BucketProperties'ttl = y__}))
              Prelude.id
instance Data.ProtoLens.Message BucketProperties where
        messageName _ = Data.Text.pack "BucketProperties"
        fieldsByTag
          = let n__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "n"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                allowMult__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "allow_mult"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'allowMult")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                lastWriteWins__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "last_write_wins"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'lastWriteWins")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                precommit__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "precommit"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor CommitHook)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "precommit")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                hasPrecommit__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "has_precommit"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'hasPrecommit")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                postcommit__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "postcommit"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor CommitHook)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "postcommit")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                hasPostcommit__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "has_postcommit"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'hasPostcommit")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                chashKeyfun__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "chash_keyfun"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ModuleFunction)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'chashKeyfun")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                linkfun__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "linkfun"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ModuleFunction)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'linkfun")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                oldVclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "old_vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'oldVclock")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                youngVclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "young_vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'youngVclock")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                bigVclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "big_vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'bigVclock")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                smallVclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "small_vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'smallVclock")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                pr__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pr"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pr")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                r__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "r"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'r")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                w__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "w"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'w")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                pw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pw")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                dw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dw")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                rw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "rw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rw")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                basicQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "basic_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'basicQuorum")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                notfoundOk__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "notfound_ok"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'notfoundOk")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                backend__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "backend"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'backend")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                search__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "search"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'search")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                repl__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "repl"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor BucketProperties'RpbReplMode)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'repl")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                searchIndex__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "search_index"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'searchIndex")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                datatype__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "datatype"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'datatype")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                consistent__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "consistent"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'consistent")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                writeOnce__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "write_once"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'writeOnce")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                hllPrecision__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "hll_precision"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'hllPrecision")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
                ttl__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ttl"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'ttl")))
                      :: Data.ProtoLens.FieldDescriptor BucketProperties
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, n__field_descriptor),
                 (Data.ProtoLens.Tag 2, allowMult__field_descriptor),
                 (Data.ProtoLens.Tag 3, lastWriteWins__field_descriptor),
                 (Data.ProtoLens.Tag 4, precommit__field_descriptor),
                 (Data.ProtoLens.Tag 5, hasPrecommit__field_descriptor),
                 (Data.ProtoLens.Tag 6, postcommit__field_descriptor),
                 (Data.ProtoLens.Tag 7, hasPostcommit__field_descriptor),
                 (Data.ProtoLens.Tag 8, chashKeyfun__field_descriptor),
                 (Data.ProtoLens.Tag 9, linkfun__field_descriptor),
                 (Data.ProtoLens.Tag 10, oldVclock__field_descriptor),
                 (Data.ProtoLens.Tag 11, youngVclock__field_descriptor),
                 (Data.ProtoLens.Tag 12, bigVclock__field_descriptor),
                 (Data.ProtoLens.Tag 13, smallVclock__field_descriptor),
                 (Data.ProtoLens.Tag 14, pr__field_descriptor),
                 (Data.ProtoLens.Tag 15, r__field_descriptor),
                 (Data.ProtoLens.Tag 16, w__field_descriptor),
                 (Data.ProtoLens.Tag 17, pw__field_descriptor),
                 (Data.ProtoLens.Tag 18, dw__field_descriptor),
                 (Data.ProtoLens.Tag 19, rw__field_descriptor),
                 (Data.ProtoLens.Tag 20, basicQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 21, notfoundOk__field_descriptor),
                 (Data.ProtoLens.Tag 22, backend__field_descriptor),
                 (Data.ProtoLens.Tag 23, search__field_descriptor),
                 (Data.ProtoLens.Tag 24, repl__field_descriptor),
                 (Data.ProtoLens.Tag 25, searchIndex__field_descriptor),
                 (Data.ProtoLens.Tag 26, datatype__field_descriptor),
                 (Data.ProtoLens.Tag 27, consistent__field_descriptor),
                 (Data.ProtoLens.Tag 28, writeOnce__field_descriptor),
                 (Data.ProtoLens.Tag 29, hllPrecision__field_descriptor),
                 (Data.ProtoLens.Tag 30, ttl__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _BucketProperties'_unknownFields
              (\ x__ y__ -> x__{_BucketProperties'_unknownFields = y__})
        defMessage
          = BucketProperties{_BucketProperties'n = Prelude.Nothing,
                             _BucketProperties'allowMult = Prelude.Nothing,
                             _BucketProperties'lastWriteWins = Prelude.Nothing,
                             _BucketProperties'precommit = [],
                             _BucketProperties'hasPrecommit = Prelude.Nothing,
                             _BucketProperties'postcommit = [],
                             _BucketProperties'hasPostcommit = Prelude.Nothing,
                             _BucketProperties'chashKeyfun = Prelude.Nothing,
                             _BucketProperties'linkfun = Prelude.Nothing,
                             _BucketProperties'oldVclock = Prelude.Nothing,
                             _BucketProperties'youngVclock = Prelude.Nothing,
                             _BucketProperties'bigVclock = Prelude.Nothing,
                             _BucketProperties'smallVclock = Prelude.Nothing,
                             _BucketProperties'pr = Prelude.Nothing,
                             _BucketProperties'r = Prelude.Nothing,
                             _BucketProperties'w = Prelude.Nothing,
                             _BucketProperties'pw = Prelude.Nothing,
                             _BucketProperties'dw = Prelude.Nothing,
                             _BucketProperties'rw = Prelude.Nothing,
                             _BucketProperties'basicQuorum = Prelude.Nothing,
                             _BucketProperties'notfoundOk = Prelude.Nothing,
                             _BucketProperties'backend = Prelude.Nothing,
                             _BucketProperties'search = Prelude.Nothing,
                             _BucketProperties'repl = Prelude.Nothing,
                             _BucketProperties'searchIndex = Prelude.Nothing,
                             _BucketProperties'datatype = Prelude.Nothing,
                             _BucketProperties'consistent = Prelude.Nothing,
                             _BucketProperties'writeOnce = Prelude.Nothing,
                             _BucketProperties'hllPrecision = Prelude.Nothing,
                             _BucketProperties'ttl = Prelude.Nothing,
                             _BucketProperties'_unknownFields = ([])}
instance Control.DeepSeq.NFData BucketProperties where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_BucketProperties'_unknownFields x__)
                (Control.DeepSeq.deepseq (_BucketProperties'n x__)
                   (Control.DeepSeq.deepseq (_BucketProperties'allowMult x__)
                      (Control.DeepSeq.deepseq (_BucketProperties'lastWriteWins x__)
                         (Control.DeepSeq.deepseq (_BucketProperties'precommit x__)
                            (Control.DeepSeq.deepseq (_BucketProperties'hasPrecommit x__)
                               (Control.DeepSeq.deepseq (_BucketProperties'postcommit x__)
                                  (Control.DeepSeq.deepseq (_BucketProperties'hasPostcommit x__)
                                     (Control.DeepSeq.deepseq (_BucketProperties'chashKeyfun x__)
                                        (Control.DeepSeq.deepseq (_BucketProperties'linkfun x__)
                                           (Control.DeepSeq.deepseq
                                              (_BucketProperties'oldVclock x__)
                                              (Control.DeepSeq.deepseq
                                                 (_BucketProperties'youngVclock x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_BucketProperties'bigVclock x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_BucketProperties'smallVclock x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_BucketProperties'pr x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_BucketProperties'r x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_BucketProperties'w x__)
                                                                (Control.DeepSeq.deepseq
                                                                   (_BucketProperties'pw x__)
                                                                   (Control.DeepSeq.deepseq
                                                                      (_BucketProperties'dw x__)
                                                                      (Control.DeepSeq.deepseq
                                                                         (_BucketProperties'rw x__)
                                                                         (Control.DeepSeq.deepseq
                                                                            (_BucketProperties'basicQuorum
                                                                               x__)
                                                                            (Control.DeepSeq.deepseq
                                                                               (_BucketProperties'notfoundOk
                                                                                  x__)
                                                                               (Control.DeepSeq.deepseq
                                                                                  (_BucketProperties'backend
                                                                                     x__)
                                                                                  (Control.DeepSeq.deepseq
                                                                                     (_BucketProperties'search
                                                                                        x__)
                                                                                     (Control.DeepSeq.deepseq
                                                                                        (_BucketProperties'repl
                                                                                           x__)
                                                                                        (Control.DeepSeq.deepseq
                                                                                           (_BucketProperties'searchIndex
                                                                                              x__)
                                                                                           (Control.DeepSeq.deepseq
                                                                                              (_BucketProperties'datatype
                                                                                                 x__)
                                                                                              (Control.DeepSeq.deepseq
                                                                                                 (_BucketProperties'consistent
                                                                                                    x__)
                                                                                                 (Control.DeepSeq.deepseq
                                                                                                    (_BucketProperties'writeOnce
                                                                                                       x__)
                                                                                                    (Control.DeepSeq.deepseq
                                                                                                       (_BucketProperties'hllPrecision
                                                                                                          x__)
                                                                                                       (Control.DeepSeq.deepseq
                                                                                                          (_BucketProperties'ttl
                                                                                                             x__)
                                                                                                          (())))))))))))))))))))))))))))))))
data BucketProperties'RpbReplMode = BucketProperties'FALSE
                                  | BucketProperties'REALTIME
                                  | BucketProperties'FULLSYNC
                                  | BucketProperties'TRUE
                                      deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum BucketProperties'RpbReplMode
         where
        maybeToEnum 0 = Prelude.Just BucketProperties'FALSE
        maybeToEnum 1 = Prelude.Just BucketProperties'REALTIME
        maybeToEnum 2 = Prelude.Just BucketProperties'FULLSYNC
        maybeToEnum 3 = Prelude.Just BucketProperties'TRUE
        maybeToEnum _ = Prelude.Nothing
        showEnum BucketProperties'FALSE = "FALSE"
        showEnum BucketProperties'REALTIME = "REALTIME"
        showEnum BucketProperties'FULLSYNC = "FULLSYNC"
        showEnum BucketProperties'TRUE = "TRUE"
        readEnum k
          | (Prelude.==) k "FALSE" = Prelude.Just BucketProperties'FALSE
          | (Prelude.==) k "REALTIME" =
            Prelude.Just BucketProperties'REALTIME
          | (Prelude.==) k "FULLSYNC" =
            Prelude.Just BucketProperties'FULLSYNC
          | (Prelude.==) k "TRUE" = Prelude.Just BucketProperties'TRUE
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded BucketProperties'RpbReplMode where
        minBound = BucketProperties'FALSE
        maxBound = BucketProperties'TRUE
instance Prelude.Enum BucketProperties'RpbReplMode where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum RpbReplMode: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum BucketProperties'FALSE = 0
        fromEnum BucketProperties'REALTIME = 1
        fromEnum BucketProperties'FULLSYNC = 2
        fromEnum BucketProperties'TRUE = 3
        succ BucketProperties'TRUE
          = Prelude.error
              "BucketProperties'RpbReplMode.succ: bad argument BucketProperties'TRUE. This value would be out of bounds."
        succ BucketProperties'FALSE = BucketProperties'REALTIME
        succ BucketProperties'REALTIME = BucketProperties'FULLSYNC
        succ BucketProperties'FULLSYNC = BucketProperties'TRUE
        pred BucketProperties'FALSE
          = Prelude.error
              "BucketProperties'RpbReplMode.pred: bad argument BucketProperties'FALSE. This value would be out of bounds."
        pred BucketProperties'REALTIME = BucketProperties'FALSE
        pred BucketProperties'FULLSYNC = BucketProperties'REALTIME
        pred BucketProperties'TRUE = BucketProperties'FULLSYNC
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault BucketProperties'RpbReplMode
         where
        fieldDefault = BucketProperties'FALSE
instance Control.DeepSeq.NFData BucketProperties'RpbReplMode where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.modfun' @:: Lens' CommitHook ModuleFunction@
    * 'Proto.Proto.Riak_Fields.maybe'modfun' @:: Lens' CommitHook (Prelude.Maybe ModuleFunction)@
    * 'Proto.Proto.Riak_Fields.name' @:: Lens' CommitHook Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'name' @:: Lens' CommitHook (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data CommitHook = CommitHook{_CommitHook'modfun ::
                             !(Prelude.Maybe ModuleFunction),
                             _CommitHook'name :: !(Prelude.Maybe Data.ByteString.ByteString),
                             _CommitHook'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CommitHook where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' CommitHook "modfun" (ModuleFunction)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CommitHook'modfun
                 (\ x__ y__ -> x__{_CommitHook'modfun = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' CommitHook "maybe'modfun"
           (Prelude.Maybe ModuleFunction)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CommitHook'modfun
                 (\ x__ y__ -> x__{_CommitHook'modfun = y__}))
              Prelude.id
instance Lens.Labels.HasLens' CommitHook "name"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CommitHook'name
                 (\ x__ y__ -> x__{_CommitHook'name = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' CommitHook "maybe'name"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CommitHook'name
                 (\ x__ y__ -> x__{_CommitHook'name = y__}))
              Prelude.id
instance Data.ProtoLens.Message CommitHook where
        messageName _ = Data.Text.pack "CommitHook"
        fieldsByTag
          = let modfun__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "modfun"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ModuleFunction)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'modfun")))
                      :: Data.ProtoLens.FieldDescriptor CommitHook
                name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'name")))
                      :: Data.ProtoLens.FieldDescriptor CommitHook
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, modfun__field_descriptor),
                 (Data.ProtoLens.Tag 2, name__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _CommitHook'_unknownFields
              (\ x__ y__ -> x__{_CommitHook'_unknownFields = y__})
        defMessage
          = CommitHook{_CommitHook'modfun = Prelude.Nothing,
                       _CommitHook'name = Prelude.Nothing,
                       _CommitHook'_unknownFields = ([])}
instance Control.DeepSeq.NFData CommitHook where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_CommitHook'_unknownFields x__)
                (Control.DeepSeq.deepseq (_CommitHook'modfun x__)
                   (Control.DeepSeq.deepseq (_CommitHook'name x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.value' @:: Lens' Content Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.contentType' @:: Lens' Content Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'contentType' @:: Lens' Content (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.charset' @:: Lens' Content Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'charset' @:: Lens' Content (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.contentEncoding' @:: Lens' Content Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'contentEncoding' @:: Lens' Content (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.vtag' @:: Lens' Content Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'vtag' @:: Lens' Content (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.links' @:: Lens' Content [Link]@
    * 'Proto.Proto.Riak_Fields.lastMod' @:: Lens' Content Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'lastMod' @:: Lens' Content (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.lastModUsecs' @:: Lens' Content Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'lastModUsecs' @:: Lens' Content (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.usermeta' @:: Lens' Content [Pair]@
    * 'Proto.Proto.Riak_Fields.indexes' @:: Lens' Content [Pair]@
    * 'Proto.Proto.Riak_Fields.deleted' @:: Lens' Content Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'deleted' @:: Lens' Content (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.ttl' @:: Lens' Content Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'ttl' @:: Lens' Content (Prelude.Maybe Data.Word.Word32)@
 -}
data Content = Content{_Content'value ::
                       !Data.ByteString.ByteString,
                       _Content'contentType ::
                       !(Prelude.Maybe Data.ByteString.ByteString),
                       _Content'charset :: !(Prelude.Maybe Data.ByteString.ByteString),
                       _Content'contentEncoding ::
                       !(Prelude.Maybe Data.ByteString.ByteString),
                       _Content'vtag :: !(Prelude.Maybe Data.ByteString.ByteString),
                       _Content'links :: ![Link],
                       _Content'lastMod :: !(Prelude.Maybe Data.Word.Word32),
                       _Content'lastModUsecs :: !(Prelude.Maybe Data.Word.Word32),
                       _Content'usermeta :: ![Pair], _Content'indexes :: ![Pair],
                       _Content'deleted :: !(Prelude.Maybe Prelude.Bool),
                       _Content'ttl :: !(Prelude.Maybe Data.Word.Word32),
                       _Content'_unknownFields :: !Data.ProtoLens.FieldSet}
                 deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Content where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Content "value"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'value
                 (\ x__ y__ -> x__{_Content'value = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "contentType"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'contentType
                 (\ x__ y__ -> x__{_Content'contentType = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'contentType"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'contentType
                 (\ x__ y__ -> x__{_Content'contentType = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "charset"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'charset
                 (\ x__ y__ -> x__{_Content'charset = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'charset"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'charset
                 (\ x__ y__ -> x__{_Content'charset = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "contentEncoding"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'contentEncoding
                 (\ x__ y__ -> x__{_Content'contentEncoding = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'contentEncoding"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'contentEncoding
                 (\ x__ y__ -> x__{_Content'contentEncoding = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "vtag"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'vtag
                 (\ x__ y__ -> x__{_Content'vtag = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'vtag"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'vtag
                 (\ x__ y__ -> x__{_Content'vtag = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "links" ([Link]) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'links
                 (\ x__ y__ -> x__{_Content'links = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "lastMod" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'lastMod
                 (\ x__ y__ -> x__{_Content'lastMod = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'lastMod"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'lastMod
                 (\ x__ y__ -> x__{_Content'lastMod = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "lastModUsecs"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'lastModUsecs
                 (\ x__ y__ -> x__{_Content'lastModUsecs = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'lastModUsecs"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'lastModUsecs
                 (\ x__ y__ -> x__{_Content'lastModUsecs = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "usermeta" ([Pair]) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'usermeta
                 (\ x__ y__ -> x__{_Content'usermeta = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "indexes" ([Pair]) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'indexes
                 (\ x__ y__ -> x__{_Content'indexes = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "deleted" (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'deleted
                 (\ x__ y__ -> x__{_Content'deleted = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'deleted"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'deleted
                 (\ x__ y__ -> x__{_Content'deleted = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Content "ttl" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'ttl
                 (\ x__ y__ -> x__{_Content'ttl = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Content "maybe'ttl"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Content'ttl
                 (\ x__ y__ -> x__{_Content'ttl = y__}))
              Prelude.id
instance Data.ProtoLens.Message Content where
        messageName _ = Data.Text.pack "Content"
        fieldsByTag
          = let value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "value")))
                      :: Data.ProtoLens.FieldDescriptor Content
                contentType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'contentType")))
                      :: Data.ProtoLens.FieldDescriptor Content
                charset__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "charset"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'charset")))
                      :: Data.ProtoLens.FieldDescriptor Content
                contentEncoding__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content_encoding"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'contentEncoding")))
                      :: Data.ProtoLens.FieldDescriptor Content
                vtag__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vtag"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'vtag")))
                      :: Data.ProtoLens.FieldDescriptor Content
                links__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "links"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Link)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "links")))
                      :: Data.ProtoLens.FieldDescriptor Content
                lastMod__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "last_mod"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'lastMod")))
                      :: Data.ProtoLens.FieldDescriptor Content
                lastModUsecs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "last_mod_usecs"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'lastModUsecs")))
                      :: Data.ProtoLens.FieldDescriptor Content
                usermeta__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "usermeta"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Pair)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "usermeta")))
                      :: Data.ProtoLens.FieldDescriptor Content
                indexes__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "indexes"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Pair)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "indexes")))
                      :: Data.ProtoLens.FieldDescriptor Content
                deleted__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "deleted"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'deleted")))
                      :: Data.ProtoLens.FieldDescriptor Content
                ttl__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "ttl"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'ttl")))
                      :: Data.ProtoLens.FieldDescriptor Content
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, value__field_descriptor),
                 (Data.ProtoLens.Tag 2, contentType__field_descriptor),
                 (Data.ProtoLens.Tag 3, charset__field_descriptor),
                 (Data.ProtoLens.Tag 4, contentEncoding__field_descriptor),
                 (Data.ProtoLens.Tag 5, vtag__field_descriptor),
                 (Data.ProtoLens.Tag 6, links__field_descriptor),
                 (Data.ProtoLens.Tag 7, lastMod__field_descriptor),
                 (Data.ProtoLens.Tag 8, lastModUsecs__field_descriptor),
                 (Data.ProtoLens.Tag 9, usermeta__field_descriptor),
                 (Data.ProtoLens.Tag 10, indexes__field_descriptor),
                 (Data.ProtoLens.Tag 11, deleted__field_descriptor),
                 (Data.ProtoLens.Tag 12, ttl__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Content'_unknownFields
              (\ x__ y__ -> x__{_Content'_unknownFields = y__})
        defMessage
          = Content{_Content'value = Data.ProtoLens.fieldDefault,
                    _Content'contentType = Prelude.Nothing,
                    _Content'charset = Prelude.Nothing,
                    _Content'contentEncoding = Prelude.Nothing,
                    _Content'vtag = Prelude.Nothing, _Content'links = [],
                    _Content'lastMod = Prelude.Nothing,
                    _Content'lastModUsecs = Prelude.Nothing, _Content'usermeta = [],
                    _Content'indexes = [], _Content'deleted = Prelude.Nothing,
                    _Content'ttl = Prelude.Nothing, _Content'_unknownFields = ([])}
instance Control.DeepSeq.NFData Content where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Content'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Content'value x__)
                   (Control.DeepSeq.deepseq (_Content'contentType x__)
                      (Control.DeepSeq.deepseq (_Content'charset x__)
                         (Control.DeepSeq.deepseq (_Content'contentEncoding x__)
                            (Control.DeepSeq.deepseq (_Content'vtag x__)
                               (Control.DeepSeq.deepseq (_Content'links x__)
                                  (Control.DeepSeq.deepseq (_Content'lastMod x__)
                                     (Control.DeepSeq.deepseq (_Content'lastModUsecs x__)
                                        (Control.DeepSeq.deepseq (_Content'usermeta x__)
                                           (Control.DeepSeq.deepseq (_Content'indexes x__)
                                              (Control.DeepSeq.deepseq (_Content'deleted x__)
                                                 (Control.DeepSeq.deepseq (_Content'ttl x__)
                                                    (())))))))))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.increment' @:: Lens' CounterUpdate Data.Int.Int64@
    * 'Proto.Proto.Riak_Fields.maybe'increment' @:: Lens' CounterUpdate (Prelude.Maybe Data.Int.Int64)@
 -}
data CounterUpdate = CounterUpdate{_CounterUpdate'increment ::
                                   !(Prelude.Maybe Data.Int.Int64),
                                   _CounterUpdate'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CounterUpdate where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' CounterUpdate "increment"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CounterUpdate'increment
                 (\ x__ y__ -> x__{_CounterUpdate'increment = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' CounterUpdate "maybe'increment"
           (Prelude.Maybe Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CounterUpdate'increment
                 (\ x__ y__ -> x__{_CounterUpdate'increment = y__}))
              Prelude.id
instance Data.ProtoLens.Message CounterUpdate where
        messageName _ = Data.Text.pack "CounterUpdate"
        fieldsByTag
          = let increment__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "increment"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'increment")))
                      :: Data.ProtoLens.FieldDescriptor CounterUpdate
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, increment__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _CounterUpdate'_unknownFields
              (\ x__ y__ -> x__{_CounterUpdate'_unknownFields = y__})
        defMessage
          = CounterUpdate{_CounterUpdate'increment = Prelude.Nothing,
                          _CounterUpdate'_unknownFields = ([])}
instance Control.DeepSeq.NFData CounterUpdate where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_CounterUpdate'_unknownFields x__)
                (Control.DeepSeq.deepseq (_CounterUpdate'increment x__) (()))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.counter' @:: Lens' Crdt Data.Int.Int64@
    * 'Proto.Proto.Riak_Fields.maybe'counter' @:: Lens' Crdt (Prelude.Maybe Data.Int.Int64)@
    * 'Proto.Proto.Riak_Fields.set' @:: Lens' Crdt [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.map' @:: Lens' Crdt [MapEntry]@
    * 'Proto.Proto.Riak_Fields.hll' @:: Lens' Crdt Data.Word.Word64@
    * 'Proto.Proto.Riak_Fields.maybe'hll' @:: Lens' Crdt (Prelude.Maybe Data.Word.Word64)@
    * 'Proto.Proto.Riak_Fields.gset' @:: Lens' Crdt [Data.ByteString.ByteString]@
 -}
data Crdt = Crdt{_Crdt'counter :: !(Prelude.Maybe Data.Int.Int64),
                 _Crdt'set :: ![Data.ByteString.ByteString],
                 _Crdt'map :: ![MapEntry],
                 _Crdt'hll :: !(Prelude.Maybe Data.Word.Word64),
                 _Crdt'gset :: ![Data.ByteString.ByteString],
                 _Crdt'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Crdt where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Crdt "counter" (Data.Int.Int64) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'counter
                 (\ x__ y__ -> x__{_Crdt'counter = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Crdt "maybe'counter"
           (Prelude.Maybe Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'counter
                 (\ x__ y__ -> x__{_Crdt'counter = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Crdt "set"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'set
                 (\ x__ y__ -> x__{_Crdt'set = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Crdt "map" ([MapEntry]) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'map
                 (\ x__ y__ -> x__{_Crdt'map = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Crdt "hll" (Data.Word.Word64) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'hll
                 (\ x__ y__ -> x__{_Crdt'hll = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Crdt "maybe'hll"
           (Prelude.Maybe Data.Word.Word64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'hll
                 (\ x__ y__ -> x__{_Crdt'hll = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Crdt "gset"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Crdt'gset
                 (\ x__ y__ -> x__{_Crdt'gset = y__}))
              Prelude.id
instance Data.ProtoLens.Message Crdt where
        messageName _ = Data.Text.pack "Crdt"
        fieldsByTag
          = let counter__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "counter"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'counter")))
                      :: Data.ProtoLens.FieldDescriptor Crdt
                set__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "set"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "set")))
                      :: Data.ProtoLens.FieldDescriptor Crdt
                map__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "map"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapEntry)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "map")))
                      :: Data.ProtoLens.FieldDescriptor Crdt
                hll__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "hll"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hll")))
                      :: Data.ProtoLens.FieldDescriptor Crdt
                gset__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gset"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "gset")))
                      :: Data.ProtoLens.FieldDescriptor Crdt
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, counter__field_descriptor),
                 (Data.ProtoLens.Tag 2, set__field_descriptor),
                 (Data.ProtoLens.Tag 3, map__field_descriptor),
                 (Data.ProtoLens.Tag 4, hll__field_descriptor),
                 (Data.ProtoLens.Tag 5, gset__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Crdt'_unknownFields
              (\ x__ y__ -> x__{_Crdt'_unknownFields = y__})
        defMessage
          = Crdt{_Crdt'counter = Prelude.Nothing, _Crdt'set = [],
                 _Crdt'map = [], _Crdt'hll = Prelude.Nothing, _Crdt'gset = [],
                 _Crdt'_unknownFields = ([])}
instance Control.DeepSeq.NFData Crdt where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Crdt'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Crdt'counter x__)
                   (Control.DeepSeq.deepseq (_Crdt'set x__)
                      (Control.DeepSeq.deepseq (_Crdt'map x__)
                         (Control.DeepSeq.deepseq (_Crdt'hll x__)
                            (Control.DeepSeq.deepseq (_Crdt'gset x__) (()))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.counter' @:: Lens' CrdtUpdate CounterUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'counter' @:: Lens' CrdtUpdate (Prelude.Maybe CounterUpdate)@
    * 'Proto.Proto.Riak_Fields.set' @:: Lens' CrdtUpdate SetUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'set' @:: Lens' CrdtUpdate (Prelude.Maybe SetUpdate)@
    * 'Proto.Proto.Riak_Fields.map' @:: Lens' CrdtUpdate MapUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'map' @:: Lens' CrdtUpdate (Prelude.Maybe MapUpdate)@
    * 'Proto.Proto.Riak_Fields.hll' @:: Lens' CrdtUpdate HllOp@
    * 'Proto.Proto.Riak_Fields.maybe'hll' @:: Lens' CrdtUpdate (Prelude.Maybe HllOp)@
    * 'Proto.Proto.Riak_Fields.gset' @:: Lens' CrdtUpdate GSetOp@
    * 'Proto.Proto.Riak_Fields.maybe'gset' @:: Lens' CrdtUpdate (Prelude.Maybe GSetOp)@
 -}
data CrdtUpdate = CrdtUpdate{_CrdtUpdate'counter ::
                             !(Prelude.Maybe CounterUpdate),
                             _CrdtUpdate'set :: !(Prelude.Maybe SetUpdate),
                             _CrdtUpdate'map :: !(Prelude.Maybe MapUpdate),
                             _CrdtUpdate'hll :: !(Prelude.Maybe HllOp),
                             _CrdtUpdate'gset :: !(Prelude.Maybe GSetOp),
                             _CrdtUpdate'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CrdtUpdate where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' CrdtUpdate "counter" (CounterUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'counter
                 (\ x__ y__ -> x__{_CrdtUpdate'counter = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' CrdtUpdate "maybe'counter"
           (Prelude.Maybe CounterUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'counter
                 (\ x__ y__ -> x__{_CrdtUpdate'counter = y__}))
              Prelude.id
instance Lens.Labels.HasLens' CrdtUpdate "set" (SetUpdate) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'set
                 (\ x__ y__ -> x__{_CrdtUpdate'set = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' CrdtUpdate "maybe'set"
           (Prelude.Maybe SetUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'set
                 (\ x__ y__ -> x__{_CrdtUpdate'set = y__}))
              Prelude.id
instance Lens.Labels.HasLens' CrdtUpdate "map" (MapUpdate) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'map
                 (\ x__ y__ -> x__{_CrdtUpdate'map = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' CrdtUpdate "maybe'map"
           (Prelude.Maybe MapUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'map
                 (\ x__ y__ -> x__{_CrdtUpdate'map = y__}))
              Prelude.id
instance Lens.Labels.HasLens' CrdtUpdate "hll" (HllOp) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'hll
                 (\ x__ y__ -> x__{_CrdtUpdate'hll = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' CrdtUpdate "maybe'hll"
           (Prelude.Maybe HllOp)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'hll
                 (\ x__ y__ -> x__{_CrdtUpdate'hll = y__}))
              Prelude.id
instance Lens.Labels.HasLens' CrdtUpdate "gset" (GSetOp) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'gset
                 (\ x__ y__ -> x__{_CrdtUpdate'gset = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' CrdtUpdate "maybe'gset"
           (Prelude.Maybe GSetOp)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _CrdtUpdate'gset
                 (\ x__ y__ -> x__{_CrdtUpdate'gset = y__}))
              Prelude.id
instance Data.ProtoLens.Message CrdtUpdate where
        messageName _ = Data.Text.pack "CrdtUpdate"
        fieldsByTag
          = let counter__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "counter"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor CounterUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'counter")))
                      :: Data.ProtoLens.FieldDescriptor CrdtUpdate
                set__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "set"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor SetUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'set")))
                      :: Data.ProtoLens.FieldDescriptor CrdtUpdate
                map__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "map"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'map")))
                      :: Data.ProtoLens.FieldDescriptor CrdtUpdate
                hll__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "hll"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor HllOp)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hll")))
                      :: Data.ProtoLens.FieldDescriptor CrdtUpdate
                gset__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gset"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor GSetOp)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'gset")))
                      :: Data.ProtoLens.FieldDescriptor CrdtUpdate
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, counter__field_descriptor),
                 (Data.ProtoLens.Tag 2, set__field_descriptor),
                 (Data.ProtoLens.Tag 3, map__field_descriptor),
                 (Data.ProtoLens.Tag 4, hll__field_descriptor),
                 (Data.ProtoLens.Tag 5, gset__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _CrdtUpdate'_unknownFields
              (\ x__ y__ -> x__{_CrdtUpdate'_unknownFields = y__})
        defMessage
          = CrdtUpdate{_CrdtUpdate'counter = Prelude.Nothing,
                       _CrdtUpdate'set = Prelude.Nothing,
                       _CrdtUpdate'map = Prelude.Nothing,
                       _CrdtUpdate'hll = Prelude.Nothing,
                       _CrdtUpdate'gset = Prelude.Nothing,
                       _CrdtUpdate'_unknownFields = ([])}
instance Control.DeepSeq.NFData CrdtUpdate where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_CrdtUpdate'_unknownFields x__)
                (Control.DeepSeq.deepseq (_CrdtUpdate'counter x__)
                   (Control.DeepSeq.deepseq (_CrdtUpdate'set x__)
                      (Control.DeepSeq.deepseq (_CrdtUpdate'map x__)
                         (Control.DeepSeq.deepseq (_CrdtUpdate'hll x__)
                            (Control.DeepSeq.deepseq (_CrdtUpdate'gset x__) (()))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' DeleteRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' DeleteRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.rw' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'rw' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.vclock' @:: Lens' DeleteRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'vclock' @:: Lens' DeleteRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.r' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'r' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.w' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'w' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pr' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pr' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pw' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pw' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.dw' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'dw' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.sloppyQuorum' @:: Lens' DeleteRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'sloppyQuorum' @:: Lens' DeleteRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.n' @:: Lens' DeleteRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'n' @:: Lens' DeleteRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' DeleteRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' DeleteRequest (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data DeleteRequest = DeleteRequest{_DeleteRequest'bucket ::
                                   !Data.ByteString.ByteString,
                                   _DeleteRequest'key :: !Data.ByteString.ByteString,
                                   _DeleteRequest'rw :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'vclock ::
                                   !(Prelude.Maybe Data.ByteString.ByteString),
                                   _DeleteRequest'r :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'w :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'pr :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'pw :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'dw :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'timeout :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
                                   _DeleteRequest'n :: !(Prelude.Maybe Data.Word.Word32),
                                   _DeleteRequest'type' ::
                                   !(Prelude.Maybe Data.ByteString.ByteString),
                                   _DeleteRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DeleteRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'bucket
                 (\ x__ y__ -> x__{_DeleteRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'key
                 (\ x__ y__ -> x__{_DeleteRequest'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "rw" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'rw
                 (\ x__ y__ -> x__{_DeleteRequest'rw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'rw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'rw
                 (\ x__ y__ -> x__{_DeleteRequest'rw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "vclock"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'vclock
                 (\ x__ y__ -> x__{_DeleteRequest'vclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'vclock"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'vclock
                 (\ x__ y__ -> x__{_DeleteRequest'vclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "r" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'r
                 (\ x__ y__ -> x__{_DeleteRequest'r = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'r"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'r
                 (\ x__ y__ -> x__{_DeleteRequest'r = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "w" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'w
                 (\ x__ y__ -> x__{_DeleteRequest'w = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'w"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'w
                 (\ x__ y__ -> x__{_DeleteRequest'w = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "pr" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'pr
                 (\ x__ y__ -> x__{_DeleteRequest'pr = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'pr"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'pr
                 (\ x__ y__ -> x__{_DeleteRequest'pr = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "pw" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'pw
                 (\ x__ y__ -> x__{_DeleteRequest'pw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'pw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'pw
                 (\ x__ y__ -> x__{_DeleteRequest'pw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "dw" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'dw
                 (\ x__ y__ -> x__{_DeleteRequest'dw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'dw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'dw
                 (\ x__ y__ -> x__{_DeleteRequest'dw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'timeout
                 (\ x__ y__ -> x__{_DeleteRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'timeout
                 (\ x__ y__ -> x__{_DeleteRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "sloppyQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_DeleteRequest'sloppyQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'sloppyQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_DeleteRequest'sloppyQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "n" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'n
                 (\ x__ y__ -> x__{_DeleteRequest'n = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'n"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'n
                 (\ x__ y__ -> x__{_DeleteRequest'n = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeleteRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'type'
                 (\ x__ y__ -> x__{_DeleteRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' DeleteRequest "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeleteRequest'type'
                 (\ x__ y__ -> x__{_DeleteRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message DeleteRequest where
        messageName _ = Data.Text.pack "DeleteRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                rw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "rw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rw")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                vclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'vclock")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                r__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "r"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'r")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                w__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "w"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'w")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                pr__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pr"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pr")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                pw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pw")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                dw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dw")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                sloppyQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sloppy_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'sloppyQuorum")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                n__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "n"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor DeleteRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, key__field_descriptor),
                 (Data.ProtoLens.Tag 3, rw__field_descriptor),
                 (Data.ProtoLens.Tag 4, vclock__field_descriptor),
                 (Data.ProtoLens.Tag 5, r__field_descriptor),
                 (Data.ProtoLens.Tag 6, w__field_descriptor),
                 (Data.ProtoLens.Tag 7, pr__field_descriptor),
                 (Data.ProtoLens.Tag 8, pw__field_descriptor),
                 (Data.ProtoLens.Tag 9, dw__field_descriptor),
                 (Data.ProtoLens.Tag 10, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 11, sloppyQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 12, n__field_descriptor),
                 (Data.ProtoLens.Tag 13, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DeleteRequest'_unknownFields
              (\ x__ y__ -> x__{_DeleteRequest'_unknownFields = y__})
        defMessage
          = DeleteRequest{_DeleteRequest'bucket =
                            Data.ProtoLens.fieldDefault,
                          _DeleteRequest'key = Data.ProtoLens.fieldDefault,
                          _DeleteRequest'rw = Prelude.Nothing,
                          _DeleteRequest'vclock = Prelude.Nothing,
                          _DeleteRequest'r = Prelude.Nothing,
                          _DeleteRequest'w = Prelude.Nothing,
                          _DeleteRequest'pr = Prelude.Nothing,
                          _DeleteRequest'pw = Prelude.Nothing,
                          _DeleteRequest'dw = Prelude.Nothing,
                          _DeleteRequest'timeout = Prelude.Nothing,
                          _DeleteRequest'sloppyQuorum = Prelude.Nothing,
                          _DeleteRequest'n = Prelude.Nothing,
                          _DeleteRequest'type' = Prelude.Nothing,
                          _DeleteRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData DeleteRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DeleteRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DeleteRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_DeleteRequest'key x__)
                      (Control.DeepSeq.deepseq (_DeleteRequest'rw x__)
                         (Control.DeepSeq.deepseq (_DeleteRequest'vclock x__)
                            (Control.DeepSeq.deepseq (_DeleteRequest'r x__)
                               (Control.DeepSeq.deepseq (_DeleteRequest'w x__)
                                  (Control.DeepSeq.deepseq (_DeleteRequest'pr x__)
                                     (Control.DeepSeq.deepseq (_DeleteRequest'pw x__)
                                        (Control.DeepSeq.deepseq (_DeleteRequest'dw x__)
                                           (Control.DeepSeq.deepseq (_DeleteRequest'timeout x__)
                                              (Control.DeepSeq.deepseq
                                                 (_DeleteRequest'sloppyQuorum x__)
                                                 (Control.DeepSeq.deepseq (_DeleteRequest'n x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_DeleteRequest'type' x__)
                                                       (()))))))))))))))
{- | Fields :

 -}
data DeleteResponse = DeleteResponse{_DeleteResponse'_unknownFields
                                     :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message DeleteResponse where
        messageName _ = Data.Text.pack "DeleteResponse"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens _DeleteResponse'_unknownFields
              (\ x__ y__ -> x__{_DeleteResponse'_unknownFields = y__})
        defMessage = DeleteResponse{_DeleteResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData DeleteResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DeleteResponse'_unknownFields x__) (())
{- | Fields :

 -}
data EmptyPutResponse = EmptyPutResponse{_EmptyPutResponse'_unknownFields
                                         :: !Data.ProtoLens.FieldSet}
                          deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show EmptyPutResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message EmptyPutResponse where
        messageName _ = Data.Text.pack "EmptyPutResponse"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens _EmptyPutResponse'_unknownFields
              (\ x__ y__ -> x__{_EmptyPutResponse'_unknownFields = y__})
        defMessage
          = EmptyPutResponse{_EmptyPutResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData EmptyPutResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_EmptyPutResponse'_unknownFields x__) (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.errmsg' @:: Lens' ErrorResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.code' @:: Lens' ErrorResponse Data.Word.Word32@
 -}
data ErrorResponse = ErrorResponse{_ErrorResponse'errmsg ::
                                   !Data.ByteString.ByteString,
                                   _ErrorResponse'code :: !Data.Word.Word32,
                                   _ErrorResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ErrorResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ErrorResponse "errmsg"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ErrorResponse'errmsg
                 (\ x__ y__ -> x__{_ErrorResponse'errmsg = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ErrorResponse "code"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ErrorResponse'code
                 (\ x__ y__ -> x__{_ErrorResponse'code = y__}))
              Prelude.id
instance Data.ProtoLens.Message ErrorResponse where
        messageName _ = Data.Text.pack "ErrorResponse"
        fieldsByTag
          = let errmsg__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "errmsg"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errmsg")))
                      :: Data.ProtoLens.FieldDescriptor ErrorResponse
                code__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "code"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "code")))
                      :: Data.ProtoLens.FieldDescriptor ErrorResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, errmsg__field_descriptor),
                 (Data.ProtoLens.Tag 2, code__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ErrorResponse'_unknownFields
              (\ x__ y__ -> x__{_ErrorResponse'_unknownFields = y__})
        defMessage
          = ErrorResponse{_ErrorResponse'errmsg =
                            Data.ProtoLens.fieldDefault,
                          _ErrorResponse'code = Data.ProtoLens.fieldDefault,
                          _ErrorResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData ErrorResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_ErrorResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ErrorResponse'errmsg x__)
                   (Control.DeepSeq.deepseq (_ErrorResponse'code x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.adds' @:: Lens' GSetOp [Data.ByteString.ByteString]@
 -}
data GSetOp = GSetOp{_GSetOp'adds :: ![Data.ByteString.ByteString],
                     _GSetOp'_unknownFields :: !Data.ProtoLens.FieldSet}
                deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GSetOp where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GSetOp "adds"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GSetOp'adds
                 (\ x__ y__ -> x__{_GSetOp'adds = y__}))
              Prelude.id
instance Data.ProtoLens.Message GSetOp where
        messageName _ = Data.Text.pack "GSetOp"
        fieldsByTag
          = let adds__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "adds"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "adds")))
                      :: Data.ProtoLens.FieldDescriptor GSetOp
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, adds__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GSetOp'_unknownFields
              (\ x__ y__ -> x__{_GSetOp'_unknownFields = y__})
        defMessage
          = GSetOp{_GSetOp'adds = [], _GSetOp'_unknownFields = ([])}
instance Control.DeepSeq.NFData GSetOp where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GSetOp'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GSetOp'adds x__) (()))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' GetBucketPropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' GetBucketPropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' GetBucketPropertiesRequest
  (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data GetBucketPropertiesRequest = GetBucketPropertiesRequest{_GetBucketPropertiesRequest'bucket
                                                             :: !Data.ByteString.ByteString,
                                                             _GetBucketPropertiesRequest'type' ::
                                                             !(Prelude.Maybe
                                                                 Data.ByteString.ByteString),
                                                             _GetBucketPropertiesRequest'_unknownFields
                                                             :: !Data.ProtoLens.FieldSet}
                                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetBucketPropertiesRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetBucketPropertiesRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetBucketPropertiesRequest'bucket
                 (\ x__ y__ -> x__{_GetBucketPropertiesRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetBucketPropertiesRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetBucketPropertiesRequest'type'
                 (\ x__ y__ -> x__{_GetBucketPropertiesRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetBucketPropertiesRequest
           "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetBucketPropertiesRequest'type'
                 (\ x__ y__ -> x__{_GetBucketPropertiesRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetBucketPropertiesRequest where
        messageName _ = Data.Text.pack "GetBucketPropertiesRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor GetBucketPropertiesRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor GetBucketPropertiesRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _GetBucketPropertiesRequest'_unknownFields
              (\ x__ y__ ->
                 x__{_GetBucketPropertiesRequest'_unknownFields = y__})
        defMessage
          = GetBucketPropertiesRequest{_GetBucketPropertiesRequest'bucket =
                                         Data.ProtoLens.fieldDefault,
                                       _GetBucketPropertiesRequest'type' = Prelude.Nothing,
                                       _GetBucketPropertiesRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetBucketPropertiesRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_GetBucketPropertiesRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetBucketPropertiesRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_GetBucketPropertiesRequest'type' x__)
                      (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.props' @:: Lens' GetBucketPropertiesResponse BucketProperties@
 -}
data GetBucketPropertiesResponse = GetBucketPropertiesResponse{_GetBucketPropertiesResponse'props
                                                               :: !BucketProperties,
                                                               _GetBucketPropertiesResponse'_unknownFields
                                                               :: !Data.ProtoLens.FieldSet}
                                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetBucketPropertiesResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetBucketPropertiesResponse "props"
           (BucketProperties)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetBucketPropertiesResponse'props
                 (\ x__ y__ -> x__{_GetBucketPropertiesResponse'props = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetBucketPropertiesResponse where
        messageName _ = Data.Text.pack "GetBucketPropertiesResponse"
        fieldsByTag
          = let props__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "props"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor BucketProperties)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "props")))
                      :: Data.ProtoLens.FieldDescriptor GetBucketPropertiesResponse
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, props__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _GetBucketPropertiesResponse'_unknownFields
              (\ x__ y__ ->
                 x__{_GetBucketPropertiesResponse'_unknownFields = y__})
        defMessage
          = GetBucketPropertiesResponse{_GetBucketPropertiesResponse'props =
                                          Data.ProtoLens.defMessage,
                                        _GetBucketPropertiesResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetBucketPropertiesResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_GetBucketPropertiesResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetBucketPropertiesResponse'props x__)
                   (()))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' GetBucketTypePropertiesRequest Data.ByteString.ByteString@
 -}
data GetBucketTypePropertiesRequest = GetBucketTypePropertiesRequest{_GetBucketTypePropertiesRequest'type'
                                                                     :: !Data.ByteString.ByteString,
                                                                     _GetBucketTypePropertiesRequest'_unknownFields
                                                                     :: !Data.ProtoLens.FieldSet}
                                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetBucketTypePropertiesRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetBucketTypePropertiesRequest
           "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetBucketTypePropertiesRequest'type'
                 (\ x__ y__ -> x__{_GetBucketTypePropertiesRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetBucketTypePropertiesRequest
         where
        messageName _ = Data.Text.pack "GetBucketTypePropertiesRequest"
        fieldsByTag
          = let type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor GetBucketTypePropertiesRequest
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _GetBucketTypePropertiesRequest'_unknownFields
              (\ x__ y__ ->
                 x__{_GetBucketTypePropertiesRequest'_unknownFields = y__})
        defMessage
          = GetBucketTypePropertiesRequest{_GetBucketTypePropertiesRequest'type'
                                             = Data.ProtoLens.fieldDefault,
                                           _GetBucketTypePropertiesRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetBucketTypePropertiesRequest
         where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_GetBucketTypePropertiesRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq
                   (_GetBucketTypePropertiesRequest'type' x__)
                   (()))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' GetCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' GetCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' GetCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.r' @:: Lens' GetCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'r' @:: Lens' GetCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pr' @:: Lens' GetCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pr' @:: Lens' GetCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.basicQuorum' @:: Lens' GetCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'basicQuorum' @:: Lens' GetCrdtRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.notfoundOk' @:: Lens' GetCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'notfoundOk' @:: Lens' GetCrdtRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' GetCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' GetCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.sloppyQuorum' @:: Lens' GetCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'sloppyQuorum' @:: Lens' GetCrdtRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.n' @:: Lens' GetCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'n' @:: Lens' GetCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.includeContext' @:: Lens' GetCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'includeContext' @:: Lens' GetCrdtRequest (Prelude.Maybe Prelude.Bool)@
 -}
data GetCrdtRequest = GetCrdtRequest{_GetCrdtRequest'bucket ::
                                     !Data.ByteString.ByteString,
                                     _GetCrdtRequest'key :: !Data.ByteString.ByteString,
                                     _GetCrdtRequest'type' :: !Data.ByteString.ByteString,
                                     _GetCrdtRequest'r :: !(Prelude.Maybe Data.Word.Word32),
                                     _GetCrdtRequest'pr :: !(Prelude.Maybe Data.Word.Word32),
                                     _GetCrdtRequest'basicQuorum :: !(Prelude.Maybe Prelude.Bool),
                                     _GetCrdtRequest'notfoundOk :: !(Prelude.Maybe Prelude.Bool),
                                     _GetCrdtRequest'timeout :: !(Prelude.Maybe Data.Word.Word32),
                                     _GetCrdtRequest'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
                                     _GetCrdtRequest'n :: !(Prelude.Maybe Data.Word.Word32),
                                     _GetCrdtRequest'includeContext ::
                                     !(Prelude.Maybe Prelude.Bool),
                                     _GetCrdtRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetCrdtRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetCrdtRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'bucket
                 (\ x__ y__ -> x__{_GetCrdtRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'key
                 (\ x__ y__ -> x__{_GetCrdtRequest'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'type'
                 (\ x__ y__ -> x__{_GetCrdtRequest'type' = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "r" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'r
                 (\ x__ y__ -> x__{_GetCrdtRequest'r = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'r"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'r
                 (\ x__ y__ -> x__{_GetCrdtRequest'r = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "pr"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'pr
                 (\ x__ y__ -> x__{_GetCrdtRequest'pr = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'pr"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'pr
                 (\ x__ y__ -> x__{_GetCrdtRequest'pr = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "basicQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'basicQuorum
                 (\ x__ y__ -> x__{_GetCrdtRequest'basicQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'basicQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'basicQuorum
                 (\ x__ y__ -> x__{_GetCrdtRequest'basicQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "notfoundOk"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'notfoundOk
                 (\ x__ y__ -> x__{_GetCrdtRequest'notfoundOk = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'notfoundOk"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'notfoundOk
                 (\ x__ y__ -> x__{_GetCrdtRequest'notfoundOk = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'timeout
                 (\ x__ y__ -> x__{_GetCrdtRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'timeout
                 (\ x__ y__ -> x__{_GetCrdtRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "sloppyQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_GetCrdtRequest'sloppyQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'sloppyQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_GetCrdtRequest'sloppyQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "n" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'n
                 (\ x__ y__ -> x__{_GetCrdtRequest'n = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'n"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'n
                 (\ x__ y__ -> x__{_GetCrdtRequest'n = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtRequest "includeContext"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'includeContext
                 (\ x__ y__ -> x__{_GetCrdtRequest'includeContext = y__}))
              (Data.ProtoLens.maybeLens Prelude.True)
instance Lens.Labels.HasLens' GetCrdtRequest "maybe'includeContext"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtRequest'includeContext
                 (\ x__ y__ -> x__{_GetCrdtRequest'includeContext = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetCrdtRequest where
        messageName _ = Data.Text.pack "GetCrdtRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                r__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "r"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'r")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                pr__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pr"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pr")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                basicQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "basic_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'basicQuorum")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                notfoundOk__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "notfound_ok"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'notfoundOk")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                sloppyQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sloppy_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'sloppyQuorum")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                n__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "n"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
                includeContext__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "include_context"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'includeContext")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, key__field_descriptor),
                 (Data.ProtoLens.Tag 3, type'__field_descriptor),
                 (Data.ProtoLens.Tag 4, r__field_descriptor),
                 (Data.ProtoLens.Tag 5, pr__field_descriptor),
                 (Data.ProtoLens.Tag 6, basicQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 7, notfoundOk__field_descriptor),
                 (Data.ProtoLens.Tag 8, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 9, sloppyQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 10, n__field_descriptor),
                 (Data.ProtoLens.Tag 11, includeContext__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GetCrdtRequest'_unknownFields
              (\ x__ y__ -> x__{_GetCrdtRequest'_unknownFields = y__})
        defMessage
          = GetCrdtRequest{_GetCrdtRequest'bucket =
                             Data.ProtoLens.fieldDefault,
                           _GetCrdtRequest'key = Data.ProtoLens.fieldDefault,
                           _GetCrdtRequest'type' = Data.ProtoLens.fieldDefault,
                           _GetCrdtRequest'r = Prelude.Nothing,
                           _GetCrdtRequest'pr = Prelude.Nothing,
                           _GetCrdtRequest'basicQuorum = Prelude.Nothing,
                           _GetCrdtRequest'notfoundOk = Prelude.Nothing,
                           _GetCrdtRequest'timeout = Prelude.Nothing,
                           _GetCrdtRequest'sloppyQuorum = Prelude.Nothing,
                           _GetCrdtRequest'n = Prelude.Nothing,
                           _GetCrdtRequest'includeContext = Prelude.Nothing,
                           _GetCrdtRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetCrdtRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GetCrdtRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetCrdtRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_GetCrdtRequest'key x__)
                      (Control.DeepSeq.deepseq (_GetCrdtRequest'type' x__)
                         (Control.DeepSeq.deepseq (_GetCrdtRequest'r x__)
                            (Control.DeepSeq.deepseq (_GetCrdtRequest'pr x__)
                               (Control.DeepSeq.deepseq (_GetCrdtRequest'basicQuorum x__)
                                  (Control.DeepSeq.deepseq (_GetCrdtRequest'notfoundOk x__)
                                     (Control.DeepSeq.deepseq (_GetCrdtRequest'timeout x__)
                                        (Control.DeepSeq.deepseq (_GetCrdtRequest'sloppyQuorum x__)
                                           (Control.DeepSeq.deepseq (_GetCrdtRequest'n x__)
                                              (Control.DeepSeq.deepseq
                                                 (_GetCrdtRequest'includeContext x__)
                                                 (()))))))))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.context' @:: Lens' GetCrdtResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'context' @:: Lens' GetCrdtResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' GetCrdtResponse GetCrdtResponse'CrdtType@
    * 'Proto.Proto.Riak_Fields.value' @:: Lens' GetCrdtResponse Crdt@
    * 'Proto.Proto.Riak_Fields.maybe'value' @:: Lens' GetCrdtResponse (Prelude.Maybe Crdt)@
 -}
data GetCrdtResponse = GetCrdtResponse{_GetCrdtResponse'context ::
                                       !(Prelude.Maybe Data.ByteString.ByteString),
                                       _GetCrdtResponse'type' :: !GetCrdtResponse'CrdtType,
                                       _GetCrdtResponse'value :: !(Prelude.Maybe Crdt),
                                       _GetCrdtResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                         deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetCrdtResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetCrdtResponse "context"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtResponse'context
                 (\ x__ y__ -> x__{_GetCrdtResponse'context = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetCrdtResponse "maybe'context"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtResponse'context
                 (\ x__ y__ -> x__{_GetCrdtResponse'context = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtResponse "type'"
           (GetCrdtResponse'CrdtType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtResponse'type'
                 (\ x__ y__ -> x__{_GetCrdtResponse'type' = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetCrdtResponse "value" (Crdt) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtResponse'value
                 (\ x__ y__ -> x__{_GetCrdtResponse'value = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' GetCrdtResponse "maybe'value"
           (Prelude.Maybe Crdt)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetCrdtResponse'value
                 (\ x__ y__ -> x__{_GetCrdtResponse'value = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetCrdtResponse where
        messageName _ = Data.Text.pack "GetCrdtResponse"
        fieldsByTag
          = let context__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "context"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'context")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtResponse
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor GetCrdtResponse'CrdtType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtResponse
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Crdt)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'value")))
                      :: Data.ProtoLens.FieldDescriptor GetCrdtResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, context__field_descriptor),
                 (Data.ProtoLens.Tag 2, type'__field_descriptor),
                 (Data.ProtoLens.Tag 3, value__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GetCrdtResponse'_unknownFields
              (\ x__ y__ -> x__{_GetCrdtResponse'_unknownFields = y__})
        defMessage
          = GetCrdtResponse{_GetCrdtResponse'context = Prelude.Nothing,
                            _GetCrdtResponse'type' = Data.ProtoLens.fieldDefault,
                            _GetCrdtResponse'value = Prelude.Nothing,
                            _GetCrdtResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetCrdtResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GetCrdtResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetCrdtResponse'context x__)
                   (Control.DeepSeq.deepseq (_GetCrdtResponse'type' x__)
                      (Control.DeepSeq.deepseq (_GetCrdtResponse'value x__) (()))))
data GetCrdtResponse'CrdtType = GetCrdtResponse'COUNTER
                              | GetCrdtResponse'SET
                              | GetCrdtResponse'MAP
                              | GetCrdtResponse'HLL
                              | GetCrdtResponse'GSET
                                  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum GetCrdtResponse'CrdtType where
        maybeToEnum 1 = Prelude.Just GetCrdtResponse'COUNTER
        maybeToEnum 2 = Prelude.Just GetCrdtResponse'SET
        maybeToEnum 3 = Prelude.Just GetCrdtResponse'MAP
        maybeToEnum 4 = Prelude.Just GetCrdtResponse'HLL
        maybeToEnum 5 = Prelude.Just GetCrdtResponse'GSET
        maybeToEnum _ = Prelude.Nothing
        showEnum GetCrdtResponse'COUNTER = "COUNTER"
        showEnum GetCrdtResponse'SET = "SET"
        showEnum GetCrdtResponse'MAP = "MAP"
        showEnum GetCrdtResponse'HLL = "HLL"
        showEnum GetCrdtResponse'GSET = "GSET"
        readEnum k
          | (Prelude.==) k "COUNTER" = Prelude.Just GetCrdtResponse'COUNTER
          | (Prelude.==) k "SET" = Prelude.Just GetCrdtResponse'SET
          | (Prelude.==) k "MAP" = Prelude.Just GetCrdtResponse'MAP
          | (Prelude.==) k "HLL" = Prelude.Just GetCrdtResponse'HLL
          | (Prelude.==) k "GSET" = Prelude.Just GetCrdtResponse'GSET
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded GetCrdtResponse'CrdtType where
        minBound = GetCrdtResponse'COUNTER
        maxBound = GetCrdtResponse'GSET
instance Prelude.Enum GetCrdtResponse'CrdtType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum CrdtType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum GetCrdtResponse'COUNTER = 1
        fromEnum GetCrdtResponse'SET = 2
        fromEnum GetCrdtResponse'MAP = 3
        fromEnum GetCrdtResponse'HLL = 4
        fromEnum GetCrdtResponse'GSET = 5
        succ GetCrdtResponse'GSET
          = Prelude.error
              "GetCrdtResponse'CrdtType.succ: bad argument GetCrdtResponse'GSET. This value would be out of bounds."
        succ GetCrdtResponse'COUNTER = GetCrdtResponse'SET
        succ GetCrdtResponse'SET = GetCrdtResponse'MAP
        succ GetCrdtResponse'MAP = GetCrdtResponse'HLL
        succ GetCrdtResponse'HLL = GetCrdtResponse'GSET
        pred GetCrdtResponse'COUNTER
          = Prelude.error
              "GetCrdtResponse'CrdtType.pred: bad argument GetCrdtResponse'COUNTER. This value would be out of bounds."
        pred GetCrdtResponse'SET = GetCrdtResponse'COUNTER
        pred GetCrdtResponse'MAP = GetCrdtResponse'SET
        pred GetCrdtResponse'HLL = GetCrdtResponse'MAP
        pred GetCrdtResponse'GSET = GetCrdtResponse'HLL
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault GetCrdtResponse'CrdtType where
        fieldDefault = GetCrdtResponse'COUNTER
instance Control.DeepSeq.NFData GetCrdtResponse'CrdtType where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' GetRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' GetRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.r' @:: Lens' GetRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'r' @:: Lens' GetRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pr' @:: Lens' GetRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pr' @:: Lens' GetRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.basicQuorum' @:: Lens' GetRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'basicQuorum' @:: Lens' GetRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.notfoundOk' @:: Lens' GetRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'notfoundOk' @:: Lens' GetRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.ifModified' @:: Lens' GetRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'ifModified' @:: Lens' GetRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.head' @:: Lens' GetRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'head' @:: Lens' GetRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.deletedvclock' @:: Lens' GetRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'deletedvclock' @:: Lens' GetRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' GetRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' GetRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.sloppyQuorum' @:: Lens' GetRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'sloppyQuorum' @:: Lens' GetRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.n' @:: Lens' GetRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'n' @:: Lens' GetRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' GetRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' GetRequest (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data GetRequest = GetRequest{_GetRequest'bucket ::
                             !Data.ByteString.ByteString,
                             _GetRequest'key :: !Data.ByteString.ByteString,
                             _GetRequest'r :: !(Prelude.Maybe Data.Word.Word32),
                             _GetRequest'pr :: !(Prelude.Maybe Data.Word.Word32),
                             _GetRequest'basicQuorum :: !(Prelude.Maybe Prelude.Bool),
                             _GetRequest'notfoundOk :: !(Prelude.Maybe Prelude.Bool),
                             _GetRequest'ifModified ::
                             !(Prelude.Maybe Data.ByteString.ByteString),
                             _GetRequest'head :: !(Prelude.Maybe Prelude.Bool),
                             _GetRequest'deletedvclock :: !(Prelude.Maybe Prelude.Bool),
                             _GetRequest'timeout :: !(Prelude.Maybe Data.Word.Word32),
                             _GetRequest'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
                             _GetRequest'n :: !(Prelude.Maybe Data.Word.Word32),
                             _GetRequest'type' :: !(Prelude.Maybe Data.ByteString.ByteString),
                             _GetRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'bucket
                 (\ x__ y__ -> x__{_GetRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'key
                 (\ x__ y__ -> x__{_GetRequest'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "r" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'r
                 (\ x__ y__ -> x__{_GetRequest'r = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'r"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'r
                 (\ x__ y__ -> x__{_GetRequest'r = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "pr" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'pr
                 (\ x__ y__ -> x__{_GetRequest'pr = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'pr"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'pr
                 (\ x__ y__ -> x__{_GetRequest'pr = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "basicQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'basicQuorum
                 (\ x__ y__ -> x__{_GetRequest'basicQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'basicQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'basicQuorum
                 (\ x__ y__ -> x__{_GetRequest'basicQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "notfoundOk"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'notfoundOk
                 (\ x__ y__ -> x__{_GetRequest'notfoundOk = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'notfoundOk"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'notfoundOk
                 (\ x__ y__ -> x__{_GetRequest'notfoundOk = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "ifModified"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'ifModified
                 (\ x__ y__ -> x__{_GetRequest'ifModified = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'ifModified"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'ifModified
                 (\ x__ y__ -> x__{_GetRequest'ifModified = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "head" (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'head
                 (\ x__ y__ -> x__{_GetRequest'head = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'head"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'head
                 (\ x__ y__ -> x__{_GetRequest'head = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "deletedvclock"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'deletedvclock
                 (\ x__ y__ -> x__{_GetRequest'deletedvclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'deletedvclock"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'deletedvclock
                 (\ x__ y__ -> x__{_GetRequest'deletedvclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'timeout
                 (\ x__ y__ -> x__{_GetRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'timeout
                 (\ x__ y__ -> x__{_GetRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "sloppyQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_GetRequest'sloppyQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'sloppyQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_GetRequest'sloppyQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "n" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'n
                 (\ x__ y__ -> x__{_GetRequest'n = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'n"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'n
                 (\ x__ y__ -> x__{_GetRequest'n = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'type'
                 (\ x__ y__ -> x__{_GetRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetRequest "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetRequest'type'
                 (\ x__ y__ -> x__{_GetRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetRequest where
        messageName _ = Data.Text.pack "GetRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                r__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "r"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'r")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                pr__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pr"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pr")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                basicQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "basic_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'basicQuorum")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                notfoundOk__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "notfound_ok"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'notfoundOk")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                ifModified__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "if_modified"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'ifModified")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                head__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "head"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'head")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                deletedvclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "deletedvclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'deletedvclock")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                sloppyQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sloppy_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'sloppyQuorum")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                n__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "n"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor GetRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, key__field_descriptor),
                 (Data.ProtoLens.Tag 3, r__field_descriptor),
                 (Data.ProtoLens.Tag 4, pr__field_descriptor),
                 (Data.ProtoLens.Tag 5, basicQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 6, notfoundOk__field_descriptor),
                 (Data.ProtoLens.Tag 7, ifModified__field_descriptor),
                 (Data.ProtoLens.Tag 8, head__field_descriptor),
                 (Data.ProtoLens.Tag 9, deletedvclock__field_descriptor),
                 (Data.ProtoLens.Tag 10, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 11, sloppyQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 12, n__field_descriptor),
                 (Data.ProtoLens.Tag 13, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GetRequest'_unknownFields
              (\ x__ y__ -> x__{_GetRequest'_unknownFields = y__})
        defMessage
          = GetRequest{_GetRequest'bucket = Data.ProtoLens.fieldDefault,
                       _GetRequest'key = Data.ProtoLens.fieldDefault,
                       _GetRequest'r = Prelude.Nothing, _GetRequest'pr = Prelude.Nothing,
                       _GetRequest'basicQuorum = Prelude.Nothing,
                       _GetRequest'notfoundOk = Prelude.Nothing,
                       _GetRequest'ifModified = Prelude.Nothing,
                       _GetRequest'head = Prelude.Nothing,
                       _GetRequest'deletedvclock = Prelude.Nothing,
                       _GetRequest'timeout = Prelude.Nothing,
                       _GetRequest'sloppyQuorum = Prelude.Nothing,
                       _GetRequest'n = Prelude.Nothing,
                       _GetRequest'type' = Prelude.Nothing,
                       _GetRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GetRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_GetRequest'key x__)
                      (Control.DeepSeq.deepseq (_GetRequest'r x__)
                         (Control.DeepSeq.deepseq (_GetRequest'pr x__)
                            (Control.DeepSeq.deepseq (_GetRequest'basicQuorum x__)
                               (Control.DeepSeq.deepseq (_GetRequest'notfoundOk x__)
                                  (Control.DeepSeq.deepseq (_GetRequest'ifModified x__)
                                     (Control.DeepSeq.deepseq (_GetRequest'head x__)
                                        (Control.DeepSeq.deepseq (_GetRequest'deletedvclock x__)
                                           (Control.DeepSeq.deepseq (_GetRequest'timeout x__)
                                              (Control.DeepSeq.deepseq
                                                 (_GetRequest'sloppyQuorum x__)
                                                 (Control.DeepSeq.deepseq (_GetRequest'n x__)
                                                    (Control.DeepSeq.deepseq (_GetRequest'type' x__)
                                                       (()))))))))))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.content' @:: Lens' GetResponse [Content]@
    * 'Proto.Proto.Riak_Fields.vclock' @:: Lens' GetResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'vclock' @:: Lens' GetResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.unchanged' @:: Lens' GetResponse Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'unchanged' @:: Lens' GetResponse (Prelude.Maybe Prelude.Bool)@
 -}
data GetResponse = GetResponse{_GetResponse'content :: ![Content],
                               _GetResponse'vclock :: !(Prelude.Maybe Data.ByteString.ByteString),
                               _GetResponse'unchanged :: !(Prelude.Maybe Prelude.Bool),
                               _GetResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetResponse "content" ([Content])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetResponse'content
                 (\ x__ y__ -> x__{_GetResponse'content = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetResponse "vclock"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetResponse'vclock
                 (\ x__ y__ -> x__{_GetResponse'vclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetResponse "maybe'vclock"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetResponse'vclock
                 (\ x__ y__ -> x__{_GetResponse'vclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetResponse "unchanged"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetResponse'unchanged
                 (\ x__ y__ -> x__{_GetResponse'unchanged = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetResponse "maybe'unchanged"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetResponse'unchanged
                 (\ x__ y__ -> x__{_GetResponse'unchanged = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetResponse where
        messageName _ = Data.Text.pack "GetResponse"
        fieldsByTag
          = let content__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Content)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "content")))
                      :: Data.ProtoLens.FieldDescriptor GetResponse
                vclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'vclock")))
                      :: Data.ProtoLens.FieldDescriptor GetResponse
                unchanged__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "unchanged"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'unchanged")))
                      :: Data.ProtoLens.FieldDescriptor GetResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, content__field_descriptor),
                 (Data.ProtoLens.Tag 2, vclock__field_descriptor),
                 (Data.ProtoLens.Tag 3, unchanged__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GetResponse'_unknownFields
              (\ x__ y__ -> x__{_GetResponse'_unknownFields = y__})
        defMessage
          = GetResponse{_GetResponse'content = [],
                        _GetResponse'vclock = Prelude.Nothing,
                        _GetResponse'unchanged = Prelude.Nothing,
                        _GetResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GetResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetResponse'content x__)
                   (Control.DeepSeq.deepseq (_GetResponse'vclock x__)
                      (Control.DeepSeq.deepseq (_GetResponse'unchanged x__) (()))))
{- | Fields :

 -}
data GetServerInfoRequest = GetServerInfoRequest{_GetServerInfoRequest'_unknownFields
                                                 :: !Data.ProtoLens.FieldSet}
                              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetServerInfoRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message GetServerInfoRequest where
        messageName _ = Data.Text.pack "GetServerInfoRequest"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens _GetServerInfoRequest'_unknownFields
              (\ x__ y__ -> x__{_GetServerInfoRequest'_unknownFields = y__})
        defMessage
          = GetServerInfoRequest{_GetServerInfoRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetServerInfoRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GetServerInfoRequest'_unknownFields x__)
                (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.node' @:: Lens' GetServerInfoResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'node' @:: Lens' GetServerInfoResponse
  (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.version' @:: Lens' GetServerInfoResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'version' @:: Lens' GetServerInfoResponse
  (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data GetServerInfoResponse = GetServerInfoResponse{_GetServerInfoResponse'node
                                                   :: !(Prelude.Maybe Data.ByteString.ByteString),
                                                   _GetServerInfoResponse'version ::
                                                   !(Prelude.Maybe Data.ByteString.ByteString),
                                                   _GetServerInfoResponse'_unknownFields ::
                                                   !Data.ProtoLens.FieldSet}
                               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show GetServerInfoResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' GetServerInfoResponse "node"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetServerInfoResponse'node
                 (\ x__ y__ -> x__{_GetServerInfoResponse'node = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetServerInfoResponse "maybe'node"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetServerInfoResponse'node
                 (\ x__ y__ -> x__{_GetServerInfoResponse'node = y__}))
              Prelude.id
instance Lens.Labels.HasLens' GetServerInfoResponse "version"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetServerInfoResponse'version
                 (\ x__ y__ -> x__{_GetServerInfoResponse'version = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' GetServerInfoResponse "maybe'version"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _GetServerInfoResponse'version
                 (\ x__ y__ -> x__{_GetServerInfoResponse'version = y__}))
              Prelude.id
instance Data.ProtoLens.Message GetServerInfoResponse where
        messageName _ = Data.Text.pack "GetServerInfoResponse"
        fieldsByTag
          = let node__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "node"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'node")))
                      :: Data.ProtoLens.FieldDescriptor GetServerInfoResponse
                version__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "version"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'version")))
                      :: Data.ProtoLens.FieldDescriptor GetServerInfoResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, node__field_descriptor),
                 (Data.ProtoLens.Tag 2, version__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _GetServerInfoResponse'_unknownFields
              (\ x__ y__ -> x__{_GetServerInfoResponse'_unknownFields = y__})
        defMessage
          = GetServerInfoResponse{_GetServerInfoResponse'node =
                                    Prelude.Nothing,
                                  _GetServerInfoResponse'version = Prelude.Nothing,
                                  _GetServerInfoResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData GetServerInfoResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_GetServerInfoResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_GetServerInfoResponse'node x__)
                   (Control.DeepSeq.deepseq (_GetServerInfoResponse'version x__)
                      (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.adds' @:: Lens' HllOp [Data.ByteString.ByteString]@
 -}
data HllOp = HllOp{_HllOp'adds :: ![Data.ByteString.ByteString],
                   _HllOp'_unknownFields :: !Data.ProtoLens.FieldSet}
               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show HllOp where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' HllOp "adds"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _HllOp'adds
                 (\ x__ y__ -> x__{_HllOp'adds = y__}))
              Prelude.id
instance Data.ProtoLens.Message HllOp where
        messageName _ = Data.Text.pack "HllOp"
        fieldsByTag
          = let adds__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "adds"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "adds")))
                      :: Data.ProtoLens.FieldDescriptor HllOp
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, adds__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _HllOp'_unknownFields
              (\ x__ y__ -> x__{_HllOp'_unknownFields = y__})
        defMessage = HllOp{_HllOp'adds = [], _HllOp'_unknownFields = ([])}
instance Control.DeepSeq.NFData HllOp where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_HllOp'_unknownFields x__)
                (Control.DeepSeq.deepseq (_HllOp'adds x__) (()))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.index' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.qtype' @:: Lens' IndexRequest IndexRequest'IndexQueryType@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'key' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.rangeMin' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'rangeMin' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.rangeMax' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'rangeMax' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.returnTerms' @:: Lens' IndexRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'returnTerms' @:: Lens' IndexRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.stream' @:: Lens' IndexRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'stream' @:: Lens' IndexRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.maxResults' @:: Lens' IndexRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'maxResults' @:: Lens' IndexRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.continuation' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'continuation' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' IndexRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' IndexRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.termRegex' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'termRegex' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.paginationSort' @:: Lens' IndexRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'paginationSort' @:: Lens' IndexRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.coverContext' @:: Lens' IndexRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'coverContext' @:: Lens' IndexRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.returnBody' @:: Lens' IndexRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'returnBody' @:: Lens' IndexRequest (Prelude.Maybe Prelude.Bool)@
 -}
data IndexRequest = IndexRequest{_IndexRequest'bucket ::
                                 !Data.ByteString.ByteString,
                                 _IndexRequest'index :: !Data.ByteString.ByteString,
                                 _IndexRequest'qtype :: !IndexRequest'IndexQueryType,
                                 _IndexRequest'key :: !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'rangeMin ::
                                 !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'rangeMax ::
                                 !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'returnTerms :: !(Prelude.Maybe Prelude.Bool),
                                 _IndexRequest'stream :: !(Prelude.Maybe Prelude.Bool),
                                 _IndexRequest'maxResults :: !(Prelude.Maybe Data.Word.Word32),
                                 _IndexRequest'continuation ::
                                 !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'timeout :: !(Prelude.Maybe Data.Word.Word32),
                                 _IndexRequest'type' :: !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'termRegex ::
                                 !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'paginationSort :: !(Prelude.Maybe Prelude.Bool),
                                 _IndexRequest'coverContext ::
                                 !(Prelude.Maybe Data.ByteString.ByteString),
                                 _IndexRequest'returnBody :: !(Prelude.Maybe Prelude.Bool),
                                 _IndexRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show IndexRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' IndexRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'bucket
                 (\ x__ y__ -> x__{_IndexRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "index"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'index
                 (\ x__ y__ -> x__{_IndexRequest'index = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "qtype"
           (IndexRequest'IndexQueryType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'qtype
                 (\ x__ y__ -> x__{_IndexRequest'qtype = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'key
                 (\ x__ y__ -> x__{_IndexRequest'key = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'key"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'key
                 (\ x__ y__ -> x__{_IndexRequest'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "rangeMin"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'rangeMin
                 (\ x__ y__ -> x__{_IndexRequest'rangeMin = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'rangeMin"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'rangeMin
                 (\ x__ y__ -> x__{_IndexRequest'rangeMin = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "rangeMax"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'rangeMax
                 (\ x__ y__ -> x__{_IndexRequest'rangeMax = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'rangeMax"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'rangeMax
                 (\ x__ y__ -> x__{_IndexRequest'rangeMax = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "returnTerms"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'returnTerms
                 (\ x__ y__ -> x__{_IndexRequest'returnTerms = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'returnTerms"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'returnTerms
                 (\ x__ y__ -> x__{_IndexRequest'returnTerms = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "stream" (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'stream
                 (\ x__ y__ -> x__{_IndexRequest'stream = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'stream"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'stream
                 (\ x__ y__ -> x__{_IndexRequest'stream = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "maxResults"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'maxResults
                 (\ x__ y__ -> x__{_IndexRequest'maxResults = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'maxResults"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'maxResults
                 (\ x__ y__ -> x__{_IndexRequest'maxResults = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "continuation"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'continuation
                 (\ x__ y__ -> x__{_IndexRequest'continuation = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'continuation"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'continuation
                 (\ x__ y__ -> x__{_IndexRequest'continuation = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'timeout
                 (\ x__ y__ -> x__{_IndexRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'timeout
                 (\ x__ y__ -> x__{_IndexRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'type'
                 (\ x__ y__ -> x__{_IndexRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'type'
                 (\ x__ y__ -> x__{_IndexRequest'type' = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "termRegex"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'termRegex
                 (\ x__ y__ -> x__{_IndexRequest'termRegex = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'termRegex"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'termRegex
                 (\ x__ y__ -> x__{_IndexRequest'termRegex = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "paginationSort"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'paginationSort
                 (\ x__ y__ -> x__{_IndexRequest'paginationSort = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'paginationSort"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'paginationSort
                 (\ x__ y__ -> x__{_IndexRequest'paginationSort = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "coverContext"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'coverContext
                 (\ x__ y__ -> x__{_IndexRequest'coverContext = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'coverContext"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'coverContext
                 (\ x__ y__ -> x__{_IndexRequest'coverContext = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexRequest "returnBody"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'returnBody
                 (\ x__ y__ -> x__{_IndexRequest'returnBody = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexRequest "maybe'returnBody"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexRequest'returnBody
                 (\ x__ y__ -> x__{_IndexRequest'returnBody = y__}))
              Prelude.id
instance Data.ProtoLens.Message IndexRequest where
        messageName _ = Data.Text.pack "IndexRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                index__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "index"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "index")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                qtype__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "qtype"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor IndexRequest'IndexQueryType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "qtype")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                rangeMin__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "range_min"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rangeMin")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                rangeMax__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "range_max"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rangeMax")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                returnTerms__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "return_terms"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'returnTerms")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                stream__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "stream"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'stream")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                maxResults__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "max_results"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'maxResults")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                continuation__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "continuation"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'continuation")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                termRegex__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "term_regex"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'termRegex")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                paginationSort__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pagination_sort"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'paginationSort")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                coverContext__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "cover_context"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'coverContext")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
                returnBody__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "return_body"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnBody")))
                      :: Data.ProtoLens.FieldDescriptor IndexRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, index__field_descriptor),
                 (Data.ProtoLens.Tag 3, qtype__field_descriptor),
                 (Data.ProtoLens.Tag 4, key__field_descriptor),
                 (Data.ProtoLens.Tag 5, rangeMin__field_descriptor),
                 (Data.ProtoLens.Tag 6, rangeMax__field_descriptor),
                 (Data.ProtoLens.Tag 7, returnTerms__field_descriptor),
                 (Data.ProtoLens.Tag 8, stream__field_descriptor),
                 (Data.ProtoLens.Tag 9, maxResults__field_descriptor),
                 (Data.ProtoLens.Tag 10, continuation__field_descriptor),
                 (Data.ProtoLens.Tag 11, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 12, type'__field_descriptor),
                 (Data.ProtoLens.Tag 13, termRegex__field_descriptor),
                 (Data.ProtoLens.Tag 14, paginationSort__field_descriptor),
                 (Data.ProtoLens.Tag 15, coverContext__field_descriptor),
                 (Data.ProtoLens.Tag 16, returnBody__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _IndexRequest'_unknownFields
              (\ x__ y__ -> x__{_IndexRequest'_unknownFields = y__})
        defMessage
          = IndexRequest{_IndexRequest'bucket = Data.ProtoLens.fieldDefault,
                         _IndexRequest'index = Data.ProtoLens.fieldDefault,
                         _IndexRequest'qtype = Data.ProtoLens.fieldDefault,
                         _IndexRequest'key = Prelude.Nothing,
                         _IndexRequest'rangeMin = Prelude.Nothing,
                         _IndexRequest'rangeMax = Prelude.Nothing,
                         _IndexRequest'returnTerms = Prelude.Nothing,
                         _IndexRequest'stream = Prelude.Nothing,
                         _IndexRequest'maxResults = Prelude.Nothing,
                         _IndexRequest'continuation = Prelude.Nothing,
                         _IndexRequest'timeout = Prelude.Nothing,
                         _IndexRequest'type' = Prelude.Nothing,
                         _IndexRequest'termRegex = Prelude.Nothing,
                         _IndexRequest'paginationSort = Prelude.Nothing,
                         _IndexRequest'coverContext = Prelude.Nothing,
                         _IndexRequest'returnBody = Prelude.Nothing,
                         _IndexRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData IndexRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_IndexRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_IndexRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_IndexRequest'index x__)
                      (Control.DeepSeq.deepseq (_IndexRequest'qtype x__)
                         (Control.DeepSeq.deepseq (_IndexRequest'key x__)
                            (Control.DeepSeq.deepseq (_IndexRequest'rangeMin x__)
                               (Control.DeepSeq.deepseq (_IndexRequest'rangeMax x__)
                                  (Control.DeepSeq.deepseq (_IndexRequest'returnTerms x__)
                                     (Control.DeepSeq.deepseq (_IndexRequest'stream x__)
                                        (Control.DeepSeq.deepseq (_IndexRequest'maxResults x__)
                                           (Control.DeepSeq.deepseq (_IndexRequest'continuation x__)
                                              (Control.DeepSeq.deepseq (_IndexRequest'timeout x__)
                                                 (Control.DeepSeq.deepseq (_IndexRequest'type' x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_IndexRequest'termRegex x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_IndexRequest'paginationSort x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_IndexRequest'coverContext x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_IndexRequest'returnBody x__)
                                                                (())))))))))))))))))
data IndexRequest'IndexQueryType = IndexRequest'eq
                                 | IndexRequest'range
                                     deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum IndexRequest'IndexQueryType
         where
        maybeToEnum 0 = Prelude.Just IndexRequest'eq
        maybeToEnum 1 = Prelude.Just IndexRequest'range
        maybeToEnum _ = Prelude.Nothing
        showEnum IndexRequest'eq = "eq"
        showEnum IndexRequest'range = "range"
        readEnum k
          | (Prelude.==) k "eq" = Prelude.Just IndexRequest'eq
          | (Prelude.==) k "range" = Prelude.Just IndexRequest'range
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded IndexRequest'IndexQueryType where
        minBound = IndexRequest'eq
        maxBound = IndexRequest'range
instance Prelude.Enum IndexRequest'IndexQueryType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum IndexQueryType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum IndexRequest'eq = 0
        fromEnum IndexRequest'range = 1
        succ IndexRequest'range
          = Prelude.error
              "IndexRequest'IndexQueryType.succ: bad argument IndexRequest'range. This value would be out of bounds."
        succ IndexRequest'eq = IndexRequest'range
        pred IndexRequest'eq
          = Prelude.error
              "IndexRequest'IndexQueryType.pred: bad argument IndexRequest'eq. This value would be out of bounds."
        pred IndexRequest'range = IndexRequest'eq
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault IndexRequest'IndexQueryType
         where
        fieldDefault = IndexRequest'eq
instance Control.DeepSeq.NFData IndexRequest'IndexQueryType where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.keys' @:: Lens' IndexResponse [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.results' @:: Lens' IndexResponse [Pair]@
    * 'Proto.Proto.Riak_Fields.continuation' @:: Lens' IndexResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'continuation' @:: Lens' IndexResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.done' @:: Lens' IndexResponse Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'done' @:: Lens' IndexResponse (Prelude.Maybe Prelude.Bool)@
 -}
data IndexResponse = IndexResponse{_IndexResponse'keys ::
                                   ![Data.ByteString.ByteString],
                                   _IndexResponse'results :: ![Pair],
                                   _IndexResponse'continuation ::
                                   !(Prelude.Maybe Data.ByteString.ByteString),
                                   _IndexResponse'done :: !(Prelude.Maybe Prelude.Bool),
                                   _IndexResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show IndexResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' IndexResponse "keys"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexResponse'keys
                 (\ x__ y__ -> x__{_IndexResponse'keys = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexResponse "results" ([Pair])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexResponse'results
                 (\ x__ y__ -> x__{_IndexResponse'results = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexResponse "continuation"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexResponse'continuation
                 (\ x__ y__ -> x__{_IndexResponse'continuation = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexResponse "maybe'continuation"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexResponse'continuation
                 (\ x__ y__ -> x__{_IndexResponse'continuation = y__}))
              Prelude.id
instance Lens.Labels.HasLens' IndexResponse "done" (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexResponse'done
                 (\ x__ y__ -> x__{_IndexResponse'done = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' IndexResponse "maybe'done"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _IndexResponse'done
                 (\ x__ y__ -> x__{_IndexResponse'done = y__}))
              Prelude.id
instance Data.ProtoLens.Message IndexResponse where
        messageName _ = Data.Text.pack "IndexResponse"
        fieldsByTag
          = let keys__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "keys"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "keys")))
                      :: Data.ProtoLens.FieldDescriptor IndexResponse
                results__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "results"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Pair)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "results")))
                      :: Data.ProtoLens.FieldDescriptor IndexResponse
                continuation__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "continuation"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'continuation")))
                      :: Data.ProtoLens.FieldDescriptor IndexResponse
                done__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "done"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'done")))
                      :: Data.ProtoLens.FieldDescriptor IndexResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, keys__field_descriptor),
                 (Data.ProtoLens.Tag 2, results__field_descriptor),
                 (Data.ProtoLens.Tag 3, continuation__field_descriptor),
                 (Data.ProtoLens.Tag 4, done__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _IndexResponse'_unknownFields
              (\ x__ y__ -> x__{_IndexResponse'_unknownFields = y__})
        defMessage
          = IndexResponse{_IndexResponse'keys = [],
                          _IndexResponse'results = [],
                          _IndexResponse'continuation = Prelude.Nothing,
                          _IndexResponse'done = Prelude.Nothing,
                          _IndexResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData IndexResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_IndexResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_IndexResponse'keys x__)
                   (Control.DeepSeq.deepseq (_IndexResponse'results x__)
                      (Control.DeepSeq.deepseq (_IndexResponse'continuation x__)
                         (Control.DeepSeq.deepseq (_IndexResponse'done x__) (())))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' Link Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'bucket' @:: Lens' Link (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' Link Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'key' @:: Lens' Link (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.tag' @:: Lens' Link Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'tag' @:: Lens' Link (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data Link = Link{_Link'bucket ::
                 !(Prelude.Maybe Data.ByteString.ByteString),
                 _Link'key :: !(Prelude.Maybe Data.ByteString.ByteString),
                 _Link'tag :: !(Prelude.Maybe Data.ByteString.ByteString),
                 _Link'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Link where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Link "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Link'bucket
                 (\ x__ y__ -> x__{_Link'bucket = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Link "maybe'bucket"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Link'bucket
                 (\ x__ y__ -> x__{_Link'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Link "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Link'key
                 (\ x__ y__ -> x__{_Link'key = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Link "maybe'key"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Link'key
                 (\ x__ y__ -> x__{_Link'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Link "tag"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Link'tag
                 (\ x__ y__ -> x__{_Link'tag = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Link "maybe'tag"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Link'tag
                 (\ x__ y__ -> x__{_Link'tag = y__}))
              Prelude.id
instance Data.ProtoLens.Message Link where
        messageName _ = Data.Text.pack "Link"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'bucket")))
                      :: Data.ProtoLens.FieldDescriptor Link
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")))
                      :: Data.ProtoLens.FieldDescriptor Link
                tag__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "tag"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'tag")))
                      :: Data.ProtoLens.FieldDescriptor Link
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, key__field_descriptor),
                 (Data.ProtoLens.Tag 3, tag__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Link'_unknownFields
              (\ x__ y__ -> x__{_Link'_unknownFields = y__})
        defMessage
          = Link{_Link'bucket = Prelude.Nothing, _Link'key = Prelude.Nothing,
                 _Link'tag = Prelude.Nothing, _Link'_unknownFields = ([])}
instance Control.DeepSeq.NFData Link where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Link'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Link'bucket x__)
                   (Control.DeepSeq.deepseq (_Link'key x__)
                      (Control.DeepSeq.deepseq (_Link'tag x__) (()))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.field' @:: Lens' MapEntry MapField@
    * 'Proto.Proto.Riak_Fields.counterValue' @:: Lens' MapEntry Data.Int.Int64@
    * 'Proto.Proto.Riak_Fields.maybe'counterValue' @:: Lens' MapEntry (Prelude.Maybe Data.Int.Int64)@
    * 'Proto.Proto.Riak_Fields.setValue' @:: Lens' MapEntry [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.registerValue' @:: Lens' MapEntry Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'registerValue' @:: Lens' MapEntry (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.flagValue' @:: Lens' MapEntry Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'flagValue' @:: Lens' MapEntry (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.mapValue' @:: Lens' MapEntry [MapEntry]@
 -}
data MapEntry = MapEntry{_MapEntry'field :: !MapField,
                         _MapEntry'counterValue :: !(Prelude.Maybe Data.Int.Int64),
                         _MapEntry'setValue :: ![Data.ByteString.ByteString],
                         _MapEntry'registerValue ::
                         !(Prelude.Maybe Data.ByteString.ByteString),
                         _MapEntry'flagValue :: !(Prelude.Maybe Prelude.Bool),
                         _MapEntry'mapValue :: ![MapEntry],
                         _MapEntry'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MapEntry where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MapEntry "field" (MapField) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'field
                 (\ x__ y__ -> x__{_MapEntry'field = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapEntry "counterValue"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'counterValue
                 (\ x__ y__ -> x__{_MapEntry'counterValue = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapEntry "maybe'counterValue"
           (Prelude.Maybe Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'counterValue
                 (\ x__ y__ -> x__{_MapEntry'counterValue = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapEntry "setValue"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'setValue
                 (\ x__ y__ -> x__{_MapEntry'setValue = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapEntry "registerValue"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'registerValue
                 (\ x__ y__ -> x__{_MapEntry'registerValue = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapEntry "maybe'registerValue"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'registerValue
                 (\ x__ y__ -> x__{_MapEntry'registerValue = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapEntry "flagValue" (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'flagValue
                 (\ x__ y__ -> x__{_MapEntry'flagValue = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapEntry "maybe'flagValue"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'flagValue
                 (\ x__ y__ -> x__{_MapEntry'flagValue = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapEntry "mapValue" ([MapEntry])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapEntry'mapValue
                 (\ x__ y__ -> x__{_MapEntry'mapValue = y__}))
              Prelude.id
instance Data.ProtoLens.Message MapEntry where
        messageName _ = Data.Text.pack "MapEntry"
        fieldsByTag
          = let field__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "field"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapField)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "field")))
                      :: Data.ProtoLens.FieldDescriptor MapEntry
                counterValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "counter_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'counterValue")))
                      :: Data.ProtoLens.FieldDescriptor MapEntry
                setValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "set_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "setValue")))
                      :: Data.ProtoLens.FieldDescriptor MapEntry
                registerValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "register_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'registerValue")))
                      :: Data.ProtoLens.FieldDescriptor MapEntry
                flagValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "flag_value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'flagValue")))
                      :: Data.ProtoLens.FieldDescriptor MapEntry
                mapValue__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "map_value"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapEntry)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "mapValue")))
                      :: Data.ProtoLens.FieldDescriptor MapEntry
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, field__field_descriptor),
                 (Data.ProtoLens.Tag 2, counterValue__field_descriptor),
                 (Data.ProtoLens.Tag 3, setValue__field_descriptor),
                 (Data.ProtoLens.Tag 4, registerValue__field_descriptor),
                 (Data.ProtoLens.Tag 5, flagValue__field_descriptor),
                 (Data.ProtoLens.Tag 6, mapValue__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MapEntry'_unknownFields
              (\ x__ y__ -> x__{_MapEntry'_unknownFields = y__})
        defMessage
          = MapEntry{_MapEntry'field = Data.ProtoLens.defMessage,
                     _MapEntry'counterValue = Prelude.Nothing, _MapEntry'setValue = [],
                     _MapEntry'registerValue = Prelude.Nothing,
                     _MapEntry'flagValue = Prelude.Nothing, _MapEntry'mapValue = [],
                     _MapEntry'_unknownFields = ([])}
instance Control.DeepSeq.NFData MapEntry where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MapEntry'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MapEntry'field x__)
                   (Control.DeepSeq.deepseq (_MapEntry'counterValue x__)
                      (Control.DeepSeq.deepseq (_MapEntry'setValue x__)
                         (Control.DeepSeq.deepseq (_MapEntry'registerValue x__)
                            (Control.DeepSeq.deepseq (_MapEntry'flagValue x__)
                               (Control.DeepSeq.deepseq (_MapEntry'mapValue x__) (())))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.name' @:: Lens' MapField Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' MapField MapField'MapFieldType@
 -}
data MapField = MapField{_MapField'name ::
                         !Data.ByteString.ByteString,
                         _MapField'type' :: !MapField'MapFieldType,
                         _MapField'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MapField where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MapField "name"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapField'name
                 (\ x__ y__ -> x__{_MapField'name = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapField "type'"
           (MapField'MapFieldType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapField'type'
                 (\ x__ y__ -> x__{_MapField'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message MapField where
        messageName _ = Data.Text.pack "MapField"
        fieldsByTag
          = let name__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "name"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")))
                      :: Data.ProtoLens.FieldDescriptor MapField
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor MapField'MapFieldType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor MapField
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, name__field_descriptor),
                 (Data.ProtoLens.Tag 2, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MapField'_unknownFields
              (\ x__ y__ -> x__{_MapField'_unknownFields = y__})
        defMessage
          = MapField{_MapField'name = Data.ProtoLens.fieldDefault,
                     _MapField'type' = Data.ProtoLens.fieldDefault,
                     _MapField'_unknownFields = ([])}
instance Control.DeepSeq.NFData MapField where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MapField'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MapField'name x__)
                   (Control.DeepSeq.deepseq (_MapField'type' x__) (())))
data MapField'MapFieldType = MapField'COUNTER
                           | MapField'SET
                           | MapField'REGISTER
                           | MapField'FLAG
                           | MapField'MAP
                               deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum MapField'MapFieldType where
        maybeToEnum 1 = Prelude.Just MapField'COUNTER
        maybeToEnum 2 = Prelude.Just MapField'SET
        maybeToEnum 3 = Prelude.Just MapField'REGISTER
        maybeToEnum 4 = Prelude.Just MapField'FLAG
        maybeToEnum 5 = Prelude.Just MapField'MAP
        maybeToEnum _ = Prelude.Nothing
        showEnum MapField'COUNTER = "COUNTER"
        showEnum MapField'SET = "SET"
        showEnum MapField'REGISTER = "REGISTER"
        showEnum MapField'FLAG = "FLAG"
        showEnum MapField'MAP = "MAP"
        readEnum k
          | (Prelude.==) k "COUNTER" = Prelude.Just MapField'COUNTER
          | (Prelude.==) k "SET" = Prelude.Just MapField'SET
          | (Prelude.==) k "REGISTER" = Prelude.Just MapField'REGISTER
          | (Prelude.==) k "FLAG" = Prelude.Just MapField'FLAG
          | (Prelude.==) k "MAP" = Prelude.Just MapField'MAP
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded MapField'MapFieldType where
        minBound = MapField'COUNTER
        maxBound = MapField'MAP
instance Prelude.Enum MapField'MapFieldType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum MapFieldType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum MapField'COUNTER = 1
        fromEnum MapField'SET = 2
        fromEnum MapField'REGISTER = 3
        fromEnum MapField'FLAG = 4
        fromEnum MapField'MAP = 5
        succ MapField'MAP
          = Prelude.error
              "MapField'MapFieldType.succ: bad argument MapField'MAP. This value would be out of bounds."
        succ MapField'COUNTER = MapField'SET
        succ MapField'SET = MapField'REGISTER
        succ MapField'REGISTER = MapField'FLAG
        succ MapField'FLAG = MapField'MAP
        pred MapField'COUNTER
          = Prelude.error
              "MapField'MapFieldType.pred: bad argument MapField'COUNTER. This value would be out of bounds."
        pred MapField'SET = MapField'COUNTER
        pred MapField'REGISTER = MapField'SET
        pred MapField'FLAG = MapField'REGISTER
        pred MapField'MAP = MapField'FLAG
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault MapField'MapFieldType where
        fieldDefault = MapField'COUNTER
instance Control.DeepSeq.NFData MapField'MapFieldType where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.field' @:: Lens' MapFieldUpdate MapField@
    * 'Proto.Proto.Riak_Fields.counter' @:: Lens' MapFieldUpdate CounterUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'counter' @:: Lens' MapFieldUpdate (Prelude.Maybe CounterUpdate)@
    * 'Proto.Proto.Riak_Fields.set' @:: Lens' MapFieldUpdate SetUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'set' @:: Lens' MapFieldUpdate (Prelude.Maybe SetUpdate)@
    * 'Proto.Proto.Riak_Fields.register' @:: Lens' MapFieldUpdate Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'register' @:: Lens' MapFieldUpdate (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.flag' @:: Lens' MapFieldUpdate MapFieldUpdate'FlagUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'flag' @:: Lens' MapFieldUpdate (Prelude.Maybe MapFieldUpdate'FlagUpdate)@
    * 'Proto.Proto.Riak_Fields.map' @:: Lens' MapFieldUpdate MapUpdate@
    * 'Proto.Proto.Riak_Fields.maybe'map' @:: Lens' MapFieldUpdate (Prelude.Maybe MapUpdate)@
 -}
data MapFieldUpdate = MapFieldUpdate{_MapFieldUpdate'field ::
                                     !MapField,
                                     _MapFieldUpdate'counter :: !(Prelude.Maybe CounterUpdate),
                                     _MapFieldUpdate'set :: !(Prelude.Maybe SetUpdate),
                                     _MapFieldUpdate'register ::
                                     !(Prelude.Maybe Data.ByteString.ByteString),
                                     _MapFieldUpdate'flag ::
                                     !(Prelude.Maybe MapFieldUpdate'FlagUpdate),
                                     _MapFieldUpdate'map :: !(Prelude.Maybe MapUpdate),
                                     _MapFieldUpdate'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MapFieldUpdate where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MapFieldUpdate "field" (MapField)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'field
                 (\ x__ y__ -> x__{_MapFieldUpdate'field = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapFieldUpdate "counter"
           (CounterUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'counter
                 (\ x__ y__ -> x__{_MapFieldUpdate'counter = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' MapFieldUpdate "maybe'counter"
           (Prelude.Maybe CounterUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'counter
                 (\ x__ y__ -> x__{_MapFieldUpdate'counter = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapFieldUpdate "set" (SetUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'set
                 (\ x__ y__ -> x__{_MapFieldUpdate'set = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' MapFieldUpdate "maybe'set"
           (Prelude.Maybe SetUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'set
                 (\ x__ y__ -> x__{_MapFieldUpdate'set = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapFieldUpdate "register"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'register
                 (\ x__ y__ -> x__{_MapFieldUpdate'register = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapFieldUpdate "maybe'register"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'register
                 (\ x__ y__ -> x__{_MapFieldUpdate'register = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapFieldUpdate "flag"
           (MapFieldUpdate'FlagUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'flag
                 (\ x__ y__ -> x__{_MapFieldUpdate'flag = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapFieldUpdate "maybe'flag"
           (Prelude.Maybe MapFieldUpdate'FlagUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'flag
                 (\ x__ y__ -> x__{_MapFieldUpdate'flag = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapFieldUpdate "map" (MapUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'map
                 (\ x__ y__ -> x__{_MapFieldUpdate'map = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' MapFieldUpdate "maybe'map"
           (Prelude.Maybe MapUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapFieldUpdate'map
                 (\ x__ y__ -> x__{_MapFieldUpdate'map = y__}))
              Prelude.id
instance Data.ProtoLens.Message MapFieldUpdate where
        messageName _ = Data.Text.pack "MapFieldUpdate"
        fieldsByTag
          = let field__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "field"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapField)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "field")))
                      :: Data.ProtoLens.FieldDescriptor MapFieldUpdate
                counter__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "counter"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor CounterUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'counter")))
                      :: Data.ProtoLens.FieldDescriptor MapFieldUpdate
                set__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "set"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor SetUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'set")))
                      :: Data.ProtoLens.FieldDescriptor MapFieldUpdate
                register__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "register"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'register")))
                      :: Data.ProtoLens.FieldDescriptor MapFieldUpdate
                flag__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "flag"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor MapFieldUpdate'FlagUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'flag")))
                      :: Data.ProtoLens.FieldDescriptor MapFieldUpdate
                map__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "map"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapUpdate)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'map")))
                      :: Data.ProtoLens.FieldDescriptor MapFieldUpdate
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, field__field_descriptor),
                 (Data.ProtoLens.Tag 2, counter__field_descriptor),
                 (Data.ProtoLens.Tag 3, set__field_descriptor),
                 (Data.ProtoLens.Tag 4, register__field_descriptor),
                 (Data.ProtoLens.Tag 5, flag__field_descriptor),
                 (Data.ProtoLens.Tag 6, map__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MapFieldUpdate'_unknownFields
              (\ x__ y__ -> x__{_MapFieldUpdate'_unknownFields = y__})
        defMessage
          = MapFieldUpdate{_MapFieldUpdate'field = Data.ProtoLens.defMessage,
                           _MapFieldUpdate'counter = Prelude.Nothing,
                           _MapFieldUpdate'set = Prelude.Nothing,
                           _MapFieldUpdate'register = Prelude.Nothing,
                           _MapFieldUpdate'flag = Prelude.Nothing,
                           _MapFieldUpdate'map = Prelude.Nothing,
                           _MapFieldUpdate'_unknownFields = ([])}
instance Control.DeepSeq.NFData MapFieldUpdate where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MapFieldUpdate'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MapFieldUpdate'field x__)
                   (Control.DeepSeq.deepseq (_MapFieldUpdate'counter x__)
                      (Control.DeepSeq.deepseq (_MapFieldUpdate'set x__)
                         (Control.DeepSeq.deepseq (_MapFieldUpdate'register x__)
                            (Control.DeepSeq.deepseq (_MapFieldUpdate'flag x__)
                               (Control.DeepSeq.deepseq (_MapFieldUpdate'map x__) (())))))))
data MapFieldUpdate'FlagUpdate = MapFieldUpdate'ENABLE
                               | MapFieldUpdate'DISABLE
                                   deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum MapFieldUpdate'FlagUpdate where
        maybeToEnum 1 = Prelude.Just MapFieldUpdate'ENABLE
        maybeToEnum 2 = Prelude.Just MapFieldUpdate'DISABLE
        maybeToEnum _ = Prelude.Nothing
        showEnum MapFieldUpdate'ENABLE = "ENABLE"
        showEnum MapFieldUpdate'DISABLE = "DISABLE"
        readEnum k
          | (Prelude.==) k "ENABLE" = Prelude.Just MapFieldUpdate'ENABLE
          | (Prelude.==) k "DISABLE" = Prelude.Just MapFieldUpdate'DISABLE
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded MapFieldUpdate'FlagUpdate where
        minBound = MapFieldUpdate'ENABLE
        maxBound = MapFieldUpdate'DISABLE
instance Prelude.Enum MapFieldUpdate'FlagUpdate where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum FlagUpdate: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum MapFieldUpdate'ENABLE = 1
        fromEnum MapFieldUpdate'DISABLE = 2
        succ MapFieldUpdate'DISABLE
          = Prelude.error
              "MapFieldUpdate'FlagUpdate.succ: bad argument MapFieldUpdate'DISABLE. This value would be out of bounds."
        succ MapFieldUpdate'ENABLE = MapFieldUpdate'DISABLE
        pred MapFieldUpdate'ENABLE
          = Prelude.error
              "MapFieldUpdate'FlagUpdate.pred: bad argument MapFieldUpdate'ENABLE. This value would be out of bounds."
        pred MapFieldUpdate'DISABLE = MapFieldUpdate'ENABLE
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault MapFieldUpdate'FlagUpdate
         where
        fieldDefault = MapFieldUpdate'ENABLE
instance Control.DeepSeq.NFData MapFieldUpdate'FlagUpdate where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.request' @:: Lens' MapReduceRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.contentType' @:: Lens' MapReduceRequest Data.ByteString.ByteString@
 -}
data MapReduceRequest = MapReduceRequest{_MapReduceRequest'request
                                         :: !Data.ByteString.ByteString,
                                         _MapReduceRequest'contentType ::
                                         !Data.ByteString.ByteString,
                                         _MapReduceRequest'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                          deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MapReduceRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MapReduceRequest "request"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceRequest'request
                 (\ x__ y__ -> x__{_MapReduceRequest'request = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapReduceRequest "contentType"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceRequest'contentType
                 (\ x__ y__ -> x__{_MapReduceRequest'contentType = y__}))
              Prelude.id
instance Data.ProtoLens.Message MapReduceRequest where
        messageName _ = Data.Text.pack "MapReduceRequest"
        fieldsByTag
          = let request__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "request"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "request")))
                      :: Data.ProtoLens.FieldDescriptor MapReduceRequest
                contentType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "contentType")))
                      :: Data.ProtoLens.FieldDescriptor MapReduceRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, request__field_descriptor),
                 (Data.ProtoLens.Tag 2, contentType__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MapReduceRequest'_unknownFields
              (\ x__ y__ -> x__{_MapReduceRequest'_unknownFields = y__})
        defMessage
          = MapReduceRequest{_MapReduceRequest'request =
                               Data.ProtoLens.fieldDefault,
                             _MapReduceRequest'contentType = Data.ProtoLens.fieldDefault,
                             _MapReduceRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData MapReduceRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MapReduceRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MapReduceRequest'request x__)
                   (Control.DeepSeq.deepseq (_MapReduceRequest'contentType x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.phase' @:: Lens' MapReduceResponse Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'phase' @:: Lens' MapReduceResponse (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.response' @:: Lens' MapReduceResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'response' @:: Lens' MapReduceResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.done' @:: Lens' MapReduceResponse Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'done' @:: Lens' MapReduceResponse (Prelude.Maybe Prelude.Bool)@
 -}
data MapReduceResponse = MapReduceResponse{_MapReduceResponse'phase
                                           :: !(Prelude.Maybe Data.Word.Word32),
                                           _MapReduceResponse'response ::
                                           !(Prelude.Maybe Data.ByteString.ByteString),
                                           _MapReduceResponse'done :: !(Prelude.Maybe Prelude.Bool),
                                           _MapReduceResponse'_unknownFields ::
                                           !Data.ProtoLens.FieldSet}
                           deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MapReduceResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MapReduceResponse "phase"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceResponse'phase
                 (\ x__ y__ -> x__{_MapReduceResponse'phase = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapReduceResponse "maybe'phase"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceResponse'phase
                 (\ x__ y__ -> x__{_MapReduceResponse'phase = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapReduceResponse "response"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceResponse'response
                 (\ x__ y__ -> x__{_MapReduceResponse'response = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapReduceResponse "maybe'response"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceResponse'response
                 (\ x__ y__ -> x__{_MapReduceResponse'response = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapReduceResponse "done"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceResponse'done
                 (\ x__ y__ -> x__{_MapReduceResponse'done = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' MapReduceResponse "maybe'done"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapReduceResponse'done
                 (\ x__ y__ -> x__{_MapReduceResponse'done = y__}))
              Prelude.id
instance Data.ProtoLens.Message MapReduceResponse where
        messageName _ = Data.Text.pack "MapReduceResponse"
        fieldsByTag
          = let phase__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "phase"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'phase")))
                      :: Data.ProtoLens.FieldDescriptor MapReduceResponse
                response__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "response"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'response")))
                      :: Data.ProtoLens.FieldDescriptor MapReduceResponse
                done__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "done"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'done")))
                      :: Data.ProtoLens.FieldDescriptor MapReduceResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, phase__field_descriptor),
                 (Data.ProtoLens.Tag 2, response__field_descriptor),
                 (Data.ProtoLens.Tag 3, done__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MapReduceResponse'_unknownFields
              (\ x__ y__ -> x__{_MapReduceResponse'_unknownFields = y__})
        defMessage
          = MapReduceResponse{_MapReduceResponse'phase = Prelude.Nothing,
                              _MapReduceResponse'response = Prelude.Nothing,
                              _MapReduceResponse'done = Prelude.Nothing,
                              _MapReduceResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData MapReduceResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MapReduceResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MapReduceResponse'phase x__)
                   (Control.DeepSeq.deepseq (_MapReduceResponse'response x__)
                      (Control.DeepSeq.deepseq (_MapReduceResponse'done x__) (()))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.removes' @:: Lens' MapUpdate [MapField]@
    * 'Proto.Proto.Riak_Fields.updates' @:: Lens' MapUpdate [MapFieldUpdate]@
 -}
data MapUpdate = MapUpdate{_MapUpdate'removes :: ![MapField],
                           _MapUpdate'updates :: ![MapFieldUpdate],
                           _MapUpdate'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MapUpdate where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MapUpdate "removes" ([MapField])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapUpdate'removes
                 (\ x__ y__ -> x__{_MapUpdate'removes = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MapUpdate "updates"
           ([MapFieldUpdate])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MapUpdate'updates
                 (\ x__ y__ -> x__{_MapUpdate'updates = y__}))
              Prelude.id
instance Data.ProtoLens.Message MapUpdate where
        messageName _ = Data.Text.pack "MapUpdate"
        fieldsByTag
          = let removes__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "removes"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapField)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "removes")))
                      :: Data.ProtoLens.FieldDescriptor MapUpdate
                updates__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "updates"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapFieldUpdate)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "updates")))
                      :: Data.ProtoLens.FieldDescriptor MapUpdate
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, removes__field_descriptor),
                 (Data.ProtoLens.Tag 2, updates__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MapUpdate'_unknownFields
              (\ x__ y__ -> x__{_MapUpdate'_unknownFields = y__})
        defMessage
          = MapUpdate{_MapUpdate'removes = [], _MapUpdate'updates = [],
                      _MapUpdate'_unknownFields = ([])}
instance Control.DeepSeq.NFData MapUpdate where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MapUpdate'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MapUpdate'removes x__)
                   (Control.DeepSeq.deepseq (_MapUpdate'updates x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.module'' @:: Lens' ModuleFunction Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.function' @:: Lens' ModuleFunction Data.ByteString.ByteString@
 -}
data ModuleFunction = ModuleFunction{_ModuleFunction'module' ::
                                     !Data.ByteString.ByteString,
                                     _ModuleFunction'function :: !Data.ByteString.ByteString,
                                     _ModuleFunction'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ModuleFunction where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ModuleFunction "module'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModuleFunction'module'
                 (\ x__ y__ -> x__{_ModuleFunction'module' = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ModuleFunction "function"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ModuleFunction'function
                 (\ x__ y__ -> x__{_ModuleFunction'function = y__}))
              Prelude.id
instance Data.ProtoLens.Message ModuleFunction where
        messageName _ = Data.Text.pack "ModuleFunction"
        fieldsByTag
          = let module'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "module"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "module'")))
                      :: Data.ProtoLens.FieldDescriptor ModuleFunction
                function__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "function"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "function")))
                      :: Data.ProtoLens.FieldDescriptor ModuleFunction
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, module'__field_descriptor),
                 (Data.ProtoLens.Tag 2, function__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ModuleFunction'_unknownFields
              (\ x__ y__ -> x__{_ModuleFunction'_unknownFields = y__})
        defMessage
          = ModuleFunction{_ModuleFunction'module' =
                             Data.ProtoLens.fieldDefault,
                           _ModuleFunction'function = Data.ProtoLens.fieldDefault,
                           _ModuleFunction'_unknownFields = ([])}
instance Control.DeepSeq.NFData ModuleFunction where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_ModuleFunction'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ModuleFunction'module' x__)
                   (Control.DeepSeq.deepseq (_ModuleFunction'function x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.key' @:: Lens' Pair Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.value' @:: Lens' Pair Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'value' @:: Lens' Pair (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data Pair = Pair{_Pair'key :: !Data.ByteString.ByteString,
                 _Pair'value :: !(Prelude.Maybe Data.ByteString.ByteString),
                 _Pair'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Pair where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Pair "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Pair'key
                 (\ x__ y__ -> x__{_Pair'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Pair "value"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Pair'value
                 (\ x__ y__ -> x__{_Pair'value = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' Pair "maybe'value"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Pair'value
                 (\ x__ y__ -> x__{_Pair'value = y__}))
              Prelude.id
instance Data.ProtoLens.Message Pair where
        messageName _ = Data.Text.pack "Pair"
        fieldsByTag
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")))
                      :: Data.ProtoLens.FieldDescriptor Pair
                value__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "value"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'value")))
                      :: Data.ProtoLens.FieldDescriptor Pair
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, key__field_descriptor),
                 (Data.ProtoLens.Tag 2, value__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Pair'_unknownFields
              (\ x__ y__ -> x__{_Pair'_unknownFields = y__})
        defMessage
          = Pair{_Pair'key = Data.ProtoLens.fieldDefault,
                 _Pair'value = Prelude.Nothing, _Pair'_unknownFields = ([])}
instance Control.DeepSeq.NFData Pair where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Pair'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Pair'key x__)
                   (Control.DeepSeq.deepseq (_Pair'value x__) (())))
{- | Fields :

 -}
data PingRequest = PingRequest{_PingRequest'_unknownFields ::
                               !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message PingRequest where
        messageName _ = Data.Text.pack "PingRequest"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens _PingRequest'_unknownFields
              (\ x__ y__ -> x__{_PingRequest'_unknownFields = y__})
        defMessage = PingRequest{_PingRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData PingRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_PingRequest'_unknownFields x__) (())
{- | Fields :

 -}
data PingResponse = PingResponse{_PingResponse'_unknownFields ::
                                 !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message PingResponse where
        messageName _ = Data.Text.pack "PingResponse"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens _PingResponse'_unknownFields
              (\ x__ y__ -> x__{_PingResponse'_unknownFields = y__})
        defMessage = PingResponse{_PingResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData PingResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_PingResponse'_unknownFields x__) (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' PutRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' PutRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'key' @:: Lens' PutRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.vclock' @:: Lens' PutRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'vclock' @:: Lens' PutRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.content' @:: Lens' PutRequest Content@
    * 'Proto.Proto.Riak_Fields.w' @:: Lens' PutRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'w' @:: Lens' PutRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.dw' @:: Lens' PutRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'dw' @:: Lens' PutRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.returnBody' @:: Lens' PutRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'returnBody' @:: Lens' PutRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.pw' @:: Lens' PutRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pw' @:: Lens' PutRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.ifNotModified' @:: Lens' PutRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'ifNotModified' @:: Lens' PutRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.ifNoneMatch' @:: Lens' PutRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'ifNoneMatch' @:: Lens' PutRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.returnHead' @:: Lens' PutRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'returnHead' @:: Lens' PutRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' PutRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' PutRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.asis' @:: Lens' PutRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'asis' @:: Lens' PutRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.sloppyQuorum' @:: Lens' PutRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'sloppyQuorum' @:: Lens' PutRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.n' @:: Lens' PutRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'n' @:: Lens' PutRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' PutRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' PutRequest (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data PutRequest = PutRequest{_PutRequest'bucket ::
                             !Data.ByteString.ByteString,
                             _PutRequest'key :: !(Prelude.Maybe Data.ByteString.ByteString),
                             _PutRequest'vclock :: !(Prelude.Maybe Data.ByteString.ByteString),
                             _PutRequest'content :: !Content,
                             _PutRequest'w :: !(Prelude.Maybe Data.Word.Word32),
                             _PutRequest'dw :: !(Prelude.Maybe Data.Word.Word32),
                             _PutRequest'returnBody :: !(Prelude.Maybe Prelude.Bool),
                             _PutRequest'pw :: !(Prelude.Maybe Data.Word.Word32),
                             _PutRequest'ifNotModified :: !(Prelude.Maybe Prelude.Bool),
                             _PutRequest'ifNoneMatch :: !(Prelude.Maybe Prelude.Bool),
                             _PutRequest'returnHead :: !(Prelude.Maybe Prelude.Bool),
                             _PutRequest'timeout :: !(Prelude.Maybe Data.Word.Word32),
                             _PutRequest'asis :: !(Prelude.Maybe Prelude.Bool),
                             _PutRequest'sloppyQuorum :: !(Prelude.Maybe Prelude.Bool),
                             _PutRequest'n :: !(Prelude.Maybe Data.Word.Word32),
                             _PutRequest'type' :: !(Prelude.Maybe Data.ByteString.ByteString),
                             _PutRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PutRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' PutRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'bucket
                 (\ x__ y__ -> x__{_PutRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'key
                 (\ x__ y__ -> x__{_PutRequest'key = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'key"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'key
                 (\ x__ y__ -> x__{_PutRequest'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "vclock"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'vclock
                 (\ x__ y__ -> x__{_PutRequest'vclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'vclock"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'vclock
                 (\ x__ y__ -> x__{_PutRequest'vclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "content" (Content) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'content
                 (\ x__ y__ -> x__{_PutRequest'content = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "w" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'w
                 (\ x__ y__ -> x__{_PutRequest'w = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'w"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'w
                 (\ x__ y__ -> x__{_PutRequest'w = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "dw" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'dw
                 (\ x__ y__ -> x__{_PutRequest'dw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'dw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'dw
                 (\ x__ y__ -> x__{_PutRequest'dw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "returnBody"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'returnBody
                 (\ x__ y__ -> x__{_PutRequest'returnBody = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'returnBody"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'returnBody
                 (\ x__ y__ -> x__{_PutRequest'returnBody = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "pw" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'pw
                 (\ x__ y__ -> x__{_PutRequest'pw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'pw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'pw
                 (\ x__ y__ -> x__{_PutRequest'pw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "ifNotModified"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'ifNotModified
                 (\ x__ y__ -> x__{_PutRequest'ifNotModified = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'ifNotModified"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'ifNotModified
                 (\ x__ y__ -> x__{_PutRequest'ifNotModified = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "ifNoneMatch"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'ifNoneMatch
                 (\ x__ y__ -> x__{_PutRequest'ifNoneMatch = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'ifNoneMatch"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'ifNoneMatch
                 (\ x__ y__ -> x__{_PutRequest'ifNoneMatch = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "returnHead"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'returnHead
                 (\ x__ y__ -> x__{_PutRequest'returnHead = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'returnHead"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'returnHead
                 (\ x__ y__ -> x__{_PutRequest'returnHead = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'timeout
                 (\ x__ y__ -> x__{_PutRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'timeout
                 (\ x__ y__ -> x__{_PutRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "asis" (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'asis
                 (\ x__ y__ -> x__{_PutRequest'asis = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'asis"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'asis
                 (\ x__ y__ -> x__{_PutRequest'asis = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "sloppyQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_PutRequest'sloppyQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'sloppyQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_PutRequest'sloppyQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "n" (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'n
                 (\ x__ y__ -> x__{_PutRequest'n = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'n"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'n
                 (\ x__ y__ -> x__{_PutRequest'n = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'type'
                 (\ x__ y__ -> x__{_PutRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutRequest "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutRequest'type'
                 (\ x__ y__ -> x__{_PutRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message PutRequest where
        messageName _ = Data.Text.pack "PutRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                vclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'vclock")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                content__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Content)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "content")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                w__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "w"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'w")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                dw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dw")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                returnBody__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "return_body"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnBody")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                pw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pw")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                ifNotModified__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "if_not_modified"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'ifNotModified")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                ifNoneMatch__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "if_none_match"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'ifNoneMatch")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                returnHead__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "return_head"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnHead")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                asis__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "asis"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'asis")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                sloppyQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sloppy_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'sloppyQuorum")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                n__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "n"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor PutRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, key__field_descriptor),
                 (Data.ProtoLens.Tag 3, vclock__field_descriptor),
                 (Data.ProtoLens.Tag 4, content__field_descriptor),
                 (Data.ProtoLens.Tag 5, w__field_descriptor),
                 (Data.ProtoLens.Tag 6, dw__field_descriptor),
                 (Data.ProtoLens.Tag 7, returnBody__field_descriptor),
                 (Data.ProtoLens.Tag 8, pw__field_descriptor),
                 (Data.ProtoLens.Tag 9, ifNotModified__field_descriptor),
                 (Data.ProtoLens.Tag 10, ifNoneMatch__field_descriptor),
                 (Data.ProtoLens.Tag 11, returnHead__field_descriptor),
                 (Data.ProtoLens.Tag 12, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 13, asis__field_descriptor),
                 (Data.ProtoLens.Tag 14, sloppyQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 15, n__field_descriptor),
                 (Data.ProtoLens.Tag 16, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _PutRequest'_unknownFields
              (\ x__ y__ -> x__{_PutRequest'_unknownFields = y__})
        defMessage
          = PutRequest{_PutRequest'bucket = Data.ProtoLens.fieldDefault,
                       _PutRequest'key = Prelude.Nothing,
                       _PutRequest'vclock = Prelude.Nothing,
                       _PutRequest'content = Data.ProtoLens.defMessage,
                       _PutRequest'w = Prelude.Nothing, _PutRequest'dw = Prelude.Nothing,
                       _PutRequest'returnBody = Prelude.Nothing,
                       _PutRequest'pw = Prelude.Nothing,
                       _PutRequest'ifNotModified = Prelude.Nothing,
                       _PutRequest'ifNoneMatch = Prelude.Nothing,
                       _PutRequest'returnHead = Prelude.Nothing,
                       _PutRequest'timeout = Prelude.Nothing,
                       _PutRequest'asis = Prelude.Nothing,
                       _PutRequest'sloppyQuorum = Prelude.Nothing,
                       _PutRequest'n = Prelude.Nothing,
                       _PutRequest'type' = Prelude.Nothing,
                       _PutRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData PutRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_PutRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_PutRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_PutRequest'key x__)
                      (Control.DeepSeq.deepseq (_PutRequest'vclock x__)
                         (Control.DeepSeq.deepseq (_PutRequest'content x__)
                            (Control.DeepSeq.deepseq (_PutRequest'w x__)
                               (Control.DeepSeq.deepseq (_PutRequest'dw x__)
                                  (Control.DeepSeq.deepseq (_PutRequest'returnBody x__)
                                     (Control.DeepSeq.deepseq (_PutRequest'pw x__)
                                        (Control.DeepSeq.deepseq (_PutRequest'ifNotModified x__)
                                           (Control.DeepSeq.deepseq (_PutRequest'ifNoneMatch x__)
                                              (Control.DeepSeq.deepseq (_PutRequest'returnHead x__)
                                                 (Control.DeepSeq.deepseq (_PutRequest'timeout x__)
                                                    (Control.DeepSeq.deepseq (_PutRequest'asis x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_PutRequest'sloppyQuorum x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_PutRequest'n x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_PutRequest'type' x__)
                                                                (())))))))))))))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.content' @:: Lens' PutResponse [Content]@
    * 'Proto.Proto.Riak_Fields.vclock' @:: Lens' PutResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'vclock' @:: Lens' PutResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' PutResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'key' @:: Lens' PutResponse (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data PutResponse = PutResponse{_PutResponse'content :: ![Content],
                               _PutResponse'vclock :: !(Prelude.Maybe Data.ByteString.ByteString),
                               _PutResponse'key :: !(Prelude.Maybe Data.ByteString.ByteString),
                               _PutResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PutResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' PutResponse "content" ([Content])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutResponse'content
                 (\ x__ y__ -> x__{_PutResponse'content = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutResponse "vclock"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutResponse'vclock
                 (\ x__ y__ -> x__{_PutResponse'vclock = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutResponse "maybe'vclock"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutResponse'vclock
                 (\ x__ y__ -> x__{_PutResponse'vclock = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PutResponse "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutResponse'key
                 (\ x__ y__ -> x__{_PutResponse'key = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' PutResponse "maybe'key"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PutResponse'key
                 (\ x__ y__ -> x__{_PutResponse'key = y__}))
              Prelude.id
instance Data.ProtoLens.Message PutResponse where
        messageName _ = Data.Text.pack "PutResponse"
        fieldsByTag
          = let content__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Content)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "content")))
                      :: Data.ProtoLens.FieldDescriptor PutResponse
                vclock__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vclock"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'vclock")))
                      :: Data.ProtoLens.FieldDescriptor PutResponse
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")))
                      :: Data.ProtoLens.FieldDescriptor PutResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, content__field_descriptor),
                 (Data.ProtoLens.Tag 2, vclock__field_descriptor),
                 (Data.ProtoLens.Tag 3, key__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _PutResponse'_unknownFields
              (\ x__ y__ -> x__{_PutResponse'_unknownFields = y__})
        defMessage
          = PutResponse{_PutResponse'content = [],
                        _PutResponse'vclock = Prelude.Nothing,
                        _PutResponse'key = Prelude.Nothing,
                        _PutResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData PutResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_PutResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_PutResponse'content x__)
                   (Control.DeepSeq.deepseq (_PutResponse'vclock x__)
                      (Control.DeepSeq.deepseq (_PutResponse'key x__) (()))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' ResetBucketPropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' ResetBucketPropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' ResetBucketPropertiesRequest
  (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data ResetBucketPropertiesRequest = ResetBucketPropertiesRequest{_ResetBucketPropertiesRequest'bucket
                                                                 :: !Data.ByteString.ByteString,
                                                                 _ResetBucketPropertiesRequest'type'
                                                                 ::
                                                                 !(Prelude.Maybe
                                                                     Data.ByteString.ByteString),
                                                                 _ResetBucketPropertiesRequest'_unknownFields
                                                                 :: !Data.ProtoLens.FieldSet}
                                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetBucketPropertiesRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ResetBucketPropertiesRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ResetBucketPropertiesRequest'bucket
                 (\ x__ y__ -> x__{_ResetBucketPropertiesRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ResetBucketPropertiesRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ResetBucketPropertiesRequest'type'
                 (\ x__ y__ -> x__{_ResetBucketPropertiesRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' ResetBucketPropertiesRequest
           "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ResetBucketPropertiesRequest'type'
                 (\ x__ y__ -> x__{_ResetBucketPropertiesRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message ResetBucketPropertiesRequest where
        messageName _ = Data.Text.pack "ResetBucketPropertiesRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor ResetBucketPropertiesRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor ResetBucketPropertiesRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _ResetBucketPropertiesRequest'_unknownFields
              (\ x__ y__ ->
                 x__{_ResetBucketPropertiesRequest'_unknownFields = y__})
        defMessage
          = ResetBucketPropertiesRequest{_ResetBucketPropertiesRequest'bucket
                                           = Data.ProtoLens.fieldDefault,
                                         _ResetBucketPropertiesRequest'type' = Prelude.Nothing,
                                         _ResetBucketPropertiesRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData ResetBucketPropertiesRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_ResetBucketPropertiesRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ResetBucketPropertiesRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_ResetBucketPropertiesRequest'type' x__)
                      (())))
{- | Fields :

 -}
data ResetBucketPropertiesResponse = ResetBucketPropertiesResponse{_ResetBucketPropertiesResponse'_unknownFields
                                                                   :: !Data.ProtoLens.FieldSet}
                                       deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResetBucketPropertiesResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message ResetBucketPropertiesResponse where
        messageName _ = Data.Text.pack "ResetBucketPropertiesResponse"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens
              _ResetBucketPropertiesResponse'_unknownFields
              (\ x__ y__ ->
                 x__{_ResetBucketPropertiesResponse'_unknownFields = y__})
        defMessage
          = ResetBucketPropertiesResponse{_ResetBucketPropertiesResponse'_unknownFields
                                            = ([])}
instance Control.DeepSeq.NFData ResetBucketPropertiesResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_ResetBucketPropertiesResponse'_unknownFields x__)
                (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' SetBucketPropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.props' @:: Lens' SetBucketPropertiesRequest BucketProperties@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' SetBucketPropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' SetBucketPropertiesRequest
  (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data SetBucketPropertiesRequest = SetBucketPropertiesRequest{_SetBucketPropertiesRequest'bucket
                                                             :: !Data.ByteString.ByteString,
                                                             _SetBucketPropertiesRequest'props ::
                                                             !BucketProperties,
                                                             _SetBucketPropertiesRequest'type' ::
                                                             !(Prelude.Maybe
                                                                 Data.ByteString.ByteString),
                                                             _SetBucketPropertiesRequest'_unknownFields
                                                             :: !Data.ProtoLens.FieldSet}
                                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetBucketPropertiesRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' SetBucketPropertiesRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetBucketPropertiesRequest'bucket
                 (\ x__ y__ -> x__{_SetBucketPropertiesRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' SetBucketPropertiesRequest "props"
           (BucketProperties)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetBucketPropertiesRequest'props
                 (\ x__ y__ -> x__{_SetBucketPropertiesRequest'props = y__}))
              Prelude.id
instance Lens.Labels.HasLens' SetBucketPropertiesRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetBucketPropertiesRequest'type'
                 (\ x__ y__ -> x__{_SetBucketPropertiesRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' SetBucketPropertiesRequest
           "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetBucketPropertiesRequest'type'
                 (\ x__ y__ -> x__{_SetBucketPropertiesRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message SetBucketPropertiesRequest where
        messageName _ = Data.Text.pack "SetBucketPropertiesRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor SetBucketPropertiesRequest
                props__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "props"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor BucketProperties)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "props")))
                      :: Data.ProtoLens.FieldDescriptor SetBucketPropertiesRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor SetBucketPropertiesRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, props__field_descriptor),
                 (Data.ProtoLens.Tag 3, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _SetBucketPropertiesRequest'_unknownFields
              (\ x__ y__ ->
                 x__{_SetBucketPropertiesRequest'_unknownFields = y__})
        defMessage
          = SetBucketPropertiesRequest{_SetBucketPropertiesRequest'bucket =
                                         Data.ProtoLens.fieldDefault,
                                       _SetBucketPropertiesRequest'props =
                                         Data.ProtoLens.defMessage,
                                       _SetBucketPropertiesRequest'type' = Prelude.Nothing,
                                       _SetBucketPropertiesRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData SetBucketPropertiesRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_SetBucketPropertiesRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_SetBucketPropertiesRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_SetBucketPropertiesRequest'props x__)
                      (Control.DeepSeq.deepseq (_SetBucketPropertiesRequest'type' x__)
                         (()))))
{- | Fields :

 -}
data SetBucketPropertiesResponse = SetBucketPropertiesResponse{_SetBucketPropertiesResponse'_unknownFields
                                                               :: !Data.ProtoLens.FieldSet}
                                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetBucketPropertiesResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message SetBucketPropertiesResponse where
        messageName _ = Data.Text.pack "SetBucketPropertiesResponse"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens
              _SetBucketPropertiesResponse'_unknownFields
              (\ x__ y__ ->
                 x__{_SetBucketPropertiesResponse'_unknownFields = y__})
        defMessage
          = SetBucketPropertiesResponse{_SetBucketPropertiesResponse'_unknownFields
                                          = ([])}
instance Control.DeepSeq.NFData SetBucketPropertiesResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_SetBucketPropertiesResponse'_unknownFields x__)
                (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' SetBucketTypePropertiesRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.props' @:: Lens' SetBucketTypePropertiesRequest BucketProperties@
 -}
data SetBucketTypePropertiesRequest = SetBucketTypePropertiesRequest{_SetBucketTypePropertiesRequest'type'
                                                                     :: !Data.ByteString.ByteString,
                                                                     _SetBucketTypePropertiesRequest'props
                                                                     :: !BucketProperties,
                                                                     _SetBucketTypePropertiesRequest'_unknownFields
                                                                     :: !Data.ProtoLens.FieldSet}
                                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetBucketTypePropertiesRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' SetBucketTypePropertiesRequest
           "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetBucketTypePropertiesRequest'type'
                 (\ x__ y__ -> x__{_SetBucketTypePropertiesRequest'type' = y__}))
              Prelude.id
instance Lens.Labels.HasLens' SetBucketTypePropertiesRequest
           "props"
           (BucketProperties)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetBucketTypePropertiesRequest'props
                 (\ x__ y__ -> x__{_SetBucketTypePropertiesRequest'props = y__}))
              Prelude.id
instance Data.ProtoLens.Message SetBucketTypePropertiesRequest
         where
        messageName _ = Data.Text.pack "SetBucketTypePropertiesRequest"
        fieldsByTag
          = let type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor SetBucketTypePropertiesRequest
                props__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "props"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor BucketProperties)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "props")))
                      :: Data.ProtoLens.FieldDescriptor SetBucketTypePropertiesRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, type'__field_descriptor),
                 (Data.ProtoLens.Tag 2, props__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _SetBucketTypePropertiesRequest'_unknownFields
              (\ x__ y__ ->
                 x__{_SetBucketTypePropertiesRequest'_unknownFields = y__})
        defMessage
          = SetBucketTypePropertiesRequest{_SetBucketTypePropertiesRequest'type'
                                             = Data.ProtoLens.fieldDefault,
                                           _SetBucketTypePropertiesRequest'props =
                                             Data.ProtoLens.defMessage,
                                           _SetBucketTypePropertiesRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData SetBucketTypePropertiesRequest
         where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_SetBucketTypePropertiesRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq
                   (_SetBucketTypePropertiesRequest'type' x__)
                   (Control.DeepSeq.deepseq
                      (_SetBucketTypePropertiesRequest'props x__)
                      (())))
{- | Fields :

 -}
data SetBucketTypePropertiesResponse = SetBucketTypePropertiesResponse{_SetBucketTypePropertiesResponse'_unknownFields
                                                                       :: !Data.ProtoLens.FieldSet}
                                         deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetBucketTypePropertiesResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message SetBucketTypePropertiesResponse
         where
        messageName _ = Data.Text.pack "SetBucketTypePropertiesResponse"
        fieldsByTag = let in Data.Map.fromList []
        unknownFields
          = Lens.Family2.Unchecked.lens
              _SetBucketTypePropertiesResponse'_unknownFields
              (\ x__ y__ ->
                 x__{_SetBucketTypePropertiesResponse'_unknownFields = y__})
        defMessage
          = SetBucketTypePropertiesResponse{_SetBucketTypePropertiesResponse'_unknownFields
                                              = ([])}
instance Control.DeepSeq.NFData SetBucketTypePropertiesResponse
         where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_SetBucketTypePropertiesResponse'_unknownFields x__)
                (())
{- | Fields :

    * 'Proto.Proto.Riak_Fields.adds' @:: Lens' SetUpdate [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.removes' @:: Lens' SetUpdate [Data.ByteString.ByteString]@
 -}
data SetUpdate = SetUpdate{_SetUpdate'adds ::
                           ![Data.ByteString.ByteString],
                           _SetUpdate'removes :: ![Data.ByteString.ByteString],
                           _SetUpdate'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show SetUpdate where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' SetUpdate "adds"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetUpdate'adds
                 (\ x__ y__ -> x__{_SetUpdate'adds = y__}))
              Prelude.id
instance Lens.Labels.HasLens' SetUpdate "removes"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _SetUpdate'removes
                 (\ x__ y__ -> x__{_SetUpdate'removes = y__}))
              Prelude.id
instance Data.ProtoLens.Message SetUpdate where
        messageName _ = Data.Text.pack "SetUpdate"
        fieldsByTag
          = let adds__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "adds"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "adds")))
                      :: Data.ProtoLens.FieldDescriptor SetUpdate
                removes__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "removes"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "removes")))
                      :: Data.ProtoLens.FieldDescriptor SetUpdate
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, adds__field_descriptor),
                 (Data.ProtoLens.Tag 2, removes__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _SetUpdate'_unknownFields
              (\ x__ y__ -> x__{_SetUpdate'_unknownFields = y__})
        defMessage
          = SetUpdate{_SetUpdate'adds = [], _SetUpdate'removes = [],
                      _SetUpdate'_unknownFields = ([])}
instance Control.DeepSeq.NFData SetUpdate where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_SetUpdate'_unknownFields x__)
                (Control.DeepSeq.deepseq (_SetUpdate'adds x__)
                   (Control.DeepSeq.deepseq (_SetUpdate'removes x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' StreamBucketsRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' StreamBucketsRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.stream' @:: Lens' StreamBucketsRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'stream' @:: Lens' StreamBucketsRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' StreamBucketsRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' StreamBucketsRequest
  (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data StreamBucketsRequest = StreamBucketsRequest{_StreamBucketsRequest'timeout
                                                 :: !(Prelude.Maybe Data.Word.Word32),
                                                 _StreamBucketsRequest'stream ::
                                                 !(Prelude.Maybe Prelude.Bool),
                                                 _StreamBucketsRequest'type' ::
                                                 !(Prelude.Maybe Data.ByteString.ByteString),
                                                 _StreamBucketsRequest'_unknownFields ::
                                                 !Data.ProtoLens.FieldSet}
                              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamBucketsRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' StreamBucketsRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsRequest'timeout
                 (\ x__ y__ -> x__{_StreamBucketsRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamBucketsRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsRequest'timeout
                 (\ x__ y__ -> x__{_StreamBucketsRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' StreamBucketsRequest "stream"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsRequest'stream
                 (\ x__ y__ -> x__{_StreamBucketsRequest'stream = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamBucketsRequest "maybe'stream"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsRequest'stream
                 (\ x__ y__ -> x__{_StreamBucketsRequest'stream = y__}))
              Prelude.id
instance Lens.Labels.HasLens' StreamBucketsRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsRequest'type'
                 (\ x__ y__ -> x__{_StreamBucketsRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamBucketsRequest "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsRequest'type'
                 (\ x__ y__ -> x__{_StreamBucketsRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message StreamBucketsRequest where
        messageName _ = Data.Text.pack "StreamBucketsRequest"
        fieldsByTag
          = let timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor StreamBucketsRequest
                stream__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "stream"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'stream")))
                      :: Data.ProtoLens.FieldDescriptor StreamBucketsRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor StreamBucketsRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 2, stream__field_descriptor),
                 (Data.ProtoLens.Tag 3, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _StreamBucketsRequest'_unknownFields
              (\ x__ y__ -> x__{_StreamBucketsRequest'_unknownFields = y__})
        defMessage
          = StreamBucketsRequest{_StreamBucketsRequest'timeout =
                                   Prelude.Nothing,
                                 _StreamBucketsRequest'stream = Prelude.Nothing,
                                 _StreamBucketsRequest'type' = Prelude.Nothing,
                                 _StreamBucketsRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData StreamBucketsRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_StreamBucketsRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_StreamBucketsRequest'timeout x__)
                   (Control.DeepSeq.deepseq (_StreamBucketsRequest'stream x__)
                      (Control.DeepSeq.deepseq (_StreamBucketsRequest'type' x__) (()))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.buckets' @:: Lens' StreamBucketsResponse [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.done' @:: Lens' StreamBucketsResponse Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'done' @:: Lens' StreamBucketsResponse (Prelude.Maybe Prelude.Bool)@
 -}
data StreamBucketsResponse = StreamBucketsResponse{_StreamBucketsResponse'buckets
                                                   :: ![Data.ByteString.ByteString],
                                                   _StreamBucketsResponse'done ::
                                                   !(Prelude.Maybe Prelude.Bool),
                                                   _StreamBucketsResponse'_unknownFields ::
                                                   !Data.ProtoLens.FieldSet}
                               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamBucketsResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' StreamBucketsResponse "buckets"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsResponse'buckets
                 (\ x__ y__ -> x__{_StreamBucketsResponse'buckets = y__}))
              Prelude.id
instance Lens.Labels.HasLens' StreamBucketsResponse "done"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsResponse'done
                 (\ x__ y__ -> x__{_StreamBucketsResponse'done = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamBucketsResponse "maybe'done"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamBucketsResponse'done
                 (\ x__ y__ -> x__{_StreamBucketsResponse'done = y__}))
              Prelude.id
instance Data.ProtoLens.Message StreamBucketsResponse where
        messageName _ = Data.Text.pack "StreamBucketsResponse"
        fieldsByTag
          = let buckets__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "buckets"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "buckets")))
                      :: Data.ProtoLens.FieldDescriptor StreamBucketsResponse
                done__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "done"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'done")))
                      :: Data.ProtoLens.FieldDescriptor StreamBucketsResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, buckets__field_descriptor),
                 (Data.ProtoLens.Tag 2, done__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _StreamBucketsResponse'_unknownFields
              (\ x__ y__ -> x__{_StreamBucketsResponse'_unknownFields = y__})
        defMessage
          = StreamBucketsResponse{_StreamBucketsResponse'buckets = [],
                                  _StreamBucketsResponse'done = Prelude.Nothing,
                                  _StreamBucketsResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData StreamBucketsResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_StreamBucketsResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_StreamBucketsResponse'buckets x__)
                   (Control.DeepSeq.deepseq (_StreamBucketsResponse'done x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' StreamKeysRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' StreamKeysRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' StreamKeysRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' StreamKeysRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'type'' @:: Lens' StreamKeysRequest (Prelude.Maybe Data.ByteString.ByteString)@
 -}
data StreamKeysRequest = StreamKeysRequest{_StreamKeysRequest'bucket
                                           :: !Data.ByteString.ByteString,
                                           _StreamKeysRequest'timeout ::
                                           !(Prelude.Maybe Data.Word.Word32),
                                           _StreamKeysRequest'type' ::
                                           !(Prelude.Maybe Data.ByteString.ByteString),
                                           _StreamKeysRequest'_unknownFields ::
                                           !Data.ProtoLens.FieldSet}
                           deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamKeysRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' StreamKeysRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysRequest'bucket
                 (\ x__ y__ -> x__{_StreamKeysRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' StreamKeysRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysRequest'timeout
                 (\ x__ y__ -> x__{_StreamKeysRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamKeysRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysRequest'timeout
                 (\ x__ y__ -> x__{_StreamKeysRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' StreamKeysRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysRequest'type'
                 (\ x__ y__ -> x__{_StreamKeysRequest'type' = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamKeysRequest "maybe'type'"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysRequest'type'
                 (\ x__ y__ -> x__{_StreamKeysRequest'type' = y__}))
              Prelude.id
instance Data.ProtoLens.Message StreamKeysRequest where
        messageName _ = Data.Text.pack "StreamKeysRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor StreamKeysRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor StreamKeysRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'type'")))
                      :: Data.ProtoLens.FieldDescriptor StreamKeysRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 3, type'__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _StreamKeysRequest'_unknownFields
              (\ x__ y__ -> x__{_StreamKeysRequest'_unknownFields = y__})
        defMessage
          = StreamKeysRequest{_StreamKeysRequest'bucket =
                                Data.ProtoLens.fieldDefault,
                              _StreamKeysRequest'timeout = Prelude.Nothing,
                              _StreamKeysRequest'type' = Prelude.Nothing,
                              _StreamKeysRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData StreamKeysRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_StreamKeysRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_StreamKeysRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_StreamKeysRequest'timeout x__)
                      (Control.DeepSeq.deepseq (_StreamKeysRequest'type' x__) (()))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.keys' @:: Lens' StreamKeysResponse [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.done' @:: Lens' StreamKeysResponse Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'done' @:: Lens' StreamKeysResponse (Prelude.Maybe Prelude.Bool)@
 -}
data StreamKeysResponse = StreamKeysResponse{_StreamKeysResponse'keys
                                             :: ![Data.ByteString.ByteString],
                                             _StreamKeysResponse'done ::
                                             !(Prelude.Maybe Prelude.Bool),
                                             _StreamKeysResponse'_unknownFields ::
                                             !Data.ProtoLens.FieldSet}
                            deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StreamKeysResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' StreamKeysResponse "keys"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysResponse'keys
                 (\ x__ y__ -> x__{_StreamKeysResponse'keys = y__}))
              Prelude.id
instance Lens.Labels.HasLens' StreamKeysResponse "done"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysResponse'done
                 (\ x__ y__ -> x__{_StreamKeysResponse'done = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' StreamKeysResponse "maybe'done"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _StreamKeysResponse'done
                 (\ x__ y__ -> x__{_StreamKeysResponse'done = y__}))
              Prelude.id
instance Data.ProtoLens.Message StreamKeysResponse where
        messageName _ = Data.Text.pack "StreamKeysResponse"
        fieldsByTag
          = let keys__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "keys"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "keys")))
                      :: Data.ProtoLens.FieldDescriptor StreamKeysResponse
                done__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "done"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'done")))
                      :: Data.ProtoLens.FieldDescriptor StreamKeysResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, keys__field_descriptor),
                 (Data.ProtoLens.Tag 2, done__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _StreamKeysResponse'_unknownFields
              (\ x__ y__ -> x__{_StreamKeysResponse'_unknownFields = y__})
        defMessage
          = StreamKeysResponse{_StreamKeysResponse'keys = [],
                               _StreamKeysResponse'done = Prelude.Nothing,
                               _StreamKeysResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData StreamKeysResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_StreamKeysResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_StreamKeysResponse'keys x__)
                   (Control.DeepSeq.deepseq (_StreamKeysResponse'done x__) (())))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.bucket' @:: Lens' UpdateCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.key' @:: Lens' UpdateCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'key' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.type'' @:: Lens' UpdateCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.context' @:: Lens' UpdateCrdtRequest Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'context' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.update' @:: Lens' UpdateCrdtRequest CrdtUpdate@
    * 'Proto.Proto.Riak_Fields.w' @:: Lens' UpdateCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'w' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.dw' @:: Lens' UpdateCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'dw' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.pw' @:: Lens' UpdateCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'pw' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.returnBody' @:: Lens' UpdateCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'returnBody' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.timeout' @:: Lens' UpdateCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'timeout' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.sloppyQuorum' @:: Lens' UpdateCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'sloppyQuorum' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Prelude.Bool)@
    * 'Proto.Proto.Riak_Fields.n' @:: Lens' UpdateCrdtRequest Data.Word.Word32@
    * 'Proto.Proto.Riak_Fields.maybe'n' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Data.Word.Word32)@
    * 'Proto.Proto.Riak_Fields.includeContext' @:: Lens' UpdateCrdtRequest Prelude.Bool@
    * 'Proto.Proto.Riak_Fields.maybe'includeContext' @:: Lens' UpdateCrdtRequest (Prelude.Maybe Prelude.Bool)@
 -}
data UpdateCrdtRequest = UpdateCrdtRequest{_UpdateCrdtRequest'bucket
                                           :: !Data.ByteString.ByteString,
                                           _UpdateCrdtRequest'key ::
                                           !(Prelude.Maybe Data.ByteString.ByteString),
                                           _UpdateCrdtRequest'type' :: !Data.ByteString.ByteString,
                                           _UpdateCrdtRequest'context ::
                                           !(Prelude.Maybe Data.ByteString.ByteString),
                                           _UpdateCrdtRequest'update :: !CrdtUpdate,
                                           _UpdateCrdtRequest'w ::
                                           !(Prelude.Maybe Data.Word.Word32),
                                           _UpdateCrdtRequest'dw ::
                                           !(Prelude.Maybe Data.Word.Word32),
                                           _UpdateCrdtRequest'pw ::
                                           !(Prelude.Maybe Data.Word.Word32),
                                           _UpdateCrdtRequest'returnBody ::
                                           !(Prelude.Maybe Prelude.Bool),
                                           _UpdateCrdtRequest'timeout ::
                                           !(Prelude.Maybe Data.Word.Word32),
                                           _UpdateCrdtRequest'sloppyQuorum ::
                                           !(Prelude.Maybe Prelude.Bool),
                                           _UpdateCrdtRequest'n ::
                                           !(Prelude.Maybe Data.Word.Word32),
                                           _UpdateCrdtRequest'includeContext ::
                                           !(Prelude.Maybe Prelude.Bool),
                                           _UpdateCrdtRequest'_unknownFields ::
                                           !Data.ProtoLens.FieldSet}
                           deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateCrdtRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' UpdateCrdtRequest "bucket"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'bucket
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'bucket = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'key
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'key = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'key"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'key
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "type'"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'type'
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'type' = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "context"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'context
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'context = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'context"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'context
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'context = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "update"
           (CrdtUpdate)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'update
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'update = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "w"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'w
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'w = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'w"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'w
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'w = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "dw"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'dw
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'dw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'dw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'dw
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'dw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "pw"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'pw
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'pw = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'pw"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'pw
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'pw = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "returnBody"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'returnBody
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'returnBody = y__}))
              (Data.ProtoLens.maybeLens Prelude.False)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'returnBody"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'returnBody
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'returnBody = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "timeout"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'timeout
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'timeout = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'timeout"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'timeout
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'timeout = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "sloppyQuorum"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'sloppyQuorum = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest
           "maybe'sloppyQuorum"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'sloppyQuorum
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'sloppyQuorum = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "n"
           (Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'n
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'n = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtRequest "maybe'n"
           (Prelude.Maybe Data.Word.Word32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'n
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'n = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtRequest "includeContext"
           (Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'includeContext
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'includeContext = y__}))
              (Data.ProtoLens.maybeLens Prelude.True)
instance Lens.Labels.HasLens' UpdateCrdtRequest
           "maybe'includeContext"
           (Prelude.Maybe Prelude.Bool)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtRequest'includeContext
                 (\ x__ y__ -> x__{_UpdateCrdtRequest'includeContext = y__}))
              Prelude.id
instance Data.ProtoLens.Message UpdateCrdtRequest where
        messageName _ = Data.Text.pack "UpdateCrdtRequest"
        fieldsByTag
          = let bucket__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "bucket"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                type'__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                context__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "context"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'context")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                update__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "update"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor CrdtUpdate)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Required
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "update")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                w__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "w"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'w")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                dw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "dw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dw")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                pw__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "pw"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pw")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                returnBody__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "return_body"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnBody")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                timeout__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timeout"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                sloppyQuorum__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sloppy_quorum"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'sloppyQuorum")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                n__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "n"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
                includeContext__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "include_context"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                         Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) ::
                               (Lens.Labels.Proxy#) "maybe'includeContext")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, bucket__field_descriptor),
                 (Data.ProtoLens.Tag 2, key__field_descriptor),
                 (Data.ProtoLens.Tag 3, type'__field_descriptor),
                 (Data.ProtoLens.Tag 4, context__field_descriptor),
                 (Data.ProtoLens.Tag 5, update__field_descriptor),
                 (Data.ProtoLens.Tag 6, w__field_descriptor),
                 (Data.ProtoLens.Tag 7, dw__field_descriptor),
                 (Data.ProtoLens.Tag 8, pw__field_descriptor),
                 (Data.ProtoLens.Tag 9, returnBody__field_descriptor),
                 (Data.ProtoLens.Tag 10, timeout__field_descriptor),
                 (Data.ProtoLens.Tag 11, sloppyQuorum__field_descriptor),
                 (Data.ProtoLens.Tag 12, n__field_descriptor),
                 (Data.ProtoLens.Tag 13, includeContext__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _UpdateCrdtRequest'_unknownFields
              (\ x__ y__ -> x__{_UpdateCrdtRequest'_unknownFields = y__})
        defMessage
          = UpdateCrdtRequest{_UpdateCrdtRequest'bucket =
                                Data.ProtoLens.fieldDefault,
                              _UpdateCrdtRequest'key = Prelude.Nothing,
                              _UpdateCrdtRequest'type' = Data.ProtoLens.fieldDefault,
                              _UpdateCrdtRequest'context = Prelude.Nothing,
                              _UpdateCrdtRequest'update = Data.ProtoLens.defMessage,
                              _UpdateCrdtRequest'w = Prelude.Nothing,
                              _UpdateCrdtRequest'dw = Prelude.Nothing,
                              _UpdateCrdtRequest'pw = Prelude.Nothing,
                              _UpdateCrdtRequest'returnBody = Prelude.Nothing,
                              _UpdateCrdtRequest'timeout = Prelude.Nothing,
                              _UpdateCrdtRequest'sloppyQuorum = Prelude.Nothing,
                              _UpdateCrdtRequest'n = Prelude.Nothing,
                              _UpdateCrdtRequest'includeContext = Prelude.Nothing,
                              _UpdateCrdtRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData UpdateCrdtRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_UpdateCrdtRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_UpdateCrdtRequest'bucket x__)
                   (Control.DeepSeq.deepseq (_UpdateCrdtRequest'key x__)
                      (Control.DeepSeq.deepseq (_UpdateCrdtRequest'type' x__)
                         (Control.DeepSeq.deepseq (_UpdateCrdtRequest'context x__)
                            (Control.DeepSeq.deepseq (_UpdateCrdtRequest'update x__)
                               (Control.DeepSeq.deepseq (_UpdateCrdtRequest'w x__)
                                  (Control.DeepSeq.deepseq (_UpdateCrdtRequest'dw x__)
                                     (Control.DeepSeq.deepseq (_UpdateCrdtRequest'pw x__)
                                        (Control.DeepSeq.deepseq (_UpdateCrdtRequest'returnBody x__)
                                           (Control.DeepSeq.deepseq (_UpdateCrdtRequest'timeout x__)
                                              (Control.DeepSeq.deepseq
                                                 (_UpdateCrdtRequest'sloppyQuorum x__)
                                                 (Control.DeepSeq.deepseq (_UpdateCrdtRequest'n x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_UpdateCrdtRequest'includeContext x__)
                                                       (()))))))))))))))
{- | Fields :

    * 'Proto.Proto.Riak_Fields.key' @:: Lens' UpdateCrdtResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'key' @:: Lens' UpdateCrdtResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.context' @:: Lens' UpdateCrdtResponse Data.ByteString.ByteString@
    * 'Proto.Proto.Riak_Fields.maybe'context' @:: Lens' UpdateCrdtResponse (Prelude.Maybe Data.ByteString.ByteString)@
    * 'Proto.Proto.Riak_Fields.counter' @:: Lens' UpdateCrdtResponse Data.Int.Int64@
    * 'Proto.Proto.Riak_Fields.maybe'counter' @:: Lens' UpdateCrdtResponse (Prelude.Maybe Data.Int.Int64)@
    * 'Proto.Proto.Riak_Fields.set' @:: Lens' UpdateCrdtResponse [Data.ByteString.ByteString]@
    * 'Proto.Proto.Riak_Fields.map' @:: Lens' UpdateCrdtResponse [MapEntry]@
    * 'Proto.Proto.Riak_Fields.hll' @:: Lens' UpdateCrdtResponse Data.Word.Word64@
    * 'Proto.Proto.Riak_Fields.maybe'hll' @:: Lens' UpdateCrdtResponse (Prelude.Maybe Data.Word.Word64)@
    * 'Proto.Proto.Riak_Fields.gset' @:: Lens' UpdateCrdtResponse [Data.ByteString.ByteString]@
 -}
data UpdateCrdtResponse = UpdateCrdtResponse{_UpdateCrdtResponse'key
                                             :: !(Prelude.Maybe Data.ByteString.ByteString),
                                             _UpdateCrdtResponse'context ::
                                             !(Prelude.Maybe Data.ByteString.ByteString),
                                             _UpdateCrdtResponse'counter ::
                                             !(Prelude.Maybe Data.Int.Int64),
                                             _UpdateCrdtResponse'set ::
                                             ![Data.ByteString.ByteString],
                                             _UpdateCrdtResponse'map :: ![MapEntry],
                                             _UpdateCrdtResponse'hll ::
                                             !(Prelude.Maybe Data.Word.Word64),
                                             _UpdateCrdtResponse'gset ::
                                             ![Data.ByteString.ByteString],
                                             _UpdateCrdtResponse'_unknownFields ::
                                             !Data.ProtoLens.FieldSet}
                            deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateCrdtResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' UpdateCrdtResponse "key"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'key
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'key = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtResponse "maybe'key"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'key
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'key = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtResponse "context"
           (Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'context
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'context = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtResponse "maybe'context"
           (Prelude.Maybe Data.ByteString.ByteString)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'context
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'context = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtResponse "counter"
           (Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'counter
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'counter = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtResponse "maybe'counter"
           (Prelude.Maybe Data.Int.Int64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'counter
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'counter = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtResponse "set"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'set
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'set = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtResponse "map" ([MapEntry])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'map
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'map = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtResponse "hll"
           (Data.Word.Word64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'hll
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'hll = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.fieldDefault)
instance Lens.Labels.HasLens' UpdateCrdtResponse "maybe'hll"
           (Prelude.Maybe Data.Word.Word64)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'hll
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'hll = y__}))
              Prelude.id
instance Lens.Labels.HasLens' UpdateCrdtResponse "gset"
           ([Data.ByteString.ByteString])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _UpdateCrdtResponse'gset
                 (\ x__ y__ -> x__{_UpdateCrdtResponse'gset = y__}))
              Prelude.id
instance Data.ProtoLens.Message UpdateCrdtResponse where
        messageName _ = Data.Text.pack "UpdateCrdtResponse"
        fieldsByTag
          = let key__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "key"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
                context__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "context"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'context")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
                counter__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "counter"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.SInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'counter")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
                set__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "set"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "set")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
                map__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "map"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MapEntry)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "map")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
                hll__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "hll"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hll")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
                gset__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "gset"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "gset")))
                      :: Data.ProtoLens.FieldDescriptor UpdateCrdtResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, key__field_descriptor),
                 (Data.ProtoLens.Tag 2, context__field_descriptor),
                 (Data.ProtoLens.Tag 3, counter__field_descriptor),
                 (Data.ProtoLens.Tag 4, set__field_descriptor),
                 (Data.ProtoLens.Tag 5, map__field_descriptor),
                 (Data.ProtoLens.Tag 6, hll__field_descriptor),
                 (Data.ProtoLens.Tag 7, gset__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _UpdateCrdtResponse'_unknownFields
              (\ x__ y__ -> x__{_UpdateCrdtResponse'_unknownFields = y__})
        defMessage
          = UpdateCrdtResponse{_UpdateCrdtResponse'key = Prelude.Nothing,
                               _UpdateCrdtResponse'context = Prelude.Nothing,
                               _UpdateCrdtResponse'counter = Prelude.Nothing,
                               _UpdateCrdtResponse'set = [], _UpdateCrdtResponse'map = [],
                               _UpdateCrdtResponse'hll = Prelude.Nothing,
                               _UpdateCrdtResponse'gset = [],
                               _UpdateCrdtResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData UpdateCrdtResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_UpdateCrdtResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_UpdateCrdtResponse'key x__)
                   (Control.DeepSeq.deepseq (_UpdateCrdtResponse'context x__)
                      (Control.DeepSeq.deepseq (_UpdateCrdtResponse'counter x__)
                         (Control.DeepSeq.deepseq (_UpdateCrdtResponse'set x__)
                            (Control.DeepSeq.deepseq (_UpdateCrdtResponse'map x__)
                               (Control.DeepSeq.deepseq (_UpdateCrdtResponse'hll x__)
                                  (Control.DeepSeq.deepseq (_UpdateCrdtResponse'gset x__)
                                     (()))))))))