{- This file was auto-generated from proto/riak.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Proto.Riak_Fields where
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

adds ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "adds" a) =>
       Lens.Family2.LensLike' f s a
adds
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "adds")
allowMult ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "allowMult" a) =>
            Lens.Family2.LensLike' f s a
allowMult
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "allowMult")
asis ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "asis" a) =>
       Lens.Family2.LensLike' f s a
asis
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "asis")
backend ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "backend" a) =>
          Lens.Family2.LensLike' f s a
backend
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "backend")
basicQuorum ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "basicQuorum" a) =>
              Lens.Family2.LensLike' f s a
basicQuorum
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "basicQuorum")
bigVclock ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "bigVclock" a) =>
            Lens.Family2.LensLike' f s a
bigVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bigVclock")
bucket ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "bucket" a) =>
         Lens.Family2.LensLike' f s a
bucket
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucket")
bucketType ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "bucketType" a) =>
             Lens.Family2.LensLike' f s a
bucketType
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "bucketType")
buckets ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "buckets" a) =>
          Lens.Family2.LensLike' f s a
buckets
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "buckets")
charset ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "charset" a) =>
          Lens.Family2.LensLike' f s a
charset
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "charset")
chashKeyfun ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "chashKeyfun" a) =>
              Lens.Family2.LensLike' f s a
chashKeyfun
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "chashKeyfun")
code ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "code" a) =>
       Lens.Family2.LensLike' f s a
code
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "code")
consistent ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "consistent" a) =>
             Lens.Family2.LensLike' f s a
consistent
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "consistent")
content ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "content" a) =>
          Lens.Family2.LensLike' f s a
content
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "content")
contentEncoding ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "contentEncoding" a) =>
                  Lens.Family2.LensLike' f s a
contentEncoding
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "contentEncoding")
contentType ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "contentType" a) =>
              Lens.Family2.LensLike' f s a
contentType
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "contentType")
context ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "context" a) =>
          Lens.Family2.LensLike' f s a
context
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "context")
continuation ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "continuation" a) =>
               Lens.Family2.LensLike' f s a
continuation
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "continuation")
counter ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "counter" a) =>
          Lens.Family2.LensLike' f s a
counter
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "counter")
counterUpdate ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "counterUpdate" a) =>
                Lens.Family2.LensLike' f s a
counterUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "counterUpdate")
coverContext ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "coverContext" a) =>
               Lens.Family2.LensLike' f s a
coverContext
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "coverContext")
datatype ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "datatype" a) =>
           Lens.Family2.LensLike' f s a
datatype
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "datatype")
defaultField ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "defaultField" a) =>
               Lens.Family2.LensLike' f s a
defaultField
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "defaultField")
defaultOp ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "defaultOp" a) =>
            Lens.Family2.LensLike' f s a
defaultOp
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "defaultOp")
deleted ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "deleted" a) =>
          Lens.Family2.LensLike' f s a
deleted
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "deleted")
deletedContext ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "deletedContext" a) =>
                 Lens.Family2.LensLike' f s a
deletedContext
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "deletedContext")
docs ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "docs" a) =>
       Lens.Family2.LensLike' f s a
docs
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docs")
done ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "done" a) =>
       Lens.Family2.LensLike' f s a
done
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "done")
dw ::
   forall f s a .
     (Prelude.Functor f, Lens.Labels.HasLens' s "dw" a) =>
     Lens.Family2.LensLike' f s a
dw
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "dw")
errmsg ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "errmsg" a) =>
         Lens.Family2.LensLike' f s a
errmsg
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errmsg")
field ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "field" a) =>
        Lens.Family2.LensLike' f s a
field
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "field")
fieldList ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "fieldList" a) =>
            Lens.Family2.LensLike' f s a
fieldList
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "fieldList")
fields ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "fields" a) =>
         Lens.Family2.LensLike' f s a
fields
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "fields")
filter ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "filter" a) =>
         Lens.Family2.LensLike' f s a
filter
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "filter")
flag ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "flag" a) =>
       Lens.Family2.LensLike' f s a
flag
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "flag")
flagUpdate ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "flagUpdate" a) =>
             Lens.Family2.LensLike' f s a
flagUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "flagUpdate")
function ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "function" a) =>
           Lens.Family2.LensLike' f s a
function
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "function")
gset ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "gset" a) =>
       Lens.Family2.LensLike' f s a
gset
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "gset")
gsetUpdate ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "gsetUpdate" a) =>
             Lens.Family2.LensLike' f s a
gsetUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "gsetUpdate")
hasPostcommit ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "hasPostcommit" a) =>
                Lens.Family2.LensLike' f s a
hasPostcommit
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hasPostcommit")
hasPrecommit ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "hasPrecommit" a) =>
               Lens.Family2.LensLike' f s a
hasPrecommit
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hasPrecommit")
head ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "head" a) =>
       Lens.Family2.LensLike' f s a
head
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "head")
hll ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "hll" a) =>
      Lens.Family2.LensLike' f s a
hll
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hll")
hllPrecision ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "hllPrecision" a) =>
               Lens.Family2.LensLike' f s a
hllPrecision
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hllPrecision")
hllUpdate ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "hllUpdate" a) =>
            Lens.Family2.LensLike' f s a
hllUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hllUpdate")
ifModified ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "ifModified" a) =>
             Lens.Family2.LensLike' f s a
ifModified
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "ifModified")
ifNoneMatch ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "ifNoneMatch" a) =>
              Lens.Family2.LensLike' f s a
ifNoneMatch
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "ifNoneMatch")
ifNotModified ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "ifNotModified" a) =>
                Lens.Family2.LensLike' f s a
ifNotModified
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "ifNotModified")
includeContext ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "includeContext" a) =>
                 Lens.Family2.LensLike' f s a
includeContext
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "includeContext")
increment ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "increment" a) =>
            Lens.Family2.LensLike' f s a
increment
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "increment")
index ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "index" a) =>
        Lens.Family2.LensLike' f s a
index
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "index")
indexes ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "indexes" a) =>
          Lens.Family2.LensLike' f s a
indexes
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "indexes")
key ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "key" a) =>
      Lens.Family2.LensLike' f s a
key
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "key")
keys ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "keys" a) =>
       Lens.Family2.LensLike' f s a
keys
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "keys")
lastMod ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "lastMod" a) =>
          Lens.Family2.LensLike' f s a
lastMod
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "lastMod")
lastModUsecs ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "lastModUsecs" a) =>
               Lens.Family2.LensLike' f s a
lastModUsecs
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "lastModUsecs")
lastWriteWins ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "lastWriteWins" a) =>
                Lens.Family2.LensLike' f s a
lastWriteWins
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "lastWriteWins")
linkfun ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "linkfun" a) =>
          Lens.Family2.LensLike' f s a
linkfun
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "linkfun")
links ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "links" a) =>
        Lens.Family2.LensLike' f s a
links
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "links")
map ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "map" a) =>
      Lens.Family2.LensLike' f s a
map
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "map")
mapUpdate ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "mapUpdate" a) =>
            Lens.Family2.LensLike' f s a
mapUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "mapUpdate")
maxResults ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maxResults" a) =>
             Lens.Family2.LensLike' f s a
maxResults
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maxResults")
maxScore ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "maxScore" a) =>
           Lens.Family2.LensLike' f s a
maxScore
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maxScore")
maybe'allowMult ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'allowMult" a) =>
                  Lens.Family2.LensLike' f s a
maybe'allowMult
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'allowMult")
maybe'asis ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'asis" a) =>
             Lens.Family2.LensLike' f s a
maybe'asis
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'asis")
maybe'backend ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'backend" a) =>
                Lens.Family2.LensLike' f s a
maybe'backend
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'backend")
maybe'basicQuorum ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'basicQuorum" a) =>
                    Lens.Family2.LensLike' f s a
maybe'basicQuorum
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'basicQuorum")
maybe'bigVclock ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'bigVclock" a) =>
                  Lens.Family2.LensLike' f s a
maybe'bigVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'bigVclock")
maybe'bucket ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'bucket" a) =>
               Lens.Family2.LensLike' f s a
maybe'bucket
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'bucket")
maybe'bucketType ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'bucketType" a) =>
                   Lens.Family2.LensLike' f s a
maybe'bucketType
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'bucketType")
maybe'charset ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'charset" a) =>
                Lens.Family2.LensLike' f s a
maybe'charset
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'charset")
maybe'chashKeyfun ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'chashKeyfun" a) =>
                    Lens.Family2.LensLike' f s a
maybe'chashKeyfun
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'chashKeyfun")
maybe'consistent ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'consistent" a) =>
                   Lens.Family2.LensLike' f s a
maybe'consistent
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'consistent")
maybe'content ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'content" a) =>
                Lens.Family2.LensLike' f s a
maybe'content
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'content")
maybe'contentEncoding ::
                      forall f s a .
                        (Prelude.Functor f,
                         Lens.Labels.HasLens' s "maybe'contentEncoding" a) =>
                        Lens.Family2.LensLike' f s a
maybe'contentEncoding
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'contentEncoding")
maybe'contentType ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'contentType" a) =>
                    Lens.Family2.LensLike' f s a
maybe'contentType
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'contentType")
maybe'context ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'context" a) =>
                Lens.Family2.LensLike' f s a
maybe'context
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'context")
maybe'continuation ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'continuation" a) =>
                     Lens.Family2.LensLike' f s a
maybe'continuation
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'continuation")
maybe'counter ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'counter" a) =>
                Lens.Family2.LensLike' f s a
maybe'counter
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'counter")
maybe'counterUpdate ::
                    forall f s a .
                      (Prelude.Functor f,
                       Lens.Labels.HasLens' s "maybe'counterUpdate" a) =>
                      Lens.Family2.LensLike' f s a
maybe'counterUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'counterUpdate")
maybe'coverContext ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'coverContext" a) =>
                     Lens.Family2.LensLike' f s a
maybe'coverContext
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'coverContext")
maybe'datatype ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'datatype" a) =>
                 Lens.Family2.LensLike' f s a
maybe'datatype
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'datatype")
maybe'defaultField ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'defaultField" a) =>
                     Lens.Family2.LensLike' f s a
maybe'defaultField
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'defaultField")
maybe'defaultOp ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'defaultOp" a) =>
                  Lens.Family2.LensLike' f s a
maybe'defaultOp
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'defaultOp")
maybe'deleted ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'deleted" a) =>
                Lens.Family2.LensLike' f s a
maybe'deleted
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'deleted")
maybe'deletedContext ::
                     forall f s a .
                       (Prelude.Functor f,
                        Lens.Labels.HasLens' s "maybe'deletedContext" a) =>
                       Lens.Family2.LensLike' f s a
maybe'deletedContext
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'deletedContext")
maybe'done ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'done" a) =>
             Lens.Family2.LensLike' f s a
maybe'done
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'done")
maybe'dw ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'dw" a) =>
           Lens.Family2.LensLike' f s a
maybe'dw
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'dw")
maybe'filter ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'filter" a) =>
               Lens.Family2.LensLike' f s a
maybe'filter
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'filter")
maybe'flag ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'flag" a) =>
             Lens.Family2.LensLike' f s a
maybe'flag
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'flag")
maybe'flagUpdate ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'flagUpdate" a) =>
                   Lens.Family2.LensLike' f s a
maybe'flagUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'flagUpdate")
maybe'gsetUpdate ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'gsetUpdate" a) =>
                   Lens.Family2.LensLike' f s a
maybe'gsetUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'gsetUpdate")
maybe'hasPostcommit ::
                    forall f s a .
                      (Prelude.Functor f,
                       Lens.Labels.HasLens' s "maybe'hasPostcommit" a) =>
                      Lens.Family2.LensLike' f s a
maybe'hasPostcommit
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'hasPostcommit")
maybe'hasPrecommit ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'hasPrecommit" a) =>
                     Lens.Family2.LensLike' f s a
maybe'hasPrecommit
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hasPrecommit")
maybe'head ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'head" a) =>
             Lens.Family2.LensLike' f s a
maybe'head
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'head")
maybe'hll ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'hll" a) =>
            Lens.Family2.LensLike' f s a
maybe'hll
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hll")
maybe'hllPrecision ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'hllPrecision" a) =>
                     Lens.Family2.LensLike' f s a
maybe'hllPrecision
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hllPrecision")
maybe'hllUpdate ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'hllUpdate" a) =>
                  Lens.Family2.LensLike' f s a
maybe'hllUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'hllUpdate")
maybe'ifModified ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'ifModified" a) =>
                   Lens.Family2.LensLike' f s a
maybe'ifModified
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'ifModified")
maybe'ifNoneMatch ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'ifNoneMatch" a) =>
                    Lens.Family2.LensLike' f s a
maybe'ifNoneMatch
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'ifNoneMatch")
maybe'ifNotModified ::
                    forall f s a .
                      (Prelude.Functor f,
                       Lens.Labels.HasLens' s "maybe'ifNotModified" a) =>
                      Lens.Family2.LensLike' f s a
maybe'ifNotModified
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'ifNotModified")
maybe'includeContext ::
                     forall f s a .
                       (Prelude.Functor f,
                        Lens.Labels.HasLens' s "maybe'includeContext" a) =>
                       Lens.Family2.LensLike' f s a
maybe'includeContext
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'includeContext")
maybe'increment ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'increment" a) =>
                  Lens.Family2.LensLike' f s a
maybe'increment
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'increment")
maybe'key ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'key" a) =>
            Lens.Family2.LensLike' f s a
maybe'key
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'key")
maybe'lastMod ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'lastMod" a) =>
                Lens.Family2.LensLike' f s a
maybe'lastMod
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'lastMod")
maybe'lastModUsecs ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'lastModUsecs" a) =>
                     Lens.Family2.LensLike' f s a
maybe'lastModUsecs
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'lastModUsecs")
maybe'lastWriteWins ::
                    forall f s a .
                      (Prelude.Functor f,
                       Lens.Labels.HasLens' s "maybe'lastWriteWins" a) =>
                      Lens.Family2.LensLike' f s a
maybe'lastWriteWins
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'lastWriteWins")
maybe'linkfun ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'linkfun" a) =>
                Lens.Family2.LensLike' f s a
maybe'linkfun
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'linkfun")
maybe'mapUpdate ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'mapUpdate" a) =>
                  Lens.Family2.LensLike' f s a
maybe'mapUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'mapUpdate")
maybe'maxResults ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'maxResults" a) =>
                   Lens.Family2.LensLike' f s a
maybe'maxResults
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'maxResults")
maybe'maxScore ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'maxScore" a) =>
                 Lens.Family2.LensLike' f s a
maybe'maxScore
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'maxScore")
maybe'modfun ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'modfun" a) =>
               Lens.Family2.LensLike' f s a
maybe'modfun
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'modfun")
maybe'n ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'n" a) =>
          Lens.Family2.LensLike' f s a
maybe'n
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'n")
maybe'name ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'name" a) =>
             Lens.Family2.LensLike' f s a
maybe'name
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'name")
maybe'node ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'node" a) =>
             Lens.Family2.LensLike' f s a
maybe'node
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'node")
maybe'notfoundOk ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'notfoundOk" a) =>
                   Lens.Family2.LensLike' f s a
maybe'notfoundOk
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'notfoundOk")
maybe'numFound ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'numFound" a) =>
                 Lens.Family2.LensLike' f s a
maybe'numFound
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'numFound")
maybe'oldVclock ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'oldVclock" a) =>
                  Lens.Family2.LensLike' f s a
maybe'oldVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'oldVclock")
maybe'paginationSort ::
                     forall f s a .
                       (Prelude.Functor f,
                        Lens.Labels.HasLens' s "maybe'paginationSort" a) =>
                       Lens.Family2.LensLike' f s a
maybe'paginationSort
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'paginationSort")
maybe'phase ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'phase" a) =>
              Lens.Family2.LensLike' f s a
maybe'phase
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'phase")
maybe'pr ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'pr" a) =>
           Lens.Family2.LensLike' f s a
maybe'pr
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pr")
maybe'presort ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'presort" a) =>
                Lens.Family2.LensLike' f s a
maybe'presort
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'presort")
maybe'pw ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'pw" a) =>
           Lens.Family2.LensLike' f s a
maybe'pw
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'pw")
maybe'r ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'r" a) =>
          Lens.Family2.LensLike' f s a
maybe'r
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'r")
maybe'rangeMax ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'rangeMax" a) =>
                 Lens.Family2.LensLike' f s a
maybe'rangeMax
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rangeMax")
maybe'rangeMin ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'rangeMin" a) =>
                 Lens.Family2.LensLike' f s a
maybe'rangeMin
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rangeMin")
maybe'register ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'register" a) =>
                 Lens.Family2.LensLike' f s a
maybe'register
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'register")
maybe'registerUpdate ::
                     forall f s a .
                       (Prelude.Functor f,
                        Lens.Labels.HasLens' s "maybe'registerUpdate" a) =>
                       Lens.Family2.LensLike' f s a
maybe'registerUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) ::
         (Lens.Labels.Proxy#) "maybe'registerUpdate")
maybe'repl ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'repl" a) =>
             Lens.Family2.LensLike' f s a
maybe'repl
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'repl")
maybe'response ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'response" a) =>
                 Lens.Family2.LensLike' f s a
maybe'response
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'response")
maybe'returnBody ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'returnBody" a) =>
                   Lens.Family2.LensLike' f s a
maybe'returnBody
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnBody")
maybe'returnHead ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'returnHead" a) =>
                   Lens.Family2.LensLike' f s a
maybe'returnHead
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnHead")
maybe'returnTerms ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'returnTerms" a) =>
                    Lens.Family2.LensLike' f s a
maybe'returnTerms
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'returnTerms")
maybe'rows ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'rows" a) =>
             Lens.Family2.LensLike' f s a
maybe'rows
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rows")
maybe'rw ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'rw" a) =>
           Lens.Family2.LensLike' f s a
maybe'rw
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'rw")
maybe'schema ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'schema" a) =>
               Lens.Family2.LensLike' f s a
maybe'schema
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'schema")
maybe'search ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'search" a) =>
               Lens.Family2.LensLike' f s a
maybe'search
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'search")
maybe'searchIndex ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'searchIndex" a) =>
                    Lens.Family2.LensLike' f s a
maybe'searchIndex
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'searchIndex")
maybe'setUpdate ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'setUpdate" a) =>
                  Lens.Family2.LensLike' f s a
maybe'setUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'setUpdate")
maybe'sloppyQuorum ::
                   forall f s a .
                     (Prelude.Functor f,
                      Lens.Labels.HasLens' s "maybe'sloppyQuorum" a) =>
                     Lens.Family2.LensLike' f s a
maybe'sloppyQuorum
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'sloppyQuorum")
maybe'smallVclock ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'smallVclock" a) =>
                    Lens.Family2.LensLike' f s a
maybe'smallVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'smallVclock")
maybe'sort ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'sort" a) =>
             Lens.Family2.LensLike' f s a
maybe'sort
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'sort")
maybe'start ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'start" a) =>
              Lens.Family2.LensLike' f s a
maybe'start
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'start")
maybe'stream ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'stream" a) =>
               Lens.Family2.LensLike' f s a
maybe'stream
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'stream")
maybe'tag ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'tag" a) =>
            Lens.Family2.LensLike' f s a
maybe'tag
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'tag")
maybe'termRegex ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'termRegex" a) =>
                  Lens.Family2.LensLike' f s a
maybe'termRegex
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'termRegex")
maybe'timeout ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'timeout" a) =>
                Lens.Family2.LensLike' f s a
maybe'timeout
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'timeout")
maybe'ttl ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'ttl" a) =>
            Lens.Family2.LensLike' f s a
maybe'ttl
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'ttl")
maybe'unchanged ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'unchanged" a) =>
                  Lens.Family2.LensLike' f s a
maybe'unchanged
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'unchanged")
maybe'value ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'value" a) =>
              Lens.Family2.LensLike' f s a
maybe'value
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'value")
maybe'version ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'version" a) =>
                Lens.Family2.LensLike' f s a
maybe'version
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'version")
maybe'vtag ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'vtag" a) =>
             Lens.Family2.LensLike' f s a
maybe'vtag
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'vtag")
maybe'w ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'w" a) =>
          Lens.Family2.LensLike' f s a
maybe'w
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'w")
maybe'writeOnce ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'writeOnce" a) =>
                  Lens.Family2.LensLike' f s a
maybe'writeOnce
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'writeOnce")
maybe'youngVclock ::
                  forall f s a .
                    (Prelude.Functor f,
                     Lens.Labels.HasLens' s "maybe'youngVclock" a) =>
                    Lens.Family2.LensLike' f s a
maybe'youngVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'youngVclock")
modfun ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "modfun" a) =>
         Lens.Family2.LensLike' f s a
modfun
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "modfun")
module' ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "module'" a) =>
          Lens.Family2.LensLike' f s a
module'
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "module'")
n ::
  forall f s a . (Prelude.Functor f, Lens.Labels.HasLens' s "n" a) =>
    Lens.Family2.LensLike' f s a
n = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "n")
name ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "name" a) =>
       Lens.Family2.LensLike' f s a
name
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "name")
node ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "node" a) =>
       Lens.Family2.LensLike' f s a
node
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "node")
notfoundOk ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "notfoundOk" a) =>
             Lens.Family2.LensLike' f s a
notfoundOk
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "notfoundOk")
numFound ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "numFound" a) =>
           Lens.Family2.LensLike' f s a
numFound
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "numFound")
oldVclock ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "oldVclock" a) =>
            Lens.Family2.LensLike' f s a
oldVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "oldVclock")
paginationSort ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "paginationSort" a) =>
                 Lens.Family2.LensLike' f s a
paginationSort
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "paginationSort")
phase ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "phase" a) =>
        Lens.Family2.LensLike' f s a
phase
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "phase")
postcommit ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "postcommit" a) =>
             Lens.Family2.LensLike' f s a
postcommit
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "postcommit")
pr ::
   forall f s a .
     (Prelude.Functor f, Lens.Labels.HasLens' s "pr" a) =>
     Lens.Family2.LensLike' f s a
pr
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "pr")
precommit ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "precommit" a) =>
            Lens.Family2.LensLike' f s a
precommit
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "precommit")
presort ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "presort" a) =>
          Lens.Family2.LensLike' f s a
presort
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "presort")
props ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "props" a) =>
        Lens.Family2.LensLike' f s a
props
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "props")
pw ::
   forall f s a .
     (Prelude.Functor f, Lens.Labels.HasLens' s "pw" a) =>
     Lens.Family2.LensLike' f s a
pw
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "pw")
query ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "query" a) =>
        Lens.Family2.LensLike' f s a
query
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "query")
r ::
  forall f s a . (Prelude.Functor f, Lens.Labels.HasLens' s "r" a) =>
    Lens.Family2.LensLike' f s a
r = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "r")
rangeMax ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "rangeMax" a) =>
           Lens.Family2.LensLike' f s a
rangeMax
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "rangeMax")
rangeMin ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "rangeMin" a) =>
           Lens.Family2.LensLike' f s a
rangeMin
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "rangeMin")
register ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "register" a) =>
           Lens.Family2.LensLike' f s a
register
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "register")
registerUpdate ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "registerUpdate" a) =>
                 Lens.Family2.LensLike' f s a
registerUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "registerUpdate")
removes ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "removes" a) =>
          Lens.Family2.LensLike' f s a
removes
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "removes")
repl ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "repl" a) =>
       Lens.Family2.LensLike' f s a
repl
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "repl")
request ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "request" a) =>
          Lens.Family2.LensLike' f s a
request
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "request")
response ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "response" a) =>
           Lens.Family2.LensLike' f s a
response
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "response")
results ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "results" a) =>
          Lens.Family2.LensLike' f s a
results
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "results")
returnBody ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "returnBody" a) =>
             Lens.Family2.LensLike' f s a
returnBody
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "returnBody")
returnHead ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "returnHead" a) =>
             Lens.Family2.LensLike' f s a
returnHead
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "returnHead")
returnTerms ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "returnTerms" a) =>
              Lens.Family2.LensLike' f s a
returnTerms
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "returnTerms")
rows ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "rows" a) =>
       Lens.Family2.LensLike' f s a
rows
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "rows")
rw ::
   forall f s a .
     (Prelude.Functor f, Lens.Labels.HasLens' s "rw" a) =>
     Lens.Family2.LensLike' f s a
rw
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "rw")
schema ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "schema" a) =>
         Lens.Family2.LensLike' f s a
schema
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "schema")
search ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "search" a) =>
         Lens.Family2.LensLike' f s a
search
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "search")
searchIndex ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "searchIndex" a) =>
              Lens.Family2.LensLike' f s a
searchIndex
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "searchIndex")
set ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "set" a) =>
      Lens.Family2.LensLike' f s a
set
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "set")
setUpdate ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "setUpdate" a) =>
            Lens.Family2.LensLike' f s a
setUpdate
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "setUpdate")
sloppyQuorum ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "sloppyQuorum" a) =>
               Lens.Family2.LensLike' f s a
sloppyQuorum
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sloppyQuorum")
smallVclock ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "smallVclock" a) =>
              Lens.Family2.LensLike' f s a
smallVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "smallVclock")
sort ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "sort" a) =>
       Lens.Family2.LensLike' f s a
sort
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sort")
start ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "start" a) =>
        Lens.Family2.LensLike' f s a
start
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "start")
stream ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "stream" a) =>
         Lens.Family2.LensLike' f s a
stream
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "stream")
tag ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "tag" a) =>
      Lens.Family2.LensLike' f s a
tag
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "tag")
termRegex ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "termRegex" a) =>
            Lens.Family2.LensLike' f s a
termRegex
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "termRegex")
timeout ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "timeout" a) =>
          Lens.Family2.LensLike' f s a
timeout
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "timeout")
ttl ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "ttl" a) =>
      Lens.Family2.LensLike' f s a
ttl
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "ttl")
type' ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "type'" a) =>
        Lens.Family2.LensLike' f s a
type'
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "type'")
unchanged ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "unchanged" a) =>
            Lens.Family2.LensLike' f s a
unchanged
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "unchanged")
update ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "update" a) =>
         Lens.Family2.LensLike' f s a
update
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "update")
updates ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "updates" a) =>
          Lens.Family2.LensLike' f s a
updates
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "updates")
usermeta ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "usermeta" a) =>
           Lens.Family2.LensLike' f s a
usermeta
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "usermeta")
value ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "value" a) =>
        Lens.Family2.LensLike' f s a
value
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "value")
version ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "version" a) =>
          Lens.Family2.LensLike' f s a
version
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "version")
vtag ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "vtag" a) =>
       Lens.Family2.LensLike' f s a
vtag
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vtag")
w ::
  forall f s a . (Prelude.Functor f, Lens.Labels.HasLens' s "w" a) =>
    Lens.Family2.LensLike' f s a
w = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "w")
writeOnce ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "writeOnce" a) =>
            Lens.Family2.LensLike' f s a
writeOnce
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "writeOnce")
youngVclock ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "youngVclock" a) =>
              Lens.Family2.LensLike' f s a
youngVclock
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "youngVclock")