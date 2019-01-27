#!/bin/sh

OUTDIR=riak2-core-internal

echo "Generating $OUTDIR/Proto/Proto/Riak.hs"
echo "Generating $OUTDIR/Proto/Proto/Riak_Fields.hs"

protoc \
  --plugin=protoc-gen-haskell=$(which proto-lens-protoc) \
  --haskell_out="$OUTDIR" proto/riak.proto
