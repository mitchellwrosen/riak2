#!/bin/sh

OUTDIR=src-proto

echo "Generating $OUTDIR/Proto/Proto/Riak.hs"
echo "Generating $OUTDIR/Proto/Proto/Riak_Fields.hs"

protoc \
  --plugin=protoc-gen-haskell=$(which proto-lens-protoc) \
  --haskell_out="$OUTDIR" proto/riak.proto
