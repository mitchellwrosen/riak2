#!/bin/bash

for f in $(find $SCHEMAS_DIR -type f); do
  BUCKET_NAME=$(basename $f)
  BUCKET_PROPS=$(cat $f)
  $RIAK_ADMIN bucket-type create $BUCKET_NAME "{\"props\":$BUCKET_PROPS}"
  $RIAK_ADMIN bucket-type activate $BUCKET_NAME
done
