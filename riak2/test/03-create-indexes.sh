#!/bin/bash

$RIAK_ADMIN wait-for-service yokozuna

curl -XPUT $HOST:$HTTP_PORT/search/index/default1 -H 'Content-Type: application/json' -d '{"n_val":1}'
curl -XPUT $HOST:$HTTP_PORT/search/index/default3
