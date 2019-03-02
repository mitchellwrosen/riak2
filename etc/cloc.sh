#!/usr/bin/env sh

set -e

badge src $(cloc --json riak2/public riak2/src | jq .Haskell.code) :blue .svg > ./etc/riak2-src-sloc.svg
badge test $(cloc --json riak2/test | jq .Haskell.code) :blue .svg > ./etc/riak2-test-sloc.svg

badge src $(cloc --json libriak/src | jq .Haskell.code) :blue .svg > ./etc/libriak-src-sloc.svg
badge test $(cloc --json libriak/test | jq .Haskell.code) :blue .svg > ./etc/libriak-test-sloc.svg

badge src $(cloc --json riakc/src | jq .Haskell.code) :blue .svg > ./etc/riakc-src-sloc.svg

badge src $(cloc --json riak2-benchmarks/src | jq .Haskell.code) :blue .svg > ./etc/riak2-benchmarks-src-sloc.svg
