#!/usr/bin/env sh

set -e

badge src $(cloc --json riak2/src | jq .Haskell.code) :blue .svg > ./etc/riak2-src-sloc.svg
badge test $(cloc --json riak2/test | jq .Haskell.code) :blue .svg > ./etc/riak2-test-sloc.svg

badge src $(cloc --json riak2-core/src | jq .Haskell.code) :blue .svg > ./etc/riak2-core-src-sloc.svg
badge test $(cloc --json riak2-core/test | jq .Haskell.code) :blue .svg > ./etc/riak2-core-test-sloc.svg

badge src $(cloc --json riak2-core-internal/src | jq .Haskell.code) :blue .svg > ./etc/riak2-core-internal-src-sloc.svg
badge test $(cloc --json riak2-core-internal/test | jq .Haskell.code) :blue .svg > ./etc/riak2-core-internal-test-sloc.svg

badge src $(cloc --json riak2-interface-impls/src | jq .Haskell.code) :blue .svg > ./etc/riak2-interface-impls-src-sloc.svg
badge test $(cloc --json riak2-interface-impls/test | jq .Haskell.code) :blue .svg > ./etc/riak2-interface-impls-test-sloc.svg

badge src $(cloc --json riakc/src | jq .Haskell.code) :blue .svg > ./etc/riakc-src-sloc.svg
