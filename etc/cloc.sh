#!/usr/bin/env sh

set -e

badge src $(cloc --json riak2/src | jq .Haskell.code) :blue .svg > ./etc/riak2-src-sloc.svg
badge test $(cloc --json riak2/test | jq .Haskell.code) :blue .svg > ./etc/riak2-test-sloc.svg

badge src $(cloc --json riak2-core/src | jq .Haskell.code) :blue .svg > ./etc/riak2-core-src-sloc.svg
badge test $(cloc --json riak2-core/test | jq .Haskell.code) :blue .svg > ./etc/riak2-core-test-sloc.svg

badge src $(cloc --json riak2-core-internal/src | jq .Haskell.code) :blue .svg > ./etc/riak2-core-internal-src-sloc.svg
badge test $(cloc --json riak2-core-internal/test | jq .Haskell.code) :blue .svg > ./etc/riak2-core-internal-test-sloc.svg

badge src $(cloc --json riak2-handle-impl-socket/src | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-socket-src-sloc.svg
badge test $(cloc --json riak2-handle-impl-socket/test | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-socket-test-sloc.svg

badge src $(cloc --json riak2-handle-impl-concurrent-socket/src | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-concurrent-socket-src-sloc.svg
badge test $(cloc --json riak2-handle-impl-concurrent-socket/test | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-concurrent-socket-test-sloc.svg

badge src $(cloc --json riak2-handle-impl-managed/src | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-managed-src-sloc.svg
badge test $(cloc --json riak2-handle-impl-managed/test | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-managed-test-sloc.svg

badge src $(cloc --json riakc/src | jq .Haskell.code) :blue .svg > ./etc/riakc-src-sloc.svg
