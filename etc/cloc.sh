#!/usr/bin/env sh

set -e

badge src $(cloc --json riak2/src | jq .Haskell.code) :blue .svg > ./etc/riak2-src-sloc.svg
badge test $(cloc --json riak2/test | jq .Haskell.code) :blue .svg > ./etc/riak2-test-sloc.svg

badge src $(cloc --json libriak/src | jq .Haskell.code) :blue .svg > ./etc/libriak-src-sloc.svg
badge test $(cloc --json libriak/test | jq .Haskell.code) :blue .svg > ./etc/libriak-test-sloc.svg

badge src $(cloc --json libriak-internal/src | jq .Haskell.code) :blue .svg > ./etc/libriak-internal-src-sloc.svg
badge test $(cloc --json libriak-internal/test | jq .Haskell.code) :blue .svg > ./etc/libriak-internal-test-sloc.svg

badge src $(cloc --json riak2-handle-impl-exclusive/src | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-exclusive-src-sloc.svg
badge test $(cloc --json riak2-handle-impl-exclusive/test | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-exclusive-test-sloc.svg

badge src $(cloc --json riak2-handle-impl-pipeline/src | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-pipeline-src-sloc.svg
badge test $(cloc --json riak2-handle-impl-pipeline/test | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-pipeline-test-sloc.svg

badge src $(cloc --json riak2-handle-impl-managed/src | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-managed-src-sloc.svg
badge test $(cloc --json riak2-handle-impl-managed/test | jq .Haskell.code) :blue .svg > ./etc/riak2-handle-impl-managed-test-sloc.svg

badge src $(cloc --json riakc/src | jq .Haskell.code) :blue .svg > ./etc/riakc-src-sloc.svg
