### `riak2-handle-impl-concurrent-socket`

* This package contains a
  ["Riak handle"](../riak2-core/src/Riak/Interface/Signature.hsig)
  implementation that improves on
  [`riak2-handle-impl-socket`](../riak2-handle-impls-socket) by pipelining
  concurrent requests to Riak.
