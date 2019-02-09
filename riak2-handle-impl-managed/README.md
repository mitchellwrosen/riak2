### `riak2-handle-impl-managed`

* This package contains a "modifier"
  ["Riak handle"](../riak2-core/src/Riak/Interface/Signature.hsig)
  implementation, i.e. it is both parameterized by and implements the signature,
  that upgrades a handle to automatically reconnect and retry on failure.
