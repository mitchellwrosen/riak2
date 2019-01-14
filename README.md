### `riak2-proto`

This package contains core Riak types generated from
[`riak.proto`](./riak2-proto/riak.proto), as well as a few helper types and type
classes that classify them.

### `riak2-client-impl-sequential-socket`

This package contains a
[`Riak.Client.Signature`](./riak2/src/Riak/Client/Signature.hsig) implementation
for a socket that provides access to one thread at a time.

### `riak2-client-impl-concurrent-socket`

This package contains a
[`Riak.Client.Signature`](./riak2/src/Riak/Client/Signature.hsig) implementation
that improves the potential throughput of the sequential socket. Multiple
threads are able to send requests to Riak concurrently.

### `riak2`

This package contains high-level Riak bindings. Requires
[`Riak.Client.Signature`](./riak2/src/Riak/Client/Signature.hsig) to be
instantiated.
