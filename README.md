### `riak2-proto`

This package contains core Riak types generated from
[`riak.proto`](./riak2-proto/riak.proto), as well as a few helper types and type
classes that classify them.

### `riak2-client-sig`

Signature for the
["Riak client"](./riak2-client-sig/src/Riak/Client/Signature.hsig) abstraction,
which exposes an API for connecting to Riak, sending requests, and receiving
responses.

### `riak2-client-impl-sequential-socket`

This package contains a `riak2-client-sig` implementation for a socket that
provides Riak access to one thread at a time.

### `riak2-client-impl-concurrent-socket`

This package contains a `riak2-client-sig` implementation that improves the
potential throughput of the sequential socket. Multiple threads are able to send
requests to Riak concurrently.

### `riak2`

This package contains high-level Riak bindings. Requires `riak2-client-sig` to
be instantiated.
