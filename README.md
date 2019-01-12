### `riak2-proto`

This package contains core Riak types generated from
[riak.proto](./riak2-proto/riak.proto), as well as a few helper types and type
classes that classify them.

### `riak2-client`

This package contains a low-level `Connection` abstraction for connecting to,
sending to, and receiving from a single Riak node.

### `riak2`

This package contains high-level Riak bindings built atop `riak2-proto` and
`riak2-client`.
