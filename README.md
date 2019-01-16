### [`riak2-proto`](./riak2-proto)

This package contains core Riak types generated from
[`riak.proto`](./riak2-proto/riak.proto), as well as a few helper types and type
classes that classify them.

### [`riak2-iface`](./riak2-iface)

Backpack signature and higher-level wrapper for the "Riak interface"
abstraction:

* [`Riak.Interface.Signature`](./riak2-iface/src/Riak/Interface/Signature.hsig)
* [`Riak.Interface`](./riak2-iface/src/Riak/Interface.hs)

### [`riak2-iface-socket`](./riak2-iface-socket)

This package contains a `riak2-iface` implementation for a socket that provides
Riak access to one thread at a time.

### [`riak2-iface-concurrent-socket`](./riak2-iface-concurrent-socket)

This package contains a `riak2-iface` implementation that improves the potential
throughput of `riak2-iface-socket`. Multiple threads are able to concurrently
pipeline requests to Riak.

### [`riak2-iface-managed`](./riak2-iface-managed)

This package contains a `riak2-iface` "modifier" implementation, i.e., it
is both parameterized by and implements the Riak interface signature.

It upgrades an interface by automatically reconnecting and retrying on failure
(not currently configurable).

### `riak2`

This package contains high-level Riak bindings.
