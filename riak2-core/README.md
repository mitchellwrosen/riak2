### `riak2-core`

This package contains low-level Riak bindings. It pulls together:

* The wire format types ([`riak2-proto`](../riak2-proto/))

* The Riak interface signature ([`riak2-interface`](../riak2-interface))

Additionally, it defines

* A small [`Riak.Socket`](./src/Riak/Socket.hs) wrapper around the `network`
  package's `Socket`.

* A few Riak interface implementations:

    * [`Riak.Interface.Impl.Socket`](./riak-core-internal/Riak/Interface/Impl/Socket.hs)

      A socket that provides Riak access to one thread at a time.

    * [`Riak.Interface.Impl.Socket.Concurrent`](./riak-core-internal/Riak/Interface/Impl/Socket/Concurrent.hs)

      A socket that pipelines concurrent requests to Riak. Streaming requests,
      however, require exclusive access to the socket until the response is
      finished.

    * [`Riak.Interface.Impl.Managed`](./riak2-interface-impl-managed/Riak/Interface/Impl/Managed.hs)

      An interface "modifier" implementation, i.e., it is both parameterized by and
      implements the Riak interface signature.

      It upgrades an interface by automatically reconnecting and retrying on failure
      (not currently configurable).

The directory/library organization is currently in flux as I figure out how to
properly link components with Backpack in a way that does not require mutual
recursion (not currently supported)...
