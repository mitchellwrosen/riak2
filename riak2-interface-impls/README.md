### `riak2-interface-impls`

* This package contains a few
  ["Riak interface"](../riak2-core/src/Riak/Interface/Signature.hsig)
  implementations:

    * [`Riak.Interface.Impl.Socket`](./src/Riak/Interface/Impl/Socket.hs)

      A socket that provides Riak access to one thread at a time.

    * [`Riak.Interface.Impl.Socket.Concurrent`](./src/Riak/Interface/Impl/Socket/Concurrent.hs)

      A socket that pipelines concurrent requests to Riak. Streaming requests,
      however, require exclusive access to the socket until the response is
      finished.

    * [`Riak.Interface.Impl.Managed`](./src/Riak/Interface/Impl/Managed.hs)

      An interface "modifier" implementation, i.e., it is both parameterized by and
      implements the Riak interface signature.

      It upgrades an interface by automatically reconnecting and retrying on failure
      (not currently configurable).
