### `riak2-core`

This package contains low-level Riak bindings.

* Protobuf data types generated from [`riak.proto`](./proto/riak.proto) by
  [`generate-proto.sh`](./generate-proto.sh).

* Big [`Request`](./src/Riak/Request.hs) and
  [`Response`](./src/Riak/Response.hs) sum types and related encoding/decoding
  functions.

* A small [`Riak.Socket`](./src/Riak/Socket.hs) wrapper around the `network`
  package's `Socket`.

* The ["Riak interface"](./src/Riak/Interface/Signature.hsig) as a Backpack
  signature.
