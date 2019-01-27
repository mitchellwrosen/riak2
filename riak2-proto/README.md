### `riak2-proto`

This package contains:

* Protobuf data types generated from [`riak.proto`](./proto/riak.proto) by
  [`generate-proto.sh`](./generate-proto.sh).

* Big [`Request`](./src/Riak/Request.hs) and
  [`Response`](./src/Riak/Response.hs) sum types and related encoding/decoding
  functions.
