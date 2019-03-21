### [`riak2`](./riak2)

[![riak2-src-sloc](./etc/riak2-src-sloc.svg)](./riak2/src) [![riak2-test-sloc](./etc/riak2-test-sloc.svg)](./riak2/test)

This package contains high-level Riak bindings.

### [`libriak`](./libriak)

[![libriak-src-sloc](./etc/libriak-src-sloc.svg)](./libriak/src) [![libriak-test-sloc](./etc/libriak-test-sloc.svg)](./libriak/test)

This package contain low-level Riak bindings:

* Protobuf data types re-exported from `riak-protobuf`.

* [`Request`](./libriak-internal/src/Libriak/Request.hs) and
  [`Response`](./libriak-internal/src/Libriak/Response.hs) sum types and related
  encoding/decoding functions.

* A [`Connection`](./libriak-internal/src/Libriak/Internal/Connection.hs) socket
  wrapper capable of sending and receiving length-prefixed packets.

// TODO update libriak readme description

They are intended to be a moderately usable interface to Riak, fully faithful to
the exposed protobuf API. If you want to build a high level Riak library, you
should be happy building it on top of `libriak`.

### [`riakc`](./riakc)

[![riakc-src-sloc](./etc/riakc-src-sloc.svg)](./riakc/src)

This package contains an command-line Riak client.

### [`riak2-benchmarks`](./riak2-benchmarks)

[![riak2-benchmarks-src-sloc](./etc/riak2-benchmarks-src-sloc.svg)](./riak2-benchmarks/src)

Misc benchmarks.
