### [`riak2`](./riak2)

![riak2-src-sloc](./etc/riak2-src-sloc.svg) ![riak2-test-sloc](./etc/riak2-test-sloc.svg)

This package contains high-level Riak bindings.

### [`riak2-core`](./riak2-core)

![riak2-core-src-sloc](./etc/riak2-core-src-sloc.svg) ![riak2-core-test-sloc](./etc/riak2-core-test-sloc.svg)

This package re-exports all of [`riak2-core-internal`](./riak2-core-internal),
and defines the ["Riak handle"](./riak2-core/src/Riak/Handle/Signature.hsig)
Backpack signature.

### [`riak2-core-internal`](./riak2-core-internal)

![riak2-core-internal-src-sloc](./etc/riak2-core-internal-src-sloc.svg) ![riak2-core-internal-test-sloc](./etc/riak2-core-internal-test-sloc.svg)

This package contains low-level Riak bindings:

* Protobuf data types generated from
  [`riak.proto`](./riak2-core-internal/proto/riak.proto) by
  [`generate-proto.sh`](./riak2-core-internal/generate-proto.sh).

* [`Request`](./riak2-core-internal/src/Riak/Request.hs) and
  [`Response`](./riak2-core-internal/src/Riak/Response.hs) sum types and related
  encoding/decoding functions.

* A small [`Riak.Socket`](./riak2-core-internal/src/Riak/Socket.hs) wrapper
  around the `network` package's `Socket`.

### [`riak2-handle-impl-exclusive`](./riak2-handle-impl-exclusive)

![riak2-handle-impl-exclusive-src-sloc](./etc/riak2-handle-impl-exclusive-src-sloc.svg) ![riak2-handle-impl-exclusive-test-sloc](./etc/riak2-handle-impl-exclusive-test-sloc.svg)

This package contains a
["Riak handle"](./riak2-core/src/Riak/Interface/Signature.hsig)
implementation that provides exclusive Riak access to one thread at a time.

### [`riak2-handle-impl-pipeline`](./riak2-handle-impl-pipeline)

![riak2-handle-impl-pipeline-src-sloc](./etc/riak2-handle-impl-pipeline-src-sloc.svg) ![riak2-handle-impl-pipeline-test-sloc](./etc/riak2-handle-impl-pipeline-test-sloc.svg)

This package contains a
["Riak handle"](./riak2-core/src/Riak/Interface/Signature.hsig)
implementation that improves on
[`riak2-handle-impl-exclusive`](./riak2-handle-impl-exclusive) by pipelining
concurrent requests to Riak.

### [`riak2-handle-impl-managed`](./riak2-handle-impl-managed)

![riak2-handle-impl-managed-src-sloc](./etc/riak2-handle-impl-managed-src-sloc.svg) ![riak2-handle-impl-managed-test-sloc](./etc/riak2-handle-impl-managed-test-sloc.svg)

This package contains a "modifier"
["Riak handle"](./riak2-core/src/Riak/Interface/Signature.hsig)
implementation, i.e. it is both parameterized by and implements the signature,
that upgrades a handle to automatically reconnect and retry on failure.

### [`riakc`](./riakc)

![riakc-src-sloc](./etc/riakc-src-sloc.svg)

This package contains an command-line Riak client.
