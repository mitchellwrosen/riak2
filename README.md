### [`riak2`](./riak2)

[![riak2-src-sloc](./etc/riak2-src-sloc.svg)](./riak2/src) [![riak2-test-sloc](./etc/riak2-test-sloc.svg)](./riak2/test)

This package contains high-level Riak bindings.

### [`libriak`](./libriak) / [`libriak-internal`](./libriak-internal)

[![libriak-src-sloc](./etc/libriak-src-sloc.svg)](./libriak/src) [![libriak-test-sloc](./etc/libriak-test-sloc.svg)](./libriak/test)

[![libriak-internal-src-sloc](./etc/libriak-internal-src-sloc.svg)](./libriak-internal/src) [![libriak-internal-test-sloc](./etc/libriak-internal-test-sloc.svg)](./libriak-internal/test)

These packages contain low-level Riak bindings:

* Protobuf data types re-exported from `riak-protobuf`.

* [`Request`](./libriak-internal/src/Libriak/Request.hs) and
  [`Response`](./libriak-internal/src/Libriak/Response.hs) sum types and related
  encoding/decoding functions.

* A [`Connection`](./libriak-internal/src/Libriak/Internal/Connection.hs) socket
  wrapper capable of sending and receiving length-prefixed packets.

* The the ["Riak handle"](./libriak/src/Riak/Handle/Signature.hsig) Backpack
  signature.

They are intended to be a fully usable interface to Riak, fully faithful to the
exposed protobuf API. If you want to build a high level Riak library, you should
be happy building it on top of `libriak`.

As for why there is an `-internal` package: it is not possible for one package
to both define a Backpack signature, and provide a module intended to satisfy
that signature. If you are defining a new handle, you must depend on
`libriak-internal`, which includes the core types you need. Otherwise, use
`libriak`, which re-exports all of `libriak-internal`.

### [`riak2-handle-impl-exclusive`](./riak2-handle-impl-exclusive)

[![riak2-handle-impl-exclusive-src-sloc](./etc/riak2-handle-impl-exclusive-src-sloc.svg)](./riak2-handle-impl-exclusive/src) [![riak2-handle-impl-exclusive-test-sloc](./etc/riak2-handle-impl-exclusive-test-sloc.svg)](./riak2-handle-impl-exclusive/test)

This package contains a
["Riak handle"](./libriak/src/Riak/Handle/Signature.hsig)
implementation that provides exclusive Riak access to one thread at a time.

### [`riak2-handle-impl-pipeline`](./riak2-handle-impl-pipeline)

[![riak2-handle-impl-pipeline-src-sloc](./etc/riak2-handle-impl-pipeline-src-sloc.svg)](./riak2-handle-impl-pipeline/src) [![riak2-handle-impl-pipeline-test-sloc](./etc/riak2-handle-impl-pipeline-test-sloc.svg)](./riak2-handle-impl-pipeline/test)

This package contains a
["Riak handle"](./libriak/src/Riak/Handle/Signature.hsig)
implementation that improves on
[`riak2-handle-impl-exclusive`](./riak2-handle-impl-exclusive) by pipelining
concurrent requests to Riak.

### [`riak2-handle-impl-managed`](./riak2-handle-impl-managed)

[![riak2-handle-impl-managed-src-sloc](./etc/riak2-handle-impl-managed-src-sloc.svg)](./riak2-handle-impl-managed/src) [![riak2-handle-impl-managed-test-sloc](./etc/riak2-handle-impl-managed-test-sloc.svg)](./riak2-handle-impl-managed/test)

This package contains a "modifier"
["Riak handle"](./libriak/src/Riak/Handle/Signature.hsig)
implementation, i.e. it is both parameterized by and implements the signature,
that upgrades a handle to automatically reconnect and retry on failure.

### [`riak2-handle-impl-striped`](./riak2-handle-impl-striped)

[![riak2-handle-impl-striped-src-sloc](./etc/riak2-handle-impl-striped-src-sloc.svg)](./riak2-handle-impl-striped/src) [![riak2-handle-impl-striped-test-sloc](./etc/riak2-handle-impl-striped-test-sloc.svg)](./riak2-handle-impl-striped/test)

This package contains another "modifier"
["Riak handle"](./libriak/src/Riak/Handle/Signature.hsig)
implementation that upgrades a handle to an array of handles.

### [`riakc`](./riakc)

[![riakc-src-sloc](./etc/riakc-src-sloc.svg)](./riakc/src)

This package contains an command-line Riak client.

### [`riak2-benchmarks`](./riak2-benchmarks)

[![riak2-benchmarks-src-sloc](./etc/riak2-benchmarks-src-sloc.svg)](./riak2-benchmarks/src)

Misc benchmarks.
