### [`riak2`](./riak2)

![riak2-src-sloc](./etc/riak2-src-sloc.svg) ![riak2-test-sloc](./etc/riak2-test-sloc.svg)

This package contains high-level Riak bindings.

### [`libriak`](./libriak) / [`libriak-internal`](./libriak-internal)

![libriak-src-sloc](./etc/libriak-src-sloc.svg) ![libriak-test-sloc](./etc/libriak-test-sloc.svg)

These packages contain low-level Riak bindings:

* Protobuf data types generated from
  [`riak.proto`](./libriak-internal/proto/riak.proto) by
  [`generate-proto.sh`](./libriak-internal/generate-proto.sh).

* [`Request`](./libriak-internal/src/Riak/Request.hs) and
  [`Response`](./libriak-internal/src/Riak/Response.hs) sum types and related
  encoding/decoding functions.

* A [`Riak.Connection`](./libriak-internal/src/Riak/Connection.hs) socket
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

![riak2-handle-impl-exclusive-src-sloc](./etc/riak2-handle-impl-exclusive-src-sloc.svg) ![riak2-handle-impl-exclusive-test-sloc](./etc/riak2-handle-impl-exclusive-test-sloc.svg)

This package contains a
["Riak handle"](./libriak/src/Riak/Interface/Signature.hsig)
implementation that provides exclusive Riak access to one thread at a time.

### [`riak2-handle-impl-pipeline`](./riak2-handle-impl-pipeline)

![riak2-handle-impl-pipeline-src-sloc](./etc/riak2-handle-impl-pipeline-src-sloc.svg) ![riak2-handle-impl-pipeline-test-sloc](./etc/riak2-handle-impl-pipeline-test-sloc.svg)

This package contains a
["Riak handle"](./libriak/src/Riak/Interface/Signature.hsig)
implementation that improves on
[`riak2-handle-impl-exclusive`](./riak2-handle-impl-exclusive) by pipelining
concurrent requests to Riak.

### [`riak2-handle-impl-managed`](./riak2-handle-impl-managed)

![riak2-handle-impl-managed-src-sloc](./etc/riak2-handle-impl-managed-src-sloc.svg) ![riak2-handle-impl-managed-test-sloc](./etc/riak2-handle-impl-managed-test-sloc.svg)

This package contains a "modifier"
["Riak handle"](./libriak/src/Riak/Interface/Signature.hsig)
implementation, i.e. it is both parameterized by and implements the signature,
that upgrades a handle to automatically reconnect and retry on failure.

### [`riakc`](./riakc)

![riakc-src-sloc](./etc/riakc-src-sloc.svg)

This package contains an command-line Riak client.

### [`riak2-benchmarks`](./riak2-benchmarks)

![riak2-benchmarks-src-sloc](./etc/riak2-benchmarks-src-sloc.svg)

Misc benchmarks.
