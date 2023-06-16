# purescript-node-http2

PureScript bindings to the `http2` Node.js module

## Usage

The bindings for the module have been implemented, excluding the [Compatibility API](https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#compatibility-api).

This repo is incompatible with existing `node-streams` and `node-net` bindings in the default package set. Instead, this repo uses custom forks of [`node-streams@update-ffi-build-on-node-event-emitters`](https://github.com/purescript-node/purescript-node-streams/tree/update-ffi-build-on-node-event-emitters) and [`node-net@fully-implement`](https://github.com/purescript-node/purescript-node-net/tree/fully-implement) that implement their event handlers using the bindings exposed via [`node-event-emitter@v2.0.0`](https://github.com/purescript-node/purescript-node-event-emitter/tree/v2.0.0). See the [packages.dhall](./packages.dhall) file to see how to refer to these forks if you want to use this repo.

The bindings written thus far are mostly safe. There are a few cases where safety cannot be properly handled in PureScript due to some decisions made by Node.js. In other cases, it's not yet clear to me whether to represent something as `foreign import data`, `Object`, and/or an `ADT`.
