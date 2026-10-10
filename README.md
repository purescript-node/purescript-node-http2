# purescript-node-http2

PureScript bindings to the `http2` Node.js module

## Usage

The bindings for the module have been implemented, excluding the [Compatibility API](https://nodejs.org/dist/latest-v18.x/docs/api/http2.html#compatibility-api).

Install the package with Spago:

```sh
spago install node-http2
```

The bindings written thus far are mostly safe. There are a few cases where safety cannot be properly handled in PureScript due to some decisions made by Node.js. In other cases, it's not yet clear to me whether to represent something as `foreign import data`, `Object`, and/or an `ADT`.
