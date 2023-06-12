{ name = "node-http2-tests"
, dependencies =
  [ "console", "effect", "node-event-emitter", "prelude", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
