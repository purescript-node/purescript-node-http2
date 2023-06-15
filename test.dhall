{ name = "node-http2-tests"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "node-buffer"
  , "node-event-emitter"
  , "node-fs"
  , "node-net"
  , "node-path"
  , "node-streams"
  , "node-tls"
  , "nullable"
  , "partial"
  , "prelude"
  , "refs"
  , "spec"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
