{ name = "node-http2"
, dependencies =
  [ "datetime"
  , "effect"
  , "either"
  , "exceptions"
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
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
}
