{ name = "node-http2"
, dependencies =
  [ "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "functions"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-net"
  , "node-path"
  , "node-streams"
  , "nullable"
  , "partial"
  , "prelude"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
