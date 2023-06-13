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
  , "node-fs"
  , "node-net"
  , "node-path"
  , "node-streams"
  , "nullable"
  , "partial"
  , "prelude"
  , "refs"
  , "spec"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
