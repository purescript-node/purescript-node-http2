let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230609/packages.dhall
        sha256:cb9995a97812b86f41f6d8efe4be981b55cd924252002f256084cfe2836bdfd6

in  upstream
      with node-event-emitter.version = "v3.0.0"
      with node-event-emitter.dependencies =
        [ "effect"
        , "either"
        , "functions"
        , "maybe"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-buffer.version = "v9.0.0"
      with node-buffer.dependencies =
        [ "arraybuffer-types"
        , "effect"
        , "maybe"
        , "st"
        , "unsafe-coerce"
        , "nullable"
        ]
      with node-streams.version = "v9.0.0"
      with node-streams.dependencies =
        [ "aff"
        , "effect"
        , "exceptions"
        , "maybe"
        , "node-buffer"
        , "node-event-emitter"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-fs.version = "v9.1.0"
      with node-fs.dependencies =
        [ "datetime"
        , "effect"
        , "either"
        , "enums"
        , "exceptions"
        , "functions"
        , "integers"
        , "js-date"
        , "maybe"
        , "node-buffer"
        , "node-path"
        , "node-streams"
        , "nullable"
        , "partial"
        , "prelude"
        , "strings"
        , "unsafe-coerce"
        ]
      with node-net.version = "v5.1.0"
      with node-net.dependencies =
        [ "console"
        , "datetime"
        , "effect"
        , "exceptions"
        , "maybe"
        , "node-buffer"
        , "node-event-emitter"
        , "node-fs"
        , "node-streams"
        , "nullable"
        , "partial"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-tls =
        { dependencies =
            [ "console"
            , "effect"
            , "either"
            , "exceptions"
            , "foreign"
            , "maybe"
            , "node-buffer"
            , "node-event-emitter"
            , "node-net"
            , "node-streams"
            , "nullable"
            , "partial"
            , "prelude"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/JordanMartinez/purescript-node-tls.git"
        , version = "v0.3.1"
        }
