let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230609/packages.dhall
        sha256:cb9995a97812b86f41f6d8efe4be981b55cd924252002f256084cfe2836bdfd6

in  upstream
      with node-event-emitter.version = "7da1c367c338be31fb05e22078e8edc5489904e2"
      with node-event-emitter.dependencies =
        [ "effect"
        , "either"
        , "functions"
        , "prelude"
        , "unsafe-coerce" 
        ]
      with node-streams.version = "4e12ea5fbd06923c8f2e24b4fa194236b2fabaae"
      with node-streams.dependencies =
        [ "effect"
        , "exceptions"
        , "maybe"
        , "node-buffer"
        , "node-event-emitter"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-net.version = "df7d82991fdf17c0d213c1e202fbb940ea3150e2"
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
        , version = "30e95487ec975de72a1052e8ebe61280616e1fc2"
        }
