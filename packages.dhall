let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230609/packages.dhall
        sha256:cb9995a97812b86f41f6d8efe4be981b55cd924252002f256084cfe2836bdfd6

in  upstream
      with node-event-emitter.version = "v2.0.0"
      with node-event-emitter.dependencies =
        [ "effect"
        , "either"
        , "functions"
        , "prelude"
        , "unsafe-coerce" 
        ]
      with node-streams.version = "eb31fe5d4040d5da4b4143c9f33d11a3ab1a9bdd"
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
      with node-net.version = "4d408a3b5f1025f56f5c1e97cb6f833a8f04651b"
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
        , version = "729af71451508bb51e1f3b9c056fa6ce093980eb"
        }
