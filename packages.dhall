let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230609/packages.dhall
        sha256:cb9995a97812b86f41f6d8efe4be981b55cd924252002f256084cfe2836bdfd6

in  upstream
      with node-event-emitter.version = "b283c3eb6abc32a88fd8876af746b9548e78d93f"
      with node-event-emitter.dependencies =
        [ "effect"
        , "either"
        , "functions"
        , "maybe"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-streams.version = "684041e14e56c75c0bf49db0e556aec6d0248e5a"
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
      with node-fs.version = "be0a346f4b72826947a124486c299b842c5e2601"
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
      with node-net.version = "b0a93e2f27144386a31039aef9781b7da17e048f"
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
        , version = "6ca365ac80c1dd43b93adc9e0a1ded6389b8a7c3"
        }
