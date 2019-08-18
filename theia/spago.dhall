{ name =
    "theia"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "psci-support"
    , "test-unit"
    , "toppokki"
    , "web-dom"
    , "web-html"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
