{ name =
    "my-project"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "psci-support"
    , "test-unit"
    , "toppokki"
    , "web-html"
    , "web-dom"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
